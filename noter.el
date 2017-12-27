;;; noter.el --- A synchronized, external annotator              -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Gonçalo Santos

;; Author: Gonçalo Santos (aka. weirdNox@GitHub)
;; Homepage: https://github.com/weirdNox/noter
;; Keywords: lisp pdf interleave annotate external sync notes documents
;; Package-Requires: ((emacs "24.4") cl-lib (org "9.0"))
;; Version: 1.0

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The idea is to let you create notes that are kept in sync when you scroll through the
;; document, but that are external to it - the notes themselves live in an Org-mode file. As
;; such, this leverages the power of Org-mode (the notes may have outlines, latex fragments,
;; babel, etc...) while acting like notes that are made /in/ the document.

;; Also, I must thank Sebastian for the original idea and inspiration!
;; Link to the original Interleave package:
;; https://github.com/rudolfochrist/interleave

;;; Code:
(require 'org)
(require 'org-element)
(require 'cl-lib)

(declare-function pdf-view-goto-page "ext:pdf-view")
(declare-function doc-view-goto-page "doc-view")
(declare-function image-mode-window-get "image-mode")

;; --------------------------------------------------------------------------------
;; NOTE(nox): User variables
(defgroup noter nil
  "A synchronized, external annotator"
  :group 'convenience
  :version "25.3.1")

(defcustom noter-property-pdf-file "NOTER_PDF"
  "Name of the property which specifies the PDF file."
  :group 'noter
  :type 'string)

(defcustom noter-property-note-page "NOTER_PAGE"
  "Name of the property which specifies the page of the current note."
  :group 'noter
  :type 'string)

(defcustom noter-split-direction 'horizontal
  "Whether the Noter frame should be split horizontally or vertically."
  :group 'noter
  :type '(choice (const :tag "Horizontal" horizontal)
                 (const :tag "Vertical" vertical)))

(defcustom noter-default-heading-title "Notes for page $p$"
  "The title of the headings created by `noter-insert-note'.
$p$ is replaced by the number of the page you are in at the
moment."
  :group 'noter
  :type 'string)

;; --------------------------------------------------------------------------------
;; NOTE(nox): Private variables
(cl-defstruct noter--session frame pdf-mode display-name property-text
              org-file-path pdf-file-path notes-buffer
              pdf-buffer level)

(defvar noter--sessions nil
  "List of Noter sessions.")

(defvar-local noter--session nil
  "Session associated with the current buffer.")

(defvar noter--inhibit-page-handler nil
  "Prevent page change from updating point in notes.")

;; --------------------------------------------------------------------------------
;; NOTE(nox): Utility functions
(defun noter--valid-session (session)
  (if (and session
           (frame-live-p (noter--session-frame session))
           (buffer-live-p (noter--session-pdf-buffer session))
           (buffer-live-p (noter--session-notes-buffer session)))
      t
    (noter-kill-session session)
    nil))

(defmacro noter--with-valid-session (&rest body)
  `(let ((session noter--session))
     (when (noter--valid-session session)
       (progn ,@body))))

(defun noter--handle-kill-buffer ()
  (noter--with-valid-session
   (let ((buffer (current-buffer))
         (notes-buffer (noter--session-notes-buffer session))
         (pdf-buffer (noter--session-pdf-buffer session)))
     ;; NOTE(nox): This needs to be checked in order to prevent session killing because of
     ;; temporary buffers with the same local variables
     (when (or (eq buffer notes-buffer)
               (eq buffer pdf-buffer))
       (noter-kill-session session)))))

(defun noter--handle-delete-frame (frame)
  (dolist (session noter--sessions)
    (when (eq (noter--session-frame session) frame)
      (noter-kill-session session))))

(defun noter--parse-root (&optional buffer property-pdf-path)
  ;; TODO(nox): Maybe create IDs in each noter session and use that instead of using
  ;; the property text that may be repeated... This would simplify some things
  (let* ((session noter--session)
         (use-args (and (stringp property-pdf-path)
                        (buffer-live-p buffer)
                        (with-current-buffer buffer (eq major-mode 'org-mode))))
         (notes-buffer (if use-args
                           buffer
                         (when session (noter--session-notes-buffer session))))
         (wanted-value (if use-args
                           property-pdf-path
                         (when session (noter--session-property-text session))))
         element)
    (when (buffer-live-p notes-buffer)
      (with-current-buffer notes-buffer
        (org-with-wide-buffer
         (unless (org-before-first-heading-p)
           ;; NOTE(nox): Start by trying to find a parent heading with the specified
           ;; property
           (let ((try-next t) property-value)
             (while try-next
               (setq property-value (org-entry-get nil noter-property-pdf-file))
               (when (and property-value (string= property-value wanted-value))
                 (org-narrow-to-subtree)
                 (setq element (org-element-parse-buffer 'greater-element)))
               (setq try-next (and (not element) (org-up-heading-safe))))))
         (unless element
           ;; NOTE(nox): Could not find parent with property, do a global search
           (let ((pos (org-find-property noter-property-pdf-file wanted-value)))
             (when pos
               (goto-char pos)
               (org-narrow-to-subtree)
               (setq element (org-element-parse-buffer 'greater-element)))))
         (car (org-element-contents element)))))))

(defun noter--get-properties-end (ast &optional force-trim)
  (when ast
    (let* ((properties (when ast
                         (org-element-map (org-element-contents ast)
                             'property-drawer 'identity nil t
                             'headline)))
           (has-contents
            (org-element-map (org-element-contents ast) org-element-all-elements
              (lambda (element)
                (unless (memq (org-element-type element) '(section property-drawer))
                  t))
              nil t))
           properties-end)
      (if (not properties)
          (org-element-property :contents-begin ast)
        (setq properties-end (org-element-property :end properties))
        (while (and (or force-trim (not has-contents))
                    (not (eq (char-before properties-end) ?:)))
          (setq properties-end (1- properties-end)))
        properties-end))))

(defun noter--set-read-only (ast)
  (org-with-wide-buffer
   (when ast
     (let* ((level (org-element-property :level ast))
            (begin (org-element-property :begin ast))
            (title-begin (+ 1 level begin))
            (contents-begin (org-element-property :contents-begin ast))
            (properties-end (noter--get-properties-end ast t))
            (inhibit-read-only t)
            (modified (buffer-modified-p)))
       (add-text-properties (1- begin) begin '(read-only t))
       (add-text-properties begin (1- title-begin) '(read-only t front-sticky t))
       (add-text-properties (1- title-begin) title-begin '(read-only t rear-nonsticky t))
       (add-text-properties (1- contents-begin) (1- properties-end) '(read-only t))
       (add-text-properties (1- properties-end) properties-end
                            '(read-only t rear-nonsticky t))
       (set-buffer-modified-p modified)))))

(defun noter--unset-read-only (ast)
  (org-with-wide-buffer
   (when ast
     (let ((begin (org-element-property :begin ast))
           (end (noter--get-properties-end ast t))
           (inhibit-read-only t)
           (modified (buffer-modified-p)))
       (remove-list-of-text-properties (1- begin) end
                                       '(read-only front-sticky rear-nonsticky))
       (set-buffer-modified-p modified)))))

(defun noter--narrow-to-root (ast)
  (when ast
    (let ((old-point (point))
          (begin (org-element-property :begin ast))
          (end (org-element-property :end ast))
          (contents-pos (noter--get-properties-end ast)))
      (goto-char begin)
      (org-show-entry)
      (org-narrow-to-subtree)
      (org-show-children)
      (if (or (< old-point contents-pos)
              (and (not (eq end (point-max))) (>= old-point end)))
          (goto-char contents-pos)
        (goto-char old-point)))))

(defun noter--set-scroll (window &rest ignored)
  (with-selected-window window
    (noter--with-valid-session
     (let* ((level (noter--session-level session))
            (goal (* (1- level) 2))
            (current-scroll (window-hscroll)))
       (when (and (bound-and-true-p org-indent-mode) (< current-scroll goal))
         (scroll-right current-scroll)
         (scroll-left goal t))))))

(defun noter--insert-heading (level)
  (org-insert-heading)
  (let* ((initial-level (org-element-property :level (org-element-at-point)))
         (changer (if (> level initial-level) 'org-do-demote 'org-do-promote))
         (number-of-times (abs (- level initial-level))))
    (dotimes (_ number-of-times)
      (funcall changer))))

(defun noter--get-notes-window ()
  (noter--with-valid-session
   (display-buffer (noter--session-notes-buffer session) nil
                   (noter--session-frame session))))

(defun noter--get-pdf-window ()
  (noter--with-valid-session
   (get-buffer-window (noter--session-pdf-buffer session)
                      (noter--session-frame session))))

(defun noter--current-page ()
  (noter--with-valid-session
   (with-current-buffer (noter--session-pdf-buffer session)
     (image-mode-window-get 'page))))

(defun noter--doc-view-advice (page)
  (when (noter--valid-session noter--session)
    (noter--page-change-handler page)))

(defun noter--selected-note-page (&optional with-start-page)
  (noter--with-valid-session
   (org-with-wide-buffer
    (let ((root-pdf-prop-vale (noter--session-property-text session)))
      (noter--page-property
       (catch 'break
         (let ((try-next t)
               property-value at-root)
           (while try-next
             (setq property-value (org-entry-get nil noter-property-note-page)
                   at-root (string= (org-entry-get nil noter-property-pdf-file)
                                    root-pdf-prop-vale))
             (when (and property-value
                        (or with-start-page (not at-root)))
               (throw 'break property-value))
             (setq try-next (org-up-heading-safe))))))))))

(defun noter--page-property (arg)
  (let* ((property (if (stringp arg) arg
                     (org-element-property (intern (concat ":" noter-property-note-page))
                                           arg)))
         value)
    (when (and (stringp property) (> (length property) 0))
      (setq value (car (read-from-string property)))
      (cond ((and (consp value) (integerp (car value)) (numberp (cdr value)))
             (cons (max 1 (car value)) (max 0 (min 1 (cdr value)))))
            ((integerp value)
             (cons (max 1 value) 0))
            (t nil)))))

(defun noter--get-slice ()
  (let* ((slice (or (image-mode-window-get 'slice) '(0 0 1 1)))
         (slice-top (float (nth 1 slice)))
         (slice-height (float (nth 3 slice))))
    (when (or (> slice-top 1)
              (> slice-height 1))
      (let ((height (cdr (image-size (image-mode-window-get 'image) t))))
        (setq slice-top (/ slice-top height)
              slice-height (/ slice-height height))))
    (cons slice-top slice-height)))

(defun noter--ask-scroll-percentage ()
  (noter--with-valid-session
   (let ((window (noter--get-pdf-window))
         event)
     (when (window-live-p window)
       (with-selected-window window
         (while (not (and (eq 'mouse-1 (car event))
                          (eq window (posn-window (event-start event)))))
           (setq event (read-event "Click where you want the start of the note to be!")))
         (let* ((slice (noter--get-slice))
                (display-height (cdr (image-display-size (image-get-display-property))))
                (current-scroll (window-vscroll))
                (top (+ current-scroll (cdr (posn-col-row (event-start event)))))
                (display-percentage (/ top display-height))
                (percentage (+ (car slice) (* (cdr slice) display-percentage))))
           (max 0 (min 1 percentage))))))))

(defun noter--scroll-to-percentage (percentage)
  (noter--with-valid-session
   (let ((window (noter--get-pdf-window)))
     (when (window-live-p window)
       (with-selected-window window
         (let* ((slice (noter--get-slice))
                (display-height (cdr (image-display-size (image-get-display-property))))
                (current-scroll (window-vscroll))
                (display-percentage (/ (- percentage (car slice)) (cdr slice)))
                goal-scroll diff-scroll)
           (setq display-percentage (min 1 (max 0 display-percentage))
                 goal-scroll (max 0 (floor (* display-percentage display-height)))
                 diff-scroll (- goal-scroll current-scroll))
           (image-scroll-up diff-scroll)))))))

(defun noter--goto-page (page-cons)
  (noter--with-valid-session
   (with-selected-window (noter--get-pdf-window)
     (cond ((eq (noter--session-pdf-mode session) 'pdf-view-mode)
            (pdf-view-goto-page (car page-cons)))
           ((eq (noter--session-pdf-mode session) 'doc-view-mode)
            (doc-view-goto-page (car page-cons)))
           (t (error "This mode is not supported")))
     (noter--scroll-to-percentage (cdr page-cons)))))

(defun noter--focus-notes-region (notes)
  (noter--with-valid-session
   (with-selected-window (noter--get-notes-window)
     (save-excursion
       (dolist (note notes)
         (goto-char (org-element-property :begin note))
         (org-show-context)
         (org-show-siblings)
         (org-show-subtree)))
     (let* ((begin (org-element-property :begin (car notes)))
            (end (org-element-property :end (car (last notes))))
            (window-start (window-start))
            (window-end (window-end nil t))
            (num-lines (count-lines begin end))
            (curr-point (point))
            (target (noter--get-properties-end (car notes))))
       (if (> num-lines (window-height))
           (progn
             (goto-char begin)
             (recenter 0))
         (cond ((< begin window-start)
                (goto-char begin)
                (recenter 0))
               ((> end window-end)
                (goto-char end)
                (recenter -2))))
       (if (or (< curr-point begin)
               (and (not (eq (point-max) end))
                    (>= curr-point end)))
           (goto-char target)
         (goto-char curr-point))
       (org-cycle-hide-drawers 'all)))))

(defun noter--page-change-handler (&optional page-arg)
  (noter--with-valid-session
   (unless noter--inhibit-page-handler
     (let* ((ast (noter--parse-root))
            (contents (when ast (org-element-contents ast)))
            (page (or page-arg (noter--current-page)))
            notes)
       ;; NOTE(nox): This only considers the first group of notes from the same page that
       ;; are together in the document (no notes from other pages in between).
       (org-element-map contents 'headline
         (lambda (headline)
           (let ((property (car (noter--page-property headline))))
             (when property
               (if (not (= page property))
                   notes
                 (push headline notes)
                 nil))))
         nil t org-element-all-elements)
       (when notes
         (setq notes (nreverse notes))
         (noter--focus-notes-region notes))))))

(defun noter--restore-windows (session)
  (when (noter--valid-session session)
    (with-selected-frame (noter--session-frame session)
      (delete-other-windows)
      (let ((pdf-window (selected-window))
            (pdf-buffer (noter--session-pdf-buffer session))
            (notes-window (if (eq noter-split-direction 'horizontal)
                              (split-window-right)
                            (split-window-below)))
            (notes-buffer (noter--session-notes-buffer session)))
        (set-window-buffer pdf-window pdf-buffer)
        (set-window-dedicated-p pdf-window t)
        (set-window-buffer notes-window notes-buffer)
        (with-current-buffer notes-buffer
          (noter--narrow-to-root
           (noter--parse-root
            notes-buffer (noter--session-property-text session))))
        (cons pdf-window notes-window)))))

;; --------------------------------------------------------------------------------
;; NOTE(nox): User commands
(defun noter-set-start-page (arg)
  "Set current page the one to show when starting a new session.
With a prefix ARG, remove start page."
  (interactive "P")
  (noter--with-valid-session
   (let ((inhibit-read-only t)
         (ast (noter--parse-root))
         (page (noter--current-page)))
     (with-current-buffer (noter--session-notes-buffer session)
       (org-with-wide-buffer
        (goto-char (org-element-property :begin ast))
        (if arg
            (org-entry-delete nil noter-property-note-page)
          (org-entry-put nil noter-property-note-page (number-to-string page))))))))

(defun noter-kill-session (&optional session)
  "Kill a Noter session.

When called interactively, if there is no prefix argument and the
buffer has an Noter session, it will kill it; else, it will show
a list of Noter sessions, asking for which to kill.

When called from elisp code, you have to pass in the SESSION you
want to kill."
  (interactive "P")
  (when (and (called-interactively-p 'any) (> (length noter--sessions) 0))
    ;; NOTE(nox): `session' is representing a prefix argument
    (if (and noter--session (not session))
        (setq session noter--session)
      (setq session nil)
      (let (collection default pdf-display-name org-file-name display)
        (dolist (session noter--sessions)
          (setq pdf-display-name (noter--session-display-name session)
                org-file-name (file-name-nondirectory
                               (noter--session-org-file-path session))
                display (concat pdf-display-name " - " org-file-name))
          (when (eq session noter--session) (setq default display))
          (push (cons display session) collection))
        (setq session (cdr (assoc (completing-read "Which session? " collection nil t
                                                   nil nil default)
                                  collection))))))
  (when (and session (memq session noter--sessions))
    (let ((frame (noter--session-frame session))
          (notes-buffer (noter--session-notes-buffer session))
          (pdf-buffer (noter--session-pdf-buffer session)))
      (setq noter--sessions (delq session noter--sessions))
      (when (eq (length noter--sessions) 0)
        (setq delete-frame-functions (delq 'noter--handle-delete-frame
                                           delete-frame-functions))
        (when (featurep 'doc-view)
          (advice-remove  'noter--doc-view-advice 'doc-view-goto-page)))
      (when (frame-live-p frame)
        (delete-frame frame))
      (when (buffer-live-p pdf-buffer)
        (kill-buffer pdf-buffer))
      (when (buffer-live-p notes-buffer)
        (let ((base-buffer (buffer-base-buffer notes-buffer))
              (modified (buffer-modified-p notes-buffer)))
          (with-current-buffer notes-buffer
            (noter--unset-read-only (noter--parse-root))
            (set-buffer-modified-p nil)
            (kill-buffer notes-buffer))
          (with-current-buffer base-buffer
            (set-buffer-modified-p modified)))))))

(defun noter-insert-note (&optional arg scroll-percentage)
  "Insert note associated with the current page.

If:
  - There are no notes for this page yet, this will insert a new
    subheading inside the root heading.
  - There is only one note for this page, it will insert there
  - If there are multiple notes for this page, it will ask you in
    which one to write

When inserting a new note, it will ask you for a title; if you
want the default title, input an empty string.

If you want to force the creation of a separate note, use a
prefix ARG. SCROLL-PERCENTAGE makes the new note associated with
that part of the page (see `noter-insert-localized-note' for
more info)."
  (interactive "P")
  (noter--with-valid-session
   (let* ((ast (noter--parse-root))
          (contents (when ast (org-element-contents ast)))
          (page (noter--current-page))
          (insertion-level (1+ (org-element-property :level ast)))
          (window (noter--get-notes-window))
          notes best-previous-element)
     (setq scroll-percentage (or scroll-percentage 0))
     (org-element-map contents 'headline
       (lambda (headline)
         (let ((property-cons (noter--page-property headline)))
           (if property-cons
               (if (= page (car property-cons))
                   (progn
                     (push headline notes)
                     (when (<= (cdr property-cons) scroll-percentage)
                       (setq best-previous-element headline)))
                 (when (< (car property-cons) page)
                   (setq best-previous-element headline)))
             (unless (noter--page-property best-previous-element)
               (setq best-previous-element headline)))))
       nil nil org-element-all-elements)
     (setq notes (nreverse notes))
     (with-selected-window window
       ;; NOTE(nox): Need to be careful changing the next part, it is a bit complicated to
       ;; get it right...
       (if (and notes (not arg))
           (let ((point (point))
                 default note has-contents num-blank collection match)
             (if (eq (length notes) 1)
                 (setq note (car notes))
               (dolist (iterator notes (setq collection (nreverse collection)))
                 (let ((display (org-element-property :raw-value iterator)))
                   (when (or (not default)
                             (>= point (org-element-property :begin iterator)))
                     (setq default display))
                   (push (cons display iterator) collection)))
               (setq note
                     (cdr
                      (assoc (completing-read "Insert in which note? " collection nil t nil nil
                                              default)
                             collection))))
             (when note
               (setq has-contents
                     (org-element-map (org-element-contents note) org-element-all-elements
                       (lambda (element)
                         (unless (memq (org-element-type element) '(section property-drawer))
                           t))
                       nil t)
                     num-blank (org-element-property :post-blank note))
               (goto-char (org-element-property :end note))
               ;; NOTE(nox): Org doesn't count `:post-blank' when at the end of the buffer
               (when (org-next-line-empty-p) ;; NOTE(nox): This is only true at the end, I think
                 (goto-char (point-max))
                 (save-excursion
                   (beginning-of-line)
                   (while (looking-at "[[:space:]]*$")
                     (setq num-blank (1+ num-blank))
                     (beginning-of-line 0))))
               (cond (has-contents
                      (while (< num-blank 2)
                        (insert "\n")
                        (setq num-blank (1+ num-blank))))
                     (t (when (eq num-blank 0) (insert "\n"))))
               (when (org-at-heading-p)
                 (forward-line -1))))
         (let ((title (read-string "Title: ")))
           (when (zerop (length title))
             (setq title (replace-regexp-in-string
                          (regexp-quote "$p$") (number-to-string page)
                          noter-default-heading-title)))
           (if best-previous-element
               (progn
                 (goto-char (org-element-property :end best-previous-element))
                 (noter--insert-heading insertion-level))
             (goto-char
              (org-element-map contents 'section
                (lambda (section)
                  (org-element-property :end section))
                nil t org-element-all-elements))
             ;; NOTE(nox): This is needed to insert in the right place...
             (outline-show-entry)
             (noter--insert-heading insertion-level))
           (insert title)
           (end-of-line)
           (if (and (not (eobp)) (org-next-line-empty-p))
               (forward-line)
             (insert "\n"))
           (org-entry-put nil noter-property-note-page
                          (format "%s" (if (zerop scroll-percentage)
                                           page
                                         (cons page scroll-percentage))))))
       (org-show-context)
       (org-show-siblings)
       (org-show-subtree)
       (org-cycle-hide-drawers 'all))
     (select-window window))))

(defun noter-insert-localized-note ()
  "Insert note associated with part of a page.
This will ask you to click where you want Noter to scroll to when
you sync the PDF to this note. You should click on the top of
that part. Will always create a new note.

See `noter-insert-note' docstring for more."
  (interactive)
  (noter-insert-note t (noter--ask-scroll-percentage)))

(defun noter-sync-previous-page-note ()
  "Go to the page of the previous note.

This is in relation to the current note (where the point is now)."
  (interactive)
  (noter--with-valid-session
   (with-selected-window (noter--get-notes-window)
     (let ((noter--inhibit-page-handler t)
           (contents (org-element-contents (noter--parse-root)))
           previous)
       (org-element-map contents 'headline
         (lambda (headline)
           (let ((begin (org-element-property :begin headline))
                 (end (org-element-property :end headline)))
             (if (< (point) begin)
                 t
               (if (or (<  (point) end)
                       (eq (point-max) end))
                   t
                 (setq previous (if (noter--page-property headline) headline previous))
                 nil))))
         nil t org-element-all-elements)
       (if previous
           (progn
             (noter--goto-page (noter--page-property previous))
             (noter--focus-notes-region (list previous)))
         (error "There is no previous note")))
     (select-window (noter--get-pdf-window)))))

(defun noter-sync-page-note ()
  "Go to the page of the selected note (where the point is now)."
  (interactive)
  (noter--with-valid-session
   (with-selected-window (noter--get-notes-window)
     (let ((page (noter--selected-note-page)))
       (if page
           (noter--goto-page page)
         (error "No note selected"))))
   (select-window (noter--get-pdf-window))))

(defun noter-sync-next-page-note ()
  "Go to the page of the next note.

This is in relation to the current note (where the point is now)."
  (interactive)
  (noter--with-valid-session
   (let ((noter--inhibit-page-handler t)
         (contents (org-element-contents (noter--parse-root)))
         (point (with-selected-window (noter--get-notes-window) (point)))
         (property-name (intern (concat ":" noter-property-note-page)))
         next)
     (org-element-map contents 'headline
       (lambda (headline)
         (when (and
                (noter--page-property headline)
                (< point (org-element-property :begin headline)))
           (setq next headline)))
       nil t org-element-all-elements)
     (if next
         (progn
           (noter--goto-page (noter--page-property next))
           (noter--focus-notes-region (list next)))
       (error "There is no next note")))
   (select-window (noter--get-pdf-window))))

(define-minor-mode noter-pdf-mode
  "Minor mode for the Noter PDF buffer."
  :keymap `((,(kbd   "i") . noter-insert-note)
            (,(kbd "M-i") . noter-insert-localized-note)
            (,(kbd   "q") . noter-kill-session)
            (,(kbd "M-p") . noter-sync-previous-page-note)
            (,(kbd "M-.") . noter-sync-page-note)
            (,(kbd "M-n") . noter-sync-next-page-note)))

(define-minor-mode noter-notes-mode
  "Minor mode for the Noter notes buffer."
  :keymap `((,(kbd "M-p") . noter-sync-previous-page-note)
            (,(kbd "M-.") . noter-sync-page-note)
            (,(kbd "M-n") . noter-sync-next-page-note)))

;;;###autoload
(defun noter-other-window-config (arg)
  "Start Noter with alternative window configuration.
See `noter' docstring for more info."
  (interactive "P")
  (let ((noter-split-direction (if (eq noter-split-direction 'horizontal)
                                        'vertical
                                      'horizontal)))
    (noter arg)))

;;;###autoload
(defun noter (arg)
  "Start Noter.

This will open a session for taking your notes, with indirect
buffers to the PDF and the notes side by side. Your current
window configuration won't be changed, because this opens in a
new frame.

You only need to run this command inside a heading (which will
hold the notes for this PDF). If no PDF path property is found,
this command will ask you for the target file.

With a prefix universal argument ARG, only check for the property
in the current heading, don't inherit from parents.

With a prefix number ARG, open the PDF without a Noter session if
ARG >= 0, or open the folder containing the PDF when ARG < 0."
  (interactive "P")
  (when (eq major-mode 'org-mode)
    (when (org-before-first-heading-p)
      (error "Noter must be issued inside a heading"))
    (let ((org-file-path (buffer-file-name))
          (pdf-property (org-entry-get nil noter-property-pdf-file
                                       (not (equal arg '(4)))))
          pdf-file-path ast session)
      (when (stringp pdf-property) (setq pdf-file-path (expand-file-name pdf-property)))
      (unless (and pdf-file-path
                   (not (file-directory-p pdf-file-path))
                   (file-readable-p pdf-file-path))
        (setq pdf-file-path (expand-file-name
                             (read-file-name
                              "Invalid or no PDF property found. Please specify a PDF path: "
                              nil nil t)))
        (when (or (file-directory-p pdf-file-path) (not (file-readable-p pdf-file-path)))
          (error "Invalid file path"))
        (setq pdf-property (if (y-or-n-p "Do you want a relative file name? ")
                               (file-relative-name pdf-file-path)
                             pdf-file-path))
        (org-entry-put nil noter-property-pdf-file pdf-property))
      (setq ast (noter--parse-root (current-buffer) pdf-property))
      (when (catch 'should-continue
              (when (or (numberp arg) (eq arg '-))
                (let ((number (prefix-numeric-value arg)))
                  (if (>= number 0)
                      (find-file pdf-file-path)
                    (find-file (file-name-directory pdf-file-path))))
                (throw 'should-continue nil))
              (dolist (session noter--sessions)
                (when (noter--valid-session session)
                  (when (and (string= (noter--session-pdf-file-path session)
                                      pdf-file-path)
                             (string= (noter--session-org-file-path session)
                                      org-file-path))
                    (let ((test-ast (with-current-buffer
                                        (noter--session-notes-buffer session)
                                      (noter--parse-root))))
                      (when (eq (org-element-property :begin ast)
                                (org-element-property :begin test-ast))
                        ;; NOTE(nox): This is an existing session!
                        (noter--restore-windows session)
                        (select-frame-set-input-focus (noter--session-frame session))
                        (throw 'should-continue nil))))))
              t)
        (setq
         session
         (let* ((display-name (org-element-property :raw-value ast))
                (notes-buffer-name
                 (generate-new-buffer-name (format "Noter - Notes of %s" display-name)))
                (pdf-buffer-name
                 (generate-new-buffer-name (format "Noter - %s" display-name)))
                (orig-pdf-buffer (find-file-noselect pdf-file-path))
                (frame (make-frame `((name . ,(format "Emacs - Noter %s" display-name))
                                     (fullscreen . maximized))))
                (notes-buffer (make-indirect-buffer (current-buffer) notes-buffer-name t))
                (pdf-buffer (make-indirect-buffer orig-pdf-buffer pdf-buffer-name))
                (pdf-mode (buffer-local-value 'major-mode orig-pdf-buffer))
                (level (org-element-property :level ast)))
           (make-noter--session :frame frame :pdf-mode pdf-mode :display-name display-name
                                     :property-text pdf-property :org-file-path org-file-path
                                     :pdf-file-path pdf-file-path :notes-buffer notes-buffer
                                     :pdf-buffer pdf-buffer :level level)))
        (add-hook 'delete-frame-functions 'noter--handle-delete-frame)
        (push session noter--sessions)
        (let ((windows (noter--restore-windows session)))
          (with-selected-window (car windows)
            (setq buffer-file-name pdf-file-path)
            (cond ((eq (noter--session-pdf-mode session) 'pdf-view-mode)
                   (pdf-view-mode)
                   (add-hook 'pdf-view-after-change-page-hook
                             'noter--page-change-handler nil t))
                  ((eq (noter--session-pdf-mode session) 'doc-view-mode)
                   (doc-view-mode)
                   (advice-add 'doc-view-goto-page :after 'noter--doc-view-advice))
                  (t (error "This PDF handler is not supported :/")))
            (noter-pdf-mode 1)
            (setq noter--session session)
            (kill-local-variable 'kill-buffer-hook)
            (add-hook 'kill-buffer-hook 'noter--handle-kill-buffer nil t))
          (with-selected-window (cdr windows)
            (noter-notes-mode 1)
            (setq buffer-file-name org-file-path
                  noter--session session
                  fringe-indicator-alist '((truncation . nil)))
            (add-hook 'kill-buffer-hook 'noter--handle-kill-buffer nil t)
            (add-hook 'window-scroll-functions 'noter--set-scroll nil t)
            (noter--set-scroll (selected-window))
            (let ((ast (noter--parse-root))
                  (current-page (noter--selected-note-page t)))
              (noter--set-read-only ast)
              (if current-page
                  (noter--goto-page current-page)
                (noter--page-change-handler 1))))))))
  (when (and (not (eq major-mode 'org-mode)) (noter--valid-session noter--session))
    (noter--restore-windows noter--session)
    (select-frame-set-input-focus (noter--session-frame noter--session))))

(provide 'noter)

;;; noter.el ends here
