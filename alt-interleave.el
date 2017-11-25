;;; alt-interleave.el --- Annotate PDFs in an interleaved fashion!          -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Gonçalo Santos

;; Author: Gonçalo Santos (aka. weirdNox@GitHub)
;; Homepage: https://github.com/weirdNox/interleave
;; Keywords: lisp pdf interleave annotate
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

;; This is a rewrite from scratch of Sebastian Christ's amazing Interleave package, which
;; helps you annotate PDFs in Org mode.

;; The idea is that, like an interleaved textbook, this opens the PDF and the notes buffer
;; side by side and, as you scroll through your PDF, it will present you the notes you
;; have for each page. Taking a note is as simple as pressing i and writing away!

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
(defgroup interleave nil
  "Annotate PDFs in an interleaved fashion!"
  :group 'convenience
  :version "25.3.1")

(defcustom interleave-property-pdf-file "INTERLEAVE_PDF"
  "Name of the property which specifies the PDF file."
  :group 'interleave
  :type 'string)

(defcustom interleave-property-note-page "INTERLEAVE_NOTE_PAGE"
  "Name of the property which specifies the page of the current note."
  :group 'interleave
  :type 'string)

(defcustom interleave-split-direction 'horizontal
  "Whether the interleave frame should be split horizontally or vertically."
  :group 'interleave
  :type '(choice (const :tag "Horizontal" horizontal)
                 (const :tag "Vertical" vertical)))

(defcustom interleave-default-heading-title "Notes for page $p$"
  "The title of the headings created by `interleave-insert-note'.
$p$ is replaced by the number of the page you are in at the
moment."
  :group 'interleave
  :type 'string)

;; --------------------------------------------------------------------------------
;; NOTE(nox): Private variables
(cl-defstruct interleave--session frame pdf-mode display-name property-text
              org-file-path pdf-file-path notes-buffer
              pdf-buffer level)

(defvar interleave--sessions nil
  "List of Interleave sessions.")

(defvar-local interleave--session nil
  "Session associated with the current buffer.")

;; --------------------------------------------------------------------------------
;; NOTE(nox): Utility functions
(defun interleave--valid-session (session)
  (if (and session
           (frame-live-p (interleave--session-frame session))
           (buffer-live-p (interleave--session-pdf-buffer session))
           (buffer-live-p (interleave--session-notes-buffer session)))
      t
    (interleave-kill-session session)
    nil))

(defmacro interleave--with-valid-session (&rest body)
  `(let ((session interleave--session))
     (when (interleave--valid-session session)
       (progn ,@body))))

(defun interleave--handle-kill-buffer ()
  (interleave--with-valid-session
   (let ((buffer (current-buffer))
         (notes-buffer (interleave--session-notes-buffer session))
         (pdf-buffer (interleave--session-pdf-buffer session)))
     ;; NOTE(nox): This needs to be checked in order to prevent session killing because of
     ;; temporary buffers with the same local variables
     (when (or (eq buffer notes-buffer)
               (eq buffer pdf-buffer))
       (interleave-kill-session session)))))

(defun interleave--handle-delete-frame (frame)
  (dolist (session interleave--sessions)
    (when (eq (interleave--session-frame session) frame)
      (interleave-kill-session session))))

(defun interleave--parse-root (&optional buffer property-pdf-path)
  (let* ((session interleave--session)
         (use-args (and (stringp property-pdf-path)
                        (buffer-live-p buffer)
                        (with-current-buffer buffer (eq major-mode 'org-mode))))
         (notes-buffer (if use-args
                           buffer
                         (when session (interleave--session-notes-buffer session))))
         (wanted-value (if use-args
                           property-pdf-path
                         (when session (interleave--session-property-text session))))
         element)
    (when (buffer-live-p notes-buffer)
      (with-current-buffer notes-buffer
        (org-with-wide-buffer
         (unless (org-before-first-heading-p)
           ;; NOTE(nox): Start by trying to find a parent heading with the specified
           ;; property
           (let ((try-next t) property-value)
             (while try-next
               (setq property-value (org-entry-get nil interleave-property-pdf-file))
               (when (and property-value (string= property-value wanted-value))
                 (org-narrow-to-subtree)
                 (setq element (org-element-parse-buffer 'greater-element)))
               (setq try-next (and (not element) (org-up-heading-safe))))))
         (unless element
           ;; NOTE(nox): Could not find parent with property, do a global search
           (let ((pos (org-find-property interleave-property-pdf-file wanted-value)))
             (when pos
               (goto-char pos)
               (org-narrow-to-subtree)
               (setq element (org-element-parse-buffer 'greater-element)))))
         (car (org-element-contents element)))))))

(defun interleave--get-properties-end (ast &optional force-trim)
  (when ast
    (let* ((properties (org-element-map ast 'property-drawer 'identity nil t))
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

(defun interleave--set-read-only (ast)
  (org-with-wide-buffer
   (when ast
     (let* ((level (org-element-property :level ast))
            (begin (org-element-property :begin ast))
            (title-begin (+ 1 level begin))
            (contents-begin (org-element-property :contents-begin ast))
            (properties-end (interleave--get-properties-end ast t))
            (inhibit-read-only t)
            (modified (buffer-modified-p)))
       (add-text-properties (1- begin) (1- title-begin) '(read-only t))
       (add-text-properties (1- title-begin) title-begin '(read-only t rear-nonsticky t))
       (add-text-properties (1- contents-begin) (1- properties-end) '(read-only t))
       (add-text-properties (1- properties-end) properties-end
                            '(read-only t rear-nonsticky t))
       (set-buffer-modified-p modified)))))

(defun interleave--unset-read-only (ast)
  (org-with-wide-buffer
   (when ast
     (let ((begin (org-element-property :begin ast))
           (end (interleave--get-properties-end ast t))
           (inhibit-read-only t)
           (modified (buffer-modified-p)))
       (remove-list-of-text-properties (1- begin) end
                                       '(read-only front-sticky rear-nonsticky))
       (set-buffer-modified-p modified)))))

(defun interleave--narrow-to-root (ast)
  (when ast
    (let ((old-point (point))
          (begin (org-element-property :begin ast))
          (end (org-element-property :end ast))
          (contents-pos (interleave--get-properties-end ast)))
      (goto-char begin)
      (org-show-entry)
      (org-narrow-to-subtree)
      (org-show-children)
      (if (or (< old-point begin) (>= old-point end))
          (goto-char contents-pos)
        (goto-char old-point)))))

(defun interleave--set-scroll (window &rest ignored)
  (with-selected-window window
    (interleave--with-valid-session
     (let* ((level (interleave--session-level session))
            (goal (* (1- level) 2))
            (current-scroll (window-hscroll)))
       (when (and org-indent-mode (< current-scroll goal))
         (scroll-right current-scroll)
         (scroll-left goal t))))))

(defun interleave--insert-heading (level)
  (org-insert-heading)
  (let* ((initial-level (org-element-property :level (org-element-at-point)))
         (changer (if (> level initial-level) 'org-do-demote 'org-do-promote))
         (number-of-times (abs (- level initial-level))))
    (dotimes (_ number-of-times)
      (funcall changer))))

(defun interleave--get-notes-window ()
  (interleave--with-valid-session
   (display-buffer (interleave--session-notes-buffer session) nil
                   (interleave--session-frame session))))

(defun interleave--get-pdf-window ()
  (interleave--with-valid-session
   (get-buffer-window (interleave--session-pdf-buffer session)
                      (interleave--session-frame session))))

;; TODO(nox): Convert all calls to page number
(defun interleave--goto-page (page)
  (interleave--with-valid-session
   (with-selected-window (interleave--get-pdf-window)
     (cond ((eq major-mode 'pdf-view-mode)
            (pdf-view-goto-page page))
           ((eq major-mode 'doc-view-mode)
            (doc-view-goto-page page))))))

(defun interleave--current-page ()
  (interleave--with-valid-session
   (with-current-buffer (interleave--session-pdf-buffer session)
     (image-mode-window-get 'page))))

(defun interleave--doc-view-advice (page)
  (when (interleave--valid-session interleave--session)
    (interleave--page-change-handler page)))

(defun interleave--ask-scroll-percentage ()
  (interleave--with-valid-session
   (let ((window (interleave--get-pdf-window))
         event)
     (when (window-live-p window)
       (with-selected-window window
         (while (not (eq (event-basic-type event) 'mouse-1))
           (setq event (read-event "Click where you want the start of the note to be!")))
         (let* ((slice (or (image-mode-window-get 'slice) '(0 0 1 1)))
                (slice-top (nth 1 slice))
                (slice-heigth (nth 3 slice))
                (display-height (cdr (image-display-size (image-get-display-property))))
                (current-scroll (image-mode-window-get 'vscroll))
                (top (+ current-scroll (cdr (posn-col-row (event-start event)))))
                (display-percentage (/ top display-height))
                (percentage (+ slice-top (* slice-heigth display-percentage))))
           (max 0 (min 1 percentage))))))))

(defun interleave--scroll-to-percentage (percentage)
  (interleave--with-valid-session
   (let ((window (interleave--get-pdf-window)))
     (when (window-live-p window)
       (with-selected-window window
         (let* ((slice (or (image-mode-window-get 'slice) '(0 0 1 1)))
                (slice-top (nth 1 slice))
                (slice-heigth (nth 3 slice))
                (display-height (cdr (image-display-size (image-get-display-property))))
                (current-scroll (image-mode-window-get 'vscroll))
                (display-percentage (/ (- percentage slice-top) slice-heigth))
                goal-scroll diff-scroll)
           (setq display-percentage (min 1 (max 0 display-percentage))
                 goal-scroll (max 0 (floor (* display-percentage display-height)))
                 diff-scroll (- goal-scroll current-scroll))
           (image-scroll-up diff-scroll)))))))

(defun interleave--page-property (element)
  (let ((value (car (read-from-string
                     (org-element-property
                      (intern (concat ":" interleave-property-note-page))
                      element)))))
    (cond ((and (consp value) (integerp (car value)) (numberp (cdr value)))
           (cons (max 1 (car value)) (max 0 (min 1 (cdr value)))))
          ((integerp value)
           (cons (max 1 value) 0))
          (t '(1 . 0)))))

(defun interleave--page-change-handler (&optional page-arg)
  (interleave--with-valid-session
   (let* ((page (or page-arg (interleave--current-page)))
          (ast (interleave--parse-root))
          (contents (when ast (org-element-contents ast)))
          notes)
     ;; NOTE(nox): This only considers the first group of notes from the same page that
     ;; are together in the document (no notes from other pages in between).
     (org-element-map contents 'headline
       (lambda (headline)
         (if (not (= page (car (interleave--page-property headline))))
             (when notes t)
           (push headline notes)
           nil))
       nil t org-element-all-elements)
     (when notes
       (setq notes (nreverse notes))
       (with-selected-window (interleave--get-notes-window)
         (let ((pos (point))
               (target-pos (interleave--get-properties-end (car notes)))
               (begin (org-element-property :begin (car notes)))
               (end (org-element-property :end (car (last notes)))))
           (save-excursion
             (goto-char target-pos)
             (recenter 10)
             (unless (pos-visible-in-window-p end)
               (goto-char end)
               (recenter -1)))
           (when (or (< pos begin)
                     (and (not (eq (point-max) end))
                          (>= pos end)))
             (goto-char target-pos))
           (org-show-context)
           (org-show-siblings)
           (org-show-subtree)
           (org-cycle-hide-drawers 'all)))))))

(defun interleave--restore-windows (session)
  (when (interleave--valid-session session)
    (with-selected-frame (interleave--session-frame session)
      (delete-other-windows)
      (let ((pdf-window (selected-window))
            (notes-window (if (eq interleave-split-direction 'horizontal)
                              (split-window-right)
                            (split-window-below))))
        (set-window-buffer pdf-window (interleave--session-pdf-buffer session))
        (set-window-dedicated-p pdf-window t)
        (set-window-buffer notes-window (interleave--session-notes-buffer session))
        (cons pdf-window notes-window)))))

;; --------------------------------------------------------------------------------
;; NOTE(nox): User commands
(defun interleave-kill-session (&optional session)
  "Kill a interleave session.

When called interactively, if there is no prefix argument and the
buffer has an interleave session, it will kill it; if the current
buffer has no session defined or it is called with a prefix
argument, it will show a list of interleave sessions, asking for
which to kill.

When called from elisp code, you have to pass in the SESSION you
want to kill."
  (interactive "P")
  (when (and (called-interactively-p 'any) (> (length interleave--sessions) 0))
    ;; NOTE(nox): `session' is representing a prefix argument
    (if (and interleave--session (not session))
        (setq session interleave--session)
      (setq session nil)
      (let (collection default pdf-display-name org-file-name display)
        (dolist (session interleave--sessions)
          (setq pdf-display-name (interleave--session-display-name session)
                org-file-name (file-name-nondirectory
                               (interleave--session-org-file-path session))
                display (concat pdf-display-name " with notes from " org-file-name))
          (when (eq session interleave--session) (setq default display))
          (push (cons display session) collection))
        (setq session (cdr (assoc (completing-read "Which session? " collection nil t
                                                   nil nil default)
                                  collection))))))
  (when (and session (memq session interleave--sessions))
    (let ((frame (interleave--session-frame session))
          (notes-buffer (interleave--session-notes-buffer session))
          (pdf-buffer (interleave--session-pdf-buffer session)))
      (with-current-buffer notes-buffer
        (interleave--unset-read-only (interleave--parse-root)))
      (setq interleave--sessions (delq session interleave--sessions))
      (when (eq (length interleave--sessions) 0)
        (setq delete-frame-functions (delq 'interleave--handle-delete-frame
                                           delete-frame-functions))
        (when (featurep 'doc-view)
          (advice-remove  'interleave--doc-view-advice 'doc-view-goto-page)))
      (when (frame-live-p frame)
        (delete-frame frame))
      (when (buffer-live-p pdf-buffer)
        (kill-buffer pdf-buffer))
      (when (buffer-live-p notes-buffer)
        (kill-buffer notes-buffer)))))

(defun interleave-insert-note (&optional arg scroll-percentage)
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
that part of the page (see `interleave-insert-localized-note' for
more info)."
  (interactive "P")
  (interleave--with-valid-session
   (let* ((ast (interleave--parse-root))
          (contents (when ast (org-element-contents ast)))
          (page (interleave--current-page))
          (property-symbol (intern (concat ":" interleave-property-note-page)))
          (insertion-level (1+ (org-element-property :level ast)))
          (window (interleave--get-notes-window))
          notes best-previous-element)
     (org-element-map contents 'headline
       (lambda (headline)
         (let ((property-cons (interleave--page-property headline)))
           (if (= page (car property-cons))
               (progn
                 (push headline notes)
                 (when (< (cdr property-cons) scroll-percentage)
                   (setq best-previous-element headline)))
             (when (< (car property-cons) page)
               (setq best-previous-element headline)))))
       nil nil org-element-all-elements)
     (setq notes (nreverse notes))
     (with-selected-window window
       ;; NOTE(nox): Need to be careful changing the next part, it is a bit complicated to
       ;; get it right...
       (if (and notes (not arg))
           (let (note has-contents num-blank collection match)
             (if (eq (length notes) 1)
                 (setq note (car notes))
               (dolist (iterator notes (setq collection (nreverse collection)))
                 (push (cons (org-element-property :raw-value iterator)
                             iterator)
                       collection))
               (setq note
                     (cdr
                      (assoc (completing-read "Insert in which note? " collection nil t)
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
                          interleave-default-heading-title)))
           (if best-previous-element
               (progn
                 (goto-char (org-element-property :end best-previous-element))
                 (interleave--insert-heading insertion-level))
             (goto-char (interleave--get-properties-end ast t))
             (interleave--insert-heading insertion-level))
           (insert title)
           (if (and (not (eobp)) (org-next-line-empty-p))
               (forward-line)
             (insert "\n"))
           (org-entry-put nil interleave-property-note-page
                          (format "%s" (cons page scroll-percentage)))))
       (org-show-context)
       (org-show-siblings)
       (org-show-subtree)
       (org-cycle-hide-drawers 'all))
     (select-window window))))

(defun interleave-insert-localized-note ()
  "Insert note associated with part of a page.
This will ask you to click where you want Interleave to scroll to
when you sync the PDF to this note. You should click on the top
of that part. Will always create a new note.

See `interleave-insert-note' docstring for more."
  (interactive)
  (interleave-insert-note t (interleave--ask-scroll-percentage)))

;; TODO(nox): FINISH THIS
(defun interleave-sync-previous-page-note ()
  "Go to the page of the previous note.

This is in relation to the current note (where the point is now)."
  (interactive)
  (interleave--with-valid-session
   (let ((contents (org-element-contents (interleave--parse-root)))
         (point-info
          (with-selected-window (interleave--get-notes-window)
            (cons (point) (point-max))))
         (property-name (intern (concat ":" interleave-property-note-page)))
         (current-page (interleave--current-page))
         previous-page-string page-string)
     (org-element-map contents 'headline
       (lambda (headline)
         (let ((begin (org-element-property :begin headline))
               (end (org-element-property :end headline)))
           (if (< (car point-info) begin)
               t
             (if (and (>= (car point-info) begin)
                      (or (<  (car point-info) end)
                          (eq (cdr point-info) end)))
                 (setq page-string previous-page-string)
               (setq previous-page-string
                     (or (org-element-property property-name headline)
                         previous-page-string))
               nil))))
       nil t 'headline)
     (if page-string
         (if (eq current-page (string-to-number previous-page-string))
             (interleave--page-change-handler current-page)
           (interleave--goto-page previous-page-string))
       (error "There is no previous note")))
   (select-window (interleave--get-pdf-window))))

(defun interleave-sync-page-note ()
  "Go to the page of the selected note (where the point is now)."
  (interactive)
  (interleave--with-valid-session
   (with-selected-window (interleave--get-notes-window)
     (let ((page-string (org-entry-get nil interleave-property-note-page t)))
       (if page-string
           (interleave--goto-page page-string)
         (error "No note selected"))))
   (select-window (interleave--get-pdf-window))))

(defun interleave-sync-next-page-note ()
  "Go to the page of the next note.

This is in relation to the current note (where the point is now)."
  (interactive)
  (interleave--with-valid-session
   (let ((contents (org-element-contents (interleave--parse-root)))
         (point (with-selected-window (interleave--get-notes-window) (point)))
         (property-name (intern (concat ":" interleave-property-note-page)))
         (current-page (interleave--current-page))
         page-string)
     (org-element-map contents 'headline
       (lambda (headline)
         (when (< point (org-element-property :begin headline))
           (setq start-searching t))
         t)
       nil t)
     (org-element-map contents 'headline
       (lambda (headline)
         (when (< point (org-element-property :begin headline))
           (setq page-string (org-element-property property-name headline))))
       nil t 'headline)
     (if page-string
         (if (eq current-page (string-to-number page-string))
             (interleave--page-change-handler current-page)
           (interleave--goto-page page-string))
       (error "There is no next note")))
   (select-window (interleave--get-pdf-window))))

(define-minor-mode interleave-pdf-mode
  "Minor mode for the Interleave PDF buffer."
  :keymap `((,(kbd   "i") . interleave-insert-note)
            (,(kbd   "q") . interleave-kill-session)
            (,(kbd "M-p") . interleave-sync-previous-page-note)
            (,(kbd "M-.") . interleave-sync-page-note)
            (,(kbd "M-n") . interleave-sync-next-page-note)))

(define-minor-mode interleave-notes-mode
  "Minor mode for the Interleave notes buffer."
  :keymap `((,(kbd "M-p") . interleave-sync-previous-page-note)
            (,(kbd "M-.") . interleave-sync-page-note)
            (,(kbd "M-n") . interleave-sync-next-page-note)))

;;;###autoload
(defun interleave (arg)
  "Start Interleave.

This will open a session for interleaving your notes, with
indirect buffers to the PDF and the notes side by side. Your
current window configuration won't be changed, because this opens
in a new frame.

You only need to run this command inside a heading (which will
hold the notes for this PDF). If no PDF path property is found,
this command will ask you for the target file.

With a prefix universal argument ARG, only check for the property
in the current heading, don't inherit from parents.

With a prefix number ARG, open the PDF without interleaving if
ARG >= 0, or open the folder containing the PDF when ARG < 0."
  (interactive "P")
  (when (eq major-mode 'org-mode)
    (when (org-before-first-heading-p)
      (error "Interleave must be issued inside a heading"))
    (let ((org-file-path (buffer-file-name))
          (pdf-property (org-entry-get nil interleave-property-pdf-file
                                       (not (eq arg '(4)))))
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
        (org-entry-put nil interleave-property-pdf-file pdf-property))
      (setq ast (interleave--parse-root (current-buffer) pdf-property))
      (when (catch 'should-continue
              (when (or (numberp arg) (eq arg '-))
                (let ((number (prefix-numeric-value arg)))
                  (if (>= number 0)
                      (find-file pdf-file-path)
                    (find-file (file-name-directory pdf-file-path))))
                (throw 'should-continue nil))
              (dolist (session interleave--sessions)
                (when (interleave--valid-session session)
                  (when (and (string= (interleave--session-pdf-file-path session)
                                      pdf-file-path)
                             (string= (interleave--session-org-file-path session)
                                      org-file-path))
                    (let ((test-ast (with-current-buffer
                                        (interleave--session-notes-buffer session)
                                      (interleave--parse-root))))
                      (when (eq (org-element-property :begin ast)
                                (org-element-property :begin test-ast))
                        ;; NOTE(nox): This is an existing session!
                        (interleave--restore-windows session)
                        (select-frame-set-input-focus (interleave--session-frame session))
                        (throw 'should-continue nil))))))
              t)
        (setq
         session
         (let* ((display-name (org-element-property :raw-value ast))
                (notes-buffer-name
                 (generate-new-buffer-name (format "Interleave - Notes of %s" display-name)))
                (pdf-buffer-name
                 (generate-new-buffer-name (format "Interleave - %s" display-name)))
                (orig-pdf-buffer (find-file-noselect pdf-file-path))
                (frame (make-frame `((name . ,(format "Emacs - Interleave %s" display-name))
                                     (fullscreen . maximized))))
                (notes-buffer (make-indirect-buffer (current-buffer) notes-buffer-name t))
                (pdf-buffer (make-indirect-buffer orig-pdf-buffer pdf-buffer-name))
                (pdf-mode (with-current-buffer orig-pdf-buffer major-mode))
                (level (org-element-property :level ast)))
           (make-interleave--session :frame frame :pdf-mode pdf-mode :display-name display-name
                                     :property-text pdf-property :org-file-path org-file-path
                                     :pdf-file-path pdf-file-path :notes-buffer notes-buffer
                                     :pdf-buffer pdf-buffer :level level)))
        (let ((windows (interleave--restore-windows session)))
          (with-selected-window (car windows)
            (setq buffer-file-name pdf-file-path)
            (cond ((eq (interleave--session-pdf-mode session) 'pdf-view-mode)
                   (pdf-view-mode)
                   (add-hook 'pdf-view-after-change-page-hook
                             'interleave--page-change-handler nil t))
                  ((eq (interleave--session-pdf-mode session) 'doc-view-mode)
                   (doc-view-mode)
                   (advice-add 'doc-view-goto-page :after 'interleave--doc-view-advice))
                  (t (error "This PDF handler is not supported :/")))
            (kill-local-variable 'kill-buffer-hook)
            (setq interleave--session session)
            (add-hook 'kill-buffer-hook 'interleave--handle-kill-buffer nil t)
            (interleave-pdf-mode 1))
          (with-selected-window (cdr windows)
            (setq interleave--session session
                  fringe-indicator-alist '((truncation . nil)))
            (add-hook 'kill-buffer-hook 'interleave--handle-kill-buffer nil t)
            (add-hook 'window-scroll-functions 'interleave--set-scroll nil t)
            (interleave--set-scroll (selected-window))
            (let ((ast (interleave--parse-root)))
              (interleave--set-read-only ast)
              (interleave--narrow-to-root ast))
            (interleave-notes-mode 1)))
        (add-hook 'delete-frame-functions 'interleave--handle-delete-frame)
        (push session interleave--sessions)
        (let ((current-note-page (org-entry-get nil interleave-property-note-page t)))
          (with-current-buffer (interleave--session-pdf-buffer session)
            (if current-note-page
                (interleave--goto-page current-note-page)
              (interleave--page-change-handler 1)))))))
  (when (and (not (eq major-mode 'org-mode)) (interleave--valid-session interleave--session))
    (interleave--restore-windows interleave--session)
    (select-frame-set-input-focus (interleave--session-frame interleave--session))))

(provide 'alt-interleave)

;;; alt-interleave.el ends here
