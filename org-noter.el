;;; org-noter.el --- A synchronized, Org-mode, document annotator       -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Gonçalo Santos

;; Author: Gonçalo Santos (aka. weirdNox@GitHub)
;; Homepage: https://github.com/weirdNox/org-noter
;; Keywords: lisp pdf interleave annotate external sync notes documents org-mode
;; Package-Requires: ((emacs "24.4") (cl-lib "0.6") (org "9.0"))
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

(declare-function pdf-view-mode "ext:pdf-view")
(declare-function pdf-view-goto-page "ext:pdf-view")
(declare-function doc-view-goto-page "doc-view")
(declare-function image-mode-window-get "image-mode")

;; --------------------------------------------------------------------------------
;; NOTE(nox): User variables
(defgroup org-noter nil
  "A synchronized, external annotator"
  :group 'convenience
  :version "25.3.1")

(defcustom org-noter-property-doc-file "NOTER_DOCUMENT"
  "Name of the property that specifies the document."
  :group 'org-noter
  :type 'string)

(defcustom org-noter-property-note-page "NOTER_PAGE"
  "Name of the property that specifies the page of the current note."
  :group 'org-noter
  :type 'string)

(defcustom org-noter-default-heading-title "Notes for page $p$"
  "The title of the headings created by `org-noter-insert-note'.
$p$ is replaced by the number of the page you are in at the
moment."
  :group 'org-noter
  :type 'string)

(defcustom org-noter-notes-window-behavior '(start scroll)
  "This setting specifies in what situations the notes window should be created.

When the list contains:
- `start', the window will be created when starting a `org-noter' session.
- `scroll', it will be created when you go to a page with an associated note."
  :group 'org-noter
  :type '(set (const :tag "Session start" start)
              (const :tag "Scrolling" scroll)))

(defcustom org-noter-notes-window-location 'horizontal-split
  "Whether the notes window should appear in the main frame (horizontal or vertical split) or on a separate frame.

Note that this will only have effect on session startup if `start'
is member of `org-noter-notes-window-behavior' (which see)."
  :group 'org-noter
  :type '(choice (const :tag "Horizontal" horizontal-split)
                 (const :tag "Vertical" vertical-split)
                 (const :tag "Other frame" 'other-frame)))

;; --------------------------------------------------------------------------------
;; NOTE(nox): Private variables or constants
(cl-defstruct org-noter--session
  frame doc-mode display-name notes-file-path doc-file-path property-text
  notes-buffer doc-buffer level window-behavior window-location)

(defvar org-noter--sessions nil
  "List of `org-noter' sessions.")

(defvar-local org-noter--session nil
  "Session associated with the current buffer.")

(defvar org-noter--inhibit-page-handler nil
  "Prevent page change from updating point in notes.")

(defconst org-noter-property-behavior "NOTER_NOTES_BEHAVIOR"
  "Property for overriding global `org-noter-notes-window-behavior'.")

(defconst org-noter-property-location "NOTER_NOTES_LOCATION"
  "Property for overriding global `org-noter-notes-window-location'.")

;; --------------------------------------------------------------------------------
;; NOTE(nox): Utility functions
(defun org-noter--create-session (ast document-property-value notes-file-path)
  (let* ((display-name (org-element-property :raw-value ast))
         (document (find-file-noselect document-property-value))

         (document-buffer
          (make-indirect-buffer
           document (generate-new-buffer-name (format "org-noter - %s" display-name))))

         (notes-buffer
          (make-indirect-buffer
           (current-buffer)
           (generate-new-buffer-name (format "org-noter - Notes of %s" display-name)) t))

         (session
          (make-org-noter--session
           :display-name display-name
           :frame (make-frame `((name . ,(format "Emacs Org-noter - %s" display-name))
                                (fullscreen . maximized)))
           :doc-mode (buffer-local-value 'major-mode document)
           :property-text document-property-value
           :notes-file-path notes-file-path
           :doc-file-path doc-file-path
           :doc-buffer document-buffer
           :notes-buffer notes-buffer
           :level (org-element-property :level ast)
           :window-behavior (or (org-noter--notes-window-behavior-property ast) org-noter-notes-window-behavior)
           :window-location (or (org-noter--notes-window-location-property ast) org-noter-notes-window-location))))

    (add-hook 'delete-frame-functions 'org-noter--handle-delete-frame)
    (push session org-noter--sessions)

    (org-noter--setup-windows session)

    (with-current-buffer document-buffer
      (setq buffer-file-name doc-file-path)
      (cond ((eq (org-noter--session-doc-mode session) 'pdf-view-mode)
             (pdf-view-mode)
             (add-hook 'pdf-view-after-change-page-hook
                       'org-noter--page-change-handler nil t))
            ((eq (org-noter--session-doc-mode session) 'doc-view-mode)
             (doc-view-mode)
             (advice-add 'doc-view-goto-page :after 'org-noter--doc-view-advice))
            (t (error "This document handler is not supported :/")))
      (org-noter-doc-mode 1)
      (setq org-noter--session session)
      (kill-local-variable 'kill-buffer-hook)
      (add-hook 'kill-buffer-hook 'org-noter--handle-kill-buffer nil t))

    (with-current-buffer notes-buffer
      (org-noter-notes-mode 1)
      (setq buffer-file-name notes-file-path
            org-noter--session session
            fringe-indicator-alist '((truncation . nil)))
      (add-hook 'kill-buffer-hook 'org-noter--handle-kill-buffer nil t)
      (add-hook 'window-scroll-functions 'org-noter--set-scroll nil t)
      (let ((ast (org-noter--parse-root))
            (current-page (org-noter--selected-note-page t)))
        (org-noter--set-read-only ast)
        (if current-page
            (org-noter--goto-page current-page)
          (org-noter--page-change-handler 1))))))

(defun org-noter--valid-session (session &optional first-run)
  (if (and session
           (frame-live-p (org-noter--session-frame session))
           (buffer-live-p (org-noter--session-doc-buffer session))
           (or first-run
               (get-buffer-window (org-noter--session-doc-buffer session)
                                  (org-noter--session-frame session)))
           (buffer-live-p (org-noter--session-notes-buffer session)))
      t
    (org-noter-kill-session session)
    nil))

(defmacro org-noter--with-valid-session (&rest body)
  `(let ((session org-noter--session))
     (when (org-noter--valid-session session)
       (progn ,@body))))

(defun org-noter--handle-kill-buffer ()
  (org-noter--with-valid-session
   (let ((buffer (current-buffer))
         (notes-buffer (org-noter--session-notes-buffer session))
         (doc-buffer (org-noter--session-doc-buffer session)))
     ;; NOTE(nox): This needs to be checked in order to prevent session killing because of
     ;; temporary buffers with the same local variables
     (when (or (eq buffer notes-buffer)
               (eq buffer doc-buffer))
       (org-noter-kill-session session)))))

(defun org-noter--handle-delete-frame (frame)
  (dolist (session org-noter--sessions)
    (when (eq (org-noter--session-frame session) frame)
      (org-noter-kill-session session))))

(defun org-noter--parse-root (&optional buffer property-doc-path)
  ;; TODO(nox): Maybe create IDs in each noter session and use that instead of using
  ;; the property text that may be repeated... This would simplify some things
  (let* ((session org-noter--session)
         (use-args (and (stringp property-doc-path)
                        (buffer-live-p buffer)
                        (with-current-buffer buffer (eq major-mode 'org-mode))))
         (notes-buffer (if use-args
                           buffer
                         (when session (org-noter--session-notes-buffer session))))
         (wanted-value (if use-args
                           property-doc-path
                         (when session (org-noter--session-property-text session))))
         element)
    (when (buffer-live-p notes-buffer)
      (with-current-buffer notes-buffer
        (org-with-wide-buffer
         (unless (org-before-first-heading-p)
           ;; NOTE(nox): Start by trying to find a parent heading with the specified
           ;; property
           (let ((try-next t) property-value)
             (while try-next
               (setq property-value (org-entry-get nil org-noter-property-doc-file))
               (when (and property-value (string= property-value wanted-value))
                 (org-narrow-to-subtree)
                 (setq element (org-element-parse-buffer 'greater-element)))
               (setq try-next (and (not element) (org-up-heading-safe))))))
         (unless element
           ;; NOTE(nox): Could not find parent with property, do a global search
           (let ((pos (org-find-property org-noter-property-doc-file wanted-value)))
             (when pos
               (goto-char pos)
               (org-narrow-to-subtree)
               (setq element (org-element-parse-buffer 'greater-element)))))
         (car (org-element-contents element)))))))

(defun org-noter--get-properties-end (ast &optional force-trim)
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

(defun org-noter--set-read-only (ast)
  (org-with-wide-buffer
   (when ast
     (let* ((level (org-element-property :level ast))
            (begin (org-element-property :begin ast))
            (title-begin (+ 1 level begin))
            (contents-begin (org-element-property :contents-begin ast))
            (properties-end (org-noter--get-properties-end ast t))
            (inhibit-read-only t)
            (modified (buffer-modified-p)))
       (add-text-properties (max 1 (1- begin)) begin '(read-only t))
       (add-text-properties begin (1- title-begin) '(read-only t front-sticky t))
       (add-text-properties (1- title-begin) title-begin '(read-only t rear-nonsticky t))
       (add-text-properties (1- contents-begin) (1- properties-end) '(read-only t))
       (add-text-properties (1- properties-end) properties-end
                            '(read-only t rear-nonsticky t))
       (set-buffer-modified-p modified)))))

(defun org-noter--unset-read-only (ast)
  (org-with-wide-buffer
   (when ast
     (let ((begin (org-element-property :begin ast))
           (end (org-noter--get-properties-end ast t))
           (inhibit-read-only t)
           (modified (buffer-modified-p)))
       (remove-list-of-text-properties (max 1 (1- begin)) end
                                       '(read-only front-sticky rear-nonsticky))
       (set-buffer-modified-p modified)))))

(defun org-noter--set-scroll (window &rest ignored)
  (when window
    (with-selected-window window
      (org-noter--with-valid-session
       (let* ((level (org-noter--session-level session))
              (goal (* (1- level) 2))
              (current-scroll (window-hscroll)))
         (when (and (bound-and-true-p org-indent-mode) (< current-scroll goal))
           (scroll-right current-scroll)
           (scroll-left goal t)))))))

(defun org-noter--insert-heading (level)
  (org-insert-heading)
  (let* ((initial-level (org-element-property :level (org-element-at-point)))
         (changer (if (> level initial-level) 'org-do-demote 'org-do-promote))
         (number-of-times (abs (- level initial-level))))
    (dotimes (_ number-of-times)
      (funcall changer))))

(defun org-noter--narrow-to-root (ast)
  (when ast
    (save-excursion
      (goto-char (org-element-property :begin ast))
      (org-show-entry)
      (org-narrow-to-subtree)
      (org-show-children))))

(defun org-noter--setup-windows (session)
  "Setup windows when starting session, respecting user configuration."
  (when (org-noter--valid-session session t)
    (with-selected-frame (org-noter--session-frame session)
      (delete-other-windows)
      (let* ((doc-buffer (org-noter--session-doc-buffer session))
             (doc-window (selected-window))
             (notes-buffer (org-noter--session-notes-buffer session))
             (notes-window-location (org-noter--session-window-location session))
             (notes-window-behavior (org-noter--session-window-behavior session))
             notes-window)

        (set-window-buffer doc-window doc-buffer)
        (set-window-dedicated-p doc-window t)

        (with-current-buffer notes-buffer
          (org-noter--narrow-to-root
           (org-noter--parse-root
            notes-buffer (org-noter--session-property-text session))))

        (setq
         notes-window
         (or (get-buffer-window notes-buffer t)
             (when (member 'start notes-window-behavior)
               (if (eq notes-window-location 'other-frame)
                   (get-buffer-window (switch-to-buffer-other-frame notes-buffer) t)
                 (set-window-buffer
                  (if (eq notes-window-location 'horizontal-split)
                      (split-window-right)
                    (split-window-below))
                  notes-buffer)))))

        (org-noter--set-scroll notes-window)))))

(defun org-noter--get-doc-window ()
  (org-noter--with-valid-session
   (get-buffer-window (org-noter--session-doc-buffer session)
                      (org-noter--session-frame session))))

(defun org-noter--get-notes-window (&optional type)
  (org-noter--with-valid-session
   (let ((buffer (org-noter--session-notes-buffer session))
         (window-location (org-noter--session-window-location session))
         (window-behavior (org-noter--session-window-behavior session)))
     (or (get-buffer-window buffer t)
         (when (or (eq type 'force) (memq type window-behavior))
           (if (eq window-location 'other-frame)
               (get-buffer-window (switch-to-buffer-other-frame buffer) t)
             ;; TODO(nox): This should honor the split right/down setting... But what
             ;; should it do when there is another window besides the PDF? Maybe just
             ;; doing this is fine...
             (display-buffer buffer nil (org-noter--session-frame session))))))))

(defmacro org-noter--with-selected-notes-window (error-str &rest body)
  `(org-noter--with-valid-session
    (let ((notes-window (org-noter--get-notes-window)))
      (if notes-window
          (with-selected-window notes-window
            (progn ,@body))
        (when ,error-str
          (user-error "%s" ,error-str))))))

(defun org-noter--notes-window-behavior-property (ast)
  (let ((property (org-element-property (intern (concat ":" org-noter-property-behavior)) ast))
        value)
    (when (and (stringp property) (> (length property) 0))
      (setq value (car (read-from-string property)))
      (when (listp value)
        value))))

(defun org-noter--notes-window-location-property (ast)
  (let ((property (org-element-property (intern (concat ":" org-noter-property-location)) ast))
        value)
    (when (and (stringp property) (> (length property) 0))
      (setq value (intern property))
      (when (memq value '(horizontal-split vertical-split other-frame))
        value))))

(defun org-noter--current-page ()
  (org-noter--with-valid-session
   (with-current-buffer (org-noter--session-doc-buffer session)
     (image-mode-window-get 'page))))

(defun org-noter--doc-view-advice (page)
  (when (org-noter--valid-session org-noter--session)
    (org-noter--page-change-handler page)))

(defun org-noter--selected-note-page (&optional with-start-page)
  (org-noter--with-valid-session
   (org-with-wide-buffer
    (let ((root-doc-prop-vale (org-noter--session-property-text session)))
      (org-noter--page-property
       (catch 'break
         (let ((try-next t)
               property-value at-root)
           (while try-next
             (setq property-value (org-entry-get nil org-noter-property-note-page)
                   at-root (string= (org-entry-get nil org-noter-property-doc-file)
                                    root-doc-prop-vale))
             (when (and property-value
                        (or with-start-page (not at-root)))
               (throw 'break property-value))
             (setq try-next (org-up-heading-safe))))))))))

(defun org-noter--page-property (arg)
  (let* ((property (if (stringp arg) arg
                     (org-element-property (intern (concat ":" org-noter-property-note-page))
                                           arg)))
         value)
    (when (and (stringp property) (> (length property) 0))
      (setq value (car (read-from-string property)))
      (cond ((and (consp value) (integerp (car value)) (numberp (cdr value)))
             (cons (max 1 (car value)) (max 0 (min 1 (cdr value)))))
            ((integerp value)
             (cons (max 1 value) 0))
            (t nil)))))

(defun org-noter--get-slice ()
  (let* ((slice (or (image-mode-window-get 'slice) '(0 0 1 1)))
         (slice-top (float (nth 1 slice)))
         (slice-height (float (nth 3 slice))))
    (when (or (> slice-top 1)
              (> slice-height 1))
      (let ((height (cdr (image-size (image-mode-window-get 'image) t))))
        (setq slice-top (/ slice-top height)
              slice-height (/ slice-height height))))
    (cons slice-top slice-height)))

(defun org-noter--ask-scroll-percentage ()
  (org-noter--with-valid-session
   (let (event)
     (with-selected-window (org-noter--get-doc-window)
       (while (not (and (eq 'mouse-1 (car event))
                        (eq window (posn-window (event-start event)))))
         (setq event (read-event "Click where you want the start of the note to be!")))
       (let* ((slice (org-noter--get-slice))
              (display-height (cdr (image-display-size (image-get-display-property))))
              (current-scroll (window-vscroll))
              (top (+ current-scroll (cdr (posn-col-row (event-start event)))))
              (display-percentage (/ top display-height))
              (percentage (+ (car slice) (* (cdr slice) display-percentage))))
         (max 0 (min 1 percentage)))))))

(defun org-noter--scroll-to-percentage (percentage)
  (org-noter--with-valid-session
   (with-selected-window (org-noter--get-doc-window)
     (let* ((slice (org-noter--get-slice))
            (display-height (cdr (image-display-size (image-get-display-property))))
            (current-scroll (window-vscroll))
            (display-percentage (/ (- percentage (car slice)) (cdr slice)))
            goal-scroll diff-scroll)
       (setq display-percentage (min 1 (max 0 display-percentage))
             goal-scroll (max 0 (floor (* display-percentage display-height)))
             diff-scroll (- goal-scroll current-scroll))
       (image-scroll-up diff-scroll)))))

(defun org-noter--goto-page (page-cons)
  (when (consp page-cons)
    (org-noter--with-valid-session
     (with-selected-window (org-noter--get-doc-window)
       (cond ((eq (org-noter--session-doc-mode session) 'pdf-view-mode)
              (pdf-view-goto-page (car page-cons)))
             ((eq (org-noter--session-doc-mode session) 'doc-view-mode)
              (doc-view-goto-page (car page-cons)))
             (t (error "This mode is not supported")))
       (org-noter--scroll-to-percentage (cdr page-cons))))))

(defun org-noter--compare-page-cons (comp p1 p2)
  "Compare P1 and P2, page-cons.
When COMP is '< or '>, it works as expected.
When COMP is '>f, it will return t when P1 is a page greater than
P2 or, when in the same page, if P1 is the First of the two."
  (cond ((not p2)
         t)
        ((eq comp '<)
         (or (< (car p1) (car p2))
             (and (= (car p1) (car p2))
                  (< (cdr p1) (cdr p2)))))
        ((eq comp '>)
         (or (> (car p1) (car p2))
             (and (= (car p1) (car p2))
                  (> (cdr p1) (cdr p2)))))
        ((eq comp '>f)
         (or (> (car p1) (car p2))
             (and (= (car p1) (car p2))
                  (< (cdr p1) (cdr p2)))))))

(defun org-noter--focus-notes-region (notes)
  (when notes
    (org-noter--with-selected-notes-window
     nil
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
            (target (org-noter--get-properties-end (car notes))))

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

(defun org-noter--page-change-handler (&optional page-arg)
  (org-noter--with-valid-session
   (unless org-noter--inhibit-page-handler
     (let* ((ast (org-noter--parse-root))
            (contents (when ast (org-element-contents ast)))
            (page (or page-arg (org-noter--current-page)))
            notes)
       ;; NOTE(nox): This only considers the first group of notes from the same page that
       ;; are together in the document (no notes from other pages in between).
       ;; TODO(nox): Should this focus another one if the point is inside those?
       (org-element-map contents 'headline
         (lambda (headline)
           (let ((property (car (org-noter--page-property headline))))
             (when property
               (if (not (= page property))
                   notes
                 (push headline notes)
                 nil))))
         nil t org-element-all-elements)

       (org-noter--get-notes-window 'scroll)
       (org-noter--focus-notes-region (nreverse notes))))))

;; --------------------------------------------------------------------------------
;; NOTE(nox): User commands
(defun org-noter-set-start-page (arg)
  "Set current page the one to show when starting a new session.
With a prefix ARG, remove start page."
  (interactive "P")
  (org-noter--with-valid-session
   (let ((inhibit-read-only t)
         (ast (org-noter--parse-root))
         (page (org-noter--current-page)))
     (with-current-buffer (org-noter--session-notes-buffer session)
       (org-with-wide-buffer
        (goto-char (org-element-property :begin ast))
        (if arg
            (org-entry-delete nil org-noter-property-note-page)
          (org-entry-put nil org-noter-property-note-page (number-to-string page))))))))

(defun org-noter-set-notes-window-behavior (arg)
  "Set the notes window behaviour for the current session.
With a prefix argument, it becomes persistent for that document.

See `org-noter-notes-window-behavior' for more information."
  (interactive "P")
  (org-noter--with-valid-session
   (let* ((inhibit-read-only t)
          (ast (org-noter--parse-root))
          (behavior-possibilities
           '(("Default" . nil)
             ("On start and scroll" . (start scroll))
             ("On start" . (start))
             ("On scroll" . (scroll))
             ("Never" . (never))))
          (behavior
           (cdr (assoc (completing-read "Behavior: " behavior-possibilities nil t)
                       behavior-possibilities))))
     (setf (org-noter--session-window-behavior session)
           (or behavior org-noter-notes-window-behavior))
     (when arg
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if behavior
              (org-entry-put nil org-noter-property-behavior
                             (format "%s" behavior))
            (org-entry-delete nil org-noter-property-behavior))))))))

(defun org-noter-set-notes-window-location (arg)
  "Set the notes window default location for the current session.
With a prefix argument, it becomes persistent for that document.

See `org-noter-notes-window-behavior' for more information."
  (interactive "P")
  (org-noter--with-valid-session
   (let* ((inhibit-read-only t)
          (ast (org-noter--parse-root))
          (location-possibilities
           '(("Default" . nil)
             ("Horizontal split" . horizontal-split)
             ("Vertical split" . vertical-split)
             ("Other frame" . other-frame)))
          (location
           (cdr (assoc (completing-read "Location: " location-possibilities nil t)
                       location-possibilities))))
     (setf (org-noter--session-window-location session)
           (or location org-noter-notes-window-location))
     (when arg
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if location
              (org-entry-put nil org-noter-property-location
                             (format "%s" location))
            (org-entry-delete nil org-noter-property-location))))))))

(defun org-noter-kill-session (&optional session)
  "Kill an `org-noter' session.

When called interactively, if there is no prefix argument and the
buffer has an annotation session, it will kill it; else, it will
show a list of open `org-noter' sessions, asking for which to
kill.

When called from elisp code, you have to pass in the SESSION you
want to kill."
  (interactive "P")
  (when (and (called-interactively-p 'any) (> (length org-noter--sessions) 0))
    ;; NOTE(nox): `session' is representing a prefix argument
    (if (and org-noter--session (not session))
        (setq session org-noter--session)
      (setq session nil)
      (let (collection default doc-display-name notes-file-name display)
        (dolist (session org-noter--sessions)
          (setq doc-display-name (org-noter--session-display-name session)
                notes-file-name (file-name-nondirectory
                                 (org-noter--session-notes-file-path session))
                display (concat doc-display-name " - " notes-file-name))
          (when (eq session org-noter--session) (setq default display))
          (push (cons display session) collection))
        (setq session (cdr (assoc (completing-read "Which session? " collection nil t
                                                   nil nil default)
                                  collection))))))
  (when (and session (memq session org-noter--sessions))
    (let ((frame (org-noter--session-frame session))
          (notes-buffer (org-noter--session-notes-buffer session))
          (doc-buffer (org-noter--session-doc-buffer session)))
      (setq org-noter--sessions (delq session org-noter--sessions))
      (when (eq (length org-noter--sessions) 0)
        (setq delete-frame-functions (delq 'org-noter--handle-delete-frame
                                           delete-frame-functions))
        (when (featurep 'doc-view)
          (advice-remove  'org-noter--doc-view-advice 'doc-view-goto-page)))
      (when (frame-live-p frame)
        (delete-frame frame))
      (when (buffer-live-p doc-buffer)
        (kill-buffer doc-buffer))
      (when (buffer-live-p notes-buffer)
        (let ((base-buffer (buffer-base-buffer notes-buffer))
              (modified (buffer-modified-p notes-buffer)))
          (with-current-buffer notes-buffer
            (org-noter--unset-read-only (org-noter--parse-root))
            (set-buffer-modified-p nil)
            (kill-buffer notes-buffer))
          (with-current-buffer base-buffer
            (set-buffer-modified-p modified)))))))

(defun org-noter-insert-note (&optional arg scroll-percentage)
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
that part of the page (see `org-noter-insert-localized-note' for
more info)."
  (interactive "P")
  (org-noter--with-valid-session
   (let* ((ast (org-noter--parse-root))
          (contents (when ast (org-element-contents ast)))
          (page (org-noter--current-page))
          (insertion-level (1+ (org-element-property :level ast)))
          (window (org-noter--get-notes-window 'force))
          notes best-previous-element)
     (setq scroll-percentage (or scroll-percentage 0))

     (org-element-map contents 'headline
       (lambda (headline)
         (let ((property-cons (org-noter--page-property headline)))
           (if property-cons
               (if (= page (car property-cons))
                   (progn
                     (push headline notes)
                     (when (<= (cdr property-cons) scroll-percentage)
                       (setq best-previous-element headline)))
                 (when (< (car property-cons) page)
                   (setq best-previous-element headline)))
             (unless (org-noter--page-property best-previous-element)
               (setq best-previous-element headline)))))
       nil nil org-element-all-elements)
     (setq notes (nreverse notes))

     (select-frame-set-input-focus (window-frame window))
     (select-window window)

     ;; NOTE(nox): Need to be careful changing the next part, it is a bit complicated to
     ;; get it right...
     (if (and notes (not arg))
         (let ((point (point))
               default note has-contents num-blank collection)
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
                        org-noter-default-heading-title)))
         (if best-previous-element
             (progn
               (goto-char (org-element-property :end best-previous-element))
               (org-noter--insert-heading insertion-level))
           (goto-char
            (org-element-map contents 'section
              (lambda (section)
                (org-element-property :end section))
              nil t org-element-all-elements))
           ;; NOTE(nox): This is needed to insert in the right place...
           (outline-show-entry)
           (org-noter--insert-heading insertion-level))
         (insert title)
         (end-of-line)
         (if (and (not (eobp)) (org-next-line-empty-p))
             (forward-line)
           (insert "\n"))
         (org-entry-put nil org-noter-property-note-page
                        (format "%s" (if (zerop scroll-percentage)
                                         page
                                       (cons page scroll-percentage))))))
     (org-show-context)
     (org-show-siblings)
     (org-show-subtree)
     (org-cycle-hide-drawers 'all))))

(defun org-noter-insert-localized-note ()
  "Insert note associated with part of a page.
This will ask you to click where you want to scroll to when you
sync the document to this note. You should click on the top of
that part. Will always create a new note.

See `org-noter-insert-note' docstring for more."
  (interactive)
  (org-noter-insert-note t (org-noter--ask-scroll-percentage)))

(defun org-noter-sync-prev-page ()
  "Show previous page that has notes, in relation to the current page.
This will force the notes window to popup."
  (interactive)
  (org-noter--with-valid-session
   (let ((current-page (cons (org-noter--current-page) 0))
         (contents (org-element-contents (org-noter--parse-root)))
         target-page)
     (org-noter--get-notes-window 'force)

     (org-element-map contents 'headline
       (lambda (headline)
         (let ((page (org-noter--page-property headline)))
           (when (and (org-noter--compare-page-cons '<  page current-page)
                      (org-noter--compare-page-cons '>f page target-page))
             (setq target-page page))))
       nil nil org-element-all-elements)

     (org-noter--get-notes-window 'force)
     (select-window (org-noter--get-doc-window))
     (if target-page
         (org-noter--goto-page target-page)
       (user-error "There are no more previous pages with notes")))))

(defun org-noter-sync-current-page ()
  "Show current page notes.
This will force the notes window to popup."
  (interactive)
  (org-noter--with-valid-session
   (let ((current-page (org-noter--current-page)))
     (select-window (org-noter--get-notes-window 'force))
     (org-noter--page-change-handler current-page))))

(defun org-noter-sync-next-page ()
  "Show next page that has notes, in relation to the current page.
This will force the notes window to popup."
  (interactive)
  (org-noter--with-valid-session
   (let ((current-page (cons (org-noter--current-page) 1))
         (contents (org-element-contents (org-noter--parse-root)))
         target-page)

     (org-element-map contents 'headline
       (lambda (headline)
         (let ((page (org-noter--page-property headline)))
           (when (and (org-noter--compare-page-cons '> page current-page)
                      (org-noter--compare-page-cons '< page target-page))
             (setq target-page page))))
       nil nil org-element-all-elements)

     (org-noter--get-notes-window 'force)
     (select-window (org-noter--get-doc-window))
     (if target-page
         (org-noter--goto-page target-page)
       (user-error "There are no more following pages with notes")))))

(defun org-noter-sync-prev-note ()
  "Go to the page of the previous note, in relation to where the point is.
As such, it will only work when the notes window exists."
  (interactive)
  (org-noter--with-selected-notes-window
   "No notes window exists"
   (let ((org-noter--inhibit-page-handler t)
         (contents (org-element-contents (org-noter--parse-root)))
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
               (setq previous (if (org-noter--page-property headline) headline previous))
               nil))))
       nil t org-element-all-elements)
     (if previous
         (progn
           ;; NOTE(nox): This needs to be manual so we can focus the correct note (there
           ;; may be several notes on this page, and the automatic page handler would
           ;; focus the first one).
           (org-noter--goto-page (org-noter--page-property previous))
           (org-noter--focus-notes-region (list previous)))
       (error "There is no previous note"))))
  (select-window (org-noter--get-doc-window)))

(defun org-noter-sync-current-note ()
  "Go the page of the selected note, in relation to where the point is.
As such, it will only work when the notes window exists."
  (interactive)
  (org-noter--with-selected-notes-window
   "No notes window exists"
   (let ((page (org-noter--selected-note-page)))
     (if page
         (org-noter--goto-page page)
       (error "No note selected"))))
  (select-window (org-noter--get-doc-window)))

(defun org-noter-sync-next-note ()
  "Go to the page of the next note, in relation to where the point is.
As such, it will only work when the notes window exists."
  (interactive)
  (org-noter--with-selected-notes-window
   "No notes window exists"
   (let ((org-noter--inhibit-page-handler t)
         (contents (org-element-contents (org-noter--parse-root)))
         next)
     (org-element-map contents 'headline
       (lambda (headline)
         (when (and
                (org-noter--page-property headline)
                (< (point) (org-element-property :begin headline)))
           (setq next headline)))
       nil t org-element-all-elements)
     (if next
         (progn
           (org-noter--goto-page (org-noter--page-property next))
           (org-noter--focus-notes-region (list next)))
       (error "There is no next note"))))
  (select-window (org-noter--get-doc-window)))

(define-minor-mode org-noter-doc-mode
  "Minor mode for the document buffer."
  :keymap `((,(kbd   "i")   . org-noter-insert-note)
            (,(kbd "M-i")   . org-noter-insert-localized-note)
            (,(kbd   "q")   . org-noter-kill-session)
            (,(kbd "M-p")   . org-noter-sync-prev-page)
            (,(kbd "M-.")   . org-noter-sync-current-page)
            (,(kbd "M-n")   . org-noter-sync-next-page)
            (,(kbd "C-M-p") . org-noter-sync-prev-note)
            (,(kbd "C-M-.") . org-noter-sync-current-note)
            (,(kbd "C-M-n") . org-noter-sync-next-note)))

(define-minor-mode org-noter-notes-mode
  "Minor mode for the notes buffer."
  :keymap `((,(kbd "M-p")   . org-noter-sync-prev-page)
            (,(kbd "M-.")   . org-noter-sync-current-page)
            (,(kbd "M-n")   . org-noter-sync-next-page)
            (,(kbd "C-M-p") . org-noter-sync-prev-note)
            (,(kbd "C-M-.") . org-noter-sync-current-note)
            (,(kbd "C-M-n") . org-noter-sync-next-note)))

;;;###autoload
(defun org-noter (arg)
  "Start `org-noter' session.

This will open a session for taking your notes, with indirect
buffers to the document and the notes side by side. Your current
window configuration won't be changed, because this opens in a
new frame.

You only need to run this command inside a heading (which will
hold the notes for this document). If no document path property is found,
this command will ask you for the target file.

With a prefix universal argument ARG, only check for the property
in the current heading, don't inherit from parents.

With a prefix number ARG, only open the document like `find-file'
would if ARG >= 0, or open the folder containing the document
when ARG < 0."
  (interactive "P")
  (when (eq major-mode 'org-mode)
    (when (org-before-first-heading-p)
      (error "`org-noter' must be issued inside a heading"))

    (let* ((notes-file-path (buffer-file-name))

           (document-property
            (org-entry-get nil org-noter-property-doc-file
                           (not (equal arg '(4)))))

           (expanded-document-path
            (when (stringp document-property)
              (setq doc-file-path (expand-file-name document-property))))

           ast session)

      (unless (and expanded-document-path
                   (not (file-directory-p expanded-document-path))
                   (file-readable-p expanded-document-path))

        (setq doc-file-path (expand-file-name
                             (read-file-name
                              "Invalid or no document property found. Please specify a document path: "
                              nil nil t)))
        (when (or (file-directory-p doc-file-path)
                  (not (file-readable-p doc-file-path)))
          (user-error "Invalid file path"))

        (setq document-property (if (y-or-n-p "Do you want a relative file name? ")
                                    (file-relative-name doc-file-path)
                                  doc-file-path))
        (org-entry-put nil org-noter-property-doc-file document-property))

      (setq ast (org-noter--parse-root (current-buffer) document-property))

      (when (catch 'should-continue
              (when (or (numberp arg) (eq arg '-))
                (let ((number (prefix-numeric-value arg)))
                  (if (>= number 0)
                      (find-file doc-file-path)
                    (find-file (file-name-directory doc-file-path))))
                (throw 'should-continue nil))

              ;; NOTE(nox): Test for existing sessions
              (dolist (session org-noter--sessions)
                (when (org-noter--valid-session session)
                  (when (and (string= (org-noter--session-doc-file-path session)
                                      doc-file-path)
                             (string= (org-noter--session-notes-file-path session)
                                      notes-file-path))
                    (let ((test-ast (with-current-buffer
                                        (org-noter--session-notes-buffer session)
                                      (org-noter--parse-root))))
                      (when (eq (org-element-property :begin ast)
                                (org-element-property :begin test-ast))
                        (org-noter--setup-windows session)
                        (select-frame-set-input-focus (org-noter--session-frame session))
                        (throw 'should-continue nil))))))
              t)

        (org-noter--create-session ast document-property notes-file-path))))

  (when (and (not (eq major-mode 'org-mode)) (org-noter--valid-session org-noter--session))
    (org-noter--setup-windows org-noter--session)
    (select-frame-set-input-focus (org-noter--session-frame org-noter--session))))

(provide 'org-noter)

;;; org-noter.el ends here
