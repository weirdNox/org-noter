;;; interleave.el --- Interleave PDFs                     -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Gonçalo Santos

;; Author: Gonçalo Santos (aka. weirdNox)
;; Keywords: lisp pdf interleave
;; Version: 0.0.1

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

;; THE NAME IS TEMPORARY
;; This is a rewrite from scratch of Interleave mode, by rudolfochrist, using many of his
;; great ideas, and trying to achieve better user experience by providing extra features.

(require 'cl-lib)

(defconst interleave--property-pdf-file "INTERLEAVE_PDF"
  "Name of the property which specifies the PDF file.")

;; TODO(nox): Change this string, this is for compatibility with previous interleave mode
(defconst interleave--property-note-page "INTERLEAVE_PAGE_NOTE"
  "Name of the property which specifies the page of the current note.")

(cl-defstruct interleave--session frame property-text org-file-path
              pdf-file-path notes-buffer pdf-buffer)

(defvar interleave--sessions nil
  "List of Interleave sessions")

(defvar-local interleave--session nil
  "Session associated with the current buffer.")

(defun interleave-kill-session (&optional session)
  (interactive)
  (when (and (interactive-p) (> (length interleave--sessions) 0))
    (let (collection default pdf-file-name org-file-name display)
      (dolist (session interleave--sessions)
        (setq pdf-file-name (file-name-nondirectory
                             (interleave--session-pdf-file-path session))
              org-file-name (file-name-nondirectory
                             (interleave--session-org-file-path session))
              display (concat pdf-file-name " with notes from " org-file-name))
        (when (eq session interleave--session) (setq default display))
        (push (cons display session) collection))
      (setq session (cdr (assoc (completing-read "Which session? " collection nil t
                                                 nil nil default)
                                collection)))))
  (when (and session (memq session interleave--sessions))
    (let ((frame (interleave--session-frame session))
          (notes-buffer (interleave--session-notes-buffer session))
          (pdf-buffer (interleave--session-pdf-buffer session)))
      (with-current-buffer notes-buffer
        (interleave--unset-read-only (interleave--parse-root)))
      (setq interleave--sessions (delq session interleave--sessions))
      (when (eq (length interleave--sessions) 0)
        (setq delete-frame-functions (delq 'interleave--handle-delete-frame
                                           delete-frame-functions)))
      (when (frame-live-p frame)
        (delete-frame frame))
      (when (buffer-live-p pdf-buffer)
        (kill-buffer pdf-buffer))
      (when (buffer-live-p notes-buffer)
        (kill-buffer notes-buffer)))))

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
  (interleave-kill-session interleave--session))

(defun interleave--handle-delete-frame (frame)
  (dolist (session interleave--sessions)
    (when (eq (interleave--session-frame session) frame)
      (interleave-kill-session session))))

(defun interleave--parse-root ()
  (interleave--with-valid-session
   (with-current-buffer (interleave--session-notes-buffer session)
     (org-with-wide-buffer
      (let ((wanted-value (interleave--session-property-text session))
            element)
        (unless (org-before-first-heading-p)
          ;; NOTE(nox): Start by trying to find a parent heading with the specified
          ;; property
          (let ((try-next t) property-value)
            (while try-next
              (setq property-value (org-entry-get nil interleave--property-pdf-file))
              (when (and property-value (string= property-value wanted-value))
                (org-narrow-to-subtree)
                (setq element (org-element-parse-buffer 'greater-element)))
              (setq try-next (and (not element) (org-up-heading-safe))))))
        (unless element
          ;; NOTE(nox): Could not find parent with property, try all the document, if
          ;; permited
          (let ((pos (org-find-property interleave--property-pdf-file wanted-value)))
            (when pos
              (goto-char pos)
              (org-narrow-to-subtree)
              (setq element (org-element-parse-buffer 'greater-element)))))
        (car (org-element-contents element)))))))

(defun interleave--get-properties-end (ast)
  (when ast
    (let* ((properties (org-element-map ast 'property-drawer 'identity nil t))
           (properties-end (org-element-property :end properties)))
      (while (not (eq (char-before properties-end) ?:))
        (setq properties-end (1- properties-end)))
      properties-end)))

(defun interleave--set-read-only (ast)
  (when ast
    (let ((begin (org-element-property :begin ast))
          (properties-end (interleave--get-properties-end ast))
          (modified (buffer-modified-p)))
      (add-text-properties begin (1+ begin) '(read-only t front-sticky t))
      (add-text-properties (1+ begin) (1- properties-end) '(read-only t))
      (add-text-properties (1- properties-end) properties-end '(read-only t rear-nonsticky t))
      (set-buffer-modified-p modified))))

(defun interleave--unset-read-only (ast)
  (when ast
    (let ((begin (org-element-property :begin ast))
          (end (interleave--get-properties-end ast))
          (inhibit-read-only t)
          (modified (buffer-modified-p)))
      (remove-list-of-text-properties begin end '(read-only front-sticky rear-nonsticky))
      (set-buffer-modified-p modified))))

(defun interleave--narrow-to-root (ast)
  (when ast
    (let ((old-point (point))
          (begin (org-element-property :begin ast))
          (end (org-element-property :end ast))
          (contents-pos (interleave--get-properties-end ast)))
      (goto-char begin)
      (org-show-entry)
      (org-show-children 1)
      (org-narrow-to-subtree)
      (if (or (< old-point begin) (> old-point end))
          (goto-char contents-pos)
        (goto-char old-point)))))

(defun interleave--current-page ()
  (interleave--with-valid-session
   (with-current-buffer (interleave--session-pdf-buffer session)
     (image-mode-window-get 'page))))

(defun interleave-insert-note ()
  (interactive)
  (interleave--with-valid-session
   (let* ((ast (interleave--parse-root))
          (page (interleave--current-page))
          (page-string (number-to-string page))
          note-element closest-previous-element)
     (when ast
       (setq
        note-element
        (org-element-map (org-element-contents ast) 'headline
          (lambda (headline)
            (let ((property-value (org-element-property
                                   (intern (concat ":" interleave--property-note-page)) headline)))
              (cond ((string= property-value page-string) headline)
                    ((or (not property-value) (string< property-value page-string))
                     (setq closest-previous-element headline)
                     nil))))
          nil t 'headline))
       (with-selected-frame (interleave--session-frame session)
         (pop-to-buffer (interleave--session-notes-buffer session))
         (with-selected-window (get-buffer-window (interleave--session-notes-buffer session))
           (if note-element
               (let ((last (car (last (car (org-element-contents note-element)))))
                     (num-blank (org-element-property :post-blank note-element)))
                 (goto-char (org-element-property :end note-element))
                 (cond ((eq (org-element-type last) 'property-drawer)
                        (when (eq num-blank 0) (insert "\n")))
                       (t (while (< num-blank 2)
                            (insert "\n")
                            (setq num-blank (1+ num-blank)))))
                 (forward-line -1)
                 (org-show-entry)
                 (org-show-siblings))
             (if closest-previous-element
                 (progn
                   (goto-char (org-element-property :end closest-previous-element))
                   (org-insert-heading))
               (goto-char (interleave--get-properties-end ast))
               (while (not (eq (char-before) ?:))
                 (message "%s" (char-before))
                 (backward-char))
               (outline-show-entry)
               (org-insert-subheading nil))
             (insert (format "Notes for page %d\n" page))
             (org-entry-put nil interleave--property-note-page page-string))
           (org-cycle-hide-drawers 'all)))))))

;;;###autoload
(defun interleave (arg)
  "Start Interleave.
When with an argument, only check for the property in the current
heading"
  (interactive "P")
  (when (eq major-mode 'org-mode)
    (when (org-before-first-heading-p)
      (error "Interleave must be issued inside a heading."))
    (let ((org-file-path (buffer-file-name))
          (pdf-property (org-entry-get nil interleave--property-pdf-file (not arg)))
          pdf-file-path pdf-name session)
      (when (stringp pdf-property) (setq pdf-file-path (expand-file-name pdf-property)))
      (unless (and pdf-file-path (not (file-directory-p pdf-file-path)) (file-readable-p pdf-file-path))
        (setq pdf-file-path (expand-file-name
                             (read-file-name
                              "No INTERLEAVE_PDF property found. Please specify a PDF path: "
                              nil nil t)))
        (when (or (file-directory-p pdf-file-path) (not (file-readable-p pdf-file-path)))
          (error "Invalid file path."))
        (setq pdf-property (if (y-or-n-p "Do you want a relative file name? ")
                               (file-relative-name pdf-file-path)
                             pdf-file-path))
        (org-entry-put nil pdf-property interleave--property-pdf-file))
      (when (catch 'should-continue
              (dolist (session interleave--sessions)
                (when (string= (interleave--session-pdf-file-path session)
                               pdf-file-path)
                  (if (string= (interleave--session-org-file-path session)
                               org-file-path)
                      (if (interleave--session-valid session)
                          (progn
                            (raise-frame (interleave--session-frame session))
                            (throw 'should-continue nil))
                        ;; NOTE(nox): This should not happen, but we may as well account
                        ;; for it
                        (interleave-kill-session session)
                        (throw 'should-continue t))
                    (if (y-or-n-p (format "%s is already being Interleaved in another notes file. \
Should I end the session? "))
                        (progn
                          (interleave-kill-session session)
                          (throw 'should-continue t))
                      (throw 'should-continue nil)))))
              t)
        (setq pdf-name (file-name-nondirectory
                        (file-name-sans-extension pdf-file-path))
              session
              (make-interleave--session
               :frame (make-frame `((name . ,(format "Emacs - Interleave %s" pdf-name))
                                    (fullscreen . maximized)))
               :property-text pdf-property
               :org-file-path org-file-path
               :pdf-file-path pdf-file-path
               :notes-buffer (make-indirect-buffer
                              (current-buffer)
                              (generate-new-buffer-name
                               (format "Interleave - Notes of %s" pdf-name))
                              t)
               :pdf-buffer (make-indirect-buffer (find-file-noselect pdf-file-path)
                                                 (generate-new-buffer-name
                                                  (format "Interleave - %s" pdf-name)))))
        (with-current-buffer (interleave--session-pdf-buffer session)
          (setq buffer-file-name pdf-file-path)
          (pdf-tools-install)
          (kill-local-variable 'kill-buffer-hook)
          (setq interleave--session session)
          (add-hook 'kill-buffer-hook 'interleave--handle-kill-buffer nil t))
        (with-current-buffer (interleave--session-notes-buffer session)
          (setq interleave--session session)
          (let ((ast (interleave--parse-root)))
            (interleave--set-read-only ast)
            (interleave--narrow-to-root ast)
            (add-hook 'kill-buffer-hook 'interleave--handle-kill-buffer nil t)))
        (with-selected-frame (interleave--session-frame session)
          (let ((pdf-window (selected-window))
                (notes-window (split-window-right)))
            ;; TODO(nox): Option to customize this
            (set-window-buffer pdf-window (interleave--session-pdf-buffer session))
            (set-window-dedicated-p pdf-window t)
            (set-window-buffer notes-window (interleave--session-notes-buffer session))))
        (add-hook 'delete-frame-functions 'interleave--handle-delete-frame)
        (push session interleave--sessions)))))
