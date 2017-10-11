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

(cl-defstruct interleave--session frame org-file-path pdf-file-path notes-buffer pdf-buffer)

(defvar interleave--sessions nil
  "List of Interleave sessions")

(defvar-local interleave--session nil
  "Session associated with the current buffer.")

(defun interleave--session-valid (session)
  (and (frame-live-p (interleave--session-frame session))
       (buffer-live-p (interleave--session-pdf-buffer session))
       (buffer-live-p (interleave--session-notes-buffer session))))

(defun interleave--handle-buffer-kill ()
  (interleave-kill-session interleave--session))

(defun interleave--handle-delete-frame (frame)
  (dolist (session interleave--sessions)
    (when (eq (interleave--session-frame session) frame)
      (interleave-kill-session session))))

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
    (setq interleave--sessions (delq session interleave--sessions))
    (when (eq (length interleave--sessions) 0)
      (setq delete-frame-functions (delq 'interleave--handle-delete-frame
                                         delete-frame-functions)))
    (let ((frame (interleave--session-frame session))
          (notes-buffer (interleave--session-notes-buffer session))
          (pdf-buffer (interleave--session-pdf-buffer session)))
      (when (frame-live-p frame)
        (delete-frame frame))
      (when (buffer-live-p pdf-buffer)
        (kill-buffer pdf-buffer))
      (when (buffer-live-p notes-buffer)
        (kill-buffer notes-buffer)))))

;;;###autoload
(defun interleave ()
  (interactive)
  (when (eq major-mode 'org-mode)
    (when (org-before-first-heading-p)
      (error "Interleave must be issued inside a heading."))
    (let ((org-file-path (buffer-file-name))
          (pdf-property (org-entry-get nil interleave--property-pdf-file t))
          pdf-file-path pdf-file-name session)
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
        (setq pdf-file-name (file-name-nondirectory
                             (file-name-sans-extension pdf-file-path))
              session
              (make-interleave--session
               :frame (make-frame `((name . ,(format "Emacs - Interleave %s" pdf-file-name))
                                    (fullscreen . maximized)))
               :org-file-path org-file-path
               :pdf-file-path pdf-file-path
               :notes-buffer (make-indirect-buffer
                              (current-buffer)
                              (generate-new-buffer-name
                               (format "Interleave - Notes of %s" pdf-file-name))
                              t)
               :pdf-buffer (make-indirect-buffer (find-file-noselect pdf-file-path)
                                                 (generate-new-buffer-name
                                                  (format "Interleave - %s"
                                                          pdf-file-name))
                                                 t)))
        (with-current-buffer (interleave--session-pdf-buffer session)
          (setq interleave--session session
                buffer-file-name pdf-file-path)
          (kill-local-variable 'kill-buffer-hook)
          (add-hook 'kill-buffer-hook 'interleave--handle-buffer-kill nil t))
        (with-current-buffer (interleave--session-notes-buffer session)
          (setq interleave--session session)
          (add-hook 'kill-buffer-hook 'interleave--handle-buffer-kill nil t)
          (let ((heading-location (org-find-property interleave--property-pdf-file pdf-property))
                heading properties properties-begin  properties-end)
            (goto-char heading-location)
            (org-show-entry)
            (org-show-children)
            (setq heading (org-element-at-point)
                  properties-begin (org-element-property ':contents-begin heading)
                  properties (save-excursion (goto-char properties-begin)
                                             (org-element-at-point))
                  properties-end (org-element-property ':end properties))
            (when (eq properties-end (org-element-property ':contents-end heading))
              (save-excursion
                (goto-char properties-end)
                (insert "\n")))
            (save-excursion
              (narrow-to-region properties-end
                                (progn (org-end-of-subtree t t)
                                       (when (and (org-at-heading-p) (not (eobp))) (backward-char 1))
                                       (point))))))
        (with-selected-frame (interleave--session-frame session)
          (let ((pdf-window (selected-window))
                (notes-window (split-window-right)))
            ;; TODO(nox): Option to customize this
            (set-window-buffer pdf-window (interleave--session-pdf-buffer session))
            (set-window-dedicated-p pdf-window t)
            (set-window-buffer notes-window (interleave--session-notes-buffer session))
            (set-window-dedicated-p notes-window t)))
        (add-hook 'delete-frame-functions 'interleave--handle-delete-frame)
        (push session interleave--sessions)))))
