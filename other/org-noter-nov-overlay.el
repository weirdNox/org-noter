;;; org-noter-nov-overlay.el --- Module to highlight text in nov-mode with notes  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Charlie Gordon

;; Author: Charlie Gordon <char1iegordon@protonmail.com>
;; Keywords: multimedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License,
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Highlight your precise notes in nov with org-noter-nov-overlay.el

;;; Code:
(require 'org-noter)
(require 'nov)
(require 'seq)

(defcustom org-noter-nov-overlay-color-property "NOTER_OVERLAY"
  "A property that specifies the overlay color for `org-noter-nov-make-ov'.")

(defcustom org-noter-nov-overlay-default-color "SkyBlue"
  "Name of the default background color of the overlay `org-noter-nov-make-ov' makes.

Should be one of the element in `defined-colors'.")

(defun org-noter-nov-make-overlays ()
  (org-noter--with-selected-notes-window
   (let* ((page (buffer-local-value 'nov-documents-index (org-noter--session-doc-buffer session)))
          (regexp (org-re-property org-noter-property-note-location t nil
                                   (format (rx "(" (* space) "%d" (+ space)
                                               (+ digit) (+ space) "."  (+ space)
                                               (+ digit) (* space) ")")
                                           page))))
     (org-with-wide-buffer
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (when-let ((location (org-entry-get nil org-noter-property-note-location nil t)))
          (org-noter-nov-make-overlay-no-question)))))))

(defun org-noter-nov-make-overlay ()
  "TODO"
  (org-noter--with-selected-notes-window
   "No notes window exists"
   (when (eq (org-noter--session-doc-mode session) 'nov-mode)
     (let* ((location-property (org-entry-get nil org-noter-property-note-location nil t))
                 (location-cons (cdr (read location-property)))
                 (beg (car location-cons))
                 (end (cdr location-cons))
                 (ov-pair (list (make-overlay beg end (org-noter--session-doc-buffer session))))
                 (hl-color (or (org-entry-get nil org-noter-nov-overlay-color-property nil t)
                               (if org-noter-insert-note-no-questions
                                   org-noter-nov-overlay-default-color
                                 (read-color "Highlight color: "))))
                 (hl-color-alt (color-lighten-name hl-color 15))
                 (action-functions (list
                                    #'org-noter-nov-overlay-sync-current-note
                                    #'org-noter-nov-overlay-sync-current-page-or-chapter)))
       
       (save-excursion
         (org-back-to-heading t)
         (re-search-forward org-heading-regexp nil t)
         (push (make-overlay (match-beginning 1) (match-end 1)) ov-pair))

       (dolist (ov ov-pair)
         (overlay-put ov 'button ov)
         (overlay-put ov 'category 'default-button)
         (overlay-put ov 'face (list :background hl-color
                                     :foreground (readable-foreground-color hl-color)))

         (org-entry-put nil org-noter-nov-overlay-color-property hl-color)
         
         (overlay-put ov 'mouse-face (list :background hl-color-alt
                                           :foreground (readable-foreground-color hl-color-alt)))

         (overlay-put ov 'action (pop action-functions)))))))

(defun org-noter-nov-make-overlay-no-question ()
  "Like `org-noter-nov-make-ov', but doesn't ask user to select the overlay color."
  (org-noter--with-valid-session
   (let ((org-noter-insert-note-no-questions t))
     (org-noter-nov-make-overlay))))

(defun org-noter-nov-overlay-sync-current-page-or-chapter (_overlay)
  "A wrapper function for `org-noter-sync-current-page-or-chapter'
used exclusively with overlays made with `org-noter-nov-make-overlay'

This wrapper ignores the first argument passed to it and just call
`org-noter-sync-current-page-or-chapter'."
  
  (org-noter-sync-current-page-or-chapter))

(defun org-noter-nov-overlay-sync-current-note (_overlay)
  "A wrapper function for `org-noter-nov-overlay-sync-current-note'
used exclusively with overlays made with `org-noter-nov-make-overlay'

This wrapper ignores the first argument passed to it and just call
`org-noter-nov-overlay-sync-current-note'."
  (org-noter-sync-current-note))

(add-hook 'nov-post-html-render-hook #'org-noter-nov-make-overlays)

(provide 'org-noter-nov-overlay)
;;; org-noter-nov-ov.el ends here

