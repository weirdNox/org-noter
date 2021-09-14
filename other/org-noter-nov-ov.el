;;; org-noter-nov-ov.el --- Module to highlight text in nov-mode with notes  -*- lexical-binding: t; -*-

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

;; Highlight your precise notes in nov with org-noter-nov-ov.el

;;; Code:
(require 'org-noter)
(require 'nov)
(require 'seq)

(defun org-noter-nov-go-to-origin-note (overlay)
  (org-noter--with-selected-notes-window
   (org-with-wide-buffer
    (goto-char (overlay-get overlay 'origin)))))

(defun org-noter-nov-make-ov ()
  "TODO"
  (org-noter--with-valid-session
   (when-let* ((is-this-nov (eq (org-noter--session-doc-mode session) 'nov-mode)) 
               (origin-note-location (org-element-property :begin (org-element-at-point)))
               (location-property (org-entry-get nil org-noter-property-note-location nil t))
               (location-cons (cdr (read location-property)))
               (beg (if (consp location-cons)
                        (car location-cons)
                      location-cons))
               (end (if (consp location-cons)
                        (cdr location-cons)
                      (1+ beg)))
               (ov (make-overlay beg end (org-noter--session-doc-buffer session)))
               (hl-color "yellow"))
     
     (overlay-put ov 'button ov)
     (overlay-put ov 'category 'default-button)
     (overlay-put ov 'face (list :background (if org-noter-insert-note-no-questions                                                  
                                                 hl-color
                                               (setq hl-color    (read-color "Highlight color: ")))
                                 :foreground (readable-foreground-color hl-color)))
     (overlay-put ov 'mouse-face (list :background (setq hl-color (color-lighten-name hl-color 5))
                                       :foreground (readable-foreground-color hl-color)))

     (overlay-put ov 'origin origin-note-location)

     (overlay-put ov 'action #'org-noter-nov-go-to-origin-note))))

(add-hook 'org-noter-insert-heading-hook #'org-noter-nov-make-ov)

(provide 'org-noter-nov-ov)
;;; org-noter-nov-ov.el ends here

