;;; org-noter-djvu.el --- Module for DJVU            -*- lexical-binding: t; -*-

;; Copyright (C) 2022  c1-g

;; Author: c1-g <char1iegordon@protonmail.com>
;; Keywords: multimedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defun org-noter-djvu--pretty-print-location (major-mode location)
  (when (eq major-mode '(djvu-read-mode))
    (if (or (not (org-noter--get-location-top location)) (<= (org-noter--get-location-top location) 0))
        (car location)
      location)))

(defun org-noter-djvu-approx-location-cons (major-mode &optional precise-info _force-new-ref)
  (cons djvu-doc-page (if (or (numberp precise-info)
                              (and (consp precise-info)
                                   (numberp (car precise-info))
                                   (numberp (cdr precise-info))))
                          precise-info
                        (max 1 (/ (+ (window-start) (window-end nil t)) 2)))))

(defun org-noter-djvu--get-precise-info (major-mode)
  (when (eq mode 'djvu-read-mode)
    (if (region-active-p)
        (cons (mark) (point))
      (while (not (and (eq 'mouse-1 (car event))
                       (eq window (posn-window (event-start event)))))
        (setq event (read-event "Click where you want the start of the note to be!")))
      (posn-point (event-start event)))))

(defun org-noter-djvu-setup-handler (major-mode)
  (when (eq major-mode 'djvu-read-mode)
    (advice-add 'djvu-init-page :after 'org-noter--location-change-advice)
    t))

(defun org-noter-djvu-goto-location (mode location)
  (when (eq mode 'djvu-read-mode)
    (djvu-goto-page (car location))
    (goto-char (org-noter--get-location-top location))))

(defun org-noter-djvu--get-current-view (mode)
  (when (eq mode 'djvu-read-mode)
    (vector 'paged (car (org-noter--doc-approx-location-cons)))))

(defun org-noter-djvu--get-selected-text (mode)
  (when (and (eq mode 'djvu-read-mode)
             (region-active-p))
    (buffer-substring-no-properties (mark) (point))))

(provide 'org-noter-djvu)
;;; org-noter-djvu.el ends here
