;;; org-noter-nov.el --- Integration with Nov.el     -*- lexical-binding: t; -*-

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


(defun org-noter-get-buffer-file-name-nov ()
  (bound-and-true-p nov-file-name))


(defun org-noter-nov-approx-location-cons (major-mode &optional precise-info _force-new-ref)
  (when (eq major-mode 'nov-mode)
    (cons nov-documents-index (if (or (numberp precise-info)
                                      (and (consp precise-info)
                                           (numberp (car precise-info))
                                           (numberp (cdr precise-info))))
                                  precise-info
                                (max 1 (/ (+ (window-start) (window-end nil t)) 2))))))

(defun org-noter-nov-setup-handler (major-mode)
  (when (eq major-mode 'nov-mode)
    (rename-buffer document-buffer-name)
    (advice-add 'nov-render-document :after 'org-noter--nov-scroll-handler)
    (add-hook 'window-scroll-functions 'org-noter--nov-scroll-handler nil t)))

(provide 'org-noter-nov)
;;; org-noter-nov.el ends here
