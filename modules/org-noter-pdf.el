;;; org-noter-pdf.el --- Modules for PDF-Tools and DocView mode  -*- lexical-binding: t; -*-

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

(defun org-noter-pdf-approx-location-cons (major-mode &optional precise-info _force-new-ref)
  (when (memq major-mode '(doc-view-mode pdf-view-mode))
    (cons (image-mode-window-get 'page) (if (and (listp precise-info)
                                                 (numberp (car precise-info))
                                                 (numberp (cadr precise-info)))
                                            precise-info 0))))

(defun org-noter-pdf-view-setup-handler (major-mode)
  (when (eq document-major-mode 'pdf-view-mode)
    (setq buffer-file-name document-path)
    (pdf-view-mode)
    (add-hook 'pdf-view-after-change-page-hook 'org-noter--doc-location-change-handler nil t)))

(defun org-noter-doc-view-setup-handler (major-mode)
  (when (eq document-major-mode 'doc-view-mode)
    (setq buffer-file-name document-path)
    (doc-view-mode)
    (advice-add 'doc-view-goto-page :after 'org-noter--location-change-advice)))

(defun org-noter-pdf--pretty-print-location (location)
  (when (memq (org-noter--session-doc-mode session) '(doc-view-mode pdf-view-mode))
    (if (or (not (org-noter--get-location-top location)) (<= (org-noter--get-location-top location) 0))
        (car location)
      location)))

(provide 'org-noter-pdf)
;;; org-noter-pdf.el ends here
