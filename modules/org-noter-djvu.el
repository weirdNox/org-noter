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

(defun org-noter-djvu--pretty-print-location (location)
  (when (eq (org-noter--session-doc-mode session) '(djvu-read-mode))
    (if (or (not (org-noter--get-location-top location)) (<= (org-noter--get-location-top location) 0))
        (car location)
      location)))

(provide 'org-noter-djvu)
;;; org-noter-djvu.el ends here
