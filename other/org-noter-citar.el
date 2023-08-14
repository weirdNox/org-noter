;;; org-noter-citar.el --- Module for finding note files from `citar'  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  c1-g

;; Author: c1-g <char1iegordon@protonmail.com>
;; Keywords: convenience

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

;;; Code:
(require 'citar)
(require 'org-ref)
(require 'seq)

;; Regexp stolen from org-roam-bibtex; orb-utils-citekey-re.
(defvar org-noter-citar-cite-key-re
  (rx
   (or
    (seq (group-n 2 (regexp
                     ;; If Org-ref is available, use its types
                     ;; default to "cite"
                     (if (boundp 'org-ref-cite-types)
                         (regexp-opt
                          (mapcar
                           (lambda (el)
                             ;; Org-ref v3 cite type is a list of strings
                             ;; Org-ref v2 cite type is a plain string
                             (or (car-safe el) el))
                           org-ref-cite-types))
                       "cite")))
         ":"
         (or
          ;; Org-ref v2 style `cite:links'
          (group-n 1 (+ (any "a-zA-Z0-9_:.-")))
          ;; Org-ref v3 style `cite:Some&key'
          (seq (*? (not "&")) "&"
               (group-n 1 (+ (any "!#-+./:<>-@^-`{-~-" word))))))
    ;; Org-cite [cite/@citations]
    (seq "@" (group-n 1 (+ (any "!#-+./:<>-@^-`{-~-" word))))))
  "Universal regexp to match citations in ROAM_REFS.

Supports Org-ref v2 and v3 and Org-cite.")

(defun org-noter-citar-find-document-from-refs (cite-key)
  "Return a note file associated with CITE-KEY.
When there is more than one note files associated with CITE-KEY, have
user select one of them."
  (when (and (stringp cite-key) (string-match org-noter-citar-cite-key-re cite-key))
    (let* ((key (match-string 1 cite-key))
           (entries (citar--ensure-entries (list key)))
           (files (citar-file--files-for-multiple-entries
                   entries
                   (append citar-library-paths citar-notes-paths) nil))
           (url (list (citar-get-link (car entries))))
           (documents (flatten-list (append (seq-remove #'file-directory-p files) url))))
      (cond ((= (length documents) 1)
             (car documents))
            ((> (length documents) 1)
             (completing-read (format "Which document from %s?: " key) documents))))))

(defun org-noter-citar-find-key-from-this-file (filename)
  (let* ((entry-alist (mapcan (lambda (entry)
                                (when-let ((file (citar-get-value citar-file-variable entry)))
                                  (list (cons file (citar-get-value "=key=" entry)))))
                              (citar--get-candidates)))
         (key (alist-get filename entry-alist nil nil (lambda (s regexp)
                                                        (string-match-p regexp s)))))
    (when key
      (file-name-with-extension key "org"))))

(add-to-list 'org-noter-parse-document-property-hook #'org-noter-citar-find-document-from-refs)

(add-to-list 'org-noter-find-additional-notes-functions #'org-noter-citar-find-key-from-this-file)

(provide 'org-noter-citar)
;;; org-noter-citar.el ends here
