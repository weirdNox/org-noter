;;; org-noter-org-roam --- org-roam support for org-noter       -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Dmitry Markushevich

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

;; This module adds org-roam integration to org-noter

;;; Code:

(defun org-noter--get-filename-for-org-roam-node ()
  "Use the standard org-roam interface to select an existing node or create a new one and return a path to it"
  (let* ((templates (list (append (car org-roam-capture-templates) '(:immediate-finish t))))
         (all-files-containing-notes (org-noter--get-files-containing-notes))
         (node (org-roam-node-read nil (lambda (node)
                                          (member (org-roam-node-file node)
                                                all-files-containing-notes))))
         (_ (org-roam-capture-
             :node node
             :info nil
             :templates templates
             :props nil))
         (node-id (org-roam-node-id node))
         (file-path-for-new-entry (org-roam-node-file (org-roam-node-from-id node-id))))
    (message "%s" file-path-for-new-entry)
    file-path-for-new-entry))


(defun org-noter--create-session-from-document-file-supporting-org-roam (&optional arg doc-path)
  "TBD."
  (let* ((file-path-for-org-roam-node (org-noter--get-filename-for-org-roam-node))
         (_ (message "[d] opening up notes: %s doc: %s" file-path-for-org-roam-node doc-path))
         (top-level-heading-for-doc-position (with-current-buffer (find-file-noselect file-path-for-org-roam-node)
                                               (org-noter--find-create-top-level-heading-for-doc doc-path (file-name-base doc-path)))))
    (message "going to pos: %s" top-level-heading-for-doc-position)
    (with-current-buffer (find-file-noselect file-path-for-org-roam-node)
    (goto-char top-level-heading-for-doc-position)
    (org-noter))))




(defun org-noter--find-top-level-heading-for-document-path (doc-path)
  "Given publication path, DOC-PATH tries to see if the current buffer has a
top level (\"NOTER_DOCUMENT\") heading for it. It returns the point for the heading (if found)
`nil' otherwise."
  (let ((found-heading-position nil))
    (org-with-point-at (point-min)
      (condition-case nil
          ;; look for NOTER_DOCUMENT property that matches the doc-path
          (while (and (not found-heading-position)
                      (re-search-forward (org-re-property org-noter-property-doc-file)))
            (let ((current-file-name (expand-file-name (match-string 3)))
                  (looking-for-filename (expand-file-name doc-path)))
              (when (file-equal-p current-file-name looking-for-filename)
                (setq found-heading-position (point)))))
        (search-failed   ;; when re=search-forward hits the end it throws an error which we should catch
         (message "This buffer doesn't seem to have a matching NOTER_DOCUMENT heading.") nil)))
    found-heading-position))


(defun org-noter--find-create-top-level-heading-for-doc (doc-path desired-heading)
  "In current buffer, looks for a top level heading for document at DOC-PATH.
If one is not found, creates one and returns it's position"
    (let* ((top-level-heading-for-doc-position (org-noter--find-top-level-heading-for-document-path doc-path)))
      ;; does this buffer have a top level notes heading for this document?
      (if (eq top-level-heading-for-doc-position nil)
        (org-noter--create-notes-heading desired-heading doc-path)
      top-level-heading-for-doc-position)))


;; TODO How is this different from org-noter--insert-heading?
;; org-noter--insert-heading doesn't deal with top level headings.
(defun org-noter--create-notes-heading (notes-heading document-path)
  "Create a top level notes heading for the document along with the path to the backing document.
Return the point where the heading was inserted."
  (cl-assert notes-heading t "notes-heading cannot be nil. we can't insert a nil heading.")
  (goto-char (point-max))
  (insert (if (save-excursion (beginning-of-line) (looking-at "[[:space:]]*$")) "" "\n")
          "* " notes-heading )
  (org-entry-put nil org-noter-property-doc-file
                 (expand-file-name document-path))
  (point))



(provide 'org-noter-org-roam)
