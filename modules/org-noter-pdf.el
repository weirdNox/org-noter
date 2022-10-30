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
(require 'org-noter)

(defun org-noter-pdf-approx-location-cons (major-mode &optional precise-info _force-new-ref)
  (when (memq major-mode '(doc-view-mode pdf-view-mode))
    (cons (image-mode-window-get 'page) (if (and (listp precise-info)
                                                 (numberp (car precise-info))
                                                 (numberp (cadr precise-info)))
                                            precise-info 0))))

(defun org-noter-get-buffer-file-name-pdf (&optional major-mode)
  "Return the file naming backing the document buffer"
  (bound-and-true-p pdf-file-name))


(defun org-noter-pdf-check-location-property (&optional property)
  "Check if PROPERTY is a valid location property"
  (equal 5 (length (read property))))


(add-to-list 'org-noter--check-location-property-hook #'org-noter-pdf-check-location-property)
(add-to-list 'org-noter--doc-approx-location-hook #'org-noter-pdf-approx-location-cons)

(defun org-noter-pdf-view-setup-handler (major-mode)
  (when (eq major-mode 'pdf-view-mode)
    ;; (setq buffer-file-name document-path)
    (pdf-view-mode)
    (add-hook 'pdf-view-after-change-page-hook 'org-noter--doc-location-change-handler nil t)
    t))

(add-to-list 'org-noter-set-up-document-hook #'org-noter-pdf-view-setup-handler)

(defun org-noter-doc-view-setup-handler (major-mode)
  (when (eq major-mode 'doc-view-mode)
    ;; (setq buffer-file-name document-path)
    (doc-view-mode)
    (advice-add 'doc-view-goto-page :after 'org-noter--location-change-advice)
    t))

(add-to-list 'org-noter-set-up-document-hook #'org-noter-doc-view-setup-handler)

(defun org-noter-pdf--pretty-print-location (location)
  (org-noter--with-valid-session
   (when (memq (org-noter--session-doc-mode session) '(doc-view-mode pdf-view-mode))
     (format "%s" (if (or (not (org-noter--get-location-top location)) (<= (org-noter--get-location-top location) 0))
                      (car location)
                    location)))))

(add-to-list 'org-noter--pretty-print-location-hook #'org-noter-pdf--pretty-print-location)

(defun org-noter-pdf--get-precise-info (major-mode)
  (when (eq major-mode 'pdf-view-mode)
    (if (pdf-view-active-region-p)
        (let ((edges (pdf-view-active-region)))
          (car edges))

      (while (not (and (eq 'mouse-1 (car event))
                       (eq window (posn-window (event-start event)))))
        (setq event (read-event "Click where you want the start of the note to be!")))
      (let ((col-row (posn-col-row (event-start event))))
        (org-noter--conv-page-scroll-percentage (+ (window-vscroll) (cdr col-row))
                                                (+ (window-hscroll) (car col-row)))))))

(add-to-list 'org-noter--get-precise-info-hook #'org-noter-pdf--get-precise-info)

(defun org-noter-doc--get-precise-info (major-mode)
  (when (eq major-mode 'doc-view-mode)
    (while (not (and (eq 'mouse-1 (car event))
                     (eq window (posn-window (event-start event)))))
      (setq event (read-event "Click where you want the start of the note to be!")))
    (org-noter--conv-page-scroll-percentage (+ (window-vscroll)
                                               (cdr (posn-col-row (event-start event)))))))

(add-to-list 'org-noter--get-precise-info-hook #'org-noter-doc--get-precise-info)


(defun org-noter-pdf-goto-location (mode location)
  (when (memq mode '(doc-view-mode pdf-view-mode))
    (let ((top (org-noter--get-location-top location))
          (window (org-noter--get-doc-window))
          (left (org-noter--get-location-left location)))

      (if (eq mode 'doc-view-mode)
          (doc-view-goto-page (org-noter--get-location-page location))
        (pdf-view-goto-page (org-noter--get-location-page location))
        ;; NOTE(nox): This timer is needed because the tooltip may introduce a delay,
        ;; so syncing multiple pages was slow
        (when (>= org-noter-arrow-delay 0)
          (when org-noter--arrow-location (cancel-timer (aref org-noter--arrow-location 0)))
          (setq org-noter--arrow-location
                (vector (run-with-idle-timer org-noter-arrow-delay nil 'org-noter--show-arrow)
                        window
                        top
                        left))))
      (image-scroll-up (- (org-noter--conv-page-percentage-scroll top)
                          (window-vscroll))))))

(add-to-list 'org-noter--doc-goto-location-hook #'org-noter-pdf-goto-location)

(defun org-noter-pdf--get-current-view (mode)
  (when (memq mode '(doc-view-mode pdf-view-mode))
    (vector 'paged (car (org-noter-pdf-approx-location-cons mode)))))

(add-to-list 'org-noter--get-current-view-hook #'org-noter-pdf--get-current-view)

(defun org-noter-pdf--get-selected-text (mode)
  (when (and (eq mode 'pdf-view-mode)
             (pdf-view-active-region-p))
    (mapconcat 'identity (pdf-view-active-region-text) ? )))

(add-to-list 'org-noter-get-selected-text-hook #'org-noter-pdf--get-selected-text)

(defun org-noter-create-skeleton-pdf (mode)
  "Create notes skeleton with the PDF outline or annotations."
  (when (eq mode 'pdf-view-mode)
    (org-noter--with-valid-session
     (let* ((ast (org-noter--parse-root))
            (top-level (or (org-element-property :level ast) 0))
            (options '(("Outline" . (outline))
                       ("Annotations" . (annots))
                       ("Both" . (outline annots))))
            answer output-data)
       (with-current-buffer (org-noter--session-doc-buffer session)
         (setq answer (assoc (completing-read "What do you want to import? " options nil t) options))

         (when (memq 'outline answer)
           (dolist (item (pdf-info-outline))
             (let ((type (alist-get 'type item))
                   (page (alist-get 'page item))
                   (depth (alist-get 'depth item))
                   (title (alist-get 'title item))
                   (top (alist-get 'top item)))
               (when (and (eq type 'goto-dest) (> page 0))
                 (push (vector title (cons page top) (1+ depth) nil) output-data)))))

         (when (memq 'annots answer)
           (let ((possible-annots (list '("Highlights" . highlight)
                                        '("Underlines" . underline)
                                        '("Squigglies" . squiggly)
                                        '("Text notes" . text)
                                        '("Strikeouts" . strike-out)
                                        '("Links" . link)
                                        '("ALL" . all)))
                 chosen-annots insert-contents pages-with-links)
             (while (> (length possible-annots) 1)
               (let* ((chosen-string (completing-read "Which types of annotations do you want? "
                                                      possible-annots nil t))
                      (chosen-pair (assoc chosen-string possible-annots)))
                 (cond ((eq (cdr chosen-pair) 'all)
                        (dolist (annot possible-annots)
                          (when (and (cdr annot) (not (eq (cdr annot) 'all)))
                            (push (cdr annot) chosen-annots)))
                        (setq possible-annots nil))
                       ((cdr chosen-pair)
                        (push (cdr chosen-pair) chosen-annots)
                        (setq possible-annots (delq chosen-pair possible-annots))
                        (when (= 1 (length chosen-annots)) (push '("DONE") possible-annots)))
                       (t
                        (setq possible-annots nil)))))

             (setq insert-contents (y-or-n-p "Should we insert the annotations contents? "))

             (dolist (item (pdf-info-getannots))
               (let* ((type (alist-get 'type item))
                      (page (alist-get 'page item))
                      (edges (or (org-noter--pdf-tools-edges-to-region (alist-get 'markup-edges item))
                                 (alist-get 'edges item)))
                      (top (nth 1 edges))
                      (item-subject (alist-get 'subject item))
                      (item-contents (alist-get 'contents item))
                      name contents)
                 (when (and (memq type chosen-annots) (> page 0))
                   (if (eq type 'link)
                       (cl-pushnew page pages-with-links)
                     (setq name (cond ((eq type 'highlight) "Highlight")
                                      ((eq type 'underline) "Underline")
                                      ((eq type 'squiggly) "Squiggly")
                                      ((eq type 'text) "Text note")
                                      ((eq type 'strike-out) "Strikeout")))

                     (when insert-contents
                       (setq contents (cons (pdf-info-gettext page edges)
                                            (and (or (and item-subject (> (length item-subject) 0))
                                                     (and item-contents (> (length item-contents) 0)))
                                                 (concat (or item-subject "")
                                                         (if (and item-subject item-contents) "\n" "")
                                                         (or item-contents ""))))))

                     (push (vector (format "%s on page %d" name page) (cons page top) 'inside contents)
                           output-data)))))

             (dolist (page pages-with-links)
               (let ((links (pdf-info-pagelinks page))
                     type)
                 (dolist (link links)
                   (setq type (alist-get 'type link))
                   (unless (eq type 'goto-dest) ;; NOTE(nox): Ignore internal links
                     (let* ((edges (alist-get 'edges link))
                            (title (alist-get 'title link))
                            (top (nth 1 edges))
                            (target-page (alist-get 'page link))
                            target heading-text)

                       (unless (and title (> (length title) 0)) (setq title (pdf-info-gettext page edges)))

                       (cond
                        ((eq type 'uri)
                         (setq target (alist-get 'uri link)
                               heading-text (format "Link on page %d: [[%s][%s]]" page target title)))

                        ((eq type 'goto-remote)
                         (setq target (concat "file:" (alist-get 'filename link))
                               heading-text (format "Link to document on page %d: [[%s][%s]]" page target title))
                         (when target-page
                           (setq heading-text (concat heading-text (format " (target page: %d)" target-page)))))

                        (t (error "Unexpected link type")))

                       (push (vector heading-text (cons page top) 'inside nil) output-data))))))))


         (when output-data
           (if (memq 'annots answer)
               (setq output-data
                     (sort output-data
                           (lambda (e1 e2)
                             (or (not (aref e1 1))
                                 (and (aref e2 1)
                                      (org-noter--compare-locations '< (aref e1 1) (aref e2 1)))))))
             (setq output-data (nreverse output-data)))

           (push (vector "Skeleton" nil 1 nil) output-data)))

       (with-current-buffer (org-noter--session-notes-buffer session)
         ;; NOTE(nox): org-with-wide-buffer can't be used because we want to reset the
         ;; narrow region to include the new headings
         (widen)
         (save-excursion
           (goto-char (org-element-property :end ast))

           (let (last-absolute-level
                 title location relative-level contents
                 level)
             (dolist (data output-data)
               (setq title (aref data 0)
                     location (aref data 1)
                     relative-level (aref data 2)
                     contents (aref data 3))

               (if (symbolp relative-level)
                   (setq level (1+ last-absolute-level))
                 (setq last-absolute-level (+ top-level relative-level)
                       level last-absolute-level))

               (org-noter--insert-heading level title)

               (when location
                 (org-entry-put nil org-noter-property-note-location (org-noter--pretty-print-location location)))

               (when org-noter-doc-property-in-notes
                 (org-entry-put nil org-noter-property-doc-file (org-noter--session-property-text session))
                 (org-entry-put nil org-noter--property-auto-save-last-location "nil"))

               (when (car contents)
                 (org-noter--insert-heading (1+ level) "Contents")
                 (insert (car contents)))
               (when (cdr contents)
                 (org-noter--insert-heading (1+ level) "Comment")
                 (insert (cdr contents)))))

           (setq ast (org-noter--parse-root))
           (org-noter--narrow-to-root ast)
           (goto-char (org-element-property :begin ast))
           (outline-hide-subtree)
           (org-show-children 2)))
       output-data))))

(add-to-list 'org-noter-create-skeleton-functions #'org-noter-create-skeleton-pdf)

(add-to-list 'org-noter--parse-location-property-hook #'org-noter-pdf--parse-location)

(defun org-noter-pdf--parse-location (arg)
  "return a pdf location from an existing property. expecting (page left)"
  (let* ((location (car (read-from-string arg))))
    location))

(defun org-noter-pdf--create-missing-annotation ()
  "Add a highlight from a selected note."
  (let* ((location (org-noter--parse-location-property (org-noter--get-containing-element))))
    (with-selected-window (org-noter--get-doc-window)
      (org-noter-pdf-goto-location 'pdf-view-mode location)
      (pdf-annot-add-highlight-markup-annotation (cdr location)))))


(provide 'org-noter-pdf)
;;; org-noter-pdf.el ends here
