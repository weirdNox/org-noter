;;; org-noter-dynamic-block.el --- Use special blocks as notes  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  c1-g

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
(require 'org-noter-core)

(defun org-noter-insert-precise-dynamic-block (&optional toggle-no-questions)
  "Insert note associated with a specific location.
This will ask you to click where you want to scroll to when you
sync the document to this note. You should click on the top of
that part. Will always create a new note.

When text is selected, it will automatically choose the top of
the selected text as the location and the text itself as the
title of the note (you may change it anyway!).

See `org-noter-insert-note' docstring for more."
  (interactive "P")
  (org-noter--with-valid-session
   (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                 (not org-noter-insert-note-no-questions)
                                               org-noter-insert-note-no-questions)))
     (org-noter-insert-dynamic-block (org-noter--get-precise-info)))))

(defun org-noter-insert-dynamic-block (&optional precise-info)
  "Insert note associated with the current location.

This command will prompt for a title of the note and then insert
it in the notes buffer. When the input is empty, a title based on
`org-noter-default-heading-title' will be generated.

If there are other notes related to the current location, the
prompt will also suggest them. Depending on the value of the
variable `org-noter-closest-tipping-point', it may also
suggest the closest previous note.

PRECISE-INFO makes the new note associated with a more
specific location (see `org-noter-insert-precise-note' for more
info).

When you insert into an existing note and have text selected on
the document buffer, the variable `org-noter-insert-selected-text-inside-note'
defines if the text should be inserted inside the note."
  (interactive)
  (org-noter--with-valid-session
   (let* ((ast (org-noter--parse-root))
          (contents (org-element-contents ast))
          (window (org-noter--get-notes-window 'force))
          (selected-text
           (pcase (org-noter--session-doc-mode session)
             ('pdf-view-mode
              (when (pdf-view-active-region-p)
                (mapconcat 'identity (pdf-view-active-region-text) ? )))

             ((or 'nov-mode 'djvu-read-mode)
              (when (region-active-p)
                (buffer-substring-no-properties (mark) (point))))))

          force-new
          (location (org-noter--doc-approx-location (or precise-info 'interactive) (gv-ref force-new)))
          (view-info (org-noter--get-view-info (org-noter--get-current-view) location)))

     (let ((inhibit-quit t))
       (with-local-quit
         (select-frame-set-input-focus (window-frame window))
         (select-window window)

         ;; IMPORTANT(nox): Need to be careful changing the next part, it is a bit
         ;; complicated to get it right...

         (let ((point (point))
               (minibuffer-local-completion-map org-noter--completing-read-keymap)
               collection default default-begin title
               (empty-lines-number (if org-noter-separate-notes-from-heading 2 1)))

           (cond
            ;; NOTE(nox): Both precise and without questions will create new notes
            ((or precise-info force-new)
             (setq default (and selected-text (replace-regexp-in-string "\n" " " selected-text))))
            (org-noter-insert-note-no-questions)
            (t
             (dolist (note-cons (org-noter--view-info-notes view-info))
               (let ((display (org-element-property :raw-value (car note-cons)))
                     (begin (org-element-property :begin (car note-cons))))
                 (push (cons display note-cons) collection)
                 (when (and (>= point begin) (> begin (or default-begin 0)))
                   (setq default display
                         default-begin begin))))))

           ;; NOTE(nox): Inserting a new note
           (let ((reference-element-cons (org-noter--view-info-reference-for-insertion view-info))
                 level)
             
             (if reference-element-cons
                 (progn
                   (cond
                    ((eq (car reference-element-cons) 'before)
                     (goto-char (org-element-property :begin (cdr reference-element-cons))))
                    ((eq (car reference-element-cons) 'after)
                     (goto-char (org-element-property :end (cdr reference-element-cons)))))
                   ;; NOTE(nox): This is here to make the automatic "should insert blank" work better.
                   (when (org-at-heading-p) (backward-char))
                   (setq level (org-element-property :level (cdr reference-element-cons))))

               (goto-char (or (org-element-map contents 'section
                                (lambda (section) (org-element-property :end section))
                                nil t org-element-all-elements)
                              (org-element-map ast 'section
                                (lambda (section) (org-element-property :end section))
                                nil t org-element-all-elements))))

               ;; (setq level (1+ (or (org-element-property :level ast) 0))))

               ;; NOTE(nox): This is needed to insert in the right place
               (unless (org-noter--no-heading-p) (outline-show-entry))
               ;; (org-noter--insert-heading level title empty-lines-number location)
               (insert
                "\n"
                (string-join (list (format "#+BEGIN: note %s"
                                           (if location
                                               (concat ":" org-noter-property-note-location
                                                       (format " %S" location))
                                             ""))
                                   (or selected-text "")
                                   "#+END:")
                             "\n")
                "\n")
               
               (when (org-noter--session-hide-other session) (org-overview))

               (setf (org-noter--session-num-notes-in-view session)
                     (1+ (org-noter--session-num-notes-in-view session)))))

           (org-show-set-visibility t)
           (org-cycle-hide-drawers 'all)
           (org-cycle-show-empty-lines t)))
       
       (when quit-flag
         ;; NOTE(nox): If this runs, it means the user quitted while creating a note, so
         ;; revert to the previous window.
         (select-frame-set-input-focus (org-noter--session-frame session))
         (select-window (get-buffer-window (org-noter--session-doc-buffer session)))))))

(defun org-dblock-write:note (params)
  (let ((location (plist-get params
                             (intern (concat ":" org-noter-property-note-location))))
        (content (plist-get params :content))
        (session org-noter--session)
        (origin-window (selected-window))
        (origin-location))
    
    (org-noter--with-valid-session
     (setq origin-location (org-noter--doc-approx-location))
     (when (and location
                (org-noter--get-location-top location)
                (org-noter--get-location-left location))
       (org-noter--doc-goto-location location)
       (with-current-buffer (org-noter--session-doc-buffer session)
         (setq content
               (pcase major-mode
                 ('pdf-view-mode (pdf-info-gettext (car location) (cdr location)))
                 ((or 'nov-mode 'djvu-read-mode)
                  (buffer-substring (org-noter--get-location-top location)
                                    (org-noter--get-location-left location))))))
       (org-noter--doc-goto-location origin-location)
       (select-window origin-window)))
    (insert content)))

(defun org-noter--get-location-dynamic-block (dblock)
  (let ((params (read (concat "(" (org-element-property :arguments dblock) ")"))))
    (format "%S" (plist-get params (intern (concat ":" org-noter-property-note-location))))))

(defun org-noter-get-containing-dynamic-block (&optional _include-root)
  (org-noter--with-valid-session
   (org-with-wide-buffer
    (let ((elt (org-element-at-point)))
      (catch 'break
        (while (org-element-property :parent elt)
          (cond
           ((eq (org-element-type elt) 'dynamic-block)
            (throw 'break elt))
           (t
            (setq elt (org-element-property :parent elt))))))))))

(add-hook 'org-noter--get-containing-element-hook #'org-noter-get-containing-dynamic-block)

(add-hook 'org-noter--get-location-property-hook #'org-noter--get-location-dynamic-block)

(provide 'org-noter-dynamic-block)
;;; org-noter-dynamic-block.el ends here

