(defcustom org-noter-max-short-length 80
  "Maximum length of a short text selection"
  :group 'org-noter
  :type 'integer)

(defun phm/org-noter-insert-note (&optional precise-info)
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
defines if the text should be inserted inside the note.

Guiding principles for this (phm/) refactor
  1. The preferred title is the one the user enters in the minibuffer.
  2. Selected text should be used in the note, either as the title or in the body
  3. Refrain from making notes in the same location with the same title
  4. Precise notes generally have different locations, always make new
     precise notes
"
  (interactive)
  (org-noter--with-valid-session
   (let* ((ast (org-noter--parse-root)) (contents (org-element-contents ast))
          (window (org-noter--get-notes-window 'force))
          (selected-text (run-hook-with-args-until-success
                          'org-noter-get-selected-text-hook
                          (org-noter--session-doc-mode session)))

          force-new
          (location (org-noter--doc-approx-location (or precise-info 'interactive) (gv-ref force-new)))
          (view-info (org-noter--get-view-info (org-noter--get-current-view) location)))

     (let ((inhibit-quit t)
           (short-selected-text (if (and (> (length selected-text) 0)
                                         (<= (length selected-text) org-noter-max-short-length))
                                    selected-text)))
       (with-local-quit
         (select-frame-set-input-focus (window-frame window))
         (select-window window)

         ;; IMPORTANT(nox): Need to be careful changing the next part, it is a bit
         ;; complicated to get it right...

         (let ((point (point))
               (minibuffer-local-completion-map org-noter--completing-read-keymap)
               collection title note-body existing-note
               (default-title (or short-selected-text
                                  (replace-regexp-in-string (regexp-quote "$p$")
                                                            (org-noter--pretty-print-location location)
                                                            org-noter-default-heading-title)))
               (empty-lines-number (if org-noter-separate-notes-from-heading 2 1)))

           ;; NOTE(phm): prompt for title unless this is a precise note
           (unless precise-info
             ;; construct collection for matching existing notes
             (dolist (note-cons (org-noter--view-info-notes view-info))
               (let ((display (org-element-property :raw-value (car note-cons))))
                 (push (cons display note-cons) collection))))

           (setq collection (nreverse collection)
                 ;; prompt for title (unless no-Q's)
                 title (if org-noter-insert-note-no-questions default-title
                         (completing-read "Note: " collection nil nil nil nil default-title))
                 note-body (unless (equal title short-selected-text) selected-text)
                 ;; is this an existing note? skip for precise notes
                 existing-note (unless precise-info (cdr (assoc title collection))))

           (if existing-note
               ;; NOTE(nox): Inserting on an existing note
               (let* ((note (car existing-note))
                      (insert-before-element (cdr existing-note))
                      (has-content
                       (eq (org-element-map (org-element-contents note) org-element-all-elements
                             (lambda (element)
                               (if (org-noter--check-location-property element)
                                   'stop
                                 (not (memq (org-element-type element) '(section property-drawer)))))
                             nil t)
                           t)))
                 (when has-content (setq empty-lines-number 2))
                 (if insert-before-element
                     (goto-char (org-element-property :begin insert-before-element))
                   (goto-char (org-element-property :end note)))

                 (if (org-at-heading-p)
                     (progn
                       (org-N-empty-lines-before-current empty-lines-number)
                       (forward-line -1))
                   (unless (bolp) (insert "\n"))
                   (org-N-empty-lines-before-current (1- empty-lines-number)))

                 (when (and org-noter-insert-selected-text-inside-note note-body)
                   (if short-selected-text
                       (insert "``" note-body "''")
                     (insert "#+BEGIN_QUOTE\n" note-body "\n#+END_QUOTE"))))

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
                                (point-max))))

               (setq level (or level
                               (1+ (or (org-element-property :level ast) 0))))

               ;; NOTE(nox): This is needed to insert in the right place
               (unless (org-noter--no-heading-p) (outline-show-entry))
               (org-noter--insert-heading level title empty-lines-number location)
               (when note-body
                 (save-excursion
                   (if short-selected-text
                       (insert "``" note-body "''")
                     (insert "#+BEGIN_QUOTE\n" note-body "\n#+END_QUOTE"))))
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
         (select-window (get-buffer-window (org-noter--session-doc-buffer session))))))))

(defun phm/org-noter-insert-precise-note (&optional toggle-no-questions)
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
     (phm/org-noter-insert-note (org-noter--get-precise-info)))))


(defun phm/org-noter-insert-note-toggle-no-questions ()
  "Insert note associated with the current location.
This is like `org-noter-insert-note', except it will toggle `org-noter-insert-note-no-questions'"
  (interactive)
  (org-noter--with-valid-session
   (let ((org-noter-insert-note-no-questions (not org-noter-insert-note-no-questions)))
     (phm/org-noter-insert-note))))

(defun phm/org-noter-test-mouse-event ()
  "test code for precise notes in pdfs"
  (interactive)
  (let ((event nil)
        (window (car (window-list))))
    (while (not (and (eq 'mouse-1 (car event))
                     (eq window (posn-window (event-start event)))))
      (setq event (read-event "Click where you want the start of the note to be!")))
    (let ((col-row (posn-col-row (event-start event)))
          (win-edges (window-inside-edges))
          (display-size (image-display-size (image-get-display-property)))) ; from next fn
      (org-noter--conv-page-scroll-percentage (+ (window-vscroll) (cdr col-row))
                                              (+ (window-hscroll) (car col-row)))
      (message (format "%f %f %f %f; Disp (%f %f); Win (%d %d)"
                       (window-vscroll) (cdr col-row)
                       (window-hscroll) (car col-row)
                       (car display-size) (cdr display-size)
                       (- (nth 2 win-edges) (nth 0 win-edges))
                       (- (nth 3 win-edges) (nth 1 win-edges)))))))

(define-key org-noter-doc-mode-map (kbd "i") 'phm/org-noter-insert-note)
(define-key org-noter-doc-mode-map (kbd "M-i") 'phm/org-noter-insert-precise-note)
(define-key org-noter-doc-mode-map (kbd "C-i") 'phm/org-noter-insert-note-toggle-no-questions)
(define-key org-noter-doc-mode-map (kbd "e") 'phm/org-noter-test-mouse-event)
