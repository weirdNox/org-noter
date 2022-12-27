(add-to-list 'load-path "modules")
(require 'org-noter-pdf)
(require 'with-simulated-input)

(defvar mock-contents-simple-notes-file
  "* solove-nothing-to-hide
:PROPERTIES:
:NOTER_DOCUMENT: pubs/solove-nothing-to-hide.pdf
:END:
")



(defun with-mock-contents (contents lambda)
  "Create a real buffer with CONTENTS and then execute the LAMBDA"
  (message "\n--------------------------------------------")

  ;; TODO: when an assert fails in buttercup, an exception (??) is thrown,
  ;; so temp file isnt being cleaned up. This is the sledgehammer approach.
  ;; Needs to be fixed so that it's cleaned up properly.
  (when (boundp 'org-noter-test-file)
    (progn
      (message (format "Removing org-noter-test-file: %s\n" org-noter-test-file))
      (delete-file org-noter-test-file)))
  (let* ((tempfile (make-temp-file "Notes" nil ".org" contents)))
    (message (format "Creating a tempfile: %s\n" tempfile))
    (setq org-noter-test-file tempfile)
    (message "Opening the file..")
    (org-mode)
    (find-file tempfile)
    (org-mode)
    (message "Starting the test..")
    (message "%s" (buffer-string))
    (funcall lambda)
    (message "About to kill buffer..")
    (kill-current-buffer)
    (message (format "Removing tempfile %s" tempfile))
    (delete-file tempfile)
    (message "+++++++++++++++++++++++++++++++++++++++++")
  ))

(defun org-noter-test-get-selected-text (mode)
  (message "🧪org-noter-core-test-return-text")
  "⚠️org-noter-core-test-return-text
org-noter-core-test-return-text
org-noter-core-test-return-text
org-noter-core-test-return-text
org-noter-core-test-return-text
")


(defun org-noter-core-test-document-property (&optional param)
  (message "🧪org-noter-core-test-document-property %s" param)
  org-noter-test-file)

(defun org-noter-core-test-view-setup-handler (&optional param)
  (message "org-noter-core-test-view-setup-handler")
  t)

(defun org-noter-core-test-open-document-functions (&optional doc)
  (message "org-noter-core-test-open-document-functions")
  (find-file (org-noter-core-test-document-property)))

(defun org-noter-core-test-approx-location (major-mode &optional precise-info _force-new-ref)
  (message "approx-location %s" precise-info)
  (cons 99 precise-info))

(defun org-noter-core-test-get-current-view (mode)
  t)

(defun org-noter-core-test-get-precise-info (mode)
  (message "🧪org-noter-core-test-get-precise-info %s" mode)
  (list 1 2 3 4))

(defun org-noter-core-test-pretty-print-location (location)
  (format "%s" location))

(defun org-noter-core-test-create-session ()
  (org-noter--create-session (org-noter--parse-root) "NOTER_DOCUMENT" org-noter-test-file))

(describe "org-noter-core"
                    (before-each
                     ;; if this is not set; make-session fails and the test crashes with a stack overflow.
                     (setq org-noter-always-create-frame nil)


                     ;; setup spies so we can verify that things have been called
                     (spy-on 'org-noter-test-get-selected-text :and-call-through)
                     (spy-on 'org-noter-core-test-approx-location :and-call-through)
                     (spy-on 'org-noter-core-test-get-precise-info :and-call-through)



                     (add-to-list 'org-noter-get-selected-text-hook #'org-noter-test-get-selected-text)
                     (add-to-list 'org-noter-parse-document-property-hook  #'org-noter-core-test-document-property)
                     (add-to-list 'org-noter-set-up-document-hook #'org-noter-core-test-view-setup-handler)
                     (add-to-list 'org-noter-open-document-functions #'org-noter-core-test-open-document-functions)
                     (add-to-list 'org-noter--doc-approx-location-hook #'org-noter-core-test-approx-location)
                     (add-to-list 'org-noter--get-current-view-hook #'org-noter-core-test-get-current-view)
                     (add-to-list 'org-noter--get-precise-info-hook #'org-noter-core-test-get-precise-info)
                     (add-to-list 'org-noter--pretty-print-location-hook #'org-noter-core-test-pretty-print-location)
                     )

          (describe "note taking functionality"
                    (it "can parse a note file ast that is not empty"
                        (with-mock-contents
                         mock-contents-simple-notes-file
                         '(lambda () (let ((mock-ast (org-noter--parse-root)))
                                           (message "%s" mock-ast)
                                           (expect mock-ast :not :to-be nil)))))

                    (it "can take a basic note"
                        (with-mock-contents
                         mock-contents-simple-notes-file
                         '(lambda ()
                            (org-noter-core-test-create-session)
                            (org-noter-insert-note nil "NEW NOTE")
                            (message "with note: %s" (buffer-string))
                            (expect 'org-noter-test-get-selected-text :to-have-been-called)
                            (expect (string-match "Notes for page" (buffer-string))  :not :to-be nil))))

                    (it "can take a precise note"
                        (with-mock-contents
                         mock-contents-simple-notes-file
                         '(lambda ()
                            (org-noter-core-test-create-session)
                            (with-simulated-input "precise SPC note RET"
                                                  (org-noter-insert-precise-note))
                            (message "with note: %s" (buffer-string))
                            (expect (string-match "precise note" (buffer-string))  :not :to-be nil))))

                    (it "precise note has precise data"
                        (with-mock-contents
                         mock-contents-simple-notes-file
                         '(lambda ()
                            (org-noter-core-test-create-session)
                            (with-simulated-input "precise SPC note RET"
                                                  (org-noter-insert-precise-note))
                            (message "with note: %s" (buffer-string))

                            (expect (string-match "NOTER_PAGE:" (buffer-string))  :not :to-be nil)
                            (expect (string-match "BEGIN_QUOTE" (buffer-string))  :not :to-be nil)
                            (expect 'org-noter-core-test-get-precise-info :to-have-been-called)
                            )))




          )
)
