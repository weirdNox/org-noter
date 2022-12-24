(add-to-list 'load-path "modules")
(require 'org)
(require 'org-noter-pdf)

(defvar mock-contents-simple-notes-file
  "* solove-nothing-to-hide
:PROPERTIES:
:NOTER_DOCUMENT: pubs/solove-nothing-to-hide.pdf
:END:
")

(defun with-mock-contents (contents lambda)
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

(describe "org-noter-core"
          (describe "note taking functionality"
                    (it "can parse a note file ast that is not empty"
                        (with-mock-contents
                         mock-contents-simple-notes-file
                         '(lambda () (let ((mock-ast (org-noter--parse-root)))
                                           (message "%s" mock-ast)
                                           (expect mock-ast :not :to-be nil)))
                            )
                         )
                        )
                    )
