
(require 'log4e)

;; org-noter-test logger = ont
(log4e:deflogger "ont" "ont %t [%l] %m" "%H:%M:%S")
(ont--log-enable-logging)
(ont--log-enable-debugging)
(ont--log-enable-messaging)
(ont--log-set-level 'info)
(ont--log-debug "ont")


(defvar mock-contents-simple-notes-file
  "
:PROPERTIES:
:ID:       FAKE_1
:END:
#+TITLE: Test book notes (simple)
* solove-nothing-to-hide
:PROPERTIES:
:NOTER_DOCUMENT: pubs/solove-nothing-to-hide.pdf
:END:
")

(defvar mock-contents-simple-notes-file-with-a-single-note
  ":PROPERTIES:
:ID:       FAKE_90283
:END:
#+TITLE: Test book notes

* solove-nothing-to-hide
:PROPERTIES:
:NOTER_DOCUMENT: pubs/solove-nothing-to-hide.pdf
:END:
** Note from page 1
:PROPERTIES:
:NOTER_PAGE: 99
:END:
"
)




;;;;;;;;;;;
;; helpers
(defun org-noter-core-test-create-session ()
  "Call this manually with an existing notes buffer to generate a new session"
  (org-noter--create-session (org-noter--parse-root) "pubs/solove-nothing-to-hide.pdf" org-noter-test-file))


(defun with-mock-contents (contents lambda)
  "Create a real buffer with CONTENTS and then execute the LAMBDA"
  (ont--log-debug "\n--------------------------------------------")

  ;; TODO: when an assert fails in buttercup, an exception (??) is thrown,
  ;; so temp file isnt being cleaned up. This is the sledgehammer approach.
  ;; Needs to be fixed so that it's cleaned up properly.
  (when (boundp 'org-noter-test-file)
    (progn
      (ont--log-debug (format "Removing org-noter-test-file: %s\n" org-noter-test-file))
      (delete-file org-noter-test-file)))
  (let* ((tempfile (make-temp-file "Notes" nil ".org" contents)))
    (ont--log-debug (format "Creating a tempfile: %s\n" tempfile))
    (setq org-noter-test-file tempfile)
    (ont--log-debug "Opening the file..")
    (org-mode)
    (find-file tempfile)
    (org-mode)
    (ont--log-debug "Starting the test..")
    (ont--log-debug "%s" (buffer-string))
    (funcall lambda)
    (ont--log-debug "About to kill buffer..")
    (kill-current-buffer)
    (ont--log-debug (format "Removing tempfile %s" tempfile))
    (delete-file tempfile)
    (ont--log-debug "+++++++++++++++++++++++++++++++++++++++++")
  ))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hooks - org-noter calls these

(defun org-noter-test-get-selected-text (mode)
  "⚠️org-noter-core-test-return-text
org-noter-core-test-return-text
org-noter-core-test-return-text
org-noter-core-test-return-text
org-noter-core-test-return-text
")


(defun org-noter-core-test-document-property (&optional param)
  org-noter-test-file)

(defun org-noter-core-test-view-setup-handler (&optional param)
  t)

(defun org-noter-core-test-open-document-functions (&optional doc)
  (find-file (org-noter-core-test-document-property)))

(defun org-noter-core-test-approx-location (major-mode &optional precise-info _force-new-ref)
  (cons 99 precise-info))

(defun org-noter-core-test-get-current-view (mode)
  t)

;; TODO This doesn't look right
(defun org-noter-core-test-get-precise-info (mode window)
  (list 1 2 3 4))

(defun org-noter-core-test-pretty-print-location (location)
  (format "%s" location))

(defun org-noter-core-test-add-highlight (major-mode precise-info)
  t)

(defun org-noter-core-test-get-current-view (mode)
  'org-noter-core-test-view)

(defun org-noter-core-test-get-highlight-location ()
  "HARDCODED_HIGHLIGHT_LOCATION")

(defun org-noter-core-test-pretty-print-location-for-title (location)
  "TEST PRETTY PRINT LOCATION")

(defun create-org-noter-test-session ()

  ;; if this is not set; make-session fails and the test crashes with a stack overflow.
  (setq org-noter-always-create-frame nil)

  (setq org-noter-highlight-selected-text t)

  ;; setup spies so we can verify that things have been called
  (spy-on 'org-noter-test-get-selected-text :and-call-through)
  (spy-on 'org-noter-core-test-approx-location :and-call-through)
  (spy-on 'org-noter-core-test-get-precise-info :and-call-through)
  (spy-on 'org-noter-core-test-add-highlight :and-call-through)
  (spy-on 'org-noter-core-test-get-current-view :and-call-through)

  ;; register all the hooks so we can fake a org-noter-test mode
  (add-to-list 'org-noter-get-selected-text-hook #'org-noter-test-get-selected-text)
  (add-to-list 'org-noter-parse-document-property-hook  #'org-noter-core-test-document-property)
  (add-to-list 'org-noter-set-up-document-hook #'org-noter-core-test-view-setup-handler)
  (add-to-list 'org-noter-open-document-functions #'org-noter-core-test-open-document-functions)
  (add-to-list 'org-noter--doc-approx-location-hook #'org-noter-core-test-approx-location)
  (add-to-list 'org-noter--get-current-view-hook #'org-noter-core-test-get-current-view)
  (add-to-list 'org-noter--get-precise-info-hook #'org-noter-core-test-get-precise-info)
  (add-to-list 'org-noter--pretty-print-location-hook #'org-noter-core-test-pretty-print-location)
  (add-to-list 'org-noter--pretty-print-location-for-title-hook #'org-noter-core-test-pretty-print-location-for-title)
  (add-to-list 'org-noter--add-highlight-hook  #'org-noter-core-test-add-highlight)
  (add-to-list 'org-noter--get-highlight-location-hook #'org-noter-core-test-get-highlight-location)
  )




(provide 'org-noter-test-utils)
