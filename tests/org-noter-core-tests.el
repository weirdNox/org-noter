(add-to-list 'load-path "modules")
(require 'org-noter-pdf)
(require 'with-simulated-input)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hooks - org-noter calls these

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

(defun org-noter-core-test-get-precise-info (mode window)
  (message "🧪org-noter-core-test-get-precise-info %s" mode)
  (list 1 2 3 4))

(defun org-noter-core-test-pretty-print-location (location)
  (format "%s" location))

(defun org-noter-core-test-highlight-location (major-mode precise-info)
  t)

(defun org-noter-core-test-get-current-view (mode)
  'org-noter-core-test-view)

(defun org-noter-core-test-get-highlight-location ()
  "HARDCODED_HIGHLIGHT_LOCATION")

(describe "org-noter-core"
                    (before-each
                     ;; if this is not set; make-session fails and the test crashes with a stack overflow.
                     (setq org-noter-always-create-frame nil)


                     ;; setup spies so we can verify that things have been called
                     (spy-on 'org-noter-test-get-selected-text :and-call-through)
                     (spy-on 'org-noter-core-test-approx-location :and-call-through)
                     (spy-on 'org-noter-core-test-get-precise-info :and-call-through)
                     (spy-on 'org-noter-core-test-highlight-location :and-call-through)
                     (spy-on 'org-noter-core-test-get-current-view :and-call-through)

                     (add-to-list 'org-noter-get-selected-text-hook #'org-noter-test-get-selected-text)
                     (add-to-list 'org-noter-parse-document-property-hook  #'org-noter-core-test-document-property)
                     (add-to-list 'org-noter-set-up-document-hook #'org-noter-core-test-view-setup-handler)
                     (add-to-list 'org-noter-open-document-functions #'org-noter-core-test-open-document-functions)
                     (add-to-list 'org-noter--doc-approx-location-hook #'org-noter-core-test-approx-location)
                     (add-to-list 'org-noter--get-current-view-hook #'org-noter-core-test-get-current-view)
                     (add-to-list 'org-noter--get-precise-info-hook #'org-noter-core-test-get-precise-info)
                     (add-to-list 'org-noter--pretty-print-location-hook #'org-noter-core-test-pretty-print-location)
                     (add-to-list 'org-noter--pretty-print-location-hook #'org-noter-core-test-pretty-print-location)
                     (add-to-list 'org-noter-highlight-precise-note-hook #'org-noter-core-test-highlight-location)

                     )

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (describe "note taking functionality"
                    ;; checking to make sure that `with-mock-contents` works fine.
                    (it "can parse a note file ast that is not empty"
                        (with-mock-contents
                         mock-contents-simple-notes-file
                         '(lambda () (let ((mock-ast (org-noter--parse-root)))
                                           (message "%s" mock-ast)
                                           (expect mock-ast :not :to-be nil)))))

                    ;; basic note should insert a default heading
                    (it "can take a basic note"
                        (with-mock-contents
                         mock-contents-simple-notes-file
                         '(lambda ()
                            (org-noter-core-test-create-session)
                            (org-noter-insert-note nil "NEW NOTE")
                            (message "with note: %s" (buffer-string))
                            (expect 'org-noter-test-get-selected-text :to-have-been-called)
                            (expect (string-match "Notes for page" (buffer-string))  :not :to-be nil))))

                    ;; enter a heading when taking a precise note; expect the heading to be there.
                    (it "can take a precise note"
                        (with-mock-contents
                         mock-contents-simple-notes-file
                         '(lambda ()
                            (org-noter-core-test-create-session)
                            (with-simulated-input "precise SPC note RET"
                                                  (org-noter-insert-precise-note))
                            (message "with note: %s" (buffer-string))
                            (expect (string-match "precise note" (buffer-string))  :not :to-be nil))))

                    ;; there should be precise data in the note properties when entering a precise note
                    (it "precise note has precise data"
                        (with-mock-contents
                         mock-contents-simple-notes-file
                         '(lambda ()
                            (org-noter-core-test-create-session)
                            (with-simulated-input "precise SPC note RET"
                                                  (org-noter-insert-precise-note))
                            (expect (string-match "NOTER_PAGE:" (buffer-string)) :not :to-be nil)
                            (expect (string-match "BEGIN_QUOTE" (buffer-string)) :not :to-be nil)
                            (expect 'org-noter-core-test-get-precise-info :to-have-been-called)
                            )))

                    ;; highlight code should be called when a precise note is entered
                    (it "precise note calls the highlight hook"
                        (with-mock-contents
                         mock-contents-simple-notes-file
                         '(lambda ()
                            (org-noter-core-test-create-session)
                            (with-simulated-input "precise SPC note RET"
                                                  (org-noter-insert-precise-note))
                            (expect 'org-noter-core-test-highlight-location :to-have-been-called)
                            (expect (spy-calls-all-args 'org-noter-core-test-highlight-location)
                                    :to-equal
                                    '((org-mode
                                      (1 2 3 4)))))))

                    ;; hit C-g when entering a note; expect no highlight
                    (it "precise note DOES NOT call the highlight hook when the note is aborted"
                        (with-mock-contents
                         mock-contents-simple-notes-file
                         '(lambda ()
                            (org-noter-core-test-create-session)
                            ;; this is how you trap a C-g
                            (condition-case nil
                                (with-simulated-input "C-g" (org-noter-insert-precise-note))
                              (quit nil))
                            (expect 'org-noter-core-test-highlight-location :not :to-have-been-called))))

          )

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (describe "session creation"
                    ;; check that the narrowed buffer is named correctly
                    (it "narrowed buffer is named correctly"
                        (with-mock-contents
                         mock-contents-simple-notes-file-with-a-single-note
                         '(lambda ()
                            (org-noter-core-test-create-session)
                            (let* ((session org-noter--session))
                              (expect (buffer-name (org-noter--session-notes-buffer session)) :to-equal "Notes of solove-nothing-to-hide")
                              ))))

                    ;; check that session properties are set correctly
                    (it "session properties are set correctly"
                        (with-mock-contents
                         mock-contents-simple-notes-file-with-a-single-note
                         '(lambda ()
                            (org-noter-core-test-create-session)
                            (let* ((session org-noter--session))
                              (message "0000000000000000000000 %s" (type-of (org-noter--session-display-name session)))
                              (expect (org-noter--session-property-text session) :to-equal "pubs/solove-nothing-to-hide.pdf")
                              (expect (org-noter--session-display-name session) :to-equal "solove-nothing-to-hide")
                              (expect (org-noter--session-notes-file-path session) :to-equal org-noter-test-file)
                              (expect (buffer-file-name (org-noter--session-notes-buffer session)) :to-equal org-noter-test-file)
                              ;; TODO: Need test-specific-major mode somehow?
                              ;; (expect (org-noter--session-doc-mode session) :to-equal 'org-core-test)
                              ))))

                    )


          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (describe "view-info"
                    (it "can get view info"
                        (with-mock-contents
                         mock-contents-simple-notes-file-with-a-single-note
                         '(lambda ()
                            (org-noter-core-test-create-session)
                            (let* ((view-info (org-noter--get-view-info (org-noter--get-current-view))))
                              (message "%s" view-info)
                              (expect 'org-noter-core-test-get-current-view :to-have-been-called)
                              ))))
                    )

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (describe "locations"
                    (defvar test-precise-location '(3 1 . 0.1))
                    (defvar test-simple-location '(3 1))
                    (defvar test-extra-precise-location '(4 1 0.1 0.2 0.3))

                    (describe "precise locations"
                              (it "can get page from a precise location"
                                  (expect (org-noter--get-location-page test-precise-location) :to-equal 3))

                              (it "can get top from a precise location"
                                  (expect (org-noter--get-location-top test-precise-location) :to-equal 1))

                              (it "can get left from a precise location"
                                  (expect (org-noter--get-location-left test-precise-location) :to-equal 0.1))
                              )

                    (describe "simple locations"

                              (it "doesn't get a left location for simple location"
                                  (expect (org-noter--get-location-left test-simple-location) :to-equal nil)
                              )

                              (it "can get top from a simple location"
                                  (expect (org-noter--get-location-top test-simple-location) :to-equal 1))

                              (it "can get page from a simple location"
                                  (expect (org-noter--get-location-page test-simple-location) :to-equal 3))
                              )

                    (describe "extra precise locations"
                              (it "can get page from an extra precise location"
                                  (expect (org-noter--get-location-page test-extra-precise-location) :to-equal 4))

                              (it "can get top from an extra precise location"
                                  (expect (org-noter--get-location-top test-extra-precise-location) :to-equal 1))


                              (it "can get left from an extra precise location"
                                  (expect (org-noter--get-location-left test-extra-precise-location) :to-equal 0.1)))
                    )

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (describe "persistent highlights"
                    (describe "no hooks are setup for precise note highlights"
                              ;; if no hooks for highlights are setup we expect no :HIGHLIGHT: property
                              (it "can take a precise note without a highlight appearing"
                                  (with-mock-contents
                                   mock-contents-simple-notes-file
                                   '(lambda ()
                                      (org-noter-core-test-create-session)
                                      (with-simulated-input "precise SPC note RET"
                                                            (org-noter-insert-precise-note))
                                      (message "--- no highlight with note: %s" (buffer-string))
                                      (expect (string-match ":HIGHLIGHT:" (buffer-string))  :to-be nil)))))


                    (describe "hooks for persistent highlights are setup"
                              ;; setup hooks for highlighting
                              (before-each
                               (add-to-list 'org-noter--get-highlight-location-hook #'org-noter-core-test-get-highlight-location)
                               (spy-on 'org-noter-core-test-get-highlight-location :and-call-through)
                               )
                              ;; now that the hooks for highlights are setup, we expect :HIGHLIGHT: property to appear.
                              (it "can take a precise note WITH a highlight appearing"
                                  (with-mock-contents
                                   mock-contents-simple-notes-file
                                   '(lambda ()
                                      (org-noter-core-test-create-session)
                                      (with-simulated-input "precise SPC note RET"
                                                            (org-noter-insert-precise-note))
                                      (message "with note: %s" (buffer-string))
                                      (expect (string-match "\\:HIGHLIGHT\\:" (buffer-string))  :not :to-be nil)
                                      (expect (string-match "HARDCODED_HIGHLIGHT_LOCATION" (buffer-string))  :not :to-be nil)))))

                    )





)
