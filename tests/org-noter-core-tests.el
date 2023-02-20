(add-to-list 'load-path "modules")
(require 'org-noter)
(require 'with-simulated-input)
(require 'org-noter-test-utils)


(describe "org-noter-core"
          (before-each
           (create-org-noter-test-session)
           )

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (describe "note taking functionality"
                    ;; checking to make sure that `with-mock-contents` works fine.
                    (it "can parse a note file ast that is not empty"
                        (with-mock-contents
                         mock-contents-simple-notes-file
                         '(lambda () (let ((mock-ast (org-noter--parse-root)))
                                           (expect mock-ast :not :to-be nil)))))

                    ;; basic note should insert a default heading
                    (it "can take a basic note"
                        (with-mock-contents
                         mock-contents-simple-notes-file
                         '(lambda ()
                            (org-noter-core-test-create-session)
                            (let ((org-noter-insert-note-no-questions t))
                              (org-noter-insert-note nil "NEW NOTE"))
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
                            (expect 'org-noter-core-test-add-highlight :to-have-been-called))))

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
                            (expect 'org-noter-core-test-add-highlight :not :to-have-been-called))))

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
                              (before-each
                               (setq org-noter--get-highlight-location-hook '())
                               )
                              (it "can take a precise note without a highlight appearing"
                                  (with-mock-contents
                                   mock-contents-simple-notes-file
                                   '(lambda ()
                                      (org-noter-core-test-create-session)
                                      (with-simulated-input "precise SPC note RET"
                                                            (org-noter-insert-precise-note))
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
                                      (expect (string-match "\\:HIGHLIGHT\\:" (buffer-string))  :not :to-be nil)
                                      (expect (string-match "HARDCODED_HIGHLIGHT_LOCATION" (buffer-string))  :not :to-be nil)))))
                    )





)
