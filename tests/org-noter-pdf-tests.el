(add-to-list 'load-path "modules")
(require 'org-noter-pdf)
(require 'org-noter-test-utils)


(defvar expected-highlight-info (make-pdf-highlight :page 747 :coords '(0.1 0.2 0.3 0.4)))

(describe "org-noter-pdf-functionality"
          ;; todo refactor 👇
          (describe "location functionality"
                    )


          (describe "pdf specific highlight functionality"
                    (before-each
                     (spy-on 'pdf-view-active-region-p :and-return-value t)
                     (spy-on 'pdf-view-active-region :and-return-value '(0.1 0.2 0.3 0.4))
                     (spy-on 'image-mode-window-get :and-return-value 747)
                     )

                    (it "can get coordinates from pdf-view"
                        (let ((highlight-info (org-noter-pdf--get-highlight)))
                          (expect 'pdf-view-active-region-p :to-have-been-called)
                          (expect highlight-info :to-equal expected-highlight-info)))

                    (describe "highlight persistence"
                              (before-each
                               (create-org-noter-test-session)
                               ;; (create-org-noter-test-session) sets up a highlight hook, so we have to reset it back.
                               ;; this might be ok for now? maybe filter out all "-core-test-" hooks instead?
                               (setq org-noter--get-highlight-location-hook '(org-noter-pdf--get-highlight))
                               )
                              (it "can take a precise note WITH a highlight appearing"
                                  (with-mock-contents
                                   mock-contents-simple-notes-file
                                   '(lambda ()
                                      (org-noter-core-test-create-session)
                                      (with-simulated-input "precise SPC note RET"
                                                            (org-noter-insert-precise-note))
                                      (ont--log-debug "%s" (buffer-string))
                                      (expect (string-match "\\:HIGHLIGHT\\:" (buffer-string))  :not :to-be nil)
                                      (expect (string-match (format "%s" expected-highlight-info) (buffer-string))  :not :to-be nil)
                                      )

                                   ))

                              )

                    )
          )
