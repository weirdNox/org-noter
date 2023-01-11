(add-to-list 'load-path "modules")
(require 'org-noter-pdf)
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
                        (let ((highlight-info (org-noter-pdf-get-highlight-location))
                              (expected-highlight-info (ht ('PAGE 747)
                                                           ('TYPE 'PDF-HIGHLIGHT)
                                                           ('COORDS '(0.1 0.2 0.3 0.4)))))

                          (expect 'pdf-view-active-region-p :to-have-been-called)
                          (expect highlight-info :to-equal (ht->plist expected-highlight-info))))
                    )



)
