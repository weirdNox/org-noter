
(add-to-list 'load-path "modules")
(require 'org-noter-pdf)
(require 'with-simulated-input)
(require 'org-noter-test-utils)


(describe "org-noter very custom behavior"
          (before-each
           (create-org-noter-test-session)
           )
          (describe "with advice"
                    (before-each
                     (setq org-noter-max-short-selected-text-length 700000)

                     (define-advice org-noter--insert-heading (:after (level title &optional newlines-number location) add-full-body-quote)
                       "Advice for org-noter--insert-heading.

  When inserting a precise note insert the text of the note in the body as an org mode QUOTE block.

  =org-noter-max-short-length= should be set to a large value to short circuit the normal behavior:
  =(setq org-noter-max-short-length 80000)="

                       ;; this tells us it's a precise note that's being invoked.
                       (if (consp location)
                           (insert (format "#+BEGIN_QUOTE\n%s\n#+END_QUOTE" title))))
                     (create-org-noter-test-session)
                     )
                    (after-each
                     (setq org-noter-max-short-selected-text-length 80)
                     (advice-remove #'org-noter--insert-heading 'org-noter--insert-heading@add-full-body-quote)
                     )
                    (it "should insert the highlighted text as an org-mode QUOTE when advice is enabled."
                        (with-mock-contents
                         mock-contents-simple-notes-file
                         '(lambda ()
                            (org-noter-core-test-create-session)
                            ;; we're not specifying the note title
                            (with-simulated-input "RET"
                                                  (org-noter-insert-precise-note))
                            (let* ((expected-heading (regexp-quote (format "** %s" (org-trim (replace-regexp-in-string "\n" " " (org-noter-test-get-selected-text nil)))))))
                              (expect (string-match "HARDCODED_HIGHLIGHT_LOCATION" (buffer-string))  :not :to-be nil)
                              (expect (string-match "BEGIN_QUOTE" (buffer-string))  :not :to-be nil)
                              (expect (string-match "END_QUOTE" (buffer-string))  :not :to-be nil)
                              (expect (string-match  expected-heading  (buffer-string))  :not :to-be nil))))))


          (describe "without advice"
                    (it "should revert back to standard title"
                        (with-mock-contents
                         mock-contents-simple-notes-file
                         '(lambda ()
                            (org-noter-core-test-create-session)
                            (with-simulated-input "RET"
                                                  (org-noter-insert-precise-note))
                            (expect (string-match  "\\*\\* Notes for page" (buffer-string))  :not :to-be nil))))))
