(add-to-list 'load-path "modules")
(require 'org-noter-pdf)
(describe "org-noter-pdf-functionality"
          (describe "location functionality"
                    (it "can correctly recognize precise notes location"
                        (expect
                         (org-noter-pdf-check-location-property "(16 0.3073263558515699 0.7254290171606864 0.2274024738344434 0.65600624024961)")
                         :to-equal t))
                    (it "correctly rejects non location properties"
                        (expect
                         (org-noter-pdf-check-location-property "(asdf)")
                         :to-equal nil))

                    )
          )
