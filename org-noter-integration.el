(require 'org-noter)
(require 'org-pdftools)

(declare-function pdf-info-editannots "ext:pdf-info")
(declare-function pdf-annot-add-text-annotation "ext:pdf-annot")
(declare-function pdf-annot-get-id "ext:pdf-annot")

(defcustom org-noter-markup-pointer-function 'pdf-annot-add-highlight-markup-annotation
  "Color for markup pointer annotations.
Can be one of highlight/underline/strikeout/squiggly."
  :group 'org-noter
  :type 'function)

(defcustom org-noter-markup-pointer-color "#A9A9A9"
  "Color for markup pointer annotations"
  :group 'org-noter
  :type 'string)

(defcustom org-noter-markup-pointer-opacity 1.0
  "Color for markup pointer annotations"
  :group 'org-noter
  :type 'float)

(defcustom org-noter-free-pointer-icon "Circle"
  "Color for free pointer annotations. Refer to `pdf-annot-standard-text-icons`."
  :group 'org-noter
  :type 'string)

(defcustom org-noter-free-pointer-color "#FFFFFF"
  "Color for free pointer annotations"
  :group 'org-noter
  :type 'string)

(defcustom org-noter-free-pointer-opacity 1.0
  "Color for free pointer annotations"
  :group 'org-noter
  :type 'float)

(defcustom org-noter-use-pdftools-link-location t
  "When non-nil, org-pdftools link is used instead of location-cons when inserting notes."
  :group 'org-noter
  :type 'boolean)

(defcustom org-noter-use-org-id t
  "When non-nil, an org-id is generated for each heading for linking with PDF annotations and record entry parents."
  :group 'org-noter
  :type 'boolean)

(defcustom org-noter-export-to-pdf t
  "When non-nil, PDF annotation contents will include both org-id of original notes and org-id of its parent.

To use this, `org-noter-use-org-id' has to be t."
  :group 'org-noter
  :type 'boolean)

(defcustom org-noter-export-to-pdf-with-structure t
  "When non-nil, PDF annotation contents will include both org-id of original notes and org-id of its parent.

To use this, `org-noter-use-org-id' has to be t."
  :group 'org-noter
  :type 'boolean)

(defcustom org-noter-use-unique-org-id t
  "When non-nil, an org-id is generated for each heading for linking with PDF annotations and record entry parents."
  :group 'org-noter
  :type 'boolean)

(cl-defstruct org-noter-pdftools--location
  path page height annot-id search-string original-property)

(defun org-noter-pdftools--location-link-p (location)
  (and location
       (stringp location)
       (string-prefix-p "pdftools:" location)))

(defun org-noter--location-cons-to-link (location)
  (cond ((consp location)
         (concat
          "::"
          (number-to-string
           (car location))
          "++"
          (format "%.2f" (cdr location))))
        ((integerp location)
         (concat
          "::"
          (number-to-string
           (car location))))))

(defun org-noter--location-link-to-cons (location)
  "Convert a org-pdftools link to old location cons."
  (cons (org-noter-pdftools--location-page location) (or (org-noter-pdftools--location-height location) 0.0)))

;; --------------------------------------------------------------------------------
;; NOTE(nox): Interface
(defun org-noter-pdftools--check-link (property)
  (org-noter-pdftools--location-link-p property))

(defun org-noter-pdftools--parse-link (property)
  (when (org-noter-pdftools--location-link-p property)
    (string-match "\\(.*\\)::\\([0-9]*\\)\\(\\+\\+\\)?\\([[0-9]\\.*[0-9]*\\)?\\(;;\\|\\$\\$\\)?\\(.*\\)?" property)
    (let ((path (match-string 1 property))
          (page (match-string 2 property))
          (height (match-string 4 property))
          annot-id search-string)
      (cond ((string-equal (match-string 5 property) ";;")
             (setq annot-id (match-string 6 property)))
            ((string-equal (match-string 5 property) "$$")
             (setq search-string (replace-regexp-in-string "%20" " " (match-string 6 property)))))
      (make-org-noter-pdftools--location
       :path path
       :page (and page (string-to-number page))
       :height (and height (string-to-number height))
       :annot-id annot-id
       :search-string search-string
       :original-property property))))

(defun org-noter-pdftools--pretty-print-location (location)
  (and (org-noter-pdftools--location-p location)
       (org-noter-pdftools--location-original-property location)))

(defun org-noter-pdftools--convert-to-location-cons (location)
  (when (org-noter-pdftools--location-p location)
    (org-noter--location-link-to-cons location)))

(defun org-noter-pdftools--doc-goto-location (mode location)
  (when (and (eq mode 'pdf-view-mode) (org-noter-pdftools--location-p location))
    (when (org-noter-pdftools--location-page location)
      (pdf-view-goto-page (org-noter-pdftools--location-page location)))
    (when (org-noter-pdftools--location-height location)
      (image-set-window-vscroll
       (round (/ (* (org-noter-pdftools--location-height location) (cdr (pdf-view-image-size)))
                 (frame-char-height)))))
    (when (org-noter-pdftools--location-annot-id location)
      (pdf-annot-show-annotation (pdf-info-getannot (org-noter-pdftools--location-annot-id location)) t))
    (when (org-noter-pdftools--location-search-string location)
      (isearch-mode t)
      (isearch-yank-string (org-noter-pdftools--location-search-string location)))
    t))

(defun org-noter-pdftools--note-after-tipping-point (point location view)
  (when (org-noter-pdftools--location-p location)
    (cons t (org-noter--note-after-tipping-point point (org-noter--location-link-to-cons location) view))))

(defun org-noter-pdftools--relative-position-to-view (location view)
  (when (org-noter-pdftools--location-p location)
    (org-noter--relative-position-to-view (org-noter--location-link-to-cons location) view)))

(defun org-noter-pdftools--get-precise-info (mode)
  (when (eq mode 'pdf-view-mode)
    (let ((org-pdftools-free-pointer-icon org-noter-free-pointer-icon)
          (org-pdftools-free-pointer-color org-noter-free-pointer-color)
          (org-pdftools-free-pointer-opacity org-noter-free-pointer-opacity)
          (org-pdftools-markup-pointer-color org-noter-markup-pointer-color)
          (org-pdftools-markup-pointer-opacity org-noter-markup-pointer-opacity)
          (org-pdftools-markup-pointer-function org-noter-markup-pointer-function))
      (org-noter-pdftools--parse-link (org-pdftools-get-link t)))))

(defun org-noter-pdftools--doc-approx-location (mode precise-info force-new-ref)
  (org-noter--with-valid-session
   (when (eq mode 'pdf-view-mode)
     (cond ((or (numberp precise-info) (not precise-info))
            (org-noter-pdftools--parse-link
             (concat "pdftools:" (expand-file-name (org-noter--session-property-text session)) "::"
                     (number-to-string (image-mode-window-get 'page))
                     (when precise-info (concat "++" (number-to-string precise-info))))))
           ((org-noter-pdftools--location-p precise-info) precise-info)
           ((eq precise-info 'interactive)
            (when force-new-ref
              (setf (gv-deref force-new-ref) t))
            (org-noter-pdftools--get-precise-info mode))
           (t (error "Invalid pdftools precise-info case: %s" precise-info))))))

(defun org-noter-pdftools--insert-heading ()
  (let ((location-property (org-entry-get nil org-noter-property-note-location)))
    (when (string-match ".*;;\\(.*\\)" location-property)
      (org-noter--with-valid-session
       (let ((id (match-string 1 location-property)))
         (if org-noter-use-org-id
             (org-entry-put nil "ID"
                            (if org-noter-use-unique-org-id
                                (concat
                                 (org-noter--session-property-text session)
                                 "-"
                                 id)
                              id))))))))

(dolist (pair '((org-noter--check-location-property-hook   . org-noter-pdftools--check-link)
                (org-noter--parse-location-property-hook   . org-noter-pdftools--parse-link)
                (org-noter--pretty-print-location-hook     . org-noter-pdftools--pretty-print-location)
                (org-noter--convert-to-location-cons-hook  . org-noter-pdftools--convert-to-location-cons)
                (org-noter--doc-goto-location-hook         . org-noter-pdftools--doc-goto-location)
                (org-noter--note-after-tipping-point-hook  . org-noter-pdftools--note-after-tipping-point)
                (org-noter--relative-position-to-view-hook . org-noter-pdftools--relative-position-to-view)
                (org-noter--get-precise-info-hook          . org-noter-pdftools--get-precise-info)
                (org-noter--doc-approx-location-hook       . org-noter-pdftools--doc-approx-location)
                (org-noter-insert-heading-hook             . org-noter-pdftools--insert-heading)))
  (add-hook (car pair) (cdr pair)))

;; --------------------------------------------------------------------------------
;; NOTE(nox): User commands
(defun org-noter-convert-old-org-heading ()
  "Covert an old org heading to a new one for compatiblility."
  (interactive)
  (org-noter--with-valid-session
   (cond ((eq (org-noter--session-doc-mode
               session)
              'pdf-view-mode)
          (let* ((document-property (org-noter--session-property-text
                                     session)))
            (let* ((location (org-noter--location-property
                              (org-entry-get
                               nil
                               org-noter-property-note-location)))
                   (path document-property)
                   (page (if (consp location)
                             (car location)
                           location))
                   (height (if (consp location)
                               (cdr location)
                             0.0))
                   (pos `(0 . ,(round
                                (*
                                 (cdr (with-current-buffer
                                          (org-noter--session-doc-buffer
                                           session)
                                        (pdf-view-image-size)))
                                 height))))
                   (annot-id (symbol-name
                              (pdf-annot-get-id
                               (save-excursion
                                 (with-selected-window
                                     (org-noter--get-doc-window)
                                   (pdf-view-goto-page page)
                                   (funcall-interactively
                                    #'pdf-annot-add-text-annotation
                                    pos
                                    org-pdftools-free-pointer-icon
                                    `((color . ,org-pdftools-free-pointer-color)
                                      (opacity . ,org-pdftools-free-pointer-opacity)))))))))
              (org-entry-put
               nil
               org-noter-property-note-location
               (concat
                "pdftools:"
                path
                (org-noter--location-cons-to-link
                 location)
                ";;"
                annot-id))
              (when org-noter-use-org-id
                (org-entry-put
                 nil
                 "ID"
                 (if org-noter-use-unique-org-id
                     (concat
                      document-property
                      "-"
                      annot-id)
                   annot-id)))
              (when org-noter-export-to-pdf
                (let* ((content (if (and (> (org-current-level) 2)
                                         org-noter-export-to-pdf-with-structure)
                                    (let ((parent-id (save-excursion
                                                       (org-up-heading-safe)
                                                       (org-id-get))))
                                      (if parent-id
                                          (concat
                                           "#+PROPERTY: PARENT "
                                           parent-id
                                           "\n"
                                           (save-excursion
                                             (org-back-to-heading nil)
                                             (buffer-substring-no-properties
                                              (point)
                                              (org-end-of-subtree nil t))))))
                                  (save-excursion
                                    (org-back-to-heading nil)
                                    (buffer-substring-no-properties
                                     (point)
                                     (org-end-of-subtree nil t))))))
                  (with-selected-window
                      (org-noter--get-doc-window)
                    (pdf-info-editannot
                     (intern annot-id)
                     `((contents . ,content)))))))))
         (t
          (error
           "This command is only supported on PDF Tools")))))

(defun org-noter-convert-old-notes ()
  "Convert old notes (location cons based) to new format (link based)."
  (interactive)
  (org-noter--with-valid-session
   (goto-char (point-min))
   (when (org-before-first-heading-p)
     (org-next-visible-heading 1))
   (while (not (eq (point) (point-max)))
     (org-next-visible-heading 1)
     (goto-char (point-at-eol))
     (let ((prop (org-entry-get
                  nil
                  org-noter-property-note-location)))
       (if (and prop
                (not (string-prefix-p
                      "pdftools:"
                      prop)))
           (call-interactively
            #'org-noter-convert-old-org-heading))))))

(defun org-noter-jump-to-note (a)
  "Jump from a PDF annotation A to the corresponding org heading."
  (interactive (list
                (with-selected-window
                    (org-noter--get-doc-window)
                  (pdf-annot-read-annotation
                   "Left click the annotation "))))
  (when (not org-noter-use-org-id)
    "You have to enable `org-noter-use-org-id'!")
  (org-noter--with-valid-session
   (pdf-annot-show-annotation a t)
   (let ((id (symbol-name
              (pdf-annot-get-id a))))
     (select-window
      (org-noter--get-notes-window))
     (condition-case-unless-debug
         nil
         (progn
           (require 'org-id)
           (goto-char
            (cdr (org-id-find-id-in-file
                  (if org-noter-use-unique-org-id
                      (concat
                       (org-noter--session-property-text
                        session)
                       "-"
                       id)
                    id)
                  buffer-file-name))))
       (error nil))
     t)))

;; TODO(nox): Implement interface for skeleton creation
(defun org-noter-create-skeleton ()
  "Create notes skeleton with the PDF outline or annotations.
Only available with PDF Tools."
  (interactive)
  (org-noter--with-valid-session
   (cond
    ((eq (org-noter--session-doc-mode session) 'pdf-view-mode)
     (let* ((ast (org-noter--parse-root))
            (top-level (org-element-property :level ast))
            (options '(("Outline" . (outline))
                       ("Annotations" . (annots))
                       ("Both" . (outline annots))))
            answer output-data)
       (with-current-buffer (org-noter--session-doc-buffer session)
         (setq answer (assoc (completing-read "What do you want to import? " options nil t) options))

         (when (memq 'outline answer)
           (dolist (item (pdf-info-outline))
             (let ((type  (alist-get 'type item))
                   (page  (alist-get 'page item))
                   (depth (alist-get 'depth item))
                   (title (alist-get 'title item))
                   (top   (alist-get 'top item))
                   pdftools-link path)
               (when (and (eq type 'goto-dest)
                          (> page 0))
                 (when org-noter-use-pdftools-link-location
                   (setq path (file-relative-name
                               (expand-file-name
                                (org-noter--session-property-text
                                 session))
                               org-pdftools-root-dir))
                   (if title
                       (setq pdftools-link
                             (concat
                              "pdftools:"
                              path
                              "::"
                              (number-to-string page)
                              "++"
                              (number-to-string top)
                              "$$"
                              (replace-regexp-in-string
                               " "
                               "%20"
                               title)))
                     (setq pdftools-link
                           (concat
                            "pdftools:"
                            path
                            "::"
                            (number-to-string page)
                            "++"
                            (number-to-string top)))))
                 (push
                  (vector
                   title
                   (if org-noter-use-pdftools-link-location pdftools-link
                     (cons page top))
                   (1+ depth)
                   nil)
                  output-data)))))

         (when (memq 'annots answer)
           (let ((possible-annots (list '("Highlights" . highlight)
                                        '("Underlines" . underline)
                                        '("Squigglies" . squiggly)
                                        '("Text notes" . text)
                                        '("Strikeouts" . strike-out)
                                        '("Links" . link)
                                        '("ALL" . all)))
                 chosen-annots insert-contents pages-with-links)
             (while (> (length possible-annots) 1)
               (let* ((chosen-string (completing-read "Which types of annotations do you want? "
                                                      possible-annots nil t))
                      (chosen-pair (assoc chosen-string possible-annots)))
                 (cond ((eq (cdr chosen-pair) 'all)
                        (dolist (annot possible-annots)
                          (when (and (cdr annot) (not (eq (cdr annot) 'all)))
                            (push (cdr annot) chosen-annots)))
                        (setq possible-annots nil))
                       ((cdr chosen-pair)
                        (push (cdr chosen-pair) chosen-annots)
                        (setq possible-annots (delq chosen-pair possible-annots))
                        (when (= 1 (length chosen-annots)) (push '("DONE") possible-annots)))
                       (t
                        (setq possible-annots nil)))))

             (setq insert-contents (y-or-n-p "Should we insert the annotations contents? "))

             (dolist (item (pdf-info-getannots))
               (let* ((type (alist-get 'type item))
                      (page (alist-get 'page item))
                      (edges (or (org-noter--pdf-tools-edges-to-region (alist-get 'markup-edges item))
                                 (alist-get 'edges item)))
                      (top (nth 1 edges))
                      (item-subject (alist-get 'subject item))
                      (item-contents (alist-get 'contents item))
                      name contents pdftools-link id path)
                 (when org-noter-use-pdftools-link-location
                   (setq path
                         (file-relative-name
                          (expand-file-name
                           (org-noter--session-property-text
                            session))
                          org-pdftools-root-dir))
                   (setq id (symbol-name (alist-get 'id item)))
                   (setq pdftools-link (concat "pdftools:" path "::"
                                               (number-to-string page) "++"
                                               (number-to-string top) ";;"
                                               id)))

                 (when (and (memq type chosen-annots) (> page 0))
                   (if (eq type 'link)
                       (cl-pushnew page pages-with-links)
                     (setq name (cond ((eq type 'highlight) "Highlight")
                                      ((eq type 'underline) "Underline")
                                      ((eq type 'squiggly) "Squiggly")
                                      ((eq type 'text) "Text note")
                                      ((eq type 'strike-out) "Strikeout")))

                     (when insert-contents
                       (setq contents (cons (pdf-info-gettext page edges)
                                            (and (or (and item-subject (> (length item-subject) 0))
                                                     (and item-contents (> (length item-contents) 0)))
                                                 (concat (or item-subject "")
                                                         (if (and item-subject item-contents) "\n" "")
                                                         (or item-contents ""))))))

                     (push (vector (format "%s on page %d" name page) (if org-noter-use-pdftools-link-location
                                                                          pdftools-link
                                                                        (cons page top)) 'inside contents)
                           output-data)))))

             (dolist (page pages-with-links)
               (let ((links (pdf-info-pagelinks page))
                     type)
                 (dolist (link links)
                   (setq type (alist-get 'type link))
                   (unless (eq type 'goto-dest) ;; NOTE(nox): Ignore internal links
                     (let* ((edges (alist-get 'edges link))
                            (title (alist-get 'title link))
                            (top (nth 1 edges))
                            (target-page (alist-get 'page link))
                            target heading-text pdftools-link path)
                       (when org-noter-use-pdftools-link-location
                         (setq path
                               (file-relative-name
                                (expand-file-name
                                 (org-noter--session-property-text
                                  session))
                                org-pdftools-root-dir))
                         (setq pdftools-link (concat "pdftools:" path "::"
                                                     (number-to-string page) "++"
                                                     (number-to-string top))))
                       (unless (and title (> (length title) 0)) (setq title (pdf-info-gettext page edges)))

                       (cond
                        ((eq type 'uri)
                         (setq target (alist-get 'uri link)
                               heading-text (format "Link on page %d: [[%s][%s]]" page target title)))

                        ((eq type 'goto-remote)
                         (setq target (concat "file:" (alist-get 'filename link))
                               heading-text (format "Link to document on page %d: [[%s][%s]]" page target title))
                         (when target-page
                           (setq heading-text (concat heading-text (format " (target page: %d)" target-page)))))

                        (t (error "Unexpected link type")))

                       (push
                        (vector
                         heading-text
                         (if org-noter-use-pdftools-link-location
                             pdftools-link
                           (cons page top))
                         'inside
                         nil)
                        output-data))))))))


         (when output-data
           (if (memq 'annots answer)
               (setq output-data
                     (sort output-data
                           (lambda (e1 e2)
                             (or (not (aref e1 1))
                                 (and (aref e2 1)
                                      (org-noter--compare-locations '< (aref e1 1) (aref e2 1)))))))
             (setq output-data (nreverse output-data)))

           (push (vector "Skeleton" nil 1 nil) output-data)))

       (with-current-buffer (org-noter--session-notes-buffer session)
         ;; NOTE(nox): org-with-wide-buffer can't be used because we want to reset the
         ;; narrow region to include the new headings
         (widen)
         (save-excursion
           (goto-char (org-element-property :end ast))

           (let (last-absolute-level
                 title location relative-level contents
                 level)
             (dolist (data output-data)
               (setq title          (aref data 0)
                     location       (aref data 1)
                     relative-level (aref data 2)
                     contents       (aref data 3))

               (if (symbolp relative-level)
                   (setq level (1+ last-absolute-level))
                 (setq last-absolute-level (+ top-level relative-level)
                       level last-absolute-level))

               (org-noter--insert-heading level title nil location)

               (when (car contents)
                 (org-noter--insert-heading (1+ level) "Contents")
                 (insert (car contents)))
               (when (cdr contents)
                 (org-noter--insert-heading (1+ level) "Comment")
                 (insert (cdr contents)))))

           (setq ast (org-noter--parse-root))
           (org-noter--narrow-to-root ast)
           (goto-char (org-element-property :begin ast))
           (outline-hide-subtree)
           (org-show-children 2)))))

    (t (error "This command is only supported on PDF Tools.")))))
