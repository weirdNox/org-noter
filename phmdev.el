;; add this to org-noter-core.el
(defvar org-noter--doc-extensions nil
  "List of extensions handled by org-noter when documents are moved.
Used by `phm/org-noter--sync-doc-rename-in-notes'.")
;; add to modules/..pdf
(push "pdf" org-noter--doc-extensions)
;; add to modules/..nov
(push "epub" org-noter--doc-extensions)
;; add to modules/..dvju
(push "djvu" org-noter--doc-extensions)

(defun phm/org-noter--sync-doc-rename-in-notes (document-path &optional new-document-path)
  "Rename org-noter references to document file whose name has changed.

DOCUMENT-PATH is the original filename.
NEW-DOCUMENT-PATH is the new filename.

This advice runs after `dired-rename-file' completes successfully
on files with `file-name-extension' in `org-noter--doc-extensions'.

For notes files that have the same `file-name-base' as the
document, the notes filename will be changed, but not its
`file-name-directory'.

If the document is moved to a path above the notes file, a
warning will be issued, but the sync will proceed.  The directory
of the notes file will not be changed, as there may be other
documents referenced in the notes file.  An `org-noter' session
can still be initiated from the notes file, but not vice-versa,
nor will future renames of the document be synced in the notes
file.
"
  (when (member-ignore-case (file-name-extension document-path) org-noter--doc-extensions)
    (let* ((document-name (file-name-nondirectory document-path))
           (document-base (file-name-base document-name))
           (document-directory (file-name-directory document-path))

           (search-names (remove nil (append org-noter-default-notes-file-names
                                             (list (concat document-base ".org"))
                                             (list (run-hook-with-args-until-success 'org-noter-find-additional-notes-functions document-path)))))
           notes-files-annotating ; List of files annotating document
           notes-files) ; List of found notes files (annotating or not)

      ;; seems like the next two blocks can be combined into a single block if
      ;; the `locate-dominating-file' paths can be combined with
      ;; `org-noter-notes-search-path' check `org-noter-notes-search-path' for

      ;; annotating notes files
      (dolist (path org-noter-notes-search-path)
        (dolist (name search-names)
          (let ((notes-path (expand-file-name name path)))
            (when (file-exists-p notes-path)
              (push notes-path notes-files)
              (when (org-noter--check-if-document-is-annotated-on-file document-path notes-path)
                (push notes-path notes-files-annotating))))))

      ;; check filenames in `search-names' from the doc path up for annotating notes files
      (dolist (name search-names)
        (let ((directory (locate-dominating-file document-directory name))
              notes-path)
          (when directory
            (setq notes-path (expand-file-name name directory))
            (unless (member notes-path notes-files) (push notes-path notes-files))
            (when (org-noter--check-if-document-is-annotated-on-file document-path notes-path)
              (push notes-path notes-files-annotating)))))

      (setq notes-files-annotating (delete-dups notes-files-annotating))

      ;; in each annotating notes file, find the entry for this file and update
      ;; the document's relative path
      (dolist (notes-path notes-files-annotating)
        (with-temp-buffer
          (insert-file-contents notes-path)
          (org-with-point-at (point-min)
            (catch 'break ;stop when we find a match
              (while (re-search-forward (org-re-property org-noter-property-doc-file) nil)
                (let ((property-value (match-string 3))
                      (notes-directory (file-name-directory notes-path)))
                  (when (string-equal (expand-file-name property-value notes-directory)
                                      document-path)
                    (let ((doc-relative-name (file-relative-name new-document-path notes-directory))
                          msg)
                      (org-set-property org-noter-property-doc-file doc-relative-name)
                      (when (string-prefix-p "../" doc-relative-name) ;warn against docs that reside above notes in path
                        (setq msg
                              (format-message "Document file has moved above notes file (%s). `org-noter' will not be able to find the notes file from the new document path (%s)." notes-path doc-relative-name))
                        (display-warning 'org-noter msg :warning)))
                    (write-file notes-path nil)
                    ;; change the notes filename if it was based on the document filename
                    (if (string-equal (file-name-base notes-path) document-base)
                        (let ((new-notes-path (concat (file-name-directory notes-path)
                                                      (file-name-base new-document-path) ".org")))
                          (rename-file notes-path new-notes-path)))
                    (throw 'break t)))))))))))

(advice-add 'dired-rename-file :after #'phm/org-noter--sync-doc-rename-in-notes)


;;(dired-rename-file FILE NEWNAME OK-IF-ALREADY-EXISTS)
