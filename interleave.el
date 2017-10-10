;;; interleave.el --- Interleave PDFs                     -*- lexical-binding: t; -*-
(require 'cl-lib)

(defconst interleave--property-pdf-file "INTERLEAVE_PDF"
  "Name of the property which specifies the PDF file.")

(cl-defstruct interleave--session frame org-file-path pdf-file-path notes-buffer pdf-buffer)
(defvar interleave--sessions nil
  "List of Interleave sessions")

(defun interleave ()
  (interactive)
  (when (eq major-mode 'org-mode)
    (when (org-before-first-heading-p)
      (error "Interleave must be issued inside a heading."))
    (let ((org-file-path (buffer-file-name))
          (pdf-property (org-entry-get nil interleave--property-pdf-file t))
          pdf-file-path pdf-file-name session)
      (when (stringp pdf-property) (setq pdf-file-path (expand-file-name pdf-property)))
      (unless (and pdf-file-path (not (file-directory-p pdf-file-path)) (file-readable-p pdf-file-path))
        (setq pdf-file-path (expand-file-name
                             (read-file-name
                              "No INTERLEAVE_PDF property found. Please specify a PDF path: "
                              nil nil t)))
        (when (or (file-directory-p pdf-file-path) (not (file-readable-p pdf-file-path)))
          (error "Invalid file path."))
        (setq pdf-property (if (y-or-n-p "Do you want a relative file name? ")
                               (file-relative-name pdf-file-path)
                             pdf-file-path))
        (org-entry-put nil pdf-property interleave--property-pdf-file))
      (dolist (session interleave--sessions)
        (when (string= (interleave--session-pdf-file-path session)
                       pdf-file-path)
          (error "%s is already being Interleaved" pdf-file-path)))
      (setq pdf-file-name (file-name-nondirectory pdf-file-path)
            session
            (make-interleave--session
             :frame (make-frame `((name . ,(format "Emacs - Interleave %s" pdf-file-name))
                                  (fullscreen . maximized)))
             :org-file-path org-file-path
             :pdf-file-path pdf-file-path
             :notes-buffer (make-indirect-buffer (current-buffer) (generate-new-buffer-name
                                                                   (format "Interleave - Notes of %s"
                                                                           pdf-file-name))
                                                 t)
             :pdf-buffer (make-indirect-buffer (find-file-noselect pdf-file-path)
                                               (generate-new-buffer-name
                                                (format "Interleave - %s"
                                                        pdf-file-name))
                                               t)))
      (with-current-buffer (interleave--session-pdf-buffer session)
        (setq buffer-file-name pdf-file-name
              kill-buffer-hook nil))
      (with-current-buffer (interleave--session-notes-buffer session)
        (goto-char (org-find-property interleave--property-pdf-file pdf-property))
        (org-show-entry)
        (org-show-children)
        (let* ((heading (org-element-at-point))
               (heading-contents-end )
               (properties-begin (org-element-property ':contents-begin heading))
               (properties (progn (goto-char properties-begin)
                                  (org-element-at-point)))
               (properties-end (org-element-property ':end properties)))
          (when (eq properties-end (org-element-property ':contents-end heading))
            (save-excursion
              (goto-char properties-end)
              (insert "\n")))
          (save-excursion
            (narrow-to-region properties-end
                              (progn (org-end-of-subtree t t)
                                     (when (and (org-at-heading-p) (not (eobp))) (backward-char 1))
                                     (point))))))
      (with-selected-frame (interleave--session-frame session)
        (let ((pdf-window (selected-window))
              (notes-window (split-window-right)))
          ;; TODO(nox): Option to customize this
          (set-window-buffer pdf-window (interleave--session-pdf-buffer session))
          (set-window-dedicated-p pdf-window t)
          (set-window-buffer notes-window (interleave--session-notes-buffer session))
          (set-window-dedicated-p notes-window t)))
      (push session interleave--sessions))))
