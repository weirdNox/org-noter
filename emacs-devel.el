(require 'cask "/opt/homebrew/share/emacs/site-lisp/cask/cask.el")
(cask-initialize ".")

(setq mac-option-modifier 'meta)

(push (expand-file-name ".") load-path)
(require 'org-noter)
