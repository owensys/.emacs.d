;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "~/.emacs.d/eye-packages/use-package-2.4.1")
  (require 'use-package))

(setq package-archives '(("gnu"   . "http://elpa.zilongshanren.com/gnu/")
                         ("melpa" . "http://elpa.zilongshanren.com/stable-melpa/")))

(setq use-package-always-ensure t)

(setq package-user-dir "~/.emacs.d/eye-packages")



(use-package smex)
(use-package treemacs)
