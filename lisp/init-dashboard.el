(eye/use-package 'dashboard
                 :load-path "emacs-dashboard"
                 :ensure t
                 :config
                 (progn
                   (setq dashboard-startup-banner (expand-file-name "bin/logo.png" user-emacs-directory))
                   (setq dashboard-display-icons-p t) ;; display icons on both GUI and terminal
                   (setq dashboard-icon-type 'nerd-icons) ;; use `nerd-icons' package
                   (dashboard-setup-startup-hook)

                   )
                 )


(provide 'init-dashboard)
