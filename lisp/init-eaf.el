;;;;eaf
(when (and is-linux (>= emacs-major-version 29) (fboundp 'json-parse-buffer))
  (eye/use-package
   'eaf
   :ensure nil
   :load-path '("emacs-application-framework" "emacs-application-framework/core" "emacs-application-framework/app/git")
   :config
   (progn
     (require 'eaf)
     (require 'eaf-terminal)
     (require 'eaf-pdf-viewer)
     (require 'eaf-git)
     (require 'eaf-browser)
     (require 'eaf-rss-reader)

     (setq eaf-proxy-type "http")
     (setq eaf-proxy-host "192.168.1.3")
     (setq eaf-proxy-port "9008")
     )
   )
  )

(provide 'init-eaf)
