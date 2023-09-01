 ;;;; header-line
(defun eye-header-line-setup(&optional theme)
  (setq-default header-line-format
                '(" "
                  (:eval (if buffer-file-name (buffer-file-name) "%b")) ;; buffer name or file path
                  " "
                  (:eval (if buffer-read-only "🔒")) ;; readonly state
                  "%n" ;; narrow state
                  ;;" %I "
                  (:eval (format " %s  " buffer-file-coding-system))
                  (:eval (string-replace "-mode" "" (format "%s" major-mode))
                         ;;"%-"
                         )
                  ))
  )

(defun eye-mode-line-setup (&optional theme)
  (setq-default mode-line-format "")
  (set-face-attribute 'mode-line nil :height 0.1 :background "dark red")
  (set-face-attribute 'mode-line-inactive nil :height 0.1)
  )

(add-hook 'after-init-hook #'eye-mode-line-setup)
(add-hook 'after-init-hook #'eye-header-line-setup)
(add-hook 'load-theme-after-hook #'eye-mode-line-setup)
(add-hook 'load-theme-after-hook #'eye-header-line-setup)


(provide 'init-modeline)
