;; no ask when M-x load-theme
(setq custom-safe-themes t)

;;;; load-theme后face再设置
;; Code block face，和doom-theme加载会有冲突
;; @see https://emacs-china.org/t/topic/12520
(defcustom load-theme-before-hook nil
 "Functions to run before load theme."
 :type 'hook)

(defcustom load-theme-after-hook nil
 "Functions to run after load theme."
 :type 'hook)

(defun load-theme-hook-wrapper (origin-func theme &rest args)
 "A wrapper of hooks around `load-theme'."
 (mapc #'disable-theme custom-enabled-themes)
 (run-hook-with-args 'load-theme-before-hook theme)
 (apply origin-func theme args)
 (run-hook-with-args 'load-theme-after-hook theme))

(advice-add 'load-theme :around #'load-theme-hook-wrapper)

(defun eye-after-load-theme-setup (theme)
  (message "setup after load theme")
  ;; (set-face-attribute 'mode-line nil :background (face-attribute 'default :background))
  (if (eye-current-theme-is-dark)
      (progn
        (set-face-attribute 'font-lock-comment-face nil :foreground "tan3")
        (set-face-attribute 'header-line nil :height 160 :background "#051e59" :foreground "gray60")) ;; dark #003045
    (progn
      (set-face-attribute 'font-lock-comment-face nil :foreground "dark red")
      (set-face-attribute 'header-line nil :height 160 :background "#f3e0d5" :foreground "gray36"))
    ))
(add-hook 'load-theme-after-hook #'eye-after-load-theme-setup)

(require 'init-modeline) ;; 先加载modeline配置，如果放到load theme之后，modeline会没有背景颜色


(eye/use-package
 'ef-themes
 :ensure t
 :load-path '("ef-themes")
 :config
 (progn
   (setq ef-themes-to-toggle '(ef-spring ef-winter))

   ;;
   (if is-gui
       (let* ((hh (string-to-int (format-time-string "%H" (current-time)))))
         (if (or (>= hh 17) (<= hh 6))
             (load-theme 'ef-winter t)
           (load-theme 'ef-spring t)
           ))
     (load-theme 'wombat t))

   (setq current-theme 'ef-spring)
   (defun eye/switch-theme ()
     (interactive)
     (if (eq current-theme 'ef-spring)
         (progn
           (load-theme 'ef-winter t)
           (setq current-theme 'ef-winter)
           )
       (progn
         (load-theme 'ef-spring t)
         (setq current-theme 'ef-spring)
         )
       )
     )
   ))


(provide 'init-theme)
