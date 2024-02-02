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
  ;; https://www.tusij.com/color/palette
  ;; (set-face-attribute 'mode-line nil :background (face-attribute 'default :background))
  ;; (set-face-attribute 'tab-bar nil :background  "#e3e8ee")
  ;; (set-face-attribute 'tab-bar-tab nil :background  "#e3e8ee" :box '(:line-width -2 :color "#e3e8ee") :foreground "gray50")
  ;; (set-face-attribute 'tab-bar-tab-inactive nil :background  "#d7dde9" :box '(:line-width -2 :color "#d7dde9"))
  ;; (set-face-attribute 'default nil :background "#073642")
  ;; (set-face-attribute 'default nil :background "#180614")
  (if (eye-current-theme-is-dark)
      (progn
        (set-face-attribute 'font-lock-comment-face nil :foreground "tan3")
        ;; (set-face-attribute 'header-line nil :height 160 :background "#051e59" :foreground "gray60") ;; dark #003045
        )
    (progn
      (set-face-attribute 'font-lock-comment-face nil :foreground "dark red")
      ;; (set-face-attribute 'header-line nil :height 160 :background "#f3e0d5" :foreground "gray36")
      )
    ))
(add-hook 'load-theme-after-hook #'eye-after-load-theme-setup)

(require 'init-modeline) ;; 先加载modeline配置，如果放到load theme之后，modeline会没有背景颜色

;; (eye/use-package 'naysayer
;;                  :ensure nil
;;                  :load-path "naysayer-theme.el"
;;                  :init
;;                  (progn
;;                    (add-to-list 'custom-theme-load-path (concat eye-packages-dir "/naysayer-theme.el"))
;;                    (add-hook 'after-init-hook (lambda ()
;;                                                 (load-theme 'naysayer t)))
;;                    ))

(require 'init-tab-bar)

(eye/use-package
 'ef-themes
 :load-path '("ef-themes")
 :command '(ef-themes-select ef-themes-toggle)
 :init
 (progn
   (add-to-list 'custom-theme-load-path (concat eye-packages-dir "/ef-themes"))
   (defun eye-load-startup-theme ()
     (if is-gui
         (load-theme 'ef-elea-dark t)
         ;; (let* ((hh (string-to-int (format-time-string "%H" (current-time)))))
         ;;   (if (or (>= hh 17) (<= hh 6))
         ;;       (load-theme 'ef-elea-dark t)
         ;;     (load-theme 'ef-elea-light t)
         ;;     ))
       (load-theme 'wombat t))
     )
   (add-hook 'after-init-hook #'eye-load-startup-theme)
   )
 :config
 (progn
   (setq ef-themes-to-toggle '(ef-melissa-dark ef-melissa-light))
   ))


(provide 'init-theme)
