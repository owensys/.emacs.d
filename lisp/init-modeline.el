
;; (eye/use-package
;;  'doom-modeline
;;  :load-path '("doom-modeline" "nerd-icons" "eldoc-eval" "shrink-path" "emacs-memoize" "s" "f" "dash" "compat")
;;  :ensure t
;;  :init
;;  (progn
;;    (setq
;;     ;; display the colorful icon for ‘major-mode’.
;;     doom-modeline-major-mode-color-icon t
;;  	;;How tall the mode-line should be (only respected in GUI Emacs).
;;  	doom-modeline-height 25
;;  	;;How wide the mode-line bar should be (only respected in GUI Emacs).
;;  	doom-modeline-bar-width 4
;;  	;;don't use github notifications, because must set github.user, if not, will has a lot of emacs error process
;;  	doom-modeline-github nil
;;  	))
;;  :config
;;  (progn
;;    (doom-modeline-mode 1)
;;    ;;(cancel-timer doom-modeline--github-timer)

;;    ;; (set-face-attribute 'mode-line-inactive nil :background "gray20" :foreground "gray60")
;;    ;; (set-face-attribute 'mode-line nil :background "gray10" :foreground "gray60")

;;    ))


;;;; awesome-tray
(when is-gui
  (eye/use-package
   'awesome-tray
   :ensure t
   :load-path "awesome-tray"
   :command 'awesome-tray-mode
   :config
   (progn
     ;; Create a module to show file encoding
     (defface tray-module-encode-face
       '((((background light))
	      :foreground "#00a400" :bold t)
	     (t
	      :foreground "green3" :bold t))
       "Encode name face."
       :group 'awesome-tray)

     ;;;;
     (defun tray-module-encode-info ()
       (format "%s" buffer-file-coding-system))

     (add-to-list 'awesome-tray-module-alist
		          '("encode" . (tray-module-encode-info tray-module-encode-face)))


     ;;;;
     (defun tray-module-readonly-status-info ()
       (if buffer-read-only "🔒" ""))

     (add-to-list 'awesome-tray-module-alist '("ro" . (tray-module-readonly-status-info
                                                       tray-module-encode-face)))

     ;;;;
     ;; Create a module to show current input method
     (defun tray-module-im-info ()
       (if current-input-method
	   (format "%s" current-input-method)
	 "En"))
     (add-to-list 'awesome-tray-module-alist
		  '("im" . (tray-module-im-info)))
     ;; (add-to-list 'awesome-tray-active-modules "im")

     (setq-default mode-line-format "")

     ;; 激活模块
     (setq awesome-tray-active-modules '("ro" "buffer-name" "encode" "mode-name"))

     ;; Enable awesome-tray-mode
     (defun eye--activate-awesome-tray (theme)
       (awesome-tray-mode 1))
     (add-hook 'load-theme-after-hook #'eye--activate-awesome-tray)
     )))


(when is-terminal
;;;; modeline: encode
  (defun modeline-module-encode-info ()
    (let* ((encoding buffer-file-coding-system)
           show-str
           )
      ;; 显示简短信息
      (cond ((eq encoding 'utf-8-unix) (setq show-str "UTF8 LF"))
            ((eq encoding 'utf-8-dos) (setq show-str "UTF8 CRLF"))
            ((eq encoding 'chinese-gb18030-unix) (setq show-str "GBK LF"))
            ((eq encoding 'chinese-gb18030-dos) (setq show-str "GBK CRLF"))
            (t (setq show-str (format "%s" encoding)))
            )
      show-str
      ))

;;;; modeline: project dir
  ;; 显示在哪个git目录下，没有在git目录，则显示父目录
  (defun modeline-module-project-dir-info ()
    ;; (interactive)
    (let* ((project-dir (locate-dominating-file ".git" ".git"))
           )
      (if (not project-dir)
          (setq project-dir (file-name-nondirectory (directory-file-name default-directory)))
        (progn
          ;; 从目录中获取最后一个目录名
          (setq project-dir (file-name-nondirectory
                             (s-left (- (length project-dir) 1) project-dir)
                             )))
        )
      (format "%s" project-dir))
    )

;;;; modeline: git-info
  (defvar awesome-tray-git-command-last-time 0)
  (defvar awesome-tray-git-command-cache "")
  (defvar awesome-tray-battery-status-last-time 0)
  (defvar awesome-tray-git-update-duration 5)

  (defun awesome-tray-current-seconds ()
    (string-to-number (format-time-string "%s")))

  (defun awesome-tray-process-exit-code-and-output (program &rest args)
    "Run PROGRAM with ARGS and return the exit code and output in a list."
    (with-temp-buffer
      (list (apply 'call-process program nil (current-buffer) nil args)
            (buffer-string))))

  (defun awesome-tray-update-git-command-cache ()
    (let* ((git-info (awesome-tray-process-exit-code-and-output "git" "symbolic-ref" "--short" "HEAD"))
           (status (nth 0 git-info))
           (result (format "Git:%s" (nth 1 git-info))))
      (setq awesome-tray-git-command-cache
            (if (equal status 0)
                (replace-regexp-in-string "\n" "" result)
              ""))
      awesome-tray-git-command-cache))

  (defun awesome-tray-module-git-info ()
    (if (executable-find "git")
        (let ((current-seconds (awesome-tray-current-seconds)))
          (if (> (- current-seconds awesome-tray-git-command-last-time) awesome-tray-git-update-duration)
              (progn
                (setq awesome-tray-git-command-last-time current-seconds)
                (awesome-tray-update-git-command-cache))
            awesome-tray-git-command-cache))
      ""))


;;;; modeline: readonly status
  (defun modeline-readonly-status ()
    (if buffer-read-only
        "RO"
      ""
      ))


;;;; modeline: format
  (defvar enable-custom-mode-line t)
  (when enable-custom-mode-line
    (setq-default mode-line-format
                  (list
                   " " '(:eval (buffer-name))

                   ;; modify status, %:readonly, -:saved, *:modified
                   " %* "

                   ;; file size
                   " %I "

                   ;; narrow state
	           "%n  "

                   "[" '(:eval (modeline-module-project-dir-info)) "]  "
                   "[" '(:eval (awesome-tray-module-git-info)) "]  "
                   ;; major-mode
                   '(:eval (propertize "%m" 'face nil)) "  "
                   "[" '(:eval (modeline-module-encode-info)) "]  "

	           ;; line and column
	           "" ;; '%02' to set to 2 chars at least; prevents flickering
	           "L%02l %p %-" ;; "," "%01c"
	           ;;global-mode-string, org-timer-set-timer in org-mode need this
	           ;; (propertize "%M" 'face nil)
                   ))

    (set-face-attribute 'mode-line-inactive nil :background "color242" :foreground "brightgreen")
    (set-face-attribute 'mode-line nil :background "color253" :foreground "brightgreen")
    )
  )


(provide 'init-modeline)
