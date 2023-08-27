(eye/use-package
 'magit
 :ensure nil
 :load-path '("dash" "compat" "transient/lisp" "with-editor" "with-editor/lisp" "magit/lisp")
 :command '((magit-status . "magit-status"))
 :config
 (progn
   (setq git-commit-summary-max-length 80)

   ;; 在新 frame 中打开 magit-status 设置显示 magit buffer 的函数
   (when is-gui
     (defun magit-display-buffer-pop-up-frame (buffer)
       (if (with-current-buffer buffer (eq major-mode 'magit-status-mode))
	       (display-buffer buffer
			               '((display-buffer-reuse-window
                              display-buffer-pop-up-frame) ;; 在新的 frame 中显示
                             (reusable-frames . t)))
         (magit-display-buffer-traditional buffer))) ;; magit-display-buffer-traditional 是默认的函数

     (setq magit-display-buffer-function #'magit-display-buffer-pop-up-frame)
     (define-key magit-mode-map (kbd "q") 'delete-frame) ;; 自动关闭 frame
     )
   )
 )


(provide 'init-magit)
