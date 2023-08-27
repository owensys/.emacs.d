
;;;; writeroom-mode
(eye/use-package
 'writeroom-mode
 :ensure t
 :load-path '("writeroom-mode" "visual-fill-column")
 :command '(writeroom-mode global-writeroom-mode)
 :init
 (progn
   ;; (setq writeroom-fullscreen-effect 'maximized) ;; 不全屏，只最大化window
   (setq writeroom-fullscreen-effect nil ;; 不全屏
         writeroom-maximize-window nil ;; 不要最大化
         writeroom-width 100 ;; 写的区域宽度，默认80
         writeroom-mode-line nil ;; modeline enable ?
         )
   ;; (setq writeroom-major-modes '(text-mode prog-mode Info-mode dired-mode conf-mode makefile-mode org-mode))

   (defun writeroom-mode-on ()
     (interactive)
     (add-hook 'prog-mode-hook 'writeroom-mode)
     (add-hook 'org-mode-hook 'writeroom-mode) ;; orgmode开启writeroom-mode
     (writeroom-mode))
   (defun writeroom-mode-off ()
     (interactive)
     (remove-hook 'prog-mode-hook 'writeroom-mode)
     (remove-hook 'org-mode-hook 'writeroom-mode)
     (writeroom-mode -1))
   )
 :config
 (progn
   ;; (global-writeroom-mode t)
   )
 )

(provide 'init-writeroom)
