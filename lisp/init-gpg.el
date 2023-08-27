(eye/use-package
 'epa
 :ensure t
 :config
 (progn
   ;; 没有gui时，在emacs中输入密码
   (setq epa-pinentry-mode 'loopback)
   ;; 只输入一次密码
   ;; 为了安全考虑，关闭emacs后需要kill gpg-agent 进程
   (setq epa-file-cache-passphrase-for-symmetric-encryption t)
   )
 )

(provide 'init-gpg)
