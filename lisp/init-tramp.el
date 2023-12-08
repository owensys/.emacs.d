(require 'tramp)
(setq password-cache-expiry 360000000)      ;; 设置密码过期时间，避免每次询问密码

(when is-windows
  ;; windows必须设置bash才能使用tramp
  ;;  (setq explicit-shell-file-name "/bin/bash") ;; plink不需要设置
  (setq tramp-default-method "plink")
  ;;  (setq tramp-password-end-of-line "\r\n"))
  )
  
(when is-linux
  (setq tramp-default-method "ssh"))


;; (setq tramp-verbose 4)
;;(setq tramp-debug-to-file t)
(setq tramp-connection-timeout 15)


;; https://stackoverflow.com/questions/22972621/how-to-use-sshpass-when-loging-in-to-remote-server-with-emacs-tramp
(require 'auth-source) ;; 会自动加载，输入密码后询问是否保存
(setq auth-source-debug t)
;; machine melancholia port scp login daniel password geheim
(setq auth-sources '("~/.authinfo"))

;; test open
;; (setq default-process-coding-system '(utf-8-dos . cp936))
;; tramp-cleanup-this-connection
;; (find-file "/plink:owen@192.168.56.2:/home/owen/tmp/test.txt")
;; (find-file "/plink:owen@192.168.56.2:~/")
;;(find-file "/plink:owen@192.168.56.2:/home/owen/tmp/auto-dark-emacs/auto-dark.el")


(provide 'init-tramp)
