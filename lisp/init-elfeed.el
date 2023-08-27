
(eye/use-package
 'elfeed
 :urls '(("elfeed" . "https://github.com/skeeto/elfeed.git"))
 :load-path "elfeed"
 :ensure t
 :command '(elfeed elfeed-update)
 :config
 (progn

   (setq elfeed-curl-extra-arguments '("-xhttp://127.0.0.1:10813")) ;; ok

   (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory))
   (setq elfeed-show-entry-switch 'display-buffer)
   (setq elfeed-curl-timeout 30)
   (setq elfeed-use-curl t)
   (setf url-queue-timeout 40)
   (push "-k" elfeed-curl-extra-arguments)
   (setq elfeed-search-filter "@3-months-ago +unread")
   ;; 在同一个 buffer 中显示条目。
   (setq elfeed-show-unique-buffers nil)
   (setq elfeed-search-title-max-width 120)
   (setq elfeed-search-date-format '("%Y-%m-%d %H:%M" 20 :left))
   (setq elfeed-log-level 'info) ;; warn

   (setq elfeed-feeds
         '(
           "http://nullprogram.com/feed/"
           "https://planet.emacslife.com/atom.xml"
           "https://v2ex.com/index.xml"
           ))
   ;; load opml
   (let ((opml "~/.emacs.d/feedly.opml"))
     (if (file-exists-p opml)
         (elfeed-load-opml opml)))


   ;; 支持收藏 feed, 参考：http://pragmaticemacs.com/emacs/star-and-unstar-articles-in-elfeed/
   (defalias 'elfeed-toggle-star (elfeed-expose #'elfeed-search-toggle-all 'star))
   (eval-after-load 'elfeed-search '(define-key elfeed-search-mode-map (kbd "m") 'elfeed-toggle-star))
   (defface elfeed-search-star-title-face '((t :foreground "#f77")) "Marks a starred Elfeed entry.")
   (push '(star elfeed-search-star-title-face) elfeed-search-face-alist)
   )
 )

;; https://emacstalk.github.io/post/007/


(eye/use-package
 'mb-url
 :urls '(("mb-url" . "https://github.com/jiacai2050/mb-url"))
 :ensure t
 :load-path "mb-url"
 :command '(mb-url-http-around-advice)
 :init
 (progn
   (setq socks-proxy "socks5h://127.0.0.1:10812"))
 :config
 (progn
   (setq mb-url-http-backend 'mb-url-http-curl
	    mb-url-http-curl-switches `("--max-time" "20" "-x" ,socks-proxy))
   (advice-add 'url-http :around 'mb-url-http-around-advice)

   ;; test fetch logo and display
   ;; (progn
   ;;   (let* ((buffer (url-retrieve-synchronously "https://emacstalk.github.io/images/logo.png"))
   ;;          (data (with-current-buffer buffer
   ;;  			    (goto-char (point-min))
   ;;  			    (search-forward "\n\n")
   ;;  			    (buffer-substring-no-properties (point) (point-max)))))
   ;;     (insert-image (create-image data nil t))
   ;;     (kill-buffer buffer))
   ;;   )
   )
 )





(provide 'init-elfeed)
