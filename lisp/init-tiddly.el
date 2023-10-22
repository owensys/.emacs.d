(setq eye-tiddly-dir "E:/home/Dropbox/mywiki")
(setq eye-image-server-dir "E:/home/Dropbox/mywiki/images")
(setq eye-image-server-port 6001)

(eye/use-package
 'simple-httpd
 :load-path "emacs-web-server"
 :ensure t
 :config
 (progn
   (f-mkdir eye-image-server-dir)
   (setq httpd-root eye-image-server-dir)
   (setq httpd-port eye-image-server-port)
   (httpd-start)
   )
 )

(defun eye/tid-insert-image ()
  (interactive)
  (let* ((convert-path (executable-find "convert"))
         (filename (format-time-string "%Y%m%d%H%M%S.png"))
	 (save-path (concat eye-image-server-dir "/" filename))
         (command (format "%s clipboard: %s" convert-path (to-windows-path save-path)))
         )
    (shell-command command)
    (if (f-exists-p save-path)
        (progn
          (message "save image %s ok." filename)
          (let ((links (format "[img[http://localhost:%s/%s]]" eye-image-server-port filename)))
            (insert links)
            (kill-new links))
          )
      (message "save failed"))
    ))

(defun eye/tid-find-file ()
  (interactive)
  (let* ((default-directory (concat eye-tiddly-dir "/tiddlers/")))
    (counsel-find-file)
    ))


(defun eye/tid-new-file ()
  (interactive)
  (let* ((note-name (read-string "Note:"))
         (file-path (concat eye-tiddly-dir "/tiddlers/" note-name ".tid")))
    (find-file file-path)
    (insert (format "created: %s\n" (format-time-string "%Y%m%d%H%M%S%3N")))
    (insert "creator: owen\n")
    (insert (format "modified: %s\n" (format-time-string "%Y%m%d%H%M%S%3N")))
    (insert "tags: \n")
    (insert (format "title: %s\n\n" note-name))
    ))



(provide 'init-tiddly)
