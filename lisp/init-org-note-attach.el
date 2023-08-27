;;;;
;; on windows, create symbol link for use home path in org file
;; same as on linux.
;; mklink /d d:\emacs_env\home\attach x:\orgnotes\attach
(require 'f)

(setq eye-org-file-attach-base-dir "~/attach")
(if is-work (setq eye-org-file-attach-base-dir "~/attach_ts"))
(setq eye-org-file-attach-auto-show-image t)

(defun eye--get-org-file-attach-id ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (string-equal (string (char-after 1)) "*")
      (newline)
      (goto-char (point-min))
      )
    (org-id-get-create)
    ))

(defun eye/get-org-file-attach-path ()
  "获取附件目录"
  ;; (interactive)
  ;; (let ((id (eye--get-org-file-attach-id)))
  ;;   (when id
  ;;     (format "%s/%s" eye-org-file-attach-base-dir id)
  ;;     ))
  eye-org-file-attach-base-dir ;; 使用同一个目录
  )

(defun eye/open-org-file-attach-dir ()
  "Open explorer of current buffer directory.
locale-notebook-dir use absolute path for advise.
打开附件目录
"
  (interactive)
  (when (eq system-type 'windows-nt)
    (let* ((dir (eye/get-org-file-attach-path))
	   (homedir (replace-regexp-in-string "\\\\" "/" (getenv "HOME")))
	   (explorer (replace-regexp-in-string "/" "\\\\" (executable-find "C:/Windows/SysWOW64/explorer")))
	   (command))
      ;; 确保目录存在，不存在则创建
      (f-mkdir dir)
      (setq dir (encode-coding-string
		 (replace-regexp-in-string "/" "\\\\" dir) 'gbk-dos))
      (setq dir (replace-regexp-in-string "~" homedir dir))
      (setq dir (replace-regexp-in-string "/" "\\\\" dir))
      (setq command (concat explorer " " dir))
      (shell-command command nil nil)
      (message command))
    )
  (when (eq system-type 'gnu/linux)
    (let* ((dir (eye/get-org-file-attach-path))
	   (command (concat "thunar " dir)))
      (shell-command command)
    )
  ))

(defun eye/copy-org-attach-dir-for-win32 ()
  "Open explorer of current buffer directory.
locale-notebook-dir use absolute path for advise.
"
  (interactive)
  (when (eq system-type 'gnu/linux)
    (let* ((dir (eye/get-org-file-attach-path)))
      (setq dir (replace-regexp-in-string "~/attach/" "t:/" dir))
      (setq dir (subst-char-in-string ?/ ?\\ dir))
      (kill-new dir)
      (message "Get dir:%s" dir)
    )
  ))

(setq eye-match-drop-file-exp
      (concat "\\(jpg\\|png\\|gif\\|"
	      "zip\\|xz\\|gz\\|7z\\|"
	      "jpeg\\|txt\\|doc\\|docx\\|xlsx\\|"
	      "apk\\|rar\\|md\\|json\\|"
	      "html\\|bak\\|db\\|"
	      "pptx\\|pdf\\)$"))

(setq eye-match-image-file-exp
      (concat "\\(jpg\\|jpeg\\|png\\|gif\\)$"))

;;(setq is-support-imagemagic (string-match-p "imagemagic" system-configuration-features))
(setq is-support-imagemagic (image-type-available-p 'imagemagick))

;; see http://that-year.blogspot.com/2008/10/emacs_5377.html?m=1
(defun my-dnd-insert-link (uri action)
  (if (and (or (eq 'org-mode major-mode)
               (eq 'org-journal-mode major-mode))
	   ;;(string-match eye-match-drop-file-exp uri)
	   1 ;; ignore file extension
	   )
      (let* ((file-line (dnd-get-local-file-uri uri))
	     (attach-dir (eye/get-org-file-attach-path))
	     file-path file-name new-file-path)
	;; chinese path
	(setq file-path (replace-regexp-in-string "^file:" "" uri))
	(setq file-path (decode-coding-string (url-unhex-string file-path) 'utf-8))
	(setq file-name (file-name-nondirectory file-path))
	(setq new-file-path (expand-file-name file-name attach-dir))
	
	;; create attach dir
	(unless (f-directory? attach-dir)
	  (f-mkdir attach-dir))

	(if (f-exists-p new-file-path)
	    (message "file already exists!")
	  (progn    
	    ;; copy/move to attach dir
	    (if (>= (f-size file-path) (* 10 1024 1024))
		(if (yes-or-no-p "file size >= 10MB, move it?")
		    (f-move file-path new-file-path)
		  (f-copy file-path new-file-path))
	      (f-copy file-path new-file-path))
	    
	    (if (f-exists-p new-file-path)
		(message "copy/move file ok.")
	      (message "copy/move file failed."))
	    ))

	;; make sure insert with a new line
	(end-of-line)
	(newline)
	;; 由于图片加了描述时不能显示，所以区分对待
	(if (string-match eye-match-image-file-exp uri)
	    (progn
	      	(insert "#+attr_org: :width 300\n")
	        (insert (format "[[file:%s%s]]"
                                eye-org-file-attach-base-dir
			        (substring new-file-path (length eye-org-file-attach-base-dir))))
	        )
	  (insert (format "[[file:%s%s][%s]]"
                          eye-org-file-attach-base-dir
			  (substring new-file-path (length eye-org-file-attach-base-dir))
			  file-name)))
	(newline)
	(if (and (string-match eye-match-image-file-exp uri) eye-org-file-attach-auto-show-image)
		(org-redisplay-inline-images))
	)
    (dnd-open-file uri action)))


(defun my-dnd-insert-link-v2 (uri action)
  (if (and (or (eq 'org-mode major-mode)
               (eq 'org-journal-mode major-mode))
	   ;;(string-match eye-match-drop-file-exp uri)
	   1 ;; ignore file extension
	   )
      (let* ((file-line (dnd-get-local-file-uri uri))
			 (attach-dir (eye/get-org-file-attach-path))
			 file-path file-name new-file-path resize-command resized-fullpath)
		;; chinese path
		(setq file-path (replace-regexp-in-string "^file:" "" uri))
		(setq file-path (decode-coding-string (url-unhex-string file-path) 'utf-8))
		(setq file-name (concat
                         (format-time-string "%Y%m%d_%H%M%S_")
                         (file-name-nondirectory file-path)))
		(setq new-file-path (expand-file-name file-name attach-dir))		
		
		;; create attach dir
		(unless (f-directory? attach-dir)
		  (f-mkdir attach-dir))

		(if (f-exists-p new-file-path)
			(message "file already exists!")
		  (progn    
			;; copy/move to attach dir
			(if (>= (f-size file-path) (* 10 1024 1024))
				(if (yes-or-no-p "file size >= 10MB, move it?")
					(f-move file-path new-file-path)
				  (f-copy file-path new-file-path))
			  (f-copy file-path new-file-path))
			
			(if (f-exists-p new-file-path)
				(message "copy/move file ok.")
			  (message "copy/move file failed."))
			))

	    ;; make sure insert with a new line
	    (end-of-line)
	    (newline)

	    ;; only process image file
	    (when (and (string-match eye-match-image-file-exp uri)
			       (executable-find "resize_image.exe")
                   (equal org-image-actual-width t) ;; 已经设置了就不再需要用_s.png
                   )
	      ;; for resize. example: /path/of/xxx.jpg to /path/of/xxx_s.jpg
	      (setq resized-fullpath (expand-file-name (concat (file-name-base file-name)
							                               "_s."
							                               (file-name-extension file-name)
							                               )
						                           attach-dir
						                           ))
	      (setq resize-command (format "%s %s 640 480 %s"
				                       (executable-find "resize_image.exe")
				                       new-file-path
				                       resized-fullpath))
	      ;; s-replace-regexp 中的 regexp 不支持windows下的两个反斜杠，需要先转换成 /
	      (setq resize-command (s-replace-regexp "~" (subst-char-in-string ?\\ ?/ (getenv "HOME")) resize-command))
	      (shell-command resize-command)
	      (if (f-exists-p resized-fullpath)
	          (setq new-file-path resized-fullpath))
	      )

	    ;; 在windows上兼容 ~ 路径
	    (setq new-file-path
	          (s-replace-regexp (subst-char-in-string ?\\ ?/ (getenv "HOME")) "~" new-file-path))

	    ;; 由于图片加了描述时不能显示，所以区分对待
	    (if (string-match eye-match-image-file-exp uri)
	        (progn
	          ;;(insert "#+attr_org: :width 300\n")
	          (insert (format "[[file:%s]]" new-file-path)))
	      
	      (insert (format "[[file:%s][%s]]" new-file-path
			              file-name)))
	    (newline)
	    (if (and (string-match eye-match-image-file-exp uri) eye-org-file-attach-auto-show-image)
		    (org-redisplay-inline-images))
	    )
    (dnd-open-file uri action)))



(defun eye/org-attach-enable ()
  "Enable my drop event handler."
  (setq dnd-protocol-alist
	    `(("^file:" . my-dnd-insert-link-v2)
	      ("^\\(https?\\|ftp\\|file\\|nfs\\)://" . my-dnd-insert-link-v2))))


(eye/org-attach-enable)


(defun eye/delete-org-link-file ()
  "删除链接位置的文件"
  (interactive)
  (save-excursion
    (let* ((p1 (search-backward "[[file:" nil t))
           (p2 (search-forward "]" nil t))
           file-dir
           file-path
           file-name ;; 文件名，对于图片，有 _s.png
           origin-file-name ;; 对于图片，是没有 _s 的png文件名
           delete-file-path
           )
      (when (and p1 p2)
        (setq p1 (+ p1 7)) ;; 指向路径开头
        (setq p2 (- p2 1)) ;; 指向路径结束
        (setq file-path (buffer-substring-no-properties p1 p2))
        (setq delete-file-path (concat file-path ".delete"))
        (setq file-name (file-name-nondirectory file-path))
        (setq file-dir (file-name-directory file-path))
        (if (string-match "_s.png" file-name)
            (setq origin-file-name (s-replace "_s.png" ".png" file-name))
          )
        (if (string-match "_s.jpg" file-name)
            (setq origin-file-name (s-replace "_s.jpg" ".jpg" file-name))
          )
        (if (string-match "_s.jpeg" file-name)
            (setq origin-file-name (s-replace "_s.jpeg" ".jpeg" file-name))
          )
        (if (string-match "_s.bmp" file-name)
            (setq origin-file-name (s-replace "_s.bmp" ".bmp" file-name))
          )
        ;; (message "origin file name:%s" origin-file-name)
        (if (file-exists-p file-path)
            (f-move file-path delete-file-path))
        (if (and origin-file-name (file-exists-p (concat file-dir "/" origin-file-name)))
            (f-move (concat file-dir "/" origin-file-name)
                    (concat file-dir "/" origin-file-name ".delete"))
            )
        (message "path:%s" file-path)
        (if (not (file-exists-p file-path))
            (progn
              (setq p1 (- p1 7)) ;; 回到[[
              (goto-char p1)
              (setq p2 (search-forward "]]" nil t))
              ;; (setq p2 (+ p2 2))
              (goto-char p1)
              (delete-char (- p2 p1))
              (message "delete file to %s." delete-file-path)))
        )
      )
    )
  )


(defun eye/paste-image-from-clipboard ()
  (interactive)
  (let* ((convert-path (executable-find "convert"))
	     (attach-dir (eye/get-org-file-attach-path))
	     (imagename (concat (format-time-string "%Y%m%d_%H%M%S") ".png"))
	     (tmppath (concat "d:\\\\" imagename))
	     (fullpath (concat attach-dir "/" imagename))
         (resize-path (executable-find "resize_image.exe"))
         (resized-fullpath (expand-file-name
                            (concat (file-name-base imagename) "_s.png")
						    attach-dir
						    ))
         (resize-command (format "%s %s 670 480 %s"
				                 resize-path
				                 fullpath
				                 resized-fullpath))
	     command)
    (if convert-path
	    (progn
	      ;; create attach dir
	      (unless (f-directory? attach-dir)
	        (f-mkdir attach-dir))
	      (setq command (format "%s clipboard: %s" convert-path tmppath))
	      ;; (message (format "fullpath: %s" fullpath))
	      (shell-command command)
	      (f-move tmppath fullpath)

          (if (equal org-image-actual-width t) ;; 已经设置了就不再需要用_s.png
              (progn
                ;; s-replace-regexp 中的 regexp 不支持windows下的两个反斜杠，需要先转换成 /
	            (setq resize-command (s-replace-regexp "~" (subst-char-in-string ?\\ ?/ (getenv "HOME")) resize-command))
                )
            (progn
              (setq resize-command nil) ;; 不需要调用 resize时，设置命令为空
              (setq resized-fullpath fullpath)
              )
            )
          (if resize-command ;; 需要执行命令才执行
	          (shell-command resize-command))

          (if (f-exists-p resized-fullpath)
	          (setq fullpath resized-fullpath))
          ;; (message "resized-fullpath:%s" resized-fullpath)
	      ;; (insert "#+attr_org: :width 300\n")
	      (insert (format "[[file:%s/%s]]"
                          eye-org-file-attach-base-dir
			              (file-name-nondirectory fullpath)))
	      (newline)
	      (org-redisplay-inline-images)
              (if (file-exists-p (concat "d:/" imagename))
                  (f-delete (concat "d:/" imagename))
                  )
	      )
      (message "convert.exe not found.")
      )))


(defun xah-html-encode-percent-encoded-url ()
  "Percent encode URL in current line or selection.
          Example:
          http://example.org/(Dürer)
          becomes
          http://example.org/(D%C3%BCrer)

          Example:
          http://example.org/文本编辑器
          becomes
          http://example.org/%E6%96%87%E6%9C%AC%E7%BC%96%E8%BE%91%E5%99%A8

          URL `http://ergoemacs.org/emacs/emacs_url_percent_decode.html'     
         Version 2018-10-26"
  ;; (interactive)
  (let ($p1 $p2 $input-str $newStr)
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (setq $p1 (line-beginning-position) $p2 (line-end-position)))
    (setq $input-str (buffer-substring-no-properties $p1 $p2))
    (require 'url-util)
    (setq $newStr (url-encode-url $input-str))
    (if (string-equal $newStr $input-str)
        (progn (message "no change" ))
      (progn
        (delete-region $p1 $p2)
        (insert $newStr)))))

(defun xah-html-decode-percent-encoded-url ()
  "Decode percent encoded URL of current line or selection.

          Example:
           %28D%C3%BCrer%29
          becomes
           (Dürer)

          Example:
           %E6%96%87%E6%9C%AC%E7%BC%96%E8%BE%91%E5%99%A8
          becomes
           文本编辑器

          URL `http://ergoemacs.org/emacs/emacs_url_percent_decode.html'
          Version 2018-10-26"
  ;; (interactive)     
  (let ( $p1 $p2 $input-str $newStr)
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (setq $p1 (line-beginning-position) $p2 (line-end-position)))
    (setq $input-str (buffer-substring-no-properties $p1 $p2))
    (require 'url-util)
    (setq $newStr (url-unhex-string $input-str))
    (if (string-equal $newStr $input-str)
        (progn (message "no change" ))
      (progn
        (delete-region $p1 $p2)
        (insert (decode-coding-string $newStr 'utf-8))))))

;; @see https://emacs-china.org/t/org-mode-link/17059/4
;; 把原函数的interactive特性去掉，自己包装了一下
;; 如果想直接对整个buffer转码，就把mark-whole-buffer前面的注释去掉
(defun buffer-url-decode()
  (interactive)
  ;; (mark-whole-buffer)
  (xah-html-decode-percent-encoded-url))

(defun buffer-url-encode()
  (interactive)
  ;; (mark-whole-buffer)
  (xah-html-encode-percent-encoded-url))



(require 's)

(defun eye/open-git-file (&optional filename)
  (interactive)
  (let* (
         ;;(default-directory (get-locale-book-dir))
         (default-directory (get-locale-book-dir))
	 (current-filename (file-name-base (buffer-name)))
	 (link (format " - [[wiki:%s]]" current-filename))
	 (is-in-references (search-backward "* linked references" nil t))
	 temp-path)
    (shell-command "git ls-tree -r HEAD --name-only > d:/list_files")
    (setq temp-path
	  (ivy-read "select:"
		    (s-split "\n" (get-string-from-file "d:/list_files"))
		    :initial-input filename
		    ))
    (delete-file "d:/list_files")
    (when (file-exists-p temp-path)
      (find-file temp-path)
      ;; check and insert backlinks
      ;; 还需要排除是从linked references打开的链接，则不再添加
      (when (not is-in-references)
	(save-excursion
	  (goto-char (point-max))
	  (if (not (search-backward "* linked references" nil t))
	      (progn
		(newline)
		(insert "* linked references")
		))
	  (if (not (search-forward link nil t))
	      (progn
		(goto-char (point-max))
		(newline)
		(insert link)
		(save-buffer)
		(message "inserted linked references ok.")
		)
	    (message "already has linked references.")
	    )
	  )))
      ))


(defun eye/open-git-file-v2 (&optional filename)
  (interactive)
  (let* (
         ;; (default-directory (get-locale-book-dir))
         (default-directory (get-locale-book-dir))
	 (current-filename (file-name-base (buffer-name)))
	 temp-path sel-list)
    (shell-command "git ls-tree -r HEAD --name-only > d:/list_files")
    (setq sel-list (s-split "\n" (get-string-from-file "d:/list_files")))
    (add-to-list 'sel-list (concat "note/" filename "(new file)"))
    (setq temp-path
	  (ivy-read "select:"
				sel-list
				:initial-input filename
				))
    (delete-file "d:/list_files")
    (if (string-match "new file" temp-path)
        (eye/org-new-file filename)
      (find-file temp-path)) ;; 文件不存在则创建
    ))

(defun eye/open-org-note-file ()
(interactive)
(let* (
       (ivy-posframe-height (eye--ivy-height)) ;; 根据当前frame使用动态高度
       (ivy-height ivy-posframe-height)
       (default-directory (concat (get-locale-book-dir) "/note"))
	   (current-filename (file-name-base (buffer-name)))
       (search-exe (executable-find "search-title.exe"))
       (result-file (concat default-directory "/titles.dat"))
       command sel-list select-item
       )
    (setq command (format "%s %s --" search-exe default-directory))
	;; temp-path sel-list)
    (shell-command command)
    (setq result-content (get-string-from-file result-file))
    (setq sel-list (s-split "\n" result-content))

    (setq select-item (ivy-read "Open: " sel-list))
    (when select-item
      (let ((tmp (s-split "::" select-item)))
        (find-file (cadr tmp))
        )
      )
    )
  )

;; custom link type
;; https://orgmode.org/manual/Adding-Hyperlink-Types.html
(if (> emacs-major-version 26)
    (require 'ol))

(defun org-wiki-open (path _)
  "定义回车打开链接"
  (eye/open-git-file-v2 path))

(defun eye--org-wiki-completion (&optional arg)
  "定义complete，支持用C-c C-l的方式自动插入链接"
  (let* ((file-list (eye--get-dir-file-list
                     ;; (get-locale-book-dir)
                     (get-locale-book-dir)
                     ))
	     sel-name)
    (setq sel-name (ivy-read "Select wiki note: " file-list))
    (setq sel-name (file-name-base sel-name))
    ;; (insert (format "[[wiki:%s]]" sel-name))
    (format "wiki:%s" sel-name)
    ))

(org-link-set-parameters "wiki"
			 :face '(:foreground "dark green" :underline t)
			 :follow #'org-wiki-open
			 :complete 'eye--org-wiki-completion)


(defvar eye--wiki-page-link nil)
(defun eye/org-store-current-page-link ()
  (interactive)
  (when (equal 'org-mode major-mode)
	(setq eye--wiki-page-link (format "[[wiki:%s]]" (file-name-base)))
	(message "Stored link: %s" eye--wiki-page-link)
	))

(defun eye/org-insert-store-link ()
  (interactive)
  (insert eye--wiki-page-link))


(defun eye/search-org-file ()
  (interactive)
  (let ((default-directory (get-locale-book-dir)))
    ;; (eye/open-git-file-v2) ;; 基于文件名搜索
    (eye/open-org-note-file) ;; 搜索以日期命名的笔记
    ))



;; (defun eye/search-note-rg ()
;;   "同时匹配多个字符串，可以写成 str1.*str2
;;    要搜索带点的字符串，要用转义 \.
;;   "
;;   (interactive)
;;   (require 'color-rg)
;;   (let (
;;         ;; (default-directory (get-locale-book-dir))
;;         (default-directory (get-locale-book-dir))
;;         )
;;     (color-rg-search-input)))

;; (defun eye/search-note-ag ()
;;   "windows下中文搜索有问题"
;;   (interactive)
;;   (counsel-ag (read-string "Search:")
;; 	      ;; (get-locale-book-dir)
;;               (get-locale-book-dir)
;;               ))


(defun eye--get-dir-file-list (dir)
  "获取一个git目录下的所有文件相对路径"
  (interactive)
  (let ((default-directory dir)	file-list)
    (shell-command "git ls-tree -r HEAD --name-only > d:/list_files")
    (setq file-list (s-split "\n" (get-string-from-file "d:/list_files")))
    (delete-file "list_files")
    file-list
    ))


(defun eye/org-insert-note-link-with-backlinks ()
  "以wiki方式选择一个文件，并插入链接和反向链接（不再使用）"
  (interactive)
  (save-excursion
    (let* ((current-filename (file-name-base (buffer-name)))
	   (link (format " - [[wiki:%s]]" current-filename))
	   (file-list (eye--get-dir-file-list (get-locale-book-dir)))
	   sel-name link-file-path)
      (setq sel-name (ivy-read "Select wiki note: " file-list))
      (setq link-file-path sel-name)
      (setq sel-name (file-name-base sel-name))
      (insert (format "[[wiki:%s]]" sel-name))
      (message "link to:%s" link-file-path)
      (find-file (expand-file-name link-file-path (get-locale-book-dir)))
      (goto-char (point-max))
      (if (not (search-backward "* linked references" nil t))
	  (progn
	    (newline)
	    (insert "* linked references")
	    ))
      (if (not (search-forward link nil t))
	  (progn
	    (goto-char (point-max))
	    (newline)
	    (insert link)
	    (save-buffer)
	    (kill-this-buffer)
	    (message "inserted linked references ok.")
	    )
	(message "already has linked references.")
	)
    )))

(defun eye/org-remove-note-link ()
  "删除链接和对应的反向链接（不再使用）"
  (interactive)
  (save-excursion
    (let* ((wiki-begin-tag "[[wiki:")
	   (wiki-end-tag "]]")
	   (current-link (format "[[wiki:%s]]" (file-name-base (buffer-file-name))))
	   start end linkname file-list linkpath)
      (setq start (search-backward wiki-begin-tag))
      (setq start (+ start (length wiki-begin-tag)))
      (setq end (search-forward wiki-end-tag))
      (setq end (- end (length wiki-end-tag)))
      (setq linkname (buffer-substring-no-properties start end))
      ;;(message linkname)
      (setq file-list (eye--get-dir-file-list (get-locale-book-dir)))
      (setq linkpath (ivy-read "Delete backlinks in file: " file-list :initial-input linkname))
      (when linkpath
	(find-file (expand-file-name linkpath (get-locale-book-dir)))
	;; 找到反向链接
	(goto-char (point-max))
	(if (search-backward current-link nil t)
	    (progn
	      ;; 删除反向链接
	      (beginning-of-line)
	      (kill-line)
	      (save-buffer)
	      (message "removed backlinks ok.")
	      ))
	(kill-this-buffer)
	)

      (delete-region (- start (length wiki-begin-tag))
		     (+ end (length wiki-end-tag))
		     )
      
      )))


(defun eye/org-get-windows-file-link ()
  "获取当前光标处的链接并转换为windows下的路径，复制到剪贴板
attach目录要在windows中映射到t目录"
  (interactive)
  (save-excursion
    (let (start end win32-link)
      (setq start (search-backward "[["))
      (when (> start 1)
	(setq start (search-forward ":"))       
	(setq end (search-forward "]"))
	(setq end (- end 1))
	(setq win32-link (buffer-substring-no-properties start end))
	(setq win32-link (replace-regexp-in-string "~/attach/" "t:/" win32-link))
	(setq win32-link (subst-char-in-string ?/ ?\\ win32-link))
	(kill-new win32-link)
	(message "Copyed: %s" win32-link)
	)
      )))

(setq eye--attach-file-tmp-dir "~/orgnotes/attach/0000")
(defun eye/org-insert-from-share-dir ()
  "从共享目录下移动文件到附件目录，支持文件夹复制"
  (interactive)
  (let (oldpath newpath attach-dir)
    (setq attach-dir (eye/get-org-file-attach-path))
    (f-mkdir attach-dir)
    (dolist (filename (directory-files eye--attach-file-tmp-dir))
      (when (and (not (equal filename "."))
				 (not (equal filename "..")))
		(end-of-line)
		(setq oldpath (concat eye--attach-file-tmp-dir "/" filename))
		(setq newpath (concat attach-dir "/" filename))
		(f-move oldpath newpath)
		(if (string-match eye-match-image-file-exp filename)
			(insert "\n#+attr_org: :width 300\n"))
		(insert (format "[[file:%s]]\n" newpath))
		)
      )))

(defun eye/org-new-file (notename &optional anchor-id)
  "Create a new file named SLUG.
SLUG is the short file name, without a path or a file extension."
  (interactive "sNew note (without extension): ")
  (let* ((filename (format "%s.org" notename))
	 (dir (read-directory-name "Save to: " (concat (get-locale-book-dir) "/note")))
	 ;; (dir (concat (get-locale-book-dir) "/note"))
	 (savepath (concat dir "/" filename))
	 )
    (if (f-exists-p savepath)
	(user-error "文件已存在，请使用其它文件名！")
      (progn
	(find-file savepath)
	(insert (format "#+title: %s" notename))
        (if (not anchor-id)
            (setq anchor-id (s-left 8 (my-generate-uuid))))
	(newline)
	(insert "#+tags: :draft:") ;; 默认设置成draft
	(newline)
	;;(insert (format "#+date: [[a:%s][%s]]" anchor-id (format-time-string "%Y-%m-%d")))
	(insert (format "#+date: %s" (format-time-string "%Y-%m-%d")))
    ;; (newline)
    ;; (insert "#+SETUPFILE: x:/orgnotes/org/theme.setup")
	(newline)
	(newline)
	(goto-char (point-max))
    (org-id-get-create)
    (save-buffer)
    ;; (eye/get-org-id)
    ))
    ))

(defun eye/org-new-file-by-date (notename &optional anchor-id)
  "Create a new file named SLUG.
SLUG is the short file name, without a path or a file extension."
  (interactive "sNew note by date (without extension): ")
  (let* (
         ;; (filename (format "%s_%s.org" (format-time-string "%Y%m%d%H%M%S") notename)) ;; date_notename.org
         (filename (format "%s.org" (format-time-string "%Y%m%d%H%M%S"))) ;; only date.org
	 ;;(dir (read-directory-name "Save to: " (concat (get-locale-book-dir) "/03-resources")))
	 (dir (concat (get-locale-book-dir) "/note"))
	 (savepath (concat dir "/" filename))
	 )
    (if (f-exists-p savepath)
	(user-error "文件已存在，请使用其它文件名！")
      (progn
	(find-file savepath)

	(insert (format "#+title: %s" notename))
        (if (not anchor-id)
            (setq anchor-id (s-left 8 (my-generate-uuid))))
	(newline)
	(insert "#+tags: :draft:") ;; 默认设置成draft
	(newline)
	;;(insert (format "#+date: [[a:%s][%s]]" anchor-id (format-time-string "%Y-%m-%d")))
	(insert (format "#+date: %s" (format-time-string "%Y-%m-%d")))
    ;; (newline)
    ;; (insert "#+SETUPFILE: x:/orgnotes/org/theme.setup") ;; 需要导出时手动添加这行
	(newline)
	(newline)
	(goto-char (point-max))
    (org-id-get-create)
    (save-buffer)

    ))
    ))

(defun eye/search-by-tag ()
  (interactive)
  (eye-async-shell-command "TagList.exe" (concat (get-locale-book-dir) "/note"))
  (let* ((default-directory (concat (get-locale-book-dir) "/note"))
         (result (get-string-from-file "x:/tmp/tags.tmp"))
         (tag (ivy-read "Search tag:"
                        (s-split "\n" result)))
         )
    (when tag
      (message "Search tag: %s" tag)
      (color-rg-search-input (format ":%s:" tag))
      )
    ))


(defun eye/show-tags ()
  (interactive)

  (eye-async-shell-command "TagList.exe" (concat (get-locale-book-dir) "/note"))
  (let* ((default-directory (concat (get-locale-book-dir) "/note"))
         (result (get-string-from-file "x:/tmp/tags.tmp"))
         (tags (s-split "\n" result))
         )

    ;; create *tags* buffer
    (get-buffer-create "*tags")
    ;; insert tag list
    (switch-to-buffer "*tags*")
    (erase-buffer)
    (dolist (tag tags)
      (insert-text-button tag
                          'action
                          (lambda (b)
                            (let ((default-directory (concat (get-locale-book-dir) "/note")))
                              (color-rg-search-input (format ":%s:" (button-label b))))))
      (insert " ")
      )
    )
  )



(defun eye-list-attach-file (attach-dir)
  (let* ((file-list (directory-files attach-dir nil nil t))
         final-list)
    (dolist (file file-list)
      (if (and
           (not (s-contains-p "ffs_db" file))
           (not (string-equal "." file))
           (not (string-equal ".." file))
           (not (s-contains-p ".delete" file))
           (not (s-contains-p "_s.png" file))
           (not (s-contains-p ".png" file))
           )
          (add-to-list 'final-list file))
      )
    final-list
    )
  )

(defun eye/insert-attach-file ()
  "insert attach file from list"
  (interactive)
  (let* ((attach-dir eye-org-file-attach-base-dir)
         (file-list (eye-list-attach-file attach-dir))
         (select-file (ivy-read "Open file:" file-list))
         )
    (insert (format "[[file:~/attach_ts/%s][%s]]" select-file select-file))
    (save-buffer)
    ))


(defun eye/open-attach-file ()
  "列出attach目录下的文件，以日期新的优先显示，选中并打开"
  (interactive)
  (let* ((attach-dir eye-org-file-attach-base-dir)
         (file-list (eye-list-attach-file attach-dir))
         (select-file (ivy-read "Open file:" file-list))
         (homedir (subst-char-in-string ?\\ ?/ (getenv "HOME")))
         (explorer (executable-find "C:/Windows/SysWOW64/explorer"))
         full-path
         action
         )
    (if select-file
        (progn
          (setq full-path (concat attach-dir "/" select-file))
          (setq full-path (replace-regexp-in-string "~" homedir full-path))
          (setq full-path (replace-regexp-in-string "/" "\\\\" full-path))
          ;; (message "open:%s" full-path)
          (setq action (ivy-read "Action:" '("open" "copy")))
          (if (string-equal "open" action)
              (eye-open-file-native full-path)
            (let ((target-dir "d:/tmp"))
              (if (not (f-directory-p target-dir))
                  (f-mkdir target-dir))
              (copy-file full-path (concat target-dir "/") t)
              (eye-select-file-in-explorer (concat target-dir "/" select-file))
              )
            ))
      (message "No selected file.")
      )
    ))



(defun eye/note-index ()
  (interactive)
  (let ((index-path "x:/orgnotes/org/note/0-index.org"))
    (when (file-exists-p index-path)
      (find-file index-path)
      )))
  
;; 设置全局快捷键
(global-set-key (kbd "C-x RET d") 'buffer-url-decode)
(global-set-key (kbd "C-x RET e") 'buffer-url-encode)

;;(require 'config-org-note-attach-after)


(defun eye--get-org-title ()
  "获取页面标题"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let* ((p1 (search-forward "TITLE:" nil t))
           (p2 (search-forward "\n" nil t))
           (title "")
           )
      (when (and p1 p2)
        (setq title (buffer-substring-no-properties p1 p2))
        (require 's)
        (setq title (s-replace-regexp "\\[\\[a:.*\\]\\[" "" title))
        (setq title (s-replace-regexp "\\]\\]" "" title))
        (setq title (s-replace-regexp "\\$.*\\$" "" title))
        (setq title (s-trim title))
        (message "title:%s" title)
        title
        )
      )
    ))




(defun eye/org-show-page-refs ()
  (interactive)
  (save-excursion
    (eye/open-ref-side-window)
    (let ((search-str "")
          (note-dir (concat (get-locale-book-dir) "/note"))
          (title (eye--get-org-title))
          )
      (unless title (setq title "nil")) ;;没有标题时
      (goto-char (point-min))
      ;; 收集所有anchor链接
      (let ((p1 0) (p2 0) (p3 0))
        (while (search-forward "[[a:" nil t)
          (setq p1 (point))
          (setq p2 (+ p1 8)) ;; 固定长度
          (if (not (string-empty-p search-str))
              (setq search-str (concat search-str ","))) ;;用color-rg的话要添加空格，用自定义程序的话用逗号分割
          (setq search-str (concat search-str (buffer-substring-no-properties p1 p2))) ;; 添加或关键词
          (setq p2 (+ p2 2))
          ;; get search:0e91a23b@keyword,0e91a23b@keyword,271f30ca@test123
          ;; (setq p3 (- (search-forward "]]" nil t) 2))
          ;; (setq search-str (concat search-str "@" (buffer-substring-no-properties p2 p3))) ;; 添加或关键词
          )
        (if (string-empty-p search-str) (setq search-str "nil")) ;; 没有anchor的页面
        (message "search title:%s, str:%s" title search-str)
        ;; 用color-rg搜索，看不到标题
        ;; (color-rg-search-input (eye--build-or-regexp-by-keywords search-str) note-dir)

       (let* ((genexe (executable-find "gen_linked_refs")))
         (let ((future (pfuture-new genexe
                                    "2" ;; run mode
				                     note-dir
				                     (url-encode-url search-str)
                                     (url-encode-url title)
				                     (concat (get-locale-book-dir) "/#refs#.org")
                                     "0"
				                     )))
	        (pfuture-await-to-finish future)
	        (message "gen_linked_refs search:%s, output: %s" search-str (string-trim (pfuture-result future)))
           ;; (switch-to-buffer "#refs#.org") ;; 注：打开了多个文件的话，buffer名会不一样
           ;; (revert-buffer)
           )
         )
       ))))


;; auto refresh backlink
(setq eye-last-ref-org-buffer "")
(defun eye/auto-refresh-ref-side-window ()
  (interactive)
  (if (and
       (equal major-mode 'org-mode)
       (not (string-equal eye-last-ref-org-buffer (buffer-file-name)))
       (string-match (get-locale-book-dir) (buffer-file-name))
       (not (string-equal "#refs#.org" (buffer-name)))
       )
      (progn
        (eye/org-show-page-refs)
        (setq eye-last-ref-org-buffer (buffer-file-name))
        ))
  ;; (if (not (equal major-mode 'org-mode))
      ;; todo: auto close ref side window?
      ;; )
  )

(defun eye/auto-refresh-ref-on ()
  (interactive)
  (setq eye-auto-ref-side-window-timer
        (eye-add-timer 1 t 'eye/auto-refresh-ref-side-window))
  (message "auto refresh ref on")
  )

(defun eye/auto-refresh-ref-off ()
  (interactive)
  (if eye-auto-ref-side-window-timer
      (cancel-timer eye-auto-ref-side-window-timer))
  (message "auto refresh ref off")
  )

;; (eye/auto-refresh-ref-on)


;; (url-encode-url "[[a:sdfadf][ddd]]")

(defun eye--get-keyword-id (search-str)
  "根据关键词获取已有的对应的id，形如[[a:xxxx][keyword]]"
  (let* ((genexe (executable-find "gen_linked_refs"))
         (note-dir (concat (get-locale-book-dir) "/note"))
         )
    (let ((future (pfuture-new genexe
                               "1" ;; run mode
				               note-dir
				               (url-encode-url search-str)
				               )))
	  (pfuture-await-to-finish future)
	  ;; (message "gen_linked_refs search:%s, output:%s" search-str (string-trim (pfuture-result future)))
      (string-trim (pfuture-result future))
      )
    ))



 (defun org-wiki-ref-open (path _)
   (interactive)
   (let* ((strlist (s-split "@" path))
	  (full-path (car strlist))
	  (line (car (cdr strlist))))
     (other-window 1)
     (find-file full-path)
     (org-show-all) ;; 展开
     (goto-line (string-to-number line))
     (recenter-top-bottom 10)
     ))
  
(org-link-set-parameters "ref"
			 :face '(:foreground "DeepSkyBlue" :underline nil)
			 :follow #'org-wiki-ref-open)



;; See @https://emacs-china.org/t/topic/12895/5
(dolist (buffer '("^\\*Flymake diagnostics"
                  "^\\*Flycheck errors\\*$"
                  "^\\*Compile"
                  "^\\*Completions\\*$"
                  "^\\*compilation\\*$"
                  "^\\*Async Shell Command\\*$"))
  (add-to-list 'display-buffer-alist
               `(,buffer
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side            . bottom)
                 (window-height   . 0.33))))

(defun eye/get-org-id-url()
  "获取URL形式的id链接"
  (interactive)
  (save-excursion
    (let ((id (org-id-get-create))
	  scheme
	  )
      (setq scheme (format "org://%s" id))
      (message scheme)

      (w32-set-clipboard-data scheme t)
    )
  ))


(defun eye--get-org-id-no-url()
  (save-excursion
    (goto-char (point-min))
    (when (string-equal (string (char-after 1)) "*")
      (newline)
      (goto-char (point-min))
      )
    (org-id-get-create)
    ))


(defun eye/org-new-entry ()
  (interactive)
  (end-of-visual-line)
  (org-meta-return))


(defun eye/insert-new-file ()
  "插入一个附件文件链接，基于模板文件"
  (interactive)
  (let* ((filetype (ivy-read "New file type: " '("drawio" "mindmap" "uml" "xlsx" "docx" "ppt")))
         (filename (read-string "File name: "))
         (timestr (format-time-string "%Y%m%d_%H%M%S"))
         suffix
         full-file-name
         template-path
         new-file-path
         )
    (cond ((string-equal filetype "drawio") (setq suffix "drawio"))
          ((string-equal filetype "mindmap") (setq suffix "mm"))
          ((string-equal filetype "uml") (setq suffix "uml"))
          ((string-equal filetype "xlsx") (setq suffix "xlsx"))
          ((string-equal filetype "docx") (setq suffix "docx"))
          ((string-equal filetype "ppt") (setq suffix "ppt"))
          (t (message "Wrong file type")))

    ;; (message "suffix:%s" suffix)
    (setq full-file-name (format "%s_%s.%s"
                             timestr
                             filename
                             suffix
                             ))
    (setq template-path (concat (get-locale-book-dir) (format "/template/template.%s" suffix)))
    (setq new-file-path (concat eye-org-file-attach-base-dir "/" full-file-name))
    (f-copy template-path new-file-path)
    (insert (format "[[file:%s][%s]]" new-file-path full-file-name))
    (if (string-equal filetype "drawio")
        (progn
          (insert (format "\n[[file:%s/%s_%s.png]]\n"
                          eye-org-file-attach-base-dir
                          timestr filename))
          ))
    )
  )

(defun eye/new-mind-map ()
  (interactive)
  (save-excursion
    (let* ((file (read-string "Map name: "))
           ;; (filename (concat file ".mm"))
           ;; (org-id (eye--get-org-id-no-url))
           (filename (format "%s_%s.mm"
                             (format-time-string "%Y%m%d_%H%M%S")
                             file))
           (template-path (concat (get-locale-book-dir) "/template/template.mm"))
           (new-file-path (concat eye-org-file-attach-base-dir "/" filename))
           )
      (f-copy template-path new-file-path)
      (insert (format "[[file:%s][%s]]" new-file-path filename))
      )
    )
  )

(defun eye/new-drawio ()
  (interactive)
  (save-excursion
    (let* ((file (read-string "Drawio name: "))
           (filename (format "%s_%s.drawio"
                             (format-time-string "%Y%m%d_%H%M%S")
                             file))
           (image-name (replace-regexp-in-string ".drawio" ".png" filename)) ;; 用于在后面添加同名的png文件
           ;; (org-id (eye--get-org-id-no-url))
           (timestr )
           (template-path (concat (get-locale-book-dir) "/template/template.drawio"))
           (new-file-path (concat eye-org-file-attach-base-dir "/" filename))
           )
      (f-copy template-path new-file-path)
      (insert (format "[[file:%s][%s]]" new-file-path filename))
      (newline)
      (insert (format "[[file:%s]]" (concat eye-org-file-attach-base-dir "/" image-name)))
      )
    )
  )


(defun eye/new-excel ()
  (interactive)
  (save-excursion
    (let* ((file (read-string "Excel name: "))
           (filename (format "%s_%s.xlsx"
                             (format-time-string "%Y%m%d_%H%M%S")
                             file))
           ;; (org-id (eye--get-org-id-no-url))
           (timestr )
           (template-path (concat (get-locale-book-dir) "/template/template.xlsx"))
           (new-file-path (concat eye-org-file-attach-base-dir "/" filename))
           )
      (f-copy template-path new-file-path)
      (insert (format "[[file:%s][%s]]" new-file-path filename))
      )
    )
  )


(defun eye/new-docx ()
  (interactive)
  (save-excursion
    (let* ((file (read-string "Excel name: "))
           (filename (format "%s_%s.docx"
                             (format-time-string "%Y%m%d_%H%M%S")
                             file))
           ;; (org-id (eye--get-org-id-no-url))
           (timestr )
           (template-path (concat (get-locale-book-dir) "/template/template.docx"))
           (new-file-path (concat eye-org-file-attach-base-dir "/" filename))
           )
      (f-copy template-path new-file-path)
      (insert (format "[[file:%s][%s]]" new-file-path filename))
      )
    )
  )


(defun eye/new-ppt ()
  (interactive)
  (save-excursion
    (let* ((file (read-string "Excel name: "))
           (filename (format "%s_%s.ppt"
                             (format-time-string "%Y%m%d_%H%M%S")
                             file))
           ;; (org-id (eye--get-org-id-no-url))
           (timestr )
           (template-path (concat (get-locale-book-dir) "/template/template.ppt"))
           (new-file-path (concat eye-org-file-attach-base-dir "/" filename))
           )
      (f-copy template-path new-file-path)
      (insert (format "[[file:%s][%s]]" new-file-path filename))
      )
    )
  )


(defun eye/new-uml ()
  (interactive)
  (save-excursion
    (let* ((file (read-string "UML File Name: "))
           ;; (filename (concat file ".uml"))
           (filename (format "%s_%s.uml"
                             (format-time-string "%Y%m%d_%H%M%S")
                             file))
           ;; (org-id (eye--get-org-id-no-url))
           (template-path (concat (get-locale-book-dir) "/template/template.uml"))
           (new-file-path (concat eye-org-file-attach-base-dir "/" filename))
           )
      (f-copy template-path new-file-path)
      (insert (format "[[file:%s][%s]]" new-file-path filename))
      )
    )
  )


(defun eye-open-file (percent-file-path &optional line-num goto-str)
  "用于外部程序调用emacsclient打开文件，line-num为跳转到哪个行号"
  (interactive)
  (let ((real-file-path (decode-coding-string (url-unhex-string percent-file-path)  'utf-8)))
    (when (file-exists-p real-file-path)
      ;;(message "open:%s" real-file-path)
      (find-file real-file-path)
      (when line-num
        (goto-line line-num)
        (when goto-str
          (search-forward goto-str)
          )
        )
      )
    ))


;; 使用系统程序打开mindmap文件
(add-to-list 'org-file-apps '("\\.mm\\'" . system))
(add-to-list 'org-file-apps '("\\.drawio\\'" . system))

(defun eye-reset-image-size-at-point ()
  "重新修改当前显示图片大小"
  (interactive)
  (let* ((image-path (eye--get-image-file-path-at-point))
         (resized-path (s-replace ".png" "_s.png" image-path))
         (exe-path (executable-find "resize_image.exe"))
         (target-w (read-string "Resized to width: "))
         (target-h (read-string "Resized to height: "))
         (output-file "")
         )
    (message "resize %s to %s" image-path resized-path)
    (f-delete resized-path)
    (resize-command (format "%s %s %s %s %s"
			    resize-path
			    fullpath
                            target-w target-h
			    resized-fullpath))

    (if (f-exists-p resized-path)
        (setq output-file resized-path)
      (setq output-file image-path))
    (beginning-of-line)
    (kill-visual-line)
    (insert (format "[[file:%s/%s]]"
                    eye-org-file-attach-base-dir
		    (file-name-nondirectory fullpath)))
    )
  )


(defun eye--get-image-file-path-at-point ()
  (save-excursion
    (let* (begin end relpath abspath basename will-open-filepath)
      (search-backward "[[file:")
      (setq begin (+ 7 (point)))
      (search-forward "]")
      (setq end (- (point) 1))
      (setq relpath (buffer-substring-no-properties begin end))
      (setq basename (file-name-base relpath))
      (setq will-open-filepath relpath)
      ;; use origin image
      (when (string-equal (s-right 2 basename) "_s")
        ;; remove _s
        (setq relpath (s-replace basename
                                 (s-left (- (length basename) 2) basename)
                                 relpath))
        (if (file-exists-p relpath)
            (setq will-open-filepath relpath)))

      (when will-open-filepath
        ;; convert slash
        (setq will-open-filepath (s-replace "~" (getenv "HOME") will-open-filepath))
        (setq will-open-filepath (s-replace "/" "\\" will-open-filepath))
        )
      will-open-filepath
      ))
  )

(defun eye-open-image-at-point ()
  "打开org光标处的图片，注意要将光标放到图片后边"
  (interactive)
  (let* ((will-open-filepath (eye--get-image-file-path-at-point)))
    (when will-open-filepath
      (message "open image:%s" will-open-filepath)
      (async-shell-command-no-window (concat "C:\\Windows\\SysWOW64\\explorer.exe " will-open-filepath))
      )
    )
  )

(defun eye-copy-file-at-point ()
  "复制org光标处的文件"
  (interactive)
  (save-excursion
    (let* (begin end relpath abspath basename will-open-filepath target-dir)
      (search-backward "[[file:")
      (setq begin (+ 7 (point)))
      (search-forward "]")
      (setq end (- (point) 1))
      (setq relpath (buffer-substring-no-properties begin end))
      (setq basename (file-name-base relpath))
      (setq will-open-filepath relpath)
      ;; (message "copy %s" will-open-filepath)
      (setq target-dir "d:/tmp");
      (if (y-or-n-p "复制到指定目录？")
          (setq target-dir (read-string "Target dir: ")))
      (if (not (f-directory-p target-dir))
          (f-mkdir target-dir))

      (copy-file will-open-filepath (concat target-dir "/") t)
      (eye-open-dir target-dir)
      )
    ))


(defun eye/add-export-html-option ()
  (interactive)
  (insert "#+SETUPFILE: x:/orgnotes/org/theme.setup")
  )


;; (file-writable-p "d:/tmp/20211108145927.html")
(setq eye-html-export-dirctory "d:/tmp/")
(defun eye-export-html-and-open ()
  (interactive)
  (require 'ox-html)

  (save-excursion
    (goto-char (point-min))
    (when (not (search-forward "/theme.setup" nil t))
      (search-forward "#+DATE:" nil t)
      (end-of-line)
      (newline)
      (insert "#+SETUPFILE: x:/orgnotes/org/theme.setup")
      (newline)
      (save-buffer)
      )

    ;; (org-html-export-to-html)
    ;; 导出到临时目录下
    (let* ((extension (concat
		               (when (> (length "html") 0) ".")
		               "html"))
           (file (org-export-output-file-name extension nil eye-html-export-dirctory))
           (org-export-coding-system org-html-coding-system))
      (message "export to :%s" file)
      (delete-file file) ;; 保证导出最新的文件
      ;;    (touch file)
      (org-export-to-file 'html file)
      (when (and (y-or-n-p "Open html in browser?")
                 (file-exists-p file))
        (browse-url file)
        ))))



(provide 'init-org-note-attach)
