(defun eye/open-bookmarks ()
  (interactive)
  (eye--open-bookmarks (concat org-directory "/bookmarks.org")))

(defun eye/open-bookmark-work ()
  (interactive)
  (eye--open-bookmarks (concat org-directory "/bookmarks-work.org")))



(defun eye/insert-src-block-example ()
  (interactive)
  (insert (format "#+begin_example")) ;; 要大写才能显示图标？
  (org-newline-and-indent) ;; 自动缩进
  ;;(newline-and-indent)
  (insert "#+end_example")
  (org-newline-and-indent) ;; 自动缩进
  (previous-line 2) ;; 进到内容编辑区域
  (org-edit-src-code)
  )

(defun eye/insert-src-block-quote ()
  (interactive)
  (insert (format "#+begin_quote"))
  (org-newline-and-indent) ;; 自动缩进
  ;;(newline-and-indent)
  (insert "#+end_quote")
  (org-newline-and-indent) ;; 自动缩进
  (previous-line 2) ;; 进到内容编辑区域
  (end-of-line)
  )

(defun eye/insert-src-block-plantuml ()
  (interactive)
  (insert (format "#+begin_src plantuml :cmdline -charset utf-8 :file %s/%s"
                  eye-org-file-attach-base-dir
                  (format-time-string "%Y-%m-%d_%H-%M-%S.svg")))
  (org-newline-and-indent) ;; 自动缩进
  (insert "@startuml\n\nscale 2
!theme cerulean-outline
'修正中文字体
skinparam defaultFontName YaHei

'标题
title 链接库关系图

@enduml\n")
  (insert "#+end_src")
  (org-newline-and-indent) ;; 自动缩进
  (previous-line 3) ;; 进到内容编辑区域
  (org-edit-src-code)
  )

(defun eye/insert-src-block-mindmap ()
  (interactive)
  (insert (format "#+begin_src plantuml :cmdline -charset utf-8 :file %s/%s"
                  eye-org-file-attach-base-dir
                  (format-time-string "%Y-%m-%d_%H-%M-%S.svg")))
  (org-newline-and-indent) ;; 自动缩进
  (insert "@startmindmap\n\n@endmindmap\n")
  (insert "#+end_src")
  (org-newline-and-indent) ;; 自动缩进
  (previous-line 3) ;; 进到内容编辑区域
  (org-edit-src-code)
  )

;; 交互式选择插入代码块 @See http://wenshanren.org/?p=327
(defun eye/org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
	  '("c" "c++" "shell" "emacs-lisp" "java" "lisp" "dart" "mytex" "quote" "python" "js" "clojure" "css"
	    "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
	    "octave" "oz" "plantuml" "mindmap" "R" "sass" "screen" "sql" "awk" "ditaa"
	    "haskell" "latex" "matlab" "ocaml" "org" "perl" "ruby"
	    "scheme" "sqlite")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    ;;(newline-and-indent) ; no auto indent space

    (cond ((string-equal "example" src-code-type)
           (eye/insert-src-block-example))
          ((string-equal "quote" src-code-type)
           (eye/insert-src-block-quote))
          ((string-equal "plantuml" src-code-type)
           (eye/insert-src-block-plantuml))
          ((string-equal "mindmap" src-code-type)
           (eye/insert-src-block-mindmap))
          ((string-equal "mytex" src-code-type)
           (eye/insert-src-block-mytex))
          (t
           (insert (format "#+begin_src %s" src-code-type))
           (org-newline-and-indent)
           (insert "#+end_src")
           (org-newline-and-indent)
           (previous-line 2) ;; 进到内容编辑区域
           (org-edit-src-code)
           ))
    ))





(defun eye/org-insert-create-date ()
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (org-set-property "CREATED" (format-time-string "%Y-%m-%d %T"))))




(defun eye--execute-cmd (exe args)
  "执行命令，并获取打印结果"
  (let* ((tmp-file "d:/tmp/tmp.dat")
         result-content command)
    ;; 第一个参数固定为命令行输出文件
    (setq command (format "%s %s %s" exe tmp-file args))
    ;; (message "execute cmd:%s" command)
    (shell-command command)
    (setq result-content (get-string-from-file tmp-file))
    (delete-file tmp-file)
    result-content
    )
  )


(defun eye--open-bookmarks (&optional bookmarks-file)
  (let ((result (eye--execute-cmd (executable-find "list-bookmarks.exe") bookmarks-file))
        sel-list select-item url
        )
    (if (and result (not (string-empty-p result)))
        (progn
          (setq sel-list (s-split "\n" result))
          (setq select-item (ivy-read "Open bookmark: " sel-list))
          (setq sel-list (s-split "::" select-item))
          (setq url (cadr sel-list))
          ;; (browse-url-default-browser (cadr sel-list))
          (let ((my/default-browser (ivy-read "Use browser: " '("firefox" "chrome"))))
            (eye/open-url url))
          )
      (message "bookmarks is empty.");
      )))

;; 只折叠当前root heading
;; @see https://christiantietze.de/posts/2019/06/org-fold-heading/
(defun eye/org-foldup ()
  "Hide the entire subtree from root headline at point."
  (interactive)
  (while (ignore-errors (outline-up-heading 1)))
  (org-flag-subtree t))


(defun eye/org-shifttab (&optional arg)
  (interactive "P")
  (if (or (null (org-current-level))     ; point is before 1st heading, or
          (and (= 1 (org-current-level)) ; at level-1 heading, or
               (org-at-heading-p))
          (org-at-table-p))              ; in a table (to preserve cell movement)
      ; perform org-shifttab at root level elements and inside tables
      (org-shifttab arg)
      ; try to fold up elsewhere
      (eye/org-foldup)))
(org-defkey org-mode-map (kbd "S-<tab>") 'eye/org-shifttab)


(defun eye-get-time-week (time)
  (let ((week (format-time-string "%A" time)))
    (cond
     ((string-equal "Monday"    week) "周一")
     ((string-equal "Tuesday"   week) "周二")
     ((string-equal "Wednesday" week) "周三")
     ((string-equal "Thursday"  week) "周四")
     ((string-equal "Friday"    week) "周五")
     ((string-equal "Saturday"  week) "周六")
     ((string-equal "Sunday"    week) "周日")
     (t nil)
     )))




(defun eye/org-add-sibling-headline ()
  "创建同级heading，当折叠的情况下，到不了最后位置，要用org-show-entry执行一下"
  (interactive)
  (let* ((level nil))
    (if (not (org-element-property :title (org-element-at-point)))
        (org-previous-visible-heading 1))

    (setq level (org-element-property :level (org-element-at-point)))
    (when level
      (org-show-subtree)
      (org-end-of-subtree)
      (newline)
      (dotimes (_ level) (insert "*"))
      (insert " ")
      )
    ))

(defun eye/org-add-child-headline ()
  "创建子级heading，当折叠的情况下，到不了最后位置，要用org-show-entry执行一下"
  (interactive)
  (let* ((level nil))
    (if (not (org-element-property :title (org-element-at-point)))
        (org-previous-visible-heading 1))

    (setq level (org-element-property :level (org-element-at-point)))
    (when level
      (org-show-subtree)
      (org-end-of-subtree)
      (newline)
      (dotimes (_ (+ 1 level)) (insert "*"))
      (insert " ")
      )
    ))


(defun eye/export-org-to-docx (&optional filename auto-open)
  (interactive)
  (if (executable-find "pandoc")
      (let* ((org-file-name (or filename (buffer-name)))
             (docx-file (concat (file-name-sans-extension org-file-name) ".docx")))
        (save-buffer)
        (when (file-exists-p docx-file) (delete-file docx-file))
        (shell-command (format "pandoc %s -o %s --reference-doc=template.docx"
                               org-file-name
                               docx-file
                               ))
        (if auto-open (org-open-file docx-file))
        (message "Convert finish: %s" docx-file)
        docx-file
        )
    (message "No pandoc found!")
    )
  )

(defun eye/export-org-to-docx-with-image (open-docx)
  (interactive)
  (let* ((filename (buffer-name))
         (base-name (file-name-sans-extension filename))
         (temp-file (concat base-name ".docx.org"))
         )
    (with-temp-buffer
      (insert-file-contents filename)
      ;; replace file:~ to actual home path
      (beginning-of-buffer)
      (replace-string "[[file:~/"
                      (concat "[[file:"
                              (subst-char-in-string
                               ?\\ ?/
                               (getenv "HOME"))
                              "/"))

      ;; replace file_s.png]] to file.png]]
      (beginning-of-buffer)
      (replace-string "_s.png]]" ".png]]")

      ;; write to temp file
      (write-file temp-file)
      )

    ;; convert temp file
    (eye/export-org-to-docx temp-file open-docx)
    (when (file-exists-p temp-file) (delete-file temp-file))
    ))

(defun eye/export-org-to-pdf ()
  (interactive)
  (let* (
         (base-name (file-name-sans-extension (buffer-name)))

         (exe (executable-find "convert_docx_to_pdf"))
         (docx-file (format "%s/%s.docx.docx"
                            (file-name-directory (buffer-file-name))
                            base-name
                            ))
         (pdf-file (format "%s/%s.pdf"
                           (file-name-directory (buffer-file-name))
                           base-name
                           ))
         (cmd (format "%s %s %s" exe
                      (url-encode-url docx-file)
                      (url-encode-url pdf-file)))
         )

    (eye/export-org-to-docx-with-image nil)
    (if (file-exists-p docx-file)
        (progn
          (shell-command cmd nil nil)
          (org-open-file pdf-file)
          (message "Convert to pdf finished."))
      (message "docx file not found!")
      )
    ))


(defun eye/copy-org-id-link ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let* ((id (org-id-get-create))
           (title (org-element-property :title (org-element-at-point)))
           (org-link "")
           )
      ;; (setq org-link (format "[[id:%s][%s]]" id title))
      ;; 由于默认的[[id:xxxx]]在大的journals文件中可能会卡住emacs或者报错，这里使用pos:
      (setq org-link (format "[[pos:%s][%s]]" (s-left 8 id) title))
      (kill-new org-link)
      (save-buffer)
      (message "Copied link: %s" org-link)
      ))
  )

;; @See https://hungyi.net/posts/copy-org-mode-url/
(defun eye/copy-org-link-at-point ()
  "Copies the URL from an org link at the point"
  (interactive)
  (let ((plain-url (url-get-url-at-point)))
    (if plain-url
        (progn
          (kill-new plain-url)
          (message (concat "Copied: " plain-url)))
      (let* ((link-info (assoc :link (org-context)))
             (text (when link-info
                     (buffer-substring-no-properties
                      (or (cadr link-info) (point-min))
                      (or (caddr link-info) (point-max))))))
        (if (not text)
            (error "Oops! Point isn't in an org link")
          (string-match org-link-bracket-re text)
          (let ((url (substring text (match-beginning 1) (match-end 1))))
            (kill-new url)
            (message (concat "Copied: " url))))))))





(defun eye--not-actionable-next ()
  (let ((read-answer-short t))
  (read-answer "trash(d), maybe(m), reference(r), select:"
     '(("trash" ?d "move to trash?")
       ("maybe" ?m "someday/maybe")
       ("ref"  ?r "reference")))))

(defun eye/process-inbox-item ()
  "根据GTD流程自动处理inbox item"
  (interactive)
  (if (y-or-n-p "Is it actionable?")
      (if (y-or-n-p "Will it take less than 2 minutes?")
	  (message "Do it now!")
	(progn
	  (org-todo)
	  (org-refile)))
    (progn
      (let ((ret (eye--not-actionable-next)))
	(cond ((string-equal ret "trash") (org-todo "CANCELLED"))
	      ((string-equal ret "maybe") (org-todo "MAYBE"))
	      ((string-equal ret "ref") (message "Please move it to reference note."))
	      ))
      )))


;; inspire from https://fuco1.github.io/2019-02-10-Refiling-hydra-with-pre-defined-targets.html
(defun eye/refile ()
  "select project file for quick refile"
  (interactive)
  (let* ((targets (ivy-read "Move to:" org-agenda-files)))
    (org-refile-cache-clear)
     (let ((org-refile-target-verify-function nil)
           (org-refile-targets '((targets :maxlevel . 9))))
       (call-interactively 'org-refile)))
  )

;; 在agenda buffer中的[x/x]添加face
;; https://fuco1.github.io/2022-01-04-Add-fontification-for-progress-cookie-in-org-agenda.html
(defun my-fontify-progress-cookie ()
  "Fontify progress cookies in org agenda."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\[[[:digit:]]+/[[:digit:]]+\\]" nil t)
      (add-face-text-property (match-beginning 0) (match-end 0)
                              'org-checkbox-statistics-todo))
    (goto-char (point-min))
    (while (re-search-forward "\\[[[:digit:]]+%\\]" nil t)
      (add-face-text-property (match-beginning 0) (match-end 0)
                              'org-checkbox-statistics-todo))))
(add-hook 'org-agenda-finalize-hook 'my-fontify-progress-cookie)


(defun eye/insert-checkbox ()
  (interactive)
  (insert "- [ ] ")
  )


(defun eye/add-journal ()
  "open journal file and insert new note"
  (interactive)
  (let* ((journal-file (concat org-directory
                              (format-time-string "/journals/%Y-%m.org" (current-time))
                              ))
         (today-date (format-time-string "%Y-%m-%d" (current-time))) ;; today
         (goto-date (org-read-date)) ;; select date
         )
    (find-file journal-file)
    (beginning-of-buffer)
    (if (search-forward (format "* %s" goto-date) nil t)
        (progn
          (org-end-of-subtree)
          (newline)
          (insert "** ")
          )
      (progn
        (if (string-equal today-date goto-date)
            (progn ;; insert today note
              ;;(org-end-of-subtree)
              (end-of-buffer)
              (newline)
              (insert (format "* %s" goto-date))
              (newline)
              (insert "** ")
              )
          (message "Not found date %s" goto-date)
          )
        )
      )
    )
  )






(defun my-dnd-insert-link-v2 (uri action)
  (if (and (or (eq 'org-mode major-mode)
               (eq 'org-journal-mode major-mode))
	   ;;(string-match eye-match-drop-file-exp uri)
	   1 ;; ignore file extension
	   )
      (let* ((file-line (dnd-get-local-file-uri uri))
			 (attach-dir eye-org-file-attach-base-dir)
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


(defun eye/open-org-file-attach-dir ()
  "Open explorer of current buffer directory.
locale-notebook-dir use absolute path for advise.
打开附件目录
"
  (interactive)
  (when (eq system-type 'windows-nt)
    (let* ((dir eye-org-file-attach-base-dir)
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
    (let* ((dir eye-org-file-attach-base-dir)
	   (command (concat "thunar " dir)))
      (shell-command command)
    )
  ))




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
	     (attach-dir eye-org-file-attach-base-dir)
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
    (setq template-path (concat org-directory (format "/template/template.%s" suffix)))
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
           (template-path (concat org-directory "/template/template.mm"))
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
           (template-path (concat org-directory "/template/template.drawio"))
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
           (template-path (concat org-directory "/template/template.xlsx"))
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
           (template-path (concat org-directory "/template/template.docx"))
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
           (template-path (concat org-directory "/template/template.ppt"))
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
           (template-path (concat org-directory "/template/template.uml"))
           (new-file-path (concat eye-org-file-attach-base-dir "/" filename))
           )
      (f-copy template-path new-file-path)
      (insert (format "[[file:%s][%s]]" new-file-path filename))
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



(provide 'init-org-utils)

