(defun my-anchor-insert ()
  "插入一个锚点(not used)"
  (interactive)
  (let* ((uuid (my-generate-uuid))
         (desc (read-string "Desc:" "⚓"))
         )
    (setq uuid (s-left 8 uuid)) ;; 只要前面8位，避免太长
    ;; ⚓U+2693
    (insert (format "[[a:%s][%s]]" uuid desc))
    ;; (my-anchor-link-click uuid nil)
    )
  )

(defun eye-org-get-page-id ()
  "获取Page id"
  ;; (interactive)
  (save-excursion
    (beginning-of-buffer)
    (let* ((uid (s-left 8 (org-id-get-create))))
      (save-buffer)
      uid
      )
    ))


(defun eye--get-headling-id ()
  "获取当前heading的id"
  (let* ((uid (s-left 8 (org-id-get-create))))
    (save-buffer)
    uid
    )
  )


(setq eye-tmp-source-page-buffer "")

(defun mynote-copy-ref ()
  (interactive)
  (let* ((copyed-str ""))
    (if (region-active-p)
        (progn
          (setq copyed-str (format "[[pos:%s][%s]]" (eye--get-headling-id) (eye-get-region-text)))
          (deactivate-mark)
          )
      (progn
        (setq copyed-str (format "[[pos:%s][%s]]" (eye--get-headling-id) (read-string "Desc: " (eye-org-get-headline-or-page-title))))
        )
      )
    (kill-new copyed-str)
    (setq eye-tmp-source-page-buffer (buffer-name)) ;; 临时保存buffer名
    (message "Coped link: %s" copyed-str)
    ))

(defun eye/org-copy-link-desc ()
  (interactive)
  (save-excursion
    (let ((line-begin (line-beginning-position))
          (abegin 0)
          (aend 0)
          (tmpstr "")
          )
      (if (search-backward "[[a:" line-begin t)
          (progn
            (setq abegin (point))
            (search-forward "]]" (line-end-position) t)
            (setq aend (point));
            (setq tmpstr (buffer-substring-no-properties abegin aend))
            (setq tmpstr (cadr (s-split "\\]\\[" tmpstr)))
            (setq tmpstr (s-trim (s-replace "]]" "" tmpstr)))
            (kill-new tmpstr)
            (message "Copied %s" tmpstr)
            )))
    ))

(defun insert-linked-refs (ref-path)
  "read #refs#.org, parse filepath,line, get headline in line, insert refs"
  (interactive)
  (let* ((refcontent (get-string-from-file ref-path))
         (ref-count 0)
         (ref-list nil)
         (filepath "")
         (line 0)
         (insertcontent "")
         )
    (setq ref-list (s-split "\n" refcontent))
    (when ref-list
      (dolist (ref ref-list)
        (when (not (string-empty-p (s-trim ref)))
          (setq ref-count (+ ref-count 1))
          (setq filepath (car (s-split "," ref)))
          (setq line (cl-parse-integer (cadr (s-split  ","  ref))))

          (with-temp-buffer
            (insert-file-contents filepath)
            (org-mode)
            (org-show-all) ;; 展开所有节点，避免后面 outline-up-heading 报错。

            (goto-line line)

            (let ((is-break nil)
                  (title-list nil)
                  (title nil)
                  (is-top-level nil)
                  )
              (while (not is-break)
                (if (org-element-property :title (org-element-at-point))
                    (progn
                      ;; level
                      (if (= 1 (org-element-property :level (org-element-at-point)))
                          (progn
                            (setq is-top-level t)
                            )
                        (progn
                          (message "title:%s, org level is %s"
                                   (org-element-property :title (org-element-at-point))
                                   (org-element-property :level (org-element-at-point)))
                          (if (not (outline-up-heading 1)) ;; 注意，在org中不能使用overview，否则这里会报错
                              (setq is-top-level t)
                            )
                          (message "parse title finished")
                          )
                        )
                      )
                  (org-previous-visible-heading 1) ;; maybe same level headline
                  )

                (setq title (org-element-property :title (org-element-at-point)))
                (if title
                    (setq title-list (format "%s %s" title
                                             (if title-list (concat "➞ " title-list)
                                               ""
                                               )))
                  (setq is-break t)
                  )

                (if (and title (= 1 (org-element-property :level (org-element-at-point))))
                    (setq is-break t))
                ;; (message "find title:%s" title)
                )

              ;; (setq title-list (concat (format "[[ref:%s@%s][※]] " filepath line)
              ;;                          (eye-org-get-page-title filepath)
              ;;                          " >> " title-list
              ;;                          "\n: " (eye-org-get-line-content line)
              ;;                          ))

              (setq title-list (concat (format "** [[ref:%s@%s][" filepath line)
                                       (eye-org-get-page-title filepath)
                                       (if (string-empty-p title-list)
                                           "]]\n" ;; 没有headline时不需要添加箭头
                                         " ➞ " title-list "]]\n" ;; U+279E
                                         )
                                       ;; 移除前面的星号，空格，减号
                                       " - " (s-replace-regexp "^[* -]+ " "" (eye-org-get-line-content line))
                                       ))

              (setq insertcontent (concat insertcontent title-list "\n\n"))

              ;; (message "insertcontent:%s" insertcontent)
              )

            )
        )
      )
      )
    (insert (format "* %s Linked references\n\n%s" ref-count insertcontent))

    ))

(defun eye-org-get-line-content (line)
  ""
  (interactive)
  (save-excursion
    (goto-line line)
    (s-trim (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
    )
  )

(defun eye-org-get-page-title (filepath)
  "获取页面标题"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (let* ((title nil))
      (if (or (search-forward "#+title:" nil t)
              (search-forward "#+TITLE:" nil t))
          (progn
            (setq title (s-trim (buffer-substring-no-properties (point) (point-at-eol))))
            )
        (progn
          (setq title (file-name-base filepath))
          )
        )
      title
      )))

(defun eye-org-get-headline-or-page-title ()
  "获取当前heading内容，或者获取页面标题"
  (interactive)
  (let* ((headline (org-element-property :raw-value (org-element-at-point))))
    (if headline
        headline
      (eye-org-get-page-title (buffer-name))
      )
    ))


;; (insert-linked-refs (concat (get-locale-book-dir) "/#refs#.org"))


(defun mynote-update-ref()
  "更新反向链接"
  (interactive)
  (save-excursion
    ;; (beginning-of-buffer)
    (let* ((anchor (eye--get-headling-id))
           (keyword "d")
           (genexe (executable-find "gen_linked_refs"))
           (note-dir (concat (get-locale-book-dir) "/note"))
           (ref-path (concat (get-locale-book-dir) "/#refs#.org"))
           )
      (if anchor
          (progn
            (message "anchor:%s, keyword:%s" anchor keyword)
            (when (and (not (string-empty-p anchor))
                       (not (string-empty-p keyword))
                       )
              (let ((future (pfuture-new genexe
                                         "2" ;; run mode
				                         note-dir
				                         (url-encode-url anchor)
                                         (url-encode-url keyword)
				                         ref-path
				                         )))
	            (pfuture-await-to-finish future)
	            (message "gen_linked_refs search:%s, output: %s" anchor (string-trim (pfuture-result future)))
                ;; (get-buffer-create "*myrefs*")
                (with-current-buffer "*myrefs*"
                ;; (switch-to-buffer "*myrefs*")
                  (erase-buffer)
                  ;; (kill-region (point) (point-max))
                  (insert-linked-refs ref-path)
                  (org-mode)
                  (org-indent-mode 1)

                  )
                )
              )
            )
        (message "No anchor in this point!")
        )
      )
    )
  )



(defun eye/search-anchor-refs()
  "搜索当前光标处anchor的引用和提及"
  (interactive)
  (save-excursion
    (let* ((p1 (search-backward "[[a:" nil t))
           (p2 0)
           (p3 0)
           (anchor "")
           (keyword "")
           )
    (when (> p1 0)
      (setq p1 (+ p1 4))
      (setq p2 (- (search-forward "][" nil t) 2))
      (setq anchor (buffer-substring-no-properties p1 p2))
      (setq p1 (+ p2 2))
      (setq p3 (- (search-forward "]]" nil t) 2))
      (setq keyword (buffer-substring-no-properties p1 p3))
      (message "anchor:%s, keyword:%s" anchor keyword)
      (when (and (not (string-empty-p anchor))
                 (not (string-empty-p keyword))
                 )
        (let* ((genexe (executable-find "gen_linked_refs"))
               (note-dir (concat (get-locale-book-dir) "/note"))
               )
          (let ((future (pfuture-new genexe
                                     "2" ;; run mode
				                     note-dir
				                     (url-encode-url anchor)
                                     (url-encode-url keyword)
				                     (concat (get-locale-book-dir) "/#refs#.org")
				                     )))
	        (pfuture-await-to-finish future)
	        (message "gen_linked_refs search:%s, output: %s" anchor (string-trim (pfuture-result future)))
            ;; (switch-to-buffer "#refs#.org") ;; 注：打开了多个文件的话，buffer名会不一样
            ;; (revert-buffer)
            )
          )
        )
      )
    )
    ))



(defun my-anchor-pos-open (path _)
  "按回车打开链接位置，最好把org-startup-folded 设置为nil，不要折叠，方便跳转到内部位置。"
  (let* (
         ;;(default-directory (concat locale-notebook-dir "/org"))
         (search-dir (get-locale-book-dir))
         (uuid path)
         (org-startup-folded nil)
         )
    ;; (xah-next-window-or-frame)
    (async-shell-command-no-window (format "search-anchor %s %s" search-dir uuid))
    ))


(defun my-anchor-link-click (path _)
  (save-excursion
    (let* ((act (my-anchor-ask "Action, copy link(c), copy external link(l): ")))
      (if (string-equal act "copy")
          (progn
            (let* ((p1 (+ (search-backward "[[a:" nil t) 2))
                   (p2 (- (search-forward "]]") 2))
                   ;; 默认用关键词
                   (desc (read-string "Desc:"
                                      ;;(buffer-substring-no-properties p1 p2)
                                      ))
                   (full-link (format "[[pos:%s][%s]]" path desc))
                   )
              (kill-new full-link)
              (message "Copied %s" full-link)
              ))
        )
      (if (string-equal act "link")
          (progn
            (w32-set-clipboard-data (format "org://exec/open-anchor/%s" path))
            (message "Copied org anchor link %s" path)
            )
        )
      )
  ))

(defun eye/org-set-anchor ()
  "选中关键词，然后创建链接，如[[a:xxxx][keyword]]"
  (interactive)
  (if (region-active-p)
      (let* ((p1 (region-beginning))
             (p2 (region-end))
             ;; (keyword (buffer-substring-no-properties p1 p2))
             (uuid (my-generate-uuid))
             (kid "")
             )
        ;; (setq kid (eye--get-keyword-id keyword))
        (if (string-empty-p kid)
            (setq kid (s-left 8 uuid)))
        (goto-char p2)
        (insert "]]")
        (goto-char p1)
        (insert (format "[[a:%s][" kid))
        (message "set keyword to id:%s" kid)
        )
    ;; (message "No region selected!")
    (let* (
           (uuid (my-generate-uuid))
           (kid (s-left 8 uuid))
           )
      (insert (format "[[a:%s][⚓]]" kid))
      ))
  )


(defun eye/org-set-link ()
  "选中关键词，添加链接"
  (interactive)
  (if (region-active-p)
      (let* ((p1 (region-beginning))
             (p2 (region-end))
             ;; (desc (buffer-substring-no-properties p1 p2))
             (link (read-string "Set link: "))
             )
        (setq link (s-trim link)) ;; 去除空白
        (goto-char p2)
        (insert "]]")
        (goto-char p1)
        (insert (format "[[%s][" link))
        )
    (message "No region selected!")
    )
  )

(defun eye-get-region-text ()
  (if (region-active-p)
      (let* ((p1 (region-beginning))
             (p2 (region-end))
             (text (buffer-substring-no-properties p1 p2))
             )
        text
        )
    nil))

(defun eye/org-set-tag ()
  "选中关键词，设置为tag"
  (interactive)
  (save-excursion
    (if (region-active-p)
        (let* ((p1 (region-beginning))
               (p2 (region-end))
               (desc (buffer-substring-no-properties p1 p2))
               ;; (link (read-string "Set link: "))
               )
          ;; (setq link (s-trim link)) ;; 去除空白
          (goto-char p2)
          (insert "]]")
          (goto-char p1)
          (insert (format "[[tag:%s][#" desc))
          ;; (message (format "[[tag:%s][#" desc))
          )
      (message "No region selected!")
      )
    ))


(defun my-anchor-ask (ask)
  (let ((read-answer-short t))
    (read-answer ask
                 '(("copy"  ?c "copy link")
                   ("ref"   ?r "search ref")
                   ("link"   ?l "copy external link")
                   ("quit"  ?q "exit")))))


;; 问题：anchor支持多处位置后，引用指向哪里？
(defun eye/insert-pos-link ()
  "插入引用"
  (interactive)
  (let* (
         (default-directory (get-locale-book-dir)
           )
	 (current-filename (file-name-base (buffer-name)))
         (search-exe (executable-find "list-anchors"))
         (result-file (concat default-directory "/anchors.dat"))
         command sel-list select-item
         )
    (setq command (format "%s %s --" search-exe default-directory))
    ;; temp-path sel-list)
    (shell-command command)
    (setq result-content (get-string-from-file result-file))
    (setq sel-list (s-split "\n" result-content))

    (setq select-item (ivy-read "Insert: " sel-list))
    (when select-item
      (setq select-item (s-replace "a:" "pos:" select-item))
      (insert select-item)
      ;; (message "insert:%s" select-item)
      )
    ))

(defun eye--get-anchor-link-id (link)
  (let* ((tmp link))
    (setq tmp (s-replace "a:" "" tmp))
    (setq tmp (s-replace "[[" "" tmp))
    (setq tmp (s-left 8 tmp))
    tmp
    ))
;; (eye--get-anchor-link-desc "[[a:12345678][1234]]")

(defun eye/create-ref-link ()
  "基于已有文本新建引用，并自动创建新页面"
  (interactive)
  (let* ((sel-text (or (eye-get-region-text) (read-string "new ref link: ")))
         (default-directory (get-locale-book-dir))
         (search-exe (executable-find "list-anchors"))
         (result-file (concat default-directory "/anchors.dat"))
         (new-uuid (s-left 8 (my-generate-uuid)))
         command sel-list select-item
         )
    (setq command (format "%s %s --" search-exe default-directory))
    ;; temp-path sel-list)
    (shell-command command)
    (setq result-content (get-string-from-file result-file))
    (setq sel-list (s-split "\n" result-content))
    (add-to-list 'sel-list (concat sel-text "(new file)"))
    (setq select-item (ivy-read "Insert: " sel-list :initial-input sel-text))
    (when select-item
      (if (string-match "a:" select-item)
          (progn ;; 插入已有的anchor
            (if (region-active-p)
                (progn ;; 有选中文本时，添加已有链接
                  (backward-char (length sel-text))
                  (insert (format "[[pos:%s][" (eye--get-anchor-link-id select-item)))
                  (forward-char (length sel-text))
                  (insert "]]")
                  )
              (progn ;; 没有选中文本时，添加已有链接
                (setq select-item (s-replace "a:" "pos:" select-item))
                (insert select-item)
                )
              ))
        (progn ;; create new page
          ;; goto beginning of select text
          (backward-char (length sel-text))
          (insert (format "[[pos:%s][" new-uuid))
          (forward-char (length sel-text))
          (insert "]]")
          (save-buffer)
          ;; create new page
          (eye/org-new-file-by-date sel-text new-uuid)
          (message "Created new page:%s" sel-text)
          )
        )
      ;; (message "insert:%s" select-item)
      )
    ))


(defun mynote-paste-ref ()
  (interactive)
  (cua-paste nil)
  (save-buffer)
  (when (not (string-empty-p eye-tmp-source-page-buffer))
    (let* ((cur-buffer (buffer-name)))
      (save-excursion
        (switch-to-buffer eye-tmp-source-page-buffer)
        (eye/update-page-refs)
        )
      (switch-to-buffer cur-buffer)
    )))



(org-link-set-parameters "pos"
                         :face '(:foreground "BlueViolet" :underline t) ;;  inherit
                         :follow #'my-anchor-pos-open
                         ;; :complete 'eye--org-link-pos-completion
                         )


(org-link-set-parameters "a"
                         :face '(:foreground inherit :underline t)
                         :follow #'my-anchor-link-click
                         )


(org-link-set-parameters "tag"
                         :face '(:foreground "SeaGreen4" :underline nil)
                         ;; :follow #'my-anchor-link-click
                         )


;; org://exec/open-anchor/anchorid
(defun open-anchor (anchor-id)
  (let* ((default-directory (concat (get-locale-book-dir) "/note"))
         (search-dir default-directory)
         )
    ;; (xah-next-window-or-frame)
    (async-shell-command-no-window (format "search-anchor %s %s" search-dir anchor-id))
    )
  )




(setq display-buffer-alist
      '(("*myrefs*"
	 (display-buffer-reuse-window
	  display-buffer-in-side-window)
	 (reusable-frames . visible)
	 (side            . right)
	 (window-height   . 0.33))))


(defun eye/open-ref-side-window()
  (interactive)
  (switch-to-buffer-other-window "*myrefs*")
  ;; (save-excursion
  ;;   (find-file (concat (get-locale-book-dir) "/#refs#.org"))
  ;;   (goto-char (point-min))
  )

(defun eye/org-show-page-refs-v2 ()
  (interactive)
  (save-excursion
    ;; (eye/open-ref-side-window)
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




(provide 'init-org-anchor-link)
