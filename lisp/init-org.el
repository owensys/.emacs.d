;;;; org
(setq org-modules-loaded t) ;; 加速orgmode 加载，https://emacs-china.org/t/org-babel/18699/12

(require 'org)

(defun get-locale-book-dir ()
  (let* ((file-content (get-string-from-file "~/.notedir")))
    (require 's)
    (s-trim file-content)
    ))

;;(add-to-list 'auto-mode-alist '("\\.gtd$" . org-mode))

;; 9.3使用<s需要org-tempo
(when (>= emacs-major-version 27)
  (require 'org-tempo))


(progn

  (setq org-src-fontify-natively t) ;; 代码块内语法高亮
  (setq org-src-tab-acts-natively t)
  ;; (setq org-src-window-setup 'current-window) ;; 在当前window打开src block
  ;; (add-hook 'org-mode-hook 'yas-minor-mode)
  ;; indent content
  (setq org-edit-src-content-indentation 0) ;; 代码块默认不缩进

  ;; 不开启org-indent-mode
  ;;(setq org-startup-indented nil)
  ;;(setq-default org-startup-indented nil)
  ;; 开启org-indent-mode，此模式并不修改文件内容，正文即使缩进显示了，内容实际还是在行首的
  (setq org-startup-indented t)
  (setq-default org-startup-indented t)

  (setq org-hide-block-startup nil) ;; 是否折叠代码块
  (setq org-startup-folded nil) ;; 是否折叠
  ;; 默认显示图片出来
  (setq org-startup-with-inline-images t)
  ;; 保留几行空白行
  (setq org-cycle-separator-lines 0)
  ;; always require new line in header below
  (setq require-final-newline t)
  (setq org-tags-column 0)
  (setq org-return-follows-link t) ;; 是否回车打开link
  (setq org-startup-truncated nil)
  (setq org-fontify-quote-and-verse-blocks t)  ;; 开启begin quote区域样式
  (setq org-confirm-babel-evaluate nil) ;; code执行免应答（Eval code without confirm）

  ;; 修改headline的折叠标记
  ;;(setq org-ellipsis " ")
  ;; (setq org-ellipsis "…")  ;; ↩
  (setq org-ellipsis " [+]")
  (set-face-underline 'org-ellipsis nil) ;; 去除下划线
  ;; (set-face-underline 'org-link nil)
  )

;; 支持imagemagic时，使用缩略图
;; 在链接前添加信息：#+ATTR_ORG: :width 300
;; (when (string-match-p "imagemagic" system-configuration-features)
  ;; (setq org-image-actual-width nil)
;; )
;; 在环境变量中有imagemagic，不需要判断system-configuration-features
(require 'ox)
(setq org-image-actual-width '(600)) ;; 没有设置#+ATTR_ORG时，最大显示宽度

;; 显示网络链接的图片
(setq org-display-remote-inline-images 'download)

(setq org-support-shift-select 1) ;; 是否支持shift+方向键选择
(setq org-fontify-emphasized-text t) ;; 高亮行内代码标记等 https://orgmode.org/manual/Emphasis-and-Monospace.html#Emphasis-and-Monospace
(setq org-hide-emphasis-markers t) ;; 隐藏斜体标记/text/，如果要删除，则确保光标移到斜体文字最后
;; 模板中的file路径不是绝对路径时，将会使用org-directory进行查找

;; 用圆形符号表示列表开头，匹配" - "
;;"•" ⮞ ☰
;; (font-lock-add-keywords 'org-mode
;; 			'(("^ +\\([-*]\\) "
;; 			   (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))



;; 使用系统程序打开文件
(setq org-file-apps
      '(("\\.docx\\'" . system)
	("\\.xlsx\\'" . system)
	("\\.pptx\\'" . system)
	("\\.zip\\'" . system)
	("\\.gz\\'" . system)
	("\\.bz\\'" . system)
	("\\.xz\\'" . system)
	("\\.7z\\'" . system)
	("\\.rar\\'" . system)
	("\\.x?html?\\'" . system)
	("\\.pdf\\'" . system)
	("\\.png\\'" . system)
	("\\.jpg\\'" . system)
	("\\.jpeg\\'" . system)
	("\\.gif\\'" . system)
	("\\.svg\\'" . system)
	(auto-mode . emacs)))


;; Speed keys, @see https://orgmode.org/manual/Speed-keys.html
;; quick navigation when cursor is on a headline (before any of the stars)
;; ?:for help, n/p/f/b...
(setq org-use-speed-commands t)

;;
;;#+SEQ_TODO: REPEAT(r) NEXT(n) TODO(t) WAIT(w@/!) DELEGATED(g@/!) PROJ(p) SOMEDAY(s) | DONE(d!) CANCELLED(c@/!)
;;#+SEQ_TODO: GOAL(G) | ACHIEVED(a@) MISSED(m@)

;; (setq org-todo-keywords
;;       '(
;;         (sequence "REPEAT(r)" "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "MAYBE(m)" "DELEGATED(g@/!)" "|" "DONE(d!)" "CANCELLED(c@/!)" "STUCK(s)")
;;         (sequence "GOAL(G) " "|" " ACHIEVED(a@)" "MISSED(m@)")
;;         ))


;; logbook
(setq org-log-into-drawer t)

(define-key org-mode-map (kbd "M-RET") 'org-insert-heading-respect-content)
(define-key org-mode-map (kbd "C-k") nil)

;; 默认显示几个等级标题的内容，参考：https://emacs-china.org/t/orgmode/8673/3
;; 1.可以用 local 变量，在文件后加上
  ;;; Local Variables:
  ;;; eval: (org-content 2)
  ;;; End:
;; 用local变量时会提示，要禁止提示可以用：(setq safe-local-variable-values (quote ((eval org-content 2))))
;; 2.(org-show-children 2) ;; 不起作用
;; 3.全局hook，在文件头加上 # -*- goer-org-startup-folded: 2; -*-
;; 有提示，可以按!，下次不再提示
(add-hook (quote hack-local-variables-hook)
          (lambda ()
            (let ((symbol (quote goer-org-startup-folded)))
              (when (and (eq major-mode (quote org-mode))
                         (boundp symbol))
                (let ((value (symbol-value symbol)))
                  (when (and value (integerp value))
                    (org-shifttab value)))))))

(defun eye--org-mode-hook-setup ()
  ;; Line wrapping
  (visual-line-mode 1)

  ;; 设置行距
  ;; (setq-local line-spacing 0.45) ;; 此设置会导致图片在滚动时老是闪烁
  (setq org-startup-indented t)
  ;; 不开启缩进时，也要关闭 electric-indent-local-mode
  (electric-indent-local-mode 1)
  ;; headline缩进宽度
  ;; (setq org-indent-indentation-per-level 4)

  ;; 默认缩进
  (setq tab-width 4)
  ;; 开启数字显示在head
  ;; (org-num-mode 1)

  ;; 在echo area中显示链接详情
  (require 'help-at-pt)
  (setq help-at-pt-display-when-idle t) ;; 不会立即生效
  (setq help-at-pt-timer-delay 0.5)
  (help-at-pt-set-timer) ;; 调用才会生效

  ;; 默认显示图片
  (org-display-inline-images)

  ;; 增加行间距
  ;; (custom-theme-set-faces
  ;;  'user
  ;;  '(variable-pitch ((t (:family en-font-name :height 180 :weight thin))))
  ;;  '(fixed-pitch ((t ( :family en-font-name :height 160)))))
  ;; (variable-pitch-mode 1)

  )


(global-set-key (kbd "C-c '") 'org-edit-src-code)



;;;; calendar
(setq-default
 calendar-date-style 'iso
 calendar-day-abbrev-array ["周日" "周一" "周二" "周三" "周四" "周五" "周六"]
 calendar-day-name-array ["周日" "周一" "周二" "周三" "周四" "周五" "周六"]
 ;;calendar-month-name-array ["一月" "二月" "三月" "四月" "五月" "六月" "七月" "八月" "九月" "十月" "十一月" "十二月"]
 calendar-month-name-array ["01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12"]
 calendar-week-start-day 1 ;; 日历从周一开始显示
 )


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
	  '("c" "c++" "shell" "emacs-lisp" "java" "lisp" "mytex" "quote" "python" "js" "clojure" "css"
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



;;;; ob-shell
;; support babel execute
(require 'ob-shell)
(org-babel-do-load-languages
 'org-babel-load-languages '((emacs-lisp . t)
			     (shell . t)))



;;;; htmlize
(eye/use-package 'htmlize
                 :ensure t
                 :load-path '("htmlize" "emacs-htmlize"))


;;;; plantuml
;; must set java.exe path
(eye/use-package 'plantuml-mode
                 :ensure t
                 :load-path "plantuml-mode"
                 :init
                 (progn
                   (setq org-plantuml-jar-path (expand-file-name "~/.emacs.d/bin/plantuml.jar"))
                   (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
                   (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))

                   )
                 :config
                 (progn
                   (setq plantuml-jar-path "~/.emacs.d/bin/plantuml.jar")
                   (setq plantuml-default-exec-mode 'jar)
                   ;; Enable plantuml-mode for PlantUML files
                   (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
                   ;; (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))

                   ))


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

(defun eye/open-bookmarks ()
  (interactive)
  (eye--open-bookmarks (concat (get-locale-book-dir) "/bookmarks.org")))

(defun eye/open-bookmark-work ()
  (interactive)
  (eye--open-bookmarks (concat (get-locale-book-dir) "/bookmarks-work.org")))

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



(require 'init-org-note-attach)


;;;; org-bullets
;; (eye-install-packages '(("org-bullets" . "https://github.com/sabof/org-bullets.git")))
(eye/use-package
 'org-bullets
 :ensure t
 :load-path "org-bullets"
 :config
 (progn
   ;; find emoji list here: http://unicode.org/emoji/charts/full-emoji-list.html#1f644
   ;; (setq org-bullets-bullet-list '("◉" "☯" "○" "☯" "✸" "☯" "✿" "☯" "✜" "☯" "◆" "☯" "▶"))
   ;; (setq org-bullets-bullet-list '("◉" "☯" "✿" "☯" "✸" "☯" "✿" "☯" "✜" "☯" "◆" "☯" "▶" "○"))
   ;; (setq org-bullets-bullet-list '("☯" "☯" "☯" "☯" "☯" "☯" "☯" "☯" "☯" "☯" "☯" "☯" "☯" "☯"))
   ;; (setq org-bullets-bullet-list '("⚫" "•" "•" "•" "•" "•" "•" "•" "•" "•" "•" "•" "•" "•")) ;; too small?
   ;; (setq org-bullets-bullet-list '("⚫" "⚫" "⚫" "⚫" "⚫" "⚫" "⚫" "⚫" "⚫" "⚫" "⚫" "⚫" "⚫" "⚫")) ;; #+26AB
   ;; (setq org-bullets-bullet-list '("☰" "◆" "○" "◆" "○" "◆" "○" "◆" "○" "◆" "○" "◆" "○"))
   ;; U+24B6++++
   ;; (setq org-bullets-bullet-list '("Ⓐ" "Ⓑ" "Ⓒ" "Ⓓ" "Ⓔ" "Ⓕ" "Ⓖ" "Ⓗ" "Ⓘ" "Ⓙ"))
   ;; U+2460+++
   ;; (setq org-bullets-bullet-list '("①" "②" "③" "④" "⑤" "⑥" "⑦" "⑧" "⑨" "⑩" "⑪" "⑫"))
   ;; (setq org-bullets-bullet-list '("☰" "▶" "▶" "▶" "▶" "▶" "▶" "▶" "▶" "▶" "▶" "▶" "▶" "▶"))
   ;; (setq org-bullets-bullet-list '("☰" "◆" "◆" "◆" "◆" "◆" "◆" "◆" "◆" "◆" "◆" "◆" "◆" "◆" "◆"))
   (setq org-bullets-bullet-list '("✿" "◉" "◉" "◉" "◉" "◉" "◉" "◉" "◉" "◉" "◉" "◉" "◉" "◉" "◉" "◉" ))


   (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
   ))



;;;; pretty symbols mode
(defun eye-setup-prettify-symbols ()
  (setq prettify-symbols-alist
                      '(("[#A]" . ?🅐)
                        ("[#B]" . ?🅑)
                        ("[#C]" . ?🅒)
                        ("[ ]" . ?☐)
                        ("[X]" . ?☑)
                        ("[-]" . ?❎)
                        ("#+ARCHIVE:" . ?📦)
                        ("#+AUTHOR:" . ?👤)
                        ("#+CREATOR:" . ?💁)
                        ("#+DATE:" . ?📆)
                        ("#+date:" . ?📆)
                        ("#+date:      " . ?📆) ;; for denote frontmatter
                        ("#+DESCRIPTION:" . ?🗎)
                        ("#+EMAIL:" . ?🖂)
                        ("#+OPTIONS:" . ?⚙)
                        ("#+STARTUP:" . ?⚙)
                        ("#+TAGS:" . ?⌦)
                        ("#+tags:" . ?⌦)
                        ("#+TITLE:" . ?✇) ;; U+2707
                        ("#+title:" . ?✇)
                        ("#+title:     " . ?✇)  ;; for denote frontmatter
                        ("#+ID:" . ?⚓) ;; U+2693
                        ("#+identifier:" . ?⚓)

                        ("#+BEGIN_SRC" . ?✎)
                        ("#+END_SRC" . ?□)
                        ("#+BEGIN_QUOTE" . ?»)
                        ("#+END_QUOTE" . ?«)
                        ("#+HEADERS" . ?☰)
                        ("#+RESULTS:" . ?💻)
                        ("#+FILETAGS:" . ?⌦)
                        ("#+filetags:" . ?⌦)
                        ("#+filetags:  " . ?⌦) ;; for denote frontmatter

                        ("#+begin_src" . ?✎)
                        ("#+end_src" . ?□)
                        ("#+begin_quote" . ?»)
                        ("#+end_quote" . ?«)
                        ("#+headers" . ?☰)
                        ("#+results:" . ?💻)
                        ("#+filetags:" . ?⌦)
                        ("#+SETUPFILE:" . ?⚙)
                        ("lambda" . ?λ))
                      )
  (prettify-symbols-mode 1)
  )
(add-hook 'org-mode-hook #'eye-setup-prettify-symbols)

(add-to-list 'load-path (concat eye-packages-dir "/transient/lisp"))

;;;; my anchor link
(require 'init-org-anchor-link)

(when is-gui
  ;; 支持图片滚动
  ;; 用good-scroll也支持图片滚动，不需要同时用两个包
  ;; 2021-12-15: 用iscoll更稳定一些，good-scroll可能是卡死
  (eye/use-package
   'iscroll
   :ensure t
   :load-path "iscroll"
   :config
   (progn
     (iscroll-mode 1)
     ))
  )


(require 'init-latex)


;;;; org-tree-slide
(eye/use-package 'org-tree-slide
                 :load-path "org-tree-slide"
                 :command '(org-tree-slide-mode)
                 :ensure nil
                 )


(add-hook 'org-mode-hook 'eye--org-mode-hook-setup)


;;; readonly file
(defun ansible-check-managed ()
  "Check if the file contain \"Ansible managed\" and notify user."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (search-forward ":readonly:"
                          (point-at-bol 10) t)
      (notify-managed-file "This file is readonly default"))))

(defun notify-managed-file (&optional msg)
  "Change the buffer to read-only and alert the user.
                    MSG change default message."
  (interactive)
  (read-only-mode t)
  (if msg
      (message msg)
      (message "Alert")))

(add-hook 'find-file-hook #'ansible-check-managed)


;;;; deft
(eye/use-package
 'deft
 :ensure t
 :load-path "deft"
 :command 'deft
 :config
 (progn
   (setq deft-recursive t)
   (setq deft-use-filename-as-title nil) ;;是否把文件名作为标题
   (setq deft-extensions '("org"))
   (setq deft-directory (get-locale-book-dir))
   (setq deft-file-limit 1000) ;;最多显示多少文件，nil不限制
   (setq deft-filter-only-filenames t) ;;只搜索文件名
   ;;(setq deft-filter-only-filenames nil) ;;搜索标题
   (setq deft-auto-save-interval 0) ;;是否自动保存从deft打开的文件
   (setq deft-current-sort-method 'mtime) ;;排序方式
   (setq deft-default-extension "org")
   ;; (setq deft-strip-summary-regexp ".*")
   ;; (setq deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n")
   (setq deft-strip-summary-regexp
	 (concat "\\("
		 "[\n\t]" ;; blank
		 "\\|^#\\+[[:alpha:]_]+:.*$" ;; org-mode metadata
		 "\\|^:PROPERTIES:\n\\(.+\n\\)+:END:\n"
		 "\\)"))

   (setq deft-time-format nil) ;; set nil to hide


   (set-face-attribute 'deft-summary-face nil :font en-font-name)
   (set-face-attribute 'deft-title-face nil :font en-font-name)
   (set-face-attribute 'deft-time-face nil :font en-font-name)


   ;; (let* ((raw-title " 2022-okr-o1-kr-1 [[a:9acb9008][⚓]] "))
   ;;   (message "%s" (s-replace-regexp "\\[\\[a:[0-9a-z]+\\]\\[⚓\\]\\]" "" raw-title))
   ;;   )

   ;; (let* ((raw-title " 2022-okr-o1-kr-1 [[a:9acb9008][⚓]] "
   ;;                   ;;"[[a:4abb0f57][测试新页面]] "
   ;;                   ))
   ;;   (setq raw-title (s-replace-regexp "\\[\\[a:[0-9a-z]+\\]\\[" "" raw-title))
   ;;   (setq raw-title (s-replace-regexp "\\]\\]" "" raw-title))
   ;;   (setq raw-title (s-replace-regexp "⚓" "" raw-title))

   ;;   (message "%s" (s-trim raw-title))

   ;;   )

   ;; (setq deft-strip-title-regexp (concat deft-strip-title-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:"))
  ;;  (setq deft-strip-title-regexp
;; 	 (concat
;; 	  "\\(?:^%+\\|^#\\+TITLE: *\\|^[#* ]+\\|-\\*-[[:alpha:]]+-\\*-\\|^Title:[	 ]*\\|#+$\\)"
;; 	  ":PROPERTIES:\n\\(.+\n\\)+:END:"
;; 	  ))

;;    (let ((str ":PROPERTIES:
;; #+TITLE: ts-mv04-临时关闭人脸识别
;; ")

;;          )
;;      (setq str (s-replace-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n" "" str))
;;      (message "str:%s" str)
;;      )


   ;; 无法过滤properties
   (defun eye-parse-title-for-deft (title)
     (let ((raw-title title))
       ;; (setq raw-title (s-replace-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n" "" raw-title))
       ;; (message "title: %s, raw-title:%s" title raw-title) ;; title可能是:PROPERTIES:
       (setq raw-title (deft-strip-title raw-title))
       (if (s-contains-p "[[a:" raw-title)
           (progn
             ;; (setq raw-title (s-replace-regexp "\\[\\[a:[0-9a-zA-Z]+\\]\\[⚓\\]\\]" "" raw-title))
             (setq raw-title (s-replace-regexp "\\[\\[a:[0-9a-z]+\\]\\[" "" raw-title))
             (setq raw-title (s-replace-regexp "\\]\\]" "" raw-title))
             (setq raw-title (s-replace-regexp "⚓" "" raw-title))
             )
         )
       (s-trim raw-title)
       ))
   ;; (setq deft-parse-title-function 'eye-parse-title-for-deft)


   ;; 过滤PROPERTIES，@see https://github.com/jrblevin/deft/issues/75
   (advice-add 'deft-parse-title :override
    (lambda (file contents)
      (if deft-use-filename-as-title
	  (deft-base-filename file)
	(let* ((case-fold-search 't)
	       (begin (string-match "title: " contents))
	       (end-of-begin (match-end 0))
	       (end (string-match "\n" contents begin)))
	  (if begin
	      (eye-parse-title-for-deft (substring contents end-of-begin end))
	    (format "%s" (file-name-nondirectory file)))))))


   ))



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


;; 禁用鼠标点击时打开链接
(setq org-mouse-1-follows-link nil) ;; 不起作用
(setq mouse-1-click-follows-link nil) ;; 起作用
(define-key org-mouse-map [mouse-2] #'ignore) ;; 中键
(define-key org-mouse-map [mouse-3] #'org-open-at-mouse) ;; 右键


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
  (let* ((id (org-id-copy))
         (title (org-element-property :title (org-element-at-point)))
         (org-link "")
         )
    ;; (setq org-link (format "[[id:%s][%s]]" id title))
    ;; 由于默认的[[id:xxxx]]在大的journals文件中可能会卡住emacs或者报错，这里使用pos:
    (setq org-link (format "[[pos:%s][%s]]" (s-left 8 id) title))
    (kill-new org-link)
    (save-buffer)
    (message "Copied link: %s" org-link)
    )
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




;;;; notebook switch
(defun eye/switch-notebook ()
  "切换笔记本环境"
  (interactive)
  (let* ((notebook-list (s-split "\n" (get-string-from-file "~/.emacs.d/.notedirlist")))
         (notebook-dir (ivy-read "Select notebook: " notebook-list)))
    (f-write-text notebook-dir 'utf-8 "~/.notedir")
    (setq eye-org-file-attach-base-dir "~/attach_ts")

    (eye-setup-org-dirs)


    (when (fboundp 'org-roam-mode)
      (setq org-roam-directory notebook-dir)
      (setq org-roam-db-location (concat notebook-dir "/org-roam.db")))
    (when (fboundp 'deft)
      (setq deft-directory notebook-dir)
      )
    (when (fboundp 'denote)
      (setq denote-directory notebook-dir)
      )


    (message "Switched notebook %s." notebook-dir)

    ))

(require 'init-agenda)
(require 'init-ox-hugo)

(require 'init-org-roam)

(provide 'init-org)
