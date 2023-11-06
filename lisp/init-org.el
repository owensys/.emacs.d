;;;; notebook hooks
(defcustom notebook-before-open-hook nil
 "Functions to run before load notebook."
 :type 'hook)

(defcustom notebook-after-open-hook nil
 "Functions to run after load notebook."
 :type 'hook)


(defun eye/open-notebook ()
  (interactive)
  (require 's)
  (require 'f)
  (let* ((note-dir (counsel-read-directory-name "Select note directory: ")))
    (run-hook-with-args 'notebook-before-open-hook note-dir)
    (find-file (expand-file-name "index.org" note-dir))
    (run-hook-with-args 'notebook-after-open-hook note-dir)
    (message "opened notebook:%s" note-dir)
    ))
;;;

;;;; org
(setq org-modules-loaded t) ;; 加速orgmode 加载，https://emacs-china.org/t/org-babel/18699/12

;; speed up org load https://emacs-china.org/t/org-babel/18699/12
(with-eval-after-load 'org
  (setq org-modules (cl-set-difference org-modules '(ol-gnus
                                                     ol-eww
                                                     ol-irc
                                                     ol-rmail
                                                     ol-w3m
                                                     ol-bibtex
                                                     ))))

(require 'org)

;; 9.3使用<s需要org-tempo
(when (>= emacs-major-version 27)
  (require 'org-tempo))

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
(setq org-cycle-separator-lines 1)
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



;; 支持imagemagic时，使用缩略图
;; 在链接前添加信息：#+ATTR_ORG: :width 300
;; (when (string-match-p "imagemagic" system-configuration-features)
;; (setq org-image-actual-width nil)
;; )
;; 在环境变量中有imagemagic，不需要判断system-configuration-features
;; (require 'ox)
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
 ;; calendar-day-abbrev-array ["周日" "周一" "周二" "周三" "周四" "周五" "周六"]
 calendar-day-name-array ["周日" "周一" "周二" "周三" "周四" "周五" "周六"]
 ;; calendar-day-name-array ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"]
 ;;calendar-month-name-array ["一月" "二月" "三月" "四月" "五月" "六月" "七月" "八月" "九月" "十月" "十一月" "十二月"]
 calendar-month-name-array ["01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12"]
 calendar-week-start-day 1 ;; 日历从周一开始显示
 )

;;;; ob-shell
;; support babel execute
;; (require 'ob-shell)
;; (org-babel-do-load-languages
;;  'org-babel-load-languages '((emacs-lisp . t)
;; 			     (shell . t)))

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
;; (require 'init-org-anchor-link)


(add-hook 'org-mode-hook 'eye--org-mode-hook-setup)


;; 禁用鼠标点击时打开链接
(setq org-mouse-1-follows-link nil) ;; 不起作用
(setq mouse-1-click-follows-link nil) ;; 起作用
(define-key org-mouse-map [mouse-2] #'ignore) ;; 中键
(define-key org-mouse-map [mouse-3] #'org-open-at-mouse) ;; 右键

;;;; diary
(require 'calendar)
(require 'diary-lib)


(setq org-clock-string "计时:")
;;org-closed-string "已关闭:"
;;org-deadline-string "最后期限:"
;;org-scheduled-string "计划任务:"
;;%a表示插入时间时显示“周几”，如果没有设置system-time-locale为"C"的话，会显示乱码
(setq system-time-locale "C")
;; (setq org-time-stamp-formats '("<%Y-%m-%d %A>" . "<%Y-%m-%d %A %H:%M>"))
(setq org-time-stamp-formats  '("<%Y-%m-%d 周%u>" . "<%Y-%m-%d 周%u %H:%M>"))
;; org-time-stamp-custom-formats
(setq org-deadline-warning-days 30)	;最后期限到达前n天即给出提醒
(setq org-link-file-path-type  'relative) ;插入链接时使用相对路径
(setq org-log-done 'time)		 ;完成时添加时间
(setq org-extend-today-until 4) ;;以4点当作一天的开始，用于agenda
(setq org-enforce-todo-dependencies t) ;; Not allow to set a headline to DONE if children aren’t DONE.
(setq-default org-enforce-todo-dependencies t)

;;;; drop
(setq dnd-protocol-alist
	    `(("^file:" . my-dnd-insert-link-v2)
	      ("^\\(https?\\|ftp\\|file\\|nfs\\)://" . my-dnd-insert-link-v2)))



(setq eye-match-drop-file-exp
      (concat "\\(jpg\\|png\\|gif\\|"
	      "zip\\|xz\\|gz\\|7z\\|"
	      "jpeg\\|txt\\|doc\\|docx\\|xlsx\\|"
	      "apk\\|rar\\|md\\|json\\|"
	      "html\\|bak\\|db\\|"
	      "pptx\\|pdf\\)$"))

(setq eye-match-image-file-exp
      (concat "\\(jpg\\|jpeg\\|png\\|gif\\|svg\\|bmp\\)$"))

;;(setq is-support-imagemagic (string-match-p "imagemagic" system-configuration-features))
(setq is-support-imagemagic (image-type-available-p 'imagemagick))


;;;; org-file-apps
;; 使用系统程序打开mindmap文件
(add-to-list 'org-file-apps '("\\.mm\\'" . system))
(add-to-list 'org-file-apps '("\\.drawio\\'" . system))
(add-to-list 'org-file-apps '("\\.txt\\'" . system))


;;;; latex
(require 'init-latex)

;;;; utils
(require 'init-org-utils)


;;;; capture
(defun eye-setup-capture-template (note-dir)
  (require 'org-capture)
  ;; capture 的目标路径不能直接使用 concat
  (setq eye-org-inbox-path (concat note-dir "/bookmarks.org"))
  (setq eye-org-bookmarks-path (concat note-dir "/bookmarks.org"))
  (setq eye-org-contacts-path (concat note-dir "/contacts.org"))
  (setq eye-org-books-path (concat note-dir "/books.org"))
  (setq org-capture-templates
        '(("i" "Inbox" entry (file+headline eye-org-inbox-path "Inbox")
	   "* TODO %i%?")
          ("b" "Bookmark" entry (file+headline eye-org-bookmarks-path "Bookmarks")
           "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
          ("c" "Contacts" entry (file eye-org-contacts-path)
               "* %^{姓名} %^{手机号}p %^{邮箱}p %^{住址}p\n\n  %?" :empty-lines 1 :kill-buffer 1)
          ("o" "Books" entry (file eye-org-books-path)
           "* %^{书名} %^{作者}p \n  %?" :empty-lines 1)
          ))
  )

(defun eye-setup-org-dirs (note-dir)
  (require 's)
  (setq org-directory note-dir)
  (setq org-default-notes-file (concat note-dir "/inbox.org"))
  (setq diary-file (concat note-dir "/diary"))

  (setq org-agenda-files (directory-files (concat note-dir "/todolist") t "org$"))
  
  ;; (setq eye-org-file-attach-base-dir "~/attach")
  (setq eye-org-file-attach-base-dir (s-trim (get-string-from-file (concat note-dir "/attach_dir"))))

  (setq eye-bookmarks-path (concat note-dir "/bookmarks.org"))
  (setq eye-org-contacts-file (concat note-dir "/contacts.org"))

  ;; org-archive-subtree moving an tree to archive file
  ;; settings on org file #+ARCHIVE file head or ARCHIVE PROPERTY
  ;; (setq org-archive-location (concat gtd-archive-path "::"))
  ;; %s表示当前文件名
  ;; (setq org-archive-location "%s_archive::datetree/* Archived Tasks")
  ;; 按时间归档
  (setq org-archive-location (concat
                              note-dir
                              "/archive/" (format-time-string "%Y")
                              "_archive.org::datetree/* Archived Tasks")) ;; 归档文件


  (when (fboundp 'org-roam-mode)
    (setq org-roam-directory note-dir)
    (setq org-roam-db-location (concat note-dir "/org-roam.db")))
  (when (fboundp 'deft)
    (setq deft-directory note-dir)
    )
  (when (fboundp 'denote)
    (setq denote-directory note-dir)
    )
  
  )


(add-hook 'notebook-after-open-hook #'eye-setup-org-dirs)
(add-hook 'notebook-after-open-hook #'eye-setup-capture-template)

(require 'init-org-roam)

;;;; org-bullets
;; (eye-install-packages '(("org-bullets" . "https://github.com/sabof/org-bullets.git")))
(eye/use-package
 'org-bullets
 :load-path "org-bullets"
 :command '(org-bullets-mode)
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



;;;; todo keywords
;;#+SEQ_TODO: REPEAT(r) NEXT(n) TODO(t) WAIT(w@/!) DELEGATED(g@/!) PROJ(p) SOMEDAY(s) | DONE(d!) CANCELLED(c@/!)
;;#+SEQ_TODO: GOAL(G) | ACHIEVED(a@) MISSED(m@)
(setq org-todo-keywords
      '(
        (sequence "REPEAT(r)" "TODO(t)" "NEXT(n)" "SOMEDAY(m)" "WAIT(w@/!)" "DELEGATED(g@/!)" "|" "DONE(d!)" "CANCELLED(c@/!)" "STUCK(s)")
        (sequence "GOAL(G) " "|" " ACHIEVED(a@)" "MISSED(m@)")
        ))
;; maybe add org-superstar package
;;

;; https://hugocisneros.com/org-config
(defun my/buffer-face-mode-variable ()
  "Set font to a variable width (proportional) fonts in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "Roboto Slab"
                                :height 150
                                :width normal))
  (buffer-face-mode))
(defun my/style-org-agenda()
  (my/buffer-face-mode-variable)
  (set-face-attribute 'org-agenda-date nil :height 1.1)
  (set-face-attribute 'org-agenda-date-today nil :height 1.1 :slant 'italic)
  (set-face-attribute 'org-agenda-date-weekend nil :height 1.1))

;; (add-hook 'org-agenda-mode-hook 'my/style-org-agenda)

(setq org-agenda-breadcrumbs-separator " ❱ "
      org-agenda-current-time-string "⏰ ┈┈┈┈┈┈┈┈┈┈┈ now"
      org-agenda-time-grid '((weekly today require-timed)
                             (800 1000 1200 1400 1600 1800 2000)
                             "---" "┈┈┈┈┈┈┈┈┈┈┈┈┈")
      org-agenda-prefix-format '((agenda . "%i %-12:c%?-12t%b% s")
                                 (todo . " %i %-12:c")
                                 (tags . " %i %-12:c")
                                 (search . " %i %-12:c"))
      )

;; C-c C-w: org-refile 从inbox移到其它文件，不需要再移回inbox文件
(setq org-refile-targets
      '((org-agenda-files :maxlevel . 2)))


;;;; org-super-agenda
(eye/use-package
 'org-super-agenda
 :load-path '("ts" "ht" "org-super-agenda")
 :ensure t
 :config
 (progn
   ;; 必须启用，否则group
   (org-super-agenda-mode t)
   
   (setq org-agenda-custom-commands
	     '(("v" "Super view"
	        ((agenda "" ((org-agenda-span 'day)
			             (org-super-agenda-groups
			              '((:name "Today"
				                   :time-grid t
				                   :date today
				                   :todo "TODAY"
				                   :scheduled today
				                   :order 1)))))
             
	         (alltodo "" ((org-agenda-overriding-header "")
			              (org-super-agenda-groups
			               '(
			                 (:name "All Next to do" :todo "NEXT")
			                 (:name "All Due Soon"   :deadline future)
			                 (:name "All Delegated"  :todo "DELEGATED")
                             (:name "All Wait"       :todo "WAIT"
                                    :face (:foreground "gray60"))
			                 (:name "All Someday"    :todo "SOMEDAY"
                                    :face (:foreground "gray60"))
			                 ))))))))
   
   ))



;;;; plantuml
;; must set java.exe path
(eye/use-package 'plantuml-mode
                 :load-path "plantuml-mode"
                 :command '(plantuml-mode)
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

;;;; htmlize
(eye/use-package 'htmlize
:ensure t
:load-path '("htmlize" "emacs-htmlize"))


;;;; iscroll
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


(provide 'init-org)


