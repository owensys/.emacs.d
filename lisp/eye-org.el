;;;; org
(eye-install-packages '(("htmlize" . "https://github.com/hniksic/emacs-htmlize.git")
            ("notdeft" . "https://github.com/hasu/notdeft.git")
            ("ts" . "https://github.com/alphapapa/ts.el.git")
            ("ht" . "https://github.com/emacsmirror/ht.git")
            ("org-super-agenda" . "https://github.com/alphapapa/org-super-agenda.git")))

(require 'org)

(setq locale-notebook-dir (if is-linux "~/orgnotes" "x:/orgnotes"))

(setq auto-require-packages-dir eye-packages-dir)

(add-to-list 'auto-mode-alist '("\\.gtd$" . org-mode))

(setq gtd-inbox-path (concat locale-notebook-dir "/org/00-inbox/inbox.org"))          ;; 收集所有东西
(setq gtd-someday-path (concat locale-notebook-dir "/org/gtd/someday.org"))
(setq gtd-tickler-path (concat locale-notebook-dir "/org/gtd/tickler.org"))
(setq gtd-trash-path (concat locale-notebook-dir "/org/gtd/trash.org"))
(setq gtd-archive-path (concat locale-notebook-dir "/org/gtd/archive-2021.org")) ;; 归档文件


;; 9.3使用<s需要org-tempo
(when (>= emacs-major-version 27)
  (require 'org-tempo))


(setq system-time-locale "C")
;;%a表示插入时间时显示“周几”，如果没有设置system-time-locale为"C"的话，会显示乱码
(setq org-time-stamp-formats '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M>"))
(setq org-enforce-todo-dependencies t)


(progn
  ;; 修改headline的折叠标记
  ;;(setq org-ellipsis " ")
  (setq org-ellipsis " ↩")
  (set-face-underline 'org-ellipsis nil) ;; 去除下划线
  (set-face-underline 'org-link nil))

(setq org-src-fontify-natively t) ;; 代码块内语法高亮
(setq org-src-tab-acts-natively t)
;; (setq org-src-window-setup 'current-window) ;; 在当前window打开src block
;; (add-hook 'org-mode-hook 'yas-minor-mode)
;; indent content
(setq org-edit-src-content-indentation 0) ;; 代码块默认不缩进
(setq org-startup-indented nil) ;; 是否自动开启org-indent-mode
(setq-default org-startup-indented nil)
(setq org-hide-block-startup t)
(setq org-startup-folded t)
;; 默认显示图片出来
(setq org-startup-with-inline-images t)
;; 保留几行空白行
(setq org-cycle-separator-lines 2)
;; always require new line in header below
(setq require-final-newline t)
(setq org-tags-column 0)
(setq org-return-follows-link t) ;; 是否回车打开link
(setq org-startup-truncated nil)
(setq org-clock-string "计时:"
      ;;org-closed-string "已关闭:"
      ;;org-deadline-string "最后期限:"
      ;;org-scheduled-string "计划任务:"
      org-time-stamp-formats  '("<%Y-%m-%d 周%u>" . "<%Y-%m-%d 周%u %H:%M>")
      org-deadline-warning-days 30	;最后期限到达前n天即给出提醒
      org-link-file-path-type  'relative ;插入链接时使用相对路径
      org-log-done 'time		 ;完成时添加时间
      ;; code执行免应答（Eval code without confirm）
      org-confirm-babel-evaluate nil
      )

;; 支持imagemagic时，使用缩略图
;; 在链接前添加信息：#+ATTR_ORG: :width 300
(if (string-match-p "imagemagic" system-configuration-features)
    (setq org-image-actual-width nil))

;; set face for orgmode
(progn
  ;;(set-face-attribute 'org-meta-line nil :foreground "gray30") ;; 文件头信息
  ;;(set-face-attribute 'org-document-info-keyword nil :foreground "gray30")
  (set-face-attribute 'org-drawer nil :foreground "gray30") ;;properties之类的内容
  )

(setq org-support-shift-select 1) ;; 是否支持shift+方向键选择
(setq org-fontify-emphasized-text t) ;; 高亮行内代码标记等 https://orgmode.org/manual/Emphasis-and-Monospace.html#Emphasis-and-Monospace
(setq org-hide-emphasis-markers t) ;; 隐藏斜体标记/text/，如果要删除，则确保光标移到斜体文字最后
;; 模板中的file路径不是绝对路径时，将会使用org-directory进行查找
(setq org-directory locale-notebook-dir)
;; 用圆形符号表示列表开头，匹配" - "
(font-lock-add-keywords 'org-mode
			'(("^ +\\([-*]\\) "
			   (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; 标题字体大小，注意在设置中文字体时set-fontset-font中不要指定size，否则中文会不生效

;;(let ((idx 0))
;;  (dolist (face '(org-level-5 org-level-4 org-level-3 org-level-2 org-level-1))
;;	(set-face-attribute face nil :weight 'semi-bold :height (+ 1.0 (* idx 0.2)))
;;	(setq idx (+ idx 1))))

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
	(auto-mode . emacs)))


;; Speed keys, @see https://orgmode.org/manual/Speed-keys.html
;; quick navigation when cursor is on a headline (before any of the stars)
;; ?:for help, n/p/f/b...
(setq org-use-speed-commands t)

(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAIT(w!)" "SOMEDAY(s)" "|" "DONE(d!)" "CANCELLED(c!)")))

(setf org-todo-keyword-faces
      '(("TODO" . (:foreground "DeepPink" :bold t :weight bold))
	("WAIT" . (:foreground "sienna1" :bold t :weight bold))
	("DONE" . (:foreground "LimeGreen" :bold t :weight bold))
	("ARCHIEVED" . (:foreground "LimeGreen" :bold t :weight bold))
	("CANCELLED" . (:foreground "gray50" :bold t :weight bold))
	))

;; logbook
(setq org-log-into-drawer t)

;; org-archive-subtree moving an tree to archive file
;; settings on org file #+ARCHIVE file head or ARCHIVE PROPERTY
(setq org-archive-location (concat gtd-archive-path "::"))

<<<<<<< HEAD
;;(setq org-refile-targets
;;      `((,gtd-gtd-path :maxlevel . 2)
;;	(,gtd-someday-path :level . 1)    ;; 最多第1层
;;	(,gtd-tickler-path :level . 2)    ;; 只要第2层
;;	(,gtd-trash-path :level . 1)
;;	))

;;(setq org-refile-targets
;;      '((nil :maxlevel . 1)
;;        (org-agenda-files :maxlevel . 2)
;;        (deft-files :maxlevel . 2)))


(setq org-default-notes-file (expand-file-name "todo.today.org" locale-notebook-dir))

(define-key org-mode-map (kbd "M-RET") 'org-insert-heading-respect-content)
(define-key org-mode-map (kbd "C-k") nil)
=======
;;(setq org-default-notes-file (expand-file-name "todo.today.org" locale-notebook-dir))

>>>>>>> bfc14befeb50b4e0f857cafc9376f404dc412610

;; Line wrapping
(add-hook 'org-mode-hook (lambda ()
			   (visual-line-mode 1)
			   (org-show-children 1)
			   ;; 设置行距
			   (setq-local line-spacing 0.45)))

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


;;;; my wiki link type
;;(setq my-wiki-base-url "http://192.168.119.128:8000/")
;;(org-add-link-type
;; "MyWiki"
;; (lambda (text)
;;   (browse-url-default-browser (concat my-wiki-base-url text))
;;   ))

;;;; htmlize
(add-to-list 'load-path (concat auto-require-packages-dir "/emacs-htmlize"))
(require 'htmlize)

;;;; ob-shell
;; support babel execute
(require 'ob-shell)
(org-babel-do-load-languages
 'org-babel-load-languages '((emacs-lisp . t)

			     (shell . t)))

;;;; Code block face
;; @see https://emacs-china.org/t/topic/12520
(defcustom load-theme-before-hook nil
  "Functions to run before load theme."
  :type 'hook)

(defcustom load-theme-after-hook nil
  "Functions to run after load theme."
  :type 'hook)

(defun load-theme-hook-wrapper (origin-func theme &rest args)
  "A wrapper of hooks around `load-theme'."
  (mapc #'disable-theme custom-enabled-themes)
  (run-hook-with-args 'load-theme-before-hook theme)
  (apply origin-func theme args)
  (run-hook-with-args 'load-theme-after-hook theme))

(advice-add 'load-theme :around #'load-theme-hook-wrapper)

(add-hook 'load-theme-after-hook
		  (lambda ()
			;; @see https://emacs-china.org/t/awesome-tab/9607/6
			(set-face-attribute 'org-block-begin-line nil :foreground "gainsboro" :background (face-background 'default))
			(set-face-attribute 'org-block-end-line nil :foreground "gainsboro" :background)))


;;;; plantuml
(setq org-plantuml-jar-path (expand-file-name "~/.emacs.d/bin/plantuml.jar"))
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
(org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))



;;------------------------------------------------------
;;;; org-capture
;;------------------------------------------------------
(require 'org-capture)
;; capture 的目标路径不能直接使用 concat

(defun my-org-journal-find-location ()
  "以当前日期为文件名，第一级也是当前日期，属性为diary"
  (let* ((today (format-time-string "%Y-%m-%d" (current-time)))
	(today-file (concat locale-notebook-dir "/journal/" today ".org")))
    (find-file today-file)
    (goto-char (point-min)) ;; 防止重复创建日期节点
    (if (search-forward today nil t)
	(goto-char (point-max))
      (progn
	(goto-char (point-max))
	(insert (concat "* " today "\n:PROPERTIES:\n:CATEGORY: diary\n:END:\n")))
      )))


;; Inbox
(add-to-list 'org-capture-templates '("i" "Inbox" entry (file+headline gtd-inbox-path "Inbox")
				      "* TODO %i%?"))


;; Tickler
;; %^t 输入提醒时间
(add-to-list 'org-capture-templates '("t" "Tickler" entry (file+headline gtd-tickler-path "Tickler")
				      "* %i%? \n %U"))



;; 日记模板
;; %T  插入时间戳，便于在agenda中显示
;; %^G 插入tags
;; 问题：**无法创建二级标题，需要手动调整一下
;;(add-to-list 'org-capture-templates '("j" "Journal" entry (function my-org-journal-find-location)
;;				      "* %T %^{Title} %^G\n%i%?"))

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



;;------------------------------------------------------
;;;; org-agenda
;;------------------------------------------------------
(require 'org-agenda)

;; 每三个小时为一间隔
(setq org-agenda-time-grid (quote ((daily today require-timed)
				   (300 600 900 1200 1500 1800 2100 2400)
				   "......" "------------------")))

;;(setq org-agenda-files `(,gtd-inbox-path
;;			 ,gtd-gtd-path
;;			 ,gtd-tickler-path
;;			 ))

(when (file-exists-p locale-notebook-dir)
  (setq org-agenda-files (directory-files (concat locale-notebook-dir "/org/01-projects") t "^proj-*"))

  (add-to-list 'org-agenda-files (concat locale-notebook-dir "/org/gtd/goals.org"))
  (add-to-list 'org-agenda-files (concat locale-notebook-dir "/org/gtd/someday.org"))
  (add-to-list 'org-agenda-files (concat locale-notebook-dir "/org/gtd/tickler.org"))
  (add-to-list 'org-agenda-files (concat locale-notebook-dir "/org/00-inbox/inbox.org"))
  (dolist (okrfile (directory-files (concat locale-notebook-dir "/org/gtd") t "^okr*"))
    (add-to-list 'org-agenda-files okrfile)))


;; C-c C-w: org-refile 从inbox移到其它文件，不需要再移回inbox文件
(setq org-refile-targets
      '((org-agenda-files :maxlevel . 2)))


;;(add-to-list 'org-agenda-files (concat locale-notebook-dir "/journal/"))
;; full frame show
(setq org-agenda-window-setup 'only-window)
;; (setq org-agenda-block-separator nil)
(setq org-agenda-align-tags-to-column 1) ;在agenda视图中，使tags向左边对齐，默认是auto，向右边对齐，会换行显示
(setq
 ;;org-agenda-deadline-leaders (quote ("最后期限:  " "%3d 天后到期: " "%2d 天前: "))
 ;;org-agenda-scheduled-leaders (quote ("计划任务:" "计划任务(第%2d次激活): "))
 org-agenda-inhibit-startup t
 org-agenda-span 'day
 ;; 隐藏agenda中的tag显示
 ;;org-agenda-hide-tags-regexp "."
 )

;;(setq-default org-columns-default-format "%80ITEM %50TODO %3PRIORITY %TAGS")
;;(setq org-agenda-overriding-columns-format "%TODO %7EFFORT %PRIORITY %100ITEM 100%TAGS")

;; 自定义日期显示格式
(setq-default org-agenda-format-date (quote my-org-agenda-format-date-aligned))
(defun my-org-agenda-format-date-aligned (date)
  "Format a DATE string for display in the daily/weekly agenda.
This function makes sure that dates are aligned for easy reading."
  (require 'cal-iso)
  (let* ((dayname (calendar-day-name date))
	 (day (cadr date))
	 (day-of-week (calendar-day-of-week date))
	 (month (car date))
	 (monthname (calendar-month-name month))
	 (year (nth 2 date))
	 (iso-week (org-days-to-iso-week
		    (calendar-absolute-from-gregorian date)))
	 (weekyear (cond ((and (= month 1) (>= iso-week 52))
			  (1- year))
			 ((and (= month 12) (<= iso-week 1))
			  (1+ year))
			 (t year)))
	 (weekstring (if (= day-of-week 1)
			 (format " W%02d" iso-week)
		       "")))
    ;; 修改点：在agenda中显示的日期格式
    (format "%4d-%s-%02d %-4s %s"
	    year monthname day dayname weekstring)))

;;;; gtd workflow
(add-to-list 'org-agenda-custom-commands '("g" . "GTD Workflow"))

;; 使用tags而不是tags-todo，将显示所有inbox标签下的items
(add-to-list 'org-agenda-custom-commands '("gi" "View Inbox" tags "inbox"
					   ((org-agenda-overriding-header "Inbox"))))

;; priority-down把优先级高的显示在前面
(add-to-list 'org-agenda-custom-commands '("gt" "Todo list" todo "TODO"
					   ((org-agenda-overriding-header "Today")
					    (org-agenda-sorting-strategy '(priority-down)))))

(add-to-list 'org-agenda-custom-commands '("gn" "Next Actions" todo "NEXT"
					   ((org-agenda-overriding-header "Next Actions"))))

(add-to-list 'org-agenda-custom-commands '("gs" "Someday/Maybe " todo "SOMEDAY"
					   ((org-agenda-overriding-header "Someday/Maybe"))))

(add-to-list 'org-agenda-custom-commands '("gw" "Waiting " todo "WAIT"
					   ((org-agenda-overriding-header "Waiting"))))

;;;; other search command
;; tags-todo显示同时满足设置了todo和tag的items
(add-to-list 'org-agenda-custom-commands '("t" "View personal todolist" tags-todo "task|repeat|body"
					   ((org-agenda-overriding-header "Task"))))


;; 查看project
(add-to-list 'org-agenda-custom-commands '("p" . "View project todolist"))

(add-to-list 'org-agenda-custom-commands '("ps" "ts" tags-todo "ts"
					   ((org-agenda-overriding-header "TS") ;;用于显示的字符串
					    (org-agenda-sorting-strategy '(priority-down))
					    ;;(org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first) ;; 只显示一个
					   )))

(add-to-list 'org-agenda-custom-commands '("pm" "Memory" tags-todo "proj+memory" ;; 搜索同时满足多个tag
					   ((org-agenda-overriding-header "Memory")))) ;;用于显示的字符串

;; 只显示一个任务
(defun my-org-agenda-skip-all-siblings-but-first ()
  "Skip all but the first non-done entry."
  (let (should-skip-entry)
    (unless (org-current-is-todo)
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (org-current-is-todo)
          (setq should-skip-entry t))))
    (when should-skip-entry
      (or (outline-next-heading)
          (goto-char (point-max))))))

(defun org-current-is-todo ()
  (string= "TODO" (org-get-todo-state)))


;; 自动打开calendar
(advice-add 'org-agenda :after
	    (lambda (_)
	      (when (equal (buffer-name)
			   "*Org Agenda*")
		(calendar)
		(other-window 1))))

;; 自动退出calendar
(advice-add 'org-agenda-quit :before
		    (lambda ()
		      (let ((window (get-buffer-window calendar-buffer)))
			(when (and window (not (one-window-p window)))
			                    (delete-window window)))))



;; 交互式选择插入代码块 @See http://wenshanren.org/?p=327
(defun eye/org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
	  '("C++" "emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "css"
	    "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
	    "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
	    "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
	    "scheme" "sqlite" "example")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    ;;(newline-and-indent) ; no auto indent space
    (insert (format "#+begin_src %s\n" src-code-type)) ; use lower string
    ;;(newline-and-indent)
    (insert "#+end_src\n")
    (previous-line 2)
    (org-edit-src-code)))



;;;; notdeft-org
;;(add-to-list 'load-path (concat auto-require-packages-dir "/notdeft"))
;;(require 'notdeft-org)

(require 'init-org-note-attach)

(add-to-list 'load-path (concat auto-require-packages-dir "/ts"))
(add-to-list 'load-path (concat auto-require-packages-dir "/ht"))
(add-to-list 'load-path (concat auto-require-packages-dir "/org-super-agenda"))
(require 'org-super-agenda)
(org-super-agenda-mode)

(defun test-super ()
  (interactive)
  (let ((org-agenda-span 'day)
	(org-super-agenda-groups
	 '((:name "Time grid items"
                  :time-grid t
                  :transformer (--> it
                                    (upcase it)
                                    (propertize it 'face '(:foreground "RosyBrown1"))))
           (:name "Priority >= C items underlined, on black background"
                  :face (:background "black" :underline t)
                  :not (:priority>= "C")
                  :order 100))))
    (org-agenda nil "a"))
  )


;; 根据自定义属性分类显示
;; 分类显示时，不能使用带冒号的时间
(defun eye/agenda-by-area ()
  (interactive)
  (setq org-super-agenda-groups
	 '((:auto-property "Area")))
  (org-agenda-list))

;; copy from library, change to show Project:
(org-super-agenda--def-auto-group category "their org-category property"
  :key-form (org-super-agenda--when-with-marker-buffer (org-super-agenda--get-marker item)
              (org-get-category))
  :header-form (concat "Project: " key))

(defun eye/agenda-by-proj ()
  "默认分类就是文件名，每个文件属于一个项目"
  (interactive)
  (setq org-agenda-span 'day)
  (setq org-super-agenda-groups
	'((:auto-category t)))

    (org-agenda-list))


(defun eye/agenda-by-goals ()
  (interactive)
  (setq org-agenda-span 'day)
  (setq org-super-agenda-groups
	'((:auto-property "Goals")
	  ;; don't show other items
	  (:discard (:anything t))))

    (org-agenda-list))


(defun eye/agenda-by-okr ()
  (interactive)
  (setq org-agenda-span 'day)
  (setq org-super-agenda-groups
	'((:auto-property "OKR")
	  ;; don't show other items
	  (:discard (:anything t))))
    (org-agenda-list))


(defun eye/agenda-by-time ()
  (interactive)
  (setq org-agenda-span 'day)
  (let ((org-super-agenda-groups
	 '((:log t)  ; Automatically named "Log"
           (:name "Schedule"
                  :time-grid t)
           (:name "Today"
                  :scheduled today)
           (:habit t)
           (:name "Due today"
                  :deadline today)
           (:name "Overdue"
                  :deadline past)
           (:name "Due soon"
                  :deadline future)
           (:name "Unimportant"
                  :todo ("SOMEDAY" "MAYBE" "CHECK" "TO-READ" "TO-WATCH")
                  :order 1)
           (:name "Waiting..."
                  :todo "WAIT"
                  :order 2)
           (:name "Scheduled earlier"
                  :scheduled past))))
    (org-agenda-list)))

(defun eye/agenda-show-work-items ()
  (interactive)
  (setq org-agenda-span 'day)
  (setq org-super-agenda-groups
	`((:name "Work-related"
                  :tag "@company")))
    (org-tags-view t "@company"))


(defun eye/agenda-show-home-items ()
  (interactive)
  (setq org-agenda-span 'day)
  (setq org-super-agenda-groups
	`((:name "Personal-related"

                 :tag "@home")))
    (org-tags-view t "@home"))


;;;; review once a week
(defun eye/agenda-done-week ()
  (interactive)
  (setq org-agenda-span 'week)
  (setq org-super-agenda-groups
	'((:name "Done task by week"
		 :todo "DONE"
		 :discard (:anything t))))
  (org-agenda-list))




;; @See https://github.com/alphapapa/org-super-agenda/blob/master/examples.org
(add-to-list 'org-agenda-custom-commands
	     '("v" "Super view"
	       ((agenda "" ((org-agenda-span 'day)
			    (org-super-agenda-groups
			     '((:name "Today Tasks"
				      :time-grid t
				      :date today
				      :todo "TODAY"
				      :scheduled today
				      :order 1)))))
		(alltodo "" ((org-agenda-overriding-header "")
			     (org-super-agenda-groups
			      '((:name "All Todo items" :todo "TODO" )
				(:name "All Next to do" :todo "NEXT")
				(:name "All Important"  :tag "Important" :priority "A")
				(:name "All Due Today"  :deadline today)
				(:name "All Due Soon"   :deadline future)
				(:name "All Overdue"    :deadline past)
				(:name "All Wait"       :todo "WAIT")
				(:name "All Someday"    :todo "SOMEDAY")
				(:name "All Goals"      :todo "GOAL")
				)))))))




;;;; org-journal

(defun eye-get-current-week ()
  (let ((week (format-time-string "%A")))
    (cond
     ((string-equal "Monday"    week) "星期一")
     ((string-equal "Tuesday"   week) "星期二")
     ((string-equal "Wednesday" week) "星期三")
     ((string-equal "Thursday"  week) "星期四")
     ((string-equal "Friday"    week) "星期五")
     ((string-equal "Saturday"  week) "星期六")
     ((string-equal "Sunday"    week) "星期日")
     (t nil)
     )))

(defun org-journal-save-entry-and-exit()
  "Simple convenience function.
  Saves the buffer of the current day's entry and kills the window
  Similar to org-capture like behavior"
  (interactive)
  (save-buffer)
  (kill-buffer-and-window))


;;;; org-journal
(auto-require
 'org-journal
 :load nil
 :urls '(("org-journal" . "https://github.com/bastibe/org-journal.git"))
 :paths "org-journal"
 :after
 (progn
   (setq org-journal-dir (concat locale-notebook-dir "/org/03-resources/journal"))
   ;; 打开时是否分割窗口, or use find-file, if used org-journal-save-entry-and-exit, do not set to find-file
   ;;(setq org-journal-find-file 'find-file)
   (setq org-journal-file-format (concat "%Y-%m-%d.org"))
   (setq org-journal-date-format "%Y-%m-%d")
   (define-key org-journal-mode-map (kbd "C-x C-s") 'org-journal-save-entry-and-exit)
   ))

;;;; org-cliplink
(setq org-note-bookmark-path (concat locale-notebook-dir "/org/03-resources/bookmarks.org"))
(auto-require
 'org-cliplink
 :load t
 :urls '(("org-cliplink" . "https://github.com/rexim/org-cliplink.git"))
 :paths "org-cliplink"
 :functions '(org-cliplink-capture)
 :after
 (progn
   (global-set-key (kbd "C-x p i") 'org-cliplink)
   (add-to-list 'org-capture-templates
		'("k" "Cliplink capture task" entry
		  (file org-note-bookmark-path)
		  "* %(org-cliplink-capture) \n:PROPERTIES:\n:CREATED: %U\n:END:\n" :empty-lines 1))

   (add-to-list 'org-capture-templates '("u" "Inbox url" entry (file+headline gtd-inbox-path "Inbox")
				      "* %(org-cliplink-capture) \n %i%?"))


   ))


;;;; org-drill
;; require org-learn
;;(auto-require
;; 'org-drill
;; :load t
;; :urls '(("org-drill" . "https://github.com/hakanserce/org-drill.git"))
;; :paths "org-drill"
;; :after
;; (progn
;;   (setq org-drill-save-buffers-after-drill-sessions-p nil)
;;   ))

;;;; org-fancy-priorities
(eye-install-packages '(("org-fancy-priorities" . "https://github.com/harrybournis/org-fancy-priorities.git")))
(require 'org-fancy-priorities)
(with-eval-after-load 'org-fancy-priorities
   (add-hook 'org-mode-hook 'org-fancy-priorities-mode)
   (setq org-priority-lowest ?D)
   ;; need install symbola font
   (setq org-fancy-priorities-list '("🗲" "⬆" "⬇" "🍺"))
   )


;;;; org-bullets
(eye-install-packages '(("org-bullets" . "https://github.com/sabof/org-bullets.git")))
(require 'org-bullets)
(progn
   (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
   )



;;;; pretty symbols mode
(add-hook
 'org-mode-hook
 (lambda ()
   "Beautify org symbols."
   (add-to-list 'prettify-symbols-alist '("[ ]" . ?☐))
   (add-to-list 'prettify-symbols-alist '("[X]" . ?☑))
   (add-to-list 'prettify-symbols-alist '("[-]" . ?❎))

   (add-to-list 'prettify-symbols-alist '("#+ARCHIVE:" . ?📦))
   (add-to-list 'prettify-symbols-alist '("#+AUTHOR:" . ?👤))
   (add-to-list 'prettify-symbols-alist '("#+CREATOR:" . ?💁))
   (add-to-list 'prettify-symbols-alist '("#+DATE:" . ?📆))
   (add-to-list 'prettify-symbols-alist '("#+DESCRIPTION:" . ?🗎))
   (add-to-list 'prettify-symbols-alist '("#+EMAIL:" . ?🖂))
   (add-to-list 'prettify-symbols-alist '("#+OPTIONS:" . ?⚙))
   (add-to-list 'prettify-symbols-alist '("#+TAGS:" . ?⌦))
   (add-to-list 'prettify-symbols-alist '("#+TITLE:" . ?🕮))

   (add-to-list 'prettify-symbols-alist '("#+BEGIN_SRC" . ?✎))
   (add-to-list 'prettify-symbols-alist '("#+END_SRC" . ?□))
   (add-to-list 'prettify-symbols-alist '("#+BEGIN_QUOTE" . ?»))
   (add-to-list 'prettify-symbols-alist '("#+END_QUOTE" . ?«))
   (add-to-list 'prettify-symbols-alist '("#+HEADERS" . ?☰))
   (add-to-list 'prettify-symbols-alist '("#+RESULTS:" . ?💻))
   (add-to-list 'prettify-symbols-alist '("#+FILETAGS:" . ?⌦))
   (prettify-symbols-mode 1)
   ))



(provide 'eye-org)
