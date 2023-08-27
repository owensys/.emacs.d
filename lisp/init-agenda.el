;;------------------------------------------------------
;;;; org-agenda
;;------------------------------------------------------
(require 'org-agenda)

;;;; diary
(require 'calendar)
(require 'diary-lib)


;; 每三个小时为一间隔
(setq org-agenda-time-grid (quote ((daily today require-timed)
				   (300 600 900 1200 1500 1800 2100 2400)
				   "......" "------------------")))


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
(setq org-enforce-todo-dependencies t)

;; full frame show
;; (setq org-agenda-window-setup 'only-window)
(setq org-agenda-window-setup 'other-frame)

;; 分割线
;; (setq org-agenda-block-separator ?-)
(setq org-agenda-block-separator "")

;; (setq org-agenda-align-tags-to-column 'auto)
(setq org-agenda-align-tags-to-column 1) ;在agenda视图中，使tags向左边对齐，默认是auto，向右边对齐，会换行显示
(setq
 ;;org-agenda-scheduled-leaders (quote ("计划任务:" "计划任务(第%2d次激活): "))
 org-agenda-inhibit-startup t
 org-agenda-span 'day
 ;; 隐藏agenda中的tag显示
 org-agenda-hide-tags-regexp "."
 )

(setq org-agenda-breadcrumbs-separator " ❱ ")


;; 把分割块的线---变成空格填充
(setq org-agenda-block-separator (string-to-char " "))

;; 定制当前时间线
(setq org-agenda-current-time-string "ᐊ┈┈┈┈┈┈┈ Now")


;; deadline显示前缀格式
(setq org-agenda-deadline-leaders
      '(
       "今天到期: " ;; 显示在当天时
       "%3d天后到期: " ;; 预警显示
       "已过期%2d天: " ;; 已过时间
       ))

;; 如果同时设置了scheduled和deadline，会重复显示，设置此项后，则不再重复显示
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)




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

(set-face-attribute 'org-agenda-structure nil :height 1.0 :weight 'bold)
(set-face-attribute 'org-agenda-date-today nil :height 1.4 :weight 'bold)


;;; 参考此blog设置的任务管理方案 https://www.cnblogs.com/quantumman/p/10808374.html
;; 任务组，设置TG标签，默认情况下，某一层级条目的标签会被其所包含的所有子条目继承。
;; 这样一来，当想要通过搜索标签TG来查看所有任务组时，具体的细节任务也会一并列出，干扰视线。
;; 为此，需要设置变量org-tags-exclude-from-inheritance，对TG标签禁用继承。
(setq org-tags-exclude-from-inheritance '("TG"))

;; TODO:待做
;; DOIN:正在做
;; NEXT:下一步待做
;; DELAY:延迟
;; WAIT:等待
;; DELETATE:委托给其它人
;; SOMEDAY:以后做
(setq org-todo-keywords
      '(
        (sequence "REPEAT(r)" "TODO(t)" "DOIN(s)" "NEXT(n)" "DELAY(y)" "WAIT(w)" "DELEGATE(g)" "SOMEDAY(s)" "|" "DONE(d)" "CANCELLE(c)")
        ))


;; 可以用图标，但在设置super agenda 的 :todo 时也要用完整的keyword
;; (setq org-todo-keywords
;;       '(
;;         (sequence "☞ TODO(t)" "⚔ DOIN(s)" "☟ NEXT(n)" "☕ DELAY(y)" "⚑ WAIT(w)" "☕ DELEGATE(g)" "SOMEDAY(s)" "|" "✔ DONE(d)" "✘ CANCELLE(c)")
;;         ))




(defun eye-setup-org-dirs()
  (let ((note-dir (get-locale-book-dir)))

    (require 's)
    (setq org-agenda-files '())
    ;; 只设置org文件，不要添加带~结尾的文件
    (dolist (file (directory-files (concat note-dir "/todolist") t ".*org$"))
      (when (string-equal (s-right 3 file) "org")
        (add-to-list 'org-agenda-files file)))

    ;; C-c C-w: org-refile
    (setq org-refile-targets
          '((org-agenda-files :maxlevel . 1)))



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

    (setq org-directory note-dir)
    (setq org-default-notes-file (concat note-dir "/inbox.org"))

    ;;;; diary config
    (progn
      (setq diary-file (concat note-dir "/diary"))
      )


    ))

(if (f-directory-p (concat (get-locale-book-dir) "/todolist"))
    (eye-setup-org-dirs))

(defun eye-agenda-goto ()
  "在agenda中，按tab后，自动narrow 一下，避免显示太多无关的todo项"
  (interactive)
  (when (equal major-mode 'org-agenda-mode)
    (org-agenda-goto)
    ;; (org-narrow-to-subtree)
    ))
(define-key org-agenda-mode-map (kbd "<tab>") 'eye-agenda-goto)



(eye/use-package
 'org-super-agenda
 :ensure t
 :load-path '("ht" "ts" "org-super-agenda")
 :config
 (progn

   (org-super-agenda-mode)

   ;;    org-agenda-skip-scheduled-if-done t
   ;;    org-agenda-skip-deadline-if-done t
   ;;    org-agenda-include-deadlines t


   (setq org-agenda-include-diary t) ;; include calendar diary
   (setq org-agenda-compact-blocks t)
   (setq org-agenda-start-with-log-mode t) ;; 可以使org-super-agenda 的today区域也显示时间段

   (defun eye-quit-agenda  ()
     (interactive)
     (if (eq org-agenda-window-setup 'other-frame)
         (org-agenda-quit)
       kill-this-buffer))

   (define-key org-agenda-mode-map (kbd "q") #'eye-quit-agenda)

   ;;(add-hook 'after-init-hook
   ;;          (lambda ()
   ;;            (org-agenda nil "d")))


   (add-to-list 'org-agenda-custom-commands
                '("d" "All todo group by date"
                  (
                   (agenda "" ((org-agenda-span 'day)
                               (org-super-agenda-groups
                                '((:name "Today"
                                         :time-grid t
                                         :date today
                                         :todo "TODAY"
                                         :scheduled today
                                         :order 1)))))
                   (alltodo "" ((org-agenda-overriding-header "")
                                (org-super-agenda-groups
                                 '((:name "Next to do"
                                          :todo "NEXT"
                                          :order 1) ;; order显示顺序
                                   (:name "今天到期" ;; 今天到期
                                          :deadline today
                                          :order 2)
                                   (:name "重要的"
                                          :tag "Important" ;; 设置了Important tag的可以显示
                                          :priority "A" ;; 设置了A级的可以显示
                                          :order 6)
                                   (:name "已过期" ;; 已过期
                                          :deadline past
                                          :order 7)
                                   (:name "Waiting"
                                          :todo "WAIT"
                                          :order 10)
                                   (:name "To read"
                                          :tag "read"
                                          :order 15)
                                   (:name "分配给其它人的任务" ;; 分配给其它人的任务
                                          :todo "DELEGATE"
                                          :order 20)
                                   (:name "不重要的"
                                          :priority<= "C"
                                          :tag ("Trivial" "Unimportant")
                                          :todo ("SOMEDAY" )
                                          :order 90)
                                   (:discard (:tag "TG"))
                                        ;(:discard (:anything t)) ;; 不显示没有设置时间的和未来的

                                   )))))))

   (add-to-list 'org-agenda-custom-commands
                '("v" "All todo by group by state"
                  (
                   (alltodo "" ((org-super-agenda-groups
                                 '(
                                   (:name "todo"
                                          :and (:todo "TODO" :not (:tag "TG")))
                                   (:name "doing"
                                          :and (:todo "DOIN" :not (:tag "TG")))
                                   (:name "next to do"
                                          :and (:todo "NEXT" :not (:tag "TG")))
                                   (:name "delayed"
                                          :and (:todo "DELAY" :not (:tag "TG")))
                                   (:name "waiting"
                                          :and (:todo "WAIT" :not (:tag "TG")))
                                   (:name "delegated"
                                          :and (:todo "DELEGATE" :not (:tag "TG")))
                                   (:name "habit"
                                          :and (:todo "REPEAT" :not (:tag "TG")))
                                   (:name "someday"
                                          :and (:todo "SOMEDAY" :not (:tag "TG")))
                                   (:discard (:tag "TG"))
                                   )
                                 ))))))

   ;; 不显示TG标签的组任务
   ;; spliter unicode: U+2B9E, U+2B9C
   (add-to-list 'org-agenda-custom-commands
                '("w" "All todo group by priority"
                  (
                   (alltodo "" ((org-super-agenda-groups
                                 '(
                                   (:name "⮞⮞⮞⮞⮞⮞⮞⮞⮞⮞⮞⮞⮞⮞⮞⮞ High priority projects ⮜⮜⮜⮜⮜⮜⮜⮜⮜⮜⮜⮜⮜⮜⮜"
                                          :and (:priority "A" :not (:tag "TG")))
                                   (:name "⮞⮞⮞⮞⮞⮞⮞⮞⮞⮞⮞⮞⮞⮞⮞⮞ Middle priority projects ⮜⮜⮜⮜⮜⮜⮜⮜⮜⮜⮜⮜⮜"
                                          :and (:priority "B" :not (:tag "TG")))
                                   (:name "⮞⮞⮞⮞⮞⮞⮞⮞⮞⮞⮞⮞⮞⮞⮞⮞ Low priority projects ⮜⮜⮜⮜⮜⮜⮜⮜⮜⮜⮜⮜⮜⮜⮜⮜"
                                          :and (:priority<= "C" :not (:tag "TG")))
                                   (:discard (:tag "TG"))
                                   )
                                 ))))))

   (add-to-list 'org-agenda-custom-commands
                '("x" "All todo group by category"
                  ((alltodo "" ((org-super-agenda-groups
                                 '(
                                   (:auto-category t)
                                   )))))))

   (add-to-list 'org-agenda-custom-commands
                '("fb" "Books"
                  ((alltodo "" ((org-super-agenda-groups
                                 '(
                                   (:name "Books" :tag "book")
                                   (:discard (:anything t))
                                   )))))))
   (add-to-list 'org-agenda-custom-commands
                '("fr" "To read"
                  ((alltodo "" ((org-super-agenda-groups
                                 '(
                                   (:name "To read" :tag "read")
                                   (:discard (:anything t))
                                   )))))))


   (add-to-list 'org-agenda-custom-commands
                '("r" "All todo group by area"
                  ((alltodo "" ((org-super-agenda-groups
                                 '(
                                   (:auto-property "AREA")
                                   )))))))

   


   ))



;;; org-fancy-priorities
;; (eye-install-packages '(("org-fancy-priorities" . "https://github.com/harrybournis/org-fancy-priorities.git")))
;; (require 'org-fancy-priorities)
;; (with-eval-after-load 'org-fancy-priorities
;;    (add-hook 'org-mode-hook 'org-fancy-priorities-mode)
;;    (setq org-priority-lowest ?D)
;;    ;; need install symbola font
;;    (setq org-fancy-priorities-list '("🗲" "⬆" "⬇" "🍺"))
;;    )



;;------------------------------------------------------
;;;; org-capture
;;------------------------------------------------------
(require 'org-capture)
;; capture 的目标路径不能直接使用 concat

(defun eye/add-journal ()
  "open journal file and insert new note"
  (interactive)
  (let* ((journal-file (concat (get-locale-book-dir)
                              (format-time-string "/journals/%Y-journal.org" (current-time))
                              ))
         (today-date (format-time-string "%Y-%m-%d" (current-time))) ;; today
         (goto-date (org-read-date)) ;; select date
         )
    (when (f-exists-p journal-file)
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
                (org-end-of-subtree)
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
  )


;; Inbox
(add-to-list 'org-capture-templates '("i" "Inbox" entry (file+headline gtd-inbox-path "Inbox")
				      "* TODO %i%?"))


;; Tickler
;; %^t 输入提醒时间
(add-to-list 'org-capture-templates '("t" "Tickler" entry (file+headline gtd-tickler-path "Tickler")
				      "* %i%? \n %U"))

;; bookmark
(add-to-list 'org-capture-templates '("b" "Bookmark" entry (file+headline eye-bookmarks-path "Bookmarks")
                                      "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1))

(add-to-list 'org-capture-templates '("d" "Ideas" entry (file+headline eye-ideas-path "Ideas")
                                      "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1))

(add-to-list 'org-capture-templates
             '("c" "Contacts" entry (file eye-org-contacts-file)
               "* %^{姓名} %^{手机号}p %^{邮箱}p %^{住址}p\n\n  %?" :empty-lines 1 :kill-buffer 1))


;; (setq org-capture-project-file (concat (get-locale-book-dir) "/note/1-Projects/proj-ts.org"))
;; (add-to-list 'org-capture-templates
;;              '("p" "Project Todo" entry (file org-capture-project-file)
;;                "* TODO %?\n"))

;; (add-to-list 'org-capture-templates
;;              '("o" "Books" entry (file "x:/orgnotes/note/2-Area/books.org")
;;                "* %^{书名} %^{作者}p \n  %?" :empty-lines 1))


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

(require 'init-org-journal)
(require 'init-org-ql)

(provide 'init-agenda)
