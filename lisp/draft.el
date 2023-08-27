
(bm-modeline-info)


;; Disable buildin org, https://emacs-china.org/t/topic/3931/3
(defun eh-hack-load-path ()
  ;; Delete buildin org's PATH
  (setq load-path
        (cl-remove-if
         #'(lambda (path)
             (string-match "lisp/org$" path))
         load-path))
  ;; Demove property lists to defeat cus-load and remove autoloads
  (mapatoms
   #'(lambda (sym)
       (let ((sym-name (symbol-name sym)))
         (when (string-match "^\\(org\\|ob\\|ox\\)-?" sym-name)
           (setplist sym nil)
           (when (autoloadp sym)
             (unintern sym)))))))
;; 使用org-mode9.3版本
;; 带中文的链接不会被转成百分号
;; 附件就在当前位置插入
;;(eh-hack-load-path)
;;(autoload 'org-version "org" "" t) ;; fix yankpad-insert error if use org9.3
;;(add-to-list 'load-path (expand-file-name "org-mode/lisp" "~/src/emacs-packages"))

;; 9.3使用<s需要org-tempo
;;(require 'org-tempo)

;; 快速添加 src block，使用 <el 加 tab 键
;; 导致在emacs28中org-agenda报错，不需要使用了，直接用<s
;;(add-to-list 'org-structure-template-alist '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))
;;(add-to-list 'org-structure-template-alist '("cpp" "#+BEGIN_SRC C++\n?\n#+END_SRC"))
;;(add-to-list 'org-structure-template-alist '("lu" "#+BEGIN_SRC lua\n?\n#+END_SRC"))
;;(add-to-list 'org-structure-template-alist '("py" "#+BEGIN_SRC python\n?\n#+END_SRC"))




;; for task more times repeat a week.
;; @see https://stackoverflow.com/questions/8751287/weekly-repeating-tasks-emacs-org-mode
;;(require 'org-install)
;;(add-to-list 'org-modules 'org-habit)



;;; custom agenda command
;; method 1
;; (setq eye/gtd-someday-view
;;       `("U" "someday" todo "SOMEDAY"
;;         ((org-agenda-files (list locale-gtd-task)))))
;; (add-to-list 'org-agenda-custom-commands `,eye/gtd-someday-view)
;; method 2

;;;; simple gtd
(setq eye-journal-dir (concat locale-notebook-dir "/gtd/journal"))

(defun eye-journal-find-location ()
  (expand-file-name (format-time-string "%Y-%m-%d.org")
		    eye-journal-dir))

(defun eye/open-journal-file ()
  (interactive)
  (let* ((path (eye-journal-find-location))
	 (iscreate (not (file-exists-p path))))
    (when path
      ;; (setq today-journal-path path)
      (find-file path)
      ;; goto today entry
      ;; (search-forward (format-time-string "* %Y-%m-%d") nil t)
      ;; goto end
      (end-of-buffer)
      )))




  ;; diary, 有bug，导致agenda无显示
  ;;(setq org-agenda-include-diary t)
  ;;(setq org-agenda-diary-file "/home/dev/orgnote/standard-diary")
  ;;(setq diary-file org-agenda-diary-file)

  ;; location
  (setq calendar-longitude 116.9962)
  (setq calendar-latitude 39.91)

  ;;Sunrise and Sunset
  ;;日出而作, 日落而息
;;(require 'solar)
;;(defun diary-sunrise ()
;;  (let ((dss (diary-sunrise-sunset)))
;;    (with-temp-buffer
;;	(insert dss)
;;	(goto-char (point-min))
;;	(while (re-search-forward " ([^)]*)" nil t)
;;	  (replace-match "" nil nil))
;;	(goto-char (point-min))
;;	(search-forward ",")
;;	(buffer-substring (point-min) (match-beginning 0)))))

;;(defun diary-sunset ()
;;  (let ((dss (diary-sunrise-sunset))
;;	  start end)
;;    (with-temp-buffer
;;	(insert dss)
;;	(goto-char (point-min))
;;	(while (re-search-forward " ([^)]*)" nil t)
;;	  (replace-match "" nil nil))
;;	(goto-char (point-min))
;;	(search-forward ", ")
;;	(setq start (match-end 0))
;;	(search-forward " at")
;;	(setq end (match-beginning 0))
;;	(goto-char start)
;;	(capitalize-word 1)
;;	(buffer-substring start end))))




;; (setq org-todo-keywords
;;       '((sequence "INBOX(i)")
;; 	(sequence "TASK(t)" "SOMEDAY(s)" "REPEAT(r)" "CALENDAR(c)")
;; 	(sequence "TODO(T)" "ACTION(a)" "WAIT(w)" "DONE(d!)" "|" "CANCELLED(C)" "DEFERRED(f)")
;; 	(sequence "PLAN(p)")
;; 	))


(setf org-todo-keyword-faces
      '(;;("INBOX" . (:foreground "OliveDrab" :bold t :weight bold))
	;;("TASK" . (:foreground "OrangeRed" :bold t :weight bold))
	;;("SOMEDAY" . (:foreground "chocolate" :bold t :weight bold))
	;;("REPEAT" . (:foreground "#009900" :bold t :weight bold))
	;;("ACTION" . (:foreground "cyan" :bold t :weight bold))
	("TODO" . (:foreground "red" :bold t :weight bold))
	("WAITING" . (:foreground "DarkRed" :bold t :weight bold))
	("DONE" . (:foreground "green" :bold t :weight bold))
	("CANCELLED" . (:foreground "gray50" :bold t :weight bold))
	))


;; column view format, can write "#+COLUMNS: ..." to org file also
;; (setq org-columns-default-format "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent)  %DEADLINE(Deadline)")
(setq org-columns-default-format "%7Goal(Goal) %12DEADLINE(Deadline) %30SCHEDULED(Scheduled) %8TODO(To Do) %1PRIORITY(P) %150ITEM(Detailes)")


;; tags
;; #+TAGS: { @work(w) @life(l) @thinking(t) @study(s) }
;; #+TAGS: { @add(a) @bug(b) @fixed(f) }
(setq org-tag-alist '(("ARCHIVE" . ?a) ("work" . ?w) ("learn" . ?l) ("english" . ?e) ("brain" . ?t) ("body" . ?b) ("other" . ?o)))


(defun eye/open-inbox-file () (interactive) (find-file org-default-notes-file))


;;		(defalias 'org-beginning-of-line nil) ;
		;; (defun eye/org-meta-return ()
		;;   "确保按下M-RET时不会打断当前行（但是折叠有属性或内容时会打断属性）"
		;;   (interactive)
		;;   (org-end-of-line)
		;;   (org-meta-return))


(defun transform-square-brackets-to-round-ones(string-to-transform)
  "Transforms [ into ( and ] into ), other chars left unchanged."
  (concat
   (mapcar #'(lambda (c) (if (equal c ?[) ?\( (if (equal c ?]) ?\) c))) string-to-transform))
  )




("z" "View tasks"
 ((todo "TASK"
	((org-agenda-overriding-header "任务")))
  (todo "SOMEDAY"
	((org-agenda-overriding-header "将来/也许")))
  (todo "REPEAT"
	((org-agenda-overriding-header "重复任务")))
  (todo "CALENDAR"
	((org-agenda-overriding-header "日程表")))
  ))



(setq org-agenda-custom-commands
      '(
	("i" "View inbox" todo "INBOX")
	("t" "View tasks" todo "TASK|SOMEDAY|REPEAT|CALENDAR")
	;;("o" "View todo" todo "TODO")
	("x" "View action" todo "ACTION")

	;; 查看project rx
	("p" . "Project")
	("pr" "Rx-unlocker" tags-todo "proj+rx" ;; 搜索tag
	 ((org-agenda-overriding-header "Rx-unlocker") ;;用于显示的字符串
	  (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))
	("ph" "Hisi" tags-todo "proj+hisi" ;; 搜索tag
	 ((org-agenda-overriding-header "Hisi") ;;用于显示的字符串
	  (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))

	))


(defun org-current-is-todo ()
  (string= "TODO" (org-get-todo-state)))

;; 只查看一个需要做的任务
;; 跳过后面的todo项，只显示第一个
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


;; 设置所有以Todo开头的org文件
;; (setq org-agenda-files (directory-files locale-notebook-dir t "Todo.*.org$"))
;; 2019-12-06参考https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
;;(setq org-agenda-files (directory-files "/home/dev/orgnote/diary-by-months/" t "org$"))
;;(add-to-list 'org-agenda-files org-default-notes-file)



;;(format (org-insert-time-stamp (current-time))) ;; 自动插入时间戳




;;;; org-journal
;; System locale to use for formatting time values.
;; current is "zh_CN.UTF-8", if set to "C", Make sure that the weekdays in the
;; time stamps of your Org mode files and in the agenda appear in English.
;; @see https://github.com/batsibe/org-journal
(add-to-list 'load-path (concat auto-require-packages-dir "/org-journal"))
(require 'org-journal)
(setq org-journal-file-type 'daily)
(setq org-journal-dir (concat locale-notebook-dir "/journal/"))
(setq org-journal-file-format "%Y-%m-%d.org")
(setq org-journal-date-format "%Y-%m-%d, %u")
;; (setq today-journal-path (org-journal-find-location))

(defun org-journal-date-format-func (time)
  "Custom function to insert journal date header.
  When buffer is empty prepend a header in front the entry header."
  (concat (when (= (buffer-size) 0)
	    (concat
	     (pcase org-journal-file-type
	       (`daily "#+TITLE: Daily Journal\n")
	       (`weekly "#+TITLE: Weekly Journal\n")
	       (`monthly "#+TITLE: Monthly Journal\n")
	       (`yearly "#+TITLE: Yearly Journal\n"))))
	  org-journal-date-prefix
	  (format-time-string "%Y-%m-%d\n:PROPERTIES:\n:CATEGORY: diary\n:END:\n" time)
	  ))

(setq org-journal-date-format 'org-journal-date-format-func)

;; When =org-journal-file-pattern= has the default value, this would be the regex.
(setq org-agenda-file-regexp "\\`\\\([^.].*\\.org\\\|[0-9]\\\{8\\\}\\\(\\.gpg\\\)?\\\)\\'")
(add-to-list 'org-agenda-files org-journal-dir)


(defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  ;; Position point on the journal's top-level heading so that org-capture
  ;; will add the new entry as a child entry.
  (goto-char (point-min)))



(defun org-journal-save-entry-and-exit()
    "Simple convenience function.
  Saves the buffer of the current day's entry and kills the window
  Similar to org-capture like behavior"
    (interactive)
    (save-buffer)
    (kill-buffer-and-window))
(define-key org-journal-mode-map (kbd "C-x C-s") 'org-journal-save-entry-and-exit)


 'load-path "/home/dev/somacs/packages/dash"



  (require 'prettify-saymbols-mode)
  (add-hook 'org-mode-hook (lambda ()
			     "Beautify org symbols."
			     (push '("[ ]" . ?☐) prettify-symbols-alist)
			     (push '("[X]" . ?☑) prettify-symbols-alist)
			     (push '("[-]" . ?⛝) prettify-symbols-alist)

			     (push '("#+ARCHIVE:" . ?📦) prettify-symbols-alist)
			     (push '("#+AUTHOR:" . ?👤) prettify-symbols-alist)
			     (push '("#+CREATOR:" . ?💁) prettify-symbols-alist)
			     (push '("#+DATE:" . ?📆) prettify-symbols-alist)
			     (push '("#+DESCRIPTION:" . ?🗎) prettify-symbols-alist)
			     (push '("#+EMAIL:" . ?🖂) prettify-symbols-alist)
			     (push '("#+OPTIONS:" . ?⚙) prettify-symbols-alist)
			     (push '("#+TAGS:" . ?🏷) prettify-symbols-alist)
			     (push '("#+TITLE:" . ?🕮) prettify-symbols-alist)

			     (push '("#+BEGIN_SRC" . ?✎) prettify-symbols-alist)
			     (push '("#+END_SRC" . ?□) prettify-symbols-alist)
			     (push '("#+BEGIN_QUOTE" . ?») prettify-symbols-alist)
			     (push '("#+END_QUOTE" . ?«) prettify-symbols-alist)
			     (push '("#+HEADERS" . ?☰) prettify-symbols-alist)
			     (push '("#+RESULTS:" . ?💻) prettify-symbols-alist))
	    (prettify-symbols-mode 1)))




;;;; nox

;;Note: suggestion upgrade emacs to 27.x or 28.x, JSON parser much faster, and Nox completion will much smooth.
(add-to-list 'load-path "/home/dev/somacs-master/packages/nox")
(require 'posframe)
(require 'company)
(require 'nox)

(dolist (hook (list
               'c-mode-common-hook
               'c-mode-hook
               'c++-mode-hook
               ))
  (add-hook hook '(lambda () (nox-ensure))))




;;;; etree
;; https://gist.github.com/etyurkin/0f5a35819520f31cfdb7c1653dcdd958
(require 'json)

(defun kwarks--tree-node-action (action)
  (interactive)
  (let ((file-name (get-text-property (point) 'path)))
    (if file-name (funcall action file-name))))

(defvar kwarks--tree-view-keymap (make-sparse-keymap))

(define-key kwarks--tree-view-keymap (kbd "v")
  (lambda ()
    (interactive)
    (kwarks--tree-node-action 'view-file)))

(define-key kwarks--tree-view-keymap (kbd "o")
  (lambda ()
    (interactive)
    (kwarks--tree-node-action 'find-file)))

(define-key kwarks--tree-view-keymap (kbd "RET")
  (lambda ()
    (interactive)
    (kwarks--tree-node-action 'find-file)))

(define-key kwarks--tree-view-keymap (kbd "c")
  (lambda ()
    (interactive)
    (kwarks--tree-node-action 'kill-new)))

(defun kwarks--make-tree (root)
  (let ((cmd (format "tree %s --dirsfirst --noreport -J -l" root))
        (json-object-type 'plist)
        (json-array-type 'list)
        (json-key-type 'symbol))
    (json-read-from-string (shell-command-to-string cmd))))

(defun kwarks--tree-node-set-face (node is-directory is-link)
  (cond (is-directory (propertize node 'font-lock-face
                                  `(:foreground ,(face-foreground 'org-level-1))))
        (is-link (propertize node 'font-lock-face
                             `(:foreground ,(face-foreground 'org-level-3) :slant italic)))
        (t (propertize node 'font-lock-face
                       `(:foreground ,(face-foreground 'org-level-2))))))

(defun kwarks--make-full-name (parent name)
  (cond ((= 0 (length parent)) name)
        ((string-match "\/$" parent) (format "%s%s" parent name))
        (t (format "%s/%s" parent name))))

(defun kwarks--print-tree (tree parent spacing)
  (while tree
    (let* ((record (car tree))
           (type (plist-get record 'type))
           (is-link (string-equal type "link"))
           (name (if is-link
                     (format "%s -> %s"
                             (plist-get record 'name)
                             (plist-get record 'target))
                   (plist-get record 'name)))
           (full-name (kwarks--make-full-name parent (plist-get record 'name)))
           (children (plist-get record 'contents))
           (is-root (= (length spacing) 0))
           (is-directory (string-equal type "directory"))

           (has-more (consp (cdr tree)))
           (prefix (if has-more "├──" "└──"))
           (pad (if is-root 0 3))
           (child-spacing (if has-more
                              (format "%s│%s" spacing (make-string pad ?\s))
                            (format "%s %s" spacing (make-string pad ?\s)))))
      (if is-root
          (insert name)
        (insert
         (propertize (format "%s%s %s" spacing prefix
                             (kwarks--tree-node-set-face (propertize name 'help-echo full-name)
                                                      is-directory is-link))
                     'path full-name
                     'keymap kwarks--tree-view-keymap)))

      (newline)
      (kwarks--print-tree children full-name child-spacing)
      (setq tree (cdr tree)))))

(defun kwarks--get-buffer-directory ()
  (let ((name (buffer-file-name)))
    (cond ((null name) "~")
          (t (file-name-directory name)))))

(defun kwarks/tree (path)
  (interactive (list (read-directory-name "Directory: "
                                          (kwarks--get-buffer-directory))))
  (let* ((buffer-name (format "*tree %s*" path)))
    (get-buffer-create buffer-name)
    (set-buffer buffer-name)
    (read-only-mode -1)
    (erase-buffer)
    (kwarks--print-tree (kwarks--make-tree path) "" "")
    (goto-char (point-min))
    (special-mode)
    (toggle-truncate-lines 1)
    (switch-to-buffer buffer-name)))




(add-to-list 'load-path "~/.emacs.d/packages/emacs-smart-input-source/")
(require 'sis)

(if is-windows
    (sis-ism-lazyman-config "1033" "2052" 'im-select) ;; 设置一个中文输入法和英文输入法
  (sis-ism-lazyman-config "1" "2" 'fcitx) ; fcitx
  )

;; enable the /cursor color/ mode
(sis-global-cursor-color-mode t)
;; enable the /respect/ mode
(sis-global-respect-mode t)
;; enable the /follow context/ mode for all buffers
(sis-global-follow-context-mode t)
;; enable the /inline english/ mode for all buffers
(sis-global-inline-mode t)




(add-to-list 'load-path "~/.emacs.d/packages/org-download")
(require 'org-download)
(setq-default org-download-image-dir "N:/org/attach")
(setq org-download-display-inline-images nil)
;; 移除自动添加的#DOWNLOADED..., @https://github.com/abo-abo/org-download/issues/81
(setq org-download-annotate-function (lambda (_link) ""))
(setq org-download-display-inline-images 'posframe)
(org-download-enable)

;;
(add-to-list 'load-path "~/.emacs.d/packages/emacs-db")
(add-to-list 'load-path "~/.emacs.d/packages/emacs-kv")
(add-to-list 'load-path "~/.emacs.d/packages/emacsql-sqlite3")
(add-to-list 'load-path "~/.emacs.d/packages/emacsql")
(require 'db)
(require 'emacsql-sqlite3)
;; (require 'emacsql-sqlite)
;; all emacsql-sqlite command replacement to emacsql-sqlite3
(defvar navdb (emacsql-sqlite3 "~/nav.db"))
(emacsql navdb [:create-table my-repos2 ([(id integer :primary-key :autoincrement)
                                          (root_path varchar)
                                          (uuid varchar)
                                          ])])

(setq uuid (eye-create-short-uuid))
(emacsql navdb [:insert :into my-repos2
                        :values ([nil "~/.emacs.d" (eye-create-short-uuid)])])

(emacsql navdb [:insert :into my-repos2
                        :values ([nil "/etc" uuid])])




;;;; evil
(add-to-list 'load-path "~/.emacs.d/packages/evil")
(add-to-list 'load-path "~/.emacs.d/packages/emacs-undo-fu")
(add-to-list 'load-path "~/.emacs.d/packages/undo-tree")
(require 'evil)
(evil-mode 1)


(auto-require 'gkroam
	      :load t
	      :paths '("gkroam" "emacs-db" "emacs-kv")
	      :before
	      (progn
		(setq gkroam-root-dir (concat locale-notebook-dir "/org/gkroam"))
		(setq gkroam-prettify-page-p t
		      gkroam-show-brackets-p nil
		      gkroam-use-default-filename t
		      gkroam-window-margin 4))
	      :after
	      (progn
		(gkroam-mode 1)
		(bind-key gkroam-mode-map "C-c r I" #'gkroam-index)
		(bind-key gkroam-mode-map "C-c r d" #'gkroam-daily)
		(bind-key gkroam-mode-map "C-c r D" #'gkroam-delete)
		(bind-key gkroam-mode-map "C-c r f" #'gkroam-find)
		(bind-key gkroam-mode-map "C-c r i" #'gkroam-insert)
		(bind-key gkroam-mode-map "C-c r n" #'gkroam-dwim)
		(bind-key gkroam-mode-map "C-c r e" #'gkroam-link-edit)
		(bind-key gkroam-mode-map "C-c r u" #'gkroam-show-unlinked)
		(bind-key gkroam-mode-map "C-c r p" #'gkroam-toggle-prettify)
		(bind-key gkroam-mode-map "C-c r t" #'gkroam-toggle-brackets)
		(bind-key gkroam-mode-map "C-c r R" #'gkroam-rebuild-caches)
		(bind-key gkroam-mode-map "C-c r g" #'gkroam-update)
		))





(use-package gkroam
  :ensure t
  :hook (after-init . gkroam-mode)
  :init
  (setq gkroam-root-dir "~/gkroam/org/")
  (setq gkroam-prettify-page-p t
        gkroam-show-brackets-p nil
        gkroam-use-default-filename t
        gkroam-window-margin 4)
  :bind
  (:map gkroam-mode-map
        ()))







;;; remove duplicated options
; @see https://emacs.stackexchange.com/questions/12360/how-to-make-private-python-methods-the-last-company-mode-choices
(defun company-transform-python (candidates)
  (let ((deleted))
    (mapcar #'(lambda (c)
		(if (or (string-prefix-p "_" c) (string-prefix-p "._" c))
		    (progn
		      (add-to-list 'deleted c)
		      (setq candidates (delete c candidates)))))
            candidates)
    (append candidates (nreverse deleted))))



(add-to-list 'load-path "~/.emacs.d/packages/elfeed")
(require 'elfeed)
(setq elfeed-feeds
      '(("http://nullprogram.com/feed/" blog emacs)
        "http://www.50ply.com/atom.xml"  ; no autotagging
        ("http://nedroid.com/feed/" webcomic)))
(bind-key elfeed-search-mode-map "C-o" #'eaf-elfeed-open-url "eaf")
;; 更新elfeed后，可以用eaf-elfeed-open-url打开其链接


(add-to-list 'load-path "~/.emacs.d/packages/emacs-rime")
(require 'emacs-rime)

;; 默认值
(setq rime-translate-keybindings
  '("C-f" "C-b" "C-n" "C-p" "C-g" "<left>" "<right>" "<up>" "<down>" "<prior>" "<next>" "<delete>"))




;list of programs, corresponding to extensions
(setq alist-programs
      '(("pdf" ."okular")
        ("djvu" . "okular")
        ("mp3" . "xmms")))

(defun my-run-async-command (command file)
  "Run a command COMMAND on the file asynchronously.
   No buffers are created"
  (interactive
   (let ((file (car (dired-get-marked-files t current-prefix-arg))))
     (list
      ;last element of alist-programs, contains COMMAND
      (cdr
       (assoc
        (file-name-extension file)
        alist-programs))
      file)))
  ;begin of function body
  (if command ;command if not nil?
      (start-process "command" nil command file)
    )
)

;attach function to <f2> key
(add-hook 'dired-mode-hook
      (lambda ()
    (define-key dired-mode-map (kbd "<f2>") 'my-run-async-command)))



(add-to-list 'load-path "~/.emacs.d/packages/emacs-rime")
(require 'rime)
(setq default-input-method "rime")
(rime-librime-root "~/.emacs.d/librime/dist")

D:\emacs_env\msys2_x64\mingw64\bin\librime.dll



(auto-require
 'find-file-in-project
 :load t
 :urls '(("find-file-in-project" . "https://github.com/redguardtoo/find-file-in-project.git"))
 :paths "find-file-in-project"
 :after
 (progn
   (if (executable-find "fd")
       (setq ffip-use-rust-fd t))
   (ivy-mode 1)))


(defun eye/rg-filter-property ()
  (interactive)
  (require 'deft)
  (let ((default-directory deft-directory)
	(keyword-list
	 (split-string (read-string "search:") "&"))
	(prop))
    (setq prop (car keyword-list))
    (color-rg-search-input (format ":%s:" prop))
    (sleep-for 3)
    (switch-to-buffer "*color-rg*")
    (dolist (word (cdr keyword-list))
      (eye--filter-rg-buffer word t)
      (message "filter:%s" word)
      )))

(defun eye--filter-rg-buffer (filter-regexp match-regexp)
  (save-excursion
    (with-current-buffer color-rg-buffer
      (setq remove-counter 0)
      (goto-char (point-min))
      (while (setq start (search-forward-regexp color-rg-regexp-position nil t))
        (setq line-content (color-rg-get-line-content color-rg-buffer (line-number-at-pos)))
        (if match-regexp
            (unless (string-match filter-regexp line-content)
              (color-rg-remove-line-from-results)
              (setq remove-counter (+ 1 remove-counter))
              )
          (when (string-match filter-regexp line-content)
            (color-rg-remove-line-from-results)
            (setq remove-counter (+ 1 remove-counter))
            )))
      (if match-regexp
          (message (format "Remove %s lines not match regexp '%s'." remove-counter filter-regexp))
        (message (format "Remove %s lines match regexp '%s'." remove-counter filter-regexp)))
      ))
  (color-rg-update-header-line-hits)
  )




(auto-require
 'stripe-buffer
 :load t
 :urls '(("stripe-buffer-mode" . "https://github.com/sabof/stripe-buffer.git"))
 :paths "strip-buffer-mode"
 :after
 (progn
   (add-hook 'dired-mode-hook 'turn-on-stripe-buffer-mode)
   )
 )


(auto-require
'bbdb
:load t
:urls '(("bbdb" . "https://git.savannah.nongnu.org/git/bbdb.git"))
:paths "bbdb/lisp"
)


;;(setq calendar-day-abbrev-array "")
(setq calendar-day-header-width 2)
(setq calendar-column-width 3)
(setq calendar-week-start-day 1)
(setq calendar-day-header-array ["日" "一" "二" "三" "四" "五" "六"])
(setq  calendar-month-name-array ["一月" "二月" "三月" "四月" "五月" "六月" "七月" "八月" "九月" "十月" "十一月" "十二月"])



;;;; org-super-links
(auto-require
 'org-supper-links
 :load t
 :urls '(("org-super-links" . "https://github.com/toshism/org-super-links.git"))
 :paths "org-super-links"
 )


(eye-install-packages '(("gumshoe" . "https://github.com/Overdr0ne/gumshoe.git")
			("consult" . "https://github.com/minad/consult.git")
			))
(require 'consult)
(require 'gumshoe)





;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.


(setq display-buffer-alist
      '(("#refs#"
	 (display-buffer-reuse-window
	  display-buffer-in-side-window)
	 (reusable-frames . visible)
	 (side            . right)
	 (window-height   . 0.33))))


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

(add-to-list 'load-path "~/.emacs.d/emacs-packages/window-purpose")
(require 'window-purpose)
(purpose-mode)

;;(add-to-list 'purpose-user-name-purposes '("#refs#" . ))
;;(purpose-compile-user-configuration) ; activates your changes

(add-to-list 'purpose-user-mode-purposes '(python-mode . py))
(add-to-list 'purpose-user-mode-purposes '(inferior-python-mode . py-repl))
(purpose-compile-user-configuration)

(add-to-list 'load-path "~/.emacs.d/emacs-packages/svg-lib")
(require 'svg-lib)

https://github.com/rougier/svg-tag-mode

(image-type-available-p 'svg)

(insert-image (svg-lib-tag "TODO" nil :padding 1 :stroke 2))

(insert-image (svg-lib-button "check-bold" "DONE" nil
                              :font-family "Roboto Mono"
                              :font-weight 500
                              :stroke 0 :background "#673AB7" :foreground "white"))





(add-to-list 'load-path "~/.emacs.d/emacs-packages/ocodo-svg-modelines")
(add-to-list 'load-path "~/.emacs.d/emacs-packages/svg-mode-line-themes")
(add-to-list 'load-path "~/.emacs.d/emacs-packages/xmlgen")

(require 'svg-mode-line-themes)
(require 'ocodo-svg-modelines)

(smt/enable)
(smt/set-theme 'black-crystal)


(add-to-list 'load-path "~/.emacs.d/emacs-packages/svg-tag-mode")
(require 'svg-tag-mode)



(add-to-list 'load-path "~/.emacs.d/emacs-packages/button-lock")
(require 'button-lock)
(button-lock-set-button "http://google.com" 'browse-url-at-mouse)




(defun sh-send-line-or-region (&optional step)
  (interactive ())
  (let ((proc (get-process "shell"))
        pbuf min max command)
    (unless proc
      (let ((currbuff (current-buffer)))
        (shell)
        (switch-to-buffer currbuff)
        (setq proc (get-process "shell"))
        ))
    (setq pbuff (process-buffer proc))
    (if (use-region-p)
        (setq min (region-beginning)
              max (region-end))
      (setq min (point-at-bol)
            max (point-at-eol)))
    (setq command (concat (buffer-substring min max) "\n"))
    (with-current-buffer pbuff
      (goto-char (process-mark proc))
      (insert command)
      (move-marker (process-mark proc) (point))
      ) ;;pop-to-buffer does not work with save-current-buffer -- bug?
    (process-send-string  proc command)
    (display-buffer (process-buffer proc) t)
    (when step
      (goto-char max)
      (next-line))
    ))

(defun sh-send-line-or-region-and-step ()
  (interactive)
  (sh-send-line-or-region t))
(defun sh-switch-to-process-buffer ()
  (interactive)
  (pop-to-buffer (process-buffer (get-process "shell")) t))




(define-key sh-mode-map [(control ?j)] 'sh-send-line-or-region-and-step)
(define-key sh-mode-map [(control ?c) (control ?z)] 'sh-switch-to-process-buffer)


(auto-require
'selectrum
:load t
:urls '(("selectrum" . "https://github.com/raxod502/selectrum.git"))
:paths "selectrum"
:after
(progn
  (selectrum-mode +1)
  )
)


(selectrum-completing-read "input:" '("hello" "world"))


(auto-require
 'page-break-lines
 :load t
 :urls '(("page-break-lines" . "https://github.com/purcell/page-break-lines.git"))
 :paths "page-break-lines"
 :after
 (global-page-break-lines-mode))


https://github.com/raxod502/ctrlf.git
(ctrlf-mode +1)








(auto-require
 'elscreen
 :load t
 :urls '(("elscreen" . "https://github.com/knu/elscreen.git"))
 :paths "elscreen"
 :after
 (progn
   (elscreen-start)
   (set-face-attribute 'elscreen-tab-background-face nil :background "#335ea8")
   (set-face-attribute 'elscreen-tab-control-face nil :foreground "white" :background "#335ea8")
   (set-face-attribute 'elscreen-tab-current-screen-face nil :foreground "#335ea8" :background "#c6c6c6")
   (set-face-attribute 'elscreen-tab-other-screen-face nil :foreground "#eeeeee" :background "#335ea8")
   )
 )


(auto-require
 'perspective
 :load t
 :urls '(("perspective-el" . "https://github.com/nex3/perspective-el.git"))
 :paths "perspective-el"
 :after
 (progn
   ;; (require 'perspective)
   (setq persp-state-default-file "~/.emacs.d/perspective-data.db")
   (add-hook 'kill-emacs-hook #'persp-state-save)
   ;; (bind-key global-map "C-x b" 'persp-switch-to-buffer*)
   ;; (bind-key global-map "C-x k" 'persp-kill-buffer*)
   ;; (setq persp-mode-prefix-key "C-w C-e")
   (define-key persp-mode-map persp-mode-prefix-key 'perspective-map)
   (persp-mode)
   ;; (bind-key global-map "<C-S-left>" 'persp-prev)
   ;; (bind-key global-map "<C-S-right>" 'persp-next)
   (set-face-attribute 'persp-selected-face nil :foreground "HotPink1")

   (persp-state-load persp-state-default-file)
   )
 )



(add-to-list 'load-path "~/.emacs.d/emacs-packages/nerdtab")
(require 'nerdtab)

(defun my-agenda-query-button-func ()
  '("stuck" "TODO" "DONE" "emacs" "git" "repeat" "train" "face" "drive" "others"
    "marry" "memoryapp" "learn" "project" "inbox"))

(setq nerdtab-list-func 'my-agenda-query-button-func)

(defun nerdtab--make-tab-list ()
  "Make a tab list from `nerdtab--tab-list'."
  (let ((tab-list ())
        (max-tab-num (nerdtab--h-this-v-that|
                      (nerdtab-max-tab-horizontal)
                      (nerdtab-max-tab-vertical)))
        (count 0))
    (catch 'max-num
      (dolist (buffer (funcall nerdtab-list-func))
          (when (>= count max-tab-num)
            (throw 'max-num count))
          (setq count (1+ count))
          (push buffer tab-list))
        )
    tab-list))



(defun nerdtab--draw-tab (tab index &optional current-buffer)
  ;;(message "nerdtab--draw-tab")
  ;;  (insert-text-button (format "%s" tab))
  (let* ((tab-name tab))
    (insert-text-button (format "%d %s" index tab-name)
                        'keymap
                        (let ((keymap (make-sparse-keymap)))
                          (define-key keymap [mouse-1]
                            `(lambda ()
                               (interactive)
                               (nerdtab--open-query ,tab-name)))
			              (define-key keymap (kbd "<RET>")
                            `(lambda ()
                               (interactive)
                               (nerdtab--open-query ,tab-name)))
                          keymap)
                        'follow-link nil
                        'mouse-face 'nerdtab-tab-mouse-face)))



(defun nerdtab--open-query (keyword)
  (if (string-match "nerdtab" (buffer-name))
      (other-window 1))
  (cond ((string-equal "TODO" keyword)
         (eye/agenda-query-TODO ())
         )
        ((string-equal "stuck" keyword)
         (org-agenda-list-stuck-projects)
         )
        ((string-equal "project" keyword)
         (eye-agenda-by-proj)
         )
        (t
         ;; (message "Query:%s" keyword)
         (eye/agenda-query-tag keyword)
         )
        )
  )


(defun eye/agenda-query-TODO ()
  (interactive)
  (apply 'org-agenda nil '("t"))
  )

(defun eye/agenda-query-tag (tag-name)
  (org-tags-view t tag-name))


(eye/agenda-query-tag "repeat")

(let ((org-super-agenda-groups
       '((:auto-group t))))
  (org-agenda-list))


(add-to-list 'load-path "~/.emacs.d/emacs-packages/organic-green-theme")
(require 'organic-green-theme)
(load-theme 'organic-green)


line-height

ddasdf

(defun eye/)

(run-with-idle-timer 10 t (lambda ()
                            (read-only-mode t)
                            (user-error "read only mode enabled!")
                            ))




;;;; awesome-tray
(when is-gui
  (auto-require
   'awesome-tray
   :load t
   :paths "awesome-tray"
   :functions 'awesome-tray-mode
   :after
   (progn
     (progn
       ;; Create a module to show file encoding
       (defun tray-module-encode-info ()
         (let* ((encoding buffer-file-coding-system)
                show-str
                )
           ;; 显示简短信息
           (cond ((eq encoding 'utf-8-unix) (setq show-str "utf8 LF"))
                 ((eq encoding 'utf-8-dos) (setq show-str "utf8 CRLF"))
                 ((eq encoding 'chinese-gb18030-unix) (setq show-str "gbk LF"))
                 ((eq encoding 'chinese-gb18030-dos) (setq show-str "gbk CRLF"))
                 (t (setq show-str (format "%s" encoding)))
                 )
           show-str
           ))
       (defface tray-module-encode-face
         '((((background light))
            :foreground "#00a400" :bold t)
           (t
            :foreground "green3" :bold t))
         "Encode name face."
         :group 'awesome-tray)
       (add-to-list 'awesome-tray-module-alist
                    '("encode" . (tray-module-encode-info tray-module-encode-face)))
       (add-to-list 'awesome-tray-active-modules "encode")
       )

     (progn
       ;; 显示在哪个git目录下，没有在git目录，则显示父目录
       (defun awesome-tray-module-project-dir-info ()
         ;; (interactive)
         (let* ((project-dir (locate-dominating-file ".git" ".git"))
                )
           (if (not project-dir)
               (setq project-dir (file-name-nondirectory (directory-file-name default-directory)))
             (progn
               ;; 从目录中获取最后一个目录名
               (setq project-dir (file-name-nondirectory
                                  (s-left (- (length project-dir) 1) project-dir)
                                  )))
             )
           (format "%s" project-dir))
         )
       (add-to-list 'awesome-tray-module-alist
                    '("project" . (awesome-tray-module-project-dir-info tray-module-encode-face)))
       (add-to-list 'awesome-tray-active-modules "project")
       )

     (progn
       ;; 显示在哪个函数
       (defun awesome-tray-module-function-name-info ()
         (format "%s" (which-function)))
       (add-to-list 'awesome-tray-module-alist
                    '("func-name" . (awesome-tray-module-function-name-info awesome-tray-default-face)))
       (add-to-list 'awesome-tray-active-modules "func-name")
       (which-function-mode 1)
       )

     (setq awesome-tray-file-name-max-length 40)
     (setq awesome-tray-buffer-name-max-length 40)
     (setq awesome-tray-active-modules '( "location" "buffer-name" "git"  "project"  "encode"))

     ;; set color
     (setq awesome-tray-mode-line-active-color "DarkGreen")

     ;; Enable awesome-tray-mode
     (awesome-tray-mode 1)

     ;; headerline用于显示elscreen
     (if (featurep 'elscreen)
         (progn
           (set-face-attribute 'header-line nil
                               :background "#335ea8"
                               :height 10.0)
           ))


     )))


(setq layout-rt-window nil)

(defun eye/set-my-layout ()
  (interactive)
  (setq layout-rt-window (split-window-right))
  (other-window 1)
  (split-window-below)
  )


(auto-require
 'edwina
 :urls '(("edwina" . "https://github.com/ajgrf/edwina.git"))
 :load t
 )



;; 合并org文件
(let* ((jor-dir "x:/orgnotes/note/journal")
       (files (directory-files jor-dir t "2022-"))
       (new-file-name  "x:/orgnotes/note/journal/2022.org")
       )
  (with-temp-file new-file-name
    (dolist (file files)
      (message "org file:%s" file)
      (insert-file file)
      (end-of-buffer)
      (newline)
      )
    (write-file new-file-name)
    (message "save finished.")
    )
)




(require 'magit)



;;
(use-package popper
  :ensure t ; or :straight t
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
(popper-echo-mode +1))                ; For echo area hints

(auto-require
'popper
:urls '(("popper" . "https://github.com/karthink/popper.git"))
:load t
:paths "popper"
:functions '(popper-toggle-latest popper-cycle popper-toggle-type)
:before
(progn
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  )
:after
(progn
  (popper-mode +1)
  ;; (require 'popper-echo)
  ;; (popper-echo-mode +1)
  )
)

;; org-link-beautify
;; 支持生成pdf文件封面，在org中显示
(add-to-list 'load-path "~/.emacs.d/emacs-packages/org-link-beautify")
(require 'org-link-beautify)



;;;; dirvish
(auto-require 'dirvish
:load t
:urls '(("dirvish" . "https://github.com/alexluigit/dirvish.git"))
:paths "dirvish"
:after
(progn
  ;; (require 'dirvish)
  )
)

(eye/use-package
'emojify
:ensure t
:urls '(("emojify" . "https://github.com/iqbalansari/emacs-emojify.git"))
:load-path "emojify"
)

  :hook (after-init . global-emojify-mode))



(eye/use-package
'shanty-theme
:ensure t
:urls '(("shanty-theme" . "https://github.com/qhga/shanty-theme.git"))
)

(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-packages/shanty-theme")
(load-theme 'shanty-dark t)

(eye/use-package 'org-remark
:ensure t
:urls '(("org-remark" . "https://github.com/nobiot/org-remark"))
:load-path "org-remark"
)

(org-remark-create "red"
                   '(:foreground "red" :underline "black")
                   '(CATEGORY "exam"))

(org-remark-create "magenta"
                   '(modus-themes-nuanced-magenta))

(org-remark-create "typo"
                   '(:underline (:color "#8f0075" :style wave))
                   '(help-echo "Fix the typo"))



;; https://www.reddit.com/r/unixporn/comments/s7p7pr/so_which_run_launcher_do_you_use_rofi_or_dmenu/
(defun emacs-run-launcher ()
"Create and select a frame called emacs-run-launcher which consists only of a minibuffer and has specific dimensions.
 Run counsel-linux-app on that frame, which is an emacs command that prompts you to select an app and
 open it in a dmenu like behaviour. Delete the frame after that command has exited"
(interactive)
(with-selected-frame (make-frame '((name . "emacs-run-launcher")
                                   (minibuffer . only)
                                   (width . 120)
                                   (height . 11)))
  (unwind-protect
      (counsel-linux-app)
    (delete-frame))))


(user-uid)


(eye/use-package 'eno
:urls '(("eno" . "https://github.com/enoson/eno.el.git")
("edit-at-point" . "https://github.com/enoson/edit-at-point.el.git"))
:ensure t
:load-path '("eno" "edit-at-point")
)



(setq org-preview-latex-default-process 'dvisvgm)
(setq org-preview-latex-process-alist
        '((dvisvgm :programs
                   ("xelatex" "dvisvgm")
                   :description "xdv > svg" :message "you need to install the programs: xelatex and dvisvgm." :use-xcolor t :image-input-type "xdv" :image-output-type "svg" :image-size-adjust
                   (1.7 . 1.5)
                   :latex-compiler
                   ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
                   :image-converter
                   ("dvisvgm %f -n -b min -c %S -o %O"))
          (imagemagick :programs
                       ("xelatex" "convert")
                       :description "pdf > png" :message "you need to install the programs: xelatex and imagemagick." :use-xcolor t :image-input-type "pdf" :image-output-type "png" :image-size-adjust
                       (1.0 . 1.0)
                       :latex-compiler
                       ("xelatex -interaction nonstopmode -output-directory %o %f")
                       :image-converter
                       ("convert -density %D -trim -antialias %f -quality 100 %O"))))




(setq org-latex-packages-alist
      '(("fontset=macnew,UTF8" "ctex" t)))

 (setq org-preview-latex-default-process 'imagemagick)

  (setq org-preview-latex-process-alist
  '(
    (dvisvgm
     :programs ("xelatex" "dvisvgm")
     :description "xdv > svg"
     :message "you need to install the programs: xelatex and dvisvgm."
     :image-input-type "xdv"
     :image-output-type "svg"
     :image-size-adjust (1.7 . 1.5)
     :latex-compiler ("xelatex --no-pdf -interaction nonstopmode -output-directory %o %f")
     :image-converter ("dvisvgm %f -n -b min -c %S -o %O"))
    (imagemagick
     :programs ("latex" "convert")
     :description "pdf > png"
     :message "you need to install the programs: xelatex and imagemagick."
     :image-input-type "pdf"
     :image-output-type "png"
     :image-size-adjust (1.0 . 1.0)
     :latex-compiler ("xelatex -interaction nonstopmode -output-directory %o %f")
     :image-converter
     ("convert -density %D -trim -antialias %f -quality 100 %O"))))

(setq org-preview-latex-process-alist
      '(
        (imagemagick
         :programs ("pdflatex" "convert")
         :description "pdf > png"
         :message "you need to install the programs: miktex and imagemagick."
         :image-input-type "pdf"
         :image-output-type "png"
         :image-size-adjust (0.5 . 0.5)
         :latex-compiler ("pdflatex %f")
         :image-converter
         ("convert -density %D %f -quality 90 %O")
         )
        )
      )








(defcustom org-inline-image-background nil
  "The color used as the default background for inline images.
  When nil, use the default face background."
  :group 'org
:type '(choice color (const nil)))



(defun org--create-inline-image (file width)
  "Create image located at FILE, or return nil.
WIDTH is the width of the image.  The image may not be created
according to the value of `org-display-remote-inline-images'."
  (let* ((remote? (file-remote-p file))
	 (file-or-data
	  (pcase org-display-remote-inline-images
	    ((guard (not remote?)) file)
	    (`download (with-temp-buffer
			 (set-buffer-multibyte nil)
			 (insert-file-contents-literally file)
			 (buffer-string)))
	    (`cache (let ((revert-without-query '(".")))
		      (with-current-buffer (find-file-noselect file)
			(buffer-string))))
	    (`skip nil)
	    (other
	     (message "Invalid value of `org-display-remote-inline-images': %S"
		      other)
	     nil))))
    (when file-or-data
      (create-image file-or-data
		    (and (image-type-available-p 'imagemagick)
			 width
			 'imagemagick)
		    remote?
		    :width width
                    :background org-inline-image-background))))


(set-face-attribute 'link nil :foreground "red")

(set-face-attribute 'highlight nil :inherit 'default)

(set-face-attribute 'org-link nil :foreground "red")
(set-face-attribute 'cursor nil :foreground "red" :background "green")
(set-face-attribute 'show-paren-match nil :foreground "red" :background "green")


(set-face-attribute 'org-block-begin-line nil :foreground "gray50" :background "gray20")
(set-face-attribute 'org-block-end-line nil :foreground "gray50" :background "gray20")




(executable-find "mpv")


(transient-define-prefix eye/org-command ()
[["Open"
  ("l" "Note list" eye/open-org-note-file)
  ("b" "image" eye-open-image-at-point)
  ]

["Insert"
 ("a" "Anchor" eye/open-org-note-file)
 ("i" "image" eye-open-image-at-point)
 ]


])

(org-nav-source-open "3ce2669c@bc316aad" nil)

[[nav:3ce2669c@bc316aad][mpv调试]]



;; [[nav:3ce2669c@80395ce6][⚓]]


(eye/use-package 'helm-org-rifle
:urls '(("helm-org-rifle" . "https://github.com/alphapapa/org-rifle.git"))
:load-path '("org-rifle" "helm" "async")
:ensure t)
helm-org-rifle-org-directory ;; 卡
(setq org-directory "x:/orgnotes/note")


;;;; json
(with-progress "load json-mode"
(eye/use-package 'json-mode
:urls '(("json-snatcher" . "https://github.com/Sterlingg/json-snatcher.git")
("json-mode" . "https://github.com/joshwnj/json-mode.git"))
:ensure t
:load-path '("json-snatcher" "json-mode")
))







(set-face-attribute 'hl-line nil :background "#262829")



(setq org-agenda-custom-commands
      '(("d" "Daily agenda and all TODOs"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "------High-priority unfinished tasks:------")))
          (agenda "" ((org-agenda-ndays 1) ;; day agenda会显示和前面A级任务重复项
                      (org-agenda-skip-function '(air-org-skip-subtree-if-priority ?A)) ;; invalid
                      ))
          (alltodo ""
                   ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
                                                   (air-org-skip-subtree-if-priority ?A)
                                                   (org-agenda-skip-if nil '(scheduled deadline))))
                    (org-agenda-overriding-header "------ALL normal priority tasks:------"))))
         ((org-agenda-compact-blocks t)))))


(defun air-org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))



(eye/use-package
'elisp-tree-sitter
:urls '(("elisp-tree-sitter" . "https://github.com/emacs-tree-sitter/elisp-tree-sitter.git"))
:ensure t
:load-path '("elisp-tree-sitter/lisp" "elisp-tree-sitter/core")
:config
(progn
(require 'tree-sitter)
(require 'tree-sitter-langs)

)
)




;; Please set your themes directory to 'custom-theme-load-path
(add-to-list 'custom-theme-load-path
             (file-name-as-directory "D:/download/replace-colorthemes-master"))

(load-theme 'aalto-dark t t)
 (enable-theme 'aalto-dark)


(add-to-list 'custom-theme-load-path (concat eye-packages-dir "/silver-gray"))
(add-to-list 'custom-theme-load-path (concat eye-packages-dir "/brown-theme"))

(load-theme 'brown t)



(add-to-list 'load-path (concat eye-packages-dir "/citre"))
(require 'citre)
(require 'citre-config)


(defun eye/replace-windows-slash ()
"替换选中区域的"
(interactive)

)




(defun eye/refile-current-to-dir ()
  (interactive)
  (when (buffer-file-name)
(f-move (buffer-file-name) (ido-read-directory-name "Move to: "))
    )

  )





(eye/use-package 'grey-paper-theme
:urls '(("grey-paper-theme" . "https://github.com/gugod/grey-paper-theme.git"))
:ensure t
)

(eye/use-package
'mindre-theme
:urls '(("mindre-theme" . "https://github.com/Elilif/mindre-theme"))
:ensure t
:config
(progn
(require 'mindre-theme)
(setq mindre-use-more-bold nil
      mindre-use-faded-lisp-parens t)
)
)





(setq org-agenda-prefix-format
      (quote
       ((agenda . "%-20c%?-16t% s")
        (timeline . "% s")
        (todo . "%-12c")
        (tags . "%-12c")
        (search . "%-12c"))))

(setq org-agenda-deadline-leaders (quote ("!D!: " "D%2d: " "")))
(setq org-agenda-scheduled-leaders (quote ("" "S%3d: ")))

(add-to-list 'load-path (concat eye-packages-dir "/org-imagine"))
(require 'org-imagine)


;;;; extend treemacs

;; extension for treemacs
(require 'dash)
(require 'treemacs)
(require 'treemacs-treelib)
(defun treemacs-showcase--buffer-major-modes ()
  (->> (buffer-list)
       (--reject (string-prefix-p " " (buffer-name it)))
       (--map (buffer-local-value 'major-mode it))
       (-distinct)))

(defun treemacs-showcase--buffers-by-mode (mode)
  (->> (buffer-list)
       (--filter (eq mode (buffer-local-value 'major-mode it)))
       (--reject (string-prefix-p " " (buffer-name it)))))

(defun treemacs-showcase-RET-buffer-action (&optional _)
  (let ((buffer (-some-> (treemacs-current-button)
                  (treemacs-button-get :buffer))))
    (when (buffer-live-p buffer)
      (pop-to-buffer buffer))))

(treemacs-define-entry-node-type showcase-buffers
  :label (propertize "Buffers" 'face 'font-lock-keyword-face)
  :key 'showcase-buffers
  :open-icon (treemacs-get-icon-value 'list)
  :closed-icon (treemacs-get-icon-value 'list)
  :children (treemacs-showcase--buffer-major-modes)
  :child-type 'showcase-buffer-group)

(treemacs-define-expandable-node-type showcase-buffer-group
  :closed-icon "+ "
  :open-icon "- "
  :label (propertize (symbol-name item) 'face 'font-lock-variable-name-face)
  :key item
  :children (treemacs-showcase--buffers-by-mode (treemacs-button-get btn :major-mode))
  :child-type 'showcase-buffer-leaf
  :more-properties `(:major-mode ,item))

(treemacs-define-leaf-node-type showcase-buffer-leaf
  :icon "• "
  :label (propertize (or (buffer-name item) "#<killed buffer>")
                     'face 'font-lock-string-face)
  :key item
  :more-properties `(:buffer ,item)
  :ret-action #'treemacs-showcase-RET-buffer-action)

(treemacs-enable-project-extension
 :extension 'showcase-buffers
 :position 'top
:predicate (lambda (project) (eq project (car (treemacs-workspace->projects (treemacs-current-workspace))))))

(treemacs-define-expandable-node-type showcase-monotype-buffers
  :closed-icon
  (if (bufferp item)
      "• "
    "+ ")
  :open-icon
  (if (bufferp item)
      "•"
    "- ")
  :label
  (if (bufferp item)
      (propertize (buffer-name item) 'face 'font-lock-string-face)
    (propertize (symbol-name item) 'face 'font-lock-variable-name-face))
  :key
  (if (bufferp item)
      (buffer-name item)
    item)
  :children
  (when (symbolp item)
    (treemacs-showcase--buffers-by-mode item))
  :child-type
  'showcase-monotype-buffers
  :more-properties
  (if (bufferp item)
      `(:buffer ,item :leaf t)
`(:major-mode ,item)))

(treemacs-define-entry-node-type showcase-buffers-monotype-entry
  :key 'showcase-buffers-monotype-entry
  :label (propertize "Monotype Buffers" 'face 'font-lock-keyword-face)
  :open-icon (treemacs-get-icon-value 'list)
  :closed-icon (treemacs-get-icon-value 'list)
  :children (treemacs-showcase--buffer-major-modes)
  :more-properties nil
  :child-type 'showcase-monotype-buffers)

(treemacs-enable-project-extension
 :extension 'showcase-buffers-monotype-entry
 :predicate (lambda (project) (eq project (car (treemacs-workspace->projects (treemacs-current-workspace)))))
:position 'top)

(showcase-buffer-groups)


;;;;eaf
(eye/use-package
'emacs-application-framework
:ensure t
:load-path '("emacs-application-framework" "emacs-application-framework/core" "emacs-application-framework/app/git")
:config
(progn
(require 'eaf)
;; (require 'eaf-terminal)
;; (require 'eaf-pdf-viewer)
(require 'eaf-git)
(require 'eaf-browser)
)
)


(defun eye/tmp-replace ()
  (interactive)
  (save-excursion
    (replace-string "../../attach" "~/attach_ts")
    (replace-string "~/attach" "~/attach_ts")
    )
  )


(defun eye/tmp-replace-in-dir ()
  (interactive)
  (dolist (file (directory-files (concat locale-notebook-dir "/org/note") "*.org"))
    (when (string-equal (s-right 3 file) "org")
      (find-file file)
      (eye/tmp-replace)
      (save-buffer)
      (kill-this-buffer)
      )
    )
  (message "replace all finished!")
  )







(defun copy-url-to-bookmarks ()
  "从firefox导出的html标签中复制链接到orgmode书签中，需要先用eww打开html文件。"
  (interactive)
  (save-excursion
    (let* ((url (get-text-property (point) 'shr-url))
           (desc "")
           p1 p2 orglink
           )
      (beginning-of-line)
      (setq p1 (point))
      (end-of-line)
      (setq p2 (point))
      (setq desc (buffer-substring-no-properties p1 p2))
      (setq orglink (org-make-link-string url desc))
      (switch-to-buffer "ts-bookmarks.org")
      (goto-char (point-max))
      (insert (format "** %s" orglink))
      (newline)
      (save-buffer)
      (switch-to-buffer "*eww*")
      (message "copy link:%s" desc)
      )))

(with-eval-after-load 'eww
  (bind-key eww-mode-map "<f10>" 'copy-url-to-bookmarks))










;; 根据自定义属性分类显示
;; 分类显示时，不能使用带冒号的时间
(defun eye-agenda-by-area ()
  (interactive)
  (setq org-super-agenda-groups
	 '((:auto-property "Area")))
  (org-agenda-list))

;; copy from library, change to show Project:
(org-super-agenda--def-auto-group category "their org-category property"
  :key-form (org-super-agenda--when-with-marker-buffer (org-super-agenda--get-marker item)
              (org-get-category))
  :header-form (concat "Project: " key))

(defun eye-agenda-by-proj ()
  "默认分类就是文件名，每个文件属于一个项目"
  (interactive)
  (let ((org-agenda-span 'day)
        (org-super-agenda-groups
	     '((:auto-category t))) ;; 用其它自定义属性分类 (:auto-property "OKR")
        )
    (org-agenda-list)))


(defun eye-agenda-view-proj-ts ()
  "查找项目todo项，:tag中不能用传参的方式"
  (interactive)
  (let ((org-super-agenda-groups
	     '(
           (:tag "ts")
           (:discard (:anything t))
           )))
    (org-agenda-list)
    ))



(defun eye-agenda-by-goals ()
  (interactive)
  (setq org-agenda-span 'day)
  (setq org-super-agenda-groups
	'((:auto-property "Goals")
	  ;; don't show other items
	  (:discard (:anything t))))
    (org-agenda-list))


(defun eye/toggle-agenda-tag-display ()
  "在agenda中切换显示tag"
  (interactive)
  (when (equal major-mode 'org-agenda-mode)
    (if (string-equal "." org-agenda-hide-tags-regexp)
        (setq org-agenda-hide-tags-regexp nil)
      (setq org-agenda-hide-tags-regexp ".")
      )
    (org-agenda-redo-all)
    ))


;;(setq-default org-columns-default-format "%80ITEM %50TODO %3PRIORITY %TAGS")
;;(setq org-agenda-overriding-columns-format "%TODO %7EFFORT %PRIORITY %100ITEM 100%TAGS")


;;;; gtd workflow
;; 使用tags而不是tags-todo，将显示所有inbox标签下的items
;; (add-to-list 'org-agenda-custom-commands '("gi" "View Inbox" tags "inbox"
;; 					                       ((org-agenda-overriding-header "Inbox"))))

;;;; other search command
;; tags-todo显示同时满足设置了todo和tag的items
;; 使用 | 可以包含多个tag
;; priority-down把优先级高的显示在前面

;; 查看project
;; (add-to-list 'org-agenda-custom-commands '("p" . "View project todolist"))

;; (add-to-list 'org-agenda-custom-commands '("ps" "ts" tags-todo "ts"
;; 					   ((org-agenda-overriding-header "TS") ;;用于显示的字符串
;; 					    (org-agenda-sorting-strategy '(priority-down))
;; 					    ;;(org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first) ;; 只显示一个
;; 					   )))

;;(add-to-list 'org-agenda-custom-commands '("pm" "Memory" tags-todo "proj+memory" ;; 搜索同时满足多个tag
;;					   ((org-agenda-overriding-header "Memory")))) ;;用于显示的字符串


;; (add-to-list 'org-agenda-custom-commands '("w" . "View by date range"))

;; (add-to-list 'org-agenda-custom-commands '("w7" "Next year" tags "@next_year"
;; 					   ((org-agenda-overriding-header "Next year")
;; 					    (org-agenda-sorting-strategy '(priority-down)))))

;; (add-to-list 'org-agenda-custom-commands '("w6" "This year" tags "@this_year"
;; 					   ((org-agenda-overriding-header "This year")
;; 					    (org-agenda-sorting-strategy '(priority-down)))))

;; (add-to-list 'org-agenda-custom-commands '("w5" "Next month" tags "@next_month"
;; 					   ((org-agenda-overriding-header "Next month")
;; 					    (org-agenda-sorting-strategy '(priority-down)))))

;; (add-to-list 'org-agenda-custom-commands '("w4" "This month" tags "@this_month|@this_week|@today"
;; 					   ((org-agenda-overriding-header "This month")
;; 					    (org-agenda-sorting-strategy '(priority-down)))))

;; (add-to-list 'org-agenda-custom-commands '("w3" "Next week" tags "@next_week"
;; 					   ((org-agenda-overriding-header "Next week")
;; 					    (org-agenda-sorting-strategy '(priority-down)))))

;; (add-to-list 'org-agenda-custom-commands '("w2" "This week" tags "@this_week\|@today"
;; 					   ((org-agenda-overriding-header "This week")
;; 					    (org-agenda-sorting-strategy '(priority-down)))))

;; (add-to-list 'org-agenda-custom-commands '("w1" "Today" tags "@today"
;; 					                       ((org-agenda-overriding-header "Today")
;;                                             ;;(org-agenda-start-with-log-mode '(closed))
;; 					                        (org-agenda-sorting-strategy '(priority-down)))))

;; (add-to-list 'org-agenda-custom-commands '("wr" "Repeat" tags "@repeat|repeat"
;; 					   ((org-agenda-overriding-header "Repeat")
;; 					    (org-agenda-sorting-strategy '(priority-down)))))


;; (setq org-todo-keywords
;;       '((sequence "TODO(t)" "ONGOING(o)" "MAYBE(m)" "WAIT(w)" "DELAYED(d)" "|"
;;                   "DONE(f)" "CANCELLED(c)" "STUCK(s)"
;;                   )
;;         (sequence "TASK(T)" "EVET(E)" "NOTE(N)" "⚓(D)")
;;         ))


;; (setf org-todo-keyword-faces
;;       '(("TODO" . (:foreground "VioletRed" :bold t :weight bold))
;;         ("ONGOING" . (:foreground "IndianRed3" :bold t :weight bold))
;; 	("MAYBE" . (:foreground "grey26" :bold t :weight bold))
;; 	("WAIT" . (:foreground "chartreuse4" :bold t :weight bold))
;;         ("DELAYED" . (:foreground "gray30" :bold t :weight bold))
;; 	("DONE" . (:foreground "medium sea green" :bold t :weight bold))
;; 	("CANCELLED" . (:foreground "gray50" :bold t :weight bold))
;; 	("STUCK" . (:foreground "firebrick" :bold t :weight bold))
;; 	))


;; ;; 图标设置
;; (setq org-agenda-category-icon-alist '(
;;                                        ("cat" "x:/book-solid.png" nil nil :ascent center :mask heuristic)
;;                                        ("xx" "x:/cogs-solid.png" nil nil :ascent center :mask heuristic)
;;                                        ("sss" "x:/car-solid.png" nil nil :ascent center :mask heuristic)
;;                                        ("fff" "x:/raspberry-pi-brands.png" nil nil :ascent center :mask heuristic)
;;                                        ))

;;;; agenda commands
(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
         (
          ;; top pri task
          (tags "PRIORITY=\"A\""
                ((org-agenda-overriding-header "----------------------------------------------------------------------\n⮞⮞⮞⮞⮞⮞ ⚡ High-priority unfinished tasks:\n----------------------------------------------------------------------")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done)) ;; 跳过已完成
                 ))

          ;; daily, and old scheduled task will show here
          ;; deadline项在org-deadline-warning-days 天会提前显示在此
          (agenda "" ((org-agenda-overriding-header "----------------------------------------------------------------------\n⮞⮞⮞⮞⮞⮞ ⏰ Today tasks:\n----------------------------------------------------------------------") ;; U+23f0
                      (org-agenda-use-time-grid nil) ;; 不显示time grid
                      (org-agenda-span 1) ;; 显示几天task
                      ))

          ;; all tasks
          (alltodo "" ((org-agenda-overriding-header "----------------------------------------------------------------------\n⮞⮞⮞⮞⮞⮞ ☐ Other tasks(no scheduled, no deadline):\n----------------------------------------------------------------------")
                       (org-agenda-skip-function
                        '(or
                          (air-org-skip-subtree-if-habit) ;; 跳过STYLE 为habit的
                          (air-org-skip-subtree-if-priority ?A) ;; 跳过A级task
                          (org-agenda-skip-if nil '(scheduled deadline)) ;; 有deadline或有设置时间的不再显示
                          ))))

          ;; show done tasks, org use org-todo-list, 8 r to show
          (agenda ""
                  ((org-agenda-overriding-header "----------------------------------------------------------------------\n⮞⮞⮞⮞⮞⮞ ☑ Done tasks:\n----------------------------------------------------------------------")
                   (org-agenda-start-day "-14d")
                   (org-agenda-span 21) ;; 3 weeks day
                   (org-agenda-start-on-weekday 1)
                   (org-agenda-start-with-log-mode '(closed))
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "^\\*\\* DONE "))))


          )
         ((org-agenda-compact-blocks nil)) ;; 默认nil时显示===分割线，t时不显示分割线，分割线可用 org-agenda-block-separator 设置
         ("~/gtdweb/tasks.html")
         )
        ))

(add-to-list 'org-agenda-custom-commands '("b" "Memory" tags-todo "proj+memory" ;; 搜索同时满足多个tag
					                       ((org-agenda-overriding-header "Memory"))  ;;用于显示的字符串
                                           ("~/gtdweb/proj-memory.html")
                                           ))

(add-to-list 'org-agenda-custom-commands '("d" "Project lists"
                                           (
                                            (tags-todo "proj" ;; 搜索同时满足多个tag
                                                       ((org-agenda-overriding-header "Memory")
                                                        (eye--org-agenda-skip-not-this-category "Memory")
                                                        )  ;;用于显示的字符串
                                                       )
                                            (tags-todo "proj" ;; 搜索同时满足多个tag
                                                       ((org-agenda-overriding-header "学车")
                                                        (eye--org-agenda-skip-not-this-category "学车")
                                                        )  ;;用于显示的字符串
                                                       )

                                            )))


(defun eye/agenda-simple-view ()
  (interactive)
  (org-agenda nil "c")
  )

(defun eye/agenda-show-dones ()
  (interactive)
  (org-todo-list 8))



;;;; agenda-commands
(defun air-org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))


(defun eye--org-agenda-skip-not-this-category (cat)
  (let ((cur-cat (format "%s" (org-get-category (point)))))
    (if (and cur-cat (string-equal cur-cat cat))
        nil ;; 相同，则不跳过
      t ;; 不等于，跳过
    )
  ))

;; 排除只有时间戳但没有TODO标记的
(defun eye--org-agenda-skip-only-timestamp-entries ()
  (org-agenda-skip-entry-if 'nottodo 'any))

;; Skip entries which are not deadlines.
(defun eye--org-agenda-skip-not-deadline-entries ()
  (org-agenda-skip-entry-if 'notdeadline))

 ;; Skip entries which are not finished. for get done tasks
(defun eye--org-agenda-skip-unfinished-entries ()
  (org-agenda-skip-entry-if 'nottodo 'done) ;; 要设置了Scheduled时间并完成的项才会显示出来
  ;; (org-agenda-skip-entry-if 'nottodo '("TODO")) ;; same as prev line
  )

;; Skip unscheduled entries. for inbox
(defun eye--org-agenda-skip-scheduled-entries ()
  (org-agenda-skip-entry-if 'timestamp
                            'todo '("ONGOING" "WAIT" "DELAYED")
                            'regexp ":TG:"))

(setq  org-agenda-use-time-grid nil) ;; 设置为nil将不再显示没有任务的时间段



;; 执行C-u C-c C-w，可以跳转到其它条目

;; (add-to-list 'org-agenda-custom-commands
;;              '("dm" "Month done task"
;;                agenda ""
;;                ((org-agenda-start-day "-14d")
;;                 (org-agenda-span 30)
;;                 (org-agenda-overriding-header "最近已完成")
;;                 ;;(org-agenda-span 'month)
;;                 (org-agenda-archives-mode t) ;; 包含archieved
;;                 (org-agenda-start-on-weekday 1)
;;                 (org-agenda-start-with-log-mode '(closed))
;;                 ;; 过滤 xxx DONE some thing
;;                 (org-agenda-skip-function '(org-agenda-skip-entry-if
;;                                             'notregexp "^\\*\\* DONE ")))))

;; 只显示一个任务
;; (defun my-org-agenda-skip-all-siblings-but-first ()
;;   "Skip all but the first non-done entry."
;;   (let (should-skip-entry)
;;     (unless (org-current-is-todo)
;;       (setq should-skip-entry t))
;;     (save-excursion
;;       (while (and (not should-skip-entry) (org-goto-sibling t))
;;         (when (org-current-is-todo)
;;           (setq should-skip-entry t))))
;;     (when should-skip-entry
;;       (or (outline-next-heading)
;;           (goto-char (point-max))))))

;; (defun org-current-is-todo ()
;;   (string= "TODO" (org-get-todo-state)))

;;;;

;; 自动打开calendar
;;(advice-add 'org-agenda :after
;;	    (lambda (_)
;;	      (when (equal (buffer-name)
;;			   "*Org Agenda*")
;;		(calendar)
;;		(other-window 1))))
;;
;;;; 自动退出calendar
;;(advice-add 'org-agenda-quit :before
;;		    (lambda ()
;;		      (let ((window (get-buffer-window calendar-buffer)))
;;			(when (and window (not (one-window-p window)))
;;			                    (delete-window window)))))



;; @See https://github.com/alphapapa/org-super-agenda/blob/master/examples.org
;; (add-to-list 'org-agenda-custom-commands
;; 	         '("ww" "Super view"
;; 	           ((alltodo
;;                  "" ((org-agenda-overriding-header "")
;; 		             (org-super-agenda-groups
;; 		              '((:name "Repeat" :tag "@repeat")
;;                         (:name "今天" :tag "@today")
;; 			            (:name "本周" :tag "@this_week")
;; 			            (:name "下周" :tag "@next_week")
;; 			            (:name "这个月" :tag "@this_month")
;; 			            (:name "下个月" :tag "@next_month")
;; 			            (:name "今年" :tag "@this_year")
;; 			            (:name "明年" :tag "@next_year")
;; 			            )))))))




;; # notes-list
;; git clone https://github.com/rougier/notes-list.git
;; git clone git://git.smrk.net/stripes.el stripes
;; git clone https://github.com/rougier/svg-lib.git
;; git clone https://github.com/rougier/svg-tag-mode.git
;; git clone https://github.com/rougier/nano-theme.git
;; # notes-list end

(eye/use-package
 'notes-list
 :ensure t
 :load-path '("notes-list" "stripes" "svg-lib" "svg-tag-mode" "nano-theme")
 :config
 (progn
(setq notes-list-directories '("x:/notes-list-test"))
(setq notes-list-display-icons nil)
))


(eye/use-package
'ef-themes
:ensure t
:load-path "ef-themes"
:config
(progn

)
)


;;;; end
