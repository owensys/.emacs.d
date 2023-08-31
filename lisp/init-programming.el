(setq python-indent-offset 4)

;;;; etags
(defun eye/create-ctags-file ()
  "Create ctags file(windows ok)"
  (interactive)
  ;; ctags必须加-e选项，否则counsel-xxx-find-tag-xx无法识别其中的tagname
  (let ((tags-dir (ido-read-directory-name "TAGS DIR:"))
	;; 需要传"\\("，否则出现错误：bash: -c:行0: 未预期的符号 `(' 附近有语法错误
	(command "find %s \\( -iwholename \"*.h\" -or -iwholename \"*.cpp\" \\) -print | ctags -e -f %sTAGS -V -R -L -"))
    (setq command (format command tags-dir tags-dir))
    (message command)
    (let ((proc (start-process "ctags" nil shell-file-name shell-command-switch command)))  ;; shell-command-switch值为-c，表示后面的是命令行参数
      (set-process-sentinel proc `(lambda (proc msg)
				    (let ((status (process-status proc)))
				      (when (memq status '(exit signal))
					(message "ctags:%s" msg)
					)))))
    ;;    (async-shell-command command)
    ))

(defun eye/create-ctags-file-by-git ()
  "在项目目录下执行此命令，即可生成TAGS文件，如果弹出提示时，需要选yes
也可以手动执行命令：ctags.exe -R -f TAGS --output-format=etags .
"
  (interactive)
  (async-shell-command-no-window "git ls-tree -r HEAD --name-only > d:/list_files")
  (with-temp-buffer
    (insert "--output-format=etags\r\n")
    (insert "-f TAGS\r\n")
    (insert "-L d:/list_files\r\n")
    (write-file "d:/opt.ctags"))
  (async-shell-command-no-window (concat (executable-find "ctags") " --options=d:/opt.ctags")))


(defun eye/create-ctags-file-by-cmd (&optional tags-dir)
  "在选择目录下执行命令：ctags.exe -R -f TAGS --output-format=etags ."
  (interactive)
  (let* ((dir (ido-read-directory-name "Create TAGS in dir: "))
         (default-directory dir)
         )
    (async-shell-command-no-window "ctags -R -f TAGS --output-format=etags .")
    ))


;; 必须带一个参数，由counsel-etags调用，传入的是tags所在目录
(defun eye/update-ctags-this-file (&optional tags-dir)
  "Update current file tags"
  (interactive)
  (let ((tags-path (locate-dominating-file default-directory "TAGS"))
	(command)
	(proc))
    (when tags-path
      (setq tags-path (expand-file-name "TAGS" tags-path))
      (setq command (format "ctags -e -a -f %s %s" tags-path (buffer-file-name))) ;; -a means append
      (message (concat "custom command:" command))
      (async-shell-command-no-window command)
      )))

;; company-etags要么使用当前项目的TAGS，要么使用tags-table-list定义的TAGS文件，所以干脆直接配置tags-table-list
;;(if locale-system-tags-paths
;;    (append-to-list 'tags-table-list locale-system-tags-paths)) ;; need load init-locale
;; Don't ask before rereading the TAGS files if they have changed
(setq tags-revert-without-query t)
;; Do case-sensitive tag searches
(setq tags-case-fold-search nil) ;; t=case-insensitive, nil=case-sensitive
;; Don't warn when TAGS files are large
(setq large-file-warning-threshold nil)

(setq counsel-etags-update-interval 10)

(defun eye/load-project-root-tags ()
  "加载本地文件上层目录中对应的TAGS文件"
  (interactive)
  (let ((root-tags-file (locate-dominating-file default-directory "TAGS")))
    (when root-tags-file
      (setq root-tags-file (concat root-tags-file "TAGS"))
      (message "Loading tags file: %s" root-tags-file)
      (visit-tags-table root-tags-file)
      (add-to-list 'tags-table-list root-tags-file)
      (add-to-list 'tags-table-files root-tags-file) ;; for find-file-in-tags
      )))



;;;; counsel-etags
(eye/use-package
 'counsel-etags
 :load-path "counsel-etags"
 :command '(counsel-etags-find-tag counsel-etags-find-tag-at-point)
 :config
 (progn
   ;; Don't ask before rereading the TAGS files if they have changed
   (setq tags-revert-without-query t)
   ;; Don't warn when TAGS files are large
   (setq large-file-warning-threshold nil)
   (when is-linux
     (setq counsel-etags-tags-program "xargs etags --append") ;调用命令类似 find .... -print | xargs etags --append, etags没有递归的参数
     )
   (when is-windows (setq counsel-etags-tags-program (executable-find "ctags"))) ;; if not set, (counsel-etags-guess-program "ctags") find failed

   ;; 是否开启输出命令
   (setq counsel-etags-debug nil)

   ;;(append-to-list 'counsel-etags-extra-tags-files locale-system-tags-paths) ;;使counsel-etags能显示系统函数（但无法跳转进入）
   
   ;; Setup auto update now
   ;; (setq counsel-etags-update-tags-backend 'eye/update-ctags-this-file)
   ;; (setq counsel-etags-update-tags-backend 'eye/create-ctags-file-by-cmd)
   (setq counsel-etags-update-tags-backend nil) ;; 设置为空，避免总是提示创建tags文件
   ;; (add-hook 'c++-mode-hook
   ;;       (lambda ()
   ;;         (add-hook 'after-save-hook 'counsel-etags-virtual-update-tags 'append 'local)
   ;;         ;;(add-hook 'after-save-hook 'eye/update-ctags-this-file))
   ;;         ))

   ;; counsel-etags-ignore-directories does NOT support wildcast
   (dolist (dirname (list ".git" ".svn" ".vs" "ipch" "Debug" "Release" "Bin" "tmp"))
     (add-to-list 'counsel-etags-ignore-directories dirname))

   ;; counsel-etags-ignore-filenames supports wildcast
   (dolist (filename (list "TAGS" "GPATH" "GTAGS" "*.json" "ui_*.h" "*.ui" "moc_*.cpp" "*.rc"
			   "*.qrc" "*.tlog" "*.md" "*.bat" "*.txt" "*.pdb" "*.filters" "*.user"
			   "*.vcproj" "*.vcxproj" "*.db" "*.opendb" "*.htm" "*.user" "*.make"
			   "*.sln" "*.exp" "*.sdf" "*.opensdf"))
     (add-to-list 'counsel-etags-ignore-filenames filename))
   ))


;;;; company
;;; 对不同的major-mode设置不同的company-backends
;; 1.设置(set 'company-backend xxx)切换buffer后又会变为空，即使设置了也不能自动弹出TAGS选项，所以改为使用
;; 2.设置after-change-major-mode-hook后，切换buffer时会得到major-mode为minibuffer-inactive-mode，不能得到切换后buffer的major-mode
;; 3.add-hook majoe-mode hook也不能检测到切换buffer后的major-mode，要重新打开buffer才能调用到hook
;; 4.add company-mode-hook 修改company-backends, add major-mode hook开启company-mode（非global），问题：也不能检测到切换buffer后的major-mode
;; 5.add global-company-mode-hook根据major-mode修改company-backends，切换buffer后，重新开启global-company-mode
;; 6.最终选择：手动开启global-company-mode，根据major-mode修改company-backends，当切换buffer后，重新开启global-company-mode，效果同第5条
;; 切换buffer时，不能自动修改backends，要手动关开global company mode，很烦，可以通过使用local 变量


;; function to push company backend to local variable - company-backends.
;; 2021.12.1: 经常要重新加载TAGS
;; (defun eye--push-local-company-backend (backend)
;;   "Add BACKEND to a buffer-local version of 'company-backends'."
;;   (make-local-variable 'company-backends)
;;   (if (not (member backend company-backends))
;;       (push backend company-backends)))


(defun buffer-mode (&optional buffer-or-name)
  "Returns the major mode associated with a buffer.
If buffer-or-name is nil return current buffer's mode."
  (buffer-local-value 'major-mode

		      (if buffer-or-name (get-buffer buffer-or-name) (current-buffer))))

;; (defun eye-set-major-mode-backends (backends modmap)
;;   "设置company-backends并设置相应major mode map下的按键，由于define-key第一个参数不能是symbol，需要在hook函数中使用"
;;   (message "Change company-backends to: %s" backends)
;;   (setq company-backends backends)
;;   ;;(set (make-local-variable 'company-backends) backends) ;;不能使用setq设置local variable 'company-backend
;;   (when (keymapp modmap)
;;     (define-key modmap (kbd "<backtab>") 'company-manual-begin)
;;     (define-key modmap (kbd "<S-tab>") 'company-manual-begin)))

;; (defun eye-setup-company-backends ()
;;   "Setup company-backends for major-mode"
;;   (when (or (bound-and-true-p global-company-mode) (bound-and-true-p company-mode))

;;     ))


;; (defun eye/company-remove-dups (candidates)
;;   "自动移除company列表的中重复元素"
;;   (let ((newseq))
;;     (mapcar #'(lambda (c)
;; 		(if (not (member c newseq))
;; 		    (add-to-list 'newseq c)))
;; 	    candidates)
;;     newseq))


;; (eye/use-package
;;  'company
;;  :ensure t
;;  :urls '(("company-mode" . "https://github.com/company-mode/company-mode.git"))
;;  :load-path "company-mode"
;;  :config
;;  (progn

;;    (define-key company-active-map (kbd "M-/") #'company-complete)
;;    (define-key global-map (kbd "M-/") #'company-complete)


;;    (setq company-idle-delay
;;          (lambda () (if (company-in-string-or-comment) nil 0.3)))

;;    (setq company-idle-delay 0.1
;;          company-tooltip-idle-delay 0.1 ;; 候选列表出现时间
;;          company-selection-wrap-around t ;; 循环previous/next
;;          company-require-match nil ;; 出现提示框后，输入是否要求匹配候选项，如果设置为t，对快速输入有影响
;;          company-tooltip-limit 20 ;; 候选项数量
;;          company-show-numbers t ;; 在候选项后面显示数字，可用alt+数字快速选择
;;          company-minimum-prefix-length 2 ;; 输入几个字符后使用company提示
;;          company-frontends
;;          '(company-pseudo-tooltip-unless-just-one-frontend-with-delay
;;            company-preview-frontend
;;            company-echo-metadata-frontend))

;;    (setq company-tooltip-align-annotations nil)

;;    (setq company-backends '(company-semantic company-cmake company-capf
;;                                          ;; company-clang
;;                                          ;; company-files
;;                                          (company-dabbrev-code company-gtags company-etags company-keywords)
;;                                          company-oddmuse company-dabbrev))

;;    (global-company-mode 1)


;;    ))


;; 仅对编辑过的行做行尾空白删除：https://emacs-china.org/t/topic/15870
;; 很有用的package.
(eye/use-package 'ws-butler
                 :ensure t
                 :load-path "ws-butler"
                 :config
                 (progn                   
                   (add-hook 'prog-mode-hook #'ws-butler-mode)
                   ))


(defun setup-java-mode()
  (setq tab-width 4)
  )

(add-hook 'java-mode-hook
          'setup-java-mode);


;; 高亮显示tab
(defface extra-whitespace-face
   '((t (:background "dark green")))
   "Used for tabs and such.")

(defvar my-extra-keywords
   '(("\t" . 'extra-whitespace-face)))

(defun eye-show-tabs ()
  (interactive)
  (font-lock-add-keywords nil my-extra-keywords))

(defun eye-hide-tabs ()
  (interactive)
  (font-lock-remove-keywords nil my-extra-keywords))




;; ;;;; yasnippet
;; (eye/use-package 'yasnippet
;;                  :urls '(("yasnippet" . "https://github.com/joaotavora/yasnippet.git"))
;;                  :load-path "yasnippet"
;;                  :command '(yas-reload-all yas-minor-mode yas-global-mode)
;;                  :init
;;                  (progn
;;                    ;; non global mode usage
;;                    (yas-reload-all)
;;                    (add-hook 'prog-mode-hook #'yas-minor-mode)

;;                    ;; global usage
;;                    ;; (yas-global-mode 1)

;;                    ;; yas-snippet-dirs is snippet directory
;;                    )
;;                  )

;; (eye/use-package 'ivy-yasnippet
;;                  :urls '(("ivy-yasnippet" . "https://github.com/mkcms/ivy-yasnippet.git"))
;;                  :load-path "ivy-yasnippet"
;;                  :command '(ivy-yasnippet)
;;                  :init
;;                  (progn
;;                    ;; (eval-after-load 'yasnippet
;;                    ;;   (require 'ivy-yasnippet)
;;                    ;;   )
;;                    )
;;                  )

;; (eye/use-package 'yasnippet-snippets
;;                  :urls '(("yasnippet-snippets" . "https://github.com/AndreaCrotti/yasnippet-snippets"))
;;                  :load-path "yasnippet-snippets"
;;                  :ensure t
;;                  )



;; ;;;; json
;; (eye/use-package 'json-mode
;;                  :ensure t
;;                  :load-path '("json-snatcher" "json-mode")
;;                  :command '(json-mode)
;;                  )


(require 'init-rst)

;;;; go
(eye/use-package 'go-mode
                 :ensure t
                 :load-path '("go-mode")
                 :config
                 (progn
                   (add-hook 'go-mode-hook
                             (lambda ()
                               (setq-default)
                               (setq tab-width 4)
                               (setq standard-indent 4)
                               (setq indent-tabs-mode nil)))
                   )
                 )


(provide 'init-programming)
