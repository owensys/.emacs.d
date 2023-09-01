

;; 奇怪问题：在 emacs 中使用 mingw32-make 编译时总是报错无法找到引用，链接出错。
;; 但是在命令行下却又能成功编译。
;; 所以不直接调用 mingw32-make，而是调用 build.bat 批处理文件来进行编译。
(setq build-script (if is-windows "mybuild.bat" "mybuild.sh")) ;; 编译命令

(require 'compile)
(with-eval-after-load 'compile
  (setq compilation-directory-locked nil)
  ;; Compilation
  (setq compilation-context-lines 0)

  (setq compilation-scroll-output t) ;;自动滚动
  ;; 编译时有错误，则自动跳转到第一个错误，设置为nil，避免rg搜索出现报错
  (setq compilation-auto-jump-to-first-error nil)
  (setq compilation-always-kill t) ;;执行编译时，如果有前一个编译命令正在执行，自动kill，不询问
  ;; 使next-error跳过warning @see https://emacs-china.org/t/compilation-mode-next-error-warning/9095/10
  ;; 或者使用pcre2el包使用pcre的正则语法来匹配错误
  (setq compilation-skip-threshold 2)
  (if (bound-and-true-p rxt-pcre-to-elisp) ;; pcre2el,https://github.com/joddie/pcre2el
      (progn
	(add-to-list 'compilation-error-regexp-alist 'fixed-msvc)
	(add-to-list 'compilation-error-regexp-alist-alist
		     `(fixed-msvc
		       ,(rxt-pcre-to-elisp (concat
					    "^\\s*(?:\\d+>\\s*)?"  ; for msbuild, it will add "\d+>" on each line
					    "("                    ; group 1: hyperlink
					    "((?:\\w:)?[^:\t\n]+?)" ; group 2: file path
					    "(?:\\((\\d+)\\))?"    ; group 3: line number
					    "\\s*:\\s*"
					    "(?:(note)|(warning)|(fatal )?error)(\\s+C\\d+)?" ; group 4: note, group 5: warning
					    "\\s*:"
					    ")"))
		       2 3 nil (5 . 4) 1))
	)
    ;; emacs本身的正则语法
    (setq compilation-error-regexp-alist
	  (cons '("^\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:fatal error\\|warnin\\(g\\)\\) C[0-9]+:" 2 3 nil (4))
		compilation-error-regexp-alist)))


  )

(setq eye-auto-hide-compilation-buffer nil) ;; 编译成功后是否自动隐藏

;; Success or failure of compile
(defun notify-compilation-result(buffer msg)
  "Notify that the compilation is finished.
通过build.bat中msbuild编译，有错误，但msg总是为\"finished\"
"
  ;; 跳转到错误位置时，从哪个目录搜索，由于这个变量是全局的，如果同时在不同项目下工作，可能需要打开多个emacs才可以
  (setq compilation-search-path (list (get-string-from-file (concat eye-build-dir ".build_dir"))))
  (message "set search error from dir:%s" compilation-search-path)

  (with-current-buffer buffer
    ;;0前面增加一个空格，避免匹配到10,20等
    (let ((content (buffer-substring-no-properties (point-min) (point-max))))
      (cond
       ((or (string-match "color-rg finished" content)
            (string-match "^rg finished" content))
        (tooltip-show "\n Search finished! \n "))

       ((string-match " 0 个错误" content)
        (progn
          (tooltip-show "\n Compile Success \n ")
          ;;自动关闭buffer @see https://emacs.stackexchange.com/questions/62/hide-compilation-window
          (if eye-auto-hide-compilation-buffer
              (run-at-time
               "2 sec" nil 'delete-windows-on
               (get-buffer-create "*compilation*")))
          ))

       (t
        (tooltip-show "\n Compile Finished, Check error! \n ")
        )

       )
      )))

(add-to-list 'compilation-finish-functions 'notify-compilation-result)


;; For M-x compile

;; 必须判断一下buffer-file-name是否是nil，否则出现奇怪问题，org导出html时，也会调用c++-mode-hook，但是这个为nil，导致导出不了，并卡顿，错误如下：
;; Debugger entered--Lisp error: (wrong-type-argument stringp nil)
;;   expand-file-name(nil)
;;   locate-dominating-file(nil "build.bat")
;;   (let ((dir (locate-dominating-file buffer-file-name build-script))) (if dir (progn (set (make-local-variable 'compile-command) (expand-file-name build-script dir)))))
;;   build-command()
;;   run-hooks(change-major-mode-after-body-hook prog-mode-hook c-mode-common-hook c++-mode-hook)
;;   apply(run-hooks (change-major-mode-after-body-hook prog-mode-hook c-mode-common-hook c++-mode-hook))
;;   run-mode-hooks(c++-mode-hook)
;;   c++-mode()
;;   org-html-fontify-code("" "C++")

(setq eye-build-dir "") ;; 编译时设置eye-build-dir，编译结束后从这个目录下找.build_dir文件，从里面读取从哪个目录下查找错误文件的目录
(defun build-command ()
  (when buffer-file-name
    (let* ((project-dir (locate-dominating-file buffer-file-name build-script))
          )
      (when project-dir
	    (set (make-local-variable 'compile-command)
	         (expand-file-name build-script project-dir))))))


(defun eye/auto-compile ()
  (interactive)
  (let* ((project-dir (locate-dominating-file buffer-file-name build-script)))
    (when project-dir
      (setq eye-build-dir project-dir)
      (when compile-command
        (compile compile-command))
      )
    )
  )


;; (add-hook 'java-mode-hook #'build-command)
;; (add-hook 'bat-mode-hook #'build-command)

(provide 'init-compile)
