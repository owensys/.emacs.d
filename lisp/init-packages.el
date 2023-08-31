;;; init-packages.el --- Custom package manager -*- lexical-binding: t -*-
;;
;; TODO:
;; 1.移除dash, s, f依赖 => ok
;; 2.git clone 增加 --depth=1 => ok
;; 3.把下载包的命令自动添加到shell脚本（或者使用变量保存命令列表）
;; 4.采用异步执行命令 => ok
;; 5.增加删除包的函数（删除目录，删除load-path，删除shell脚本中，更新autoloads）
;; 6.启动时调用安装包（判断本地目录是否存在，不存在则异步安装） => ok

;; depends git wget
(defvar eye-packages-dir (expand-file-name "eye-packages" user-emacs-directory)
  "All packages main directory.
must use expand-file-name, use absolute path")

(defun eye/install-pfuture ()
  (mkdir eye-packages-dir t)
  (let ((default-directory eye-packages-dir)
	(pfuture-path (concat eye-packages-dir "/pfuture")))
    (unless (file-exists-p pfuture-path)
      (call-process "git" nil t nil "clone" "https://github.com/Alexander-Miller/pfuture.git")
      (if (file-exists-p pfuture-path)
	  (add-to-list 'load-path pfuture-path)
	(message "install pfuture failed.")))))

(eye/install-pfuture)

;; 必须加到load-path中，否则autoload函数会找不到，调用出错
(add-to-list 'load-path eye-packages-dir)
(add-to-list 'load-path (concat eye-packages-dir "/pfuture"))
(require 'pfuture)

(setq eye-git-bin (executable-find "git"))
(setq eye-wget-bin (executable-find "wget"))
(setq eye-packages-debug t)
(setq eye-packages-depth-1 t)
(setq eye-packages-http-proxy nil)
;; (setq eye-packages-http-proxy (if is-windows "http://127.0.0.1:10809" ""))


;; autoload file
(setq generated-autoload-file (expand-file-name "autoload.pkg.el" eye-packages-dir))

;;(defun eye-async-shell-command (command func &rest args)
;;  "Execute string COMMAND asynchronously."
;;  (let* ((proc (start-file-process "Shell" nil shell-file-name shell-command-switch command)))
;;    (set-process-sentinel
;;     proc
;;     `(lambda (process signal)
;;        (let* ((status (process-status process)))
;;          (when (memq status '(exit signal))
;;            (cond
;;             ((string= (substring signal 0 -1) "finished")
;;              (let* ((cmd (car (cdr (cdr (process-command process))))))
;;                (if eye-packages-debug (message "`%s` executed." cmd))
;;		(apply func args)))
;;             (t
;;              (message "Failed to execute command. Error=%s CLI=%s"
;;                       signal
;;                       ,command)))))))))
;;

(mkdir eye-packages-dir t)

;;(let ((default-directory eye-packages-dir))
;;  (eye-async-shell-command
;;   (concat "git clone https://github.com/Alexander-Miller/pfuture.git pfuture")))

(defun eye-async-shell-command (&rest args)
  (let ((future1 (apply #'pfuture-new args)))
    (pfuture-await future1)

    ;; (message "Future stdout: [%s]" (pfuture-result future1))
    ;; (message "Future stderr: [%s]" (pfuture-stderr future1))
    (pfuture-result future1)
    ))

;;1.download: git clone / mkdir xx && wget -Oxx/xx.el url
(defun eye-package-download(name url)
  "Supports github and emacswiki.
example:
(eye-package-download  \"pfuture\" \"https://github.com/Alexander-Miller/pfuture.git\")
(eye-package-download \"anything\" \"http://www.emacswiki.org/emacs/download/anything.el\")
"
  (let ((default-directory eye-packages-dir) command)
    (if (string-match "github" url)
	(progn
	  (when (not (file-exists-p name))
	    (when eye-packages-http-proxy
	      (eye-async-shell-command "git" "config" "--global" "http.proxy" eye-packages-http-proxy)

	      (eye-async-shell-command "git" "config" "--global" "https.proxy" eye-packages-http-proxy)
	      )
		(eye-async-shell-command "git" "clone" url name)
		))
      (progn
	(when (not (file-exists-p (concat name "/" name ".el")))
	  (mkdir name t)
	  (when eye-packages-http-proxy
	    (setenv "http_proxy" eye-packages-http-proxy)
	    (setenv "https_proxy" eye-packages-http-proxy))
	  (eye-async-shell-command "wget" (concat "-O" name "/" name ".el") url)
	  )))))

(defun eye/package-install (&optional name site-url)
  (interactive)
  (let ((url site-url) (path (expand-file-name name eye-packages-dir)) command)
    (unless url (setq url (read-string "url:")))
    (unless name (setq name (read-string "package name:")))
    (setq path (concat eye-packages-dir "/" name))
    (if (not (file-exists-p path))
        (progn
          (eye-package-download name url)
          (if (file-exists-p path)
	          (progn
	            (add-to-list 'load-path path)
	            ;;(message "compile")
	            ;; compile some test el file will be error
	            ;;(byte-recompile-directory path 0)
	            (message "generate autoloads: %s" path)
	            (update-directory-autoloads path)
	            (message "package dir add to load-path."))
	        (message "download package failed"))
          )
      (progn
        (message "package dir already exists.")
        (add-to-list 'load-path path)
        )
      )))

;; must set generated-autoload-file
;;(update-directory-autoloads "~/.emacs.d/eye-packages/diminish")

(defun eye-install-packages (pkg-list)
  "install with custom git command"
  (dolist (pair pkg-list)
    (let ((name (car pair))
	  (url (cdr pair)))
      (eye/package-install name url))))

;; (defun eye-install-packages (pkg-list)
;;   "install with el-get"
;;   (dolist (pair pkg-list)
;;     (let ((name (car pair))
;; 	  (url (cdr pair)))
;;       (message "install %s from %s" name url)
;;       (el-get-bundle name :url url) ;; el-get-bundle-guess-type: Wrong type argument: stringp, url
;;       )))


(defun add-package-path (dirlist)
  (cond ((stringp dirlist)
	 (add-to-list 'load-path (concat eye-packages-dir "/" dirlist)))
	((listp dirlist)
	 (dolist (dir dirlist)
	   (add-to-list 'load-path (concat eye-packages-dir "/" dir))))
	(t (error "Wrong arg for add-package-path"))))


(defun add-autoload (functions file)
  (let ((fv (if (stringp file)
		file (car file))))
    (if (listp functions)  ;; functions是列表时
	(dolist (fun functions)
	  (if (listp fun)   ;; 支持 '((helm-find-files . "helm-find")) 的形式
	      (autoload (car fun) (cdr fun) "" t)
	    (autoload fun fv "" t)))  ;; functions是列表，且列表中直接是函数时，如 '(foo bar)
      (autoload functions fv "" t))))  ;; functions直接是 'foo 的形式



(defun eye-packages-list ()
  (let* ((dirs (directory-files eye-packages-dir))
         nofile-dirs
         )
    (dolist (d dirs)
      (if (and (not (string-equal "." d))
               (not (string-equal ".." d))
               (f-directory-p (concat eye-packages-dir "/" d))
               )
          (add-to-list 'nofile-dirs d)
        ))
    nofile-dirs
    ))

(defun eye/package-remove ()
  (interactive)
  (let* ((package (ivy-read "Delete package: " (eye-packages-list))))
    (eye/package-delete package)
    )
  )

(cl-defmacro eye/use-package (feature &key ensure reqby load-path command init config)
  `(progn
     (when ,load-path
       (add-package-path ,load-path))
     ,init
     (if ,command (add-autoload ,command ,load-path))
     (if ,ensure (require ,feature nil 'noerror))
     (with-eval-after-load ,feature
       ,config)
     ))

;;(eye-install-packages
;; '(("pfuture" . "https://github.com/Alexander-Miller/pfuture.git")
;;   ("diminish" . "https://github.com/myrjola/diminish.el.git")
;;   ("anything" . "http://www.emacswiki.org/emacs/download/anything.el")))


;;(eye/package-install "diminish" "https://github.com/myrjola/diminish.el.git")
;; (eye/package-install "http://www.emacswiki.org/emacs/download/anything.el" "anything")
;; (eye/package-install "http://www.emacswiki.org/emacs/download/anything-config.el" "anything-config")


;;2.byte compile directory
;;(byte-recompile-directory "~/packages/youdao-dictionary" nil)
;;(byte-recompile-directory "~/packages/writeroom-mode" nil)
;;(byte-recompile-directory "~/packages/writeroom-mode" nil t) ;force byte compile if existing .elc file
(defun eye/package-compile-all ()
  "编译所有包，如果有很多包没有编译过，由于耗时且卡界面，最好使用命令行方式编译
git-bash进入包目录后执行
/path/of/emacs -q --load ~/.emacs.d/lisp/init-packages.el --batch --eval '(eye/package-compile-all)'
"
  (interactive)
  (byte-recompile-directory eye-packages-dir 0)) ;must use 0 to compile

(defun eye/package-compile-dir ()
  "Compile a directory"
  (interactive)
  (let ((path (ido-read-directory-name "Select dir:")))
    (when (and path (file-exists-p path))
      (message "Compile %s" path)
      (byte-recompile-directory path 0))))

;;3.generate autoload file
;;(update-directory-autoloads "~/packages/youdao-dictionary")
;;(update-directory-autoloads "~/packages/writeroom-mode")
(defun eye/package-update-autoload ()
  (interactive)
  (let ((name (read-string "package name:"))
	path)
    (when name
      (setq path (concat eye-packages-dir "/" name))
      (if (file-exists-p path)
	  (message "Update autoloads: %s" name)
	  (update-directory-autoloads path)))))

;; traverse dir @see http://ergoemacs.org/emacs/elisp_traverse_dir.html
(defun eye/package-update-autoload-all ()
  (interactive)
  ;; delete autoload file
  (if (file-exists-p generated-autoload-file)
      (delete-file generated-autoload-file t))
  ;; update all subdir autoloads of packages directory
  (mapc (lambda (x)
	  (let ((path (expand-file-name x eye-packages-dir)))
	    (when (and (file-exists-p path)
		       (not (string-equal x "."))
		       (not (string-equal x "..")))
	      (update-directory-autoloads path)
	      )))
	(directory-files eye-packages-dir)))


;;4.upgrade package: cd ~/packages/xxx && git pull then force compile
(defun eye/package-force-compile (&optional package-path)
  (interactive)
  (let ((path package-path))
    (unless path (setq path (read-string "package dir name:")))
    (setq path (concat eye-packages-dir "/" path))
    (when (file-exists-p path)
      (byte-recompile-directory path 0 t))))


;;5.remove package: rm -rf ~/packages/xx
(defun eye/package-delete (&optional name)
  (interactive)
  (message "del package:%s" name)
  (let ((package-name (or name (read-string "Delete package name (dir):")))
        (path (concat eye-packages-dir "/" name)))
    (if (file-exists-p path)
	    (progn
	      (delete-directory path t t) ;force delete recursively
	      (message "delete package %s finished." name))
      (message "package %s not exists." name))))


(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))


;;(defvar autoload-file-exist (file-exists-p generated-autoload-file))
;;(defun require-maybe (feature)
;; (unless autoload-file-exist (require feature)))

;;; require autoload.pkg file
;; 1.由于一些包中有eval-when-compile代码，生成的autoload文件中有require其它包，必须先添加到load-path
;; 2.require autoload文件后不需要手动require，可以加快启动，但是有的包函数会自动出现在M-x中
;; 可能是由于autoload信息文件中出现了register-definition-prefixed导致，比如：
;; (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "with-editor/with-editor" '("with-editor" "start-file-process--with-editor-process-filter" "server-" "shell-command--shell-command-with-editor-mode")))
;; (eye--reset-time) ;;使用命令行编译时无法找到此函数定义，先注释
;;(add-subdirs-to-load-path eye-packages-dir) ;; all site packages don't put to .emacs.d
;;(add-subdirs-to-load-path (concat user-emacs-directory "lisp/"))
;; (eye--print-time "add packages path")

;; (eye--reset-time)
;;(setq test-time (current-time))

;; 是不是这里导致首次下载后，require失败？
(when (file-exists-p generated-autoload-file)
  (require 'autoload.pkg generated-autoload-file))

;;(message
;; (format "%.6f sec: require autoload.pkg"
;;	 (- (float-time (current-time))
;;	    (float-time test-time))))

;; (eye--print-time "require autoload.pkg")



(provide 'init-packages)
;;; init-packages.el ends here
