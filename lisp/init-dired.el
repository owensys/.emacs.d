;; https://blog.slegetank.com/blog/20170106-dired.html

(require 'dired)

;; 默认递归操作文件夹
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)

;; 拷贝,如果其他窗口也为dired mode，会以其为目标路径拷贝文件。
(setq dired-dwim-target t)


;; (define-key dired-mode-map (kbd "RET") 'dired-find-file-other-window)
;; (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))

;; 使文件夹能处在文件的前面
(defun custom-dired-sort-dir-first ()
  "Dired sort hook to list directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (and (featurep 'xemacs)
       (fboundp 'dired-insert-set-properties)
       (dired-insert-set-properties (point-min) (point-max)))
  (set-buffer-modified-p nil))



(add-hook 'dired-after-readin-hook 'custom-dired-sort-dir-first)

(defun dired-dotfiles-toggle ()
  "Show/hide dot-files"
  (interactive)
  (when (equal major-mode 'dired-mode)
    (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p) ; if currently showing
	(progn 
	  (set (make-local-variable 'dired-dotfiles-show-p) nil)
	  (message "h")
	  (dired-mark-files-regexp "^\\\.")
	  (dired-do-kill-lines))
      (progn (revert-buffer) ; otherwise just revert to re-show
	     (set (make-local-variable 'dired-dotfiles-show-p) t)))))


;; 隐藏 dired 中文件拥有者和文件权限等信息
(defun eye-dired-mode-setup ()
  "hide the file's unix owner and permission info"
  ;; 分组
  ;; (setopt dired-listing-switches "-alGhv --group-directories-first")
  ;; (dired-hide-details-mode 1)		;隐藏以.开头的文件
  ;; (dired-omit-mode 1)			;隐藏.和..本身 @see https://stackoverflow.com/questions/43628315/how-to-hide-one-dot-current-directory-in-dired-mode
  )


(add-hook 'dired-mode-hook 'eye-dired-mode-setup)


;; 解决dired访问的路径并不会被添加到最近访问里
(progn
  (require 'recentf)
  (defun recentd-track-opened-file ()
    "Insert the name of the directory just opened into the recent list."
    (and (derived-mode-p 'dired-mode) default-directory
         (recentf-add-file default-directory))
    ;; Must return nil because it is run from `write-file-functions'.
    nil)

  (defun recentd-track-closed-file ()
    "Update the recent list when a dired buffer is killed.
That is, remove a non kept dired from the recent list."
    (and (derived-mode-p 'dired-mode) default-directory
         (recentf-remove-if-non-kept default-directory)))

  (add-hook 'dired-after-readin-hook 'recentd-track-opened-file)
  (add-hook 'kill-buffer-hook 'recentd-track-closed-file)
  )



(eye/use-package
 'async
 :ensure t
 :load-path "emacs-async"
 :config
 (progn
   ;; 目录有变化时及时更新
   (setq dired-async-mode 1)
   ))


(require 'wdired)


(require 'dired-x) ;; 支持 dired-jump 进入后自动定位到当前文件名位置
;; 打开 .dired 后缀文件时，自动进入 dired-virtual-mode 模式。
;; (setq auto-mode-alist (cons '("[^/]\\.dired$" . dired-virtual-mode)
			                ;; auto-mode-alist)) ;; dired-x


;; 过滤
(setq-default dired-omit-files-p t)
(setq dired-omit-files
      (concat dired-omit-files "\\.pyc$"))

;; (add-hook 'imenu-after-jump-hook (lambda () (recenter 0)))


;; 使用 windows 程序打开文件
(when is-windows
  (eye/use-package
   'w32-browser
   :ensure t
   :load-path "w32-browser")
  )


(defun eye/dirtree-show ()
  (interactive)
  (if (and (executable-find "tree") (not (file-exists-p "dir.tree")))
      (eye-async-shell-command "tree" "-f" ">" "dir.tree") ;; only linux
      )
  (if (file-exists-p "dir.tree")
      (find-file "dir.tree")
    (message "Please run command to generate dir.tree: tree -f > dir.tree")
    )
  )

(defun eye/dirtree-open-file ()
  (interactive)
  (let ((path (thing-at-point 'filename)))
    (when (file-exists-p path)
      (find-file path)
      )
  ))


(provide 'init-dired)
