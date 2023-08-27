;;;; configuration.el --- My emacs configuration -*- lexical-binding: t -*-
;;;; debug
;;Produce backtraces when errors occur
(setq debug-on-error t)


(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'eye-startup)
(require 'init-system-path)
(require 'init-font)
(require 'init-misc)
(require 'init-encoding)
(require 'init-utils)

;;;; eye-packages
(setq eye-packages-dir (expand-file-name "emacs-packages" user-emacs-directory))
(require 'eye-packages)


;;;; backup
(auto-require
 'files
 :before
 (progn
   (defvar user-cache-directory "~/tmp/emacs_cache")
   (mkdir "~/tmp/emacs_cache" t)
   (mkdir "~/tmp/emacs_cache/bak" t)
   ;; 备份文件 file~，指定备份目录后，文件名为 !drive_f!dirname!dirname!filename~
   (setq backup-by-copying nil) ;; 需要设置为nil，否则编辑映射的驱动器里的文件保存时会出现ACL问题
   (setq delete-old-versions t)
   (setq kept-new-versions 6)
   (setq kept-old-versions 2)
   (setq version-control t)
   (setq backup-directory-alist '(("." . "~/tmp/emacs_cache/bak")))
   ;; 临时文件 #file#
   (setq auto-save-file-name-transforms '((".*" "~/tmp/emacs_cache/bak" t))) ;; 设置备份文件目录
   ;; (setq auto-save-default t) ;; 是否开启自动备份临时文件，auto-save.el 中会修改这个变量
   ;; (setq auto-save-timeout 5)
   (setq delete-by-moving-to-trash t)	;删除文件或目录时，移到回收站
   ;; display the real names on mode-line when visiting a symbolink
   (setq find-file-visit-truename t)
   ))

;;;; imenu
(auto-require
 'imenu
 :after
 (progn
   (add-to-list 'imenu-generic-expression '("sections" "^;;;; \\(.+\\)$" 1) t)
   (setq imenu-auto-rescan t
	 imenu-auto-rescan-maxout 500000)))


(auto-require
 'bookmark
 :load nil
 :after
 (progn
   ;; 自动保存书签
   (add-hook 'kill-emacs-hook
	     '(lambda ()
		(bookmark-save)))))


;;;; saveplace
(auto-require
 'saveplace
 :load t
 :after
 (progn
   (setq save-place-file "~/tmp/emacs_cache/places")
   (setq save-place-forget-unreadable-files nil)
   (save-place-mode 1)
 ))


;;;; savehist
;; save minibuffer history
(auto-require
 'savehist
 :load t
 :after
 (progn
   (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
	 history-length 100
	 savehist-autosave-interval nil ;;不开启自动保存，否则会不断的分配内存
	 savehist-additional-variables '(mark-ring global-mark-ring search-ring regexp-search-ring extended-command-history))
   ))

;;;; recentf
(auto-require
 'recentf
 :load t
 :after
 (progn
   (recentf-mode 1)
   (setq recentf-max-saved-items 100)
   (setq recentf-save-file "~/tmp/emacs_cache/recentf") ;;使双系统切换后不清空记录
   ;;(add-to-list 'recentf-exclude (expand-file-name package-user-dir))
   (add-to-list 'recentf-exclude ".cache")
   (add-to-list 'recentf-exclude ".cask")
   (add-to-list 'recentf-exclude "ido.last")
   (add-to-list 'recentf-exclude "bookmarks")
   (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
   ))


(auto-require
 'dired
 :load t
 :after
 (require 'init-dired))


(auto-require
 'smex
 :load t
 :before (add-package-path "smex")
 :after
 (progn
   ;; modify smex so that typing a space will insert a hyphen ‘-’ like in normal M-x
   ;; @see https://www.emacswiki.org/emacs/Smex
   (defadvice smex (around space-inserts-hyphen activate compile)
	 (let ((ido-cannot-complete-command 
			`(lambda ()
			   (interactive)
			   (if (string= " " (this-command-keys))
				   (insert ?-)
			     (funcall ,ido-cannot-complete-command)))))
	   ad-do-it))
   ))


;;;; ibuffer
(auto-require
 'ibuffer
 :before
 (progn
   (setq ibuffer-saved-filter-groups
	 '(("Default"
	    ("Hidden(g则不显示此分组)"  (name . "^ "))
	    ("Helm"  (or (name . "^\\*helm\\|^\\*ac-mode-")))
	    ("Help"  (or (name . "^\\*help\\|^\\*ac-mode-")))
	    ("Woman"  (name . "^\\*WoMan.*\\*$"))
	    ("Compile"  (name . "^*.*compil[ea].*$"))
	    ("Gtalk"  (or (name . "^\\*.*jabber") (name . "*fsm-debug*")))
	    ("ERC"  (mode . erc-mode))
	    ("Custom"  (mode . Custom-mode))
	    ("Shell"  (mode . shell-mode))
	    ("Mail" (or (mode . mew-summary-mode) (mode . mew-draft-mode)(mode . mew-message-mode)))
	    ("VC"  (or (name . "*magit-") (name . "^\\*vc")(mode . diff-mode) (mode . vc-dir-mode)))
	    ("Magit "  (name . "*magit:"))
	    ("Emacs"  (name . "^\\*.*$"))
	    ("Dired"  (mode . dired-mode))
	    ("Go"  (mode . go-mode))
	    ("Python"  (mode . python-mode))
	    ("EL"  (or (mode . emacs-lisp-mode) (mode . lisp-interaction-mode)))
	    ("C++" (mode . c++-mode))
	    ("Text" (name . ".txt"))
	    ))))
 :after
 (progn
   (add-hook 'ibuffer-mode-hook
	     '(lambda ()
		(ibuffer-auto-mode 1)
		(ibuffer-switch-to-saved-filter-groups "EL")))
   (setq ibuffer-show-empty-filter-groups nil)
   ))



;;;; s dash f
(eye-install-packages
 '(("dash" . "https://github.com/magnars/dash.el.git")
   ("s" . "https://github.com/magnars/s.el.git")
   ("f" . "https://github.com/rejeep/f.el.git")))

(auto-require
 'which-key
 :load t
 :urls '(("which-key" . "https://github.com/justbur/emacs-which-key.git"))
 :paths "which-key"
 :after
 (progn
   (which-key-mode 1)
   (which-key-setup-side-window-bottom)))


;;;; theme
(auto-require
 'naysayer-theme
 :load t
 :urls '(("naysayer-theme" . "https://github.com/nickav/naysayer-theme.el.git"))
 :after
 (load-theme 'naysayer t))


;;;; eye-keybindings
(auto-require
 'eye-keybindings
 :load t)


;;;; swiper counsel ivy
(auto-require
 'swiper
 :load t
 :urls '(("swiper" . "https://github.com/abo-abo/swiper.git"))
 :paths "swiper"
 :after
 (progn
   (setq ivy-initial-inputs-alist nil) ;;不需要自动添加^符号
   (setq ivy-use-virtual-buffers t)
   (setq ivy-count-format "(%d/%d)") ;; display both the index and the count
   (define-key global-map (kbd "M-x") #'counsel-M-x)
   ))



;;;; color-rg
(auto-require
 'color-rg
 :urls '(("color-rg" . "https://github.com/manateelazycat/color-rg.git"))
 :paths "color-rg"
 :functions '(color-rg-search-input))



;;;; rainbow-mode
(when is-gui
  (auto-require
   'rainbow-mode
   :paths "rainbow-mode"
   :functions 'rainbow-mode))


;;;; rainbow-delimiters
;; 括号高亮
(auto-require
 'rainbow-delimiters
 :load t
 :urls '(("rainbow-delimiters" . "https://github.com/Fanael/rainbow-delimiters.git"))
 :paths "rainbow-delimiters"
 :functions 'rainbow-delimiters-mode
 :before
 (progn
   (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))


;;;; highlight-numbers
(when is-gui
  (auto-require
   'highlight-numbers
   :urls '(("parent-mode" . "https://github.com/Fanael/parent-mode.git")
	   ("highlight-numbers" . "https://github.com/Fanael/highlight-numbers.git"))
   :paths '("highlight-numbers" "parent-mode")
   :functions 'highlight-numbers-mode
   :before
   (progn
     (add-hook 'prog-mode-hook 'highlight-numbers-mode))))


;;;; watch-other-window
(auto-require
 'watch-other-window
 :urls '(("watch-other-window" . "https://github.com/manateelazycat/watch-other-window.git"))
 :paths "watch-other-window"
 :functions '(watch-other-window-up
	      watch-other-window-down
	      watch-other-window-up-line
	      watch-other-window-down-line))


;;;; deft
(auto-require
 'deft
 :load nil
 :urls '(("deft" . "https://github.com/jrblevin/deft.git"))
 :paths "deft"
 :functions 'deft
 :after
 (progn
   (setq deft-recursive t)
   (setq deft-use-filename-as-title t) ;;是否把文件名作为标题
   (setq deft-extensions '("txt" "tex" "org"))
   (setq deft-directory (if is-linux "~/orgnotes/org" "x:/orgnotes/org"))
   (setq deft-file-limit 50) ;;最多显示多少文件，nil不限制
   (setq deft-filter-only-filenames t) ;;只搜索文件名
   (setq deft-use-filename-as-title t)
   ;;(setq deft-filter-only-filenames nil) ;;搜索标题
   (setq deft-auto-save-interval 0) ;;是否自动保存从deft打开的文件
   (setq deft-current-sort-method 'mtime) ;;排序方式
   (setq deft-default-extension "org")
   (setq deft-strip-summary-regexp ".*")))

(defun eye/deft-search(filter)
  (interactive "MFilter: ")
  (deft)
  (deft-filter filter t))

(defun deft-or-close ()
  (interactive)
  (if (eq major-mode 'deft-mode)
      (progn (kill-buffer "*Deft*"))
    (deft)
    ))

(defun eye/deft-new-file-named (slug)
  "Create a new file named SLUG.
SLUG is the short file name, without a path or a file extension."
  (interactive "sNew filename (without extension): ")
  (require 'deft)
  (let ((file (deft-absolute-filename slug "org")))
    (if (file-exists-p file)
        (message "Aborting, file already exists: %s" file)
      (deft-auto-populate-title-maybe file)
      (deft-cache-update-file file)
      (deft-refresh-filter)
      (deft-open-file file)
      (with-current-buffer (get-file-buffer (file-truename file))
		(insert "#+TITLE: ")
		(insert slug)
		(newline)
		(insert "#+FILETAGS: ::")
		(newline)
		(newline)
        (goto-char (point-max))))))



;;;; aweshell
(auto-require
 'aweshell
 :urls '(("aweshell" . "https://github.com/manateelazycat/aweshell.git"))
 :paths "aweshell"
 :functions '(aweshell-new aweshell-next aweshell-prev aweshell-toggle)
 :after
 (progn
   ;; aweshell下git命令输出乱码时，需要设置为utf-8环境
   (with-eval-after-load 'eshell (set-language-environment "utf-8")) ;; chinese-gbk
   )
 )



;;;; password-generator
(auto-require
 'password-generator
 :urls '(("password-generator" . "https://github.com/zargener/emacs-password-genarator.git"))
 :paths "password-generator"
 :functions
 '(password-generator-simple   password-generator-strong
   password-generator-paranoid password-generator-numeric))


;;;; bm
(auto-require
 'bm
 :paths "bm"
 :urls '(("bm" . "https://github.com/joodland/bm.git"))
 :functions '(bm-toggle bm-next bm-previous)
 :before
 (progn
   (require 'ext-bm)
   ;;(autoload 'counsel-bm "ext-bm")
   (setq bm-cycle-all-buffers nil		;; 是否在所有buffer中循环
	 ;; (setq bm-in-lifo-order t)		;; 先入先出
	 bm-restore-repository-on-load t
	 ;; where to store persistant files
	 bm-repository-file "~/.emacs.d/bm-repository"
	 ;; save bookmarks
	 bm-buffer-persistence t))
 :after
 (require 'init-bm))




;;;; tags
(auto-require 'init-ctags :load t)

;;;; yasnippet
(auto-require
 'yasnippet
 :urls '(("yasnippet" . "https://github.com/joaotavora/yasnippet.git"))
 :paths "yasnippet"
 :functions '(yas-minor-mode yas-global-mode))



;;;; lisp-mode
(require
 'lisp-mode
 :after
 (progn
   (defun setup-lisp-mode ()
     (setq tab-with 2))
   (add-hook 'emacs-lisp-mode-hook 'setup-lisp-mode)))


;;;; cc-mode cpp
(auto-require
 'cc-mode
 :before
 (require 'init-cpp))


;;;; treemacs
(auto-require
 'treemacs
 :urls '(("treemacs" . "https://github.com/Alexander-Miller/treemacs.git"))
 :paths '("s" "f" "ht" "ace-window" "pfuture" "treemacs/src/elisp")
 :functions '((treemacs . "treemacs"))
 :after
 (progn
   (setq treemacs-width 50)
   (bind-key treemacs-mode-map "<right>" 'treemacs-RET-action)))



;;;; bing-dict
(auto-require
 'bing-dict
 :urls '(("bing-dict" . "https://github.com/cute-jumper/bing-dict.el.git"))
 :paths "bing-dict"
 :functions 'bing-dict-brief)


(auto-require 'eye-programming :load t)

(auto-require 'eye-work :load t)

(when is-linux
  (auto-require
   'magit
   :load t
   :urls '(("dash" . "https://github.com/magnars/dash.el")
		   ("transient" . "https://github.com/magit/transient")
		   ("with-editor" . "https://github.com/magit/with-editor")
		   ("magit" . "https://github.com/magit/magit.git"))
   :paths '("dash" "transient/lisp" "with-editor" "magit/lisp")
   :after
   (progn
     )
   ))



;;;; orgmode
(auto-require 'eye-org :load t)
 

;;;; yankpad
(auto-require
 'yankpad
 :urls '(("yankpad" . "https://github.com/Kungsgeten/yankpad.git"))
 :paths "yankpad"
 :reqby 'org
 :functions 'yankpad-insert
 :after
 (progn
   ;; ~/.emacs.d/snippets is yas--default-user-snippets-dir
   ;;如果手动更换orgmode9后，这句执行后出现Not a face: nil的奇怪问题，终端下ivy无法弹出来，如果是赋值为不带/的字符串，又不会出现问题
   (setq yankpad-file (expand-file-name "org/note/yankpad.org" locale-notebook-dir))
   ;; (add-to-list 'hippie-expand-try-functions-list #'yankpad-expand)
   ))

;;;; emacs-rime
;; build lib
;; make lib EMACS_MODULE_HEADER_ROOT=/home/owen/opt/emacs-27.2/install/include/
;; switch method "C-\"
(when is-linux
  (auto-require
   'rime
   :load t
   :urls '(("dash" . "https://github.com/magnars/dash.el.git")
	   ("emacs-rime" . "https://github.com/DogLooksGood/emacs-rime.git"))
   :paths '("dash" "emacs-rime")
   :after
   (progn
     (setq default-input-method "rime")
     (setq rime-show-candidate 'popup)
     )))


;; Restore session at last.
(require 'init-session)
;;(emacs-session-restore)
