;;;; config-init.el --- My emacs configuration for orgmode -*- lexical-binding: t -*-
;;(setq debug-on-error t) ;;Produce backtraces when errors occur

;;;; preload path
(setq eye-packages-dir (expand-file-name "emacs-packages" user-emacs-directory))
(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(add-to-list 'load-path (concat eye-packages-dir "/benchmark-init-el"))

(require 'benchmark-init)
(require 'benchmark-init-modes)
(benchmark-init/activate)
(add-hook 'after-init-hook 'benchmark-init/deactivate)




(require 'init-startup)

(require 'init-system-path)



(require 'init-font)

(require 'init-misc)


(require 'init-encoding) ;; 导致tramp失败

(require 'init-tramp)
;; (require 'init-utils)

;; (setq use-fixed-minibuffer-height nil)

;; ;;;; eye-packages
;; (require 'init-packages)


;; (setq delete-by-moving-to-trash t)	;; 删除文件或目录时，移到回收站
;; ;; windows需要用函数 system-move-file-to-trash, @see https://www.masteringemacs.org/article/making-deleted-files-trash-can

;; ;; display the real names on mode-line when visiting a symbolink
;; (setq find-file-visit-truename t)

;; (setq recentf-max-saved-items 500) ;; 最近文件保存记录个数
;; (setq recentf-auto-cleanup 'never) ;; 防止samba路径不在了导致卡住
;; ;; (defun recentf-keep-it (file) t)
;; ;; (setq recentf-keep '(recentf-keep-it)) ;; 不检测远程文件，防止卡住


;;(require 'init-keys)


;; ;;;; imenu
;; (require 'imenu)
;; ;; 自定义menu
;; (setq imenu-auto-rescan t)
;; ;;;; Elisp
;; (defun imenu-elisp-sections ()
;;   (setq imenu-prev-index-position-function nil)
;;   ;; imenu-generic-expression is locale, must use hook
;;   (add-to-list 'imenu-generic-expression '("sections" "^;;;; \\(.+\\)$" 1) t))

;; (add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)

;; ;;;; theme
;; (require 'init-theme)

;; (require 'init-dired)

;; ;;;; swiper counsel ivy
;; (eye/use-package
;;  'swiper
;;  :ensure t
;;  :load-path "swiper"
;;  :command '((counsel-org-goto . "counsel")
;;             (counsel-M-x . "counsel")
;;             (counsel-find-file . "counsel")
;;             (counsel-buffer-or-recentf . "counsel")
;;             (counsel-load-theme . "counsel")
;;             (counsel-ibuffer . "counsel")
;;             (counsel-imenu . "counsel")
;;             (counsel-rg . "counsel")
;;             (counsel-git . "counsel")
;;             (counsel-describe-function . "counsel")
;;             (counsel-describe-variable . "counsel")
;;             (counsel-describe-face . "counsel")
;;             (counsel-etags-list-tag . "counsel-etags")
;;             (counsel-etags-recent-tag . "counsel-etags")
;;             (ivy-switch-buffer . "ivy")
;;             )
;;  :config
;;  (progn
;;    (define-key global-map (kbd "C-h v") 'counsel-describe-variable)
;;    (define-key global-map (kbd "C-h f") 'counsel-describe-function)
;;    (define-key global-map (kbd "C-h F") 'counsel-describe-face)
;;    (setq ivy-initial-inputs-alist nil) ;;不需要自动添加^符号
;;    (setq ivy-use-virtual-buffers t)
;;    (setq ivy-height 15)
;;    (setq ivy-count-format "(%d/%d)") ;; display both the index and the count
;;    (define-key global-map (kbd "M-x") #'counsel-M-x)

;;    ;; 使counsel-rg只需要2个字符就可以开始实时搜索
;;    (add-to-list 'ivy-more-chars-alist '(counsel-rg . 2))

;;    (when use-fixed-minibuffer-height
;;      ;; 固定minibuffer 高度
;;      (setq resize-mini-windows nil)
;;      (setq minibuffer-height 1)
;;      (defun my-set-mini-window-height (&optional frame)
;;        (let ((mini-win (minibuffer-window frame)))
;;          (when (and mini-win (< (window-size mini-win) minibuffer-height))
;;            (window-resize mini-win (- minibuffer-height (window-size mini-win))))
;;          ))
;;      (add-hook 'window-size-change-functions 'my-set-mini-window-height)
;;      )
;;    ))



;; ;;;; ivy-posframe
;; (eye/use-package
;;  'ivy-posframe
;;  :ensure t
;;  :load-path '("ivy-posframe" "posframe")
;;  :config
;;  (progn
;;    ;; (setq ivy-posframe-height nil) ;; 高度
;;    (setq ivy-posframe-border-width 3)
;;    (setq ivy-height 10) ;;ivy-posframe-height) ;; 把ivy-height设置成和ivy-posframe-height一样的高度可以让列表占满整个高度
;;    ;; display at `ivy-posframe-style'
;;    (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
;;    ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-bottom-left)))
;;    ;; (setq ivy-posframe-display-functions-alist
;;          ;; '((t . ivy-posframe-display-at-frame-center)))
;;    (ivy-posframe-mode 1)
;;    ))



;; ;;;; smex
;; ;; M-x 记录历史必须用
;; (eye/use-package
;;  'smex
;;  :ensure t
;;  :load-path "smex"
;;  :config
;;  (progn
;;    (require 'smex)
;;    ;; modify smex so that typing a space will insert a hyphen ‘-’ like in normal M-x
;;    ;; @see https://www.emacswiki.org/emacs/Smex
;;    (defadvice smex (around space-inserts-hyphen activate compile)
;;      (let ((ido-cannot-complete-command
;; 	    `(lambda ()
;; 	       (interactive)
;; 	       (if (string= " " (this-command-keys))
;; 		   (insert ?-)
;; 		 (funcall ,ido-cannot-complete-command)))))
;;        ad-do-it))
;;    ))



;; ;;;; auto readonly
;; (defun eye-check-open-with-readonly-mode ()
;;   "对于存在只读标志文件的目录，打开文件后进入只读模式"
;;   (let ((has-readonly-flag (locate-dominating-file ".readonly" ".readonly")))
;;     (if has-readonly-flag
;;         (read-only-mode )))
;;   )
;; (add-hook 'find-file-hook #'eye-check-open-with-readonly-mode)



;; ;;;; color-rg
;; ;; 出现“No such directory found via CDPATH environment variable” 时，关闭*color-rg* buffer即可。
;; ;; 在color-rg-build-command中增加(list "-g !TAGS") 可以过滤文件
;; (eye/use-package
;;  'color-rg
;;  :load-path "color-rg"
;;  :ensure t
;;  :command '(color-rg-search-input)
;;  :config
;;  (progn
;;    (set-face-attribute 'color-rg-font-lock-match nil :foreground "dark green")
;;    (setq color-rg-search-ignore-rules "-g \"!*.dat\" -g \"!#*\" -g \"!*~\"  -g \"!TAGS\"") ;; 注意，必须用双引号，否则搜索结果为空
;;    )
;;  )


;; ;; (defun eye-set-to-readonly-mode ()
;; ;;   (read-only-mode t)
;; ;;   (message "Read only mode enabled!"))
;; ;; 一分钟没有编辑，设置成只读模式
;; ;; (run-with-idle-timer 180 t #'eye-set-to-readonly-mode)

;; ;;;; bm
;; (eye/use-package
;;  'bm
;;  :load-path "bm"
;;  :ensure t
;;  :command '(bm-toggle bm-next bm-previous)
;;  :init
;;  (progn
;;    ;; (require 'ext-bm)
;;    ;;(autoload 'counsel-bm "ext-bm")
;;    (setq bm-cycle-all-buffers nil		;; 是否在所有buffer中循环
;; 	 ;; (setq bm-in-lifo-order t)		;; 先入先出
;; 	 bm-restore-repository-on-load t
;; 	 ;; where to store persistant files
;; 	 bm-repository-file "~/.emacs.d/bm-repository"
;; 	 ;; save bookmarks
;; 	 bm-buffer-persistence t))
;;  :config
;;  (require 'init-bm))

;; ;; 像素滚动
;; ;; emacs29可以用pixel-scroll-mode但在windows上体验卡顿
;; (when (and (> emacs-major-version 26)
;;            (< emacs-major-version 30))
;;   (eye/use-package
;;    'good-scroll
;;    :load-path '("good-scroll")
;;    :ensure t
;;    :config
;;    (progn
;;      (good-scroll-mode 1)
;;      ;; 绑定上下翻页键也支持像素滚动
;;      (global-set-key [next] #'good-scroll-up-full-screen)
;;      (global-set-key [prior] #'good-scroll-down-full-screen)
;;      )
;;    )
;;   )


;; ;;;; ctrlf
;; (eye/use-package
;;  'ctrlf
;;  :ensure t
;;  :load-path "ctrlf"
;;  :config
;;  (progn
;;    (ctrlf-mode +1)))


;; ;;;; super-save
;; (eye/use-package
;;  'super-save
;;  :ensure t
;;  :load-path "super-save"
;;  :config
;;  (progn
;;    (setq auto-save-default nil)
;;    (super-save-mode +1)
;;    )
;;  )

;; ;;;; eno
;; ;; similar package, https://github.com/lyjdwh/avy-thing-edit
;; (eye/use-package 'eno
;;                  :ensure t
;; 	             :load-path '("eno" "dash" "edit-at-point")
;; 	             :command '(eno-word-copy eno-word-copy-in-line eno-line-copy)
;; 	             :config
;; 	             (progn
;; 		           (defun eye/eno-copy ()
;; 		             (interactive)
;; 		             (cond
;; 		              ((equal major-mode 'c++-mode)
;; 		               (eno-word-copy))
;; 		              ((or (equal major-mode 'emacs-lisp-mode) (equal major-mode 'lisp-interaction-mode))
;; 		               (eno-symbol-copy))
;; 		              (t (eno-word-copy))))
;; 		           ))


;; ;;;; popper
;; (eye/use-package
;;  'popper
;;  :ensure t
;;  :load-path "popper"
;;  :command '(popper-toggle-latest popper-cycle popper-toggle-type)
;;  :init
;;  (progn
;;    (setq popper-reference-buffers
;;          '("\\*Messages\\*"
;;            "Output\\*$"
;;            "\\*Async Shell Command\\*"
;;            help-mode
;;            compilation-mode))
;;    )
;;  :config
;;  (progn
;;    (popper-mode +1)
;;    (global-set-key (kbd "C-`") 'popper-toggle-latest)
;;    (global-set-key (kbd "M-`") 'popper-cycle)
;;    (global-set-key (kbd "C-M-`") 'popper-toggle-type)
;;    ;; (require 'popper-echo)
;;    ;; (popper-echo-mode +1)
;;    )
;;  )

;; (require 'long-line)

;; ;;;; markdown-mode
;; (eye/use-package
;;  'markdown-mode
;;  :ensure t
;;  :load-path "markdown-mode"
;;  )


;; ;;;; lsp-bridge
;; (eye/use-package
;;  'lsp-bridge
;;  :ensure t
;;  :load-path '("posframe" "markdown-mode" "yasnippet" "lsp-bridge")
;;  :config
;;  (progn
;;    (require 'yasnippet)
;;    (yas-global-mode 1)

;;    ;; (setq lsp-bridge-c-lsp-server "clangd-13")
;;    (global-lsp-bridge-mode)
;;    (setq lsp-bridge-python-command "python3.exe") ;; windows需要复制一个python.exe为python3.exe


;;    )
;;  )

;; ;;;; cmake-mode
;; (eye/use-package
;;  'cmake-mode
;;  :ensure nil
;;  :command '(cmake-mode)
;;  :init
;;  (progn
;;    (add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
;;    (add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))
;;    (let ((cmake-el-dir (concat user-emacs-directory "site-lisp/cmake-mode")))
;;      (add-to-list 'load-path cmake-el-dir)
;;      (autoload 'cmake-mode "cmake-mode")
;;    )))

;; ;;;; symbol-overlay
;; (eye/use-package
;;  'symbol-overlay
;;  :ensure t
;;  :load-path "symbol-overlay"
;;  :command '(symbol-overlay-mode symbol-overlay-put)
;;  :config
;;  (progn
;;    (symbol-overlay-mode)
;;    ;; clear default keys
;;    (define-key symbol-overlay-map (kbd "i") nil)
;;    (define-key symbol-overlay-map (kbd "h") nil)
;;    (define-key symbol-overlay-map (kbd "p") nil)
;;    (define-key symbol-overlay-map (kbd "n") nil)
;;    (define-key symbol-overlay-map (kbd "<") nil)
;;    (define-key symbol-overlay-map (kbd ">") nil)
;;    (define-key symbol-overlay-map (kbd "w") nil)
;;    (define-key symbol-overlay-map (kbd "t") nil)
;;    (define-key symbol-overlay-map (kbd "e") nil)
;;    (define-key symbol-overlay-map (kbd "d") nil)
;;    (define-key symbol-overlay-map (kbd "s") nil)
;;    (define-key symbol-overlay-map (kbd "q") nil)
;;    (define-key symbol-overlay-map (kbd "r") nil)
;;    )
;;  )

;; ;;;; sort-tab
;; (eye/use-package
;;  'sort-tab
;;  :ensure t
;;  :load-path "sort-tab"
;;  :config
;;  (progn
;;    (require 'sort-tab)
;;    (sort-tab-mode 1)
;;    ))

;; ;;;; global-readonly
;; (eye/use-package
;;  'global-readonly-mode
;;  :ensure t
;;  :load-path "global-readonly"
;;  :command 'global-readonly-toggle
;;  :init
;;  (progn
;;    (setq global-readonly-disable-mouse nil)
;;    ))


;; ;;;; rainbow-delimiters
;; ;; 括号高亮
;; (eye/use-package
;;  'rainbow-delimiters
;;  :ensure t
;;  :load-path "rainbow-delimiters"
;;  :command 'rainbow-delimiters-mode
;;  :init
;;  (progn
;;    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))


;; ;; 使用 emacsclient 需要先启动服务
;; (require 'server)
;; ;; (if is-org
;; ;;     (setq server-name "server-org") ;;避免和gui版冲突
;; ;;   (setq server-name "server"))

;; (if (not (equal t (server-running-p)))
;;     (server-start))


;; ;;;; orgmode
;; ;; idle load

;; ;; (run-with-idle-timer 2 nil
;; ;;                      (lambda ()
;; (require 'init-programming)
;; (require 'init-cpp)
;; (require 'init-org)
;; ;;(require 'init-session)
;; (require 'init-denote)
;; (require 'init-eshell)
;; (require 'init-aweshell)
;; (require 'init-writeroom)
;; (require 'init-magit)
;; (require 'init-emms)
;; (require 'init-treemacs)
;; (require 'init-eaf)
;; (require 'init-mpvi)

;; ;;;; bing-dict
;; (eye/use-package
;;  'bing-dict
;;  :load-path "bing-dict"
;;  :command 'bing-dict-brief)

;; ))


;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-all.el ends here
