(global-auto-revert-mode 1)

(show-paren-mode 1) ;;高亮匹配的括号

(setq electric-pair-pairs '((?\{ . ?\}) (?\( . ?\)) (?\[ . ?\]) (?\" . ?\")))
(electric-pair-mode t) ;;自动输出成对括号

;; enable narrow-to-region
(put 'narrow-to-region 'disabled nil)

;; save clipboard contents into kill-ring before replace theme
(setq save-interprogram-paste-before-kill t)

(delete-selection-mode 1)

;; 退出时不要询问，@see https://stackoverflow.com/questions/2706527/make-emacs-stop-asking-active-processes-exist-kill-them-and-exit-anyway
(setq confirm-kill-processes nil)

;; 不要自动取消isearch高亮，可以用 lazy-highlight-cleanup 手动清除高亮
(setq lazy-highlight-cleanup nil)
(setq isearch-allow-scroll t) ;; 滚动时自动继续搜索

;; 全局默认缩进
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
(setq tab-width 4)

(setq mouse-yank-at-point t) ;; 强制粘贴时粘贴到光标处

;; Kill buffers without asking
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))


(setq ibuffer-expert t) ;;don't ask when delete

;; Don't open a file in a new frame
(when (boundp 'ns-pop-up-frames)
  (setq ns-pop-up-frames nil))

;;(setq track-eol t) ;; 保持光标上下移动时一直在行尾，需要设置line-move-visual为nil
;; (setq line-move-visual t)		;在长行中移动
(global-visual-line-mode 1)


(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; 鼠标滚轮滑动一次滚动多少行
;; (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
;; (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;; (setq scroll-step 5) ;; keyboard scroll one line at a time
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)


;; 固定行号显示区域的宽度
(setq display-line-numbers-width-start t)
;; 最小保留多少宽度
(setq display-line-numbers-width 5)


;; 半屏滚动
(defun scroll-half-page-down ()
  "scroll down half the page"
  (interactive)
  (scroll-down (/ (window-body-height) 2)))

(defun scroll-half-page-up ()
  "scroll up half the page"
  (interactive)
  (scroll-up (/ (window-body-height) 2)))

(global-set-key "\M-n" 'scroll-half-page-up)
(global-set-key "\M-p" 'scroll-half-page-down)

;; (menu-bar-mode -1)
;; (tool-bar-mode -1)
;; (scroll-bar-mode -1)

;; Fix load slow, https://github.com/raxod502/radian/issues/180
(when tool-bar-mode (tool-bar-mode -1)) ;; 禁用工具栏
(when menu-bar-mode (menu-bar-mode -1)) ;; 禁用菜单栏


(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

;; 防止退出时卡死在 Saving clipboard to X clipboard manager 状态
(setq x-select-enable-clipboard-manager nil)

(setq inhibit-startup-message t) ;; 禁用启动后显示的消息 buffer
(setq initial-scratch-message nil) ;; 禁止显示 *scratch* buffer 中默认出现的文本
(setq inhibit-compacting-font-caches t) ;; 防止卡顿，Don’t compact font caches during GC.
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-screen t)
(setq use-file-dialog nil)
(setq use-dialog-box nil)

(put 'suspend-frame 'disabled t) ;; 禁止 Ctrl+z 时挂起 emacs

;; 用 y/n 代替 yes/no 的回答
(defalias 'yes-or-no-p 'y-or-n-p) ;; (fset 'yes-or-no-p 'y-or-n-p) 相同的效果
(when (>= emacs-major-version 27)
  (define-key y-or-n-p-map [return] 'act) ;; 让回车键表示输入 y
  )

(setq ring-bell-function 'ignore) ;; 禁止出现烦人的响铃

(setq truncate-lines nil) ;; t--不自动折行 nil--折行

;; Fix load slow, https://github.com/raxod502/radian/issues/180
(when tool-bar-mode (tool-bar-mode -1)) ;; 禁用工具栏
(when menu-bar-mode (menu-bar-mode -1)) ;; 禁用菜单栏

;; 禁用滚动条 if no check is-gui, emacs26 -nw will be error
(when (and is-gui scroll-bar-mode)
  (scroll-bar-mode -1))

;; (setq frame-title-format "Ems") ;; 自定义标题栏
;; show full file path
(setq frame-title-format
      '("EMS - "
        (:eval
         (format "%s - %s" (buffer-name) default-directory))
        ))


;; 去掉窗口边缘和分割窗口时分割条的边缘
;; http://emacsredux.com/blog/2015/01/18/customizing-the-fringes/
;; linux maybe need gdb, and use mouse to set breakpoint on fringe, so only hide fringe on windows.
(when (and is-windows is-gui)
  (fringe-mode '(8 . 8)) ;; middle of split frame
  ;; (set-window-fringes nil 8 8) ;; border side
  (setq visual-line-fringe-indicators '(nil right-curly-arrow)) ;; 显示指示箭头
  )

(setq mouse-yank-at-point t) ;; 强制粘贴时粘贴到光标处


;;;; fringe
;; Disable ugly bitmap in fringe.
;; (define-fringe-bitmap 'left-arrow [])
;; (define-fringe-bitmap 'left-curly-arrow [])
;; (define-fringe-bitmap 'left-triangle [])

;; Display … and ↩ for truncation and wrap.
(defface fallback '((t :family "Fira Code Light"
                       :foreground "gray")) "Fallback")
(set-display-table-slot standard-display-table 'truncation
                        (make-glyph-code ?… 'fallback))
(set-display-table-slot standard-display-table 'wrap
                        (make-glyph-code ?↩ 'fallback))


;; 最大化
(defun maximize-frame ()
  "Maximizes the active frame in Windows"
  (interactive)
  (set-frame-parameter nil 'fullscreen 'maximized)
  ;; Send a `WM_SYSCOMMAND' message to the active frame with the
  ;; `SC_MAXIMIZE' parameter.
  (if is-windows
      (w32-send-sys-command 61488)))

(defun fullscreen-toggle ()
  "Toggle fullscreen/maximize status."
  (interactive)
  (if (equal 'fullboth (frame-parameter nil 'fullscreen))
      (maximize-frame)
    (set-frame-parameter nil 'fullscreen 'fullboth)))


(when is-gui
  ;; (setq-default cursor-type '(bar . 1))
  ;; (setq cursor-type '(bar . 1))
  ;; (set-cursor-color "#FF3300")

  ;;(blink-cursor-mode -1) ;; 取消光标闪烁

  (setq-default cursor-type '(bar . 2))
  (setq cursor-type '(bar . 2))


  (global-hl-line-mode t)
  )
;; (when is-gui
;; (add-hook 'after-init-hook
;; (lambda ()
;;(when is-gui (maximize-frame)) ;; VcXSrv not work
;;(setq-default cursor-type '(bar . 1))
;;(setq cursor-type '(bar . 1))
;; )))



;; 窗口透明
(when is-gui
  ;; Let the desktop background show through
  (defvar cur-is-transparent nil)
  (defun eye/switch-transparent ()
    (interactive)
    (if cur-is-transparent
        (progn
          (set-frame-parameter (selected-frame) 'alpha '(100 . 100))
          (add-to-list 'default-frame-alist '(alpha . (100 . 100)))
          (setq cur-is-transparent nil)
          )

      (progn
        (set-frame-parameter (selected-frame) 'alpha '(97 . 100))
        (add-to-list 'default-frame-alist '(alpha . (90 . 90)))
        (setq cur-is-transparent t))

      ))
  )




(setq default-directory user-emacs-directory)


(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward) ;; 同名文件区分显示


;; 向右移动时，当对当前行起作用
(setq auto-hscroll-mode 'current-line)


;; 自动保存书签
(require 'bookmark)
(add-hook 'kill-emacs-hook #'bookmark-save)


(provide 'init-misc)
