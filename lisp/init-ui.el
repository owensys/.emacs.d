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

(setq ring-bell-function 'ignore) ;; 禁止出现烦人的响铃

(setq truncate-lines t) ;; 不自动折行

;; Fix load slow, https://github.com/raxod502/radian/issues/180
(when tool-bar-mode (tool-bar-mode -1)) ;; 禁用工具栏
;;(when menu-bar-mode (menu-bar-mode -1)) ;; 禁用菜单栏

;; 禁用滚动条 if no check is-gui, emacs26 -nw will be error
(when (and is-gui scroll-bar-mode)
  (scroll-bar-mode -1))

(setq frame-title-format "GNUEmacs") ;; 自定义标题栏

;; 去掉窗口边缘和分割窗口时分割条的边缘
;; http://emacsredux.com/blog/2015/01/18/customizing-the-fringes/
;; linux maybe need gdb, and use mouse to set breakpoint on fringe, so only hide fringe on windows.
(when (and is-windows is-gui)
  (set-window-fringes nil 10 0) ;; border side
  (fringe-mode '(10 . 0)) ;; middle of split frame
  )

(when is-gui
  (blink-cursor-mode -1) ;; 取消光标闪烁
  (add-hook 'after-init-hook
	    (lambda ()
	      (set-cursor-color "#FF3300"))))

(setq mouse-yank-at-point t) ;; 强制粘贴时粘贴到光标处

(provide 'init-misc)
