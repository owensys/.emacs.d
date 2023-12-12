;; see https://emacs-china.org/t/tab-bar/26008
(eye/use-package
 'tab-bar
 :ensure t
 :config
 (progn
   (setq tab-bar-separator ""
         tab-bar-new-tab-choice "*scratch*"
         tab-bar-tab-name-truncated-max 20
         tab-bar-auto-width nil
         tab-bar-close-button-show nil
         tab-bar-tab-hints t)

   ;; 使用 super-1 super-2 ... 来切换 tab
   (customize-set-variable 'tab-bar-select-tab-modifiers '(super))

   ;; 自动截取 tab name，并且添加在每个 tab 上添加数字，方便用快捷键切换
   (setq tab-bar-tab-name-function
         (lambda () (let* ((raw-tab-name (buffer-name (window-buffer (minibuffer-selected-window))))
                           (count (length (window-list-1 nil 'nomini)))
                           (truncated-tab-name (if (< (length raw-tab-name)
                                                      tab-bar-tab-name-truncated-max)
                                                   raw-tab-name
                                                 (truncate-string-to-width raw-tab-name
                                                                           tab-bar-tab-name-truncated-max
                                                                           nil nil tab-bar-tab-name-ellipsis))))
                      (if (> count 1)
                          (concat truncated-tab-name "(" (number-to-string count) ")")
                        truncated-tab-name))))

   ;; 给 tab 两边加上空格，更好看
   (setq tab-bar-tab-name-format-function
         (lambda (tab i)
           (let ((face (funcall tab-bar-tab-face-function tab)))
             (concat
              (propertize " " 'face face)
              (propertize (number-to-string i) 'face `(:inherit ,face :weight ultra-bold :underline t))
              (propertize (concat " " (alist-get 'name tab) " ") 'face face)))))

   ;; 我把 meow 的 indicator 也放在 tab-bar 上
   (setq tab-bar-format '(meow-indicator  tab-bar-format-tabs))
   (tab-bar--update-tab-bar-lines)

   ;; WORKAROUND: update tab-bar for daemon
   (when (daemonp)
     (add-hook 'after-make-frame-functions
               #'(lambda (&rest _) (force-mode-line-update))))
   )
 )


(provide 'init-tab-bar)
