(eye/use-package 'shrink-path
                 :load-path "shrink-path"
                 :ensure t)

;; write a function to do the spacing
(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT
 aligned respectively."
  (let* ((right-space 50) ;; 右侧可用空间宽度
         (available-width (- (window-width) (length left) right-space)))
    ;; 第一层format 产生"%s %50s"
    (format (format " %%s %%%ds " available-width) left right)))

(defun eye--current-file-path ()
  (if (fboundp 'shrink-path-file)
      (shrink-path-file (buffer-file-name))
    (buffer-file-name)))

 ;;;; header-line
(defun eye-header-line-setup(&optional theme)
  (setq-default header-line-format
                '((:eval (simple-mode-line-render
                          ;; left
                          (format-mode-line (concat (if buffer-file-name (eye--current-file-path) "%b")
                                                    " "
                                                    (if buffer-read-only "🔒")
                                                    " %n"
                                                    ))
                          ;; right
                          (format-mode-line (concat (format "%s  " buffer-file-coding-system)
                                                    (string-replace "-mode" "" (format "%s" major-mode))
                                                    ""
                                                    ))
                          ))))
  )

(defun eye-mode-line-setup (&optional theme)
  ;; (setq-default mode-line-format nil)
  ;; (setq-default header-line-format nil)
  (setq-default mode-line-format "") ;; 清除空白，如果使用nil，则完全隐藏modeline
  (set-face-attribute 'mode-line nil :height 0.1 :background "#004f8c");;"#853910");;"#358594");;"7f5ab6")
  (set-face-attribute 'mode-line-inactive nil :height 0.1 :background "grey20")
  )

(add-hook 'after-init-hook #'eye-mode-line-setup)
;; (add-hook 'after-init-hook #'eye-header-line-setup)
(add-hook 'load-theme-after-hook #'eye-mode-line-setup)
;; (add-hook 'load-theme-after-hook #'eye-header-line-setup)


(eye/use-package 'awesome-tray
                 :load-path "awesome-tray"
                 :ensure t
                 :config
                 (progn
                   (defun tray-module-readonly-status-info ()
                     (if buffer-read-only "🔒" ""))
                   (add-to-list 'awesome-tray-module-alist
                                '("ro" . (tray-module-readonly-status-info awesome-tray-green-face)))

                   ;;; file pat
                   (setq awesome-tray-file-path-full-dirname-levels 10)
                   (setq awesome-tray-file-path-show-filename t)

                   (defun tray-module-encode-info ()
                     (format "%s" buffer-file-coding-system))
                   (add-to-list 'awesome-tray-module-alist
		                        '("encode" . (tray-module-encode-info awesome-tray-green-face)))
                   
                   ;; Enable awesome tray
                   (setq awesome-tray-active-modules '("ro" "file-path" "encode" "mode-name"))
                   (awesome-tray-mode t)
                   
                   ;; redefine mode name face
                   (defface awesome-tray-module-mode-name-face
                     '((((background light)) :foreground "DodgerBlue1" :bold t)
                       (t :foreground "DeepSkyBlue1" :bold t))
                     "Awesome tray mode name face."
                     :group 'awesome-tray)
                   
                   ))


(provide 'init-modeline)
