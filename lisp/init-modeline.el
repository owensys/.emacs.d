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
  (setq-default mode-line-format "")
  (set-face-attribute 'mode-line nil :height 0.1 :background "dark red")
  (set-face-attribute 'mode-line-inactive nil :height 0.1)
  )

(add-hook 'after-init-hook #'eye-mode-line-setup)
(add-hook 'after-init-hook #'eye-header-line-setup)
(add-hook 'load-theme-after-hook #'eye-mode-line-setup)
(add-hook 'load-theme-after-hook #'eye-header-line-setup)


(provide 'init-modeline)
