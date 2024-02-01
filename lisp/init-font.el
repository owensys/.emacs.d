;;;; font
;;
;; cn fonts:
;; "更纱黑体 Mono SC Nerd" https://github.com/laishulu/Sarasa-Mono-SC-Nerd
;;
;; en fonts:
;; "Ubuntu Mono" 可以显示更多行数，且org table字体对齐，但只能设置英文
;; "Inconsolata" 用 Inconsolata 可以让org表格中英文对齐，@see https://emacs-china.org/t/org-mode/12498/5
;; Courier New
;;

(if is-windows (setq cn-font-name "更纱黑体 Mono SC Nerd") (setq cn-font-name "Inconsolata"))
(setq en-font-name (if is-windows "Courier New" "Ubuntu Mono"))

(setq en-font-size 14 cn-font-size 14)
;; (setq en-font-size 16 cn-font-size 14)

(defun eye--setup-font()
  (interactive)

  ;; 获取屏幕分辨率自动增大字体
  (when (and is-gui
	     (> (x-display-pixel-width) 1366)
	     (> (x-display-pixel-height) 768))
    (setq en-font-size (+ en-font-size 2))
    (setq cn-font-size (+ cn-font-size 2)))


  (defun eye-update-font-size ()
    ;; English font
    (set-face-attribute 'default nil :font
		        (font-spec :family en-font-name :weight 'normal :slant 'normal :size en-font-size))
    ;; Chinese font
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
		        charset
		        (font-spec :family cn-font-name
				   :weight 'normal
				   :slant 'normal
				   :size en-font-size ;;不要设置这个不同的大小，会导致org表格不能对齐
				   ))))


  (defun eye/increase-font-size ()
    "Increase font size of english and chinese."
    (interactive)
    (setq en-font-size (+ en-font-size 1))
    (setq cn-font-size (+ cn-font-size 1))
    (eye-update-font-size)
    )

  (defun eye/decrease-font-size ()
    "Decrease font size of english and chinese."
    (interactive)
    (setq en-font-size (- en-font-size 1))
    (setq cn-font-size (- cn-font-size 1))
    (eye-update-font-size)
    (if (equal (frame-parameter nil 'fullscreen) 'maximize)
        (maximize-frame))
    )

  (when is-gui
    (eye-update-font-size)
    ;;(setq face-font-rescale-alist '(("微软雅黑" . 1.2) ("Microsoft Yahei" . 1.2) ("Noto Sans CJK SC Regular" . 1.2)))

    (define-key global-map (kbd "<C-wheel-up>") #'eye/increase-font-size)
    (define-key global-map (kbd "<C-wheel-down>") #'eye/decrease-font-size)
    )

  )

;; (add-hook 'after-make-frame-functions 'eye--setup-font)

(eye--setup-font)

(provide 'init-font)
