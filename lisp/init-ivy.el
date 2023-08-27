(require 'ivy)
(require 'counsel)
(require 'swiper)

;; counsel-M-x 需要require smex才会按照使用频率排序
(with-eval-after-load 'counsel (require 'smex))
(with-eval-after-load 'ivy
  (setq ivy-initial-inputs-alist nil) ;;不需要自动添加^符号
  (setq ivy-count-format "(%d/%d)") ;; display both the index and the count
  ;; 在当前光标处弹出ivy
  ;; (setq ivy-completion-beg 0)
  ;; (setq ivy-display-function 'ivy-display-function-overlay)
  (define-key ivy-minibuffer-map (kbd "M-i") 'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "M-k") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "M-o") 'ivy-occur)

  ;; 防止鼠标误点击
  (add-hook 'ivy-occur-grep-mode-hook
      (lambda ()
        (set (make-local-variable 'mouse-1-click-follows-link) nil)))

  ;; 不想让分割左右窗口后还是在左下角弹出ivy
  ;; @see https://emacs-china.org/t/topic/5754/9
  ;; (defvar maple/ivy-format-padding nil)
  ;; (defun maple/ivy-read-around (-ivy-read &rest args)
  ;;   "Advice ivy-read `-IVY-READ` `ARGS`."
  ;;   (let ((maple/ivy-format-padding (make-string (window-left-column) ?\s)))
  ;;     (setcar args (concat maple/ivy-format-padding (car args)))
  ;;     (apply -ivy-read args)))

  ;; (advice-add 'ivy-read :around #'maple/ivy-read-around)
  ;; (defun maple/ivy-format-function (cands)
  ;;   "Transform CANDS into a string for minibuffer."
  ;;   (ivy--format-function-generic
  ;;    (lambda (str)
  ;;      (concat maple/ivy-format-padding (ivy--add-face str 'ivy-current-match)))
  ;;    (lambda (str)
  ;;      (concat maple/ivy-format-padding str))
  ;;    cands "\n"))

  ;; ;(setq ivy-format-function 'maple/ivy-format-function)  
  )


;; ;; ivy-posframe
(when (and is-enable-posframe is-gui (>= emacs-major-version 26))
  (require 'ivy-posframe)
  ;; (setq ivy-display-function #'ivy-posframe-display) ;显示在window下边
  ;; (setq ivy-display-function #'ivy-posframe-display-at-frame-center)
  ;; (setq ivy-display-function #'ivy-posframe-display-at-window-center)
  (setq ivy-display-function #'ivy-posframe-display-at-frame-bottom-left)
  ;; (setq ivy-display-function #'ivy-posframe-display-at-window-bottom-left)
  ;; (setq ivy-display-function #'ivy-posframe-display-at-point)

  (push '(counsel-M-x . ivy-posframe-display-at-window-bottom-left) ivy-display-functions-alist)
  (push '(complete-symbol . ivy-posframe-display-at-point) ivy-display-functions-alist)
  (push '(swiper . ivy-posframe-display-at-point) ivy-display-functions-alist)

  ;; show border
  (set-face-attribute 'internal-border nil :background "red")
  ;; 设置ivy-posframe中的光标颜色，避免在solarized-dark theme中看不见
  (set-face-attribute 'ivy-posframe-cursor nil :foreground "#00A876")

  (setq ivy-posframe-border-width 1)
  (ivy-posframe-enable)  
  )



(defun execute-func-use-marked (func)
  (if (region-active-p)
      (let ((str (buffer-substring (region-beginning) (region-end))))
        (pop-mark)
        (funcall func str))
    (funcall func)))

(defun swiper-dwim ()
  "Search input word or current select string"
  (interactive)
  (execute-func-use-marked 'swiper))

(defun counsel-rg-marked()
  "Search input word or current select string use rg"
  (interactive)
  (execute-func-use-marked 'counsel-rg))

(defun counsel-ag-marked ()
  "Search input word or current select string use ag"
  (interactive)
  (execute-func-use-marked 'counsel-ag))
;; 如果需要输入长度小于3时不搜索，需要修改内部函数 counsel-ag-function


(defun eye/counsel-git (&optional initial-input)
  "默认的counsel-git只能在有.git的目录下才有效，重新定义一个函数"
  (interactive)
  (counsel-require-program counsel-git-cmd)
  (let* ((default-directory (locate-dominating-file default-directory ".git"))
	 ;; (default-directory (counsel-locate-git-root))
         (cands (split-string
                 (shell-command-to-string counsel-git-cmd)
                 "\n"
                 t)))
    (ivy-read "Find file: " cands
              :initial-input initial-input
              :action #'counsel-git-action
              :caller 'eye/counsel-git)))


;(defhydra+ hydra-help (:exit t :idle 1.0)
;  ("v" counsel-describe-variable "Desc var")
;  ("f" counsel-describe-function "Desc fun")
;  ("a" counsel-describe-face "Desc face")
;  ("b" counsel-descbinds "Desc bind"))



;(defhydra+ hydra-search (:idle 1.0)
;  ("s" swiper-dwim "swiper" :exit t)
;  ("l" counsel-rg "counsel-rg" :exit t)
;  ("k" counsel-rg-marked "rg marked" :exit t))


;(defhydra+ hydra-imenu (:exit t :idle 1.0)
;  ("c" counsel-imenu "counsel imenu")) ;counsel-semantic-or-imenu is not autoload function, use counsel-imenu







(provide 'init-ivy)
