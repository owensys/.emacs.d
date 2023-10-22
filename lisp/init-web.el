
(eye/use-package
 'web-mode
 :load-path "web-mode"
 :ensure t
 :init
 (progn
   (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
   (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
   ))

(eye/use-package
 'js2-mode
 :load-path "js2-mode"
 :ensure t
 :init
 (progn
   (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
   (add-to-list 'auto-mode-alist '("\\.ts\\'" . js2-mode))
   ))

;; html补全
(eye/use-package
 'emmet-mode
 :load-path "emmet-mode"
 :ensure t)

(defun eye/html-char-to-ltgt()
  (interactive)
  (if (use-region-p)
      (let ((start (region-beginning))
            (end (region-end)))
        (replace-string "<" "&lt;" nil start end)
        (goto-char start)
        (replace-string ">" "&gt;" nil start end)
        )))

(defun eye/html-ltgt-to-char()
  (interactive)
  (if (use-region-p)
      (let ((start (region-beginning))
            (end (region-end)))
        (replace-string "&lt;" "<" nil start end)
        (goto-char start)
        (replace-string "&gt;" ">" nil start end)
        )))


(provide 'init-web)

