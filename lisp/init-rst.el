
(eye/use-package
 'rst
 :init
 (progn
   (add-to-list 'auto-mode-alist '("\\.rst\\'" . rst-mode))
   )
 :config
 (progn
   (defun eye-rst-mode-setup()
     (setq-local tab-width 2)
     )
   (add-hook 'rst-mode-hook #'eye-rst-mode-setup)
   )
 )


(provide 'init-rst)
