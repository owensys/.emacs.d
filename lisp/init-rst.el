(require 'rst)

(defun eye-rst-mode-setup()
  (setq-local tab-width 2)
  )

(add-hook 'rst-mode-hook #'eye-rst-mode-setup)


(provide 'init-rst)
