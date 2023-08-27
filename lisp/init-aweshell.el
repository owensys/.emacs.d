;;;; awesomeshell
(eye/use-package
 'aweshell
 :load-path '("aweshell")
 :ensure nil
 :init
 (progn
   (autoload 'aweshell-toggle "aweshell" "" t)
   (autoload 'aweshell-new "aweshell" "" t)
   (autoload 'aweshell-next "aweshell" "" t)
   (autoload 'aweshell-prev "aweshell" "" t)
   (autoload 'aweshell-dedicated-toggle "aweshell" "" t)
   (autoload 'aweshell-dedicated-open "aweshell" "" t)
   (autoload 'aweshell-dedicated-close "aweshell" "" t)

   )
 )

(provide 'init-aweshell)

