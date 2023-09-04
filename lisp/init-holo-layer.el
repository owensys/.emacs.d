
;; must use fullscreen
(eye/use-package 'holo-layer
                 :ensure t
                 :load-path "holo-layer"
                 :config
                 (progn
                   (setq holo-layer-cursor-color "dark red")
                   (setq holo-layer-enable-cursor-animation t)
                   (setq holo-layer-hide-mode-line t)
                   (run-with-idle-timer 1 nil (lambda ()
                                                (fullscreen-toggle t)
                                                (holo-layer-enable)))
                   ))

;; test code
;;(set-frame-parameter nil 'fullscreen 'fullboth)
;; (add-to-list 'load-path "~/.emacs.d/packages/holo-layer")
;; (require 'holo-layer)

;; (setq holo-layer-cursor-color "red")
;; (setq holo-layer-enable-cursor-animation t)
;; (holo-layer-enable)



(provide 'init-holo-layer)

