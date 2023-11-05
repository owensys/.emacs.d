
;; must use fullscreen
(eye/use-package 'holo-layer
                 :ensure t
                 :load-path "holo-layer-self"
                 :config
                 (progn
                   (setq holo-layer-cursor-color "LightSeaGreen")
                   (setq holo-layer-enable-cursor-animation t)
                   (setq holo-layer-hide-mode-line nil)
                   (setq holo-layer-enable-window-border nil)
                   ;; (setq holo-layer-inactive-window-color "gainsboro")
                   ;; dark theme
                   (setq holo-layer-active-window-color "#0078d7")
                   (setq holo-layer-inactive-window-color "gray30")
                   (setq holo-layer-enable-window-number-background t)
                   (setq holo-layer-sort-tab-ui nil)

		   (holo-layer-enable)
                   ;; (run-with-idle-timer 1 nil (lambda ()
                   ;; (fullscreen-toggle t)
                   ;; (holo-layer-enable)))
                   ))

;; test code
;;(set-frame-parameter nil 'fullscreen 'fullboth)
;; (add-to-list 'load-path "~/.emacs.d/packages/holo-layer")
;; (require 'holo-layer)

;; (setq holo-layer-cursor-color "red")
;; (setq holo-layer-enable-cursor-animation t)
;; (holo-layer-enable)



(provide 'init-holo-layer)

