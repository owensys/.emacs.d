
(eye/use-package 'markdown-mode
                 :ensure t
                 :urls '(("markdown-mode" . "https://github.com/jrblevin/markdown-mode.git"))
                 :load-path '("markdown-mode")
                 :init
                 (progn
                   (autoload 'markdown-mode "markdown-mode"
                     "Major mode for editing Markdown files" t)
                   (add-to-list 'auto-mode-alist
                                '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))

                   (autoload 'gfm-mode "markdown-mode"
                     "Major mode for editing GitHub Flavored Markdown files" t)
                   (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
                   )
                 :config
                 (progn
                   )
                 )

(provide 'init-markdown)
