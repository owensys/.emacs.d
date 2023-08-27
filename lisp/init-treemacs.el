(eye/use-package
 'treemacs
 :load-path '("s" "f" "ht" "ace-window" "pfuture" "cfrs" "treemacs/src/elisp")
 :ensure nil
 :command '((treemacs . "treemacs"))
 :config
 (progn
   (setq treemacs-width 50)
   ;; (bind-key treemacs-mode-map "<right>" 'treemacs-RET-action)

   ;; hide some files
   (defun treemacs-ignore-some-files (file _)
     (or
      (string= file ".gitignore")
      (string= file ".cache") ;; clangd cache dir
      (string= file "TAGS")
      (string= file ".lib")
      (string= file ".obj")
      (string= file ".deps")
      (string= file ".github")
      (string= file "a.out")
      (s-ends-with-p "dired" file)
      ;; s-starts-with-p
      )
     )
  (push #'treemacs-ignore-some-files treemacs-ignored-file-predicates)

  ))

(provide 'init-treemacs)
