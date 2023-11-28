(require 'org)

(eye/use-package
 'vertico
 :ensure t
 :load-path '("compat" "vertico")
 :config
 (progn
   (vertico-mode)
   )
 )

(eye/use-package
 'vertico-posframe
 :ensure t
 :load-path '("vertico-posframe" "vertico/extensions")
 :config
 (progn
   (vertico-posframe-mode 1)
   ))

(eye/use-package
 'orderless
 :ensure t
 :load-path "orderless"
 :config
 (progn
   (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))
   )
 )

;; 使用vertico实现垂直显示模糊查找，查找时用“星号+关键词”
;; 如果再加上orderless，就可以不用星号，直接模糊查找
;; 再加了vertico和vertico-posfram，可以弹出child frame显示
(eye/use-package
 'org-roam
 :ensure t
 :load-path '("f" "s" "dash" "emacsql" "emacsql-sqlite3" "async" "magit/lisp" "compat" "org-roam" "org-roam/extensions")
 :config
 (progn
   (setq org-roam-database-connector 'sqlite-builtin) ;; fix sqlite3 library was not found
   (setq org-roam-directory org-directory)
   (if (not (f-dir-p org-roam-directory))
       (f-mkdir org-roam-directory))
   
   ;; 忽略目录, daily notes内容不删除，但不放到org-roam中
   ;; (setq org-roam-file-exclude-regexp
   ;; (concat "^" (expand-file-name org-roam-directory) "/journals/"))
   (setq org-roam-file-exclude-regexp nil)

   ;; 1.add date
   ;; 2.support select directory, see https://github.com/org-roam/org-roam/issues/888
   ;;   example, tip dir and input: books/, will be create note in sub dir.
   (setq org-roam-capture-templates '(("d" "default" plain "%?" :target
                                      (file+head "${dir}%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n#+tags: \n- tags :: \n")
                                      :unnarrowed t)
                                      ))

   (defun eye/insert-template-book ()
     (interactive)
     (insert-file-contents (concat (get-locale-book-dir) "/template/book.tpl"))
     )

   (defun eye/insert-template-literature ()
     (interactive)
     (insert-file-contents (concat (get-locale-book-dir) "/template/literature.tpl"))
     )


   (require 'custom-org-roam)
   

   )
 )


(eye/use-package
 'org-roam-ui
 :load-path '("org-roam-ui" "emacs-web-server" "emacs-websocket" "org-roam/extensions")
 :ensure t
 :config
 (setq org-roam-ui-sync-theme t
       org-roam-ui-follow nil
       org-roam-ui-update-on-save t
       org-roam-ui-open-on-start t)
 )


(provide 'init-org-roam)
