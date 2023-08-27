(eye/use-package
 'vertico
 :ensure t
 :load-path "vertico"
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
   (setq org-roam-database-connector 'sqlite3) ;; fix sqlite3 library was not found
   (setq org-roam-directory (get-locale-book-dir))
   (if (not (f-dir-p org-roam-directory))
       (f-mkdir org-roam-directory))
   (org-roam-db-autosync-mode)

   ;; 忽略目录, daily notes内容不删除，但不放到org-roam中
   (setq org-roam-file-exclude-regexp
         (concat "^" (expand-file-name org-roam-directory) "/journals/"))

   ;; 1.add date
   ;; 2.support select directory, see https://github.com/org-roam/org-roam/issues/888
   ;;   example, tip dir and input: books/, will be create note in sub dir.
   (setq org-roam-capture-templates '(("d" "default" plain "%?" :target
                                      (file+head "${dir}%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n- tags :: \n")
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

   ;; 避免被隐藏
   ;; for org-roam-buffer-toggle
   ;; Use side-window like V1
   ;; This can take advantage of slots available with it
   (add-to-list 'display-buffer-alist
                '("\\*org-roam\\*"
                  (display-buffer-in-side-window)
                  (side . right)
                  (slot . 0)
                  (window-width . 0.25)
                  (preserve-size . (t . nil))
                  (window-parameters . ((no-other-window . t)
                                        (no-delete-other-windows . t)))))

   (progn
     ;; https://github.com/org-roam/org-roam/wiki/User-contributed-Tricks#showing-the-number-of-backlinks-for-each-node-in-org-roam-node-find
     (cl-defmethod org-roam-node-directories ((node org-roam-node))
       (if-let ((dirs (file-name-directory (file-relative-name (org-roam-node-file node) org-roam-directory))))
           (format "(%s)" (car (split-string dirs "/")))
         ""))

     (cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
       (let* ((count (caar (org-roam-db-query
                            [:select (funcall count source)
                                     :from links
                                     :where (= dest $s1)
                                     :and (= type "id")]
                            (org-roam-node-id node)))))
         (format "[%d]" count)))

     (setq org-roam-node-display-template "${directories:10} ${tags:10} ${title:100} ${backlinkscount:6}")


     ;; Run org-roam-db-sync when Emacs is idle
     (defvar my/auto-org-roam-db-sync--timer nil)
     (defvar my/auto-org-roam-db-sync--timer-interval 5)

     (define-minor-mode my/auto-org-roam-db-sync-mode
       "Toggle automatic `org-roam-db-sync' when Emacs is idle.
Referece: `auto-save-visited-mode'"
       :group 'org-roam
       :global t
       (when my/auto-org-roam-db-sync--timer (cancel-timer my/auto-org-roam-db-sync--timer))
       (setq my/auto-org-roam-db-sync--timer
             (when my/auto-org-roam-db-sync-mode
               (run-with-idle-timer
                my/auto-org-roam-db-sync--timer-interval :repeat
                #'org-roam-db-sync))))

     )

   )
 )


(eye/use-package
 'org-roam-ui
 :load-path '("org-roam-ui" "emacs-web-server" "emacs-websocket" "org-roam/extensions")
 :ensure t
 :config
 (setq org-roam-ui-sync-theme t
       org-roam-ui-follow t
       org-roam-ui-update-on-save t
       org-roam-ui-open-on-start t)
 )


(provide 'init-org-roam)
