;;;; which-key
(eye/use-package
 'which-key
 :load-path '("dash" "s" "f" "emacs-which-key")
 :command '((which-key-mode . "which-key"))
 :config
 (progn
   (setq which-key-popup-type 'frame)
   (which-key-setup-side-window-bottom)
   (defalias 'wkey 'which-key-add-key-based-replacements)
   ))

;; hydra
(eye/use-package 'hydra
                 :load-path "hydra"
                 )

(eye/use-package 'keyfreq
                 :load-path "keyfreq"
                 :command '(keyfreq-mode)
                 :init (add-hook 'after-init-hook #'keyfreq-mode)
                 :config
                 (progn
                   (setq keyfreq-excluded-commands
                         '(self-insert-command
                           forward-char
                           backward-char
                           previous-line
                           next-line
                           mwheel-scroll
                           ))
                   )
                 )

;;;; alias
(defalias 'kill 'kill-emacs)
(defalias 'eb 'eval-buffer)


(defun eye-bind-ctrl-key (mode-map keyseq func &optional custom-name)
  (let* (keycode)
    (mapcar (lambda (c)
              (setq keycode (concat keycode "C-" (char-to-string c) " "))
              )
            keyseq)
    (bind-key mode-map keycode func)
    ;; (if custom-name
    ;;     (which-key-add-key-based-replacements keycode custom-name))
    ))

(defun eye-empty-func ()
  (interactive)
  (user-error "Empty function"))

(defun unset-mode-key (modemap kbd)
  (define-key modemap kbd nil))


;;;; cua mode
;; C-c:copy, C-v:paste, C-x:cut, C-z:undo
(cua-mode 1)

;; unset some keys
(bind-key global-map "C-x C-c" 'ignore) ;; 不使用这个快捷键退出

;;;; setup keys
(defun eye-set-keys (modemap)
  ;; quick key setup
  (bind-key modemap "<f1>" #'my-switch-buffer)
  (bind-key modemap "<f5>" #'revert-buffer)
  (bind-key modemap "<f3>" #'goto-line)
  (bind-key modemap "<f5>" #'revert-buffer)
  (bind-key modemap "<f6>" 'eye/org-add-sibling-headline)
  (bind-key modemap "<f7>" 'eye/org-add-child-headline)
  (bind-key modemap "<f9>" #'(lambda ()
                               (interactive)
                               (delete-other-windows)
                               (org-agenda nil "v")))
  (bind-key modemap "<f10>" #'eye/agenda-show-projects)
  (bind-key prog-mode-map "<backspace>" #'intellij-backspace)

  (bind-key modemap "M-o" #'other-window)
  (bind-key modemap "M-j" #'xah-extend-selection)
  (bind-key modemap "M-d" #'delete-char)
  (bind-key modemap "M-k" #'kill-line)
  (bind-key modemap "M-i" #'eye/imenu)
  (bind-key modemap "M-<down>" #'forward-paragraph)
  (bind-key modemap "M-<up>" #'backward-paragraph)
  (bind-key modemap "M-a" #'beginning-of-line)
  (bind-key modemap "M-e" #'end-of-line)
  (bind-key modemap "M-/" #'xah-comment-dwim)


  (unset-mode-key modemap (kbd "C-a"))
  (unset-mode-key modemap (kbd "C-b"))
  (unset-mode-key modemap (kbd "C-d"))
  (unset-mode-key modemap (kbd "C-e"))
  (unset-mode-key modemap (kbd "C-f"))
  (unset-mode-key modemap (kbd "C-w"))
  (unset-mode-key modemap (kbd "C-t"))

  ;; aa:search
  (eye-bind-ctrl-key modemap "aac" #'eye-occur)
  (eye-bind-ctrl-key modemap "aas" #'swiper)
  (eye-bind-ctrl-key modemap "aaf" #'isearch-forward)
  (eye-bind-ctrl-key modemap "aab" #'isearch-backward)
  (eye-bind-ctrl-key modemap "aad" #'eye-search-in-dir)
  ;;(wkey "C-a C-a" "search")

  ;; ab:buffer
  (eye-bind-ctrl-key modemap "aba" #'bookmark-set)
  (eye-bind-ctrl-key modemap "abj" #'bookmark-jump)
  (eye-bind-ctrl-key modemap "abo" #'xah-open-file-fast)
  (eye-bind-ctrl-key modemap "abl" #'bookmark-bmenu-list)
  (eye-bind-ctrl-key modemap "abs" #'bookmark-bmenu-save)
  ;;(wkey "C-a C-b" "bookmark")

  ;; ac:eno/copy
  (eye-bind-ctrl-key modemap "acw" #'eno-word-copy)
  (eye-bind-ctrl-key modemap "acs" #'eno-symbol-copy)
  (eye-bind-ctrl-key modemap "acf" #'eye/copy-file-name)
  (eye-bind-ctrl-key modemap "acp" #'xah-copy-file-path "copy path")
  ;;(wkey "C-a C-c" "eno/copy")

  ;; bb:buffer
  (eye-bind-ctrl-key modemap "bba" #'beginning-of-buffer)
  (eye-bind-ctrl-key modemap "bbe" #'end-of-buffer)
  (eye-bind-ctrl-key modemap "bbk" #'kill-this-buffer)
  (eye-bind-ctrl-key modemap "bbf" #'narrow-to-defun)
  (eye-bind-ctrl-key modemap "bbr" #'narrow-to-region)
  (eye-bind-ctrl-key modemap "bbl" #'org-narrow-to-block)
  (eye-bind-ctrl-key modemap "bbo" #'org-narrow-to-subtree)
  (eye-bind-ctrl-key modemap "bbs" #'save-buffer)
  (eye-bind-ctrl-key modemap "bbw" #'eye/swap-buffer)
  (eye-bind-ctrl-key modemap "bbq" #'widen)
  ;;(wkey "C-b C-b" "buffer/narrow")

  ;; buffer and file
  (eye-bind-ctrl-key modemap "ffo" 'my-find-file "find file")
  (eye-bind-ctrl-key modemap "ffs" 'eye/open-project-file)
  (eye-bind-ctrl-key modemap "ffd" 'eye/find-project-file-in-dir)
  (eye-bind-ctrl-key modemap "ffk" 'kill-this-buffer)
  (eye-bind-ctrl-key modemap "ffc" 'xah-open-file-at-cursor)
  (eye-bind-ctrl-key modemap "ffe" 'eye/open-svn-or-git-file)
  (eye-bind-ctrl-key modemap "fff" 'xah-open-file-fast)
  (eye-bind-ctrl-key modemap "ffr" 'counsel-buffer-or-recentf)
  (eye-bind-ctrl-key modemap "ffn" 'eye/create-tmp-buffer)
  ;;(wkey "C-f C-f" "file")

  (eye-bind-ctrl-key modemap "fdj" 'dired-jump)
  ;;(wkey "C-f C-d" "dired")

  ;; window
  (eye-bind-ctrl-key modemap "2" 'split-window-below "split hor")
  (eye-bind-ctrl-key modemap "3" 'split-window-right "split ver")
  (eye-bind-ctrl-key modemap "1" 'delete-other-windows)
  (eye-bind-ctrl-key modemap "0" 'delete-window)
  ;;(wkey "C-w C-w" "window")

  (eye-bind-ctrl-key modemap "wf1" 'make-frame-command "new frame")
  (eye-bind-ctrl-key modemap "wfq" 'other-frame)
  ;;(wkey "C-w C-f" "frame")

  (eye-bind-ctrl-key modemap "wtn" 'tab-new)
  (eye-bind-ctrl-key modemap "wtc" 'tab-close)
  (eye-bind-ctrl-key modemap "wtr" 'tab-rename)
  (eye-bind-ctrl-key modemap "wts" 'tab-bar-select-tab-by-name)
  ;; ;;(wkey "C-w C-t" "tab")
  (eye-bind-ctrl-key modemap "ww" 'holo-layer-jump-to-window)

  (eye-bind-ctrl-key modemap "ws" 'shrink-window)
  (eye-bind-ctrl-key modemap "we" 'enlarge-window)

  (eye-bind-ctrl-key modemap "tt" 'centaur-tabs-mode)
  (eye-bind-ctrl-key modemap "tc" 'treemacs)
  (eye-bind-ctrl-key modemap "tl" 'global-display-line-numbers-mode "linenum")
  (eye-bind-ctrl-key modemap "tm" 'menu-bar-mode)
  (eye-bind-ctrl-key modemap "ts" 'scroll-bar-mode)
  (eye-bind-ctrl-key modemap "tw" 'which-key-mode)
  (eye-bind-ctrl-key modemap "ti" 'ivy-posframe-mode)
  (eye-bind-ctrl-key modemap "tn" 'toggle-truncate-lines)
  (eye-bind-ctrl-key modemap "tr" 'global-readonly-toggle)
  (eye-bind-ctrl-key modemap "te" 'counsel-load-theme) ;; 比load-theme更好用，且会先disable theme再load theme。
  ;; (eye-bind-ctrl-key modemap "ta" 'tab-bar-new-tab)
  (eye-bind-ctrl-key modemap "tc" 'eye-toggle-global-company)
  (eye-bind-ctrl-key modemap "thk" 'lazy-highlight-cleanup)
  (eye-bind-ctrl-key modemap "thc" 'highlight-changes-mode)
  (eye-bind-ctrl-key modemap "thp" 'rainbow-delimiters-mode)


  (eye-bind-ctrl-key modemap "err" 'replace-rectangle)
  (eye-bind-ctrl-key modemap "erk" 'kill-rectangle)
  ;;(wkey "C-e C-r" "rectangle")

  (eye-bind-ctrl-key modemap "dbt" 'bm-toggle)
  (eye-bind-ctrl-key modemap "dbn" 'bm-next)
  (eye-bind-ctrl-key modemap "dbp" 'bm-previous)
  (eye-bind-ctrl-key modemap "dbs" 'counsel-bm)
  ;;(wkey "C-d C-b" "bm")

  (eye-bind-ctrl-key modemap "dcl" 'counsel-etags-list-tag "list tag")
  (eye-bind-ctrl-key modemap "dcd" 'counsel-etags-find-tag-at-point "find tag at point")
  (eye-bind-ctrl-key modemap "dcf" 'counsel-etags-find-tag "find tag")
  (eye-bind-ctrl-key modemap "dcr" 'counsel-etags-recent-tag "recent tag")
  (eye-bind-ctrl-key modemap "dc0" 'eye/create-ctags-file-by-cmd "create tags file")
  (eye-bind-ctrl-key modemap "dc1" 'eye-find-h-or-c "find h/c file")
  (eye-bind-ctrl-key modemap "dct" 'goto-line)
  ;;(wkey "C-d C-c" "code navigate")

  (eye-bind-ctrl-key modemap "dsd" 'lsp-bridge-find-def)
  (eye-bind-ctrl-key modemap "dsq" 'lsp-bridge-find-def-return)
  (eye-bind-ctrl-key modemap "dsr" 'lsp-bridge-find-references)
  ;;(wkey "C-d C-s" "lsp-bridge")

  (eye-bind-ctrl-key modemap "dom" 'outline-minor-mode)
  (eye-bind-ctrl-key modemap "dos" 'outline-show-entry)
  (eye-bind-ctrl-key modemap "doh" 'outline-hide-entry)
  (eye-bind-ctrl-key modemap "don" 'outline-next-heading)
  (eye-bind-ctrl-key modemap "dop" 'outline-previous-heading)
  (eye-bind-ctrl-key modemap "doa" 'outline-show-all)
  ;;(wkey "C-d C-o" "outline")

  (unset-mode-key modemap (kbd "C-o"))
  (eye-bind-ctrl-key modemap "ony" 'org-toggle-link-display "toggle link")
  (eye-bind-ctrl-key modemap "onu" 'eye/open-org-file-attach-dir "open attach dir")
  (eye-bind-ctrl-key modemap "oni" 'org-redisplay-inline-images);
  (eye-bind-ctrl-key modemap "ono" 'eye/org-insert-src-block "insert code")
  (eye-bind-ctrl-key modemap "onp" 'my-anchor-insert "insert anchor")
  (eye-bind-ctrl-key modemap "onh" 'eye/paste-image-from-clipboard "past img")
  ;; (eye-bind-ctrl-key modemap "onj" 'eye/org-new-file "new note")
  (eye-bind-ctrl-key modemap "onj" 'eye/add-journal "add journal")
  ;; (eye-bind-ctrl-key modemap "onk" 'eye/org-new-file-by-date "new note(date)")
  (eye-bind-ctrl-key modemap "onl" 'eye/open-org-note-file "search(file)")
  (eye-bind-ctrl-key modemap "on;" 'eye/insert-pos-link "insert ref")
  (eye-bind-ctrl-key modemap "onf" 'eye/open-attach-file "open attach file")
  (eye-bind-ctrl-key modemap "onn" 'eye/get-org-id-url "get id url")
  (eye-bind-ctrl-key modemap "onm" 'eye/open-ref-side-window "show ref side")
  ;; (eye-bind-ctrl-key modemap "on," 'eye/search-by-tag "search(tag)")
  ;; (eye-bind-ctrl-key modemap "on." 'eye/note-index "open index")
  (eye-bind-ctrl-key modemap "on/" 'eye-export-html-and-open "export html")
  ;; (eye-bind-ctrl-key modemap "ona" 'eye/org-set-anchor "set anchor")
  ;; (eye-bind-ctrl-key modemap "one" 'eye/org-set-link "set link")

  ;; mynote
  (eye-bind-ctrl-key modemap "ona" 'mynote-copy-ref)
  (eye-bind-ctrl-key modemap "one" 'mynote-paste-ref)
  (eye-bind-ctrl-key modemap "onr" 'mynote-update-ref "update page reference")
  (eye-bind-ctrl-key modemap "ons" 'counsel-rg)

  (eye-bind-ctrl-key modemap "ond" 'org-toggle-archive-tag "set archive")
  (eye-bind-ctrl-key modemap "onb" 'eye-open-image-at-point "open image")
  (eye-bind-ctrl-key modemap "onc" 'eye-copy-file-at-point "copy file")
  (eye-bind-ctrl-key modemap "onw" 'eye/copy-org-link-at-point "copy link")

  (eye-bind-ctrl-key modemap "on[" 'eye/create-ref-link "create ref link")

  (eye-bind-ctrl-key modemap "ooa" 'org-agenda "agenda")
  (eye-bind-ctrl-key modemap "ooc" 'org-capture "capture")
  (eye-bind-ctrl-key modemap "ood" 'calendar)
  (eye-bind-ctrl-key modemap "ooe" 'eye/open-file-manager)

  (eye-bind-ctrl-key modemap "oou" 'eye/list-bookmarks)
  (eye-bind-ctrl-key modemap "ooj" 'eye/ts-open-bookmark)
  (eye-bind-ctrl-key modemap "ooy" 'bing-dict-brief)
  ;;(wkey "C-o C-n" "note")
  ;;(wkey "C-o C-o" "agenda")

  (eye-bind-ctrl-key modemap "otn" 'org-table-next-row)
  (eye-bind-ctrl-key modemap "otcr" 'org-table-insert-row)
  (eye-bind-ctrl-key modemap "otcl" 'org-table-insert-hline)
  (eye-bind-ctrl-key modemap "otcc" 'org-table-insert-column)
  (eye-bind-ctrl-key modemap "otkr" 'org-table-kill-row)
  (eye-bind-ctrl-key modemap "otkc" 'org-table-delete-column)
  ;;(wkey "C-o C-t" "org-table")
  ;;(wkey "C-o C-t C-k" "remove")
  ;;(wkey "C-o C-t C-c" "create")

  (eye-bind-ctrl-key modemap "olh" 'outline-hide-entry)
  ;;(wkey "C-o C-l" "outline")


  (eye-bind-ctrl-key modemap "ohh" 'symbol-overlay-put)
  (eye-bind-ctrl-key modemap "ohp" 'symbol-overlay-jump-prev)
  (eye-bind-ctrl-key modemap "ohn" 'symbol-overlay-jump-next)
  (eye-bind-ctrl-key modemap "ohr" 'symbol-overlay-rename)
  (eye-bind-ctrl-key modemap "ohk" 'symbol-overlay-remove-all)
  ;;(wkey "C-o C-h" "overlay highlight")

  ;; denote
  ;; (eye-bind-ctrl-key modemap "ojj" #'denote) ; our custom command
  ;; (eye-bind-ctrl-key modemap "ojn" #'denote-link)
  ;; (eye-bind-ctrl-key modemap "ojo" #'denote-open-or-create)
  ;; (eye-bind-ctrl-key modemap "ojs" #'denote-subdirectory)
  ;; (eye-bind-ctrl-key modemap "ojr" #'denote-rename-file)
  (eye-bind-ctrl-key modemap "ojf" #'org-ql-find) ;; current buffer
  (eye-bind-ctrl-key modemap "oja" #'org-ql-find-in-agenda)
  (eye-bind-ctrl-key modemap "ojn" #'org-ql-find-in-org-directory)

  (eye-bind-ctrl-key modemap "oud" #'org-hugo-export-to-md)
  (eye-bind-ctrl-key modemap "ouv" #'org-hugo-export-as-md)


  (eye-bind-ctrl-key modemap "oif" #'org-roam-node-find)
  (eye-bind-ctrl-key modemap "oii" #'org-roam-node-insert)
  (eye-bind-ctrl-key modemap "oiv" #'org-roam-node-visit)
  (eye-bind-ctrl-key modemap "oir" #'org-roam-node-random)
  (eye-bind-ctrl-key modemap "oib" #'org-roam-buffer-toggle)
  (eye-bind-ctrl-key modemap "ois" #'org-roam-db-sync)
  (eye-bind-ctrl-key modemap "oiu" #'org-roam-ui-mode)
  (eye-bind-ctrl-key modemap "oit" #'org-roam-ui-follow-mode)
  (eye-bind-ctrl-key modemap "oid" #'org-id-get-create)
  (eye-bind-ctrl-key modemap "oik" #'eye/org-roam-insert-tag)

  (unset-mode-key modemap (kbd "C-7"))
  (eye-bind-ctrl-key modemap "77" 'xah-extend-selection)
  (eye-bind-ctrl-key modemap "78" 'xah-select-current-line)
  (eye-bind-ctrl-key modemap "79" 'xah-select-text-in-quote)
  (eye-bind-ctrl-key modemap "70" 'mark-whole-buffer)

  (bind-key modemap "<C-down-mouse-1>" 'counsel-etags-find-tag-at-point)

  ;; (bind-key modemap "<C-tab>" 'tab-bar-switch-to-next-tab)
  ;; (bind-key modemap "<C-S-tab>" 'tab-bar-switch-to-prev-tab)
  ;; (bind-key modemap "C-S-<iso-lefttab>" 'tab-bar-switch-to-prev-tab)

  (bind-key modemap "<C-tab>" 'sort-tab-select-next-tab)
  (bind-key modemap "<C-S-tab>" 'sort-tab-select-prev-tab)
  (bind-key modemap "C-S-<iso-lefttab>" 'sort-tab-select-prev-tab)


  (bind-key modemap "M-x" 'my-M-x)
  (bind-key modemap "M-z" 'blink-search)
  (bind-key modemap "C-x C-f" 'my-find-file)
  (bind-key modemap "C-x b" 'my-switch-buffer)
  (bind-key modemap "M-SPC" 'set-mark-command)

  (bind-key modemap "C-c C-j" 'org-journal-new-entry)

  (unset-mode-key modemap (kbd "<mouse-2>")) ;; 鼠标中键，不使用

  (bind-key modemap "M-;" 'yas-expand)

  )

(eye-set-keys global-map)
(add-hook 'dired-mode-hook (lambda () (eye-set-keys dired-mode-map)))
(add-hook 'c-mode-hook (lambda () (eye-set-keys c-mode-map)))
(add-hook 'c++-mode-hook (lambda () (eye-set-keys c++-mode-map)))
(add-hook 'python-mode-hook (lambda () (eye-set-keys python-mode-map)))
(add-hook 'java-mode-hook (lambda () (eye-set-keys java-mode-map)))
(add-hook 'deft-mode-hook (lambda () (eye-set-keys deft-mode-map)))

;; 永远不使用overwrite模式，避免按错
(progn
  (global-unset-key (kbd "<insertchar>"))
  (global-unset-key (kbd "<insert>")))

(global-set-key (kbd "<C-left>")  'windmove-left)
(global-set-key (kbd "<C-right>") 'windmove-right)
(global-set-key (kbd "<C-up>")    'windmove-up)
(global-set-key (kbd "<C-down>")  'windmove-down)
(global-set-key (kbd "<C-M-left>") 'xah-previous-user-buffer)
(global-set-key (kbd "<C-M-right>") 'xah-next-user-buffer)
(global-set-key (kbd "C-S-n") 'make-frame-command)
(global-set-key (kbd "C-S-f") 'eye-search-in-dir)
(global-set-key (kbd "<f6>") 'org-agenda)
(global-set-key (kbd "<next>") 'scroll-half-page-up)
(global-set-key (kbd "<prior>") 'scroll-half-page-down)

(bind-key messages-buffer-mode-map "C-k C-k" #'clear-messages-buffer)

;; tab
;; (global-set-key (kbd "<C-prior>") 'centaur-tabs-backward) ;; C- page up
;; (global-set-key (kbd "<C-next>") 'centaur-tabs-forward) ;; C- page down
;; (global-set-key (kbd "<C-S-next>") 'centaur-tabs-counsel-switch-group) ;; C- page down

(global-set-key (kbd "<C-prior>") 'tab-bar-switch-to-prev-tab) ;; C- page up
(global-set-key (kbd "<C-next>") 'tab-bar-switch-to-next-tab) ;; C- page down
(global-set-key (kbd "<C-S-prior>") 'tab-bar-new-tab)
(global-set-key (kbd "<C-S-next>") 'tab-bar-select-tab-by-name) ;; C- page down

;; (global-set-key (kbd "<C-insert>") 'emms-stop) ;; C- insert

;; menu
(setq right-popup-menu
  (let ((menu (make-sparse-keymap "Commands")))
    (define-key menu [undo] (cons "Undo" 'undo))
    (define-key menu [redo] (cons "Redo" 'redo))
    (define-key menu [save-buffer] (cons "Save buffer" 'save-buffer))
    (define-key menu [mark-whole-buffer] (cons "Select all" 'mark-whole-buffer))
    (define-key menu [goto-line] (cons "Goto" 'goto-line))
    (define-key menu [xah-new-empty-buffer] (cons "New buffer" 'xah-new-empty-buffer))
    (define-key menu [yank] (cons "Paste" 'yank))
    (define-key menu [copy-region-as-kill] (cons "Copy" 'copy-region-as-kill))
    menu))

(defun right-popup-command ()
  "Run the command selected fromright-popup-menu'."
  (interactive)
  (call-interactively (or (car (x-popup-menu t right-popup-menu)))))
(global-set-key [mouse-3] 'right-popup-command)


;; 注意，没有加载的mode，会报错，导致后面的设置不生效
(with-eval-after-load 'org
  (eye-set-keys org-mode-map)
  (bind-key org-mode-map "M-RET" 'org-insert-heading-respect-content)
  (bind-key org-mode-map "C-k" nil)
  (bind-key org-mode-map "C-c '" 'org-edit-src-code)
  (bind-key org-mode-map "<S-return>" 'eye/org-new-entry)
  (bind-key org-mode-map "M-<up>" #'org-move-subtree-up)
  (bind-key org-mode-map "M-<down>" #'org-move-subtree-down)

  )


(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-i") 'company-select-previous)
  (define-key company-active-map (kbd "M-k") 'company-select-next)
  (define-key company-active-map (kbd "<tab>") 'company-select-next)
  (define-key company-active-map (kbd "TAB") 'company-select-next)
  (define-key company-active-map (kbd "<S-tab>") 'company-select-previous))

(provide 'init-keys)
