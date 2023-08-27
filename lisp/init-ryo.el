;; 由于模式编辑和输入法切换不顺畅，暂不使用
;; ryo-modal-mode绑定的函数必须是已加载的，不能使用autoload在按键时再加载的方式
;; 导致启动耗时
;;
(require 'which-key)
(defalias 'wkey 'which-key-add-key-based-replacements)


(bind-key global-map "<f5>" 'revert-buffer)
(bind-key global-map "<f3>" 'repeat)
(bind-key global-map "<f8>" 'org-agenda)
(bind-key global-map "<f9>" 'eye/agenda-simple-view)
(bind-key global-map "<f10>" 'eye/agenda-show-projects)
(bind-key global-map "C-x C-c" 'ignore)
(bind-key prog-mode-map "<backspace>" #'intellij-backspace)
(bind-key global-map "C-z" 'undo)


(eye/use-package 'hydra
                 :ensure t
                 :load-path "hydra"
                 )


(setq leader-help-buffer-name "*leader-help*")
(setq leader-help-content "leader help:")
(setq leader-help-is-show nil)

(defun leader-help-close ()
  (interactive)
  (posframe-hide leader-help-buffer-name)
  (setq leader-help-is-show nil)
  )

(define-minor-mode leader-help-mode
  "Get your foos in the right places."
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "q") 'eye-close-leader-help-frame)
            map))

(defun leader-help-show ()
  (interactive)
  (let* ((name "*leader-help*")
        (buffer (generate-new-buffer leader-help-buffer-name)))
    (with-current-buffer leader-help-buffer-name
      (leader-help-mode t)
      (erase-buffer)
      (insert leader-help-content))
    (posframe-show name
                   :poshandler 'posframe-poshandler-frame-center
                   :background-color "#cdcdcd"
                   )
    (setq leader-help-is-show t)
    )
  )

(defun leader-help ()
  (interactive)
  (if leader-help-is-show
      (leader-help-close)
    (leader-help-show))
  )

(defun leader-help-content-append (str)
  (setq leader-help-content (concat leader-help-content str)))

(setq leader-help-content "\n leader help:\n")
(leader-help-content-append
 "
 a: [aa] search   [ab] bookmark    [ac] eno/copy    [as] aweshell
 b: [bb] buffer
 d: [dj] dired    [ds] lsp-bridge  [dv] outline     [dg] git
 f: [ff] file
 r: [rr] replace/rectangle
 w: [ww] window/frame  [wt] tab
 t: [tt] toggle mode
 o: [oa] agenda  [od] denote [oh] symbol-overlay
 y: [yy] misc
 "
 )

(eye/use-package
 'ryo-modal
 :ensure t
 :load-path "ryo-modal"
 :config
 (progn
   (setq ryo-modal-cursor-type 'box)
   (setq ryo-modal-cursor-color nil)
   (dolist (hook '(text-mode-hook prog-mode-hook fundamental-mode-hook conf-unix-mode-hook help-mode-hook))
     (add-hook hook #'ryo-modal-mode))

   (setq leader-key ",")
   (defmacro ryo-leader (keys)
     `(ryo-modal-key leader-key ,keys)
     )
   (defalias 'ryo-key 'ryo-modal-keys)

   (defun eye-enable-ryo-modal-mode () (interactive) (ryo-modal-mode t))

   (bind-key global-map "M-SPC" #'ryo-modal-mode)
   (bind-key global-map "<f1>" 'ryo-modal-mode)
   (bind-key global-map "<insert>" #'eye-enable-ryo-modal-mode)

   ;; 使ryo定义的name在which key中生效
   (push '((nil . "ryo:.*:") . (nil . "")) which-key-replacement-alist)

   (add-hook 'find-file-hook #'eye-enable-ryo-modal-mode)

   ;; default keys
   (ryo-key
    ("q" my-switch-buffer)
    ("w" other-window)
    ("e" eye-empty-func)
    ("r" eye-empty-func)
    ("t" eye-empty-func)

    ("a" eye-empty-func)
    ("s" xah-extend-selection)
    ("d" delete-char)
    ("f" ryo-modal-mode)
    ("g" eye-empty-func)

    ("z" undo)
    ("x" kill-region)
    ("c" kill-ring-save)
    ("v" yank)
    ("b" eye-empty-func)

    ("y" eye-empty-func)
    ("u" backward-word)
    ("i" previous-line)
    ("o" forward-word)
    ("p" backward-paragraph)

    ("h" beginning-of-line)
    ("j" backward-char)
    ("k" next-line)
    ("l" forward-char)
    (";" end-of-line)

    ("n" forward-paragraph)
    ("m" set-mark-command)
    ("." ryo-modal-repeat)
    ("/" xah-comment-dwim)
    )


   (ryo-key
    ;; 使ryo model下直接按数字时触发Alt+数字的效果
    (:norepeat t)
    ("0" "M-0")
    ("1" "M-1")
    ("2" "M-2")
    ("3" "M-3")
    ("4" "M-4")
    ("5" "M-5")
    ("6" "M-6")
    ("7" "M-7")
    ("8" "M-8")
    ("9" "M-9"))

   (ryo-leader
    '(
      ("/" leader-help)
      ;; search
      ("aa" (
             ("c" eye-occur)
             ("s" swiper)
             ("f" isearch-forward)
             ("b" isearch-backward)
             ("d" eye-search-in-dir)
             ))
      ;; bookmark
      ("ab" (
             ("a" bookmark-set)
             ("j" bookmark-jump)
             ("o" xah-open-file-fast) ;; open bookmark
             ("l" bookmark-bmenu-list)
             ("s" bookmark-bmenu-save)
             ))
      ("ac" (
             ("w" eno-word-copy)
             ("s" eno-symbol-copy)
             ))
      ;; eshell
      ("as" (
             ("t" aweshell-toggle)
             ("c" aweshell-new)
             ("e" aweshell-next)
             ("w" aweshell-prev)
             ))
      ;; buffer
      ("bb" (
             ("a" beginning-of-buffer)
             ("e" end-of-buffer)
             ("k" kill-this-buffer)
             ("f" narrow-to-defun)
             ("g" narrow-to-region)
             ("l" org-narrow-to-block)
             ("o" org-narrow-to-subtree)
             ("s" save-buffer)
             ("w" eye/swap-buffer)
             ("q" widen)
             ))

      ("dc" (
             ("i" counsel-imenu :name "imenu") ;; C-i也是TAB
             ("l" counsel-etags-list-tag :name "list tag")
             ("d" counsel-etags-find-tag-at-point :name "find tag at point")
             ("f" counsel-etags-find-tag :name "find tag")
             ("r" counsel-etags-recent-tag :name "recent tag")
             ("0" eye/create-ctags-file-by-cmd :name "create tags file")
             ("h" eye-find-h-or-c :name "find h/c file")
             ("t" goto-line)
             ))

      ;; bm
      ("db" (
             ("t" bm-toggle)
             ("n" bm-next)
             ("p" bm-previous)
             ("j" counsel-bm :name "jump")
             ))

      ;; delete
      ("dd" (
             ("u" backward-kill-word)
             ("o" kill-word)
             ("l" kill-whole-line)
             ))

      ;; dired
      ("dj" (
             ("j" dired-jump)
             ("o" eye/open-file-manager :name "open folder")
             ))


      ;; lsp-bridge
      ("ds" (
             ("d" lsp-bridge-find-def)
             ("q" lsp-bridge-find-def-return)
             ("r" lsp-bridge-find-references)
             ))

      ;; outline
      ("dv" (
             ("a" outline-show-all)
             ("e" outline-hide-entry)
             ("m" outline-minor-mode)
             ("s" outline-show-entry)
             ("h" outline-hide-entry)
             ("n" outline-next-heading)
             ("p" outline-previous-heading)
             ))

      ;; magit
      ("dg" (
             ("b" eye/git-blame-file-or-region "blame file")
             ("c" eye/git-show-commit "show commit")
             ("s" magit-status)
             ))

      ;; file
      ("ff" (
             ("c" xah-open-file-at-cursor)
             ("d" eye/find-project-file-in-dir)
             ("e" eye/open-svn-or-git-file)
             ("n" eye/create-tmp-buffer)
             ("k" kill-this-buffer)
             ("o" my-find-file :name "find file")
             ("s" eye/open-project-file)
             ("f" xah-open-file-fast)
             ("r" counsel-buffer-or-recentf)
             ("D" xah-delete-current-file :name "delete") ;; 用大写，防止误删
             ))

      ;; replace
      ("rr" (
             ("a" query-replace)
             ("b" replace-string)
             ("c" kill-rectangle)
             ("d" replace-rectangle)
             ))

      ;; window/frame
      ("ww" (
             ("x" delete-window)
             ("1" delete-other-windows)
             ("2" split-window-below :name "split hor")
             ("3" split-window-right :name "split ver")
             ("c" make-frame-command)
             ("q" other-frame)
             ("<up>" windmove-up)
             ("<down>" windmove-down)
             ("<left>" windmove-left)
             ("<right>" windmove-right)
             ))
      ;; tab
      ("wt" (
             ("o" tab-bar-mode)
             ("t" tab-new)
             ("x" tab-close)
             ("n" tab-next)
             ("p" tab-previous)
             ("r" tab-rename)
             ("s" tab-bar-select-tab-by-name)
             ("l" sort-tab-select-next-tab "sortab next")
             ("j" sort-tab-select-prev-tab "sortab prev")
             ))

      ;; toggle
      ("tt" (
             ("a" symbol-overlay-mode)
             ("e" treemacs)
             ("l" global-display-line-numbers-mode :name "linenum")
             ("m" menu-bar-mode)
             ("s" scroll-bar-mode)
             ("t" toggle-truncate-lines)
             ("r" global-readonly-toggle)
             ("d" counsel-load-theme) ;; 比load-theme更好用，且会先disable theme再load theme。
             ("k" lazy-highlight-cleanup)
             ("h" highlight-changes-mode)
             ("i" rainbow-delimiters-mode)
             ("g" org-toggle-link-display :name "toggle link(org)")
             ("b" org-redisplay-inline-images :name "toggle image(org)");

             ))

      ;; agenda
      ("oa" (
             ("a" org-agenda)
             ("c" org-capture)
             ("j" eye/add-journal)
             ("u" eye/get-org-id-url :name "get id url")
             ("p" eye/paste-image-from-clipboard :name "past img")
             ("b" eye/org-insert-src-block :name "code block")
             ("A" org-toggle-archive-tag :name "set archive")
             ("D" calendar)
             ))

      ;; denote
      ("od" (
             ("n" denote) ; our custom command
             ("o" denote-open-or-create)
             ("s" denote-subdirectory)
             ("l" denote-link-add-links)
             ("r" denote-rename-file)
             ))

      ;; symbol-overlay
      ("oh" (
             ("h" symbol-overlay-put)
             ("p" symbol-overlay-jump-prev)
             ("n" symbol-overlay-jump-next)
             ("r" symbol-overlay-rename)
             ("k" symbol-overlay-remove-all)
             ))

      ;; misc
      ("yy" (
             ("d" bing-dict-brief)
             ))

      ;; ("onp" my-anchor-insert :name "insert anchor")
      ;; ("onj" eye/org-new-file :name "new note")
      ;; ("onk" eye/org-new-file-by-date :name "new note(date)")
      ;; ("onl" eye/open-org-note-file :name "search(file)") ;;eye/search-org-file "search(file)")
      ;; ("on;" eye/insert-pos-link "insert ref")
      ;; ("onf" eye/open-attach-file :name "open attach file")

      ;; ("onm" eye/open-ref-side-window :name "show ref side")
      ;; ;; ("on," 'eye/search-by-tag "search(tag)")
      ;; ;; ("on." 'eye/note-index "open index")
      ;; ("on/" eye-export-html-and-open :name "export html")
      ;; ;; ("ona" 'eye/org-set-anchor "set anchor")
      ;; ;; ("one" 'eye/org-set-link "set link")

      ;; mynote
      ;; ("ona" mynote-copy-ref)
      ;; ("one" mynote-paste-ref)
      ;; ("onr" mynote-update-ref :name "update page reference")
      ;; ("ons" counsel-rg)


      ;; ("onb" eye-open-image-at-point :name "open image")
      ;; ("onc" eye-copy-file-at-point :name "copy file")
      ;; ("onw" eye/copy-org-link-at-point :name "copy link")

      ;; ("on[" eye/create-ref-link :name "create ref link")

      ;; ("oou" eye/list-bookmarks)
      ;; ("ooj" eye/ts-open-bookmark)

      ;; ("otn" org-table-next-row)
      ;; ("otcr" org-table-insert-row)
      ;; ("otcl" org-table-insert-hline)
      ;; ("otcc" org-table-insert-column)
      ;; ("otkr" org-table-kill-row)
      ;; ("otkc" org-table-delete-column)

      ))

   ;;
   ;; (wkey ",ww" "window")
   ;; (wkey ",wf" "frame")
   ;; (wkey ",a" "(a)search(b)bookmark(c)copy(s)shell")

   ;; (wkey ",aa" "search")
   ;; (wkey ",ab" "bookmark")
   ;; (wkey ",ac" "copy")
   ;; (wkey ",as" "shell")
   ;; (wkey ",d" "(b)bm")
   ;; (wkey ",db" "bm")
   ;;(wkey ",b")
   )
 )

(with-eval-after-load 'org
  (bind-key org-mode-map "M-RET" 'org-insert-heading-respect-content)
  (bind-key org-mode-map "C-k" nil)
  (bind-key org-mode-map "C-c '" 'org-edit-src-code)
  (bind-key org-mode-map "<f6>" 'eye/org-add-sibling-headline)
  (bind-key org-mode-map "<f7>" 'eye/org-add-child-headline)
  (bind-key org-mode-map "<S-return>" 'eye/org-new-entry)
  (bind-key org-mode-map "C-o" 'counsel-org-goto)
  )


(provide 'init-ryo)
