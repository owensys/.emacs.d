(require 'hydra)

;;; more hydra define
(defhydra hydra-help (:exit t :idle 1.0)
  ("v" describe-variable "Desc var")
  ("f" describe-function "Desc fun")
  ("k" describe-key "Desc key")
  ("a" describe-face "Desc face")
  ("b" describe-bindings "desc bindgings")
  ("m" describe-mode "desc mode")
  ("i" info "Info")
  ("c" list-colors-display "List colors")
  ("s" list-faces-display "List faces"))

;;;; rectangle
(defhydra hydra-rect (:idle 1.0)
  ("r" replace-rectangle "Replace rectangle" :exit t)
  ("k" kill-rectangle "Kill rectangle" :exit t))


(defhydra hydra-jump (:exit t :idle 1.0)
  ("SPC" keyboard-quit "quit")
  ("b" pop-to-mark-command "pop local mark" :exit nil)
  ("p" pop-global-mark "pop global mark" :exit nil)
  ("t" pop-tag-mark "pop tag mark")
  ("g" ace-jump-char-mode "Goto char")
  ("l" ace-jump-line-mode "Goto line")
  ("d" hydra-dumb-jump/body "dumb jump")
  )


(defhydra hydra-file (:exit t :idle 1.0)
  "
Open
[_o_] find file
[_h_] history file
[_f_] fast bookmark file
[_z_] last closed
[_t_] find in tags
[_g_] find in git

Other
[_d_] dired
[_s_] save
[_k_] close buffer
[_b_] set bookmark

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
"
  ;; ("a" switch-to-buffer "Switch buffer" nil)
  ;; ("l" bookmark-bmenu-list "List bookmark" nil)
  ;; ("p" xah-previous-user-buffer "Previous buffer" nil)
  ;; ("n" xah-next-user-buffer "Next buffer")  
  ;; ("a" ido-switch-buffer "Switch buffer")
  ;; ("o" ido-find-file "Find file"))
  ;; ("h" recentf-open-files nil)
  ;; ("k" kill-this-buffer nil)

  ("d" dired-jump nil)
  ("s" save-buffer nil)
  ("o" helm-find-files nil)
  ("z" xah-open-last-closed nil)
  ("k" xah-close-current-buffer nil)
  ("b" bookmark-set nil)
  ("f" xah-open-file-fast nil)
  ("t" find-file-in-tags nil) ;need tags-table-files
  ("a" counsel-ibuffer nil)
  ("g" counsel-git nil);查找在git仓库中的文件，注意最好子目录下没有.git目录，否则可能不会显示出文件列表
  ("h" helm-recentf nil)
  )

;;;; select
(defhydra hydra-select (:idle 1.0)
  ("SPC" keyboard-quit "quit" :exit t)
  ("a" mark-whole-buffer "Select all" :exit t)
  ("e" xah-extend-selection "Extend")
  ("q" xah-select-text-in-quote "Select quote" :exit t)
  ("l" xah-select-line "Select line" :exit t)
  ("b" xah-select-block "select block")
  ("n" narrow-to-region "Narrorw" :exit t)
  ("w" widen "widen" :exit t)
  )

;;;; delete
(defhydra hydra-delete (:idle 1.0)
  "delete:"
  ("SPC" nil "quit")
  ("d" delete-line-no-copy :exit t)
  ("l" delete-char)
  ("j" backward-delete-char)
  ("u" delete-inner-word-no-copy "backward word")
  ("o" delete-forward-word-no-copy "forward word")
  ("h" delete-beginning-of-line-no-copy "begin line" :exit t)
  (";" delete-end-of-line-no-copy "end line" :exit t)
  ("b" xah-delete-current-text-block "block" :exit t)
  )


;;;; window
(defhydra hydra-window (:idle 1.0)
  ("SPC" nil "quit")
  ("n" eye/new-frame)
  ("o" xah-next-window-or-frame "Next window/frame")
  ("0" delete-window-or-frame "Delete window/frame" :exit t)
  ("1" delete-other-windows "Delete other window" :exit t)
  ("3" split-window-horizontally "Split hor" :exit t)
  ("4" split-window-vertically "Split ver" :exit t))

;;;; search
(defhydra hydra-search (:idle 1.0)
  ("SPC" nil "quit" :exit t)
  ("s" occur "Occur" :exit t)
  ("f" isearch-forward "isearch-forward" :exit t)
  ("b" isearch-backward "isearch-backward" :exit t)
  ("q" query-replace "query-replace" :exit t)
  ("r" eye/replace-string-buffer "Replace all" :exit t)
  ("o" multi-occur-in-matching-buffers "Occur buffers" :exit t)
  )
(define-key isearch-mode-map (kbd "M-k") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "M-i") 'isearch-repeat-backward)


;;;; outline
(defhydra hydra-outline ()
  "
    _s_: outline show entry    _a_: outline show all    _n_: outline next heading      _t_: toggle children
    _h_: outline hide entry    _b_: outline hide body   _p_: outline previous heading
"
  ("SPC" nil "quit")
  ("s" outline-show-entry nil)
  ("h" outline-hide-entry nil)
  ("a" outline-show-all nil)
  ("b" outline-hide-body nil)
  ("n" outline-next-heading nil)
  ("p" outline-previous-heading nil)
  ("t" outline-toggle-children nil))

;;;; imenu
(defhydra hydra-imenu (:exit t :idle 1.0)
  ("SPC" nil "quit" :exit t))


;;;; funcs
(defhydra hydra-funcs (:idle 1.0)
  "
[_c_] capture
[_a_] agenda
[_p_] pop mark

Toggle mode:
[_l_] line number
[_r_] readonly
[_t_] truncate lines
[_g_] company
[_h_] highlight changes
[_v_] visual line

[_n_] Note

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
"
  ("SPC" nil "quit" :exit t)
  ("p" pop-global-mark :exit t)
  ("r" read-only-mode :exit t)
  ("l" global-display-line-numbers-mode)
  ("t" toggle-truncate-lines)
  ("g" global-company-mode :exit t)
  ("c" org-capture :exit t)
  ("a" org-agenda :exit t)
  ("n" hydra-note/body :exit t)
  ("h" highlight-changes-mode :exit t)
  ("v" global-visual-line-mode :exit t)
  )


(defhydra hydra-dired (:exit t)
  ("SPC" nil "quit")
  ("o" dired-w32-browser "open")
  ("e" dired-w32explorer "explorer"))


(defhydra hydra-elisp (:exit t)
  ("x" eval-last-sexp "Eval last")
  ("e" eval-expression "Eval exp")
  ("i" imenu "imenu")
  ("SPC" keyboard-quit "quit"))



(defhydra hydra-cpp (:exit t)
  "
_a_: list tags
"
  ("a" counsel-etags-list-tag)
  ("c" counsel-etags-scan-code "create TAGS")
  ("d" counsel-etags-find-tag-at-point "find tag at point")
  ("e" counsel-etags-find-tag "find tag")
  ("r" counsel-etags-recent-tag "recent tag")
  ("t" eye/update-ctags-this-file "update file tags")
  ("f" eye/find-header-or-source-file "find h or cpp")
  ("l" eye/load-project-root-tags "load root tags")
  ("s" eye/search-cpp-doc "cpp doc")
  ("g" eye/auto-compile "compile"))


(defhydra hydra-note (:exit t :idle 1.0)
  "
[_d_] open note dired
[_n_] new note
[_s_] search by keyword
[_f_] search by file name

[_t_] deft
"
  ("SPC" nil "quit")
  ("d" eye/notes-dired)
  ("n" eye/notes-new)
  ;; ("a" eye/notes-create-attachment "Create attach dir")
  ;; ("o" eye/notes-open-attachment "Open attach")
  ("s" eye/notes-search-keyword)
  ("f" eye/notes-search-file)
  ("t" deft-or-close)
  )


(defhydra hydra-org (:exit t)
  "
[_a_]: attach
[_gp_]: previous block

Clock:
[_ci_] in   [_co_] out   [_cr_] report   [_cc_] cancel

Insert:
[_il_] link    [_ia_] attach link   [_s_] src block    [_S_] subheading

Toggle:
[_tl_] display link   [_ti_] inline image

Wiki:
[_wi_] insert new       [_wk_] insert link     [_wc_] insert block
[_wo_] open at point    [_wn_] wiki nav
[_wu_] open url         [_wf_] open from url
[_we_] export page     

"
  ("SPC" nil "quit")
  ("ci" org-clock-in)
  ("co" org-clock-out)
  ("cr" org-clock-report)
  ("cc" org-clock-cancel)
  ("a" org-attach)
  ("gp" org-previous-block)
  ("ia" eye/insert-attach-link)
  ("il" org-insert-link)
  ("s" eye/org-insert-src-block)
  ("S" org-insert-subheading)
  ("tl" org-toggle-link-display)
  ("ti" org-toggle-inline-images)
  ("we" org-note-export-to-html)
  ("wi" org-note-new)
  ("wk" org-wiki-insert-link)
  ("wo" org-open-at-point)
  ;; ("wh" org-wiki-helm)
  ("wn" org-wiki-nav)
  ("wu" org-note-export-and-open)
  ("wf" org-wiki-from-url)
  ("wc" org-wiki-insert-block)
  )

(defhydra hydra-highlight ()
  "symbol-overlay"
  ("h" symbol-overlay-put "put")
  ("n" symbol-overlay-jump-next "next")
  ("p" symbol-overlay-jump-prev "prev")
  ("f" symbol-overlay-jump-first "first")
  ("l" symbol-overlay-jump-last "last")
  ("r" symbol-overlay-remove-all "remove all"))


(defhydra hydra-gtd ()
  "
Getting Thing Done system:

  [_ci_] capture收集  [_cr_] capture rx task
  [_vi_] 查看收集蓝（处理）    [_vt_] 查看任务（建立清单）    [_vo_] 查看TODO项（准备下一步行动）    [_vx_] 查看下一步行动

  [_a_] agenda     [_j_] journal file

"
  ("a" org-agenda nil :exit t)
  ("ci" (lambda () (interactive) (org-capture nil "i")) nil :exit t)
  ("cr" (lambda () (interactive) (org-capture nil "w")) nil :exit t)
  ("j" eye/open-journal-file nil :exit t)
  ("vi" (lambda () (interactive) (org-agenda nil "i")))
  ("vt" (lambda () (interactive) (org-agenda nil "t")))
  ("vo" (lambda () (interactive) (org-agenda nil "o")))
  ("vx" (lambda () (interactive) (org-agenda nil "x")))
)


(provide 'init-hydra)
