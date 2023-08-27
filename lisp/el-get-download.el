;;;; el-get-download.el --- My emacs package downloader -*- lexical-binding: t -*-
;;;;
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(load-file "~/.emacs.d/site-lisp/el-get-install.el")

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(setq el-get-user-package-directory "~/.emacs.d/el-get-init-files")

(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)
;; (package-refresh-contents)

;;;; f
(el-get-bundle 'f)
(el-get-bundle 's)
(el-get-bundle 'dash)
(el-get-bundle 'ctrlf)
(el-get-bundle 'pfuture)

(el-get-bundle whicy-key :url "https://github.com/justbur/emacs-which-key.git")

;;;; theme
(el-get-bundle naysayer-theme :url "https://github.com/nickav/naysayer-theme.el.git")

(el-get-bundle eldoc-eval)
(el-get-bundle doom-modeline)
(el-get-bundle all-the-icons)
(el-get-bundle shrink-path)
(el-get-bundle hydra :url "https://github.com/abo-abo/hydra")
(el-get-bundle emacs-memoize :url "https://github.com/skeeto/emacs-memoize")
(el-get-bundle awesome-tray :url "https://github.com/manateelazycat/awesome-tray")
(el-get-bundle swiper :url "https://github.com/abo-abo/swiper.git")
(el-get-bundle whicy-key :url "https://github.com/justbur/emacs-which-key.git")
(el-get-bundle sort-tab :url "https://github.com/manateelazycat/sort-tab.git")
(el-get-bundle devdocs :url "https://github.com/astoff/devdocs.el.git")
(el-get-bundle bing-dict :url "https://github.com/cute-jumper/bing-dict.el.git")
(el-get-bundle lsp-bridge :url "https://github.com/manateelazycat/lsp-bridge")
(el-get-bundle markdown-mode :url "https://github.com/jrblevin/markdown-mode")

(el-get-bundle denote :url "https://github.com/protesilaos/denote")

(el-get-bundle eno :url "https://github.com/enoson/eno.el.git")
(el-get-bundle edit-at-point :url "https://github.com/enoson/edit-at-point.el.git")
(el-get-bundle popper :url "https://github.com/karthink/popper.git")

(el-get-bundle aweshell :url "https://github.com/manateelazycat/aweshell.git")
(el-get-bundle ctrlf :url "https://github.com/raxod502/ctrlf.git")
(el-get-bundle super-save :url "https://github.com/bbatsov/super-save.git")

(el-get-bundle good-scroll :url "https://github.com/io12/good-scroll.el.git")

(el-get-bundle bm :url "https://github.com/joodland/bm.git")
(el-get-bundle color-rg :url "https://github.com/manateelazycat/color-rg.git")

(el-get-bundle smex :url "https://github.com/nonsequitur/smex.git")

(el-get-bundle posframe :url "https://github.com/tumashu/posframe.git")
(el-get-bundle ivy-posframe :url "https://github.com/tumashu/ivy-posframe.git")


(el-get-bundle htmlize :url "https://github.com/hniksic/emacs-htmlize.git")
(el-get-bundle notdeft :url "https://github.com/hasu/notdeft.git")
(el-get-bundle ts :url "https://github.com/alphapapa/ts.el.git")
(el-get-bundle posframeht :url "https://github.com/emacsmirror/ht.git")
(el-get-bundle org-super-agenda :url "https://github.com/alphapapa/org-super-agenda.git")


(el-get-bundle org-fancy-priorities "https://github.com/harrybournis/org-fancy-priorities.git")
(el-get-bundle org-bullets :url "https://github.com/sabof/org-bullets.git")


(el-get-bundle peg :url "https://github.com/emacsmirror/peg.git")
(el-get-bundle ov :url "https://github.com/emacsorphanage/ov.git")
(el-get-bundle transient :url "https://github.com/magit/transient.git")
(el-get-bundle org-sidebar :url "https://github.com/alphapapa/org-sidebar.git")


(el-get-bundle iscroll :url "https://github.com/casouri/iscroll.git")
(el-get-bundle writeroom-mode :url "https://github.com/joostkremers/writeroom-mode.git")
(el-get-bundle visual-fill-column :url "https://github.com/joostkremers/visual-fill-column.git")
(el-get-bundle org-tree-slide :url "https://github.com/takaxp/org-tree-slide.git")
(el-get-bundle deft :url "https://github.com/jrblevin/deft.git")


(provide 'el-get-download)
