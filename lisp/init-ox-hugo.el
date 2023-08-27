(eye/use-package
 'ox-hugo
 :ensure t
 :load-path '("tomelr" "ox-hugo")
 :config
 (progn

   ))

(defun eye/insert-hugo-header ()
  (interactive)
     (insert "
#+hugo_base_dir: e:/home/Dropbox/hugo-blog/myblog
#+hugo_auto_set_lastmod: t
#+hugo_custom_front_matter: :author "owen"
#+hugo_custom_front_matter: :summary "sort description"
#+hugo_tags: draft
#+hugo_categories:
#+options: \n:t
"))

(provide 'init-ox-hugo)
