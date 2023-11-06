(eye/use-package 'org-ql
                 :ensure t
                 :load-path '("ts" "peg" "ht" "ov" "org-ql" "org-super-agenda")
                 :config
                 (progn
                   (require 'org-ql)
                   (require 'org-ql-search)
                   (require 'org-ql-find)
                   (require 'org-ql-view)
                   ))

;; (setq org-ql-search-directories-files-recursive nil)


;; (org-ql-query
;;   :select #'org-get-heading
;;   :from "d:/home/dropbox/Dropbox/tsnotes/journals/2023-11.org"
;;   :where '(and (property "CUSTOM_ID")
;;                (string-match "issue" (org-entry-get (point) "CUSTOM_ID"))))

(provide 'init-org-ql)
