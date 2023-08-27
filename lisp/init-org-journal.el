;;;; org-journal
;; System locale to use for formatting time values.
;; current is "zh_CN.UTF-8", if set to "C", Make sure that the weekdays in the
;; time stamps of your Org mode files and in the agenda appear in English.
;; @see https://github.com/batsibe/org-journal
(add-to-list 'load-path (concat eye-packages-dir "/org-journal"))
(require 'org-journal)
(setq org-journal-file-type 'daily)
(setq org-journal-dir (concat (get-locale-book-dir) "/journals"))
;; (setq org-journal-file-format "%Y-%m-%d.org")
;; (setq org-journal-date-format "%Y-%m-%d")
;; (setq org-journal-date-prefix "") ;; 使不要出现重复的headline

;; ;;自定义文件头信息
;; (setq org-journal-file-header #'my/org-journal-file-header)
;; (defun my/org-journal-file-header(tm)
;;   (format ":PROPERTIES:\n:ID:       %s\n:END:\n#+title: %s-%s\n\n"
;;           (my-generate-uuid)
;;           (format-time-string "%Y-%m-%d" tm)
;;           (eye-get-time-week tm)))



(setq org-journal-file-type 'yearly)
(setq org-journal-file-format "%Y.org")
(defun my/org-journal-date-format (tm)
  (format "* %s %s"
          (format-time-string "%Y-%m-%d" tm)
          (eye-get-time-week tm))
  )
(setq org-journal-date-format #'my/org-journal-date-format)

;;; usage
;; M-x
;; org-journal-new-entry
;; org-journal-new-date-entry

;; Calendar mode keys
;; jd view journal of selected day
;; jn create journal
;;

(provide 'init-org-journal)
