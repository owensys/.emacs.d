;; https://github.com/Vidianos-Giannitsis/Dotfiles/blob/master/emacs/.emacs.d/libs/zettelkasten.org#org-roam-general-setup
(cl-defmethod org-roam-node-directories ((node org-roam-node))
  "Access slot \"directory\" of org-roam-node struct CL-X"
  (if-let ((dirs (file-name-directory (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (format "(%s)" (car (f-split dirs)))
    ""))

(cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
  "Access slot \"backlinks\" of org-roam-node struct CL-X"
  (let* ((count (caar (org-roam-db-query
		       [:select (funcall count source)
				:from links
				:where (= dest $s1)
				:and (= type "id")]
		       (org-roam-node-id node)))))
    (format "[%d]" count)))

(cl-defmethod org-roam-node-backlinkscount-number ((node org-roam-node))
  "Access slot \"backlinks\" of org-roam-node struct CL-X. This
     is identical to `org-roam-node-backlinkscount' with the
     difference that it returns a number instead of a fromatted
     string. This is to be used in
     `org-roam-node-sort-by-backlinks'"
  (let* ((count (caar (org-roam-db-query
		       [:select (funcall count source)
				:from links
				:where (= dest $s1)
				:and (= type "id")]
		       (org-roam-node-id node)))))
    count))

(cl-defmethod org-roam-node-todostate ((node org-roam-node))
  "Modified version of org-roam-node-todo to look a bit better"
  (if-let ((state (org-roam-node-todo node)))
      (format "Status: %s" state)))

(cl-defmethod org-roam-node-buffer ((node org-roam-node))
  "Access slot \"buffer\" of org-roam-node struct CL-X"
  (let ((buffer (get-file-buffer (org-roam-node-file node))))
    buffer))

(setq org-roam-node-display-template "${title:115} ${backlinkscount:6} ${todostate:20} ${directories:10} ${tags:15}")

;; (add-to-list 'display-buffer-alist
;; 	     '("\\*org-roam\\*"
;; 	       (display-buffer-in-direction)
;; 	       (direction . right)
;; 	       (window-width . 0.40)
;; 	       (window-height . fit-window-to-buffer)))


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





(defun eye/org-roam-insert-tag ()
  "用于快速添加tags，过滤非tags节点"
  (interactive)
  (org-roam-node-insert (lambda (NODE)
                          (s-contains-p "#" (org-roam-node-title NODE)))))



(provide 'custom-org-roam)
