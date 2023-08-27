;; create mark
;; get all root path and select
;; get uuid of root path
;; insert a mark line

;; update mark pos
;; get root path
;; query mark by uuid
;; update record

;; open mark
;;

;; delete mark

;;
;;
;; (bookmark-set (eye-create-short-uuid))


(defface navsource-fringe-face
  '((((class grayscale)
      (background light)) (:background "DimGray"))
    (((class grayscale)
      (background dark))  (:background "LightGray"))
    (((class color)
      (background light)) (:foreground "White" :background "DarkOrange1"))
    (((class color)
      (background dark))  (:foreground "Black" :background "DarkOrange1")))
  "Face used to highlight bookmarks in the fringe."
  :group 'navsource)



(defun eye-file-close-update-mark ()
  (interactive)
  (let* ((file (buffer-file-name))
         )
    (maphash
     (lambda (k v)
       (message "traverse:%s, mark pos:%s" (nth 2 v) (marker-position (nth 0 v)))
       (if (string-equal file (nth 2 v))
           (progn
             (message "update mark")
             (puthash k (list nil k (nth 2 v) (marker-position (nth 0 v))) eye-markers-table) ;; marker uid buffer pos
             ))
       )
     eye-markers-table)
    ))




(add-hook 'kill-buffer-hook 'eye-file-close-update-mark)
;; (defun org-nav-source-open (path _)
;;   (interactive)

;;   (let* ((strlist (s-split ":" path))
;;          (full-path (concat default-directory (car strlist)))
;;          (line (car (cdr strlist))))
;;     (message "path:%s line:%s ex:%s" full-path line (file-exists-p full-path))
;;     (when (file-exists-p full-path)
;;       (other-window 1)
;;       (find-file full-path)
;;       (goto-line (string-to-number line)))
;;   )
;; )


(defun org-nav-source-open (path _)
  (interactive)

  (message "uid:%s" path)
  (let* ((uid path)
         file-path
         hash-value
         mark-pos
         )
    (setq hash-value (gethash uid eye-markers-table)) ;; marker uid buffer pos
    (setq file-path (nth 2 hash-value))
    (if (nth 0 hash-value)
        (setq mark-pos (marker-position (nth 0 hash-value)))
      (setq mark-pos (nth 3 hash-value))
      )
    (message "uid:%s, buffer:%s, pos:%s"
             uid file-path mark-pos)

    (when (file-exists-p file-path)
      (other-window 1)
      (find-file file-path)
      (goto-char mark-pos)))
  )

(org-link-set-parameters "nav"
			 :face '(:foreground "dark green" :underline t)
			 :follow #'org-nav-source-open)



;;https://www.gnu.org/software/emacs/manual/html_node/elisp/Other-Display-Specs.html
;; (let ((s "*source-bookmark*"))
;;   (put-text-property 0 (length s) 'display (list 'left-fringe 'bm-marker-left bm-fringe-face) s)
;;   (insert s)
;;   )*source-bookmark*


;; http://xahlee.info/emacs/emacs/elisp_hash_table.html
(setq eye-markers-table (make-hash-table :test 'equal))

;; (setq tm (make-marker))
;; (set-marker tm 10)

(defun eye-create-short-uuid ()
  (require 's)
  (s-left 8 (my-generate-uuid)))

(defun eye-get-fringe-marker ()
  (let ((s "*source-bookmark*"))
    (put-text-property 0 (length s) 'display (list 'left-fringe 'navsource-marker-left navsource-fringe-face) s)
    s))

(defun eye-start-position nil
  "Return the bookmark start position."
  (point-at-bol))


(defun eye-end-position nil
  "Return the bookmark end position."
  (min (point-max) (+ 1 (point-at-eol))))



(defun eye-bookmarkp (bookmark)
  "Return the BOOKMARK if overlay is a bookmark."
  (if (and (overlayp bookmark)
           (overlay-buffer bookmark)
           (string= (overlay-get bookmark 'category) "eye"))
      bookmark
    nil))


(defun eye-bookmark-at (point)
  "Get bookmark at POINT."
  (let ((overlays (overlays-at point))
        (bookmark nil))
    (while (and (not bookmark) overlays)
      (if (eye-bookmarkp (car overlays))
          (setq bookmark (car overlays))
        (setq overlays (cdr overlays))))
    bookmark))

(defun eye-bookmark-remove (&optional bookmark)
  "Remove bookmark at point or the BOOKMARK specified as parameter."
  (interactive)
  (if (null bookmark)
      (setq bookmark (eye-bookmark-at (point))))

  (if (eye-bookmarkp bookmark)
      (progn
        (delete-overlay bookmark)
        ;;  todo: remove from hash table
        )
    (message "not a bookmark")
    ))

;; *source-bookmark*

(defun eye-make-mark ()
  "创建书签"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let* ((m1 (point-marker))
           (uid (upcase (eye-create-short-uuid)))
           (buffer (buffer-file-name))
           (link (concat "nav:" uid))
           (desc (read-string "Desc: "))
           (bookmark (make-overlay (eye-start-position) (eye-end-position)))
           )
      (puthash uid (list m1 uid buffer 0) eye-markers-table)
      (kill-new (org-make-link-string link desc))
      (overlay-put bookmark 'time (float-time))
      (overlay-put bookmark 'position m1)
      (overlay-put bookmark 'category 'eye)
      (overlay-put bookmark 'before-string (eye-get-fringe-marker))
      (message "create a mark, markers table count[%s]" (hash-table-count eye-markers-table))
      )
    )
  )



;; (defun eye-mark-read-file (file) (eye-bookmark-remove)
;;   (with-temp-buffer
;;     (insert-file-contents file)
;;     (goto-char (point-min))
;;     (read (current-buffer))))

;; (defun eye-mark-save-file (file)
;;   (when file
;;     (condition-case nil
;;         (with-temp-file file
;;           (set-buffer-file-coding-system 'utf-8)
;;           (insert ";; eye.el -- persistent bookmarks. ")
;;           (insert "Do not edit this file.\n")
;;           (prin1 eye-mark-list (current-buffer))
;;           (insert "\n"))
;;       (error "Cannot save repository to %s" file))
;;     ))

;; (eye-mark-save-file "~/test-mark-data")
;; (defun eye-mark-load ()
;;   (setq eye-mark-list (eye-mark-read-file "~/test-mark-data")
;;   ))

;; (eye-mark-load)








;; (eye-make-mark)



;; (setq m1 (point-marker));;853

;; (eye-make-mark)


;; (markerp m1)
;; (marker-buffer m1)
;; (marker-position m1)



;; (eye-bookmark-at (point))

;; (defun eye-bookmarkp (bookmark)
;;   "Return the BOOKMARK if overlay is a bookmark."
;;   (if (and (overlayp bookmark)
;;            (overlay-buffer bookmark)
;;            (string= (overlay-get bookmark 'category) "eye"))
;;       bookmark
;;     nil))


;; (defun eye-bookmark-at (point)
;;   "Get bookmark at POINT."
;;   (let ((overlays (overlays-at point))
;;         (bookmark nil))
;;     (while (and (not bookmark) overlays)
;;       (if (eye-bookmarkp (car overlays))
;;           (setq bookmark (car overlays))
;;         (setq overlays (cdr overlays))))
;;     bookmark))



(provide 'eye-source-bookmark)
