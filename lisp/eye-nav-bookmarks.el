(setq bookmarks-roots-table (make-hash-table :test 'equal))
(setq eye-markers-table (make-hash-table :test 'equal))

;; (insert (eye-create-short-uuid))
(puthash "3ce2669c" "~/.emacs.d" bookmarks-roots-table)
;; (puthash "ddcc9a63" "e:/driveraidapps-xche-dms" bookmarks-roots-table)
(puthash "ddcc9a63" "e:/tmp/xche-test" bookmarks-roots-table)
(puthash "f7f4d1d6" "e:/tmp/xche-test" bookmarks-roots-table)

(add-to-list 'load-path "~/.emacs.d/packages/emacs-db")
(add-to-list 'load-path "~/.emacs.d/packages/emacs-kv")

(require 'db)

(setq my-db
  (db-make
   `(db-hash
     :filename ,(format "~/my-bookmarks.db"))))

;; (db-put "3ce2669c@4fc23565" '("/var/path" 33) my-db)

;; (db-get "3ce2669c@4fc23565" my-db)

(defun eye-start-position ()
  (point-at-bol))

(defun eye-end-position ()
  (point-at-eol))


(defvar bm-marker 'navsource-marker-left
  "Fringe marker side. Left of right.")


;; avoid errors on emacs running in a terminal
(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'navsource-marker-left   [#x00 #x00 #xFC #xFE #x0F #xFE #xFC #x00])
  (define-fringe-bitmap 'navsource-marker-right  [#x00 #x00 #x3F #x7F #xF0 #x7F #x3F #x00]))


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

(defcustom navsource-fringe-face 'navsource-fringe-face
  "*Specify face used to highlight the fringe."
  :type 'face
  :group 'navsource)



(defun eye-bookmark-roots-path-list ()
  (let* ((list '()))
    (maphash (lambda (k v)
               (add-to-list 'list (concat k "@" v))
               )
             bookmarks-roots-table)
    list
    )
  )


(defun eye-get-fringe-marker ()
  (interactive)
  (let ((s "*source-bookmark*"))
    (put-text-property 0 (length s) 'display (list 'left-fringe 'navsource-marker-left navsource-fringe-face) s)
    s))


;; update
(defun eye/create-bookmark ()
  (interactive)
  (let((bookmark (nav-bookmark-at (point))))
    (if bookmark
        (progn (setq bm-current bookmark)
               (overlay-put bookmark 'position (point-marker))
               (overlay-put bookmark 'time (or time (float-time))));; update time
      (let* ((bookmark (make-overlay (eye-start-position) (eye-end-position)))
             (mark-uuid (eye-create-short-uuid))
             (m1 (point-marker))

             (root-value-list (s-split "@" (ivy-read "Root Dir: " (eye-bookmark-roots-path-list))))
             (root-uuid (car root-value-list))
             (root-dir (nth 1 root-value-list))
             (name (concat root-uuid "@" mark-uuid))
             (desc (read-string "Desc: "))
             ;; (bookmark (bookmark-set name))
             (relpath (f-relative (buffer-file-name) root-dir))
             (val `((:relpath ,relpath)))
             )

        ;; highlight
        (overlay-put bookmark 'time (float-time))
        (overlay-put bookmark 'position m1)
        (overlay-put bookmark 'category 'eye)
        (overlay-put bookmark 'before-string (eye-get-fringe-marker))

        ;; add to db
        (puthash name m1 eye-markers-table)
        (add-to-list 'val `(:pos ,(marker-position m1)))
        (add-to-list 'val `(:desc ,desc))
        ;; (db-put name `(,relpath ,(bookmark-get-position name) ,desc) my-db)
        (db-put name val my-db)

        ;; copy
        (kill-new (org-make-link-string (concat "nav:" name) desc))

        (message "created a bookmark %s at %s" name relpath)
        )
      )
    ))



(defun nav-bookmarkp (bookmark)
  "Return the BOOKMARK if overlay is a bookmark."
  (if (and (overlayp bookmark) ;; 是overlay
           (overlay-buffer bookmark) ;; 在buffer中
           (string= (overlay-get bookmark 'category) "eye")) ;; 获取分类
      bookmark
    nil))

(defun nav-bookmark-at (point)
  "判断一个点上的自定义bookmark"
  (let ((overlays (overlays-at point))
        (bookmark nil))
    (while (and (not bookmark) overlays)
      (if (nav-bookmarkp (car overlays))
          (setq bookmark (car overlays))
        (setq overlays (cdr overlays))))
    bookmark))


(defun nav-bookmark-remove (&optional bookmark)
  "Remove bookmark at point or the BOOKMARK specified as parameter."
  (if (null bookmark)
      (setq bookmark (nav-bookmark-at (point))))

  (if (nav-bookmarkp bookmark)
      (delete-overlay bookmark))) ;; delete overylay


(defun eye/toggle-bookmark ()
  (interactive)
  (let ((bookmark (nav-bookmark-at (point))))
    (if bookmark
        (progn
          (nav-bookmark-remove bookmark)
          (message "remove nav bookmark")
          )
      (eye/create-bookmark))
    )
  )


(defun org-nav-source-open (path _)
  (interactive)
  (let* ((uuid-list (s-split "@" path))
         (root-uuid (nth 0 uuid-list))
         (mark-uuid (nth 1 uuid-list))
         (root-dir (gethash root-uuid bookmarks-roots-table))
         (name (concat root-uuid "@" mark-uuid))
         (hash-value (db-get name my-db))
         (relpath (assoc :relpath hash-value)) ;;(nth 0 hash-value))
         (pos (cadr (assoc :pos hash-value))) ;; (nth 1 hash-value))
         ;; (desc (assoc :desc hash-value))
         (real-file-path (concat root-dir "/" (cadr relpath)))
         )
    (message "hash-value:%s" hash-value)
    ;; (message "path:%s, pos:%s, name:%s, Desc:%s"
             ;; (cadr relpath) (cadr pos) name (cadr desc))
    (xah-next-window-or-frame)
    (find-file real-file-path)
    (goto-char pos)
    )
  )


(defun eye-bookmarks-show-list ()
  "获取显示列表"
  (let* ((string-list nil))
    (db-hash-map (lambda (k v)
                   (message "k:%s, v:%s" k v)
                   (add-to-list 'string-list
                                (format "%s|%s|%s"
                                        k
                                        (cadr (assoc :relpath v))
                                        (cadr (assoc :desc v))
                                        )))
                 my-db
                 )
    string-list))



(defun eye/goto-nav-bookmark ()
  "进入哪个位置"
  (interactive)
  (let* ((select-item (ivy-read "Goto nav bookmark: " (eye-bookmarks-show-list))))
    (org-nav-source-open (car (split-string select-item "|")) nil)
    ))

(defun eye/delete-nav-bookmark ()
  "删除哪个位置"
  (interactive)
  (let* ((select-item (ivy-read "Goto nav bookmark: " (eye-bookmarks-show-list))))
    (db-hash- (car (split-string select-item "|")) nil)
    ))

;;todo
;; 1.update mark pos
;; 2.recreate db
;;   scan all bookmark org file, check nav in my-db, if not, remove it
;; 3.how to remove a key?
;;

(defun eye-get-mark-pos (name)
  (let* ((hash-value (db-get name my-db))
         (pos -1))
    (if hash-value
        (setq pos (cadr (assoc :pos hash-value)))
        )
    pos
    )
  )

;; test
;; (defun eye-change-mark-pos (name pos)
;;   (let* ((hash-value (db-get name my-db))
;;          (pos -1)
;;          (new-value '())
;;          relpath
;;          desc
;;          )
;;     (if hash-value
;;         (progn
;;           (setq pos (nth 1 hash-value))
;;           (setq relpath)
;;         )
;;     pos
;;     )
;;   )


(defun eye-update-bookmarks ()
  "自动更新mark位置"
  (interactive)
  (maphash (lambda (k v)
             (let* ((name k)
                    (pos (marker-position v))
                    (pos-in-db (eye-get-mark-pos name))
                    (hash-value (db-get name my-db))
                    relpath desc
                    val
                    )
               ;; (message "check name:%s, pos:%s, bookmark pos:%s" name pos-in-db pos)
               (if (not (equal pos pos-in-db))
                   (let ((hash-value (db-get name my-db)))
                     ;; (message "update name:%s, pos:%s" name pos)
                     (when hash-value
                       (setq desc (cadr (assoc :desc hash-value)))
                       (setq relpath (cadr (assoc :relpath hash-value)))
                       (add-to-list 'val `(:relpath ,relpath))
                       (add-to-list 'val `(:pos ,pos))
                       (add-to-list 'val `(:desc ,desc))
                       (db-put name val my-db)
                       (message "update name:%s, pos:%s, new pos:%s" name pos-in-db pos)
                       )
                     ))
               ))
           eye-markers-table)
  )

(defun eye/clean-not-use-bookmarks ()
  (interactive)
  (org-element-map (org-element-parse-buffer) 'link
  (lambda (link)
    (when (string= (org-element-property :type link) "nav")
      (org-element-property :path link))))

  )


(setq update-bookmarks-interval 10)

;; (setq update-bookmarks-timer (run-with-idle-timer 10 t 'eye-update-bookmarks))


(org-link-set-parameters "nav"
			 :face '(:foreground "dark green" :underline t)
			 :follow #'org-nav-source-open)


(provide 'eye-nav-bookmarks)
