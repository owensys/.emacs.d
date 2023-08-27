
(setq my-org-html-web-dir "x:/www")
(setq my-org-html-web-port 7000)

(eye/use-package
 'simple-httpd
 :load-path "emacs-web-server"
 :ensure t
 :config
 (progn
   (f-mkdir my-org-html-web-dir)
   (setq httpd-root my-org-html-web-dir)
   (setq httpd-port my-org-html-web-port)
   (httpd-start)
   )
 )


;;;; 添加一个org export选项
(defun org-html-export-to-tbhtml (async subtree visible body)
  (cl-letf (((symbol-function 'org-html--format-image) 'format-image-myimg))
    (org-html-export-to-html nil subtree visible body)))

;; 复制图片到web目录下的attach目录，并修正图片链接
(defun format-image-myimg (source attributes info)
  (require 'f)
  ;; source -> file:///d:/emacs_env/home/attach/20230708_211823_wallhaven-dp15dj.jpg
  ;; 内容较多，不适合打印
  (let* ((mysource (string-replace "file:///d:/emacs_env/home" "." source)) ;; 替换为相对web目录
        (image-file-name (file-name-nondirectory source))
        (src-image-path (concat "x:/orgnotes/attach/" image-file-name)) ;; 真实图片全路径
        (target-image-dir (expand-file-name "attach" my-org-html-web-dir))
        (target-image-path (concat target-image-dir "/" image-file-name)) ;; 待复制到的web图片目录图片全路径
        )
    (f-mkdir target-image-dir)
    (if (not (f-exists-p target-image-path))
        (f-copy src-image-path target-image-path))
    (org-html-close-tag
     "img"
     (org-html--make-attribute-string
      (org-combine-plists
       (list :src mysource
             :alt (if (string-match-p
                       (concat "^" org-preview-latex-image-directory) mysource)
                      (org-html-encode-plain-text
                       (org-find-text-property-in-string 'org-latex-src mysource))
                    (file-name-nondirectory mysource))
             ;; :width "60%" ;; 限制图片显示大小，也可使用 #+ATTR_HTML: :width 50% :height 30%
             ;; :height "60%"
             )
       (if (string= "svg" (file-name-extension mysource))
           (org-combine-plists '(:class "org-svg") attributes '(:fallback nil))
         attributes)))
     info)))

(org-export-define-derived-backend 'html-inline-images 'html
  :menu-entry '(?h "Export to Thebrain Link" ((?t "As HTML file and move" org-html-export-to-tbhtml))))


(defun eye-add-edit-button ()
  "创建可点击的Edit按钮，使用urlhandler.exe"
  (save-excursion
    (goto-char (point-min))
    (org-show-subtree) ;; 展开property，避免添加重复的内容
    (next-line 3) ;; 如果在第一行调用org-id-get-create，将会生成新的id
    (beginning-of-line)
    (if (not (search-forward "#+HTML: <a href='org" nil t))
        (progn
          (goto-char (point-min))
          (next-line 3)
          (insert (format "#+HTML: <a href='org://%s'>Edit</a>\n" (org-id-get-create))))
      )
    )
  )



(defun eye-add-export-filename ()
  "创建导出的html文件名，使用<ID>.html"
  (save-excursion
    (goto-char (point-min))
    (org-show-subtree) ;; 展开property，避免添加重复的内容
    (next-line 4) ;; 如果在第一行调用org-id-get-create，将会生成新的id
    (beginning-of-line)
    (if (not (search-forward "#+EXPORT_FILE_NAME:" nil t))
        (progn
          (goto-char (point-min))
          (next-line 4)
          (insert (format "#+EXPORT_FILE_NAME: %s.html\n" (org-id-get-create))))
      (message "has exported file name")
      )
    )
  )



(defun eye/export-to-thebrain-html (&optional ASYNC subtree visible
                                       body)
  (interactive)
  (eye-add-edit-button)
  (eye-add-export-filename)

  (cl-letf (((symbol-function 'org-html--format-image) 'format-image-myimg)
            (html-file-name "")
            (target-file-path "")
            )
    ;; org-html-export-to-html set body only to t, hide created and validation...
    ;; but no code highlight
    (setq html-file-name (org-html-export-to-html nil subtree visible nil))
    (setq target-file-path (expand-file-name html-file-name my-org-html-web-dir))
    (if (f-exists-p target-file-path)
      (f-delete target-file-path))
    (f-move html-file-name target-file-path)

    (kill-new (format "http://localhost:%s/%s" my-org-html-web-port html-file-name))
    (message "Exported finished and copyed link:%s" html-file-name)

    )

  )


(defun eye/check-update-html ()
  ;; (interactive)
  (when (and (eq major-mode 'org-mode)
             (string-match org-directory (buffer-file-name)))
    (save-excursion
      (goto-char (point-min))
      (next-line 2)
      
      (let* ((id (org-id-get-create))
             (html-file (format "%s/%s.html" my-org-html-web-dir id)))
        (if (f-exists-p html-file)
            (eye/export-to-thebrain-html)))
      ))
  )

(add-hook 'after-save-hook #'eye/check-update-html)


(provide 'init-local-html)
