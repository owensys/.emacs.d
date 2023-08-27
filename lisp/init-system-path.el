;; PATH and exec-path
;; @see http://ergoemacs.org/emacs/emacs_env_var_paths.html
(defvar system-path-list)

(defvar sys-system-path-list-filename
  (if is-windows
      ".system-path-win32-list"
    ".system-path-linux-list"
   ))

;; http://ergoemacs.org/emacs/elisp_read_file_content.html
(defun get-string-from-file (filepath)
  "Return filePath's file content."
  (if (file-exists-p filepath)
      (with-temp-buffer
        (insert-file-contents filepath)
        (buffer-string))
    ""
    ))


(defun eye-get-locale-file-list-content (filepath)
  "Get content list from local file"
  (let* ((file-content (get-string-from-file filepath))
         (content-list nil)
         )
    (if (not (string-equal "" file-content))
        (progn
          (setq content-list (split-string file-content "\n"))
          )
      )
    content-list
    ))


(defun eye-get-locale-project-list ()
  "Get project dir list from local file"
  (eye-get-locale-file-list-content (concat user-emacs-directory ".project-list")))


(defun eye/update-system-path-env ()
  (interactive)
  (let ((system-path-list (eye-get-locale-file-list-content
                           (concat user-emacs-directory sys-system-path-list-filename))))


    (setenv "PATH" (mapconcat 'identity system-path-list (if is-windows ";" ":")))
    (setq exec-path (append system-path-list (list "." exec-directory)))
    (message "system path updated.")
    ))

(eye/update-system-path-env)

;; auto update when file save
(add-hook 'after-save-hook
          (lambda ()
            (if (string-equal (buffer-name) sys-system-path-list-filename)
                (eye/update-system-path-env)
              )
            ))


(provide 'init-system-path)
