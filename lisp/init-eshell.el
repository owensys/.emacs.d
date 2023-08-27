(defun eye/eshell-clear ()
  "Clear eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (local-set-key (kbd "C-l") 'eye/eshell-clear)))




;; custom eshell prompts, @see https://www.bytedude.com/custom-eshell-prompts/
(defun with-face (str &rest face-plist)
  (propertize str 'face face-plist))
(defun custom-eshell-prompt ()
  (let* (
         ;; Get the git branch.
         (git-branch-unparsed "" ;; set to empty on windows
          ;;(shell-command-to-string "git rev-parse --abbrev-ref HEAD 2>/dev/null") ;; windows can't work.
          )
         (git-branch
          (if (string= git-branch-unparsed "")
              ""
            ;; Remove the trailing newline.
            (substring git-branch-unparsed 0 -1)))
         )
    (concat
     ;; Timestamp.
     (with-face
      (format-time-string "[%a, %b %d | %H:%M:%S]\n" (current-time))
      :inherit font-lock-builtin-face)
     ;; Directory.
     (with-face (concat (eshell/pwd) " ") :inherit font-lock-constant-face)
     ;; Git branch.
     (unless (string= git-branch "")
       (with-face (concat "[" git-branch "]") :inherit font-lock-string-face))
     "\n"
     ;; Prompt.
     ;; NOTE: Need to keep " $" for the next/previous prompt regexp to work.
     (with-face " $" :inherit font-lock-preprocessor-face)
     " "
     )))
(setq eshell-prompt-function 'custom-eshell-prompt)
(setq eshell-highlight-prompt nil)





(provide 'init-eshell)
