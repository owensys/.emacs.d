
(setq so-long-threshold 2048)

(defun my/comint-font-lock-off-if-long-line (string)
  (when (bound-and-true-p font-lock-mode)
    (let ((long-line-found nil))
      (mapc #'(lambda (line)
                (if (> (length line) so-long-threshold)
                    (setq long-line-found t)))
            (split-string string "\n"))
      (when long-line-found
        (font-lock-mode -1)
        (message "disable `font-lock-mode' because of long line found in buffer '%s'" (buffer-name))))))

(defun my/shell--ask-if-multiline-input (input)
  "Avoid accidentally INPUT too many commands to shell."
  (let ((p1 (search "\n" input))
        (p2 (search "\n" input :from-end t))
        (buffer nil))
    (when (and (not (eq nil p1)) (not (eq nil p2)) (not (eq p1 p2)))
      (setq buffer (get-buffer-create "*Multiple Line Shell Input*"))
      (with-current-buffer buffer
        (read-only-mode -1)
        (erase-buffer)
        (insert input)
        (read-only-mode t)
        (let ((o (make-overlay (point-min) (point-max) buffer nil t)))
          (overlay-put o 'face `(:background "#000" :foreground "#FFF")))
        (display-buffer buffer))
      (unless (yes-or-no-p "Input multiple line to shell:")
        (kill-buffer buffer)
        (error "Input multiple line to shell aborted"))
      (kill-buffer buffer))))

(add-to-list 'comint-output-filter-functions 'my/comint-font-lock-off-if-long-line)
(add-to-list 'comint-input-filter-functions 'my/shell--ask-if-multiline-input)


(provide 'long-line)
