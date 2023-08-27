(add-to-list 'load-path "~/.emacs.d/emacs-packages/org-brain")
(require 'org-brain)
(require 'org-archive)
(require 'org-agenda)

(setq org-brain-path (if is-work "d:/work/tsnotes/tsbrain" "x:/orgnotes/brain"))
(setq org-id-locations-file (if is-work "d:/work/tsnotes/tsbrain/org-brain-id" "x:/orgnotes/org-brain-id"))

;;(setq org-id-track-globally t)
;(add-hook 'before-save-hook #'org-brain-ensure-ids-in-buffer)
(setq org-brain-visualize-default-choices 'all)
;;(setq org-brain-title-max-length 0)
(setq org-brain-headline-entry-name-format-string "%2$s")
;;(setq my/default-org-brain-file "brain")
;;(setq org-brain-default-file-parent my/default-org-brain-file)
(setq org-brain-default-file-parent nil)

(setq org-brain-include-file-entries t
      org-brain-file-entries-use-title nil)


(defun eye--check-has-buffer (name)
  (catch 'foo
    (dolist (buffer (buffer-list))
      (if (string-equal name (format "%s" buffer))
          (throw 'foo buffer)
        )
      )
    ))



(defun eye/goto-brain()
  (interactive)
  (if (eye--check-has-buffer "*org-brain*")
      (switch-to-buffer "*org-brain*")
    (org-brain-visualize "brain")
    ))


(provide 'eye-brain)
