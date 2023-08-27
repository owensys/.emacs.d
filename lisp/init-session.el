;; desktop save
(require 'desktop)

(setq desktop-base-file-name ".session.dat")
(setq desktop-base-lock-name ".session.dat.lock")
(setq desktop-auto-save-timeout 300) ;; 5 minutes for save

;; (desktop-read desktop-dirname) ;; load session
;; (desktop-auto-save-set-timer) ;; auto save session

;; use only one desktop
(setq desktop-dirname (concat user-emacs-directory ".session"))
(f-mkdir desktop-dirname)

;; remove desktop after it's been read
;;(add-hook 'desktop-after-read-hook
;;	  '(lambda ()
;;	     ;; desktop-remove clears desktop-dirname
;;	     (setq desktop-dirname-tmp desktop-dirname)
;;	     (desktop-remove)
;;	     (setq desktop-dirname desktop-dirname-tmp)))

(defun saved-session ()
  )

;; use session-restore to restore theme desktop manually
(defun session-restore ()
  "Restore a saved emacs session."
  (interactive)
  (if (saved-session)
      (progn
	(delete-other-windows)
	;;(kill-unused-buffers)
	(desktop-read user-cache-directory))
    (message "No desktop found.")))

;; use session-save to save the desktop manually
(defun session-save ()
  "Save an emacs session."
  (interactive)
  ;;(require 'auto-save) ;; https://github.com/manateelazycat/auto-save
  ;;(auto-save-buffers)
  (if (file-exists-p (concat desktop-dirname "/" desktop-base-file-name))
      (if (y-or-n-p "Overwrite existing desktop? ")
	  (desktop-save-in-desktop-dir)
	(message "Session not saved."))
  (desktop-save-in-desktop-dir)))

;; ask user whether to restore desktop at start-up
;;(add-hook 'after-init-hook
;; 	  '(lambda ()
;; 	     (if (saved-session)
;; 		 (if (y-or-n-p "Restore desktop? ")
;; 		     (session-restore)))))

(add-hook 'kill-emacs-hook 'session-save)


(provide 'init-session)
