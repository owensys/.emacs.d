;; emacs24 require calling `package-initialize' explicitly
;; example:
;;(require-package 'swiper)
;;
;;(require-package-list
;; '(s f ht async dash
;;     ))


(require 'package)
(package-initialize)

(setq package-archives
      '(
	;;("melpa" . "https://elpa.emacs-china.org/melpa/")
	("melpa-stable" . "https://elpa.emacs-china.org/melpa-stable/")))

(setq package-user-dir (concat user-emacs-directory "melpa"))

;; On-demand installation of packages
(defun require-package (package &optional min-version no-refresh)
  "Ask elpa to install given PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))


(defun require-package-list (package-list)
  (dolist (pkg package-list)
    (require-package pkg)
    ))


(provide 'init-build-in-packages)
