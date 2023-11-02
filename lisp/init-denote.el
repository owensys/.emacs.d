
(eye/use-package
 'denote
 :ensure t
 :load-path "denote"
 :config
 (progn
   (setq denote-directory (s-trim (get-string-from-file "~/.emacs.d/.note-dir")))
   (setq denote-known-keywords '("emacs" "qt"
                                 "cpp" "python" "ruby" "java" "dart" "php"
                                 "english" "project" "ideas" "database"
                                 "bash" "make" "fish" "git" "frp" "book"))
   (setq denote-infer-keywords t)
   (setq denote-sort-keywords t)
   (setq denote-file-type nil) ; Org is the default, set others here
   (setq denote-prompts '(title keywords))
   (setq denote-excluded-directories-regexp nil)
   (setq denote-excluded-keywords-regexp nil)

   ;; (setq project-read-file-name-function 'project--read-file-cpd-relative)

   (defun eye/denote-create-new-note-from-region (beg end)
     "Create note whose contents include the text between BEG and END.
Prompt for title and keywords of the new note."
     (interactive "r")
     (if-let (((region-active-p))
              (text (buffer-substring-no-properties beg end)))
         (progn
           (denote (denote--title-prompt) (denote--keywords-prompt))
           (insert text))
       (user-error "No region is available")))

   (defun eye/denote-org-extract-subtree ()
  "Create new Denote note using current Org subtree.
Make the new note use the Org file type, regardless of the value
of `denote-file-type'.

Use the subtree title as the note's title.  If available, use the
tags of the heading are used as note keywords.

Delete the original subtree."
  (interactive)
  (if-let ((text (org-get-entry))
           (heading (org-get-heading :no-tags :no-todo :no-priority :no-comment)))
      (progn
        (delete-region (org-entry-beginning-position) (org-entry-end-position))
        (denote heading (denote--keywords-prompt))
        (insert text))
    (user-error "No subtree to extract; aborting")))



   ))


(provide 'init-denote)
