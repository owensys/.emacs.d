;; https://emacstil.com/til/2021/12/19/my-first-macro-until-loop/
(defmacro do-until (test &rest body)
  `(while (not ,test)
    ,@body))


(defun eye/git-clone-clipboard-url ()
  "Clone git URL in clipboard asynchronously and open in dired when finished."
  (interactive)
  (cl-assert (string-match-p "^\\(http\\|https\\|ssh\\)://" (current-kill 0)) nil "No URL in clipboard")
  (let* ((url (current-kill 0))
         (download-dir (expand-file-name "~/Downloads/"))
         (project-dir (concat (file-name-as-directory download-dir)
                              (file-name-base url)))
         (default-directory download-dir)
         (command (format "git clone %s" url))
         (buffer (generate-new-buffer (format "*%s*" command)))
         (proc))
    (when (file-exists-p project-dir)
      (if (y-or-n-p (format "%s exists. delete?" (file-name-base url)))
          (delete-directory project-dir t)
        (user-error "Bailed")))
    (switch-to-buffer buffer)
    (setq proc (start-process-shell-command (nth 0 (split-string command)) buffer command))
    (with-current-buffer buffer
      (setq default-directory download-dir)
      (shell-command-save-pos-or-erase)
      (require 'shell)
      (shell-mode)
      (view-mode +1))
    (set-process-sentinel proc (lambda (process state)
                                 (let ((output (with-current-buffer (process-buffer process)
                                                 (buffer-string))))
                                   (kill-buffer (process-buffer process))
                                   (if (= (process-exit-status process) 0)
                                       (progn
                                         (message "finished: %s" command)
                                         (dired project-dir))
                                     (user-error (format "%s\n%s" command output))))))
    (set-process-filter proc #'comint-output-filter)))


(defun eye/insert-date ()
  "Insert a date string but no time"
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun eye/insert-date-time ()
  "Insert a date string with time"
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun eye/insert-time-no-date ()
  "Insert a time string but no date"
  (interactive)
  (insert (format-time-string "%H:%M:%S")))

(defun eye/insert-date-with-week ()
  (interactive)
  (eye/insert-date)
  (insert " ")
  ;; (insert (ivy-read "Week:" '("周一" "周二" "周三" "周四" "周五" "周六" "周日")))
  ;; (insert (eye-get-time-week (date-to-time "2022-05-15T00:00:00")))
  (insert (eye-get-time-week (current-time)))
  )

(defun eye/insert-date-for-filename ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d_%H-%M-%S__"))
  )

(defun eye-get-dir-files-by-time (dir &optional regexp newer-first)
  "获取一个目录下的文件列表, newer-first为t时按最新时间的排在前面"
  (mapcar #'car
	  (sort (directory-files-and-attributes dir nil regexp)
		#'(lambda (x y)
		    ;; 最新时间的排在前面
		    (if newer-first
			(progn
			  (not (time-less-p (nth 6 x) (nth 6 y))))
		      (time-less-p (nth 6 x) (nth 6 y))
		      )))))



(defun eye-open-dir (dir)
  "Open explorer of current buffer directory.
locale-notebook-dir use absolute path for advise.
"
  (interactive)
  (when (eq system-type 'windows-nt)
    (let* ((explorer (replace-regexp-in-string "/" "\\\\" (executable-find "C:/Windows/SysWOW64/explorer")))
	   (command))
      (setq dir (encode-coding-string
		 (replace-regexp-in-string "/" "\\\\" dir) 'gbk-dos))
      (setq command (concat explorer " " dir))
      (shell-command command nil nil)
      (message command))
    ))

(defun eye-select-file-in-explorer (file-path)
  "打开文件管理器，并选中文件"
  (when (eq system-type 'windows-nt)
    (let* ((explorer (replace-regexp-in-string "/" "\\\\" (executable-find "C:/Windows/SysWOW64/explorer")))
	   (command))
      (setq file-path (encode-coding-string
		 (replace-regexp-in-string "/" "\\\\" file-path) 'gbk-dos))
      (setq command (concat explorer " /select, " file-path))
      (async-shell-command-no-window command)
      )))


(defun eye-open-file-native (file-path)
  "用系统默认应用打开文件"
  (when (eq system-type 'windows-nt)
    (let* ((explorer (executable-find "C:/Windows/SysWOW64/explorer"))
	   (command))
      (setq file-path (encode-coding-string file-path 'gbk-dos))
      (setq command (concat explorer " " file-path))
      (setq command (replace-regexp-in-string "/" "\\\\" command))
      (shell-command command nil nil)
      )))



(defun eye/open-thunar ()
  "Open thunar of current buffer directory."
  (when (and default-directory (executable-find "thunar"))
    (start-process "File manager" nil "thunar" default-directory)))

(defun eye/open-explorer ()
  "Open explorer of current buffer directory."
  (when (and default-directory (file-directory-p default-directory)
	     (eq system-type 'windows-nt))
    (let ((dir default-directory)
	  (explorer (replace-regexp-in-string "/" "\\\\" (executable-find "C:/Windows/SysWOW64/explorer")))
	  (command))
      (setq dir (encode-coding-string
		 (replace-regexp-in-string "/" "\\\\" dir) 'gbk-dos))
      (setq command (concat explorer " " dir))
      (shell-command command nil nil)
      (message command))
    ))

(defun eye/open-dir-explorer ()
  "使用系统管理器打开目录"
  (interactive)
  (let* ((dir (ido-read-directory-name "Open dir:"))
         (default-directory dir))
    (eye/open-file-manager)
  ))

(defun eye/open-file-manager ()
  "Open external file manager."
  (interactive)
  (cond ((eq system-type 'windows-nt)
	 (eye/open-explorer))
	((eq system-type 'gnu/linux)
	 (eye/open-thunar))
	(t (message "Not support current system."))
	))


(defun eye/open-url-at-point ()
  "open url in region"
  (interactive)
  (save-excursion
    (let ((url (buffer-substring-no-properties (region-beginning) (region-end))))
      (if url
	  (browse-url url)))
    ))


(defun xah-comment-dwim ()
  "Like `comment-dwim', but toggle comment if cursor is not at end of line.
URL `http://ergoemacs.org/emacs/emacs_toggle_comment_by_line.html'
Version 2016-10-25"
  (interactive)
  (if (region-active-p)
      (comment-dwim nil)
    (let (($lbp (line-beginning-position))
          ($lep (line-end-position)))
      (if (eq $lbp $lep)
          (progn
            (comment-dwim nil))
        (if (eq (point) $lep)
            (progn
              (comment-dwim nil))
          (progn
            (comment-or-uncomment-region $lbp $lep)
            (forward-line )))))))



(defun eye--get-region-line-number ()
  "获取选中区域开始行号和结束行号。
参考：https://emacs.stackexchange.com/questions/62023/how-to-get-the-real-line-number-of-a-selection"
  (let* ((rend (region-end))
         (startl (line-number-at-pos (region-beginning)))
         (endl (line-number-at-pos rend))
         (real-endl (if (= rend (save-excursion (goto-char rend) (beginning-of-line) (point)))
                        (- endl 1)
                      endl)))
    ;; (message (format "%d %d" startl real-endl))
    (cons startl real-endl)
    ))


(defun eye/git-blame-file-or-region ()
  "显示当前文件git blame 记录，如果有选中区域，则显示选中区域历史记录: git blame -Lstart,end file"
  (interactive)
  (let* ((file-path (xah-copy-file-path))
         (file-name (buffer-name))
         (output-buffer (format "*blame-%s*" file-name))
         (args "")
         (command "git blame ")
         p1 p2
         )
    (if (region-active-p)
        (let ((region-lines (eye--get-region-line-number)))
          (setq p1 (car region-lines))
          (setq p2 (cdr region-lines))
          (setq command (concat command (format "-L%s,%s " p1 p2)))
          (deactivate-mark)
          )
      )
    (setq command (concat command file-path))
    (shell-command command output-buffer nil)

    (switch-to-buffer output-buffer)
    (toggle-truncate-lines 1) ;; 禁用折行
    (delete-other-windows)
    (if (string-equal "c" (f-ext file-name))
        (c-mode))
    (if (string-equal "h" (f-ext file-name))
        (c-mode))
    (if (string-equal "cpp" (f-ext file-name))
        (c++-mode))
    (if (string-equal "cc" (f-ext file-name))
        (c++-mode))
    )
  )



(defun eye/git-show-commit (&optional commit-id)
  "光标停在commit id上，显示一个commit id的记录"
  (interactive)
  (save-excursion
    (let* ((cid (or (thing-at-point 'word) (read-string "commit id:") ))
           (command (format "git show %s" cid))
           (output-buffer (format "*commit-%s*" cid))
           )
      (shell-command command output-buffer nil)
      (switch-to-buffer output-buffer)
      (toggle-truncate-lines 1) ;; 禁用折行
      (diff-mode)
      (delete-other-windows)
      )
    ))




(defun eye/git-log-current-file ()
  "显示当前文件的git记录"
  (interactive)
  (let* ((file-path (xah-copy-file-path))
         (file-name (buffer-name))
         (output-buffer (format "*gitlog-%s*" file-name))
         (args "")
         (command "git log --date=iso -- ")
         )
    (setq command (concat command file-path))
    (shell-command command output-buffer nil)
    )
  )

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun eye/rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name))
          )))))

(defun eye/copy-file-name ()
  (interactive)
  (let ((file-name (file-name-nondirectory (buffer-file-name))))
    (kill-new file-name)
    (message "copyed file name: %s" file-name)
    ))


(defun xah-copy-file-path (&optional @dir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
Result is full path.
If `universal-argument' is called first, copy only the dir path.
If in dired, copy the file/dir cursor is on, or marked files.
If a buffer is not file and not dired, copy value of `default-directory' (which is usually the “current” dir when that buffer was created)
URL `http://ergoemacs.org/emacs/emacs_copy_file_path.html'
Version 2018-06-18"
  (interactive "P")
  (let (($fpath
         (if (string-equal major-mode 'dired-mode)
             (progn
               (let (($result (mapconcat 'identity (dired-get-marked-files) "\n")))
                 (if (equal (length $result) 0)
                     (progn default-directory )
                   (progn $result))))
           (if (buffer-file-name)
               (buffer-file-name)
             (expand-file-name default-directory)))))
    (kill-new
     (if @dir-path-only-p
         (progn
           (message "Directory copied: %s" (file-name-directory $fpath))
           (file-name-directory $fpath))
       (progn
         (message "File path copied: %s" $fpath)
         $fpath )))))


(defun xah-select-current-line ()
  "Select current line.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2016-07-22"
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))

(defun xah-select-text-in-quote ()
  "Select text between the nearest left and right delimiters.
Delimiters here includes the following chars: \"`<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）
This command select between any bracket chars, does not consider nesting. For example, if text is
 (a(b)c▮)
 the selected char is “c”, not “a(b)c”.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2020-11-24"
  (interactive)
  (let (
        ($skipChars "^\"`<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）〘〙")
        $p1
        )
    (skip-chars-backward $skipChars)
    (setq $p1 (point))
    (skip-chars-forward $skipChars)
    (set-mark $p1)))


(defun xah-extend-selection ()
  "Select the current word, bracket/quote expression, or expand selection.
Subsequent calls expands the selection.
when there's no selection,
• if cursor is on a any type of bracket (including parenthesis, quotation mark), select whole bracketed thing including bracket
• else, select current word.
when there's a selection, the selection extension behavior is still experimental. But when cursor is on a any type of bracket (parenthesis, quote), it extends selection to outer bracket.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2020-02-04"
  (interactive)
  (if (region-active-p)
      (progn
        (let (($rb (region-beginning)) ($re (region-end)))
          (goto-char $rb)
          (cond
           ((looking-at "\\s(")
            (if (eq (nth 0 (syntax-ppss)) 0)
                (progn
                  ;; (message "left bracket, depth 0.")
                  (end-of-line) ; select current line
                  (set-mark (line-beginning-position)))
              (progn
                ;; (message "left bracket, depth not 0")
                (up-list -1 t t)
                (mark-sexp))))
           ((eq $rb (line-beginning-position))
            (progn
              (goto-char $rb)
              (let (($firstLineEndPos (line-end-position)))
                (cond
                 ((eq $re $firstLineEndPos)
                  (progn
                    ;; (message "exactly 1 line. extend to next whole line." )
                    (forward-line 1)
                    (end-of-line)))
                 ((< $re $firstLineEndPos)
                  (progn
                    ;; (message "less than 1 line. complete the line." )
                    (end-of-line)))
                 ((> $re $firstLineEndPos)
                  (progn
                    ;; (message "beginning of line, but end is greater than 1st end of line" )
                    (goto-char $re)
                    (if (eq (point) (line-end-position))
                        (progn
                          ;; (message "exactly multiple lines" )
                          (forward-line 1)
                          (end-of-line))
                      (progn
                        ;; (message "multiple lines but end is not eol. make it so" )
                        (goto-char $re)
                        (end-of-line)))))
                 (t (error "logic error 42946" ))))))
           ((and (> (point) (line-beginning-position)) (<= (point) (line-end-position)))
            (progn
              ;; (message "less than 1 line" )
              (end-of-line) ; select current line
              (set-mark (line-beginning-position))))
           (t
            ;; (message "last resort" )
            nil))))
    (progn
      (cond
       ((looking-at "\\s(")
        ;; (message "left bracket")
        (mark-sexp)) ; left bracket
       ((looking-at "\\s)")
        ;; (message "right bracket")
        (backward-up-list) (mark-sexp))
       ((looking-at "\\s\"")
        ;; (message "string quote")
        (mark-sexp)) ; string quote
       ;; ((and (eq (point) (line-beginning-position)) (not (looking-at "\n")))
       ;;  (message "beginning of line and not empty")
       ;;  (end-of-line)
       ;;  (set-mark (line-beginning-position)))
       ((or (looking-back "\\s_" 1) (looking-back "\\sw" 1))
        ;; (message "left is word or symbol")
        (skip-syntax-backward "_w" )
        ;; (re-search-backward "^\\(\\sw\\|\\s_\\)" nil t)
        (push-mark)
        (skip-syntax-forward "_w")
        (setq mark-active t)
        ;; (exchange-point-and-mark)
        )
       ((and (looking-at "\\s ") (looking-back "\\s " 1))
        ;; (message "left and right both space" )
        (skip-chars-backward "\\s " ) (set-mark (point))
        (skip-chars-forward "\\s "))
       ((and (looking-at "\n") (looking-back "\n" 1))
        ;; (message "left and right both newline")
        (skip-chars-forward "\n")
        (set-mark (point))
        (re-search-forward "\n[ \t]*\n")) ; between blank lines, select next text block
       (t
        ;; (message "just mark sexp" )
        (mark-sexp)
        (exchange-point-and-mark))
       ;;
       ))))


(defun xah-user-buffer-q ()
  "Return t if current buffer is a user buffer, else nil.
Typically, if buffer name starts with *, it's not considered a user buffer.
This function is used by buffer switching command and close buffer command, so that next buffer shown is a user buffer.
You can override this function to get your idea of “user buffer”.
Version 2016-06-18"
  (interactive)
  (cond
   ((string-equal "*" (substring (buffer-name) 0 1)) nil)
   ((string-equal major-mode "dired-mode") nil)
   ((string-equal major-mode "eww-mode") nil)
   ((string-match "autoload.pkg.el" (buffer-name)) nil)
   (t t)))

(defun xah-next-user-buffer ()
  "Switch to the next user buffer.
“user buffer” is determined by `xah-user-buffer-q'.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (xah-user-buffer-q))
          (progn (next-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun xah-previous-user-buffer ()
  "Switch to the previous user buffer.
“user buffer” is determined by `xah-user-buffer-q'.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (xah-user-buffer-q))
          (progn (previous-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))


(defun xah-next-emacs-buffer ()
  "Switch to the next emacs buffer.
“emacs buffer” here is buffer whose name starts with *.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (not (string-equal "*" (substring (buffer-name) 0 1))) (< i 20))
      (setq i (1+ i)) (next-buffer))))

(defun xah-previous-emacs-buffer ()
  "Switch to the previous emacs buffer.
“emacs buffer” here is buffer whose name starts with *.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (not (string-equal "*" (substring (buffer-name) 0 1))) (< i 20))
      (setq i (1+ i)) (previous-buffer))))


(defun xah-new-empty-buffer ()
  "Create a new empty buffer.
New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.
It returns the buffer (for elisp programing).
URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2017-11-01"
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    ;; (funcall initial-major-mode)
    (fundamental-mode)
    (setq buffer-offer-save t)
    $buf
    ))


(declare-function bookmark-maybe-load-default-file "bookmark" ())
(defvar bookmark-alist)
(declare-function bookmark-get-filename "bookmark" (bookmark-name-or-record))
(defun xah-open-file-fast ()
  "Prompt to open a file from bookmark `bookmark-bmenu-list'.
This command is similar to `bookmark-jump', but use `ido-mode' interface, and ignore cursor position in bookmark.
URL `http://ergoemacs.org/emacs/emacs_hotkey_open_file_fast.html'
Version 2019-02-26"
  (interactive)
  (require 'bookmark)
  (bookmark-maybe-load-default-file)
  (let (($this-bookmark
         (ido-completing-read "Open bookmark:" (mapcar (lambda ($x) (car $x)) bookmark-alist))))
    (find-file (bookmark-get-filename $this-bookmark))
    ;; (bookmark-jump $this-bookmark)
    ))


(defun xah-open-file-at-cursor ()
  "Open the file path under cursor.
If there is text selection, uses the text selection for path.
If the path starts with “http://”, open the URL in browser.
Input path can be {relative, full path, URL}.
Path may have a trailing “:‹n›” that indicates line number, or “:‹n›:‹m›” with line and column number. If so, jump to that line number.
If path does not have a file extension, automatically try with “.el” for elisp files.
This command is similar to `find-file-at-point' but without prompting for confirmation.
URL `http://ergoemacs.org/emacs/emacs_open_file_path_fast.html'
Version 2020-10-17 2021-02-24"
  (interactive)
  (let* (
         ($inputStr
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (let ($p0 $p1 $p2
                      ;; chars that are likely to be delimiters of file path or url, e.g. whitespace, comma. The colon is a problem. cuz it's in url, but not in file name. Don't want to use just space as delimiter because path or url are often in brackets or quotes as in markdown or html
                      ($pathStops "^  \t\n\"`'‘’“”|[]{}「」<>〔〕〈〉《》【】〖〗«»‹›❮❯❬❭〘〙·。\\"))
              (setq $p0 (point))
              (skip-chars-backward $pathStops)
              (setq $p1 (point))
              (goto-char $p0)
              (skip-chars-forward $pathStops)
              (setq $p2 (point))
              (goto-char $p0)
              (buffer-substring-no-properties $p1 $p2))))
         ($path
          (replace-regexp-in-string
           "^/C:/" "/"
           (replace-regexp-in-string
            "^file://" ""
            (replace-regexp-in-string
             ":\\'" "" $inputStr)))))
    (if (string-match-p "\\`https?://" $path)
        (if (fboundp 'xahsite-url-to-filepath)
            (let (($x (xahsite-url-to-filepath $path)))
              (if (string-match "^http" $x )
                  (browse-url $x)
                (find-file $x)))
          (progn (browse-url $path)))
      (progn ; not starting “http://”
        (if (string-match "#" $path )
            (let (
                  ( $fpath (substring $path 0 (match-beginning 0)))
                  ( $fractPart (substring $path (1+ (match-beginning 0)))))
              (if (file-exists-p $fpath)
                  (progn
                    (find-file $fpath)
                    (goto-char 1)
                    (search-forward $fractPart ))
                (when (y-or-n-p (format "file does not exist: 「%s」. Create?" $fpath))
                  (find-file $fpath))))
          (if (string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\(:[0-9]+\\)?\\'" $path)
              (let (
                    ($fpath (match-string 1 $path))
                    ($line-num (string-to-number (match-string 2 $path))))
                (if (file-exists-p $fpath)
                    (progn
                      (find-file $fpath)
                      (goto-char 1)
                      (forward-line (1- $line-num)))
                  (when (y-or-n-p (format "file does not exist: 「%s」. Create?" $fpath))
                    (find-file $fpath))))
            (if (file-exists-p $path)
                (progn ; open f.ts instead of f.js
                  (let (($ext (file-name-extension $path))
                        ($fnamecore (file-name-sans-extension $path)))
                    (if (and (string-equal $ext "js")
                             (file-exists-p (concat $fnamecore ".ts")))
                        (find-file (concat $fnamecore ".ts"))
                      (find-file $path))))
              (if (file-exists-p (concat $path ".el"))
                  (find-file (concat $path ".el"))
                (when (y-or-n-p (format "file does not exist: 「%s」. Create?" $path))
                  (find-file $path ))))))))))



;; same as eye/open-file-manager
(defun xah-show-in-desktop ()
  "Show current file in desktop.
 (Mac Finder, Windows Explorer, Linux file manager)
This command can be called when in a file buffer or in `dired'.
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2020-11-20 2021-01-31"
  (interactive)
  (let (($path (if (eq major-mode 'dired-mode)
                   (if (eq nil (dired-get-marked-files ))
                       default-directory
                     (car (dired-get-marked-files )))
                 (if (buffer-file-name) (buffer-file-name) default-directory))))
    (cond
     ((string-equal system-type "windows-nt")
      (shell-command (format "PowerShell -Command invoke-item '%s'" (expand-file-name default-directory )))
      ;; (let ( ($cmd (format "Explorer /select,%s"  (shell-quote-argument (replace-regexp-in-string "/" "\\" $path "FIXEDCASE" "LITERAL" )))))
      ;;   (shell-command $cmd))
      )
     ((string-equal system-type "darwin")
      (shell-command
       (concat "open -R " (shell-quote-argument $path))))
     ((string-equal system-type "gnu/linux")
      (let (
            (process-connection-type nil)
            (openFileProgram (if (file-exists-p "/usr/bin/gvfs-open")
                                 "/usr/bin/gvfs-open"
                               "/usr/bin/xdg-open")))
        (start-process "" nil openFileProgram (shell-quote-argument $path)))
      ;; (shell-command "xdg-open .") ;; 2013-02-10 this sometimes froze emacs till the folder is closed. eg with nautilus
      ))))


(defun eye/org-inc-indent ()
  "如果第一行就是空格，插入4个空格后，光标不会自动移到最后，需要手动移动"
  (interactive)
  (when (eq major-mode 'org-mode)
    (save-excursion
      (beginning-of-visual-line)
      (when (char-after (point))
	(if (string-equal "*" (char-to-string (char-after (point)))) ;; 是星号时，demote
	    (org-demote-subtree)
	  (insert "    ") ;; 是其它字符时，增加缩进
	  )
	)
      ;; 是新行
      (if (not (char-after (point)))
	  (insert "    ")
	)
      )))

(defun eye/org-dec-indent ()
  (interactive)
  (when (eq major-mode 'org-mode)
    (save-excursion
      (beginning-of-visual-line)
      (when (char-after (point))
	(if (string-equal "*" (char-to-string (char-after (point))))
	    (org-promote-subtree)
	  ;; 判断有空格并删除4个空格
	  (dotimes (i 4)
	    (if (and (char-after (point))
		     (string-equal " " (char-to-string (char-after (point)))))
		(delete-char 1))
	    ))
	)
      )))


(defun xah-next-user-buffer ()
  "Switch to the next user buffer.
“user buffer” is determined by `xah-user-buffer-q'.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (xah-user-buffer-q))
          (progn (next-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun xah-previous-user-buffer ()
  "Switch to the previous user buffer.
“user buffer” is determined by `xah-user-buffer-q'.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (xah-user-buffer-q))
          (progn (previous-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))


(defun xah-delete-current-file-make-backup (&optional @no-backup-p)
  "Delete current file, makes a backup~, closes the buffer.

Backup filename is “‹name›~‹date time stamp›~”. Existing file of the same name is overwritten. If the file is not associated with buffer, the backup file name starts with “xx_”.

When `universal-argument' is called first, don't create backup.

URL `http://ergoemacs.org/emacs/elisp_delete-current-file.html'
Version 2016-07-20"
  (interactive "P")
  (let* (
         ($fname (buffer-file-name))
         ($buffer-is-file-p $fname)
         ($backup-suffix (concat "~" (format-time-string "%Y%m%dT%H%M%S") "~")))
    (if $buffer-is-file-p
        (progn
          (save-buffer $fname)
          (when (not @no-backup-p)
            (copy-file
             $fname
             (concat $fname $backup-suffix)
             t))
          (delete-file $fname)
          (message "Deleted. Backup created at 「%s」." (concat $fname $backup-suffix)))
      (when (not @no-backup-p)
        (widen)
        (write-region (point-min) (point-max) (concat "xx" $backup-suffix))
        (message "Backup created at 「%s」." (concat "xx" $backup-suffix))))
    (kill-buffer (current-buffer))))

(defun xah-delete-current-file (&optional @no-backup-p)
  "Delete current file.
If buffer is a file, make a backup~, push content to `kill-ring' (unless buffer is greater than 1 mega bytes.), then delete it.
If buffer is not associate with a file, push content to `kill-ring' (unless buffer is greater than 1 mega bytes.), then kill it.
If buffer is dired, do nothing.

URL `http://ergoemacs.org/emacs/elisp_delete-current-file.html'
Version 2020-02-14 2021-08-06"
  (interactive "P")
  (if (eq major-mode 'dired-mode)
      (message "you in dired. nothing's done.")
    (let (($bstr (buffer-string)))
      (when (> (length $bstr) 0)
        (if (< (point-max) 1000000)
            (kill-new $bstr)
          (message "Content not copied. buffer size is greater than 1 megabytes.")))
      (if (buffer-file-name)
          (xah-delete-current-file-make-backup @no-backup-p)
        (when (buffer-file-name)
          (when (file-exists-p (buffer-file-name))
            (progn
              (delete-file (buffer-file-name))
              (message "Deleted file: 「%s」." (buffer-file-name)))))
        (let ((buffer-offer-save nil))
          (set-buffer-modified-p nil)
          (kill-buffer (current-buffer)))))))

(defun xah-next-window-or-frame ()
  "Switch to next window or frame.
If current frame has only one window, switch to next frame.
If `universal-argument' is called first, do switch frame.
Version 2017-01-27"
  (interactive)
  (if current-prefix-arg
      (other-frame 1)
    (if (one-window-p)
        (other-frame 1)
      (other-window 1))))

(defun xah-unsplit-window-or-next-frame ()
  "Unsplit window. If current frame has only one window, switch to next frame.
Version 2017-01-29"
  (interactive)
  (if (one-window-p)
      (other-frame 1)
    (delete-other-windows)))


(defun create-shell ()
  "creates a shell with a given name"
  (interactive);; "Prompt\n shell name:")
  (let ((shell-name (read-string "shell name: " nil)))
    (shell (concat "*" shell-name "*"))))


(defun run-bash-in-emacs ()
  (interactive)
  (let ((explicit-shell-file-name "D:/emacs_env/PortableGit/bin/bash.exe"))
    ;; (call-interactively 'shell)
    (shell "*bash*")
    ;; 使中文输出不乱码
    (set-buffer-process-coding-system 'utf-8 'utf-8)
    ))

(defun run-cmdexe-in-emacs ()
  (interactive)
  (let ((shell-file-name "C:/Windows/SysWOW64/cmd.exe"))
    (shell "*cmd.exe*")))


(defun run-bash-here ()
  (interactive)
  (let* ((cur-dir default-directory)
        (command (format
                  "D:\\Installed\\Git-for-windows\\git-bash.exe --cd=%s"
                  (s-replace "/" "\\\\" cur-dir)
                  ))
        )
    ;; (message "command:%s" command)
    (async-shell-command-no-window command)
    )
  )

(defun run-bash-current-project ()
  (interactive)
  (let* ((project-dir (locate-dominating-file default-directory ".git"))
        (command (format
                  "D:\\Installed\\Git-for-windows\\git-bash.exe --cd=%s"
                  (s-replace "/" "\\\\" project-dir)))
        )
    ;; (message "command:%s" command)
    (setq command (s-replace-regexp "~" (subst-char-in-string ?\\ ?/ (getenv "HOME")) command))
    (async-shell-command-no-window command)
    )
  )


(defun eye-search-in-dir ()
  (interactive)
  (let* ((keyword (color-rg-read-input))
         (dir (counsel-read-directory-name "Search in dir: " default-directory))
         ;; (color-rg-search-ignore-rules "-g \"!#*\"")
         )
    ;; (color-rg-search-input keyword dir)
    (eye/search-and-by-rg keyword dir)
    )
  )


(defun eye--build-or-regexp-by-keywords (keywords)
  "构建or语法的正则"
  (let (wordlist tmp regexp)
    (setq wordlist (split-string keywords " "))
    (dolist (word wordlist)
      (setq tmp (format "(%s)" word))
      (if regexp (setq regexp (concat regexp "|")))
      (setq regexp (concat regexp tmp)))
    regexp
    ))

(defun eye--build-and-regexp-by-keywords (keywords)
  "构建and语法的正则"
  (let (reg wlist fullreg reglist)
    (setq wlist (split-string keywords " "))
    (dolist (w1 wlist)
      (setq reg w1)
      (dolist (w2 wlist)
	(unless (string-equal w1 w2)
	  (setq reg (format "%s.*%s" reg w2))))
      (setq reg (format "(%s)" reg))
      (add-to-list 'reglist reg)
      )
    ;; 还要反过来一次
    (dolist (w1 wlist)
      (setq reg w1)
      (dolist (w2 (reverse wlist))
	(unless (string-equal w1 w2)
	  (setq reg (format "%s.*%s" reg w2))))
      (setq reg (format "(%s)" reg))
      (add-to-list 'reglist reg)
      )

    (dolist (r reglist)
      (if fullreg (setq fullreg (concat fullreg "|")))
      (setq fullreg (concat fullreg r)))

    fullreg
    ))

(defun eye/search-or-by-rg (&optional keyword dir)
  "以空格分割关键词，以or条件搜索多个关键词的内容
如果要搜索tag，可以输入`:tag1 :tag2 :tag3'
"
  (interactive)
  (let* ((keywords (read-string "Or Search(rg): "))
	     (regexp (eye--build-or-regexp-by-keywords keywords)))
    (message "search regexp:%s" regexp)
    (color-rg-search-input regexp)
    ))


(defun eye/search-and-by-rg (&optional keyword dir)
  "以空格分割关键词，以and条件搜索同时包含多个关键词的内容
如果要搜索tag，可以输入`:tag1 :tag2 :tag3'
"
  (interactive)
  (let* ((keywords (or keyword (read-string "And Search(rg): ")))
         (dir (or dir default-directory))
	     (regexp (eye--build-and-regexp-by-keywords keywords)))
    (message "search regexp:%s" regexp)
    (color-rg-search-input regexp dir)
    ))



(defun eye--read-input (prompt)
  (require 'color-rg)
  (let* ((current-symbol (color-rg-pointer-string))
         (input-string
          (string-trim
           (read-string
            (concat prompt (format " (%s): " current-symbol))
            nil
            'color-rg-read-input-history
            ))))
    (when (string-blank-p input-string)
      (setq input-string current-symbol))
    input-string))

(defun eye-occur ()
  (interactive)
  (let* ((keyword (eye--read-input "Find"))
         )
    (occur keyword)
    )
  )

(defun eye-find-h-or-c ()
  "查找当前文件对应的头文件或源文件"
  (interactive)
  (let* ((file-name (buffer-name))
         find-file-name)
    (cond ((string-match ".h" file-name)
           (progn
             (if (file-exists-p (concat (file-name-base file-name) ".c"))
                 (find-file (concat (file-name-base file-name) ".c"))
               (if (file-exists-p (concat (file-name-base file-name) ".cpp"))
                   (find-file (concat (file-name-base file-name) ".cpp"))
                 (counsel-git (concat (file-name-base file-name) ".c"))
                 )
               )
             )
           )
          ((string-match ".c" file-name)
           (progn
             (if (file-exists-p (concat (file-name-base file-name) ".h"))
                 (find-file (concat (file-name-base file-name) ".h"))
               (if (file-exists-p (concat (file-name-base file-name) ".hpp"))
                   (find-file (concat (file-name-base file-name) ".hpp"))
                 (counsel-git (concat (file-name-base file-name) ".h"))
                 )
               )
             )
           )
          (t
           (message "Current file is not .h or .c")
           ))
    )
  )


(defun bind-key (keymap keycode function &optional file)
  "绑定全局按键，或者已经存在的keymap，file参数用于autoload"
  (if file
      (autoload function file))
  (define-key keymap (kbd keycode) function))

(defmacro bind-mode-key (mode keymap keycode function &optional file)
  "定义和模式相关的key，由于keymap需要存在才能调用define-key，这里使用defmacro，mode加载后才执行define-key。
示例：(bind-mode-key 'cc-mode c++-mode-map \"M-p\" 'find-file)
"
  `(progn
     (if ,file
	 (autoload ,function ,file))
     (with-eval-after-load ,mode
       (define-key ,keymap (kbd ,keycode) ,function))))


(defun async-shell-command-no-window (command)
  (let ((display-buffer-alist
         (list (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil))))
		(output-buffer (format "*Async Shell Command*.%s" (random 1000)))
		(error-buffer (format "*Async Shell Command*.%s" (random 1000)))
		)
    (async-shell-command command output-buffer error-buffer)))



(defun eye-open-url ()
  (interactive)
  (let* ((url (read-string "Url: ")))
    (browse-url-default-browser url)
    )
  )



(defun eye/web-search ()
  (interactive)
  (let* ((keyword (read-string "Web search keyword: "))
         (engine (ivy-read "Search by: " '("google" "bing" "duck" "github" "bilibili")))
         url
         )
    (cond
     ((string-equal engine "google")
      (setq url (concat "https://www.google.com/search?q=" keyword)))
     ((string-equal engine "bing")
      (setq url (concat "https://www.bing.com/search?q=" keyword)))
     ((string-equal engine "duck")
      (setq url (concat "https://www.duckduckgo.com/search?q=" keyword)))
     ((string-equal engine "github")
      (setq url (concat "https://github.com/search?q=" keyword)))
     ((string-equal engine "youtube")
      (setq url (concat "https://www.youtube.com/results?search_query=" keyword)))
     ((string-equal engine "bilibili")
      (setq url (concat "https://search.bilibili.com/all?keyword=" keyword)))
     (t (setq url (concat "https://www.bing.com/search?q=" keyword)))
     )
    (eye/open-url url)
    ))


(defun eye/search-cpp-doc ()
  "Find cpp reference document."
  (interactive)
  (let ((url "http://zh.cppreference.com/mwiki/index.php?search=")
        (keyword (or (thing-at-point 'word) (read-string "Search cpp: ")))
        )
    (setq url (concat url keyword))
    (browse-url-firefox url)))



(defun my-www-get-page-title (url)
  "根据url获取标题"
  (interactive)
  (save-excursion
    (shell-command (concat "curl " url) "*curl*")
    (switch-to-buffer "*curl*")
    (goto-char (point-min))
    (let ((p1 (search-forward "<title>"))
          (p2 (- (search-forward "</") 2))
          title
          )
      (when (and p1 p2)
        (setq title (buffer-substring-no-properties p1 p2))
        (kill-buffer "*curl*")
        )
      title
      )))

(defun my-url-linkify ()
  "Make URL at cursor point into an Org-mode link.
If there's a text selection, use the text selection as input.

Example: http://example.com/xyz.htm
becomes
\[\[http://example.com/xyz.htm\]\[Source example.com\]\]

Adapted code from: http://ergoemacs.org/emacs/elisp_html-linkify.html"
  (interactive)
  (let (resultLinkStr bds p1 p2 domainName)
    ;; get the boundary of URL or text selection
    (if (region-active-p)
        (setq bds (cons (region-beginning) (region-end)) )
      (setq bds (bounds-of-thing-at-point 'url))
      )
    ;; set URL
    (setq p1 (car bds))
    (setq p2 (cdr bds))
    (let (
          (url (buffer-substring-no-properties p1 p2))
          )
      ;; retrieve title
      (let ((title (my-www-get-page-title url)))
        (message (concat "title is: " title))
        ;;(setq url (replace-regexp-in-string "&" "&" url))
        (let ((resultLinkStr (concat "[[" url "][" title "]]")))
          ;; delete url and insert the link
          (delete-region p1 p2)
          (insert resultLinkStr)
          )
        )
      )
    )
  )


;;----------------------------------------------------------------
;; 从 browse-url-chromium 修改而来
(defun browse-url-google (url &optional _new-window)
  "Ask the Chromium WWW browser to load URL.
Default to the URL around or before point.  The strings in
variable `browse-url-chromium-arguments' are also passed to
Chromium.
The optional argument NEW-WINDOW is not used."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply 'start-process
	   (concat "chrome " url) nil
	   (executable-find "chrome")
	   (append
	    browse-url-chromium-arguments
	    (list url)))))

(setq my/default-browser "firefox")

(defun eye/open-url (url)
  (interactive)
  (if (string-equal "firefox" my/default-browser)
      (browse-url-firefox url)
    (browse-url-google url)
    )
  )

(defun eye/set-default-browser ()
  (interactive)
  (let* ((browser (ivy-read "Set browser to: " '("firefox" "chrome"))))
    (setq my/default-browser browser)
    )
  )

(defun eye--ivy-height ()
  "动态获取可用frame高度"
  (* (/ (frame-height) 8) 7)
  )



(defun eye/open-svn-or-git-file ()
  (interactive)
  (let ((git-repo-dir (locate-dominating-file ".git" ".git"))
        (svn-repo-dir (locate-dominating-file ".svn" ".svn"))
        )
    (if git-repo-dir
        (eye/counsel-git)
      (eye/open-svn-file)
      )
    )
  )


(defun eye/open-svn-file ()
  (interactive)
  (let ((default-directory (locate-dominating-file ".svn" ".svn"))
        (svn-file-list (locate-dominating-file ".svn-file-list" ".svn-file-list"))
        (filepath nil)
        )
    (if (not svn-file-list)
        (shell-command "svn -R list > .svn-file-list")
      )
    (setq filepath (ivy-read "Open file: " (eye-get-locale-file-list-content ".svn-file-list")))
    (if filepath
        (find-file filepath)
      )))



(defun eye/open-project-file ()
  (interactive)
  (let* (
         (ivy-posframe-height (eye--ivy-height))
         (ivy-height ivy-posframe-height)
         (project-dir (ivy-read "Open project file: " (eye-get-locale-project-list)) )
         )
    (if (and project-dir (file-exists-p project-dir))
        (let ((default-directory project-dir)
              )
          (eye/open-svn-or-git-file))
      (eye/open-svn-or-git-file)
      )
    ))

(defun eye/counsel-git ()
  "基于当前项目查找打开文件"
  (interactive)
  (let* (
         (ivy-posframe-height (eye--ivy-height))
         (ivy-height ivy-posframe-height)
         )
    (counsel-git)
    ))

(defun eye/find-project-file-in-dir ()
  "基于当前目录查找打开文件"
  (interactive)
  (let* ((init-input (f-relative
                      default-directory
                      (locate-dominating-file default-directory ".git"))))
    (counsel-git init-input)
    ))


;; https://stackoverflow.com/questions/2321904/elisp-how-to-save-data-in-a-file
(defun print-to-file (filename data)
  (with-temp-file filename
    (prin1 data (current-buffer))))

(defun read-from-file (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (cl-assert (eq (point) (point-min)))
    (read (current-buffer))))

;; http://xahlee.info/emacs/emacs/elisp_association_list.html
(defun eye/run-app ()
  "Run app from emacs
.app-list.el file content
((\"notepad\" . \"D:\\emacs_env\\Notepad2.exe\")
 (\"calc\" . \"C:\\Windows\\SysWOW64\\calc.exe\")
 )
"
  (interactive)
  (let* ((apps-list (read-from-file "~/.emacs.d/.app-list.el"))
         (app-names nil)
         (select-app "")
         )
    (dolist (app-kv apps-list)
      (add-to-list 'app-names (car app-kv))
      )
    (setq select-app (ivy-read "Run: " app-names))
    (async-shell-command-no-window (cdr (assoc select-app apps-list)))
    ))


;; @see https://emacs-china.org/t/leader-vscode/19166/29
(defun open-with-vscode ()
  "Open current file with vscode.
command line option, see https://code.visualstudio.com/docs/editor/command-line
"
  (interactive)
  (let* ((line (number-to-string (line-number-at-pos)))
        (column (number-to-string (current-column)))
        (command (concat "code.exe " buffer-file-name ":" line ":" column " --goto"))
        )
    (setq command (replace-regexp-in-string "/" "\\\\" command))
    (async-shell-command-no-window command)
    ))

(defun open-with-editplus ()
  "Open current file with editplus.
command line option, see https://documentation.help/EditPlus/commandline_option.htm
"
  (interactive)
  (let* ((line (number-to-string (line-number-at-pos)))
        (column (number-to-string (current-column)))
        (command (concat "editplus.exe " buffer-file-name "  -cursor " line ":" column))
        )
    (setq command (replace-regexp-in-string "/" "\\\\" command))
    (async-shell-command-no-window command)
    ))

(defun open-with-typora ()
  "Open current file with typora.
Please add typora.exe dir to .system-path-win32-list
"
  (interactive)
  (let* ((line (number-to-string (line-number-at-pos)))
        (column (number-to-string (current-column)))
        (command (concat "typora.exe " buffer-file-name))
        )
    (setq command (replace-regexp-in-string "/" "\\\\" command))
    (async-shell-command-no-window command)
    ))


(defun eye/save-buffer ()
  (interactive)
  (when (buffer-file-name)
    (save-buffer)
    ))

(defun eye-add-timer (secs repeat func)
  (run-with-idle-timer secs repeat func)
  )

(defun eye/imenu ()
  (interactive)
  (if (eq 'org-mode major-mode)
      (counsel-org-goto)
    (counsel-imenu))
  (recenter-top-bottom 0)
  )

(defun my-generate-uuid ()
  (let* ((uuid (string-trim (eye-async-shell-command "uuidgen.exe"))))
    uuid
    ))


(defun eye-create-short-uuid ()
  (require 's)
  (s-left 8 (my-generate-uuid)))


(defun eye/insert-short-uuid ()
  (interactive)
  (insert (eye-create-short-uuid)))




(defun eye/find-header-or-source-file (&optional is-open-other-window)
  "Find the header or source file of this one."
  (interactive "P")
  (let ((full-base-name (file-name-sans-extension buffer-file-name)) ;;无后缀的文件路径
	(header-or-source-path nil))

    (cond ((string-match "\\.h" buffer-file-name)
	   (if (file-exists-p (concat full-base-name ".c"))
	       (setq header-or-source-path (concat full-base-name ".c"))
	     (setq header-or-source-path (concat full-base-name ".cpp"))))

	  ((string-match "\\.c" buffer-file-name)
	   (setq header-or-source-path (concat full-base-name ".h")))

	  ((string-match "\\.cpp" buffer-file-name)
	   (setq header-or-source-path (concat full-base-name ".h")))

	  (t (message "File name no suffix")))
    (if header-or-source-path
	(if is-open-other-window (find-file-other-window header-or-source-path)
	  (find-file header-or-source-path))
      (error "Unable to find a header or source file"))))

(defun eye/goto-error-with-full-path ()
  (interactive)
  (save-excursion
    (if (region-active-p)
        (let* ((str (buffer-substring-no-properties (region-beginning) (region-end)))
               pan path line
               )
          (setq pan (nth 0 (s-split ":" str)))
          (setq path (nth 1 (s-split ":" str)))
          (setq path (s-replace "\\" "/" path))
          (setq path (concat pan ":" path))
          (setq line (nth 2 (s-split ":" str)))
          (if (file-exists-p path)
              (progn
                (other-window 1)
                (find-file path)
                (goto-line (string-to-number line))
                )
              )
          ;; (message "%s" path)
          )
      (message "No region selected!")
      )
    ))


(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (remove-if-not 'buffer-file-name (buffer-list)))))

(defun clear-messages-buffer ()
  (interactive)
  (let ((inhibit-read-only t))
    (with-current-buffer "*Messages*"
      (erase-buffer))))



;; https://www.reddit.com/r/emacs/comments/t1736p/is_there_a_function_or_macro_that_handles_xxx_and/
;; (with-progress "test"
;;   (require 'recentf))
;;
(defmacro with-progress (pr-args &rest body)
  "Perform BODY wrapped in a progress reporter.
PR-ARGS is the list of arguments to pass to
`make-progress-reporter'; it can be a single string for the
message, as well.  If you want to use a formatted string, wrap
the `format' call in a list."
  (declare (indent 1))
  (let ((reporter (gensym))
        (pr-args (if (listp pr-args) pr-args (list pr-args))))
    `(let ((,reporter (make-progress-reporter ,@pr-args)))
       (prog1 (progn ,@body)
         (progress-reporter-done ,reporter)))))



(defun intellij-backspace (arg)
  "智能删除到缩进位置：https://emacs-china.org/t/backspace/18163/5"
  (interactive "*P")
  (if (or (region-active-p) (not (looking-back "^[[:space:]]*" (line-beginning-position))))
      (backward-delete-char-untabify (prefix-numeric-value arg))
    (let* ((beg (point))
           (end (progn (indent-for-tab-command) (point))))
      (when (<= beg end)
        (if (save-excursion (forward-line -1) (line-blank-p))
            (progn (delete-region (line-beginning-position 0) (line-beginning-position)) (back-to-indentation))
          (delete-indentation))))))

(defun line-blank-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))


(defun string-to-int (s) (string-to-number s))



(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))


(defun eye--get-unix-timestamp()
  "获取unix时间"
  (format "%d" (time-to-seconds)))


(defun eye/insert-bookname-symbol ()
  (interactive)
  (insert "《》")
  )


(defun eye/refile-current-to-dir ()
  "移动当前文件到另一个目录"
  (interactive)
  (require 'f)
  (when (buffer-file-name)
    (f-move (buffer-file-name) (ido-read-directory-name "Move to: "))
    )
  )




(defun eye/swap-buffer ()
  "quick switch two window's buffer"
  (interactive)
  (save-excursion
    (let ((cur-buf (buffer-name))
          other-buf
          )
      (other-window 1)
      (setq other-buf (buffer-name))
      (switch-to-buffer cur-buf)
      (other-window 1)
      (switch-to-buffer other-buf)
      )
    )
  )


(defun eye/create-tmp-buffer ()
  (interactive)
  (let* ((height (window-height))
         (delta (/ height 5)))
    (delete-other-windows)
    (split-window-vertically)
    (other-window 1)
    (xah-new-empty-buffer)
    (shrink-window delta)
    ))

(defun eye-empty-func ()
  (interactive)
  (user-error "Empty function"))

(defun my-M-x ()
  (interactive)
  (cond
    ((fboundp 'counsel-M-x)
     ;; `counsel-M-x' will use `smex' to remember history
     (counsel-M-x))
    ((fboundp 'smex)
     (smex))
    (t
     (execute-extended-command))))

(defun my-find-file ()
  (interactive)
  (cond
   ((fboundp 'counsel-find-file)
    (counsel-find-file))
   (t
    (find-file)
    )
   )
  )


(defun my-switch-buffer ()
  (interactive)
  (cond
   ((fboundp 'counsel-ibuffer)
    ;; (counsel-switch-buffer) ;; 会自动切换预览buffer，老是闪，并不需要。
    ;; (counsel-buffer-or-recentf) ;; counsel-ibuffer 或者 counsel-buffer-or-recentf
    (ivy-switch-buffer) ;; ivy-switch-buffer不会切换预览
    )
   (t
    (switch-to-buffer)
    )
   )
  )


(defun unset-mode-key (modemap kbd)
  (define-key modemap kbd nil))



(defun eye/open-startup-dir ()
  "打开windows启动项目录"
  (interactive)
  (let* ((default-directory "C:\\Users\\XYSM\\AppData\\Roaming\\Microsoft\\Windows\\Start Menu\\Programs\\Startup"))
    (eye/open-explorer)
    )
  )



(defun eye-get-emacs-submodule-list ()
  (let ((pkglist))
    (dolist (var (directory-files (concat user-emacs-directory ".git/modules/packages") nil))
      (if (and (not (string-equal var "..")) (not (string-equal var ".")))
          (add-to-list 'pkglist var)
        ))
    pkglist
    ))

(defun eye/git-remove-submodule ()
  (interactive)
  (let ((default-directory user-emacs-directory)
        ;; (pkgname (read-string "package name:"))
        (pkgname (ivy-read "Remove package:" (eye-get-emacs-submodule-list)))
        )
    ;; Remove the submodule entry from .git/config
    (shell-command (format "git submodule deinit -f packages/%s" pkgname))
    ;; Remove the submodule directory from the superproject's .git/modules directory
    (shell-command (format "rm -rf .git/modules/packages/%s" pkgname))
    ;; Remove the entry in .gitmodules and remove the submodule directory located at path/to/submodule
    (shell-command (format "git rm -f packages/%s" pkgname))
    (shell-command (format "git commit -m 'remove submodule %s'" pkgname))
    (message "Remove submodule %s finished!" pkgname)
    )
  )

(defun eye-current-theme-is-dark ()
  (equal (frame-parameter nil 'background-mode) 'dark)
  )

;; 半屏滚动
(defun scroll-half-page-down ()
  "scroll down half the page"
  (interactive)
  (scroll-down 3));; (/ (window-body-height) 2)))

(defun scroll-half-page-up ()
  "scroll up half the page"
  (interactive)
  (scroll-up 3));; (/ (window-body-height) 2)))



(defun split-window-right-and-switch-to-scratch()
  (interactive)
  (split-window-right)
  (other-window 1)
  (switch-to-buffer "*scratch*"))

(defun split-window-below-and-switch-to-scratch()
  (interactive)
  (split-window-below)
  (other-window 1)
  (switch-to-buffer "*scratch*"))




(provide 'init-utils)
