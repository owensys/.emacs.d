
;; preview latex settings

;; (setq org-latex-default-class "article")
(with-eval-after-load 'ox-latex
 ;; http://orgmode.org/worg/org-faq.html#using-xelatex-for-pdf-export
 ;; latexmk runs pdflatex/xelatex (whatever is specified) multiple times
 ;; automatically to resolve the cross-references.
 (setq org-latex-pdf-process '("latexmk -xelatex -quiet -shell-escape -f %f"))
 (add-to-list 'org-latex-classes
               '("elegantpaper"
                 "\\documentclass[lang=cn]{elegantpaper}
                 [NO-DEFAULT-PACKAGES]
                 [PACKAGES]
                 [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


 (add-to-list 'org-latex-classes
               '("myself"
                 "\\documentclass[lang=cn]{myself}
                 [NO-DEFAULT-PACKAGES]
                 [PACKAGES]
                 [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (setq org-latex-listings 'minted)
  (add-to-list 'org-latex-packages-alist '("" "minted"))

  (setq org-latex-minted-options
      '(("bgcolor" "codebg")
        ;; frame=lines: draws two lines, one on top and one at the bottom of the code to frame it. Other possible values are leftline, topline, bottomline and single.
        ("frame" "lines") ;; 开启边框
        ("rulecolor" "codeborder") ;; 边框颜色
        ("framesep" "3mm") ;; 边框和代码间距
        ("baselinestretch" "1.0") ;; 代码行间距
        ))

  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 自定义导出过程
;; (setq org-preview-latex-default-process 'imagemagick)
;; (setq org-format-latex-header (concat "\\PassOptionsToPackage{normalem}{ulem}\n" org-format-latex-header))
;; (setq org-latex-default-packages-alist '(("T1" "fontenc" t ("pdflatex"))
;;                                          ("" "graphicx" t)
;;                                          ("" "grffile" t)
;;                                          ("" "longtable" nil)
;;                                          ("" "wrapfig" nil)
;;                                          ("" "rotating" nil)
;;                                          ("normalem" "ulem" t)
;;                                          ("" "amsmath" t)
;;                                          ("" "textcomp" t)
;;                                          ("" "amssymb" t)
;;                                          ("" "capt-of" nil)
;;                                          ("" "hyperref" nil)))

;; (setq org-latex-packages-alist '(("fontset=windows,UTF8" "ctex" t)
;;                                  ("range-phrase=-,range-units=single" "siunitx" t)
;;                                  ("" "upgreek" t)))

;; convert用参数 -trim -antialias 可以剪去多余的部分，而不是显示很大一张图片
(setq org-preview-latex-process-alist
      '(
        (imagemagick
         :programs
         ("pdflatex" "convert")
         ;; ("xelatex" "convert")
         :description "pdf > png"
         :message "you need to install the programs: miktex and imagemagick."
         :image-input-type "pdf"
         :image-output-type "png"
         :image-size-adjust (1.0 . 1.0)
         :latex-compiler
         ("pdflatex.exe -shell-escape -output-directory %o %f") ;; minted need -shell-escape
         ;; ("xelatex -interaction nonstopmode -output-directory %o %f")
         :image-converter
         ("convert.exe -density %D -trim -antialias  %f -quality 90 %O")
         )
        )
      )


;; 放大一下
(plist-put org-format-latex-options :scale 2.0)

(defun convert-path-to-native-path (path)
  (let* ((new-path (s-replace "/" "\\" path)))
    new-path
    ))

(setq windows-temp-directory "C:\\Users\\Administrator\\AppData\\Local\\Temp")

(defun gen-temp-path-of-file (file-path)
(concat (format "%s\\%s" windows-temp-directory (file-name-nondirectory file-path)))
)


;; 默认的org-compile-file没有转换路径/为\\，导致无法生成临时的pdf，也无法生成临时图片，这里重写一下
;; M-x org-preview-latex-fragment 进行预览

;; 设置一下tmp环境变量, https://emacs-china.org/t/org/19465/5
;; (setenv "tmp" "d:\\tmp")
;; (setenv "temp" "d:\\tmp")
;; 需要在启动脚本中进行设置，用setenv设置不起作用
;; 设置后就不需要重定义org-compile-file函数了。


(defun eye--org-compile-file (source process ext &optional err-msg log-buf spec)
  "Compile a SOURCE file using PROCESS.

PROCESS is either a function or a list of shell commands, as
strings.  EXT is a file extension, without the leading dot, as
a string.  It is used to check if the process actually succeeded.

PROCESS must create a file with the same base name and directory
as SOURCE, but ending with EXT.  The function then returns its
filename.  Otherwise, it raises an error.  The error message can
then be refined by providing string ERR-MSG, which is appended to
the standard message.

If PROCESS is a function, it is called with a single argument:
the SOURCE file.

If it is a list of commands, each of them is called using
`shell-command'.  By default, in each command, %b, %f, %F, %o and
%O are replaced with, respectively, SOURCE base name, name, full
name, directory and absolute output file name.  It is possible,
however, to use more place-holders by specifying them in optional
argument SPEC, as an alist following the pattern

  (CHARACTER . REPLACEMENT-STRING).

When PROCESS is a list of commands, optional argument LOG-BUF can
be set to a buffer or a buffer name.  `shell-command' then uses
it for output."

  (let* ((base-name (file-name-base source))
	     (full-name (file-truename source))
	     (out-dir (or (file-name-directory source) "./"))
         (full-out-dir (file-truename out-dir))
	     (output (expand-file-name (concat base-name "." ext) out-dir))
	     (time (current-time))
         ;; windows path
         (native-source (gen-temp-path-of-file source))
         (native-output (gen-temp-path-of-file output))
         (native-out-dir windows-temp-directory)
	     (err-msg (if (stringp err-msg) (concat ".  " err-msg) "")))
    (message "my source: basename:%s, full-name:%s, out-dir:%s, output:%s"
             base-name full-name out-dir output)
    (save-window-excursion
      (pcase process
	    ((pred functionp) (funcall process (shell-quote-argument source)))
	    ((pred consp)
	     (let ((log-buf (and log-buf (get-buffer-create log-buf)))
	           (spec (append spec
			                 `((?b . ,(shell-quote-argument base-name))
			                   (?f . ,(shell-quote-argument native-source))
			                   (?F . ,(shell-quote-argument full-name))
			                   (?o . ,(shell-quote-argument native-out-dir))
			                   (?O . ,(shell-quote-argument native-output))))))
	       (dolist (command process)
             (message "exec cmd:%s" (format-spec command spec))
	         (shell-command (format-spec command spec) log-buf))
	       (when log-buf (with-current-buffer log-buf (compilation-mode)))))
	    (_ (error "No valid command to process %S%s" source err-msg))))
    ;; Check for process failure.  Output file is expected to be
    ;; located in the same directory as SOURCE.
    (unless (org-file-newer-than-p output time)
      (error (format "File %S wasn't produced%s" output err-msg)))
    output))




;; 由于latex预览比较慢，写几个函数，插入图片
;; 2022-02-20 发现公式没变的情况下，重新打开文档，并不会导致重新生成所有的latex预览
;; 所以以下函数可以不用
;; 以下方式生成的图片会显得很小，用scalebox又会导致图片上方多出来空白区域
;; 实际看发现上方空白并不是图片里的，而是orgmode本身显示图片会这样
;; 空白区域是highlight样式
;; org-latex-preview生成的图片是放在ltximg 目录下的, org-preview-latex-image-directory
;; (set-face-attribute 'highlight nil :inherit 'default)

(setq org-preview-latex-image-directory "~/attach/ltximg/") ;; 指定preview生成的图片放在不同目录下

(defun write-body-to-tex-file (tex-body output-file)
  (with-temp-buffer
    (insert "\\PassOptionsToPackage{normalem}{ulem}\n")
    (insert "\\documentclass[preview]{standalone}\n")
    (insert "\\usepackage{amsmath}\n")
    (insert "\\usepackage{graphicx}\n")
    (insert "\\usepackage{adjustbox}\n")
    (insert "\\usepackage[fontset=windows,UTF8]{ctex}\n")
    (insert "\\usepackage[range-phrase=-,range-units=single]{siunitx}\n")
    (insert "\\usepackage[T1]{fontenc}\n")
    (insert "\\usepackage[normalem]{ulem}\n")
    (insert "\\setlength{\\textwidth}{\\paperwidth}\n")
    (insert "\\addtolength{\\textwidth}{-3cm}\n")
    (insert "\\setlength{\\oddsidemargin}{1.5cm}\n")
    (insert "\\addtolength{\\oddsidemargin}{-2.54cm}\n")
    (insert "\\setlength{\\evensidemargin}{\\oddsidemargin}\n")
    (insert "\\setlength{\\textheight}{\\paperheight}\n")
    (insert "\\addtolength{\\textheight}{-\\headheight}\n")
    (insert "\\addtolength{\\textheight}{-\\headsep}\n")
    (insert "\\addtolength{\\textheight}{-\\footskip}\n")
    (insert "\\addtolength{\\textheight}{-3cm}\n")
    (insert "\\setlength{\\topmargin}{1.5cm}\n")
    (insert "\\addtolength{\\topmargin}{-2.54cm}\n")

    (insert "\\begin{document}\n")
    (insert tex-body)
    (insert "\n")
    (insert "\\end{document}\n")
    (write-file output-file)
    (message "write tex file finished.");
    )
  )

(defun execute-tex-to-pdf (tex-file)
  (let* ((cmd (format "pdflatex %s "
                      (convert-home-path-to-abs-path tex-file))))
    (save-window-excursion
      (shell-command cmd nil nil))
    (message "tex to pdf executed:%s" cmd)
    ))

(defun execute-pdf-to-png (pdf-file png-file)
  (let* ((cmd (format "convert -density 72 %s -trim -antialias -quality 100 %s"
                      (convert-home-path-to-abs-path pdf-file)
                      (convert-home-path-to-abs-path png-file)
                      )))
    (save-window-excursion
      (shell-command cmd nil nil))
    (message "pdf to png executed:%s" cmd)
    ))

(defun execute-png-resize (infile outfile)
  (let* ((cmd (format "%s %s 640 480 %s"
		      (executable-find "resize_image.exe")
		      ;; (convert-home-path-to-abs-path (expand-file-name infile default-directory))
                      infile
		      (convert-home-path-to-abs-path outfile))))
    (save-window-excursion
      (shell-command cmd nil nil))
    (message "resize png executed:%s" cmd)
    ))

(defun eye/insert-latex-image ()
  (interactive)
  (insert (format "[[file:~/attach/%s_tex.png]]" (s-left 8 (my-generate-uuid))))
  )


;; (defun eye/insert-latex-image ()
;;   (interactive)
;;   (insert (format "[[file:~/attach/%s_tex.png][∑]]" (s-left 8 (my-generate-uuid))))
;;   )


(defun eye/edit-latex-image ()
  (interactive)
  (let* ((full-link (thing-at-point 'filename))
         (png-file (s-replace "file:" "" full-link))
         (tex-file (s-replace ".png" ".tex" png-file))
         )
    (other-window 1)
    (find-file tex-file)
    ;; (message "tex file:%s" tex-file)
    ))

(defun convert-home-path-to-abs-path (path)
  "convert ~ to absolute path on windows"
  (let* ((tmp (s-replace "~" (getenv "HOME") path)))
    (setq tmp (s-replace "/" "\\" tmp))
    tmp
    ))

(defun eye/convert-tex-to-png-hook ()
  (when (and
         (s-contains-p "attach/" (buffer-file-name))
         (s-contains-p "_tex.tex" (buffer-name))
         )
    (let* ((base-name (file-name-base (buffer-file-name)))
           (temp-name (format "%s_tmp" base-name))
	   (full-body (buffer-substring-no-properties (point-min) (point-max))) ;; 得到block内容
           (temp-tex-file (format "%s.tex" temp-name))
           (temp-pdf-file (format "%s.pdf" temp-name))
           ;; (temp-png-file1 (format "%s.png" temp-name))
           ;; (temp-png-file2  (format "%s_s.png" temp-name))
           (out-file (format "%s.png" base-name))
           )
      (if (file-exists-p out-file)
          (f-delete out-file)) ;; remove old file
      (write-body-to-tex-file full-body temp-tex-file)
      (execute-tex-to-pdf temp-tex-file)
      (if (file-exists-p temp-pdf-file)
          (progn
            (execute-pdf-to-png temp-pdf-file out-file)
            (if (file-exists-p out-file)
                (progn
                  (message "Generate png finished")
                  (f-delete temp-tex-file)
                  (f-delete temp-pdf-file)
                  )
              (message "Generate png failed")
              )
            ;; (if (file-exists-p temp-png-file1)
            ;;     (progn
            ;;       (execute-png-resize temp-png-file1 temp-png-file2)
            ;;       ;; (if (file-exists-p temp-png-file2)
            ;;       ;;     (f-move temp-png-file2 out-file)
            ;;       ;;   )
            ;;       )
            ;;   )
            ))
      ;; (f-delete temp-png-file1)
      )))


(add-hook 'after-save-hook 'eye/convert-tex-to-png-hook)





(provide 'init-latex)
