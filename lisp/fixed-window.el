(setq neo-buffer-name "asdfasdf") ;; #refs#

(defvar neo-global--window nil)

(defcustom neo-window-position 'left
  "*The position of NeoTree window."
  :group 'neotree
  :type '(choice (const left)
                 (const right)))

(defun neo-default-display-fn (buffer _alist)
  "Display BUFFER to the left or right of the root window.
The side is decided according to `neo-window-position'.
The root window is the root window of the selected frame.
_ALIST is ignored."
  (let ((window-pos (if (eq neo-window-position 'left) 'left 'right)))
    (display-buffer-in-side-window buffer `((side . ,window-pos)))))

(defun neo-window--init (window buffer)
  "Make WINDOW a NeoTree window.
NeoTree buffer is BUFFER."
  (neo-buffer--with-resizable-window
   (switch-to-buffer buffer)
   (set-window-parameter window 'no-delete-other-windows t)
   (set-window-dedicated-p window t))
  window)



(defmacro neo-global--with-buffer (&rest body)
  "Execute the forms in BODY with global NeoTree buffer."
  (declare (indent 0) (debug t))
  `(let ((neotree-buffer (neo-global--get-buffer)))
     (unless (null neotree-buffer)
       (with-current-buffer neotree-buffer
         ,@body))))

(defun neo-global--create-window ()
  "Create global neotree window."
  (let ((window nil)
        (buffer (neo-global--get-buffer t)))
    (setq window
          (select-window
           (display-buffer buffer neo-display-action)))
    (neo-window--init window buffer)
    ;;(neo-global--attach)
    (neo-global--reset-width)
    window))


(defun neo-global--window-exists-p ()
  "Return non-nil if neotree window exists."
  (and (not (null (window-buffer neo-global--window)))
       (eql (window-buffer neo-global--window) (neo-global--get-buffer))))

(defun neo-global--select-window ()
  "Select the NeoTree window."
  (interactive)
  (let ((window (neo-global--get-window t)))
    (select-window window)))

(defcustom neo-window-width 40
  "*Specifies the width of the NeoTree window."
  :type 'integer
  :group 'neotree)


(defun neo-buffer--lock-width ()
  "Lock the width size for NeoTree window."
  (if neo-window-fixed-size
      (setq window-size-fixed 'width)))

(defun neo-buffer--unlock-width ()
  "Unlock the width size for NeoTree window."
  (setq window-size-fixed nil))


(defmacro neo-buffer--with-resizable-window (&rest body)
  "Execute BODY in neotree window without `window-size-fixed' restriction."
  `(let (rlt)
     (neo-global--with-buffer
       (neo-buffer--unlock-width))
     (setq rlt (progn ,@body))
     (neo-global--with-buffer
       (neo-buffer--lock-width))
     rlt))


(defadvice balance-windows
    (around neotree-balance-windows activate)
  "Fix neotree inhibits balance-windows."
  (if (neo-global--window-exists-p)
      (let (old-width)
        (neo-global--with-window
          (setq old-width (window-width)))
        (neo-buffer--with-resizable-window
         ad-do-it)
        (neo-global--with-window
          (neo-global--set-window-width old-width)))
    ad-do-it))


(defun neo-global--get-window (&optional auto-create-p)
  "Return the neotree window if it exists, else return nil.
But when the neotree window does not exist and AUTO-CREATE-P is non-nil,
it will create the neotree window and return it."
  (unless (neo-global--window-exists-p)
    (setf neo-global--window nil))
  (when (and (null neo-global--window)
             auto-create-p)
    (setq neo-global--window
          (neo-global--create-window)))
  neo-global--window)

(defun neo-global--select-window ()
  "Select the NeoTree window."
  (interactive)
  (let ((window (neo-global--get-window t)))
    (select-window window)))

(defmacro neo-global--with-window (&rest body)
  "Execute the forms in BODY with global NeoTree window."
  (declare (indent 0) (debug t))
  `(save-selected-window
     (neo-global--select-window)
     ,@body))

(defun neo-global--set-window-width (width)
  "Set neotree window width to WIDTH."
  (neo-global--with-window
    (neo-buffer--with-resizable-window
     (neo-util--set-window-width (selected-window) width))))

(defun neo-global--reset-width ()
  "Set neotree window width to `neo-window-width'."
  (neo-global--set-window-width neo-window-width))



(defcustom neo-display-action '(neo-default-display-fn)
  "*Action to use for displaying NeoTree window.
If you change the action so it doesn't use
`neo-default-display-fn', then other variables such as
`neo-window-position' won't be respected when opening NeoTree
window."
  :type 'sexp
  :group 'neotree)


(defvar neo-global--buffer nil)

(defun neo-global--get-buffer (&optional init-p)
  "Return the global neotree buffer if it exists.
If INIT-P is non-nil and global NeoTree buffer not exists, then create it."
  (unless (equal (buffer-name neo-global--buffer)
                 neo-buffer-name)
    (setf neo-global--buffer nil))
  (when (and init-p
             (null neo-global--buffer))
    (save-window-excursion
      (setq neo-global--buffer
            (neo-buffer--create))))
  neo-global--buffer)


(defun neo-buffer--create ()
  "Create and switch to NeoTree buffer."
  (switch-to-buffer
   (generate-new-buffer-name neo-buffer-name))
  ;;(neotree-mode)
  ;; disable linum-mode
  (when (and (boundp 'linum-mode)
             (not (null linum-mode)))
    (linum-mode -1))
  ;; Use inside helm window in NeoTree
  ;; Refs https://github.com/jaypei/emacs-neotree/issues/226
  (setq-local helm-split-window-inside-p t)
  (current-buffer))



(setq neo-window-position 'right)

;;(display-buffer "#refs#" neo-display-action)
;;(neo-global--create-window)


(defun neo-global--get-window (&optional auto-create-p)
  "Return the neotree window if it exists, else return nil.
But when the neotree window does not exist and AUTO-CREATE-P is non-nil,
it will create the neotree window and return it."
  (unless (neo-global--window-exists-p)
    (setf neo-global--window nil))
  (when (and (null neo-global--window)
             auto-create-p)
    (setq neo-global--window
          (neo-global--create-window)))
  neo-global--window)

(defun neo-global--open ()
  "Show the NeoTree window."
  (let ((valid-start-node-p nil))
    (neo-global--with-buffer
      (setf valid-start-node-p (neo-buffer--valid-start-node-p)))
    ;;(if (not valid-start-node-p)
    ;;    (neo-global--open-dir (neo-path--get-working-dir))
      (neo-global--get-window t)))


(defcustom neo-toggle-window-keep-p nil
  "If not nil, not switch to *NeoTree* buffer when executing `neotree-toggle'."
  :type 'boolean
  :group 'neotree)

(defcustom neo-smart-open nil
  "*If non-nil, every time when the neotree window is opened, it will try to find current file and jump to node."
  :type 'boolean
  :group 'neotree)

(defcustom neo-window-fixed-size t
  "*If the neotree windows is fixed, it won't be resize when rebalance windows."
  :type 'boolean
  :group 'neotree)

(defun neo-buffer--valid-start-node-p ()
  (and (not (null neo-buffer--start-node))
       (file-accessible-directory-p neo-buffer--start-node)))

(defvar-local neo-buffer--start-node nil
  "Start node(i.e. directory) for the window.")

;;;###autoload
(defun neotree-show-v2 ()
  "Show the NeoTree window."
  (interactive)
  (let ((cw (selected-window))
        (path (buffer-file-name)))  ;; save current window and buffer
    (if neo-smart-open
        (progn
          (when (and (fboundp 'projectile-project-p)
                     (projectile-project-p)
                     (fboundp 'projectile-project-root))
            (neotree-dir (projectile-project-root)))
          (neotree-find path))
      (neo-global--open))
    (neo-global--select-window)
    (when neo-toggle-window-keep-p
      (select-window cw))))


;;(eye-install-packages '(("neotree" . "https://github.com/jaypei/emacs-neotree")))
;;(require 'neotree)
