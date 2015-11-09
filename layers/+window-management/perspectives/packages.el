;;; packages.el --- Perspectives Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(setq perspectives-packages '(persp-mode spaceline))

(defun perspectives/init-persp-mode ()
  (use-package persp-mode
    :commands spacemacs/layouts-micro-state
    :diminish persp-mode
    :init
    (progn
      (setq persp-nil-name "Default"
            persp-auto-resume-time -1
            persp-reset-windows-on-nil-window-conf nil
            persp-set-last-persp-for-new-frames nil
            persp-save-dir spacemacs-layouts-save-dir)

      (defvar spacemacs--layouts-ms-doc-toggle 0
        "Display a short doc when nil, full doc otherwise.")

      (defvar spacemacs--last-selected-layout persp-nil-name
        "Previously selected layout.")

      (defvar spacemacs--custom-layout-alist nil
        "List of custom layouts with their bound keys.
 Do not modify directly, use provided `spacemacs|define-custom-layout'")

      (defvar spacemacs--layouts-autosave-timer nil
        "Timer for layouts auto-save.")

      (when dotspacemacs-display-default-layout
        ;; load `persp-mode' if default perspective must be
        ;; displayed
        (unless (bound-and-true-p persp-mode)
          ;; we need this check because calling persp-mode again seems to
          ;; reset the list of perspectives...
          (persp-mode)))

      (defun spacemacs/jump-to-last-layout ()
        "Open the previously selected layout."
        (interactive)
        (persp-switch spacemacs--last-selected-layout))

      ;; Perspectives micro-state -------------------------------------------

      (defun spacemacs//layouts-ms-toggle-doc ()
        "Toggle the full documenation for the layouts micro-state."
        (interactive)
        (setq spacemacs--layouts-ms-doc-toggle
              (logxor spacemacs--layouts-ms-doc-toggle 1)))

      (defun spacemacs//layout-format-name (name pos)
        "Format the layout name given by NAME for display in  mode-line."
        (let* ((string-name (format "%s" name))
               (current (equal name (spacemacs//current-layout-name)))
               (caption (concat (number-to-string (if (eq 9 pos) 0 (1+ pos)))
                                ":" string-name)))
          (if current
              (concat (when current "[") caption (when current "]"))
            caption)))

      (defvar spacemacs--layouts-ms-documentation
        "
  [?]                  toggle this help
  [0,9]                go to nth layout
  [tab]                last layout
  [a]                  add a buffer from another layout
  [A]                  add all buffers from another layout
  [b]                  select buffer in current layout
  [c]                  close layout (buffers are not closed)
  [C]                  close other layout(s) (buffers are not closed)
  [l]                  jump to a layout
  [L]                  load saved layouts
  [n] [C-n] or [C-l]   next layout
  [N] [C-p] or [C-h]   previous layout
  [o]                  custom layouts
  [p]                  open project and create associated layout
  [r]                  remove current buffer from layout
  [R]                  rename or create layout
  [s]                  save layouts
  [t]                  show a buffer without adding it to current layout
  [x]                  kill layout and its buffers
  [X]                  kill other layout(s) and their buffers")

      (defun spacemacs//layouts-ms-doc ()
        "Return the docstring for the layouts micro-state."
        (let* ((persp-list (or (persp-names-current-frame-fast-ordered)
                               (list persp-nil-name)))
               (formatted-persp-list
                (concat
                 (mapconcat
                  (lambda (persp)
                    (spacemacs//layout-format-name
                     persp (position persp persp-list))) persp-list " | "))))
          (concat formatted-persp-list
                  (when (equal 1 spacemacs--layouts-ms-doc-toggle)
                    spacemacs--layouts-ms-documentation))))

      (spacemacs|define-micro-state layouts
        :on-enter (unless (bound-and-true-p persp-mode) (persp-mode))
        :doc (spacemacs//layouts-ms-doc)
        :use-minibuffer t
        :evil-leader "l"
        :bindings
        ;; need to exit in case number doesn't exist
        ("?" spacemacs//layouts-ms-toggle-doc)
        ("1" spacemacs/persp-switch-to-1 :exit t)
        ("2" spacemacs/persp-switch-to-2 :exit t)
        ("3" spacemacs/persp-switch-to-3 :exit t)
        ("4" spacemacs/persp-switch-to-4 :exit t)
        ("5" spacemacs/persp-switch-to-5 :exit t)
        ("6" spacemacs/persp-switch-to-6 :exit t)
        ("7" spacemacs/persp-switch-to-7 :exit t)
        ("8" spacemacs/persp-switch-to-8 :exit t)
        ("9" spacemacs/persp-switch-to-9 :exit t)
        ("0" spacemacs/persp-switch-to-0 :exit t)
        ("<tab>" spacemacs/jump-to-last-layout)
        ("<return>" nil :exit t)
        ("C-h" persp-prev)
        ("C-l" persp-next)
        ("C-n" persp-next)
        ("C-p" persp-prev)
        ("a" persp-add-buffer :exit t)
        ("A" persp-import-buffers :exit t)
        ("b" spacemacs/persp-helm-mini :exit t)
        ("c" spacemacs/layouts-ms-close)
        ("C" spacemacs/layouts-ms-close-other :exit t)
        ("l" spacemacs/helm-perspectives :exit t)
        ("L" persp-load-state-from-file :exit t)
        ("n" persp-next)
        ("N" persp-prev)
        ("o" spacemacs/select-custom-layout :exit t)
        ("p" spacemacs/helm-persp-switch-project :exit t)
        ("r" persp-remove-buffer :exit t)
        ("R" spacemacs/layouts-ms-rename :exit t)
        ("s" persp-save-state-to-file :exit t)
        ("t" persp-temporarily-display-buffer :exit t)
        ("x" spacemacs/layouts-ms-kill)
        ("X" spacemacs/layouts-ms-kill-other :exit t))

      (defun spacemacs/layout-switch-by-pos (pos)
        "Switch to perspective of position POS."
        (let ((persp-to-switch
               (nth pos (persp-names-current-frame-fast-ordered))))
          (if persp-to-switch
              (persp-switch persp-to-switch)
            (when (y-or-n-p
                   (concat "Perspective in this position doesn't exist.\n"
                           "Do you want to create one? "))
              (persp-switch nil)))))

      ;; Define all `spacemacs/persp-switch-to-X' functions
      (dolist (i (number-sequence 9 0 -1))
        (eval `(defun ,(intern (format "spacemacs/persp-switch-to-%s" i)) nil
                   ,(format "Switch to layout %s." i)
                 (interactive)
                 (spacemacs/layout-switch-by-pos ,(if (eq 0 i) 9 (1- i)))
                 (spacemacs/layouts-micro-state))))

      (defun spacemacs/layouts-ms-rename ()
        "Rename a layout and get back to the perspectives micro-state."
        (interactive)
        (call-interactively 'persp-rename)
        (spacemacs/layouts-micro-state))

      (defun spacemacs/layouts-ms-close ()
        "Kill current perspective"
        (interactive)
        (persp-kill-without-buffers (spacemacs//current-layout-name)))

      (defun spacemacs/layouts-ms-close-other ()
        (interactive)
        (call-interactively 'spacemacs/helm-persp-close)
        (spacemacs/layouts-micro-state))

      (defun spacemacs/layouts-ms-kill ()
        "Kill current perspective"
        (interactive)
        (persp-kill (spacemacs//current-layout-name)))

      (defun spacemacs/layouts-ms-kill-other ()
        (interactive)
        (call-interactively 'spacemacs/helm-persp-kill)
        (spacemacs/layouts-micro-state))

      (defun spacemacs/layouts-ms-last ()
        "Switch to the last active perspective"
        (interactive)
        (persp-switch persp-last-persp-name))

      ;; Custom perspectives micro-state -------------------------------------

      (defun spacemacs//custom-layout-func-name (name)
        "Return the name of the custom-perspective function for NAME."
        (intern (concat "spacemacs/custom-perspective-" name)))

      (defmacro spacemacs|define-custom-layout (name &rest props)
        "Define a custom-perspective called NAME.

FUNC is a FUNCTION defined using NAME and the result of
`spacemacs//custom-layout-func-name', it takes care of
creating the perspective NAME and executing the expressions given
in the :body property to this macro.

NAME is a STRING.

Available PROPS:

`:binding STRING'
   Key to be bound to the function FUNC

`:body EXPRESSIONS'
  One or several EXPRESSIONS that are going to be evaluated after
  we change into the perspective NAME."
        (declare (indent 1))
        (let* ((func (spacemacs//custom-layout-func-name name))
               (binding (car (spacemacs/mplist-get props :binding)))
               (body (spacemacs/mplist-get props :body))
               (already-defined? (cdr (assoc binding
                                             spacemacs--custom-layout-alist))))
          `(progn
             (defun ,func ()
               ,(format "Open custom perspective %s" name)
               (interactive)
               (let ((initialize (not (gethash ,name *persp-hash*))))
                 (persp-switch ,name)
                 (when initialize
                   (delete-other-windows)
                   ,@body)))
             ;; Check for Clashes
             (if ,already-defined?
                 (unless (equal ,already-defined? ,name)
                   (warn "Replacing existing binding \"%s\" for %s with %s"
                         ,binding ,already-defined? ,name )
                   (push '(,binding . ,name) spacemacs--custom-layout-alist))
               (push '(,binding . ,name) spacemacs--custom-layout-alist)))))

      (spacemacs|define-custom-layout "@Spacemacs"
        :binding "e"
        :body
        (spacemacs/find-dotfile))

      (defun spacemacs/select-custom-layout ()
        "Update the custom-perspectives microstate and then activate it."
        (interactive)
        (spacemacs//update-custom-layouts)
        (spacemacs/custom-layouts-micro-state))

      (defun spacemacs//custom-layouts-ms-documentation ()
        "Return the docstring for the custom perspectives micro-state."
        (if spacemacs--custom-layout-alist
            (concat (mapconcat (lambda (custom-persp)
                                 (format "[%s] %s"
                                         (car custom-persp) (cdr custom-persp)))
                               spacemacs--custom-layout-alist " ")
                    "\n[q] quit to perspectives-micro-state")
          (warn (format "`spacemacs--custom-layout-alist' variable is empty" ))))

      (defun spacemacs//update-custom-layouts ()
        "Ensure the custom-perspectives micro-state is updated.
Takes each element in the list `spacemacs--custom-layout-alist'
format so they are supported by the
`spacemacs/custom-layouts-micro-state' macro."
        (let ((bindings '(("q" spacemacs/layouts-micro-state :exit t))))
          (dolist (custom-persp spacemacs--custom-layout-alist bindings)
            (let* ((binding (car custom-persp))
                   (name (cdr custom-persp))
                   (func-name (spacemacs//custom-layout-func-name name)))
              (push (list binding func-name) bindings)))
          (eval `(spacemacs|define-micro-state custom-layouts
                   :doc (spacemacs//custom-layouts-ms-documentation)
                   :use-minibuffer t
                   :bindings
                   ,@bindings))))
      )
    :config
    (progn
      (defadvice persp-activate (before spacemacs//save-toggle-layout activate)
        (setq spacemacs--last-selected-layout persp-last-persp-name))
      (add-hook 'persp-mode-hook 'spacemacs//layout-autosave)
      ;; By default, persp mode wont affect either helm or ido
      (remove-hook 'ido-make-buffer-list-hook 'persp-restrict-ido-buffers)
      )))

(defun perspectives/post-init-spaceline ()
  ;; redefine the persp-name format to allow optional
  ;; default perspective display
  ;; also display the perspective name only in active window
  (spaceline-define-segment persp-name
    "The current perspective name."
    (let ((name (safe-persp-name (get-frame-persp))))
      (if (file-directory-p name)
          (file-name-nondirectory (directory-file-name name))
        (propertize name 'face 'bold)))
    :when (and active (bound-and-true-p persp-mode)
               ;; There are multiple implementations of
               ;; persp-mode with different APIs
               (fboundp 'safe-persp-name) (fboundp 'get-frame-persp)
               (or (not (equal persp-nil-name
                               (safe-persp-name (get-frame-persp))))
                   dotspacemacs-display-default-layout))))
