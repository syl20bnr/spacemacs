;;; packages.el --- Spacemacs Layouts Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(setq spacemacs-layouts-packages
      '(;; temporary switch on a fork to fix
        ;; https://github.com/syl20bnr/spacemacs/issues/4120
        (persp-mode :location (recipe :fetcher github
                                      :repo "syl20bnr/persp-mode.el"
                                      :branch "fix-emacsclient-crash"))
        spaceline
        eyebrowse
        helm
        swiper))

(defun spacemacs-layouts/init-persp-mode ()
  (use-package persp-mode
    :diminish persp-mode
    :init
    (progn
      (setq persp-auto-resume-time (if (or dotspacemacs-auto-resume-layouts
                                           spacemacs-force-resume-layouts)
                                       1 -1)
            persp-nil-name dotspacemacs-default-layout-name
            persp-reset-windows-on-nil-window-conf nil
            persp-set-last-persp-for-new-frames nil
            persp-save-dir spacemacs-layouts-directory)

      ;; always activate persp-mode
      (persp-mode)

      (defvar spacemacs--layouts-ms-doc-toggle 0
        "Display a short doc when nil, full doc otherwise.")

      (defvar spacemacs--last-selected-layout persp-nil-name
        "Previously selected layout.")

      (defvar spacemacs--custom-layout-alist nil
        "List of custom layouts with their bound keys.
 Do not modify directly, use provided `spacemacs|define-custom-layout'")

      (defvar spacemacs--layouts-autosave-timer nil
        "Timer for layouts auto-save.")

      (defun spacemacs/jump-to-last-layout ()
        "Open the previously selected layout, if it exists."
        (interactive)
        (unless (eq 'non-existent
                    (gethash spacemacs--last-selected-layout
                             *persp-hash* 'non-existent))
          (persp-switch spacemacs--last-selected-layout)))

      ;; Perspectives transient-state -------------------------------------------

      (defun spacemacs//layouts-ms-toggle-doc ()
        "Toggle the full documenation for the layouts transient-state."
        (interactive)
        (setq spacemacs--layouts-ms-doc-toggle
              (logxor spacemacs--layouts-ms-doc-toggle 1)))

      (defun spacemacs//layout-format-name (name pos)
        "Format the layout name given by NAME for display in mode-line."
        (let* ((layout-name (if (file-directory-p name)
                                (file-name-nondirectory (directory-file-name name))
                              name))
               (string-name (format "%s" layout-name))
               (current (equal name (spacemacs//current-layout-name)))
               (caption (concat (number-to-string (if (eq 9 pos) 0 (1+ pos)))
                                ":" string-name)))
          (if current
              (concat (when current "[") caption (when current "]"))
            caption)))

      (defun spacemacs//layouts-ms-doc ()
        "Return the docstring for the layouts transient-state."
        (let* ((persp-list (or (persp-names-current-frame-fast-ordered)
                               (list persp-nil-name)))
               (formatted-persp-list
                (concat
                 (mapconcat
                  (lambda (persp)
                    (spacemacs//layout-format-name
                     persp (position persp persp-list))) persp-list " | "))))
          (concat formatted-persp-list
                  (if (equal 1 spacemacs--layouts-ms-doc-toggle)
                      spacemacs--layouts-ms-documentation
                    (concat
                     "\n ["
                     (propertize "?" 'face 'hydra-face-red)
                     "]   toggle help")))))

      (spacemacs|define-transient-state layouts
        :title "Layouts Transient State"
        :additional-docs
        (spacemacs--layouts-ms-documentation .
                                             "\n
 Go to^^^^^^                         Add/Remove/Rename^^
--^-^--^^^^-----------------------  --^-^---------------------------
 [_b_]^^^^       buffer in layout    [_a_] add buffer
 [_h_]^^^^       default layout      [_A_] add all from layout
 [_o_]^^^^       custom layout       [_r_] remove current buffer
 [_l_]^^^^       layout w/helm/ivy   [_d_] close current layout
 [_L_]^^^^       layouts in file     [_D_] close other layout
 [_0_,_9_]^^     nth layout          [_x_] kill current w/buffers
 [_n_/_C-l_]^^   next layout         [_X_] kill other w/buffers
 [_N_/_p_/_C-h_] prev layout         [_R_] rename current layout
 [_<tab>_]^^^^   last layout
--^^^^^^^^----------------------------------------------------------
 [_s_/_S_] save all layouts/save by names
 [_t_]^^   show a buffer without adding it to current layout
 [_w_]^^   workspaces micro-state (requires eyebrowse layer)
 [_?_]^^   toggle help
")
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
        ("a" persp-add-buffer :exit t)
        ("A" persp-import-buffers :exit t)
        ("b" spacemacs/persp-helm-mini :exit t)
        ("d" spacemacs/layouts-ms-close)
        ("D" spacemacs/layouts-ms-close-other :exit t)
        ("h" spacemacs/layout-goto-default :exit t)
        ("l" spacemacs/helm-perspectives :exit t)
        ("L" persp-load-state-from-file :exit t)
        ("n" persp-next)
        ("N" persp-prev)
        ("o" spacemacs/select-custom-layout :exit t)
        ("p" persp-prev)
        ("r" persp-remove-buffer :exit t)
        ("R" spacemacs/layouts-ms-rename :exit t)
        ("s" persp-save-state-to-file :exit t)
        ("S" persp-save-to-file-by-names :exit t)
        ("t" persp-temporarily-display-buffer :exit t)
        ("w" spacemacs/layout-workspaces-transient-state :exit t)
        ("x" spacemacs/layouts-ms-kill)
        ("X" spacemacs/layouts-ms-kill-other :exit t))
      (spacemacs/set-leader-keys "l" 'spacemacs/layouts-transient-state/body)

      ;; This enables the dynamic behavior from micro-states using ?
      (add-hook 'spacemacs-post-user-config-hook
                (lambda ()
                  (setq spacemacs/layouts-transient-state/hint
                        `(concat
                          ,(when dotspacemacs-show-transient-state-title
                             (concat
                              (propertize "Layouts Transient State"
                                          'face 'spacemacs-transient-state-title-face)
                              "\n"))
                          (spacemacs//layouts-ms-doc))))
                t)

      (defun spacemacs/layout-switch-by-pos (pos)
        "Switch to perspective of position POS."
        (let ((persp-to-switch
               (nth pos (persp-names-current-frame-fast-ordered))))
          (if persp-to-switch
              (persp-switch persp-to-switch)
            (when (y-or-n-p
                   (concat "Perspective in this position doesn't exist.\n"
                           "Do you want to create one? "))
              (let ((persp-reset-windows-on-nil-window-conf t))
                (persp-switch nil)
                (spacemacs/home-delete-other-windows))))))

      ;; Define all `spacemacs/persp-switch-to-X' functions
      (dolist (i (number-sequence 9 0 -1))
        (eval `(defun ,(intern (format "spacemacs/persp-switch-to-%s" i)) nil
                 ,(format "Switch to layout %s." i)
                 (interactive)
                 (spacemacs/layout-switch-by-pos ,(if (eq 0 i) 9 (1- i)))
                 (spacemacs/layouts-transient-state/body))))

      (defun spacemacs/layout-goto-default ()
        "Go to `dotspacemacs-default-layout-name` layout"
        (interactive)
        (when dotspacemacs-default-layout-name
          (persp-switch dotspacemacs-default-layout-name)))

      (defun spacemacs/layouts-ms-rename ()
        "Rename a layout and get back to the perspectives transient-state."
        (interactive)
        (call-interactively 'persp-rename)
        (spacemacs/layouts-transient-state/body))

      (defun spacemacs/layouts-ms-close ()
        "Kill current perspective"
        (interactive)
        (persp-kill-without-buffers (spacemacs//current-layout-name)))

      (defun spacemacs/layouts-ms-close-other ()
        (interactive)
        (call-interactively 'spacemacs/helm-persp-close)
        (spacemacs/layouts-transient-state/body))

      (defun spacemacs/layouts-ms-kill ()
        "Kill current perspective"
        (interactive)
        (persp-kill (spacemacs//current-layout-name)))

      (defun spacemacs/layouts-ms-kill-other ()
        (interactive)
        (call-interactively 'spacemacs/helm-persp-kill)
        (spacemacs/layouts-transient-state/body))

      (defun spacemacs/layouts-ms-last ()
        "Switch to the last active perspective"
        (interactive)
        (persp-switch persp-last-persp-name))

      ;; Custom perspectives transient-state -------------------------------------

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
                   (spacemacs-buffer/warning "Replacing existing binding \"%s\" for %s with %s"
                         ,binding ,already-defined? ,name )
                   (push '(,binding . ,name) spacemacs--custom-layout-alist))
               (push '(,binding . ,name) spacemacs--custom-layout-alist)))))

      (spacemacs|define-custom-layout "@Spacemacs"
        :binding "e"
        :body
        (spacemacs/find-dotfile))

      (defun spacemacs/select-custom-layout ()
        "Update the custom-perspectives transient-state and then activate it."
        (interactive)
        (spacemacs//update-custom-layouts)
        (spacemacs/custom-layouts-transient-state/body))

      (defun spacemacs//custom-layouts-ms-documentation ()
        "Return the docstring for the custom perspectives transient-state."
        (if spacemacs--custom-layout-alist
            (mapconcat (lambda (custom-persp)
                         (format "[%s] %s"
                                 (car custom-persp) (cdr custom-persp)))
                       spacemacs--custom-layout-alist " ")
          (spacemacs-buffer/warning (format "`spacemacs--custom-layout-alist' variable is empty" ))))

      (defun spacemacs//update-custom-layouts ()
        "Ensure the custom-perspectives transient-state is updated.
Takes each element in the list `spacemacs--custom-layout-alist'
format so they are supported by the
`spacemacs/custom-layouts-transient-state' macro."
        (let (bindings)
          (dolist (custom-persp spacemacs--custom-layout-alist bindings)
            (let* ((binding (car custom-persp))
                   (name (cdr custom-persp))
                   (func-name (spacemacs//custom-layout-func-name name)))
              (push (list binding func-name) bindings)))
          (eval `(spacemacs|define-transient-state custom-layouts
                   :doc (concat (spacemacs//custom-layouts-ms-documentation))
                   :bindings
                   ,@bindings))))
      )
    :config
    (progn
      (defadvice persp-activate (before spacemacs//save-toggle-layout activate)
        (setq spacemacs--last-selected-layout persp-last-persp-name))
      (add-hook 'persp-mode-hook 'spacemacs//layout-autosave)

      (defun spacemacs/alternate-buffer-in-persp ()
        "Switch back and forth between current and last buffer in the
current perspective."
        (interactive)
        (with-persp-buffer-list ()
          (switch-to-buffer (other-buffer (current-buffer) t))))

      (defun spacemacs-layouts/non-restricted-buffer-list ()
        (interactive)
        (remove-hook 'ido-make-buffer-list-hook  #'persp-restrict-ido-buffers)
        (helm-mini)
        (add-hook 'ido-make-buffer-list-hook  #'persp-restrict-ido-buffers))

      (spacemacs/declare-prefix "b" "persp-buffers")
      (spacemacs/declare-prefix "B" "global-buffers")

      ;; Override SPC TAB to only change buffers in perspective
      (spacemacs/set-leader-keys
        "TAB"  'spacemacs/alternate-buffer-in-persp
        "ba"   'persp-add-buffer
        "br"   'persp-remove-buffer
        "Bb"   'spacemacs-layouts/non-restricted-buffer-list
        ))))

(defun spacemacs-layouts/post-init-spaceline ()
  (setq spaceline-display-default-perspective
        dotspacemacs-display-default-layout))

(defun spacemacs-layouts/post-init-eyebrowse ()
  (add-hook 'persp-before-switch-functions #'spacemacs/update-eyebrowse-for-perspective)
  (add-hook 'eyebrowse-post-window-switch-hook #'spacemacs/save-eyebrowse-for-perspective)
  (add-hook 'persp-activated-hook #'spacemacs/load-eyebrowse-for-perspective))

(defun spacemacs-layouts/post-init-helm ()
  (spacemacs/set-leader-keys
    "pl" 'spacemacs/helm-persp-switch-project))

(defun spacemacs-layouts/post-init-swiper ()
  (defun spacemacs/ivy-persp-switch-project (arg)
    (interactive "P")
    (ivy-read "Switch to Project Perspective: "
              (if (projectile-project-p)
                  (cons (abbreviate-file-name (projectile-project-root))
                        (projectile-relevant-known-projects))
                projectile-known-projects)
              :action (lambda (project)
                        (let ((persp-reset-windows-on-nil-window-conf t))
                          (persp-switch project)
                          (let ((projectile-completion-system 'ivy))
                            (projectile-switch-project-by-name project))))))

  (spacemacs/set-leader-keys
    "pl" 'spacemacs/ivy-persp-switch-project))
