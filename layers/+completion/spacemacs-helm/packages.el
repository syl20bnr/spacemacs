;;; packages.el --- Spacemacs Core Layer packages File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq spacemacs-helm-packages
      '(ace-jump-helm-line
        helm
        helm-ag
        helm-descbinds
        helm-flx
        helm-make
        helm-mode-manager
        helm-projectile
        helm-swoop
        helm-themes
        (helm-spacemacs-help :location local)))

;; Initialization of packages

(defun spacemacs-helm/init-ace-jump-helm-line ()
  (use-package ace-jump-helm-line
    :defer t
    :init
    (with-eval-after-load 'helm
      (define-key helm-map (kbd "C-q") 'ace-jump-helm-line))))

(defun spacemacs-helm/init-helm ()
  (use-package helm
    :defer 1
    :commands (spacemacs/helm-find-files)
    :init
    (progn
      (setq helm-prevent-escaping-from-minibuffer t
            helm-bookmark-show-location t
            helm-display-header-line nil
            helm-split-window-in-side-p t
            helm-always-two-windows t
            helm-echo-input-in-header-line t
            helm-imenu-execute-action-at-once-if-one nil
            helm-org-format-outline-path t)

      ;; hide minibuffer in Helm session, since we use the header line already
      (defun helm-hide-minibuffer-maybe ()
        (when (with-helm-buffer helm-echo-input-in-header-line)
          (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
            (overlay-put ov 'window (selected-window))
            (overlay-put ov 'face
                         (let ((bg-color (face-background 'default nil)))
                           `(:background ,bg-color :foreground ,bg-color)))
            (setq-local cursor-type nil))))
      (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

      ;; fuzzy matching setting
      (with-eval-after-load 'helm-source
        (defun spacemacs//helm-make-source (f &rest args)
          (let ((source-type (cadr args))
                (props (cddr args)))
            (unless (eq source-type 'helm-source-async)
              (plist-put props :fuzzy-match t)))
          (apply f args))
        (advice-add 'helm-make-source :around #'spacemacs//helm-make-source))

      ;; Use helm to provide :ls, unless ibuffer is used
      (unless (configuration-layer/package-usedp 'ibuffer)
        (evil-ex-define-cmd "buffers" 'helm-buffers-list))

      (defun spacemacs//helm-do-grep-region-or-symbol
          (&optional targs use-region-or-symbol-p)
        "Version of `helm-do-grep' with a default input."
        (interactive)
        (require 'helm)
        (cl-letf*
            (((symbol-function 'this-fn) (symbol-function 'helm-do-grep-1))
             ((symbol-function 'helm-do-grep-1)
              (lambda (targets &optional recurse zgrep exts
                               default-input region-or-symbol-p)
                (let* ((new-input (when region-or-symbol-p
                                    (if (region-active-p)
                                        (buffer-substring-no-properties
                                         (region-beginning) (region-end))
                                      (thing-at-point 'symbol t))))
                       (quoted-input (when new-input
                                       (rxt-quote-pcre new-input))))
                  (this-fn targets recurse zgrep exts
                           default-input quoted-input))))
             (preselection (or (dired-get-filename nil t)
                               (buffer-file-name (current-buffer))))
             (targets   (if targs
                            targs
                          (helm-read-file-name
                           "Search in file(s): "
                           :marked-candidates t
                           :preselect (if helm-ff-transformer-show-only-basename
                                          (helm-basename preselection)
                                        preselection)))))
          (helm-do-grep-1 targets nil nil nil nil use-region-or-symbol-p)))

      (defun spacemacs/helm-file-do-grep ()
        "Search in current file with `grep' using a default input."
        (interactive)
        (spacemacs//helm-do-grep-region-or-symbol
         (list (buffer-file-name (current-buffer))) nil))

      (defun spacemacs/helm-file-do-grep-region-or-symbol ()
        "Search in current file with `grep' using a default input."
        (interactive)
        (spacemacs//helm-do-grep-region-or-symbol
         (list (buffer-file-name (current-buffer))) t))

      (defun spacemacs/helm-files-do-grep ()
        "Search in files with `grep'."
        (interactive)
        (spacemacs//helm-do-grep-region-or-symbol nil nil))

      (defun spacemacs/helm-files-do-grep-region-or-symbol ()
        "Search in files with `grep' using a default input."
        (interactive)
        (spacemacs//helm-do-grep-region-or-symbol nil t))

      (defun spacemacs/helm-buffers-do-grep ()
        "Search in opened buffers with `grep'."
        (interactive)
        (let ((buffers (cl-loop for buffer in (buffer-list)
                                when (buffer-file-name buffer)
                                collect (buffer-file-name buffer))))
          (spacemacs//helm-do-grep-region-or-symbol buffers nil)))

      (defun spacemacs/helm-buffers-do-grep-region-or-symbol ()
        "Search in opened buffers with `grep' with a default input."
        (interactive)
        (let ((buffers (cl-loop for buffer in (buffer-list)
                                when (buffer-file-name buffer)
                                collect (buffer-file-name buffer))))
          (spacemacs//helm-do-grep-region-or-symbol buffers t)))

      (defun spacemacs/resume-last-search-buffer ()
        "open last helm-ag or hgrep buffer."
        (interactive)
        (cond ((get-buffer "*helm ag results*")
               (switch-to-buffer-other-window "*helm ag results*"))
              ((get-buffer "*helm-ag*")
               (helm-resume "*helm-ag*"))
              ((get-buffer "*hgrep*")
               (switch-to-buffer-other-window "*hgrep*"))
              (t
               (message "No previous search buffer found"))))

      (defun spacemacs/helm-faces ()
        "Describe face."
        (interactive)
        (require 'helm-elisp)
        (let ((default (or (face-at-point) (thing-at-point 'symbol))))
          (helm :sources (helm-def-source--emacs-faces
                          (format "%s" (or default "default")))
                :buffer "*helm faces*")))

      (defun helm-available-repls ()
        "Show all the repls available."
        (interactive)
        (let ((helm-available-repls
               `((name . "HELM available REPLs")
                 (candidates . ,(mapcar #'car spacemacs-repl-list))
                 (action . (lambda (candidate)
                             (let ((repl (cdr (assoc candidate spacemacs-repl-list))))
                               (require (car repl))
                               (call-interactively (cdr repl))))))))
          (helm :sources '(helm-available-repls)
                :buffer "*helm repls*")))

      ;; use helm by default for M-x
      (unless (configuration-layer/package-usedp 'smex)
        (global-set-key (kbd "M-x") 'helm-M-x))

      (spacemacs/set-leader-keys
        "<f1>" 'helm-apropos
        "a'"   'helm-available-repls
        "bb"   'helm-mini
        "Cl"   'helm-colors
        "ff"   'spacemacs/helm-find-files
        "fF"   'helm-find-files
        "fL"   'helm-locate
        "fr"   'helm-recentf
        "fb"   'helm-filtered-bookmarks
        "hdd"  'helm-apropos
        "hdF"  'spacemacs/helm-faces
        "hi"   'helm-info-at-point
        "hm"   'helm-man-woman
        "iu"   'helm-ucs
        "jI"   'helm-imenu-in-all-buffers
        "rm"   'helm-all-mark-rings
        "rl"   'helm-resume
        "rr"   'helm-register
        "rs"   'spacemacs/resume-last-search-buffer
        "ry"   'helm-show-kill-ring
        "sl"   'spacemacs/resume-last-search-buffer
        "sj"   'spacemacs/jump-in-buffer)

      ;; search with grep
      (spacemacs/set-leader-keys
        "sgb"  'spacemacs/helm-buffers-do-grep
        "sgB"  'spacemacs/helm-buffers-do-grep-region-or-symbol
        "sgf"  'spacemacs/helm-files-do-grep
        "sgF"  'spacemacs/helm-files-do-grep-region-or-symbol
        "sgg"  'spacemacs/helm-file-do-grep
        "sgG"  'spacemacs/helm-file-do-grep-region-or-symbol)

      ;; define the key binding at the very end in order to allow the user
      ;; to overwrite any key binding
      (add-hook 'emacs-startup-hook
                (lambda ()
                  (unless (configuration-layer/package-usedp 'smex)
                    (spacemacs/set-leader-keys
                      dotspacemacs-emacs-command-key 'helm-M-x))))

      (defun spacemacs//hide-cursor-in-helm-buffer ()
        "Hide the cursor in helm buffers."
        (with-helm-buffer
          (setq cursor-in-non-selected-windows nil)))
      (add-hook 'helm-after-initialize-hook
                'spacemacs//hide-cursor-in-helm-buffer)

      (defvar spacemacs-helm-display-help-buffer-regexp '("*.*Helm.*Help.**"))
      (defvar spacemacs-helm-display-buffer-regexp
        `("*.*helm.**"
          (display-buffer-in-side-window)
          (inhibit-same-window . t)
          (side . ,dotspacemacs-helm-position)
          (window-width . 0.6)
          (window-height . 0.4)))
      (defvar spacemacs-display-buffer-alist nil)
      (defun spacemacs//helm-prepare-display ()
        "Prepare necessary settings to make Helm display properly."
        ;; avoid Helm buffer being diplaye twice when user
        ;; sets this variable to some function that pop buffer to
        ;; a window. See https://github.com/syl20bnr/spacemacs/issues/1396
        (let ((display-buffer-base-action '(nil)))
          (setq spacemacs-display-buffer-alist display-buffer-alist)
          ;; the only buffer to display is Helm, nothing else we must set this
          ;; otherwise Helm cannot reuse its own windows for copyinng/deleting
          ;; etc... because of existing popwin buffers in the alist
          (setq display-buffer-alist nil)
          (popwin-mode -1)
          ;; workaround for a helm-evil incompatibility
          ;; see https://github.com/syl20bnr/spacemacs/issues/3700
          (when helm-prevent-escaping-from-minibuffer
            (define-key evil-motion-state-map [down-mouse-1] nil))))

      (defun spacemacs//display-helm-window (buffer)
        (let ((display-buffer-alist
               (list spacemacs-helm-display-help-buffer-regexp
                     ;; this or any specialized case of Helm buffer must be
                     ;; added AFTER `spacemacs-helm-display-buffer-regexp'.
                     ;; Otherwise, `spacemacs-helm-display-buffer-regexp' will
                     ;; be used before
                     ;; `spacemacs-helm-display-help-buffer-regexp' and display
                     ;; configuration for normal Helm buffer is applied for helm
                     ;; help buffer, making the help buffer unable to be
                     ;; displayed.
                     spacemacs-helm-display-buffer-regexp)))
          (helm-default-display-buffer buffer)))
      (setq helm-display-function 'spacemacs//display-helm-window)

      (defun spacemacs//restore-previous-display-config ()
        ;; workaround for a helm-evil incompatibility
        ;; see https://github.com/syl20bnr/spacemacs/issues/3700
        (when helm-prevent-escaping-from-minibuffer
          (define-key evil-motion-state-map
            [down-mouse-1] 'evil-mouse-drag-region))
        (popwin-mode 1)
        ;; we must enable popwin-mode first then restore `display-buffer-alist'
        ;; Otherwise, popwin keeps adding up its own buffers to
        ;; `display-buffer-alist' and could slow down Emacs as the list grows
        (setq display-buffer-alist spacemacs-display-buffer-alist))

      (add-hook 'helm-after-initialize-hook 'spacemacs//helm-prepare-display)
      ;;  Restore popwin-mode after a Helm session finishes.
      (add-hook 'helm-cleanup-hook 'spacemacs//restore-previous-display-config)

      ;; Add minibuffer history with `helm-minibuffer-history'
      (define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

      (defun spacemacs//helm-cleanup ()
        "Cleanup some helm related states when quitting."
        ;; deactivate any running transient map (transient-state)
        (setq overriding-terminal-local-map nil))
      (add-hook 'helm-cleanup-hook 'spacemacs//helm-cleanup)

      (defface spacemacs-helm-navigation-ms-face
        `((t :background ,(face-attribute 'error :foreground)
             :foreground "black"))
        "Face for helm heder when helm transient-state is activated."
        :group 'spacemacs))

    :config
    (progn
      (helm-mode +1)

      (when (and dotspacemacs-helm-resize
                 (or (eq dotspacemacs-helm-position 'bottom)
                     (eq dotspacemacs-helm-position 'top)))
        (setq helm-autoresize-min-height 10)
        (helm-autoresize-mode 1))

      ;; from https://www.reddit.com/r/emacs/comments/2z7nbv/lean_helm_window/
      (defvar helm-source-header-default-background
        (face-attribute 'helm-source-header :background))
      (defvar helm-source-header-default-foreground
        (face-attribute 'helm-source-header :foreground))
      (defvar helm-source-header-default-box
        (face-attribute 'helm-source-header :box))
      (defvar helm-source-header-default-height
        (face-attribute 'helm-source-header :height) )

      (defadvice spacemacs/post-theme-init (after spacemacs/helm-header-line-adv activate)
        "Update defaults for `helm' header line whenever a new theme is loaded"
        (setq helm-source-header-default-foreground (face-attribute 'helm-source-header :foreground)
              helm-source-header-default-background (face-attribute 'helm-source-header :background)
              helm-source-header-default-box (face-attribute 'helm-source-header :box)
              helm-source-header-default-height (face-attribute 'helm-source-header :height)))

      (defun helm-toggle-header-line ()
        "Hide the `helm' header is there is only one source."
        (when dotspacemacs-helm-no-header
          (if (> (length helm-sources) 1)
              (set-face-attribute
               'helm-source-header
               nil
               :foreground helm-source-header-default-foreground
               :background helm-source-header-default-background
               :box helm-source-header-default-box
               :height helm-source-header-default-height)
            (set-face-attribute
             'helm-source-header
             nil
             :foreground (face-attribute 'default :background)
             :background (face-attribute 'default :background)
             :box nil
             :height 0.1))))
      (add-hook 'helm-before-initialize-hook 'helm-toggle-header-line)

      (defun spacemacs/helm-find-files (arg)
        "Custom spacemacs implementation for calling helm-find-files-1.

Removes the automatic guessing of the initial value based on thing at point. "
        (interactive "P")
        (let* ((hist          (and arg helm-ff-history (helm-find-files-history)))
               (default-input hist )
               (input         (cond ((and (eq major-mode 'dired-mode) default-input)
                                     (file-name-directory default-input))
                                    ((and (not (string= default-input ""))
                                          default-input))
                                    (t (expand-file-name (helm-current-directory))))))
          (set-text-properties 0 (length input) nil input)
          (helm-find-files-1 input )))

      ;; helm-locate uses es (from everything on windows,
      ;; which doesnt like fuzzy)
      (helm-locate-set-command)
      (setq helm-locate-fuzzy-match (string-match "locate" helm-locate-command))

      (defun spacemacs//set-dotted-directory ()
        "Set the face of diretories for `.' and `..'"
        (set-face-attribute 'helm-ff-dotted-directory
                            nil
                            :foreground nil
                            :background nil
                            :inherit 'helm-ff-directory))
      (add-hook 'helm-find-files-before-init-hook
                'spacemacs//set-dotted-directory)

      ;; alter helm-bookmark key bindings to be simpler
      (defun simpler-helm-bookmark-keybindings ()
        (define-key helm-bookmark-map (kbd "C-d") 'helm-bookmark-run-delete)
        (define-key helm-bookmark-map (kbd "C-e") 'helm-bookmark-run-edit)
        (define-key helm-bookmark-map
          (kbd "C-f") 'helm-bookmark-toggle-filename)
        (define-key helm-bookmark-map
          (kbd "C-o") 'helm-bookmark-run-jump-other-window)
        (define-key helm-bookmark-map (kbd "C-/") 'helm-bookmark-help))
      (add-hook 'helm-mode-hook 'simpler-helm-bookmark-keybindings)

      ;; helm navigation on hjkl
      (defun spacemacs//helm-hjkl-navigation (&optional arg)
        "Set navigation in helm on `jklh'.
ARG non nil means Vim like movements."
        (cond
         (arg
          ;; better navigation on homerow
          ;; rebind `describe-key' for convenience
          (define-key helm-map (kbd "C-j") 'helm-next-line)
          (define-key helm-map (kbd "C-k") 'helm-previous-line)
          (define-key helm-map (kbd "C-h") 'helm-next-source)
          (define-key helm-map (kbd "C-S-h") 'describe-key)
          (define-key helm-map (kbd "C-l") (kbd "RET"))
          (with-eval-after-load 'helm-files
            (dolist (keymap (list helm-find-files-map helm-read-file-map))
              (define-key keymap (kbd "C-l") 'helm-execute-persistent-action)
              (define-key keymap (kbd "C-h") 'helm-find-files-up-one-level)
              (define-key keymap (kbd "C-S-h") 'describe-key))))
         (t
          (define-key helm-map (kbd "C-j") 'helm-execute-persistent-action)
          (define-key helm-map (kbd "C-k") 'helm-delete-minibuffer-contents)
          (define-key helm-map (kbd "C-h") nil)
          (define-key helm-map
            (kbd "C-l") 'helm-recenter-top-bottom-other-window))))
      (add-hook 'spacemacs--hjkl-completion-navigation-functions
                'spacemacs//helm-hjkl-navigation)
      (run-hook-with-args 'spacemacs--hjkl-completion-navigation-functions
                          (member dotspacemacs-editing-style '(vim hybrid)))

      (defun spacemacs/helm-edit ()
        "Switch in edit mode depending on the current helm buffer."
        (interactive)
        (cond
         ((string-equal "*helm-ag*" helm-buffer)
          (helm-ag-edit))))

      (defun spacemacs//helm-navigation-ms-on-enter ()
        "Initialization of helm transient-state."
        ;; faces
        (spacemacs//helm-navigation-ms-set-face)
        (setq spacemacs--helm-navigation-ms-face-cookie-minibuffer
              (face-remap-add-relative
               'minibuffer-prompt
               'spacemacs-helm-navigation-ms-face)))

      (defun spacemacs//helm-navigation-ms-set-face ()
        "Set the face for helm header in helm navigation transient-state"
        (with-helm-window
          (setq spacemacs--helm-navigation-ms-face-cookie-header
                (face-remap-add-relative
                 'helm-header
                 'spacemacs-helm-navigation-ms-face))))

      (defun spacemacs//helm-navigation-ms-on-exit ()
        "Action to perform when exiting helm transient-state."
        (with-helm-window
          (face-remap-remove-relative
           spacemacs--helm-navigation-ms-face-cookie-header))
        (face-remap-remove-relative
         spacemacs--helm-navigation-ms-face-cookie-minibuffer))

      ;; Define functions to pick actions
      (dotimes (n 10)
        (let ((func (intern (format "spacemacs/helm-action-%d" n)))
              (doc (format "Select helm action #%d" n)))
          (eval `(defun ,func ()
                   ,doc
                   (intern)
                   (helm-select-nth-action ,(1- n))))))

      (defun spacemacs/helm-transient-state-select-action ()
        (interactive)
        (call-interactively 'helm-select-action)
        (spacemacs//helm-navigation-ms-set-face))

      (spacemacs|define-transient-state helm-navigation
        :title "Helm Transient State"
        :doc "
[_j_/_k_]  next/prev candidate  [_v_]^^     persistent action     [_e_]^^    edit occurrences
[_h_/_l_]  prev/next source     [_1_.._0_]  action 1..10          [_t_/_T_]  toggle visible/all mark
[_q_]^^    quit                 [_a_]^^     action selection pg"
        :foreign-keys run
        :on-enter (spacemacs//helm-navigation-ms-on-enter)
        :on-exit  (spacemacs//helm-navigation-ms-on-exit)
        :bindings
        ("1" spacemacs/helm-action-1 :exit t)
        ("2" spacemacs/helm-action-2 :exit t)
        ("3" spacemacs/helm-action-3 :exit t)
        ("4" spacemacs/helm-action-4 :exit t)
        ("5" spacemacs/helm-action-5 :exit t)
        ("6" spacemacs/helm-action-6 :exit t)
        ("7" spacemacs/helm-action-7 :exit t)
        ("8" spacemacs/helm-action-8 :exit t)
        ("9" spacemacs/helm-action-9 :exit t)
        ("0" spacemacs/helm-action-10 :exit t)
        ("<tab>" helm-select-action :exit t)
        ("TAB" helm-select-action :exit t)
        ("<RET>" helm-maybe-exit-minibuffer :exit t)
        ;; ("?" nil :doc (spacemacs//helm-navigation-ms-full-doc))
        ("a" spacemacs/helm-transient-state-select-action)
        ("e" spacemacs/helm-edit)
        ("g" helm-beginning-of-buffer)
        ("G" helm-end-of-buffer)
        ("h" helm-previous-source)
        ("j" helm-next-line)
        ("k" helm-previous-line)
        ("l" helm-next-source)
        ("q" nil :exit t)
        ("t" helm-toggle-visible-mark)
        ("T" helm-toggle-all-marks)
        ("v" helm-execute-persistent-action))
      (define-key helm-map (kbd "M-SPC")
        'spacemacs/helm-navigation-transient-state/body)
      (define-key helm-map (kbd "s-M-SPC")
        'spacemacs/helm-navigation-transient-state/body)

      ;; Swap default TAB and C-z commands.
      ;; For GUI.
      (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
      (define-key helm-find-files-map
        (kbd "S-<tab>") 'helm-find-files-up-one-level)
      (define-key helm-find-files-map
        (kbd "<backtab>") 'helm-find-files-up-one-level)
      ;; For terminal.
      (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
      (define-key helm-find-files-map
        (kbd "S-TAB") 'helm-find-files-up-one-level)
      (define-key helm-map (kbd "C-z") 'helm-select-action)

      (with-eval-after-load 'helm-mode ; required
        (spacemacs|hide-lighter helm-mode)))))

(defun spacemacs-helm/init-helm-ag ()
  (use-package helm-ag
    :defer t
    :init
    (progn
      (defun spacemacs//helm-do-ag-region-or-symbol (func &optional dir)
        "Search with `ag' with a default input."
        (require 'helm-ag)
        (cl-letf* (((symbol-value 'helm-ag-insert-at-point) 'symbol)
                   ;; make thing-at-point choosing the active region first
                   ((symbol-function 'this-fn) (symbol-function 'thing-at-point))
                   ((symbol-function 'thing-at-point)
                    (lambda (thing)
                      (let ((res (if (region-active-p)
                                     (buffer-substring-no-properties
                                      (region-beginning) (region-end))
                                   (this-fn thing))))
                        (when res (rxt-quote-pcre res))))))
          (funcall func dir)))

      (defun spacemacs//helm-do-search-find-tool (base tools default-inputp)
        "Create a cond form given a TOOLS string list and evaluate it."
        (eval
         `(cond
           ,@(mapcar
              (lambda (x)
                `((executable-find ,x)
                  ',(let ((func
                           (intern
                            (format (if default-inputp
                                        "spacemacs/%s-%s-region-or-symbol"
                                      "spacemacs/%s-%s")
                                    base x))))
                      (if (fboundp func)
                          func
                        (intern (format "%s-%s"  base x))))))
              tools)
           (t 'helm-do-grep))))

      ;; Search in current file ----------------------------------------------

      (defun spacemacs/helm-file-do-ag (&optional _)
        "Wrapper to execute `helm-ag-this-file.'"
        (interactive)
        (helm-ag-this-file))

      (defun spacemacs/helm-file-do-ag-region-or-symbol ()
        "Search in current file with `ag' using a default input."
        (interactive)
        (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-file-do-ag))

      (defun spacemacs/helm-file-smart-do-search (&optional default-inputp)
        "Search in current file using `dotspacemacs-search-tools'.
Search for a search tool in the order provided by `dotspacemacs-search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
are used as default input."
        (interactive)
        (call-interactively
         (spacemacs//helm-do-search-find-tool "helm-file-do"
                                              dotspacemacs-search-tools
                                              default-inputp)))

      (defun spacemacs/helm-file-smart-do-search-region-or-symbol ()
        "Search in current file using `dotspacemacs-search-tools' with
 default input.
Search for a search tool in the order provided by `dotspacemacs-search-tools'."
        (interactive)
        (spacemacs/helm-file-smart-do-search t))

      ;; Search in files -----------------------------------------------------

      (defun spacemacs/helm-files-do-ag (&optional dir)
        "Search in files with `ag' using a default input."
        (interactive)
        (helm-do-ag dir))

      (defun spacemacs/helm-files-do-ag-region-or-symbol ()
        "Search in files with `ag' using a default input."
        (interactive)
        (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-files-do-ag))

      (defun spacemacs/helm-files-do-ack (&optional dir)
        "Search in files with `ack'."
        (interactive)
        (let ((helm-ag-base-command "ack --nocolor --nogroup"))
          (helm-do-ag dir)))

      (defun spacemacs/helm-files-do-ack-region-or-symbol ()
        "Search in files with `ack' using a default input."
        (interactive)
        (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-files-do-ack))

      (defun spacemacs/helm-files-do-pt (&optional dir)
        "Search in files with `pt'."
        (interactive)
        (let ((helm-ag-base-command "pt -e --nocolor --nogroup"))
          (helm-do-ag dir)))

      (defun spacemacs/helm-files-do-pt-region-or-symbol ()
        "Search in files with `pt' using a default input."
        (interactive)
        (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-files-do-pt))

      (defun spacemacs/helm-files-smart-do-search (&optional default-inputp)
        "Search in opened buffers using `dotspacemacs-search-tools'.
Search for a search tool in the order provided by `dotspacemacs-search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
are used as default input."
        (interactive)
        (call-interactively
         (spacemacs//helm-do-search-find-tool "helm-files-do"
                                              dotspacemacs-search-tools
                                              default-inputp)))

      (defun spacemacs/helm-files-smart-do-search-region-or-symbol ()
        "Search in opened buffers using `dotspacemacs-search-tools'.
with default input.
Search for a search tool in the order provided by `dotspacemacs-search-tools'."
        (interactive)
        (spacemacs/helm-files-smart-do-search t))

      ;; Search in buffers ---------------------------------------------------

      (defun spacemacs/helm-buffers-do-ag (&optional _)
        "Wrapper to execute `helm-ag-buffers.'"
        (interactive)
        (helm-do-ag-buffers))

      (defun spacemacs/helm-buffers-do-ag-region-or-symbol ()
        "Search in opened buffers with `ag' with a default input."
        (interactive)
        (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-buffers-do-ag))

      (defun spacemacs/helm-buffers-do-ack (&optional _)
        "Search in opened buffers with `ack'."
        (interactive)
        (let ((helm-ag-base-command "ack --nocolor --nogroup"))
          (helm-do-ag-buffers)))

      (defun spacemacs/helm-buffers-do-ack-region-or-symbol ()
        "Search in opened buffers with `ack' with a default input."
        (interactive)
        (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-buffers-do-ack))

      (defun spacemacs/helm-buffers-do-pt (&optional _)
        "Search in opened buffers with `pt'."
        (interactive)
        (let ((helm-ag-base-command "pt -e --nocolor --nogroup"))
          (helm-do-ag-buffers)))

      (defun spacemacs/helm-buffers-do-pt-region-or-symbol ()
        "Search in opened buffers with `pt' using a default input."
        (interactive)
        (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-buffers-do-pt))

      (defun spacemacs/helm-buffers-smart-do-search (&optional default-inputp)
        "Search in opened buffers using `dotspacemacs-search-tools'.
Search for a search tool in the order provided by `dotspacemacs-search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
are used as default input."
        (interactive)
        (call-interactively
         (spacemacs//helm-do-search-find-tool "helm-buffers-do"
                                              dotspacemacs-search-tools
                                              default-inputp)))

      (defun spacemacs/helm-buffers-smart-do-search-region-or-symbol ()
        "Search in opened buffers using `dotspacemacs-search-tools' with
default input.
Search for a search tool in the order provided by `dotspacemacs-search-tools'."
        (interactive)
        (spacemacs/helm-buffers-smart-do-search t))

      ;; Search in project ---------------------------------------------------

      (defun spacemacs/helm-project-do-ag ()
        "Search in current project with `ag'."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (helm-do-ag dir)
            (message "error: Not in a project."))))

      (defun spacemacs/helm-project-do-ag-region-or-symbol ()
        "Search in current project with `ag' using a default input."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (spacemacs//helm-do-ag-region-or-symbol 'helm-do-ag dir)
            (message "error: Not in a project."))))

      (defun spacemacs/helm-project-do-ack ()
        "Search in current project with `ack'."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (spacemacs/helm-files-do-ack dir)
            (message "error: Not in a project."))))

      (defun spacemacs/helm-project-do-ack-region-or-symbol ()
        "Search in current project with `ack' using a default input."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (spacemacs//helm-do-ag-region-or-symbol
               'spacemacs/helm-files-do-ack dir)
            (message "error: Not in a project."))))

      (defun spacemacs/helm-project-do-pt ()
        "Search in current project with `pt'."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (spacemacs/helm-files-do-pt dir)
            (message "error: Not in a project."))))

      (defun spacemacs/helm-project-do-pt-region-or-symbol ()
        "Search in current project with `pt' using a default input."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (spacemacs//helm-do-ag-region-or-symbol
               'spacemacs/helm-files-do-pt dir)
            (message "error: Not in a project."))))

      (defun spacemacs/helm-project-smart-do-search (&optional default-inputp)
        "Search in current project using `dotspacemacs-search-tools'.
Search for a search tool in the order provided by `dotspacemacs-search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
are used as default input."
        (interactive)
        (let ((projectile-require-project-root nil))
          (call-interactively
           (spacemacs//helm-do-search-find-tool "helm-project-do"
                                                dotspacemacs-search-tools
                                                default-inputp))))

      (defun spacemacs/helm-project-smart-do-search-region-or-symbol ()
        "Search in current project using `dotspacemacs-search-tools' with
 default input.
Search for a search tool in the order provided by `dotspacemacs-search-tools'."
        (interactive)
        (spacemacs/helm-project-smart-do-search t))

      ;; This overrides the default C-s action in helm-projectile-switch-project
      ;; to search using ag/pt/whatever instead of just grep
      (with-eval-after-load 'helm-projectile
        (defun spacemacs/helm-project-smart-do-search-in-dir (dir)
          (interactive)
          (let ((default-directory dir))
            (spacemacs/helm-project-smart-do-search)))
        (define-key helm-projectile-projects-map
          (kbd "C-s")
          (lambda ()
            (interactive)
            (helm-exit-and-execute-action
             'spacemacs/helm-project-smart-do-search-in-dir))))

      ;; evilify the helm-grep buffer
      (evilified-state-evilify helm-grep-mode helm-grep-mode-map
        (kbd "RET") 'helm-grep-mode-jump-other-window
        (kbd "q") 'quit-window)

      (spacemacs/set-leader-keys
        ;; helm-ag marks
        "s`"  'helm-ag-pop-stack
        ;; opened buffers scope
        "sb"  'spacemacs/helm-buffers-smart-do-search
        "sB"  'spacemacs/helm-buffers-smart-do-search-region-or-symbol
        "sab" 'helm-do-ag-buffers
        "saB" 'spacemacs/helm-buffers-do-ag-region-or-symbol
        "skb" 'spacemacs/helm-buffers-do-ack
        "skB" 'spacemacs/helm-buffers-do-ack-region-or-symbol
        "stb" 'spacemacs/helm-buffers-do-pt
        "stB" 'spacemacs/helm-buffers-do-pt-region-or-symbol
        ;; current file scope
        "ss"  'spacemacs/helm-file-smart-do-search
        "sS"  'spacemacs/helm-file-smart-do-search-region-or-symbol
        "saa" 'helm-ag-this-file
        "saA" 'spacemacs/helm-file-do-ag-region-or-symbol
        ;; files scope
        "sf"  'spacemacs/helm-files-smart-do-search
        "sF"  'spacemacs/helm-files-smart-do-search-region-or-symbol
        "saf" 'helm-do-ag
        "saF" 'spacemacs/helm-files-do-ag-region-or-symbol
        "skf" 'spacemacs/helm-files-do-ack
        "skF" 'spacemacs/helm-files-do-ack-region-or-symbol
        "stf" 'spacemacs/helm-files-do-pt
        "stF" 'spacemacs/helm-files-do-pt-region-or-symbol
        ;; current project scope
        "/"   'spacemacs/helm-project-smart-do-search
        "*"   'spacemacs/helm-project-smart-do-search-region-or-symbol
        "sp"  'spacemacs/helm-project-smart-do-search
        "sP"  'spacemacs/helm-project-smart-do-search-region-or-symbol
        "sap" 'spacemacs/helm-project-do-ag
        "saP" 'spacemacs/helm-project-do-ag-region-or-symbol
        "skp" 'spacemacs/helm-project-do-ack
        "skP" 'spacemacs/helm-project-do-ack-region-or-symbol
        "stp" 'spacemacs/helm-project-do-pt
        "stP" 'spacemacs/helm-project-do-pt-region-or-symbol))
    :config
    (progn
      (evil-define-key 'normal helm-ag-map "SPC" spacemacs-default-map)
      (evilified-state-evilify helm-ag-mode helm-ag-mode-map
        (kbd "RET") 'helm-ag-mode-jump-other-window
        (kbd "q") 'quit-window))))

(defun spacemacs-helm/init-helm-descbinds ()
  (use-package helm-descbinds
    :defer t
    :init
    (progn
      (setq helm-descbinds-window-style 'split)
      (add-hook 'helm-mode-hook 'helm-descbinds-mode)
      (spacemacs/set-leader-keys "?" 'helm-descbinds))))

(defun spacemacs-helm/init-helm-flx ()
  (use-package helm-flx
    :defer t)
  (spacemacs|use-package-add-hook helm
    :pre-config
    (progn
      ;; Disable for helm-find-files until performance issues are sorted
      ;; https://github.com/PythonNut/helm-flx/issues/9
      (setq helm-flx-for-helm-find-files nil)
      (helm-flx-mode))))

(defun spacemacs-helm/init-helm-make ()
  (use-package helm-make
    :defer t
    :init
    (spacemacs/set-leader-keys
      "cc" 'helm-make-projectile
      "cm" 'helm-make)))

(defun spacemacs-helm/init-helm-mode-manager ()
  (use-package helm-mode-manager
    :defer t
    :init
    (spacemacs/set-leader-keys
      "hM"    'helm-switch-major-mode
      ;; "hm"    'helm-disable-minor-mode
      "h C-m" 'helm-enable-minor-mode)))

(defun spacemacs-helm/init-helm-projectile ()
  (use-package helm-projectile
    :commands (helm-projectile-switch-to-buffer
               helm-projectile-find-dir
               helm-projectile-dired-find-dir
               helm-projectile-recentf
               helm-projectile-find-file
               helm-projectile-grep
               helm-projectile
               helm-projectile-switch-project)
    :init
    (progn
      (setq projectile-switch-project-action 'helm-projectile)

      (defconst spacemacs-use-helm-projectile t
        "This variable is only defined if helm-projectile is used.")

      ;; needed for smart search if user's default tool is grep
      (defalias 'spacemacs/helm-project-do-grep 'helm-projectile-grep)
      (defalias
        'spacemacs/helm-project-do-grep-region-or-symbol 'helm-projectile-grep)

      (spacemacs/set-leader-keys
        "pb"  'helm-projectile-switch-to-buffer
        "pd"  'helm-projectile-find-dir
        "pf"  'helm-projectile-find-file
        "pF"  'helm-projectile-find-file-dwim
        "ph"  'helm-projectile
        "pp"  'helm-projectile-switch-project
        "pr"  'helm-projectile-recentf
        "pv"  'projectile-vc
        "sgp" 'helm-projectile-grep))))

(defun spacemacs-helm/init-helm-spacemacs-help ()
  (use-package helm-spacemacs-help
    :commands (helm-spacemacs-help-dotspacemacs
               helm-spacemacs-help
               helm-spacemacs-help-faq
               helm-spacemacs-help-layers
               helm-spacemacs-help-packages
               helm-spacemacs-help-docs
               helm-spacemacs-help-toggles)
    :init
    (progn
      (defun spacemacs-base/helm-spacemacs-deprecated (arg)
        "Provide helm-spacemacs with a binding's depreciation message."
        (interactive "P")
        (warn (concat "The 'SPC f e h' (or 'M-m f e h') binding is now "
                      "deprecated and will be remove in the next release. "
                      "Please use 'SPC h SPC' (or 'M-m h SPC') instead."))
        (helm-spacemacs arg))
      (spacemacs/set-leader-keys "feh" 'spacemacs-base/helm-spacemacs-deprecated)
      (spacemacs/set-leader-keys "fef" 'helm-spacemacs-help-faq)
      (spacemacs/set-leader-keys
        "h ."   'helm-spacemacs-help-dotspacemacs
        "h SPC" 'helm-spacemacs-help
        "h f"   'helm-spacemacs-help-faq
        "h l"   'helm-spacemacs-help-layers
        "h p"   'helm-spacemacs-help-packages
        "h r"   'helm-spacemacs-help-docs
        "h t"   'helm-spacemacs-help-toggles))))

(defun spacemacs-helm/init-helm-swoop ()
  (use-package helm-swoop
    :defer t
    :init
    (progn
      (setq helm-swoop-split-with-multiple-windows t
            helm-swoop-split-direction 'split-window-vertically
            helm-swoop-speed-or-color t
            helm-swoop-split-window-function 'helm-default-display-buffer
            helm-swoop-pre-input-function (lambda () ""))

      (defun spacemacs/helm-swoop-region-or-symbol ()
        "Call `helm-swoop' with default input."
        (interactive)
        (let ((helm-swoop-pre-input-function
               (lambda ()
                 (if (region-active-p)
                     (buffer-substring-no-properties (region-beginning)
                                                     (region-end))
                   (let ((thing (thing-at-point 'symbol t)))
                     (if thing thing ""))))))
          (call-interactively 'helm-swoop)))

      (spacemacs/set-leader-keys
        "ss"    'helm-swoop
        "sS"    'spacemacs/helm-swoop-region-or-symbol
        "s C-s" 'helm-multi-swoop-all)
      (defadvice helm-swoop (before add-evil-jump activate)
        (evil-set-jump)))))

(defun spacemacs-helm/init-helm-themes ()
  (use-package helm-themes
    :defer t
    :init
    (spacemacs/set-leader-keys
      "Th" 'helm-themes)))
