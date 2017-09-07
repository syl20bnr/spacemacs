;;; packages.el --- Helm Layer packages File
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq helm-packages
      '(
        ace-jump-helm-line
        auto-highlight-symbol
        bookmark
        helm
        helm-ag
        helm-descbinds
        helm-flx
        helm-make
        helm-mode-manager
        helm-projectile
        helm-swoop
        helm-themes
        (helm-spacemacs-help :location local)
        (helm-spacemacs-faq :location local)
        imenu
        persp-mode
        popwin
        projectile
        ))

;; Initialization of packages

(defun helm/init-ace-jump-helm-line ()
  (use-package ace-jump-helm-line
    :defer t
    :init
    (with-eval-after-load 'helm
      (define-key helm-map (kbd "C-q") 'ace-jump-helm-line))))

(defun helm/pre-init-auto-highlight-symbol ()
  (spacemacs|use-package-add-hook auto-highlight-symbol
    :post-init
    ;; add some functions to ahs transient states
    (setq spacemacs--symbol-highlight-transient-state-doc
          (concat spacemacs--symbol-highlight-transient-state-doc
                  "  [_b_] search buffers [_/_] search proj [_f_] search files")
     spacemacs-symbol-highlight-transient-state-add-bindings
     '(("/" spacemacs/helm-project-smart-do-search-region-or-symbol :exit t)
       ("b" spacemacs/helm-buffers-smart-do-search-region-or-symbol :exit t)
       ("f" spacemacs/helm-files-smart-do-search-region-or-symbol :exit t)))))

(defun helm/post-init-bookmark ()
  (spacemacs/set-leader-keys "fb" 'helm-filtered-bookmarks))

(defun helm/init-helm ()
  (use-package helm
    :defer 1
    :commands (spacemacs/helm-find-files)
    :init
    (progn
      (add-hook 'helm-cleanup-hook #'spacemacs//helm-cleanup)
      ;; key bindings
      ;; Use helm to provide :ls, unless ibuffer is used
      (unless (configuration-layer/package-usedp 'ibuffer)
        (evil-ex-define-cmd "buffers" 'helm-buffers-list))
      ;; use helm by default for M-x, C-x C-f, and C-x b
      (unless (configuration-layer/package-usedp 'smex)
        (global-set-key (kbd "M-x") 'helm-M-x))
      (global-set-key (kbd "C-x C-f") 'spacemacs/helm-find-files)
      (global-set-key (kbd "C-x b") 'helm-buffers-list)
      ;; use helm everywhere
      (spacemacs/set-leader-keys
        "<f1>" 'helm-apropos
        "a'"   'helm-available-repls
        "bb"   'helm-mini
        "Cl"   'helm-colors
        "ff"   'spacemacs/helm-find-files
        "fF"   'helm-find-files
        "fL"   'helm-locate
        "fr"   'helm-recentf
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
        "sj"   'spacemacs/helm-jump-in-buffer)
      ;; search with grep
      (spacemacs/set-leader-keys
        "sgb"  'spacemacs/helm-buffers-do-grep
        "sgB"  'spacemacs/helm-buffers-do-grep-region-or-symbol
        "sgf"  'spacemacs/helm-files-do-grep
        "sgF"  'spacemacs/helm-files-do-grep-region-or-symbol
        "sgg"  'spacemacs/helm-file-do-grep
        "sgG"  'spacemacs/helm-file-do-grep-region-or-symbol)
      ;; various key bindings
      (spacemacs||set-helm-key "fel"  helm-locate-library)
      (spacemacs||set-helm-key "hdm" describe-mode)
      (spacemacs||set-helm-key "sww" helm-wikipedia-suggest)
      (spacemacs||set-helm-key "swg" helm-google-suggest)
      (with-eval-after-load 'helm-files
        (define-key helm-find-files-map
          (kbd "C-c C-e") 'spacemacs/helm-find-files-edit))
      ;; Add minibuffer history with `helm-minibuffer-history'
      (define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)
      ;; define the key binding at the very end in order to allow the user
      ;; to overwrite any key binding
      (add-hook 'emacs-startup-hook
                (lambda ()
                  (unless (configuration-layer/package-usedp 'smex)
                    (spacemacs/set-leader-keys
                      dotspacemacs-emacs-command-key 'helm-M-x)))))
    :config
    (progn
      (helm-mode)
      (advice-add 'helm-grep-save-results-1 :after 'spacemacs//gne-init-helm-grep)
      ;; helm-locate uses es (from everything on windows which doesnt like fuzzy)
      (helm-locate-set-command)
      (setq helm-locate-fuzzy-match (string-match "locate" helm-locate-command))
      ;; alter helm-bookmark key bindings to be simpler
      (defun simpler-helm-bookmark-keybindings ()
        (define-key helm-bookmark-map (kbd "C-d") 'helm-bookmark-run-delete)
        (define-key helm-bookmark-map (kbd "C-e") 'helm-bookmark-run-edit)
        (define-key helm-bookmark-map
          (kbd "C-f") 'helm-bookmark-toggle-filename)
        (define-key helm-bookmark-map
          (kbd "C-o") 'helm-bookmark-run-jump-other-window)
        (define-key helm-bookmark-map (kbd "C-/") 'helm-bookmark-help))
      (with-eval-after-load 'helm-bookmark
        (simpler-helm-bookmark-keybindings))
      (with-eval-after-load 'helm-mode ; required
        (spacemacs|hide-lighter helm-mode)))))

(defun helm/init-helm-ag ()
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
        (helm-do-ag-this-file))

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
      (advice-add 'helm-ag--save-results :after 'spacemacs//gne-init-helm-ag)
      (evil-define-key 'normal helm-ag-map "SPC" spacemacs-default-map)
      (evilified-state-evilify helm-ag-mode helm-ag-mode-map
        (kbd "RET") 'helm-ag-mode-jump-other-window
        (kbd "gr") 'helm-ag--update-save-results
        (kbd "q") 'quit-window))))

(defun helm/init-helm-descbinds ()
  (use-package helm-descbinds
    :defer t
    :init
    (progn
      (setq helm-descbinds-window-style 'split)
      (add-hook 'helm-mode-hook 'helm-descbinds-mode)
      (spacemacs/set-leader-keys "?" 'helm-descbinds))))

(defun helm/init-helm-flx ()
  (use-package helm-flx
    :defer t)
  (spacemacs|use-package-add-hook helm
    :pre-config
    (progn
      ;; Disable for helm-find-files until performance issues are sorted
      ;; https://github.com/PythonNut/helm-flx/issues/9
      (setq helm-flx-for-helm-find-files nil)
      (helm-flx-mode))))

(defun helm/init-helm-make ()
  (use-package helm-make
    :defer t
    :init
    (spacemacs/set-leader-keys
      "cc" 'helm-make-projectile
      "cm" 'helm-make)))

(defun helm/init-helm-mode-manager ()
  (use-package helm-mode-manager
    :defer t
    :init
    (spacemacs/set-leader-keys
      "hM"    'helm-switch-major-mode
      ;; "hm"    'helm-disable-minor-mode
      "h C-m" 'helm-enable-minor-mode)))

(defun helm/init-helm-projectile ()
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
      ;; needed for smart search if user's default tool is grep
      (defalias 'spacemacs/helm-project-do-grep 'helm-projectile-grep)
      (defalias
        'spacemacs/helm-project-do-grep-region-or-symbol
        'helm-projectile-grep)
      ;; overwrite projectile settings
      (spacemacs|use-package-add-hook projectile
        :post-init
        (progn
          (setq projectile-switch-project-action 'helm-projectile)
          (spacemacs/set-leader-keys
            "pb"  'helm-projectile-switch-to-buffer
            "pd"  'helm-projectile-find-dir
            "pf"  'helm-projectile-find-file
            "pF"  'helm-projectile-find-file-dwim
            "ph"  'helm-projectile
            "pp"  'helm-projectile-switch-project
            "pr"  'helm-projectile-recentf
            "sgp" 'helm-projectile-grep))))))

(defun helm/init-helm-spacemacs-help ()
  (use-package helm-spacemacs-help
    :commands (helm-spacemacs-help-dotspacemacs
               helm-spacemacs-help
               helm-spacemacs-help-faq
               helm-spacemacs-help-layers
               helm-spacemacs-help-packages
               helm-spacemacs-help-docs
               helm-spacemacs-help-toggles)
    :init (spacemacs/set-leader-keys
            "h ."   'helm-spacemacs-help-dotspacemacs
            "h SPC" 'helm-spacemacs-help
            "h f"   'helm-spacemacs-help-faq
            "h l"   'helm-spacemacs-help-layers
            "h p"   'helm-spacemacs-help-packages
            "h r"   'helm-spacemacs-help-docs
            "h t"   'helm-spacemacs-help-toggles)))

(defun helm/init-helm-spacemacs-faq ()
  (use-package helm-spacemacs-faq
    :commands helm-spacemacs-help-faq
    :init (spacemacs/set-leader-keys "h f" 'helm-spacemacs-help-faq)))

(defun helm/init-helm-swoop ()
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

(defun helm/init-helm-themes ()
  (use-package helm-themes
    :defer t
    :init
    (spacemacs/set-leader-keys
      "Ts" 'spacemacs/helm-themes)))

(defun helm/post-init-imenu ()
  (spacemacs/set-leader-keys "ji" 'spacemacs/helm-jump-in-buffer))

(defun helm/post-init-popwin ()
  ;; disable popwin-mode while Helm session is running
  (add-hook 'helm-after-initialize-hook #'spacemacs//helm-prepare-display)
  ;;  Restore popwin-mode after a Helm session finishes.
  (add-hook 'helm-cleanup-hook #'spacemacs//helm-restore-display))

(defun helm/post-init-projectile ()
  (setq projectile-completion-system 'helm))

(defun helm/post-init-persp-mode ()
   (setq spacemacs-layouts-transient-state-add-bindings
           '(("b" spacemacs/persp-helm-mini :exit t)
             ("l" spacemacs/helm-perspectives :exit t))))
