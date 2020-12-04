;;; packages.el --- Helm Layer packages File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
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
        (helm-ls-git :require git)
        helm-make
        helm-mode-manager
        helm-org
        helm-projectile
        helm-swoop
        helm-themes
        (helm-space-macs-help :location local)
        (helm-space-macs-faq :location local)
        helm-xref
        imenu
        persp-mode
        popwin
        projectile
        ))

;; Initialization of packages

(defun helm/init-ace-jump-helm-line ()
  (use-package ace-jump-helm-line
    :defer (space-macs/defer)
    :init
    (with-eval-after-load 'helm
      (define-key helm-map (kbd "C-q") 'ace-jump-helm-line))))

(defun helm/pre-init-auto-highlight-symbol ()
  (space-macs|use-package-add-hook auto-highlight-symbol
    :post-init
    ;; add some functions to ahs transient states
    (setq space-macs--symbol-highlight-transient-state-doc
          (concat
           space-macs--symbol-highlight-transient-state-doc
           "  Search: [_s_] swoop  [_b_] buffers  [_f_] files  [_/_] project"))
    (space-macs/transient-state-register-add-bindings 'symbol-highlight
      '(("s" space-macs/helm-swoop-region-or-symbol :exit t)
        ("b" space-macs/helm-buffers-smart-do-search-region-or-symbol :exit t)
        ("f" space-macs/helm-files-smart-do-search-region-or-symbol :exit t)
        ("/" space-macs/helm-project-smart-do-search-region-or-symbol :exit t)))))

(defun helm/post-init-bookmark ()
  (space-macs/set-leader-keys "fb" 'helm-filtered-bookmarks))

(defun helm/init-helm ()
  (use-package helm
    :defer (space-macs/defer)
    :init
    (progn
      (space-macs|diminish helm-ff-cache-mode)
      (space-macs|add-transient-hook completing-read
        (lambda (&rest _args) (require 'helm))
        lazy-load-helm-for-completing-read)
      (space-macs|add-transient-hook completion-at-point
        (lambda (&rest _args) (require 'helm))
        lazy-load-helm-for-completion-at-point)
      (space-macs|add-transient-hook read-file-name
        (lambda (&rest _args) (require 'helm))
        lazy-load-helm-for-read-file-name)
      (add-hook 'helm-cleanup-hook #'space-macs//helm-cleanup)
      ;; key bindings
      ;; Use helm to provide :ls, unless ibuffer is used
      (unless (configuration-layer/package-used-p 'ibuffer)
        (evil-ex-define-cmd "buffers" 'helm-buffers-list))
      ;; use helm by default for M-x, C-x C-f, and C-x b
      (unless (configuration-layer/layer-usedp 'smex)
        (global-set-key (kbd "M-x") 'space-macs/helm-M-x-fuzzy-matching))
      (global-set-key (kbd "C-x C-f") 'space-macs/helm-find-files)
      (global-set-key (kbd "C-x b") 'helm-buffers-list)
      ;; use helm everywhere
      (space-macs||set-helm-key "<f1>" helm-apropos)
      (space-macs||set-helm-key "a'"   helm-available-repls)
      (space-macs||set-helm-key "bb"   helm-mini)
      (space-macs||set-helm-key "bU"   space-macs/helm-buffers-list-unfiltered)
      (space-macs||set-helm-key "Cl"   helm-colors)
      (space-macs||set-helm-key "ff"   space-macs/helm-find-files)
      (space-macs||set-helm-key "fF"   helm-find-files)
      (space-macs||set-helm-key "fL"   helm-locate)
      (space-macs||set-helm-key "fr"   helm-recentf)
      (space-macs||set-helm-key "hda"  helm-apropos)
      (space-macs||set-helm-key "hdF"  space-macs/helm-faces)
      (space-macs||set-helm-key "hi"   helm-info-at-point)
      (space-macs||set-helm-key "hm"   helm-man-woman)
      (space-macs||set-helm-key "iu"   helm-ucs)
      (space-macs||set-helm-key "jI"   helm-imenu-in-all-buffers)
      (space-macs||set-helm-key "rm"   helm-all-mark-rings)
      (space-macs||set-helm-key "rl"   helm-resume)
      (space-macs||set-helm-key "rr"   helm-register)
      (space-macs||set-helm-key "rs"   space-macs/resume-last-search-buffer)
      (space-macs||set-helm-key "ry"   helm-show-kill-ring)
      (space-macs||set-helm-key "sl"   space-macs/resume-last-search-buffer)
      (space-macs||set-helm-key "sj"   space-macs/helm-jump-in-buffer)
      ;; search with grep
      (space-macs||set-helm-key "sgb"  space-macs/helm-buffers-do-grep)
      (space-macs||set-helm-key "sgB"  space-macs/helm-buffers-do-grep-region-or-symbol)
      (space-macs||set-helm-key "sgf"  space-macs/helm-files-do-grep)
      (space-macs||set-helm-key "sgF"  space-macs/helm-files-do-grep-region-or-symbol)
      (space-macs||set-helm-key "sgg"  space-macs/helm-file-do-grep)
      (space-macs||set-helm-key "sgG"  space-macs/helm-file-do-grep-region-or-symbol)
      ;; various key bindings
      (space-macs||set-helm-key "fel" helm-locate-library)
      (space-macs||set-helm-key "hdm" describe-mode)
      (space-macs||set-helm-key "swg" helm-google-suggest)
      (with-eval-after-load 'helm-files
        (define-key helm-find-files-map
          (kbd "C-c C-e") 'space-macs/helm-find-files-edit)
        (defun space-macs//add-action-helm-find-files-edit ()
          (helm-add-action-to-source
           "Edit files in dired `C-c C-e'" 'space-macs//helm-find-files-edit
           helm-source-find-files))
        (add-hook 'helm-find-files-before-init-hook
                  'space-macs//add-action-helm-find-files-edit))
      ;; Add minibuffer history with `helm-minibuffer-history'
      (define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)
      ;; Delay this key bindings to override the defaults
      (add-hook 'e-macs-startup-hook
                (lambda ()
                  (space-macs||set-helm-key "hdb" describe-bindings)
                  (space-macs||set-helm-key "hdc" describe-char)
                  (space-macs||set-helm-key "hdf" describe-function)
                  (space-macs||set-helm-key "hdk" describe-key)
                  (space-macs||set-helm-key "hdl" space-macs/describe-last-keys)
                  (space-macs||set-helm-key "hdp" describe-package)
                  (space-macs||set-helm-key "hdP" configuration-layer/describe-package)
                  (space-macs||set-helm-key "hds" space-macs/describe-system-info)
                  (space-macs||set-helm-key "hdt" describe-theme)
                  (space-macs||set-helm-key "hdv" describe-variable)
                  (space-macs||set-helm-key "hI"  space-macs/report-issue)
                  (space-macs||set-helm-key "hn"  view-e-macs-news)
                  (space-macs||set-helm-key "hPs" profiler-start)
                  (space-macs||set-helm-key "hPk" profiler-stop)
                  (space-macs||set-helm-key "hPr" profiler-report)
                  (space-macs||set-helm-key "hPw" profiler-report-write-profile)
                  ;; define the key binding at the very end in order to allow the user
                  ;; to overwrite any key binding
                  (unless (configuration-layer/layer-usedp 'smex)
                    (space-macs/set-leader-keys
                      dotspace-macs-e-macs-command-key 'space-macs/helm-M-x-fuzzy-matching)))))
    :config
    (progn
      (helm-mode)
      (space-macs|hide-lighter helm-mode)
      (advice-add 'helm-grep-save-results-1 :after 'space-macs//gne-init-helm-grep)
      ;; helm-locate uses es (from everything on windows which doesn't like fuzzy)
      (helm-locate-set-command)
      (setq helm-locate-fuzzy-match (and helm-use-fuzzy (string-match "locate" helm-locate-command)))
      (setq helm-boring-buffer-regexp-list
            (append helm-boring-buffer-regexp-list
                    space-macs-useless-buffers-regexp))
      (setq helm-white-buffer-regexp-list
            (append helm-white-buffer-regexp-list
                    space-macs-useful-buffers-regexp))
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
      (when (configuration-layer/package-used-p 'winum)
        (define-key helm-buffer-map
          (kbd "RET") 'space-macs/helm-find-buffers-windows)
        (define-key helm-generic-files-map
          (kbd "RET") 'space-macs/helm-find-files-windows)
        (define-key helm-find-files-map
          (kbd "RET") 'space-macs/helm-find-files-windows)))))

(defun helm/init-helm-ag ()
  (use-package helm-ag
    :defer (space-macs/defer)
    :init
    (progn
      (setq helm-ag-use-grep-ignore-list t)
      ;; This overrides the default C-s action in helm-projectile-switch-project
      ;; to search using rg/ag/pt/whatever instead of just grep
      (with-eval-after-load 'helm-projectile
        (define-key helm-projectile-projects-map
          (kbd "C-s") 'space-macs/helm-projectile-grep)
        ;; `space-macs/helm-projectile-grep' calls:
        ;; `space-macs/helm-project-smart-do-search-in-dir'
        ;; which needs to be an action.
        ;; Delete the current action.
        (helm-delete-action-from-source
         "Grep in projects `C-s'" helm-source-projectile-projects)
        (helm-add-action-to-source
         "Search in projects `C-s'"
         'space-macs/helm-project-smart-do-search-in-dir
         helm-source-projectile-projects))

      ;; evilify the helm-grep buffer
      (evilified-state-evilify helm-grep-mode helm-grep-mode-map
        (kbd "q") 'quit-window)

      (space-macs/set-leader-keys
        ;; helm-ag marks
        "s`"  'helm-ag-pop-stack
        ;; opened buffers scope
        "sb"  'space-macs/helm-buffers-smart-do-search
        "sB"  'space-macs/helm-buffers-smart-do-search-region-or-symbol
        "sab" 'helm-do-ag-buffers
        "saB" 'space-macs/helm-buffers-do-ag-region-or-symbol
        "skb" 'space-macs/helm-buffers-do-ack
        "skB" 'space-macs/helm-buffers-do-ack-region-or-symbol
        "srb" 'space-macs/helm-buffers-do-rg
        "srB" 'space-macs/helm-buffers-do-rg-region-or-symbol
        "stb" 'space-macs/helm-buffers-do-pt
        "stB" 'space-macs/helm-buffers-do-pt-region-or-symbol
        ;; current file scope
        "ss"  'space-macs/helm-file-smart-do-search
        "sS"  'space-macs/helm-file-smart-do-search-region-or-symbol
        "saa" 'helm-ag-this-file
        "saA" 'space-macs/helm-file-do-ag-region-or-symbol
        ;; files scope
        "sf"  'space-macs/helm-files-smart-do-search
        "sF"  'space-macs/helm-files-smart-do-search-region-or-symbol
        "saf" 'helm-do-ag
        "saF" 'space-macs/helm-files-do-ag-region-or-symbol
        "skf" 'space-macs/helm-files-do-ack
        "skF" 'space-macs/helm-files-do-ack-region-or-symbol
        "srf" 'space-macs/helm-files-do-rg
        "srF" 'space-macs/helm-files-do-rg-region-or-symbol
        "stf" 'space-macs/helm-files-do-pt
        "stF" 'space-macs/helm-files-do-pt-region-or-symbol
        ;; current dir scope
        "sd"  'space-macs/helm-dir-smart-do-search
        "sD"  'space-macs/helm-dir-smart-do-search-region-or-symbol
        "sad" 'space-macs/helm-dir-do-ag
        "saD" 'space-macs/helm-dir-do-ag-region-or-symbol
        "skd" 'space-macs/helm-dir-do-ack
        "skD" 'space-macs/helm-dir-do-ack-region-or-symbol
        "srd" 'space-macs/helm-dir-do-rg
        "srD" 'space-macs/helm-dir-do-rg-region-or-symbol
        "std" 'space-macs/helm-dir-do-pt
        "stD" 'space-macs/helm-dir-do-pt-region-or-symbol
        ;; current project scope
        "/"   'space-macs/helm-project-smart-do-search
        "*"   'space-macs/helm-project-smart-do-search-region-or-symbol
        "sp"  'space-macs/helm-project-smart-do-search
        "sP"  'space-macs/helm-project-smart-do-search-region-or-symbol
        "sap" 'space-macs/helm-project-do-ag
        "saP" 'space-macs/helm-project-do-ag-region-or-symbol
        "skp" 'space-macs/helm-project-do-ack
        "skP" 'space-macs/helm-project-do-ack-region-or-symbol
        "srp" 'space-macs/helm-project-do-rg
        "srP" 'space-macs/helm-project-do-rg-region-or-symbol
        "stp" 'space-macs/helm-project-do-pt
        "stP" 'space-macs/helm-project-do-pt-region-or-symbol))
    :config
    (progn
      (advice-add 'helm-ag--save-results :after 'space-macs//gne-init-helm-ag)
      (evil-define-key 'normal helm-ag-map "SPC" space-macs-default-map)
      (evilified-state-evilify helm-ag-mode helm-ag-mode-map
        (kbd "gr") 'helm-ag--update-save-results
        (kbd "q") 'quit-window))))

(defun helm/init-helm-descbinds ()
  (use-package helm-descbinds
    :defer (space-macs/defer)
    :init
    (progn
      (setq helm-descbinds-window-style 'split)
      (add-hook 'helm-mode-hook 'helm-descbinds-mode)
      (space-macs/set-leader-keys "?" 'helm-descbinds))))

(defun helm/pre-init-helm-flx ()
  (space-macs|use-package-add-hook helm
    :pre-config
    (progn
      ;; Disable for helm-find-files until performance issues are sorted
      ;; https://github.com/PythonNut/helm-flx/issues/9
      (setq helm-flx-for-helm-find-files nil)
      (helm-flx-mode))))

(defun helm/init-helm-flx ()
  (use-package helm-flx
    :defer (space-macs/defer)))

(defun helm/init-helm-ls-git ()
  (use-package helm-ls-git
    :defer t
    :init (space-macs/set-leader-keys "gff" 'helm-ls-git-ls)
    :config
    ;; Set `helm-ls-git-status-command' conditonally on `git' layer
    ;; If `git' is in use, use default `\'magit-status-setup-buffer'
    ;; Otherwise, use defaault `\'vc-dir'
    (when (configuration-layer/package-usedp 'magit)
      (setq helm-ls-git-status-command 'magit-status-setup-buffer))))

(defun helm/init-helm-make ()
  (use-package helm-make
    :defer t
    :init
    (space-macs/set-leader-keys
      "cc" 'helm-make-projectile
      "cm" 'helm-make)))

(defun helm/init-helm-mode-manager ()
  (use-package helm-mode-manager
    :defer t
    :init
    (space-macs/set-leader-keys
      "hM"    'helm-switch-major-mode
      ;; "hm"    'helm-disable-minor-mode
      "h C-m" 'helm-enable-minor-mode)))

(defun helm/init-helm-org ()
  (use-package helm-org
    :commands (helm-org-in-buffer-headings)
    :defer (space-macs/defer)))

(defun helm/pre-init-helm-projectile ()
  ;; overwrite projectile settings
  (space-macs|use-package-add-hook projectile
    :post-init
    (progn
      (setq projectile-switch-project-action 'helm-projectile)
      (space-macs/set-leader-keys
        "pb"  'helm-projectile-switch-to-buffer
        "pd"  'helm-projectile-find-dir
        "pf"  'helm-projectile-find-file
        "pF"  'helm-projectile-find-file-dwim
        "ph"  'helm-projectile
        "pp"  'helm-projectile-switch-project
        "pr"  'helm-projectile-recentf
        "sgp" 'helm-projectile-grep))))

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
      (defalias 'space-macs/helm-project-do-grep 'helm-projectile-grep)
      (defalias
        'space-macs/helm-project-do-grep-region-or-symbol
        'helm-projectile-grep))
    :config (when (configuration-layer/package-used-p 'winum)
              (define-key helm-projectile-find-file-map
                (kbd "RET") 'space-macs/helm-find-files-windows))))

(defun helm/init-helm-space-macs-help ()
  (use-package helm-space-macs-help
    :commands (helm-space-macs-help-dotspace-macs
               helm-space-macs-help
               helm-space-macs-help-faq
               helm-space-macs-help-layers
               helm-space-macs-help-packages
               helm-space-macs-help-docs
               helm-space-macs-help-toggles)
    :init (space-macs/set-leader-keys
            "h ."   'helm-space-macs-help-dotspace-macs
            "h SPC" 'helm-space-macs-help
            "h f"   'helm-space-macs-help-faq
            "h l"   'helm-space-macs-help-layers
            "h p"   'helm-space-macs-help-packages
            "h r"   'helm-space-macs-help-docs
            "h t"   'helm-space-macs-help-toggles)))

(defun helm/init-helm-space-macs-faq ()
  (use-package helm-space-macs-faq
    :commands helm-space-macs-help-faq
    :init (space-macs/set-leader-keys "h f" 'helm-space-macs-help-faq)))

(defun helm/init-helm-swoop ()
  (use-package helm-swoop
    :defer (space-macs/defer)
    :init
    (progn
      (setq helm-swoop-split-with-multiple-windows t
            helm-swoop-split-direction 'split-window-vertically
            helm-swoop-speed-or-color t
            helm-swoop-split-window-function 'space-macs/helm-swoop-split-window-function
            helm-swoop-pre-input-function (lambda () ""))

      (defun space-macs/helm-swoop-split-window-function (&rest args)
        "Override to make helm settings (like `helm-split-window-default-side') work"
        (let (;; current helm-swoop implemenatation prevents it from being used fullscreen
               (helm-full-frame nil)
               (pop-up-windows t))
          (apply 'helm-default-display-buffer args)))

      (defun space-macs/helm-swoop-region-or-symbol ()
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

      (defun space-macs/helm-swoop-clear-cache ()
        "Call `helm-swoop--clear-cache' to clear the cache"
        (interactive)
        (helm-swoop--clear-cache)
        (message "helm-swoop cache cleaned."))

      (space-macs/set-leader-keys
        "sC"    'space-macs/helm-swoop-clear-cache
        "ss"    'helm-swoop
        "sS"    'space-macs/helm-swoop-region-or-symbol
        "s C-s" 'helm-multi-swoop-all)
      (defadvice helm-swoop (before add-evil-jump activate)
        (evil-set-jump)))))

(defun helm/init-helm-themes ()
  (use-package helm-themes
    :defer t
    :init
    (space-macs/set-leader-keys
      "Ts" 'space-macs/helm-themes)))

(defun helm/init-helm-xref ()
  (use-package helm-xref
    :commands (helm-xref-show-xrefs-27 helm-xref-show-xrefs)
    :init
    (progn
      ;; This is required to make `xref-find-references' not give a prompt.
      ;; `xref-find-references' asks the identifier (which has no text property)
      ;; and then passes it to `lsp-mode', which requires the text property at
      ;; point to locate the references.
      ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=29619
      (setq xref-prompt-for-identifier '(not xref-find-definitions
                                             xref-find-definitions-other-window
                                             xref-find-definitions-other-frame
                                             xref-find-references
                                             space-macs/jump-to-definition))
      ;; Use helm-xref to display `xref.el' results.
      (setq xref-show-xrefs-function (if (< e-macs-major-version 27)
                                         #'helm-xref-show-xrefs
                                       #'helm-xref-show-xrefs-27)))))


(defun helm/post-init-imenu ()
  (space-macs/set-leader-keys "ji" 'space-macs/helm-jump-in-buffer))

(defun helm/post-init-popwin ()
  ;; disable popwin-mode while Helm session is running
  (add-hook 'helm-after-initialize-hook #'space-macs//helm-prepare-display)
  ;;  Restore popwin-mode after a Helm session finishes.
  (add-hook 'helm-cleanup-hook #'space-macs//helm-restore-display))

(defun helm/pre-init-persp-mode ()
  (space-macs|use-package-add-hook persp-mode
    :post-config
    (setq
     space-macs--persp-display-buffers-func 'space-macs/persp-helm-mini
     space-macs--persp-display-perspectives-func 'space-macs/helm-perspectives)))

(defun helm/post-init-projectile ()
  (setq projectile-completion-system 'helm))


