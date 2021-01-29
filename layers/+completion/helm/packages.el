;;; packages.el --- Helm Layer packages File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
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
        (helm-ls-git :require git)
        helm-make
        helm-mode-manager
        helm-org
        helm-projectile
        helm-swoop
        helm-themes
        (helm-spacemacs-help :location local)
        (helm-spacemacs-faq :location local)
        helm-xref
        imenu
        persp-mode
        popwin
        projectile
        ))

;; Initialization of packages

(defun helm/init-ace-jump-helm-line ()
  (use-package ace-jump-helm-line
    :defer (spacemacs/defer)
    :init
    (with-eval-after-load 'helm
      (define-key helm-map (kbd "C-q") 'ace-jump-helm-line))))

(defun helm/pre-init-auto-highlight-symbol ()
  (spacemacs|use-package-add-hook auto-highlight-symbol
    :post-init
    ;; add some functions to ahs transient states
    (setq spacemacs--symbol-highlight-transient-state-doc
          (concat
           spacemacs--symbol-highlight-transient-state-doc
           "  Search: [_s_] swoop  [_b_] buffers  [_f_] files  [_/_] project"))
    (spacemacs/transient-state-register-add-bindings 'symbol-highlight
      '(("s" spacemacs/helm-swoop-region-or-symbol :exit t)
        ("b" spacemacs/helm-buffers-smart-do-search-region-or-symbol :exit t)
        ("f" spacemacs/helm-files-smart-do-search-region-or-symbol :exit t)
        ("/" spacemacs/helm-project-smart-do-search-region-or-symbol :exit t)))))

(defun helm/post-init-bookmark ()
  (spacemacs/set-leader-keys "fb" 'helm-filtered-bookmarks))

(defun helm/init-helm ()
  (use-package helm
    :defer (spacemacs/defer)
    :init
    (progn
      (spacemacs|diminish helm-ff-cache-mode)
      (spacemacs|add-transient-hook completing-read
        (lambda (&rest _args) (require 'helm))
        lazy-load-helm-for-completing-read)
      (spacemacs|add-transient-hook completion-at-point
        (lambda (&rest _args) (require 'helm))
        lazy-load-helm-for-completion-at-point)
      (spacemacs|add-transient-hook read-file-name
        (lambda (&rest _args) (require 'helm))
        lazy-load-helm-for-read-file-name)
      (add-hook 'helm-cleanup-hook #'spacemacs//helm-cleanup)
      ;; key bindings
      ;; Use helm to provide :ls, unless ibuffer is used
      (unless (configuration-layer/package-used-p 'ibuffer)
        (evil-ex-define-cmd "buffers" 'helm-buffers-list))
      ;; use helm by default for M-x, C-x C-f, and C-x b
      (unless (configuration-layer/layer-usedp 'smex)
        (global-set-key (kbd "M-x") 'spacemacs/helm-M-x-fuzzy-matching))
      (global-set-key (kbd "C-x C-f") 'spacemacs/helm-find-files)
      (global-set-key (kbd "C-x b") 'helm-buffers-list)
      ;; use helm to switch last(/previous) visited buffers with C(-S)-tab
      (evil-global-set-key 'motion (kbd "<C-tab>") 'helm-buffers-list)
      (evil-global-set-key 'motion (kbd "<C-iso-lefttab>") 'helm-buffers-list)
      ;; use helm everywhere
      (spacemacs||set-helm-key "<f1>" helm-apropos)
      (spacemacs||set-helm-key "a'"   helm-available-repls)
      (spacemacs||set-helm-key "bb"   helm-mini)
      (spacemacs||set-helm-key "bU"   spacemacs/helm-buffers-list-unfiltered)
      (spacemacs||set-helm-key "Cl"   helm-colors)
      (spacemacs||set-helm-key "ff"   spacemacs/helm-find-files)
      (spacemacs||set-helm-key "fF"   helm-find-files)
      (spacemacs||set-helm-key "fL"   helm-locate)
      (spacemacs||set-helm-key "fr"   helm-recentf)
      (spacemacs||set-helm-key "hda"  helm-apropos)
      (spacemacs||set-helm-key "hdF"  spacemacs/helm-faces)
      (spacemacs||set-helm-key "hi"   helm-info-at-point)
      (spacemacs||set-helm-key "hm"   helm-man-woman)
      (spacemacs||set-helm-key "iu"   helm-ucs)
      (spacemacs||set-helm-key "jI"   helm-imenu-in-all-buffers)
      (spacemacs||set-helm-key "rm"   helm-all-mark-rings)
      (spacemacs||set-helm-key "rl"   helm-resume)
      (spacemacs||set-helm-key "rr"   helm-register)
      (spacemacs||set-helm-key "rs"   spacemacs/resume-last-search-buffer)
      (spacemacs||set-helm-key "ry"   helm-show-kill-ring)
      (spacemacs||set-helm-key "sl"   spacemacs/resume-last-search-buffer)
      (spacemacs||set-helm-key "sj"   spacemacs/helm-jump-in-buffer)
      ;; search with grep
      (spacemacs||set-helm-key "sgb"  spacemacs/helm-buffers-do-grep)
      (spacemacs||set-helm-key "sgB"  spacemacs/helm-buffers-do-grep-region-or-symbol)
      (spacemacs||set-helm-key "sgf"  spacemacs/helm-files-do-grep)
      (spacemacs||set-helm-key "sgF"  spacemacs/helm-files-do-grep-region-or-symbol)
      (spacemacs||set-helm-key "sgg"  spacemacs/helm-file-do-grep)
      (spacemacs||set-helm-key "sgG"  spacemacs/helm-file-do-grep-region-or-symbol)
      ;; various key bindings
      (spacemacs||set-helm-key "fel" helm-locate-library)
      (spacemacs||set-helm-key "hdm" describe-mode)
      (spacemacs||set-helm-key "swg" helm-google-suggest)
      (with-eval-after-load 'helm-files
        (define-key helm-find-files-map
          (kbd "C-c C-e") 'spacemacs/helm-find-files-edit)
        (defun spacemacs//add-action-helm-find-files-edit ()
          (helm-add-action-to-source
           "Edit files in dired `C-c C-e'" 'spacemacs//helm-find-files-edit
           helm-source-find-files))
        (add-hook 'helm-find-files-before-init-hook
                  'spacemacs//add-action-helm-find-files-edit))
      ;; Add minibuffer history with `helm-minibuffer-history'
      (define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)
      ;; Delay this key bindings to override the defaults
      (add-hook 'emacs-startup-hook
                (lambda ()
                  (spacemacs||set-helm-key "hdb" describe-bindings)
                  (spacemacs||set-helm-key "hdc" describe-char)
                  (spacemacs||set-helm-key "hdf" describe-function)
                  (spacemacs||set-helm-key "hdk" describe-key)
                  (spacemacs||set-helm-key "hdl" spacemacs/describe-last-keys)
                  (spacemacs||set-helm-key "hdp" describe-package)
                  (spacemacs||set-helm-key "hdP" configuration-layer/describe-package)
                  (spacemacs||set-helm-key "hds" spacemacs/describe-system-info)
                  (spacemacs||set-helm-key "hdt" describe-theme)
                  (spacemacs||set-helm-key "hdv" describe-variable)
                  (spacemacs||set-helm-key "hI"  spacemacs/report-issue)
                  (spacemacs||set-helm-key "hn"  view-emacs-news)
                  (spacemacs||set-helm-key "hPs" profiler-start)
                  (spacemacs||set-helm-key "hPk" profiler-stop)
                  (spacemacs||set-helm-key "hPr" profiler-report)
                  (spacemacs||set-helm-key "hPw" profiler-report-write-profile)
                  ;; define the key binding at the very end in order to allow the user
                  ;; to overwrite any key binding
                  (unless (configuration-layer/layer-usedp 'smex)
                    (spacemacs/set-leader-keys
                      dotspacemacs-emacs-command-key 'spacemacs/helm-M-x-fuzzy-matching)))))
    :config
    (progn
      (helm-mode)
      (spacemacs|hide-lighter helm-mode)
      (advice-add 'helm-grep-save-results-1 :after 'spacemacs//gne-init-helm-grep)
      ;; helm-locate uses es (from everything on windows which doesn't like fuzzy)
      (helm-locate-set-command)
      (setq helm-locate-fuzzy-match (and (bound-and-true-p helm-use-fuzzy)
                                         (string-match "locate" helm-locate-command)))
      (setq helm-boring-buffer-regexp-list
            (append helm-boring-buffer-regexp-list
                    spacemacs-useless-buffers-regexp))
      (setq helm-white-buffer-regexp-list
            (append helm-white-buffer-regexp-list
                    spacemacs-useful-buffers-regexp))
      ;; use helm to switch last(/previous) visited buffers with C(-S)-tab
      (define-key helm-map (kbd "<C-tab>") 'helm-follow-action-forward)
      (define-key helm-map (kbd "<C-iso-lefttab>") 'helm-follow-action-backward)
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
          (kbd "RET") 'spacemacs/helm-find-buffers-windows)
        (define-key helm-generic-files-map
          (kbd "RET") 'spacemacs/helm-find-files-windows)
        (define-key helm-find-files-map
          (kbd "RET") 'spacemacs/helm-find-files-windows)))))

(defun helm/init-helm-ag ()
  (use-package helm-ag
    :defer (spacemacs/defer)
    :init
    (progn
      (setq helm-ag-use-grep-ignore-list t)
      ;; This overrides the default C-s action in helm-projectile-switch-project
      ;; to search using rg/ag/pt/whatever instead of just grep
      (with-eval-after-load 'helm-projectile
        (define-key helm-projectile-projects-map
          (kbd "C-s") 'spacemacs/helm-projectile-grep)
        ;; `spacemacs/helm-projectile-grep' calls:
        ;; `spacemacs/helm-project-smart-do-search-in-dir'
        ;; which needs to be an action.
        ;; Delete the current action.
        (helm-delete-action-from-source
         "Grep in projects `C-s'" helm-source-projectile-projects)
        (helm-add-action-to-source
         "Search in projects `C-s'"
         'spacemacs/helm-project-smart-do-search-in-dir
         helm-source-projectile-projects))

      ;; evilify the helm-grep buffer
      (evilified-state-evilify helm-grep-mode helm-grep-mode-map
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
        "srb" 'spacemacs/helm-buffers-do-rg
        "srB" 'spacemacs/helm-buffers-do-rg-region-or-symbol
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
        "srf" 'spacemacs/helm-files-do-rg
        "srF" 'spacemacs/helm-files-do-rg-region-or-symbol
        "stf" 'spacemacs/helm-files-do-pt
        "stF" 'spacemacs/helm-files-do-pt-region-or-symbol
        ;; current dir scope
        "sd"  'spacemacs/helm-dir-smart-do-search
        "sD"  'spacemacs/helm-dir-smart-do-search-region-or-symbol
        "sad" 'spacemacs/helm-dir-do-ag
        "saD" 'spacemacs/helm-dir-do-ag-region-or-symbol
        "skd" 'spacemacs/helm-dir-do-ack
        "skD" 'spacemacs/helm-dir-do-ack-region-or-symbol
        "srd" 'spacemacs/helm-dir-do-rg
        "srD" 'spacemacs/helm-dir-do-rg-region-or-symbol
        "std" 'spacemacs/helm-dir-do-pt
        "stD" 'spacemacs/helm-dir-do-pt-region-or-symbol
        ;; current project scope
        "/"   'spacemacs/helm-project-smart-do-search
        "*"   'spacemacs/helm-project-smart-do-search-region-or-symbol
        "sp"  'spacemacs/helm-project-smart-do-search
        "sP"  'spacemacs/helm-project-smart-do-search-region-or-symbol
        "sap" 'spacemacs/helm-project-do-ag
        "saP" 'spacemacs/helm-project-do-ag-region-or-symbol
        "skp" 'spacemacs/helm-project-do-ack
        "skP" 'spacemacs/helm-project-do-ack-region-or-symbol
        "srp" 'spacemacs/helm-project-do-rg
        "srP" 'spacemacs/helm-project-do-rg-region-or-symbol
        "stp" 'spacemacs/helm-project-do-pt
        "stP" 'spacemacs/helm-project-do-pt-region-or-symbol))
    :config
    (progn
      (advice-add 'helm-ag--save-results :after 'spacemacs//gne-init-helm-ag)
      (evil-define-key 'normal helm-ag-map "SPC" spacemacs-default-map)
      (evilified-state-evilify helm-ag-mode helm-ag-mode-map
        (kbd "gr") 'helm-ag--update-save-results
        (kbd "q") 'quit-window))))

(defun helm/init-helm-descbinds ()
  (use-package helm-descbinds
    :defer (spacemacs/defer)
    :init
    (progn
      (setq helm-descbinds-window-style 'split)
      (add-hook 'helm-mode-hook 'helm-descbinds-mode)
      (spacemacs/set-leader-keys "?" 'helm-descbinds))))

(defun helm/pre-init-helm-flx ()
  (spacemacs|use-package-add-hook helm
    :pre-config
    (progn
      ;; Disable for helm-find-files until performance issues are sorted
      ;; https://github.com/PythonNut/helm-flx/issues/9
      (setq helm-flx-for-helm-find-files nil)
      (helm-flx-mode))))

(defun helm/init-helm-flx ()
  (use-package helm-flx
    :defer (spacemacs/defer)))

(defun helm/init-helm-ls-git ()
  (use-package helm-ls-git
    :defer t
    :init (spacemacs/set-leader-keys "gff" 'helm-ls-git-ls)
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

(defun helm/init-helm-org ()
  (use-package helm-org
    :commands (helm-org-in-buffer-headings)
    :defer (spacemacs/defer)))

(defun helm/pre-init-helm-projectile ()
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
      (defalias 'spacemacs/helm-project-do-grep 'helm-projectile-grep)
      (defalias
        'spacemacs/helm-project-do-grep-region-or-symbol
        'helm-projectile-grep))
    :config (when (configuration-layer/package-used-p 'winum)
              (define-key helm-projectile-find-file-map
                (kbd "RET") 'spacemacs/helm-find-files-windows))))

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
    :defer (spacemacs/defer)
    :init
    (progn
      (setq helm-swoop-split-with-multiple-windows t
            helm-swoop-split-direction 'split-window-vertically
            helm-swoop-speed-or-color t
            helm-swoop-split-window-function 'spacemacs/helm-swoop-split-window-function
            helm-swoop-pre-input-function (lambda () ""))

      (defun spacemacs/helm-swoop-split-window-function (&rest args)
        "Override to make helm settings (like `helm-split-window-default-side') work"
        (let (;; current helm-swoop implemenatation prevents it from being used fullscreen
               (helm-full-frame nil)
               (pop-up-windows t))
          (apply 'helm-default-display-buffer args)))

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

      (defun spacemacs/helm-swoop-clear-cache ()
        "Call `helm-swoop--clear-cache' to clear the cache"
        (interactive)
        (helm-swoop--clear-cache)
        (message "helm-swoop cache cleaned."))

      (spacemacs/set-leader-keys
        "sC"    'spacemacs/helm-swoop-clear-cache
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
                                             spacemacs/jump-to-definition))
      ;; Use helm-xref to display `xref.el' results.
      (setq xref-show-xrefs-function (if (< emacs-major-version 27)
                                         #'helm-xref-show-xrefs
                                       #'helm-xref-show-xrefs-27)))))


(defun helm/post-init-imenu ()
  (spacemacs/set-leader-keys "ji" 'spacemacs/helm-jump-in-buffer))

(defun helm/post-init-popwin ()
  ;; disable popwin-mode while Helm session is running
  (add-hook 'helm-after-initialize-hook #'spacemacs//helm-prepare-display)
  ;;  Restore popwin-mode after a Helm session finishes.
  (add-hook 'helm-cleanup-hook #'spacemacs//helm-restore-display))

(defun helm/pre-init-persp-mode ()
  (spacemacs|use-package-add-hook persp-mode
    :post-config
    (setq
     spacemacs--persp-display-buffers-func 'spacemacs/persp-helm-mini
     spacemacs--persp-display-perspectives-func 'spacemacs/helm-perspectives)))

(defun helm/post-init-projectile ()
  (setq projectile-completion-system 'helm))
