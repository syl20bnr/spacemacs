;;; packages.el --- Auto-completion Layer packages File for Spacemacs
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

(setq auto-completion-packages
      '(
        auto-complete
        ac-ispell
        company
        company-statistics
        helm-c-yasnippet
        hippie-exp
        yasnippet
        auto-yasnippet
        ))

;; company-quickhelp from MELPA is not compatible with 24.3 anymore
(unless (version< emacs-version "24.4")
  (push 'company-quickhelp auto-completion-packages))

(defun auto-completion/init-ac-ispell ()
  (use-package ac-ispell
    :defer t
    :init
    (progn
      (setq ac-ispell-requires 4)
      (eval-after-load 'auto-complete
        '(ac-ispell-setup))
      ;; (add-hook 'markdown-mode-hook 'ac-ispell-ac-setup)
      )))

(defun auto-completion/init-auto-complete ()
  (use-package auto-complete
    :defer t
    :init
    (setq ac-auto-start 0
          ac-delay 0.2
          ac-quick-help-delay 1.
          ac-use-fuzzy t
          ac-fuzzy-enable t
          ac-comphist-file (concat spacemacs-cache-directory "ac-comphist.dat")
          ;; use 'complete when auto-complete is disabled
          tab-always-indent 'complete
          ac-dwim t)
    :config
    (progn
      (require 'auto-complete-config)
      (setq-default ac-sources '(ac-source-abbrev
                                 ac-source-dictionary
                                 ac-source-words-in-same-mode-buffers))
      (when (configuration-layer/package-usedp 'yasnippet)
        (push 'ac-source-yasnippet ac-sources))
      (add-to-list 'completion-styles 'initials t)
      (define-key ac-completing-map (kbd "C-j") 'ac-next)
      (define-key ac-completing-map (kbd "C-k") 'ac-previous)
      (define-key ac-completing-map (kbd "<S-tab>") 'ac-previous)
      (spacemacs|diminish auto-complete-mode " ⓐ" " a"))))

(defun auto-completion/init-company ()
  (use-package company
    :defer t
    :init
    (progn
      (setq company-idle-delay 0.2
            company-minimum-prefix-length 2
            company-require-match nil
            company-dabbrev-ignore-case nil
            company-dabbrev-downcase nil
            company-frontends '(company-pseudo-tooltip-frontend)
            company-clang-prefix-guesser 'company-mode/more-than-prefix-guesser))
            (defvar-local company-fci-mode-on-p nil)

        (defun company-turn-off-fci (&rest ignore)
          (when (boundp 'fci-mode)
            (setq company-fci-mode-on-p fci-mode)
            (when fci-mode (fci-mode -1))))

        (defun company-maybe-turn-on-fci (&rest ignore)
          (when company-fci-mode-on-p (fci-mode 1)))

        (add-hook 'company-completion-started-hook 'company-turn-off-fci)
        (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
        (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)
    :config
    (progn
      (spacemacs|diminish company-mode " ⓐ" " a")

      ;; key bindings
      (defun spacemacs//company-complete-common-or-cycle-backward ()
        "Complete common prefix or cycle backward."
        (interactive)
        (company-complete-common-or-cycle -1))
      (spacemacs//auto-completion-set-RET-key-behavior 'company)
      (spacemacs//auto-completion-set-TAB-key-behavior 'company)
      (spacemacs//auto-completion-setup-key-sequence 'company)
      (let ((map company-active-map))
        (define-key map (kbd "C-/") 'company-search-candidates)
        (define-key map (kbd "C-M-/") 'company-filter-candidates)
        (define-key map (kbd "C-d") 'company-show-doc-buffer)
        (define-key map (kbd "C-j") 'company-select-next)
        (define-key map (kbd "C-k") 'company-select-previous)
        (define-key map (kbd "C-l") 'company-complete-selection))
      ;; Nicer looking faces
      (custom-set-faces
       '(company-tooltip-common
         ((t (:inherit company-tooltip :weight bold :underline nil))))
       '(company-tooltip-common-selection
         ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
      ;; Transformers
      (defun spacemacs//company-transformer-cancel (candidates)
        "Cancel completion if prefix is in the list
`company-mode-completion-cancel-keywords'"
        (unless (member company-prefix company-mode-completion-cancel-keywords)
          candidates))
      (setq company-transformers '(spacemacs//company-transformer-cancel
                                   company-sort-by-occurrence)))))

(defun auto-completion/init-company-statistics ()
  (use-package company-statistics
    :if auto-completion-enable-sort-by-usage
    :defer t
    :init
    (progn
      (setq company-statistics-file (concat spacemacs-cache-directory
                                            "company-statistics-cache.el"))
      (add-hook 'company-mode-hook 'company-statistics-mode))))

(defun auto-completion/init-company-quickhelp ()
  (use-package company-quickhelp
    :if (and auto-completion-enable-help-tooltip (display-graphic-p))
    :defer t
    :init (add-hook 'company-mode-hook 'company-quickhelp-mode)))

(defun auto-completion/init-helm-c-yasnippet ()
  (use-package helm-c-yasnippet
    :defer t
    :init
    (progn
      (defun spacemacs/helm-yas ()
        "Properly lazy load helm-c-yasnipper."
        (interactive)
        (spacemacs/load-yasnippet)
        (require 'helm-c-yasnippet)
        (call-interactively 'helm-yas-complete))
      (evil-leader/set-key "is" 'spacemacs/helm-yas)
      (setq helm-c-yas-space-match-any-greedy t))))

(defun auto-completion/init-hippie-exp ()
  ;; replace dabbrev-expand
  (global-set-key (kbd "M-/") 'hippie-expand)
  (define-key evil-insert-state-map (kbd "C-p") 'hippie-expand)
  (setq hippie-expand-try-functions-list
        '(
          ;; Try to expand word "dynamically", searching the current buffer.
          try-expand-dabbrev
          ;; Try to expand word "dynamically", searching all other buffers.
          try-expand-dabbrev-all-buffers
          ;; Try to expand word "dynamically", searching the kill ring.
          try-expand-dabbrev-from-kill
          ;; Try to complete text as a file name, as many characters as unique.
          try-complete-file-name-partially
          ;; Try to complete text as a file name.
          try-complete-file-name
          ;; Try to expand word before point according to all abbrev tables.
          try-expand-all-abbrevs
          ;; Try to complete the current line to an entire line in the buffer.
          try-expand-list
          ;; Try to complete the current line to an entire line in the buffer.
          try-expand-line
          ;; Try to complete as an Emacs Lisp symbol, as many characters as
          ;; unique.
          try-complete-lisp-symbol-partially
          ;; Try to complete word as an Emacs Lisp symbol.
          try-complete-lisp-symbol))
  (when (configuration-layer/package-usedp 'yasnippet)
    ;; Try to expand yasnippet snippets based on prefix
    (push 'yas-hippie-try-expand hippie-expand-try-functions-list)))

(defun auto-completion/init-yasnippet ()
  (use-package yasnippet
    :commands yas-global-mode
    :init
    (progn
      ;; disable yas minor mode map
      ;; use hippie-expand instead
      (setq yas-minor-mode-map (make-sparse-keymap))

      ;; allow nested expansions
      (setq yas-triggers-in-field t)

      ;; this makes it easy to get out of a nested expansion
      (define-key yas-minor-mode-map
        (kbd "M-s-/") 'yas-next-field)

      ;; add key into candidate list
      (setq helm-yas-display-key-on-candidate t)
      (setq spacemacs--auto-completion-dir
            (configuration-layer/get-layer-property 'auto-completion :dir))

      (defun spacemacs/load-yasnippet ()
        (unless yas-global-mode
          (progn
            (yas-global-mode 1)
            (let ((private-yas-dir (concat
                                    configuration-layer-private-directory
                                    "snippets/"))
                  (spacemacs-snippets-dir (expand-file-name
                                           "snippets"
                                           spacemacs--auto-completion-dir)))
              (setq yas-snippet-dirs
                    (append (list private-yas-dir)
                            (when (boundp 'yas-snippet-dirs)
                              yas-snippet-dirs)
                            spacemacs-snippets-dir))
              (yas-load-directory spacemacs-snippets-dir t)
              (yas-load-directory private-yas-dir t)
              (setq yas-wrap-around-region t))))
        (yas-minor-mode 1))

      (add-to-hooks 'spacemacs/load-yasnippet '(prog-mode-hook
                                                markdown-mode-hook
                                                org-mode-hook))
      (spacemacs|add-toggle yasnippet
                            :status yas-minor-mode
                            :on (yas-minor-mode)
                            :off (yas-minor-mode -1)
                            :documentation "Enable yasnippet."
                            :evil-leader "ty")

      (defun spacemacs/force-yasnippet-off ()
        (yas-minor-mode -1)
        (setq yas-dont-activate t))

      (add-to-hooks 'spacemacs/force-yasnippet-off '(term-mode-hook
                                                     shell-mode-hook
                                                     eshell-mode-hook)))
    :config
    (progn
      ;;  We need to know whether the smartparens was enabled, see
      ;; `yas-before-expand-snippet-hook' below.
      (defvar smartparens-enabled-initially t
        "Stored whether smartparens is originally enabled or not.")

      (add-hook 'yas-before-expand-snippet-hook (lambda ()
                                                  ;; If enabled, smartparens will mess snippets expanded by `hippie-expand`
                                                  (setq smartparens-enabled-initially smartparens-mode)
                                                  (smartparens-mode -1)))
      (add-hook 'yas-after-exit-snippet-hook (lambda ()
                                               (when smartparens-enabled-initially
                                                 (smartparens-mode 1))))
      (spacemacs|diminish yas-minor-mode " ⓨ" " y"))))

(defun auto-completion/init-auto-yasnippet ()
  (use-package auto-yasnippet
    :defer t
    :init
    (progn
      (setq aya-persist-snippets-dir (concat
                                      configuration-layer-private-directory
                                      "snippets/"))
      (defun spacemacs/auto-yasnippet-expand ()
        "Call `yas-expand' and switch to `insert state'"
        (interactive)
        (call-interactively 'aya-expand)
        (evil-insert-state))
      (evil-leader/set-key
        "iSc" 'aya-create
        "iSe" 'spacemacs/auto-yasnippet-expand
        "iSw" 'aya-persist-snippet))))
