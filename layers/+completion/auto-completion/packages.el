;;; packages.el --- Auto-completion Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
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
        (company-quickhelp :toggle auto-completion-enable-help-tooltip)
        company-statistics
        (helm-company :toggle (configuration-layer/package-usedp 'helm))
        (helm-c-yasnippet :toggle (configuration-layer/package-usedp 'helm))
        hippie-exp
        yasnippet
        auto-yasnippet
        smartparens
        ))

;; TODO replace by company-ispell which comes with company
;; to be moved to spell-checking layer as well
(defun auto-completion/init-ac-ispell ()
  (use-package ac-ispell
    :defer t
    :init
    (progn
      (setq ac-ispell-requires 4)
      (with-eval-after-load 'auto-complete
        (ac-ispell-setup))
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
            company-dabbrev-downcase nil)

      (add-hook 'company-completion-started-hook 'company-turn-off-fci)
      (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
      (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci))
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
        (define-key map (kbd "C-/")   'company-search-candidates)
        (define-key map (kbd "C-M-/") 'company-filter-candidates)
        (define-key map (kbd "C-d")   'company-show-doc-buffer))
      (add-hook 'spacemacs-editing-style-hook 'spacemacs//company-active-navigation)
      ;; ensure that the correct bindings are set at startup
      (spacemacs//company-active-navigation dotspacemacs-editing-style)

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
    :commands company-quickhelp-manual-begin
    :init
    (spacemacs|do-after-display-system-init
     (with-eval-after-load 'company
       (setq company-frontends (delq 'company-echo-metadata-frontend company-frontends))
       (define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin)
       (unless (eq auto-completion-enable-help-tooltip 'manual)
         (company-quickhelp-mode))))))

(defun auto-completion/init-helm-c-yasnippet ()
  (use-package helm-c-yasnippet
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys "is" 'spacemacs/helm-yas)
      (setq helm-c-yas-space-match-any-greedy t))))

(defun auto-completion/init-helm-company ()
  (use-package helm-company
    :if (configuration-layer/package-usedp 'company)
    :defer t
    :init
    (with-eval-after-load 'company
      (define-key company-active-map (kbd "C-/") 'helm-company))))

(defun auto-completion/init-hippie-exp ()
  ;; replace dabbrev-expand
  (global-set-key (kbd "M-/") 'hippie-expand)
  (define-key evil-insert-state-map [remap evil-complete-previous] 'hippie-expand)
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
    :commands (yas-global-mode yas-minor-mode)
    :init
    (progn
      ;; We don't want undefined variable errors
      (defvar yas-global-mode nil)
      (setq yas-triggers-in-field t
            yas-wrap-around-region t
            helm-yas-display-key-on-candidate t)
      ;; on multiple keys, fall back to completing read
      ;; typically this means helm
      (setq yas-prompt-functions '(yas-completing-prompt))
      ;; disable yas minor mode map
      ;; use hippie-expand instead
      (setq yas-minor-mode-map (make-sparse-keymap))
      ;; this makes it easy to get out of a nested expansion
      (define-key yas-minor-mode-map (kbd "M-s-/") 'yas-next-field)
      ;; configure snippet directories
      (let* ((spacemacs--auto-completion-dir
              (configuration-layer/get-layer-local-dir 'auto-completion))
             (private-yas-dir (if auto-completion-private-snippets-directory
                                  auto-completion-private-snippets-directory
                                (concat
                                 configuration-layer-private-directory
                                 "snippets/")))
             (spacemacs-layer-snippets-dir (expand-file-name
                                      "snippets"
                                      spacemacs--auto-completion-dir))
             (dotspacemacs-directory-snippets-dir (when dotspacemacs-directory
                                                    (expand-file-name
                                                     "snippets"
                                                     dotspacemacs-directory))))
        (setq yas-snippet-dirs nil)
        ;; ~/.emacs.d/layers/auto-completion/snippets
        (push spacemacs-layer-snippets-dir yas-snippet-dirs)
        ;; ~/.emacs.d/elpa/yasnippet-xxxxx/snippets
        (push 'yas-installed-snippets-dir yas-snippet-dirs)
        ;; ~/.spacemacs.d/snippets
        (when dotspacemacs-directory-snippets-dir
          (push dotspacemacs-directory-snippets-dir yas-snippet-dirs))
        ;; arbitrary directories in `auto-completion-private-snippets-directory'
        (when private-yas-dir
          (if (listp private-yas-dir)
              (setq yas-snippet-dirs (append yas-snippet-dirs private-yas-dir))
            (push private-yas-dir yas-snippet-dirs))))

      (spacemacs/add-to-hooks 'spacemacs/load-yasnippet '(prog-mode-hook
                                                          markdown-mode-hook
                                                          org-mode-hook))
      (spacemacs|add-toggle yasnippet
        :mode yas-minor-mode
        :documentation "Enable snippets."
        :evil-leader "ty")

      (spacemacs/add-to-hooks
       'spacemacs/force-yasnippet-off '(term-mode-hook
                                        shell-mode-hook
                                        eshell-mode-hook)))
    :config (spacemacs|diminish yas-minor-mode " ⓨ" " y")))

(defun auto-completion/init-auto-yasnippet ()
  (use-package auto-yasnippet
    :defer t
    :init
    (progn
      (setq aya-persist-snippets-dir
            (or auto-completion-private-snippets-directory
                (concat configuration-layer-private-directory "snippets/")))
      (spacemacs/declare-prefix "iS" "auto-yasnippet")
      (spacemacs/set-leader-keys
        "iSc" 'aya-create
        "iSe" 'spacemacs/auto-yasnippet-expand
        "iSw" 'aya-persist-snippet))))

(defun auto-completion/post-init-smartparens ()
  (with-eval-after-load 'smartparens
    (add-hook 'yas-before-expand-snippet-hook
              #'spacemacs//smartparens-disable-before-expand-snippet)
    (add-hook 'yas-after-exit-snippet-hook
              #'spacemacs//smartparens-restore-after-exit-snippet)))
