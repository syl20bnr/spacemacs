(defvar company-mode-packages
  '(company
    company-quickhelp))

(defvar company-mode-excluded-packages
  '(auto-complete ac-ispell tern-auto-complete auto-complete-clang edts)
  "Packages that use auto-complete that are no longer necessary and might
conflict.")

(defun company-mode/init-company ()
  (use-package company
    :defer t
    :init
    (progn
      (setq company-idle-delay 0.5
            company-minimum-prefix-length 2
            company-require-match nil
            company-dabbrev-ignore-case nil
            company-dabbrev-downcase nil
            company-tooltip-flip-when-above t
            company-frontends '(company-pseudo-tooltip-frontend)
            company-clang-prefix-guesser 'company-mode/more-than-prefix-guesser)
      (add-hook 'prog-mode-hook 'global-company-mode))
    :config
    (progn
      (spacemacs|diminish company-mode " Ⓒ" " C")
      ;; Set the completion key
      (if company-mode-use-tab-instead-of-enter
          (progn
            ;; have tab stand in for enter
            (define-key company-active-map (kbd "TAB") 'company-complete-selection)
            (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
            (define-key company-active-map [tab] 'company-complete-selection)
            ;;disable enter
            (define-key company-active-map [return] nil)
            (define-key company-active-map (kbd "RET") nil))
        ;; Fix integration of company and yasnippet
        (define-key company-active-map (kbd "TAB") nil)
        (define-key company-active-map (kbd "<tab>") nil)
        (define-key company-active-map [tab] nil))
      ;; key bindings
      (define-key company-active-map (kbd "C-j") 'company-select-next)
      (define-key company-active-map (kbd "C-k") 'company-select-previous)
      (define-key company-active-map (kbd "C-/") 'company-search-candidates)
      (define-key company-active-map (kbd "C-M-/") 'company-filter-candidates)
      (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)
      ;; Nicer looking faces
      (custom-set-faces
       '(company-tooltip-common
         ((t (:inherit company-tooltip :weight bold :underline nil))))
       '(company-tooltip-common-selection
         ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
      ;; Transformers
      (defun spacemacs//company-transformer-cancel (candidates)
        "Cancel completion if prefix is in the list `company-mode-completion-cancel-keywords'"
        (unless (or company-mode-use-tab-instead-of-enter
                    (member company-prefix
                            company-mode-completion-cancel-keywords))
          candidates))
      (setq company-transformers '(spacemacs//company-transformer-cancel
                                   company-sort-by-occurrence))
      ;; Backends
      (setq company-backends
            (mapcar 'spacemacs/company-backend-with-yas company-backends)))))

(defun company-mode/init-company-quickhelp ()
  (use-package company-quickhelp
    :if (display-graphic-p)
    :defer t
    :init (add-hook 'company-mode-hook 'company-quickhelp-mode)))
