(defvar company-mode-packages
  '(
    company
    company-tern
    company-c-headers
    company-quickhelp
    ))

(defvar company-mode-excluded-packages
  '(auto-complete ac-ispell tern-auto-complete auto-complete-clang edts)
  "Packages that use auto-complete that are no longer necessary and might conflict.")

(defvar company-mode/completion-cancel-keywords '("do" "then" "begin" "case")
  "Keywords on which to cancel completion so that you can use RET to complet without blocking common line endings.")

(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defvar company-mode/use-tab-instead-of-enter-to-complete nil
  "use tab instead of enter for completion in company-mode")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(defun company-mode/init-company ()
  (use-package company
    :config
    (progn
      ;; this isn't needed if we use tab instead of enter
      (if (not company-mode/use-tab-instead-of-enter-to-complete)
          (progn
            (defun company-mode/keyword-cancel-transformer (candidates)
              "company frontend that cancels completion when a keyword is typed
so that you don't have 'do' completed to 'downcase' in Ruby"
              (if (member company-prefix company-mode/completion-cancel-keywords) '() candidates))
            (setq
             company-transformers '(company-mode/keyword-cancel-transformer company-sort-by-occurrence))
            )
          (setq company-transformers '(company-sort-by-occurrence)) ;else
          )

      (setq company-idle-delay 0.0
            company-minimum-prefix-length 2
            company-require-match nil
            company-dabbrev-ignore-case nil
            company-dabbrev-downcase nil
            company-tooltip-flip-when-above t
            company-frontends '(company-pseudo-tooltip-frontend)
            company-clang-prefix-guesser 'company-mode/more-than-prefix-guesser)

      (global-company-mode 1)

      ;; set completion key
      (company-mode/set-completion-key)

      (company-mode/setup-keybindings)

      (add-hook 'markdown-mode-hook '(lambda () (company-mode -1)))

      ;; The default common face is a really ugly underlined thing with a different background.
      (custom-set-faces
       '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
       '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))

      (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

      (spacemacs|diminish company-mode " Ⓒ" " C"))))

(defun company-mode/init-company-c-headers ()
  (use-package company-c-headers
    :defer t
    :init
    (add-to-list 'company-backends 'company-c-headers)))

(defun company-mode/init-company-tern ()
  (use-package company-tern
    :defer t
    :init
    (add-to-list 'company-backends (company-mode/backend-with-yas 'company-tern))))

(defun company-mode/setup-keybindings ()
  (progn
    (define-key company-active-map (kbd "C-j") 'company-select-next)
    (define-key company-active-map (kbd "C-k") 'company-select-previous)
    (define-key company-active-map (kbd "C-/") 'company-search-candidates)
    (define-key company-active-map (kbd "C-M-/") 'company-filter-candidates)
    (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)
    ))

(defun company-mode/set-completion-key ()
  (if company-mode/use-tab-instead-of-enter-to-complete
      (progn
        (define-key company-active-map (kbd "TAB") 'company-complete-selection) ;have tab stand in for enter
        (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
        (define-key company-active-map [tab] 'company-complete-selection)
        (define-key company-active-map [return] nil) ;disable enter
        (define-key company-active-map (kbd "RET") nil)
        )
      (progn
        ;; Fix integration of company and yasnippet
        (define-key company-active-map (kbd "TAB") nil)
        (define-key company-active-map (kbd "<tab>") nil)
        (define-key company-active-map [tab] nil)
        )))

(defun company-mode/init-company-quickhelp ()
  (use-package company-quickhelp
    :defer t
    :init
    (progn
      (when (display-graphic-p) (company-quickhelp-mode 1)))))
