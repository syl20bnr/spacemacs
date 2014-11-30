(defvar company-mode-packages
  '(
    company
    company-tern
    ))

(defvar company-mode-excluded-packages
  '(auto-complete ac-ispell tern-auto-complete auto-complete-clang ensime edts)
  "Packages that use auto-complete that are no longer necessary and might conflict.")

(defvar company-mode/completion-cancel-keywords '("do" "then" "begin" "case")
  "Keywords on which to cancel completion so that you can use RET to complet without blocking common line endings.")

(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(defun company-mode/init-company ()
  (use-package company
    :config
    (progn
      (defun company-mode/keyword-cancel-transformer (candidates)
        "company frontend that cancels completion when a keyword is typed
so that you don't have 'do' completed to 'downcase' in Ruby"
        (if (member company-prefix company-mode/completion-cancel-keywords) '() candidates))

      (setq company-idle-delay 0.0
            company-minimum-prefix-length 2
            company-require-match nil
            company-transformers '(company-mode/keyword-cancel-transformer company-sort-by-occurrence)
            company-dabbrev-ignore-case nil
            company-dabbrev-downcase nil
            company-tooltip-flip-when-above t
            company-frontends '(company-pseudo-tooltip-frontend)
            company-clang-prefix-guesser 'company-mode/more-than-prefix-guesser)

      (global-company-mode 1)

      ;; Fix integration of company and yasnippet
      (define-key company-active-map (kbd "TAB") nil)
      (define-key company-active-map (kbd "<tab>") nil)
      (define-key company-active-map [tab] nil)

      (add-hook 'markdown-mode-hook '(lambda () (company-mode -1)))

      ;; The default common face is a really ugly underlined thing with a different background.
      (custom-set-faces
       '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
       '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))

      (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

      (spacemacs//diminish company-mode " Ⓒ"))))

(defun company-mode/init-company-tern ()
  (use-package company-tern
    :defer t
    :init
    (add-to-list 'company-backends (company-mode/backend-with-yas 'company-tern))))
