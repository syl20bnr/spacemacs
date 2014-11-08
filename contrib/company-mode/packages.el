(defvar company-mode-packages
  '(
    company
    company-tern
    ))

(defun company-mode/init-company ()
  (use-package company
    :config
    (progn
      (setq company-idle-delay 0.0
            company-minimum-prefix-length 2
            company-require-match nil
            company-transformers '(company-sort-by-occurrence)
            company-dabbrev-ignore-case nil
            company-dabbrev-downcase nil
            company-tooltip-flip-when-above t
            company-frontends '(company-pseudo-tooltip-frontend)
            company-clang-prefix-guesser 'company-mode/more-than-prefix-guesser)

      (global-company-mode 1)

      ; Fix integration of company and yasnippet
      (define-key company-active-map (kbd "TAB") nil)
      (define-key company-active-map (kbd "<tab>") nil)
      (define-key company-active-map [tab] nil)

      (add-hook 'markdown-mode-hook '(lambda () (company-mode -1)))

      ; The default common face is a really ugly underlined thing with a different background.
      (custom-set-faces
       '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
       '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))

      (spacemacs//diminish company-mode " Ⓒ"))))

(defun company-mode/init-company-tern ()
   (use-package company-tern
     :defer t
     :config
     (add-to-list 'company-backends 'company-tern)))
