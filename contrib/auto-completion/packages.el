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

(defvar auto-completion-packages
  '(
    company
    ac-ispell
    auto-complete
    ))

;; company-quickhelp from MELPA is not compatible with 24.3 anymore
(unless (version< emacs-version "24.4")
  (push 'company-quickhelp auto-completion-packages))

(defvar auto-completion-excluded-packages '()
  "Packages that use auto-complete that are no longer necessary and might
conflict.")

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
            company-tooltip-flip-when-above t
            company-frontends '(company-pseudo-tooltip-frontend)
            company-clang-prefix-guesser 'company-mode/more-than-prefix-guesser))
    :config
    (progn
      (spacemacs|diminish company-mode " ⓐ" " a")
      ;; Set the completion key
      (if auto-completion-use-tab-instead-of-enter
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
        "Cancel completion if prefix is in the list
`company-mode-completion-cancel-keywords'"
        (unless (and (member company-prefix company-mode-completion-cancel-keywords)
                     (not auto-completion-use-tab-instead-of-enter))
          candidates))
      (setq company-transformers '(spacemacs//company-transformer-cancel
                                   company-sort-by-occurrence)))))

(defun auto-completion/init-company-quickhelp ()
  (use-package company-quickhelp
    :if (and auto-completion-enable-company-help-tooltip
             (display-graphic-p))
    :defer t
    :init (add-hook 'company-mode-hook 'company-quickhelp-mode)))
