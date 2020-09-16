;;; packages.el --- coq layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Jeremy Bi <bixuanxbi@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq coq-packages
      '(
        (company-coq :requires company)
        proof-general
        smartparens
        vi-tilde-fringe
        ))

(defun coq/init-company-coq ()
  (use-package company-coq
    :defer t
    :init
    (progn
      (add-hook 'coq-mode-hook #'company-coq-mode)
      (add-to-list 'spacemacs-jump-handlers-coq-mode
                   'company-coq-jump-to-definition)
      (setq company-coq-disabled-features '(hello)))
    :config
    (progn
      (spacemacs|hide-lighter company-coq-mode)
      (dolist (prefix '(("mi" . "coq/insert")
                        ("mh" . "coq/document")))
        (spacemacs/declare-prefix-for-mode
          'coq-mode
          (car prefix) (cdr prefix)))
      (spacemacs/set-leader-keys-for-major-mode 'coq-mode
        "il" 'company-coq-lemma-from-goal
        "im" 'company-coq-insert-match-construct
        "ao" 'company-coq-occur
        "he" 'company-coq-document-error
        "hE" 'company-coq-browse-error-messages
        "hh" 'company-coq-doc))))

(defun coq/init-proof-general ()
  (use-package proof-site
    :mode ("\\.v\\'" . coq-mode)
    :defer t
    :init
    (progn
      (setq coq/proof-general-load-path
            (concat (configuration-layer/get-elpa-package-install-directory
                     'proof-general) "generic")
            proof-three-window-mode-policy 'hybrid
            proof-script-fly-past-comments t
            proof-splash-seen t)
      (add-to-list 'load-path coq/proof-general-load-path))
    :config
    (progn
      (spacemacs|hide-lighter holes-mode)
      (spacemacs|hide-lighter proof-active-buffer-fake-minor-mode)
      ;; key bindings
      (dolist (prefix '(("ml" . "pg/layout")
                        ("mp" . "pg/prover")
                        ("ma" . "pg/ask-prover")
                        ("mai" . "show-implicits")
                        ("mg" . "pg/goto")))
        (spacemacs/declare-prefix-for-mode
         'coq-mode
         (car prefix) (cdr prefix)))
      (spacemacs/set-leader-keys-for-major-mode 'coq-mode
        ;; Basic proof management
        "]" 'proof-assert-next-command-interactive
        "[" 'proof-undo-last-successful-command
        "." 'proof-goto-point
        ;; Layout
        "lc" 'pg-response-clear-displays
        "ll" 'proof-layout-windows
        "lp" 'proof-prf
        ;; Prover Interaction
        "pi" 'proof-interrupt-process
        "pp" 'proof-process-buffer
        "pq" 'proof-shell-exit
        "pr" 'proof-retract-buffer
        ;; Prover queries ('ask prover')
        "aa" 'coq-Print
        "aA" 'coq-Print-with-all
        "ab" 'coq-About
        "aB" 'coq-About-with-all
        "ac" 'coq-Check
        "aC" 'coq-Check-show-all
        "af" 'proof-find-theorems
        "aib" 'coq-About-with-implicits
        "aic" 'coq-Check-show-implicits
        "aii" 'coq-Print-with-implicits
        ;; Moving the point (goto)
        "ge" 'proof-goto-command-end
        "gl" 'proof-goto-end-of-locked
        "gs" 'proof-goto-command-start
        ;; Insertions
        "ic" 'coq-insert-command
        "ie" 'coq-end-Section
        "ii" 'coq-insert-intros
        "ir" 'coq-insert-requires
        "is" 'coq-insert-section-or-module
        "it" 'coq-insert-tactic
        "iT" 'coq-insert-tactical
        ;; Options
        "Te" 'proof-electric-terminator-toggle))))

(defun coq/post-init-smartparens ()
  (spacemacs/add-to-hooks (if dotspacemacs-smartparens-strict-mode
                              'smartparens-strict-mode
                            'smartparens-mode)
                          '(coq-mode-hook)))

(defun coq/post-init-vi-tilde-fringe ()
  (spacemacs/add-to-hooks 'spacemacs/disable-vi-tilde-fringe
                          '(coq-response-mode-hook
                            coq-goals-mode-hook)))
