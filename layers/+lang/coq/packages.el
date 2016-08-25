;;; packages.el --- coq layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Jeremy Bi <bixuanxbi@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(setq coq-packages
      '(
        (company-coq :toggle (configuration-layer/package-usedp 'company))
        (proof-general :location (recipe
                                  :fetcher github
                                  :repo "ProofGeneral/PG"
                                  :files ("*")))
        smartparens
        vi-tilde-fringe
        ))

(defun coq/init-company-coq ()
  (use-package company-coq
    :defer t
    :init (add-hook 'coq-mode-hook #'company-coq-mode)
    :config
    (progn
      (spacemacs/declare-prefix-for-mode
       'coq-mode
       "mi" "coq/insert")
      (spacemacs/set-leader-keys-for-major-mode 'coq-mode
        "il" 'company-coq-lemma-from-goal
        "im" 'company-coq-insert-match-construct
        "ao" 'company-coq-occur
        "gd" 'company-coq-jump-to-definition))))

(defun coq/init-proof-general ()
  (use-package proof-site
    :mode ("\\.v\\'" . coq-mode)
    :defer t
    :init
    (progn
      (setq coq/proof-general-load-path
            (concat (configuration-layer/get-elpa-package-install-directory
                     'proof-general) "generic"))
      (add-to-list 'load-path coq/proof-general-load-path))
    :config
    (progn
      (spacemacs|diminish company-coq-mode)
      (spacemacs|diminish holes-mode)
      (spacemacs|diminish hs-minor-mode)
      (spacemacs|diminish outline-minor-mode)
      (spacemacs|diminish proof-active-buffer-fake-minor-mode)
      (spacemacs|diminish yas-minor-mode " ⓨ" " y")

      (setq company-coq-disabled-features '(hello)
            proof-three-window-mode-policy 'hybrid
            proof-script-fly-past-comments t
            proof-splash-seen t)

      (dolist (prefix '(("ml" . "pg/layout")
                        ("mp" . "pg/prover")
                        ("ma" . "pg/ask-prover")
                        ("mai" . "show-implicits")
                        ("man" . "show-all") ; n is for notation
                        ("mg" . "pg/goto")))
        (spacemacs/declare-prefix-for-mode
         'coq-mode
         (car prefix) (cdr prefix)))

      ;; key bindings
      (spacemacs/set-leader-keys-for-major-mode 'coq-mode
        ;; Basic proof management
        "]" 'proof-assert-next-command-interactive
        "[" 'proof-undo-last-successful-command
        "." 'proof-goto-point
        ;; Layout
        "ll" 'proof-layout-windows
        "lc" 'pg-response-clear-displays
        "lp" 'proof-prf
        ;; Prover Interaction
        "px" 'proof-shell-exit
        "pc" 'proof-interrupt-process
        "pr" 'proof-retract-buffer
        "pb" 'proof-process-buffer
        ;; Prover queries ('ask prover')
        "af" 'proof-find-theorems
        "ap" 'coq-Print
        "ac" 'coq-Check
        "ab" 'coq-About
        "aip" 'coq-Print-with-implicits
        "aic" 'coq-Check-show-implicits
        "aib" 'coq-About-with-implicits
        "anp" 'coq-Print-with-all
        "anc" 'coq-Check-show-all
        "anb" 'coq-About-with-all
        ;; Moving the point (goto)
        "g." 'proof-goto-end-of-locked
        "ga" 'proof-goto-command-start
        "ge" 'proof-goto-command-end
        ;; Insertions
        "ie" 'coq-end-Section))))

(defun coq/post-init-smartparens ()
  (spacemacs/add-to-hooks
   (if dotspacemacs-smartparens-strict-mode
       'smartparens-strict-mode
     'smartparens-mode)
   '(coq-mode-hook)))

(defun coq/post-init-vi-tilde-fringe ()
  (spacemacs/add-to-hooks 'spacemacs/disable-vi-tilde-fringe
                          '(coq-response-mode-hook
                            coq-goals-mode-hook)))


;;; packages.el ends here
