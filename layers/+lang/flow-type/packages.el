;;; packages.el --- flow-type layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2019 Sylvain Benner & Contributors
;;
;; Author: Mike Holm <coldpour@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst flow-type-packages
  '(
    flow-minor-mode
    flow-js2-mode
    company
    (company-flow :toggle (configuration-layer/package-usedp 'company))
    (flycheck-flow :toggle (configuration-layer/package-usedp 'flycheck))
    eldoc))

(defun flow-type/init-flow-minor-mode()
  (use-package flow-minor-mode
    :defer t
    :init
    (progn
      (add-hook 'js2-mode-hook #'flow-minor-enable-automatically)
      (add-hook 'rjsx-mode-hook #'flow-minor-enable-automatically))
    :config
    (progn
      ;; enable jumping with ,gg
      (add-to-list 'spacemacs-jump-handlers-js2-mode #'flow-minor-jump-to-definition)
      (add-to-list 'spacemacs-jump-handlers-rjsx-mode #'flow-minor-jump-to-definition)

      (spacemacs/declare-prefix-for-mode 'flow-minor-mode "mf" "flow" "flow type checker commands")
      (spacemacs/set-leader-keys-for-minor-mode 'flow-minor-mode
        "fb" 'xref-pop-marker-stack
        "fd" 'flow-minor-jump-to-definition
        "fc" 'flow-minor-status
        "ff" 'flow-minor-suggest
        "fo" 'flow-minor-coverage
        "ft" 'flow-minor-type-at-pos))))

(defun flow-type/init-flow-js2-mode()
  (use-package flow-js2-mode
    :defer t
    :init
    (progn
      (add-hook 'js2-mode-hook #'flow-js2-mode))))

(defun flow-type/post-init-eldoc()
  (when flow-type-enable-eldoc-type-info
    (add-hook 'js2-mode-hook 'flow-type-eldoc-hook)
    (add-hook 'rjsx-mode-hook 'flow-type-eldoc-hook)))

(defun flow-type/init-company-flow ()
  (use-package company-flow
    :defer t
    :config
    (push 'rjsx-mode company-flow-modes)))

(defun flow-type/post-init-company()
  (spacemacs|add-company-backends :backends company-flow :modes js2-mode rjsx-mode))

(defun flow-type/init-flycheck-flow()
  (with-eval-after-load 'flycheck
    (use-package flycheck-flow
      :config
      (progn
        ;; Don't run flow if there's no @flow pragma
        (custom-set-variables '(flycheck-javascript-flow-args (quote ("--respect-pragma"))))
        ;; Run flow in rjsx-mode files
        (flycheck-add-mode 'javascript-flow 'rjsx-mode)
        ;; After running js-flow, run js-eslint
        ;; doing this in the other order causes a lot of repeated errors!!!
        (flycheck-add-next-checker 'javascript-flow 'javascript-eslint)))))
