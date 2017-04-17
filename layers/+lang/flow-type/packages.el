;;; packages.el --- flow-type layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Jonathan del Strother <jdelStrother@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst flow-type-packages
  '((flow-minor-mode :location (recipe :fetcher github :repo "jdelStrother/flow-minor-mode"))
    company
    (company-flow :toggle (configuration-layer/package-usedp 'company))
    (flycheck-flow :toggle (configuration-layer/package-usedp 'flycheck))
    eldoc
    js2-mode
    web-mode
    add-node-modules-path))

(defun flow-type/init-flow-minor-mode()
  (use-package flow-minor-mode)
  (spacemacs/declare-prefix-for-mode 'flow-minor-mode "mf" "flow" "flow type checker commands")
  (spacemacs/set-leader-keys-for-minor-mode 'flow-minor-mode
    "fd" 'flow-minor-jump-to-definition
    "fc" 'flow-minor-status
    "ft" 'flow-minor-type-at-pos
    ))

(defun flow-type/init-add-node-modules-path()
  (use-package add-node-modules-path))

(defun flow-type/post-init-eldoc()
  (when (configuration-layer/package-usedp 'js2-mode)
    (push 'flow-type/enable-eldoc js2-mode-hook))
  (when (configuration-layer/layer-usedp 'react)
    (push 'flow-type/enable-eldoc react-mode-hook)))

(defun flow-type/post-init-company()
  (spacemacs|add-company-backends :backends company-flow :modes js2-mode react-mode))

(defun flow-type/post-init-js2-mode()
  (add-hook 'js2-mode-hook 'flow-minor-enable-automatically)
  (add-hook 'js2-mode-hook 'add-node-modules-path)
  (add-to-list 'spacemacs-jump-handlers-js2-mode 'flow-minor-jump-to-definition))

(defun flow-type/post-init-web-mode()
  (when (configuration-layer/layer-usedp 'react)
    (add-hook 'react-mode-hook 'flow-minor-enable-automatically)
    (add-hook 'react-mode-hook 'add-node-modules-path)
    (add-to-list 'spacemacs-jump-handlers-react-mode 'flow-minor-jump-to-definition)
    ))

(defun flow-type/init-company-flow ()
  (use-package company-flow
    :defer t
    :config
    (when (configuration-layer/layer-usedp 'react)
      (push 'react-mode company-flow-modes))))

(defun flow-type/init-flycheck-flow()
  (with-eval-after-load 'flycheck
    (use-package flycheck-flow
      :config
      (progn
        ;; Don't run flow if there's no @flow pragma
        (custom-set-variables '(flycheck-javascript-flow-args (quote ("--respect-pragma"))))
        ;; Run flow in react-mode files
        (flycheck-add-mode 'javascript-flow 'react-mode)
        ;; After running js-flow, run js-eslint
        (flycheck-add-next-checker 'javascript-flow 'javascript-eslint)
      ))))
