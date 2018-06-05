;;; packages.el --- flow-type layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Jonathan del Strother <jdelStrother@gmail.com>
;;         Seong Yong-ju <sei40kr@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst flow-type-packages
  '(company
    (company-flow :toggle (configuration-layer/package-used-p 'company))
    eldoc
    (flow-js2-mode :location (recipe :fetcher github
                                     :repo "Fuco1/flow-js2-mode"))
    flow-minor-mode
    (flycheck-flow :toggle (configuration-layer/package-used-p 'flycheck))
    js2-mode
    prettier-js
    rjsx-mode))

(defun flow-type/post-init-company ()
  (spacemacs|add-company-backends
    :backends company-flow
    :modes js2-mode react-mode))

(defun flow-type/init-company-flow ()
  (use-package company-flow
    :defer t))

(defun flow-type/post-init-eldoc ()
  (if (configuration-layer/package-used-p 'js2-mode)
    (add-hook 'js2-mode-hook #'flow-type/enable-eldoc))
  (if (configuration-layer/layer-used-p 'react)
    (add-hook 'react-mode-hook #'flow-type/enable-eldoc)))

(defun flow-type/init-flow-js2-mode ()
  (use-package flow-js2-mode
    :defer t
    :init
    (progn
      (add-hook 'js2-mode-hook #'flow-type/activate-flow-js2-mode)
      (add-hook 'rjsx-mode-hook #'flow-type/activate-flow-js2-mode))
    :config
    (spacemacs|hide-lighter flow-js2-mode)))

(defun flow-type/init-flow-minor-mode ()
  (use-package flow-minor-mode
    :defer t
    :config
    (progn
      (spacemacs/declare-prefix-for-mode 'flow-minor-mode
        "mf" "flow" "flow type checker commands")
      (spacemacs/set-leader-keys-for-minor-mode 'flow-minor-mode
        "gf" #'flow-minor-jump-to-definition
        "fc" #'flow-minor-status
        "ff" #'flow-minor-suggest
        "fo" #'flow-minor-coverage
        "ft" #'flow-minor-type-at-pos))))

(defun flow-type/init-flycheck-flow ()
  (use-package flycheck-flow
    :config
    (progn
      ;; Don't run flow if there's no @flow pragma
      (custom-set-variables
       '(flycheck-javascript-flow-args (quote ("--respect-pragma"))))
      ;; Run flow in react-mode files
      (flycheck-add-mode 'javascript-flow 'react-mode)
      ;; After running js-flow, run js-eslint
      (flycheck-add-next-checker 'javascript-flow 'javascript-eslint))))

(defun flow-type/post-init-js2-mode ()
  (add-hook 'js2-mode-hook #'flow-type/may-activate-flow-minor-mode t))

(defun flow-type/post-init-prettier-js ()
  (eval-after-load 'flow-minor-mode
    '(add-hook 'flow-minor-mode-hook
              #'(lambda ()
                  (set (make-local-variable 'prettier-js-args) '("--parser" "flow"))))))

(defun flow-type/post-init-rjsx-mode ()
  (if (configuration-layer/layer-used-p 'react)
      (add-hook 'react-mode-hook #'flow-type/may-activate-flow-minor-mode t)))
