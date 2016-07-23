;;; packages.el --- rest layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author:  <wwguo@hiGDP>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `rest-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `rest/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `rest/pre-init-PACKAGE' and/or
;;   `rest/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(setq rest-packages
      '((rst :location built-in)
        (rst-directives :location local)
        (rst-lists :location local)
        (rst-sphinx :location local)
        flyspell
        smartparens
        yasnippet
        ))

(defun rest/init-rst-directives ()
  (use-package rst-directives))

(defun rest/init-rst-lists ()
  (use-package rst-lists))

(defun rest/init-rst-sphinx ()
  (use-package rst-sphinx))

(defun rest/init-rst ()
  (use-package rst
    :defer t
    ;; :bind (("b" . rst-sphinx-compile))
    :config
    (progn
      (add-hook 'rst-adjust-hook 'rst-toc-update)
      (add-hook 'rst-mode-hook
                (lambda ()
                  ;; Keybindings in hybird state.
                  (define-key evil-hybrid-state-local-map (kbd "<S-return>") 'rst-list-insert)
                  (define-key evil-hybrid-state-local-map (kbd "C-c C-d") 'rst-directive-insert)
                  (define-key evil-hybrid-state-local-map (kbd "C-c C-o") 'rst-directive-option-insert)
                  ;; Keybindings in normal state.
                  (spacemacs/set-leader-keys-for-major-mode 'rst-mode
                    "c" 'rst-sphinx-compile
                    "f" 'rst-sphinx-target-open)
                  )))
    ))

(defun rest/post-init-flyspell ()
  (spell-checking/add-flyspell-hook 'rst-mode-hook))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun rest/post-init-yasnippet ()
    (add-hook 'rst-mode-hook 'spacemacs/load-yasnippet)))

(defun rest/post-init-smartparens ()
  (add-hook 'rst-mode-hook 'smartparens-mode))

;;; packages.el ends here
