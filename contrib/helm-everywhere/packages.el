;;; packages.el --- Use helm everywhere in your emacs

;;; Commentary:
;;  Author: Daniel Wu <zw339@nyu.edu>
;;  Version: 1.0
;;
;;  This contribution is to enable helm globally and bind keys to use helm
;;  completion system as much as possible.
;;
;;  To use this contribution, add `helm-everywhere` in your `.spacemacs`:
;;
;;  (defvar dotspacemacs-configuration-layers '(helm-everywhere)
;;    "List of contribution to load."
;;  )
;;
;;  For more information about how to use a contribution in spacemacs,
;;  please see https://github.com/syl20bnr/spacemacs
;;
;;  For more information about Helm, please visit http://emacs-helm.github.io/helm/
;;
;;  Or visit http://tuhdo.github.io/helm-intro.html for an awesome
;;  introduction to helm.

;;; Code:

(defvar helm-everywhere-packages
  '(
    helm
    helm-descbinds
    helm-projectile
    )
  "List of all packages to install and/or initialize.
Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun helm-everywhere/init-helm ()
  "Initialize helm settings."
  (use-package helm
    :defer t
    :init
    (progn
        ;; use helm to list eshell history
        (add-hook 'eshell-mode-hook
                  #'(lambda ()
                      (substitute-key-definition 'eshell-list-history 'helm-eshell-history eshell-mode-map)))

        (substitute-key-definition 'find-tag 'helm-etags-select global-map)
        (setq projectile-completion-system          'helm
              helm-quick-update                      t
              helm-split-window-in-side-p            nil
              helm-buffers-fuzzy-matching            t
              helm-move-to-line-cycle-in-source      t
              helm-ff-search-library-in-sexp         t
              helm-ff-file-name-history-use-recentf  t
              helm-always-two-windows                t
              )

        (helm-descbinds-mode)
        (helm-mode 1)

        ;; show prettier mode line symbol
        (spacemacs//diminish helm-mode " Ⓗ")

        ;; enable Helm version of Projectile with replacment commands
        (require 'helm-projectile)
        (helm-projectile-on)
        )))

;;; packages.el ends here
