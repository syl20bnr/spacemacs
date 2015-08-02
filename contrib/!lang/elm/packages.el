;;; packages.el --- elm Layer packages File for Spacemacs
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

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.

(setq elm-packages
    '(
      company
      elm-mode
      flycheck
      flycheck-elm
      popwin
      smartparens
      ))

;; List of packages to exclude.
(setq elm-excluded-packages '())

(defun elm/init-flycheck-elm ()
  "Initialize flycheck-elm"
  (use-package flycheck-elm
    :if (configuration-layer/layer-usedp 'syntax-checking)
    :defer t
    :init (add-hook 'flycheck-mode-hook #'flycheck-elm-setup)))

(defun elm/init-elm-mode ()
  "Initialize elm-mode"
  (use-package elm-mode
    :mode ("\\.elm\\'" . elm-mode)
    :init
    (progn

      (defun spacemacs/init-elm-mode ()
        "Disable electric-indent-mode and let indentation cycling feature work"
        (if (fboundp 'electric-indent-local-mode)
            (electric-indent-local-mode -1)))

      (add-hook 'elm-mode-hook 'spacemacs/init-elm-mode)

      (when (configuration-layer/layer-usedp 'auto-completion)
        (defun elm/post-init-company ()
          (spacemacs|add-company-hook elm-mode)
          (add-hook 'elm-mode-hook #'elm-oracle-setup-completion))))
    :config
    (progn
      (push "\\*elm\\*" spacemacs-useful-buffers-regexp)

      (defun spacemacs/elm-compile-buffer-output ()
        (interactive)
        (let* ((fname (format "%s.js" (downcase (file-name-base (buffer-file-name))))))
          (elm-compile--file (elm--buffer-local-file-name) fname)))

      (evil-leader/set-key-for-mode 'elm-mode
        ;; make
        "mcb" 'elm-compile-buffer
        "mcB" 'spacemacs/elm-compile-buffer-output
        "mcm" 'elm-compile-main

        ;; oracle
        "mlt" 'elm-oracle-type-at-point

        ;; repl
        "mrl" 'load-elm-repl
        "mrp" 'push-elm-repl
        "mrP" 'push-decl-elm-repl

        ;; reactor
        "mRn" 'elm-preview-buffer
        "mRm" 'elm-preview-main

        ;; package
        "mpi" 'elm-import
        "mpc" 'elm-package-catalog
        "mpd" 'elm-documentation-lookup
        )

      (evilify elm-package-mode elm-package-mode-map
        "g" 'elm-package-refresh
        "n" 'elm-package-next
        "p" 'elm-package-prev
        "v" 'elm-package-view
        "m" 'elm-package-mark
        "u" 'elm-package-unmark
        "x" 'elm-package-install
        "q" 'quit-window
        )
      )
    )
  )

(defun elm/pre-init-popwin ()
  (spacemacs|use-package-add-hook popwin
    :post-config
    (push '("*elm*" :tail t :noselect t) popwin:special-display-config)
    (push '("*elm-make*" :tail t :noselect t) popwin:special-display-config)
    ))

(defun elm/post-init-smartparens ()
  (if dotspacemacs-smartparens-strict-mode
      (add-hook 'elm-mode-hook #'smartparens-strict-mode)
    (add-hook 'elm-mode-hook #'smartparens-mode)))
