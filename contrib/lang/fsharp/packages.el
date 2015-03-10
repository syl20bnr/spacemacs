;;; packages.el --- fsharp Layer packages File for Spacemacs

(defvar fsharp-packages '(fsharp-mode))
(defun fsharp/init-fsharp-mode ()
  (use-package fsharp-mode
    :defer t
    :config
    (progn
      (setq fsharp-doc-idle-delay .2)
      (setq fsharp-build-command "/usr/local/bin/xbuild")
      ;;;;;;;;; Keybindings ;;;;;;;;;;
      (evil-leader/set-key-for-mode 'fsharp-mode
        ;; Compile
        "mcc" 'compile
        "mer" 'fsharp-eval-region
        "mep" 'fsharp-eval-phrase
        "mef" 'fsharp-load-buffer-file
        "mst" 'fsharp-ac/show-tooltip-at-point
        "mgd" 'fsharp-ac/gotodefn-at-point
        "mss" 'fsharp-show-subshell
        "mee" 'fsharp-run-executable-file
        "mfa" 'fsharp-find-alternate-file
        "men" 'next-error
        "mep" 'previous-error
    )
 )))
