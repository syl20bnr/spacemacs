;;; packages.el --- ensime layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Tor Hedin Brønner <torhedinbronner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst ensime-packages '(ensime))

(defun ensime/init-ensime ()
  (use-package ensime
    :defer t
    :commands ensime-mode
    :config
    (progn
      (setq ensime-startup-dirname (expand-file-name "ensime" spacemacs-cache-directory))

      (defun ensime/configure-keybindings (mode)
        (dolist (prefix '(("mb" . "build")
                          ("mc" . "check")
                          ("md" . "debug")
                          ("me" . "errors")
                          ("mg" . "goto")
                          ("mh" . "docs")
                          ("mi" . "inspect")
                          ("mn" . "ensime")
                          ("mr" . "refactor")
                          ("mt" . "test")
                          ("ms" . "repl")
                          ("my" . "yank")))
          (spacemacs/declare-prefix-for-mode mode (car prefix) (cdr prefix)))

        (spacemacs/set-leader-keys-for-major-mode mode
          "/"      'ensime-search
          "'"      'ensime-inf-switch

          "bc"     'ensime-sbt-do-compile
          "bC"     'ensime-sbt-do-clean
          "bi"     'ensime-sbt-switch
          "bp"     'ensime-sbt-do-package
          "br"     'ensime-sbt-do-run

          "ct"     'ensime-typecheck-current-buffer
          "cT"     'ensime-typecheck-all

          "dA"     'ensime-db-attach
          "db"     'ensime-db-set-break
          "dB"     'ensime-db-clear-break
          "dC"     'ensime-db-clear-all-breaks
          "dc"     'ensime-db-continue
          "di"     'ensime-db-inspect-value-at-point
          "dn"     'ensime-db-next
          "do"     'ensime-db-step-out
          "dq"     'ensime-db-quit
          "dr"     'ensime-db-run
          "ds"     'ensime-db-step
          "dt"     'ensime-db-backtrace

          "ee"     'ensime-print-errors-at-point
          "el"     'ensime-show-all-errors-and-warnings
          "es"     'ensime-stacktrace-switch

          "gp"     'ensime-pop-find-definition-stack
          "gi"     'ensime-goto-impl
          "gt"     'ensime-goto-test

          "hh"     'ensime-show-doc-for-symbol-at-point
          "hT"     'ensime-type-at-point-full-name
          "ht"     'ensime-type-at-point
          "hu"     'ensime-show-uses-of-symbol-at-point

          "ii"     'ensime-inspect-type-at-point
          "iI"     'ensime-inspect-type-at-point-other-frame
          "ip"     'ensime-inspect-project-package

          "nF"     'ensime-reload-open-files
          "ns"     'ensime
          "nS"     'ensime-gen-and-restart

          "ra"     'ensime-refactor-add-type-annotation
          "rd"     'ensime-refactor-diff-inline-local
          "rD"     'ensime-undo-peek
          "rf"     'ensime-format-source
          "ri"     'ensime-refactor-diff-organize-imports
          "rm"     'ensime-refactor-diff-extract-method
          "rr"     'ensime-refactor-diff-rename
          "rt"     'ensime-import-type-at-point
          "rv"     'ensime-refactor-diff-extract-local

          "ta"     'ensime-sbt-do-test-dwim
          "tr"     'ensime-sbt-do-test-quick-dwim
          "tt"     'ensime-sbt-do-test-only-dwim

          "sa"     'ensime-inf-load-file
          "sb"     'ensime-inf-eval-buffer
          "sB"     'ensime-inf-eval-buffer-switch
          "si"     'ensime-inf-switch
          "sr"     'ensime-inf-eval-region
          "sR"     'ensime-inf-eval-region-switch

          "yT"     'ensime/yank-type-at-point-full-name
          "yt"     'ensime/yank-type-at-point

          "z"      'ensime-expand-selection-command
          )
        )

      (evil-define-key 'insert ensime-mode-map
        (kbd ".") 'ensime/completing-dot
        (kbd "M-.") 'ensime-edit-definition
        (kbd "M-,") 'ensime-pop-find-definition-stack)

      (evil-define-key 'normal ensime-mode-map
        (kbd "M-.") 'ensime-edit-definition
        (kbd "M-,") 'ensime-pop-find-definition-stack)

      (evil-define-key 'normal ensime-popup-buffer-map
        (kbd "q") 'ensime-popup-buffer-quit-function)

      (evil-define-key 'normal ensime-inspector-mode-map
        (kbd "q") 'ensime-popup-buffer-quit-function)

      (evil-define-key 'normal ensime-refactor-info-map
        (kbd "q") 'spacemacs/ensime-refactor-cancel
        (kbd "c") 'spacemacs/ensime-refactor-accept
        (kbd "RET") 'spacemacs/ensime-refactor-accept)

      (evil-define-key 'normal ensime-compile-result-map
        (kbd "g") 'ensime-show-all-errors-and-warnings
        (kbd "TAB") 'forward-button
        (kbd "<backtab>") 'backward-button
        (kbd "M-n") 'forward-button
        (kbd "M-p") 'backward-button
        (kbd "n") 'forward-button
        (kbd "N") 'backward-button)

      (defun ensime-gen-and-restart()
        "Regenerate `.ensime' file and restart the ensime server."
        (interactive)
        (progn
          (sbt-command ";ensimeConfig;ensimeConfigProject")
          (ensime-shutdown)
          (ensime)))

      (defun ensime-inf-eval-buffer-switch ()
        "Send buffer content to shell and switch to it in insert mode."
        (interactive)
        (ensime-inf-eval-buffer)
        (ensime-inf-switch)
        (evil-insert-state))

      (defun ensime-inf-eval-region-switch (start end)
        "Send region content to shell and switch to it in insert mode."
        (interactive "r")
        (ensime-inf-switch)
        (ensime-inf-eval-region start end)
        (evil-insert-state)))
    ))


;;; packages.el ends here
