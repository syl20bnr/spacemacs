;;; packages.el --- Scala Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq scala-packages
  '(
    ensime
    flycheck
    ggtags
    helm-gtags
    noflet
    org
    scala-mode
    sbt-mode
    ))

(defun scala/init-ensime ()
  (use-package ensime
    :defer t
    :init
    ;; note ensime-mode is hooked to scala-mode-hook automatically by
    ;; ensime-mode via an autoload
    (progn
      (spacemacs/register-repl 'ensime 'ensime-inf-switch "ensime")
      (when scala-enable-eldoc
        (add-hook 'ensime-mode-hook 'scala/enable-eldoc))
      (add-hook 'scala-mode-hook 'scala/configure-flyspell)
      (add-hook 'scala-mode-hook 'scala/configure-ensime)
      (when scala-auto-start-ensime
        (add-hook 'scala-mode-hook 'scala/maybe-start-ensime))
      (add-to-list 'spacemacs-jump-handlers-scala-mode 'ensime-edit-definition))
    :config
    (progn
      (setq user-emacs-ensime-directory ".cache/ensime")

      (evil-define-key 'insert ensime-mode-map
        (kbd ".") 'scala/completing-dot
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
        (evil-insert-state))

      (dolist (prefix '(("mb" . "scala/build")
                        ("mc" . "scala/check")
                        ("md" . "scala/debug")
                        ("me" . "scala/errors")
                        ("mg" . "scala/goto")
                        ("mh" . "scala/docs")
                        ("mi" . "scala/inspect")
                        ("mn" . "scala/ensime")
                        ("mr" . "scala/refactor")
                        ("mt" . "scala/test")
                        ("ms" . "scala/repl")
                        ("my" . "scala/yank")))
        (spacemacs/declare-prefix-for-mode 'scala-mode (car prefix) (cdr prefix)))

      (spacemacs/set-leader-keys-for-major-mode 'scala-mode
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

        "yT"     'scala/yank-type-at-point-full-name
        "yt"     'scala/yank-type-at-point

        "z"      'ensime-expand-selection-command
        )

      ;; Don't use scala checker if ensime mode is active, since it provides
      ;; better error checking.
      (with-eval-after-load 'flycheck
        (defun scala/disable-flycheck-scala ()
          (push 'scala flycheck-disabled-checkers))

        (add-hook 'ensime-mode-hook 'scala/disable-flycheck-scala))

      ;; Enable Expand Region integration from Ensime.  Ignore load errors to
      ;; handle older Ensime versions gracefully.
      (when (configuration-layer/package-usedp 'expand-region)
        (require 'ensime-expand-region nil 'noerror)))))

(defun scala/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'scala-mode))

(defun scala/init-noflet ()
  (use-package noflet))

(defun scala/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(scala . t))))

(defun scala/init-sbt-mode ()
  (use-package sbt-mode
    :defer t
    :init (spacemacs/set-leader-keys-for-major-mode 'scala-mode
            "b." 'sbt-hydra
            "bb" 'sbt-command)))

(defun scala/init-scala-mode ()
  (use-package scala-mode
    :defer t
    :init
    (progn
      (dolist (ext '(".cfe" ".cfs" ".si" ".gen" ".lock"))
        (add-to-list 'completion-ignored-extensions ext)))
    :config
    (progn
      ;; Automatically insert asterisk in a comment when enabled
      (defun scala/newline-and-indent-with-asterisk ()
        (interactive)
        (newline-and-indent)
        (when scala-auto-insert-asterisk-in-comments
          (scala-indent:insert-asterisk-on-multiline-comment)))

      (evil-define-key 'insert scala-mode-map
        (kbd "RET") 'scala/newline-and-indent-with-asterisk)

      ;; Automatically replace arrows with unicode ones when enabled
      (defconst scala-unicode-arrows-alist
        '(("=>" . "⇒")
          ("->" . "→")
          ("<-" . "←")))

      (defun scala/replace-arrow-at-point ()
        "Replace the arrow before the point (if any) with unicode ones.
An undo boundary is inserted before doing the replacement so that
it can be undone."
        (let* ((end (point))
               (start (max (- end 2) (point-min)))
               (x (buffer-substring start end))
               (arrow (assoc x scala-unicode-arrows-alist)))
          (when arrow
            (undo-boundary)
            (backward-delete-char 2)
            (insert (cdr arrow)))))

      (defun scala/gt ()
        "Insert a `>' to the buffer. If it's part of a right arrow (`->' or `=>'),
replace it with the corresponding unicode arrow."
        (interactive)
        (insert ">")
        (scala/replace-arrow-at-point))

      (defun scala/hyphen ()
        "Insert a `-' to the buffer. If it's part of a left arrow (`<-'),
replace it with the unicode arrow."
        (interactive)
        (insert "-")
        (scala/replace-arrow-at-point))

      (when scala-use-unicode-arrows
        (define-key scala-mode-map
          (kbd ">") 'scala/gt)
        (define-key scala-mode-map
          (kbd "-") 'scala/hyphen))

      (evil-define-key 'normal scala-mode-map "J" 'spacemacs/scala-join-line)

      ;; Compatibility with `aggressive-indent'
      (setq scala-indent:align-forms t
            scala-indent:align-parameters t
            scala-indent:default-run-on-strategy scala-indent:operator-strategy))))

(defun scala/post-init-ggtags ()
  (add-hook 'scala-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun scala/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'scala-mode))
