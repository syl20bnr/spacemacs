;;; packages.el --- Scala Layer packages File for Spacemacs
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

(setq scala-packages
  '(
    ensime
    noflet
    sbt-mode
    scala-mode2
    ))

(defun scala/init-ensime ()
  (use-package ensime
    :commands (ensime-mode)
    :init
    (progn
      (add-hook 'ensime-mode-hook 'scala/enable-eldoc)
      (add-hook 'scala-mode-hook 'scala/configure-flyspell)
      (add-hook 'scala-mode-hook 'scala/configure-ensime)
      (add-hook 'scala-mode-hook 'scala/maybe-start-ensime))
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
          (sbt-command "gen-ensime")
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

      (evil-leader/set-key-for-mode 'scala-mode
        "m/"     'ensime-search

        "mbc"     'ensime-sbt-do-compile
        "mbC"     'ensime-sbt-do-clean
        "mbi"     'ensime-sbt-switch
        "mbp"     'ensime-sbt-do-package
        "mbr"     'ensime-sbt-do-run

        "mct"     'ensime-typecheck-current-file
        "mcT"     'ensime-typecheck-all

        "mdA"     'ensime-db-attach
        "mdb"     'ensime-db-set-break
        "mdB"     'ensime-db-clear-break
        "mdC"     'ensime-db-clear-all-breaks
        "mdc"     'ensime-db-continue
        "mdd"     'ensime-db-start
        "mdi"     'ensime-db-inspect-value-at-point
        "mdl"     'ensime-db-list-locals
        "mdn"     'ensime-db-next
        "mdo"     'ensime-db-step-out
        "mdq"     'ensime-db-quit
        "mdr"     'ensime-db-run
        "mds"     'ensime-db-step
        "mdt"     'ensime-db-backtrace

        "mee"     'ensime-print-errors-at-point
        "mel"     'ensime-show-all-errors-and-warnings
        "mes"     'ensime-stacktrace-switch

        "mgg"     'ensime-edit-definition
        "mgp"     'ensime-pop-find-definition-stack
        "mgi"     'ensime-goto-impl
        "mgt"     'ensime-goto-test

        "mhh"     'ensime-show-doc-for-symbol-at-point
        "mhu"     'ensime-show-uses-of-symbol-at-point
        "mht"     'ensime-print-type-at-point

        "mii"     'ensime-inspect-type-at-point
        "miI"     'ensime-inspect-type-at-point-other-frame
        "mip"     'ensime-inspect-project-package

        "mnF"     'ensime-reload-open-files
        "mns"     'ensime
        "mnS"     'ensime-gen-and-restart

        "mrd"     'ensime-refactor-inline-local
        "mrD"     'ensime-undo-peek
        "mrf"     'ensime-format-source
        "mri"     'ensime-refactor-organize-imports
        "mrm"     'ensime-refactor-extract-method
        "mrr"     'ensime-refactor-rename
        "mrt"     'ensime-import-type-at-point
        "mrv"     'ensime-refactor-extract-local

        "mta"     'ensime-sbt-do-test
        "mtr"     'ensime-sbt-do-test-quick
        "mtt"     'ensime-sbt-do-test-only

        "msa"     'ensime-inf-load-file
        "msb"     'ensime-inf-eval-buffer
        "msB"     'ensime-inf-eval-buffer-switch
        "msi"     'ensime-inf-switch
        "msr"     'ensime-inf-eval-region
        "msR"     'ensime-inf-eval-region-switch

        "mz"      'ensime-expand-selection-command
        )

      ;; Don't use scala checker if ensime mode is active, since it provides
      ;; better error checking.
      (eval-after-load 'flycheck
        '(progn
           (defun scala/disable-flycheck () (flycheck-mode -1))
           (add-hook 'ensime-mode-hook 'scala/disable-flycheck))))))

(defun scala/init-noflet ())

(defun scala/init-sbt-mode ())

(defun scala/init-scala-mode2 ()
  (use-package scala-mode2
    :defer t
    :init
    (dolist (ext '(".cfe" ".cfs" ".si" ".gen" ".lock"))
      (add-to-list 'completion-ignored-extensions ext))
    :config
    (progn
      (evil-define-key 'normal scala-mode-map "J" 'spacemacs/scala-join-line)

      ;; Compatibility with `aggressive-indent'
      (setq scala-indent:align-forms t
            scala-indent:align-parameters t
            scala-indent:default-run-on-strategy scala-indent:operator-strategy)

      (require 'noflet)

      (defadvice scala-indent:indent-code-line (around retain-trailing-ws activate)
        "Keep trailing-whitespace when indenting."
        (noflet ((scala-lib:delete-trailing-whitespace ()))
                ad-do-it)))))
