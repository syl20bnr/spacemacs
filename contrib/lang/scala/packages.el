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

(defvar scala-packages
  '(
    ensime
    sbt-mode
    scala-mode2
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun scala/init-ensime ()
  (use-package ensime
    :commands (ensime-mode)
    :init
    (progn
      (add-hook 'scala-mode-hook 'scala/configure-flyspell)
      (add-hook 'scala-mode-hook 'scala/configure-ensime))
    :config
    (progn
      (evil-define-key 'insert ensime-mode-map (kbd ".") 'scala/completing-dot)

      (evil-define-key 'normal ensime-popup-buffer-map
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

      (defun ensime-gen-and-reload()
        (interactive)
        (progn
          (sbt-command "gen-ensime")
          (ensime-shutdown)
          (ensime))
        )
      
      (evil-leader/set-key-for-mode 'scala-mode
        "mg"     'ensime-edit-definition

        "m."      'ensime-gen-and-reload
        "m,"      'ensime
        "mri"     'ensime-refactor-inline-local
        "mrl"     'ensime-refactor-extract-local
        "mrm"     'ensime-refactor-extract-method
        "mro"     'ensime-refactor-organize-imports
        "mrr"     'ensime-refactor-rename
        "mrt"     'ensime-import-type-at-point

        "mbS"     'ensime-stacktrace-switch
        "mbT"     'ensime-sbt-do-test
        "mbc"     'ensime-sbt-do-compile
        "mbn"     'ensime-sbt-do-clean
        "mbo"     'ensime-sbt-do-test-only
        "mbp"     'ensime-sbt-do-package
        "mbr"     'ensime-sbt-do-run
        "mbs"     'ensime-sbt-switch
        "mbt"     'ensime-sbt-do-test-quick

        "mda"     'ensime-db-clear-all-breaks
        "mdb"     'ensime-db-set-break
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
        "mdu"     'ensime-db-clear-break

        "mti"     'ensime-goto-impl
        "mtt"     'ensime-goto-test

        "mca"     'ensime-typecheck-all
        "mcc"     'ensime-typecheck-current-file
        "mce"     'ensime-show-all-errors-and-warnings
        "mcr"     'ensime-reload-open-files

        "mvR"     'ensime-inf-eval-region
        "mv."     'ensime-expand-selection-command
        "mvb"     'ensime-inf-eval-buffer
        "mvd"     'ensime-show-doc-for-symbol-at-point
        "mve"     'ensime-print-errors-at-point
        "mvf"     'ensime-format-source
        "mvi"     'ensime-inspect-type-at-point
        "mvI"     'ensime-inspect-type-at-point-other-frame
        "mvl"     'ensime-inf-load-file
        "mvo"     'ensime-inspect-project-package
        "mvp"     'ensime-inspect-package-at-point
        "mvr"     'ensime-show-uses-of-symbol-at-point
        "mvs"     'ensime-sbt-switch
        "mvt"     'ensime-print-type-at-point
        "mvu"     'ensime-undo-peek
        "mvv"     'ensime-search
        "mvx"     'ensime-scalex
        "mvz"     'ensime-inf-switch
        )

      ;; Don't use scala checker if ensime mode is active, since it provides
      ;; better error checking.
      (eval-after-load 'flycheck
        '(progn
           (defun scala/disable-flycheck () (flycheck-mode -1))
           (add-hook 'ensime-mode-hook 'scala/disable-flycheck))))))

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
      (custom-set-variables
       '(scala-indent:align-forms t)
       '(scala-indent:align-parameters t)
       '(scala-indent:default-run-on-strategy scala-indent:operator-strategy))

      (require 'noflet)

      (defadvice scala-indent:indent-code-line (around retain-trailing-ws activate)
        "Keep trailing-whitespace when indenting."
        (noflet ((scala-lib:delete-trailing-whitespace ()))
          ad-do-it)))))
