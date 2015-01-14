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

      (defadvice scala-indent:indent-code-line (around retain-trailing-ws activate)
        "Keep trailing-whitespace when indenting."
        (noflet ((scala-lib:delete-trailing-whitespace ()))
          ad-do-it)))))
