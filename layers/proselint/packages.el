;;; packages.el --- proselint layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Chris Ewald <chrisewald@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst proselint-packages
  '(flycheck))

(defun proselint/post-init-flycheck ()

  (defun flycheck-proselint-enabled-p ()
    enable-proselint-feedback)

  (flycheck-define-checker proselint
    "A linter for prose."
    :command ("proselint" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
              (id (one-or-more (not (any " "))))
              (message (one-or-more not-newline)
                       (zero-or-more "\n" (any " ") (one-or-more not-newline)))
              line-end))
    :predicate flycheck-proselint-enabled-p)
    ;; :modes (text-mode markdown-mode gfm-mode))

  (add-to-list 'flycheck-checkers 'proselint)

  (spacemacs|add-toggle flycheck-proselint
    :status enable-proselint-feedback
    :on (progn
          (setq enable-proselint-feedback t)
          (flycheck-buffer))
    :off (progn
           (setq enable-proselint-feedback nil)
           (flycheck-buffer))
    :documentation "Better writing with proselint feedback."
    :evil-leader "tP"))

;;; packages.el ends here
