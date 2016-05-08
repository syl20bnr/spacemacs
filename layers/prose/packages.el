;;; packages.el --- prose layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Chris Ewald <chrisewald@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst prose-packages
  '(flycheck))

(defun prose/post-init-flycheck ()

  (defun prose-proselint-enabled-p ()
    "Workaround for proselint flycheck checker predicate"
    prose-proselint-enabled)

  (flycheck-define-checker proselint
    "A linter for prose."
    :command ("proselint" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
              (id (one-or-more (not (any " "))))
              (message (one-or-more not-newline)
                       (zero-or-more "\n" (any " ") (one-or-more not-newline)))
              line-end))
    :predicate prose-proselint-enabled-p)
    ;; :modes (text-mode markdown-mode gfm-mode))

  (add-to-list 'flycheck-checkers 'proselint)

  (spacemacs|add-toggle flycheck-proselint
    :status prose-proselint-enabled
    :on (progn
          (setq prose-proselint-enabled t)
          (flycheck-buffer))
    :off (progn
           (setq prose-proselint-enabled nil)
           (flycheck-buffer))
    :documentation "Better writing with proselint feedback."
    :evil-leader "tP"))

;;; packages.el ends here
