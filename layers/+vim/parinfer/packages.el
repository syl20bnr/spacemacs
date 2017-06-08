;;; packages.el --- parinfer layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: DogLooksGood <DogLooksGood@rMBP>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst parinfer-packages
  '(parinfer))

(defun parinfer/init-parinfer ()
  (use-package parinfer
    :defer t
    :diminish parinfer-mode
    :init
    (progn
      (add-hook 'emacs-lisp-mode-hook 'parinfer-mode)
      (add-hook 'clojure-mode-hook 'parinfer-mode)
      (add-hook 'common-lisp-mode-hook 'parinfer-mode)
      (add-hook 'scheme-mode-hook 'parinfer-mode)
      (add-hook 'lisp-mode-hook 'parinfer-mode)
      (spacemacs|add-toggle parinfer-indent
        :evil-leader "tP"
        :documentation "Enable Parinfer Indent Mode."
        :if (bound-and-true-p parinfer-mode)
        :status (eq parinfer--mode 'indent)
        :on (parinfer-toggle-mode)
        :off (parinfer-toggle-mode))
      (setq parinfer-extensions '(defaults pretty-parens evil smart-yank)))))

;;; packages.el ends here
