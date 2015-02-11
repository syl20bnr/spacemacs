;;; packages.el --- Finance Layer packages File for Spacemacs
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

(defvar markdown-packages
  '(
    markdown-mode
    markdown-toc
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun markdown/init-markdown-mode ()
  (use-package markdown-mode
    :mode ("\\.md" . markdown-mode)
    :defer t
    :init
    (eval-after-load 'smartparens
      '(add-hook 'markdown-mode-hook 'smartparens-mode))
    :config
    ;; Don't do terrible things with Github code blocks (```)
    (when (fboundp 'sp-local-pair)
      (sp-local-pair 'markdown-mode "`" nil :actions '(:rem autoskip))
      (sp-local-pair 'markdown-mode "'" nil :actions nil))))

(defun markdown/init-markdown-toc ()
  (use-package markdown-toc
    :defer t))
