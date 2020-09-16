;;; packages.el --- Better Emacs Defaults Layer functions File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Thomas de Beauchêne <thomas.de.beauchene@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst better-defaults-packages
  '(
    mwim
    unfill
    )
  "The list of Lisp packages required by the mwim layer.")

(defun better-defaults/init-mwim ()
  (use-package mwim
    :defer t
    :init
    (progn
      (if better-defaults-move-to-beginning-of-code-first
	  (global-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line)
	(global-set-key (kbd "C-a") 'mwim-beginning-of-line-or-code))

      (if better-defaults-move-to-end-of-code-first
	  (global-set-key (kbd "C-e") 'mwim-end-of-code-or-line)
	(global-set-key (kbd "C-e") 'mwim-end-of-line-or-code)))))

(defun better-defaults/init-unfill ()
  (use-package unfill
    :defer t
    :commands (unfill-region unfill-paragraph unfill-toggle)
    :init
    (global-set-key [remap fill-paragraph] #'unfill-toggle)))
