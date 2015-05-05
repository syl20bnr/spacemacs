;;; funcs.el --- Better Emacs Defaults Layer key bindings File
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

(global-set-key (kbd "C-a") 'spacemacs/smart-move-beginning-of-line)

;; emacs state bindings
(define-key evil-emacs-state-map (kbd "C-o") 'evil-execute-in-normal-state)
(add-hook 'dired-mode-hook (lambda ()
                             (local-set-key (kbd "M-o") 'dired-display-file)))
