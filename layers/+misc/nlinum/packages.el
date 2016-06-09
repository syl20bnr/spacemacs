;;; packages.el --- nlinum Layer packages File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst nlinum-packages
  '(
    (linum :excluded t)
    (linum-relative :excluded t)
    nlinum
    nlinum-relative
    ))

(defun nlinum/init-nlinum ()
  (use-package nlinum
    :init
    (progn
      (when dotspacemacs-line-numbers
        (add-hook 'prog-mode-hook 'nlinum-mode)
        (add-hook 'text-mode-hook 'nlinum-mode))
      (setq nlinum-format "%4d")
      (spacemacs|add-toggle line-numbers
        :mode nlinum-mode
        :documentation "Show the line numbers."
        :evil-leader "tn"))))

(defun nlinum/init-nlinum-relative ()
  (use-package nlinum-relative
    :commands (nlinum-relative-toggle nlinum-relative-on)
    :init
    (progn
      (setq nlinum-relative-current-symbol ""
            nlinum-relative-redisplay-delay 0)
      (when (eq dotspacemacs-line-numbers 'relative)
        (nlinum-relative-setup-evil)
        (add-hook 'nlinum-mode-hook 'nlinum-relative-on))
      (spacemacs/set-leader-keys "tr" 'nlinum-relative-toggle))))
