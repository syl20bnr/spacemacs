;;; packages.el --- nlinum Layer packages File
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
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
    (spacemacs|add-toggle line-numbers
      :mode nlinum-mode
      :documentation "Show the line numbers."
      :evil-leader "tn")
    :config
    (progn
      (if (or (eq dotspacemacs-line-numbers t)
              (eq dotspacemacs-line-numbers 'relative))
          (progn
            (add-hook 'prog-mode-hook 'nlinum-mode)
            (add-hook 'text-mode-hook 'nlinum-mode))
        (add-hook 'after-change-major-mode-hook 'spacemacs/nlinum-maybe-on))
      (setq nlinum-format "%4d"))))

(defun nlinum/init-nlinum-relative ()
  (use-package nlinum-relative
    :commands (nlinum-relative-toggle nlinum-relative-on)
    :init
    (progn
      (setq nlinum-relative-current-symbol ""
            nlinum-relative-redisplay-delay 0)
      (when (or (eq dotspacemacs-line-numbers 'relative)
                (and (listp dotspacemacs-line-numbers)
                     (car (spacemacs/mplist-get dotspacemacs-line-numbers
                                                :relative))))
        (nlinum-relative-setup-evil)
        (add-hook 'nlinum-mode-hook 'nlinum-relative-on))
      (spacemacs/set-leader-keys "tr" 'spacemacs/nlinum-relative-toggle))))
