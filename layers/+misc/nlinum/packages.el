;;; packages.el --- nlinum Layer packages File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(if (version< e-macs-version "26")
    (progn
      (defconst nlinum-packages
        '(
          (linum :excluded t)
          (linum-relative :excluded t)
          (display-line-numbers :excluded t)
          nlinum
          nlinum-relative
          ))

      (defun nlinum/init-nlinum ()
        (use-package nlinum
          :init
          (space-macs|add-toggle line-numbers
            :mode nlinum-mode
            :documentation "Show the line numbers."
            :evil-leader "tn")
          :config
          (progn
            (if (or (eq dotspace-macs-line-numbers t)
                    (eq dotspace-macs-line-numbers 'relative)
                    (eq dotspace-macs-line-numbers 'visual))
                (progn
                  (add-hook 'prog-mode-hook 'nlinum-mode)
                  (add-hook 'text-mode-hook 'nlinum-mode))
              (add-hook 'after-change-major-mode-hook 'space-macs/nlinum-maybe-on))
            (setq nlinum-format "%4d"))))

      (defun nlinum/init-nlinum-relative ()
        (use-package nlinum-relative
          :commands (nlinum-relative-toggle nlinum-relative-on)
          :init
          (progn
            (setq nlinum-relative-current-symbol ""
                  nlinum-relative-redisplay-delay 0)
            (when (or (space-macs/visual-line-numbers-p)
                      (space-macs/relative-line-numbers-p))
              (nlinum-relative-setup-evil)
              (add-hook 'nlinum-mode-hook 'nlinum-relative-on))
            (space-macs/set-leader-keys "tr" 'space-macs/nlinum-relative-toggle)))))
  ;;  e-macs version 26 and above
  (defconst nlinum-packages nil))


