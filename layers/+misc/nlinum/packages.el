;;; packages.el --- nlinum Layer packages File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(if (version< emacs-version "26")
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
          (spacemacs|add-toggle line-numbers
            :mode nlinum-mode
            :documentation "Show the line numbers."
            :evil-leader "tn")
          :config
          (progn
            (if (or (eq dotspacemacs-line-numbers t)
                    (eq dotspacemacs-line-numbers 'relative)
                    (eq dotspacemacs-line-numbers 'visual))
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
            (when (or (spacemacs/visual-line-numbers-p)
                      (spacemacs/relative-line-numbers-p))
              (nlinum-relative-setup-evil)
              (add-hook 'nlinum-mode-hook 'nlinum-relative-on))
            (spacemacs/set-leader-keys "tr" 'spacemacs/nlinum-relative-toggle)))))

  (defconst nlinum-packages nil)
  (when (configuration-layer/layer-usedp 'nlinum)
    (spacemacs-buffer/warning (concat "nlinum layer is deprecated for Emacs 26.1 and above."
                                      " You can safely remove it from your dotfile."))))
