;;; packages.el --- unicode-fonts layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Aaron Jensen <aaronjensen@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst unicode-fonts-packages
  '(unicode-fonts
    persistent-soft
    (ligature :location (recipe
                         :fetcher github
                         :repo "mickeynp/ligature.el")
              :toggle (and (> emacs-major-version 26) unicode-fonts-enable-ligatures))))

(defun unicode-fonts/init-persistent-soft ()
  (use-package persistent-soft
    :defer t))

(defun unicode-fonts/init-unicode-fonts ()
  (use-package unicode-fonts
    :init
    (progn
      (when (and unicode-fonts-force-multi-color-on-mac
                 (eq window-system 'ns))
        (setq unicode-fonts-skip-font-groups '(decorative low-quality-glyphs)))
      (unicode-fonts-setup))))

(defun unicode-fonts/init-ligature ()
  "Initialise the ligatures for emacs 27+"
  (dolist (mode unicode-fonts-ligature-modes)
    (ligature-set-ligatures mode unicode-fonts-ligature-set))
  (global-ligature-mode t))
