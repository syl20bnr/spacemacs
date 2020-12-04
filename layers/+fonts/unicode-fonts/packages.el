;;; packages.el --- unicode-fonts layer packages file for Space-macs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Aaron Jensen <aaronjensen@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defconst unicode-fonts-packages
  '(unicode-fonts
    persistent-soft
    (ligature :location (recipe
                         :fetcher github
                         :repo "mickeynp/ligature.el")
              :toggle (and (> e-macs-major-version 26) unicode-fonts-enable-ligatures))))

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
  "Initialise the ligatures for e-macs 27+"
  (dolist (mode unicode-fonts-ligature-modes)
    (ligature-set-ligatures mode unicode-fonts-ligature-set))
  (global-ligature-mode t))


