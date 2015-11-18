;;; packages.el --- Source Control Layer packages File for Spacemacs
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

(setq version-control-packages
      '(
        diff-mode
        diff-hl))

(defun version-control/init-diff-mode ()
  (use-package diff-mode
    :defer t
    :config
    (evilified-state-evilify diff-mode diff-mode-map
      "j" 'diff-hunk-next
      "k" 'diff-hunk-prev)))

(defun version-control/init-diff-hl ()
  (use-package diff-hl
    :init
    (progn
      (setq diff-hl-side 'right)
      (global-diff-hl-mode)
      (unless (display-graphic-p)
        (setq diff-hl-side 'left)
        (diff-hl-margin-mode))
      (spacemacs/set-leader-keys
        "gdg" 'diff-hl-diff-goto-hunk
        "gdN" 'diff-hl-previous-hunk
        "gdn" 'diff-hl-next-hunk
        "gdr" 'diff-hl-revert-hunk))))
