;;; config.el --- common-tweaks Layer packages File for Spacemacs
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

(ct|tweak ct-neotree-close-on-open
  :description
  "With this tweak, the neotree window is closed when a file is
opened from it."
  :loader
  (spacemacs|use-package-add-hook neotree :post-config BODY)
  :functions
  (progn
    (defun neo-open-file-hide (full-path &optional arg)
      "Open a file node and hides tree."
      (neo-global--select-mru-window arg)
      (find-file full-path)
      (neotree-hide))
    (defun neotree-enter-hide (&optional arg)
      "Enters file and hides neotree directly"
      (interactive "P")
      (neo-buffer--execute arg 'neo-open-file-hide 'neo-open-dir)))
  :tweak
  (let ((keymap (evil-get-auxiliary-keymap neotree-mode-map 'evilified)))
    (define-key keymap (kbd "l") 'neotree-enter-hide)
    (define-key keymap (kbd "RET") 'neotree-enter-hide)))

(ct|tweak ct-neotree-collapse-on-parent
  :description
  "With this tweak, the tree's hierarchy is collapsed when going
on the parent's node."
  :loader
  (spacemacs|use-package-add-hook neotree :post-config BODY)
  :functions
  (defun neotree-to-parent-and-close (&optional arg)
    "Close parent subtree"
    (interactive "P")
    (neotree-select-up-node)
    (neotree-enter))
  :tweak
  (let ((keymap (evil-get-auxiliary-keymap neotree-mode-map 'evilified)))
    (define-key keymap (kbd "h") 'neotree-to-parent-and-close)))
