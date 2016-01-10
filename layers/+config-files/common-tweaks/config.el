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

(ct|tweak ct-helm-no-dots
  :description
  "Remove dots (`.' and `..') from helm files buffers."
  :pre
  (defvar no-dots-whitelist '() "List of helm buffers in which to show dots.")
  :loading
  (with-eval-after-load 'helm-files BODY)
  :functions
  (progn
    (require 'cl-lib)
    (defun no-dots/whitelistedp ()
      (member (with-helm-buffer (buffer-name)) no-dots-whitelist))
    (defun no-dots/helm-ff-filter-candidate-one-by-one (fcn file)
      (when (or (no-dots/whitelistedp)
                (not (string-match "\\(?:/\\|\\`\\)\\.\\{1,2\\}\\'" file)))
        (funcall fcn file)))
    (defun no-dots/helm-file-completion-source-p (&rest args) t)
    (defun no-dots/helm-find-files-up-one-level (fcn &rest args)
      (prog2
          (advice-add 'helm-file-completion-source-p
                      :around 'no-dots/helm-file-completion-source-p)
          (apply fcn args)
        (advice-remove 'helm-file-completion-source-p
                       'no-dots/helm-file-completion-source-p))))
  :tweak
  (progn
    (advice-add 'helm-ff-filter-candidate-one-by-one
                :around 'no-dots/helm-ff-filter-candidate-one-by-one)
    (advice-add 'helm-find-files-up-one-level
                :around 'no-dots/helm-find-files-up-one-level)))

(ct|tweak ct-neotree-close-on-open
  :description
  "Close the neotree's window when a file is opened from it."
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
  "Collapse the neotree hierarchy when going on a parent node."
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
