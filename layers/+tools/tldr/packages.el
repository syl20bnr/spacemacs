;;; packages.el --- tldr layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst tldr-packages '(tldr))

(defun tldr/init-tldr ()
  (use-package tldr
    :defer t
    :init
    (progn
      (setq tldr-directory-path (concat spacemacs-cache-directory "tldr/"))
      (spacemacs/declare-prefix "d" "documentation")
      (spacemacs/set-leader-keys
        "dm" 'tldr
        "dM" 'tldr-at-point))
    :config
    (progn
      (evilified-state-evilify-map tldr-mode-map
        :mode tldr-mode
        :bindings
        (kbd "o") 'ace-link-help
        (kbd "q") 'quit-window)
      (spacemacs/set-leader-keys-for-major-mode 'tldr-mode
        "r" 'tldr-update-docs))))

;;; packages.el ends here
