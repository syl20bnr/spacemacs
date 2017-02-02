;;; packages.el --- rest layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author:  <wwguo@hiGDP>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq restructuredtext-packages
      '((rst :location built-in)
        (rst-directives :location local)
        (rst-lists :location local)
        (rst-sphinx :location local)
        flyspell
        smartparens
        yasnippet
        ))

(defun restructuredtext/init-rst-directives ()
  (use-package rst-directives))

(defun restructuredtext/init-rst-lists ()
  (use-package rst-lists))

(defun restructuredtext/init-rst-sphinx ()
  (use-package rst-sphinx))

(defun restructuredtext/init-rst ()
  (use-package rst
    :defer t
    :config
    (progn
      (add-hook 'rst-adjust-hook 'rst-toc-update)
      (spacemacs/set-leader-keys-for-major-mode 'rst-mode
        "c" 'rst-sphinx-compile
        "f" 'rst-sphinx-target-open))))

(defun restructuredtext/post-init-flyspell ()
  (spell-checking/add-flyspell-hook 'rst-mode-hook))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun restructuredtext/post-init-yasnippet ()
    (add-hook 'rst-mode-hook 'spacemacs/load-yasnippet)))

(defun restructuredtext/post-init-smartparens ()
  (add-hook 'rst-mode-hook 'smartparens-mode))

