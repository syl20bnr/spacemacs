;;; packages.el --- lsp-python layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Yuan Fu <casouri@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(spacemacs/set-leader-keys-for-major-mode 'python-mode (kbd "a") #'pyvenv-activate)
(spacemacs/set-leader-keys-for-major-mode 'python-mode (kbd "d") #'pyvenv-deactivate)
(spacemacs/set-leader-keys-for-major-mode 'python-mode (kbd "e") #'spacemacs/run-current-file-in-python)
(spacemacs/set-leader-keys-for-major-mode 'python-mode (kbd "k") #'spacemacs/kill-python-interpreter)
(spacemacs/set-leader-keys-for-major-mode 'python-mode (kbd "r") #'xref-find-references)
(spacemacs/set-leader-keys-for-major-mode 'python-mode (kbd "d") #'xref-find-definitions)
