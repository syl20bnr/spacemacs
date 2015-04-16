;;; extensions.el --- Eshell Layer extensions File
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

;; Extensions are in emacs_paths/extensions
;; Pre extensions are loaded *before* the packages
(defvar eshell-pre-extensions
  '())

;; Post extensions are loaded *after* the packages
(defvar eshell-post-extensions
  '(emacs-builtin-eshell))

(defun eshell/init-emacs-builtin-eshell ()
  (when (configuration-layer/layer-usedp 'auto-completion)
    (push '(company-capf :with company-yasnippet)
          company-backends-eshell-mode)
    (spacemacs|add-company-hook eshell-mode)))
