;;; packages.el --- faust layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author:  Bart Brouns <bart@magnetophon.nl>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst faust-packages
  '(company
    faust-mode
    yasnippet))

(defun faust/init-faust-mode ()
  (use-package faust-mode
  :defer t
  :mode "\\.\\(dsp\\|lib\\)\\'"))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun faust/post-init-company ()
    (spacemacs|add-company-hook faust-mode)
    (spacemacs/add-to-hooks 'spacemacs/load-yasnippet '(faust-mode-hook))))
;;; packages.el ends here
