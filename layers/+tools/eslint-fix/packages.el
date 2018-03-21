;;; packages.el --- eslint-fix layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Holm <holmi09@holmi09mac132>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst eslint-fix-packages
  '(eslint-fix)
  )

(defun eslint-fix/init-eslint-fix ()
  (use-package eslint-fix
    :defer t
    :init
    (progn
      (add-hook 'react-mode-hook 'eslint-fix-hook)
      (add-hook 'js2-mode-hook 'eslint-fix-hook)
      )))

;;; packages.el ends here
