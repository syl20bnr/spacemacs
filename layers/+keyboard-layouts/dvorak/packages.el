;;; packages.el --- dvorak layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Alejandro Catalina Feliú <alecatfel@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst dvorak-packages
  '((evil-dvorak :location (recipe
                            :fetcher github
                            :repo "AlejandroCatalina/evil-dvorak"))))

(defun dvorak/init-evil-dvorak ()
  (use-package evil-dvorak
    :config
    (global-evil-dvorak-mode)))

;; package.el ends here
