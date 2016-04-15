;;; packages.el --- rebox layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Christian E. Hopps <chopps@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq rebox-packages '(rebox2))

(defun rebox/init-rebox2 ()
  (use-package rebox2
    :commands (rebox-dwim rebox-mode)
    :init
    (progn
      (spacemacs/set-leader-keys "cb" 'rebox-dwim)

      (defun rebox-enable-hook ()
        "Until rebox is fixed for C style comments we have to disable"
        (and (not (or (eq major-mode 'c-mode)
                      (eq major-mode 'c++-mode)
                      (eq major-mode 'objc-mode)))
             (rebox-mode)))
      (add-hook 'prog-mode-hook 'rebox-enable-hook))
    :config
    (progn
      (rebox-register-template 71 176 ["?"
                                       "? box123456"
                                       "?"])

      (rebox-register-template 72 176 ["? ---------"
                                       "? box123456"
                                       "? ---------"])

      (rebox-register-template 73 376 ["? ========="
                                       "? box123456"
                                       "? ========="])

      (rebox-register-template 74 176 ["?-----------"
                                       "? box123456 "
                                       "?-----------"])

      (rebox-register-template 75 276 ["?-----------+"
                                       "? box123456 "
                                       "?-----------+"])

      (rebox-register-template 76 376 ["?==========="
                                       "? box123456 "
                                       "?==========="])

      (rebox-register-template 81 176 ["??"
                                       "?? box123456"
                                       "??"])

      (rebox-register-template 82 286 ["?? ---------"
                                       "?? box123456"
                                       "?? ---------"])

      (rebox-register-template 83 486 ["?? ========="
                                       "?? box123456"
                                       "?? ========="])

      (rebox-register-template 84 286 ["??-----------"
                                       "?? box123456 "
                                       "??-----------"])

      (rebox-register-template 85 386 ["??-----------+"
                                       "?? box123456 "
                                       "??-----------+"])

      (rebox-register-template 86 486 ["??==========="
                                       "?? box123456 "
                                       "??==========="])

      )))
