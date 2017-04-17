;;; packages.el --- swift Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Martin Yrjölä & Contributors
;;
;; Author: Martin Yrjölä <martin.yrjola@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq swift-packages
    '(
      swift-mode
      ))

(setq swift-excluded-packages '())

(defun swift/init-swift-mode ()
  "Initialize swift-mode with Spacemacs bindings"
  (use-package swift-mode
    :defer t
    :config
    (evil-leader/set-key-for-mode 'swift-mode
      "msi" 'swift-mode-run-repl
      "msb" 'swift-mode-send-buffer
      "msr" 'swift-mode-send-region)
    )
  )
