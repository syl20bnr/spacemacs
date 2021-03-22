;;; packages.el --- parinfer layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: DogLooksGood <DogLooksGood@rMBP>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst parinfer-packages
  '(parinfer-rust-mode))

(defun parinfer/init-parinfer-rust-mode ()
  (use-package parinfer-rust-mode
    :defer t
    :diminish parinfer-rust-mode
    :hook emacs-lisp-mode clojure-mode scheme-mode common-lisp-mode
    :init
    (progn
      (setq parinfer-rust-auto-download t)
      ;;; TODO smile13241324 07-03-2021
      ;;; Needed to duplicate the lib name algorithm to safely change the lib
      ;;; install directory to our cache folder. This is caused by the folders
      ;;; not having been separated from the system dependent library name.
      ;;; A PR has been opened see here
      ;;; https://github.com/justinbarclay/parinfer-rust-mode/pull/39
      (setq parinfer-rust-library
            (concat spacemacs-cache-directory
                    "parinfer-rust/"
                    (cond
                     ((eq system-type 'darwin) "parinfer-rust-darwin.so")
                     ((eq system-type 'gnu/linux) "parinfer-rust-linux.so")
                     ((eq system-type 'windows-nt) "parinfer-rust-windows.dll"))))
      (spacemacs|add-toggle parinfer-smart-indent
        :evil-leader "tP"
        :documentation "Enable Parinfer Smart Indent Mode."
        :if (bound-and-true-p parinfer-rust-mode)
        :status (eq parinfer-rust--mode 'smart)
        :on (parinfer-rust-toggle-paren-mode)
        :off (parinfer-rust-toggle-paren-mode)))))
