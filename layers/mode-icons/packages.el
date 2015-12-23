;;; packages.el --- mode-icons Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: krazedkrish<krazedkrish@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq mode-icons-packages
      '(
        ;; package names go here
        (mode-icons :location (recipe
                               :fetcher github
                               :repo "krazedkrish/mode-icons"
                               :files ("*.el" "icons")
                               ))
        ))

(defun mode-icons/init-mode-icons()
  (require 'mode-icons)
  (mode-icons-mode 1)
  )
