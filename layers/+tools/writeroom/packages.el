;;; packages.el --- writeroom Layer packages File for Spacemacs
;;
;; Copyright (c) 2015 Gergely Nagy
;;
;; Author: Gergely Nagy <algernon@madhouse-project.org>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq writeroom-packages
      '(
        writeroom-mode
        ))

(defun writeroom/init-writeroom-mode ()
  "Initialize writeroom-mode"

  (use-package writeroom-mode
    :init (spacemacs/set-leader-keys "aW" #'writeroom-mode)))
