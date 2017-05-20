;;; packages.el --- org-pomodoro Layer packages File for Spacemacs
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

(defvar org-pomodoro-packages
  '(
    ;; package org-pomodoros go here
    org-pomodoro
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar org-pomodoro-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function org-pomodoro/init-<package-org-pomodoro>
;;
;; (defun org-pomodoro/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package


(defun org-pomodoro/init-org-pomodoro ()
  "Initialize org-pomodoro"
  (use-package org-pomodoro
    :defer t
    :init
    (progn
      (when (system-is-mac)
        (setq org-pomodoro-audio-player "/usr/bin/afplay"))
      (evil-leader/set-key-for-mode 'org-mode
        "p" nil "mp" 'org-pomodoro)))
  )
