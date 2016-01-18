;;; packages.el --- command-log Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq command-log-packages
      ;; bmag/command-log-mode is a fork of lewang/command-log-mode, and there
      ;; is an open PR to merge the fork into the original repo.
      ;; TODO when the PR is merged upstream, change to use the original package
      ;; from MELPA (IOW remove :location argument)
      '((command-log-mode :location (recipe :fetcher github
                                            :repo "bmag/command-log-mode"
                                            :branch "color"))))

(defun command-log/init-command-log-mode ()
  (use-package command-log-mode
    :commands global-command-log-mode
    ;; :commands (clm/open-command-log-buffer global-command-log-mode spacemacs/toggle-command-log-mode)
    :init
    (spacemacs/set-leader-keys "aL" #'global-command-log-mode)
    :config
    (setq clm/log-command-exceptions* (append clm/log-command-exceptions*
                                              '(evil-next-line
                                                evil-previous-line
                                                evil-forward-char
                                                evil-backward-char))
          command-log-mode-auto-show t)))
