;;; packages.el --- command-log Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(defconst command-log-packages
  ;; bmag/command-log-mode is a fork of lewang/command-log-mode, and there
  ;; is an open PR to merge the fork into the original repo.
  ;; TODO when the PR is merged upstream, change to use the original package
  ;; from MELPA (IOW remove :location argument)
  '((command-log-mode :location (recipe :fetcher github
                                        :repo "bmag/command-log-mode"
                                        :branch "color"))
    keycast))

(defun command-log/init-command-log-mode ()
  (use-package command-log-mode
    :commands global-command-log-mode
    ;; :commands (clm/open-command-log-buffer global-command-log-mode spacemacs/toggle-command-log-mode)
    :init
    (progn
      (spacemacs/declare-prefix "atl" "command log")
      (spacemacs/set-leader-keys "atll" #'global-command-log-mode))
    :config
    (setq clm/log-command-exceptions* (append clm/log-command-exceptions*
                                              '(evil-next-line
                                                evil-previous-line
                                                evil-forward-char
                                                evil-backward-char))
          command-log-mode-auto-show t)))

(defun command-log/init-keycast ()
  (use-package keycast
    :init
    (progn
      (spacemacs/declare-prefix "atK" "keycast")
      (spacemacs/set-leader-keys "atKm" #'keycast-mode-line-mode)
      (spacemacs/set-leader-keys "atKh" #'keycast-header-line-mode)
      (spacemacs/set-leader-keys "atKt" #'keycast-tab-bar-mode)

      ;; Include keycast in modeline
      (setq keycast-mode-line-insert-after "%e"))))
