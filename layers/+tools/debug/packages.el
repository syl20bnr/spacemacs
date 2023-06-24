;;; packages.el --- Debug Layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Troy Hinckley <troy.hinckley@gmail.com>
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


(defconst debug-packages
  '(realgud))

(defun debug/init-realgud ()
  (use-package realgud
    :defer t
    :init
    (dolist (debugger (mapcar 'spacemacs/debug-generate-symbol
                              debug-additional-debuggers))
      (autoload debugger "realgud" nil t))
    (advice-add 'realgud-short-key-mode-setup
                :before #'spacemacs/debug-short-key-state)
    (evilified-state-evilify-map realgud:shortkey-mode-map
      :eval-after-load realgud
      :mode realgud-short-key-mode
      :bindings
      "bb" 'realgud:cmd-break
      "bc" 'realgud:cmd-clear
      "bd" 'realgud:cmd-delete
      "bs" 'realgud:cmd-disable
      "be" 'realgud:cmd-enable
      "c" 'realgud:cmd-continue
      "i" 'realgud:cmd-step
      "J" 'realgud:cmd-jump
      "o" 'realgud:cmd-finish
      "q" 'realgud:cmd-quit
      "r" 'realgud:cmd-restart
      "s" 'realgud:cmd-next
      "S" 'realgud-window-cmd-undisturb-src
      "v" 'realgud:cmd-eval-dwim)))
