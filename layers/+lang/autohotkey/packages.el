;;; packages.el --- autohotkey Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; Author: Rich Alesi <https://github.com/ralesi>
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


(setq autohotkey-packages
      '(
        company
        ahk-mode
        ))

(defun autohotkey/init-ahk-mode ()
  (use-package ahk-mode
    :mode "\\.ahk\\'"
    :defer t
    :init
    ;; work-around for issue #21
    ;; https://github.com/ralesi/ahk-mode/issues/21
    (add-hook 'ahk-mode-hook 'spacemacs/run-prog-mode-hooks)
    (spacemacs/declare-prefix-for-mode 'ahk-mode "mc" "comment")
    (spacemacs/declare-prefix-for-mode 'ahk-mode "me" "eval")
    (spacemacs/declare-prefix-for-mode 'ahk-mode "mh" "help")
    (spacemacs/set-leader-keys-for-major-mode 'ahk-mode
      "cb" 'ahk-comment-block-dwim
      "cc" 'ahk-comment-dwim
      "eb" 'ahk-run-script
      "hh" 'ahk-lookup-web
      "hH" 'ahk-lookup-chm)))

(defun autohotkey/post-init-company ()
  (spacemacs|add-company-backends
    :backends company-capf
    :modes ahk-mode
    :variables company-tooltip-align-annotations t))
