;;; packages.el --- rebox layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Christian E. Hopps <chopps@gmail.com>
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


(defconst rebox-packages '(rebox2))

(defun rebox/init-rebox2 ()
  (use-package rebox2
    :defer t
    :init
    (progn
      (when rebox-enable-in-text-mode
        (add-hook 'text-mode-hook 'rebox-mode))
      (add-hook 'prog-mode-hook 'rebox-enable-hook)

      (spacemacs|define-transient-state rebox
        :title "Rebox Transient State"
        :bindings
        (">" rebox-space "Move right")
        ("<" rebox-backspace "Move left")
        ("b" rebox-dwim "Cycle next")
        ("B" spacemacs/rebox-dwim-previous "Cycle previous")
        ("c" rebox-center "Center"))

      (spacemacs/declare-prefix "xb" "boxes")
      (spacemacs/set-leader-keys
        "xb>" 'spacemacs/rebox-transient-state/rebox-space
        "xb<" 'spacemacs/rebox-transient-state/rebox-backspace
        "xbb" 'spacemacs/rebox-transient-state/rebox-dwim
        "xbB" 'spacemacs/rebox-transient-state/spacemacs/rebox-dwim-previous
        "xbc" 'spacemacs/rebox-transient-state/rebox-center))
    :config
    (progn
      (spacemacs|hide-lighter rebox-mode)
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
                                       "??==========="]))))
