;;; packages.el --- prolog layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Newres Al Haider <newrescode@gmail.com>
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


(defconst prolog-packages
  '(
    (prolog :location built-in)
    (ediprolog)
    ))


(defun prolog/init-prolog ()
  (use-package prolog
    :defer t
    :init
    (progn
      (autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
      (autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
      (setq auto-mode-alist (append '(("\\.pl$" . prolog-mode))
                                    auto-mode-alist))        )
    :config
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'prolog-mode
        ;;Key Mappings
        ;;Consulting
        "sb" 'prolog-consult-buffer
        "sf" 'prolog-consult-file
        "sp" 'prolog-consult-predicate
        "sr" 'prolog-consult-region
        ;;Compiling
        "cb" 'prolog-compile-buffer
        "cc" 'prolog-compile-file
        "cp" 'prolog-compile-predicate
        "cr" 'prolog-compile-region
        ;;Formatting
        "=" 'prolog-indent-buffer
        ;;Insert
        "im" 'prolog-insert-module-modeline
        "in" 'prolog-insert-next-clause
        "ip" 'prolog-insert-predicate-template
        "is" 'prolog-insert-predspec
        ;;Help
        "ha" 'prolog-help-apropos
        "hp" 'prolog-help-on-predicate
        )

      (dolist (prefix '(("ms" . "consulting")
                        ("mc" . "compiling")
                        ("mh" . "help")
                        ("mi" . "inserting")
                        ))
        (spacemacs/declare-prefix-for-mode 'prolog-mode (car prefix) (cdr prefix))))
    ))

(defun prolog/init-ediprolog ()
  (use-package ediprolog
    :config
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'prolog-mode
        ;;Key Mappings
        "ee" 'ediprolog-dwim)
      (spacemacs/declare-prefix-for-mode 'prolog-mode "me" "evaluating" ))))

;;; packages.el ends here
