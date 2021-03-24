;;; packages.el --- Agda2 Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Oliver Charles <ollie@ocharles.org.uk>
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


(setq agda-packages
      '(
        (agda :location local)
        company
        golden-ratio
        ))

(defun agda/post-init-company ()
  (spacemacs|add-company-backends :backends company-capf :modes agda2-mode))

(defun agda/init-agda ()
  (if (and (eq 'use-helper agda-mode-path)
           (not (executable-find "agda-mode")))
      (spacemacs-buffer/warning
       (concat "Couldn't find `agda-mode', make sure it is "
               "available in your PATH or check the installation "
               "instructions in the README file."))

    (when (eq 'use-helper agda-mode-path)
      (setq agda-mode-path (let ((coding-system-for-read 'utf-8))
                             (shell-command-to-string "agda-mode locate"))))

    (progn
      (setq agda-version-out (let ((coding-system-for-read 'utf-8))
                               (shell-command-to-string "agda --version")))
      (string-match "\\([0-9]+\\.\\)*[0-9]+" agda-version-out)
      (setq agda-version (match-string 0 agda-version-out)))

    (setq agda2-auto (if (string< agda-version "2.6.0")
                         'agda2-auto
                       'agda2-auto-maybe-all))

    (use-package agda2-mode
      :defer t
      :init (when agda-mode-path (load-file agda-mode-path))
      (progn
        (mapc
         (lambda (x) (add-to-list 'face-remapping-alist x))
         '((agda2-highlight-datatype-face              . font-lock-type-face)
           (agda2-highlight-function-face              . font-lock-type-face)
           (agda2-highlight-inductive-constructor-face . font-lock-function-name-face)
           (agda2-highlight-keyword-face               . font-lock-keyword-face)
           (agda2-highlight-module-face                . font-lock-constant-face)
           (agda2-highlight-number-face                . nil)
           (agda2-highlight-postulate-face             . font-lock-type-face)
           (agda2-highlight-primitive-type-face        . font-lock-type-face)
           (agda2-highlight-record-face                . font-lock-type-face))))
      :config
      (progn
        ; don't lose indentation on paste
        (add-to-list 'spacemacs-indent-sensitive-modes 'agda2-mode)

        (spacemacs|define-transient-state goal-navigation
          :title "Goal Navigation Transient State"
          :doc "\n[_f_] next [_b_] previous [_q_] quit"
          :bindings
          ("f" agda2-next-goal)
          ("b" agda2-previous-goal)
          ("q" nil :exit t))
        (spacemacs/set-leader-keys-for-major-mode 'agda2-mode
          "f" 'spacemacs/goal-navigation-transient-state/agda2-next-goal
          "b" 'spacemacs/goal-navigation-transient-state/agda2-previous-goal)

        (spacemacs/set-leader-keys-for-major-mode 'agda2-mode
          "?"   'agda2-show-goals
          "."   'agda2-goal-and-context-and-inferred
          ","   'agda2-goal-and-context
          "="   'agda2-show-constraints
          "SPC" 'agda2-give
          "a"   agda2-auto
          "c"   'agda2-make-case
          "d"   'agda2-infer-type-maybe-toplevel
          "e"   'agda2-show-context
          "gG"  'agda2-go-back
          "h"   'agda2-helper-function-type
          "l"   'agda2-load
          "n"   'agda2-compute-normalised-maybe-toplevel
          "p"   'agda2-module-contents-maybe-toplevel
          "r"   'agda2-refine
          "s"   'agda2-solveAll
          "t"   'agda2-goal-type
          "w"   'agda2-why-in-scope-maybe-toplevel
          "xc"  'agda2-compile
          "xd"  'agda2-remove-annotations
          "xh"  'agda2-display-implicit-arguments
          "xq"  'agda2-quit
          "xr"  'agda2-restart)))))

(defun agda/pre-init-golden-ratio ()
  (spacemacs|use-package-add-hook golden-ratio
    :post-config
    (add-to-list 'golden-ratio-exclude-buffer-names
                 "*Agda information*")))
