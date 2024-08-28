;;; packages.el --- evil-better-jumper Layer Packages File for Spacemacs.
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanh@gmail.com>
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


(defconst evil-better-jumper-packages
  '(better-jumper)
  "The list of Lisp packages required by the evil-better-jumper layer.")

(defun evil-better-jumper/init-better-jumper ()
  (use-package better-jumper
    :init
    ;; Fix https://github.com/gilbertw1/better-jumper/pull/26
    ;; (can be removed once the above PR is merged)
    (define-obsolete-variable-alias 'evil--jumps-jumping-backward 'evil--jumps-jump-command "2024-07")
    (global-set-key [remap evil-jump-forward]  #'better-jumper-jump-forward)
    (global-set-key [remap evil-jump-backward] #'better-jumper-jump-backward)
    (global-set-key [remap xref-pop-marker-stack] #'better-jumper-jump-backward)
    :config
    (better-jumper-mode 1)
    (spacemacs|hide-lighter better-jumper-mode)
    (spacemacs|hide-lighter better-jumper-local-mode))

  ;; Creates a jump point before killing a buffer. This allows you to undo
  ;; killing a buffer easily (only works with file buffers though; it's not
  ;; possible to resurrect special buffers).
  (advice-add #'kill-current-buffer :around #'evil-better-jumper/set-jump-a)

  ;; Create a jump point before jumping with imenu.
  (advice-add #'imenu :around #'evil-better-jumper/set-jump-a))
