;;; packages.el --- Outshine layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
;;
;; Author: Langston Barrett <langston.barrett@gmail.com>
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


(defconst outshine-packages
  '(outshine
    outorg))

(defun outshine/init-outshine ()
  (use-package outshine
    :defer t
    :init
    (add-hook 'prog-mode-hook          'outline-minor-mode)
    (add-hook 'outline-minor-mode-hook 'outshine-mode)
    :config
    (spacemacs|hide-lighter outline-minor-mode)
    (spacemacs|hide-lighter outshine-mode)
    (spacemacs/declare-prefix
      "aO"  "out(line/org/shine)"
      "aOg" "goto"
      "aOi" "insert")
    (spacemacs/set-leader-keys
      "aO."  'spacemacs/outshine-transient-state/body
      "aOS"  'outline-show-all
      "aOgu" 'outline-up-heading
      "aOgn" 'outline-next-heading
      "aOgj" 'outline-forward-same-level
      "aOgk" 'outline-backward-same-level
      "aOih" 'outline-insert-heading
      "aOI"  'outshine-imenu
      "aOK"  'outline-move-subtree-up
      "aOJ"  'outline-move-subtree-down
      "aO>"  'outline-demote
      "aO<"  'outline-promote)
    (spacemacs|define-transient-state outshine
      :title "Outshine Transient State"
      :doc "
Navigate headings^^^^      Move subtrees^^^^               Other^^
─────────────────^^^^────  ─────────────^^^^─────────────  ─────^^─────────────
[_j_/_k_] down/up          [_J_/_K_] move subtree down/up  [_q_] quit
[_n_/_N_] next/up heading  [_>_/_<_] demote/promote        [_i_] insert heading
[_I_]^^   heading imenu"
      :bindings
      ("q" nil :exit t)
      ("i" outline-insert-heading :exit t)
      ("I" outshine-imenu :exit t)
      ;; Navigate headings
      ("n" outline-next-heading)
      ("N" outline-up-heading)
      ("j" outline-forward-same-level)
      ("k" outline-backward-same-level)
      ;; Move headings
      ("J" outline-move-subtree-down)
      ("K" outline-move-subtree-up)
      ;; Move headings
      (">" outline-demote)
      ("<" outline-promote))))

(defun outshine/init-outorg ()
  (use-package outorg
    :defer t
    :config
    (spacemacs/set-leader-keys
      "aOc"  'outorg-copy-edits-and-exit
      "aOe"  'outorg-edit-as-org)))

;;; packages.el ends here
