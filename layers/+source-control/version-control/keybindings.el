;;; keybindings.el --- Version control keybindings
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
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


(spacemacs|define-transient-state vcs
  :title "VCS Transient State"
  :doc "
 Hunk Commands^^^^^^                 Magit Commands^^^^^^                             Others
----------------------------^^^^^^  ------------------------------------------^^^^^^  ------------^^
 [_n_]^^^^      next hunk            [_w_/_u_]^^    stage/unstage in current file     [_z_] recenter
 [_N_/_p_]^^    previous hunk        [_c_/_C_]^^    commit with popup/direct commit   [_q_] quit
 [_r_/_s_/_h_]  revert/stage/show    [_f_/_F_/_P_]  fetch/pull/push popup
 [_t_]^^^^      toggle diff signs    [_l_/_d_/_D_]  log/ediff/diff popup"
  :on-enter (spacemacs/vcs-enable-margin)
  :bindings
  ("C" magit-commit :exit t)
  ("d" magit-ediff :exit t)
  ("D" magit-diff-unstaged :exit t)
  ("F" magit-pull :exit t)
  ("P" magit-push :exit t)
  ("c" magit-commit :exit t)
  ("f" magit-fetch :exit t)
  ("l" magit-log :exit t)
  ("u" magit-unstage-file)
  ("w" magit-stage-file)
  ("n" spacemacs/vcs-next-hunk)
  ("N" spacemacs/vcs-previous-hunk)
  ("p" spacemacs/vcs-previous-hunk)
  ("r" spacemacs/vcs-revert-hunk)
  ("s" spacemacs/vcs-stage-hunk)
  ("h" spacemacs/vcs-show-hunk)
  ("t" spacemacs/toggle-version-control-margin)
  ("z" recenter-top-bottom)
  ("q" nil :exit t))
(spacemacs/set-leader-keys "g." 'spacemacs/vcs-transient-state/body)

