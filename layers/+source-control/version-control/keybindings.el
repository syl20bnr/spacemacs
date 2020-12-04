;;; keybindings.el --- Version control keybindings
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(space-macs|define-transient-state vcs
  :title "VCS Transient State"
  :doc "
 Hunk Commands^^^^^^                 Magit Commands^^^^^^                             Others
----------------------------^^^^^^  ------------------------------------------^^^^^^  ------------^^
 [_n_]^^^^      next hunk            [_w_/_u_]^^    stage/unstage in current file     [_z_] recenter
 [_N_/_p_]^^    previous hunk        [_c_/_C_]^^    commit with popup/direct commit   [_q_] quit
 [_r_/_s_/_h_]  revert/stage/show    [_f_/_F_/_P_]  fetch/pull/push popup
 [_t_]^^^^      toggle diff signs    [_l_/_D_]^^    log/diff popup"
  :on-enter (space-macs/vcs-enable-margin)
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
  ("n" space-macs/vcs-next-hunk)
  ("N" space-macs/vcs-previous-hunk)
  ("p" space-macs/vcs-previous-hunk)
  ("r" space-macs/vcs-revert-hunk)
  ("s" space-macs/vcs-stage-hunk)
  ("h" space-macs/vcs-show-hunk)
  ("t" space-macs/toggle-version-control-margin)
  ("z" recenter-top-bottom)
  ("q" nil :exit t))
(space-macs/set-leader-keys "g." 'space-macs/vcs-transient-state/body)



