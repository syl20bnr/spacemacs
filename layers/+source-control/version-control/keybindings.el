;;; keybindings.el --- Version control keybindings
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(spacemacs|define-transient-state vcs
  :title "VCS Transient State"
  :doc "
 Hunk Commands^^^^^^                 Magit Commands
----------------------------^^^^^^  ------------------------------------------
 [_n_]^^^^      next hunk            [_w_/_u_]^^    stage/unstage in current file
 [_N_/_p_]^^    previous hunk        [_c_/_C_]^^    commit with popup/direct commit
 [_r_/_s_/_h_]  revert/stage/show    [_f_/_F_/_P_]  fetch/pull/push popup
 [_t_]^^^^      toggle diff signs    [_l_/_D_]^^    log/diff popup"
  :on-enter (version-control/enable-margin)
  :bindings
  ("C" magit-commit :exit t)
  ("d" magit-ediff-popup :exit t)
  ("D" magit-diff-unstaged :exit t)
  ("F" magit-pull-popup :exit t)
  ("P" magit-push-popup :exit t)
  ("c" magit-commit-popup :exit t)
  ("f" magit-fetch-popup :exit t)
  ("l" magit-log-popup :exit t)
  ("u" magit-unstage-file)
  ("w" magit-stage-file)
  ("n" version-control/next-hunk)
  ("N" version-control/previous-hunk)
  ("p" version-control/previous-hunk)
  ("r" version-control/revert-hunk)
  ("s" version-control/stage-hunk)
  ("h" version-control/show-hunk)
  ("t" spacemacs/toggle-version-control-margin)
  ("q" nil :exit t))
(spacemacs/set-leader-keys "g." 'spacemacs/vcs-transient-state/body)

