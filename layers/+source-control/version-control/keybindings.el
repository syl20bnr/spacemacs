;;; keybindings.el --- Version control keybindings
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
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
  :on-enter (spacemacs/vcs-enable-margin)
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
  ("n" spacemacs/vcs-next-hunk)
  ("N" spacemacs/vcs-previous-hunk)
  ("p" spacemacs/vcs-previous-hunk)
  ("r" spacemacs/vcs-revert-hunk)
  ("s" spacemacs/vcs-stage-hunk)
  ("h" spacemacs/vcs-show-hunk)
  ("t" spacemacs/toggle-version-control-margin)
  ("q" nil :exit t))
(spacemacs/set-leader-keys "g." 'spacemacs/vcs-transient-state/body)

