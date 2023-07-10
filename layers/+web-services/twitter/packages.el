;;; packages.el --- twitter Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
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


(defconst twitter-packages
  '(twittering-mode))

(defun twitter/init-twittering-mode ()
  (use-package twittering-mode
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys "awt" 'twit)
      (setq twittering-initial-timeline-spec-string '(":home")
            twittering-icon-mode t
            twittering-use-icon-storage 1
            twittering-enable-unread-status-notifier t
            twittering-display-remaining t
            twittering-edit-skeleton 'inherit-any
            twittering-url-show-status nil
            twittering-timeline-header  ""
            twittering-timeline-footer  ""
            twitter-images-directory (concat spacemacs-cache-directory "twitter")
            twittering-status-format "%i  %S, %RT{%FACE[bold]{%S}} %@  %FACE[shadow]{%p%f%L%r}\n%FOLD[        ]{%T}\n")
      (unless (file-exists-p twitter-images-directory)
        (make-directory twitter-images-directory)))
    :config
    (progn
      ;; twittering mode overwrite the leader key, make sure that our bindings prevail
      (define-key twittering-mode-map (kbd "SPC") nil)
      ;; redefine better defaults
      (let ((map twittering-mode-map))
        (define-key map "?"   'spacemacs/twittering-mode-transient-state/body)
        (define-key map "/"   'twittering-search)
        (define-key map "a"   'twittering-toggle-activate-buffer)
        (define-key map "b"   'twittering-favorite)
        (define-key map "B"   'twittering-unfavorite)
        (define-key map "d"   'twittering-direct-message)
        (define-key map "e"   'twittering-edit-mode)
        (define-key map "f"   'twittering-follow)
        (define-key map "F"   'twittering-unfollow)
        (define-key map "g"   'beginning-of-buffer)
        (define-key map "G"   'end-of-buffer)
        (define-key map "i"   'twittering-view-user-page)
        (define-key map "Q"   'twittering-kill-buffer)
        (define-key map "I"   'twittering-icon-mode)
        (define-key map "j"   'twittering-goto-next-status)
        (define-key map "J"   'twittering-goto-next-status-of-user)
        (define-key map "k"   'twittering-goto-previous-status)
        (define-key map "K"   'twittering-goto-previous-status-of-user)
        (define-key map "n"   'twittering-update-status-interactive)
        (define-key map "o"   'twittering-click)
        (define-key map "r"   'twittering-native-retweet)
        (define-key map "R"   'twittering-organic-retweet)
        (define-key map "t"   'twittering-toggle-or-retrieve-replied-statuses)
        (define-key map "u"   'twittering-current-timeline)
        (define-key map "X"   'twittering-delete-status)
        (define-key map "y"   'twittering-push-uri-onto-kill-ring)
        (define-key map "Y"   'twittering-push-tweet-onto-kill-ring))
      ;; associated transient state
      (spacemacs|define-transient-state twittering-mode
        :title "Twittering Mode Transient State"
        :doc "
 Tweets^^^^^^                                   User^^^^                Other^^
 ──────^^^^^^────────────────────────────────── ────^^^^─────────────── ─────^^───────────────────
 [_j_/_k_] down/up        [_r_] retweet         [_d_]^^ direct message  [_a_] toggle auto-refresh
 [_RET_]^^ open or reply  [_R_] retweet & edit  [_f_]^^ follow          [_q_] quit
 [_b_]^^   heart          [_n_] post new tweet  [_F_]^^ unfollow        [_Q_] quit twitter
 [_B_]^^   unheart        [_t_] show thread     [_i_]^^ profile         [_u_] update
 [_e_]^^   edit mode      [_X_] delete tweet    [_J_/_K_] down/up       [_/_] search
 [_g_]^^   first          [_y_] yank url        ^^^^                    [_I_] toggle images
 [_G_]^^   last           [_Y_] yank tweet
 [_o_]^^   open url"
        :bindings
        ("?"          nil :exit t)
        ("RET"        twittering-enter :exit t)
        ("/"          twittering-search :exit t)
        ("a"          twittering-toggle-activate-buffer)
        ("b"          twittering-favorite)
        ("B"          twittering-unfavorite)
        ("d"          twittering-direct-message :exit t)
        ("e"          twittering-edit-mode :exit t)
        ("f"          twittering-follow)
        ("F"          twittering-unfollow)
        ("g"          beginning-of-buffer)
        ("G"          end-of-buffer)
        ("i"          twittering-view-user-page)
        ("q"          nil :exit t)
        ("Q"          twittering-kill-buffer :exit t)
        ("I"          twittering-icon-mode)
        ("j"          twittering-goto-next-status)
        ("J"          twittering-goto-next-status-of-user)
        ("k"          twittering-goto-previous-status)
        ("K"          twittering-goto-previous-status-of-user)
        ("n"          twittering-update-status-interactive :exit t)
        ("o"          twittering-click :exit t)
        ("r"          twittering-native-retweet :exit t)
        ("R"          twittering-organic-retweet :exit t)
        ("t"          twittering-toggle-or-retrieve-replied-statuses :exit t)
        ("u"          twittering-current-timeline)
        ("X"          twittering-delete-status)
        ("y"          twittering-push-uri-onto-kill-ring)
        ("Y"          twittering-push-tweet-onto-kill-ring)))))
