;;; packages.el --- bm Layer packages File
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
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


(setq bm-packages '(bm))

(defun bm/init-bm ()
  "Initializes bm-emacs and adds a key binding to `SPC f z'"
  (use-package bm
    :defer t
    :commands (bm-buffer-restore)
    :init 
    ;; restore on load (even before you require bm)
    (setq bm-restore-repository-on-load t)
    ;; Allow cross-buffer 'next'
    (setq bm-cycle-all-buffers t)
    ;; save bookmarks
    (setq-default bm-buffer-persistence t)
    ;; where to store persistent files
    (setq bm-repository-file (format "%sbm-repository"
                                     spacemacs-cache-directory))
    (spacemacs|define-transient-state bm
      :title "BM Transient State"
      :doc "
 Go to bookmark^^^^       Toggle^^                 Show^^      Other^^
 ──────────────^^^^─────  ──────^^───────────────  ────^^────  ─────^^───
 [_n_/_N_] next/previous  [_t_] bookmark at point  [_s_] show  [_q_] quit"
      :bindings
      ("q" nil :exit t)
      ;; Go to bookmark
      ("n" bm-next)
      ("N" bm-previous)
      ;; Toggle
      ("t" bm-toggle)
      ;; Show
      ("s" bm-show-all :exit t))
    (evil-leader/set-key
      "atb" 'spacemacs/bm-transient-state/body)
    (advice-add 'spacemacs/bm-transient-state/body
                :before #'bm-buffer-restore)
    :config
    ;; Saving bookmarks
    (add-hook 'kill-buffer-hook #'bm-buffer-save)

    ;; Saving the repository to file when on exit.
    ;; kill-buffer-hook is not called when Emacs is killed, so we
    ;; must save all bookmarks first.
    (add-hook 'kill-emacs-hook #'(lambda nil
                                   (bm-buffer-save-all)
                                   (bm-repository-save)))

    ;; The `after-save-hook' is not necessary to use to achieve persistence,
    ;; but it makes the bookmark data in repository more in sync with the file
    ;; state.
    (add-hook 'after-save-hook #'bm-buffer-save)

    ;; Restoring bookmarks
    (add-hook 'find-file-hooks   #'bm-buffer-restore)
    (add-hook 'after-revert-hook #'bm-buffer-restore)

    ;; The `after-revert-hook' is not necessary to use to achieve persistence,
    ;; but it makes the bookmark data in repository more in sync with the file
    ;; state. This hook might cause trouble when using packages
    ;; that automatically reverts the buffer (like vc after a check-in).
    ;; This can easily be avoided if the package provides a hook that is
    ;; called before the buffer is reverted (like `vc-before-checkin-hook').
    ;; Then new bookmarks can be saved before the buffer is reverted.
    ;; Make sure bookmarks is saved before check-in (and revert-buffer)
    (add-hook 'vc-before-checkin-hook #'bm-buffer-save)))
