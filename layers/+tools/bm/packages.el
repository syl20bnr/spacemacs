;;; packages.el --- bm Layer packages File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(setq bm-packages '(bm))

(defun bm/init-bm ()
  "initializes bm-e-macs and adds a key binding to `SPC f z'"
  (use-package bm
    :defer t
    :commands (bm-buffer-restore)
    :init (progn
            ;; restore on load (even before you require bm)
            (setq bm-restore-repository-on-load t)
            ;; Allow cross-buffer 'next'
            (setq bm-cycle-all-buffers t)
            ;; save bookmarks
            (setq-default bm-buffer-persistence t)
            ;; where to store persistent files
            (setq bm-repository-file (format "%sbm-repository"
                                             space-macs-cache-directory))
            (space-macs|define-transient-state bm
              :title "BM Transient State"
              :doc "
 Go to bookmark^^^^       Toggle^^                 Other^^
 â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€^^^^â”€â”€â”€â”€â”€  â”€â”€â”€â”€â”€â”€^^â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€  â”€â”€â”€â”€â”€^^â”€â”€â”€
 [_n_/_N_] next/previous  [_t_] bookmark at point  [_q_] quit"
              :bindings
              ("q" nil :exit t)
              ;; Go to bookmark
              ("n" bm-next)
              ("N" bm-previous)
              ;; Toggle
              ("t" bm-toggle))
            (evil-leader/set-key
              "atb" 'space-macs/bm-transient-state/body)
            (advice-add 'space-macs/bm-transient-state/body
                        :before #'bm-buffer-restore))
    :config (progn
              ;; Saving bookmarks
              (add-hook 'kill-buffer-hook #'bm-buffer-save)
              ;; Saving the repository to file when on exit.
              ;; kill-buffer-hook is not called when e-macs is killed, so we
              ;; must save all bookmarks first.
              (add-hook 'kill-e-macs-hook #'(lambda nil
                                             (bm-buffer-save-all)
                                             (bm-repository-save)))
              ;; Restoring bookmarks
              (add-hook 'find-file-hooks   #'bm-buffer-restore)
              ;; Make sure bookmarks is saved before check-in (and revert-buffer)
              (add-hook 'vc-before-checkin-hook #'bm-buffer-save))))


