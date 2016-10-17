;;; packages.el --- Mercurial Layer for Spacemacs
;;
;; Copyright (c) 2016 Joseph Benden
;;
;; Author: Joseph Benden <joe@benden.us>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq mercurial-packages
  '(
    monky
   ))

(defun mercurial/init-monky ()
  (use-package monky
    :commands (monky-blame
               monky-blame-current-file
               monky-branches
               monky-commit
               monky-display-process
               monky-hg-command
               monky-log
               monky-queue
               monky-status)
    :init
    (progn
      (dolist (mode '(monky-branches-mode
                      monky-blame-mode
                      monky-log-mode
                      monky-queue-mode))
        (evil-set-initial-state mode 'emacs))

      ;; add key bindings
      (spacemacs/declare-prefix "gg" "mercurial")
      (evil-leader/set-key
        "gg$" 'monky-display-process
        "gg:" 'monky-hg-command
        "ggb" 'monky-blame
        "ggB" 'monky-blame-current-file
        "ggb" 'monky-branches
        "ggl" 'monky-log
        "ggQ" 'monky-queue
        "ggs" 'monky-status
        "ggC" 'monky-commit))
    :config
    (progn
      ;; mode maps
      (evilified-state-evilify-map monky-mode-map
        :mode monky-status-mode
        :bindings
        (kbd "C-S-j") 'monky-next-section
        (kbd "C-S-k") 'monky-prev-section
        (kbd "C-n") 'monky-next-section
        (kbd "C-p") 'monky-prev-section)

      ;; full screen monky-status
      (when mercurial-monky-status-fullscreen
        (defadvice monky-status (around monky-fullscreen activate)
          (window-configuration-to-register :monky-fullscreen)
          ad-do-it
          (delete-other-windows))
        (defadvice monky-quit-window (around monky-restore-screen activate)
          ad-do-it
          (jump-to-register :monky-fullscreen)))

      ;; set chosen process mode
      (setq monky-process-type mercurial-monky-process-type)

      ;; enable spell checking when available
      (when (configuration-layer/layer-usedp 'spell-checking)
        (spacemacs/add-to-hook 'monky-log-edit-mode-hook
                               '(lambda () (spacemacs/toggle-spelling-checking-on)))))))
