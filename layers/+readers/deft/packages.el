;;; packages.el --- deft Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;;         Bruno Morais <brunosmmm@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(setq deft-packages
      '(
        deft
        (zetteldeft :toggle (eq deft-zetteldeft t))
        ))

(defun deft/init-zetteldeft ()
  (use-package zetteldeft
    :if (eq deft-zetteldeft t)
    :init
    (progn
      (space-macs/declare-prefix-for-mode 'deft-mode "mz" "zetteldeft")
      (space-macs/declare-prefix-for-mode 'org-mode "mz" "zetteldeft")
      (space-macs/declare-prefix "ardz" "zetteldeft")
      ;; zetteldeft actions in deft mode
      (space-macs/set-leader-keys-for-major-mode 'deft-mode
        "zT" 'zetteldeft-tag-buffer
        "zn" 'zetteldeft-new-file
        )
      ;; zetteldeft actions in org mode
      (space-macs/set-leader-keys-for-major-mode 'org-mode
        "zc" 'zetteldeft-search-current-id
        "zf" 'zetteldeft-follow-link
        "zt" 'zetteldeft-avy-tag-search
        "zN" 'zetteldeft-new-file-and-link
        "zr" 'zetteldeft-file-rename
        "zi" 'zetteldeft-find-file-id-insert
        "zI" 'zetteldeft-find-file-full-title-insert
        "zs" 'zetteldeft-search-at-point
        "zl" 'zetteldeft-avy-link-search
        "zF" 'zetteldeft-avy-file-search-ace-window
        "zo" 'zetteldeft-find-file
        )
      ;; new zetteldeft file under capture
      (space-macs/set-leader-keys "Cz" 'zetteldeft-new-file)
      ;; actions under applications/deft/zetteldeft
      (space-macs/set-leader-keys "ardzn" 'zetteldeft-new-file)
      (space-macs/set-leader-keys "ardzT" 'zetteldeft-tag-buffer)
      (space-macs/set-leader-keys "ardzs" 'zetteldeft-search-at-point)
      (space-macs/set-leader-keys "ardzo" 'zetteldeft-find-file)
    )))

(defun deft/init-deft ()
  (use-package deft
    :defer t
    :init
    (progn
      (setq deft-extensions '("org" "md" "txt")
            deft-text-mode 'org-mode
            deft-use-filename-as-title t
            deft-use-filter-string-for-filename t)
      ;; in applications prefix, NOTE: backward incompatible keybindings
      (if deft-zetteldeft
          (progn
            (space-macs/declare-prefix "ard" "deft")
            (space-macs/set-leader-keys "ardn" 'space-macs/deft))
        (space-macs/set-leader-keys "ard" 'space-macs/deft))
      ;; put in capture prefix
      (space-macs/set-leader-keys "Cd" 'deft-new-file)

      (defun space-macs/deft ()
        "Helper to call deft and then fix things so that it is nice and works"
        (interactive)
        (deft)
        ;; Hungry delete wrecks deft's DEL override
        (when (fboundp 'hungry-delete-mode)
          (hungry-delete-mode -1))
        ;; When opening it you always want to filter right away
        (evil-insert-state nil)))
    :config (space-macs/set-leader-keys-for-major-mode 'deft-mode
        "c" 'deft-filter-clear
        "d" 'deft-delete-file
        "i" 'deft-toggle-incremental-search
        "n" 'deft-new-file
        "N" 'deft-new-file-named
        "q" 'quit-window
        "o" 'deft-open-file-other-window
        "r" 'deft-rename-file)))


