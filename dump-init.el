(setq spacemacs-dump-mode 'dumping)
(load (concat (file-name-directory load-file-name) "init.el"))
;; we need to backup the load-path in order to restore them
;; when we use the dump file
(setq load-path-backup load-path)
;; disable undo-tree to prevent from segfaulting when loading the dump
(when (fboundp 'global-undo-tree-mode)
  (global-undo-tree-mode -1))
;; Enable some modes that seem to not survive to the dumping process
(spacemacs|unless-dumping-and-eval-after-loaded-dump activate-modes
  (global-font-lock-mode)
  (when (fboundp 'global-undo-tree-mode)
    (global-undo-tree-mode t))
  (winner-mode 1))
(configuration-layer/message "Dumping Emacs...")
(setq spacemacs-dump-mode 'dumped)
(garbage-collect)
