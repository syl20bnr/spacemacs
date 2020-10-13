(setq spacemacs-dump-mode 'dumping)
;; load init.el
(load (concat (file-name-directory load-file-name) "init.el"))
;; prepare the dump
(spacemacs/dump-save-load-path)
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
