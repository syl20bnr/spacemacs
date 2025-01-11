;; -*- no-byte-compile: t -*-
;; Initialize Spacemacs dump mode
(setq spacemacs-dump-mode 'dumping)

;; Load init.el
(setq spacemacs-start-directory (file-name-directory load-file-name))
(load (expand-file-name "init" spacemacs-start-directory))

;; Prepare the dump
(spacemacs/dump-save-load-path)

;; Disable undo-tree to prevent segfaulting during dump loading
(when (fboundp 'global-undo-tree-mode)
  (global-undo-tree-mode -1))

;; Enable essential modes post-dump
(spacemacs|unless-dumping-and-eval-after-loaded-dump activate-modes
  (global-font-lock-mode 1)  ;; Ensure syntax highlighting survives dump
  (when (fboundp 'global-undo-tree-mode)
    (global-undo-tree-mode 1))  ;; Re-enable undo-tree
  (winner-mode 1))  ;; Enable winner-mode for window configuration management

;; Log dump status
(configuration-layer/message "Dumping Emacs...")

;; Finalize dump mode and run garbage collection
(setq spacemacs-dump-mode 'dumped)
(garbage-collect)
