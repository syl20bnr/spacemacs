(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching
;; Auto refresh buffers
(global-auto-revert-mode 1)
;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
;; activate winner mode use to undo and redo windows layout
(winner-mode t)
;; no beep pleeeeeease ! (and no visual blinking too please)
(custom-set-variables '(ring-bell-function 'ignore))
(setq visible-bell nil)

