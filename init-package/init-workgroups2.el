(require 'workgroups2)
;; Settings:
;; (setq wg-prefix-key (kbd "C-c z")
;;       wg-restore-associated-buffers nil ; restore all buffers opened in this WG?
;;       wg-use-default-session-file nil   ; turn off for "emacs --daemon"
;;       wg-default-session-file "~/.emacs_files/workgroups"
;;       wg-use-faces nil
;;       wg-morph-on nil)                  ; animation off

;; ;; Keyboard shortcuts - load, save, switch
;; (global-set-key (kbd "<pause>")     'wg-reload-session)
;; (global-set-key (kbd "C-S-<pause>") 'wg-save-session)
;; (global-set-key (kbd "s-z")         'wg-switch-to-workgroup)
;; (global-set-key (kbd "s-/")         'wg-switch-to-previous-workgroup)

(setq wg-switch-on-load nil)
(setq wg-file (expand-file-name "~/.emacs.d/.wg-file.el"))
(setq wg-prefix-key (kbd "M-w"))
(setq wg-morph-on nil)
(setq wg-mode-line-left-brace "")
(setq wg-mode-line-right-brace "")

(workgroups-mode 1)
