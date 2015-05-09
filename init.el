;;; init.el --- Spacemacs Initialization File
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(defconst spacemacs-version          "0.101.3" "Spacemacs version.")
(defconst spacemacs-emacs-min-version   "24.3" "Minimal version of Emacs.")

(defun spacemacs/emacs-version-ok ()
  (version<= spacemacs-emacs-min-version emacs-version))

(when (spacemacs/emacs-version-ok)
  (load-file (concat user-emacs-directory "core/core-load-paths.el"))
  (require 'core-spacemacs)
  (require 'core-configuration-layer)
  (spacemacs/init)
  (configuration-layer/sync)
  (spacemacs/setup-after-init-hook)
  (spacemacs/maybe-install-dotfile)
  (require 'server)
  (unless (server-running-p) (server-start)))

;; p4
;;(add-to-list 'load-path "/u/snandan/.emacs.d/elpa/p4-20150331.222")
;;(require 'p4)

;; cscope
;;(require 'xcscope)


;; Invoke 'compile' with these key-stroke
(global-set-key '[(control c) (o)] 'compile)

;; Goto line
(global-set-key '[(control l)] 'goto-line)


;; to copy lines
(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))
(global-set-key '[(control c) (c)] 'copy-line)


;; different c-mode hooks
(defun my-c-mode-hook ()
  (local-set-key '[(control c) (control v)] 'eassist-switch-h-cpp) ;; switch between cpp and h
  (local-set-key '[(control c) (e)]         'eassist-list-methods)
  (local-set-key '[(control c) (control r)] 'semantic-symref)      ;; semantic reference
  (local-set-key '[(control return)]        'semantic-ia-complete-symbol)
  (local-set-key '[(meta g) (a)]            'align)
  (local-set-key '[(meta g) (A)]            'align-regexp)
  (local-set-key '[(meta g) (w)]            'fixup-whitespace)

  (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-hook)


;; magit
(setq magit-last-seen-setup-instructions "1.4.0")
;;(require 'magit)
(global-set-key '[(meta g) (s)] 'magit-status)


;; autopair
;;(require 'autopair)
;;(autopair-global-mode)



;; VG specific indentation rules
(load-file "/u/regress/INFRA_HOME/pub/utils/vg-coding-style.el")
(add-hook 'c-mode-common-hook 'vg-set-coding-style)
(add-hook 'c-mode-common-hook 'vg-make-newline-indent)

;; remove trailing whitespaces
(add-hook 'c-mode-common-hook (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))


;; open file from link
(defun follow-file-from-link ()
  (interactive)
  (if (not (buffer-modified-p (current-buffer)))
      (let ((f-name (buffer-file-name (current-buffer)))
            (b-name (buffer-name (current-buffer))))
        (kill-buffer b-name)
        (set-buffer (find-file (file-truename f-name)))
        )
    )
)
(global-set-key '[(control c) (control j)] 'follow-file-from-link)

;; to hold down shift key to switch buffers
(windmove-default-keybindings)

;; org settings
(load-file "/u/snandan/.emacs-lisp/org-settings.el")

;; helm settings
;(load-file "/u/snandan/.emacs-lisp/helm-settings.el")


;; transpose-frame
;(load-file "/u/snandan/.emacs-lisp/transpose-frame-settings.el")

;; ace-jump-mode
;(load-file "/u/snandan/.emacs-lisp/ace-jump-settings.el")

;; gdb-settings
;(load-file "/u/snandan/.emacs-lisp/gdb-settings.el")
