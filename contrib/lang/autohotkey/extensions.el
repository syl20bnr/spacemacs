;; Extensions are in emacs_paths/extensions

;; Pre extensions are loaded *before* the packages
(defvar autohotkey-pre-extensions
  '(
    ))

;; Post extensions are loaded *after* the packages
(defvar autohotkey-post-extensions
  '(
    ahk-mode
    ))

;; Initialize the extensions

(defun autohotkey/init-ahk-mode ()
  (use-package ahk-mode
    :init
    (evil-leader/set-key-for-mode 'ahk-mode
      "m E" 'run-this-autohotkey-script
      "m D" 'ahk-lookup-autohotkey-ref
      )
    :config
    (progn
      )))
