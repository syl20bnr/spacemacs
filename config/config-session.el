;; no welcome buffer
(setq inhibit-startup-screen t)
;; scratch buffer empty
(setq initial-scratch-message nil)
(setq redisplay-dont-pause t)
;; don't create backup~ or #auto-save# files
(setq backup-by-copying t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)
(require 'uniquify)
;; When having windows with repeated filenames, uniquify them
;; by the folder they are in rather those annoying <2>,<3>,.. etc
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
; don't screw special buffers
(setq uniquify-ignore-buffers-re "^\\*")
;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
                (file-name-history        . 100)
                (grep-history             . 30)
                (compile-history          . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                (search-ring              . 20)
                (shell-command-history    . 50)
                tags-file-name
                register-alist)))

;; Make emacs open all files in last emacs session (taken from ergoemacs).

;; This functionality is provided by desktop-save-mode
;; (“feature” name: “desktop”).
;;
;; The mode is not on by default in emacs 23.1, and has a lot options.
;; The following is init settings for the mode for ErgoEmacs.
;; Goal: have emacs always auto open the set of opened files in last session,
;; even if emacs crashed in last session or the OS crashed in last session.
;; Also, don't bother users by asking questions like “do you want to save
;; desktop?” or “do you want to override last session file?”, because these are
;; annoying and terms like “session” or “desktop” are confusing to most users
;; because it can have many meanings.
;;
;; Some tech detail: set the desktop session file 〔.emacs.desktop〕
;; at the variable “user-emacs-directory” (default value is “~/.emacs.d/”).
;; This file is our desktop file. It will be auto created and or over-written.
;; If a emacs expert has other desktop session files elsewhere, he can still use
;; or manage those.

(require 'desktop)

(defun desktop-settings-setup ()
  "Some settings setup for desktop-save-mode."
  (interactive)

  ;; At this point the desktop.el hook in after-init-hook was
  ;; executed, so (desktop-read) is avoided.
  (when (not (eq (emacs-pid) (desktop-owner))) ; Check that emacs did not load a desktop yet
    ;; Here we activate the desktop mode
    (desktop-save-mode 1)

    ;; The default desktop is saved always
    (setq desktop-save t)

    ;; The default desktop is loaded anyway if it is locked
    (setq desktop-load-locked-desktop t)

    ;; Set the location to save/load default desktop
    (setq desktop-dirname user-emacs-directory)

    ;; Make sure that even if emacs or OS crashed, emacs
    ;; still have last opened files.
    (add-hook 'find-file-hook
     (lambda ()
       (run-with-timer 5 nil
          (lambda ()
            ;; Reset desktop modification time so the user is not bothered
            (setq desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name))))
            (desktop-save user-emacs-directory)))))

    ;; Read default desktop
    (if (file-exists-p (concat desktop-dirname desktop-base-file-name))
        (desktop-read desktop-dirname))

    ;; Add a hook when emacs is closed to we reset the desktop
    ;; modification time (in this way the user does not get a warning
    ;; message about desktop modifications)
    (add-hook 'kill-emacs-hook
              (lambda ()
                ;; Reset desktop modification time so the user is not bothered
                (setq desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name))))))
    )
  )

(add-hook 'after-init-hook
          'desktop-settings-setup
          (lambda ()
            ;; No splash screen
            (setq inhibit-startup-screen t)

            ;; If the *scratch* buffer is the current one, then create a new
            ;; empty untitled buffer to hide *scratch*
            (if (string= (buffer-name) "*scratch*")
                (new-empty-buffer))
            )
          t) ;; append this hook to the tail

;; seems pointless to warn. There's always undo.
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
