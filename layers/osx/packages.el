(setq osx-packages
  '(
    osx-trash
    pbcopy
    launchctl
    reveal-in-osx-finder
    ))

(when (spacemacs/system-is-mac)
  ;; Note: `delete-by-moving-to-trash' is set to true globaly in
  ;; `spacemacs/config.el'
  ;; (setq trash-directory "~/.Trash/emacs") ; bare minimum

  ;; Use `trash' cli tool, if installed.
  ;; See brew info trash (or osx-tools)
  ;; otherwise, enable built-in support for trashing using Finder API

  ;; Enable built-in trash support via finder API if available (only on Emacs
  ;; Mac Port)
  (when (boundp 'mac-system-move-file-to-trash-use-finder)
    (setq mac-system-move-file-to-trash-use-finder t))

  ;; Use `gls' if `coreutils' was installed prefixed ('g') otherwise, leave
  ;; alone. Manually add to config `(setq dired-use-ls-dired nil)' to surpesss
  ;; warnings, when not using `coreutils' version of 'ls' on OS X.
  ;; See brew info coreutils
  (when (executable-find "gls")
    ;; maybe absolute or relative name of the `ls' program used by
    ;; `insert-directory'.
    (setq insert-directory-program "gls"
          dired-listing-switches "-aBhl --group-directories-first")
    ))

(defun osx/init-osx-trash ()
  (use-package osx-trash
    :if (and (spacemacs/system-is-mac)
             (not (boundp 'mac-system-move-file-to-trash-use-finder)))
    :init (osx-trash-setup)))

(defun osx/init-pbcopy ()
  (use-package pbcopy
    :if (and (spacemacs/system-is-mac) (not (display-graphic-p)))
    :init (turn-on-pbcopy)))

(defun osx/init-launchctl ()
  (use-package launchctl
    :if (spacemacs/system-is-mac)
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.plist$" . nxml-mode))
      (evil-leader/set-key "al" 'launchctl))
    :config
    (progn
      (evilify launchctl-mode launchctl-mode-map
               (kbd "q") 'quit-window
               (kbd "s") 'tabulated-list-sort
               (kbd "g") 'launchctl-refresh
               (kbd "n") 'launchctl-new
               (kbd "e") 'launchctl-edit
               (kbd "v") 'launchctl-view
               (kbd "l") 'launchctl-load
               (kbd "u") 'launchctl-unload
               (kbd "r") 'launchctl-reload
               (kbd "S") 'launchctl-start
               (kbd "K") 'launchctl-stop
               (kbd "R") 'launchctl-restart
               (kbd "D") 'launchctl-remove
               (kbd "d") 'launchctl-disable
               (kbd "E") 'launchctl-enable
               (kbd "i") 'launchctl-info
               (kbd "f") 'launchctl-filter
               (kbd "=") 'launchctl-setenv
               (kbd "#") 'launchctl-unsetenv
               (kbd "h") 'launchctl-help))))

(defun osx/init-reveal-in-osx-finder ()
  (use-package reveal-in-osx-finder
    :if (spacemacs/system-is-mac)
    :commands reveal-in-osx-finder))
