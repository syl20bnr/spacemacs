(setq osx-packages
  '(
    launchctl
    pbcopy
    ))

(if (executable-find "gls")
    ;; maybe absolute or relative name of the `ls' program used by
    ;; `insert-directory'.
    ;; brew info coreutils
    (setq insert-directory-program "gls"
          dired-listing-switches "-aBhl --group-directories-first")
  (setq dired-use-ls-dired nil))

(defun osx/init-launchctl ()
  (use-package launchctl
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.plist$" . nxml-mode))
      (evil-leader/set-key "al" 'launchctl))
    :config
    (progn
      ;; evilified mapping
      (evilify launchctl-mode launchctl-mode-map
               (kbd "q") 'quit-window
               (kbd "g") 'launchctl-refresh
               (kbd "n") 'launchctl-new
               (kbd "e") 'launchctl-edit
               (kbd "v") 'launchctl-view
               (kbd "t") 'tabulated-list-sort
               (kbd "l") 'launchctl-load)
               (kbd "u") 'launchctl-unload
               (kbd "r") 'launchctl-reload
               (kbd "s") 'launchctl-start
               (kbd "o") 'launchctl-stop
               (kbd "a") 'launchctl-restart
               (kbd "m") 'launchctl-remove
               (kbd "d") 'launchctl-disable
               (kbd "p") 'launchctl-enable
               (kbd "i") 'launchctl-info
               (kbd "*") 'launchctl-filter
               (kbd "$") 'launchctl-setenv
               (kbd "#") 'launchctl-unsetenv
               (kbd "h") 'launchctl-help
      )))

(defun osx/init-pbcopy ()
  (use-package pbcopy
    :if (not (display-graphic-p))
    :init (turn-on-pbcopy)))
