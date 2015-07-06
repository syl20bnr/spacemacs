(setq osx-packages
  '(
    pbcopy
    launchctl
    reveal-in-finder
    ))

(if (and (system-is-mac) (executable-find "gls")) 
    ;; maybe absolute or relative name of the `ls' program used by
    ;; `insert-directory'.
    ;; brew info coreutils
    (setq insert-directory-program "gls"
          dired-listing-switches "-aBhl --group-directories-first"))

(defun osx/init-pbcopy ()
  (use-package pbcopy
    :if (and (system-is-mac)(not (display-graphic-p))) 
    :init (turn-on-pbcopy)))

(defun osx/init-launchctl ()
  (use-package launchctl
    :if (system-is-mac)
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

(defun osx/init-reveal-in-finder ()
  (use-package reveal-in-finder
    :if (system-is-mac)
    :commands reveal-in-finder))
