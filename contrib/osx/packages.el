(setq osx-packages
  '(
    pbcopy
    reveal-in-finder
    ))

(when (system-is-mac)
  ;; Note: `delete-by-moving-to-trash' is set to true globaly in
  ;; `spacemacs/config.el'
  ;; (setq trash-directory "~/.Trash/emacs") ; bare minimum

  ;; Use `trash' cli tool, if installed.
  ;; See brew info trash (or osx-tools)
  ;; otherwise, enable built-in support for trashing using Finder API

  (if (executable-find "trash")
      (defun system-move-file-to-trash (file)
        "Use `trash' to move FILE to the system/volume trash can.
Can be installed with `brew install trash'." 
        (call-process (executable-find "trash") nil 0 nil file))
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

(defun osx/init-pbcopy ()
  (use-package pbcopy
    :if (and (system-is-mac)(not (display-graphic-p))) 
    :init (turn-on-pbcopy)))

(defun osx/init-reveal-in-finder ()
  (use-package reveal-in-finder
    :if (system-is-mac)
    :commands reveal-in-finder))
