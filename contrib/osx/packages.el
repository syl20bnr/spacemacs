(setq osx-packages
  '(
    pbcopy
    ))

(if (executable-find "gls")
    ;; maybe absolute or relative name of the `ls' program used by
    ;; `insert-directory'.
    ;; brew info coreutils
    (setq insert-directory-program "gls"
          dired-listing-switches "-aBhl --group-directories-first")
  (setq dired-use-ls-dired nil))

;; use trash if installed
(if (executable-find "trash")
    (defun system-move-file-to-trash (file)
      "Use `trash' to move FILE to the system trash.
Can be installed with `brew install trash', or `brew install osxutils`''."
      (call-process (executable-find "trash") nil 0 nil file))
  ;; regular move to trash directory
  (setq trash-directory "~/.Trash/emacs"))

(defun osx/init-pbcopy ()
  (use-package pbcopy
    :if (not (display-graphic-p))
    :init (turn-on-pbcopy)))
