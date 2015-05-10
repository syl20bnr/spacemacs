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

(defun osx/init-pbcopy ()
  (use-package pbcopy
    :if (not (display-graphic-p))
    :init (turn-on-pbcopy)))
