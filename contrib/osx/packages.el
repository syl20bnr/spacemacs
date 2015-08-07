(setq osx-packages
  '(
    pbcopy
    reveal-in-osx-finder
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

(defun osx/init-reveal-in-osx-finder ()
  (use-package reveal-in-osx-finder
    :if (system-is-mac)
    :commands reveal-in-osx-finder))
