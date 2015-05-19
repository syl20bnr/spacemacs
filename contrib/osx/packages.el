(setq osx-packages
  '(
    osx-trash
    pbcopy
    ))


(if (executable-find "gls")
    ;; maybe absolute or relative name of the `ls' program used by
    ;; `insert-directory'.
    ;; brew info coreutils
    (setq insert-directory-program "gls"
          dired-listing-switches "-aBhl --group-directories-first")
  (setq dired-use-ls-dired nil))

(defun osx/init-osx-trash ()
  (use-package osx-trash
    :init
    (progn
      (osx-trash-setup)
      (setq delete-by-moving-to-trash t))))

(defun osx/init-pbcopy ()
  (use-package pbcopy
    :if (not (display-graphic-p))
    :init (turn-on-pbcopy)))
