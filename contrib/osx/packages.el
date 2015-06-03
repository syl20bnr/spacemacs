(setq osx-packages
  '(
    pbcopy
    reveal-in-finder
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

(defun osx/init-reveal-in-finder ()
  (use-package reveal-in-finder
    :defer t
    :init
    (progn
      (evil-leader/set-key
        "bf" 'reveal-in-finder))))
