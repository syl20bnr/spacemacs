(defvar git-enable-github-support nil
  "If non nil the Github packages and extensions are enabled.")

(setq git/key-binding-prefixes '(("gh" . "smeargle")))
(when git-enable-github-support
  (push (cons "gf" "file") git/key-binding-prefixes)
  (push (cons "gg" "gist") git/key-binding-prefixes))
(mapc (lambda (x) (spacemacs/declare-prefix (car x) (cdr x)))
      git/key-binding-prefixes)
