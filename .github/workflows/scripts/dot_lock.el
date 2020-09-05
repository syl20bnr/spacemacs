(setq configuration-layer-elpa-archives
      `(("melpa"    . "melpa.org/packages/")
        ("org"      . "orgmode.org/elpa/")
        ("gnu"      . "elpa.gnu.org/packages/")
        ("spacelpa" . ,(concat configuration-layer-stable-elpa-directory
                               "spacelpa-"
                               configuration-layer-stable-elpa-version))
        ("testelpa" . ,(format "%s/../testelpa_mirror"
                               (getenv "GITHUB_WORKSPACE")))))

(setq package-archive-priorities
      '(("testelpa" . 9)
        ("spacelpa" . 8)
        ("melpa"    . 4)
        ("org"      . 2)
        ("gnu"      . 1)))
