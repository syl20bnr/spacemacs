(defconst spacemacs-themes
  '(
    (base16-chalk . base16-theme)
    (base16-default . base16-theme)
    (base16-eighties . base16-theme)
    (base16-greenscreen . base16-theme)
    (base16-mocha . base16-theme)
    (base16-monokai . base16-theme)
    (base16-ocean . base16-theme)
    (base16-railscasts . base16-theme)
    (base16-solarized . base16-theme)
    (base16-tomorrow . base16-theme)
    (sanityinc-solarized-dark . color-theme-sanityinc-solarized)
    (sanityinc-solarized-light . color-theme-sanityinc-solarized)
    (sanityinc-tomorrow-blue . color-theme-sanityinc-tomorrow)
    (sanityinc-tomorrow-bright . color-theme-sanityinc-tomorrow)
    (sanityinc-tomorrow-day .  color-theme-sanityinc-tomorrow)
    (sanityinc-tomorrow-eighties . color-theme-sanityinc-tomorrow)
    (sanityinc-tomorrow-night . color-theme-sanityinc-tomorrow)
    )
  "alist matching theme name with its package name.")

(defun spacemacs/load-default-theme ()
  "Load the default theme defined in `dotspacemacs-default-theme'"
  ;; Unless Emacs stock themes
  (unless (memq dotspacemacs-default-theme (custom-available-themes))
    (cond
     ;; Spacemacs default theme
     ((or (eq 'solarized-light dotspacemacs-default-theme)
          (eq 'solarized-dark dotspacemacs-default-theme))
      (add-to-list 'load-path (concat spacemacs-directory
                                      "extensions/solarized-theme/"))
      ;; solarized dependency
      (spacemacs/load-or-install-package 'dash)
      (require 'solarized)
      (deftheme solarized-dark "The dark variant of the Solarized colour theme")
      (deftheme solarized-light "The light variant of the Solarized colour theme"))
     ;; Support for all base16 themes
     ((assq dotspacemacs-default-theme spacemacs-themes)
      (let* ((pkg (cdr (assq dotspacemacs-default-theme spacemacs-themes)))
             (pkg-dir (spacemacs/load-or-install-package pkg)))
        (add-to-list 'custom-theme-load-path pkg-dir)))
     (t
      ;; other themes
      ;; we assume that the package name is suffixed with `-theme'
      ;; if not we will handle the special themes as we get issues in the tracker.
      (let ((pkg (format "%s-theme" (symbol-name dotspacemacs-default-theme))))
        (spacemacs/load-or-install-package (intern pkg))))))
  (load-theme dotspacemacs-default-theme t)
  (setq-default spacemacs-cur-theme dotspacemacs-default-theme))

(provide 'themes-support)
