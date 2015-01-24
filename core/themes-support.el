;;; themes-support.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst spacemacs-theme-name-to-package
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
  "alist matching a theme name with its package name, required when
package name does not match theme name + `-theme' suffix.")

(defun spacemacs/load-theme (theme)
  "Load THEME."
  ;; Unless Emacs stock themes
  (unless (memq theme (custom-available-themes))
    (cond
     ;; solarized theme, official spacemacs theme
     ((or (eq 'solarized-light theme)
          (eq 'solarized-dark theme))
      (add-to-list 'load-path (concat spacemacs-directory
                                      "extensions/solarized-theme/"))
      ;; solarized dependency
      (spacemacs/load-or-install-package 'dash)
      (require 'solarized)
      (deftheme solarized-dark "The dark variant of the Solarized colour theme")
      (deftheme solarized-light "The light variant of the Solarized colour theme"))
     ;; themes with explicitly declared package names
     ((assq theme spacemacs-theme-name-to-package)
      (let* ((pkg (cdr (assq theme spacemacs-theme-name-to-package)))
             (pkg-dir (spacemacs/load-or-install-package pkg)))
        (add-to-list 'custom-theme-load-path pkg-dir)))
     (t
      ;; other themes
      ;; we assume that the package name is suffixed with `-theme'
      ;; if not we will handle the special themes as we get issues in the tracker.
      (let ((pkg (format "%s-theme" (symbol-name theme))))
        (spacemacs/load-or-install-package (intern pkg))))))
  (load-theme theme t))

(defun spacemacs/cycle-spacemacs-theme ()
  "Cycle through themes defined in `dotspacemacs-themes.'"
  (interactive)
  (when  spacemacs--cur-theme
    (disable-theme  spacemacs--cur-theme)
    (setq spacemacs--cycle-themes
          (append spacemacs--cycle-themes (list spacemacs--cur-theme))))
  (setq  spacemacs--cur-theme (pop spacemacs--cycle-themes))
  (message "Loading theme %s..." spacemacs--cur-theme)
  (spacemacs/load-theme spacemacs--cur-theme))

(defadvice load-theme (after spacemacs/load-theme-adv activate)
  "Perform post load processing."
  (let ((theme (ad-get-arg 0)))
    (setq spacemacs--cur-theme theme)
    (spacemacs/post-theme-init theme)))

(defun spacemacs/post-theme-init (theme)
  " Some processing that needs to be done when the current theme has been
changed to THEME."
  (interactive)
      ;; Define a face for each state
  (if (fboundp 'spacemacs/set-state-faces)
      (spacemacs/set-state-faces))
  (if (fboundp 'spacemacs/set-flycheck-mode-line-faces)
      (spacemacs/set-flycheck-mode-line-faces))
  (if (fboundp 'spacemacs/set-new-version-lighter-mode-line-faces)
      (spacemacs/set-new-version-lighter-mode-line-faces))
  (if (fboundp 'powerline-reset)
      (powerline-reset)))

(provide 'themes-support)
