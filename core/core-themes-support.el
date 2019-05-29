;;; core-themes-support.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst emacs-built-in-themes (cons 'default (custom-available-themes))
  "List of emacs built-in themes")

(defvar spacemacs--fallback-theme 'spacemacs-dark
  "Fallback theme if user theme cannot be applied.")

(defvar spacemacs--delayed-user-theme nil
  "Internal variable storing user theme to be installed.")

(defvar spacemacs--cur-theme nil
  "Internal variable storing currently loaded theme.")

(defface org-kbd
  '((t (:background "LemonChiffon1" :foreground "black" :box
                    (:line-width 2 :color nil :style released-button))))
  "Face for displaying key bindings in Spacemacs documents."
  :group 'org-faces)

(defconst spacemacs-theme-name-to-package
  '(
    (alect-black-alt                  . alect-themes)
    (alect-black                      . alect-themes)
    (alect-dark-alt                   . alect-themes)
    (alect-dark                       . alect-themes)
    (alect-light-alt                  . alect-themes)
    (alect-light                      . alect-themes)
    (ample-light                      . ample-theme)
    (ample-flat                       . ample-theme)
    (apropospriate-light              . apropospriate-theme)
    (apropospriate-dark               . apropospriate-theme)
    (base16-3024                      . base16-theme)
    (base16-apathy                    . base16-theme)
    (base16-ashes                     . base16-theme)
    (base16-atelier-cave-light        . base16-theme)
    (base16-atelier-cave              . base16-theme)
    (base16-atelier-dune-light        . base16-theme)
    (base16-atelier-dune              . base16-theme)
    (base16-atelier-estuary-light     . base16-theme)
    (base16-atelier-estuary           . base16-theme)
    (base16-atelier-forest-light      . base16-theme)
    (base16-atelier-forest            . base16-theme)
    (base16-atelier-heath-light       . base16-theme)
    (base16-atelier-heath             . base16-theme)
    (base16-atelier-lakeside-light    . base16-theme)
    (base16-atelier-lakeside          . base16-theme)
    (base16-atelier-plateau-light     . base16-theme)
    (base16-atelier-plateau           . base16-theme)
    (base16-atelier-savanna-light     . base16-theme)
    (base16-atelier-savanna           . base16-theme)
    (base16-atelier-seaside-light     . base16-theme)
    (base16-atelier-seaside           . base16-theme)
    (base16-atelier-sulphurpool-light . base16-theme)
    (base16-atelier-sulphurpool       . base16-theme)
    (base16-bespin                    . base16-theme)
    (base16-brewer                    . base16-theme)
    (base16-bright                    . base16-theme)
    (base16-brushtrees                . base16-theme)
    (base16-brushtrees-dark           . base16-theme)
    (base16-chalk                     . base16-theme)
    (base16-circus                    . base16-theme)
    (base16-classic-dark              . base16-theme)
    (base16-classic-light             . base16-theme)
    (base16-codeschool                . base16-theme)
    (base16-cupcake                   . base16-theme)
    (base16-cupertino                 . base16-theme)
    (base16-darktooth                 . base16-theme)
    (base16-default-dark              . base16-theme)
    (base16-default-light             . base16-theme)
    (base16-dracula                   . base16-theme)
    (base16-eighties                  . base16-theme)
    (base16-embers                    . base16-theme)
    (base16-flat                      . base16-theme)
    (base16-github                    . base16-theme)
    (base16-google-dark               . base16-theme)
    (base16-google-light              . base16-theme)
    (base16-grayscale-dark            . base16-theme)
    (base16-grayscale-light           . base16-theme)
    (base16-greenscreen               . base16-theme)
    (base16-gruvbox-dark-hard         . base16-theme)
    (base16-gruvbox-dark-medium       . base16-theme)
    (base16-gruvbox-dark-pale         . base16-theme)
    (base16-gruvbox-dark-soft         . base16-theme)
    (base16-gruvbox-light-hard        . base16-theme)
    (base16-gruvbox-light-medium      . base16-theme)
    (base16-gruvbox-light-soft        . base16-theme)
    (base16-harmonic-dark             . base16-theme)
    (base16-harmonic-light            . base16-theme)
    (base16-hopscotch                 . base16-theme)
    (base16-irblack                   . base16-theme)
    (base16-isotope                   . base16-theme)
    (base16-london-tube               . base16-theme)
    (base16-macintosh                 . base16-theme)
    (base16-marrakesh                 . base16-theme)
    (base16-materia                   . base16-theme)
    (base16-material-darker           . base16-theme)
    (base16-material-lighter          . base16-theme)
    (base16-material-palenight        . base16-theme)
    (base16-material                  . base16-theme)
    (base16-mellow-purple             . base16-theme)
    (base16-mexico-light              . base16-theme)
    (base16-mocha                     . base16-theme)
    (base16-monokai                   . base16-theme)
    (base16-nord                      . base16-theme)
    (base16-ocean                     . base16-theme)
    (base16-oceanicnext               . base16-theme)
    (base16-onedark                   . base16-theme)
    (base16-one-light                 . base16-theme)
    (base16-paraiso                   . base16-theme)
    (base16-phd                       . base16-theme)
    (base16-pico                      . base16-theme)
    (base16-pop                       . base16-theme)
    (base16-porple                    . base16-theme)
    (base16-railscasts                . base16-theme)
    (base16-rebecca                   . base16-theme)
    (base16-seti                      . base16-theme)
    (base16-seti-ui                   . base16-theme)
    (base16-shapeshifter              . base16-theme)
    (base16-solarflare                . base16-theme)
    (base16-solarized-dark            . base16-theme)
    (base16-solarized-light           . base16-theme)
    (base16-spacemacs                 . base16-theme)
    (base16-summerfruit-dark          . base16-theme)
    (base16-summerfruit-light         . base16-theme)
    (base16-tomorrow-night            . base16-theme)
    (base16-tomorrow                  . base16-theme)
    (base16-tube                      . base16-theme)
    (base16-twilight                  . base16-theme)
    (base16-unikitty-dark             . base16-theme)
    (base16-unikitty-light            . base16-theme)
    (base16-woodland                  . base16-theme)
    (base16-xcode-dusk                . base16-theme)
    (base16-zenburn                   . base16-theme)
    (sanityinc-solarized-dark         . color-theme-sanityinc-solarized)
    (sanityinc-solarized-light        . color-theme-sanityinc-solarized)
    (sanityinc-tomorrow-blue          . color-theme-sanityinc-tomorrow)
    (sanityinc-tomorrow-bright        . color-theme-sanityinc-tomorrow)
    (sanityinc-tomorrow-day           . color-theme-sanityinc-tomorrow)
    (sanityinc-tomorrow-eighties      . color-theme-sanityinc-tomorrow)
    (sanityinc-tomorrow-night         . color-theme-sanityinc-tomorrow)
    (doom-challenger-deep             . doom-themes)
    (doom-city-lights                 . doom-themes)
    (doom-dracula                     . doom-themes)
    (doom-molokai                     . doom-themes)
    (doom-mono-dark                   . doom-themes)
    (doom-mono-light                  . doom-themes)
    (doom-nord                        . doom-themes)
    (doom-nord-light                  . doom-themes)
    (doom-nova                        . doom-themes)
    (doom-one                         . doom-themes)
    (doom-one-light                   . doom-themes)
    (doom-opera                       . doom-themes)
    (doom-opera-light                 . doom-themes)
    (doom-peacock                     . doom-themes)
    (doom-spacegrey                   . doom-themes)
    (doom-sourcerer                   . doom-themes)
    (doom-solarized-light             . doom-themes)
    (doom-tomorrow-day                . doom-themes)
    (doom-tomorrow-night              . doom-themes)
    (doom-tron                        . doom-themes)
    (doom-vibrant                     . doom-themes)
    (doom-x                           . doom-themes)
    (solarized-light                  . solarized-theme)
    (solarized-dark                   . solarized-theme)
    (spacemacs-light                  . spacemacs-theme)
    (spacemacs-dark                   . spacemacs-theme)
    (colorsarenice-dark               . colorsarenice-theme)
    (colorsarenice-light              . colorsarenice-theme)
    (hemisu-dark                      . hemisu-theme)
    (hemisu-light                     . hemisu-theme)
    (majapahit-dark                   . majapahit-theme)
    (majapahit-light                  . majapahit-theme)
    (material-light                   . material-theme)
    (minimal-light                    . minimal-theme)
    (moe-dark                         . moe-theme)
    (moe-light                        . moe-theme)
    (stekene-dark                     . stekene-theme)
    (stekene-light                    . stekene-theme)
    (brin                             . sublime-themes)
    (dorsey                           . sublime-themes)
    (fogus                            . sublime-themes)
    (graham                           . sublime-themes)
    (granger                          . sublime-themes)
    (hickey                           . sublime-themes)
    (junio                            . sublime-themes)
    (mccarthy                         . sublime-themes)
    (odersky                          . sublime-themes)
    (omtose-darker                    . omtose-phellack-theme)
    (omtose-softer                    . omtose-phellack-theme)
    (ritchie                          . sublime-themes)
    (spolsky                          . sublime-themes)
    (wilson                           . sublime-themes)
    (zonokai-blue                     . zonokai-theme)
    (zonokai-red                      . zonokai-theme)
    (tao-yin                          . tao-theme)
    (tao-yang                         . tao-theme)
    (farmhouse-light                  . farmhouse-theme)
    (farmhouse-dark                   . farmhouse-theme)
    (gruvbox-dark-soft                . gruvbox-theme)
    (gruvbox-dark-medium              . gruvbox-theme)
    (gruvbox-dark-hard                . gruvbox-theme)
    (gruvbox-light-soft               . gruvbox-theme)
    (gruvbox-light-medium             . gruvbox-theme)
    (gruvbox-light-hard               . gruvbox-theme)
    (kaolin-dark                      . kaolin-themes)
    (kaolin-light                     . kaolin-themes)
    (kaolin-aurora                    . kaolin-themes)
    (kaolin-breeze                    . kaolin-themes)
    (kaolin-bubblegum                 . kaolin-themes)
    (kaolin-eclipse                   . kaolin-themes)
    (kaolin-galaxy                    . kaolin-themes)
    (kaolin-mono-dark                 . kaolin-themes)
    (kaolin-ocean                     . kaolin-themes)
    (kaolin-temple                    . kaolin-themes)
    (kaolin-valley-dark               . kaolin-themes)
    (kaolin-valley-light              . kaolin-themes)
    (eziam-light                      . eziam-theme)
    (eziam-dark                       . eziam-theme)
    (eziam-dusk                       . eziam-theme)
    )
  "alist matching a theme name with its package name, required when
package name does not match theme name + `-theme' suffix.")

(defvar spacemacs-post-theme-change-hook nil
  "Hook run after theme has changed.")

(defun spacemacs/get-theme-package-name (theme-name)
  "Returns the package theme for the given THEME name."
  (cond
   ;; built-in
   ((memq theme-name emacs-built-in-themes) nil)
   ;; from explicit alist
   ((assq theme-name spacemacs-theme-name-to-package)
    (cdr (assq theme-name spacemacs-theme-name-to-package)))
   ;; fallback to <name>-theme
   (t (intern (format "%S-theme" theme-name)))))

(defun spacemacs//get-theme-name (theme)
  "Return the name of THEME."
  (if (listp theme)
      (car theme)
    theme))

(defun spacemacs//get-theme-package-directory (theme)
  "Return the THEME location on disk."
  (let* ((theme-name (spacemacs//get-theme-name theme))
         (pkg-name (spacemacs/get-theme-package-name theme-name))
         (dir (when (listp theme)
                (configuration-layer/get-location-directory
                 pkg-name
                 (plist-get (cdr theme) :location)
                 'dotfile))))
    (unless dir
      ;; fallback to elpa directory
      (setq dir (configuration-layer/get-elpa-package-install-directory
                 pkg-name)))
    dir))

(defun spacemacs/load-default-theme (&optional fallback-theme disable)
  "Load default theme.
Default theme is the car of `dotspacemacs-themes'.
If FALLBACK-THEME is non-nil it must be a package name which will be loaded if
THEME cannot be applied."
  (spacemacs/load-theme (car dotspacemacs-themes) fallback-theme disable))

(defun spacemacs/load-theme (theme &optional fallback-theme disable)
  "Apply user theme.
If FALLBACK-THEME is non-nil it must be a package name which will be loaded if
THEME cannot be applied.
If DISABLE is non-nil then disable all previously applied themes before applying
THEME."
  (let ((theme-name (spacemacs//get-theme-name theme)))
    (condition-case err
        (progn
          ;; Load theme
          (unless (or (memq theme-name (custom-available-themes))
                      (eq 'default theme-name))
            (let ((pkg-dir (spacemacs//get-theme-package-directory theme))
                  (pkg-name (spacemacs/get-theme-package-name theme-name)))
              (when pkg-dir
                ;; package activate should be enough, but not all themes
                ;; have add themselves to `custom-theme-load-path' in autoload.
                ;; (for example, moe-theme).
                (add-to-list 'custom-theme-load-path pkg-dir)
                (package-activate pkg-name))))
          (when disable
            (mapc 'disable-theme custom-enabled-themes))
          (unless (eq 'default theme-name)
            (load-theme theme-name t))
          (unless (display-graphic-p)
            (eval `(spacemacs|do-after-display-system-init
                    (load-theme ',theme-name t))))
          (setq-default spacemacs--cur-theme theme-name))
      ('error
       (message "error: %s" err)
       (if fallback-theme
           ;; fallback to Spacemacs default theme
           (progn
             (setq spacemacs--delayed-user-theme theme-name)
             (spacemacs/load-fallback-theme fallback-theme disable))
         ;; no fallback theme was specified, so we log explicit warning
         (spacemacs-buffer/warning
          (concat "An error occurred while applying "
                  "the theme \"%s\", fallback on theme \"%s\". \n"
                  "Error was: %s")
          theme-name spacemacs--fallback-theme err)
         (spacemacs-buffer/warning
          (concat "Please check the value of \"dotspacemacs-themes\" in your "
                  "dotfile or open an issue \n"
                  "so we can add support for the theme \"%s\".")
          theme-name))))))

(defun spacemacs/load-fallback-theme (theme &optional disable)
  "Apply the fallback theme.
If DISABLE is non-nil then disable all previously applied themes before applying
THEME."
  (let ((theme-name (spacemacs//get-theme-name theme)))
    ;; pop up fallback theme to the top of the list
    (setq spacemacs--cur-theme theme-name)
    (setq dotspacemacs-themes (delq theme-name dotspacemacs-themes))
    (add-to-list 'dotspacemacs-themes theme-name)
    (when disable
      (mapc 'disable-theme custom-enabled-themes))
    (load-theme theme-name t)
    (unless (display-graphic-p)
      (eval `(spacemacs|do-after-display-system-init
              (load-theme ',theme-name t))))))

(defun spacemacs/cycle-spacemacs-theme (&optional backward)
  "Cycle through themes defined in `dotspacemacs-themes'.
When BACKWARD is non-nil, or with universal-argument, cycle backwards."
  (interactive "P")
  (let* ((themes (if backward (reverse dotspacemacs-themes) dotspacemacs-themes))
         (next-theme (car (or (cdr (memq spacemacs--cur-theme themes))
                              ;; if current theme isn't in cycleable themes, start
                              ;; over
                              themes))))
    (when spacemacs--cur-theme
      (disable-theme spacemacs--cur-theme))
    (let ((progress-reporter
           (make-progress-reporter
            (format "Loading theme %s..." next-theme))))
      (spacemacs/load-theme next-theme nil 'disable)
      (progress-reporter-done progress-reporter))))

(defun spacemacs/cycle-spacemacs-theme-backward ()
  "Cycle through themes defined in `dotspacemacs-themes' backward."
  (interactive)
  (spacemacs/cycle-spacemacs-theme t))

(defadvice load-theme (after spacemacs/load-theme-adv activate)
  "Perform post load processing."
  (let ((theme (ad-get-arg 0)))
    ;; Without this a popup is raised every time emacs25 starts up for
    ;; assignment to a free variable
    (with-no-warnings
      (setq spacemacs--cur-theme theme))
    (spacemacs/post-theme-init theme)))

(defun spacemacs/post-theme-init (theme)
  "Some processing that needs to be done when the current theme
has been changed to THEME."
  (interactive)
  (run-hooks 'spacemacs-post-theme-change-hook))

(defun spacemacs//add-theme-packages-to-additional-packages ()
  "Add all theme packages from `dotspacemacs-themes' to packages to install."
  (setq dotspacemacs--additional-theme-packages nil)
  (dolist (theme dotspacemacs-themes)
    (let* ((theme-name (spacemacs//get-theme-name theme))
           (pkg-name (spacemacs/get-theme-package-name theme-name))
           (theme2 (copy-tree theme)))
      (when pkg-name
        (if (listp theme2)
            (setcar theme2 pkg-name)
          (setq theme2 pkg-name))
        (add-to-list 'dotspacemacs--additional-theme-packages theme2)))))
(add-hook 'configuration-layer-pre-load-hook
          'spacemacs//add-theme-packages-to-additional-packages)

(provide 'core-themes-support)
