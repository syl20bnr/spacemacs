;;; core-themes-support.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
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

(defvar spacemacs--default-user-theme nil
  "Internal variable storing user theme to be installed.")

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
    (doom-molokai                     . doom-themes)
    (doom-mono-dark                   . doom-themes)
    (doom-mono-light                  . doom-themes)
    (doom-nova                        . doom-themes)
    (doom-one                         . doom-themes)
    (doom-one-light                   . doom-themes)
    (doom-peacock                     . doom-themes)
    (doom-spacegrey                   . doom-themes)
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
    )
  "alist matching a theme name with its package name, required when
package name does not match theme name + `-theme' suffix.")

(defvar spacemacs-post-theme-change-hook nil
  "Hook run after theme has changed.")

(defun spacemacs//get-theme-package (theme)
  "Returns the package theme for the given THEME name."
  (cond
   ;; built-in
   ((memq theme emacs-built-in-themes) nil)
   ;; from explicit alist
   ((assq theme spacemacs-theme-name-to-package)
    (cdr (assq theme spacemacs-theme-name-to-package)))
   ;; fallback to <name>-theme
   (t (intern (format "%S-theme" theme)))))

(defun spacemacs/load-theme (theme &optional install)
  "Load THEME.
 If INSTALL is non-nil then attempt to install the theme."
  ;; Required dependencies for some themes
  (condition-case err
      (progn
        (when install
          (spacemacs-buffer/append
           (format "--> Installing user theme: %s..."
                   spacemacs--default-user-theme))
          (redisplay))
        ;; Load theme
        (when (or (memq theme '(zonokai-blue
                                zonokai-red
                                solarized-light
                                solarized-dark
                                doom-one
                                doom-molokai)))
          (configuration-layer/load-or-install-package 'dash install))
        ;; Unless Emacs stock themes
        (unless (or (memq theme (custom-available-themes))
                    (eq 'default theme))
          (cond
           ;; themes with explicitly declared package names
           ((assq theme spacemacs-theme-name-to-package)
            (let* ((pkg (spacemacs//get-theme-package theme))
                   (pkg-dir (configuration-layer/load-or-install-package
                             pkg install)))
              (when (or (eq 'moe-light theme)
                        (eq 'moe-dark theme))
                (load-file (concat pkg-dir "moe-light-theme.el"))
                (load-file (concat pkg-dir "moe-dark-theme.el")))
              (when pkg-dir
                (add-to-list 'custom-theme-load-path pkg-dir))))
           (t
            ;; other themes
            ;; we assume that the package name is suffixed with `-theme'
            ;; if not we will handle the special themes as we get issues
            ;; in the tracker.
            (let ((pkg (spacemacs//get-theme-package theme)))
              (configuration-layer/load-or-install-package pkg install)))))
        ;; Apply theme
        (mapc 'disable-theme custom-enabled-themes)
        ;; explicitly reload the theme for the first GUI client
        (eval `(spacemacs|do-after-display-system-init
                (load-theme ',theme t)))
        (unless (display-graphic-p)
          (load-theme theme t))
        (when install
          (spacemacs-buffer/replace-last-line
           (format (concat "--> User theme \"%s\" has been applied, you may "
                           "have to restart Emacs.\n")
                   spacemacs--default-user-theme))
          (redisplay)))
    ('error
     (if install
         (progn
           (spacemacs-buffer/warning
            (concat "An error occurred while applying "
                    "the theme \"%s\", fallback on theme \"%s\". \n"
                    "Error was: %s") theme spacemacs--fallback-theme err)
           (spacemacs-buffer/warning
            (concat "Please check the value of \"dotspacemacs-themes\" in your "
                    "dotfile or open an issue \n"
                    "so we can add support for the theme \"%s\".") theme)
           (unless (display-graphic-p)
             (eval `(spacemacs|do-after-display-system-init
                     (load-theme ',spacemacs--fallback-theme t)))))
       (throw 'error)))))

(defun spacemacs/cycle-spacemacs-theme ()
  "Cycle through themes defined in `dotspacemacs-themes.'"
  (interactive)
  (when spacemacs--cur-theme
    (disable-theme spacemacs--cur-theme)
    ;; if current theme isn't in cycleable themes, start over
    (setq spacemacs--cycle-themes
          (or (cdr (memq spacemacs--cur-theme dotspacemacs-themes))
              dotspacemacs-themes)))
  (setq spacemacs--cur-theme (pop spacemacs--cycle-themes))
  (let ((progress-reporter
         (make-progress-reporter
          (format "Loading theme %s..." spacemacs--cur-theme))))
    (spacemacs/load-theme spacemacs--cur-theme)
    (progress-reporter-done progress-reporter)))

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

(provide 'core-themes-support)
