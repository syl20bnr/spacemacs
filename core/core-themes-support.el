;;; core-themes-support.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst emacs-built-in-themes (cons 'default (custom-available-themes))
  "List of emacs built-in themes")

(defface org-kbd
  '((t (:background "LemonChiffon1" :foreground "black" :box
                    (:line-width 2 :color nil :style released-button))))
  "Face for displaying key bindings in Spacemacs documents."
  :group 'org-faces)

(defconst spacemacs-theme-name-to-package
  '(
    (alect-black-alt . alect-themes)
    (alect-black     . alect-themes)
    (alect-dark-alt  . alect-themes)
    (alect-dark      . alect-themes)
    (alect-light-alt . alect-themes)
    (alect-light     . alect-themes)
    (ample-light . ample-theme)
    (ample-flat  . ample-theme)
    (apropospriate-light . apropospriate-theme)
    (apropospriate-dark  . apropospriate-theme)
    (base16-3024-dark . base16-theme)
    (base16-3024-light . base16-theme)
    (base16-apathy-dark . base16-theme)
    (base16-apathy-light . base16-theme)
    (base16-ashes-dark . base16-theme)
    (base16-ashes-light . base16-theme)
    (base16-ateliercave-dark . base16-theme)
    (base16-ateliercave-light . base16-theme)
    (base16-atelierdune-dark . base16-theme)
    (base16-atelierdune-light . base16-theme)
    (base16-atelierestuary-dark . base16-theme)
    (base16-atelierestuary-light . base16-theme)
    (base16-atelierforest-dark . base16-theme)
    (base16-atelierforest-light . base16-theme)
    (base16-atelierheath-dark . base16-theme)
    (base16-atelierheath-light . base16-theme)
    (base16-atelierlakeside-dark . base16-theme)
    (base16-atelierlakeside-light . base16-theme)
    (base16-atelierplateau-dark . base16-theme)
    (base16-atelierplateau-light . base16-theme)
    (base16-ateliersavanna-dark . base16-theme)
    (base16-ateliersavanna-light . base16-theme)
    (base16-atelierseaside-dark . base16-theme)
    (base16-atelierseaside-light . base16-theme)
    (base16-ateliersulphurpool-dark . base16-theme)
    (base16-ateliersulphurpool-light . base16-theme)
    (base16-bespin-dark . base16-theme)
    (base16-bespin-light . base16-theme)
    (base16-brewer-dark . base16-theme)
    (base16-brewer-light . base16-theme)
    (base16-bright-dark . base16-theme)
    (base16-bright-light . base16-theme)
    (base16-chalk-dark . base16-theme)
    (base16-chalk-light . base16-theme)
    (base16-codeschool-dark . base16-theme)
    (base16-codeschool-light . base16-theme)
    (base16-colors-dark . base16-theme)
    (base16-colors-light . base16-theme)
    (base16-darktooth-dark . base16-theme)
    (base16-darktooth-light . base16-theme)
    (base16-default-dark . base16-theme)
    (base16-default-light . base16-theme)
    (base16-eighties-dark . base16-theme)
    (base16-eighties-light . base16-theme)
    (base16-embers-dark . base16-theme)
    (base16-embers-light . base16-theme)
    (base16-flat-dark . base16-theme)
    (base16-flat-light . base16-theme)
    (base16-google-dark . base16-theme)
    (base16-google-light . base16-theme)
    (base16-grayscale-dark . base16-theme)
    (base16-grayscale-light . base16-theme)
    (base16-greenscreen-dark . base16-theme)
    (base16-greenscreen-light . base16-theme)
    (base16-harmonic16-dark . base16-theme)
    (base16-harmonic16-light . base16-theme)
    (base16-hopscotch-dark . base16-theme)
    (base16-hopscotch-light . base16-theme)
    (base16-irblack-dark . base16-theme)
    (base16-irblack-light . base16-theme)
    (base16-isotope-dark . base16-theme)
    (base16-isotope-light . base16-theme)
    (base16-londontube-dark . base16-theme)
    (base16-londontube-light . base16-theme)
    (base16-marrakesh-dark . base16-theme)
    (base16-marrakesh-light . base16-theme)
    (base16-mocha-dark . base16-theme)
    (base16-mocha-light . base16-theme)
    (base16-monokai-dark . base16-theme)
    (base16-monokai-light . base16-theme)
    (base16-ocean-dark . base16-theme)
    (base16-ocean-light . base16-theme)
    (base16-oceanicnext-dark . base16-theme)
    (base16-oceanicnext-light . base16-theme)
    (base16-paraiso-dark . base16-theme)
    (base16-paraiso-light . base16-theme)
    (base16-phd-dark . base16-theme)
    (base16-phd-light . base16-theme)
    (base16-pop-dark . base16-theme)
    (base16-pop-light . base16-theme)
    (base16-railscasts-dark . base16-theme)
    (base16-railscasts-light . base16-theme)
    (base16-setiui-dark . base16-theme)
    (base16-setiui-light . base16-theme)
    (base16-shapeshifter-dark . base16-theme)
    (base16-shapeshifter-light . base16-theme)
    (base16-solarized-dark . base16-theme)
    (base16-solarized-light . base16-theme)
    (base16-summerfruit-dark . base16-theme)
    (base16-summerfruit-light . base16-theme)
    (base16-tomorrow-dark . base16-theme)
    (base16-tomorrow-light . base16-theme)
    (base16-twilight-dark . base16-theme)
    (base16-twilight-light . base16-theme)
    (base16-yesterday-dark . base16-theme)
    (base16-yesterday-light . base16-theme)
    (base16-yesterdaybright-dark . base16-theme)
    (base16-yesterdaybright-light . base16-theme)
    (base16-yesterdaynight-dark . base16-theme)
    (base16-yesterdaynight-light . base16-theme)
    (sanityinc-solarized-dark    . color-theme-sanityinc-solarized)
    (sanityinc-solarized-light   . color-theme-sanityinc-solarized)
    (sanityinc-tomorrow-blue     . color-theme-sanityinc-tomorrow)
    (sanityinc-tomorrow-bright   . color-theme-sanityinc-tomorrow)
    (sanityinc-tomorrow-day      . color-theme-sanityinc-tomorrow)
    (sanityinc-tomorrow-eighties . color-theme-sanityinc-tomorrow)
    (sanityinc-tomorrow-night    . color-theme-sanityinc-tomorrow)
    (solarized-light . solarized-theme)
    (solarized-dark . solarized-theme)
    (spacemacs-light . spacemacs-theme)
    (spacemacs-dark . spacemacs-theme)
    (colorsarenice-dark  . colorsarenice-theme)
    (colorsarenice-light . colorsarenice-theme)
    (hemisu-dark  . hemisu-theme)
    (hemisu-light . hemisu-theme)
    (majapahit-dark . majapahit-theme)
    (majapahit-light . majapahit-theme)
    (material-light . material-theme)
    (minimal-light . minimal-theme)
    (moe-dark  . moe-theme)
    (moe-light . moe-theme)
    (stekene-dark  . stekene-theme)
    (stekene-light . stekene-theme)
    (brin     . sublime-themes)
    (dorsey   . sublime-themes)
    (fogus    . sublime-themes)
    (graham   . sublime-themes)
    (granger  . sublime-themes)
    (hickey   . sublime-themes)
    (junio    . sublime-themes)
    (mccarthy . sublime-themes)
    (odersky  . sublime-themes)
    (ritchie  . sublime-themes)
    (spolsky  . sublime-themes)
    (wilson   . sublime-themes)
    (zonokai-blue . zonokai-theme)
    (zonokai-red  . zonokai-theme)
    (tao-yin . tao-theme)
    (tao-yang . tao-theme)
    (farmhouse-light . farmhouse-theme)
    (farmhouse-dark . farmhouse-theme)
    )
  "alist matching a theme name with its package name, required when
package name does not match theme name + `-theme' suffix.")

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

(defun spacemacs/load-theme (theme)
  "Load THEME."
  ;; Required dependencies for some themes
  (condition-case err
      (progn
        (when (or (eq 'zonokai-blue theme)
                  (eq 'zonokai-red theme)
                  (eq 'solarized-light theme)
                  (eq 'solarized-dark theme))
          (spacemacs/load-or-install-package 'dash))
        ;; Unless Emacs stock themes
        (unless (or (memq theme (custom-available-themes))
                    (eq 'default theme))
          (cond
           ;; themes with explicitly declared package names
           ((assq theme spacemacs-theme-name-to-package)
            (let* ((pkg (spacemacs//get-theme-package theme))
                   (pkg-dir (spacemacs/load-or-install-package pkg)))
              (when (or (eq 'moe-light theme)
                        (eq 'moe-dark theme))
                (load-file (concat pkg-dir "moe-light-theme.el"))
                (load-file (concat pkg-dir "moe-dark-theme.el")))
              (add-to-list 'custom-theme-load-path pkg-dir)))
           (t
            ;; other themes
            ;; we assume that the package name is suffixed with `-theme'
            ;; if not we will handle the special themes as we get issues
            ;; in the tracker.
            (let ((pkg (spacemacs//get-theme-package theme)))
              (spacemacs/load-or-install-package pkg))))))
    ('error
     (setq theme 'default)
     (display-warning 'spacemacs
                      (format (concat "An error occurred while retrieving the "
                                      "theme, using default theme. (error: %s)")
                              err)
                      :warning)))
  (mapc 'disable-theme custom-enabled-themes)
  (if (eq 'default theme)
      (progn
        (setq spacemacs--cur-theme 'default)
        (spacemacs/post-theme-init 'default))
    (load-theme theme t)
    ;; explicitly reload the theme for the first GUI client
    (eval `(spacemacs|do-after-display-system-init
            (load-theme ',theme t)))))

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
  (message "Loading theme %s..." spacemacs--cur-theme)
  (spacemacs/load-theme spacemacs--cur-theme))

(defadvice load-theme (after spacemacs/load-theme-adv activate)
  "Perform post load processing."
  (let ((theme (ad-get-arg 0)))
    ;; Without this a popup is raised every time emacs25 starts up for
    ;; assignment to a free variable
    (with-no-warnings
      (setq spacemacs--cur-theme theme))
    (spacemacs/post-theme-init theme)))

(defun spacemacs/post-theme-init (theme)
  " Some processing that needs to be done when the current theme has been
changed to THEME."
  (interactive)
  (when (fboundp 'spacemacs/set-state-faces)
    (spacemacs/set-state-faces))
  (when (fboundp 'spacemacs/set-flycheck-mode-line-faces)
    (spacemacs/set-flycheck-mode-line-faces))
  (when (fboundp 'spacemacs/set-new-version-lighter-mode-line-faces)
    (spacemacs/set-new-version-lighter-mode-line-faces))
  (when (fboundp 'spacemacs/defface-micro-state-faces)
    (spacemacs/defface-micro-state-faces))
  (when (fboundp 'spacemacs/customize-powerline-faces)
    (spacemacs/customize-powerline-faces))
  (when (fboundp 'powerline-reset)
    (powerline-reset))
  (when (fboundp 'spacemacs/adaptive-evil-highlight-persist-face)
    (spacemacs/adaptive-evil-highlight-persist-face)))

(provide 'core-themes-support)
