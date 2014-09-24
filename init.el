;; load core source and display spacemacs buffer
(defconst spacemacs-core-directory
  (expand-file-name (concat user-emacs-directory "core/"))
  "Spacemacs core directory.")
(dolist (elisp '("spacemacs-mode.el" "contribsys.el"))
  (load (concat spacemacs-core-directory elisp)))
(spacemacs-buffer)

;; additional paths
(defconst user-home-directory
  (expand-file-name (concat user-emacs-directory "../"))
  "User home directory (~/).")
(defconst spacemacs-contrib-config-directory
  (expand-file-name (concat user-emacs-directory "contrib/"))
  "Spacemacs contribution layers base directory.")
(defconst user-dropbox-directory
  (expand-file-name (concat user-home-directory "Dropbox/"))
  "Dropbox directory.")
;; if you have a dropbox, then ~/Dropbox/emacs is added to load path
(add-to-list 'load-path (concat user-dropbox-directory "emacs/"))

;; User configuration file for Spacemacs: ~/.spacemacs 
(spacemacs-load-dotfile)
(dotspacemacs/init)
;; initialisation of the contribution system based on configuration layers
;; additional configuration layers are declared in ~/.spacemacs
;; in variable dotspacemacs-configuration-layers
(contribsys/declare-layer 'spacemacs)
(contribsys/declare-configuration-layers)
(contribsys/load-layers)

;; Ultimate configuration decisions are given to the user who can defined them 
;; in his/her ~/.spacemacs file
(dotspacemacs/config)
