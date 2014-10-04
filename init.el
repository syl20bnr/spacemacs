;; spacemacs special buffer
(defconst spacemacs-core-directory
  (expand-file-name (concat user-emacs-directory "core/"))
  "Spacemacs core directory.")
(load (concat spacemacs-core-directory "spacemacs-mode.el"))
(spacemacs/buffer)

(unless (not (spacemacs/emacs-version-ok))
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
  (spacemacs/load-dotfile)
  (dotspacemacs/init)
  ;; initialisation of the contribution system based on configuration layers
  ;; additional configuration layers are declared in ~/.spacemacs
  ;; in variable dotspacemacs-configuration-layers
  (load (concat spacemacs-core-directory "contribsys.el"))
  (contribsys/declare-layer 'spacemacs)
  (contribsys/declare-configuration-layers)
  (contribsys/load-layers)
  ;; Ultimate configuration decisions are given to the user who can defined
  ;; them in his/her ~/.spacemacs file
  (dotspacemacs/config))
