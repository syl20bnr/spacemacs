;; spacemacs special buffer
(defconst spacemacs-core-directory
  (expand-file-name (concat user-emacs-directory "core/"))
  "Spacemacs core directory.")
(load (concat spacemacs-core-directory "spacemacs-mode.el"))
(spacemacs/buffer)

(defgroup spacemacs nil
  "Spacemacs customizations."
  :group 'starter-kit
  :prefix 'spacemacs-)

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
  (load (concat spacemacs-core-directory "contribsys.el"))
  ;; User configuration file for Spacemacs: ~/.spacemacs
  (contribsys/load-dotfile)
  (contribsys/call-dotfile-func dotspacemacs/init)
  ;; default configuration layer of spacemacs
  (contribsys/declare-layer 'spacemacs)
  ;; configuration layers coming from `dotspacemacs-configuration-layers'
  (contribsys/discover-contrib-layers)
  (contribsys/declare-user-configuration-layers)
  ;; heavy lifting, load all packages and extensions
  (contribsys/load-layers)
  ;; Temporary fix until automatic orphan packages deletion is ported to
  ;; Emacs 24.4
  (contribsys/delete-orphan-packages)
  ;; Ultimate configuration decisions are given to the user who can defined
  ;; them in his/her ~/.spacemacs file
  (contribsys/call-dotfile-func dotspacemacs/config)
  (contribsys/setup-after-init-hook)

  ;; start a server for subsequent emacs clients
  (require 'server)
  (unless (server-running-p)
    (server-start)))
