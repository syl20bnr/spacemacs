;; from jwiegley
;; https://github.com/jwiegley/dot-emacs/blob/master/init.el
(setq message-log-max 16384)
(defconst emacs-start-time (current-time))

;; paths
(defconst user-home-directory
  (expand-file-name (concat user-emacs-directory "../"))
  "User home directory (~/).")
(defconst spacemacs-core-directory
  (expand-file-name (concat user-emacs-directory "core/"))
  "Spacemacs core directory.")
(defconst spacemacs-contrib-config-directory
  (expand-file-name (concat user-emacs-directory "contrib/"))
  "Spacemacs contribution layers base directory.")
(defconst user-dropbox-directory
  (expand-file-name (concat user-home-directory "Dropbox/"))
  "Dropbox directory.")
;; if you have a dropbox, then ~/Dropbox/emacs is added to load path
(add-to-list 'load-path (concat user-dropbox-directory "emacs/"))

;; load core source
(dolist (elisp '("spacemacs-mode.el" "contribsys.el"))
  (load (concat spacemacs-core-directory elisp)))

(spacemacs-buffer)

;; User configuration file for Spacemacs: ~/.spacemacs 
(spacemacs-load-dotfile)
(dotspacemacs/init)

(contribsys/declare-layer 'spacemacs)
(contribsys/declare-configuration-layers)
(contribsys/load-layers)

;; Last configuration decisions are given to the user who can defined them 
;; in ~/.spacemacs
(dotspacemacs/config)

; from jwiegley
;; https://github.com/jwiegley/dot-emacs/blob/master/init.el
;; Display load times after init.el and after all buffers has been loaded
(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))
(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed (float-time
                             (time-subtract (current-time) emacs-start-time))))
               (message "Loading %s...done (%.3fs) [after-init]"
                        ,load-file-name elapsed))) t)
