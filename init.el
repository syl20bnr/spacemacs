;; from jwiegley
;; https://github.com/jwiegley/dot-emacs/blob/master/init.el
(setq message-log-max 16384)
(defconst emacs-start-time (current-time))

(defconst user-home-directory
  (expand-file-name (concat user-emacs-directory "../"))
  "User home directory (~/).")
(defconst contrib-config-directory
  (expand-file-name (concat user-emacs-directory "contrib/"))
  "Contribution layers base directory.")
(defvar user-dropbox-directory
  (expand-file-name (concat user-home-directory "Dropbox/"))
  "Dropbox directory.")
;; if you have a dropbox, then ~/Dropbox/emacs is added to load path
(add-to-list 'load-path (concat user-dropbox-directory "emacs/"))

;; User configuration file for Spacemacs: ~/.spacemacs 
(load (concat user-home-directory ".spacemacs"))
(dotspacemacs/init)

;; ---------------------------------------------------------------------------
;; Init Macros
;; ---------------------------------------------------------------------------

(defmacro spacemacs/declare-layer (name &optional contrib)
  "Declare a layer with name NAME. If CONTRIB is non nil then the layer is a
 contribution layer."
  (let ((layer-dir-sym (intern (concat name "-config-directory")))
        (base-dir (if contrib contrib-config-directory
                    user-emacs-directory)))
    `(progn 
       (defconst ,layer-dir-sym
         (expand-file-name (concat ,base-dir (concat ,name "/")))
         ,(concat name " layer directory."))
       (defconst ,(intern (concat name "-extensions-directory"))
         (concat ,layer-dir-sym "extensions/")
         ,(concat name " layer extensions directory."))
       (defconst ,(intern (concat name "-init-extension-directory"))
         (concat ,layer-dir-sym "init-extension/")
         ,(concat name " layer extension initialization directory."))
       (defconst ,(intern (concat name "-init-package-directory"))
         (concat ,layer-dir-sym "init-package/")
         ,(concat name " layer package initialization directory."))
       (defconst ,(intern (concat name "-contribp")) ,contrib))))

(defmacro spacemacs/initialize-layer (name)
  "Initialize the given Spacemacs configuration layer. NAME is the
 configuration layer name."
  (declare (indent 1))
  (let ((base-dir (intern (concat name "-config-directory"))))
    `(progn
       (load (concat ,base-dir "funcs.el"))
       (load (concat ,base-dir "macros.el"))
       (load (concat ,base-dir "extensions.el"))
       (load (concat ,base-dir "packages.el"))
       ;; pre-extensions
       (spacemacs/load-and-initialize-extensions
        ,(intern (concat name "-pre-extensions"))
        ,(intern (concat name "-extensions-directory"))
        ,(intern (concat name "-init-extension-directory")))
       ;; packages
       (spacemacs/install-missing-packages
        ,(intern (concat name "-packages")))
       (spacemacs/initialize-packages
        ,(intern (concat name "-init-package-directory")))
       ;; post extensions
       (spacemacs/load-and-initialize-extensions
        ,(intern (concat name "-post-extensions"))
        ,(intern (concat name "-extensions-directory"))
        ,(intern (concat name "-init-extension-directory")))
       ;; key bindings
       (load (concat ,base-dir "keybindings.el"))
       ;; emacs config
       (load (concat ,base-dir "config.el")))))

;; ---------------------------------------------------------------------------
;; Configuration Layers
;; ---------------------------------------------------------------------------
(spacemacs/declare-layer "spacemacs")
(spacemacs/initialize-layer "spacemacs")

;; for now hardcoded contrib config layers
(spacemacs/declare-layer "syl20bnr" t)
(spacemacs/initialize-layer "syl20bnr")

;; Last configuration decisions are given to the user who can defined them 
;; in ~/.spacemacs
(dotspacemacs/config)

;; ---------------------------------------------------------------------------
;; Post initialization
;; ---------------------------------------------------------------------------
;; from jwiegley
;; https://github.com/jwiegley/dot-emacs/blob/master/init.el
;; Display load times after init.el and after all buffers has been loaded
(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))

