(load (concat user-emacs-directory "core/spacemacs-mode.el"))
(require 'config-system)

;; spacemacs special buffer
(spacemacs/buffer)

(unless (not (spacemacs/emacs-version-ok))

  (config-system/package.el-initialize)

  ;; Initializing configuration from ~/.spacemacs
  (dotspacemacs/load)
  (dotspacemacs|call-func dotspacemacs/init)

  ;; Load configuration layers
  (config-system/declare-layer 'spacemacs)
  (config-system/declare-dotspacemacs-configuration-layers)
  (config-system/load-layers)
  (config-system/delete-orphan-packages)

  ;; Ultimate configuration decisions are given to the user who can defined
  ;; them in his/her ~/.spacemacs file
  (dotspacemacs|call-func dotspacemacs/config)
  (config-system/setup-after-init-hook)

  ;; start a server for subsequent emacs clients
  (require 'server)
  (unless (server-running-p)
    (server-start)))
