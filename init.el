(load (concat user-emacs-directory "core/spacemacs-mode.el"))
(require 'config-system)

(when (spacemacs/emacs-version-ok)
  (dotspacemacs/load)
  (spacemacs/initialize)
  (config-system/package.el-initialize)
  ;; Initializing configuration from ~/.spacemacs
  (dotspacemacs|call-func dotspacemacs/init)
  ;; synchronize and load configuration layers
  (config-system/declare-layers)
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
