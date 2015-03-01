;;; init.el --- Spacemacs Initialization File
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(defconst spacemacs-version           "0.63.0" "Spacemacs version.")
(defconst spacemacs-emacs-min-version   "24.3" "Minimal version of Emacs.")

(defun spacemacs/emacs-version-ok ()
  (version<= spacemacs-emacs-min-version emacs-version))

(when (spacemacs/emacs-version-ok)
  (load-file (concat user-emacs-directory "core/core-load-paths.el"))
  (require 'core-spacemacs-mode)
  (require 'core-configuration-layer)
  (dotspacemacs/load-file)
  ;; initialization
  (dotspacemacs|call-func dotspacemacs/init "Calling dotfile init...")
  (spacemacs/init)
  ;; layers configuration
  (dotspacemacs|call-func dotspacemacs/layers "Calling dotfile layers...")
  (configuration-layer/init-layers)
  (configuration-layer/load-layers)
  (when dotspacemacs-delete-orphan-packages
    (configuration-layer/delete-orphan-packages))
  ;; post initialization
  (configuration-layer/setup-after-init-hook)
  (require 'server)
  (unless (server-running-p) (server-start)))
