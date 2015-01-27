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
(defconst spacemacs-version     "0.50.0" "Spacemacs version.")
(defconst spacemacs-min-version   "24.3" "Minimal required version of Emacs.")

(defun spacemacs/emacs-version-ok ()
  (version<= spacemacs-min-version emacs-version))

(when (spacemacs/emacs-version-ok)
  (load-file (concat user-emacs-directory "core/core-load-paths.el"))
  (require 'core-spacemacs-mode)
  (require 'core-configuration-layer)
  (dotspacemacs/load)
  (spacemacs/initialize)
  ;; Initializing configuration from ~/.spacemacs
  (dotspacemacs|call-func dotspacemacs/init)
  ;; synchronize and load configuration layers
  (configuration-layer/declare-all-layers)
  (configuration-layer/load-layers)
  (configuration-layer/delete-orphan-packages)
  ;; Ultimate configuration decisions are given to the user who can defined
  ;; them in his/her ~/.spacemacs file
  (dotspacemacs|call-func dotspacemacs/config)
  (configuration-layer/setup-after-init-hook)
  ;; start a server for subsequent emacs clients
  (require 'server)
  (unless (server-running-p)
    (server-start)))
