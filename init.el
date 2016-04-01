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

(add-to-list 'load-path (concat user-emacs-directory "core/"))

(require 'spacemacs-mode)
(require 'configuration-layer)

(when (spacemacs/emacs-version-ok)
  (dotspacemacs/load)
  (spacemacs/initialize)
  ;; Initializing configuration from ~/.spacemacs
  (dotspacemacs|call-func dotspacemacs/init)
  ;; synchronize and load configuration layers
  (configuration-layer/declare-layers)
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
