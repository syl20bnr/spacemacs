;;; config.el --- Spacemacs Base Layer configuration File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Export global environment variables.
(shell-command-to-string "eval $(cat /etc/environment | sed 's/^/export /')")

(defconst docker-spacemacs--default-dump-layer-data-file-path
  "/tmp/docker-spacemacs-layer-data.el")

;; fix Xpra + Emacs compatibility http://xpra.org/trac/ticket/1327
(setq frame-resize-pixelwise t)

;; Start spacemacs in the workspace
(setq default-directory (concat (getenv "WORKSPACE") "/"))
