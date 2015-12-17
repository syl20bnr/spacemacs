;;; config.el --- Puppet Layer configuration file for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variables

(spacemacs|defvar-company-backends puppet-mode)

;; Enable ruby-mode for Puppetfile support
(configuration-layer/declare-layer 'ruby)
