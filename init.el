;;; init.el --- Spacemacs Initialization File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize)

;; Avoid garbage collection during startup.
;; see `SPC h . dotspacemacs-gc-cons' for more info
(setq gc-cons-threshold 402653184 gc-cons-percentage 0.6)
(load-file (concat (file-name-directory load-file-name)
                   "core/core-versions.el"))
(load-file (concat (file-name-directory load-file-name)
                   "core/core-load-paths.el"))

(if (not (version<= spacemacs-emacs-min-version emacs-version))
    (error (concat "Your version of Emacs (%s) is too old. "
                   "Spacemacs requires Emacs version %s or above.")
           emacs-version spacemacs-emacs-min-version)
  (require 'core-spacemacs)
  (configuration-layer/load-lock-file)
  (spacemacs/init)
  (configuration-layer/stable-elpa-download-tarball)
  (configuration-layer/load)
  (spacemacs-buffer/display-startup-note)
  (spacemacs/setup-startup-hook)
  (require 'server)
  (unless (server-running-p) (server-start)))
