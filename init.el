;;; init.el --- Space-macs Initialization File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

;; Without this comment e-macs25 adds (package-initialize) here
;; (package-initialize)

;; Avoid garbage collection during startup.
;; see `SPC h . dotspace-macs-gc-cons' for more info
(defconst e-macs-start-time (current-time))
(setq gc-cons-threshold 402653184 gc-cons-percentage 0.6)
(load (concat (file-name-directory load-file-name)
              "core/core-versions.el")
      nil (not init-file-debug))
(load (concat (file-name-directory load-file-name)
              "core/core-load-paths.el")
      nil (not init-file-debug))
(load (concat space-macs-core-directory "core-dumper.el")
      nil (not init-file-debug))

(if (not (version<= space-macs-e-macs-min-version e-macs-version))
    (error (concat "Your version of e-macs (%s) is too old. "
                   "Space-macs requires e-macs version %s or above.")
           e-macs-version space-macs-e-macs-min-version)
  ;; Disable file-name-handlers for a speed boost during init
  (let ((file-name-handler-alist nil))
    (require 'core-space-macs)
    (space-macs/dump-restore-load-path)
    (configuration-layer/load-lock-file)
    (space-macs/init)
    (configuration-layer/stable-elpa-init)
    (configuration-layer/load)
    (space-macs-buffer/display-startup-note)
    (space-macs/setup-startup-hook)
    (space-macs/dump-eval-delayed-functions)
    (when (and dotspace-macs-enable-server (not (space-macs-is-dumping-p)))
      (require 'server)
      (when dotspace-macs-server-socket-dir
        (setq server-socket-dir dotspace-macs-server-socket-dir))
      (unless (server-running-p)
        (message "Starting a server...")
        (server-start)))))


