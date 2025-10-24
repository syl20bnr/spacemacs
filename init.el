;;; init.el --- Spacemacs Initialization File -*- no-byte-compile: t; lexical-binding: nil; -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; ---------------------------------------------------------------------------
;; * Startup Optimization
;; ---------------------------------------------------------------------------
;; Increase garbage collection threshold to speed up startup.
(defconst emacs-start-time (current-time))
(setq gc-cons-threshold 402653184 gc-cons-percentage 0.6)
;; ---------------------------------------------------------------------------
;; * Load Core Paths
;; ---------------------------------------------------------------------------
;; Load the paths to Spacemacs core files.
(load (concat (file-name-directory load-file-name) "core/core-load-paths")
      nil (not init-file-debug))

;; ---------------------------------------------------------------------------
;; * Load Version Info
;; ---------------------------------------------------------------------------
;; Load Spacemacs and Emacs version information.
(load (concat spacemacs-core-directory "core-versions")
      nil (not init-file-debug))

;; ---------------------------------------------------------------------------
;; * Remove Stale Compiled Files
;; ---------------------------------------------------------------------------
;; Remove stale compiled files if Emacs version has changed.
(load (concat spacemacs-core-directory "core-compilation")
      nil (not init-file-debug))
(load spacemacs--last-emacs-version-file t (not init-file-debug))

;; Update saved Emacs version if necessary.
(unless (string= spacemacs--last-emacs-version emacs-version)
  (spacemacs//update-last-emacs-version))

;; ---------------------------------------------------------------------------
;; * Emacs Version Check
;; ---------------------------------------------------------------------------
;; Stop initialization if the version of Emacs the user is using is too old.
(when (not (version<= spacemacs-emacs-min-version emacs-version))
  (error (concat "Your version of emacs (%s) is too old. "
                 "Spacemacs requires emacs version %s or above."
                  "Please update your emacs version."
	          "You can install a newer version at https://www.gnu.org/software/emacs/download.html") 
		     
         emacs-version spacemacs-emacs-min-version))

;; -------------------------------------------------------------------------
;; * Startup Speed Tweaks
;; -------------------------------------------------------------------------
;; Simplify file-name-handler-alist for faster startup.
;; Prefer newer files over older compiled ones.
(let ((load-prefer-newer t)
      (file-name-handler-alist '(("\\.gz\\'" . jka-compr-handler))))

  ;; -----------------------------------------------------------------------
  ;; * Load Spacemacs Core
  ;; -----------------------------------------------------------------------
  ;; Load main Spacemacs core and configuration layers.
  (require 'core-spacemacs)
  (configuration-layer/load-lock-file)
  (spacemacs/init)
  (configuration-layer/stable-elpa-init)
  (configuration-layer/load)
  (spacemacs-buffer/display-startup-note)
  (spacemacs/setup-startup-hook)

  ;; -----------------------------------------------------------------------
  ;; * Start Emacs Server (Optional)
  ;; -----------------------------------------------------------------------
  ;; Start Emacs server if enabled in user config.
  (when (and dotspacemacs-enable-server (not noninteractive))
    (require 'server)
    (when dotspacemacs-server-socket-dir
      (setq server-socket-dir dotspacemacs-server-socket-dir))
    (unless (or (daemonp) (server-running-p))
      (message "Starting a Emacs server...")
      (server-start))))
