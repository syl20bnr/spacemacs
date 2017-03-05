;;; config.el --- Spacemacs Base Layer configuration File
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst spacemacs-docker-env-fp
  "/etc/environment")

;; Export global configs.
(with-temp-buffer
  (insert-file-contents spacemacs-docker-env-fp)
  (goto-char (point-min))
  (while (re-search-forward "\\(.*\\)=\"\\(.*\\)\"" nil t)
    (setenv (match-string 1) (match-string 2))))

(defconst spacemacs-docker-temp-deps-dir
  (format "%sspacemacs-deps-tmp/"
          temporary-file-directory))

(defconst spacemacs-docker-deps-installers-dir
  (format (concat "%s/.emacs.d/layers/+distributions/"
                  "spacemacs-docker/deps-install/installers")
          (getenv "UHOME")))

(defconst spacemacs-docker-temp-deps-elpa-dir
  (format "%selpa"
          spacemacs-docker-temp-deps-dir))

(defconst spacemacs-docker-dotfile-fp
  (format "%s/.spacemacs" (getenv "UHOME")))

(defconst spacemacs-docker-dump-layer-data-fp
  (format "%sspacemacs-docker-layer-data.el"
          spacemacs-docker-temp-deps-dir))

;; Start spacemacs in the workspace.
(setq default-directory
      (concat (getenv "WORKSPACE") "/"))
