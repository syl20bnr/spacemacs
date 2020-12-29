;;; early-init.el --- Spacemacs Early Init File
;;
;; Copyright (c) 2020 Sylvain Benner & Contributors
;;
;; Author: Miciah Dashiel Butler Masters <miciah.masters@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Before Emacs 27, the init file was responsible for initializing the package
;; manager by calling `package-initialize'. Emacs 27 changed the default
;; behavior: It now calls `package-initialize' before loading the init file.
;; This behavior would prevent Spacemacs's own package initialization from
;; running. However, Emacs 27 also loads the "early init" file (this file)
;; before it initializes the package manager, and Spacemacs can use this early
;; init file to prevent Emacs from initializing the package manager. (See
;; <http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=24acb31c04b4048b85311d794e600ecd7ce60d3b>.)
;;
;; Earlier Emacs versions do not load the early init file and do not initialize
;; the package manager before loading the init file, so this file is neither
;; needed nor loaded on those versions.
(setq package-enable-at-startup nil)

;; The rest of this file sets up straight.el so that packages are installed by
;; it rather than built-in package.el.

;; Use "develop" rather than default "master" to use bleeding edge version.
(setq straight-repository-branch "develop")

;; Enable this (default is nil) so that (use-package ...) will use straight.
(setq straight-use-package-by-default t)

;; Default value is '(find-at-startup find-when-checking)
(setq straight-check-for-modifications '(find-at-startup find-when-checking))

;; Default is 'full which means to clone complete history.
(setq straight-vc-git-default-clone-depth 10)

;; 'https is the default, but can be set to 'ssh
(setq straight-vc-git-default-protocol 'https)

;; Following are standard setup lines from https://github.com/raxod502/straight.el.git
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package first and setup so that use-package will use straight.el
;; rather than package.el by default.
(straight-use-package 'use-package)

;; Following is from https://github.com/raxod502/straight.el/issues/352
;; This can be fixed when org 9.5 is released where org and contrib will be separate packages.
(straight-use-package '(org-plus-contrib :includes (org)))

;; These are needed after removing copies from core/libs.
(use-package dash :defer t)
(use-package ht :defer t)
(use-package ido-vertical-mode :defer t)
(use-package load-env-vars :defer t)
(use-package mocker :defer t)
(use-package package-build :defer t)
(use-package page-break-lines :defer t)
(use-package quelpa :defer t)
(use-package spinner :defer t)

;; Install modus-themes to avoid this error on start up:
;;   error: (error Unable to find theme file for ‘modus-operandi’)
(use-package modus-themes :defer t)

(straight-use-package '(ox-gfm :fork (:repo "syl20bnr/ox-gfm")))
(straight-use-package '(cpp-auto-include :fork (:repo "syohex/emacs-cpp-auto-include")))
(straight-use-package '(sql-indent :fork (:repo "alex-hhh/emacs-sql-indent" :files ("sql-indent.el"))))
(straight-use-package '(font-lock+ :fork (:repo "emacsmirror/font-lock-plus")))
(straight-use-package '(command-log-mode :fork (:repo "bmag/command-log-mode" :branch "color")))

;; This is to work around startup problem where org related packages fail to
;; load due to org-version being empty string. -rk 12/29/2020
(defadvice org-version (around hack-org-version activate)
  (setq ad-return-value "9.4.4"))

(defadvice package-install (around use-straight activate)
  "Use straight.el to install packages."
  (straight-use-package (if (symbolp pkg) pkg (package-desc-name pkg))))
