;;; early-init.el --- Space-macs Early Init File
;;
;; Copyright (c) 2020 Sylvain Benner & Contributors
;;
;; Author: Miciah Dashiel Butler Masters <miciah.masters@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

;; Before e-macs 27, the init file was responsible for initializing the package
;; manager by calling `package-initialize'. e-macs 27 changed the default
;; behavior: It now calls `package-initialize' before loading the init file.
;; This behavior would prevent Space-macs's own package initialization from
;; running. However, e-macs 27 also loads the "early init" file (this file)
;; before it initializes the package manager, and Space-macs can use this early
;; init file to prevent e-macs from initializing the package manager. (See
;; <http://git.savannah.gnu.org/cgit/e-macs.git/commit/?id=24acb31c04b4048b85311d794e600ecd7ce60d3b>.)
;;
;; Earlier e-macs versions do not load the early init file and do not initialize
;; the package manager before loading the init file, so this file is neither
;; needed nor loaded on those versions.
(setq package-enable-at-startup nil)


