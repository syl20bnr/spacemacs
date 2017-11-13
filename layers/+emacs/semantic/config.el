;;; config.el --- semantic Layer configuration
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sebastian Wiesner <swiesner@lunaryorn.com
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq srecode-map-save-file (concat spacemacs-cache-directory
                                    "srecode-map.el"))
(setq semanticdb-default-save-directory (concat spacemacs-cache-directory
                                                "semanticdb/"))
(defvar global-semantic-stickyfunc-mode-turned-on 't
  "Toggles global-semantic-stickyfunc-mode. If value is non-nil then global-semantic-sticky-func-mode is turned on. (Default)")

(setq semanticdb-find-default-throttle '(file local project))
(unless (file-exists-p semanticdb-default-save-directory)
  (make-directory semanticdb-default-save-directory))
