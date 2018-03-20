;;; config.el --- cquery layer config file for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Cormac Cannon <cormacc-public@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar cquery-extra-init-params '(:cacheFormat "msgpack")
  "Extra initialisation parameters to pass to cquery. See
https://github.com/cquery-project/cquery/blob/master/src/config.h for details.")

(defvar cquery-project-whitelist nil
  "A list of project directory patterns for which cquery should be
initialized. This overrides `cquery-project-blacklist'.")

(defvar cquery-project-blacklist nil
  "A list of project root patterns for which cquery shouldn't be
initialized. `cquery-project-whitelist' is checked first, then this,
if no pattern matches the project root, cquery will be initialized.")

(defvar cquery-executable "cquery"
  "Path to cquery executable (default value assumes it's in the path)")

(defvar cquery-sem-highlight-method 'font-lock
  "Set to 'font-lock or 'overlay to enable semantic highlighting (Apparently font-lock requires theme support. Does overlay?).")

(defvar cquery-sem-highlight-rainbow nil
  "When non-nil, use rainbow semantic highlighting")
