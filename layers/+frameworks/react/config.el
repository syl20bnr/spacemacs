;;; config.el --- react layer config file for Spacemacs. -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Andrea Moretti <axyzxp@gmail.com>
;; URL: https://github.com/axyz
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(spacemacs|define-jump-handlers rjsx-mode)

(defvar react-import-tool nil
  "The import backend to import modules. Possible values are `import-js' and `nil' to disable.")
