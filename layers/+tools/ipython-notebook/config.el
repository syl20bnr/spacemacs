;;; funcs.el --- ipython-notebook Layer Configuration File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Senghoo Kim <me@senghoo.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar ein-backend 'nil
  "The backend to use for IDE features.
Possible values are `jupyter' and `nil'.
If `jupyter' then the API provided by jupyter will be used.
If `nil' the ide feature will not be enabled.")

