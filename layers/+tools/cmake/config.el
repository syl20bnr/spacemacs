;;; config.el --- CMake layer config file for Spacemacs.
;;
;; Copyright (c) 2018 Sylvain Benner & Contributors
;;
;; Author: Alexander Dalshov <dalshov@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; variables

(defconst cmake-modes '(c-mode c++-mode cmake-mode)
  "Primary major modes where `cmake-ide' could be used.")

(defvar cmake-enable-cmake-ide-support nil
  "If non nil CMake related packages and configuration are enabled.")
