;;; config.el --- erc Layer configuration File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defvar erc-enable-sasl-auth nil
  "If non nil then use SASL authenthication with ERC.")

(defvar erc-space-macs-layout-name "@ERC"
  "Name used in the setup for `space-macs-layouts' micro-state")

(defvar erc-space-macs-layout-binding "E"
  "Binding used in the setup for `space-macs-layouts' micro-state")

(defvar erc-server-list nil
  "If non nil, connect automatically to the specified servers with the given credentials.")

(defvar erc-enable-notifications t
  "If non nil, enable ERC notifications.")


