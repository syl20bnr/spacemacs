;;; keybindings.el --- Quickurl dispatch layer.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Spenser Truex <web@spensertruex.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

;; Keybindings to use Quickurl, including mode-local ones for the listing.

(space-macs/declare-prefix "atq" "quickurl")
(space-macs/set-leader-keys
  "atql" 'quickurl-list
  "atqq" 'quickurl
  "atqi" 'quickurl-ask
  "atqe" 'quickurl-edit-urls
  "atqa" 'quickurl-add-url
  "atqb" 'quickurl-browse-url-ask)


