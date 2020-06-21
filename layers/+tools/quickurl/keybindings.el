;;; keybindings.el --- Quickurl dispatch layer.
;;
;; Copyright (c) 2012-2019 Sylvain Benner & Contributors
;;
;; Author: Spenser Truex <web@spensertruex.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Keybindings to use Quickurl, including mode-local ones for the listing.

(spacemacs/declare-prefix "atq" "quickurl")
(spacemacs/set-leader-keys
  "atql" 'quickurl-list
  "atqq" 'quickurl
  "atqi" 'quickurl-ask
  "atqe" 'quickurl-edit-urls
  "atqa" 'quickurl-add-url
  "atqb" 'quickurl-browse-url-ask)
