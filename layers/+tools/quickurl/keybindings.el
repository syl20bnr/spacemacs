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

(spacemacs/declare-prefix "Q" "quickurl")
(spacemacs/set-leader-keys
  "Ql" 'quickurl-list
  "QQ" 'quickurl
  "Qi" 'quickurl-ask
  "Qe" 'quickurl-edit-urls
  "Qa" 'quickurl-add-url
  "Qb" 'quickurl-browse-url-ask)
