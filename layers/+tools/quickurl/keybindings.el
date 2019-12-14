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

(spacemacs/declare-prefix "aQ" "quickurl")
(spacemacs/set-leader-keys
  "aQl" 'quickurl-list
  "aQQ" 'quickurl
  "aQi" 'quickurl-ask
  "aQe" 'quickurl-edit-urls
  "aQa" 'quickurl-add-url
  "aQb" 'quickurl-browse-url-ask)
