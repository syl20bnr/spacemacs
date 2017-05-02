;;; config.el --- bepo Layer extensions File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Fabien Dubosson & Contributors
;;
;; Author: Fabien Dubosson <fabien.dubosson@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(bepo|config "evil-escape"
  :description
  "Provide a better default escape combination than 'fd'."
  :loader
  (spacemacs|use-package-add-hook evil-escape :pre-init BODY)
  :config
  (setq-default evil-escape-key-sequence "gq"))

(bepo|config "avk-keys"
  :description
  "Better prefix for joining multi-character places (central keys)"
  :loader
  (spacemacs|use-package-add-hook avy :post-init BODY)
  :config
  (setq-default avy-keys '(?t ?e ?s ?i ?r ?u ?n ?a)))

(bepo|config "evil-surround-pairs"
  :description
  "Add support for `«' `»' in evil-surround"
  :loader
  (spacemacs|use-package-add-hook evil-surround :post-init BODY)
  :config
  (progn
    (setq-default evil-surround-pairs-alist (cons '(?« "« " . " »") evil-surround-pairs-alist))
    (setq-default evil-surround-pairs-alist (cons '(?» "«" . "»") evil-surround-pairs-alist))))

