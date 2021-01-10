;;; config.el --- prolog layer config File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Newres Al Haider <newrescode@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; variables

(defvar prolog-system `swi
  "The type of Prolog system used when setting up the Prolog layer. A number of configuration values are derived from this, notably in prolog-mode. The default value is `swi for SWI-Prolog. The recognized symbol values are:
swi     - SWI Prolog
sicstus - SICStus Prolog
eclipse - Eclipse Prolog
xsb     - XSB <http://xsb.sourceforge.net>
gnu     - GNU Prolog
yap     - YAP Prolog")

;;; config.el ends here
