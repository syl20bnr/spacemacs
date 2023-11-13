;;; google-translate.el --- Emacs interface to Google Translate

;; Copyright (C) 2012 Oleksandr Manzyuk <manzyuk@gmail.com>

;; Author: Oleksandr Manzyuk <manzyuk@gmail.com>
;; Maintainer: Andrey Tykhonov <atykhonov@gmail.com>
;; URL: https://github.com/atykhonov/google-translate
;; Package-Requires: ((emacs "24.3") (popup "0.5.8"))
;; Version: 0.12.0
;; Keywords: convenience

;; Contributors:
;;   Tassilo Horn <tsdh@gnu.org>
;;   Bernard Hurley <bernard@marcade.biz>
;;   Chris Bilson <cbilson@pobox.com>
;;   Takumi Kinjo <takumi.kinjo@gmail.com>
;;   momomo5717 <momomo5717@gmail.com>
;;   stardiviner <numbchild@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Installation:

;; From MELPA or Marmalade.

;; Just run `M-x package-install RET google-translate RET`

;; Manual installation.

;; Assuming that the file `google-translate.el' and other files which
;; relates to this package is somewhere on the load path, add the
;; following lines to your `.emacs' file:

;; (require 'google-translate)
;; (require 'google-translate-default-ui)
;; (global-set-key "\C-ct" 'google-translate-at-point)
;; (global-set-key "\C-cT" 'google-translate-query-translate)

;; or

;; (require 'google-translate)
;; (require 'google-translate-smooth-ui)
;; (global-set-key "\C-ct" 'google-translate-smooth-translate)
;;
;; Change the key bindings to your liking.

;; The difference between these configurations is in UI which will be
;; used: Default UI or Smooth UI.
;;
;; Please read the source of `google-translate-default-ui.el' and
;; `google-translate-smooth-ui.el' for more details.

;; Customization:

;; Variables which are available for customization are depends on UI
;; package which is selected for the google-translate
;; package. google-translate-default-ui - is UI which is selected by
;; default. It loads by default and is available right after
;; google-translate installation and its initialization. Please read
;; documentation for the `google-translate-core-ui.el' and
;; `google-translate-default-ui.el' packages for more info about
;; customization.
;;

;;; Code:

(require 'google-translate-default-ui)

(provide 'google-translate)

;;; google-translate.el ends here
