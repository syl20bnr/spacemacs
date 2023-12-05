;;; config.el --- unicode-fonts configuration file for Spacemacs. -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
;;
;; Author: Aaron Jensen <aaronjensen@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(spacemacs|defc unicode-fonts-force-multi-color-on-mac nil
  "If non nil unicode-fonts will enable multi-color Emoji.
This is only needed in emacs-plus.
The Emacs macOS port automatically turns multi-color Emoji support on and
so it's unnecessary."
  '(boolean))

(spacemacs|defc unicode-fonts-enable-ligatures nil
  "If non-nil, enable unicode-fonts.
By default it's enabled only for `prog-mode' buffers.
For a finer control of the behavior, see `unicode-fonts-ligature-modes'."
  '(boolean))

(spacemacs|defc unicode-fonts-ligature-set '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                             ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                             "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                             "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                             "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                             "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                             "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                             "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                             ">=" ">>" ">-" "-~" "-|" "->" "-<" "<~" "<*" "<|" "<:" "<$"
                                             "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!" "##"
                                             "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:" "?="
                                             "?." "??" ";;" "/*" "/**" "/=" "/>" "__" "~~" "(*" "*)"
                                             "://")
  "List of ligatures to enable."
  '(repeat string))

(spacemacs|defc unicode-fonts-ligature-modes '(prog-mode)
  "This only takes effect when `unicode-fonts-enable-ligatures' is non-nil."
  '(repeat symbol))

;;; config.el ends here
