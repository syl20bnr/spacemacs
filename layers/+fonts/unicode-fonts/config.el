;;; config.el --- unicode-fonts configuration file for Spacemacs.
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
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


(defvar unicode-fonts-force-multi-color-on-mac nil
  "If non nil unicode-fonts will enable multi-color fonts (emoji)
on macs.

This should only be set when using the multi-color patch as
emacs-plus does. It is unnecessary to set this when using the
macOS port version.")
(defvar unicode-fonts-enable-ligatures nil
  "If you want to enable font ligatures")

(defvar unicode-fonts-ligature-set '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
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
  "List of ligatures you would like enabled")

(defvar unicode-fonts-ligature-modes '(prog-mode)
  "The modes ligatures are enabled in, default is prog-mode")
