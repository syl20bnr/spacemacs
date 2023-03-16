;;; packages.el --- Themes Mega-Pack Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
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


(setq themes-megapack-packages
  '(
    afternoon-theme
    alect-themes
    ample-theme
    ample-zen-theme
    apropospriate-theme
    anti-zenburn-theme
    ;; contains errors
    ;; badger-theme
    badwolf-theme
    birds-of-paradise-plus-theme
    bubbleberry-theme
    busybee-theme
    cherry-blossom-theme
    chocolate-theme
    clues-theme
    color-theme-sanityinc-solarized
    color-theme-sanityinc-tomorrow
    cyberpunk-theme
    dakrone-theme
    darkmine-theme
    darkokai-theme
    darktooth-theme
    django-theme
    doom-themes
    dracula-theme
    espresso-theme
    exotica-theme
    eziam-themes
    farmhouse-themes
    flatland-theme
    flatui-theme
    gandalf-theme
    gotham-theme
    grandshell-theme
    gruber-darker-theme
    gruvbox-theme
    hc-zenburn-theme
    hemisu-theme
    heroku-theme
    inkpot-theme
    ir-black-theme
    jazz-theme
    jbeans-theme
    kaolin-themes
    light-soap-theme
    lush-theme
    madhat2r-theme
    majapahit-themes
    material-theme
    minimal-theme
    modus-themes
    moe-theme
    molokai-theme
    monokai-theme
    monochrome-theme
    mustang-theme
    naquadah-theme
    noctilux-theme
    obsidian-theme
    occidental-theme
    omtose-phellack-theme
    oldlace-theme
    organic-green-theme
    phoenix-dark-mono-theme
    phoenix-dark-pink-theme
    planet-theme
    professional-theme
    purple-haze-theme
    railscasts-theme
    rebecca-theme
    reverse-theme
    seti-theme
    smyx-theme
    soft-charcoal-theme
    soft-morning-theme
    soft-stone-theme
    solarized-theme
    soothe-theme
    spacegray-theme
    subatomic-theme
    subatomic256-theme
    sublime-themes
    sunny-day-theme
    tango-2-theme
    tango-plus-theme
    tangotango-theme
    tao-theme
    ;; contains error
    ;; tommyh-theme
    toxi-theme
    twilight-anti-bright-theme
    twilight-bright-theme
    twilight-theme
    ujelly-theme
    underwater-theme
    white-sand-theme
    zen-and-art-theme
    zenburn-theme
    (zonokai-emacs :location (recipe
                              :fetcher github
                              :repo "ZehCnaS34/zonokai-emacs"))
    ))

;; define programmatically the init functions
(dolist (pkg themes-megapack-packages)
  (eval `(defun ,(intern (format "themes-megapack/init-%S" (if (listp pkg) (car pkg) pkg))) nil)))

(defun themes-megapack/init-darkokai-theme ()
  (setq darkokai-mode-line-padding 1))
