;;; packages.el --- Themes Mega-Pack Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq themes-megapack-packages
  '(
    afternoon-theme
    alect-themes
    ample-theme
    ample-zen-theme
    apropospriate-theme
    anti-zenburn-theme
    ;; contains errors
    ; badger-theme
    birds-of-paradise-plus-theme
    bubbleberry-theme
    busybee-theme
    cherry-blossom-theme
    clues-theme
    color-theme-sanityinc-solarized
    color-theme-sanityinc-tomorrow
    colorsarenice-theme
    cyberpunk-theme
    dakrone-theme
    darkburn-theme
    darkmine-theme
    darktooth-theme
    django-theme
    espresso-theme
    firebelly-theme
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
    light-soap-theme
    lush-theme
    material-theme
    minimal-theme
    moe-theme
    molokai-theme
    monochrome-theme
    mustang-theme
    naquadah-theme
    niflheim-theme
    noctilux-theme
    obsidian-theme
    occidental-theme
    oldlace-theme
    organic-green-theme
    pastels-on-dark-theme
    phoenix-dark-mono-theme
    phoenix-dark-pink-theme
    planet-theme
    professional-theme
    purple-haze-theme
    reverse-theme
    seti-theme
    smyx-theme
    soft-charcoal-theme
    soft-morning-theme
    soft-stone-theme
    soothe-theme
    spacegray-theme
    stekene-theme
    subatomic-theme
    subatomic256-theme
    sublime-themes
    sunny-day-theme
    tango-2-theme
    tango-plus-theme
    tangotango-theme
    tao-theme
    ;; contains error
    ; tommyh-theme
    toxi-theme
    tronesque-theme
    twilight-anti-bright-theme
    twilight-bright-theme
    twilight-theme
    ujelly-theme
    underwater-theme
    zen-and-art-theme
    zenburn-theme
    zonokai-theme
    ))

;; programmatically defin the init functions
(dolist (pkg themes-megapack-packages)
  (eval `(defun ,(intern (format "themes-megapack/init-%S" pkg)) nil)))
