;;; packages.el --- Themes Mega-Pack Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
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
    darkburn-theme
    darkmine-theme
    darkokai-theme
    darktooth-theme
    django-theme
    doom-themes
    dracula-theme
    espresso-theme
    exotica-theme
    eziam-theme
    farmhouse-theme
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
    majapahit-theme
    material-theme
    minimal-theme
    modus-operandi-theme
    modus-vivendi-theme
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
    ))

;; define programmatically the init functions
(dolist (pkg themes-megapack-packages)
  (eval `(defun ,(intern (format "themes-megapack/init-%S" pkg)) nil)))

(defun themes-megapack/init-darkokai-theme ()
  (setq darkokai-mode-line-padding 1))
