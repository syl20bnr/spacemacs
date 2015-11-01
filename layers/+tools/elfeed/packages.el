;;; packages.el --- elfeed Layer extensions File for Spacemacs
;;
;; Copyright (c) 2012-2015 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq elfeed-packages
      '(elfeed
        elfeed-web
        elfeed-org))

(defun elfeed/init-elfeed ()
  (use-package elfeed
    :defer t
    :commands (elfeed-web-start elfeed-web-stop)
    :init
    (evil-leader/set-key "af" 'elfeed)
    :config
    (progn
      (spacemacs|evilify-map elfeed-search-mode-map
        :mode elfeed-search-mode
        :bindings
        "q" 'quit-window
        "c" 'elfeed-db-compact
        "o" 'elfeed-load-opml
        "w" 'elfeed-web-start
        "W" 'elfeed-web-stop
        "r" 'elfeed-search-update--force
        "l" 'elfeed-update)
      (spacemacs|evilify-map elfeed-show-mode-map
        :mode elfeed-show-mode
        :bindings
        "q" 'quit-window))))

(defun elfeed/init-elfeed-org ()
  (use-package elfeed-org
    :defer t
    :if (boundp 'rmh-elfeed-org-files)
    :commands elfeed-org
    :init
    (spacemacs|use-package-add-hook
        :pre-config (elfeed-org))))

(defun elfeed/init-elfeed-web ()
  (use-package elfeed-web
    :commands elfeed-web-start
    :init
    (progn
      (when elfeed-web-enabled-on-emacs-startup
        (elfeed-web-start)))))
