;;; config.el --- OSX Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq osx-packages
      '(
        exec-path-from-shell
        helm
        launchctl
        (osx-dictionary :toggle osx-use-dictionary-app)
        osx-trash
        pbcopy
        reveal-in-osx-finder
        term
        ))

(when (spacemacs/system-is-mac)
  ;; Enable built-in trash support via finder API if available (only on Emacs
  ;; Mac Port)
  (when (boundp 'mac-system-move-file-to-trash-use-finder)
    (setq mac-system-move-file-to-trash-use-finder t)))

(defun osx/post-init-exec-path-from-shell ()
  ;; Use GNU ls as `gls' from `coreutils' if available.  Add `(setq
  ;; dired-use-ls-dired nil)' to your config to suppress the Dired warning when
  ;; not using GNU ls.  We must look for `gls' after `exec-path-from-shell' was
  ;; initialized to make sure that `gls' is in `exec-path'
  (when (spacemacs/system-is-mac)
    (let ((gls (executable-find "gls")))
      (when gls
        (setq insert-directory-program gls
              dired-listing-switches "-aBhl --group-directories-first")))))

(defun osx/pre-init-helm ()
  ;; Use `mdfind' instead of `locate'.
  (when (spacemacs/system-is-mac)
    (spacemacs|use-package-add-hook helm
      :post-config
      ;; Disable fuzzy matchting to make mdfind work with helm-locate
      ;; https://github.com/emacs-helm/helm/issues/799
      (setq helm-locate-fuzzy-match nil)
      (setq helm-locate-command "mdfind -name %s %s"))))

(defun osx/init-launchctl ()
  (use-package launchctl
    :if (spacemacs/system-is-mac)
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.plist\\'" . nxml-mode))
      (spacemacs/set-leader-keys "al" 'launchctl))
    :config
    (progn
      (evilified-state-evilify launchctl-mode launchctl-mode-map
        (kbd "q") 'quit-window
        (kbd "s") 'tabulated-list-sort
        (kbd "g") 'launchctl-refresh
        (kbd "n") 'launchctl-new
        (kbd "e") 'launchctl-edit
        (kbd "v") 'launchctl-view
        (kbd "l") 'launchctl-load
        (kbd "u") 'launchctl-unload
        (kbd "r") 'launchctl-reload
        (kbd "S") 'launchctl-start
        (kbd "K") 'launchctl-stop
        (kbd "R") 'launchctl-restart
        (kbd "D") 'launchctl-remove
        (kbd "d") 'launchctl-disable
        (kbd "E") 'launchctl-enable
        (kbd "i") 'launchctl-info
        (kbd "f") 'launchctl-filter
        (kbd "=") 'launchctl-setenv
        (kbd "#") 'launchctl-unsetenv
        (kbd "h") 'launchctl-help))))

(defun osx/init-osx-dictionary ()
  (use-package osx-dictionary
    :if osx-use-dictionary-app
    :init (spacemacs/set-leader-keys "xwd" 'osx-dictionary-search-pointer)
    :commands (osx-dictionary-search-pointer
               osx-dictionary-search-input
               osx-dictionary-cli-find-or-recompile)
    :config
    (progn
      (evilified-state-evilify-map osx-dictionary-mode-map
        :mode osx-dictionary-mode
        :bindings
        "q" 'osx-dictionary-quit
        "r" 'osx-dictionary-read-word
        "s" 'osx-dictionary-search-input
        "o" 'osx-dictionary-open-dictionary.app))))

(defun osx/init-osx-trash ()
  (use-package osx-trash
    :if (and (spacemacs/system-is-mac)
             (not (boundp 'mac-system-move-file-to-trash-use-finder)))
    :init (osx-trash-setup)))

(defun osx/init-pbcopy ()
  (use-package pbcopy
    :if (and (spacemacs/system-is-mac) (not (display-graphic-p)))
    :init (turn-on-pbcopy)))

(defun osx/init-reveal-in-osx-finder ()
  (use-package reveal-in-osx-finder
    :if (spacemacs/system-is-mac)
    :commands reveal-in-osx-finder))

(defun osx/post-init-term ()
  (with-eval-after-load 'term
    (define-key term-raw-map (kbd "s-v") 'term-paste)))
