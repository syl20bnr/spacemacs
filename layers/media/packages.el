;;; packages.el --- media layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Alejandro Erickson <alejandro.erickson@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `media-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `media/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `media/pre-init-PACKAGE' and/or
;;   `media/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst media-packages
  '(
    ;; We need this recipe because MELPA version doesn't download the taglib metadata reader
    (emms :location (recipe
                     :fetcher github
                     :repo "alejandroerickson/emms"
                     :files ("lisp/*.el"
                             ("cd-here-and-make-emms-print-metadata-and-put-in-path" "Makefile")
                             ("cd-here-and-make-emms-print-metadata-and-put-in-path/src" "src/*")
                             )
                     )
          )
    emms-state
    helm-emms
    )
  "The list of Lisp packages required by the media layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun media/init-emms ()
  (use-package emms
    :defer t
    :init
    (progn
      ;; TODO: find a better global key, more evily
      (global-set-key [(f7)] 'emms-smart-browse)
      (spacemacs/declare-prefix "am" "music")
      (spacemacs/declare-prefix "ame" "EMMS")
      (spacemacs/set-leader-keys
        "ames" 'emms-streams
        "ameb" 'emms-browser
        "amep" 'emms-playlist-mode-go
        "ameo" 'emms-show
        "a SPC" 'emms-play-pause-dwim
        "a ." 'emms-next
        "a ," 'emms-previous
        "a RET" 'emms-smart-browse
        )

      (add-hook 'emms-browser-show-display-hook 'evil-initialize)
      (add-hook 'emms-stream-hook 'evil-initialize)
      )
    :config
    (progn
      ;;(require 'emms-setup)
      (emms-all)
      (emms-mode-line 0)
      (emms-playing-time 1)
      (emms-default-players)
      (autoload 'emms-smart-browse "emms-browser.el" "Browse with EMMS" t)
      (define-key emms-browser-mode-map (kbd "D") 'emms-browser-move-files-to-trash)
      (define-key emms-browser-mode-map (kbd "t") 'emms-browser-toggle-subitems)
      (require 'emms-info-libtag)
      (setq emms-info-functions '(emms-info-libtag))

      (evilified-state-evilify-map emms-stream-mode-map
        :mode emms-stream-mode
        )
      (evilified-state-evilify-map emms-mark-mode-map
        :mode emms-mark-mode
        :bindings
        "t" 'emms-mark-toggle
        "u" 'emms-mark-unmark-forward
        "K" 'emms-mark-kill-marked-tracks
        "M" 'emms-mark-mode-disable
        )
      (evilified-state-evilify-map emms-playlist-mode-map
        :mode emms-playlist-mode
        :bindings
        "l" 'emms-next
        "h" 'emms-previous
        "H" 'emms-playlist-mode-first
        "L" 'emms-playlist-mode-last
        "W" 'emms-playlist-save
        ;; P also works for emms-pause but it's kind of a stupid binding.
        ;; can't use SPC, so we'll make do with TAB
        (kbd "TAB") 'emms-pause
        "," 'emms-seek-minute-backward
        "." 'emms-seek-minute-forward
        "u" 'emms-playlist-mode-undo
        "p" 'emms-playlist-mode-yank
        "P" 'emms-playlist-mode-yank-pop
        "O" 'emms-playlist-mode-insert-newline
        ;; having trouble with this because it is
        ;; sometimes calling 'emms-playlist-mode-current-kill
        "K" 'emms-mark-kill-marked-tracks
        "M" 'emms-mark-mode
        )
      (evilified-state-evilify-map emms-browser-mode-map
        :mode emms-browser-mode
        :bindings
        ;; since this is normally SPC
        "t" 'emms-browser-toggle-subitems
        ;; makes more sense than C-j
        (kbd "<S-return>") 'emms-browser-add-tracks-and-play
        )
      ;; TODO: emms-browser search mode keybindings
      )
    )
  )

(defun media/init-emms-state ()
  (use-package emms-state
    ;; for some reason if this is deferred you can't bring up the smart browser.
    :config
    (emms-state-mode 0)
    ))

(defun media/init-helm-emms ()
  (use-package helm-emms
    :defer t
    )
  )

;;; packages.el ends here
