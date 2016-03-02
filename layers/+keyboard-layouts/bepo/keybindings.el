;;; keybindings.el --- bepo Layer extensions File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Fabien Dubosson <fabien.dubosson@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(bepo|config ace-window
  :description
  "Remap `ace-window' bindings, and change the keys to the ones
   on the central row."
  :loader
  (spacemacs|use-package-add-hook ace-window :post-init BODY)
  :config
  (progn
    ;; HACK: Manual binding, otherwise conflicts with evil-window auto
    ;; remapping.
    (spacemacs/set-leader-keys
      "wC" 'evil-window-move-far-left
      "wL" 'ace-delete-window
      "wQ" 'ace-delete-window)
    (setq aw-keys '(?t ?e ?s ?i ?r ?u ?n ?a ?c ?,))))

(bepo|config avy
  :description
  "Change `avy' keys to the ones on the central row."
  :loader
  (spacemacs|use-package-add-hook avy :post-init BODY)
  :config
  (setq-default avy-keys '(?t ?e ?s ?i ?r ?u ?n ?a ?c ?,)))

(bepo|config buffer-move
  :description
  "Remap `buffer-move' bindings."
  :loader
  (with-eval-after-load 'buffer-move BODY)
  :config
  (bepo/leader-correct-keys
    "bmh"
    "bmj"
    "bmk"
    "bml"
    ))

(bepo|config comint-mode
  :description
  "Remap `comint-mode' bindings."
  :loader
  (with-eval-after-load 'shell BODY)
  :config
  (dolist (m '(normal insert))
    (eval `(bepo/evil-correct-keys `,m comint-mode-map
             "C-j"
             "C-k"))))

(bepo|config company
  :description
  "Remap `company' bindings."
  :loader
  (spacemacs|use-package-add-hook company :post-config BODY)
  :config
  (bepo/correct-keys company-active-map
    "C-h"
    "C-j"
    "C-k"
    "C-l"))

(bepo|config elfeed
  :description
  "Remap `elfeed' bindings."
  :loader
  (spacemacs|use-package-add-hook elfeed :post-config BODY)
  :config
  (progn
    (bepo/evil-correct-keys 'evilified elfeed-search-mode-map
      "j"
      "k")
    (bepo/evil-correct-keys 'evilified elfeed-show-mode-map
      "C-j"
      "C-k")
    ;; HACK: The auto correction doesn't work... mystery.
    (evil-define-key 'evilified elfeed-search-mode-map
      "k" 'elfeed-search-live-filter)))

(bepo|config evil
  :description
  "Remap `evil' bindings, and map some unused ones as aliases."
  :loader
  (spacemacs|use-package-add-hook evil :post-config BODY)
  :config
  (progn
    (dolist (map bepo--all-evil-states-but-insert)
      (bepo/correct-keys map
        "h"
        "j"
        "k"
        "l"
        ;;
        "H"
        "J"
        "K"
        "L"
        ;;
        "gj"
        "gk"))
    (bepo/set-in-all-evil-states-but-insert
      "é" 'evil-forward-word-begin
      "É" 'evil-forward-WORD-begin)
    (bepo/set-in-state evil-inner-text-objects-map
      "é" 'evil-inner-word
      "É" 'evil-inner-WORD)
    (bepo/set-in-state evil-outer-text-objects-map
      "é" 'evil-a-word
      "É" 'evil-a-WORD)
    (bepo/set-in-all-evil-states-but-insert
      "«" 'evil-shift-left
      "»" 'evil-shift-right)))

(bepo|config evil-escape
  :description
  "Change `evil-escape' default escape combination for a better
   one than `fd'."
  :loader
  (spacemacs|use-package-add-hook evil-escape :post-init BODY)
  :config
  (setq-default evil-escape-key-sequence "gq"))

(bepo|config evil-surround
  :description
  "Remap `evil-surround' bindings and add the `« »' pair."
  :loader
  (spacemacs|use-package-add-hook evil-surround :post-init BODY)
  :config
  (progn
    (bepo/evil-correct-keys 'visual evil-surround-mode-map "s")
    (setq-default
     evil-surround-pairs-alist (cons '(?« "« " . " »") evil-surround-pairs-alist)
     evil-surround-pairs-alist (cons '(?» "«" . "»") evil-surround-pairs-alist))))

(bepo|config evil-evilified-state
  :description
  "Remap `evil-evilified-state' bindings."
  :loader
  (with-eval-after-load 'evil-evilified-state BODY)
  :config
  (bepo/correct-keys evil-evilified-state-map
    "h"
    "j"
    "k"
    "l"))

(bepo|config evil-window
  :description
  "Remap `evil-window' bindings. Add `é' as an alias for `w' and
   `q' for closing."
  :config
  (progn
    (bepo/leader-correct-keys
     "wh"
     "wj"
     "wk"
     "wl"
     ;;
     "wH"
     "wJ"
     "wK"
     "wL")
    (spacemacs/set-leader-keys
      "wé" 'other-window
      "wq" 'delete-window)
    (bepo/leader-alias-of "é" "w")))

(bepo|config eyebrowse
  :description
  "Remap `eyebrowse' keybindings conflicting with evil."
  :loader
  (spacemacs|use-package-add-hook eyebrowse :post-init BODY)
  :config
  (bepo/correct-keys evil-motion-state-map
    "gj"
    "gJ"))

(bepo|config flycheck-error-list
  :description
  "Remap `flycheck-error-list' bindings."
  :loader
  (spacemacs|use-package-add-hook flycheck :post-config BODY)
  :config
  (bepo/evil-correct-keys 'evilified flycheck-error-list-mode-map
    "j"
    "k"))

(bepo|config helm
  :description
  "Remap `helm' bindings."
  :loader
  (spacemacs|use-package-add-hook helm :post-config BODY)
  :config
  (bepo/correct-keys helm-map
    "C-h"
    "C-j"
    "C-k"
    "C-l"))

(bepo|config helm-buffers
  :description
  "Remap `helm-buffers' bindings."
  :loader
  (with-eval-after-load 'helm-buffers BODY)
  :config
  ;; HACK: Forced to correct wrong behaviour
  (bepo/set-in-state helm-buffer-map "C-s" 'helm-previous-line))

(bepo|config helm-files
  :description
  "Remap `helm-files' bindings."
  :loader
  (with-eval-after-load 'helm-files BODY)
  :config
  (progn
    ;; HACK: Forced to correct wrong behaviour
    (bepo/set-in-state helm-find-files-map "C-s" 'helm-previous-line)
    (bepo/set-in-state helm-find-files-map "C-k" 'helm-ff-run-grep)
    (bepo/set-in-state helm-find-files-map "C-r" 'helm-maybe-exit-minibuffer)
    (bepo/set-in-state helm-read-file-map "C-s" 'helm-previous-line)
    (bepo/set-in-state helm-read-file-map "C-K" 'helm-previous-line)))

(bepo|config helm-locate
  :description
  "Remap `helm-locate' bindings."
  :loader
  (with-eval-after-load 'helm-locate BODY)
  :config
  (progn
    ;; HACK: Forced to correct wrong behaviour
    (bepo/set-in-state helm-generic-files-map "C-s" 'helm-previous-line)
    (bepo/set-in-state helm-generic-files-map "C-k" 'helm-ff-run-grep)))

(bepo|config magit
  :description
  "Remap `magit' bindings."
  :loader
  (spacemacs|use-package-add-hook magit :post-config BODY)
  :config
  (progn
    (bepo/evil-correct-keys evil-magit-state magit-mode-map
      "j"
      "k"
      "C-j"
      "C-k")
    (bepo/evil-correct-keys 'normal evil-magit-toggle-text-minor-mode-map
      "C-j")
    (dolist (map (list magit-branch-section-map
                       magit-commit-section-map
                       magit-file-section-map
                       magit-hunk-section-map
                       magit-remote-section-map
                       magit-staged-section-map
                       magit-unstaged-section-map
                       magit-module-commit-section-map
                       magit-stash-section-map
                       magit-stashes-section-map
                       magit-tag-section-map
                       magit-unpulled-section-map
                       magit-unpushed-section-map
                       magit-untracked-section-map))
      (bepo/correct-keys map
        "j"
        "k"
        "C-j"
        "C-k"))
    (magit-change-popup-key 'magit-dispatch-popup :actions ?t ?j)
    (magit-change-popup-key 'magit-dispatch-popup :actions ?s ?k)
    (magit-change-popup-key 'magit-dispatch-popup :actions ?S ?K)))

(bepo|config neotree
  :descripition
  "Remap `neotree' bindings."
  :loader
  (spacemacs|use-package-add-hook neotree :post-config BODY)
  :config
  (progn
    (bepo/evil-correct-keys 'evilified neotree-mode-map
      "h"
      "j"
      "k"
      "l"
      ;;
      "H"
      "J"
      "K"
      "L")
    (bepo/set-in-state (evil-get-auxiliary-keymap neotree-mode-map 'evilified)
      "h" 'neotree-hidden-file-toggle
      "k" 'neotree-rename-node)))

(bepo|config org
  :description
  "Remap keys in `org-mode'."
  :loader
  (with-eval-after-load 'org BODY)
  :config
  (progn
    (evil-define-key 'normal evil-org-mode-map
      "t" 'evil-next-line
      "j" 'org-todo)
    (dolist (m '(normal insert))
      (eval `(evil-define-key ',m evil-org-mode-map
               ;; ctsr
               (kbd "M-c") 'org-metaleft
               (kbd "M-t") 'org-metadown
               (kbd "M-s") 'org-metaup
               (kbd "M-r") 'org-metaright
               (kbd "M-C") 'org-shiftmetaleft
               (kbd "M-T") 'org-shiftmetadown
               (kbd "M-S") 'org-shiftmetaup
               (kbd "M-R") 'org-shiftmetaright
               ;; hjkl
               (kbd "M-h") 'capitalize-word
               (kbd "M-j") 'transpose-chars
               (kbd "M-k") 'kill-sentence
               (kbd "M-l") 'move-to-window-line-top-bottom
               (kbd "M-H") 'capitalize-word
               (kbd "M-J") 'transpose-chars
               (kbd "M-K") 'kill-sentence
               (kbd "M-L") 'move-to-window-line-top-bottom
               )))
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      ;; ctsr
      "C-S-c" 'org-shiftcontrolleft
      "C-S-t" 'org-shiftcontroldown
      "C-S-s" 'org-shiftcontrolup
      "C-S-r" 'org-shiftcontrolright
      ;; hjkl
      "C-S-h" nil
      "C-S-j" nil
      "C-S-k" nil
      "C-S-l" nil
      )
    (evil-define-key 'normal evil-org-mode-map
      ;; ctsr
      "gt" 'org-forward-heading-same-level
      "gs" 'org-backward-heading-same-level
      ;; hjkl
      "gj" nil
      "gk" nil
      ;; additional
      (kbd "«") 'org-metaleft
      (kbd "»") 'org-metaright
      )))

(bepo|config ranger
  :description
  "Remap navigation keys in `ranger'."
  :loader
  (with-eval-after-load 'ranger BODY)
  :config
  (bepo/correct-keys ranger-mode-map
    "h"
    "j"
    "k"
    "l"
    ;;
    "H"
    "K"
    "J"
    "L"))

(bepo|config spacemacs
  :description
  "Customize some `spacemacs' bindings."
  :config
  (bepo/leader-correct-keys
    "jh"
    "jj"
    "jk"
    "jl"
    ;;
    "jJ"
    ))
