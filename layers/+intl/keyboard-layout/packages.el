;;; packages.el --- keyboard-layout Layer Packages File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Fabien Dubosson <fabien.dubosson@gmail.com>
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


(defconst keyboard-layout-packages
  '(
    ace-window
    avy
    comint
    company
    ediff
    elfeed
    evil
    evil-collection
    evil-cleverparens
    evil-escape
    evil-evilified-state
    evil-lisp-state
    evil-surround
    eyebrowse
    flycheck
    helm
    imenu-list
    ivy
    magit
    mu4e
    neotree
    org
    org-agenda
    ranger
    twittering-mode
    undo-tree
    ))

(defun keyboard-layout/pre-init-ace-window ()
  (kl|config ace-window
    :description
    "Change `ace-window' keys to the central row."
    :loader
    (spacemacs|use-package-add-hook ace-window :post-init BODY)
    :bepo
    (setq aw-keys '(?a ?u ?i ?e ?t ?s ?r ?n))
    :dvorak
    (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s))
    :neo
    (setq aw-keys '(?u ?i ?a ?e ?n ?r ?t ?d))
    :colemak-neio
    (setq aw-keys '(?a ?r ?s ?t ?n ?e ?i ?o))
    :colemak-hnei
    (setq aw-keys '(?a ?r ?s ?t ?n ?e ?i ?o))
    :colemak-jkhl
    (setq aw-keys '(?a ?r ?s ?t ?n ?e ?i ?o))
    :workman
    (setq aw-keys '(?a ?s ?h ?t ?n ?e ?o ?i))))

(defun keyboard-layout/pre-init-avy ()
  (kl|config avy
    :description
    "Change `avy' keys to the central row."
    :loader
    (spacemacs|use-package-add-hook avy :post-init BODY)
    :bepo
    (setq-default avy-keys '(?a ?u ?i ?e ?t ?s ?r ?n))
    :dvorak
    (setq-default avy-keys '(?a ?o ?e ?u ?h ?t ?n ?s))
    :neo
    (setq-default avy-keys '(?u ?i ?a ?e ?n ?r ?t ?d))
    :colemak-neio
    (setq-default avy-keys '(?a ?r ?s ?t ?n ?e ?i ?o))
    :colemak-hnei
    (setq-default avy-keys '(?a ?r ?s ?t ?n ?e ?i ?o))
    :colemak-jkhl
    (setq-default avy-keys '(?a ?r ?s ?t ?n ?e ?i ?o))
    :workman
    (setq-default avy-keys '(?a ?s ?h ?t ?n ?e ?o ?i))))

(defun keyboard-layout/pre-init-comint ()
  (kl|config comint-mode
    :description
    "Remap `comint' bindings."
    :loader
    (with-eval-after-load 'shell BODY)
    :common
    (dolist (m '(normal insert))
      (eval `(kl/evil-correct-keys `,m comint-mode-map
               "C-j"
               "C-k")))))

(defun keyboard-layout/pre-init-company ()
  (kl|config company
    :description
    "Remap `company' bindings."
    :loader
    (spacemacs|use-package-add-hook company :post-config BODY)
    :common
    (kl/correct-keys company-active-map
      "C-h"
      "C-j"
      "C-k"
      "C-l")))

(defun keyboard-layout/pre-init-ediff ()
  (kl|config ediff
    :description
    "Remap `ediff' bindings."
    :loader
    ;; HACK: ediff-mode-map is only defined when ediff is started
    (add-hook 'ediff-startup-hook #'(lambda () BODY))
    :common
    (kl/correct-keys ediff-mode-map
      "h"
      "j"
      "k"
      "l")))

(defun keyboard-layout/pre-init-elfeed ()
  (kl|config elfeed
    :description
    "Remap `elfeed' bindings."
    :loader
    (spacemacs|use-package-add-hook elfeed :post-config BODY)
    :common
    (progn
      (kl/evil-correct-keys 'evilified elfeed-search-mode-map
        "j"
        "k")
      (kl/evil-correct-keys 'evilified elfeed-show-mode-map
        "j"
        "k"
        "C-j"
        "C-k"))
    :bepo
    ;; HACK: The auto correction doesn't work... mystery.
    (evil-define-key 'evilified elfeed-search-mode-map
      "k" 'elfeed-search-live-filter)))

(defun keyboard-layout/pre-init-evil ()
  (kl|config evil
    :description
    "Remap `evil' bindings."
    :loader
    (with-eval-after-load 'evil BODY)
    :common
    (dolist (map kl--all-evil-states-but-insert)
      (kl/correct-keys map
        "h"
        "j"
        "k"
        "l"
        ;;
        "H"
        "J"
        "K"
        "L"))
    :bepo
    (progn
      (kl/set-in-all-evil-states-but-insert
        "é" 'evil-forward-word-begin
        "É" 'evil-forward-WORD-begin)
      (kl/set-in-state evil-inner-text-objects-map
        "é" 'evil-inner-word
        "É" 'evil-inner-WORD)
      (kl/set-in-state evil-outer-text-objects-map
        "é" 'evil-a-word
        "É" 'evil-a-WORD)
      (kl/set-in-all-evil-states-but-insert
        "«" 'evil-shift-left
        "»" 'evil-shift-right))
    :dvorak
    ;; Invert it twice to reset `k' and `K' for searching
    (dolist (map kl--all-evil-states-but-insert)
      (kl/correct-keys map
        "K"))
    :colemak-jkhl
    (progn
      (define-key evil-motion-state-map "J" 'evil-join)
      (define-key evil-motion-state-map "K" 'evil-window-bottom)
      (define-key evil-motion-state-map "H" 'evil-window-top)
      (define-key evil-motion-state-map "L" 'evil-lookup)))

  (kl|config evil-window
    :description
    "Remap `evil-window' bindings."
    :loader
    (with-eval-after-load 'evil-commands BODY)
    ;; :common
    ;; ;; FIXME: Not working
    ;; (kl/leader-correct-keys
    ;;   "wh"
    ;;   "wj"
    ;;   "wk"
    ;;   "wl"
    ;;   ;;
    ;;   "wH"
    ;;   "wJ"
    ;;   "wK"
    ;;   "wL")
    :bepo
    (progn
      (spacemacs/set-leader-keys
        "wé" 'other-window
        "wq" 'delete-window)
      (kl/leader-alias-of "é" "w"))))

;; HACK: These are defined by the spacemacs-bootstrap layer, and this is the
;; only I've found to make them stick.  An unfortunate consequence of using
;; `kl|config evil' twice is that user hooks for this configuration will be run
;; twice as well.
(defun keyboard-layout/post-init-evil ()
  (kl|config evil
    :description
    "Remap `evil' bindings."
    :colemak-jkhl
    (progn
      (define-key evil-normal-state-map "K" nil)
      (define-key evil-normal-state-map "L" 'spacemacs/evil-smart-doc-lookup))))

(defun keyboard-layout/pre-init-evil-cleverparens ()
  (kl|config evil-cleverparens
    :description
    "Remap `evil-cleverparens' bindings."
    :loader
    ;; (spacemacs|use-package-add-hook evil-cleverparens :post-init BODY)
    (with-eval-after-load 'evil-cleverparens BODY)
    :common
    (kl/evil-correct-keys 'normal evil-cleverparens-mode-map
      "h"
      "j"
      "k"
      "l")))

(defun keyboard-layout/pre-init-evil-escape ()
  (kl|config evil-escape
    :description
    "Change `evil-escape' default escape combination for a better one than `fd'."
    :loader
    (spacemacs|use-package-add-hook evil-escape :post-init BODY)
    :bepo
    (setq-default evil-escape-key-sequence "gq")
    :colemak-neio
    (setq-default evil-escape-key-sequence "tn")))

(defun keyboard-layout/pre-init-evil-evilified-state ()
  (kl|config evil-evilified-state
    :description
    "Remap `evil-evilified-state' bindings."
    :loader
    (with-eval-after-load 'evil-evilified-state BODY)
    :common
    (kl/correct-keys evil-evilified-state-map
      "h"
      "j"
      "k"
      "l")))

(defun keyboard-layout/pre-init-evil-lisp-state ()
  (kl|config evil-lisp-state
    :description
    "Remap `evil-lisp-state' bindings."
    :loader
    (with-eval-after-load 'evil-lisp-state BODY)
    :common
    (kl/correct-keys evil-lisp-state-map
      "h"
      "j"
      "k"
      "l"
      ;;
      "H"
      "J"
      "K"
      "L")))

(defun keyboard-layout/pre-init-evil-collection ()
  (kl|config evil-collection
    :description
    "Remap `evil-collection-magit' bindings."
    :loader
    (with-eval-after-load 'evil-collection-magit BODY)
    :common
    (dolist (state (if evil-collection-magit-use-y-for-yank
                       (list evil-collection-magit-state 'visual)
                     (list evil-collection-magit-state)))
      (kl/evil-correct-keys state magit-mode-map
        "j"
        "k"
        "C-j"
        "C-k"))
    (kl/evil-correct-keys 'normal evil-collection-magit-toggle-text-minor-mode-map
      "C-j")))

(defun keyboard-layout/pre-init-evil-surround ()
  (kl|config evil-surround
    :description
    "Remap `evil-surround' bindings."
    :loader
    (spacemacs|use-package-add-hook evil-surround :post-config BODY)
    :common
    (kl/evil-correct-keys 'visual evil-surround-mode-map "s")))

(defun keyboard-layout/pre-init-eyebrowse ()
  (kl|config eyebrowse
    :description
    "Remap `eyebrowse' keybindings conflicting with evil."
    :loader
    (spacemacs|use-package-add-hook eyebrowse :post-init BODY)
    :common
    (kl/correct-keys evil-motion-state-map "gj"))

  (kl|config eyebrowse
    :description
    "Remap `eyebrowse' keybindings conflicting with evil-commands."
    :loader
    (with-eval-after-load 'evil-commands BODY)
    :common
    (kl/correct-keys evil-motion-state-map "gJ")))

(defun keyboard-layout/pre-init-flycheck ()
  (kl|config flycheck-error-list
    :description
    "Remap `flycheck-error-list' bindings."
    :loader
    (spacemacs|use-package-add-hook flycheck :post-config BODY)
    :common
    (kl/evil-correct-keys 'evilified flycheck-error-list-mode-map
      "j"
      "k")))

(defun keyboard-layout/pre-init-helm ()
  (kl|config helm
    :description
    "Remap `helm' bindings."
    :loader
    (spacemacs|use-package-add-hook helm :post-config BODY)
    :common
    (kl/correct-keys helm-map
      "C-h"
      "C-j"
      "C-k"
      "C-l"))

  (kl|config helm-buffers
    :description
    "Remap `helm-buffers' bindings."
    :loader
    (with-eval-after-load 'helm-buffers BODY)
    :bepo
    ;; HACK: Forced to correct wrong behaviour
    (kl/set-in-state helm-buffer-map "C-s" 'helm-previous-line))

  (kl|config helm-files
    :description
    "Remap `helm-files' bindings."
    :loader
    (with-eval-after-load 'helm-files BODY)
    :bepo
    (progn
      ;; HACK: Forced to correct wrong behaviour
      (kl/set-in-state helm-find-files-map "C-s" 'helm-previous-line)
      (kl/set-in-state helm-find-files-map "C-k" 'helm-ff-run-grep)
      (kl/set-in-state helm-find-files-map "C-r" 'helm-maybe-exit-minibuffer)
      (kl/set-in-state helm-read-file-map "C-s" 'helm-previous-line)
      (kl/set-in-state helm-read-file-map "C-K" 'helm-previous-line))
    :neo
    (progn
      (kl/set-in-state helm-find-files-map "C-r" 'helm-previous-line)
      (kl/set-in-state helm-find-files-map "C-s" 'helm-next-line))
    :colemak-jkhl
    (progn
      ;; HACK: Forced to correct wrong behaviour
      (kl/set-in-state helm-find-files-map "C-h" 'helm-previous-line)
      (kl/set-in-state helm-find-files-map "C-j" 'helm-find-files-up-one-level)))

  (kl|config helm-locate
    :description
    "Remap `helm-locate' bindings."
    :loader
    (with-eval-after-load 'helm-locate BODY)
    :bepo
    (progn
      ;; HACK: Forced to correct wrong behaviour
      (kl/set-in-state helm-generic-files-map "C-s" 'helm-previous-line)
      (kl/set-in-state helm-generic-files-map "C-k" 'helm-ff-run-grep))
    :bepo
    (progn
      ;; HACK: Forced to correct wrong behaviour
      (kl/set-in-state helm-generic-files-map "C-n" 'helm-previous-line)
      (kl/set-in-state helm-generic-files-map "C-k" 'helm-ff-run-grep))))

(defun keyboard-layout/pre-init-imenu-list ()
  (kl|config imenu-list
    :description
    "Remap `imenu-list' bindings."
    :loader
    (spacemacs|use-package-add-hook imenu-list :post-config BODY)
    :common
    (kl/evil-correct-keys 'evilified imenu-list-major-mode-map
      "j"
      "k")))

(defun keyboard-layout/pre-init-ivy ()
  (kl|config ivy
    :description
    "Remap `ivy' bindings."
    :loader
    (spacemacs|use-package-add-hook ivy :post-config BODY)
    :common
    (progn
      (kl/correct-keys ivy-minibuffer-map
        "C-h"
        "C-j"
        "C-k"
        "C-l"))))

(defun keyboard-layout/pre-init-magit ()
  (kl|config magit
    :description
    "Remap `magit' bindings."
    :loader
    (spacemacs|use-package-add-hook magit :post-config BODY)
    :common
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
      (kl/correct-keys map
        "j"
        "k"
        "C-j"
        "C-k"))
    :bepo
    (progn
      (transient-suffix-put 'magit-dispatch "t" :key "j")
      (transient-suffix-put 'magit-dispatch "s" :key "k")
      (transient-suffix-put 'magit-dispatch "S" :key "K"))
    :colemak-jkhl
    (kl/evil-correct-keys 'visual magit-mode-map
      "j"
      "k")))

(defun keyboard-layout/pre-init-mu4e ()
  (kl|config mu4e
    :description
    "Remap navigation keys in `mu4e' headers and view mode."
    :loader
    (spacemacs|use-package-add-hook mu4e :post-config BODY)
    :common
    (dolist (map (list mu4e-headers-mode-map
                       mu4e-view-mode-map))
      (kl/evil-correct-keys 'evilified map
        "h"
        "j"
        "k"
        "l"
        "C-j"
        "C-k"))
    :bepo
    (dolist (map (list mu4e-headers-mode-map
                       mu4e-view-mode-map))
      (evil-define-key 'evilified map
        "è" 'mu4e-headers-mark-subthread
        "/" 'mu4e-headers-search))))

(defun keyboard-layout/pre-init-neotree ()
  (kl|config neotree
    :descripition
    "Remap `neotree' bindings."
    :loader
    (spacemacs|use-package-add-hook neotree :post-config BODY)
    :common
    (kl/evil-correct-keys 'evilified neotree-mode-map
      "h"
      "j"
      "k"
      "l"
      ;;
      "H"
      "J"
      "K"
      "L")
    :bepo
    (kl/set-in-state (evil-get-auxiliary-keymap neotree-mode-map 'evilified)
                     "h" 'neotree-hidden-file-toggle
                     "k" 'neotree-rename-node)))

(defun keyboard-layout/pre-init-org ()
  (kl|config org
    :description
    "Remap keys in `org-mode'."
    :loader
    (with-eval-after-load 'org BODY)
    :bepo
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
                 (kbd "M-L") 'move-to-window-line-top-bottom)))
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
        "C-S-l" nil)
      (evil-define-key 'normal evil-org-mode-map
        ;; ctsr
        "gt" 'org-forward-heading-same-level
        "gs" 'org-backward-heading-same-level
        ;; hjkl
        "gj" nil
        "gk" nil
        ;; additional
        (kbd "«") 'org-metaleft
        (kbd "»") 'org-metaright))
    :colemak-neio
    (progn
      (spacemacs|use-package-add-hook evil-org
        :post-config
        (progn
          (evil-define-key 'normal evil-org-mode-map
            "O" 'org-forward-heading-same-level
            "o" 'evil-forward-char
            "E" 'org-forward-element
            "I" 'org-backward-element
            "N" 'org-backward-heading-same-level))))
    :neo
    (progn
      (spacemacs|use-package-add-hook evil-org
        :post-config
        (progn
          (evil-define-key 'normal evil-org-mode-map
            "t" 'evil-forward-char
            "l" 'org-todo
            "gn" 'org-forward-heading-same-level
            "gr" 'org-backward-heading-same-level
            "gj" nil
            "gk" nil)
          (dolist (m '(normal insert))
            (eval `(evil-define-key ',m evil-org-mode-map
                     ;; snrt
                     (kbd "M-s") 'org-metaleft
                     (kbd "M-n") 'org-metadown
                     (kbd "M-r") 'org-metaup
                     (kbd "M-t") 'org-metaright
                     (kbd "M-S") 'org-shiftmetaleft
                     (kbd "M-N") 'org-shiftmetadown
                     (kbd "M-R") 'org-shiftmetaup
                     (kbd "M-T") 'org-shiftmetaright
                     ;; hjkl
                     (kbd "M-h") 'capitalize-word
                     (kbd "M-j") 'transpose-chars
                     (kbd "M-k") 'kill-sentence
                     (kbd "M-l") 'move-to-window-line-top-bottom
                     (kbd "M-H") 'capitalize-word
                     (kbd "M-J") 'transpose-chars
                     (kbd "M-K") 'kill-sentence
                     (kbd "M-L") 'move-to-window-line-top-bottom)))
          (spacemacs/set-leader-keys-for-major-mode 'org-mode
            ;; snrt
            "C-S-s" 'org-shiftcontrolleft
            "C-S-n" 'org-shiftcontroldown
            "C-S-r" 'org-shiftcontrolup
            "C-S-t" 'org-shiftcontrolright
            ;; hjkl
            "C-S-h" nil
            "C-S-j" nil
            "C-S-k" nil
            "C-S-l" nil))))))

(defun keyboard-layout/pre-init-org-agenda ()
  (kl|config org-agenda
    :description
    "Remap `org-agenda' bindings."
    :loader
    (spacemacs|use-package-add-hook org-agenda :post-config BODY)
    :common
    (kl/evil-correct-keys 'evilified org-agenda-mode-map
      "j"
      "k"
      "M-h"
      "M-j"
      "M-k"
      "M-l")))

(defun keyboard-layout/pre-init-ranger ()
  (kl|config ranger
    :description
    "Remap navigation keys in `ranger'."
    :loader
    (spacemacs|use-package-add-hook ranger :post-config BODY)
    :common
    (kl/correct-keys ranger-mode-map
      "h"
      "j"
      "k"
      "l")))

(defun keyboard-layout/pre-init-twittering-mode ()
  (kl|config twittering-mode
    :description
    "Remap navigation keys in `twittering-mode'."
    :loader
    (spacemacs|use-package-add-hook twittering-mode :post-config BODY)
    :common
    (kl/correct-keys twittering-mode-map
      "h"
      "j"
      "k"
      "l"
      ;;
      "H"
      "J"
      "K"
      "L")))

(defun keyboard-layout/pre-init-undo-tree ()
  (kl|config undo-tree
    :description
    "Remap navigation keys in `undo-tree-visualizer-mode'."
    :loader
    (spacemacs|use-package-add-hook undo-tree :post-config BODY)
    :common
    (kl/evil-correct-keys 'evilified undo-tree-visualizer-mode-map
      "h"
      "j"
      "k"
      "l")))
