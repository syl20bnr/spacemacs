;;; keybindings.el --- dvorak Layer key bindings File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Alejandro Catalina <alecatfel@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;

;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(dvorak|config ace-window
  :description
  "Remap `ace-window' bindings, and change the keys to the ones
   on the central row."
  :loader
  (spacemacs|use-package-add-hook ace-window :post-init BODY)
  :config
  (progn
    ;; HACK: Manual binding, otherwise conflicts with evil-window auto
    ;; remapping.
    (setq aw-keys '(?t ?e ?s ?i ?r ?u ?n ?a))))

(dvorak|config buffer-move
  :description
  "Remap `buffer-move' bindings."
  :loader
  (with-eval-after-load 'buffer-move BODY)
  :config
  (dvorak/leader-correct-keys
    "bmh"
    "bmj"
    "bmk"
    "bml"))

(dvorak|config comint-mode
  :description
  "Remap `comint-mode' bindings."
  :loader
  (with-eval-after-load 'shell BODY)
  :config
  (dolist (m '(normal insert))
    (eval `(dvorak/evil-correct-keys `,m comint-mode-map
             "C-j"
             "C-k"))))

(dvorak|config company
  :description
  "Remap `company' bindings."
  :loader
  (spacemacs|use-package-add-hook company :post-config BODY)
  :config
  (dvorak/correct-keys company-active-map
    "C-h"
    "C-j"
    "C-k"
    "C-l"))

(dvorak|config elfeed
  :description
  "Remap `elfeed' bindings."
  :loader
  (spacemacs|use-package-add-hook elfeed :post-config BODY)
  :config
  (dvorak/evil-correct-keys 'evilified elfeed-show-mode-map
    "C-j"
    "C-k"))

(dvorak|config evil-evilified-state
  :description
  "Remap `evil-evilified-state' bindings."
  :loader
  (with-eval-after-load 'evil-evilified-state BODY)
  :config
  (dvorak/correct-keys evil-evilified-state-map
    "h"
    "j"
    "k"
    "l"))

(dvorak|config evil-window
  :description
  "Remap `evil-window' bindings."
  :config
  (progn
    (dvorak/leader-correct-keys
      "wh"
      "wj"
      "wk"
      "wl"
      ;;
      "wH"
      "wJ"
      "wK"
      "wL")))

(dvorak|config eyebrowse
  :description
  "Remap `eyebrowse' keybindings conflicting with evil."
  :loader
  (spacemacs|use-package-add-hook eyebrowse :post-init BODY)
  :config
  (dvorak/correct-keys evil-motion-state-map
    "gj"
    "gJ"))

(dvorak|config helm
  :description
  "Remap `helm' bindings."
  :loader
  (spacemacs|use-package-add-hook helm :post-config BODY)
  :config
  (dvorak/correct-keys helm-map
    "C-h"
    "C-j"
    "C-k"
    "C-l"))

(dvorak|config helm-buffers
  :description
  "Remap `helm-buffers' bindings."
  :loader
  (with-eval-after-load 'helm-buffers BODY)
  :config
  ;; HACK: Forced to correct wrong behaviour
  (dvorak/set-in-state helm-buffer-map "C-s" 'helm-previous-line))

(dvorak|config helm-files
  :description
  "Remap `helm-files' bindings."
  :loader
  (with-eval-after-load 'helm-files BODY)
  :config
  (progn
    ;; HACK: Forced to correct wrong behaviour
    (dvorak/set-in-state helm-find-files-map "C-s" 'helm-previous-line)
    (dvorak/set-in-state helm-find-files-map "C-k" 'helm-ff-run-grep)
    (dvorak/set-in-state helm-find-files-map "C-r" 'helm-maybe-exit-minibuffer)
    (dvorak/set-in-state helm-read-file-map "C-s" 'helm-previous-line)
    (dvorak/set-in-state helm-read-file-map "C-K" 'helm-previous-line)))

(dvorak|config helm-locate
  :description
  "Remap `helm-locate' bindings."
  :loader
  (with-eval-after-load 'helm-locate BODY)
  :config
  (progn
    ;; HACK: Forced to correct wrong behaviour
    (dvorak/set-in-state helm-generic-files-map "C-s" 'helm-previous-line)
    (dvorak/set-in-state helm-generic-files-map "C-k" 'helm-ff-run-grep)))

(dvorak|config ivy
  :description
  "Remap `ivy' bindings."
  :loader
  (spacemacs|use-package-add-hook ivy :post-config BODY)
  :config
  (dvorak/correct-keys ivy-minibuffer-map
    "C-h"
    "C-j"
    "C-k"
    "C-l"))

(dvorak|config magit
  :description
  "Remap `magit' bindings."
  :loader
  (spacemacs|use-package-add-hook magit :post-config BODY)
  :config
  (progn
    (dvorak/evil-correct-keys evil-magit-state magit-mode-map
      "j"
      "k"
      "C-j"
      "C-k")
    (dolist (map (list magit-branch-section-map
                       magit-commit-section-map
                       magit-file-section-map
                       magit-hunk-section-map
                       magit-remote-section-map
                       magit-staged-section-map
                       magit-unstaged-section-map
                       ))
      (dvorak/correct-keys map
        "j"
        "k"
        "C-j"
        "C-k"))
    (magit-change-popup-key 'magit-dispatch-popup :actions ?t ?j)
    (magit-change-popup-key 'magit-dispatch-popup :actions ?h ?k)
    (magit-change-popup-key 'magit-dispatch-popup :actions ?H ?K)))

(dvorak|config ranger
  :description
  "Remap navigation keys in `ranger'."
  :loader
  (with-eval-after-load 'ranger BODY)
  :config
  (dvorak/correct-keys ranger-mode-map
    "h"
    "j"
    "k"
    "l"
    ;;
    "H"
    "K"
    "J"
    "L"))

(dvorak|config spacemacs
  :description
  "Customize some `spacemacs' bindings."
  :config
  (dvorak/leader-correct-keys
    "jh"
    "jj"
    "jk"
    "jl"
    ;;
    "jJ"
    ))

(dvorak|config org
  :description
  "Remap keys in `org-mode' for dvorak layout."
  :loader
  (with-eval-after-load 'org BODY)
  :config
  (progn
    (dolist (m '(normal insert))
      (eval `(evil-define-key ',m evil-org-mode-map
               ;; Navigation
               (kbd "M-h") 'org-metaleft
               (kbd "M-t") 'org-metadown
               (kbd "M-n") 'org-metaup
               (kbd "M-s") 'org-metaright
               (kbd "M-H") 'org-shiftmetaleft
               (kbd "M-T") 'org-shiftmetadown
               (kbd "M-N") 'org-shiftmetaup
               (kbd "M-S") 'org-shiftmetarigh)))
    (evil-define-key 'normal evil-org-mode-map
      "o" 'evil-backward-word-begin
      "O" 'evil-backward-WORD-begin
      "l" 'org-todo
      "t" 'evil-next-line
      "gj" nil
      "gk" 'outline-up-heading
      "gl" nil
      "gh" 'outline-previous-visible-heading
      "gt" 'org-forward-heading-same-level
      "gn" 'org-backward-heading-same-level
      "gs" 'outline-next-visible-heading)))

(dvorak|config org-agenda
  :description
  "Remap `org-agenda' keys."
  :loader
  (spacemacs|use-package-add-hook org-agenda :post-config BODY)
  :config
  (progn
    (evilified-state-evilify-map org-agenda-mode-map
      :mode org-agenda-mode
      :bindings
      "t" 'org-agenda-next-line
      "n" 'org-agenda-previous-line
      "l" 'org-agenda-todo
      (kbd "M-t") 'org-agenda-next-item
      (kbd "M-n") 'org-agenda-previous-item
      (kbd "M-h") 'org-agenda-earlier
      (kbd "M-s") 'org-agenda-later
      (kbd "gd") 'org-agenda-toggle-time-grid
      (kbd "gr") 'org-agenda-redo
      (kbd "M-RET") 'org-agenda-show-and-scroll-up
      (kbd "RET") 'org-agenda-goto)))

(dvorak|config evil-mu4e
  :description
  "Remap `evil-mu4e' bindings."
  :loader
  (with-eval-after-load 'evil-mu4e BODY)
  :config
  (progn
    (evil-define-key 'motion mu4e-main-mode-map
      "j" nil
      "k" nil
      "t" 'evil-next-line
      "n" 'evil-previous-line)
    (evil-define-key 'motion mu4e-headers-mode-map
      "j" nil
      "k" nil
      "t" 'evil-next-line
      "n" 'evil-previous-line
      "\C-t" 'mu4e-headers-next
      "\C-n" 'mu4e-headers-prev)
    (evil-define-key 'motion mu4e-view-mode-map
      "\C-t" 'mu4e-view-headers-next
      "\C-n" 'mu4e-view-headers-prev)))

(dvorak/correct-keys evil-motion-state-map
  "h"
  "j"
  "k"
  "l")
