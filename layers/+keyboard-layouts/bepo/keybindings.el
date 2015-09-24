;;; keybindings.el --- bepo Layer extensions File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Fabien Dubosson & Contributors
;;
;; Author: Fabien Dubosson <fabien.dubosson@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(bepo|rebind "company"
  :description
  "Remap company keybindings to navigate between lines in company completion."
  :loader
  (spacemacs|use-package-add-hook company :post-config BODY)
  :remap
  (progn
    (bepo/set-in-state company-active-map
      (kbd "C-t") 'company-select-next
      (kbd "C-s") 'company-select-previous
      (kbd "C-r") 'company-complete-selection
      ))
  :switch
  (progn
    (bepo/set-in-state company-active-map
      (kbd "C-j") nil
      (kbd "C-k") nil
      (kbd "C-l") nil
    )))


(bepo|rebind "evil"
  :description
  "Remap evil navigation keys, and map unused `é', `«', `»' keys to something
   useful:
     - `é' as an alias for word since `w' is hard to reach on bepo layout.
     - `«' and `»' as directs access to indent/unindent."
  :remap
  (progn
    (bepo/set-in-all-evil-states-but-insert
      "c" 'evil-backward-char
      "t" 'evil-next-line
      "s" 'evil-previous-line
      "r" 'evil-forward-char
      ;;
      "C" 'evil-window-top
      "T" 'evil-join
      "S" 'spacemacs/evil-smart-doc-lookup
      "R" 'evil-window-bottom
      ))
  :switch
  (progn
    (bepo/set-in-all-evil-states-but-insert
      "h" 'evil-replace
      "j" 'evil-find-char-to
      "k" 'evil-substitute
      "l" 'evil-change
      ;;
      "H" 'evil-replace-state
      "J" 'evil-find-char-to-backward
      "K" 'evil-change-whole-line
      "L" 'evil-change-line
      ))
  :additional
  (progn
    (bepo/set-in-all-evil-states-but-insert
      "é" 'evil-forward-word-begin
      "É" 'evil-forward-WORD-begin
      )
    (bepo/set-in-state evil-inner-text-objects-map
      "é" 'evil-inner-word
      "É" 'evil-inner-WORD
      )
    (bepo/set-in-state evil-outer-text-objects-map
      "é" 'evil-a-word
      "É" 'evil-a-WORD
      )
    (bepo/set-in-state evil-normal-state-map
      "«" 'evil-shift-left
      "»" 'evil-shift-right
      )))


(bepo|rebind "evil-surround"
  :description
  "Remap `s' in visual mode to `k' in evil-surround."
  :loader
  (spacemacs|use-package-add-hook evil-surround :post-init BODY)
  :remap
  (progn
    (evil-define-key 'visual evil-surround-mode-map
      "s" 'evil-previous-visual-line
      ))
  :switch
  (progn
    (evil-define-key 'visual evil-surround-mode-map
      "k" 'evil-surround-region
      ))
  :additional
  (progn
    (evil-define-key 'visual evil-surround-mode-map
      "K" 'evil-Surround-region
      )))


(bepo|rebind "evil-window"
  :description
  "Remap `SPC w' mapping for manipulating windows. Add `SPC é'as
   an alias for it."
  :remap
  (progn
    (evil-leader/set-key
      "wc" 'evil-window-left
      "wt" 'evil-window-down
      "ws" 'evil-window-up
      "wr" 'evil-window-right
      ;;
      "wC" nil ; see special
      "wT" 'evil-window-move-very-bottom
      "wS" 'evil-window-move-very-top
      "wR" 'evil-window-move-far-right
      ))
  :switch
  (progn
    (evil-leader/set-key
      "wh" nil
      "wj" nil
      "wk" 'split-window-below
      "wl" 'delete-window
      ;;
      "wH" 'spacemacs/rotate-windows
      "wJ" nil
      "wK" 'split-window-below-and-focus
      "wL" 'ace-delete-window 
      ))
  :additional
  (progn
    (evil-leader/set-key
      "wé" 'other-window
      "wq" 'delete-window
      "é" (lookup-key evil-leader--default-map "w")
      ))
  :special
  (spacemacs|use-package-add-hook ace-window
    :post-init
    (evil-leader/set-key
      "wC" 'evil-window-move-far-left
      )))


(bepo|rebind "helm"
  :description
  "Remap keybindings to navigate between lines in helm."
  :loader
  (spacemacs|use-package-add-hook helm :post-init (progn (helm-mode +1) BODY))
  :remap
  (progn
    (bepo/set-in-states (list helm-map
                              helm-find-files-map
                              helm-read-file-map
                              helm-generic-files-map)
      (kbd "C-t") 'helm-next-line
      (kbd "C-s") 'helm-previous-line
      ))
  :switch
  (progn
    (bepo/set-in-states (list helm-map
                              helm-find-files-map
                              helm-read-file-map
                              helm-generic-files-map)
      (kbd "C-j") 'helm-toggle-resplit-and-swap-windows
      (kbd "C-k") 'helm-ff-run-grep
      )))


(bepo|rebind "magit"
  :description
  "Remap magit keybindings. Only magit-status and commit-popup
   are remapped yet. Kind of WIP."
  :loader
  (spacemacs|use-package-add-hook magit :post-config BODY)
  :remap
  (progn
    (dolist (map (list magit-status-mode-map
                       magit-branch-section-map
                       magit-commit-section-map
                       magit-file-section-map
                       magit-hunk-section-map
                       magit-module-commit-section-map
                       magit-remote-section-map
                       magit-staged-section-map
                       magit-stash-section-map
                       magit-stashes-section-map
                       magit-tag-section-map
                       magit-unpulled-section-map
                       magit-unpushed-section-map
                       magit-unstaged-section-map
                       magit-untracked-section-map))
      (spacemacs|evilify-map map
        :mode magit-status-mode
        :bindings
        ;; Remap CRTS
        (kbd "c") 'magit-commit-popup
        (kbd "r") 'magit-rebase-popup
        (kbd "t") 'evil-next-line
        (kbd "s") 'evil-previous-line
        ;; Remap HJKL from default spacemacs
        (kbd "h") 'magit-discard
        (kbd "j") 'magit-tag-popup
        (kbd "k") 'magit-stage
        (kbd "l") 'magit-log-popup
        ;; Correct others
        (kbd "v") 'magit-revert-popup
        (kbd "g") 'magit-refresh
        ))
    (dolist (map (list magit-popup-mode-map
                       magit-popup-help-mode-map
                       magit-popup-sequence-mode-map))
      (spacemacs|evilify-map map
        :mode magit-commit-mode
        :bindings
        (kbd "c") 'magit-commit
        (kbd "s") 'magit-commit-squash
        (kbd "S") 'magit-commit-instant-squash
        ))
    ))


(bepo|rebind "neotree"
  :descripition
  "Remap navigation keys to bepo layout in neotree.
   Note: The normal mapping is not used here, in order to have
   `h' for showing/hidding hidden files. It is better than having
   it to rename a node."
  :loader
  (with-eval-after-load 'neotree (add-hook 'neotree-mode-hook (lambda () BODY)))
  :remap
  (progn
    (bepo/set-in-state evil-motion-state-local-map
      "c" 'spacemacs/neotree-collapse-or-up
      "t" 'evil-next-visual-line
      "s" 'evil-previous-visual-line
      "r" 'spacemacs/neotree-expand-or-open
      ;;
      "C" 'neotree-select-previous-sibling-node
      "T" 'neotree-select-down-node
      "S" 'neotree-select-up-node
      "R" 'neotree-select-next-sibling-node
      ))
  :switch
  (progn
    (bepo/set-in-state evil-motion-state-local-map
      "h" 'neotree-hidden-file-toggle
      "j" nil
      "k" 'neotree-rename-node
      "l" 'neotree-create-node
      ;;
      "H" 'neotree-change-root
      "J" nil
      "K" nil
      "L" nil
      )))


(bepo|rebind "org"
  :description
  "Remap keys in org-mode."
  :loader
  (defun org/post-init-org () BODY)
  :remap
  (progn
    (evil-define-key 'normal evil-org-mode-map
      "t" 'evil-next-visual-line
      )
    (dolist (m '(normal insert))
      (eval `(evil-define-key ',m evil-org-mode-map
               (kbd "M-c") 'org-metaleft
               (kbd "M-t") 'org-metadown
               (kbd "M-s") 'org-metaup
               (kbd "M-r") 'org-metaright
               (kbd "M-C") 'org-shiftmetaleft
               (kbd "M-T") 'org-shiftmetadown
               (kbd "M-S") 'org-shiftmetaup
               (kbd "M-R") 'org-shiftmetaright
               )))
    (evil-leader/set-key-for-mode 'org-mode
      "mC" 'org-shiftleft
      "mT" 'org-shiftdown
      "mS" 'org-shiftup
      "ms" 'org-schedule
      "mR" 'org-shiftright
      "m C-S-c" 'org-shiftcontrolleft
      "m C-S-t" 'org-shiftcontroldown
      "m C-S-s" 'org-shiftcontrolup
      "m C-S-r" 'org-shiftcontrolright
      "mtC" 'org-table-move-column-left
      "mtc" 'org-table-previous-field
      "mtT" 'org-table-move-row-down
      "mtt" 'org-table-next-row
      "mtS" 'org-table-move-row-up
      "mtR" 'org-table-move-column-right
      "mtr" 'org-table-next-field
      )
    (evil-define-key 'normal evil-org-mode-map
      "gt" 'org-forward-heading-same-level
      "gs" 'org-backward-heading-same-level
      ))
  :switch
  (progn
    (evil-define-key 'normal evil-org-mode-map
      "j" 'org-todo
      )
    (dolist (m '(normal insert))
      (eval `(evil-define-key ',m evil-org-mode-map
               (kbd "M-h") 'capitalize-word
               (kbd "M-j") 'transpose-chars
               (kbd "M-k") nil ; TODO find it!
               (kbd "M-l") 'move-to-window-line-top-bottom
               (kbd "M-H") 'capitalize-word
               (kbd "M-J") 'transpose-chars
               (kbd "M-K") nil ; TODO find it!
               (kbd "M-L") 'move-to-window-line-top-bottom
               )))
    (evil-leader/set-key-for-mode 'org-mode
      "mH" 'org-refile
      "mJ" 'org-show-todo-tree
      "mK" nil
      "mKr" 'org-demote-subtree
      "mKc" 'org-promote-subtree
      "mKt" 'org-move-subtree-down
      "mKs" 'org-move-subtree-up
      "mL" 'evil-org-recompute-clocks
      "m C-S-h" nil
      "m C-S-j" nil
      "m C-S-k" nil
      "m C-S-l" nil
      "mtH" nil
      "mth" 'org-table-recalculate
      "mtJ" nil
      "mtj" nil
      "mtK" nil
      "mtk" 'org-table-sort-lines
      "mtL" nil
      "mtl" 'org-table-convert
      "mtjf" 'org-table-toggle-formula-debugger
      "mtjo" 'org-table-toggle-coordinate-overlays
      "ms" 'org-schedule
      )
    (evil-define-key 'normal evil-org-mode-map
      "gj" nil
      "gk" nil
      ))
  :additional
  (evil-define-key 'normal evil-org-mode-map
    (kbd "«") 'org-metaleft
    (kbd "»") 'org-metaright
    ))


(bepo|rebind "ranger"
  :description
  "Remap navigation keys in ranger."
  :loader
  (spacemacs|use-package-add-hook ranger :post-init (with-eval-after-load 'evil BODY))
  :remap
  (progn
    (evil-define-key 'normal ranger-mode-map
      "c" 'ranger-up-directory
      "t" 'ranger-next-file
      "s" 'ranger-prev-file
      "r" 'ranger-find-file
      ;;
      "C" 'ranger-prev-history
      "T" 'ranger-next-subdir
      "S" 'ranger-prev-subdir
      "R" 'ranger-next-history
      )) 
  :switch
  (progn
    (evil-define-key 'normal ranger-mode-map
      "h" nil
      "j" nil
      "k" nil
      "l" nil
      ;;
      "H" 'dired-do-rename
      "K" 'eshell
      "J" nil
      "L" nil
      )))


(bepo|rebind "spacemacs-specific"
  :description
  "Remap spacemacs defined key bindings.
   Note: `SPC T' is not remaped because it correspond to an important group. Use
     original `SPC J' instead.
   Note: The character `s' for snippets have been move to `y' instead of `k'
     because of the mnemonic with yasnippet."
  :remap
  (progn
    (evil-leader/set-key
      "jc" 'spacemacs/push-mark-and-goto-beginning-of-line
      "jt" 'sp-newline
      "js" 'spacemacs/evil-goto-next-line-and-indent
      "jr" 'spacemacs/push-mark-and-goto-end-of-line
      ;;
      "jT" 'spacemacs/split-and-new-line
      ;;
      "it" 'spacemacs/evil-insert-line-below
      "iT" 'spacemacs/insert-line-below-no-indent
      ))
  :switch
  (progn
    (evil-leader/set-key
      "jh" nil
      "jj" nil
      "jk" nil
      "jl" nil
      ;;
      "jJ" nil
      ;;
      "ij" nil
      "ik" nil
      ;; For the followings, they are set in additional and special
      "iJ" nil
      "iK" nil
      ;;
      "iSc" nil
      "iSe" nil
      "iSw" nil
      ))
  :additional
  (progn
    (evil-leader/set-key
      "iy" 'spacemacs/helm-yas
      "iYc" 'aya-create
      "iYe" 'spacemacs/auto-yasnippet-expand
      "iYw" 'aya-persist-snippet
      ))
  :special
  (progn
    (spacemacs|use-package-add-hook helm-c-yasnippet
      :post-init
      (evil-leader/set-key
        "is" 'spacemacs/evil-insert-line-above
        ))
    (spacemacs|use-package-add-hook auto-yasnippet
      :post-init
      (evil-leader/set-key
        "iS" 'spacemacs/insert-line-above-no-indent
        ))
    (spacemacs/declare-prefix "iY" "auto-yasnippet")))
