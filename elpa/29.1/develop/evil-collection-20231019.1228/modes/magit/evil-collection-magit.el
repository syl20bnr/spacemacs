;;; evil-collection-magit.el --- Evil-based key bindings for magit

;; Copyright (C) 2015-2016, 2021 Justin Burkett

;; Author: Justin Burkett <justin@burkett.cc>
;; Maintainer: Justin Burkett <justin@burkett.cc>
;; James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; Package-Requires: ((emacs "26.3") (evil "1.2.3") (magit "2.6.0"))
;; Homepage: https://github.com/emacs-evil/evil-collection
;; Version: 0.4.1

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library configures Magit and Evil to play well with each
;; other. For some background see https://github.com/magit/evil-magit/issues/1.

;; See the README at
;; https://github.com/emacs-evil/evil-collection/tree/master/modes/magit
;; for a table describing the key binding changes.

;;; Code:

(require 'evil-collection)
(require 'magit nil t)

(defvar magit-blame-mode-map)
(defvar magit-blame-read-only-mode-map)
(defvar magit-blob-mode-map)
(defvar magit-cherry-mode-map)
(defvar magit-diff-mode-map)
(defvar magit-file-section-map)
(defvar magit-hunk-section-map)
(defvar magit-log-mode-map)
(defvar magit-log-read-revs-map)
(defvar magit-log-select-mode-map)
(defvar magit-process-mode-map)
(defvar magit-reflog-mode-map)
(defvar magit-refs-mode-map)
(defvar magit-repolist-mode-map)
(defvar magit-status-mode-map)
(defvar magit-submodule-list-mode-map)

(defconst evil-collection-magit-maps
  '(evil-collection-magit-toggle-text-minor-mode-map
    magit-log-mode-map ; -> parent: `magit-mode-map'
    magit-status-mode-map ; -> parent: `magit-mode-map'
    magit-blame-mode-map
    magit-blame-read-only-mode-map
    magit-blob-mode-map
    magit-submodule-list-mode-map ; -> parent: `magit-repolist-mode-map'
    magit-repolist-mode-map
    magit-mode-map))

(defcustom evil-collection-magit-use-y-for-yank t
  "When non nil (Default is t),
replace \"y\" for `magit-show-refs' with
\"yy\" for `evil-collection-magit-yank-whole-line',
\"ys\" for `magit-copy-section-value',
\"yb\" for `magit-copy-buffer-revision' and
\"yr\" for `magit-show-refs'.
This keeps \"y\" for `magit-show-refs',
in the help popup (`magit-dispatch')."
  :group 'magit
  :type 'boolean)

(defcustom evil-collection-magit-want-horizontal-movement nil
  "When non nil, use \"h\" and \"l\" for horizontal movement in
most magit buffers. The old \"h\" for help is moved to \"H\" and
similarly for the old \"l\" for the log popup. The old \"L\" is
then put on \"C-l\"."
  :group 'magit
  :type  'boolean)

(defcustom evil-collection-magit-use-z-for-folds nil
  "When non nil, use \"z\" as a prefix for common vim fold commands, such as
  - z1 Reset visibility to level 1 for all sections
  - z2 Reset visibility to level 2 for all sections
  - z3 Reset visibility to level 3 for all sections
  - z4 Reset visibility to level 4 for all sections
  - za Toggle a section
  - zo Show section
  - zO Show sections recursively
  - zc Hide section
  - zC Hide sections recursively
  - zr Same as z4.

When this option is enabled, the stash popup is available on \"Z\"."
  :group 'magit
  :type  'boolean)

(defcustom evil-collection-magit-use-$-for-end-of-line t
  "When non nil, use \"$\" for `evil-end-of-line'.

Move `magit-process-buffer' to \"`\"."
  :group 'magit
  :type 'boolean)

(defcustom evil-collection-magit-use-0-for-beginning-of-line t
  "When non nil, use \"0\" for `evil-beginning-of-line'.

Move `magit-diff-default-context' to \"~\"."
  :group 'magit
  :type 'boolean)

(defcustom evil-collection-magit-state (if evil-collection-magit-use-y-for-yank 'normal 'motion)
  "State to use for most magit buffers."
  :group 'magit
  :type  'symbol)

;; without this set-mark-command activates visual-state which is just annoying
;; and introduces possible bugs
(defun evil-collection-magit-remove-visual-activate-hook ()
  (when (derived-mode-p 'magit-mode)
    (remove-hook 'activate-mark-hook 'evil-visual-activate-hook t)))
(add-hook 'evil-local-mode-hook 'evil-collection-magit-remove-visual-activate-hook)

(defun evil-collection-magit-maybe-deactivate-mark ()
  "Deactivate mark if region is active. Used for ESC binding."
  (interactive)
  (when (region-active-p) (deactivate-mark)))

(defvar evil-collection-magit-emacs-to-default-state-modes
  '(git-commit-mode)
  "Modes that should be in the default evil state.")

(defvar evil-collection-magit-emacs-to-evil-collection-magit-state-modes
  '(git-rebase-mode
    magit-mode
    magit-cherry-mode
    magit-diff-mode
    magit-log-mode
    magit-log-select-mode
    magit-process-mode
    magit-reflog-mode
    magit-refs-mode
    magit-revision-mode
    magit-stash-mode
    magit-stashes-mode
    magit-status-mode)
  "Modes that switch from Emacs state to `evil-collection-magit-state'.")

(defvar evil-collection-magit-default-to-evil-collection-magit-state-modes
  '(magit-blob-mode
    magit-gitflow-mode)
  "Modes that switch from default state to `evil-collection-magit-state'.")

(defvar evil-collection-magit-untouched-modes
  ;; TODO do something here
  '(git-popup-mode
    magit-blame-mode
    magit-blame-read-only-mode)
  "Modes whose evil states are unchanged.")

(defvar evil-collection-magit-ignored-modes
  '(git-commit-major-mode
    magit-auto-revert-mode
    magit-blame-put-keymap-before-view-mode
    magit-diff-mode
    magit-merge-preview-mode
    transient-resume-mode
    magit-rebase-mode
    magit-wip-after-save-mode
    magit-wip-after-save-local-mode-major-mode
    magit-wip-after-save-local-mode
    magit-wip-after-apply-mode
    magit-wip-before-change-mode
    magit-wip-initial-backup-mode
    magit-wip-mode
    ;; gh
    magit-gh-pulls-mode
    ;; git-gutter
    git-gutter-mode
    git-gutter-mode-major-mode
    git-gutter+-commit-mode
    git-gutter+-mode
    git-gutter+-enable-fringe-display-mode
    git-gutter+-enable-default-display-mode)
  "Currently ignored modes. They are collected here for testing purposes.")

(defun evil-collection-magit-set-initial-states ()
  "Set the initial state for relevant modes."
  (dolist (mode (append evil-collection-magit-emacs-to-evil-collection-magit-state-modes
                        evil-collection-magit-default-to-evil-collection-magit-state-modes))
    (evil-set-initial-state mode evil-collection-magit-state))
  (dolist (mode evil-collection-magit-emacs-to-default-state-modes)
    (evil-set-initial-state mode evil-default-state)))

(defun evil-collection-magit-revert-initial-states ()
  "Revert the initial state for modes to their values before evil-collection-magit was loaded."
  (dolist (mode (append evil-collection-magit-emacs-to-evil-collection-magit-state-modes
                        evil-collection-magit-emacs-to-default-state-modes))
    (evil-set-initial-state mode 'emacs))
  (dolist (mode evil-collection-magit-default-to-evil-collection-magit-state-modes)
    (evil-set-initial-state mode evil-default-state)))

(defvar evil-collection-magit-section-maps
  '(magit-branch-section-map
    magit-commit-section-map
    magit-commit-message-section-map
    magit-error-section-map
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
    magit-untracked-section-map

    ;; new ones that I haven't looked at yet
    magit-button-section-map
    magit-commitbuf-section-map
    magit-diff-section-map
    magit-diffbuf-section-map
    magit-diffstat-section-map
    magit-headers-section-map
    magit-log-section-map
    magit-message-section-map
    ;; FIXME: deal with new bindings in this one
    magit-module-section-map
    magit-modules-section-map
    magit-processbuf-section-map
    magit-process-section-map
    magit-pulls-section-map
    magit-unmerged-section-map
    magit-status-section-map
    magit-worktree-section-map)
  "All magit section maps. For testing purposes only at the moment.")

;; Old way of excluding newlines
;; (when evil-collection-magit-use-y-for-yank
;;   (dolist (map evil-collection-magit-section-maps)
;;     (when (and map (keymapp (symbol-value map)))
;;       (map-keymap
;;        (lambda (_ def)
;;          (when (commandp def)
;;            (evil-set-command-property def :exclude-newline t)))
;;        (symbol-value map)))))

(defvar evil-collection-magit-in-visual-pre-command)

(defun evil-collection-magit--around-visual-pre-command (orig-func &rest args)
  (let ((evil-collection-magit-in-visual-pre-command t))
    (apply orig-func args)))

(defun evil-collection-magit--filter-args-visual-expand-region (arglist)
  ;; pretend that the command has the :exclude-newline property by rewriting the
  ;; EXCLUDE-NEWLINE arg to this function
  (cons (and (bound-and-true-p evil-collection-magit-in-visual-pre-command)
             (null (car arglist))
             (eq (evil-visual-type) 'line)
             (derived-mode-p 'magit-mode))
        ;; shouldn't be necessary, but this will prevent it from failing if an
        ;; arg is added.
        (cdr arglist)))

(when (and (fboundp 'advice-add) evil-collection-magit-use-y-for-yank)
  (advice-add 'evil-visual-pre-command
              :around #'evil-collection-magit--around-visual-pre-command)
  (advice-add 'evil-visual-expand-region
              :filter-args #'evil-collection-magit--filter-args-visual-expand-region))

;; keep visual state for magit-section movement commands
(dolist (cmd '(magit-section-forward-sibling
               magit-section-forward
               magit-section-backward-sibling
               magit-section-backward
               magit-section-up))
  (evil-set-command-property cmd :keep-visual t))

(defvar evil-collection-magit-mode-map-bindings
  (let ((states (if evil-collection-magit-use-y-for-yank
                    `(,evil-collection-magit-state visual)
                  `(,evil-collection-magit-state))))
    (append
     `((,states magit-mode-map "g")
       (,states magit-mode-map "\C-j"   magit-section-forward          "n")
       (,states magit-mode-map "gj"    magit-section-forward-sibling  "M-n")
       (,states magit-mode-map "]"     magit-section-forward-sibling  "M-n")
       (,states magit-mode-map "\C-k"   magit-section-backward         "p")
       (,states magit-mode-map "gk"    magit-section-backward-sibling "M-p")
       (,states magit-mode-map "["     magit-section-backward-sibling "M-p")
       (,states magit-mode-map "gr"    magit-refresh                  "g")
       (,states magit-mode-map "gR"    magit-refresh-all              "G")
       (,states magit-mode-map "x"     magit-delete-thing             "k")
       (,states magit-mode-map "X"     magit-file-untrack             "K")
       (,states magit-mode-map "-"     magit-revert-no-commit         "v")
       (,states magit-mode-map "_"     magit-revert                   "V")
       (,states magit-mode-map "p"     magit-push                     "P")
       (,states magit-mode-map "o"     magit-reset-quickly            "x")
       (,states magit-mode-map "O"     magit-reset                    "X")
       (,states magit-mode-map "|"     magit-git-command              ":")
       (,states magit-mode-map "'"     magit-submodule                "o")
       (,states magit-mode-map "\""    magit-subtree                  "O")
       (,states magit-mode-map "="     magit-diff-less-context        "-")
       (,states magit-mode-map "j"     evil-next-line)
       (,states magit-mode-map "k"     evil-previous-line)
       (,states magit-mode-map "gg"    evil-goto-first-line)
       (,states magit-mode-map "G"     evil-goto-line)
       (,states magit-mode-map "\C-d"   evil-scroll-down)
       (,states magit-mode-map "\C-f"   evil-scroll-page-down)
       (,states magit-mode-map "\C-b"   evil-scroll-page-up)
       (,states magit-mode-map ":"     evil-ex)
       (,states magit-mode-map "q"     magit-mode-bury-buffer)

       ;; these are to fix the priority of the log mode map and the magit mode map
       ;; FIXME: Conflict between this and revert. Revert seems more important here
       ;; (,states magit-log-mode-map "-" magit-log-half-commit-limit    "-")
       (,states magit-log-mode-map "=" magit-log-toggle-commit-limit  "=")

       ;; https://github.com/emacs-evil/evil-collection/issues/406
       ;; Use kbd here for S-SPC and S-DEL so evil-collection-define-key can
       ;; parse definition correctly;.
       (,states magit-mode-map ,(kbd "S-SPC") magit-diff-show-or-scroll-up   "SPC")
       (,states magit-mode-map ,(kbd "S-DEL") magit-diff-show-or-scroll-down "DEL")

       ((,evil-collection-magit-state) magit-mode-map ,(kbd evil-toggle-key) evil-emacs-state))

     (if (eq evil-search-module 'evil-search)
         `((,states magit-mode-map "/" evil-ex-search-forward)
           (,states magit-mode-map "n" evil-ex-search-next)
           (,states magit-mode-map "N" evil-ex-search-previous)
           (,states magit-blame-read-only-mode-map "n" evil-ex-search-next)
           (,states magit-blame-read-only-mode-map "N" evil-ex-search-previous))
       `((,states magit-mode-map "/" evil-search-forward)
         (,states magit-mode-map "n" evil-search-next)
         (,states magit-mode-map "N" evil-search-previous)
         (,states magit-blame-read-only-mode-map "n" evil-search-next)
         (,states magit-blame-read-only-mode-map "N" evil-search-previous)))

     `((,states magit-status-mode-map "gz"  magit-jump-to-stashes)
       (,states magit-status-mode-map "gt"  magit-jump-to-tracked)
       (,states magit-status-mode-map "gn"  magit-jump-to-untracked)
       (,states magit-status-mode-map "gu"  magit-jump-to-unstaged)
       (,states magit-status-mode-map "gs"  magit-jump-to-staged)
       (,states magit-status-mode-map "gfu" magit-jump-to-unpulled-from-upstream)
       (,states magit-status-mode-map "gfp" magit-jump-to-unpulled-from-pushremote)
       (,states magit-status-mode-map "gpu" magit-jump-to-unpushed-to-upstream)
       (,states magit-status-mode-map "gpp" magit-jump-to-unpushed-to-pushremote)
       (,states magit-status-mode-map "gh"  magit-section-up                       "^")
       (,states magit-diff-mode-map "gd" magit-jump-to-diffstat-or-diff "j")
       ((visual) magit-diff-mode-map "y" magit-copy-section-value)
       ;; NOTE This is now transient-map and the binding is C-g.
       ;; ((emacs) magit-popup-mode-map "<escape>" "q")
       )

     (when evil-collection-magit-want-horizontal-movement
       `((,states magit-mode-map "H"    magit-dispatch    "h")
         (,states magit-mode-map "L"    magit-log         "l")
         (,states magit-mode-map "\C-l"  magit-log-refresh "L")
         (,states magit-mode-map "h"    evil-backward-char)
         (,states magit-mode-map "l"    evil-forward-char)))

     (when evil-want-C-u-scroll
       `((,states magit-mode-map "\C-u" evil-scroll-up)))

     (if evil-collection-magit-use-y-for-yank
         `((,states magit-mode-map "v"    evil-visual-line)
           (,states magit-mode-map "V"    evil-visual-line)
           (,states magit-mode-map "\C-w"  evil-window-map)
           (,states magit-mode-map "y")
           (,states magit-mode-map "yy"   evil-collection-magit-yank-whole-line)
           (,states magit-mode-map "yr"   magit-show-refs            "y")
           (,states magit-mode-map "ys"   magit-copy-section-value   "C-w")
           (,states magit-mode-map "yb"   magit-copy-buffer-revision "M-w")
           ((visual) magit-mode-map "y"   magit-copy-section-value))
       `((,states magit-mode-map "v" set-mark-command)
         (,states magit-mode-map "V" set-mark-command)
         (,states magit-mode-map ,(kbd "<escape>") evil-collection-magit-maybe-deactivate-mark)))

     (when evil-collection-magit-use-$-for-end-of-line
       `((,states magit-mode-map "$" evil-end-of-line)
         (,states magit-mode-map "`" magit-process-buffer)))

     (when evil-collection-magit-use-0-for-beginning-of-line
       `((,states magit-mode-map "0" evil-beginning-of-line)
         (,states magit-mode-map "~" magit-diff-default-context)))

     (when evil-collection-magit-use-z-for-folds
       `((,states magit-mode-map "Z"    magit-stash)
         (,states magit-mode-map "z")
         (,states magit-mode-map "z1"   magit-section-show-level-1-all)
         (,states magit-mode-map "z2"   magit-section-show-level-2-all)
         (,states magit-mode-map "z3"   magit-section-show-level-3-all)
         (,states magit-mode-map "z4"   magit-section-show-level-4-all)
         (,states magit-mode-map "za"   magit-section-toggle)
         (,states magit-mode-map "zc"   magit-section-hide)
         (,states magit-mode-map "zC"   magit-section-hide-children)
         (,states magit-mode-map "zo"   magit-section-show)
         (,states magit-mode-map "zO"   magit-section-show-children)
         (,states magit-mode-map "zr"   magit-section-show-level-4-all)))))
  "evil-collection-magit bindings for major modes. Each element of this list
takes the form

\(EVIL-STATE MAGIT-MAP NEW-KEY DEF ORIG-KEY\).

ORIG-KEY is only used for testing purposes, and
denotes the original magit key for this command.")

(dolist (binding evil-collection-magit-mode-map-bindings)
  (when binding
    (dolist (state (nth 0 binding))
      (evil-collection-define-key
        state (nth 1 binding) (nth 2 binding) (nth 3 binding)))))

(defvar evil-collection-magit-minor-mode-map-bindings
  `(((,evil-collection-magit-state visual) magit-blob-mode-map "gj" magit-blob-next     "n")
    ((,evil-collection-magit-state visual) magit-blob-mode-map "gk" magit-blob-previous "p")
    ((,evil-collection-magit-state visual) git-commit-mode-map "gk" git-commit-prev-message "M-p")
    ((,evil-collection-magit-state visual) git-commit-mode-map "gj" git-commit-next-message "M-n")
    ((normal) magit-blame-read-only-mode-map "j"    evil-next-line)
    ((normal) magit-blame-read-only-mode-map "\C-j"  magit-blame-next-chunk                 "n")
    ((normal) magit-blame-read-only-mode-map "gj"   magit-blame-next-chunk                 "n")
    ((normal) magit-blame-read-only-mode-map "gJ"   magit-blame-next-chunk-same-commit     "N")
    ((normal) magit-blame-read-only-mode-map "k"    evil-previous-line)
    ((normal) magit-blame-read-only-mode-map "\C-k"  magit-blame-previous-chunk             "p")
    ((normal) magit-blame-read-only-mode-map "gk"   magit-blame-previous-chunk             "p")
    ((normal) magit-blame-read-only-mode-map "gK"   magit-blame-previous-chunk-same-commit "P"))
  "evil-collection-magit bindings for minor modes. Each element of
this list takes the form

\(EVIL-STATE MAGIT-MAP NEW-KEY DEF ORIG-KEY)\.

ORIG-KEY is only used for testing purposes, and
denotes the original magit key for this command.")

(dolist (binding evil-collection-magit-minor-mode-map-bindings)
  (when binding
    (dolist (state (nth 0 binding))
      ;; TODO: Maybe switch to `evil-define-minor-mode-key'
      (evil-collection-define-key
        state (nth 1 binding) (nth 2 binding) (nth 3 binding)))))

;; Make relevant maps into overriding maps so that they shadow the global evil
;; maps by default
(dolist (map (list magit-mode-map
                   magit-cherry-mode-map
                   magit-mode-map
                   magit-blob-mode-map
                   magit-diff-mode-map
                   magit-log-mode-map
                   magit-log-select-mode-map
                   magit-reflog-mode-map
                   magit-status-mode-map
                   magit-log-read-revs-map
                   magit-process-mode-map
                   magit-refs-mode-map))
  (evil-make-overriding-map map (if evil-collection-magit-use-y-for-yank
                                    'all
                                  evil-collection-magit-state)))

(evil-make-overriding-map magit-blame-read-only-mode-map 'normal)

(eval-after-load 'magit-gh-pulls
  `(evil-make-overriding-map magit-gh-pulls-mode-map ',evil-collection-magit-state))

;; Need to refresh evil keymaps when blame mode is entered.
(add-hook 'magit-blame-mode-hook 'evil-normalize-keymaps)

(evil-set-initial-state 'magit-repolist-mode 'normal)
(evil-collection-define-key 'normal 'magit-repolist-mode-map
  "m" 'magit-repolist-mark
  "u" 'magit-repolist-unmark
  "f" 'magit-repolist-fetch
  (kbd "RET") 'magit-repolist-status
  (kbd "gr")  'magit-list-repositories)
(add-hook 'magit-repolist-mode-hook 'evil-normalize-keymaps)

(evil-set-initial-state 'magit-submodule-list-mode 'normal)
(evil-collection-define-key 'normal 'magit-submodule-list-mode-map
  (kbd "RET") 'magit-repolist-status
  (kbd "gr")  'magit-list-submodules)
(add-hook 'magit-submodule-list-mode-hook 'evil-normalize-keymaps)


(eval-after-load 'git-rebase
  `(progn
     ;; for the compiler
     (defvar git-rebase-mode-map)
     (defvar evil-collection-magit-rebase-commands-w-descriptions
       ;; nil in the first element means don't bind here
       '(("p"    git-rebase-pick           "pick = use commit")
         ("r"    git-rebase-reword         "reword = use commit, but edit the commit message")
         ("e"    git-rebase-edit           "edit = use commit, but stop for amending")
         ("s"    git-rebase-squash         "squash = use commit, but meld into previous commit")
         ("f"    git-rebase-fixup          "fixup = like \"squash\", but discard this commit's log message")
         ("x"    git-rebase-exec           "exec = run command (the rest of the line) using shell")
         ("d"    git-rebase-kill-line      "drop = remove commit" "k")
         ("u"    git-rebase-undo           "undo last change")
         (nil    with-editor-finish        "tell Git to make it happen")
         (nil    with-editor-cancel        "tell Git that you changed your mind, i.e. abort")
         ("k"    evil-previous-line        "move point to previous line" "p")
         ("j"    evil-next-line            "move point to next line" "n")
         ("M-k"  git-rebase-move-line-up   "move the commit at point up" "\M-p")
         ("M-j"  git-rebase-move-line-down "move the commit at point down" "\M-n")
         (nil    git-rebase-show-commit    "show the commit at point in another buffer")))

     (dolist (cmd evil-collection-magit-rebase-commands-w-descriptions)
       (when (car cmd)
         (evil-collection-define-key evil-collection-magit-state 'git-rebase-mode-map
           (kbd (car cmd)) (nth 1 cmd))))

     (evil-make-overriding-map git-rebase-mode-map evil-collection-magit-state)

     (defun evil-collection-magit-add-rebase-messages ()
       "Remove evil-state annotations and reformat git-rebase buffer."
       (goto-char (point-min))
       (let ((inhibit-read-only t)
             (state-regexp (format "<%s-state> " evil-collection-magit-state))
             (aux-map (evil-get-auxiliary-keymap git-rebase-mode-map evil-collection-magit-state)))
         (save-excursion
           (save-match-data
             (goto-char (point-min))
             (when (and (boundp 'git-rebase-show-instructions)
                        git-rebase-show-instructions
                        (re-search-forward
                         (concat "^" (regexp-quote comment-start) "\\s-+p, pick") nil t))
               (goto-char (line-beginning-position))
               (flush-lines (concat "^" (regexp-quote comment-start) ".+ = "))
               (dolist (cmd evil-collection-magit-rebase-commands-w-descriptions)
                 (insert
                  (format "%s %-8s %s\n"
                          comment-start
                          (if (and (car cmd)
                                   (eq (nth 1 cmd)
                                       (lookup-key aux-map (kbd (car cmd)))))
                              (car cmd)
                            (replace-regexp-in-string
                             state-regexp ""
                             (substitute-command-keys
                              (format "\\[%s]" (nth 1 cmd)))))
                          (nth 2 cmd)))))))))
     (remove-hook 'git-rebase-mode-hook 'git-rebase-mode-show-keybindings)
     (add-hook 'git-rebase-mode-hook 'evil-collection-magit-add-rebase-messages t)))

;; section maps: evil's auxiliary maps don't work here, because these maps are
;; text overlays

(defun evil-collection-magit-stage-untracked-file-with-intent ()
  "Call `magit-stage-untracked' with optional arg."
  (interactive)
  (when (and (derived-mode-p 'magit-mode)
             (magit-apply--get-selection)
             (eq (magit-diff-type) 'untracked))
    (magit-stage-untracked t)))

(defvar evil-collection-magit-original-section-bindings
  `((,(copy-keymap magit-file-section-map) "\C-j" magit-diff-visit-worktree-file)
    (,(copy-keymap magit-hunk-section-map) "\C-j" magit-diff-visit-worktree-file))
  "For testing purposes only. The original magit keybindings that
evil-collection-magit affects.")

(defun evil-collection-magit-adjust-section-bindings ()
  "Revert changed bindings in section maps generated by evil-collection-magit"
  (define-key magit-file-section-map "I"
    'evil-collection-magit-stage-untracked-file-with-intent)
  (define-key magit-file-section-map "\C-j" nil)  ; breaking change
  (define-key magit-hunk-section-map "\C-j" nil)) ; breaking change

(defun evil-collection-magit-revert-section-bindings ()
  "Revert changed bindings in section maps generated by evil-collection-magit"
  (define-key magit-file-section-map "I" nil)
  (define-key magit-file-section-map "\C-j" 'magit-diff-visit-file-worktree)
  (define-key magit-hunk-section-map "\C-j" 'magit-diff-visit-file-worktree))

;; Popups

(defvar evil-collection-magit-dispatch-popup-backup
  (copy-tree (get 'magit-dispatch 'transient--layout) t))
(defvar evil-collection-magit-popup-keys-changed nil)

(defvar evil-collection-magit-popup-changes
  (append
   (when evil-collection-magit-use-z-for-folds
     '((magit-dispatch "Z" "%" magit-worktree)
       (magit-dispatch "z" "Z" magit-stash)))
   (when evil-collection-magit-want-horizontal-movement
     '((magit-dispatch "L" "\C-l" magit-log-refresh)
       (magit-dispatch "l" "L" magit-log)))
   '((magit-branch "x" "X" magit-branch-reset)
     (magit-branch "k" "x" magit-branch-delete)
     (magit-dispatch "o" "'" magit-submodule)
     (magit-dispatch "O" "\"" magit-subtree)
     (magit-dispatch "V" "_" magit-revert)
     (magit-dispatch "X" "O" magit-reset)
     (magit-dispatch "v" "-" magit-reverse)
     (magit-dispatch "k" "x" magit-discard)
     (magit-remote "k" "x" magit-remote-remove)
     ;; FIXME: how to properly handle a popup with a key that appears twice (in
     ;; `transient-define-prefix' definition)? Currently we rely on:
     ;; 1. first call to `evil-collection-magit-change-popup-key' changes the first "V"
     ;;    entry of `magit-revert' (the first entry in `transient-define-prefix'
     ;;    definition of `magit-revert'), second call changes the second "V".
     ;; 2. the remapping here are in the same order as in `magit-revert'
     ;;    definition
     (magit-revert "V" "_" magit-revert-and-commit)
     (magit-revert "V" "_" magit-sequencer-continue)
     (magit-tag    "k" "x" magit-tag-delete)))
  "Changes to popup keys")

(defun evil-collection-magit-change-popup-key (popup from to &rest _args)
  "Wrap `magit-change-popup-key'."
  (transient-suffix-put popup from :key to))

(defun evil-collection-magit-adjust-popups ()
  "Adjust popup keys to match evil-collection-magit."
  (unless evil-collection-magit-popup-keys-changed
    (dolist (change evil-collection-magit-popup-changes)
      (apply #'evil-collection-magit-change-popup-key change))
    (setq evil-collection-magit-popup-keys-changed t)))

(defun evil-collection-magit-revert-popups ()
  "Revert popup keys changed by evil-collection-magit."
  (put 'magit-dispatch 'transient--layout evil-collection-magit-dispatch-popup-backup)
  (setq evil-collection-magit-popup-keys-changed nil))

;;;###autoload
(defun evil-collection-magit-init ()
  "This function completes the setup of evil-collection-magit. It is called
automatically when evil-collection-magit-setup is called.. The only reason to use
this function is if you've called `evil-collection-magit-revert' and wish to
go back to evil-collection-magit behavior."
  (interactive)
  (evil-collection-magit-adjust-section-bindings)
  (evil-collection-magit-adjust-popups)
  (evil-collection-magit-set-initial-states))

;;;###autoload
(defun evil-collection-magit-revert ()
  "Revert changes by evil-collection-magit that affect default evil+magit behavior."
  (interactive)
  (evil-collection-magit-revert-section-bindings)
  (evil-collection-magit-revert-popups)
  (evil-collection-magit-revert-initial-states)
  (message "evil-collection-magit reverted"))

(define-minor-mode evil-collection-magit-toggle-text-minor-mode
  "Minor mode used to enabled toggle key in `text-mode' after
using `evil-collection-magit-toggle-text-mode'"
  :keymap (make-sparse-keymap))

(evil-collection-define-key 'normal
  'evil-collection-magit-toggle-text-minor-mode-map
  "\C-t" 'evil-collection-magit-toggle-text-mode
  "\\"   'evil-collection-magit-toggle-text-mode)
(evil-collection-define-key evil-collection-magit-state 'magit-mode-map
  "\C-t" 'evil-collection-magit-toggle-text-mode
  "\\"   'evil-collection-magit-toggle-text-mode)

(defvar evil-collection-magit-last-mode nil
  "Used to store last magit mode before entering text mode using
`evil-collection-magit-toggle-text-mode'.")

(defun evil-collection-magit-toggle-text-mode ()
  "Switch to `text-mode' and back from magit buffers."
  (interactive)
  (cond ((derived-mode-p 'magit-mode)
         (setq evil-collection-magit-last-mode major-mode)
         (message "Switching to text-mode")
         (text-mode)
         (evil-collection-magit-toggle-text-minor-mode 1)
         (evil-normalize-keymaps))
        ((and (eq major-mode 'text-mode)
              (functionp evil-collection-magit-last-mode))
         (message "Switching to %s" evil-collection-magit-last-mode)
         (evil-collection-magit-toggle-text-minor-mode -1)
         (evil-normalize-keymaps)
         (funcall evil-collection-magit-last-mode)
         (magit-refresh)
         (evil-change-state evil-collection-magit-state))
        (t
         (user-error "evil-collection-magit-toggle-text-mode unexpected state"))))

(evil-define-operator evil-collection-magit-yank-whole-line
  (beg end type register yank-handler)
  "Yank whole line."
  :motion evil-line-or-visual-line
  (interactive "<R><x>")
  (evil-yank beg end type register yank-handler))

;;;###autoload
(defun evil-collection-magit-setup ()
  "Set up `evil' bindings for `magit'."

  (evil-collection-define-key 'normal 'magit-blame-mode-map
    "q" 'magit-blame-quit)
  (evil-collection-define-key 'normal 'magit-blame-read-only-mode-map
    "q" 'magit-blame-quit)

  (evil-collection-magit-init))

;;; evil-collection-magit.el ends soon
(provide 'evil-collection-magit)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; evil-collection-magit.el ends here
