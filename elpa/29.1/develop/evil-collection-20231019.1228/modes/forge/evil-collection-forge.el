;;; evil-collection-forge.el --- Evil-based key bindings for forge

;; Copyright (C) 2021 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Package-Requires: ((emacs "26.3") (evil "1.2.3"))
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
;; other. For some background see:
;; https://github.com/emacs-evil/evil-collection/issues/543.

;;; Code:

(require 'evil-collection)
(require 'forge nil t)
(require 'transient nil t)

(defcustom evil-collection-forge-use-y-for-yank t
  "View `evil-collection-magit-state' for more information."
  :group 'magit
  :type 'boolean)

(defcustom evil-collection-forge-state
  (if evil-collection-forge-use-y-for-yank 'normal 'motion)
  "State to use for forge."
  :group 'magit
  :type  'symbol)

(defvar forge-add-default-bindings)

;;;###autoload
(defun evil-collection-forge-setup ()
  "Set up `evil' bindings for `magit'."
  ;; The latest release tag of forge doesn't include
  ;; `forge-add-default-bindings' yet, it will throw an error:
  ;;
  ;;    void-variable `forge-add-default-bindings'
  ;;
  ;; for GNU Guix and MELPA stable users.
  (when (bound-and-true-p forge-add-default-bindings)
    (message "Setting `forge-add-default-bindings' to nil in `evil-collection-forge-setup'.
To suppress this message you can set this variable to nil in your init.el file.")
    (setq forge-add-default-bindings nil))
  (let ((states (if evil-collection-forge-use-y-for-yank
                    `(,evil-collection-forge-state visual)
                  `(,evil-collection-forge-state))))
    (dolist (state states)
      (evil-collection-define-key state 'magit-mode-map "@" 'forge-dispatch)

      (evil-collection-define-key
        state 'magit-commit-section-map
        [remap magit-browse-thing] 'forge-browse-dwim)
      (evil-collection-define-key
        state 'magit-remote-section-map
        [remap magit-browse-thing] 'forge-browse-remote)
      (evil-collection-define-key
        state 'magit-branch-section-map
        [remap magit-browse-thing] 'forge-browse-branch)

      (evil-collection-define-key
       state 'magit-commit-section-map (kbd "C-c C-v") 'forge-visit-topic)
      (evil-collection-define-key
       state 'magit-branch-section-map (kbd "C-c C-v") 'forge-visit-topic)))

  ;; https://github.com/magit/forge/blob/master/lisp/forge.el

  (transient-append-suffix 'magit-dispatch "!"
    '("@" "Forge" forge-dispatch)) ;; N -> @

  (transient-append-suffix 'magit-fetch "m"
    '("n" "forge topics" forge-pull))
  (transient-append-suffix 'magit-fetch "n"
    '("N" "forge notifications" forge-pull-notifications))
  (transient-append-suffix 'magit-pull "m"
    '("n" "forge topics" forge-pull))
  (transient-append-suffix 'magit-pull "n"
    '("N" "forge notifications" forge-pull-notifications))
  (transient-append-suffix 'magit-branch "w"
    '("f" "pull-request" forge-checkout-pullreq))
  (transient-append-suffix 'magit-branch "W"
    '("F" "from pull-request" forge-branch-pullreq))
  (transient-append-suffix 'magit-worktree "c"
    '("n" "pull-request worktree" forge-checkout-worktree))
  (transient-append-suffix 'magit-status-jump "w"
    '("Np" "pull-requests" forge-jump-to-pullreqs))
  (transient-append-suffix 'magit-status-jump "Np"
    '("Ni" "issues" forge-jump-to-issues))
  (transient-append-suffix 'magit-merge "a"
    '(7 "M" "Merge using API" forge-merge)))

;;; evil-collection-forge.el ends soon
(provide 'evil-collection-forge)
;; Local Variables:
;; End:
;;; evil-collection-forge.el ends here
