;;; packages.el --- Org Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
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


(defconst org-packages
  '(
    company
    company-emoji
    emoji-cheat-sheet-plus
    evil-org
    evil-surround
    gnuplot
    (helm-org-rifle :toggle (configuration-layer/layer-used-p 'helm))
    htmlize
    ;; ob, org, org-agenda and org-contacts are installed by `org-contrib'
    (ob :location built-in)
    (org :location elpa :min-version "9.6.1")
    (org-agenda :location built-in)
    (org-wild-notifier
                :toggle org-enable-notifications)
    (org-contacts :toggle org-enable-org-contacts-support)
    org-contrib
    (org-vcard :toggle org-enable-org-contacts-support)
    (org-brain :toggle org-enable-org-brain-support)
    (org-expiry :location built-in)
    ; temporarily point org-journal to dalanicolai fork until dalanicolai's
    ; PR's https://github.com/bastibe/org-journal/pulls get merged
    (org-journal
     :location (recipe :fetcher github :repo "dalanicolai/org-journal")
     :toggle org-enable-org-journal-support)
    org-download
    (org-jira :toggle org-enable-jira-support)
    org-mime
    (org-modern :toggle org-enable-modern-support)
    org-pomodoro
    org-present
    org-cliplink
    org-rich-yank
    (org-projectile :requires projectile)
    (ox-epub :toggle org-enable-epub-support)
    (ox-twbs :toggle org-enable-bootstrap-support)
    ;; use a for of ox-gfm to fix index generation
    (ox-gfm :location (recipe :fetcher github :repo "syl20bnr/ox-gfm")
            :toggle org-enable-github-support)
    (org-re-reveal :toggle org-enable-reveal-js-support)
    persp-mode
    (ox-hugo :toggle org-enable-hugo-support)
    (ox-jira :toggle org-enable-jira-support)
    (org-trello :toggle org-enable-trello-support)
    (org-sticky-header :toggle org-enable-sticky-header)
    (verb :toggle org-enable-verb-support)
    (org-roam :toggle org-enable-roam-support)
    (org-roam-ui :toggle org-enable-roam-ui)
    (valign :toggle org-enable-valign)
    (org-appear :toggle org-enable-appear-support)
    (org-transclusion :toggle org-enable-transclusion-support)
    helm
    (ox-asciidoc :toggle org-enable-asciidoc-support)))

(defun org/post-init-company ()
  (spacemacs|add-company-backends :backends company-capf :modes org-mode))

(defun org/post-init-company-emoji ()
  (spacemacs|add-company-backends :backends company-emoji :modes org-mode))

(defun org/post-init-emoji-cheat-sheet-plus ()
  (add-hook 'org-mode-hook 'spacemacs/delay-emoji-cheat-sheet-hook))

(defun org/init-evil-org ()
  (use-package evil-org
    :defer t
    :init
    (progn
      (add-hook 'org-mode-hook 'spacemacs//evil-org-mode)
      (setq evil-org-use-additional-insert t
            evil-org-key-theme `(textobjects
                                 navigation
                                 additional
                                 ,@(when org-want-todo-bindings '(todo)))))
    :config
    (spacemacs|hide-lighter evil-org-mode)))

(defun org/post-init-evil-surround ()
  (add-hook 'org-mode-hook 'spacemacs/org-setup-evil-surround))

(defun org/init-gnuplot ()
  (use-package gnuplot
    :defer t
    :init (spacemacs/set-leader-keys-for-major-mode 'org-mode
            "tp" 'org-plot/gnuplot)))

(defun org/init-helm-org-rifle ()
  (use-package helm-org-rifle
    :defer t
    :init (spacemacs/set-leader-keys "ao/" 'helm-org-rifle)))

(defun org/init-htmlize ()
  (use-package htmlize
    :defer t))

(defun org/init-ob ()
  (use-package ob
    :defer t
    :init
    (progn
      (define-advice org-babel-execute-src-block (:before (&rest _) load-lang)
        (org-babel-do-load-languages 'org-babel-load-languages
                                     org-babel-load-languages)
        (advice-remove 'org-babel-execute-src-block
                       'org-babel-execute-src-block@load-lang))
      ;; Fix redisplay of inline images after a code block evaluation.
      (add-hook 'org-babel-after-execute-hook 'spacemacs/ob-fix-inline-images))))

(defun org/init-org ()
  (use-package org
    :defer (spacemacs/defer)
    :commands (orgtbl-mode)
    :init
    (progn
      (spacemacs|require-when-dumping 'org)
      (setq org-clock-persist-file (concat spacemacs-cache-directory
                                           "org-clock-save.el")
            org-id-locations-file (concat spacemacs-cache-directory
                                          ".org-id-locations")
            org-publish-timestamp-directory (concat spacemacs-cache-directory
                                                    ".org-timestamps/")
            org-directory "~/org" ;; needs to be defined for `org-default-notes-file'
            org-default-notes-file (expand-file-name "notes.org" org-directory)
            org-log-done 'time
            org-startup-with-inline-images t
            org-latex-prefer-user-labels t
            org-image-actual-width nil
            org-src-fontify-natively t
            org-src-tab-acts-natively t
            ;; this is consistent with the value of
            ;; `helm-org-headings-max-depth'.
            org-imenu-depth 8)

      (when org-todo-dependencies-strategy
        (setq org-enforce-todo-dependencies t)
        (add-hook 'org-after-todo-statistics-hook
                  (cl-case org-todo-dependencies-strategy
                    (naive-auto #'spacemacs/org-summary-todo-naive-auto)
                    (semiauto #'spacemacs/org-summary-todo-semiauto))))

      (with-eval-after-load 'org-indent
        (spacemacs|hide-lighter org-indent-mode))

      (defmacro spacemacs|org-emphasize (fname char)
        "Make function for setting the emphasis in org mode"
        `(defun ,fname () (interactive)
                (org-emphasize ,char)))

      ;; Follow the confirm and abort conventions
      (with-eval-after-load 'org-capture
        (defun spacemacs//org-capture-start ()
          "Make sure that the keybindings are available for org capture."
          (spacemacs/set-leader-keys-for-minor-mode 'org-capture-mode
            dotspacemacs-major-mode-leader-key 'org-capture-finalize
            "a" 'org-capture-kill
            "c" 'org-capture-finalize
            "k" 'org-capture-kill
            "r" 'org-capture-refile)
          ;; Evil bindins seem not to be applied until at least one
          ;; Evil state is executed
          (evil-normal-state))
        ;; Must be done everytime we run org-capture otherwise it will
        ;; be ignored until insert mode is entered.
        (add-hook 'org-capture-mode-hook 'spacemacs//org-capture-start))

      (with-eval-after-load 'org-src
        (spacemacs/set-leader-keys-for-minor-mode 'org-src-mode
          dotspacemacs-major-mode-leader-key 'org-edit-src-exit
          "c" 'org-edit-src-exit
          "a" 'org-edit-src-abort
          "k" 'org-edit-src-abort))

      (autoload #'org-clock-jump-to-current-clock "org-clock")
      (add-hook 'org-mode-hook 'dotspacemacs//prettify-spacemacs-docs)

      (let ((dir (configuration-layer/get-layer-local-dir 'org)))
        (setq org-export-async-init-file (concat dir "org-async-init.el")))

      ;; Insert key for org-mode and markdown a la C-h k
      ;; from SE https://emacs.stackexchange.com/a/2208
      (defun spacemacs/insert-keybinding-org (key)
        "Ask for a key then insert its description.
Will work on both org-mode and any mode that accepts plain html."
        (interactive "kType key sequence: ")
        (let* ((tag "@@html:<kbd>@@ %s @@html:</kbd>@@"))
          (if (null (equal key "\r"))
              (insert
               (format tag (help-key-description key nil)))
            (insert (format tag ""))
            (forward-char -8))))

      (dolist (prefix `(
                        ("mb" . "babel")
                        ("mC" . ,(org-clocks-prefix))
                        ("md" . "dates")
                        ("me" . "export")
                        ("mf" . "feeds")
                        ("mi" . "insert")
                        ("miD" . "download")
                        ("mm" . "more")
                        ("ms" . "trees/subtrees")
                        ("mT" . "toggles")
                        ("mt" . "tables")
                        ("mtd" . "delete")
                        ("mti" . "insert")
                        ("mtt" . "toggle")
                        ("mx" . "text")))

        (spacemacs/declare-prefix-for-mode 'org-mode (car prefix) (cdr prefix)))
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "'" 'org-edit-special
        "c" 'org-capture

        ;; Clock
        ;; These keybindings should match those under the "aoC" prefix (below)
        "Cc" 'org-clock-cancel
        "Cd" 'org-clock-display
        "Ce" 'org-evaluate-time-range
        "Cg" 'org-clock-goto
        "Ci" 'org-clock-in
        "CI" 'org-clock-in-last
        "Cj" 'spacemacs/org-clock-jump-to-current-clock
        "Co" 'org-clock-out
        "CR" 'org-clock-report
        "Cr" 'org-resolve-clocks

        "dd" 'org-deadline
        "ds" 'org-schedule
        "dt" 'org-time-stamp
        "dT" 'org-time-stamp-inactive
        "ee" 'org-export-dispatch
        "fi" 'org-feed-goto-inbox
        "fu" 'org-feed-update-all

        "a" 'org-agenda
        "[" 'org-agenda-file-to-front
        "]" 'org-remove-file

        "p" 'org-priority

        "Tc" 'org-toggle-checkbox
        "Te" 'org-toggle-pretty-entities
        "Ti" 'org-toggle-inline-images
        "Tn" 'org-num-mode
        "Tl" 'org-toggle-link-display
        "Tt" 'org-show-todo-tree
        "TT" 'org-todo
        "TV" 'space-doc-mode
        "Tx" 'org-latex-preview

        ;; More cycling options (timestamps, headlines, items, properties)
        "L" 'org-shiftright
        "H" 'org-shiftleft
        "J" 'org-shiftdown
        "K" 'org-shiftup

        ;; Change between TODO sets
        "C-S-l" 'org-shiftcontrolright
        "C-S-h" 'org-shiftcontrolleft
        "C-S-j" 'org-shiftcontroldown
        "C-S-k" 'org-shiftcontrolup

        ;; Subtree editing
        "sa" 'org-toggle-archive-tag
        "sA" 'org-archive-subtree-default
        "sb" 'org-tree-to-indirect-buffer
        "sd" 'org-cut-subtree
        "sy" 'org-copy-subtree
        "sp" 'org-paste-subtree
        "sh" 'org-promote-subtree
        "sj" 'org-move-subtree-down
        "sk" 'org-move-subtree-up
        "sl" 'org-demote-subtree
        "sn" 'org-narrow-to-subtree
        "sw" 'widen
        "sr" 'org-refile
        "ss" 'org-sparse-tree
        "sS" 'org-sort

        ;; tables
        "ta" 'org-table-align
        "tb" 'org-table-blank-field
        "tc" 'org-table-convert
        "tdc" 'org-table-delete-column
        "tdr" 'org-table-kill-row
        "te" 'org-table-eval-formula
        "tE" 'org-table-export
        "tf" 'org-table-field-info
        "th" 'org-table-previous-field
        "tH" 'org-table-move-column-left
        "tic" 'org-table-insert-column
        "tih" 'org-table-insert-hline
        "tiH" 'org-table-hline-and-move
        "tir" 'org-table-insert-row
        "tI" 'org-table-import
        "tj" 'org-table-next-row
        "tJ" 'org-table-move-row-down
        "tK" 'org-table-move-row-up
        "tl" 'org-table-next-field
        "tL" 'org-table-move-column-right
        "tn" 'org-table-create
        "tN" 'org-table-create-with-table.el
        "tr" 'org-table-recalculate
        "tR" 'org-table-recalculate-buffer-tables
        "ts" 'org-table-sort-lines
        "ttf" 'org-table-toggle-formula-debugger
        "tto" 'org-table-toggle-coordinate-overlays
        "tw" 'org-table-wrap-region

        ;; Source blocks / org-babel
        "bp"     'org-babel-previous-src-block
        "bn"     'org-babel-next-src-block
        "be"     'org-babel-execute-maybe
        "bo"     'org-babel-open-src-block-result
        "bv"     'org-babel-expand-src-block
        "bu"     'org-babel-goto-src-block-head
        "bg"     'org-babel-goto-named-src-block
        "br"     'org-babel-goto-named-result
        "bb"     'org-babel-execute-buffer
        "bs"     'org-babel-execute-subtree
        "bd"     'org-babel-demarcate-block
        "bt"     'org-babel-tangle
        "bf"     'org-babel-tangle-file
        "bc"     'org-babel-check-src-block
        "bj"     'org-babel-insert-header-arg
        "bl"     'org-babel-load-in-session
        "bi"     'org-babel-lob-ingest
        "bI"     'org-babel-view-src-block-info
        "bz"     'org-babel-switch-to-session
        "bZ"     'org-babel-switch-to-session-with-code
        "ba"     'org-babel-sha1-hash
        "bx"     'org-babel-do-key-sequence-in-edit-buffer
        "b."     'spacemacs/org-babel-transient-state/body
        ;; Multi-purpose keys
        (or dotspacemacs-major-mode-leader-key ",") 'org-ctrl-c-ctrl-c
        "*" 'org-ctrl-c-star
        "-" 'org-ctrl-c-minus
        "#" 'org-update-statistics-cookies
        "RET"   'org-ctrl-c-ret
        "M-RET" 'org-meta-return
        ;; attachments
        "A" 'org-attach
        ;; insertion
        "ib" 'org-insert-structure-template
        "id" 'org-insert-drawer
        "ie" 'org-set-effort
        "if" 'org-footnote-new
        "ih" 'org-insert-heading
        "iH" 'org-insert-heading-after-current
        "ii" 'org-insert-item
        "iK" 'spacemacs/insert-keybinding-org
        "il" 'org-insert-link
        "in" 'org-add-note
        "ip" 'org-set-property
        "is" 'org-insert-subheading
        "it" 'org-set-tags-command
        ;; region manipulation
        "xb" (spacemacs|org-emphasize spacemacs/org-bold ?*)
        "xc" (spacemacs|org-emphasize spacemacs/org-code ?~)
        "xi" (spacemacs|org-emphasize spacemacs/org-italic ?/)
        "xo" 'org-open-at-point
        "xr" (spacemacs|org-emphasize spacemacs/org-clear ? )
        "xs" (spacemacs|org-emphasize spacemacs/org-strike-through ?+)
        "xu" (spacemacs|org-emphasize spacemacs/org-underline ?_)
        "xv" (spacemacs|org-emphasize spacemacs/org-verbatim ?=))

      ;; Add global evil-leader mappings. Used to access org-agenda
      ;; functionalities – and a few others commands – from any other mode.
      (spacemacs/declare-prefix
        "ao"  "org"
        "aof" "feeds"
        "aoC" (org-clocks-prefix))
      ;; org-agenda
      (when (configuration-layer/layer-used-p 'ivy)
        (spacemacs/set-leader-keys "ao/" 'org-occur-in-agenda-files))
      (spacemacs/set-leader-keys
        "ao#" 'org-agenda-list-stuck-projects
        "aoa" 'org-agenda-list
        "aoo" 'org-agenda
        "aoc" 'org-capture
        "aoe" 'org-store-agenda-views
        "aofi" 'org-feed-goto-inbox
        "aofu" 'org-feed-update-all

        ;; Clock
        ;; These keybindings should match those under the "mC" prefix (above)
        "aoCc" 'org-clock-cancel
        "aoCg" 'org-clock-goto
        "aoCi" 'org-clock-in
        "aoCI" 'org-clock-in-last
        "aoCj" 'spacemacs/org-clock-jump-to-current-clock
        "aoCo" 'org-clock-out
        "aoCr" 'org-resolve-clocks

        "aol" 'org-store-link
        "aom" 'org-tags-view
        "aos" 'org-search-view
        "aot" 'org-todo-list
        ;; SPC C- capture/colors
        "Cc" 'org-capture)

      (define-key global-map "\C-cl" 'org-store-link)
      (define-key global-map "\C-ca" 'org-agenda)
      (define-key global-map "\C-cc" 'org-capture))
    :config
    (progn
      ;; Activate evil insert state after these commands.
      (dolist (fn '(org-insert-drawer
                    org-insert-heading
                    org-insert-item
                    org-insert-structure-template))
        (advice-add fn :after #'spacemacs//org-maybe-activate-evil-insert))

      ;; We add this key mapping because an Emacs user can change
      ;; `dotspacemacs-major-mode-emacs-leader-key' to `C-c' and the key binding
      ;; C-c ' is shadowed by `spacemacs/default-pop-shell', effectively making
      ;; the Emacs user unable to exit src block editing.
      (define-key org-src-mode-map
        (kbd (concat dotspacemacs-major-mode-emacs-leader-key " '"))
        'org-edit-src-exit)

      ;; Evilify the calendar tool on C-c .
      (unless (eq 'emacs dotspacemacs-editing-style)
        (define-key org-read-date-minibuffer-local-map (kbd "M-h")
          (lambda () (interactive)
            (org-eval-in-calendar '(calendar-backward-day 1))))
        (define-key org-read-date-minibuffer-local-map (kbd "M-l")
          (lambda () (interactive)
            (org-eval-in-calendar '(calendar-forward-day 1))))
        (define-key org-read-date-minibuffer-local-map (kbd "M-k")
          (lambda () (interactive)
            (org-eval-in-calendar '(calendar-backward-week 1))))
        (define-key org-read-date-minibuffer-local-map (kbd "M-j")
          (lambda () (interactive)
            (org-eval-in-calendar '(calendar-forward-week 1))))
        (define-key org-read-date-minibuffer-local-map (kbd "M-H")
          (lambda () (interactive)
            (org-eval-in-calendar '(calendar-backward-month 1))))
        (define-key org-read-date-minibuffer-local-map (kbd "M-L")
          (lambda () (interactive)
            (org-eval-in-calendar '(calendar-forward-month 1))))
        (define-key org-read-date-minibuffer-local-map (kbd "M-K")
          (lambda () (interactive)
            (org-eval-in-calendar '(calendar-backward-year 1))))
        (define-key org-read-date-minibuffer-local-map (kbd "M-J")
          (lambda () (interactive)
            (org-eval-in-calendar '(calendar-forward-year 1)))))

      (spacemacs|define-transient-state org-babel
        :title "Org Babel Transient state"
        :doc "
[_j_/_k_] navigate src blocks         [_e_] execute src block
[_g_]^^   goto named block            [_'_] edit src block
[_z_]^^   recenter screen             [_q_] quit"
        :bindings
        ("q" nil :exit t)
        ("j" org-babel-next-src-block)
        ("k" org-babel-previous-src-block)
        ("g" org-babel-goto-named-src-block)
        ("z" recenter-top-bottom)
        ("e" org-babel-execute-maybe)
        ("'" org-edit-special :exit t)))))

(defun org/init-org-agenda ()
  (use-package org-agenda
    :defer t
    :init
    (progn
      (setq org-agenda-restore-windows-after-quit t)
      (with-eval-after-load 'org
        (add-to-list 'org-modules 'org-habit))
      (dolist (prefix `(("mC" . ,(org-clocks-prefix))
                        ("md" . "dates")
                        ("mi" . "insert")
                        ("ms" . "trees/subtrees")))
        (spacemacs/declare-prefix-for-mode 'org-agenda-mode
          (car prefix) (cdr prefix)))
      (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
        (or dotspacemacs-major-mode-leader-key ",") 'org-agenda-ctrl-c-ctrl-c
        "a" 'org-agenda
        "c" 'org-agenda-capture
        "Cc" 'org-agenda-clock-cancel
        "Ci" 'org-agenda-clock-in
        "Co" 'org-agenda-clock-out
        "Cj" 'org-agenda-clock-goto
        "dd" 'org-agenda-deadline
        "ds" 'org-agenda-schedule
        "ie" 'org-agenda-set-effort
        "ip" 'org-agenda-set-property
        "iP" 'org-agenda-priority
        "it" 'org-agenda-set-tags
        "sr" 'org-agenda-refile)
      (spacemacs|define-transient-state org-agenda
        :title "Org-agenda transient state"
        :on-enter (setq which-key-inhibit t)
        :on-exit (setq which-key-inhibit nil)
        :evil-leader-for-mode (org-agenda-mode . ".")
        :foreign-keys run
        :doc
        "
Headline^^            Visit entry^^               Filter^^                    Date^^                  Toggle mode^^        View^^             Clock^^        Other^^
--------^^---------   -----------^^------------   ------^^-----------------   ----^^-------------     -----------^^------  ----^^---------    -----^^------  -----^^-----------
[_ht_] set status     [_SPC_] in other window     [_ft_] by tag               [_ds_] schedule         [_tf_] follow        [_vd_] day         [_cI_] in      [_gr_] reload
[_hk_] kill           [_TAB_] & go to location    [_fc_] by category          [_dS_] un-schedule      [_tl_] log           [_vw_] week        [_cO_] out     [_._]  go to today
[_hr_] refile         [_RET_] & del other windows [_fh_] by top headline      [_dd_] set deadline     [_ta_] archive       [_vt_] fortnight   [_cq_] cancel  [_gd_] go to date
[_hA_] archive        [_o_]   link                [_fx_] by regexp            [_dD_] remove deadline  [_tr_] clock report  [_vm_] month       [_cj_] jump    ^^
[_h:_] set tags       ^^                          [_fd_] delete all filters   [_dt_] timestamp        [_ti_] clock issues  [_vy_] year        ^^             ^^
[_hp_] set priority   ^^                          ^^                          [_+_]  do later         [_td_] diaries       [_vn_] next span   ^^             ^^
^^                    ^^                          ^^                          [_-_]  do earlier       ^^                   [_vp_] prev span   ^^             ^^
^^                    ^^                          ^^                          ^^                      ^^                   [_vr_] reset       ^^             ^^
[_q_] quit
"
        :bindings
        ;; Entry
        ("h:" org-agenda-set-tags)
        ("hA" org-agenda-archive-default)
        ("hk" org-agenda-kill)
        ("hp" org-agenda-priority)
        ("hr" org-agenda-refile)
        ("ht" org-agenda-todo)

        ;; Visit entry
        ("SPC" org-agenda-show-and-scroll-up)
        ("<tab>" org-agenda-goto :exit t)
        ("TAB" org-agenda-goto :exit t)
        ("RET" org-agenda-switch-to :exit t)
        ("o"   link-hint-open-link :exit t)

        ;; Date
        ("+" org-agenda-do-date-later)
        ("-" org-agenda-do-date-earlier)
        ("dd" org-agenda-deadline)
        ("dD" (lambda () (interactive)
                (let ((current-prefix-arg '(4)))
                  (call-interactively 'org-agenda-deadline))))
        ("ds" org-agenda-schedule)
        ("dS" (lambda () (interactive)
                (let ((current-prefix-arg '(4)))
                  (call-interactively 'org-agenda-schedule))))
        ("dt" org-agenda-date-prompt)

        ;; View
        ("vd" org-agenda-day-view)
        ("vm" org-agenda-month-view)
        ("vn" org-agenda-later)
        ("vp" org-agenda-earlier)
        ("vr" org-agenda-reset-view)
        ("vt" org-agenda-fortnight-view)
        ("vw" org-agenda-week-view)
        ("vy" org-agenda-year-view)

        ;; Toggle mode
        ("ta" org-agenda-archives-mode)
        ("td" org-agenda-toggle-diary)
        ("tf" org-agenda-follow-mode)
        ("ti" org-agenda-show-clocking-issues)
        ("tl" org-agenda-log-mode)
        ("tr" org-agenda-clockreport-mode)

        ;; Filter
        ("fc" org-agenda-filter-by-category)
        ("fd" org-agenda-filter-remove-all)
        ("fh" org-agenda-filter-by-top-headline)
        ("ft" org-agenda-filter-by-tag)
        ("fx" org-agenda-filter-by-regexp)

        ;; Clock
        ("cI" org-agenda-clock-in :exit t)
        ("cj" org-agenda-clock-goto :exit t)
        ("cO" org-agenda-clock-out)
        ("cq" org-agenda-clock-cancel)

        ;; Other
        ("q" nil :exit t)
        ("gr" org-agenda-redo)
        ("." org-agenda-goto-today)
        ("gd" org-agenda-goto-date)))
    :config
    (when org-enable-org-contacts-support
      (use-package org-contacts))
    (evilified-state-evilify-map org-agenda-mode-map
      :mode org-agenda-mode
      :bindings
      "j" 'org-agenda-next-line
      "k" 'org-agenda-previous-line
      "K" nil
      ;; C-h should not be rebound by evilification so we unshadow it manually
      ;; TODO add the rule in auto-evilification to ignore C-h (like we do
      ;; with C-g)
      (kbd "C-h") nil
      (kbd "M-j") 'org-agenda-next-item
      (kbd "M-k") 'org-agenda-previous-item
      (kbd "M-h") 'org-agenda-earlier
      (kbd "M-l") 'org-agenda-later
      (kbd "gd") 'org-agenda-toggle-time-grid
      (kbd "gr") 'org-agenda-redo
      (kbd "M-RET") 'org-agenda-show-and-scroll-up
      (kbd "M-SPC") 'spacemacs/org-agenda-transient-state/body
      (kbd "s-M-SPC") 'spacemacs/org-agenda-transient-state/body)))

(defun org/init-org-wild-notifier ()
  (use-package org-wild-notifier
    :defer t
    :init
    (when org-start-notification-daemon-on-startup
      (org-wild-notifier-mode))))

(defun org/init-org-brain ()
  (use-package org-brain
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix "aoB" "org-brain")
      (spacemacs/set-leader-keys
        "aoBv" 'org-brain-visualize
        "aoBa" 'org-brain-agenda)
      (spacemacs/declare-prefix-for-mode 'org-mode "mB" "org-brain")
      (spacemacs/declare-prefix-for-mode 'org-mode "mBa" "add")
      (spacemacs/declare-prefix-for-mode 'org-mode "mBg" "goto")
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "Bv" 'org-brain-visualize
        "Bac" 'org-brain-add-child
        "Bah" 'org-brain-add-child-headline
        "Bap" 'org-brain-add-parent
        "Bar" 'org-brain-add-resource
        "Baf" 'org-brain-add-friendship
        "Bgg" 'org-brain-goto
        "Bgc" 'org-brain-goto-child
        "Bgp" 'org-brain-goto-parent
        "Bgf" 'org-brain-goto-friend
        "BR"  'org-brain-refile
        "Bx"  'org-brain-delete-entry)
      (evil-set-initial-state 'org-brain-visualize-mode 'emacs)
      (when (memq dotspacemacs-editing-style '(vim hybrid))
        (with-eval-after-load 'org-brain
          (define-key org-brain-visualize-mode-map (kbd "SPC") 'spacemacs-cmds))))))

(defun org/init-org-expiry ()
  (use-package org-expiry
    :commands (org-expiry-insinuate
               org-expiry-deinsinuate
               org-expiry-insert-created
               org-expiry-insert-expiry
               org-expiry-add-keyword
               org-expiry-archive-subtree
               org-expiry-process-entry
               org-expiry-process-entries)))

(defun org/init-org-download ()
  (use-package org-download
    :commands (org-download-enable
               org-download-yank
               org-download-screenshot)
    :init
    (progn
      (add-hook 'org-mode-hook 'org-download-enable)
      (spacemacs/declare-prefix-for-mode 'org-mode "miD" "download")
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "iDy" 'org-download-yank
        "iDs" 'org-download-screenshot))))

(defun org/init-org-jira ()
  (use-package org-jira
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix
        "aoJ"  "jira"
        "aoJp" "projects"
        "aoJi" "issues"
        "aoJs" "subtasks"
        "aoJc" "comments"
        "aoJt" "todos")
      (spacemacs/set-leader-keys
        "aoJpg" 'org-jira-get-projects
        "aoJib" 'org-jira-browse-issue
        "aoJig" 'org-jira-get-issues
        "aoJih" 'org-jira-get-issues-headonly
        "aoJif" 'org-jira-get-issues-from-filter-headonly
        "aoJiu" 'org-jira-update-issue
        "aoJiw" 'org-jira-progress-issue
        "aoJir" 'org-jira-refresh-issue
        "aoJic" 'org-jira-create-issue
        "aoJiy" 'org-jira-copy-current-issue-key
        "aoJsc" 'org-jira-create-subtask
        "aoJsg" 'org-jira-get-subtasks
        "aoJcu" 'org-jira-update-comment
        "aoJtj" 'org-jira-todo-to-jira)
      (spacemacs/declare-prefix-for-mode 'org-mode "mmj" "jira")
      (spacemacs/declare-prefix-for-mode 'org-mode "mmjp" "projects")
      (spacemacs/declare-prefix-for-mode 'org-mode "mmji" "issues")
      (spacemacs/declare-prefix-for-mode 'org-mode "mmjs" "subtasks")
      (spacemacs/declare-prefix-for-mode 'org-mode "mmjc" "comments")
      (spacemacs/declare-prefix-for-mode 'org-mode "mmjt" "todos")
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "mjpg" 'org-jira-get-projects
        "mjib" 'org-jira-browse-issue
        "mjig" 'org-jira-get-issues
        "mjih" 'org-jira-get-issues-headonly
        "mjif" 'org-jira-get-issues-from-filter-headonly
        "mjiu" 'org-jira-update-issue
        "mjiw" 'org-jira-progress-issue
        "mjir" 'org-jira-refresh-issue
        "mjic" 'org-jira-create-issue
        "mjiy" 'org-jira-copy-current-issue-key
        "mjsc" 'org-jira-create-subtask
        "mjsg" 'org-jira-get-subtasks
        "mjcu" 'org-jira-update-comment
        "mjtj" 'org-jira-todo-to-jira))))

(defun org/init-org-mime ()
  (use-package org-mime
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'message-mode
        "em" 'org-mime-htmlize)
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "em" 'org-mime-org-buffer-htmlize
        "es" 'org-mime-org-subtree-htmlize))))

(defun org/init-org-modern ()
  (use-package org-modern
      :defer t
      :init
      (progn
        (add-hook 'org-mode-hook 'org-modern-mode)
        (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

        (spacemacs/set-leader-keys-for-major-mode 'org-mode
            "Tm" 'org-modern-mode))))

(defun org/init-org-pomodoro ()
  (use-package org-pomodoro
    :defer t
    :init
    (progn
      (when (spacemacs/system-is-mac)
        (setq org-pomodoro-audio-player "/usr/bin/afplay"))
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "Cp" 'org-pomodoro)
      (spacemacs/set-leader-keys-for-major-mode 'org-journal-mode
        "Cp" 'org-pomodoro)
      (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
        "Cp" 'org-pomodoro))))

(defun org/init-org-present ()
  (use-package org-present
    :defer t
    :init
    (progn
      (defun spacemacs//org-present-start ()
        "Initiate `org-present' mode"
        (org-present-big)
        (org-display-inline-images)
        (org-present-hide-cursor)
        (org-present-read-only)
        (evil-define-key 'normal org-present-mode-keymap
          "h"             'org-present-prev
          (kbd "<left>")  'org-present-prev
          "l"             'org-present-next
          (kbd "<right>") 'org-present-next
          "q"             'org-present-quit)
        ;; evil-normal-state seems to be required to load the above key bindings
        (evil-normal-state))
      (defun spacemacs//org-present-end ()
        "Terminate `org-present' mode"
        (org-present-small)
        (if (not org-startup-with-inline-images)
            (org-remove-inline-images))
        (org-present-show-cursor)
        (org-present-read-write))
      (add-hook 'org-present-mode-hook 'spacemacs//org-present-start)
      (add-hook 'org-present-mode-quit-hook 'spacemacs//org-present-end))))

(defun org/init-org-cliplink ()
  (use-package org-cliplink
    :defer t
    :init
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "iL" 'org-cliplink)))

(defun org/init-org-rich-yank ()
  (use-package org-rich-yank
    :ensure t
    :demand t
    :init
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      ;; yank is a misnomer for this function which actually puts/pastes
      ;; ir = "insert rich"
      "ir" 'org-rich-yank)))

(defun org/init-org-projectile ()
  (use-package org-projectile
    :commands (org-projectile-location-for-project)
    :init
    (progn
      (spacemacs/set-leader-keys
        "aop" 'org-projectile/capture
        "po" 'org-projectile/goto-todos)
      (with-eval-after-load 'org-capture
        (require 'org-projectile)))
    :config
    (if (file-name-absolute-p org-projectile-file)
        (progn
          (setq org-projectile-projects-file org-projectile-file)
          (push (org-projectile-project-todo-entry :empty-lines 1)
                org-capture-templates))
      (org-projectile-per-project)
      (setq org-projectile-per-project-filepath org-projectile-file))))

(defun org/pre-init-ox-epub ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-epub)))
(defun org/init-ox-epub ())

(defun org/pre-init-ox-twbs ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-twbs)))
(defun org/init-ox-twbs ())

(defun org/pre-init-ox-gfm ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-gfm)))
(defun org/init-ox-gfm ())

(defun org/pre-init-org-re-reveal ()
  (spacemacs|use-package-add-hook org :post-config (require 'org-re-reveal)))
(defun org/init-org-re-reveal ())

(defun org/post-init-persp-mode ()
  (spacemacs|define-custom-layout "@Org"
    :binding "o"
    :body
    (let ((agenda-files (org-agenda-files)))
      (if agenda-files
          (progn (find-file (if org-persp-startup-org-file org-persp-startup-org-file (cl-first agenda-files)))
                 (if org-persp-startup-with-agenda (org-agenda nil org-persp-startup-with-agenda)))

        (user-error "Error: No agenda files configured, nothing to display.")))))

(defun org/init-org-contacts ()
  (use-package org-contacts
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
        "Cf" 'org-contacts-find-file)
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "Cf" 'org-contacts-find-file)
      (spacemacs/set-leader-keys
        "aoCf" 'org-contacts-find-file))))

(defun org/init-org-contrib ()
  (use-package org-contrib
    :defer t))

(defun org/init-org-vcard ()
  (use-package org-vcard
    :defer t))

(defun org/init-org-journal ()
  (use-package org-journal
    :defer t
    :commands (org-journal-new-entry org-journal-search-forever)
    :init
    (progn
      (spacemacs/declare-prefix "aoj" "org-journal")
      (spacemacs/set-leader-keys
        "aojf" 'org-journal-open-current-journal-file
        "aojj" 'org-journal-new-entry
        "aojs" 'org-journal-search-forever
        "aojt" 'org-journal-new-scheduled-entry
        "aojv" 'org-journal-schedule-view)

      (setq spacemacs-org-journal-mode-map (copy-keymap spacemacs-org-mode-map))

      (spacemacs/set-leader-keys-for-major-mode 'calendar-mode
        "r" 'org-journal-read-entry
        "i" 'org-journal-new-date-entry
        "n" 'org-journal-next-entry
        "p" 'org-journal-previous-entry
        "s" 'org-journal-search-forever
        "w" 'org-journal-search-calendar-week
        "m" 'org-journal-search-calendar-month
        "y" 'org-journal-search-calendar-year)

      (spacemacs/set-leader-keys-for-major-mode 'org-journal-mode
        "j" 'org-journal-new-entry
        "n" 'org-journal-next-entry
        "p" 'org-journal-previous-entry)

      (spacemacs//init-leader-mode-map 'org-journal-mode 'spacemacs-org-journal-mode-map))))

(defun org/init-ox-hugo ()
  (use-package ox-hugo :after ox))

(defun org/init-ox-jira ()
  (use-package ox-jira :after ox))

(defun org/init-org-trello ()
  (use-package org-trello
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix-for-mode 'org-mode "mmt" "trello")
      (spacemacs/declare-prefix-for-mode 'org-mode "mmtd" "sync down")
      (spacemacs/declare-prefix-for-mode 'org-mode "mmtu" "sync up")
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "mtI" 'org-trello-install-key-and-token
        "mta" 'org-trello-archive-card
        "mtc" 'org-trello-create-board-and-install-metadata
        "mti" 'org-trello-install-board-metadata
        "mtm" 'org-trello-update-board-metadata
        "mtdb" 'spacemacs/org-trello-pull-buffer
        "mtdc" 'spacemacs/org-trello-pull-card
        "mtub" 'spacemacs/org-trello-push-buffer
        "mtuc" 'spacemacs/org-trello-push-card))))

(defun org/init-org-roam ()
  (use-package org-roam
    :defer t
    ;; Do not enable automatic db update until after user had a chance to setup
    ;; org-roam. See https://github.com/syl20bnr/spacemacs/issues/15724
    ;; :hook (after-init . org-roam-setup)
    :init
    (progn
      (spacemacs/declare-prefix
        "aor"  "org-roam"
        "aord" "org-roam-dailies"
        "aort" "org-roam-tags")
      (spacemacs/set-leader-keys
        "aordy" 'org-roam-dailies-goto-yesterday
        "aordt" 'org-roam-dailies-goto-today
        "aordT" 'org-roam-dailies-goto-tomorrow
        "aordd" 'org-roam-dailies-goto-date
        "aorc" 'org-roam-capture
        "aorf" 'org-roam-node-find
        "aorg" 'org-roam-graph
        "aori" 'org-roam-node-insert
        "aorl" 'org-roam-buffer-toggle
        "aorta" 'org-roam-tag-add
        "aortr" 'org-roam-tag-remove
        "aora" 'org-roam-alias-add)

      (spacemacs/declare-prefix-for-mode 'org-mode "mr" "org-roam")
      (spacemacs/declare-prefix-for-mode 'org-mode "mrd" "org-roam-dailies")
      (spacemacs/declare-prefix-for-mode 'org-mode "mrt" "org-roam-tags")
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "rdy" 'org-roam-dailies-goto-yesterday
        "rdt" 'org-roam-dailies-goto-today
        "rdT" 'org-roam-dailies-goto-tomorrow
        "rdd" 'org-roam-dailies-goto-date
        "rc" 'org-roam-capture
        "rf" 'org-roam-node-find
        "rg" 'org-roam-graph
        "ri" 'org-roam-node-insert
        "rl" 'org-roam-buffer-toggle
        "rta" 'org-roam-tag-add
        "rtr" 'org-roam-tag-remove
        "ra" 'org-roam-alias-add))

    :config
    (progn
      (spacemacs|hide-lighter org-roam-mode)

      (evilified-state-evilify-map org-roam-mode-map
        :mode org-roam-mode
        :bindings
        "o" 'link-hint-open-link
        "r" 'org-roam-buffer-refresh)))

  (use-package org-roam-protocol
    :if org-enable-roam-protocol
    :after org-protocol))

(defun org/init-org-sticky-header ()
  (use-package org-sticky-header
    :defer t
    :init
    (add-hook 'org-mode-hook 'org-sticky-header-mode)))

(defun org/init-verb ()
  (use-package verb
    :defer t
    :init
    (spacemacs/set-leader-keys-for-major-mode
      'org-mode
      "rf" #'verb-send-request-on-point
      "rs" #'verb-send-request-on-point-other-window
      "rr" #'verb-send-request-on-point-other-window-stay
      "rm" #'verb-send-request-on-point-no-window
      "rk" #'verb-kill-all-response-buffers
      "re" #'verb-export-request-on-point
      "ru" #'verb-export-request-on-point-curl
      "rb" #'verb-export-request-on-point-verb
      "rv" #'verb-set-var)
    :config
    (progn
      (spacemacs/set-leader-keys-for-minor-mode
        'verb-response-body-mode
        "rr" #'verb-toggle-show-headers
        "rk" #'verb-kill-response-buffer-and-window
        "rf" #'verb-re-send-request)
      (spacemacs/set-leader-keys-for-major-mode
        'verb-response-headers-mode
        "rq" #'verb-kill-buffer-and-window))))

(defun org/pre-init-verb ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(verb . t))))

(defun org/init-valign ()
  (use-package valign
    :after org
    :init
    (progn
      (add-hook 'org-mode-hook 'valign-mode)
      (add-hook 'valign-mode-hook (lambda () (unless valign-mode
                                               (valign-remove-advice)))))
    :config
    (spacemacs|diminish valign-mode " ㊣" " E")))

(defun org/init-org-appear ()
  (use-package org-appear
    :defer t
    :init
    (progn
      (add-hook 'org-mode-hook 'org-appear-mode)
      (setq org-appear-autolinks t
            org-appear-autoemphasis t
            org-appear-autosubmarkers t))
    :config
    (when (and (eq org-appear-trigger 'manual)
               (memq dotspacemacs-editing-style '(vim hybrid)))
      (add-hook 'org-mode-hook
                (lambda ()
                  (add-hook 'evil-insert-state-entry-hook #'org-appear-manual-start nil t)
                  (add-hook 'evil-insert-state-exit-hook #'org-appear-manual-stop nil t))))))

(defun org/init-org-transclusion ()
  (use-package org-transclusion
    :defer t
    :init
    (progn
     (spacemacs/declare-prefix-for-mode 'org-mode "mu" "org-transclusion")
     (spacemacs/set-leader-keys-for-major-mode 'org-mode
       "uu" #'org-transclusion-add
       "uU" #'org-transclusion-add-all
       "ud" #'org-transclusion-remove
       "uD" #'org-transclusion-remove-all
       "ul" #'org-transclusion-demote-subtree
       "uh" #'org-transclusion-promote-subtree
       "ur" #'org-transclusion-refresh
       "ug" #'org-transclusion-move-to-source))))

(defun org/init-ox-asciidoc ()
  (use-package ox-asciidoc
    :after ox))

(defun org/post-init-helm ()
  (if (not (boundp 'helm-imenu-extra-modes))
    (setq helm-imenu-extra-modes '(org-mode)))
  (add-to-list 'helm-imenu-extra-modes 'org-mode))

(defun org/init-org-roam-ui ()
  (use-package org-roam-ui
    :after org-roam
    :init
    (progn
      (spacemacs/set-leader-keys
        "aoru" 'org-roam-ui-mode)
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "ru" 'org-roam-ui-mode))
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t)))
