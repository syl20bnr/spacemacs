;;; packages.el --- Org Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq org-packages
      '(
        company
        company-emoji
        emoji-cheat-sheet-plus
        evil-org
        evil-surround
        gnuplot
        (helm-org-rifle :toggle (configuration-layer/layer-used-p 'helm))
        htmlize
        ;; ob, org and org-agenda are installed by `org-plus-contrib'
        (ob :location built-in)
        (org :location built-in)
        (org-agenda :location built-in)
        (org-brain :toggle (version<= "25" emacs-version))
        (org-expiry :location built-in)
        (org-journal :toggle org-enable-org-journal-support)
        org-download
        org-mime
        org-pomodoro
        org-present
        (org-projectile :requires projectile)
        (ox-twbs :toggle org-enable-bootstrap-support)
        ;; use a for of ox-gfm to fix index generation
        (ox-gfm :location (recipe :fetcher github :repo "syl20bnr/ox-gfm")
                :toggle org-enable-github-support)
        (ox-reveal :toggle org-enable-reveal-js-support)
        persp-mode
        (ox-hugo :toggle org-enable-hugo-support)
        (org-trello :toggle org-enable-trello-support)
        ))

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
    :init (spacemacs/set-leader-keys "aor" 'helm-org-rifle)))

(defun org/init-htmlize ()
  (use-package htmlize
    :defer t))

(defun org/init-ob ()
  (use-package ob
    :defer t
    :init
    (progn
      (defun spacemacs//org-babel-do-load-languages ()
        "Load all the languages declared in `org-babel-load-languages'."
        (org-babel-do-load-languages 'org-babel-load-languages
                                     org-babel-load-languages))
      (add-hook 'org-mode-hook 'spacemacs//org-babel-do-load-languages)
      ;; Fix redisplay of inline images after a code block evaluation.
      (add-hook 'org-babel-after-execute-hook 'spacemacs/ob-fix-inline-images))))

(defun org/init-org ()
  (use-package org
    :defer (spacemacs/defer)
    :commands (orgtbl-mode)
    :init
    (progn
      (spacemacs|require 'org)
      (setq org-clock-persist-file (concat spacemacs-cache-directory
                                           "org-clock-save.el")
            org-id-locations-file (concat spacemacs-cache-directory
                                          ".org-id-locations")
            org-publish-timestamp-directory (concat spacemacs-cache-directory
                                                    ".org-timestamps/")
            org-directory "~/org" ;; needs to be defined for `org-default-notes-file'
            org-default-notes-file (expand-file-name "notes.org" org-directory)
            org-log-done t
            org-startup-with-inline-images t
            org-image-actual-width nil
            org-src-fontify-natively t
            org-src-tab-acts-natively t
            ;; this is consistent with the value of
            ;; `helm-org-headings-max-depth'.
            org-imenu-depth 8)

      (with-eval-after-load 'org-indent
        (spacemacs|hide-lighter org-indent-mode))
      (let ((dir (configuration-layer/get-layer-local-dir 'org)))
        (setq org-export-async-init-file (concat dir "org-async-init.el")))
      (defmacro spacemacs|org-emphasize (fname char)
        "Make function for setting the emphasis in org mode"
        `(defun ,fname () (interactive)
                (org-emphasize ,char)))

      ;; Follow the confirm and abort conventions
      (with-eval-after-load 'org-capture
        (spacemacs/set-leader-keys-for-minor-mode 'org-capture-mode
          dotspacemacs-major-mode-leader-key 'org-capture-finalize
          "a" 'org-capture-kill
          "c" 'org-capture-finalize
          "k" 'org-capture-kill
          "r" 'org-capture-refile))

      (with-eval-after-load 'org-src
        (spacemacs/set-leader-keys-for-minor-mode 'org-src-mode
          dotspacemacs-major-mode-leader-key 'org-edit-src-exit
          "c" 'org-edit-src-exit
          "a" 'org-edit-src-abort
          "k" 'org-edit-src-abort))

      (add-hook 'org-mode-hook 'dotspacemacs//prettify-spacemacs-docs)

      (let ((dir (configuration-layer/get-layer-local-dir 'org)))
        (setq org-export-async-init-file (concat dir "org-async-init.el")))

      ;; Insert key for org-mode and markdown a la C-h k
      ;; from SE endless http://emacs.stackexchange.com/questions/2206/i-want-to-have-the-kbd-tags-for-my-blog-written-in-org-mode/2208#2208
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

      (dolist (prefix '(
                        ("mb" . "babel")
                        ("mC" . "clocks")
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
                        ("mx" . "text")
                        ))
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
        "Cj" 'org-clock-jump-to-current-clock
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

        "p" 'org-priority

        "Tc" 'org-toggle-checkbox
        "Te" 'org-toggle-pretty-entities
        "Ti" 'org-toggle-inline-images
        "Tl" 'org-toggle-link-display
        "Tt" 'org-show-todo-tree
        "TT" 'org-todo
        "TV" 'space-doc-mode
        "Tx" 'org-toggle-latex-fragment

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
        "sA" 'org-archive-subtree
        "sb" 'org-tree-to-indirect-buffer
        "sh" 'org-promote-subtree
        "sj" 'org-move-subtree-down
        "sk" 'org-move-subtree-up
        "sl" 'org-demote-subtree
        "sn" 'org-narrow-to-subtree
        "sN" 'widen
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
      (spacemacs/declare-prefix "ao" "org")
      (spacemacs/declare-prefix "aof" "feeds")
      (spacemacs/declare-prefix "aoC" "clock")
      (spacemacs/set-leader-keys
        ;; org-agenda
        "ao#" 'org-agenda-list-stuck-projects
        "ao/" 'org-occur-in-agenda-files
        "aoa" 'org-agenda-list
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
        "aoCj" 'org-clock-jump-to-current-clock
        "aoCo" 'org-clock-out
        "aoCr" 'org-resolve-clocks

        "aol" 'org-store-link
        "aom" 'org-tags-view
        "aoo" 'org-agenda
        "aos" 'org-search-view
        "aot" 'org-todo-list
        ;; SPC C- capture/colors
        "Cc" 'org-capture)

      (define-key global-map "\C-cl" 'org-store-link)
      (define-key global-map "\C-ca" 'org-agenda)
      (define-key global-map "\C-cc" 'org-capture))
    :config
    (progn
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
        ("e" org-babel-execute-maybe :exit t)
        ("'" org-edit-special :exit t)))))

(defun org/init-org-agenda ()
  (use-package org-agenda
    :defer t
    :init
    (progn
      (setq org-agenda-restore-windows-after-quit t)
      (dolist (prefix '(("mC" . "clocks")
                        ("md" . "dates")
                        ("mi" . "insert")
                        ("ms" . "trees/subtrees")))
        (spacemacs/declare-prefix-for-mode 'org-agenda-mode
          (car prefix) (cdr prefix)))
      (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
        "a" 'org-agenda
        "Cc" 'org-agenda-clock-cancel
        "Ci" 'org-agenda-clock-in
        "Co" 'org-agenda-clock-out
        "dd" 'org-agenda-deadline
        "ds" 'org-agenda-schedule
        "ie" 'org-agenda-set-effort
        "ip" 'org-agenda-set-property
        "it" 'org-agenda-set-tags
        "sr" 'org-agenda-refile)
      (spacemacs|define-transient-state org-agenda
      :title "Org-agenda transient state"
      :on-enter (setq which-key-inhibit t)
      :on-exit (setq which-key-inhibit nil)
      :foreign-keys run
      :doc
      "
Headline^^            Visit entry^^               Filter^^                    Date^^                  Toggle mode^^        View^^             Clock^^        Other^^
--------^^---------   -----------^^------------   ------^^-----------------   ----^^-------------     -----------^^------  ----^^---------    -----^^------  -----^^-----------
[_ht_] set status     [_SPC_] in other window     [_ft_] by tag               [_ds_] schedule         [_tf_] follow        [_vd_] day         [_cI_] in      [_gr_] reload
[_hk_] kill           [_TAB_] & go to location    [_fr_] refine by tag        [_dS_] un-schedule      [_tl_] log           [_vw_] week        [_cO_] out     [_._]  go to today
[_hr_] refile         [_RET_] & del other windows [_fc_] by category          [_dd_] set deadline     [_ta_] archive       [_vt_] fortnight   [_cq_] cancel  [_gd_] go to date
[_hA_] archive        [_o_]   link                [_fh_] by top headline      [_dD_] remove deadline  [_tr_] clock report  [_vm_] month       [_cj_] jump    ^^
[_h:_] set tags       ^^                          [_fx_] by regexp            [_dt_] timestamp        [_td_] diaries       [_vy_] year        ^^             ^^
[_hp_] set priority   ^^                          [_fd_] delete all filters   [_+_]  do later         ^^                   [_vn_] next span   ^^             ^^
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
      ("ds" org-agenda-schedule)
      ("dS" (lambda () (interactive)
             (let ((current-prefix-arg '(4)))
                  (call-interactively 'org-agenda-schedule))))
      ("dd" org-agenda-deadline)
      ("dt" org-agenda-date-prompt)
      ("dD" (lambda () (interactive)
             (let ((current-prefix-arg '(4)))
                  (call-interactively 'org-agenda-deadline))))
      ("+" org-agenda-do-date-later)
      ("-" org-agenda-do-date-earlier)

      ;; View
      ("vd" org-agenda-day-view)
      ("vw" org-agenda-week-view)
      ("vt" org-agenda-fortnight-view)
      ("vm" org-agenda-month-view)
      ("vy" org-agenda-year-view)
      ("vn" org-agenda-later)
      ("vp" org-agenda-earlier)
      ("vr" org-agenda-reset-view)

      ;; Toggle mode
      ("tf" org-agenda-follow-mode)
      ("tl" org-agenda-log-mode)
      ("ta" org-agenda-archives-mode)
      ("tr" org-agenda-clockreport-mode)
      ("td" org-agenda-toggle-diary)

      ;; Filter
      ("ft" org-agenda-filter-by-tag)
      ("fr" org-agenda-filter-by-tag-refine)
      ("fc" org-agenda-filter-by-category)
      ("fh" org-agenda-filter-by-top-headline)
      ("fx" org-agenda-filter-by-regexp)
      ("fd" org-agenda-filter-remove-all)

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
    (evilified-state-evilify-map org-agenda-mode-map
      :mode org-agenda-mode
      :bindings
      "j" 'org-agenda-next-line
      "k" 'org-agenda-previous-line
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

(defun org/init-org-brain ()
  (use-package org-brain
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys
        "aob" 'org-brain-visualize)
      (evil-set-initial-state 'org-brain-visualize-mode 'emacs))))

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

(defun org/init-org-mime ()
  (use-package org-mime
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'message-mode
        "em" 'org-mime-htmlize)
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "em" 'org-mime-org-buffer-htmlize))))

(defun org/init-org-pomodoro ()
  (use-package org-pomodoro
    :defer t
    :init
    (progn
      (when (spacemacs/system-is-mac)
        (setq org-pomodoro-audio-player "/usr/bin/afplay"))
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "Cp" 'org-pomodoro)
      (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
        "Cp" 'org-pomodoro))))

(defun org/init-org-present ()
  (use-package org-present
    :defer t
    :init
    (progn
      (evilified-state-evilify nil org-present-mode-keymap
        "h" 'org-present-prev
        "l" 'org-present-next
        "q" 'org-present-quit)
      (defun spacemacs//org-present-start ()
        "Initiate `org-present' mode"
        (org-present-big)
        (org-display-inline-images)
        (org-present-hide-cursor)
        (org-present-read-only)
        (evil-evilified-state))
      (defun spacemacs//org-present-end ()
        "Terminate `org-present' mode"
        (org-present-small)
        (org-remove-inline-images)
        (org-present-show-cursor)
        (org-present-read-write)
        (evil-normal-state))
      (add-hook 'org-present-mode-hook 'spacemacs//org-present-start)
      (add-hook 'org-present-mode-quit-hook 'spacemacs//org-present-end))))

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

(defun org/pre-init-ox-twbs ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-twbs)))
(defun org/init-ox-twbs ())

(defun org/pre-init-ox-gfm ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-gfm)))
(defun org/init-ox-gfm ())

(defun org/pre-init-ox-reveal ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-reveal)))
(defun org/init-ox-reveal ())

(defun org/post-init-persp-mode ()
  (spacemacs|define-custom-layout "@Org"
    :binding "o"
    :body
    (let ((agenda-files (org-agenda-files)))
      (if agenda-files
          (find-file (first agenda-files))
        (user-error "Error: No agenda files configured, nothing to display.")))))

(defun org/init-org-journal ()
  (use-package org-journal
    :defer t
    :commands (org-journal-new-entry org-journal-search-forever)
    :init
    (progn
      (spacemacs/declare-prefix "aoj" "org-journal")
      (spacemacs/set-leader-keys
        "aojj" 'org-journal-new-entry
        "aojs" 'org-journal-search-forever)

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
        "n" 'org-journal-open-next-entry
        "p" 'org-journal-open-previous-entry)

      (spacemacs//init-leader-mode-map 'org-journal-mode 'spacemacs-org-journal-mode-map))))

(defun org/init-ox-hugo ()
  (use-package ox-hugo :after ox))

(defun org/init-org-trello ()
  (use-package org-trello
    :after org
    :config
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
