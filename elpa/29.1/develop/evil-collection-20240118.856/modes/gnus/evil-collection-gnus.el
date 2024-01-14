;;; evil-collection-gnus.el --- Bindings for `gnus' -*- lexical-binding: t -*-

;; Copyright (C) 2019 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: emacs, tools, evil

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Bindings for `gnus'.

;;; Code:
(require 'gnus nil t)
(require 'evil-collection)

(defconst evil-collection-gnus-maps '(gnus-article-mode-map
                                      gnus-bookmark-bmenu-mode-map
                                      gnus-browse-mode-map
                                      gnus-group-mode-map
                                      gnus-server-mode-map
                                      gnus-summary-mode-map))

(defvar evil-collection-gnus-common-normal-bindings
  '("zz" gnus-recenter)
  "Keybindings added to all gnus normal mode-maps.
Note that there is no gnus-common-mode-map")

;;;###autoload
(defun evil-collection-gnus-setup ()
  "Set up `evil' bindings for `gnus'."

  ;; For all gnus modes:
  ;;  - Set the initial state to 'normal
  ;;  - Inhibit insert
  ;;  - Use `evil-collection-gnus-common-normal-bindings'
  (dolist (map evil-collection-gnus-maps)
    (let* ((map-string (symbol-name map))
           (mode-string (string-trim-right map-string "-map"))
           (mode (intern mode-string)))
      (evil-set-initial-state mode 'normal)
      (evil-collection-inhibit-insert-state map)
      (apply #'evil-collection-define-key 'normal map
             evil-collection-gnus-common-normal-bindings)))

  (evil-collection-define-key 'normal 'gnus-summary-mode-map
    ;; quit
    "Q"         'gnus-summary-exit-no-update
    "ZQ"        'gnus-summary-exit-no-update
    "q"         'gnus-summary-exit
    "ZZ"        'gnus-summary-exit

    "g?"        'gnus-summary-help-map

    ;; motion
    (kbd "<tab>") 'gnus-summary-widget-forward
    (kbd "<backtab>") 'gnus-summary-widget-backward
    (kbd "<delete>") 'gnus-summary-prev-page
    (kbd "S-SPC") 'gnus-summary-prev-page
    (kbd "SPC") 'gnus-summary-next-page
    (kbd "RET") 'gnus-summary-scroll-up
    "gk"        'gnus-summary-prev-unread-article
    "gj"        'gnus-summary-next-unread-article
    "[["        'gnus-summary-prev-unread-article
    "]]"        'gnus-summary-next-unread-article
    "{"         'gnus-summary-prev-thread
    "}"         'gnus-summary-next-thread
    (kbd "C-j") 'gnus-summary-next-article
    (kbd "C-k") 'gnus-summary-prev-article

    ;; Marking
    "m"         'gnus-summary-mark-as-processable
    "M"         'gnus-uu-mark-buffer
    "u"         'gnus-summary-unmark-as-processable
    "U"         'gnus-summary-unmark-all-processable
    "%"         'gnus-uu-mark-by-regexp

    ;; Composing
    "C"         'gnus-summary-mail-other-window
    "cc"        'gnus-summary-mail-other-window
    "ci"        'gnus-summary-news-other-window
    "f"         'gnus-summary-followup
    "F"         'gnus-summary-followup-with-original
    "cf"        'gnus-summary-followup
    "cF"        'gnus-summary-followup-with-original

    ;; Reply
    "r"         'gnus-summary-reply
    "R"         'gnus-summary-reply-with-original
    "cr"        'gnus-summary-reply
    "cR"        'gnus-summary-reply-with-original
    "cw"        'gnus-summary-very-wide-reply
    "cW"        'gnus-summary-very-wide-reply-with-original
    (kbd "C-c C-f") 'gnus-summary-mail-forward

    ;; Actions, like mu4e
    ;; Keep the following two bindings consistent with group's
    "."         'gnus-summary-first-unread-article
    ","         'gnus-summary-best-unread-article
    "!"         'gnus-summary-mark-as-read-forward
    "="         'gnus-summary-tick-article-forward
    "J"         'gnus-summary-goto-article
    "gr"        'gnus-summary-rescan-group
    "e"         'gnus-summary-edit-article
    "E"         'gnus-summary-mark-as-expirable
    "z/"        'gnus-summary-limit-map
    "zt"        'gnus-summary-toggle-header
    "x"         'gnus-summary-limit-to-unread

    ;; Finding the parent
    "^"         'gnus-summary-refer-parent-article
    (kbd "M-^") 'gnus-summary-refer-article

    ;; Sorting
    "oa"        'gnus-summary-sort-by-author
    "oc"        'gnus-summary-sort-by-chars
    "od"        'gnus-summary-sort-by-date
    "oi"        'gnus-summary-sort-by-score
    "ol"        'gnus-summary-sort-by-lines
    "omd"       'gnus-summary-sort-by-most-recent-date
    "omm"       'gnus-summary-sort-by-marks
    "omn"       'gnus-summary-sort-by-most-recent-number
    "on"        'gnus-summary-sort-by-number
    "oo"        'gnus-summary-sort-by-original
    "or"        'gnus-summary-sort-by-random
    "os"        'gnus-summary-sort-by-subject
    "ot"        'gnus-summary-sort-by-recipient

    ;; Threads commands
    "Tk"        'gnus-summary-kill-thread
    "Tl"        'gnus-summary-lower-thread
    "Ti"        'gnus-summary-raise-thread
    "Tm"        'gnus-uu-mark-thread        ;; was T #
    "Tu"        'gnus-uu-unmark-thread      ;; was T M-#
    "TT"        'gnus-summary-toggle-threads
    "Ts"        'gnus-summary-show-thread
    "Th"        'gnus-summary-hide-thread
    ;; show/hide can be mapped to zo/zc
    "zo"        'gnus-summary-show-thread
    "zc"        'gnus-summary-hide-thread
    "TS"        'gnus-summary-show-all-threads
    "TH"        'gnus-summary-hide-all-threads
    "Tt"        'gnus-summary-rethread-current
    "T^"        'gnus-summary-reparent-thread
    (kbd "T M-^") 'gnus-summary-reparent-children
    "Tn"        'gnus-summary-next-thread
    "Tp"        'gnus-summary-prev-thread
    "Td"        'gnus-summary-down-thread   ;;          descend
    "Ta"        'gnus-summary-up-thread     ;; was T u, ascend also makes sense
    "To"        'gnus-summary-top-thread

    ;; Saving
    "Oo"        'gnus-summary-save-article
    "Om"        'gnus-summary-save-article-mail
    "Or"        'gnus-summary-save-article-rmail
    "Of"        'gnus-summary-save-article-file
    "OF"        'gnus-summary-write-article-file
    "Ob"        'gnus-summary-save-article-body-file
    "Oh"        'gnus-summary-save-article-folder
    "Ov"        'gnus-summary-save-article-vm
    "Op"        'gnus-summary-pipe-output
    "|"         'gnus-summary-pipe-output
    "OP"        'gnus-summary-muttprint

    ;; Decoding with marked articles
    "Xu"        'gnus-uu-decode-uu
    "XU"        'gnus-uu-decode-uu-and-save
    "Xvu"       'gnus-uu-decode-uu-view
    "XvU"       'gnus-uu-decode-uu-and-save-view
    ;; Shell archives
    "Xs"        'gnus-uu-decode-unshar
    "XS"        'gnus-uu-decode-unshar-and-save
    "Xvs"       'gnus-uu-decode-unshar-view
    "XvS"       'gnus-uu-decode-unshar-and-save-view
    ;; PostScript files
    "Xp"        'gnus-uu-decode-postscript
    "XP"        'gnus-uu-decode-postscript-and-save
    "Xvp"       'gnus-uu-decode-postscript-view
    "XvP"       'gnus-uu-decode-postscript-and-save-view
    ;; Other files
    "Xo"        'gnus-uu-decode-save
    "Xb"        'gnus-uu-decode-binhex
    "XY"        'gnus-uu-decode-yenc

    ;; Mail group commands
    "Be"        'gnus-summary-expire-articles
    "BE"        'gnus-summary-expire-articles-now   ;; was B C-M-e
    "Bd"        'gnus-summary-delete-article        ;; was B DEL
    "Bm"        'gnus-summary-move-article
    "Bc"        'gnus-summary-copy-article
    "Bb"        'gnus-summary-crosspost-article     ;; was B B
    "Bi"        'gnus-summary-import-article
    "BI"        'gnus-summary-create-article
    "Br"        'gnus-summary-respool-article
    "Bw"        'gnus-summary-edit-article
    "Bq"        'gnus-summary-respool-query
    "Bt"        'gnus-summary-respool-trace
    "Bp"        'gnus-summary-article-posted-p

    ;; Searching
    (kbd "M-s") 'gnus-summary-search-article-forward
    (kbd "M-r") 'gnus-summary-search-article-backward
    (kbd "M-S") 'gnus-summary-repeat-search-article-forward
    (kbd "M-R") 'gnus-summary-repeat-search-article-backward
    "&"         'gnus-summary-execute-command
    (kbd "M-&") 'gnus-summary-universal-argument

    [mouse-2]   'gnus-mouse-pick-article
    [follow-link] 'mouse-face

    ;; Rest of the bindings "as is".
    "*"         'gnus-cache-enter-article
    (kbd "M-*") 'gnus-cache-remove-article
    (kbd "M-i") 'gnus-symbolic-argument
    "I"         'gnus-summary-increase-score
    "L"         'gnus-summary-lower-score)

  (evil-collection-define-key 'motion 'gnus-article-mode-map
    "F"         'gnus-article-followup-with-original
    "R"         'gnus-article-reply-with-original
    "W"         'gnus-article-wide-reply-with-original)
  (evil-collection-define-key 'normal 'gnus-article-mode-map
    ;; quit
    "Q"         'evil-window-delete
    "ZQ"        'evil-window-delete
    "q"         'evil-window-delete
    "ZZ"        'evil-window-delete

    ;; Movement
    (kbd "TAB") 'forward-button
    (kbd "<backtab>") 'backward-button
    (kbd "SPC") 'gnus-article-goto-next-page
    (kbd "DEL") 'gnus-article-goto-prev-page
    (kbd "S-SPC") 'gnus-article-goto-prev-page

    ;; Reply
    "r"         'gnus-summary-reply
    "R"         'gnus-article-reply-with-original ;; override `evil-replace-state'

    ;; Composing
    "C"         'gnus-article-mail
    "cc"        'gnus-article-mail
    "cr"        'gnus-summary-reply
    "cR"        'gnus-summary-reply-with-original
    "cf"        'gnus-summary-followup
    "cF"        'gnus-summary-followup-with-original
    "cw"        'gnus-summary-very-wide-reply
    "cW"        'gnus-article-wide-reply-with-original
    (kbd "C-c C-f") 'gnus-summary-mail-forward

    ;; Washing
    ;;
    ;; List of unbound commands:
    ;; - `gnus-article-remove-cr'
    ;; - `gnus-article-de-quoted-unreadable'
    ;; - `gnus-article-unsplit-urls'
    ;; - `gnus-article-wash-html'
    ;; - `gnus-article-strip-leading-blank-lines'
    ;; - `gnus-article-strip-multiple-blank-lines'
    ;; - `gnus-article-remove-trailing-blank-lines'
    ;; - `gnus-article-strip-blank-lines'
    ;; - `gnus-article-strip-all-blank-lines'
    ;; - `gnus-article-strip-leading-space'
    ;; - `gnus-article-strip-trailing-space'
    "zwl"       'gnus-summary-stop-page-breaking
    "zwr"       'gnus-summary-caesar-message
    "zwm"       'gnus-summary-morse-message
    "zwi"       'gnus-summary-idna-message
    "zwt"       'gnus-summary-toggle-header
    "zwv"       'gnus-summary-verbose-headers
    "zwo"       'gnus-article-treat-overstrike
    "zwd"       'gnus-article-treat-smartquotes
    "zwu"       'gnus-article-treat-non-ascii               ;; was W U
    "zwyf"      'gnus-article-outlook-deuglify-article
    "zwyu"      'gnus-article-outlook-unwrap-lines
    "zwya"      'gnus-article-outlook-repair-attribution
    "zwyc"      'gnus-article-outlook-rearrange-citation
    "zww"       'gnus-article-fill-cited-article
    "zwq"       'gnus-article-fill-long-lines               ;; was W Q
    "zwc"       'gnus-article-capitalize-sentences          ;; was W C
    "zw6"       'gnus-article-de-base64-unreadable
    "zwz"       'gnus-article-decode-HZ                     ;; was W Z
    "zwa"       'gnus-article-treat-ansi-sequences          ;; was W A
    "zwb"       'gnus-article-add-buttons
    "zwB"       'gnus-article-add-buttons-to-head
    "zwp"       'gnus-article-verify-x-pgp-sig
    "zws"       'gnus-summary-force-verify-and-decrypt

    ;; Actions
    (kbd "C-]") 'gnus-article-refer-article
    "s"         'gnus-article-show-summary
    "gr"        'gnus-summary-show-article
    "gX"        'gnus-summary-browse-url)

  (evil-collection-define-key 'normal 'gnus-group-mode-map
    ;; quit
    "Q"         'gnus-group-quit
    "ZQ"        'gnus-group-quit
    "q"         'gnus-group-exit
    "ZZ"        'gnus-group-exit

    ;; Movement
    "[["        'gnus-group-prev-unread-group
    "]]"        'gnus-group-next-unread-group
    "gk"        'gnus-group-prev-unread-group
    "gj"        'gnus-group-next-unread-group

    ;; Composing, like mu4e
    "C"         'gnus-group-mail
    "cc"        'gnus-group-mail
    "ci"        'gnus-group-news

    ;; Actions
    "."         'gnus-group-first-unread-group
    ","         'gnus-group-best-unread-group
    "A"         'gnus-activate-all-groups
    "B"         'gnus-group-browse-foreign-server
    "E"         'gnus-group-edit-group
    "F"         'gnus-group-find-new-groups
    "J"         'gnus-group-jump-to-group
    "R"         'gnus-group-rename-group
    "X"         'gnus-group-expunge-group
    (kbd "RET") 'gnus-group-select-group
    (kbd "SPC") 'gnus-group-read-group
    "gr"        'gnus-group-get-new-news-this-group
    "gR"        'gnus-group-get-new-news
    "gu"        'gnus-group-unsubscribe-current-group
    "gU"        'gnus-group-unsubscribe-group
    "gc"        'gnus-group-catchup-current
    "gC"        'gnus-group-catchup-current-all
    "ge"        'gnus-group-expire-articles
    "gE"        'gnus-group-expire-all-groups
    "gl"        'gnus-group-set-current-level

    ;; Deleting & Pasting
    "dd"        'gnus-group-kill-group
    "D"         'gnus-group-kill-group
    "p"         'gnus-group-yank-group
    "P"         'gnus-group-yank-group

    ;; Marking
    "m"         'gnus-group-mark-group
    "u"         'gnus-group-unmark-group
    "U"         'gnus-group-unmark-all-groups
    "M"         'gnus-group-mark-buffer
    "*"         'gnus-group-mark-buffer
    "%"         'gnus-group-mark-regexp

    ;; Searching
    "s"         'gnus-group-apropos
    "S"         'gnus-group-description-apropos
    (kbd "M-s") 'gnus-group-read-ephemeral-search-group

    ;; Sorting
    "oa"        'gnus-group-sort-groups-by-alphabet
    "ol"        'gnus-group-sort-groups-by-level
    "om"        'gnus-group-sort-groups-by-method
    "on"        'gnus-group-sort-groups-by-real-name
    "or"        'gnus-group-sort-groups-by-rank
    "os"        'gnus-group-sort-groups
    "ou"        'gnus-group-sort-groups-by-unread
    "ov"        'gnus-group-sort-groups-by-score

    ;; Listing
    "L!"        'gnus-group-list-ticked
    "L/"        'gnus-group-list-limit-map
    "L?"        'gnus-group-list-dormant
    "La"        'gnus-group-list-active        ;; was A A
    "Lc"        'gnus-group-list-cached
    "Lf"        'gnus-group-list-flush-map
    "Lk"        'gnus-group-list-killed
    "Ll"        'gnus-group-list-level
    "Lm"        'gnus-group-list-matching
    "LM"        'gnus-group-list-all-matching
    "Lp"        'gnus-group-list-plus-map
    "Ls"        'gnus-group-list-groups
    "Lu"        'gnus-group-list-all-groups
    "Lz"        'gnus-group-list-zombies

    ;; Topic commands
    ;; `gnus-topic-move-group' can be done through dd then p
    "Tn"        'gnus-topic-create-topic
    "Tj"        'gnus-topic-jump-to-topic
    "Tr"        'gnus-topic-rename
    "Td"        'gnus-topic-delete
    "zc"        'gnus-topic-hide-topic
    "zo"        'gnus-topic-show-topic

    ;; Topic sorting
    "Toa"       'gnus-topic-sort-groups-by-alphabet
    "Tou"       'gnus-topic-sort-groups-by-unread
    "Tol"       'gnus-topic-sort-groups-by-level
    "Tov"       'gnus-topic-sort-groups-by-score
    "Tor"       'gnus-topic-sort-groups-by-rank
    "Tom"       'gnus-topic-sort-groups-by-method
    "Toe"       'gnus-group-sort-groups-by-server
    "Tos"       'gnus-group-sort-groups

    "^"         'gnus-group-enter-server-mode

    (kbd "DEL") 'gnus-group-prev-unread-group
    [mouse-2]   'gnus-mouse-pick-group
    "g?"        'gnus-group-help-map)

  (evil-collection-define-key 'normal 'gnus-server-mode-map
    ;; quit
    "Q"         'gnus-server-exit
    "ZQ"        'gnus-server-exit
    "q"         'gnus-server-exit
    "ZZ"        'gnus-server-exit

    (kbd "RET") 'gnus-server-read-server
    (kbd "SPC") 'gnus-server-read-server-in-server-buffer
    "C"         'gnus-server-close-server
    "D"         'gnus-server-deny-server
    "G"         'gnus-group-make-nnir-group
    "I"         'gnus-server-set-cloud-method-server
    "L"         'gnus-server-offline-server
    "O"         'gnus-server-open-server
    "R"         'gnus-server-remove-denials
    "S"         'gnus-server-show-server
    "a"         'gnus-server-add-server
    "y"         'gnus-server-copy-server
    "e"         'gnus-server-edit-server
    "gr"        'gnus-server-regenerate-server
    "i"         'gnus-server-toggle-cloud-server
    "d"         'gnus-server-kill-server
    "L"         'gnus-server-list-servers
    "s"         'gnus-server-scan-server
    "p"         'gnus-server-yank-server
    "c"         'gnus-server-compact-server
    "M-c"       'gnus-server-close-all-servers
    "M-o"       'gnus-server-open-all-servers)

  (evil-collection-define-key 'normal 'gnus-browse-mode-map
    ;; quit
    "Q"         'gnus-browse-exit
    "ZQ"        'gnus-browse-exit
    "q"         'gnus-browse-exit
    "ZZ"        'gnus-browse-exit

    "u" 'gnus-browse-unsubscribe-current-group
    (kbd "SPC") 'gnus-browse-read-group
    (kbd "RET") 'gnus-browse-select-group)

  (evil-collection-define-key 'normal 'gnus-bookmark-bmenu-mode-map
    ;; quit
    "Q"         'quit-window
    "ZQ"        'quit-window
    "q"         'quit-window
    "ZZ"        'quit-window

    ;; mark and execution
    "m"         'gnus-bookmark-bmenu-mark
    "u"         'gnus-bookmark-bmenu-unmark
    (kbd "DEL") 'gnus-bookmark-bmenu-backup-unmark
    "d"         'gnus-bookmark-bmenu-delete
    "x"         'gnus-bookmark-bmenu-execute-deletions

    (kbd "RET") 'gnus-bookmark-bmenu-select
    [mouse-2]   'gnus-bookmark-bmenu-select-by-mouse
    "L"         'gnus-bookmark-bmenu-load
    "s"         'gnus-bookmark-bmenu-save
    "t"         'gnus-bookmark-bmenu-toggle-infos
    "a"         'gnus-bookmark-bmenu-show-details
    ;; not implemented yet
    "A"         'gnus-bookmark-bmenu-show-all-annotations
    "E"         'gnus-bookmark-bmenu-edit-annotation
    "R"         'gnus-bookmark-bmenu-rename))

(provide 'evil-collection-gnus)
;;; evil-collection-gnus.el ends here
