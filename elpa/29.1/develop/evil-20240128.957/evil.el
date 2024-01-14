;;; evil.el --- Extensible vi layer  -*- lexical-binding: t; -*-

;; The following list of authors was kept up to date until the beginning of
;; 2017, when evil moved under new maintainers. For authors since then, please
;; consult the git logs.

;;      Alessandro Piras <laynor at gmail.com>
;;      Alexander Baier <alexander.baier at mailbox.org>
;;      Antono Vasiljev <antono.vasiljev at gmail.com>
;;      Bailey Ling <bling at live.ca>
;;      Barry O'Reilly <gundaetiapo at gmail.com>
;;      Christoph Lange <langec at web.de>
;;      Daniel Reiter <danieltreiter at gmail.com>
;;      Eivind Fonn <evfonn at gmail.com>
;;      Emanuel Evans <emanuel.evans at gmail.com>
;;      Eric Siegel <siegel.eric at gmail.com>
;;      Eugene Yaremenko <w3techplayground at gmail.com>
;;      Frank Fischer <frank-fischer at shadow-soft.de>
;;      Frank Terbeck <ft at bewatermyfriend.org>
;;      Gordon Gustafson <gordon3.14 at gmail.com>
;;      Herbert Jones <jones.herbert at gmail.com>
;;      Jonas Bernoulli <jonas at bernoul.li>
;;      Jonathan Claggett <jclaggett at lonocloud.com>
;;      José A. Romero L. <escherdragon at gmail.com>
;;      Justin Burkett <justin at burkett.cc>
;;      Lars Andersen <expez at expez.com>
;;      Lintaro Ina <tarao.gnn at gmail.com>
;;      Lukasz Wrzosek <wrzoski at mail.com>
;;      Marian Schubert <maio at netsafe.cz>
;;      Matthew Malcomson <>
;;      Michael Markert <markert.michael at googlemail.com>
;;      Mike Gerwitz <mikegerwitz at gnu.org>
;;      Nikolai Weibull <now at bitwi.se>
;;      phaebz <phaebz at gmail.com>
;;      ralesi <scio62 at gmail.com>
;;      Rodrigo Setti <rodrigosetti at gmail.com>
;;      Sanel Zukan <sanelz at gmail.com>
;;      Sarah Brofeldt <sarah at thinkmonster.(none)>
;;      Simon Hafner <hafnersimon at gmail.com>
;;      Stefan Wehr <mail at stefanwehr.de>
;;      Sune Simonsen <sune.simonsen at jayway.com>
;;      Thomas Hisch <thomas at opentech.at>
;;      Tim Harper <timcharper at gmail.com>
;;      Tom Willemse <tom at ryuslash.org>
;;      Trevor Murphy <trevor.m.murphy at gmail.com>
;;      Ulrich Müller <ulm at gentoo.org>
;;      Vasilij Schneidermann <v.schneidermann at gmail.com>
;;      Vegard Øye <vegard_oye at hotmail.com>
;;      Winfred Lu <winfred.lu at gmail.com>
;;      Wolfgang Jenkner <wjenkner at inode.at>
;;      Xiao Hanyu <xiaohanyu1988 at gmail.com>
;;      York Zhao <yzhao at telecor.com>

;; The following line is included for NonGNU ELPA's build script:
;; Maintainer: Tom Dalziel <tom.dalziel@gmail.com>

;; Maintainers: The emacs-evil team. <https://github.com/orgs/emacs-evil/people>
;;      To get in touch, please use the bug tracker or the
;;      mailing list (see below).
;; Created: 2011-03-01
;; Version: 1.15.0
;; Package-Requires: ((emacs "24.1") (cl-lib "0.5") (goto-chg "1.6"))
;; Keywords: emulations
;; URL: https://github.com/emacs-evil/evil
;;      Repository: https://github.com/emacs-evil/evil.git
;;      EmacsWiki: http://www.emacswiki.org/emacs/Evil
;; Bug tracker: https://github.com/emacs-evil/evil/issues
;;      If you have bug reports, suggestions or patches, please
;;      create an issue at the bug tracker (open for everyone).
;;      Other discussions (tips, extensions) go to the mailing list.
;; Mailing list: <implementations-list at lists.ourproject.org>
;;      Subscribe: http://tinyurl.com/implementations-list
;;      Newsgroup: nntp://news.gmane.org/gmane.emacs.vim-emulation
;;      Archives: http://dir.gmane.org/gmane.emacs.vim-emulation
;;      You don't have to subscribe to post; we usually reply
;;      within a few days and CC our replies back to you.
;;
;; This file is NOT part of GNU Emacs.

;;; License:

;; This file is part of Evil.
;;
;; Evil is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Evil is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Evil.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Evil is an extensible vi layer for Emacs. It emulates the main
;; features of Vim, and provides facilities for writing custom
;; extensions.
;;
;; Evil lives in a Git repository. To obtain Evil, do
;;
;;      git clone git://github.com/emacs-evil/evil.git
;;
;; Move Evil to ~/.emacs.d/evil (or somewhere else in the `load-path').
;; Then add the following lines to ~/.emacs:
;;
;;      (add-to-list 'load-path "~/.emacs.d/evil")
;;      (require 'evil)
;;      (evil-mode 1)
;;
;; Evil requires undo-redo (Emacs 28), undo-fu or undo-tree for redo
;; functionality.  Otherwise, Evil uses regular Emacs undo.
;;
;;     https://codeberg.org/ideasman42/emacs-undo-fu
;;     https://melpa.org/#/undo-fu
;;     https://gitlab.com/tsc25/undo-tree
;;     https://elpa.gnu.org/packages/undo-tree.html
;;
;; Evil requires `goto-last-change' and `goto-last-change-reverse'
;; function for the corresponding motions g; g, as well as the
;; last-change-register `.'. One package providing these functions is
;; goto-chg.el:
;;
;;     https://github.com/emacs-evil/goto-chg
;;     https://melpa.org/#/goto-chg
;;     https://elpa.nongnu.org/nongnu/goto-chg.html
;;
;; Without this package the corresponding motions will raise an error.

;;; Code:

(require 'evil-vars)
(require 'evil-common)
(require 'evil-core)
(require 'evil-states)
(require 'evil-repeat)
(require 'evil-macros)
(require 'evil-search)
(require 'evil-ex)
(require 'evil-types)
(require 'evil-commands)
(require 'evil-jumps)
(require 'evil-maps)

(when evil-want-integration
  (require 'evil-integration))

(when evil-want-keybinding
  (require 'evil-keybindings))

(run-hooks 'evil-after-load-hook)

(provide 'evil)

;;; evil.el ends here
