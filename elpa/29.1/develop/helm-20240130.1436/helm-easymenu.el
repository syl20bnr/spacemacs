;;; helm-easymenu.el --- Helm easymenu definitions. -*- lexical-binding: t -*-

;; Copyright (C) 2015 ~ 2020 Thierry Volpiatto 

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

;;; Code:

(require 'easymenu)

(easy-menu-add-item
 nil '("Tools")
 '("Helm"
   ["Find any Files/Buffers" helm-multi-files t]
   ["Helm Everywhere (Toggle)" helm-mode t]
   ["Helm resume" helm-resume t]
   "----"
   ("Files"
    ["Find files" helm-find-files t]
    ["Recent Files" helm-recentf t]
    ["Locate" helm-locate t]
    ["Search Files with find" helm-find t]
    ["Bookmarks" helm-filtered-bookmarks t]
    ["Locate library" helm-locate-library t])
   ("Buffers"
    ["Find buffers" helm-buffers-list t])
   ("Projects"
    ["Browse project" helm-browse-project]
    ["Projects history" helm-projects-history])
   ("Commands"
    ["Emacs Commands" helm-M-x t]
    ["Externals Commands" helm-run-external-command t])
   ("Help"
    ["Helm Apropos" helm-apropos t])
   ("Info"
    ["Info at point" helm-info-at-point t]
    ["Emacs Manual index" helm-info-emacs t]
    ["Gnus Manual index" helm-info-gnus t]
    ["Helm documentation" helm-documentation t])
   ("Elpa"
    ["Elisp packages" helm-packages t])
   ("Tools"
    ["Occur" helm-occur t]
    ["Grep current directory with AG" helm-do-grep-ag t]
    ["Gid"  helm-gid t]
    ["Etags" helm-etags-select t]
    ["Lisp complete at point" helm-lisp-completion-at-point t]
    ["Browse Kill ring" helm-show-kill-ring t]
    ["Browse register" helm-register t]
    ["Mark Ring" helm-all-mark-rings t]
    ["Regexp handler" helm-regexp t]
    ["Colors & Faces" helm-colors t]
    ["Show xfonts" helm-select-xfont t]
    ["Ucs Symbols" helm-ucs t]
    ["Imenu" helm-imenu t]
    ["Imenu all" helm-imenu-in-all-buffers t]
    ["Semantic or Imenu" helm-semantic-or-imenu t]
    ["Google Suggest" helm-google-suggest t]
    ["Eval expression" helm-eval-expression-with-eldoc t]
    ["Calcul expression" helm-calcul-expression t]
    ["Man pages" helm-man-woman t]
    ["Top externals process" helm-top t]
    ["Emacs internals process" helm-list-emacs-process t])
   "----"
   ["Preferred Options" helm-configuration t])
 "Spell Checking")

(easy-menu-add-item nil '("Tools") '("----") "Spell Checking")


(provide 'helm-easymenu)

;;; helm-easymenu.el ends here
