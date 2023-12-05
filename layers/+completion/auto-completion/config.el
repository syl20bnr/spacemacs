;;; config.el --- Auto-completion configuration File for Spacemacs
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
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


;; Company -------------------------------------------------------------------

(defvar spacemacs-default-company-backends
  '((company-semantic company-dabbrev-code company-gtags company-etags company-keywords)
    company-files company-dabbrev)
  "The list of default company backends used by spacemacs.
This variable is used to configure mode-specific company backends in spacemacs.
Backends in this list will always be active in these modes, as well as any
backends added by individual spacemacs layers.")

(defvar-local auto-completion-front-end 'company
  "Which auto-completion front end to use.")

(defvar auto-completion-return-key-behavior 'complete
  "What the RET key should do when auto-completion menu is active.
Possible values are `complete' or `nil'.")

(defvar auto-completion-tab-key-behavior 'cycle
  "What the TAB key should do when auto-completion menu is active.
Possible values are `complete', `cycle' or `nil'.")

(defvar auto-completion-complete-with-key-sequence nil
  "Provide a key sequence (string) to complete the current
selection.")

(defvar auto-completion-complete-with-key-sequence-delay 0.1
  "Timeout (seconds) when waiting for the second key of
`auto-completion-complete-with-key-sequence'.")

(defvar auto-completion-minimum-prefix-length 2
  "The minimum prefix length for idle completion.")

(defvar auto-completion-idle-delay 0.2
  "Delay (seconds) before completions are shown.")

(defvar auto-completion-enable-snippets-in-popup nil
  "If non nil show snippets in the auto-completion popup.")

(defvar auto-completion-enable-sort-by-usage nil
  "If non nil suggestions are sorted by how often they are used.")

(defvar auto-completion-enable-help-tooltip nil
  "If non nil the docstring appears in a tooltip.
If set to `manual', help tooltip appears only when invoked
manually.")

(defvar auto-completion-use-company-box nil
  "If non nil company-box is used.")

(defvar auto-completion-use-company-posframe nil
  "If non nil company-posframe is used.")

(defvar auto-completion-private-snippets-directory nil
  "Configurable private snippets directory.")
