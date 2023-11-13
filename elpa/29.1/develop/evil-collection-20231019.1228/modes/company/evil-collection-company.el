;;; evil-collection-company.el --- Bindings for `company-mode' -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, company, abbrev, convenience, matching

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
;;; Bindings for `company-mode'.

;;; Code:
(require 'company nil t)
(require 'evil-collection)

(declare-function company-tng-mode "company-tng")
(declare-function company-grab-line "company")
(declare-function company-begin-backend "company")
(declare-function company-ensure-emulation-alist "company")

(defgroup evil-collection-company nil
  "Evil bindings for `company-mode'."
  :group 'evil-collection)

(defcustom evil-collection-company-supported-states '(insert replace emacs)
  "The `evil-state's which `company' function can be requested."
  :type '(repeat symbol))
(defcustom evil-collection-want-company-extended-keybindings nil
  "The \='evil-company-extended' keybindings should be requested"
  :type 'boolean)

(defcustom evil-collection-company-enable-keymap-protection t
  "Prevent evil from breaking company completion keymap.
When non-nil, prevents evil from overriding `company-active-map'
after calling `company-doc-buffer'. If disabled, the completion
keymap will be in a broken state."
  :type 'boolean)

(defvar company-active-map)
(defvar company-search-map)

(defconst evil-collection-company-maps '(company-active-map company-search-map))

(defun evil-collection-company-supported-p (command &rest _)
  "Return non-nil if `evil-state' is in supported states."
  (cond
   ((not (bound-and-true-p evil-mode)) t)
   ((eq command 'prefix)
    (memq evil-state evil-collection-company-supported-states))
   (t t)))

(defun evil-collection-company-protect-keymap ()
  "Prevent evil from overriding `company-mode' completion keymap."
  (when (and evil-collection-company-enable-keymap-protection
             (memq 'company-emulation-alist emulation-mode-map-alists))
    (company-ensure-emulation-alist)))

;;;###autoload
(defun evil-collection-company-whole-lines (command &optional arg &rest _ignored)
  "`company-mode' completion backend that completes whole-lines, akin to vim's
C-x C-l."
  (interactive (list 'interactive))
  (require 'company)
  (pcase command
    (`interactive (company-begin-backend 'evil-collection-company-whole-lines))
    (`prefix      (company-grab-line "^[\t\s]*\\(.+\\)" 1))
    (`candidates
     (all-completions
      arg
      (delete-dups
       (split-string
        (replace-regexp-in-string
         "^[\t\s]+" ""
         (concat (buffer-substring-no-properties (point-min) (line-beginning-position))
                 (buffer-substring-no-properties (line-end-position) (point-max))))
        "\\(\r\n\\|[\n\r]\\)" t))))))

;;;###autoload
(defun evil-collection-company-setup ()
  "Set up `evil' bindings for `company'."
  (evil-collection-define-key nil 'company-active-map
    (kbd "C-n") 'company-select-next-or-abort
    (kbd "C-p") 'company-select-previous-or-abort
    (kbd "C-j") 'company-select-next-or-abort
    (kbd "C-k") 'company-select-previous-or-abort
    (kbd "M-j") 'company-select-next
    (kbd "M-k") 'company-select-previous)

  (when evil-collection-want-company-extended-keybindings 
    (evil-collection-define-key nil 'company-active-map
      (kbd "C-l") 'evil-collection-company-whole-lines
      (kbd "C-]") 'company-etags
      (kbd "C-f") 'company-files
      (kbd "C-o") 'company-capf
      (kbd "C-s") 'company-ispell))

  (when evil-want-C-u-scroll
    (evil-collection-define-key nil 'company-active-map
      (kbd "C-u") 'company-previous-page))

  (when evil-want-C-d-scroll
    (evil-collection-define-key nil 'company-active-map
      (kbd "C-d") 'company-next-page))

  (evil-collection-define-key nil 'company-search-map
    (kbd "C-j") 'company-select-next-or-abort
    (kbd "C-k") 'company-select-previous-or-abort
    (kbd "M-j") 'company-select-next
    (kbd "M-k") 'company-select-previous
    (kbd "<escape>") 'company-search-abort)

  ;; https://github.com/emacs-evil/evil-collection/issues/664
  (add-hook 'evil-local-mode-hook #'evil-collection-company-protect-keymap)

  ;; Make `company-mode' not show popup when not in supported state
  (advice-add 'company-call-backend
              :before-while 'evil-collection-company-supported-p))


(provide 'evil-collection-company)
;;; evil-collection-company.el ends here
