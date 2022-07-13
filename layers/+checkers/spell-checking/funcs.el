;;; funcs.el --- Spell Checking Layer functions File for Spacemacs
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


(defun spell-checking/add-flyspell-hook (hook)
  "Add `flyspell-mode' to the given HOOK, if
`spell-checking-enable-by-default' is true."
  (when spell-checking-enable-by-default
    (add-hook hook 'flyspell-mode)))

(defun spell-checking/change-dictionary ()
  "Change the dictionary. Use the ispell version if
auto-dictionary is not used, use the adict version otherwise."
  (interactive)
  (if (fboundp 'adict-change-dictionary)
      (adict-change-dictionary)
    (call-interactively 'ispell-change-dictionary)))

(defun spacemacs/add-word-to-dict-buffer ()
  "Save word at point as correct in current buffer."
  (interactive)
  (spacemacs//add-word-to-dict 'buffer))

(defun spacemacs/add-word-to-dict-global ()
  "Save word at point as a correct word globally."
  (interactive)
  (spacemacs//add-word-to-dict 'save))

(defun spacemacs/add-word-to-dict-session ()
  "Save word at point as correct in current session."
  (interactive)
  (spacemacs//add-word-to-dict 'session))

(defun spacemacs//add-word-to-dict (scope)
  "Save word at point as a correct word.
SCOPE can be:
`save' to save globally,
`session' to save in current session or
`buffer' for buffer local."
  (let ((current-location (point))
        (word (flyspell-get-word)))
    (when (consp word)
      (if (spacemacs//word-in-dict-p (car word))
          (error "%s is already in dictionary" (car word))
        (progn
          (flyspell-do-correct scope nil (car word) current-location
                               (cadr word) (caddr word) current-location)
          (ispell-pdict-save t))))))

(defun spacemacs//word-in-dict-p (word)
  "Check if WORD is defined in any of the active dictionaries."
  ;; use the correct dictionary
  (flyspell-accept-buffer-local-defs)
  (let (poss ispell-filter)
    ;; now check spelling of word.
    (ispell-send-string "%\n")	;put in verbose mode
    (ispell-send-string (concat "^" word "\n"))
    ;; wait until ispell has processed word
    (while (progn
             (accept-process-output ispell-process)
             (not (string= "" (car ispell-filter)))))
    ;; Remove leading empty element
    (setq ispell-filter (cdr ispell-filter))
    ;; ispell process should return something after word is sent.
    ;; Tag word as valid (i.e., skip) otherwise
    (or ispell-filter
        (setq ispell-filter '(*)))
    (if (consp ispell-filter)
        (setq poss (ispell-parse-output (car ispell-filter))))
    (or (eq poss t) (stringp poss))))
