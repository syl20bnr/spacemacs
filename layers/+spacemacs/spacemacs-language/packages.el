;;; packages.el --- Spacemacs Language Layer packages File
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


(setq spacemacs-language-packages
      '((define-word :toggle (not (bound-and-true-p osx-use-dictionary-app)))
        google-translate))

(defun spacemacs-language/init-define-word ()
  (use-package define-word
    :defer t
    :init
    (spacemacs/set-leader-keys
      "xwd" 'define-word-at-point)))

(defun spacemacs-language/init-google-translate ()
  (use-package google-translate
    :commands (spacemacs/set-google-translate-languages)
    :init
    ;; fix search fail ',ttk'
    ;; (see https://github.com/atykhonov/google-translate/issues/52#issuecomment-727920888)
    (with-eval-after-load 'google-translate-tk
      (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130)))

    (defun spacemacs/set-google-translate-languages (&optional override-p)
      "Set source language for google translate.
For instance pass En as source for English."
      (interactive "P")
      (autoload 'google-translate-read-args "google-translate-default-ui")
      (let* ((langs (google-translate-read-args override-p nil))
             (source-language (car langs))
             (target-language (cadr langs)))
        (setq google-translate-default-source-language source-language)
        (setq google-translate-default-target-language target-language)
        (message
         (format "Set google translate source language to %s and target to %s"
                 source-language target-language))))

    (defun spacemacs/set-google-translate-target-language ()
      "Set the target language for google translate."
      (interactive)
      (spacemacs/set-google-translate-languages nil))

    (spacemacs/set-leader-keys
      "xgL" 'spacemacs/set-google-translate-languages
      "xgl" 'spacemacs/set-google-translate-target-language
      "xgQ" 'google-translate-query-translate-reverse
      "xgq" 'google-translate-query-translate
      "xgT" 'google-translate-at-point-reverse
      "xgt" 'google-translate-at-point)
    (setq google-translate-enable-ido-completion t)
    (setq google-translate-show-phonetic t)
    (setq google-translate-default-source-language "auto")))
