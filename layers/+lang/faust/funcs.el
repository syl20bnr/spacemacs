;;; funcs.el -- Faust Layer functions File for Spacemacs
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


(defun spacemacs/faust-to-firefox ()
  "Compile a block-diagram and show it in the browser."
  (interactive)
  (set-process-sentinel
   (start-process-shell-command "faust2svg" "faust-compile"
                                (concat  "faust2svg " buffer-file-name))
   'spacemacs//faust2svg-sentinel))

(defun spacemacs/faust-to-jack-gtk ()
  "Compile a jack-gtk program and run it."
  (interactive)
  (set-process-sentinel
   (start-process-shell-command "faust2jack" "faust-compile"
                                (concat "faust2jack " buffer-file-name))
   'spacemacs//faust-run-sentinel))

(defun spacemacs/faust-to-jack-qt ()
  "Compile a jack-qt program and run it."
  (interactive)
  (set-process-sentinel
   (start-process-shell-command "faust2jaqt" "faust-compile"
                                (concat "faust2jaqt " buffer-file-name))
   'spacemacs//faust-run-sentinel))

(defun spacemacs//faust2svg-sentinel (process event)
  "Show block-diagram in browser"
  (browse-url-of-file (concat  (file-name-sans-extension buffer-file-name)
                               "-svg/process.svg")))

(defun spacemacs//faust-run-sentinel (process event)
  "Run the program"
  (start-process-shell-command "faust-run" nil
                               (file-name-sans-extension (buffer-file-name)))
  (switch-to-buffer-other-window "faust-compile"))
