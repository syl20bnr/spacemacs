;;; funcs.el -- Faust Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs/faust-to-firefox ()
  "Compile a block-diagram and show it in the browser."
  (interactive)
  (set-process-sentinel
   (start-process-shell-command "faust2svg" "faust-compile"
                                (concat  "faust2svg " buffer-file-name))
   'space-macs//faust2svg-sentinel))

(defun space-macs/faust-to-jack-gtk ()
  "Compile a jack-gtk program and run it."
  (interactive)
  (set-process-sentinel
   (start-process-shell-command "faust2jack" "faust-compile"
                                (concat "faust2jack " buffer-file-name))
   'space-macs//faust-run-sentinel))

(defun space-macs/faust-to-jack-qt ()
  "Compile a jack-qt program and run it."
  (interactive)
  (set-process-sentinel
   (start-process-shell-command "faust2jaqt" "faust-compile"
                                (concat "faust2jaqt " buffer-file-name))
   'space-macs//faust-run-sentinel))

(defun space-macs//faust2svg-sentinel (process event)
  "Show block-diagram in browser"
  (browse-url-of-file (concat  (file-name-sans-extension buffer-file-name)
                               "-svg/process.svg")))

(defun space-macs//faust-run-sentinel (process event)
  "Run the program"
  (start-process-shell-command "faust-run" nil
                               (file-name-sans-extension (buffer-file-name)))
  (switch-to-buffer-other-window "faust-compile"))


