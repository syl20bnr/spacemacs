;;; packages.el --- faust layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author:  Bart Brouns <bart@magnetophon.nl>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst faust-packages
  '(company
    faust-mode
    yasnippet))

(defun faust/init-faust-mode ()
  (use-package faust-mode
    :defer t
    :mode "\\.\\(dsp\\|lib\\)\\'")
  (spacemacs/set-leader-keys-for-major-mode 'faust-mode
    ;; Compile
    "f" 'faust/faust-to-firefox
    "j" 'faust/faust-to-jack
    "q" 'faust/faust-to-jack-qt))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun faust/post-init-company ()
    (spacemacs|add-company-hook faust-mode)
    (spacemacs/add-to-hooks
     'spacemacs/load-yasnippet
     '(faust-mode-hook))))

(defun faust/shell-command-on-buffer (command)
  "todo"
  (interactive)
  (if (buffer-file-name
       (shell-command (concat command (buffer-file-name)))
       )
      (error "The buffer must be saved in a file first")))



(defun faust/async-shell-command-on-buffer (command)
  (interactive)
  (if (buffer-file-name
       (start-process-shell-command "faust-compile" "faust-compile" (concat command (buffer-file-name))))
      (error "The buffer must be saved in a file first")))

(defun faust/faust-to-firefox ()
  (interactive)
  (set-process-sentinel
   (start-process-shell-command "faust2svg" "faust-compile"
                                (concat  "faust2svg " buffer-file-name))
   'faust2svg-sentinel))

(defun faust2svg-sentinel (process event)
  (browse-url-of-file (concat  (file-name-sans-extension buffer-file-name) "-svg/process.svg")))

(defun faust/faust-to-jack ()
  (interactive)
  (set-process-sentinel
   (start-process-shell-command "faust2jack" "faust-compile"
                                (concat  "faust2jack " buffer-file-name))
   'faust-run-sentinel))

(defun faust/faust-to-jack-qt ()
  (interactive)
  (set-process-sentinel
   (start-process-shell-command "faust2jaqt" "faust-compile"
                                (concat  "faust2jaqt " buffer-file-name))
   'faust-run-sentinel))

(defun faust-run-sentinel (process event)
  (start-process-shell-command "faust-run" "faust-compile" (file-name-sans-extension (buffer-file-name))))
