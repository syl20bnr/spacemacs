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
    dumb-jump
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

(defun faust/init-dumb-jump ()
  ;; Load omnisharp-mode with csharp-mode, this should start the omnisharp server automatically
  (add-hook 'faust-mode-hook 'dumb-jump-mode)
  (use-package dumb-jump)
  :defer t
  (spacemacs/set-leader-keys-for-major-mode 'faust-mode
    ;; Navigation
    "g"   'dumb-jump-go
    "b"   'dumb-jump-back))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun faust/post-init-company ()
    (spacemacs|add-company-hook faust-mode)
    (spacemacs/add-to-hooks
     'spacemacs/load-yasnippet
     '(faust-mode-hook))))

(defun faust/faust-to-firefox ()
  "compile a block-diagram and show it in the browser"
  (interactive)
  (set-process-sentinel
   (start-process-shell-command "faust2svg" "faust-compile"
                                (concat  "faust2svg " buffer-file-name))
   'faust/faust2svg-sentinel))

(defun faust/faust2svg-sentinel (process event)
  "show block-diagram in browser"
  (browse-url-of-file (concat  (file-name-sans-extension buffer-file-name) "-svg/process.svg")))

(defun faust/faust-to-jack ()
  "compile a jack-gtk program and run it"
  (interactive)
  (set-process-sentinel
   (start-process-shell-command "faust2jack" "faust-compile"
                                (concat  "faust2jack " buffer-file-name))
   'faust/faust-run-sentinel))

(defun faust/faust-to-jack-qt ()
  "compile a jack-qt program and run it"
  (interactive)
  (set-process-sentinel
   (start-process-shell-command "faust2jaqt" "faust-compile"
                                (concat  "faust2jaqt " buffer-file-name))
   'faust/faust-run-sentinel))

(defun faust/faust-run-sentinel (process event)
  "run the program"
  (start-process-shell-command "faust-run" nil (file-name-sans-extension (buffer-file-name)))
  (switch-to-buffer-other-window "faust-compile"))
