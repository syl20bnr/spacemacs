;;; packages.el --- ace-window Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar ace-window-packages
  '(
    ;; package ace-windows go here
    ace-window
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar ace-window-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function ace-window/init-<package-ace-window>
;;
;; (defun ace-window/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun ace-window/init-ace-window ()
  (use-package ace-window
    :bind ("C-x o" . ace-window)
    :config (progn
              ;; Shorter key-binding
              ;; (global-set-key (kbd "M-o") 'ace-window)
              (defun ace-delete-window ()
                "Ace delete window."
                (interactive)
                (aw-delete-window
                 (aw-select " Ace - Delete Window")))

              (defun aw-delete-window (window)
                "Delete window WINDOW."
                (let ((frame (window-frame window)))
                  (when (and (frame-live-p frame)
                             (not (eq frame (selected-frame))))
                    (select-frame-set-input-focus (window-frame window)))
                  (if (= 1 (length (window-list)))
                      (delete-frame frame)
                    (if (window-live-p window)
                        (delete-window window)
                      (error "Got a dead window %S" window)))))
              (evil-leader/set-key
                "ww" 'ace-window
                "wC" 'ace-delete-window
                ))
    )
  )
