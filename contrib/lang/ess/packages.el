;;; packages.el --- ESS (R) Layer packages File for Spacemacs
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

(defvar ess-packages
  '(
    ess
    ess-R-data-view
    ess-R-object-popup
    ess-smart-underscore
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar ess-excluded-packages '()
  "List of packages to exclude.")

(defun ess/init-ess ()
  ;; ESS is not quick to load so we just load it when
  ;; we need it (see my-keybindings.el for the associated
  ;; keybinding)
  (defun load-ess-on-demand ()
    (interactive)
    (use-package ess-site)
    (use-package ess-smart-underscore)
    (use-package ess-R-object-popup)
    (use-package ess-R-data-view)
    )
  (evil-leader/set-key "ess" 'load-ess-on-demand)

  ;; R --------------------------------------------------------------------------
  (eval-after-load "ess-site"
    '(progn
       (evil-leader/set-key-for-mode 'ess-mode
         "mi" 'R
         ;; noweb
         "mcC" 'ess-eval-chunk-and-go
         "mcc" 'ess-eval-chunk
         "mcd" 'ess-eval-chunk-and-step
         "mcm" 'ess-noweb-mark-chunk
         "mcN" 'ess-noweb-previous-chunk
         "mcn" 'ess-noweb-next-chunk
         ;; helpers
         "mhd" 'ess-R-dv-pprint
         "mhi" 'ess-R-object-popup
         "mht" 'ess-R-dv-ctable
         ;; REPL
         "msB" 'ess-eval-buffer-and-go
         "msb" 'ess-eval-buffer
         "msD" 'ess-eval-function-or-paragraph-and-step
         "msd" 'ess-eval-region-or-line-and-step
         "msL" 'ess-eval-line-and-go
         "msl" 'ess-eval-line
         "msR" 'ess-eval-region-and-go
         "msr" 'ess-eval-region
         "msT" 'ess-eval-function-and-go
         "mst" 'ess-eval-function
         )
       (define-key inferior-ess-mode-map (kbd "C-j") 'comint-next-input)
       (define-key inferior-ess-mode-map (kbd "C-k") 'comint-previous-input))))
