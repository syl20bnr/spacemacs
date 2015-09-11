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

(setq ess-packages
  '(
    company
    ess
    ess-R-data-view
    ess-R-object-popup
    ess-smart-equals
    rainbow-delimiters
    ))

(defun ess/init-ess ()
  ;; ESS is not quick to load so we just load it when
  ;; we need it (see my-keybindings.el for the associated
  ;; keybinding)
  (defun load-ess-on-demand ()
    (interactive)
    (-all? '---truthy? (list
                        (require 'ess-site)
                        (require 'ess-R-object-popup)
                        (require 'ess-R-data-view))))

  (evil-leader/set-key "ess" 'load-ess-on-demand)

  (use-package ess
    :defer t
    :init
    (progn
      (setq auto-mode-alist (append
                             '(("\\.sp\\'"           . S-mode)
                               ("/R/.*\\.q\\'"       . R-mode)
                               ("\\.[qsS]\\'"        . S-mode)
                               ("\\.ssc\\'"          . S-mode)
                               ("\\.SSC\\'"          . S-mode)
                               ("\\.[rR]\\'"         . R-mode)
                               ("\\.[rR]nw\\'"       . Rnw-mode)
                               ("\\.[sS]nw\\'"       . Snw-mode)
                               ("\\.[rR]profile\\'"  . R-mode)
                               ("NAMESPACE\\'"       . R-mode)
                               ("CITATION\\'"        . R-mode)
                               ("\\.omg\\'"          . omegahat-mode)
                               ("\\.hat\\'"          . omegahat-mode)
                               ("\\.lsp\\'"          . XLS-mode)
                               ("\\.do\\'"           . STA-mode)
                               ("\\.ado\\'"          . STA-mode)
                               ("\\.[Ss][Aa][Ss]\\'" . SAS-mode)
                               ("\\.[Ss]t\\'"        . S-transcript-mode)
                               ("\\.Sout"            . S-transcript-mode)
                               ("\\.[Rr]out"         . R-transcript-mode)
                               ("\\.Rd\\'"           . Rd-mode)
                               ("\\.[Bb][Uu][Gg]\\'" . ess-bugs-mode)
                               ("\\.[Bb][Oo][Gg]\\'" . ess-bugs-mode)
                               ("\\.[Bb][Mm][Dd]\\'" . ess-bugs-mode)
                               ("\\.[Jj][Aa][Gg]\\'" . ess-jags-mode)
                               ("\\.[Jj][Oo][Gg]\\'" . ess-jags-mode)
                               ("\\.[Jj][Mm][Dd]\\'" . ess-jags-mode))
                             auto-mode-alist))

      (defun ess/auto-load-hack (mode-symbol)
        (eval
         `(defun ,mode-symbol ()
            "This is a function that will hijack itself with its
definition from ess. The reason this exists is that ess does
not play nicely with autoloads"
            (when (load-ess-on-demand)
              (,mode-symbol)))))

      (defvar ess/r-modes-list '(R-mode R-transcript-mode Rd-mode Rnw-mode S-mode S-transcript-mode
                                        SAS-mode STA-mode Snw-mode XLS-mode ess-bugs-mode ess-jags-mode omegahat-mode)
        "This is the list of modes defined by ess")

      (mapc (lambda (sym) (ess/auto-load-hack sym)) ess/r-modes-list)

      (push '(company-R-args company-R-objects) company-backends-ess-mode)))

  ;; R --------------------------------------------------------------------------
  (eval-after-load "ess-site"
    '(progn
       (add-to-list 'auto-mode-alist '("\\.R$" . R-mode))
       ;; Follow Hadley Wickham's R style guide
       (setq ess-first-continued-statement-offset 2
             ess-continued-statement-offset 0
             ess-expression-offset 2
             ess-nuke-trailing-whitespace-p t
             ess-default-style 'DEFAULT)
       (evil-leader/set-key-for-mode 'ess-mode
         "msi" 'R
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
       (define-key ess-mode-map (kbd "<s-return>") 'ess-eval-line)
       (define-key inferior-ess-mode-map (kbd "C-j") 'comint-next-input)
       (define-key inferior-ess-mode-map (kbd "C-k") 'comint-previous-input))))

(defun ess/init-ess-R-data-view ())

(defun ess/init-ess-R-object-popup ())

(defun ess/post-init-rainbow-delimiters ()
  (add-hook 'ess-mode-hook #'rainbow-delimiters-mode))

(defun ess/init-ess-smart-equals ()
  (use-package ess-smart-equals
    :defer t
    :if ess-enable-smart-equals
    :init
    (progn
      (add-hook 'ess-mode-hook 'ess-smart-equals-mode)
      (add-hook 'inferior-ess-mode-hook 'ess-smart-equals-mode))))

(defun ess/post-init-company ()
  (spacemacs|add-company-hook ess-mode))
