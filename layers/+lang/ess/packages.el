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
  (use-package ess-site
    :mode (("\\.sp\\'"           . S-mode)
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
           ("\\.jl\\'"           . ess-julia-mode)
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
    :commands (R stata julia SAS)
    :init
    (progn
      (when (configuration-layer/package-usedp 'company)
        (add-hook 'ess-mode-hook 'company-mode-on))
      (with-eval-after-load 'ess-site
        (if ess-user-style-def
            (add-to-list 'ess-style-alist ess-user-style-def))
        (if ess-user-roxy-template
            (setq ess-roxy-template-alist ess-user-roxy-template))

        ;; instead of setting style vars directly, set style with ess-set-style
        (add-hook 'ess-mode-hook (lambda () (ess-set-style ess-user-style)))

        ;; Follow Hadley Wickham's R style guide
        ;; (setq ess-first-continued-statement-offset 2
        ;;       ess-continued-statement-offset 0
        ;;       ess-expression-offset 2
        ;;       ess-nuke-trailing-whitespace-p t
        ;;       ess-default-style 'DEFAULT)

        (defun spacemacs/ess-start-repl ()
          "Start a REPL corresponding to the ess-language of the current buffer."
          (interactive)
          (cond
           ((string= "S" ess-language) (call-interactively 'R))
           ((string= "STA" ess-language) (call-interactively 'stata))
           ((string= "SAS" ess-language) (call-interactively 'SAS))))

        (spacemacs/set-leader-keys-for-major-mode 'ess-julia-mode
          "si" 'julia)
        (spacemacs/set-leader-keys-for-major-mode 'ess-mode
          "si" 'spacemacs/ess-start-repl
          ;; noweb
          "cC" 'ess-eval-chunk-and-go
          "cc" 'ess-eval-chunk
          "cd" 'ess-eval-chunk-and-step
          "cm" 'ess-noweb-mark-chunk
          "cN" 'ess-noweb-previous-chunk
          "cn" 'ess-noweb-next-chunk
          ;; REPL
          "sa" 'ess-switch-process
          "sB" 'ess-eval-buffer-and-go
          "sb" 'ess-eval-buffer
          "sD" 'ess-eval-function-or-paragraph-and-step
          "sd" 'ess-eval-region-or-line-and-step
          "sL" 'ess-eval-line-and-go
          "sl" 'ess-eval-line
          "sR" 'ess-eval-region-and-go
          "sr" 'ess-eval-region
          "sT" 'ess-eval-function-and-go
          "st" 'ess-eval-function
          "sP" 'ess-install-library
          "sp" 'ess-load-library
          "s:" 'ess-execute
          "sw" 'ess-set-working-directory
          ;; R helpers
          "hd" 'ess-R-dv-pprint
          "hi" 'ess-R-object-popup
          "ht" 'ess-R-dv-ctable
          "hh" 'ess-display-help-on-object
          "ha" 'ess-display-help-apropos
          "hp" 'ess-display-package-index
          "hv" 'ess-display-vignettes
          ;; Developer bindings
          "dl" 'ess-developer-load-package
          "de" 'ess-debug-toggle-error-action
          "dt" 'ess-build-tags-for-directory
          "dw" 'ess-watch
          "ds" 'ess-set-style
          "bs" 'ess-bp-set
          "bc" 'ess-bp-set-conditional
          "bl" 'ess-bp-set-logger
          "bt" 'ess-bp-toggle-state
          "bk" 'ess-bp-kill
          "bK" 'ess-bp-kill-all
          "bn" 'ess-bp-next
          "bp" 'ess-bp-previous
          "bm" 'ess-debug-flag-for-debugging
          "bM" 'ess-debug-unflag-for-debugging
          ;; roxygen
          "rh" 'ess-roxy-hide-all
          "rr" 'ess-roxy-update-entry
          "rn" 'ess-roxy-next-entry
          "rp" 'ess-roxy-previous-entry
          "rP" 'ess-roxy-preview-text
          "rt" 'ess-roxy-toggle-hiding
          ;; other views
          "vd"  'ess-rdired)
        (define-key ess-mode-map (kbd "<s-return>") 'ess-eval-line)
        (define-key inferior-ess-mode-map (kbd "C-j") 'comint-next-input)
        (define-key inferior-ess-mode-map (kbd "C-k") 'comint-previous-input)))))

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
