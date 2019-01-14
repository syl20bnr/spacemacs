;;; packages.el --- ESS (R) Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq ess-packages
      '(
        ess
        ess-R-data-view
        golden-ratio
        org
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
    :commands (R stata julia SAS ess-julia-mode)
    :init
    (progn
      (spacemacs/register-repl 'ess-site 'julia)
      (spacemacs/register-repl 'ess-site 'R)
      (spacemacs/register-repl 'ess-site 'SAS)
      (spacemacs/register-repl 'ess-site 'stata)
      (add-hook 'inferior-ess-mode-hook
                'spacemacs//ess-fix-read-only-inferior-ess-mode)
      (when (configuration-layer/package-used-p 'company)
        (add-hook 'ess-r-mode-hook 'company-mode))))

  ;; R --------------------------------------------------------------------------
  (setq spacemacs/ess-config
        '(progn
           ;; Follow Hadley Wickham's R style guide
           (setq ess-first-continued-statement-offset 2
                 ess-continued-statement-offset 0
                 ess-expression-offset 2
                 ess-nuke-trailing-whitespace-p t
                 ess-default-style 'DEFAULT)


           (define-key ess-doc-map "h" 'ess-display-help-on-object)
           (define-key ess-doc-map "p" 'ess-R-dv-pprint)
           (define-key ess-doc-map "t" 'ess-R-dv-ctable)
           (dolist (mode '(ess-julia-mode ess-r-mode))
             (spacemacs/declare-prefix-for-mode mode "ms" "repl")
             (spacemacs/declare-prefix-for-mode mode "mh" "help")
             (spacemacs/declare-prefix-for-mode mode "mr" "extra")
             (spacemacs/declare-prefix-for-mode mode "mw" "pkg")
             (spacemacs/declare-prefix-for-mode mode "md" "dev")
             (spacemacs/declare-prefix-for-mode mode "mc" "noweb")
             (spacemacs/set-leader-keys-for-major-mode
               mode
               ","  'ess-eval-region-or-function-or-paragraph-and-step
               "'"  'spacemacs/ess-start-repl
               "si" 'spacemacs/ess-start-repl
               "ss" 'ess-switch-to-inferior-or-script-buffer
               "sS" 'ess-switch-process
               ;; REPL
               "sB" 'ess-eval-buffer-and-go
               "sb" 'ess-eval-buffer
               "sd" 'ess-eval-region-or-line-and-step
               "sD" 'ess-eval-function-or-paragraph-and-step
               "sL" 'ess-eval-line-and-go
               "sl" 'ess-eval-line
               "sR" 'ess-eval-region-and-go
               "sr" 'ess-eval-region
               "sF" 'ess-eval-function-and-go
               "sf" 'ess-eval-function
               ;; predefined keymaps
               "h" 'ess-doc-map
               "r" 'ess-extra-map
               "w" 'ess-r-package-dev-map
               "d" 'ess-dev-map
               ;; noweb
               "cC" 'ess-eval-chunk-and-go
               "cc" 'ess-eval-chunk
               "cd" 'ess-eval-chunk-and-step
               "cm" 'ess-noweb-mark-chunk
               "cN" 'ess-noweb-previous-chunk
               "cn" 'ess-noweb-next-chunk))
           (dolist (mode '(inferior-ess-mode))
             (spacemacs/declare-prefix-for-mode mode "ms" "repl")
             (spacemacs/declare-prefix-for-mode mode "me" "eval")
             (spacemacs/declare-prefix-for-mode mode "mg" "xref")
             (spacemacs/declare-prefix-for-mode mode "mh" "help")
             (spacemacs/declare-prefix-for-mode mode "mr" "extra")
             (spacemacs/declare-prefix-for-mode mode "mw" "pkg")
             (spacemacs/declare-prefix-for-mode mode "md" "dev")
             (spacemacs/set-leader-keys-for-major-mode
               mode
               ","  'ess-smart-comma
               "ss" 'ess-switch-to-inferior-or-script-buffer
               ;; predefined keymaps
               "h" 'ess-doc-map
               "r" 'ess-extra-map
               "w" 'ess-r-package-dev-map
               "d" 'ess-dev-map))
           (define-key ess-mode-map (kbd "<s-return>") 'ess-eval-line)
           (define-key inferior-ess-mode-map (kbd "C-j") 'comint-next-input)
           (define-key inferior-ess-mode-map (kbd "C-k") 'comint-previous-input)

           (when ess-assign-key
             (define-key ess-r-mode-map          ess-assign-key #'ess-insert-assign)
             (define-key inferior-ess-r-mode-map ess-assign-key #'ess-insert-assign))))

  (eval-after-load "ess-r-mode" spacemacs/ess-config)
  (eval-after-load "ess-julia" spacemacs/ess-config)

  ;; xref integration added with #96ef5a6
  (spacemacs|define-jump-handlers ess-mode 'xref-find-definitions))

(defun ess/init-ess-help ()
  (evilified-state-evilify-map ess-help-mode-map))

(defun ess/init-ess-R-data-view ())

(defun ess/pre-init-golden-ratio ()
  (spacemacs|use-package-add-hook golden-ratio
    :post-config
    (dolist (f '(ess-eval-buffer-and-go
                 ess-eval-function-and-go
                 ess-eval-line-and-go))
      (add-to-list 'golden-ratio-extra-commands f))))

(defun ess/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(R . t))))
