;;; packages.el --- ESS (R) Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; TODO evilify mode resulting from ess-execute and thus
;; ess-execute-objects/search/... ess-execute-search, ess-execute

(setq ess-packages
      '(
        ;; company
        ess
        ess-R-data-view
        ess-R-object-popup
        ess-smart-equals
        rainbow-delimiters
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
      (spacemacs/register-repl 'ess-site 'julia)
      (spacemacs/register-repl 'ess-site 'R)
      (spacemacs/register-repl 'ess-site 'SAS)
      (spacemacs/register-repl 'ess-site 'stata)
      ;; Explicitly run prog-mode hooks since ess-mode does not derive from
      ;; prog-mode major-mode
      (add-hook 'ess-mode-hook 'spacemacs/run-prog-mode-hooks)
      (add-hook 'inferior-ess-mode-hook
                'spacemacs//ess-fix-read-only-inferior-ess-mode)
      (when (configuration-layer/package-usedp 'company)
        (add-hook 'ess-mode-hook 'company-mode))

      (with-eval-after-load 'ess-site
        ;; set user variables
        (if ess-user-style-def
            (add-to-list 'ess-style-alist ess-user-style-def))
        (if ess-user-roxy-template
            (setq ess-roxy-template-alist ess-user-roxy-template))

        ;; instead of setting style var directly, set style with ess-set-style
        (add-hook 'ess-mode-hook (lambda () (ess-set-style ess-user-style)))

        ;; evilify ess- rdired and watch modes
        (evilified-state-evilify ess-rdired-mode ess-rdired-mode-map
          "v" 'ess-rdired-view
          "V" 'ess-rdired-View
          "g" 'revert-buffer)
        (evilified-state-evilify ess-watch-mode ess-watch-mode-map
          "g" 'revert-buffer
          ;; movement bindings.
          "j" 'ess-watch-next-block
          "k" 'ess-watch-previous-bloc
          "l" 'ess-watch-next-block
          "h" 'ess-watch-previous-bloc
          "n" 'ess-watch-next-block
          "p" 'ess-watch-previous-bloc

          ;; add edit binding. Otherwise, the default evilified bindings
          ;; make sense.
          "c" 'ess-watch-edit-expression
          "D" 'ess-watch-kill

          ;; bind this as otherwise it doesn't work, looks like the individual
          ;; watches act like their own buffers
          "G" 'end-of-buffer

          ;; TODO figure out how to rebind this one...
          ;;"gg" 'beginning-of-buffer
          )

        ;; evilify our shim state to get q working in evil and emacs mode
        (evilified-state-evilify
          ess-fundamental-mode ess-fundamental-mode-map
          "q" 'ess-fundamental-kill-return)

        ;; Need to do this, for some reason the state doesn't start evilified...
        (add-hook 'ess-fundamental-mode-hook 'evil-evilified-state)

        ;; advise ess-execute to cover most ess use cases
        ;; (advice-add 'ess-execute :before #'ess-fundamental-set-source-buffer)
        (advice-add
         'ess-execute :around
         (lambda (orig-fun command &optional invert buff message)
           (ess-fundamental-set-source-buffer)
           (let ((res (orig-fun command invert buff message)))
             (with-current-buffer (concat "*" (or buff "ess-output") "*")
               (ess-fundamental-mode))
             res)))

        ;; ess-show-traceback is a special case
        (advice-add
         'ess-show-traceback :before  #'ess-fundamental-set-source-buffer)
        (advice-add
         'ess-show-traceback :after
         (lambda ()
           (with-current-buffer (get-buffer "*ess-traceback*")
             (ess-fundamental-mode))))

        ;; keybindings
        (spacemacs/declare-prefix-for-mode 'ess-mode "mc" "noweb")
        (spacemacs/declare-prefix-for-mode 'ess-mode "ms" "repl-interaction")
        (spacemacs/declare-prefix-for-mode 'ess-mode "mh" "help")
        (spacemacs/declare-prefix-for-mode 'ess-mode "md" "developer")
        (spacemacs/declare-prefix-for-mode 'ess-mode "mb" "debugging")
        (spacemacs/declare-prefix-for-mode 'ess-mode "mv" "views")
        (spacemacs/declare-prefix-for-mode 'ess-mode "mr" "roxygen")

        (spacemacs/set-leader-keys-for-major-mode 'ess-mode
          "'"  'spacemacs/ess-start-repl
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
          "hh" 'ess-display-help-on-object
          "hH" 'ess-describe-object-at-point
          "ha" 'ess-display-help-apropos
          "hp" 'ess-display-package-index
          "hv" 'ess-display-vignettes
          "hw" 'ess-help-web-search

          ;; These two aren't applicable to / implemented in R, but might be
          ;; utilizted in other dialects
          "hm" 'ess-manual-lookup
          "hr" 'ess-reference-lookup

          ;; Developer bindings
          "dT" 'ess-build-tags-for-directory
          "ds" 'ess-set-style
          "dg" 'ess-dump-object-into-edit-buffer
          ;; TODO only show these bindings if we're in R-mode.
          "dl" 'ess-r-devtools-load-package
          "dp" 'ess-r-devtools-set-pacakge
          "dt" 'ess-r-devtools-test-pacakge
          "dc" 'ess-r-devtools-check-pacakge
          "dr" 'ess-r-devtools-document-package
          "du" 'ess-r-devtools-unload-package
          "di" 'ess-r-devtools-install-package

          ;; debug bindings
          "bT" 'ess-show-traceback
          ;; "b~" 'ess-show-callstack
          ;; "bC" 'ess-show-callstack
          "bs" 'ess-bp-set
          "be" 'ess-debug-toggle-error-action
          "bc" 'ess-bp-set-conditional
          "bl" 'ess-bp-set-logger
          "bt" 'ess-bp-toggle-state
          "bd" 'ess-bp-kill
          "bD" 'ess-bp-kill-all
          "bn" 'ess-bp-next
          "bp" 'ess-bp-previous
          "bm" 'ess-debug-flag-for-debugging
          "bM" 'ess-debug-unflag-for-debugging
          "bw" 'ess-watch
          ;; roxygen
          "rh" 'ess-roxy-hide-all
          "rr" 'ess-roxy-update-entry
          "rn" 'ess-roxy-next-entry
          "rp" 'ess-roxy-previous-entry
          "rP" 'ess-roxy-preview-text
          "rt" 'ess-roxy-toggle-hiding
          ;; other views
          "vp" 'ess-R-dv-pprint
          "vi" 'ess-R-object-popup
          "vt" 'ess-R-dv-ctable
          "vd" 'ess-rdired)
        (define-key ess-mode-map (kbd "<s-return>") 'ess-eval-line)

        ;; define the applicable subset of the above keys for the inferior mode
        ;; as well
        (spacemacs/declare-prefix-for-mode 'inferior-ess-mode "ms" "repl-interaction")
        (spacemacs/declare-prefix-for-mode 'inferior-ess-mode "mh" "help")
        (spacemacs/declare-prefix-for-mode 'inferior-ess-mode "md" "developer")
        (spacemacs/declare-prefix-for-mode 'inferior-ess-mode "mb" "debugging")
        (spacemacs/declare-prefix-for-mode 'inferior-ess-mode "mv" "views")
        (spacemacs/declare-prefix-for-mode 'inferior-ess-mode "mr" "roxygen")
        (spacemacs/set-leader-keys-for-major-mode 'inferior-ess-mode
          "sP" 'ess-install-library
          "sp" 'ess-load-library
          "s:" 'ess-execute
          "sw" 'ess-set-working-directory
          ;; R helpers
          "hh" 'ess-display-help-on-object
          "hH" 'ess-describe-object-at-point
          "ha" 'ess-display-help-apropos
          "hp" 'ess-display-package-index
          "hv" 'ess-display-vignettes
          "hw" 'ess-help-web-search

          ;; These two aren't applicable to / implemented in R, but might be
          ;; utilizted in other dialects
          "hm" 'ess-manual-lookup
          "hr" 'ess-reference-lookup

          ;; Developer bindings
          "dg" 'ess-dump-object-into-edit-buffer

          ;; TODO only show these bindings if we're in R-mode.
          "dl" 'ess-r-devtools-load-package
          "dp" 'ess-r-devtools-set-pacakge
          "dt" 'ess-r-devtools-test-pacakge
          "dc" 'ess-r-devtools-check-pacakge
          "dr" 'ess-r-devtools-document-package
          "du" 'ess-r-devtools-unload-package
          "di" 'ess-r-devtools-install-package

          ;; debug bindings
          "bT" 'ess-show-traceback
          "be" 'ess-debug-toggle-error-action
          "bw" 'ess-watch
          ;; other views
          "vp" 'ess-R-dv-pprint
          "vi" 'ess-R-object-popup
          "vt" 'ess-R-dv-ctable
          "vd" 'ess-rdired)
        (define-key inferior-ess-mode-map (kbd "C-j") 'comint-next-input)
        (define-key inferior-ess-mode-map (kbd "C-k") 'comint-previous-input))

      ;; ess-mode-julia derives from julia-mode, not ess-mode, but otherwise
      ;; functions the same as e.g. R-mode. Unfortunately, that means we have to
      ;; duplicate a lot of the above logic for ess-julia here...
      (add-hook 'ess-julia-mode-hook 'spacemacs/run-prog-mode-hooks)
      (when (configuration-layer/package-usedp 'company)
        (add-hook 'ess-julia-mode-hook 'company-mode))
      (with-eval-after-load 'ess-julia
        (spacemacs/declare-prefix-for-mode 'ess-julia-mode "ms" "repl-interaction")
        (spacemacs/declare-prefix-for-mode 'ess-julia-mode "mh" "help")
        (spacemacs/set-leader-keys-for-major-mode 'ess-julia-mode
          "'"  'spacemacs/ess-start-repl
          "si" 'spacemacs/ess-start-repl
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
          ;; "sP" 'ess-install-library
          ;; "sp" 'ess-load-library
          ;; "s:" 'ess-execute
          "sw" 'ess-set-working-directory
          "hh" 'ess-display-help-on-object
          "hw" 'ess-help-web-search
          "hm" 'ess-manual-lookup)
        (define-key ess-mode-map (kbd "<s-return>") 'ess-eval-line)
        (define-key inferior-ess-mode-map (kbd "C-j") 'comint-next-input)
        (define-key inferior-ess-mode-map (kbd "C-k") 'comint-previous-input)))))

(defun ess/init-ess-R-data-view ()
  (use-package ess-R-data-view
    :defer t
    :after 'ess-site
    :config
    (progn
      (advice-add
       'ess-R-dv-pprint :around
       (lambda (orig-fun &rest args)
         (let ((buf (apply orig-fun args)))
           (with-current-buffer (get-buffer buf)
             (ess-fundamental-mode))))
       )
      (advice-add 'ess-R-dv-pprint :before #'ess-fundamental-set-source-buffer)
      (advice-add 'ess-R-dv-ctable :before #'ess-fundamental-set-source-buffer)

      ;; bind q in ess-R-dv-ctable modes as well
      (with-eval-after-load 'ctable
        (define-key ctbl:table-mode-map (kbd "q") 'ess-fundamental-kill-return)
        (evilified-state-evilify ctbl:table-mode ctbl:table-mode-map
          "q" 'ess-fundamental-kill-return)))))

(defun ess/init-ess-R-object-popup ())

(defun ess/init-ess-smart-equals ()
  (use-package ess-smart-equals
    :defer t
    :if ess-enable-smart-equals
    :init
    (progn
      (add-hook 'ess-mode-hook 'ess-smart-equals-mode)
      (add-hook 'inferior-ess-mode-hook 'ess-smart-equals-mode))))

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
