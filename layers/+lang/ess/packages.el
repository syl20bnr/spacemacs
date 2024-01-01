;;; packages.el --- ESS (R) Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
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


(defconst ess-packages
  '(
    company
    flycheck
    ess
    ess-R-data-view
    golden-ratio
    org))

(defun ess/post-init-company ()
  ;; Julia
  (spacemacs|add-company-backends
    :backends company-ess-julia-objects
    :modes ess-julia-mode inferior-ess-julia-mode)
  ;; R
  (spacemacs|add-company-backends
    :backends (company-R-library company-R-args company-R-objects :separate)
    :modes inferior-ess-r-mode)

  ;; Set R company to lsp manually to include file completion
  (unless (eq ess-r-backend 'lsp)
    (spacemacs|add-company-backends
      :backends (company-R-library company-R-args company-R-objects :separate)
      :modes ess-r-mode)))

(defun ess/post-init-flycheck ()
  (spacemacs/enable-flycheck 'ess-r-mode))

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
    (setq ess-use-company nil
          ess-offset-continued 'straight
          ess-nuke-trailing-whitespace-p t
          ess-default-style 'DEFAULT)

    ;; add support for evil states
    (evil-set-initial-state 'ess-help-mode 'motion)

    (spacemacs/register-repl 'ess-site #'spacemacs/ess-start-repl)

    (add-hook 'ess-r-mode-hook #'spacemacs//ess-may-setup-r-lsp)
    (add-hook 'inferior-ess-mode-hook
              'spacemacs//ess-fix-read-only-inferior-ess-mode)

    (with-eval-after-load 'ess-julia
      (spacemacs/ess-bind-keys-for-julia))
    (with-eval-after-load 'ess-r-mode
      (spacemacs/ess-bind-keys-for-r)
      (unless (eq ess-r-backend 'lsp)
        (spacemacs/declare-prefix-for-mode 'ess-r-mode "mg" "goto")
        (define-key ess-doc-map "h" #'ess-display-help-on-object)))
    (with-eval-after-load 'ess-inf-mode
      (spacemacs/ess-bind-keys-for-inferior))
    :config
    (define-key ess-mode-map (kbd "<s-return>") #'ess-eval-line))

  ;; xref integration added with #96ef5a6
  (spacemacs|define-jump-handlers ess-mode 'xref-find-definitions))

(defun ess/init-ess-R-data-view ()
  (use-package ess-R-data-view
    :defer t
    :config
    (dolist (mode '(ess-julia-mode ess-r-mode inferior-ess-mode))
      (spacemacs/set-leader-keys-for-major-mode mode
        "hp" #'ess-R-dv-pprint
        "ht" #'ess-R-dv-ctable))))

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
