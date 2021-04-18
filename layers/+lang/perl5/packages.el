;;; packages.el --- Perl5 Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Troy Hinckley <troyhinckley@gmail.com>
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


(defconst perl5-packages
  '(
    (company-plsense :requires company)
    (cperl-mode :location built-in)
    flycheck
    org
    dap-mode
    realgud
    smartparens))

(defun perl5/pre-init-dap-mode ()
  (add-hook 'cperl-mode-local-vars-hook #'spacemacs//perl5-setup-dap))

(defun perl5/post-init-company ()
  (add-hook 'cperl-mode-local-vars-hook #'spacemacs//perl5-setup-company))

(defun perl5/init-company-plsense ()
  (use-package company-plsense
    :defer t))

(defun perl5/init-cperl-mode ()
  (use-package cperl-mode
    :defer t
    :mode "\\.\\(p[lm]x?\\|P[LM]X?\\)\\'"
    :interpreter "perl"
    :interpreter "perl5"
    :hook (cperl-mode-local-vars . spacemacs//perl5-setup-backend)
    :custom
    (cperl-highlight-variables-indiscriminately
     t "highlight all scalar variables not just the instantiation")
    (cperl-indent-level 4 "4 spaces is the standard indentation")
    (cperl-close-paren-offset -4 "indent the closing paren back four spaces")
    (cperl-continued-statement-offset
     4 "if a statement continues indent it to four spaces")
    (cperl-indent-parens-as-block
     t "parentheses are indented with the block and not with scope")
    :config
    (progn
      ;; tab key will ident all marked code when tab key is pressed
      (add-hook 'cperl-mode-hook
                (lambda () (local-set-key (kbd "<tab>") 'indent-for-tab-command)))

      (spacemacs//perl5-setup-binding
       ;; general bindings
       '("hh" 'cperl-perldoc-at-point
         "==" 'spacemacs/perltidy-format
         "=b" 'spacemacs/perltidy-format-buffer
         "=f" 'spacemacs/perltidy-format-function
         "hd" 'cperl-perldoc
         "v" 'cperl-select-this-pod-or-here-doc)
        ;; non-lsp prefix
        '("m=" "format"
          "mg" "find-symbol"
          "mh" "perldoc"))

      (font-lock-add-keywords 'cperl-mode
                              '(("\\_<say\\_>" . cperl-nonoverridable-face))))))

(defun perl5/post-init-flycheck ()
  (spacemacs/enable-flycheck 'cperl-mode))

(defun perl5/post-init-realgud ()
  (spacemacs/add-realgud-debugger 'cperl-mode "trepan.pl"))

(defun perl5/post-init-smartparens ()
  ;; fixs a bug with electric mode and smartparens https://github.com/syl20bnr/spacemacs/issues/480
  (with-eval-after-load 'cperl-mode
    (add-hook 'smartparens-enabled-hook 'spacemacs//perl5-smartparens-enable)
    (add-hook 'smartparens-disabled-hook 'spacemacs//perl5-spartparens-disable)))

(defun perl5/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(perl . t))))
