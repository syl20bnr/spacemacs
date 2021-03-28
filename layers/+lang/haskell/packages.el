;;; packages.el --- Haskell Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
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

(defconst haskell-packages
  '(
    ;; auto-completion
    company
    (company-cabal :requires company)

    ;; syntax-checking
    flycheck
    (flycheck-haskell :requires flycheck)

    ;; gtags
    ggtags
    counsel-gtags
    helm-gtags

    ;; yassnippet
    yasnippet
    (haskell-snippets :requires yasnippet)

    ;; dante completion backend
    (dante :toggle (eq haskell-completion-backend 'dante)
           :requires (company flycheck))
    ;; dante auto refactor companion
    (attrap :requires dante)

    ;; lsp backend
    (lsp-haskell :toggle (eq haskell-completion-backend 'lsp)
                 :requries lsp)

    ;; formatter
    format-all

    haskell-mode
    (helm-hoogle :requires helm)
    (cmm-mode :toggle haskell-enable-cmm-mode)
    (hlint-refactor :toggle (and haskell-enable-hlint
                                 ;; use LSP's builtin hlint support instead
                                 (eq haskell-completion-backend 'dante)))))


;; company

(defun haskell/post-init-company ()
  (spacemacs/add-to-hooks 'spacemacs-haskell//setup-company
                          '(haskell-mode-local-vars-hook haskell-literate-mode-local-vars-hook)))

(defun haskell/init-company-cabal ()
  (use-package company-cabal
    :defer t
    :init
    (spacemacs|add-company-backends
      :backends company-cabal
      :modes haskell-cabal-mode)))


;; flycheck

(defun haskell/post-init-flycheck ()
  (when (and (eq haskell-completion-backend 'dante)
             haskell-enable-hlint)
    (add-hook 'dante-mode-hook
         (lambda () (flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint)))))
  (spacemacs/enable-flycheck 'haskell-mode))

(defun haskell/init-flycheck-haskell ()
  (use-package flycheck-haskell
    :hook (haskell-mode . flycheck-haskell-setup)))


;; gtags

(defun haskell/post-init-ggtags ()
  (spacemacs/add-to-hooks 'spacemacs/ggtags-mode-enable
                          '(haskell-mode-local-vars-hook haskell-literate-mode-local-vars-hook)))

(defun haskell/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'haskell-mode)
  (spacemacs/counsel-gtags-define-keys-for-mode 'haskell-literate-mode))

(defun haskell/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'haskell-mode)
  (spacemacs/helm-gtags-define-keys-for-mode 'haskell-literate-mode))


;; yasnippet

(defun haskell/post-init-yasnippet ()
  (spacemacs/add-to-hooks 'spacemacs/load-yasnippet
                          '(haskell-mode-hook haskell-literate-mode-hook)))

(defun haskell/init-haskell-snippets ()
  (use-package haskell-snippets
    :defer t
    :hook (yas-minor-mode . haskell-snippets-initialize)))


;; dante backend

(defun haskell/init-dante ()
  (use-package dante
    :defer t
    :config
    (dolist (mode spacemacs--haskell-modes)
      (spacemacs/declare-prefix-for-mode mode "md" "haskell/debug")
      (spacemacs/declare-prefix-for-mode mode "mr" "haskell/refactor")
      (spacemacs/declare-prefix-for-mode mode "mh" "haskell/documentation")
      (spacemacs/set-leader-keys-for-major-mode mode
        "gb" 'xref-pop-marker-stack
        ;; FIXME: the following three collides with haskell-mode
        "ht" 'dante-type-at
        "hT" 'spacemacs-haskell/dante-insert-type
        "hi" 'dante-info
        "rs" 'dante-auto-fix
        "se" 'dante-eval-block
        "sr" 'dante-restart))))

(defun haskell/init-attrap ()
  (use-package attrap
    :defer t))


;; lsp backend

(defun haskell/init-lsp-haskell ()
  (use-package lsp-haskell
    :defer t
    :custom
    (lsp-haskell-hlint-on haskell-enable-hlint)))


;; formatter

(defun haskell/init-format-all ()
  (use-package format-all
    :defer t
    :after haskell-mode
    :config
    (progn
      ;; depending on the formatter, it may or may not support `haskell-literate-mode'
      (when-let ((modes (pcase haskell-formatter-backend
                          ('brittany '(haskell-mode literate-haskell-mode))
                          ('hindent '(haskell-mode literate-haskell-mode))
                          ('ormolu '(haskell-mode))
                          ('stylish-haskell '(haskell-mode)))))
       (spacemacs/add-to-hooks 'format-all-mode modes
        (dolist (mode modes)
          (spacemacs/set-leader-keys-for-major-mode mode
            "==" 'format-all-buffer
            "T=" 'format-all-mode))))
      ;; also set lsp formattign provider
      ;; NOTE: except of 'hindent which is not currently supported (April 2021)
      (when (and (eq haskell-completion-backend 'lsp)
                 (memq haskell-formatter-backend
                       '(brittany ormolu stylish-haskell fourmolu floskell)))
        (setq lsp-haskell-formatting-provider (symbol-name haskell-formatter-backend))))))


;; haskell-mode

(defun haskell/init-haskell-mode ()
  (use-package haskell-mode
    :defer t
    :hook
    (((haskell-mode-local-vars haskell-literate-mode-local-vars) . spacemacs-haskell//setup-backend)
     (haskell-cabal-mode . spacemacs//force-haskell-mode-loading)
     ((haskell-mode haskell-cabal-mode) . spacemacs//haskell-disable-electric-indent))
    :custom
    (haskell-notify-p t "Notify with notify.el, if installed, after running Cabal commands and etc")
    (haskell-interactive-popup-errors nil "Remove annoying error popups")
    (haskell-process-suggest-remove-import-lines t "Show unused imports")
    (haskell-process-auto-import-loaded-modules t "Auto-import module")
    :config
    (progn
      ;; prefixes
      (dolist (mode spacemacs--haskell-modes)
        (unless (configuration-layer/layer-used-p 'dap)
          (spacemacs/declare-prefix-for-mode mode "md" "haskell/debug"))
        (spacemacs/declare-prefix-for-mode mode "mc" "haskell/cabal")
        (spacemacs/declare-prefix-for-mode mode "ms" "haskell/repl"))
      (spacemacs/declare-prefix-for-mode 'haskell-interactive-mode "ms" "haskell/repl")
      (spacemacs/declare-prefix-for-mode 'haskell-cabal-mode "ms" "haskell/repl")

      ;; repl
      (evil-define-key 'insert haskell-interactive-mode-map
        (kbd "C-j") 'haskell-interactive-mode-history-next
        (kbd "C-k") 'haskell-interactive-mode-history-previous
        (kbd "C-l") 'haskell-interactive-mode-clear)
      (spacemacs/register-repl 'haskell
                               'haskell-interactive-switch "haskell")

      ;; bindings
      (dolist (mode spacemacs--haskell-modes)
        (spacemacs/set-leader-keys-for-major-mode mode
          "sb"  'haskell-process-load-file
          "sc"  'haskell-interactive-mode-clear
          "sS"  'spacemacs/haskell-interactive-bring
          "ss"  'haskell-interactive-switch
          "st"  'haskell-session-change-target
          "'"   'haskell-interactive-switch

          "ca"  'haskell-process-cabal
          "cb"  'haskell-process-cabal-build
          "cc"  'haskell-compile
          "cv"  'haskell-cabal-visit-file

          "hd"  'inferior-haskell-find-haddock
          "hi"  'haskell-process-do-info
          "ht"  'haskell-process-do-type
          "hT"  'spacemacs/haskell-process-do-type-on-prev-line
          "hG"  'haskell-hoogle-lookup-from-local

          "da"  'haskell-debug/abandon
          "db"  'haskell-debug/break-on-function
          "dB"  'haskell-debug/delete
          "dc"  'haskell-debug/continue
          "dd"  'haskell-debug
          "dn"  'haskell-debug/next
          "dN"  'haskell-debug/previous
          "dp"  'haskell-debug/previous
          "dr"  'haskell-debug/refresh
          "ds"  'haskell-debug/step
          "dt"  'haskell-debug/trace

          "ri"  'spacemacs/haskell-format-imports)
        (if (eq haskell-completion-backend 'lsp)
            (spacemacs/set-leader-keys-for-major-mode mode
              "gl"  'haskell-navigate-imports
              "S"   'haskell-mode-stylish-buffer

              "hg"  'hoogle)
          (spacemacs/set-leader-keys-for-major-mode mode
            "gi"  'haskell-navigate-imports
            "F"   'haskell-mode-stylish-buffer

            "hh"  'hoogle)))

      ;; debug-mode
      (evilified-state-evilify haskell-debug-mode haskell-debug-mode-map
        "RET" 'haskell-debug/select
        "a" 'haskell-debug/abandon
        "b" 'haskell-debug/break-on-function
        "c" 'haskell-debug/continue
        "d" 'haskell-debug/delete
        "i" 'haskell-debug/step
        "s" 'haskell-debug/next
        "S" 'haskell-debug/previous
        "r" 'haskell-debug/refresh
        "t" 'haskell-debug/trace)

      ;; configure C-c C-l so it doesn't throw any errors
      (bind-key "C-c C-l" 'haskell-process-load-file haskell-mode-map)
      (bind-key "C-c C-z" 'haskell-interactive-switch haskell-mode-map)

      ;; Switch back to editor from REPL
      (spacemacs/set-leader-keys-for-major-mode 'haskell-interactive-mode
        "ss"  'haskell-interactive-switch-back)

      ;; Compile
      (spacemacs/set-leader-keys-for-major-mode 'haskell-cabal
        "C"  'haskell-compile)

      ;; Cabal-file bindings
      (spacemacs/set-leader-keys-for-major-mode 'haskell-cabal-mode
        ;; "="   'haskell-cabal-subsection-arrange-lines ;; Does a bad job, 'gg=G' works better
        "d"   'haskell-cabal-add-dependency
        "b"   'haskell-cabal-goto-benchmark-section
        "e"   'haskell-cabal-goto-executable-section
        "t"   'haskell-cabal-goto-test-suite-section
        "m"   'haskell-cabal-goto-exposed-modules
        "l"   'haskell-cabal-goto-library-section
        "n"   'haskell-cabal-next-subsection
        "p"   'haskell-cabal-previous-subsection
        "sc"  'haskell-interactive-mode-clear
        "sS"  'spacemacs/haskell-interactive-bring
        "ss"  'haskell-interactive-switch
        "N"   'haskell-cabal-next-section
        "P"   'haskell-cabal-previous-section
        "f"   'haskell-cabal-find-or-create-source-file)

      ;; Make "RET" behaviour in REPL saner
      (evil-define-key 'insert haskell-interactive-mode-map
        (kbd "RET") 'haskell-interactive-mode-return)
      (evil-define-key 'normal haskell-interactive-mode-map
        (kbd "RET") 'haskell-interactive-mode-return))

    ;; align rules for Haskell
    (with-eval-after-load 'align
      (add-to-list 'align-rules-list
                   '(haskell-types
                     (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                     (modes . spacemacs--haskell-modes)))
      (add-to-list 'align-rules-list
                   '(haskell-assignment
                     (regexp . "\\(\\s-+\\)=\\s-+")
                     (modes . spacemacs--haskell-modes)))
      (add-to-list 'align-rules-list
                   '(haskell-arrows
                     (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                     (modes . spacemacs--haskell-modes)))
      (add-to-list 'align-rules-list
                   '(haskell-left-arrows
                     (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                     (modes . spacemacs--haskell-modes))))))

(defun haskell/init-helm-hoogle ()
  (use-package helm-hoogle
    :defer t
    :init
    (dolist (mode spacemacs--haskell-modes)
      (spacemacs/set-leader-keys-for-major-mode mode "hf" 'helm-hoogle))))

(defun haskell/init-cmm-mode ()
  (use-package cmm-mode
    :defer t))

(defun haskell/init-hlint-refactor ()
  (use-package hlint-refactor
    :defer t
    :hook (haskell-mode . hlint-refactor-mode)
    :init (spacemacs/set-leader-keys-for-major-mode 'haskell-mode
            "rb" 'hlint-refactor-refactor-buffer
            "rr" 'hlint-refactor-refactor-at-point)))
