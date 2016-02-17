;;; config.el --- Spacemacs Base Layer configuration File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; ---------------------------------------------------------------------------
;; Prefixes
;; ---------------------------------------------------------------------------

;; We define prefix commands only for the sake of which-key
(setq spacemacs/key-binding-prefixes '(("a"   "applications")
                                       ("ai"  "irc")
                                       ("as"  "shells")
                                       ("b"   "buffers")
                                       ("bm"  "move")
                                       ("c"   "compile/comments")
                                       ("C"   "capture/colors")
                                       ("e"   "errors")
                                       ("f"   "files")
                                       ("fC"  "files/convert")
                                       ("fe"  "emacs(spacemacs)")
                                       ("fv"  "variables")
                                       ("g"   "git/versions-control")
                                       ("h"   "helm/help/highlight")
                                       ("hd"  "help-describe")
                                       ("i"   "insertion")
                                       ("j"   "join/split")
                                       ("k"   "lisp")
                                       ("kd"  "delete")
                                       ("kD"  "delete-backward")
                                       ("k`"  "hybrid")
                                       ("n"   "narrow/numbers")
                                       ("p"   "projects")
                                       ("p$"  "projects/shell")
                                       ("q"   "quit")
                                       ("r"   "registers/rings")
                                       ("Re"  "elisp")
                                       ("Rp"  "pcre")
                                       ("s"   "search/symbol")
                                       ("sa"  "ag")
                                       ("sg"  "grep")
                                       ("sk"  "ack")
                                       ("st"  "pt")
                                       ("sw"  "web")
                                       ("t"   "toggles")
                                       ("tC"  "colors")
                                       ("tE"  "editing-styles")
                                       ("th"  "highlight")
                                       ("tm"  "modeline")
                                       ("T"   "UI toggles/themes")
                                       ("C-t" "other toggles")
                                       ("w"   "windows")
                                       ("wp"  "popup")
                                       ("x"   "text")
                                       ("xa"  "align")
                                       ("xd"  "delete")
                                       ("xg"  "google-translate")
                                       ("xl"  "lines")
                                       ("xm"  "move")
                                       ("xt"  "transpose")
                                       ("xw"  "words")
                                       ("z"   "zoom")))
(mapc (lambda (x) (apply #'spacemacs/declare-prefix x))
      spacemacs/key-binding-prefixes)

;; ---------------------------------------------------------------------------
;; Navigation
;; ---------------------------------------------------------------------------

;; Auto refresh
(global-auto-revert-mode 1)
;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

;; Make dired "guess" target directory for some operations, like copy to
;; directory visited in other split buffer.
(setq dired-dwim-target t)

;; Regexp for useful and useless buffers for smarter buffer switching
(defvar spacemacs-useless-buffers-regexp '("*\.\+")
  "Regexp used to determine if a buffer is not useful.")
(defvar spacemacs-useful-buffers-regexp '("\\*\\(scratch\\|terminal\.\+\\|ansi-term\\|eshell\\)\\*")
  "Regexp used to define buffers that are useful despite matching
`spacemacs-useless-buffers-regexp'.")

;; no beep pleeeeeease ! (and no visual blinking too please)
(setq ring-bell-function 'ignore
      visible-bell nil)

;; Hack to fix a bug with tabulated-list.el
;; see: http://redd.it/2dgy52
(defun tabulated-list-revert (&rest ignored)
  "The `revert-buffer-function' for `tabulated-list-mode'.
It runs `tabulated-list-revert-hook', then calls `tabulated-list-print'."
  (interactive)
  (unless (derived-mode-p 'tabulated-list-mode)
    (error "The current buffer is not in Tabulated List mode"))
  (run-hooks 'tabulated-list-revert-hook)
  ;; hack is here
  ;; (tabulated-list-print t)
  (tabulated-list-print))

;; Mouse cursor in terminal mode
(xterm-mouse-mode 1)

;; Highlight and allow to open http link at point in programming buffers
;; goto-address-prog-mode only highlights links in strings and comments
(add-hook 'prog-mode-hook 'goto-address-prog-mode)
;; Highlight and follow bug references in comments and strings
(add-hook 'prog-mode-hook 'bug-reference-prog-mode)

;; Keep focus while navigating help buffers
(setq help-window-select 't)

;; Scroll compilation to first error or end
(setq compilation-scroll-output 'first-error)

;; ---------------------------------------------------------------------------
;; Edit
;; ---------------------------------------------------------------------------

;; start scratch in text mode (usefull to get a faster Emacs load time
;; because it avoids autoloads of elisp modes)
(setq initial-major-mode 'text-mode)

;; use only spaces and no tabs
(setq-default indent-tabs-mode nil
              tab-width 2)

;; Text
(setq longlines-show-hard-newlines t)

;; Use system trash for file deletion
;; should work on Windows and Linux distros
;; on OS X, see contrib/osx layer
(setq delete-by-moving-to-trash t)

;; auto fill breaks line beyond buffer's fill-column
(setq-default fill-column 80)
(spacemacs|diminish auto-fill-function " Ⓕ" " F")

;; persistent abbreviation file
(setq abbrev-file-name (concat spacemacs-cache-directory "abbrev_defs"))

;; Save clipboard contents into kill-ring before replace them
(setq save-interprogram-paste-before-kill t)

;; Single space between sentences is more widespread than double
(setq-default sentence-end-double-space nil)

;; The C-d rebinding that most shell-like buffers inherit from
;; comint-mode assumes non-evil configuration with its
;; `comint-delchar-or-maybe-eof' function, so we disable it
(with-eval-after-load 'comint
  (define-key comint-mode-map (kbd "C-d") nil))

;; whitespace-cleanup configuration
(pcase dotspacemacs-whitespace-cleanup
  (`all (add-hook 'before-save-hook 'whitespace-cleanup))
  (`trailing (add-hook 'before-save-hook 'delete-trailing-whitespace)))

;; Thanks to `editorconfig-emacs' for many of these
(defvar spacemacs--indent-variable-alist
  '(((awk-mode c-mode c++-mode java-mode groovy-mode
      idl-mode java-mode objc-mode pike-mode) . c-basic-offset)
    (python-mode . python-indent-offset)
    (cmake-mode . cmake-tab-width)
    (coffee-mode . coffee-tab-width)
    (cperl-mode . cperl-indent-level)
    (css-mode . css-indent-offset)
    (elixir-mode . elixir-smie-indent-basic)
    ((emacs-lisp-mode lisp-mode) . lisp-indent-offset)
    (enh-ruby-mode . enh-ruby-indent-level)
    (erlang-mode . erlang-indent-level)
    ((js-mode json-mode) . js-indent-level)
    (js2-mode . js2-basic-offset)
    (js3-mode . js3-indent-level)
    (latex-mode . (LaTeX-indent-level tex-indent-basic))
    (livescript-mode . livescript-tab-width)
    (mustache-mode . mustache-basic-offset)
    (nxml-mode . nxml-child-indent)
    (perl-mode . perl-indent-level)
    (puppet-mode . puppet-indent-level)
    (ruby-mode . ruby-indent-level)
    (rust-mode . rust-indent-offset)
    (scala-mode . scala-indent:step)
    (sgml-mode . sgml-basic-offset)
    (sh-mode . sh-basic-offset)
    (web-mode . web-mode-markup-indent-offset)
    (yaml-mode . yaml-indent-offset))
  "An alist where each key is either a symbol corresponding
to a major mode, a list of such symbols, or the symbol t,
acting as default. The values are either integers, symbols
or lists of these.")

;; ---------------------------------------------------------------------------
;; UI
;; ---------------------------------------------------------------------------

(defface face-of-god
  `((t (:background "SkyBlue2"
                    :foreground ,(face-background 'mode-line)
                    :box ,(face-attribute 'mode-line :box)
                    :inherit 'mode-line)))
  "Face to use when `evil-mode' is disabled or `evil-state' is
nil."
  :group 'spacemacs)

;; important for golden-ratio to better work
(setq window-combination-resize t)
;; fringes
(setq-default fringe-indicator-alist
              '((truncation . nil) (continuation . nil)))
;; Show column number in mode line
(setq column-number-mode t)
;; Activate linum-mode in all prog-mode and text-mode buffers if the setting is
;; enabled.
(when dotspacemacs-line-numbers
  (add-hook 'prog-mode-hook 'linum-mode)
  (add-hook 'text-mode-hook 'linum-mode))
;; line number
(setq linum-format "%4d")
;; highlight current line
(global-hl-line-mode t)
;; no blink
(blink-cursor-mode 0)
;; When emacs asks for "yes" or "no", let "y" or "n" suffice
(fset 'yes-or-no-p 'y-or-n-p)
;; draw underline lower
(setq x-underline-at-descent-line t)
;; don't let the cursor go into minibuffer prompt
;; Tip taken from Xah Lee: http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
;; Emacs 24.4 new features
(unless (version< emacs-version "24.4")
  (if dotspacemacs-fullscreen-at-startup
      ;; spacemacs/toggle-fullscreen-frame-on is NOT available during the startup,
      ;; but IS available during the subsequent config reloads
      (if (fboundp 'spacemacs/toggle-fullscreen-frame-on)
          (spacemacs/toggle-fullscreen-frame-on)
        (spacemacs/toggle-frame-fullscreen))
    (if dotspacemacs-maximized-at-startup
        (add-hook 'window-setup-hook 'toggle-frame-maximized))))

;; ---------------------------------------------------------------------------
;; Session
;; ---------------------------------------------------------------------------

;; save custom variables in ~/.spacemacs
(unless (bound-and-true-p custom-file)
  (setq custom-file (dotspacemacs/location)))
;; scratch buffer empty
(setq initial-scratch-message nil)
;; don't create backup~ files
(setq make-backup-files nil)

;; Auto-save file
(setq auto-save-default (not (null dotspacemacs-auto-save-file-location)))
(setq auto-save-list-file-prefix (concat spacemacs-auto-save-directory))
;; always save TRAMP URLs to cache directory no matter what is the value
;; of `dotspacemacs-auto-save-file-location'
(let ((autosave-dir (concat spacemacs-auto-save-directory "dist/")))
  (setq auto-save-file-name-transforms
        `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" ,autosave-dir  t)))
  (unless (or (file-exists-p autosave-dir)
              (null dotspacemacs-auto-save-file-location))
    (make-directory autosave-dir t)))
;; Choose auto-save location
(cl-case dotspacemacs-auto-save-file-location
  (cache (let ((autosave-dir (concat spacemacs-auto-save-directory "site/")))
           (add-to-list 'auto-save-file-name-transforms
                        `(".*" ,autosave-dir t) 'append)
           (unless (file-exists-p autosave-dir)
             (make-directory autosave-dir t))))
  (original (setq auto-save-visited-file-name t))
  (_ (setq auto-save-default nil
           auto-save-list-file-prefix nil)))

;; remove annoying ellipsis when printing sexp in message buffer
(setq eval-expression-print-length nil
      eval-expression-print-level nil)

;; cache files
(setq tramp-persistency-file-name (concat spacemacs-cache-directory "tramp/"))

;; seems pointless to warn. There's always undo.
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
;; remove prompt if the file is opened in other clients
(defun server-remove-kill-buffer-hook ()
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))
(add-hook 'server-visit-hook 'server-remove-kill-buffer-hook)
