;;; smartparens-config.el --- Default configuration for smartparens package  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2020, 2022-2023 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Created: 30 Jan 2013
;; Keywords: abbrev convenience editing
;; URL: https://github.com/Fuco1/smartparens

;; This file is not part of GNU Emacs.

;;; License:

;; This file is part of Smartparens.

;; Smartparens is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Smartparens is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Smartparens.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a default configuration for smartparens package.  If you
;; wish to set up everything by yourself, you can instead require
;; smartparens directly.

;; However, some configuration is always loaded by default, most
;; notably the built-in list of supported pairs.  If you want to erase
;; this list, simply use (setq sp-pairs nil) and then add your own
;; pairs.

;; For more info, see github readme at
;; https://github.com/Fuco1/smartparens

;;; Code:

(require 'smartparens)

(defun sp-lisp-invalid-hyperlink-p (_id action _context)
  "Test if there is an invalid hyperlink in a Lisp docstring.
ID, ACTION, CONTEXT."
  (when (eq action 'navigate)
    ;; Ignore errors due to us being at the start or end of the
    ;; buffer.
    (ignore-errors
      (or
       ;; foo'|bar
       (and (looking-at "\\sw\\|\\s_")
            ;; do not consider punctuation
            (not (looking-at "[?.,;!]"))
            (save-excursion
              (backward-char 2)
              (looking-at "\\sw\\|\\s_")))
       ;; foo|'bar
       (and (save-excursion
              (backward-char 1)
              (looking-at "\\sw\\|\\s_"))
            (save-excursion
              (forward-char 1)
              (looking-at "\\sw\\|\\s_")
              ;; do not consider punctuation
              (not (looking-at "[?.,;!]"))))))))

;; emacs is lisp hacking environment, so we set up some most common
;; lisp modes too
(sp-with-modes sp-lisp-modes
  ;; disable ', it's the quote character!
  (sp-local-pair "'" nil :actions nil))

(eval-after-load 'org
  '(progn
     (defun sp-lisp-in-lisp-src-block-p (_id _action _context)
       (when (org-in-src-block-p)
         (let* ((el (org-element-at-point))
                (lang (org-element-property :language el))
                (mode (intern (concat
                               (if (string= lang "elisp") "emacs-lisp" lang)
                               "-mode"))))
           (memq mode sp-lisp-modes))))

     ;; Disable ' pairing in lisp org source blocks
     (sp-local-pair 'org-mode "'" "'"
                    :unless '(:add sp-lisp-in-lisp-src-block-p))))

(sp-with-modes (-difference sp-lisp-modes sp-clojure-modes)
  ;; also only use the pseudo-quote inside strings where it serve as
  ;; hyperlink.
  (sp-local-pair "`" "'"
                 :when '(sp-in-string-p
                         sp-in-comment-p)
                 :unless '(sp-lisp-invalid-hyperlink-p)
                 :skip-match (lambda (ms _mb _me)
                               (cond
                                ((equal ms "'")
                                 (or (sp-lisp-invalid-hyperlink-p "`" 'navigate '_)
                                     (not (sp-point-in-string-or-comment))))
                                (t (not (sp-point-in-string-or-comment)))))))

;; TODO: this should only be active in docstring, otherwise we want
;; the regexp completion \\{\\}.  To handle this feature, we must
;; allow multiple pairs on same opening (therefore, the unique ID must
;; become the opening and closing pair)
(sp-local-pair 'emacs-lisp-mode "\\\\{" "}" :when '(sp-in-docstring-p))

;; NOTE: Normally, `sp-local-pair' accepts list of modes (or a single
;; mode) as a first argument.  The macro `sp-with-modes' adds this
;; automatically.  If you want to call sp-local-pair outside this
;; macro, you MUST supply the major mode argument.

(eval-after-load 'cc-mode                  '(require 'smartparens-c))
(eval-after-load 'clojure-mode             '(require 'smartparens-clojure))
(eval-after-load 'crystal-mode             '(require 'smartparens-crystal))
(eval-after-load 'elixir-mode              '(require 'smartparens-elixir))
(eval-after-load 'elixir-ts-mode           '(require 'smartparens-elixir))
(eval-after-load 'enh-ruby-mode            '(require 'smartparens-ruby))
(eval-after-load 'erlang-mode              '(require 'smartparens-erlang))
(eval-after-load 'ess                      '(require 'smartparens-ess))
(eval-after-load 'go-mode                  '(require 'smartparens-go))
(eval-after-load 'haskell-interactive-mode '(require 'smartparens-haskell))
(eval-after-load 'haskell-mode             '(require 'smartparens-haskell))
(--each sp--html-modes
  (eval-after-load it                      '(require 'smartparens-html)))
(eval-after-load 'latex                    '(require 'smartparens-latex))
(eval-after-load 'lua-mode                 '(require 'smartparens-lua))
(eval-after-load 'markdown-mode            '(require 'smartparens-markdown))
(--each '(python-mode python-ts-mode python)
  (eval-after-load it                      '(require 'smartparens-python)))
(eval-after-load 'org                      '(require 'smartparens-org))
(eval-after-load 'racket-mode              '(require 'smartparens-racket))
(eval-after-load 'rst                      '(require 'smartparens-rst))
(eval-after-load 'ruby-mode                '(require 'smartparens-ruby))
(eval-after-load 'rust-mode                '(require 'smartparens-rust))
(eval-after-load 'rust-ts-mode             '(require 'smartparens-rust))
(eval-after-load 'rustic                   '(require 'smartparens-rust))
(eval-after-load 'scala-mode               '(require 'smartparens-scala))
(eval-after-load 'swift-mode               '(require 'smartparens-swift))
(eval-after-load 'tex-mode                 '(require 'smartparens-latex))
(eval-after-load 'text-mode                '(require 'smartparens-text))
(eval-after-load 'tuareg                   '(require 'smartparens-ml))
(eval-after-load 'fsharp-mode              '(require 'smartparens-ml))
(eval-after-load 'unisonlang-mode          '(require 'smartparens-unison))
(--each '(js js2-mode typescript-ts-mode)
  (eval-after-load it                      '(require 'smartparens-javascript)))
(provide 'smartparens-config)

;;; smartparens-config.el ends here
