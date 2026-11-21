;;; core-dotspacemacs-utest.el --- Tests for core-dotspacemacs.el  -*- lexical-binding: t; -*-

;; Copyright (c) 2012-2026 Sylvain Benner & Contributors
;;
;; Author: Aaron L Zeng <me@bcc32.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(require 'cl-lib)
(require 'core-dotspacemacs)

(ert-deftest spacemacs/title-prepare-001 ()
  "Basic functional tests for `spacemacs/title-prepare'."
  ;; `spacemacs/title-prepare' is used with `:eval' in a `frame-title-format';
  ;; thus, its output will be interpreted by `format-mode-line'.  However,
  ;; `format-mode-line' always returns an empty string in batch Emacs, so any
  ;; assertions which use that function are vacuous unless this test is run
  ;; interactively.

  ;; Built-in format specifiers (not exhaustive)

  (should (equal (spacemacs/title-prepare "%b") "%b"))
  (should (equal (spacemacs/title-prepare "%F") "%F"))

  ;; Spacemacs-specific format specifiers

  ;; %a: abbreviated file name, or buffer name
  (with-temp-buffer
    (should (equal (spacemacs/title-prepare "%a") (buffer-name))))
  (with-temp-buffer
    (setq buffer-file-name "/tmp/foo")
    (should (equal (spacemacs/title-prepare "%a")
                   (abbreviate-file-name buffer-file-name))))
  (with-temp-buffer
    (setq buffer-file-name (expand-file-name "~/foo"))
    (should (equal (spacemacs/title-prepare "%a")
                   "~/foo")))

  ;; %t: `projectile-project-name'; not tested to avoid a dependency on
  ;; projectile.el

  ;; %I: `invocation-name'
  (should (equal (spacemacs/title-prepare "%I") invocation-name))
  ;; %S: `system-name'
  (should (equal (spacemacs/title-prepare "%S") system-name))
  ;; %U: Username
  (should (equal (spacemacs/title-prepare "%U") user-login-name)))

(ert-deftest spacemacs/title-prepare-percent-in-filename ()
  "Regression test for file names containing \"%\" characters."
  ;; FIXME: This test is currently broken because percent characters substituted
  ;; by `spacemacs/title-prepare' are not escaped, and then are subsequently
  ;; mangled by `format-mode-line'.
  :expected-result :failed
  (let ((buffer-names '(" *temp%*" " *temp%%*" " *temp%%%*" " *temp%%%%*"
                        " *temp%f*" " *temp%f%*" " *temp%f%%*" " *temp%f%%%*")))
    (cl-flet ((test-title-in-temp-buffer (buffer-name)
                (with-temp-buffer
                  (rename-buffer buffer-name)
                  ;; `spacemacs/title-prepare' is used with `:eval' in a
                  ;; `frame-title-format'; thus, its output will be interpreted
                  ;; by `format-mode-line'.  However, `format-mode-line' always
                  ;; returns an empty string in batch Emacs, so don't call it
                  ;; here.  Instead, we check the result by ensuring that it
                  ;; matches how you would escape a string to be rendered
                  ;; verbatim.
                  (spacemacs/title-prepare "%a"))))
      (dolist (buffer-name buffer-names)
        (should (equal (test-title-in-temp-buffer buffer-name)
                       (replace-regexp-in-string "%" "%%"
                                                 buffer-name)))))))

(provide 'core-dotspacemacs-utest)
;;; core-dotspacemacs-utest.el ends here
