;;; core-spacemacs-buffer-ftest.el --- Spacemacs Unit Test File
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
(require 'core-spacemacs-buffer)

(setq-default fill-column 80
	      spacemacs-buffer--window-width 75)

;; ---------------------------------------------------------------------------
;; spacemacs-buffer//notes-render-framed-text
;; ---------------------------------------------------------------------------

(defvar test-text
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.")

(ert-deftest test-render-framed-text--msg-width-caption-and-padding ()
  (should (equal (spacemacs-buffer//notes-render-framed-text test-text "Caption" "Botcaption" 4 32 32)
		 "╭─ Caption ────────────────────╮
│                              │
│    Lorem ipsum dolor sit     │
│    amet, consectetur         │
│    adipiscing elit, sed      │
│    do eiusmod tempor         │
│    incididunt ut labore      │
│    et dolore magna           │
│    aliqua.                   │
│                              │
╰─ Botcaption ─────────────────╯
"
                 )))

(ert-deftest test-render-framed-text--msg-width-caption-no-padding ()
  (should (equal (spacemacs-buffer//notes-render-framed-text test-text "Caption" "Botcaption" nil 32 32)
		 "╭─ Caption ────────────────────╮
│                              │
│ Lorem ipsum dolor sit amet,  │
│ consectetur adipiscing elit, │
│ sed do eiusmod tempor        │
│ incididunt ut labore et      │
│ dolore magna aliqua.         │
│                              │
╰─ Botcaption ─────────────────╯
"
                 )))

(ert-deftest test-render-framed-text--msg-width-no-caption-no-padding ()
  (should (equal (spacemacs-buffer//notes-render-framed-text test-text nil nil nil 32 32)
                 "╭──────────────────────────────╮
│                              │
│ Lorem ipsum dolor sit amet,  │
│ consectetur adipiscing elit, │
│ sed do eiusmod tempor        │
│ incididunt ut labore et      │
│ dolore magna aliqua.         │
│                              │
╰──────────────────────────────╯"
                 )))

(ert-deftest test-render-framed-text--msg-no-width-no-caption-no-padding ()
  (should (equal (spacemacs-buffer//notes-render-framed-text test-text)
		 "╭─────────────────────────────────────────────────────────────────────────╮
│                                                                         │
│ Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod │
│ tempor incididunt ut labore et dolore magna aliqua.                     │
│                                                                         │
╰─────────────────────────────────────────────────────────────────────────╯"
                 )))

(ert-deftest test-render-framed-text--msg-short-text ()
  (should (equal (spacemacs-buffer//notes-render-framed-text "Short content.")
		 "╭────────────────╮
│                │
│ Short content. │
│                │
╰────────────────╯"
                 )))

(ert-deftest test-render-framed-text--msg-several-paragraphs ()
  (should (equal (spacemacs-buffer//notes-render-framed-text
                  (concat "\n"
                          test-text "\n\n\n"
                          test-text "\n\n"
			  test-text "\n"))
		 "╭─────────────────────────────────────────────────────────────────────────╮
│                                                                         │
│                                                                         │
│ Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod │
│ tempor incididunt ut labore et dolore magna aliqua.                     │
│                                                                         │
│                                                                         │
│ Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod │
│ tempor incididunt ut labore et dolore magna aliqua.                     │
│                                                                         │
│ Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod │
│ tempor incididunt ut labore et dolore magna aliqua.                     │
│                                                                         │
│                                                                         │
╰─────────────────────────────────────────────────────────────────────────╯"
                 )))

(ert-deftest test-render-framed-text--file-caption-and-padding ()
  (should (equal (spacemacs-buffer//notes-render-framed-text
                  (concat spacemacs-test-directory "core/data/framed-text.txt")
		  "Caption" "Botcaption" 4 62 62)
		 "╭─ Caption ──────────────────────────────────────────────────╮
│                                                            │
│    Lorem ipsum dolor sit amet, consectetur adipiscing      │
│    elit, sed do eiusmod tempor incididunt ut labore et     │
│    dolore magna aliqua. Ut enim ad minim veniam, quis      │
│    nostrud exercitation ullamco laboris nisi ut aliquip    │
│    ex ea commodo consequat. Duis aute irure dolor in       │
│    reprehenderit in voluptate velit esse cillum dolore     │
│    eu fugiat nulla pariatur. Excepteur sint occaecat       │
│    cupidatat non proident, sunt in culpa qui officia       │
│    deserunt mollit anim id est laborum.                    │
│                                                            │
╰─ Botcaption ───────────────────────────────────────────────╯
"
                 )))
