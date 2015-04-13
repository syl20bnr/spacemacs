;;; core-spacemacs-buffer-ftest.el --- Spacemacs Unit Test File
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
(require 'core-spacemacs-buffer)

(setq-default fill-column 80)

;; ---------------------------------------------------------------------------
;; spacemacs//render-framed-text
;; ---------------------------------------------------------------------------

(defvar test-text
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.")

(ert-deftest test-render-framed-text--msg-width-caption-and-padding ()
  (should (equal (spacemacs//render-framed-text test-text 32 "Caption" 4)
                 "╭─Caption────────────────────────────╮
│                                    │
│    Lorem ipsum  dolor sit amet,    │
│    consectetur adipiscing elit,    │
│    sed   do    eiusmod   tempor    │
│    incididunt   ut  labore   et    │
│    dolore magna aliqua.            │
│                                    │
╰────────────────────────────────────╯")))

(ert-deftest test-render-framed-text--msg-width-caption-no-padding ()
  (should (equal (spacemacs//render-framed-text test-text 32 "Caption")
                 "╭─Caption─────────────────────────╮
│                                 │
│ Lorem  ipsum  dolor  sit  amet, │
│ consectetur   adipiscing  elit, │
│ sed    do     eiusmod    tempor │
│ incididunt ut  labore et dolore │
│ magna aliqua.                   │
│                                 │
╰─────────────────────────────────╯")))

(ert-deftest test-render-framed-text--msg-width-no-caption-no-padding ()
  (should (equal (spacemacs//render-framed-text test-text 32)
                 "╭─────────────────────────────────╮
│                                 │
│ Lorem  ipsum  dolor  sit  amet, │
│ consectetur   adipiscing  elit, │
│ sed    do     eiusmod    tempor │
│ incididunt ut  labore et dolore │
│ magna aliqua.                   │
│                                 │
╰─────────────────────────────────╯")))

(ert-deftest test-render-framed-text--msg-no-width-no-caption-no-padding ()
  (should (equal (spacemacs//render-framed-text test-text)
                 "╭──────────────────────────────────────────────────────────────────────────────────╮
│                                                                                  │
│ Lorem ipsum dolor  sit amet, consectetur adipiscing elit, sed  do eiusmod tempor │
│ incididunt ut labore et dolore magna aliqua.                                     │
│                                                                                  │
╰──────────────────────────────────────────────────────────────────────────────────╯")))

(ert-deftest test-render-framed-text--msg-short-text ()
  (should (equal (spacemacs//render-framed-text "Short content.")
                 "╭──────────────────────────────────────────────────────────────────────────────────╮
│                                                                                  │
│ Short content.                                                                   │
│                                                                                  │
╰──────────────────────────────────────────────────────────────────────────────────╯")))

(ert-deftest test-render-framed-text--msg-several-paragraphs ()
  (should (equal (spacemacs//render-framed-text
                  (concat "\n"
                          test-text "\n\n\n"
                          test-text "\n\n"
                          test-text "\n"))
                 "╭──────────────────────────────────────────────────────────────────────────────────╮
│                                                                                  │
│                                                                                  │
│ Lorem ipsum dolor  sit amet, consectetur adipiscing elit, sed  do eiusmod tempor │
│ incididunt ut labore et dolore magna aliqua.                                     │
│                                                                                  │
│                                                                                  │
│ Lorem ipsum dolor  sit amet, consectetur adipiscing elit, sed  do eiusmod tempor │
│ incididunt ut labore et dolore magna aliqua.                                     │
│                                                                                  │
│ Lorem ipsum dolor  sit amet, consectetur adipiscing elit, sed  do eiusmod tempor │
│ incididunt ut labore et dolore magna aliqua.                                     │
│                                                                                  │
│                                                                                  │
╰──────────────────────────────────────────────────────────────────────────────────╯")))


(ert-deftest test-render-framed-text--file-caption-and-padding ()
  (should (equal (spacemacs//render-framed-text
                  (concat spacemacs-core-directory "tests/data/framed-text.txt")
                  62 "Caption" 4)
                 "╭─Caption──────────────────────────────────────────────────────────╮
│                                                                  │
│    Lorem ipsum  dolor sit amet, consectetur  adipiscing elit,    │
│    sed do eiusmod tempor incididunt ut labore et dolore magna    │
│    aliqua. Ut enim ad minim veniam, quis nostrud exercitation    │
│    ullamco laboris  nisi ut aliquip ex  ea commodo consequat.    │
│    Duis aute irure dolor  in reprehenderit in voluptate velit    │
│    esse  cillum dolore  eu fugiat  nulla pariatur.  Excepteur    │
│    sint occaecat  cupidatat non  proident, sunt in  culpa qui    │
│    officia deserunt mollit anim id est laborum.                  │
│                                                                  │
╰──────────────────────────────────────────────────────────────────╯")))
