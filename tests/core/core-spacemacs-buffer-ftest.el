;;; core-space-macs-buffer-ftest.el --- Space-macs Unit Test File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
(require 'core-space-macs-buffer)

(setq-default fill-column 80
	      space-macs-buffer--window-width 75)

;; ---------------------------------------------------------------------------
;; space-macs-buffer//notes-render-framed-text
;; ---------------------------------------------------------------------------

(defvar test-text
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.")

(ert-deftest test-render-framed-text--msg-width-caption-and-padding ()
  (should (equal (space-macs-buffer//notes-render-framed-text test-text "Caption" "Botcaption" 4 32 32)
		 "â•­â”€ Caption â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â•®
â”‚                              â”‚
â”‚    Lorem ipsum dolor sit     â”‚
â”‚    amet, consectetur         â”‚
â”‚    adipiscing elit, sed      â”‚
â”‚    do eiusmod tempor         â”‚
â”‚    incididunt ut labore      â”‚
â”‚    et dolore magna           â”‚
â”‚    aliqua.                   â”‚
â”‚                              â”‚
â•°â”€ Botcaption â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â•¯
"
                 )))

(ert-deftest test-render-framed-text--msg-width-caption-no-padding ()
  (should (equal (space-macs-buffer//notes-render-framed-text test-text "Caption" "Botcaption" nil 32 32)
		 "â•­â”€ Caption â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â•®
â”‚                              â”‚
â”‚ Lorem ipsum dolor sit amet,  â”‚
â”‚ consectetur adipiscing elit, â”‚
â”‚ sed do eiusmod tempor        â”‚
â”‚ incididunt ut labore et      â”‚
â”‚ dolore magna aliqua.         â”‚
â”‚                              â”‚
â•°â”€ Botcaption â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â•¯
"
                 )))

(ert-deftest test-render-framed-text--msg-width-no-caption-no-padding ()
  (should (equal (space-macs-buffer//notes-render-framed-text test-text nil nil nil 32 32)
                 "â•­â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â•®
â”‚                              â”‚
â”‚ Lorem ipsum dolor sit amet,  â”‚
â”‚ consectetur adipiscing elit, â”‚
â”‚ sed do eiusmod tempor        â”‚
â”‚ incididunt ut labore et      â”‚
â”‚ dolore magna aliqua.         â”‚
â”‚                              â”‚
â•°â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â•¯"
                 )))

(ert-deftest test-render-framed-text--msg-no-width-no-caption-no-padding ()
  (should (equal (space-macs-buffer//notes-render-framed-text test-text)
		 "â•­â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â•®
â”‚                                                                         â”‚
â”‚ Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod â”‚
â”‚ tempor incididunt ut labore et dolore magna aliqua.                     â”‚
â”‚                                                                         â”‚
â•°â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â•¯"
                 )))

(ert-deftest test-render-framed-text--msg-short-text ()
  (should (equal (space-macs-buffer//notes-render-framed-text "Short content.")
		 "â•­â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â•®
â”‚                â”‚
â”‚ Short content. â”‚
â”‚                â”‚
â•°â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â•¯"
                 )))

(ert-deftest test-render-framed-text--msg-several-paragraphs ()
  (should (equal (space-macs-buffer//notes-render-framed-text
                  (concat "\n"
                          test-text "\n\n\n"
                          test-text "\n\n"
			  test-text "\n"))
		 "â•­â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â•®
â”‚                                                                         â”‚
â”‚                                                                         â”‚
â”‚ Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod â”‚
â”‚ tempor incididunt ut labore et dolore magna aliqua.                     â”‚
â”‚                                                                         â”‚
â”‚                                                                         â”‚
â”‚ Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod â”‚
â”‚ tempor incididunt ut labore et dolore magna aliqua.                     â”‚
â”‚                                                                         â”‚
â”‚ Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod â”‚
â”‚ tempor incididunt ut labore et dolore magna aliqua.                     â”‚
â”‚                                                                         â”‚
â”‚                                                                         â”‚
â•°â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â•¯"
                 )))

(ert-deftest test-render-framed-text--file-caption-and-padding ()
  (should (equal (space-macs-buffer//notes-render-framed-text
                  (concat space-macs-test-directory "core/data/framed-text.txt")
		  "Caption" "Botcaption" 4 62 62)
		 "â•­â”€ Caption â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â•®
â”‚                                                            â”‚
â”‚    Lorem ipsum dolor sit amet, consectetur adipiscing      â”‚
â”‚    elit, sed do eiusmod tempor incididunt ut labore et     â”‚
â”‚    dolore magna aliqua. Ut enim ad minim veniam, quis      â”‚
â”‚    nostrud exercitation ullamco laboris nisi ut aliquip    â”‚
â”‚    ex ea commodo consequat. Duis aute irure dolor in       â”‚
â”‚    reprehenderit in voluptate velit esse cillum dolore     â”‚
â”‚    eu fugiat nulla pariatur. Excepteur sint occaecat       â”‚
â”‚    cupidatat non proident, sunt in culpa qui officia       â”‚
â”‚    deserunt mollit anim id est laborum.                    â”‚
â”‚                                                            â”‚
â•°â”€ Botcaption â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â•¯
"
                 )))


