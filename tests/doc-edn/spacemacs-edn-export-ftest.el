;;; spacemacs-edn-export-ftest.el --- Spacemacs EDN Export Functional Test File
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(require 'mocker)

;; ------------------------------------------------------------------------
;; Spacemacs Documentation EDN Export Test
;; Currently checks whether all org documentation files can be converted to
;; Spacemacs-EDN (intermediate documentation format)
;; ------------------------------------------------------------------------
(ert-deftest test-spacemacs-edn-export ()
  (when (version<= "25.0.0" emacs-version)
    (unwind-protect
        (progn
          (message "=======================================================")
          (message "Testing Spacemacs-EDN export+")
          (message "=======================================================")
          (require 'core-documentation-edn)
          (spacemacs/publish-docs-to-edn-concurrently)
          (message "=======================================================")
          (message "Testing Spacemacs-EDN export-")
          (message "======================================================="))
      (let ((export-dir (concat spacemacs-start-directory "export/")))
        (if (file-accessible-directory-p export-dir)
            (delete-directory export-dir t)
          (error "\"%s\" doesn't exist. We haven't exported anything :("
                 export-dir))))))
