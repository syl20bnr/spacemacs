;;; core-configuration-layer-ftest.el --- Spacemacs Functional Test File
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


(require 'core-configuration-layer)

;; ---------------------------------------------------------------------------
;; configuration-layer//declare-used-layers
;; ---------------------------------------------------------------------------

(ert-deftest test-declare-layers--bootstrap-layer-always-first ()
  (let ((dotspacemacs-distribution 'spacemacs)
        (dotspacemacs-configuration-layers '(emacs-lisp
                                             (git :variables foo 'bar)))
        configuration-layer--used-layers
        (configuration-layer--indexed-layers (make-hash-table :size 1024)))
    (configuration-layer/discover-layers 'refresh-index)
    (configuration-layer//declare-used-layers dotspacemacs-configuration-layers)
    (should (eq 'spacemacs-bootstrap
                (car configuration-layer--used-layers)))))

(ert-deftest test-declare-layers--defaults-layer-is-second-for-base-distribution ()
  (let ((dotspacemacs-distribution 'spacemacs-base)
        (dotspacemacs-configuration-layers '(emacs-lisp
                                             (git :variables foo 'bar)))
        configuration-layer--used-layers
        (configuration-layer--indexed-layers (make-hash-table :size 1024)))
    (configuration-layer/discover-layers 'refresh-index)
    (configuration-layer//declare-used-layers dotspacemacs-configuration-layers)
    (should (eq 'spacemacs-defaults (cadr configuration-layer--used-layers)))))

(ert-deftest test-declare-layers--base-layer-is-third-for-base-distribution ()
  (let ((dotspacemacs-distribution 'spacemacs-base)
        (dotspacemacs-configuration-layers '(emacs-lisp
                                             (git :variables foo 'bar)))
        configuration-layer--used-layers
        (configuration-layer--indexed-layers (make-hash-table :size 1024)))
    (configuration-layer/discover-layers 'refresh-index)
    (configuration-layer//declare-used-layers dotspacemacs-configuration-layers)
    (should (eq 'spacemacs-base (caddr configuration-layer--used-layers)))))

;; ---------------------------------------------------------------------------
;; configuration-layer//stable-elpa-verify-archive
;; ---------------------------------------------------------------------------

;; FIXME: Always fail. >_> @syl20bnr
;; (ert-deftest test-stable-elpa-verify-archive--verification-ok ()
;;   (cl-letf (((symbol-function 'configuration-layer//stable-elpa-tarball-local-file)
;;              (lambda ()
;;                (concat spacemacs-test-directory
;;                        "core/data/signed-test-stable-elpa.tar.gz")))
;;             ((symbol-function 'configuration-layer//stable-elpa-tarball-local-sign-file)
;;              (lambda ()
;;                (concat spacemacs-test-directory
;;                        "core/data/signed-test-stable-elpa.tar.gz.sig")))
;;             ((symbol-function 'configuration-layer//stable-elpa-ask-to-continue)
;;              (lambda (x)
;;                (message "Verification Error: %s" x)
;;                nil))
;;             ((symbol-function 'configuration-layer//error)
;;              (lambda (x)
;;                (message "Fatal Error: %s" x)
;;                nil))
;;             ((symbol-function 'message) 'ignore))
;;     (should (equal t (configuration-layer//stable-elpa-verify-archive)))))

(ert-deftest test-stable-elpa-verify-archive--verification-failed ()
  (let (verification-error)
    (cl-letf (((symbol-function 'configuration-layer//stable-elpa-tarball-local-file)
               (lambda ()
                 (concat spacemacs-test-directory
                         "core/data/test-stable-elpa.tar.gz")))
              ((symbol-function 'configuration-layer//stable-elpa-tarball-local-sign-file)
               (lambda ()
                 (concat spacemacs-test-directory
                         "core/data/signed-test-stable-elpa.tar.gz.sig")))
              ((symbol-function 'configuration-layer//stable-elpa-ask-to-continue)
               (lambda (x)
                 (setq verification-error x)
                 nil))
              ((symbol-function 'configuration-layer//error)
               (lambda (x)
                 (message "Fatal Error: %s" x)
                 nil))
              ((symbol-function 'message) 'ignore))
      (should (and (null (configuration-layer//stable-elpa-verify-archive))
                   (string-match-p "^Verification failed!.*"
                                   verification-error))))))
