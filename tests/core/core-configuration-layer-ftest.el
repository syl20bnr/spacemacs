;;; core-configuration-layer-ftest.el --- Space-macs Functional Test File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3
(require 'core-configuration-layer)

;; ---------------------------------------------------------------------------
;; configuration-layer//declare-used-layers
;; ---------------------------------------------------------------------------

(ert-deftest test-declare-layers--bootstrap-layer-always-first ()
  (let ((dotspace-macs-distribution 'space-macs)
        (dotspace-macs-configuration-layers '(e-macs-lisp
                                             (git :variables foo 'bar)))
        configuration-layer--used-layers
        (configuration-layer--indexed-layers (make-hash-table :size 1024)))
    (configuration-layer/discover-layers 'refresh-index)
    (configuration-layer//declare-used-layers dotspace-macs-configuration-layers)
    (should (eq 'space-macs-bootstrap
                (first configuration-layer--used-layers)))))

(ert-deftest test-declare-layers--defaults-layer-is-second-for-base-distribution ()
  (let ((dotspace-macs-distribution 'space-macs-base)
         (dotspace-macs-configuration-layers '(e-macs-lisp
                                               (git :variables foo 'bar)))
         configuration-layer--used-layers
         (configuration-layer--indexed-layers (make-hash-table :size 1024)))
    (configuration-layer/discover-layers 'refresh-index)
    (configuration-layer//declare-used-layers dotspace-macs-configuration-layers)
    (should (eq 'space-macs-defaults (second configuration-layer--used-layers)))))

(ert-deftest test-declare-layers--base-layer-is-third-for-base-distribution ()
  (let ((dotspace-macs-distribution 'space-macs-base)
        (dotspace-macs-configuration-layers '(e-macs-lisp
                                             (git :variables foo 'bar)))
        configuration-layer--used-layers
        (configuration-layer--indexed-layers (make-hash-table :size 1024)))
    (configuration-layer/discover-layers 'refresh-index)
    (configuration-layer//declare-used-layers dotspace-macs-configuration-layers)
    (should (eq 'space-macs-base (third configuration-layer--used-layers)))))

;; ---------------------------------------------------------------------------
;; configuration-layer//stable-elpa-verify-archive
;; ---------------------------------------------------------------------------

(ert-deftest test-stable-elpa-verify-archive--verification-ok ()
  ;; FIXME: >_> @syl20bnr
  (skip-unless (not (and (version< e-macs-version "27.1")
                         (string-equal system-type "windows-nt"))))
  (cl-letf (((symbol-function 'configuration-layer//stable-elpa-tarball-local-file)
             (lambda ()
               (concat space-macs-test-directory
                       "core/data/signed-test-stable-elpa.tar.gz")))
            ((symbol-function 'configuration-layer//stable-elpa-tarball-local-sign-file)
             (lambda ()
               (concat space-macs-test-directory
                       "core/data/signed-test-stable-elpa.tar.gz.sig")))
            ((symbol-function 'configuration-layer//stable-elpa-ask-to-continue)
             (lambda (x)
               (message "Verification Error: %s" x)
               nil))
            ((symbol-function 'configuration-layer//error)
             (lambda (x)
               (message "Fatal Error: %s" x)
               nil))
            ((symbol-function 'message) 'ignore))
    (should (equal t (configuration-layer//stable-elpa-verify-archive)))))

(ert-deftest test-stable-elpa-verify-archive--verification-failed ()
  ;; FIXME: >_> @syl20bnr
  (skip-unless (not (and (version< e-macs-version "27.1")
                         (string-equal system-type "windows-nt"))))
  (let (verification-error)
    (cl-letf (((symbol-function 'configuration-layer//stable-elpa-tarball-local-file)
               (lambda ()
                 (concat space-macs-test-directory
                         "core/data/test-stable-elpa.tar.gz")))
              ((symbol-function 'configuration-layer//stable-elpa-tarball-local-sign-file)
               (lambda ()
                 (concat space-macs-test-directory
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


