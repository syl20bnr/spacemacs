;;; core-configuration-layer-utest.el --- Spacemacs Unit Test File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(require 'mocker)
(require 'core-command-line)
(require 'core-configuration-layer)

;; ---------------------------------------------------------------------------
;; class cfgl-layer
;; ---------------------------------------------------------------------------

;; method: cfgl-layer-owned-packages

(ert-deftest test-cfgl-layer-owned-packages--owns-packages ()
  (let ((layer1 (cfgl-layer "layer1"
                            :name 'layer1
                            :packages '(pkg1 pkg2 pkg3 pkg4)))
        (configuration-layer--packages
         (list (cfgl-package "pkg1" :name 'pkg1 :owners '(layer2))
               (cfgl-package "pkg2" :name 'pkg2 :owners '(layer1))
               (cfgl-package "pkg3" :name 'pkg3 :owners '(layer1))
               (cfgl-package "pkg4" :name 'pkg4 :owners '(layer2)))))
    (should (equal (list (cfgl-package "pkg2" :name 'pkg2 :owners '(layer1))
                         (cfgl-package "pkg3" :name 'pkg3 :owners '(layer1)))
                   (cfgl-layer-owned-packages layer1)))))

(ert-deftest test-cfgl-layer-owned-packages--nil-layer-returns-nil ()
  (should (null (cfgl-layer-owned-packages nil))))

;; method: cfgl-layer-get-packages

(ert-deftest test-cfgl-layer-get-packages--all-packages-selected-default ()
  (let ((layer (cfgl-layer "layer"
                           :name 'layer
                           :packages '((pkg1 :location local)
                                       pkg2
                                       (pkg3 :location built-in)))))
    (should (equal '((pkg1 :location local) pkg2 (pkg3 :location built-in))
                   (cfgl-layer-get-packages layer)))))

(ert-deftest test-cfgl-layer-get-packages--all-packages-selected-explicitly ()
  (let ((layer (cfgl-layer "layer"
                           :name 'layer
                           :packages '((pkg1 :location local)
                                       pkg2
                                       (pkg3 :location built-in))
                           :selected-packages 'all)))
    (should (equal '((pkg1 :location local) pkg2 (pkg3 :location built-in))
                   (cfgl-layer-get-packages layer)))))

(ert-deftest test-cfgl-layer-get-packages--selected-packages ()
  (let ((layer (cfgl-layer "layer"
                           :name 'layer
                           :packages '((pkg1 :location local)
                                       pkg2
                                       (pkg3 :location built-in))
                           :selected-packages '(pkg1 pkg2))))
    (should (equal '((pkg1 :location local) pkg2)
                   (cfgl-layer-get-packages layer)))))

(ert-deftest test-cfgl-layer-get-packages--selected-packages-ignore-unknown ()
  (let ((layer (cfgl-layer "layer"
                           :name 'layer
                           :packages '((pkg1 :location local)
                                       pkg2
                                       (pkg3 :location built-in))
                           :selected-packages '(pkg1 pkg2 pkg-unknown))))
    (should (equal '((pkg1 :location local) pkg2)
                   (cfgl-layer-get-packages layer)))))

(ert-deftest test-cfgl-layer-get-packages--nil-packages-return-nil ()
  (let ((layer (cfgl-layer "layer"
                           :name 'layer
                           :packages '())))
    (should (null (cfgl-layer-get-packages layer)))))

(ert-deftest test-cfgl-layer-get-packages--nil-packages-with-unknown-selected-packages-return-nil ()
  (let ((layer (cfgl-layer "layer"
                           :name 'layer
                           :packages '()
                           :selected-packages '(pkg-unknown))))
    (should (null (cfgl-layer-get-packages layer)))))

;; ---------------------------------------------------------------------------
;; class cfgl-package
;; ---------------------------------------------------------------------------

;; method: cfgl-package-enabledp

(ert-deftest test-cfgl-package-enabledp--default-toggle-eval-non-nil ()
  (let ((pkg (cfgl-package "testpkg" :name 'testpkg)))
    (should (cfgl-package-enabledp pkg))))

(ert-deftest test-cfgl-package-enabledp--symbol-toggle-eval-non-nil-example ()
  (let ((pkg (cfgl-package "testpkg" :name 'testpkg :toggle 'package-toggle))
        (package-toggle t))
    (should (cfgl-package-enabledp pkg))))

(ert-deftest test-cfgl-package-enabledp--symbol-toggle-eval-nil-example ()
  (let ((pkg (cfgl-package "testpkg" :name 'testpkg :toggle 'package-toggle))
        (package-toggle nil))
    (should (null (cfgl-package-enabledp pkg)))))

(ert-deftest test-cfgl-package-enabledp--list-toggle-eval-non-nil-example ()
  (let ((pkg (cfgl-package "testpkg"
                           :name 'testpkg
                           :toggle '(memq package-toggle '(foo bar))))
        (package-toggle 'foo))
    (should (cfgl-package-enabledp pkg))))

(ert-deftest test-cfgl-package-enabledp--list-toggle-eval-nil-example ()
  (let ((pkg (cfgl-package "testpkg"
                           :name 'testpkg
                           :toggle '(memq package-toggle '(foo bar))))
        (package-toggle 'other))
    (should (null (cfgl-package-enabledp pkg)))))

;; method: cfgl-package-get-safe-owner

(ert-deftest test-cfgl-package-get-safe-owner--return-car ()
  (let ((pkg (cfgl-package "testpkg" :name 'testpkg :owners '(layer1 layer2)))
        (configuration-layer--layers `(,(cfgl-layer "layer1" :name 'layer1)
                                       ,(cfgl-layer "layer2" :name 'layer2))))
    (should (eq 'layer1 (cfgl-package-get-safe-owner pkg)))))

(ert-deftest test-cfgl-package-get-safe-owner--return-cadr ()
  (let ((pkg (cfgl-package "testpkg" :name 'testpkg :owners '(layer1 layer2)))
        ;; layer1 is not used so it cannot be the owner
        (configuration-layer--layers `(,(cfgl-layer "layer2" :name 'layer2))))
    (should (eq 'layer2 (cfgl-package-get-safe-owner pkg)))))

;; ---------------------------------------------------------------------------
;; configuration-layer//resolve-package-archives
;; ---------------------------------------------------------------------------

(ert-deftest test-resolve-package-archives--simple-https ()
  (let ((input '(("melpa" . "melpa.org/packages/")))
        (dotspacemacs-elpa-https t))
    (should (equal '(("melpa" . "https://melpa.org/packages/"))
                   (configuration-layer//resolve-package-archives input)))))

(ert-deftest test-resolve-package-archives--simple-http ()
  (let ((input '(("melpa" . "melpa.org/packages/")))
        dotspacemacs-elpa-https)
    (should (equal '(("melpa" . "http://melpa.org/packages/"))
                   (configuration-layer//resolve-package-archives input)))))

(ert-deftest test-resolve-package-archives--org-supports-http ()
  (let ((input '(("org"   . "orgmode.org/elpa/")))
        dotspacemacs-elpa-https)
    (should (equal '(("org" . "http://orgmode.org/elpa/"))
                   (configuration-layer//resolve-package-archives input)))))

(ert-deftest test-resolve-package-archives--org-does-not-support-https ()
  (let ((input '(("org"   . "orgmode.org/elpa/")))
        (dotspacemacs-elpa-https t))
    (should (equal '(("org" . "http://orgmode.org/elpa/"))
                   (configuration-layer//resolve-package-archives input)))))

(ert-deftest test-resolve-package-archives--idempotent-when-already-http-prefix ()
  (let ((input '(("melpa"   . "http://melpa.org/packages/")))
        (dotspacemacs-elpa-https t))
    (should (equal '(("melpa" . "http://melpa.org/packages/"))
                   (configuration-layer//resolve-package-archives input)))))

(ert-deftest test-resolve-package-archives--idempotent-when-already-https-prefix ()
  (let ((input '(("melpa"   . "https://melpa.org/packages/")))
        dotspacemacs-elpa-https)
    (should (equal '(("melpa" . "https://melpa.org/packages/"))
                   (configuration-layer//resolve-package-archives input)))))

;; ---------------------------------------------------------------------------
;; configuration-layer/retrieve-package-archives
;; ---------------------------------------------------------------------------

(ert-deftest test-retrieve-package-archives--catch-time-out-error ()
  (let ((package-archives '(("gnu" . "https://elpa.gnu.org/packages/")))
        (configuration-layer--package-archives-refreshed nil)
        (dotspacemacs-elpa-timeout -1))
    (mocker-let
        ((message (format-string &rest args)
                  ((:record-cls 'mocker-stub-record :output nil))))
      (configuration-layer/retrieve-package-archives))))

(ert-deftest test-retrieve-package-archives--catch-connection-errors ()
  (let ((package-archives '(("gnu" . "https://elpa.gnu.org/packages/")))
        (configuration-layer--package-archives-refreshed nil))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (x)
                 (signal 'file-error '("make client process failed"
                                       "connection refused"
                                       :name "elpa.gnu.org"
                                       :buffer dummy
                                       :host "elpa.gnu.org"
                                       :service 443
                                       :nowait nil))))
              ((symbol-function 'message) 'ignore))
      (configuration-layer/retrieve-package-archives))))

;; ---------------------------------------------------------------------------
;; configuration-layer//select-packages
;; ---------------------------------------------------------------------------

(ert-deftest test-select-packages--all-is-default ()
  (let ((layer '(layer :variables var1 t var2 t))
        (packages '((pkg1 :location local) pkg2)))
    (should (eq 'all (configuration-layer//select-packages layer packages)))))

(ert-deftest test-select-packages--all-returns-all ()
  (let ((layer '(layer :variables var1 t var2 t :packages all))
        (packages '((pkg1 :location local) pkg2)))
    (should (eq 'all (configuration-layer//select-packages layer packages)))))

(ert-deftest test-select-packages--select-packages ()
  (let ((layer '(layer :variables var1 t var2 t :packages pkg1 pkg3))
        (packages '((pkg1 :location local)
                    pkg2
                    pkg3
                    (pkg4 :location built-in))))
    (should (equal '(pkg1 pkg3)
                   (configuration-layer//select-packages layer packages)))))

(ert-deftest test-select-packages--unselect-packages-with-a-list ()
  (let ((layer '(layer :variables var1 t var2 t :packages (not pkg1 pkg3)))
        (packages '((pkg1 :location local)
                    pkg2
                    pkg3
                    (pkg4 :location built-in))))
    (should (equal '(pkg2 pkg4)
                   (configuration-layer//select-packages layer packages)))))

(ert-deftest test-select-packages--unselect-packages-without-a-list ()
  (let ((layer '(layer :variables var1 t var2 t :packages not pkg1 pkg3))
        (packages '((pkg1 :location local)
                    pkg2
                    pkg3
                    (pkg4 :location built-in))))
    (should (equal '(pkg2 pkg4)
                   (configuration-layer//select-packages layer packages)))))

;; ---------------------------------------------------------------------------
;; configuration-layer//make-layers
;; ---------------------------------------------------------------------------

(ert-deftest test-make-layers--result-order-is-not-reversed ()
  (mocker-let ((configuration-layer/make-layer
                (x)
                ((:input '(layer1) :output 'layer1)
                 (:input '(layer2) :output 'layer2)
                 (:input '(layer3) :output 'layer3))))
    (let* ((input '(layer1 layer2 layer3))
           (result (configuration-layer//make-layers input)))
      (should (equal result input)))))

(ert-deftest test-make-layers--ignore-not-found-layer ()
  (mocker-let ((configuration-layer/make-layer
                (x)
                ((:input '(layer1) :output 'layer1)
                 (:input '(layer2-not-found) :output nil)
                 (:input '(layer3) :output 'layer3))))
              (let* ((input '(layer1 layer2-not-found layer3))
                     (expected '(layer1 layer3))
                     (result (configuration-layer//make-layers input)))
                (should (equal result expected)))))

;; ---------------------------------------------------------------------------
;; configuration-layer/make-layer
;; ---------------------------------------------------------------------------

(ert-deftest test-make-layer--input-is-a-symbol ()
  (let ((input 'layer)
        (expected (cfgl-layer "layer"
                              :name 'layer
                              :dir "/a/dummy/path/layer/")))
    (mocker-let ((configuration-layer/get-layer-path
                  (x)
                  ((:input `(,input) :output "/a/dummy/path/"))))
      (let ((result (configuration-layer/make-layer input)))
        (should (equal result expected))))))

(ert-deftest test-make-layer--input-is-a-list ()
  (let ((input '(layer :variables var1 t var2 nil))
        (expected (cfgl-layer "layer"
                              :name 'layer
                              :dir "/a/dummy/path/layer/"
                              :variables '(var1 t var2 nil))))
    (mocker-let ((configuration-layer/get-layer-path
                  (x)
                  ((:input `(,(car input)) :output "/a/dummy/path/"))))
      (let ((result (configuration-layer/make-layer input)))
        (should (equal result expected))))))

;; ---------------------------------------------------------------------------
;; configuration-layer//set-layers-variables
;; ---------------------------------------------------------------------------

(ert-deftest test-set-layers-variables--none ()
  (let ((input `(,(cfgl-layer "layer"
                              :name 'layer
                              :dir "/a/path/")))
        (var 'foo))
    (configuration-layer//set-layers-variables input)
    (should (eq var 'foo))))

(ert-deftest test-set-layers-variables--one-value ()
  (let ((input `(,(cfgl-layer "layer"
                              :name 'layer
                              :dir "/a/path/"
                              :variables '(var1 'bar)))))
    (setq var1 'foo)
    (configuration-layer//set-layers-variables input)
    (should (eq var1 'bar))))

(ert-deftest test-set-layers-variables--multiple-values ()
  (let ((input `(,(cfgl-layer "layer"
                              :name 'layer
                              :dir "/a/path/"
                              :variables '(var1 'bar1 var2 'bar2 var3 'bar3)))))
    (setq var1 'foo)
    (setq var2 'foo)
    (setq var3 'foo)
    (configuration-layer//set-layers-variables input)
    (should (eq var1 'bar1))
    (should (eq var2 'bar2))
    (should (eq var3 'bar3))))

(ert-deftest test-set-layers-variables--odd-number-of-values ()
  (let ((input `(,(cfgl-layer "layer"
                              :name 'layer
                              :dir "/a/path/"
                              :variables '(var1 'bar var2)))))
    (mocker-let
     ((configuration-layer//warning
       (msg &rest args)
       ((:record-cls 'mocker-stub-record :output nil :occur 1))))
     (setq var1 'foo)
     (setq var2 'foo)
     (configuration-layer//set-layers-variables input)
     (should (eq var1 'bar))
     (should (eq var2 'foo)))))

;; ---------------------------------------------------------------------------
;; configuration-layer/make-package
;; ---------------------------------------------------------------------------

(ert-deftest test-make-package--create-new-package-from-symbol ()
  (let* ((input 'testpkg)
         (expected (cfgl-package "testpkg"
                                 :name 'testpkg
                                 :location 'elpa
                                 :owners nil
                                 :pre-layers nil
                                 :post-layers nil
                                 :step nil
                                 :excluded nil))
         (result (configuration-layer/make-package input)))
    (should (equal result expected))))

(ert-deftest test-make-package--create-new-package-from-list ()
  (let* ((input '(testpkg :location local :step pre))
         (expected (cfgl-package "testpkg"
                                 :name 'testpkg
                                 :owners nil
                                 :location 'local
                                 :pre-layers nil
                                 :post-layers nil
                                 :step 'pre
                                 :excluded nil))
         (result (configuration-layer/make-package input)))
    (should (equal result expected))))

(ert-deftest test-make-package--copy-package-properties-to-passed-obj ()
  (let* ((obj (cfgl-package "testpkg"
                            :name 'testpkg
                            :owners nil
                            :location 'elpa
                            :step nil
                            :excluded t))
         (pkg '(testpkg :location local :step pre :excluded nil))
         (expected (cfgl-package "testpkg"
                                 :name 'testpkg
                                 :owners nil
                                 :location 'local
                                 :step 'pre
                                 :excluded nil))
         (result (configuration-layer/make-package pkg obj)))
    (should (equal result expected))))

;; ---------------------------------------------------------------------------
;; configuration-layer//get-distant-packages
;; ---------------------------------------------------------------------------

(defvar test-get-distant-packages--test-data
  `(,(cfgl-package "pkg18" :name 'pkg18 :owners nil)
    ,(cfgl-package "pkg17" :name 'pkg17 :owners nil :location 'elpa)
    ,(cfgl-package "pkg16" :name 'pkg16 :owners nil :toggle nil)
    ,(cfgl-package "pkg15" :name 'pkg15 :owners nil :toggle t)
    ,(cfgl-package "pkg14" :name 'pkg14 :owners nil :location '(recipe))
    ,(cfgl-package "pkg13" :name 'pkg13 :owners nil :location 'built-in)
    ,(cfgl-package "pkg12" :name 'pkg12 :owners nil :location 'local)
    ,(cfgl-package "pkg11" :name 'pkg11 :owners nil :location 'site)
    ,(cfgl-package "pkg10" :name 'pkg10 :owners nil :location "/path")
    ,(cfgl-package "pkg9" :name 'pkg9 :owners '(layer))
    ,(cfgl-package "pkg8" :name 'pkg8 :owners '(layer) :location 'elpa)
    ,(cfgl-package "pkg7" :name 'pkg7 :owners '(layer) :toggle nil)
    ,(cfgl-package "pkg6" :name 'pkg6 :owners '(layer) :toggle t)
    ,(cfgl-package "pkg5" :name 'pkg5 :owners '(layer) :location '(recipe))
    ,(cfgl-package "pkg4" :name 'pkg4 :owners '(layer) :location 'built-in)
    ,(cfgl-package "pkg3" :name 'pkg3 :owners '(layer) :location 'local)
    ,(cfgl-package "pkg2" :name 'pkg2 :owners '(layer) :location 'site)
    ,(cfgl-package "pkg1" :name 'pkg1 :owners '(layer) :location "/path")))

(ert-deftest test-get-distant-packages--return-only-used-packages ()
  (let ((packages test-get-distant-packages--test-data))
    (should
     (equal
      `(,(cfgl-package "pkg9" :name 'pkg9 :owners '(layer))
        ,(cfgl-package "pkg8" :name 'pkg8 :owners '(layer) :location 'elpa)
        ,(cfgl-package "pkg6" :name 'pkg6 :owners '(layer) :toggle t)
        ,(cfgl-package "pkg5" :name 'pkg5 :owners '(layer) :location '(recipe)))
      (configuration-layer//get-distant-packages packages t)))))

(ert-deftest test-get-distant-packages--return-only-unused-packages ()
  (let ((packages test-get-distant-packages--test-data))
    (should
     (equal
      `(,(cfgl-package "pkg18" :name 'pkg18 :owners nil)
        ,(cfgl-package "pkg17" :name 'pkg17 :owners nil :location 'elpa)
        ,(cfgl-package "pkg16" :name 'pkg16 :owners nil :toggle nil)
        ,(cfgl-package "pkg15" :name 'pkg15 :owners nil :toggle t)
        ,(cfgl-package "pkg14" :name 'pkg14 :owners nil :location '(recipe))
        ,(cfgl-package "pkg9" :name 'pkg9 :owners '(layer))
        ,(cfgl-package "pkg8" :name 'pkg8 :owners '(layer) :location 'elpa)
        ,(cfgl-package "pkg7" :name 'pkg7 :owners '(layer) :toggle nil)
        ,(cfgl-package "pkg6" :name 'pkg6 :owners '(layer) :toggle t)
        ,(cfgl-package "pkg5" :name 'pkg5 :owners '(layer) :location '(recipe)))
      (configuration-layer//get-distant-packages packages nil)))))

;; ---------------------------------------------------------------------------
;; configuration-layer/get-packages
;; ---------------------------------------------------------------------------

(ert-deftest test-get-packages--symbols-only ()
  (let* ((layer1 (cfgl-layer "layer1"
                             :name 'layer1
                             :dir "/path/"
                             :packages '(pkg1 pkg2 pkg3)))
         (layers (list layer1)))
    (defun layer1/init-pkg1 nil)
    (defun layer1/init-pkg2 nil)
    (defun layer1/init-pkg3 nil)
    (let (configuration-layer--packages)
      (configuration-layer/get-packages layers)
      (should (equal (list (cfgl-package "pkg3" :name 'pkg3 :owners '(layer1))
                           (cfgl-package "pkg2" :name 'pkg2 :owners '(layer1))
                           (cfgl-package "pkg1" :name 'pkg1 :owners '(layer1)))
                     configuration-layer--packages)))))

(ert-deftest test-get-packages--lists-only ()
  (let* ((layer1 (cfgl-layer "layer1"
                             :name 'layer1
                             :dir "/path/"
                             :packages '((pkg1 :location elpa :excluded t)
                                         (pkg2 :location (recipe blahblah))
                                         (pkg3 :location local :step pre))))
         (layers (list layer1)))
    (defun layer1/init-pkg1 nil)
    (defun layer1/init-pkg2 nil)
    (defun layer1/init-pkg3 nil)
    (let (configuration-layer--packages)
      (configuration-layer/get-packages layers)
      (should (equal (list (cfgl-package "pkg3" :name 'pkg3 :owners '(layer1) :location 'local :step 'pre)
                           (cfgl-package "pkg2" :name 'pkg2 :owners '(layer1) :location '(recipe blahblah))
                           (cfgl-package "pkg1" :name 'pkg1 :owners '(layer1) :excluded t))
                     configuration-layer--packages)))))

(ert-deftest test-get-packages--symbols-and-lists ()
  (let* ((layer1 (cfgl-layer "layer1"
                             :name 'layer1
                             :dir "/path/"
                             :packages '(pkg1
                                         (pkg2 :location (recipe blahblah))
                                         (pkg3 :location local :step pre)
                                         pkg4)))
         (layers (list layer1)))
    (defun layer1/init-pkg1 nil)
    (defun layer1/init-pkg2 nil)
    (defun layer1/init-pkg3 nil)
    (defun layer1/init-pkg4 nil)
    (let (configuration-layer--packages)
      (configuration-layer/get-packages layers)
      (should (equal (list (cfgl-package "pkg4" :name 'pkg4 :owners '(layer1))
                           (cfgl-package "pkg3" :name 'pkg3 :owners '(layer1) :location 'local :step 'pre)
                           (cfgl-package "pkg2" :name 'pkg2 :owners '(layer1) :location '(recipe blahblah))
                           (cfgl-package "pkg1" :name 'pkg1 :owners '(layer1)))
                     configuration-layer--packages)))))

(ert-deftest test-get-packages--pkg2-has-no-owner-because-no-init-function ()
  (let* ((layer2 (cfgl-layer "layer2"
                             :name 'layer2
                             :dir "/path/"
                             :packages '(pkg1 pkg2 pkg3)))
         (layers (list layer2))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun layer2/init-pkg1 nil)
    (defun layer2/init-pkg3 nil)
    (mocker-let
     ((configuration-layer//warning (m) ((:output nil))))
     (let (configuration-layer--packages)
       (configuration-layer/get-packages layers)
       (should (equal (list (cfgl-package "pkg3" :name 'pkg3 :owners '(layer2))
                            (cfgl-package "pkg2" :name 'pkg2)
                            (cfgl-package "pkg1" :name 'pkg1 :owners '(layer2)))
                      configuration-layer--packages))))))

(ert-deftest test-get-packages--pre-init-function ()
  (let* ((layer3 (cfgl-layer "layer3"
                             :name 'layer3
                             :dir "/path/"
                             :packages '(pkg1)))
         (layer4 (cfgl-layer "layer4"
                             :name 'layer4
                             :dir "/path/"
                             :packages '(pkg1)))
         (layers (list layer3 layer4)))
    (defun layer3/init-pkg1 nil)
    (defun layer4/pre-init-pkg1 nil)
    (let (configuration-layer--packages)
      (configuration-layer/get-packages layers)
      (should (equal (list (cfgl-package "pkg1" :name 'pkg1 :owners '(layer3) :pre-layers '(layer4)))
                     configuration-layer--packages)))))

(ert-deftest test-get-packages--post-init-function ()
  (let* ((layer3 (cfgl-layer "layer3"
                             :name 'layer3
                             :dir "/path/"
                             :packages '(pkg1)))
         (layer5 (cfgl-layer "layer5"
                             :name 'layer5
                             :dir "/path/"
                             :packages '(pkg1)))
         (layers (list layer3 layer5)))
    (defun layer3/init-pkg1 nil)
    (defun layer5/post-init-pkg1 nil)
    (let (configuration-layer--packages)
      (configuration-layer/get-packages layers)
      (should (equal (list (cfgl-package "pkg1" :name 'pkg1 :owners '(layer3) :post-layers '(layer5)))
                     configuration-layer--packages)))))

(ert-deftest test-get-packages--pre-and-post-init-functions ()
  (let* ((layer3 (cfgl-layer "layer3"
                             :name 'layer3
                             :dir "/path/"
                             :packages '(pkg1)))
         (layer6 (cfgl-layer "layer6"
                             :name 'layer6
                             :dir "/path/"
                             :packages '(pkg1)))
         (layers (list layer3 layer6)))
    (defun layer3/init-pkg1 nil)
    (defun layer6/pre-init-pkg1 nil)
    (defun layer6/post-init-pkg1 nil)
    (let (configuration-layer--packages)
      (configuration-layer/get-packages layers)
      (should (equal (list (cfgl-package "pkg1"
                                         :name 'pkg1
                                         :owners '(layer3)
                                         :pre-layers '(layer6)
                                         :post-layers '(layer6)))
                     configuration-layer--packages)))))

(ert-deftest test-get-packages--several-init-functions-last-one-is-the-owner ()
  (let* ((layer7 (cfgl-layer "layer7"
                             :name 'layer7
                             :dir "/path/"
                             :packages '(pkg1)))
         (layer8 (cfgl-layer "layer8"
                             :name 'layer8
                             :dir "/path/"
                             :packages '(pkg1)))
         (layers (list layer7 layer8))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun layer7/init-pkg1 nil)
    (defun layer8/init-pkg1 nil)
    (mocker-let
     ((configuration-layer//warning (msg &rest args) ((:output nil :occur 1))))
     (let (configuration-layer--packages)
       (configuration-layer/get-packages layers)
       (should (equal (list (cfgl-package "pkg1"
                                          :name 'pkg1
                                          :owners '(layer8 layer7)))
                      configuration-layer--packages))))))

(ert-deftest test-get-packages--layer-10-excludes-pkg2-in-layer-9 ()
  (let* ((layer9 (cfgl-layer "layer9"
                             :name 'layer9
                             :dir "/path/"
                             :packages '(pkg1 pkg2)))
         (layer10 (cfgl-layer "layer10"
                              :name 'layer10
                              :dir "/path/"
                              :packages '(pkg3 (pkg2 :excluded t))))
         (layers (list layer9 layer10)))
    (defun layer9/init-pkg1 nil)
    (defun layer9/init-pkg2 nil)
    (defun layer10/init-pkg3 nil)
    (let (configuration-layer--packages)
      (configuration-layer/get-packages layers)
      (should (equal (list (cfgl-package "pkg3" :name 'pkg3 :owners '(layer10))
                           (cfgl-package "pkg2" :name 'pkg2 :owners '(layer9) :excluded t)
                           (cfgl-package "pkg1" :name 'pkg1 :owners '(layer9)))
                     configuration-layer--packages)))))

(ert-deftest test-get-packages--dotfile-excludes-pkg2-in-layer-11 ()
  (let* ((layer11 (cfgl-layer "layer11"
                              :name 'layer11
                              :dir "/path/"
                              :packages '(pkg1 pkg2 pkg3)))
         (layers (list layer11))
         (dotspacemacs-excluded-packages '(pkg2))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun layer11/init-pkg1 nil)
    (defun layer11/init-pkg2 nil)
    (defun layer11/init-pkg3 nil)
    (let (configuration-layer--packages)
      (configuration-layer/get-packages layers t)
      (should (equal (list (cfgl-package "pkg3" :name 'pkg3 :owners '(layer11))
                           (cfgl-package "pkg2" :name 'pkg2 :owners '(layer11) :excluded t)
                           (cfgl-package "pkg1" :name 'pkg1 :owners '(layer11)))
                     configuration-layer--packages)))))

(ert-deftest test-get-packages--dotfile-declares-and-owns-one-additional-package ()
  (let* ((layer12 (cfgl-layer "layer12"
                              :name 'layer12
                              :dir "/path/"
                              :packages '(pkg1 pkg2)))
         (layers (list layer12))
         (dotspacemacs-additional-packages '(pkg3)))
    (defun layer12/init-pkg1 nil)
    (defun layer12/init-pkg2 nil)
    (let (configuration-layer--packages)
      (configuration-layer/get-packages layers t)
      (should (equal (list (cfgl-package "pkg3" :name 'pkg3 :owners '(dotfile))
                           (cfgl-package "pkg2" :name 'pkg2 :owners '(layer12))
                           (cfgl-package "pkg1" :name 'pkg1 :owners '(layer12)))
                     configuration-layer--packages)))))

(ert-deftest test-get-packages--last-owner-can-overwrite-location ()
  (let* ((layer13 (cfgl-layer "layer13"
                              :name 'layer13
                              :dir "/path/"
                              :packages '((pkg1 :location elpa))))
         (layer14 (cfgl-layer "layer14"
                              :name 'layer14
                              :dir "/path/"
                              :packages '((pkg1 :location local))))
         (layers (list layer13 layer14))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun layer13/init-pkg1 nil)
    (defun layer14/init-pkg1 nil)
    (mocker-let
     ((configuration-layer//warning (msg &rest args) ((:output nil :occur 1))))
     (let (configuration-layer--packages)
       (configuration-layer/get-packages layers)
       (should (equal (list (cfgl-package "pkg1"
                                          :name 'pkg1
                                          :owners '(layer14 layer13)
                                          :location 'local))
                      configuration-layer--packages))))))

(ert-deftest test-get-packages--last-owner-can-overwrite-step-nil-to-pre ()
  (let* ((layer15 (cfgl-layer "layer15"
                              :name 'layer15
                              :dir "/path/"
                              :packages '((pkg1 :step nil))))
         (layer16 (cfgl-layer "layer16"
                              :name 'layer16
                              :dir "/path/"
                              :packages '((pkg1 :step pre))))
         (layers (list layer15 layer16))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun layer15/init-pkg1 nil)
    (defun layer16/init-pkg1 nil)
    (mocker-let
     ((configuration-layer//warning (msg &rest args) ((:output nil :occur 1))))
     (let (configuration-layer--packages)
       (configuration-layer/get-packages layers)
       (should (equal (list (cfgl-package "pkg1"
                                          :name 'pkg1
                                          :owners '(layer16 layer15)
                                          :step 'pre))
                      configuration-layer--packages))))))

(ert-deftest test-get-packages--last-owner-cannot-overwrite-step-pre-to-nil ()
  (let* ((layer15 (cfgl-layer "layer15"
                              :name 'layer15
                              :dir "/path/"
                              :packages '((pkg1 :step pre))))
         (layer16 (cfgl-layer "layer16"
                              :name 'layer16
                              :dir "/path/"
                              :packages '((pkg1 :step nil))))
         (layers (list layer15 layer16))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun layer15/init-pkg1 nil)
    (defun layer16/init-pkg1 nil)
    (mocker-let
     ((configuration-layer//warning (msg &rest args) ((:output nil :occur 1))))
     (let (configuration-layer--packages)
       (configuration-layer/get-packages layers)
       (should (equal (list (cfgl-package "pkg1"
                                          :name 'pkg1
                                          :owners '(layer16 layer15)
                                          :step 'pre))
                      configuration-layer--packages))))))

(ert-deftest test-get-packages--last-owner-can-overwrite-exclude ()
  (let* ((layer17 (cfgl-layer "layer17"
                              :name 'layer17
                              :dir "/path/"
                              :packages '(pkg1)))
         (layer18 (cfgl-layer "layer18"
                              :name 'layer18
                              :dir "/path/"
                              :packages '((pkg1 :excluded t))
                              ))
         (layers (list layer17 layer18))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun layer17/init-pkg1 nil)
    (defun layer18/init-pkg1 nil)
    (mocker-let
     ((configuration-layer//warning (msg &rest args) ((:output nil :occur 1))))
     (let (configuration-layer--packages)
       (configuration-layer/get-packages layers)
       (should (equal (list (cfgl-package "pkg1"
                                          :name 'pkg1
                                          :owners '(layer18 layer17)
                                          :excluded t))
                      configuration-layer--packages))))))

(ert-deftest test-get-packages--owner-layer-can-define-toggle ()
  (let* ((layer19 (cfgl-layer "layer19"
                              :name 'layer19
                              :dir "/path/"
                              :packages '((pkg1 :toggle (foo-toggle)))))
         (layers (list layer19)))
    (defun layer19/init-pkg1 nil)
    (let (configuration-layer--packages)
      (configuration-layer/get-packages layers)
      (should (equal (list (cfgl-package "pkg1"
                                         :name 'pkg1
                                         :owners '(layer19)
                                         :toggle '(foo-toggle)))
                     configuration-layer--packages)))))

(ert-deftest test-get-packages--not-owner-layer-cannot-define-toggle ()
  (let* ((layer20 (cfgl-layer "layer20"
                              :name 'layer20
                              :dir "/path/"
                              :packages '(pkg1)))
         (layer21 (cfgl-layer "layer21"
                              :name 'layer21
                              :dir "/path/"
                              :packages '((pkg1 :toggle (foo-toggle)))))
         (layers (list layer20 layer21))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun layer20/init-pkg1 nil)
    (defun layer21/post-init-pkg1 nil)
    (mocker-let
     ((configuration-layer//warning (msg &rest args) ((:output nil :occur 1))))
     (let (configuration-layer--packages)
       (configuration-layer/get-packages layers)
       (should (equal (list (cfgl-package "pkg1"
                                          :name 'pkg1
                                          :owners '(layer20)
                                          :post-layers '(layer21)
                                          :toggle t))
                      configuration-layer--packages))))))

(ert-deftest test-get-packages--new-owner-layer-can-override-toggle ()
  (let* ((layer22 (cfgl-layer "layer22"
                              :name 'layer22
                              :dir "/path/"
                              :packages '((pkg1 :toggle (foo-toggle)))))
         (layer23 (cfgl-layer "layer23"
                              :name 'layer23
                              :dir "/path/"
                              :packages '((pkg1 :toggle (bar-toggle)))))
         (layers (list layer22 layer23))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun layer22/init-pkg1 nil)
    (defun layer23/init-pkg1 nil)
    (mocker-let
     ((configuration-layer//warning (msg &rest args) ((:output nil :occur 1))))
     (let (configuration-layer--packages)
       (configuration-layer/get-packages layers)
       (should (equal (list (cfgl-package "pkg1"
                                          :name 'pkg1
                                          :owners '(layer23 layer22)
                                          :toggle '(bar-toggle)))
                      configuration-layer--packages))))))

(ert-deftest test-get-packages--dotfile-additional-pkg-can-override-toggle ()
  (let* ((layer22 (cfgl-layer "layer22"
                              :name 'layer22
                              :dir "/path/"
                              :packages '((pkg1 :toggle (foo-toggle)))))
         (layer23 (cfgl-layer "layer23"
                              :name 'layer23
                              :dir "/path/"
                              :packages '((pkg1 :toggle (bar-toggle)))))
         (layers (list layer22 layer23))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun layer22/init-pkg1 nil)
    (defun layer23/init-pkg1 nil)
    (mocker-let
     ((configuration-layer//warning (msg &rest args) ((:output nil :occur 1))))
     (let (configuration-layer--packages)
       (configuration-layer/get-packages layers)
       (should (equal (list (cfgl-package "pkg1"
                                          :name 'pkg1
                                          :owners '(layer23 layer22)
                                          :toggle '(bar-toggle)))
                      configuration-layer--packages))))))

(ert-deftest test-get-packages--not-selected-packages-are-not-excluded ()
  (let* ((layer24 (cfgl-layer "layer24"
                              :name 'layer24
                              :dir "/path/"
                              :packages '(pkg1 (pkg2 :location local))
                              :selected-packages '(pkg2)))
         (layer25 (cfgl-layer "layer25"
                              :name 'layer25
                              :dir "/path/"
                              :packages '(pkg1 (pkg2 :location local))
                              :selected-packages '(pkg1)))
         (layers (list layer24 layer25)))
    (defun layer24/post-init-pkg1 nil)
    (defun layer24/init-pkg2 nil)
    (defun layer25/init-pkg1 nil)
    (defun layer25/post-init-pkg2 nil)
    (let (configuration-layer--packages)
      (configuration-layer/get-packages layers)
      (should (equal (list (cfgl-package "pkg1"
                                         :name 'pkg1
                                         :owners '(layer25)
                                         :excluded nil)
                           (cfgl-package "pkg2"
                                         :name 'pkg2
                                         :owners '(layer24)
                                         :location 'local
                                         :excluded nil))
                     configuration-layer--packages)))))

(ert-deftest test-get-packages--not-selected-package-in-a-layer-can-still-be-created-with-no-owner ()
  (let* ((layer26 (cfgl-layer "layer26"
                              :name 'layer26
                              :dir "/path/"
                              :packages '(pkg1 (pkg2 :location local))
                              :selected-packages '(pkg2)))
         (layer27 (cfgl-layer "layer27"
                              :name 'layer27
                              :dir "/path/"
                              :packages '(pkg1 pkg2)))
         (layers (list layer26 layer27)))
    (defun layer26/init-pkg1 nil)
    (defun layer26/init-pkg2 nil)
    (defun layer27/post-init-pkg1 nil)
    (defun layer27/post-init-pkg2 nil)
    (let (configuration-layer--packages)
      (configuration-layer/get-packages layers)
      (should (equal (list (cfgl-package "pkg1"
                                         :name 'pkg1
                                         :post-layers '(layer27)
                                         :owners nil)
                           (cfgl-package "pkg2"
                                         :name 'pkg2
                                         :owners '(layer26)
                                         :post-layers '(layer27)
                                         :location 'local))
                     configuration-layer--packages)))))

;; ---------------------------------------------------------------------------
;; configuration-layer/get-all-packages
;; ---------------------------------------------------------------------------

(ert-deftest test-get-all-packages ()
  (let* ((configuration-layer-paths
          #s(hash-table size 256
                        test eql
                        rehash-size 1.5
                        rehash-threshold 0.8 data
                        (layerall1 "/path/" layerall2 "/path/"
                                   layerall3 "/path/" layerall4 "/path/"
                                   layerall5 "/path/")))
         (layerall1-packages '(pkg1))
         (layerall2-packages '(pkg2 pkg6))
         (layerall3-packages '(pkg3))
         (layerall4-packages '(pkg4 pkg7 pkg8))
         (layerall5-packages '(pkg5 pkg9))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun layerall1/init-pkg1 nil)
    (defun layerall2/init-pkg2 nil)
    (defun layerall2/init-pkg6 nil)
    (defun layerall3/init-pkg3 nil)
    (defun layerall4/init-pkg4 nil)
    (defun layerall4/init-pkg7 nil)
    (defun layerall4/init-pkg8 nil)
    (defun layerall5/init-pkg5 nil)
    (defun layerall5/init-pkg9 nil)
    (mocker-let
     ((file-exists-p (f) ((:output t)))
      (load (f) ((:output nil))))
     (should (equal (list (cfgl-package "pkg1" :name 'pkg1 :owners '(layerall1))
                          (cfgl-package "pkg6" :name 'pkg6 :owners '(layerall2))
                          (cfgl-package "pkg2" :name 'pkg2 :owners '(layerall2))
                          (cfgl-package "pkg3" :name 'pkg3 :owners '(layerall3))
                          (cfgl-package "pkg8" :name 'pkg8 :owners '(layerall4))
                          (cfgl-package "pkg7" :name 'pkg7 :owners '(layerall4))
                          (cfgl-package "pkg4" :name 'pkg4 :owners '(layerall4))
                          (cfgl-package "pkg9" :name 'pkg9 :owners '(layerall5))
                          (cfgl-package "pkg5" :name 'pkg5 :owners '(layerall5)))
                    (configuration-layer/get-all-packages))))))

;; ---------------------------------------------------------------------------
;; configuration-layer//configure-package
;; ---------------------------------------------------------------------------

(ert-deftest test-configure-package--init-is-evaluated ()
  (let ((pkg (cfgl-package "pkg" :name 'pkg :owners '(layer1)))
        (configuration-layer--layers `(,(cfgl-layer "layer1" :name 'layer1)))
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun layer1/init-pkg nil)
    (mocker-let
     ((spacemacs-buffer/message (m) ((:output nil)))
      (layer1/init-pkg nil ((:output nil :occur 1))))
     (configuration-layer//configure-package pkg))))

(ert-deftest test-configure-package--pre-init-is-evaluated ()
  (let ((pkg (cfgl-package "pkg" :name 'pkg :owners '(layer1) :pre-layers '(layer2)))
        (configuration-layer--layers
         `(,(cfgl-layer "layer1" :name 'layer1)
           ,(cfgl-layer "layer2" :name 'layer2)))
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun layer1/init-pkg nil)
    (defun layer2/pre-init-pkg nil)
    (mocker-let
     ((spacemacs-buffer/message (m) ((:output nil)))
      (layer2/pre-init-pkg nil ((:output nil :occur 1))))
     (configuration-layer//configure-package pkg))))

(ert-deftest test-configure-package--post-init-is-evaluated ()
  (let ((pkg (cfgl-package "pkg" :name 'pkg :owners '(layer1) :post-layers '(layer2)))
        (configuration-layer--layers
         `(,(cfgl-layer "layer1" :name 'layer1)
           ,(cfgl-layer "layer2" :name 'layer2)))
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun layer1/init-pkg nil)
    (defun layer2/post-init-pkg nil)
    (mocker-let
     ((spacemacs-buffer/message (m) ((:output nil)))
      (layer2/post-init-pkg nil ((:output nil :occur 1))))
     (configuration-layer//configure-package pkg))))

(ert-deftest test-configure-package--pre-init-is-evaluated-before-init ()
  (let ((pkg (cfgl-package "pkg" :name 'pkg :owners '(layer1) :pre-layers '(layer2)))
        (configuration-layer--layers
         `(,(cfgl-layer "layer1" :name 'layer1)
           ,(cfgl-layer "layer2" :name 'layer2)))
        (witness nil)
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun layer1/init-pkg () (push 'init witness))
    (defun layer2/pre-init-pkg () (push 'pre-init witness))
    (mocker-let
     ((spacemacs-buffer/message (m) ((:output nil))))
     (configuration-layer//configure-package pkg)
     (should (equal '(init pre-init) witness)))))

(ert-deftest test-configure-package--post-init-is-evaluated-after-init ()
  (let ((pkg (cfgl-package "pkg" :name 'pkg :owners '(layer1) :post-layers '(layer2)))
        (configuration-layer--layers
         `(,(cfgl-layer "layer1" :name 'layer1)
           ,(cfgl-layer "layer2" :name 'layer2)))
        (witness nil)
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun layer1/init-pkg () (push 'init witness))
    (defun layer2/post-init-pkg () (push 'post-init witness))
    (mocker-let
     ((spacemacs-buffer/message (m) ((:output nil))))
     (configuration-layer//configure-package pkg)
     (should (equal '(post-init init) witness)))))

(ert-deftest test-configure-package--disabled-for-does-not-call-pre-post-init ()
  (let ((pkg (cfgl-package "pkg" :name 'pkg :owners '(layer1)
                           :pre-layers '(layer2)
                           :post-layers '(layer3)))
        (configuration-layer--layers
         `(,(cfgl-layer "layer1" :name 'layer1 :disabled-for '(layer2 layer3))
           ,(cfgl-layer "layer2" :name 'layer2)
           ,(cfgl-layer "layer2" :name 'layer3)))
        (witness nil)
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun layer1/init-pkg () (push 'init witness))
    (defun layer2/pre-init-pkg () (push 'pre-init witness))
    (defun layer3/post-init-pkg () (push 'post-init witness))
    (mocker-let
     ((spacemacs-buffer/message (m) ((:output nil))))
     (configuration-layer//configure-package pkg)
     (should (equal '(init) witness)))))

;; ---------------------------------------------------------------------------
;; configuration-layer//configure-packages-2
;; ---------------------------------------------------------------------------

(ert-deftest test-configure-packages-2--package-w/-layer-owner-is-configured()
  (let ((pkg (cfgl-package "pkg" :name 'pkg :owners '(layer1)))
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (mocker-let
     ((configuration-layer//configure-package (p) ((:occur 1)))
      (spacemacs-buffer/loading-animation nil ((:output nil))))
     (configuration-layer//configure-packages-2 `(,pkg)))))

(ert-deftest test-configure-packages-2--site-package-is-configured()
  (let ((pkg (cfgl-package "pkg" :name 'pkg :owners '(layer1) :location 'site))
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (mocker-let
     ((configuration-layer//configure-package (p) ((:occur 1)))
      (spacemacs-buffer/loading-animation nil ((:output nil))))
     (configuration-layer//configure-packages-2 `(,pkg)))))

(ert-deftest test-configure-packages-2--toggle-t-is-configured ()
  (let ((pkg (cfgl-package "pkg" :name 'pkg :owners '(layer1) :toggle t))
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (mocker-let
     ((configuration-layer//configure-package (p) ((:occur 1)))
      (spacemacs-buffer/loading-animation nil ((:output nil))))
     (configuration-layer//configure-packages-2 `(,pkg)))))

(ert-deftest test-configure-packages-2--toggle-nil-is-not-configured ()
  (let ((pkg (cfgl-package "pkg" :name 'pkg :owners '(layer1) :toggle nil))
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (mocker-let
     ((configuration-layer//configure-package (p) nil)
      (spacemacs-buffer/loading-animation nil ((:output nil)))
      (spacemacs-buffer/message (m) ((:output nil))))
     (configuration-layer//configure-packages-2 `(,pkg)))))

(ert-deftest test-configure-packages-2--protected-package-is-configured()
  (let ((pkg (cfgl-package "pkg" :name 'pkg :owners '(layer1) :protected t))
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (mocker-let
     ((configuration-layer//configure-package (p) ((:occur 1)))
      (spacemacs-buffer/loading-animation nil ((:output nil))))
     (configuration-layer//configure-packages-2 `(,pkg)))))

(ert-deftest test-configure-packages-2--protected-excluded-package-is-configured()
  (let ((pkg (cfgl-package "pkg" :name 'pkg :owners '(layer1) :excluded t :protected t))
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (mocker-let
     ((configuration-layer//configure-package (p) ((:occur 1)))
      (spacemacs-buffer/loading-animation nil ((:output nil))))
     (configuration-layer//configure-packages-2 `(,pkg)))))

(ert-deftest test-configure-packages-2--excluded-package-is-not-configured()
  (let ((pkg (cfgl-package "pkg" :name 'pkg :owners '(layer1) :excluded t))
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (mocker-let
     ((configuration-layer//configure-package (p) nil)
      (spacemacs-buffer/loading-animation nil ((:output nil)))
      (spacemacs-buffer/message (m) ((:output nil))))
     (configuration-layer//configure-packages-2 `(,pkg)))))

(ert-deftest test-configure-packages-2--package-w/o-owner-is-not-configured()
  (let ((pkg (cfgl-package "pkg" :name 'pkg :owners nil))
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (mocker-let
     ((configuration-layer//configure-package (p) nil)
      (spacemacs-buffer/loading-animation nil ((:output nil)))
      (spacemacs-buffer/message (m) ((:output nil))))
     (configuration-layer//configure-packages-2 `(,pkg)))))

(ert-deftest
    test-configure-packages-2--package-owned-by-dotfile-is-not-configured()
  (let ((pkg (cfgl-package "pkg" :name 'pkg :owners '(dotfile)))
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (mocker-let
     ((configuration-layer//configure-package (p) nil)
      (spacemacs-buffer/loading-animation nil ((:output nil)))
      (spacemacs-buffer/message (m) ((:output nil))))
     (configuration-layer//configure-packages-2 `(,pkg)))))

(ert-deftest test-configure-packages-2--lazy-install-package-is-not-configured()
  (let ((pkg (cfgl-package "pkg" :name 'pkg :owners '(layer) :lazy-install t))
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (mocker-let
     ((configuration-layer//configure-package (p) nil)
      (spacemacs-buffer/loading-animation nil ((:output nil)))
      (spacemacs-buffer/message (m) ((:output nil))))
     (configuration-layer//configure-packages-2 `(,pkg)))))

(ert-deftest
    test-configure-packages-2--local-package-w/-layer-owner-update-load-path()
  (let ((pkg (cfgl-package "pkg" :name 'pkg :owners '(layer1) :location 'local))
        (configuration-layer--layers `(,(cfgl-layer "layer1"
                                                    :name 'layer1
                                                    :dir "/a/path/")))
        (expected-load-path load-path)
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (mocker-let
     ((spacemacs-buffer/loading-animation nil ((:output nil)))
      (configuration-layer//configure-package (p) ((:occur 1))))
     (configuration-layer//configure-packages-2 `(,pkg))
     (push "/a/path/local/pkg/" expected-load-path)
     (should (equal expected-load-path load-path)))))

(ert-deftest
    test-configure-packages-2--local-package-w/-dotfile-owner-update-load-path()
  (let ((pkg (cfgl-package "pkg" :name 'pkg :owners '(dotfile) :location 'local))
        (expected-load-path load-path)
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (mocker-let
     ((spacemacs-buffer/loading-animation nil ((:output nil))))
     (configuration-layer//configure-packages-2 `(,pkg))
     (push (file-name-as-directory
            (concat configuration-layer-private-directory "local/pkg"))
           expected-load-path)
     (should (equal expected-load-path load-path)))))

(ert-deftest
    test-configure-packages-2--local-package-w/o-owner-doesnt-update-load-path()
  (let ((pkg (cfgl-package "pkg" :name 'pkg :owners nil :location 'local))
        (old-load-path load-path)
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (mocker-let
     ((spacemacs-buffer/loading-animation nil ((:output nil)))
      (spacemacs-buffer/message (m) ((:output nil))))
     (configuration-layer//configure-packages-2 `(,pkg))
     (should (equal load-path old-load-path)))))

(ert-deftest
    test-configure-packages-2--local-package-w/-string-location-update-load-path()
  (let ((pkg (cfgl-package "pkg"
                           :name 'pkg
                           :owners '(dotfile)
                           :location spacemacs-docs-directory))
        (expected-load-path load-path)
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (mocker-let
     ((spacemacs-buffer/loading-animation nil ((:output nil))))
     (configuration-layer//configure-packages-2 `(,pkg))
     (push spacemacs-docs-directory expected-load-path)
     (should (equal expected-load-path load-path)))))

(ert-deftest
    test-configure-packages-2--local-package-w/-bad-string-location-gives-warning()
  (let ((pkg (cfgl-package "pkg"
                           :name 'pkg
                           :owners '(dotfile)
                           :location "/this/directory/does/not/exist/"))
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (mocker-let
     ((spacemacs-buffer/loading-animation nil ((:output nil)))
      (configuration-layer//warning
       (msg &rest args)
       ((:record-cls 'mocker-stub-record :output nil :occur 1))))
     (configuration-layer//configure-packages-2 `(,pkg)))))

;; ---------------------------------------------------------------------------
;; configuration-layer//sort-packages
;; ---------------------------------------------------------------------------

(ert-deftest test-sort-packages--example ()
  (let ((pkgs `(,(configuration-layer/make-package 'pkg4)
                ,(configuration-layer/make-package 'pkg3)
                ,(configuration-layer/make-package 'pkg6)
                ,(configuration-layer/make-package 'pkg2)
                ,(configuration-layer/make-package 'pkg1))))
    (should (equal (list (cfgl-package "pkg1" :name 'pkg1)
                         (cfgl-package "pkg2" :name 'pkg2)
                         (cfgl-package "pkg3" :name 'pkg3)
                         (cfgl-package "pkg4" :name 'pkg4)
                         (cfgl-package "pkg6" :name 'pkg6))
                   (configuration-layer//sort-packages pkgs)))))

;; ---------------------------------------------------------------------------
;; configuration-layer//package-has-recipe-p
;; ---------------------------------------------------------------------------

(ert-deftest test-package-has-a-recipe-p--true ()
  (let ((configuration-layer--packages
         `(,(configuration-layer/make-package '(pkg1 :location (recipe blah)))
           ,(configuration-layer/make-package '(pkg2 :location elpa)))))
    (should (configuration-layer//package-has-recipe-p 'pkg1))))

(ert-deftest test-package-has-a-recipe-p--false ()
  (let ((configuration-layer--packages
         `(,(configuration-layer/make-package '(pkg1 :location (recipe blah)))
           ,(configuration-layer/make-package '(pkg2 :location elpa)))))
    (should (not (configuration-layer//package-has-recipe-p 'pkg2)))))

;; ---------------------------------------------------------------------------
;; configuration-layer//get-package-recipe
;; ---------------------------------------------------------------------------

(ert-deftest test-get-package-recipe--return-recipe-if-package-has-one ()
  (let ((configuration-layer--packages
         `(,(configuration-layer/make-package '(pkg1 :location (recipe blah)))
           ,(configuration-layer/make-package '(pkg2 :location elpa)))))
    (should (eq 'recipe
                (car (configuration-layer//get-package-recipe 'pkg1))))))

(ert-deftest test-get-package-recipe--return-nil-if-package-has-no-recipe ()
  (let ((configuration-layer--packages
         `(,(configuration-layer/make-package '(pkg1 :location (recipe blah)))
           ,(configuration-layer/make-package '(pkg2 :location elpa)))))
    (should (not (configuration-layer//get-package-recipe 'pkg2)))))

;; ---------------------------------------------------------------------------
;; configuration-layer/filter-objects
;; ---------------------------------------------------------------------------

(ert-deftest test-filter-packages--example-filter-excluded-packages ()
  (let* ((pkg1 (configuration-layer/make-package 'pkg1))
         (pkg2 (configuration-layer/make-package 'pkg2))
         (pkg3 (configuration-layer/make-package 'pkg3))
         (pkg4 (configuration-layer/make-package 'pkg4))
         (pkg5 (configuration-layer/make-package 'pkg5))
         (pkg6 (configuration-layer/make-package 'pkg6))
         (pkg7 (configuration-layer/make-package 'pkg7))
         (pkg8 (configuration-layer/make-package 'pkg8))
         (pkgs (list pkg1 pkg2 pkg3 pkg4 pkg5 pkg6 pkg7 pkg8)))
    (oset pkg1 :excluded t)
    (oset pkg3 :excluded t)
    (oset pkg5 :excluded t)
    (oset pkg6 :excluded t)
    (should (equal (list (cfgl-package "pkg2" :name 'pkg2)
                         (cfgl-package "pkg4" :name 'pkg4)
                         (cfgl-package "pkg7" :name 'pkg7)
                         (cfgl-package "pkg8" :name 'pkg8))
                   (configuration-layer/filter-objects
                    pkgs (lambda (x)
                           (not (oref x :excluded))))))))

(ert-deftest test-filter-packages--expample-filter-local-packages ()
  (let* ((pkg1 (configuration-layer/make-package 'pkg1))
         (pkg2 (configuration-layer/make-package 'pkg2))
         (pkg3 (configuration-layer/make-package 'pkg3))
         (pkg4 (configuration-layer/make-package 'pkg4))
         (pkg5 (configuration-layer/make-package 'pkg5))
         (pkg6 (configuration-layer/make-package 'pkg6))
         (pkg7 (configuration-layer/make-package 'pkg7))
         (pkg8 (configuration-layer/make-package 'pkg8))
         (pkgs (list pkg1 pkg2 pkg3 pkg4 pkg5 pkg6 pkg7 pkg8)))
    (oset pkg1 :location 'local)
    (oset pkg3 :location 'local)
    (oset pkg5 :location 'local)
    (oset pkg6 :location 'local)
    (should (equal (list (cfgl-package "pkg2" :name 'pkg2)
                         (cfgl-package "pkg4" :name 'pkg4)
                         (cfgl-package "pkg7" :name 'pkg7)
                         (cfgl-package "pkg8" :name 'pkg8))
                   (configuration-layer/filter-objects
                    pkgs (lambda (x)
                           (not (eq 'local (oref x :location)))))))))


(ert-deftest test-filter-packages--example-filter-packages-local-or-excluded ()
  (let* ((pkg1 (configuration-layer/make-package 'pkg1))
         (pkg2 (configuration-layer/make-package 'pkg2))
         (pkg3 (configuration-layer/make-package 'pkg3))
         (pkg4 (configuration-layer/make-package 'pkg4))
         (pkg5 (configuration-layer/make-package 'pkg5))
         (pkg6 (configuration-layer/make-package 'pkg6))
         (pkg7 (configuration-layer/make-package 'pkg7))
         (pkg8 (configuration-layer/make-package 'pkg8))
         (pkgs (list pkg1 pkg2 pkg3 pkg4 pkg5 pkg6 pkg7 pkg8)))
    (oset pkg1 :location 'local)
    (oset pkg1 :excluded t)
    (oset pkg3 :location 'local)
    (oset pkg5 :location 'local)
    (oset pkg6 :location 'local)
    (oset pkg6 :excluded t)
    (oset pkg7 :excluded t)
    (should (equal (list (cfgl-package "pkg2" :name 'pkg2)
                         (cfgl-package "pkg4" :name 'pkg4)
                         (cfgl-package "pkg8" :name 'pkg8))
                   (configuration-layer/filter-objects
                    pkgs (lambda (x)
                           (and (not (eq 'local (oref x :location)))
                                (not (oref x :excluded)))))))))

(ert-deftest test-filter-packages--example-filter-packages-both-local-and-excluded ()
  (let* ((pkg1 (configuration-layer/make-package 'pkg1))
         (pkg2 (configuration-layer/make-package 'pkg2))
         (pkg3 (configuration-layer/make-package 'pkg3))
         (pkg4 (configuration-layer/make-package 'pkg4))
         (pkg5 (configuration-layer/make-package 'pkg5))
         (pkg6 (configuration-layer/make-package 'pkg6))
         (pkg7 (configuration-layer/make-package 'pkg7))
         (pkg8 (configuration-layer/make-package 'pkg8))
         (pkgs (list pkg1 pkg2 pkg3 pkg4 pkg5 pkg6 pkg7 pkg8)))
    (oset pkg1 :location 'local)
    (oset pkg1 :excluded t)
    (oset pkg3 :location 'local)
    (oset pkg5 :excluded t)
    (oset pkg6 :location 'local)
    (oset pkg6 :excluded t)
    (should (equal (list (cfgl-package "pkg2" :name 'pkg2)
                         (cfgl-package "pkg3" :name 'pkg3 :location 'local)
                         (cfgl-package "pkg4" :name 'pkg4)
                         (cfgl-package "pkg5" :name 'pkg5 :excluded t)
                         (cfgl-package "pkg7" :name 'pkg7)
                         (cfgl-package "pkg8" :name 'pkg8))
                   (configuration-layer/filter-objects
                    pkgs (lambda (x)
                           (or (not (eq 'local (oref x :location)))
                               (not (oref x :excluded)))))))))

;; ---------------------------------------------------------------------------
;; configuration-layer/package-usedp
;; ---------------------------------------------------------------------------

(ert-deftest test-package-usedp--package-with-owner-can-be-used ()
  (let* ((layer1 (cfgl-layer "layer1" :name 'layer1 :dir "/path/"))
         (layers (list layer1))
         (layer1-packages '(pkg1 pkg2 pkg3))
         (mocker-mock-default-record-cls 'mocker-stub-record)
         (configuration-layer--packages
          (list (cfgl-package "pkg3" :name 'pkg3 :owners '(layer1))
                (cfgl-package "pkg2" :name 'pkg2 :owners '(layer1))
                (cfgl-package "pkg1" :name 'pkg1 :owners '(layer1)))))
    (should (configuration-layer/package-usedp (nth (random 3)
                                                    layer1-packages)))))

(ert-deftest test-package-usedp--package-with-no-owner-cannot-be-used ()
  (let* ((layer1 (cfgl-layer "layer1" :name 'layer1 :dir "/path/"))
         (layers (list layer1))
         (layer1-packages '(pkg1 pkg2 pkg3))
         (mocker-mock-default-record-cls 'mocker-stub-record)
         (configuration-layer--packages
          (list (cfgl-package "pkg3" :name 'pkg3)
                (cfgl-package "pkg2" :name 'pkg2)
                (cfgl-package "pkg1" :name 'pkg1))))
    (should (null (configuration-layer/package-usedp (nth (random 3)
                                                          layer1-packages))))))

;; ---------------------------------------------------------------------------
;; configuration-layer//directory-type
;; ---------------------------------------------------------------------------

(ert-deftest test-directory-type--input-is-a-file ()
  (let ((input "/a/path/to/a/layer/file"))
    (mocker-let
     ((file-directory-p (f)
                        ((:record-cls 'mocker-stub-record
                                      :output nil
                                      :occur 1))))
     (should (null (configuration-layer//directory-type input))))))

(ert-deftest test-directory-type--category ()
  (let ((input (concat configuration-layer-directory "+vim/")))
    (mocker-let
     ((file-directory-p (f)
                        ((:record-cls 'mocker-stub-record :output t :occur 1))))
     (should (eq 'category (configuration-layer//directory-type input))))))

(ert-deftest test-directory-type--input-is-an-empty-directory ()
  (let ((input "/a/path/to/a/layer/"))
    (mocker-let
     ((file-directory-p (f)
                        ((:record-cls 'mocker-stub-record :output t :occur 1)))
      (directory-files
       (directory &optional full match nosort)
       ((:record-cls 'mocker-stub-record :output nil :occur 1))))
     (should (null (configuration-layer//directory-type input))))))

(ert-deftest test-directory-type--input-is-directory-and-not-a-layer ()
  (let ((input "/a/path/to/a/layer/"))
    (mocker-let
     ((file-directory-p (f)
                        ((:record-cls 'mocker-stub-record :output t :occur 1)))
      (directory-files
       (directory &optional full match nosort)
       ((:record-cls 'mocker-stub-record
                     :output '("toto.el" "tata.el")
                     :occur 1))))
     (should (null (configuration-layer//directory-type input))))))

(ert-deftest test-directory-type--layer-with-packages.el ()
  (let ((input "/a/path/to/a/layer/"))
    (mocker-let
     ((file-directory-p (f)
                        ((:record-cls 'mocker-stub-record :output t :occur 1)))
      (directory-files
       (directory &optional full match nosort)
       ((:record-cls 'mocker-stub-record :output '("packages.el") :occur 1))))
     (should (eq 'layer (configuration-layer//directory-type input))))))

(ert-deftest test-directory-type--layer-with-config.el ()
  (let ((input "/a/path/to/a/layer/"))
    (mocker-let
     ((file-directory-p (f)
                        ((:record-cls 'mocker-stub-record :output t :occur 1)))
      (directory-files
       (directory &optional full match nosort)
       ((:record-cls 'mocker-stub-record :output '("config.el") :occur 1))))
     (should (eq 'layer (configuration-layer//directory-type input))))))

(ert-deftest test-directory-type--layer-with-keybindings.el ()
  (let ((input "/a/path/to/a/layer/"))
    (mocker-let
     ((file-directory-p (f)
                        ((:record-cls 'mocker-stub-record :output t :occur 1)))
      (directory-files
       (directory &optional full match nosort)
       ((:record-cls 'mocker-stub-record
                     :output '("keybindings.el")
                     :occur 1))))
     (should (eq 'layer (configuration-layer//directory-type input))))))

(ert-deftest test-directory-type--layer-with-funcs.el ()
  (let ((input "/a/path/to/a/layer/"))
    (mocker-let
     ((file-directory-p (f)
                        ((:record-cls 'mocker-stub-record :output t :occur 1)))
      (directory-files
       (directory &optional full match nosort)
       ((:record-cls 'mocker-stub-record :output '("funcs.el") :occur 1))))
     (should (eq 'layer (configuration-layer//directory-type input))))))

;; ---------------------------------------------------------------------------
;; configuration-layer//get-category-from-path
;; ---------------------------------------------------------------------------

(ert-deftest test-get-category-from-path--input-is-a-file ()
  (let ((input "/a/path/to/a/file"))
    (mocker-let
     ((file-directory-p (f)
                        ((:record-cls 'mocker-stub-record
                                      :output nil
                                      :occur 1))))
     (should (null (configuration-layer//get-category-from-path input))))))

(ert-deftest test-get-category-from-path--input-is-a-regular-directory ()
  (let ((input "/a/path/to/a/layer/"))
    (mocker-let
     ((file-directory-p (f)
                        ((:record-cls 'mocker-stub-record
                                      :output t
                                      :occur 1))))
     (should (null (configuration-layer//get-category-from-path input))))))

(ert-deftest test-get-category-from-path--return-category ()
  (let ((input "/a/path/to/a/+cat/"))
    (mocker-let
     ((file-directory-p (f)
                        ((:record-cls 'mocker-stub-record :output t :occur 1))))
     (should (eq 'cat (configuration-layer//get-category-from-path input))))))

;; ---------------------------------------------------------------------------
;; configuration-layer//gather-auto-mode-extensions
;; ---------------------------------------------------------------------------

(ert-deftest test-gather-auto-mode-extensions--one-entry-in-auto-mode-alist ()
  (let ((auto-mode-alist '(("\\.spacemacs\\'" . mode))))
    (should (equal
             "\\(\\.spacemacs\\'\\)"
             (configuration-layer//gather-auto-mode-extensions 'mode)))))

(ert-deftest test-gather-auto-mode-extensions--several-entries-in-auto-mode-alist ()
  (let ((auto-mode-alist '(("\\.spacemacs\\'" . mode)
                           ("\\.dotspacemacs\\'" . mode)
                           ("\\.spacelayer\\'" . mode))))
    (should (equal
             "\\(\\.spacelayer\\'\\|\\.dotspacemacs\\'\\|\\.spacemacs\\'\\)"
             (configuration-layer//gather-auto-mode-extensions 'mode)))))

(ert-deftest test-gather-auto-mode-extensions--ext-entry-is-not-symbol ()
  (let ((auto-mode-alist '(((nil t) . mode))))
    (should (null (configuration-layer//gather-auto-mode-extensions 'mode)))))

(ert-deftest test-gather-auto-mode-extensions--mode-entry-is-not-symbol ()
  (let ((auto-mode-alist '(("ext" . (lambda nil nil)))))
    (should (null (configuration-layer//gather-auto-mode-extensions 'mode)))))

(ert-deftest test-gather-auto-mode-extensions--regexp-correctness ()
  "Correctness is a big word here :-)"
  (let ((regexp (configuration-layer//gather-auto-mode-extensions
                 'emacs-lisp-mode)))
    (should (string-match-p regexp "/_emacs"))
    (should (string-match-p regexp "/.toto_gnus"))
    (should (string-match-p regexp "/.toto_viper"))
    (should (string-match-p regexp "/toto/emacs.el"))
    (should (string-match-p regexp "/toto/project.ede"))
    (should (not (string-match-p regexp "/toto/emacs.dummy")))))

;; ---------------------------------------------------------------------------
;; configuration-layer//lazy-install-extensions-for-layer
;; ---------------------------------------------------------------------------

(ert-deftest test-lazy-install-extensions-for-layer--owned-packages ()
  (let ((configuration-layer--layers
         (list (cfgl-layer "layer" :name 'layer :packages '(pkg1 pkg2))))
        (configuration-layer--packages
         (list (cfgl-package "pkg1" :name 'pkg1 :owners '(layer))
               (cfgl-package "pkg2" :name 'pkg2 :owners '(layer))))
        (auto-mode-alist '(("\\.pkg1\\'" . pkg1)
                           ("\\.pkg2\\'" . pkg2))))
    (should (equal '((pkg2 . "\\(\\.pkg2\\'\\)")
                     (pkg1 . "\\(\\.pkg1\\'\\)"))
                   (configuration-layer//lazy-install-extensions-for-layer 'layer)))))

(ert-deftest test-lazy-install-extensions-for-layer--not-owned-package ()
  (let ((configuration-layer--layers
         (list (cfgl-layer "layer" :name 'layer :packages '(pkg1))))
        (configuration-layer--packages
         (list (cfgl-package "pkg1" :name 'pkg1 :owners '(other))))
        (auto-mode-alist '(("\\.pkg1\\'" . pkg1))))
    (should (null (configuration-layer//lazy-install-extensions-for-layer 'layer)))))

;; ---------------------------------------------------------------------------
;; configuration-layer//insert-lazy-install-form
;; ---------------------------------------------------------------------------

(ert-deftest test-insert-lazy-install-form ()
  (cl-letf (((symbol-function 'insert) 'identity))
    (should
     (equal
      (concat "(configuration-layer/lazy-install 'layer "
              ":extensions '(\"\\\\(\\\\.ext\\\\'\\\\)\" mode))\n")
      (configuration-layer//insert-lazy-install-form 'layer 'mode "\\(\\.ext\\'\\)")))))

;; ---------------------------------------------------------------------------
;; configuration-layer/configured-packages-stats
;; ---------------------------------------------------------------------------

(ert-deftest test-configured-packages-stats--correct-counts ()
  (let ((packages
         (list (cfgl-package "pkg1" :name 'pkg1 :location 'built-in)
               (cfgl-package "pkg2" :name 'pkg2 :location 'built-in)
               (cfgl-package "pkg3" :name 'pkg3 :location 'elpa)
               (cfgl-package "pkg4" :name 'pkg4 :location 'elpa)
               (cfgl-package "pkg5" :name 'pkg5 :location 'elpa)
               (cfgl-package "pkg6" :name 'pkg6 :location 'local)
               (cfgl-package "pkg7" :name 'pkg7 :location '(recipe :foo bar))
               (cfgl-package "pkg8" :name 'pkg8 :location '(recipe :foo bar)))))
    (should (equal '((total 8)
                     (elpa 3)
                     (recipe 2)
                     (local 1)
                     (built-in 2))
                   (configuration-layer/configured-packages-stats packages)))))

(ert-deftest test-configured-packages-stats--sum-is-correct ()
  (let* ((packages
          (list (cfgl-package "pkg1" :name 'pkg1 :location 'built-in)
                (cfgl-package "pkg2" :name 'pkg2 :location 'built-in)
                (cfgl-package "pkg3" :name 'pkg3 :location 'elpa)
                (cfgl-package "pkg4" :name 'pkg4 :location 'elpa)
                (cfgl-package "pkg5" :name 'pkg5 :location 'elpa)
                (cfgl-package "pkg6" :name 'pkg6 :location 'local)
                (cfgl-package "pkg7" :name 'pkg7 :location '(recipe :foo bar))
                (cfgl-package "pkg8" :name 'pkg8 :location '(recipe :foo bar))))
         (stats (configuration-layer/configured-packages-stats packages)))
    (should (equal 8 (+ (cadr (assq 'elpa stats))
                        (cadr (assq 'recipe stats))
                        (cadr (assq 'local stats))
                        (cadr (assq 'built-in stats)))))))
