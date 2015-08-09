;;; core-configuration-layer-utest.el --- Spacemacs Unit Test File
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(require 'mocker)
(require 'core-configuration-layer)

;; ---------------------------------------------------------------------------
;; configuration-layer//declare-layers
;; ---------------------------------------------------------------------------

(ert-deftest test-declare-layers--result-order-is-not-reversed ()
  (mocker-let ((configuration-layer/make-layer
                (x)
                ((:input '(layer3) :output 'layer3)
                 (:input '(layer2) :output 'layer2)
                 (:input '(layer1) :output 'layer1))))
    (let* ((input '(layer1 layer2 layer3))
           (result (configuration-layer//declare-layers input)))
      (should (equal result input)))))

(ert-deftest test-declare-layers--ignore-not-found-layer ()
  (mocker-let ((configuration-layer/make-layer
                (x)
                ((:input '(layer3) :output 'layer3)
                 (:input '(layer2-not-found) :output nil)
                 (:input '(layer1) :output 'layer1))))
              (let* ((input '(layer1 layer2-not-found layer3))
                     (expected '(layer1 layer3))
                     (result (configuration-layer//declare-layers input)))
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
     ((spacemacs-buffer/warning
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

(ert-deftest test-make-package--input-is-a-symbol ()
  (let* ((input 'testpkg)
         (expected (cfgl-package "testpkg"
                                 :name 'testpkg
                                 :location 'elpa
                                 :owner nil
                                 :pre-layers nil
                                 :post-layers nil
                                 :step nil
                                 :excluded nil))
         (result (configuration-layer/make-package input)))
    (should (equal result expected))))

(ert-deftest test-make-package--input-is-a-list ()
  (let* ((input '(testpkg :location local :step pre))
         (expected (cfgl-package "testpkg"
                                 :name 'testpkg
                                 :owner nil
                                 :location 'local
                                 :pre-layers nil
                                 :post-layers nil
                                 :step 'pre
                                 :excluded nil))
         (result (configuration-layer/make-package input)))
    (should (equal result expected))))

;; ---------------------------------------------------------------------------
;; configuration-layer/get-packages
;; ---------------------------------------------------------------------------

(ert-deftest test-get-packages--symbols-only ()
  (let* ((layer1 (cfgl-layer "layer1" :name 'layer1 :dir "/path"))
         (layers (list layer1))
         (layer1-packages '(pkg1 pkg2 pkg3))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun layer1/init-pkg1 nil)
    (defun layer1/init-pkg2 nil)
    (defun layer1/init-pkg3 nil)
    (mocker-let ((file-exists-p (f) ((:output t :occur 1)
                                     (:output nil :occur 1)))
                 (configuration-layer/layer-usedp (l) ((:output t :occur 1))))
                (should (equal (list (cfgl-package "pkg3" :name 'pkg3 :owner 'layer1)
                                     (cfgl-package "pkg2" :name 'pkg2 :owner 'layer1)
                                     (cfgl-package "pkg1" :name 'pkg1 :owner 'layer1))
                               (configuration-layer/get-packages layers))))))

(ert-deftest test-get-packages--lists-only ()
  (let* ((layer1 (cfgl-layer "layer1" :name 'layer1 :dir "/path"))
         (layers (list layer1))
         (layer1-packages '((pkg1 :location elpa :excluded t)
                            (pkg2 :location recipe)
                            (pkg3 :location local :step pre)))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun layer1/init-pkg1 nil)
    (defun layer1/init-pkg2 nil)
    (defun layer1/init-pkg3 nil)
    (mocker-let ((file-exists-p (f) ((:output t :occur 1)
                                     (:output nil :occur 1)))
                 (configuration-layer/layer-usedp (l) ((:output t :occur 1))))
                (should (equal (list (cfgl-package "pkg3" :name 'pkg3 :owner 'layer1 :location 'local :step 'pre)
                                     (cfgl-package "pkg2" :name 'pkg2 :owner 'layer1 :location 'recipe)
                                     (cfgl-package "pkg1" :name 'pkg1 :owner 'layer1 :excluded t))
                               (configuration-layer/get-packages layers))))))

(ert-deftest test-get-packages--symbols-and-lists ()
  (let* ((layer1 (cfgl-layer "layer1" :name 'layer1 :dir "/path"))
         (layers (list layer1))
         (layer1-packages '(pkg1
                            (pkg2 :location recipe)
                            (pkg3 :location local :step pre)
                            pkg4))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun layer1/init-pkg1 nil)
    (defun layer1/init-pkg2 nil)
    (defun layer1/init-pkg3 nil)
    (defun layer1/init-pkg4 nil)
    (mocker-let ((file-exists-p (f) ((:output t :occur 1)
                                     (:output nil :occur 1)))
                 (configuration-layer/layer-usedp (l) ((:output t :occur 1))))
                (should (equal (list (cfgl-package "pkg4" :name 'pkg4 :owner 'layer1)
                                     (cfgl-package "pkg3" :name 'pkg3 :owner 'layer1 :location 'local :step 'pre)
                                     (cfgl-package "pkg2" :name 'pkg2 :owner 'layer1 :location 'recipe)
                                     (cfgl-package "pkg1" :name 'pkg1 :owner 'layer1))
                               (configuration-layer/get-packages layers))))))

(ert-deftest test-get-packages--pkg2-has-no-owner-because-no-init-function ()
  (let* ((layer2 (cfgl-layer "layer2" :name 'layer2 :dir "/path"))
         (layers (list layer2))
         (layer2-packages '(pkg1 pkg2 pkg3))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun layer2/init-pkg1 nil)
    (defun layer2/init-pkg3 nil)
    (mocker-let ((file-exists-p (f) ((:output t :occur 1)
                                     (:output nil :occur 1)))
                 (configuration-layer/layer-usedp (l) ((:output t :occur 1))))
                (should (equal (list (cfgl-package "pkg3" :name 'pkg3 :owner 'layer2)
                                     (cfgl-package "pkg2" :name 'pkg2)
                                     (cfgl-package "pkg1" :name 'pkg1 :owner 'layer2))
                               (configuration-layer/get-packages layers))))))

(ert-deftest test-get-packages--pre-init-function ()
  (let* ((layer3 (cfgl-layer "layer3" :name 'layer3 :dir "/path"))
         (layer4 (cfgl-layer "layer4" :name 'layer4 :dir "/path"))
         (layers (list layer3 layer4))
         (layer3-packages '(pkg1))
         (layer4-packages '(pkg1))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun layer3/init-pkg1 nil)
    (defun layer4/pre-init-pkg1 nil)
    (mocker-let ((file-exists-p (f) ((:output t :occur 1)
                                     (:output nil :occur 1)
                                     (:output t :occur 1)
                                     (:output nil :occur 1)))
                 (configuration-layer/layer-usedp (l) ((:output t :occur 2))))
                (should (equal (list (cfgl-package "pkg1" :name 'pkg1 :owner 'layer3 :pre-layers '(layer4)))
                               (configuration-layer/get-packages layers))))))

(ert-deftest test-get-packages--post-init-function ()
  (let* ((layer3 (cfgl-layer "layer3" :name 'layer3 :dir "/path"))
         (layer5 (cfgl-layer "layer5" :name 'layer5 :dir "/path"))
         (layers (list layer3 layer5))
         (layer3-packages '(pkg1))
         (layer5-packages '(pkg1))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun layer3/init-pkg1 nil)
    (defun layer5/post-init-pkg1 nil)
    (mocker-let ((file-exists-p (f) ((:output t :occur 1)
                                     (:output nil :occur 1)
                                     (:output t :occur 1)
                                     (:output nil :occur 1)))
                 (configuration-layer/layer-usedp (l) ((:output t :occur 2))))
                (should (equal (list (cfgl-package "pkg1" :name 'pkg1 :owner 'layer3 :post-layers '(layer5)))
                               (configuration-layer/get-packages layers))))))

(ert-deftest test-get-packages--pre-and-post-init-functions ()
  (let* ((layer3 (cfgl-layer "layer3" :name 'layer3 :dir "/path"))
         (layer6 (cfgl-layer "layer6" :name 'layer6 :dir "/path"))
         (layers (list layer3 layer6))
         (layer3-packages '(pkg1))
         (layer6-packages '(pkg1))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun layer3/init-pkg1 nil)
    (defun layer6/pre-init-pkg1 nil)
    (defun layer6/post-init-pkg1 nil)
    (mocker-let ((file-exists-p (f) ((:output t :occur 1)
                                     (:output nil :occur 1)
                                     (:output t :occur 1)
                                     (:output nil :occur 1)))
                 (configuration-layer/layer-usedp (l) ((:output t :occur 2))))
                (should (equal (list (cfgl-package "pkg1" :name 'pkg1 :owner 'layer3 :pre-layers '(layer6) :post-layers '(layer6)))
                               (configuration-layer/get-packages layers))))))

(ert-deftest test-get-packages--several-init-functions-last-one-is-the-owner ()
  (let* ((layer7 (cfgl-layer "layer7" :name 'layer7 :dir "/path"))
         (layer8 (cfgl-layer "layer8" :name 'layer8 :dir "/path"))
         (layers (list layer7 layer8))
         (layer7-packages '(pkg1))
         (layer8-packages '(pkg1))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun layer7/init-pkg1 nil)
    (defun layer8/init-pkg1 nil)
    (mocker-let ((file-exists-p (f) ((:output t :occur 1)
                                     (:output nil :occur 1)
                                     (:output t :occur 1)
                                     (:output nil :occur 1)))
                 (configuration-layer/layer-usedp (l) ((:output t :occur 2))))
                (should (equal (list (cfgl-package "pkg1" :name 'pkg1 :owner 'layer8))
                               (configuration-layer/get-packages layers))))))

(ert-deftest test-get-packages--layer-10-excludes-pkg2-in-layer-9 ()
  (let* ((layer9 (cfgl-layer "layer9" :name 'layer9 :dir "/path"))
         (layer10 (cfgl-layer "layer10" :name 'layer10 :dir "/path"))
         (layers (list layer9 layer10))
         (layer9-packages '(pkg1 pkg2))
         (layer10-packages '(pkg3))
         (layer10-excluded-packages '(pkg2))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun layer9/init-pkg1 nil)
    (defun layer9/init-pkg2 nil)
    (defun layer10/init-pkg3 nil)
    (mocker-let ((file-exists-p (f) ((:output t :occur 1)
                                     (:output nil :occur 1)
                                     (:output t :occur 1)
                                     (:output nil :occur 1)))
                 (configuration-layer/layer-usedp (l) ((:output t :occur 2))))
                (should (equal (list (cfgl-package "pkg3" :name 'pkg3 :owner 'layer10)
                                     (cfgl-package "pkg2" :name 'pkg2 :owner 'layer9 :excluded t)
                                     (cfgl-package "pkg1" :name 'pkg1 :owner 'layer9))
                               (configuration-layer/get-packages layers))))))

(ert-deftest test-get-packages--dotfile-excludes-pkg2-in-layer-11 ()
  (let* ((layer11 (cfgl-layer "layer11" :name 'layer11 :dir "/path"))
         (layers (list layer11))
         (layer11-packages '(pkg1 pkg2 pkg3))
         (dotspacemacs-excluded-packages '(pkg2))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun layer11/init-pkg1 nil)
    (defun layer11/init-pkg2 nil)
    (defun layer11/init-pkg3 nil)
    (mocker-let ((file-exists-p (f) ((:output t :occur 1)
                                     (:output nil :occur 1)))
                 (configuration-layer/layer-usedp (l) ((:output t :occur 1))))
                (should (equal (list (cfgl-package "pkg3" :name 'pkg3 :owner 'layer11)
                                     (cfgl-package "pkg2" :name 'pkg2 :owner 'layer11 :excluded t)
                                     (cfgl-package "pkg1" :name 'pkg1 :owner 'layer11))
                               (configuration-layer/get-packages layers t))))))

(ert-deftest test-get-packages--dotfile-declares-and-owns-one-additional-package ()
  (let* ((layer12 (cfgl-layer "layer12" :name 'layer12 :dir "/path"))
         (layers (list layer12))
         (layer12-packages '(pkg1 pkg2))
         (dotspacemacs-additional-packages '(pkg3))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun layer12/init-pkg1 nil)
    (defun layer12/init-pkg2 nil)
    (mocker-let ((file-exists-p (f) ((:output t :occur 1)
                                     (:output nil :occur 1)))
                 (configuration-layer/layer-usedp (l) ((:output t :occur 1))))
                (should (equal (list (cfgl-package "pkg3" :name 'pkg3 :owner 'dotfile)
                                     (cfgl-package "pkg2" :name 'pkg2 :owner 'layer12)
                                     (cfgl-package "pkg1" :name 'pkg1 :owner 'layer12))
                               (configuration-layer/get-packages layers t))))))

;; TODO remove extensions tests in 0.105.0

(ert-deftest test-get-packages--pre-extensions-backward-compatibility ()
  (let* ((layer1 (cfgl-layer "layer1" :name 'layer1 :dir "/path"))
         (layers (list layer1))
         (layer1-packages '(pkg1))
         (layer1-pre-extensions '(ext1 ext2 ext3))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun layer1/init-pkg1 nil)
    (defun layer1/init-ext1 nil)
    (defun layer1/init-ext2 nil)
    (defun layer1/init-ext3 nil)
    (mocker-let ((file-exists-p (f) ((:output t :occur 2)))
                 (configuration-layer/layer-usedp (l) ((:output t :occur 2))))
                (should (equal (list (cfgl-package "ext3" :name 'ext3 :owner 'layer1 :location 'local :step 'pre)
                                     (cfgl-package "ext2" :name 'ext2 :owner 'layer1 :location 'local :step 'pre)
                                     (cfgl-package "ext1" :name 'ext1 :owner 'layer1 :location 'local :step 'pre)
                                     (cfgl-package "pkg1" :name 'pkg1 :owner 'layer1))
                               (configuration-layer/get-packages layers))))))

(ert-deftest test-get-packages--post-extensions-backward-compatibility ()
  (let* ((layer1 (cfgl-layer "layer1" :name 'layer1 :dir "/path"))
         (layers (list layer1))
         (layer1-packages '(pkg1))
         (layer1-post-extensions '(ext1 ext2 ext3))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun layer1/init-pkg1 nil)
    (defun layer1/init-ext1 nil)
    (defun layer1/init-ext2 nil)
    (defun layer1/init-ext3 nil)
    (mocker-let ((file-exists-p (f) ((:output t :occur 2)))
                 (configuration-layer/layer-usedp (l) ((:output t :occur 2))))
                (should (equal (list (cfgl-package "ext3" :name 'ext3 :owner 'layer1 :location 'local :step 'post)
                                     (cfgl-package "ext2" :name 'ext2 :owner 'layer1 :location 'local :step 'post)
                                     (cfgl-package "ext1" :name 'ext1 :owner 'layer1 :location 'local :step 'post)
                                     (cfgl-package "pkg1" :name 'pkg1 :owner 'layer1))
                               (configuration-layer/get-packages layers))))))

(ert-deftest test-get-packages--extensions-backward-compatibility ()
  (let* ((layer1 (cfgl-layer "layer1" :name 'layer1 :dir "/path"))
         (layers (list layer1))
         (layer1-packages '(pkg1))
         (layer1-pre-extensions '(ext1 ext2 ext3))
         (layer1-post-extensions '(ext4 ext5 ext6))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun layer1/init-pkg1 nil)
    (defun layer1/init-ext1 nil)
    (defun layer1/init-ext2 nil)
    (defun layer1/init-ext3 nil)
    (defun layer1/init-ext4 nil)
    (defun layer1/init-ext5 nil)
    (defun layer1/init-ext6 nil)
    (mocker-let ((file-exists-p (f) ((:output t :occur 2)))
                 (configuration-layer/layer-usedp (l) ((:output t :occur 2))))
                (should (equal (list (cfgl-package "ext6" :name 'ext6 :owner 'layer1 :location 'local :step 'post)
                                     (cfgl-package "ext5" :name 'ext5 :owner 'layer1 :location 'local :step 'post)
                                     (cfgl-package "ext4" :name 'ext4 :owner 'layer1 :location 'local :step 'post)
                                     (cfgl-package "ext3" :name 'ext3 :owner 'layer1 :location 'local :step 'pre)
                                     (cfgl-package "ext2" :name 'ext2 :owner 'layer1 :location 'local :step 'pre)
                                     (cfgl-package "ext1" :name 'ext1 :owner 'layer1 :location 'local :step 'pre)
                                     (cfgl-package "pkg1" :name 'pkg1 :owner 'layer1))
                               (configuration-layer/get-packages layers))))))

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
;; configuration-layer/filter-packages
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
                   (configuration-layer/filter-packages
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
                   (configuration-layer/filter-packages
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
                   (configuration-layer/filter-packages
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
                   (configuration-layer/filter-packages
                    pkgs (lambda (x)
                           (or (not (eq 'local (oref x :location)))
                               (not (oref x :excluded)))))))))

;; ---------------------------------------------------------------------------
;; configuration-layer//directory-type
;; ---------------------------------------------------------------------------

(ert-deftest test-directory-type--input-is-a-file ()
  (let ((input "/a/path/to/a/layer/file"))
    (mocker-let
     ((file-directory-p (f)
                        ((:record-cls 'mocker-stub-record :output nil :occur 1))))
     (should (null (configuration-layer//directory-type input))))))

(ert-deftest test-directory-type--category ()
  (let ((input (concat configuration-layer-contrib-directory "!vim/")))
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
       ((:record-cls 'mocker-stub-record :output '("toto.el" "tata.el") :occur 1))))
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

(ert-deftest test-directory-type--layer-with-extensions.el ()
  (let ((input "/a/path/to/a/layer/"))
    (mocker-let
     ((file-directory-p (f)
                        ((:record-cls 'mocker-stub-record :output t :occur 1)))
      (directory-files
       (directory &optional full match nosort)
       ((:record-cls 'mocker-stub-record :output '("extensions.el") :occur 1))))
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
       ((:record-cls 'mocker-stub-record :output '("keybindings.el") :occur 1))))
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
                        ((:record-cls 'mocker-stub-record :output nil :occur 1))))
     (should (null (configuration-layer//get-category-from-path input))))))

(ert-deftest test-get-category-from-path--input-is-a-regular-directory ()
  (let ((input "/a/path/to/a/layer/"))
    (mocker-let
     ((file-directory-p (f)
                        ((:record-cls 'mocker-stub-record :output t :occur 1))))
     (should (null (configuration-layer//get-category-from-path input))))))

(ert-deftest test-get-category-from-path--return-category ()
  (let ((input "/a/path/to/a/!cat/"))
    (mocker-let
     ((file-directory-p (f)
                        ((:record-cls 'mocker-stub-record :output t :occur 1))))
     (should (eq 'cat (configuration-layer//get-category-from-path input))))))
