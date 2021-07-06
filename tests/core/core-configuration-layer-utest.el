;;; core-configuration-layer-utest.el --- Spacemacs Unit Test File
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


(require 'mocker)
(require 'core-command-line)
(require 'core-configuration-layer)

(defun helper--add-layers (layers &optional usedp)
  "Set the layer variables given a list of LAYERS objects."
  (dolist (layer layers)
    (configuration-layer//add-layer layer usedp))
  ;; hackish but we need to reverse the list in order to have the layer
  ;; in the correct order (this reverse in normally performed in function
  ;; configuration-layer//declare-used-layers )
  (when usedp
    (setq configuration-layer--used-layers
          (reverse configuration-layer--used-layers))))

(defun helper--add-packages (packages &optional usedp)
  "Set the package variables given a list of PACKAGES objects."
  (dolist (pkg packages)
    (configuration-layer//add-package pkg usedp)))

;; ---------------------------------------------------------------------------
;; class cfgl-layer
;; ---------------------------------------------------------------------------

;; method: cfgl-layer-owned-packages

(ert-deftest test-cfgl-layer-owned-packages--owns-packages-without-props ()
  (let ((layer1 (cfgl-layer "layer1"
                            :name 'layer1
                            :packages '(pkg1
                                        (pkg2 :location foo)
                                        (pkg3 :toggle (eq 1 2))
                                        (pkg4 :toggle (eq 1 1)))))
        configuration-layer--used-packages
        (configuration-layer--indexed-packages (make-hash-table :size 2048)))
    (helper--add-packages
     (list (cfgl-package "pkg1" :name 'pkg1 :owners '(layer2))
           (cfgl-package "pkg2" :name 'pkg2 :owners '(layer1))
           (cfgl-package "pkg3" :name 'pkg3 :owners '(layer1))
           (cfgl-package "pkg4" :name 'pkg4 :owners '(layer2))) t)
    (should (equal '(pkg2 pkg3)
                   (cfgl-layer-owned-packages layer1)))))

(ert-deftest test-cfgl-layer-owned-packages--owns-packages-with-props ()
  (let ((layer1 (cfgl-layer "layer1"
                            :name 'layer1
                            :packages '(pkg1
                                        (pkg2 :location foo)
                                        (pkg3 :toggle (eq 1 2))
                                        (pkg4 :toggle (eq 1 1))
                                        pkg5)))
        configuration-layer--used-packages
        (configuration-layer--indexed-packages (make-hash-table :size 2048)))
    (helper--add-packages
     (list (cfgl-package "pkg1" :name 'pkg1 :owners '(layer1))
           (cfgl-package "pkg2" :name 'pkg2 :owners '(layer1))
           (cfgl-package "pkg3" :name 'pkg3 :owners '(layer1))
           (cfgl-package "pkg4" :name 'pkg4 :owners '(layer2))
           (cfgl-package "pkg5" :name 'pkg5 :owners '(layer2))) t)
    (should (equal '(pkg1 (pkg2 :location foo)
                          (pkg3 :toggle (eq 1 2)))
                   (cfgl-layer-owned-packages layer1 'with-props)))))

(ert-deftest test-cfgl-layer-owned-packages--nil-layer-returns-nil ()
  (should (null (cfgl-layer-owned-packages nil))))

(ert-deftest test-cfgl-layer-owned-packages--nil-layer-returns-nil-with-props ()
  (should (null (cfgl-layer-owned-packages nil 'with-props))))

;; method: cfgl-layer-get-packages

(ert-deftest test-cfgl-layer-get-packages--all-packages-selected-default ()
  (let ((layer (cfgl-layer "layer"
                           :name 'layer
                           :packages '((pkg1 :location local)
                                       pkg2
                                       (pkg3 :location built-in)))))
    (should (equal '(pkg1 pkg2 pkg3)
                   (cfgl-layer-get-packages layer)))))

(ert-deftest test-cfgl-layer-get-packages--all-packages-selected-default-with-props ()
  (let ((layer (cfgl-layer "layer"
                           :name 'layer
                           :packages '((pkg1 :location local)
                                       pkg2
                                       (pkg3 :location built-in)))))
    (should (equal '((pkg1 :location local) pkg2 (pkg3 :location built-in))
                   (cfgl-layer-get-packages layer 'with-props)))))

(ert-deftest test-cfgl-layer-get-packages--all-packages-selected-explicitly ()
  (let ((layer (cfgl-layer "layer"
                           :name 'layer
                           :packages '((pkg1 :location local)
                                       pkg2
                                       (pkg3 :location built-in))
                           :selected-packages 'all)))
    (should (equal '(pkg1 pkg2 pkg3)
                   (cfgl-layer-get-packages layer)))))

(ert-deftest test-cfgl-layer-get-packages--all-packages-selected-explicitly-with-props ()
  (let ((layer (cfgl-layer "layer"
                           :name 'layer
                           :packages '((pkg1 :location local)
                                       pkg2
                                       (pkg3 :location built-in))
                           :selected-packages 'all)))
    (should (equal '((pkg1 :location local) pkg2 (pkg3 :location built-in))
                   (cfgl-layer-get-packages layer 'with-props)))))

(ert-deftest test-cfgl-layer-get-packages--selected-packages ()
  (let ((layer (cfgl-layer "layer"
                           :name 'layer
                           :packages '((pkg1 :location local)
                                       pkg2
                                       (pkg3 :location built-in))
                           :selected-packages '(pkg1 pkg2))))
    (should (equal '(pkg1 pkg2)
                   (cfgl-layer-get-packages layer)))))

(ert-deftest test-cfgl-layer-get-packages--selected-packages-with-props ()
  (let ((layer (cfgl-layer "layer"
                           :name 'layer
                           :packages '((pkg1 :location local)
                                       pkg2
                                       (pkg3 :location built-in))
                           :selected-packages '(pkg1 pkg2))))
    (should (equal '((pkg1 :location local) pkg2)
                   (cfgl-layer-get-packages layer 'with-props)))))

(ert-deftest test-cfgl-layer-get-packages--selected-packages-ignore-unknown ()
  (let ((layer (cfgl-layer "layer"
                           :name 'layer
                           :packages '((pkg1 :location local)
                                       pkg2
                                       (pkg3 :location built-in))
                           :selected-packages '(pkg1 pkg2 pkg-unknown))))
    (should (equal '(pkg1 pkg2)
                   (cfgl-layer-get-packages layer)))))

(ert-deftest test-cfgl-layer-get-packages--selected-packages-ignore-unknown-with-props ()
  (let ((layer (cfgl-layer "layer"
                           :name 'layer
                           :packages '((pkg1 :location local)
                                       pkg2
                                       (pkg3 :location built-in))
                           :selected-packages '(pkg1 pkg2 pkg-unknown))))
    (should (equal '((pkg1 :location local) pkg2)
                   (cfgl-layer-get-packages layer 'with-props)))))

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

;; method: cfgl-layer-get-shadowing-layers

(ert-deftest test-cfgl-layer-get-shadowing-layers--l2-declared-after-l1-shadows-l1 ()
  (let ((layer1 (cfgl-layer "layer1" :name 'layer1))
        (layer2 (cfgl-layer "layer2" :name 'layer2))
        (configuration-layer--used-layers nil)
        (configuration-layer--indexed-layers (make-hash-table :size 1024)))
    (helper--add-layers `(,layer1 ,layer2) 'used)
    (configuration-layer/declare-shadow-relation 'layer1 'layer2)
    (should (and (equal '(layer2) (cfgl-layer-get-shadowing-layers layer1))
                 (equal '() (cfgl-layer-get-shadowing-layers layer2))))))

(ert-deftest test-cfgl-layer-get-shadowing-layers--l1-declared-after-l2-shadows-l2 ()
  (let ((layer1 (cfgl-layer "layer1" :name 'layer1))
        (layer2 (cfgl-layer "layer2" :name 'layer2))
        (configuration-layer--used-layers nil)
        (configuration-layer--indexed-layers (make-hash-table :size 1024)))
    (helper--add-layers `(,layer1 ,layer2) 'used)
    (configuration-layer/declare-shadow-relation 'layer1 'layer2)
    (should (and (equal '(layer2) (cfgl-layer-get-shadowing-layers layer1))
                 (equal '() (cfgl-layer-get-shadowing-layers layer2))))))

(ert-deftest test-cfgl-layer-get-shadowing-layers--prevent-l2-from-shadowing-l1 ()
  (let ((layer1 (cfgl-layer "layer1" :name 'layer1))
        (layer2 (cfgl-layer "layer2" :name 'layer2 :can-shadow nil))
        (configuration-layer--used-layers nil)
        (configuration-layer--indexed-layers (make-hash-table :size 1024)))
    (helper--add-layers `(,layer1 ,layer2) 'used)
    (configuration-layer/declare-shadow-relation 'layer2 'layer1)
    (should (null (cfgl-layer-get-shadowing-layers layer1)))))

(ert-deftest test-cfgl-layer-get-shadowing-layers--prevent-l2-from-shadowing-l1-alternative ()
  ;; using the commutative property of the can-shadow relation
  ;; setting :can-shadow to nil on layer1 produces the same effect as the more
  ;; intuitive test-cfgl-layer-get-shadowing-layers--prevent-l2-from-shadowing-l1
  (let ((layer1 (cfgl-layer "layer1" :name 'layer1 :can-shadow nil))
        (layer2 (cfgl-layer "layer2" :name 'layer2))
        (configuration-layer--used-layers nil)
        (configuration-layer--indexed-layers (make-hash-table :size 1024)))
    (helper--add-layers `(,layer1 ,layer2) 'used)
    (configuration-layer/declare-shadow-relation 'layer2 'layer1)
    (should (null (cfgl-layer-get-shadowing-layers layer1)))))

;; ---------------------------------------------------------------------------
;; configuration-layer/layer-used-p
;; ---------------------------------------------------------------------------

(ert-deftest test-layer-used-p--returns-true-when-layer-is-used ()
  (let (configuration-layer--used-layers
        (configuration-layer--indexed-layers (make-hash-table :size 1024)))
    (helper--add-layers `(,(cfgl-layer "usedlayer" :name 'usedlayer)) 'used)
    (helper--add-layers `(,(cfgl-layer "notusedlayer" :name 'notusedlayer)))
    (should (configuration-layer/layer-used-p 'usedlayer))))

(ert-deftest test-layer-used-p--returns-false-when-layer-is-not-used ()
  (let (configuration-layer--used-layers
        (configuration-layer--indexed-layers (make-hash-table :size 1024)))
    (helper--add-layers `(,(cfgl-layer "usedlayer" :name 'usedlayer)) 'used)
    (helper--add-layers `(,(cfgl-layer "notusedlayer" :name 'notusedlayer)))
    (should (null (configuration-layer/layer-used-p 'notusedlayer)))))

(ert-deftest test-layer-used-p--returns-false-when-layer-is-shadowed ()
  (let ((usedlayer1 (cfgl-layer "usedlayer1" :name 'usedlayer1))
        (usedlayer2 (cfgl-layer "usedlayer2" :name 'usedlayer2))
        configuration-layer--used-layers
        (configuration-layer--indexed-layers (make-hash-table :size 1024)))
    (helper--add-layers `(,usedlayer1 ,usedlayer2) 'used)
    (configuration-layer/declare-shadow-relation 'usedlayer2 'usedlayer1)
    (should (not (configuration-layer/layer-used-p 'usedlayer1)))))

(ert-deftest test-layer-used-p--dotfile-layer-is-always-used ()
  (should (configuration-layer/layer-used-p 'dotfile)))

;; ---------------------------------------------------------------------------
;; class cfgl-package
;; ---------------------------------------------------------------------------

;; method: cfgl-package-enabled-p

(ert-deftest test-cfgl-package-enabled-p--default-toggle-eval-non-nil ()
  (let ((pkg (cfgl-package "testpkg" :name 'testpkg)))
    (should (cfgl-package-enabled-p pkg))))

(ert-deftest test-cfgl-package-enabled-p--symbol-toggle-eval-non-nil-example ()
  (let ((pkg (cfgl-package "testpkg" :name 'testpkg :toggle 'package-toggle))
        (package-toggle t))
    (should (cfgl-package-enabled-p pkg))))

(ert-deftest test-cfgl-package-enabled-p--symbol-toggle-eval-nil-example ()
  (let ((pkg (cfgl-package "testpkg" :name 'testpkg :toggle 'package-toggle))
        (package-toggle nil))
    (should (null (cfgl-package-enabled-p pkg)))))

(ert-deftest test-cfgl-package-enabled-p--list-toggle-eval-non-nil-example ()
  (let ((pkg (cfgl-package "testpkg"
                           :name 'testpkg
                           :toggle '(memq package-toggle '(foo bar))))
        (package-toggle 'foo))
    (should (cfgl-package-enabled-p pkg))))

(ert-deftest test-cfgl-package-enabled-p--list-toggle-eval-nil-example ()
  (let ((pkg (cfgl-package "testpkg"
                           :name 'testpkg
                           :toggle '(memq package-toggle '(foo bar))))
        (package-toggle 'other))
    (should (null (cfgl-package-enabled-p pkg)))))

(ert-deftest test-cfgl-package-enabled-p--depends-satisfied ()
  (let ((pkg-a (cfgl-package "pkg-a"
                             :name 'pkg-a
                             :requires '(pkg-b)))
        (pkg-b (cfgl-package "pkg-b"
                             :name 'pkg-b))
        (configuration-layer--indexed-packages (make-hash-table :size 2048)))
    (configuration-layer//add-package pkg-b)
    (should (cfgl-package-enabled-p pkg-a))))

(ert-deftest test-cfgl-package-enabled-p--depends-nonexistent ()
  (let ((pkg-a (cfgl-package "pkg-a"
                             :name 'pkg-a
                             :requires '(pkg-b)))
        (configuration-layer--indexed-packages (make-hash-table :size 2048)))
    (should (null (cfgl-package-enabled-p pkg-a)))))

(ert-deftest test-cfgl-package-enabled-p--depends-toggled-off ()
  (let ((pkg-a (cfgl-package "pkg-a"
                             :name 'pkg-a
                             :requires '(pkg-b)))
        (pkg-b (cfgl-package "pkg-b"
                             :name 'pkg-b
                             :toggle nil))
        (configuration-layer--indexed-packages (make-hash-table :size 2048)))
    (configuration-layer//add-package pkg-b)
    (should (null (cfgl-package-enabled-p pkg-a)))))

(ert-deftest test-cfgl-package-enabled-p--depends-excluded ()
  (let ((pkg-a (cfgl-package "pkg-a"
                             :name 'pkg-a
                             :requires '(pkg-b)))
        (pkg-b (cfgl-package "pkg-b"
                             :name 'pkg-b
                             :excluded t))
        (configuration-layer--indexed-packages (make-hash-table :size 2048)))
    (configuration-layer//add-package pkg-b)
    (should (null (cfgl-package-enabled-p pkg-a)))))

(ert-deftest test-cfgl-package-enabled-p--depends-transitive ()
  (let ((pkg-a (cfgl-package "pkg-a"
                             :name 'pkg-a
                             :requires '(pkg-b)))
        (pkg-b (cfgl-package "pkg-b"
                             :name 'pkg-b
                             :requires '(pkg-c)))
        (pkg-c (cfgl-package "pkg-c"
                             :name 'pkg-c))
        (configuration-layer--indexed-packages (make-hash-table :size 2048)))
    (configuration-layer//add-package pkg-b)
    (configuration-layer//add-package pkg-c)
    (should (cfgl-package-enabled-p pkg-a))))

(ert-deftest test-cfgl-package-enabled-p--depends-transitive-not-satisfied ()
  (let ((pkg-a (cfgl-package "pkg-a"
                             :name 'pkg-a
                             :requires '(pkg-b)))
        (pkg-b (cfgl-package "pkg-b"
                             :name 'pkg-b
                             :requires '(pkg-c)))
        (pkg-c (cfgl-package "pkg-c"
                             :name 'pkg-c
                             :excluded t))
        (configuration-layer--indexed-packages (make-hash-table :size 2048)))
    (configuration-layer//add-package pkg-b)
    (configuration-layer//add-package pkg-c)
    (should (null (cfgl-package-enabled-p pkg-a)))))

;; method: cfgl-package-get-safe-owner

(ert-deftest test-cfgl-package-get-safe-owner--return-car ()
  (let ((pkg (cfgl-package "testpkg" :name 'testpkg :owners '(layer1 layer2)))
        configuration-layer--used-layers
        (configuration-layer--indexed-layers (make-hash-table :size 1024)))
    (helper--add-layers `(,(cfgl-layer "layer1" :name 'layer1)
                          ,(cfgl-layer "layer2" :name 'layer2)) t)
    (should (eq 'layer1 (cfgl-package-get-safe-owner pkg)))))

(ert-deftest test-cfgl-package-get-safe-owner--return-cadr ()
  (let ((pkg (cfgl-package "testpkg" :name 'testpkg :owners '(layer1 layer2)))
        configuration-layer--used-layers
        (configuration-layer--indexed-layers (make-hash-table :size 1024)))
    ;; layer1 is not used so it cannot be the owner
    (helper--add-layers `(,(cfgl-layer "layer1" :name 'layer1)))
    (helper--add-layers `(,(cfgl-layer "layer2" :name 'layer2)) t)
    (should (eq 'layer2 (cfgl-package-get-safe-owner pkg)))))

;; method: cfgl-package-distant-p

(ert-deftest test-cfgl-package-distant-p--by-default-is-distant ()
  (let ((pkg (cfgl-package "testpkg"
                           :name 'testpkg
                           :owners '(layer1))))
    (helper--add-layers `(,(cfgl-layer "layer1" :name 'layer1)) t)
    (should (cfgl-package-distant-p pkg))))

(ert-deftest test-cfgl-package-distant-p--from-elpa-repo-is-distant ()
  (let ((pkg (cfgl-package "testpkg"
                           :name 'testpkg
                           :owners '(layer1)
                           :location 'elpa)))
    (helper--add-layers `(,(cfgl-layer "layer1" :name 'layer1)) t)
    (should (cfgl-package-distant-p pkg))))

(ert-deftest test-cfgl-package-distant-p--from-recipe-is-distant ()
  (let ((pkg (cfgl-package "testpkg"
                           :name 'testpkg
                           :owners '(layer1)
                           :location '(recipe blahblah))))
    (helper--add-layers `(,(cfgl-layer "layer1" :name 'layer1)) t)
    (should (cfgl-package-distant-p pkg))))

(ert-deftest test-cfgl-package-distant-p--built-in-is-not-distant ()
  (let ((pkg (cfgl-package "testpkg"
                           :name 'testpkg
                           :owners '(layer1)
                           :location 'built-in)))
    (helper--add-layers `(,(cfgl-layer "layer1" :name 'layer1)) t)
    (should (not (cfgl-package-distant-p pkg)))))

(ert-deftest test-cfgl-package-distant-p--site-is-not-distant ()
  (let ((pkg (cfgl-package "testpkg"
                           :name 'testpkg
                           :owners '(layer1)
                           :location 'site)))
    (helper--add-layers `(,(cfgl-layer "layer1" :name 'layer1)) t)
    (should (not (cfgl-package-distant-p pkg)))))

(ert-deftest test-cfgl-package-distant-p--local-is-not-distant ()
  (let ((pkg (cfgl-package "testpkg"
                           :name 'testpkg
                           :owners '(layer1)
                           :location 'local)))
    (helper--add-layers `(,(cfgl-layer "layer1" :name 'layer1)) t)
    (should (not (cfgl-package-distant-p pkg)))))

(ert-deftest test-cfgl-package-distant-p--location-is-a-path ()
  (let ((pkg (cfgl-package "testpkg"
                           :name 'testpkg
                           :owners '(layer1)
                           :location "/a/path/to/pkg")))
    (helper--add-layers `(,(cfgl-layer "layer1" :name 'layer1)) t)
    (should (not (cfgl-package-distant-p pkg)))))

;; method: cfgl-package-used-p

(ert-deftest test-cfgl-package-used-p--if-owned-by-layer-pkg-is-used ()
  (let ((pkg (cfgl-package "testpkg" :name 'testpkg :owners '(layer1))))
    (helper--add-layers `(,(cfgl-layer "layer1" :name 'layer1)) t)
    (should (cfgl-package-used-p pkg))))

(ert-deftest test-cfgl-package-used-p--if-no-owner-pkg-is-not-used ()
  (let ((pkg (cfgl-package "testpkg" :name 'testpkg)))
    (should (not (cfgl-package-used-p pkg)))))

(ert-deftest test-cfgl-package-used-p--if-excluded-pkg-is-not-used ()
  (let ((pkg (cfgl-package "testpkg" :name
                           'testpkg :owners '(layer1)
                           :excluded t)))
    (helper--add-layers `(,(cfgl-layer "layer1" :name 'layer1)) t)
    (should (not (cfgl-package-used-p pkg)))))

;; method: cfgl-package-reqs-satisfied-p

(ert-deftest test-cfgl-package-reqs-satisfied-p--ok-with-single-required-used-enabled-package ()
  (let ((layer1 (cfgl-layer "layer1" :name 'layer1))
        (layer2 (cfgl-layer "layer2" :name 'layer2))
        (pkg1 (cfgl-package "pkg1" :name 'pkg1 :owners '(layer1) :requires '(pkg2)))
        (pkg2 (cfgl-package "pkg2" :name 'pkg2 :owners '(layer2)))
        configuration-layer--used-packages
        (configuration-layer--indexed-packages (make-hash-table :size 2048))
        (configuration-layer--indexed-layers (make-hash-table :size 1024)))
    (helper--add-layers (list layer1 layer2) t)
    (helper--add-packages (list pkg1 pkg2) t)
    (should (cfgl-package-reqs-satisfied-p pkg1))))

(ert-deftest test-cfgl-package-reqs-satisfied-p--ok-with-single-required-unused-enabled-package ()
  (let ((layer1 (cfgl-layer "layer1" :name 'layer1))
        (layer2 (cfgl-layer "layer2" :name 'layer2))
        (pkg1 (cfgl-package "pkg1" :name 'pkg1 :owners '(layer1) :requires '(pkg2)))
        (pkg2 (cfgl-package "pkg2" :name 'pkg2 :owners '(layer2)))
        configuration-layer--used-layers
        configuration-layer--used-packages
        (configuration-layer--indexed-layers (make-hash-table :size 1024))
        (configuration-layer--indexed-packages (make-hash-table :size 2048)))
    (helper--add-layers (list layer1) t)
    (helper--add-layers (list layer2))
    (helper--add-packages (list pkg1) t)
    (helper--add-packages (list pkg2))
    (should (cfgl-package-reqs-satisfied-p pkg1))))

(ert-deftest test-cfgl-package-reqs-satisfied-p--not-ok-with-single-required-used-not-enabled-package ()
  (let ((layer1 (cfgl-layer "layer1" :name 'layer1))
        (layer2 (cfgl-layer "layer2" :name 'layer2))
        (pkg1 (cfgl-package "pkg1" :name 'pkg1 :owners '(layer1) :requires '(pkg2)))
        (pkg2 (cfgl-package "pkg2" :name 'pkg2 :toggle nil :owners '(layer3)))
        configuration-layer--used-packages
        (configuration-layer--indexed-packages (make-hash-table :size 2048))
        (configuration-layer--indexed-layers (make-hash-table :size 1024)))
    (helper--add-layers (list layer1 layer2) t)
    (helper--add-packages (list pkg1 pkg2) t)
    (should (not (cfgl-package-reqs-satisfied-p pkg1)))))

(ert-deftest test-cfgl-package-reqs-satisfied-p--not-ok-with-single-required-not-existing-package ()
  (let ((layer1 (cfgl-layer "layer1" :name 'layer1))
        (pkg1 (cfgl-package "pkg1" :name 'pkg1 :owners '(layer1) :requires '(pkg2)))
        configuration-layer--used-packages
        (configuration-layer--indexed-packages (make-hash-table :size 2048))
        (configuration-layer--indexed-layers (make-hash-table :size 1024)))
    (helper--add-layers (list layer1) t)
    (helper--add-packages (list pkg1) t)
    (should (not (cfgl-package-reqs-satisfied-p pkg1)))))

(ert-deftest test-cfgl-package-reqs-satisfied-p--ok-with-multiple-required-used-enabled-package ()
  (let ((layer1 (cfgl-layer "layer1" :name 'layer1))
        (layer2 (cfgl-layer "layer2" :name 'layer2))
        (layer3 (cfgl-layer "layer3" :name 'layer3))
        (pkg1 (cfgl-package "pkg1" :name 'pkg1 :owners '(layer1) :requires '(pkg2 pkg3)))
        (pkg2 (cfgl-package "pkg2" :name 'pkg2 :owners '(layer2)))
        (pkg3 (cfgl-package "pkg3" :name 'pkg3 :owners '(layer3)))
        configuration-layer--used-packages
        (configuration-layer--indexed-packages (make-hash-table :size 2048))
        (configuration-layer--indexed-layers (make-hash-table :size 1024)))
    (helper--add-layers (list layer1 layer2 layer3) t)
    (helper--add-packages (list pkg1 pkg2 pkg3) t)
    (should (cfgl-package-reqs-satisfied-p pkg1))))

(ert-deftest test-cfgl-package-reqs-satisfied-p--ok-with-multiple-required-unused-enabled-package ()
  (let ((layer1 (cfgl-layer "layer1" :name 'layer1))
        (layer2 (cfgl-layer "layer2" :name 'layer2))
        (layer3 (cfgl-layer "layer3" :name 'layer3))
        (pkg1 (cfgl-package "pkg1" :name 'pkg1 :owners '(layer1) :requires '(pkg2 pkg3)))
        (pkg2 (cfgl-package "pkg2" :name 'pkg2 :owners '(layer2)))
        (pkg3 (cfgl-package "pkg3" :name 'pkg3 :owners '(layer3)))
        configuration-layer--used-layers
        configuration-layer--used-packages
        (configuration-layer--indexed-layers (make-hash-table :size 1024))
        (configuration-layer--indexed-packages (make-hash-table :size 2048)))
    (helper--add-layers (list layer1 layer3) t)
    (helper--add-layers (list layer2))
    (helper--add-packages (list pkg1 pkg3) t)
    (helper--add-packages (list pkg2))
    (should (cfgl-package-reqs-satisfied-p pkg1))))

(ert-deftest test-cfgl-package-reqs-satisfied-p--not-ok-with-multiple-required-used-not-enabled-package ()
  (let ((layer1 (cfgl-layer "layer1" :name 'layer1))
        (layer2 (cfgl-layer "layer2" :name 'layer2))
        (layer3 (cfgl-layer "layer3" :name 'layer3))
        (pkg1 (cfgl-package "pkg1" :name 'pkg1 :owners '(layer1) :requires '(pkg2 pkg3)))
        (pkg2 (cfgl-package "pkg2" :name 'pkg2 :toggle nil :owners '(layer3)))
        (pkg3 (cfgl-package "pkg3" :name 'pkg3 :owners '(layer3)))
        configuration-layer--used-packages
        (configuration-layer--indexed-packages (make-hash-table :size 2048))
        (configuration-layer--indexed-layers (make-hash-table :size 1024)))
    (helper--add-layers (list layer1 layer2 layer3) t)
    (helper--add-packages (list pkg1 pkg2 pkg3) t)
    (should (not (cfgl-package-reqs-satisfied-p pkg1)))))

(ert-deftest test-cfgl-package-reqs-satisfied-p--not-ok-with-multiple-required-not-existing-package ()
  (let ((layer1 (cfgl-layer "layer1" :name 'layer1))
        (layer3 (cfgl-layer "layer3" :name 'layer3))
        (pkg1 (cfgl-package "pkg1" :name 'pkg1 :owners '(layer1) :requires '(pkg2 pkg3)))
        (pkg3 (cfgl-package "pkg3" :name 'pkg3 :owners '(layer3)))
        configuration-layer--used-packages
        (configuration-layer--indexed-packages (make-hash-table :size 2048))
        (configuration-layer--indexed-layers (make-hash-table :size 1024)))
    (helper--add-layers (list layer1 layer3) t)
    (helper--add-packages (list pkg1 pkg3) t)
    (should (not (cfgl-package-reqs-satisfied-p pkg1)))))

;; ---------------------------------------------------------------------------
;; configuration-layer//package-enabled-p
;; ---------------------------------------------------------------------------

(ert-deftest test-package-enabled-p--pre ()
  (let ((owner (cfgl-layer "owner" :name 'owner))
        (layer (cfgl-layer "layer" :name 'layer))
        (pkg (cfgl-package "pkg"
                           :name 'pkg
                           :owners '(owner)
                           :pre-layers '(layer)))
        (configuration-layer--indexed-packages (make-hash-table :size 2048))
        (configuration-layer--indexed-layers (make-hash-table :size 1024)))
    (configuration-layer//add-layer owner)
    (configuration-layer//add-layer layer)
    (configuration-layer//add-package pkg)
    (should (configuration-layer//package-enabled-p pkg 'layer))))

(ert-deftest test-package-enabled-p--post ()
  (let ((owner (cfgl-layer "owner" :name 'owner))
        (layer (cfgl-layer "layer" :name 'layer))
        (pkg (cfgl-package "pkg"
                           :name 'pkg
                           :owners '(owner)
                           :post-layers '(layer)))
        (configuration-layer--indexed-packages (make-hash-table :size 2048))
        (configuration-layer--indexed-layers (make-hash-table :size 1024)))
    (configuration-layer//add-layer owner)
    (configuration-layer//add-layer layer)
    (should (configuration-layer//package-enabled-p pkg 'layer))))

(ert-deftest test-package-enabled-p--disabled ()
  (let ((owner (cfgl-layer "owner" :name 'owner :disabled-for '(layer)))
        (layer (cfgl-layer "layer" :name 'layer))
        (pkg (cfgl-package "pkg"
                           :name 'pkg
                           :owners '(owner)
                           :post-layers '(layer)))
        (configuration-layer--indexed-packages (make-hash-table :size 2048))
        (configuration-layer--indexed-layers (make-hash-table :size 1024)))
    (configuration-layer//add-layer owner)
    (configuration-layer//add-layer layer)
    (should (null (configuration-layer//package-enabled-p pkg 'layer)))))

(ert-deftest test-package-enabled-p--enabled-precedence ()
  (let ((owner (cfgl-layer "owner" :name 'owner :disabled-for '(layer) :enabled-for '(layer)))
        (layer (cfgl-layer "layer" :name 'layer))
        (pkg (cfgl-package "pkg"
                           :name 'pkg
                           :owners '(owner)
                           :post-layers '(layer)))
        (configuration-layer--indexed-packages (make-hash-table :size 2048))
        (configuration-layer--indexed-layers (make-hash-table :size 1024)))
    (configuration-layer//add-layer owner)
    (configuration-layer//add-layer layer)
    (should (configuration-layer//package-enabled-p pkg 'layer))))

(ert-deftest test-package-enabled-p--enabled-for-none ()
  (let ((owner (cfgl-layer "owner" :name 'owner :enabled-for '()))
        (layer (cfgl-layer "layer" :name 'layer))
        (pkg (cfgl-package "pkg"
                           :name 'pkg
                           :owners '(owner)
                           :post-layers '(layer)))
        (configuration-layer--indexed-packages (make-hash-table :size 2048))
        (configuration-layer--indexed-layers (make-hash-table :size 1024)))
    (configuration-layer//add-layer owner)
    (configuration-layer//add-layer layer)
    (should (null (configuration-layer//package-enabled-p pkg 'layer)))))

(ert-deftest test-package-enabled-p--depends-on-disabled ()
  (let ((owner-a (cfgl-layer "owner-a" :name 'owner-a))
        (owner-b (cfgl-layer "owner-b" :name 'owner-b :disabled-for '(layer)))
        (layer (cfgl-layer "layer" :name 'layer))
        (pkg-a (cfgl-package "pkg-a"
                             :name 'pkg-a
                             :owners '(owner-a)
                             :requires  '(pkg-b)
                             :post-layers '(layer)))
        (pkg-b (cfgl-package "pkg-b"
                             :name 'pkg-b
                             :owners '(owner-b)))
        (configuration-layer--indexed-packages (make-hash-table :size 2048))
        (configuration-layer--indexed-layers (make-hash-table :size 1024)))
    (configuration-layer//add-layer owner-a)
    (configuration-layer//add-layer owner-b)
    (configuration-layer//add-layer layer)
    (configuration-layer//add-package pkg-b)
    (should (null (configuration-layer//package-enabled-p pkg-a 'layer)))))

(ert-deftest test-package-enabled-p--depends-on-non-owned ()
  (let ((layer (cfgl-layer "layer" :name 'layer))
        (owner (cfgl-layer "owner" :name 'owner))
        (pkg-a (cfgl-package "pkg-a"
                             :name 'pkg-a
                             :owners '(owner)
                             :requires '(pkg-b)
                             :post-layers '(layer)))
        (pkg-b (cfgl-package "pkg-b" :name 'pkg-b))
        (configuration-layer--indexed-packages (make-hash-table :size 2048))
        (configuration-layer--indexed-layers (make-hash-table :size 1024)))
    (configuration-layer//add-layer owner)
    (configuration-layer//add-package pkg-b)
    (should (null (configuration-layer//package-enabled-p pkg-a layer)))))

;; ---------------------------------------------------------------------------
;; configuration-layer/package-used-p
;; ---------------------------------------------------------------------------

(ert-deftest test-package-usedp--package-with-owner-can-be-used ()
  (let* ((layer1 (cfgl-layer "layer1" :name 'layer1 :dir "/path/"))
         (layer1-packages '(pkg1 pkg2 pkg3))
         configuration-layer--used-layers
         configuration-layer--used-packages
         (configuration-layer--indexed-layers (make-hash-table :size 2048))
         (configuration-layer--indexed-packages (make-hash-table :size 2048)))
    (helper--add-layers (list layer1) t)
    (helper--add-packages
     (list (cfgl-package "pkg3" :name 'pkg3 :owners '(layer1))
           (cfgl-package "pkg2" :name 'pkg2 :owners '(layer1))
           (cfgl-package "pkg1" :name 'pkg1 :owners '(layer1))) t)
    (should (configuration-layer/package-used-p
             (nth (random 3) layer1-packages)))))

(ert-deftest test-package-usedp--package-with-no-owner-cannot-be-used ()
  (let* ((layer1 (cfgl-layer "layer1" :name 'layer1 :dir "/path/"))
         (layers (list layer1))
         (layer1-packages '(pkg1 pkg2 pkg3))
         configuration-layer--used-layers
         (configuration-layer--used-packages
          (list (cfgl-package "pkg3" :name 'pkg3)
                (cfgl-package "pkg2" :name 'pkg2)
                (cfgl-package "pkg1" :name 'pkg1))))
    (should (null (configuration-layer/package-used-p
                   (nth (random 3) layer1-packages))))))

(ert-deftest test-package-usedp--excluded-package-cannot-be-used ()
  (let* ((layer1 (cfgl-layer "layer1" :name 'layer1 :dir "/path/"))
         (layer1-packages '(pkg1 pkg2 pkg3))
         configuration-layer--used-layers
         configuration-layer--used-packages
         (configuration-layer--indexed-layers (make-hash-table :size 2048))
         (configuration-layer--indexed-packages (make-hash-table :size 2048))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (helper--add-layers (list layer1) t)
    (helper--add-packages
     (list (cfgl-package "pkg3" :name 'pkg3 :owners '(layer1) :excluded t)
           (cfgl-package "pkg2" :name 'pkg2 :owners '(layer1) :excluded t)
           (cfgl-package "pkg1" :name 'pkg1 :owners '(layer1) :excluded t)) t)
    (should (null (configuration-layer/package-used-p
                   (nth (random 3) layer1-packages))))))

(ert-deftest test-package-usedp--used-pkg-requires-used-pkg-can-be-used ()
  (let* ((layer1 (cfgl-layer "layer1" :name 'layer1 :dir "/path/"))
         (layer1-packages '(pkg1))
         (layer2 (cfgl-layer "layer2" :name 'layer2 :dir "/path/"))
         (layer2-packages '(pkg2))
         configuration-layer--used-layers
         configuration-layer--used-packages
         (configuration-layer--indexed-layers (make-hash-table :size 2048))
         (configuration-layer--indexed-packages (make-hash-table :size 2048)))
    (helper--add-layers (list layer1 layer2) 'used)
    (helper--add-packages
     (list (cfgl-package "pkg1" :name 'pkg1 :owners '(layer1))
           (cfgl-package "pkg2" :name 'pkg2 :owners '(layer2) :requires '(pkg1)))
     'used)
    (should (configuration-layer/package-used-p 'pkg2))))

(ert-deftest test-package-usedp--used-pkg3-requires-used-pkg2-depends-on-used-pkg1-can-be-used ()
  (let* ((layer1 (cfgl-layer "layer1" :name 'layer1 :dir "/path/"))
         (layer1-packages '(pkg1))
         (layer2 (cfgl-layer "layer2" :name 'layer2 :dir "/path/"))
         (layer2-packages '(pkg2))
         (layer3 (cfgl-layer "layer3" :name 'layer3 :dir "/path/"))
         (layer3-packages '(pkg3))
         configuration-layer--used-layers
         configuration-layer--used-packages
         (configuration-layer--indexed-layers (make-hash-table :size 2048))
         (configuration-layer--indexed-packages (make-hash-table :size 2048)))
    (helper--add-layers (list layer1 layer2 layer3) 'used)
    (helper--add-packages
     (list (cfgl-package "pkg1" :name 'pkg1 :owners '(layer1))
           (cfgl-package "pkg2" :name 'pkg2 :owners '(layer2) :requires '(pkg1))
           (cfgl-package "pkg3" :name 'pkg3 :owners '(layer3) :requires '(pkg2)))

     'used)
    (should (configuration-layer/package-used-p 'pkg3))))

(ert-deftest test-package-usedp--used-pkg2-requires-unused-pkg1-cannot-be-used ()
  (let* ((layer1 (cfgl-layer "layer1" :name 'layer1 :dir "/path/"))
         (layer1-packages '(pkg1))
         (layer2 (cfgl-layer "layer2" :name 'layer2 :dir "/path/"))
         (layer2-packages '(pkg2))
         configuration-layer--used-layers
         configuration-layer--used-packages
         (configuration-layer--indexed-layers (make-hash-table :size 2048))
         (configuration-layer--indexed-packages (make-hash-table :size 2048)))
    (helper--add-layers (list layer1))
    (helper--add-layers (list layer2) 'used)
    (helper--add-packages
     (list (cfgl-package "pkg1" :name 'pkg1 :owners '(layer1))))
    (helper--add-packages
     (list (cfgl-package "pkg2" :name 'pkg2 :owners '(layer2) :requires '(pkg1)))
     'used)
    (should (null (configuration-layer/package-used-p 'pkg2)))))

(ert-deftest test-package-usedp--used-pkg3-requires-used-pkg2-requires-unused-pkg1-cannot-be-used ()
  (let* ((layer1 (cfgl-layer "layer1" :name 'layer1 :dir "/path/"))
         (layer1-packages '(pkg1))
         (layer2 (cfgl-layer "layer2" :name 'layer2 :dir "/path/"))
         (layer2-packages '(pkg2))
         (layer3 (cfgl-layer "layer3" :name 'layer3 :dir "/path/"))
         (layer3-packages '(pkg3))
         configuration-layer--used-layers
         configuration-layer--used-packages
         (configuration-layer--indexed-layers (make-hash-table :size 2048))
         (configuration-layer--indexed-packages (make-hash-table :size 2048)))
    (helper--add-layers (list layer1))
    (helper--add-layers (list layer2 layer3) 'used)
    (helper--add-packages
     (list (cfgl-package "pkg1" :name 'pkg1 :owners '(layer1))))
    (helper--add-packages
     (list (cfgl-package "pkg2" :name 'pkg2 :owners '(layer2) :requires '(pkg1))
           (cfgl-package "pkg3" :name 'pkg3 :owners '(layer3) :requires '(pkg2)))
     'used)
    (should (null (configuration-layer/package-used-p 'pkg3)))))

;; ---------------------------------------------------------------------------
;; configuration-layer//package-reqs-used-p
;; ---------------------------------------------------------------------------

(ert-deftest test-package-reqs-used-p--no-requires ()
  (let ((pkg-a (cfgl-package "pkg-a"
                             :name 'pkg-a)))
    (should (configuration-layer//package-reqs-used-p pkg-a))))

(ert-deftest test-package-reqs-used-p--requires-used-package ()
  (let ((pkg-a (cfgl-package "pkg-a"
                             :name 'pkg-a
                             :requires '(pkg-b)))
        (pkg-b (cfgl-package "pkg-b"
                             :name 'pkg-b
                             :owners '(owner)))
        (owner (cfgl-layer "owner" :name 'owner))
        configuration-layer--used-layers
        (configuration-layer--indexed-packages (make-hash-table :size 2048))
        (configuration-layer--indexed-layers (make-hash-table :size 1024)))
    (configuration-layer//add-package pkg-b)
    (configuration-layer//add-layer owner 'used)
    (should (configuration-layer//package-reqs-used-p pkg-a))))

(ert-deftest test-package-reqs-used-p--requires-unused-package ()
  (let ((pkg-a (cfgl-package "pkg-a"
                             :name 'pkg-a
                             :requires '(pkg-b)
                             :owners '(owner)))
        (pkg-b (cfgl-package "pkg-b"
                             :name 'pkg-b))
        (owner (cfgl-layer "owner" :name 'owner))
        (configuration-layer--indexed-packages (make-hash-table :size 2048))
        (configuration-layer--indexed-layers (make-hash-table :size 1024)))
    (configuration-layer//add-package pkg-b)
    (configuration-layer//add-layer owner nil)
    (should (null (configuration-layer//package-reqs-used-p pkg-a)))))

;; ---------------------------------------------------------------------------
;; configuration-layer//package-archive-absolute-path-p
;; ---------------------------------------------------------------------------

(ert-deftest test-package-archive-absolute-pathp--http-absolute-path ()
  (let ((input '("melpa" . "http://melpa.org/packages/")))
    (should (configuration-layer//package-archive-absolute-path-p input))))

(ert-deftest test-package-archive-absolute-pathp--https-absolute-path ()
  (let ((input '("melpa" . "https://melpa.org/packages/")))
    (should (configuration-layer//package-archive-absolute-path-p input))))

(ert-deftest test-package-archive-absolute-pathp--user-home-tilde-absolute-path ()
  (let ((input '("spacelpa" . "~/.elpa/spacelpa")))
    (should (configuration-layer//package-archive-absolute-path-p input))))

(ert-deftest test-package-archive-absolute-pathp--user-home-slash-absolute-path ()
  (let ((input '("spacelpa" . "/home/rms/.elpa/spacelpa")))
    (should (configuration-layer//package-archive-absolute-path-p input))))

(ert-deftest test-package-archive-absolute-pathp--windows-absolute-path ()
  (let ((input '("spacelpa" . "c:/Users/My User/.elpa/spacelpa")))
    (should (configuration-layer//package-archive-absolute-path-p input))))

(ert-deftest test-package-archive-absolute-pathp--relative-path-local ()
  (let ((input '("melpa" . "../.elpa/spacelpa")))
    (should (not (configuration-layer//package-archive-absolute-path-p input)))))

(ert-deftest test-package-archive-absolute-pathp--not-absolute-path-remote ()
  (let ((input '("melpa" . "melpa.org/spacelpa")))
    (should (not (configuration-layer//package-archive-absolute-path-p input)))))

;; ---------------------------------------------------------------------------
;; configuration-layer//package-archive-local-path-p
;; ---------------------------------------------------------------------------

(ert-deftest test-package-archive-local-pathp--http-not-local-path ()
  (let ((input '("melpa" . "http://melpa.org/packages/")))
    (should (not (configuration-layer//package-archive-local-path-p input)))))

(ert-deftest test-package-archive-local-pathp--https-not-local-path ()
  (let ((input '("melpa" . "https://melpa.org/packages/")))
    (should (not (configuration-layer//package-archive-local-path-p input)))))

(ert-deftest test-package-archive-local-pathp--user-home-tilde-local-path ()
  (let ((input '("spacelpa" . "~/.elpa/spacelpa")))
    (should (configuration-layer//package-archive-local-path-p input))))

(ert-deftest test-package-archive-local-pathp--user-home-slash-local-path ()
  (let ((input '("spacelpa" . "/home/rms/.elpa/spacelpa")))
    (should (configuration-layer//package-archive-local-path-p input))))

(ert-deftest test-package-archive-local-pathp--windows-local-path ()
  (let ((input '("spacelpa" . "c:/Users/My User/.elpa/spacelpa")))
    (should (configuration-layer//package-archive-local-path-p input))))

(ert-deftest test-package-archive-local-pathp--relative-local-path-local ()
  (let ((input '("melpa" . "../.elpa/spacelpa")))
    (should (configuration-layer//package-archive-local-path-p input))))

(ert-deftest test-package-archive-local-pathp--default-not-local-path-remote ()
  (let ((input '("melpa" . "melpa.org/spacelpa")))
    (should (not (configuration-layer//package-archive-local-path-p input)))))

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

(ert-deftest test-resolve-package-archives--org-supports-https ()
  (let ((input '(("org"   . "orgmode.org/elpa/")))
        (dotspacemacs-elpa-https t))
    (should (equal '(("org" . "https://orgmode.org/elpa/"))
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
;; configuration-layer/make-layer
;; ---------------------------------------------------------------------------

;; layer directory

(ert-deftest test-make-layer--make-layer-from-symbol-with-a-dir ()
  (should (equal (cfgl-layer "layer"
                             :name 'layer
                             :dir spacemacs-start-directory)
                 (configuration-layer/make-layer
                  'layer nil nil spacemacs-start-directory))))

(ert-deftest test-make-layer--make-layer-from-spec-with-a-dir ()
  (should (equal (cfgl-layer "layer"
                             :name 'layer
                             :dir spacemacs-start-directory)
                 (configuration-layer/make-layer
                  '(layer :name 'layer) nil nil spacemacs-start-directory))))

(ert-deftest test-make-layer--cannot-make-layer-without-a-directory ()
  (mocker-let
   ((configuration-layer//warning
     (msg &rest args)
     ((:record-cls 'mocker-stub-record :output nil :occur 1))))
   (should (null (configuration-layer/make-layer 'layer)))))

(ert-deftest test-make-layer--cannot-make-layer-with-a-non-existing-directory ()
  (mocker-let
   ((configuration-layer//warning
     (msg &rest args)
     ((:record-cls 'mocker-stub-record :output nil :occur 1))))
   (should (null (configuration-layer/make-layer 'layer nil nil "/a/dir/")))))

(ert-deftest test-make-layer--make-layer-without-a-dir-requires-an-initial-obj ()
  (let ((layer (cfgl-layer "layer"
                           :name 'layer
                           :dir spacemacs-start-directory)))
    (should (equal (cfgl-layer "layer"
                               :name 'layer
                               :dir spacemacs-start-directory)
                   (configuration-layer/make-layer 'layer layer)))))

;; load packages

(ert-deftest test-make-layer--make-used-layer-loads-packages-file ()
  (let ((layer (cfgl-layer "layer"
                           :name 'layer
                           :dir spacemacs-start-directory))
        (layer-packages '(pkg1 pkg2 pkg3))
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (mocker-let
     ((file-exists-p (f) ((:output t :occur 2)))
      (load (f &optional noerr nomsg) ((:output nil :occur 1))))
     (should (equal (cfgl-layer "layer"
                                :name 'layer
                                :disabled-for nil
                                :variables nil
                                :packages '(pkg1 pkg2 pkg3)
                                :selected-packages 'all
                                :dir spacemacs-start-directory)
                    (configuration-layer/make-layer 'layer layer 'used))))))

(ert-deftest test-make-layer--make-layer-force-load-packages-file-with-var ()
  (let ((layer (cfgl-layer "layer"
                           :name 'layer
                           :dir spacemacs-start-directory))
        (layer-packages '(pkg1 pkg2 pkg3))
        (configuration-layer--load-packages-files t)
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (mocker-let
     ((file-exists-p (f) ((:output t :occur 2)))
      (load (f &optional noerr nomsg) ((:output nil :occur 1))))
     (should (equal (cfgl-layer "layer"
                                :name 'layer
                                :disabled-for nil
                                :variables nil
                                :packages '(pkg1 pkg2 pkg3)
                                :selected-packages 'all
                                :dir spacemacs-start-directory)
                    (configuration-layer/make-layer 'layer layer))))))

(ert-deftest test-make-layer--make-layer-does-not-load-packages-file-by-default ()
  (let ((layer (cfgl-layer "layer"
                           :name 'layer
                           :dir spacemacs-start-directory))
        (layer-packages '(pkg1))
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (mocker-let
     ((file-exists-p (f) ((:output t :occur 1))))
     (configuration-layer/make-layer 'layer layer))))

;; set/override properties

(ert-deftest test-make-layer--make-used-layer-can-set-additional-properties ()
  (let ((layer (cfgl-layer "layer"
                           :name 'layer
                           :dir spacemacs-start-directory))
        (layer-specs '(layer :disabled-for pkg8 pkg9
                             :can-shadow layer2 layer3
                             :variables foo bar toto 1))
        (layer-packages '(pkg1 pkg2 pkg3))
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (mocker-let
     ((file-exists-p (f) ((:output t :occur 2)))
      (load (f &optional noerr nomsg) ((:output nil :occur 1))))
     (should (equal (cfgl-layer "layer"
                                :name 'layer
                                :disabled-for '(pkg8 pkg9)
                                :can-shadow '(layer2 layer3)
                                :variables '(foo bar toto 1)
                                :packages '(pkg1 pkg2 pkg3)
                                :selected-packages 'all
                                :dir spacemacs-start-directory)
                    (configuration-layer/make-layer layer-specs layer 'used))))))

(ert-deftest test-make-layer--make-not-used-layer-cannot-set-additional-properties ()
  (let ((layer (cfgl-layer "layer"
                           :name 'layer
                           :dir spacemacs-start-directory))
        (layer-specs '(layer :disabled-for pkg8 pkg9
                             :can-shadow layer2
                             :variables foo bar toto 1))
        (layer-packages '(pkg1 pkg2 pkg3)))
    (should (equal (cfgl-layer "layer"
                               :name 'layer
                               :disabled-for nil
                               :can-shadow 'unspecified
                               :variables nil
                               :packages nil
                               :selected-packages 'all
                               :dir spacemacs-start-directory)
                   (configuration-layer/make-layer layer-specs layer)))))

(ert-deftest test-make-layer--make-used-layer-can-override-additional-properties ()
  (let ((layer (cfgl-layer "layer"
                           :name 'layer
                           :disabled-for '(pkg10)
                           :can-shadow '()
                           :variables '(titi tata tutu 1)
                           :dir spacemacs-start-directory))
        (layer-specs '(layer :disabled-for pkg8 pkg9
                             :can-shadow layer2
                             :variables foo bar toto 1))
        (layer-packages '(pkg1 pkg2 pkg3))
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (mocker-let
     ((file-exists-p (f) ((:output t :occur 2)))
      (load (f &optional noerr nomsg) ((:output nil :occur 1))))
     (should (equal (cfgl-layer "layer"
                                :name 'layer
                                :disabled-for '(pkg8 pkg9)
                                :can-shadow '(layer2)
                                :variables '(foo bar toto 1)
                                :packages '(pkg1 pkg2 pkg3)
                                :selected-packages 'all
                                :dir spacemacs-start-directory)
                    (configuration-layer/make-layer layer-specs layer 'used))))))

(ert-deftest test-make-layer--make-not-used-layer-cannot-override-additional-properties ()
  (let ((layer (cfgl-layer "layer"
                           :name 'layer
                           :disabled-for '(pkg10)
                           :can-shadow '()
                           :variables '(titi tata tutu 1)
                           :packages '(pkg1 pkg2 pkg3)
                           :selected-packages 'all
                           :dir spacemacs-start-directory))
        (layer-specs '(layer :disabled-for pkg8 pkg9
                             :can-shadow '(layer2)
                             :variables foo bar toto 1))
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (should (equal (cfgl-layer "layer"
                               :name 'layer
                               :disabled-for '(pkg10)
                               :can-shadow '()
                               :variables '(titi tata tutu 1)
                               :packages '(pkg1 pkg2 pkg3)
                               :selected-packages 'all
                               :dir spacemacs-start-directory)
                   (configuration-layer/make-layer layer-specs layer)))))

;; ---------------------------------------------------------------------------
;; configuration-layer//declare-shadow-relation
;; ---------------------------------------------------------------------------

(ert-deftest test-declare-shadow-relation--is-commutative ()
  (let ((configuration-layer--indexed-layers (make-hash-table :size 1024)))
    (helper--add-layers
     `(,(cfgl-layer "layer-shadow-1" :name 'layer-shadow-1)
       ,(cfgl-layer "layer-shadow-2" :name 'layer-shadow-2)))
    (configuration-layer/declare-shadow-relation
     'layer-shadow-1
     'layer-shadow-2)
    (should (and
             (equal '(layer-shadow-1) (oref (configuration-layer/get-layer
                                             'layer-shadow-2)
                                            :can-shadow))
             (equal '(layer-shadow-2) (oref (configuration-layer/get-layer
                                             'layer-shadow-1)
                                            :can-shadow))))))

(ert-deftest test-declare-shadow-relation--is-idempotent ()
  (let ((configuration-layer--indexed-layers (make-hash-table :size 1024)))
    (helper--add-layers
     `(,(cfgl-layer "layer-shadow-1" :name 'layer-shadow-1)
       ,(cfgl-layer "layer-shadow-2" :name 'layer-shadow-2)))
    (dotimes (i 3)
      (configuration-layer/declare-shadow-relation
       'layer-shadow-1
       'layer-shadow-2))
    (dotimes (i 3)
      (configuration-layer/declare-shadow-relation
       'layer-shadow-2
       'layer-shadow-1))
    (should (and (equal '(layer-shadow-1)
                        (oref (configuration-layer/get-layer 'layer-shadow-2)
                              :can-shadow))
                 (equal '(layer-shadow-2)
                        (oref (configuration-layer/get-layer 'layer-shadow-1)
                              :can-shadow))))))

(ert-deftest test-declare-shadow-relation--layer-1-shadows-multiple-layers ()
  (let ((configuration-layer--indexed-layers (make-hash-table :size 1024)))
    (helper--add-layers
     `(,(cfgl-layer "layer-shadow-1" :name 'layer-shadow-1)
       ,(cfgl-layer "layer-shadow-2" :name 'layer-shadow-2)
       ,(cfgl-layer "layer-shadow-3" :name 'layer-shadow-3)))
    (configuration-layer/declare-shadow-relation
     'layer-shadow-1
     'layer-shadow-2
     'layer-shadow-3)
    (should (equal '(layer-shadow-1)
                   (oref (configuration-layer/get-layer 'layer-shadow-2)
                         :can-shadow)))
    (should (equal '(layer-shadow-1)
                   (oref (configuration-layer/get-layer 'layer-shadow-3)
                         :can-shadow)))
    (should (equal '(layer-shadow-3 layer-shadow-2)
                   (oref (configuration-layer/get-layer 'layer-shadow-1)
                         :can-shadow)))))

(ert-deftest test-declare-shadow-relation--unknown-layer-shadows-known-layer ()
  (let ((configuration-layer--indexed-layers (make-hash-table :size 1024)))
    (helper--add-layers
     `(,(cfgl-layer "layer-shadow-2" :name 'layer-shadow-2)))
    (mocker-let
     ((configuration-layer//warning
       (msg &rest args)
       ((:record-cls 'mocker-stub-record :output nil :occur 1))))
     (configuration-layer/declare-shadow-relation
      'layer-shadow-1
      'layer-shadow-2)
     (should (eq 'unspecified
                 (oref (configuration-layer/get-layer 'layer-shadow-2)
                       :can-shadow))))))

(ert-deftest test-declare-shadow-relation--known-layer-shadows-unknown-layer ()
  (let ((configuration-layer--indexed-layers (make-hash-table :size 1024)))
    (helper--add-layers
     `(,(cfgl-layer "layer-shadow-1" :name 'layer-shadow-1)))
    (mocker-let
     ((configuration-layer//warning
       (msg &rest args)
       ((:record-cls 'mocker-stub-record :output nil :occur 1))))
     (configuration-layer/declare-shadow-relation 'layer-shadow-1 'layer-shadow-2))))

(ert-deftest test-declare-shadow-relation--unknown-layer-shadows-unknown-layer ()
  (let ((configuration-layer--indexed-layers (make-hash-table :size 1024)))
    (mocker-let
     ((configuration-layer//warning
       (msg &rest args)
       ((:record-cls 'mocker-stub-record :output nil :occur 2))))
     (configuration-layer/declare-shadow-relation 'layer-shadow-1 'layer-shadow-2))))

;; ---------------------------------------------------------------------------
;; configuration-layer//set-layers-variables
;; ---------------------------------------------------------------------------

(ert-deftest test-set-layers-variables--none ()
  (let ((configuration-layer--indexed-layers (make-hash-table :size 1024))
        (var 'foo))
    (helper--add-layers
     `(,(cfgl-layer "layer"
                    :name 'layer
                    :dir "/a/path/")))
    (configuration-layer//set-layers-variables '(layer))
    (should (eq var 'foo))))

(ert-deftest test-set-layers-variables--one-value ()
  (let ((configuration-layer--indexed-layers (make-hash-table :size 1024)))
    (helper--add-layers
     `(,(cfgl-layer "layer"
                    :name 'layer
                    :dir "/a/path/"
                    :variables '(var1 'bar))))
    (setq var1 'foo)
    (configuration-layer//set-layers-variables '(layer))
    (should (eq var1 'bar))))

(ert-deftest test-set-layers-variables--multiple-values ()
  (let ((configuration-layer--indexed-layers (make-hash-table :size 1024)))
    (helper--add-layers
     `(,(cfgl-layer "layer"
                    :name 'layer
                    :dir "/a/path/"
                    :variables '(var1 'bar1 var2 'bar2 var3 'bar3))))
    (setq var1 'foo)
    (setq var2 'foo)
    (setq var3 'foo)
    (configuration-layer//set-layers-variables '(layer))
    (should (eq var1 'bar1))
    (should (eq var2 'bar2))
    (should (eq var3 'bar3))))

(ert-deftest test-set-layers-variables--odd-number-of-values ()
  (let ((configuration-layer--indexed-layers (make-hash-table :size 1024)))
    (helper--add-layers
     `(,(cfgl-layer "layer"
                    :name 'layer
                    :dir "/a/path/"
                    :variables '(var1 'bar var2))))
    (mocker-let
     ((configuration-layer//warning
       (msg &rest args)
       ((:record-cls 'mocker-stub-record :output nil :occur 1))))
     (setq var1 'foo)
     (setq var2 'foo)
     (configuration-layer//set-layers-variables '(layer))
     (should (eq var1 'bar))
     (should (eq var2 'foo)))))

;; ---------------------------------------------------------------------------
;; configuration-layer/make-package
;; ---------------------------------------------------------------------------

(ert-deftest test-make-package--make-package-from-symbol ()
  (let* (configuration-layer--used-layers
         (configuration-layer--indexed-layers (make-hash-table :size 1024))
         (input 'testpkg)
         (expected (cfgl-package "testpkg"
                                 :name 'testpkg
                                 :location 'elpa
                                 :owners '(layer-make-pkg-1)
                                 :pre-layers nil
                                 :post-layers nil
                                 :step nil
                                 :excluded nil)))
    (defun layer-make-pkg-1/init-testpkg nil)
    (helper--add-layers
     `(,(cfgl-layer "layer-make-pkg-1" :name 'layer-make-pkg-1)) t)
    (should
     (equal
      expected
      (configuration-layer/make-package input 'layer-make-pkg-1)))))

(ert-deftest test-make-package--make-package-from-list ()
  (let* (configuration-layer--used-layers
         (configuration-layer--indexed-layers (make-hash-table :size 1024))
         (input '(testpkg :location local :step pre))
         (expected (cfgl-package "testpkg"
                                 :name 'testpkg
                                 :owners '(layer-make-pkg-2)
                                 :location 'local
                                 :pre-layers nil
                                 :post-layers nil
                                 :step 'pre
                                 :excluded nil)))
    (defun layer-make-pkg-2/init-testpkg nil)
    (helper--add-layers
     `(,(cfgl-layer "layer-make-pkg-2" :name 'layer-make-pkg-2)) t)
    (should
     (equal
      expected
      (configuration-layer/make-package input 'layer-make-pkg-2)))))

(ert-deftest test-make-package--multiple-calls-invariants ()
  (defun layer-make-pkg-3/init-testpkg nil)
  (defun layer-make-pkg-4/pre-init-testpkg nil)
  (defun layer-make-pkg-5/post-init-testpkg nil)
  (let* (configuration-layer--used-layers
         configuration-layer--protected-packages
         (configuration-layer--indexed-layers (make-hash-table :size 1024))
         (input '(testpkg :protected t))
         (expected-pkg (cfgl-package "testpkg"
                                     :name 'testpkg
                                     :location 'elpa
                                     :owners '(layer-make-pkg-3)
                                     :pre-layers '(layer-make-pkg-4)
                                     :post-layers '(layer-make-pkg-5)
                                     :step nil
                                     :protected t
                                     :excluded nil))
         (expected-protected-list '(testpkg)))
    (helper--add-layers
     `(,(cfgl-layer "layer-make-pkg-3" :name 'layer-make-pkg-3)
       ,(cfgl-layer "layer-make-pkg-4" :name 'layer-make-pkg-4)
       ,(cfgl-layer "layer-make-pkg-5" :name 'layer-make-pkg-5)) t)
    (let ((obj (configuration-layer/make-package input 'layer-make-pkg-3)))
      (dotimes (x 3)
        (configuration-layer/make-package input 'layer-make-pkg-3 obj)
        (configuration-layer/make-package input 'layer-make-pkg-4 obj)
        (configuration-layer/make-package input 'layer-make-pkg-5 obj))
      (should (equal expected-pkg obj))
      (should (equal expected-protected-list
                     configuration-layer--protected-packages)))))

(ert-deftest test-make-package--can-override-toggle ()
  (let (configuration-layer--used-layers
        (configuration-layer--indexed-layers (make-hash-table :size 1024))
        (obj (cfgl-package "testpkg" :name 'testpkg :toggle 'foo))
        (pkg '(testpkg :toggle bar))
        (expected (cfgl-package "testpkg"
                                :name 'testpkg
                                :owners '(layer-make-pkg-6)
                                :toggle 'bar)))
    (defun layer-make-pkg-6/init-testpkg nil)
    (helper--add-layers
     `(,(cfgl-layer "layer-make-pkg-6" :name 'layer-make-pkg-6))
     t)
    (should
     (equal
      expected
      (configuration-layer/make-package pkg 'layer-make-pkg-6 obj)))))

(ert-deftest test-make-package--can-override-location ()
  (let* (configuration-layer--used-layers
         (configuration-layer--indexed-layers (make-hash-table :size 1024))
         (obj (cfgl-package "testpkg"
                            :name 'testpkg
                            :location 'elpa))
         (pkg '(testpkg :location local))
         (expected (cfgl-package "testpkg"
                                 :name 'testpkg
                                 :owners '(layer-make-pkg-7)
                                 :location 'local)))
    (defun layer-make-pkg-7/init-testpkg nil)
    (helper--add-layers
     `(,(cfgl-layer "layer-make-pkg-7" :name 'layer-make-pkg-7)) t)
    (should
     (equal expected
            (configuration-layer/make-package pkg 'layer-make-pkg-7 obj)))))

(ert-deftest test-make-package--can-override-step ()
  (let* (configuration-layer--used-layers
         (configuration-layer--indexed-layers (make-hash-table :size 1024))
         (obj (cfgl-package "testpkg"
                            :name 'testpkg
                            :step nil))
         (pkg '(testpkg :step pre))
         (expected (cfgl-package "testpkg"
                                 :name 'testpkg
                                 :owners '(layer-make-pkg-8)
                                 :step 'pre)))
    (defun layer-make-pkg-8/init-testpkg nil)
    (helper--add-layers
     `(,(cfgl-layer "layer-make-pkg-8" :name 'layer-make-pkg-8)) t)
    (should
     (equal
      expected
      (configuration-layer/make-package pkg 'layer-make-pkg-8 obj)))))

(ert-deftest test-make-package--cannot-override-protected ()
  (let* (configuration-layer--used-layers
         (configuration-layer--indexed-layers (make-hash-table :size 1024))
         (obj (cfgl-package "testpkg"
                            :name 'testpkg
                            :protected t))
         (pkg '(testpkg :protected nil))
         (expected (cfgl-package "testpkg"
                                 :name 'testpkg
                                 :owners '(layer-make-pkg-9)
                                 :protected t)))
    (defun layer-make-pkg-9/init-testpkg nil)
    (helper--add-layers
     `(,(cfgl-layer "layer-make-pkg-9"
                    :name 'layer-make-pkg-9)) t)
    (should
     (equal
      expected
      (configuration-layer/make-package pkg 'layer-make-pkg-9 obj)))))

(ert-deftest test-make-package--cannot-unexclude-excluded-package ()
  (let* (configuration-layer--used-layers
         (configuration-layer--indexed-layers (make-hash-table :size 1024))
         (obj (cfgl-package "testpkg"
                            :name 'testpkg
                            :excluded t))
         (pkg '(testpkg :excluded nil))
         (expected (cfgl-package "testpkg"
                                 :name 'testpkg
                                 :excluded t)))
    (helper--add-layers
     `(,(cfgl-layer "layer-make-pkg-10" :name 'layer-make-pkg-10)) t)
    (should
     (equal expected
            (configuration-layer/make-package pkg 'layer-make-pkg-10 obj)))))

(ert-deftest test-make-package--bootstrap-package-are-protected ()
  (let* (configuration-layer--used-layers
         (configuration-layer--indexed-layers (make-hash-table :size 1024))
         (pkg '(testpkg :step bootstrap))
         (expected (cfgl-package "testpkg"
                                 :name 'testpkg
                                 :owners '(layer-make-pkg-11)
                                 :step 'bootstrap
                                 :protected t)))
    (defun layer-make-pkg-11/init-testpkg nil)
    (helper--add-layers
     `(,(cfgl-layer "layer-make-pkg-11" :name 'layer-make-pkg-11)) t)
    (should
     (equal
      expected
      (configuration-layer/make-package pkg 'layer-make-pkg-11)))))

(ert-deftest test-make-package--make-package-requires-with-single-symbol ()
  (let* (configuration-layer--used-layers
         (configuration-layer--indexed-layers (make-hash-table :size 1024))
         (input '(testpkg :location local :step pre :requires requiredpkg))
         (expected (cfgl-package "testpkg"
                                 :name 'testpkg
                                 :owners '(layer-make-pkg-12)
                                 :location 'local
                                 :requires '(requiredpkg)
                                 :pre-layers nil
                                 :post-layers nil
                                 :step 'pre
                                 :excluded nil)))
    (defun layer-make-pkg-12/init-testpkg nil)
    (helper--add-layers
     `(,(cfgl-layer "layer-make-pkg-12" :name 'layer-make-pkg-12)) t)
    (should
     (equal
      expected
      (configuration-layer/make-package input 'layer-make-pkg-12)))))

(ert-deftest test-make-package--error-when-make-package-requires-with-multiple-symbols-no-list ()
  (let* (configuration-layer--used-layers
         (configuration-layer--indexed-layers (make-hash-table :size 1024))
         (input '(testpkg :location local :step pre :requires pkg1 pkg2 pkg3))
         (expected (cfgl-package "testpkg"
                                 :name 'testpkg
                                 :owners '(layer-make-pkg-13)
                                 :location 'local
                                 :requires '(pkg1 pkg2 pkg3)
                                 :pre-layers nil
                                 :post-layers nil
                                 :step 'pre
                                 :excluded nil)))
    (defun layer-make-pkg-13/init-testpkg nil)
    (helper--add-layers
     `(,(cfgl-layer "layer-make-pkg-13" :name 'layer-make-pkg-13)) t)
    ;; (message "%s" (configuration-layer/make-package input 'layer-make-pkg-13))
    (should
     (not (equal
           expected
           (configuration-layer/make-package input 'layer-make-pkg-13))))))

(ert-deftest test-make-package--make-package-requires-list-when-multiple-symbols ()
  (let* (configuration-layer--used-layers
         (configuration-layer--indexed-layers (make-hash-table :size 1024))
         (input '(testpkg :location local :step pre :requires (pkg1 pkg2 pkg3)))
         (expected (cfgl-package "testpkg"
                                 :name 'testpkg
                                 :owners '(layer-make-pkg-14)
                                 :location 'local
                                 :requires '(pkg1 pkg2 pkg3)
                                 :pre-layers nil
                                 :post-layers nil
                                 :step 'pre
                                 :excluded nil)))
    (defun layer-make-pkg-14/init-testpkg nil)
    (helper--add-layers
     `(,(cfgl-layer "layer-make-pkg-14" :name 'layer-make-pkg-14)) t)
    (message "%s" (configuration-layer/make-package input 'layer-make-pkg-14))
    (should
     (equal
      expected
      (configuration-layer/make-package input 'layer-make-pkg-14)))))

;; ---------------------------------------------------------------------------
;; configuration-layer//filter-distant-packages
;; ---------------------------------------------------------------------------

(defvar test-filter-distant-packages--test-data
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

(ert-deftest test-filter-distant-packages--return-only-used-packages ()
  (let* ((packages (mapcar 'car (object-assoc-list
                                 :name test-filter-distant-packages--test-data)))
         configuration-layer--used-packages
         (configuration-layer--indexed-packages (make-hash-table :size 2048)))
    (helper--add-packages test-filter-distant-packages--test-data t)
    (should
     (equal '(pkg9 pkg8 pkg6 pkg5)
            (configuration-layer//filter-distant-packages packages t)))))

(ert-deftest test-filter-distant-packages--return-only-unused-packages ()
  (let ((packages (mapcar 'car (object-assoc-list
                                :name test-filter-distant-packages--test-data)))
        configuration-layer--used-packages
        (configuration-layer--indexed-packages (make-hash-table :size 2048)))
    (helper--add-packages test-filter-distant-packages--test-data t)
    (should
     (equal '(pkg18 pkg17 pkg16 pkg15 pkg14 pkg9 pkg8 pkg7 pkg6 pkg5)
            (configuration-layer//filter-distant-packages packages nil)))))

;; ---------------------------------------------------------------------------
;; configuration-layer/make-packages-from-layers
;; ---------------------------------------------------------------------------

(ert-deftest test-make-packages-from-layers--symbols-only ()
  (let* ((layer1 (cfgl-layer "layer1"
                             :name 'layer1
                             :dir "/path/"
                             :packages '(pkg1 pkg2 pkg3)))
         configuration-layer--used-layers
         (configuration-layer--indexed-layers (make-hash-table :size 1024))
         configuration-layer--used-packages
         (configuration-layer--indexed-packages (make-hash-table :size 2048)))
    (helper--add-layers (list layer1) t)
    (defun layer1/init-pkg1 nil)
    (defun layer1/init-pkg2 nil)
    (defun layer1/init-pkg3 nil)
    (configuration-layer/make-packages-from-layers '(layer1))
    (should
     (and (equal (cfgl-package "pkg3" :name 'pkg3 :owners '(layer1))
                 (spacemacs-ht-get configuration-layer--indexed-packages 'pkg3))
          (equal (cfgl-package "pkg2" :name 'pkg2 :owners '(layer1))
                 (spacemacs-ht-get configuration-layer--indexed-packages 'pkg2))
          (equal (cfgl-package "pkg1" :name 'pkg1 :owners '(layer1))
                 (spacemacs-ht-get configuration-layer--indexed-packages 'pkg1))))))

(ert-deftest test-make-packages-from-layers--lists-only ()
  (let* ((layer1 (cfgl-layer "layer1"
                             :name 'layer1
                             :dir "/path/"
                             :packages '((pkg1 :location elpa :excluded t)
                                         (pkg2 :location (recipe blahblah))
                                         (pkg3 :location local :step pre))))
         configuration-layer--used-layers
         (configuration-layer--indexed-layers (make-hash-table :size 1024))
         configuration-layer--used-packages
         (configuration-layer--indexed-packages (make-hash-table :size 2048)))
    (helper--add-layers (list layer1) t)
    (defun layer1/init-pkg1 nil)
    (defun layer1/init-pkg2 nil)
    (defun layer1/init-pkg3 nil)
    (configuration-layer/make-packages-from-layers '(layer1))
    (should
     (and (equal (cfgl-package "pkg3"
                               :name 'pkg3
                               :owners '(layer1)
                               :location 'local
                               :step 'pre)
                 (spacemacs-ht-get configuration-layer--indexed-packages 'pkg3))
          (equal (cfgl-package "pkg2"
                               :name 'pkg2
                               :owners '(layer1)
                               :location '(recipe blahblah))
                 (spacemacs-ht-get configuration-layer--indexed-packages 'pkg2))
          (equal (cfgl-package "pkg1" :name 'pkg1 :owners '(layer1) :excluded t)
                 (spacemacs-ht-get configuration-layer--indexed-packages 'pkg1))))))

(ert-deftest test-make-packages-from-layers--symbols-and-lists ()
  (let* ((layer1 (cfgl-layer "layer1"
                             :name 'layer1
                             :dir "/path/"
                             :packages '(pkg1
                                         (pkg2 :location (recipe blahblah))
                                         (pkg3 :location local :step pre)
                                         pkg4)))
         configuration-layer--used-layers
         (configuration-layer--indexed-layers (make-hash-table :size 1024))
         configuration-layer--used-packages
         (configuration-layer--indexed-packages (make-hash-table :size 2048)))
    (helper--add-layers (list layer1) t)
    (defun layer1/init-pkg1 nil)
    (defun layer1/init-pkg2 nil)
    (defun layer1/init-pkg3 nil)
    (defun layer1/init-pkg4 nil)
    (configuration-layer/make-packages-from-layers '(layer1))
    (should
     (and (equal (cfgl-package "pkg4" :name 'pkg4 :owners '(layer1))
                 (spacemacs-ht-get configuration-layer--indexed-packages 'pkg4))
          (equal (cfgl-package "pkg3"
                               :name 'pkg3
                               :owners '(layer1)
                               :location 'local
                               :step 'pre)
                 (spacemacs-ht-get configuration-layer--indexed-packages 'pkg3))
          (equal (cfgl-package "pkg2"
                               :name 'pkg2
                               :owners '(layer1)
                               :location '(recipe blahblah))
                 (spacemacs-ht-get configuration-layer--indexed-packages 'pkg2))
          (equal (cfgl-package "pkg1" :name 'pkg1 :owners '(layer1))
                 (spacemacs-ht-get configuration-layer--indexed-packages 'pkg1))))))

(ert-deftest test-make-packages-from-layers--pkg2-has-no-owner-because-no-init-function ()
  (let* ((layer2 (cfgl-layer "layer2"
                             :name 'layer2
                             :dir "/path/"
                             :packages '(pkg1 pkg2 pkg3)))
         configuration-layer--used-layers
         (configuration-layer--indexed-layers (make-hash-table :size 1024))
         configuration-layer--used-packages
         (configuration-layer--indexed-packages (make-hash-table :size 2048))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (helper--add-layers (list layer2) t)
    (defun layer2/init-pkg1 nil)
    (defun layer2/init-pkg3 nil)
    (mocker-let
     ((configuration-layer//warning (msg &rest args) ((:output nil :occur 1))))
     (configuration-layer/make-packages-from-layers '(layer2))
     (should
      (and (equal (cfgl-package "pkg3" :name 'pkg3 :owners '(layer2))
                  (spacemacs-ht-get configuration-layer--indexed-packages 'pkg3))
           (equal (cfgl-package "pkg2" :name 'pkg2)
                  (spacemacs-ht-get configuration-layer--indexed-packages 'pkg2))
           (equal (cfgl-package "pkg1" :name 'pkg1 :owners '(layer2))
                  (spacemacs-ht-get configuration-layer--indexed-packages 'pkg1)))))))

(ert-deftest test-make-packages-from-layers--pre-init-function ()
  (let* ((layer3 (cfgl-layer "layer3"
                             :name 'layer3
                             :dir "/path/"
                             :packages '(pkg1)))
         (layer4 (cfgl-layer "layer4"
                             :name 'layer4
                             :dir "/path/"
                             :packages '(pkg1)))
         configuration-layer--used-layers
         (configuration-layer--indexed-layers (make-hash-table :size 1024))
         configuration-layer--used-packages
         (configuration-layer--indexed-packages (make-hash-table :size 2048)))
    (helper--add-layers (list layer3 layer4) t)
    (defun layer3/init-pkg1 nil)
    (defun layer4/pre-init-pkg1 nil)
    (configuration-layer/make-packages-from-layers '(layer3 layer4))
    (should (equal (cfgl-package "pkg1"
                                 :name 'pkg1
                                 :owners '(layer3)
                                 :pre-layers '(layer4))
                   (spacemacs-ht-get configuration-layer--indexed-packages 'pkg1)))))

(ert-deftest test-make-packages-from-layers--post-init-function ()
  (let* ((layer3 (cfgl-layer "layer3"
                             :name 'layer3
                             :dir "/path/"
                             :packages '(pkg1)))
         (layer5 (cfgl-layer "layer5"
                             :name 'layer5
                             :dir "/path/"
                             :packages '(pkg1)))
         configuration-layer--used-layers
         (configuration-layer--indexed-layers (make-hash-table :size 1024))
         configuration-layer--used-packages
         (configuration-layer--indexed-packages (make-hash-table :size 2048)))
    (helper--add-layers (list layer3 layer5) t)
    (defun layer3/init-pkg1 nil)
    (defun layer5/post-init-pkg1 nil)
    (configuration-layer/make-packages-from-layers '(layer3 layer5))
    (should (equal (cfgl-package "pkg1"
                                 :name 'pkg1
                                 :owners '(layer3)
                                 :post-layers '(layer5))
                   (spacemacs-ht-get configuration-layer--indexed-packages 'pkg1)))))

(ert-deftest test-make-packages-from-layers--pre-and-post-init-functions ()
  (let* ((layer3 (cfgl-layer "layer3"
                             :name 'layer3
                             :dir "/path/"
                             :packages '(pkg1)))
         (layer6 (cfgl-layer "layer6"
                             :name 'layer6
                             :dir "/path/"
                             :packages '(pkg1)))
         configuration-layer--used-layers
         (configuration-layer--indexed-layers (make-hash-table :size 1024))
         configuration-layer--used-packages
         (configuration-layer--indexed-packages (make-hash-table :size 2048)))
    (helper--add-layers (list layer3 layer6) t)
    (defun layer3/init-pkg1 nil)
    (defun layer6/pre-init-pkg1 nil)
    (defun layer6/post-init-pkg1 nil)
    (configuration-layer/make-packages-from-layers '(layer3 layer6))
    (should (equal (cfgl-package "pkg1"
                                 :name 'pkg1
                                 :owners '(layer3)
                                 :pre-layers '(layer6)
                                 :post-layers '(layer6))
                   (spacemacs-ht-get configuration-layer--indexed-packages 'pkg1)))))

(ert-deftest test-make-packages-from-layers--several-init-functions-last-one-is-the-owner ()
  (let* ((layer7 (cfgl-layer "layer7"
                             :name 'layer7
                             :dir "/path/"
                             :packages '(pkg1)))
         (layer8 (cfgl-layer "layer8"
                             :name 'layer8
                             :dir "/path/"
                             :packages '(pkg1)))
         configuration-layer--used-layers
         (configuration-layer--indexed-layers (make-hash-table :size 1024))
         configuration-layer--used-packages
         (configuration-layer--indexed-packages (make-hash-table :size 2048))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (helper--add-layers (list layer7 layer8) t)
    (defun layer7/init-pkg1 nil)
    (defun layer8/init-pkg1 nil)
    (mocker-let
     ((configuration-layer//warning (msg &rest args) ((:output nil :occur 1))))
     (configuration-layer/make-packages-from-layers '(layer7 layer8))
     (should (equal (cfgl-package "pkg1"
                                  :name 'pkg1
                                  :owners '(layer8 layer7))
                    (spacemacs-ht-get configuration-layer--indexed-packages 'pkg1))))))

(ert-deftest test-make-packages-from-layers--layer-10-excludes-pkg2-in-layer-9 ()
  (let* ((layer9 (cfgl-layer "layer9"
                             :name 'layer9
                             :dir "/path/"
                             :packages '(pkg1 pkg2)))
         (layer10 (cfgl-layer "layer10"
                              :name 'layer10
                              :dir "/path/"
                              :packages '(pkg3 (pkg2 :excluded t))))
         configuration-layer--used-layers
         (configuration-layer--indexed-layers (make-hash-table :size 1024))
         configuration-layer--used-packages
         (configuration-layer--indexed-packages (make-hash-table :size 2048)))
    (helper--add-layers (list layer9 layer10) t)
    (defun layer9/init-pkg1 nil)
    (defun layer9/init-pkg2 nil)
    (defun layer10/init-pkg3 nil)
    (configuration-layer/make-packages-from-layers '(layer9 layer10))
    (should
     (and (equal (cfgl-package "pkg3" :name 'pkg3 :owners '(layer10))
                 (spacemacs-ht-get configuration-layer--indexed-packages 'pkg3))
          (equal (cfgl-package "pkg2" :name 'pkg2 :owners '(layer9) :excluded t)
                 (spacemacs-ht-get configuration-layer--indexed-packages 'pkg2))
          (equal (cfgl-package "pkg1" :name 'pkg1 :owners '(layer9))
                 (spacemacs-ht-get configuration-layer--indexed-packages 'pkg1))))))

(ert-deftest test-make-packages-from-layers--last-owner-can-overwrite-location ()
  (let* ((layer13 (cfgl-layer "layer13"
                              :name 'layer13
                              :dir "/path/"
                              :packages '((pkg1 :location elpa))))
         (layer14 (cfgl-layer "layer14"
                              :name 'layer14
                              :dir "/path/"
                              :packages '((pkg1 :location local))))
         configuration-layer--used-layers
         (configuration-layer--indexed-layers (make-hash-table :size 1024))
         configuration-layer--used-packages
         (configuration-layer--indexed-packages (make-hash-table :size 2048))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (helper--add-layers (list layer13 layer14) t)
    (defun layer13/init-pkg1 nil)
    (defun layer14/init-pkg1 nil)
    (mocker-let
     ((configuration-layer//warning (msg &rest args) ((:output nil :occur 1))))
     (configuration-layer/make-packages-from-layers '(layer13 layer14))
     (should (equal (cfgl-package "pkg1"
                                  :name 'pkg1
                                  :owners '(layer14 layer13)
                                  :location 'local)
                    (spacemacs-ht-get configuration-layer--indexed-packages 'pkg1))))))

(ert-deftest test-make-packages-from-layers--last-owner-can-overwrite-step-nil-to-pre ()
  (let* ((layer15 (cfgl-layer "layer15"
                              :name 'layer15
                              :dir "/path/"
                              :packages '((pkg1 :step nil))))
         (layer16 (cfgl-layer "layer16"
                              :name 'layer16
                              :dir "/path/"
                              :packages '((pkg1 :step pre))))
         configuration-layer--used-layers
         (configuration-layer--indexed-layers (make-hash-table :size 1024))
         configuration-layer--used-packages
         (configuration-layer--indexed-packages (make-hash-table :size 2048))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (helper--add-layers (list layer15 layer16) t)
    (defun layer15/init-pkg1 nil)
    (defun layer16/init-pkg1 nil)
    (mocker-let
     ((configuration-layer//warning (msg &rest args) ((:output nil :occur 1))))
     (configuration-layer/make-packages-from-layers '(layer15 layer16))
     (should (equal (cfgl-package "pkg1"
                                  :name 'pkg1
                                  :owners '(layer16 layer15)
                                  :step 'pre)
                    (spacemacs-ht-get configuration-layer--indexed-packages 'pkg1))))))

(ert-deftest test-make-packages-from-layers--last-owner-cannot-overwrite-step-pre-to-nil ()
  (let* ((layer15 (cfgl-layer "layer15"
                              :name 'layer15
                              :dir "/path/"
                              :packages '((pkg1 :step pre))))
         (layer16 (cfgl-layer "layer16"
                              :name 'layer16
                              :dir "/path/"
                              :packages '((pkg1 :step nil))))
         configuration-layer--used-layers
         (configuration-layer--indexed-layers (make-hash-table :size 1024))
         configuration-layer--used-packages
         (configuration-layer--indexed-packages (make-hash-table :size 2048))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (helper--add-layers (list layer15 layer16) t)
    (defun layer15/init-pkg1 nil)
    (defun layer16/init-pkg1 nil)
    (mocker-let
     ((configuration-layer//warning (msg &rest args) ((:output nil :occur 1))))
     (configuration-layer/make-packages-from-layers '(layer15 layer16))
     (should (equal (cfgl-package "pkg1"
                                  :name 'pkg1
                                  :owners '(layer16 layer15)
                                  :step 'pre)
                    (spacemacs-ht-get configuration-layer--indexed-packages 'pkg1))))))

(ert-deftest test-make-packages-from-layers--last-owner-can-overwrite-exclude ()
  (let* ((layer17 (cfgl-layer "layer17"
                              :name 'layer17
                              :dir "/path/"
                              :packages '(pkg1)))
         (layer18 (cfgl-layer "layer18"
                              :name 'layer18
                              :dir "/path/"
                              :packages '((pkg1 :excluded t))))

         configuration-layer--used-layers
         (configuration-layer--indexed-layers (make-hash-table :size 1024))
         configuration-layer--used-packages
         (configuration-layer--indexed-packages (make-hash-table :size 2048))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (helper--add-layers (list layer17 layer18) t)
    (defun layer17/init-pkg1 nil)
    (defun layer18/init-pkg1 nil)
    (mocker-let
     ((configuration-layer//warning (msg &rest args) ((:output nil :occur 1))))
     (configuration-layer/make-packages-from-layers '(layer17 layer18))
     (should (equal (cfgl-package "pkg1"
                                  :name 'pkg1
                                  :owners '(layer18 layer17)
                                  :excluded t)
                    (spacemacs-ht-get configuration-layer--indexed-packages 'pkg1))))))

(ert-deftest test-make-packages-from-layers--owner-layer-can-define-toggle ()
  (let* ((layer19 (cfgl-layer "layer19"
                              :name 'layer19
                              :dir "/path/"
                              :packages '((pkg1 :toggle (foo-toggle)))))
         configuration-layer--used-layers
         (configuration-layer--indexed-layers (make-hash-table :size 1024))
         configuration-layer--used-packages
         (configuration-layer--indexed-packages (make-hash-table :size 2048)))
    (helper--add-layers (list layer19) t)
    (defun layer19/init-pkg1 nil)
    (let (configuration-layer--used-packages))
    (configuration-layer/make-packages-from-layers '(layer19))
    (should (equal (cfgl-package "pkg1"
                                 :name 'pkg1
                                 :owners '(layer19)
                                 :toggle '(foo-toggle))
                   (spacemacs-ht-get configuration-layer--indexed-packages 'pkg1)))))

(ert-deftest test-make-packages-from-layers--not-owner-layer-can-define-toggle-with-warning ()
  (let* ((layer20 (cfgl-layer "layer20"
                              :name 'layer20
                              :dir "/path/"
                              :packages '(pkg1)))
         (layer21 (cfgl-layer "layer21"
                              :name 'layer21
                              :dir "/path/"
                              :packages '((pkg1 :toggle (foo-toggle)))))
         configuration-layer--used-layers
         (configuration-layer--indexed-layers (make-hash-table :size 1024))
         configuration-layer--used-packages
         (configuration-layer--indexed-packages (make-hash-table :size 2048))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (helper--add-layers (list layer20 layer21) t)
    (defun layer20/init-pkg1 nil)
    (defun layer21/post-init-pkg1 nil)
    (mocker-let
     ((configuration-layer//warning (msg &rest args) ((:output nil :occur 1))))
     (configuration-layer/make-packages-from-layers '(layer20 layer21))
     (should (equal (cfgl-package "pkg1"
                                  :name 'pkg1
                                  :owners '(layer20)
                                  :post-layers '(layer21)
                                  :toggle '(foo-toggle))
                    (spacemacs-ht-get configuration-layer--indexed-packages 'pkg1))))))

(ert-deftest test-make-packages-from-layers--layer-can-override-toggle ()
  (let* ((layer22 (cfgl-layer "layer22"
                              :name 'layer22
                              :dir "/path/"
                              :packages '((pkg1 :toggle (foo-toggle)))))
         (layer23 (cfgl-layer "layer23"
                              :name 'layer23
                              :dir "/path/"
                              :packages '((pkg1 :toggle (bar-toggle)))))
         configuration-layer--used-layers
         (configuration-layer--indexed-layers (make-hash-table :size 1024))
         configuration-layer--used-packages
         (configuration-layer--indexed-packages (make-hash-table :size 2048))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (helper--add-layers (list layer22 layer23) t)
    (defun layer22/init-pkg1 nil)
    (defun layer23/init-pkg1 nil)
    (mocker-let
     ((configuration-layer//warning (msg &rest args) ((:output nil :occur 1))))
     (configuration-layer/make-packages-from-layers '(layer22 layer23))
     (should (equal (cfgl-package "pkg1"
                                  :name 'pkg1
                                  :owners '(layer23 layer22)
                                  :toggle '(bar-toggle))
                    (spacemacs-ht-get configuration-layer--indexed-packages 'pkg1))))))

(ert-deftest test-make-packages-from-layers--not-selected-packages-are-not-excluded ()
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
         configuration-layer--used-layers
         (configuration-layer--indexed-layers (make-hash-table :size 1024))
         configuration-layer--used-packages
         (configuration-layer--indexed-packages (make-hash-table :size 2048)))
    (helper--add-layers (list layer24 layer25))
    (defun layer24/post-init-pkg1 nil)
    (defun layer24/init-pkg2 nil)
    (defun layer25/init-pkg1 nil)
    (defun layer25/post-init-pkg2 nil)
    (configuration-layer/make-packages-from-layers '(layer24 layer25))
    (should
     (and (equal (cfgl-package "pkg1"
                               :name 'pkg1
                               :owners '(layer25)
                               :excluded nil)
                 (spacemacs-ht-get configuration-layer--indexed-packages 'pkg1))
          (equal (cfgl-package "pkg2"
                               :name 'pkg2
                               :owners '(layer24)
                               :location 'local
                               :excluded nil)
                 (spacemacs-ht-get configuration-layer--indexed-packages 'pkg2))))))

(ert-deftest test-make-packages-from-layers--not-selected-package-in-a-layer-can-still-be-created-with-no-owner ()
  (let* ((layer26 (cfgl-layer "layer26"
                              :name 'layer26
                              :dir "/path/"
                              :packages '(pkg1 (pkg2 :location local))
                              :selected-packages '(pkg2)))
         (layer27 (cfgl-layer "layer27"
                              :name 'layer27
                              :dir "/path/"
                              :packages '(pkg1 pkg2)))
         configuration-layer--used-layers
         (configuration-layer--indexed-layers (make-hash-table :size 1024))
         configuration-layer--used-packages
         (configuration-layer--indexed-packages (make-hash-table :size 2048)))
    (helper--add-layers (list layer26 layer27))
    (defun layer26/init-pkg1 nil)
    (defun layer26/init-pkg2 nil)
    (defun layer27/post-init-pkg1 nil)
    (defun layer27/post-init-pkg2 nil)
    (configuration-layer/make-packages-from-layers '(layer26 layer27))
    (should
     (and (equal (cfgl-package "pkg1"
                               :name 'pkg1
                               :post-layers '(layer27)
                               :owners nil)
                 (spacemacs-ht-get configuration-layer--indexed-packages 'pkg1))
          (equal (cfgl-package "pkg2"
                               :name 'pkg2
                               :owners '(layer26)
                               :post-layers '(layer27)
                               :location 'local)
                 (spacemacs-ht-get configuration-layer--indexed-packages 'pkg2))))))

(ert-deftest test-make-packages-from-layers--package-properties-read-only ()
  ;; we expect that :excluded is still nil
  (let* (configuration-layer--used-layers
         (configuration-layer--indexed-layers (make-hash-table :size 1024))
         (layer28 (cfgl-layer "layer28"
                              :name 'layer28
                              :packages '((pkg1 :excluded nil))))
         (layer29 (cfgl-layer "layer29"
                              :name 'layer29
                              :packages '((pkg1 :excluded t))))
         (expected (cfgl-package "pkg1"
                                 :name 'pkg1
                                 :owners '(layer28)
                                 :excluded nil))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun layer28/init-pkg1 nil)
    (helper--add-layers (list layer28) t)
    (helper--add-layers (list layer29))
    (mocker-let
     ((configuration-layer//warning (msg &rest args) ((:output nil :occur 1))))
     (configuration-layer/make-packages-from-layers '(layer28) t)
     (let ((configuration-layer--package-properties-read-onlyp t))
       (configuration-layer/make-packages-from-layers '(layer28 layer29)))
     (should
      (equal expected (spacemacs-ht-get configuration-layer--indexed-packages 'pkg1))))))

;; ---------------------------------------------------------------------------
;; configuration-layer/make-packages-from-dotfile
;; ---------------------------------------------------------------------------

(ert-deftest test-make-packages-from-dotfile--dotfile-declares-and-owns-one-additional-package ()
  (let* ((layer-dotfile-1 (cfgl-layer "layer-dotfile-1"
                                      :name 'layer-dotfile-1
                                      :dir "/path/"
                                      :packages '(pkg1 pkg2)))
         (dotspacemacs-additional-packages '(pkg3))
         configuration-layer--used-layers
         (configuration-layer--indexed-layers (make-hash-table :size 1024))
         configuration-layer--used-packages
         (configuration-layer--indexed-packages (make-hash-table :size 2048)))
    (defun layer-dotfile-1/init-pkg1 nil)
    (defun layer-dotfile-1/init-pkg2 nil)
    (helper--add-layers (list layer-dotfile-1) t)
    (configuration-layer/make-packages-from-layers '(layer-dotfile-1) 'used)
    (configuration-layer/make-packages-from-dotfile 'used)
    (should
     (and (equal (cfgl-package "pkg3" :name 'pkg3 :owners '(dotfile))
                 (spacemacs-ht-get configuration-layer--indexed-packages 'pkg3))
          (equal (cfgl-package "pkg2" :name 'pkg2 :owners '(layer-dotfile-1))
                 (spacemacs-ht-get configuration-layer--indexed-packages 'pkg2))
          (equal (cfgl-package "pkg1" :name 'pkg1 :owners '(layer-dotfile-1))
                 (spacemacs-ht-get configuration-layer--indexed-packages 'pkg1))))))

(ert-deftest test-make-packages-from-dotfile--dotfile-cannot-own-package-owned-by-layer ()
  (let* ((layer-dotfile-2 (cfgl-layer "layer-dotfile-2"
                                      :name 'layer-dotfile-2
                                      :dir "/path/"
                                      :packages '(pkg1)))
         (dotspacemacs-additional-packages '(pkg1))
         configuration-layer--used-layers
         (configuration-layer--indexed-layers (make-hash-table :size 1024))
         configuration-layer--used-packages
         (configuration-layer--indexed-packages (make-hash-table :size 2048)))
    (defun layer-dotfile-2/init-pkg1 nil)
    (helper--add-layers (list layer-dotfile-2) t)
    (configuration-layer/make-packages-from-layers '(layer-dotfile-2) 'used)
    (configuration-layer/make-packages-from-dotfile 'used)
    (should
     (equal (cfgl-package "pkg1" :name 'pkg1 :owners '(layer-dotfile-2))
            (spacemacs-ht-get configuration-layer--indexed-packages 'pkg1)))))

(ert-deftest test-make-packages-from-dotfile--dotfile-excludes-pkg2-in-layer-11 ()
  (let* ((layer-dotfile-3 (cfgl-layer "layer-dotfile-3"
                                      :name 'layer-dotfile-3
                                      :dir "/path/"
                                      :packages '(pkg1 pkg2 pkg3)))
         (dotspacemacs-excluded-packages '(pkg2))
         configuration-layer--used-layers
         (configuration-layer--indexed-layers (make-hash-table :size 1024))
         configuration-layer--used-packages
         (configuration-layer--indexed-packages (make-hash-table :size 2048))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun layer-dotfile-3/init-pkg1 nil)
    (defun layer-dotfile-3/init-pkg2 nil)
    (defun layer-dotfile-3/init-pkg3 nil)
    (helper--add-layers (list layer-dotfile-3) t)
    (configuration-layer/make-packages-from-layers '(layer-dotfile-3) 'used)
    (configuration-layer/make-packages-from-dotfile 'used)
    (should
     (and (equal (cfgl-package "pkg3" :name 'pkg3 :owners '(layer-dotfile-3))
                 (spacemacs-ht-get configuration-layer--indexed-packages 'pkg3))
          (equal (cfgl-package "pkg2" :name 'pkg2 :owners '(layer-dotfile-3)
                               :excluded t)
                 (spacemacs-ht-get configuration-layer--indexed-packages 'pkg2))
          (equal (cfgl-package "pkg1" :name 'pkg1 :owners '(layer-dotfile-3))
                 (spacemacs-ht-get configuration-layer--indexed-packages 'pkg1))))))

;; ---------------------------------------------------------------------------
;; configuration-layer/make-all-packages
;; ---------------------------------------------------------------------------

(ert-deftest test-make-all-packages ()
  (let* (configuration-layer--used-layers
         (configuration-layer--indexed-layers (make-hash-table :size 1024))
         configuration-layer--used-packages
         (configuration-layer--indexed-packages (make-hash-table :size 2048))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (helper--add-layers
     `(,(cfgl-layer "layerall1"
                    :name 'layerall1
                    :packages '(pkg1)
                    :dir "/layerall1/")
       ,(cfgl-layer "layerall2"
                    :name 'layerall2
                    :packages '(pkg2 pkg6)
                    :dir "/layerall2/")
       ,(cfgl-layer "layerall3"
                    :name 'layerall3
                    :packages '(pkg3)
                    :dir "/layerall3/")
       ,(cfgl-layer "layerall4"
                    :name 'layerall4
                    :packages '(pkg4 pkg7 pkg8)
                    :dir "/layerall4/")
       ,(cfgl-layer "layerall5"
                    :name 'layerall5
                    :packages '(pkg5 pkg9)
                    :dir "/layerall5/")))
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
     ;; skip layer declaration since we manually set
     ;; the variable `configuration-layer--indexed-layers'
     ;; Moreover `configuration-layer/declare-layers' requires a valid
     ;; path on disk etc...
     ((configuration-layer/declare-layers (layers-specs &optional skip-layer-deps)
                                          ((:output nil))))
     (configuration-layer/make-all-packages 'no-discovery 'no-layer-deps)
     (should (null configuration-layer--used-packages))
     (should (equal '(pkg1
                      pkg6
                      pkg2
                      pkg3
                      pkg8
                      pkg7
                      pkg4
                      pkg9
                      pkg5)
                    (spacemacs-ht-keys configuration-layer--indexed-packages))))))

;; ---------------------------------------------------------------------------
;; configuration-layer//pre-configure-package
;; ---------------------------------------------------------------------------

(ert-deftest test-pre-configure-package--pre-init-is-evaluated ()
  (let ((pkg (cfgl-package "pkg" :name 'pkg :owners '(layer1) :pre-layers '(layer2)))
        configuration-layer--used-layers
        (configuration-layer--indexed-layers (make-hash-table :size 1024))
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (helper--add-layers `(,(cfgl-layer "layer1" :name 'layer1)
                          ,(cfgl-layer "layer2" :name 'layer2)) t)
    (defun layer1/init-pkg nil)
    (defun layer2/pre-init-pkg nil)
    (mocker-let
     ((spacemacs-buffer/message (m) ((:output nil)))
      (layer2/pre-init-pkg nil ((:output nil :occur 1))))
     (configuration-layer//pre-configure-package pkg))))

;; ---------------------------------------------------------------------------
;; configuration-layer//post-configure-package
;; ---------------------------------------------------------------------------

(ert-deftest test-post-configure-package--post-init-is-evaluated ()
  (let ((pkg (cfgl-package "pkg" :name 'pkg :owners '(layer1) :post-layers '(layer2)))
        configuration-layer--used-layers
        (configuration-layer--indexed-layers (make-hash-table :size 1024))
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (helper--add-layers `(,(cfgl-layer "layer1" :name 'layer1)
                          ,(cfgl-layer "layer2" :name 'layer2)) t)
    (defun layer1/init-pkg nil)
    (defun layer2/post-init-pkg nil)
    (mocker-let
     ((spacemacs-buffer/message (m) ((:output nil)))
      (layer2/post-init-pkg nil ((:output nil :occur 1))))
     (configuration-layer//post-configure-package pkg))))

;; ---------------------------------------------------------------------------
;; configuration-layer//configure-package
;; ---------------------------------------------------------------------------

(ert-deftest test-configure-package--init-is-evaluated ()
  (let ((pkg (cfgl-package "pkg" :name 'pkg :owners '(layer1)))
        configuration-layer--used-layers
        (configuration-layer--indexed-layers (make-hash-table :size 1024))
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (helper--add-layers `(,(cfgl-layer "layer1" :name 'layer1)) t)
    (defun layer1/init-pkg nil)
    (mocker-let
     ((spacemacs/update-progress-bar nil ((:output nil)))
      (spacemacs-buffer/message (m) ((:output nil)))
      (layer1/init-pkg nil ((:output nil :occur 1))))
     (configuration-layer//configure-package pkg))))

(ert-deftest test-configure-package--disabled-for-does-not-call-pre-post-init ()
  (let ((pkg (cfgl-package "pkg" :name 'pkg :owners '(layer1)
                           :pre-layers '(layer2)
                           :post-layers '(layer3)))
        configuration-layer--used-layers
        (configuration-layer--indexed-layers (make-hash-table :size 1024))
        (witness nil)
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (helper--add-layers
     `(,(cfgl-layer "layer1" :name 'layer1 :disabled-for '(layer2 layer3))
       ,(cfgl-layer "layer2" :name 'layer2)
       ,(cfgl-layer "layer2" :name 'layer3)) t)
    (defun layer1/init-pkg () (push 'init witness))
    (defun layer2/pre-init-pkg () (push 'pre-init witness))
    (defun layer3/post-init-pkg () (push 'post-init witness))
    (mocker-let
     ((spacemacs/update-progress-bar nil ((:output nil)))
      (spacemacs-buffer/message (m) ((:output nil))))
     (configuration-layer//configure-package pkg)
     (should (equal '(init) witness)))))

(ert-deftest test-configure-package--enabled-for-unspecified-does-call-pre-post-init ()
  (let ((pkg (cfgl-package "pkg" :name 'pkg :owners '(layer1)
                           :pre-layers '(layer2)
                           :post-layers '(layer3)))
        configuration-layer--used-layers
        (configuration-layer--indexed-layers (make-hash-table :size 1024))
        (witness nil)
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (helper--add-layers
     `(,(cfgl-layer "layer1" :name 'layer1 :enabled-for 'unspecified)
       ,(cfgl-layer "layer2" :name 'layer2)
       ,(cfgl-layer "layer2" :name 'layer3)) t)
    (defun layer1/init-pkg () (push 'init witness))
    (defun layer2/pre-init-pkg () (push 'pre-init witness))
    (defun layer3/post-init-pkg () (push 'post-init witness))
    (mocker-let
     ((spacemacs/update-progress-bar nil ((:output nil)))
      (spacemacs-buffer/message (m) ((:output nil))))
     (configuration-layer//pre-configure-package pkg)
     (configuration-layer//configure-package pkg)
     (configuration-layer//post-configure-package pkg)
     (should (equal '(post-init init pre-init) witness)))))

(ert-deftest test-configure-package--enabled-for-nil-does-not-call-pre-post-init ()
  (let ((pkg (cfgl-package "pkg" :name 'pkg :owners '(layer1)
                           :pre-layers '(layer2)
                           :post-layers '(layer3)))
        configuration-layer--used-layers
        (configuration-layer--indexed-layers (make-hash-table :size 1024))
        (witness nil)
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (helper--add-layers
     `(,(cfgl-layer "layer1" :name 'layer1 :enabled-for nil)
       ,(cfgl-layer "layer2" :name 'layer2)
       ,(cfgl-layer "layer2" :name 'layer3)) t)
    (defun layer1/init-pkg () (push 'init witness))
    (defun layer2/pre-init-pkg () (push 'pre-init witness))
    (defun layer3/post-init-pkg () (push 'post-init witness))
    (mocker-let
     ((spacemacs/update-progress-bar nil ((:output nil)))
      (spacemacs-buffer/message (m) ((:output nil))))
     (configuration-layer//configure-package pkg)
     (should (equal '(init) witness)))))

(ert-deftest test-configure-package--enabled-for-partial ()
  (let ((pkg (cfgl-package "pkg" :name 'pkg :owners '(layer1)
                           :pre-layers '(layer2)
                           :post-layers '(layer3)))
        configuration-layer--used-layers
        (configuration-layer--indexed-layers (make-hash-table :size 1024))
        (witness nil)
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (helper--add-layers
     `(,(cfgl-layer "layer1" :name 'layer1 :enabled-for '(layer2))
       ,(cfgl-layer "layer2" :name 'layer2)
       ,(cfgl-layer "layer2" :name 'layer3)) t)
    (defun layer1/init-pkg () (push 'init witness))
    (defun layer2/pre-init-pkg () (push 'pre-init witness))
    (defun layer3/post-init-pkg () (push 'post-init witness))
    (mocker-let
     ((spacemacs/update-progress-bar nil ((:output nil)))
      (spacemacs-buffer/message (m) ((:output nil))))
     (configuration-layer//pre-configure-package pkg)
     (configuration-layer//configure-package pkg)
     (should (equal '(init pre-init) witness)))))

;; ---------------------------------------------------------------------------
;; configuration-layer//configure-packages-2
;; ---------------------------------------------------------------------------

(ert-deftest test-configure-packages-2--pre-init-is-evaluated-before-init ()
  (let ((pkg (cfgl-package "pkg" :name 'pkg :owners '(layer1) :pre-layers '(layer2)))
        configuration-layer--used-layers
        (configuration-layer--indexed-layers (make-hash-table :size 1024))
        configuration-layer--used-packages
        (configuration-layer--indexed-packages (make-hash-table :size 2048))
        (witness nil)
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (helper--add-layers `(,(cfgl-layer "layer1" :name 'layer1)
                          ,(cfgl-layer "layer2" :name 'layer2)) t)
    (helper--add-packages (list pkg) t)
    (defun layer1/init-pkg () (push 'init witness))
    (defun layer2/pre-init-pkg () (push 'pre-init witness))
    (configuration-layer//configure-packages-2 `(,(oref pkg :name)))
    (should (equal '(init pre-init) witness))))

(ert-deftest test-configure-packages-2--post-init-is-evaluated-after-init ()
  (let ((pkg (cfgl-package "pkg" :name 'pkg :owners '(layer1) :post-layers '(layer2)))
        configuration-layer--used-layers
        (configuration-layer--indexed-layers (make-hash-table :size 1024))
        configuration-layer--used-packages
        (configuration-layer--indexed-packages (make-hash-table :size 2048))
        (witness nil)
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (helper--add-layers `(,(cfgl-layer "layer1" :name 'layer1)
                          ,(cfgl-layer "layer2" :name 'layer2)) t)
    (helper--add-packages (list pkg) t)
    (defun layer1/init-pkg () (push 'init witness))
    (defun layer2/post-init-pkg () (push 'post-init witness))
    (configuration-layer//configure-packages-2 `(,(oref pkg :name)))
    (should (equal '(post-init init) witness))))

(ert-deftest test-configure-packages-2--package-w/-layer-owner-is-configured()
  (let ((pkg (cfgl-package "pkg" :name 'pkg :owners '(layer1)))
        configuration-layer--used-packages
        (configuration-layer--indexed-packages (make-hash-table :size 2048))
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (helper--add-packages (list pkg) t)
    (mocker-let
     ((configuration-layer//configure-package (p) ((:occur 1))))
     (configuration-layer//configure-packages-2 `(,(oref pkg :name))))))

(ert-deftest test-configure-packages-2--site-package-is-configured()
  (let ((pkg (cfgl-package "pkg" :name 'pkg :owners '(layer1) :location 'site))
        configuration-layer--used-packages
        (configuration-layer--indexed-packages (make-hash-table :size 2048))
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (helper--add-packages (list pkg) t)
    (mocker-let
     ((configuration-layer//configure-package (p) ((:occur 1))))
     (configuration-layer//configure-packages-2 `(,(oref pkg :name))))))

(ert-deftest test-configure-packages-2--toggle-t-is-configured ()
  (let ((pkg (cfgl-package "pkg" :name 'pkg :owners '(layer1) :toggle t))
        configuration-layer--used-packages
        (configuration-layer--indexed-packages (make-hash-table :size 2048))
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (helper--add-packages (list pkg) t)
    (mocker-let
     ((configuration-layer//configure-package (p) ((:occur 1))))
     (configuration-layer//configure-packages-2 `(,(oref pkg :name))))))

(ert-deftest test-configure-packages-2--toggle-nil-is-not-configured ()
  (let ((pkg (cfgl-package "pkg" :name 'pkg :owners '(layer1) :toggle nil))
        configuration-layer--used-packages
        (configuration-layer--indexed-packages (make-hash-table :size 2048))
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (helper--add-packages (list pkg) t)
    (mocker-let
     ((configuration-layer//configure-package (p) nil)
      (spacemacs-buffer/message (m) ((:output nil))))
     (configuration-layer//configure-packages-2 `(,(oref pkg :name))))))

(ert-deftest test-configure-packages-2--protected-package-is-configured()
  (let ((pkg (cfgl-package "pkg" :name 'pkg :owners '(layer1) :protected t))
        configuration-layer--used-packages
        (configuration-layer--indexed-packages (make-hash-table :size 2048))
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (helper--add-packages (list pkg) t)
    (mocker-let
     ((configuration-layer//configure-package (p) ((:occur 1))))
     (configuration-layer//configure-packages-2 `(,(oref pkg :name))))))

(ert-deftest test-configure-packages-2--protected-excluded-package-is-configured()
  (let ((pkg (cfgl-package "pkg" :name 'pkg :owners '(layer1) :excluded t :protected t))
        configuration-layer--used-packages
        (configuration-layer--indexed-packages (make-hash-table :size 2048))
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (helper--add-packages (list pkg) t)
    (mocker-let
     ((configuration-layer//configure-package (p) ((:occur 1))))
     (configuration-layer//configure-packages-2 `(,(oref pkg :name))))))

(ert-deftest test-configure-packages-2--excluded-package-is-not-configured()
  (let ((pkg (cfgl-package "pkg" :name 'pkg :owners '(layer1) :excluded t))
        configuration-layer--used-packages
        (configuration-layer--indexed-packages (make-hash-table :size 2048))
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (helper--add-packages (list pkg) t)
    (mocker-let
     ((configuration-layer//configure-package (p) nil)
      (spacemacs-buffer/message (m) ((:output nil))))
     (configuration-layer//configure-packages-2 `(,(oref pkg :name))))))

(ert-deftest test-configure-packages-2--package-w/o-owner-is-not-configured()
  (let ((pkg (cfgl-package "pkg" :name 'pkg :owners nil))
        configuration-layer--used-packages
        (configuration-layer--indexed-packages (make-hash-table :size 2048))
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (helper--add-packages (list pkg) t)
    (mocker-let
     ((configuration-layer//configure-package (p) nil)
      (spacemacs-buffer/message (m) ((:output nil))))
     (configuration-layer//configure-packages-2 `(,(oref pkg :name))))))

(ert-deftest
    test-configure-packages-2--package-owned-by-dotfile-is-not-configured()
  (let ((pkg (cfgl-package "pkg" :name 'pkg :owners '(dotfile)))
        configuration-layer--used-packages
        (configuration-layer--indexed-packages (make-hash-table :size 2048))
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (helper--add-packages (list pkg) t)
    (mocker-let
     ((configuration-layer//configure-package (p) nil)
      (spacemacs-buffer/message (m) ((:output nil))))
     (configuration-layer//configure-packages-2 `(,(oref pkg :name))))))

(ert-deftest test-configure-packages-2--lazy-install-package-is-not-configured()
  (let ((pkg (cfgl-package "pkg" :name 'pkg :owners '(layer) :lazy-install t))
        configuration-layer--used-packages
        (configuration-layer--indexed-packages (make-hash-table :size 2048))
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (helper--add-packages (list pkg) t)
    (mocker-let
     ((configuration-layer//configure-package (p) nil)
      (spacemacs-buffer/message (m) ((:output nil))))
     (configuration-layer//configure-packages-2 `(,(oref pkg :name))))))

(ert-deftest
    test-configure-packages-2--local-package-w/-layer-owner-update-load-path()
  (let ((pkg (cfgl-package "pkg" :name 'pkg :owners '(layer1) :location 'local))
        configuration-layer--used-layers
        (configuration-layer--indexed-layers (make-hash-table :size 1024))
        configuration-layer--used-packages
        (configuration-layer--indexed-packages (make-hash-table :size 2048))
        (expected-load-path load-path)
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (helper--add-layers `(,(cfgl-layer "layer1" :name 'layer1 :dir "/path/")) t)
    (helper--add-packages (list pkg) t)
    (mocker-let
     ((configuration-layer//configure-package (p) ((:occur 1))))
     (configuration-layer//configure-packages-2 `(,(oref pkg :name)))
     (push "/path/local/pkg/" expected-load-path)
     (should (equal expected-load-path load-path)))))

(ert-deftest
    test-configure-packages-2--local-package-w/-dotfile-owner-update-load-path()
  (let ((pkg (cfgl-package "pkg" :name 'pkg :owners '(dotfile) :location 'local))
        configuration-layer--used-packages
        (configuration-layer--indexed-packages (make-hash-table :size 2048))
        (expected-load-path load-path)
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (helper--add-packages (list pkg) t)
    (configuration-layer//configure-packages-2 `(,(oref pkg :name)))
    (push (file-name-as-directory
           (concat spacemacs-private-directory "local/pkg"))
          expected-load-path)
    (should (equal expected-load-path load-path))))

(ert-deftest
    test-configure-packages-2--local-package-w/o-owner-doesnt-update-load-path()
  (let ((pkg (cfgl-package "pkg" :name 'pkg :owners nil :location 'local))
        configuration-layer--used-packages
        (configuration-layer--indexed-packages (make-hash-table :size 2048))
        (old-load-path load-path)
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (helper--add-packages (list pkg) t)
    (mocker-let
     ((spacemacs-buffer/message (m) ((:output nil))))
     (configuration-layer//configure-packages-2 `(,(oref pkg :name)))
     (should (equal load-path old-load-path)))))

(ert-deftest
    test-configure-packages-2--local-package-w/-string-location-update-load-path()
  (let ((pkg (cfgl-package "pkg"
                           :name 'pkg
                           :owners '(dotfile)
                           :location spacemacs-docs-directory))
        configuration-layer--used-packages
        (configuration-layer--indexed-packages (make-hash-table :size 2048))
        (expected-load-path load-path)
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (helper--add-packages (list pkg) t)
    (configuration-layer//configure-packages-2 `(,(oref pkg :name)))
    (push spacemacs-docs-directory expected-load-path)
    (should (equal expected-load-path load-path))))

(ert-deftest
    test-configure-packages-2--local-package-w/-bad-string-location-gives-warning()
  (let ((pkg (cfgl-package "pkg"
                           :name 'pkg
                           :owners '(dotfile)
                           :location "/this/directory/does/not/exist/"))
        configuration-layer--used-packages
        (configuration-layer--indexed-packages (make-hash-table :size 2048))
        (mocker-mock-default-record-cls 'mocker-stub-record))
    (helper--add-packages (list pkg) t)
    (mocker-let
     ((configuration-layer//warning
       (msg &rest args)
       ((:record-cls 'mocker-stub-record :output nil :occur 1))))
     (configuration-layer//configure-packages-2 `(,(oref pkg :name))))))

;; ---------------------------------------------------------------------------
;; configuration-layer//sort-packages
;; ---------------------------------------------------------------------------

(ert-deftest test-sort-packages--example ()
  (let ((pkgs '(pkg4 pkg3 pkg6 pkg2 pkg1)))
    (should (equal '(pkg1 pkg2 pkg3 pkg4 pkg6)
                   (configuration-layer//sort-packages pkgs)))))

;; ---------------------------------------------------------------------------
;; configuration-layer//package-has-recipe-p
;; ---------------------------------------------------------------------------

(ert-deftest test-package-has-a-recipe-p--true ()
  (let (configuration-layer--used-layers
        (configuration-layer--indexed-layers (make-hash-table :size 1024))
        configuration-layer--used-packages
        (configuration-layer--indexed-packages (make-hash-table :size 2048)))
    (helper--add-layers `(,(cfgl-layer "layer1" :name 'layer1)) t)
    (helper--add-packages
     `(,(configuration-layer/make-package '(pkg1 :location (recipe blah))
                                          'layer1)
       ,(configuration-layer/make-package '(pkg2 :location elpa) 'layer1)) t)
    (should (configuration-layer//package-has-recipe-p 'pkg1))))

(ert-deftest test-package-has-a-recipe-p--false ()
  (let (configuration-layer--used-layers
        (configuration-layer--indexed-layers (make-hash-table :size 1024))
        configuration-layer--used-packages
        (configuration-layer--indexed-packages (make-hash-table :size 2048)))
    (helper--add-layers `(,(cfgl-layer "layer1" :name 'layer1)) t)
    (helper--add-packages
     `(,(configuration-layer/make-package '(pkg1 :location (recipe blah))
                                          'layer1)
       ,(configuration-layer/make-package '(pkg2 :location elpa) 'layer1)) t)
    (should (not (configuration-layer//package-has-recipe-p 'pkg2)))))

;; ---------------------------------------------------------------------------
;; configuration-layer//get-package-recipe
;; ---------------------------------------------------------------------------

(ert-deftest test-get-package-recipe--return-recipe-if-package-has-one ()
  (let (configuration-layer--used-layers
        (configuration-layer--indexed-layers (make-hash-table :size 1024))
        configuration-layer--used-packages
        (configuration-layer--indexed-packages (make-hash-table :size 2048)))
    (defun layer-no-recipe-1/init-pkg1 nil)
    (defun layer-no-recipe-1/init-pkg2 nil)
    (helper--add-layers
     `(,(cfgl-layer "layer-no-recipe-1" :name 'layer-no-recipe-1)) t)
    (helper--add-packages
     `(,(configuration-layer/make-package '(pkg1 :location (recipe blah))
                                          'layer-no-recipe-1)
       ,(configuration-layer/make-package '(pkg2 :location elpa)
                                          'layer-no-recipe-1)) t)
    (should (eq 'pkg1
                (car (configuration-layer//get-package-recipe 'pkg1))))))

(ert-deftest test-get-package-recipe--return-nil-if-package-has-no-recipe ()
  (let (configuration-layer--used-layers
        (configuration-layer--indexed-layers (make-hash-table :size 1024))
        configuration-layer--used-packages
        (configuration-layer--indexed-packages (make-hash-table :size 2048)))
    (defun layer-no-recipe-2/init-pkg1 nil)
    (defun layer-no-recipe-2/init-pkg2 nil)
    (helper--add-layers
     `(,(cfgl-layer "layer-no-recipe-2" :name 'layer-no-recipe-2)) t)
    (helper--add-packages
     `(,(configuration-layer/make-package '(pkg1 :location (recipe blah))
                                          'layer-no-recipe-2)
       ,(configuration-layer/make-package '(pkg2 :location elpa)
                                          'layer-no-recipe-2)) t)
    (should (null (configuration-layer//get-package-recipe 'pkg2)))))

;; ---------------------------------------------------------------------------
;; configuration-layer/filter-objects
;; ---------------------------------------------------------------------------

(ert-deftest test-filter-packages--example-filter-excluded-packages ()
  (defun layer-filter-1/init-pkg1 nil)
  (defun layer-filter-1/init-pkg2 nil)
  (defun layer-filter-1/init-pkg3 nil)
  (defun layer-filter-1/init-pkg4 nil)
  (defun layer-filter-1/init-pkg5 nil)
  (defun layer-filter-1/init-pkg6 nil)
  (defun layer-filter-1/init-pkg7 nil)
  (defun layer-filter-1/init-pkg8 nil)
  (let* (configuration-layer--used-layers
         (configuration-layer--indexed-layers (make-hash-table :size 1024))
         (pkg1 (configuration-layer/make-package 'pkg1 'layer-filter-1))
         (pkg2 (configuration-layer/make-package 'pkg2 'layer-filter-1))
         (pkg3 (configuration-layer/make-package 'pkg3 'layer-filter-1))
         (pkg4 (configuration-layer/make-package 'pkg4 'layer-filter-1))
         (pkg5 (configuration-layer/make-package 'pkg5 'layer-filter-1))
         (pkg6 (configuration-layer/make-package 'pkg6 'layer-filter-1))
         (pkg7 (configuration-layer/make-package 'pkg7 'layer-filter-1))
         (pkg8 (configuration-layer/make-package 'pkg8 'layer-filter-1))
         (pkgs (list pkg1 pkg2 pkg3 pkg4 pkg5 pkg6 pkg7 pkg8)))
    (helper--add-layers
     `(,(cfgl-layer "layer-filter-1" :name 'layer-filter-1)) t)
    (oset pkg1 :excluded t)
    (oset pkg3 :excluded t)
    (oset pkg5 :excluded t)
    (oset pkg6 :excluded t)
    (should
     (equal (list (cfgl-package "pkg2" :name 'pkg2 :owners '(layer-filter-1))
                  (cfgl-package "pkg4" :name 'pkg4 :owners '(layer-filter-1))
                  (cfgl-package "pkg7" :name 'pkg7 :owners '(layer-filter-1))
                  (cfgl-package "pkg8" :name 'pkg8 :owners '(layer-filter-1)))
            (configuration-layer/filter-objects
             pkgs (lambda (x)
                    (not (oref x :excluded))))))))

(ert-deftest test-filter-packages--expample-filter-local-packages ()
  (defun layer-filter-2/init-pkg1 nil)
  (defun layer-filter-2/init-pkg2 nil)
  (defun layer-filter-2/init-pkg3 nil)
  (defun layer-filter-2/init-pkg4 nil)
  (defun layer-filter-2/init-pkg5 nil)
  (defun layer-filter-2/init-pkg6 nil)
  (defun layer-filter-2/init-pkg7 nil)
  (defun layer-filter-2/init-pkg8 nil)
  (let* (configuration-layer--used-layers
         (configuration-layer--indexed-layers (make-hash-table :size 1024))
         (pkg1 (configuration-layer/make-package 'pkg1 'layer-filter-2))
         (pkg2 (configuration-layer/make-package 'pkg2 'layer-filter-2))
         (pkg3 (configuration-layer/make-package 'pkg3 'layer-filter-2))
         (pkg4 (configuration-layer/make-package 'pkg4 'layer-filter-2))
         (pkg5 (configuration-layer/make-package 'pkg5 'layer-filter-2))
         (pkg6 (configuration-layer/make-package 'pkg6 'layer-filter-2))
         (pkg7 (configuration-layer/make-package 'pkg7 'layer-filter-2))
         (pkg8 (configuration-layer/make-package 'pkg8 'layer-filter-2))
         (pkgs (list pkg1 pkg2 pkg3 pkg4 pkg5 pkg6 pkg7 pkg8)))
    (helper--add-layers
     `(,(cfgl-layer "layer-filter-2" :name 'layer-filter-2)) t)
    (oset pkg1 :location 'local)
    (oset pkg3 :location 'local)
    (oset pkg5 :location 'local)
    (oset pkg6 :location 'local)
    (should
     (equal (list (cfgl-package "pkg2" :name 'pkg2 :owners '(layer-filter-2))
                  (cfgl-package "pkg4" :name 'pkg4 :owners '(layer-filter-2))
                  (cfgl-package "pkg7" :name 'pkg7 :owners '(layer-filter-2))
                  (cfgl-package "pkg8" :name 'pkg8 :owners '(layer-filter-2)))
            (configuration-layer/filter-objects
             pkgs (lambda (x)
                    (not (eq 'local (oref x :location)))))))))


(ert-deftest test-filter-packages--example-filter-packages-local-or-excluded ()
  (defun layer-filter-3/init-pkg1 nil)
  (defun layer-filter-3/init-pkg2 nil)
  (defun layer-filter-3/init-pkg3 nil)
  (defun layer-filter-3/init-pkg4 nil)
  (defun layer-filter-3/init-pkg5 nil)
  (defun layer-filter-3/init-pkg6 nil)
  (defun layer-filter-3/init-pkg7 nil)
  (defun layer-filter-3/init-pkg8 nil)
  (let* (configuration-layer--used-layers
         (configuration-layer--indexed-layers (make-hash-table :size 1024))
         (pkg1 (configuration-layer/make-package 'pkg1 'layer-filter-3))
         (pkg2 (configuration-layer/make-package 'pkg2 'layer-filter-3))
         (pkg3 (configuration-layer/make-package 'pkg3 'layer-filter-3))
         (pkg4 (configuration-layer/make-package 'pkg4 'layer-filter-3))
         (pkg5 (configuration-layer/make-package 'pkg5 'layer-filter-3))
         (pkg6 (configuration-layer/make-package 'pkg6 'layer-filter-3))
         (pkg7 (configuration-layer/make-package 'pkg7 'layer-filter-3))
         (pkg8 (configuration-layer/make-package 'pkg8 'layer-filter-3))
         (pkgs (list pkg1 pkg2 pkg3 pkg4 pkg5 pkg6 pkg7 pkg8)))
    (helper--add-layers
     `(,(cfgl-layer "layer-filter-4" :name 'layer-filter-4)) t)
    (oset pkg1 :location 'local)
    (oset pkg1 :excluded t)
    (oset pkg3 :location 'local)
    (oset pkg5 :location 'local)
    (oset pkg6 :location 'local)
    (oset pkg6 :excluded t)
    (oset pkg7 :excluded t)
    (should
     (equal (list (cfgl-package "pkg2" :name 'pkg2 :owners '(layer-filter-3))
                  (cfgl-package "pkg4" :name 'pkg4 :owners '(layer-filter-3))
                  (cfgl-package "pkg8" :name 'pkg8 :owners '(layer-filter-3)))
            (configuration-layer/filter-objects
             pkgs (lambda (x)
                    (and (not (eq 'local (oref x :location)))
                         (not (oref x :excluded)))))))))

(ert-deftest test-filter-packages--example-filter-packages-both-local-and-excluded ()
  (defun layer-filter-4/init-pkg1 nil)
  (defun layer-filter-4/init-pkg2 nil)
  (defun layer-filter-4/init-pkg3 nil)
  (defun layer-filter-4/init-pkg4 nil)
  (defun layer-filter-4/init-pkg5 nil)
  (defun layer-filter-4/init-pkg6 nil)
  (defun layer-filter-4/init-pkg7 nil)
  (defun layer-filter-4/init-pkg8 nil)
  (let* (configuration-layer--used-layers
         (configuration-layer--indexed-layers (make-hash-table :size 1024))
         (pkg1 (configuration-layer/make-package 'pkg1 'layer-filter-4))
         (pkg2 (configuration-layer/make-package 'pkg2 'layer-filter-4))
         (pkg3 (configuration-layer/make-package 'pkg3 'layer-filter-4))
         (pkg4 (configuration-layer/make-package 'pkg4 'layer-filter-4))
         (pkg5 (configuration-layer/make-package 'pkg5 'layer-filter-4))
         (pkg6 (configuration-layer/make-package 'pkg6 'layer-filter-4))
         (pkg7 (configuration-layer/make-package 'pkg7 'layer-filter-4))
         (pkg8 (configuration-layer/make-package 'pkg8 'layer-filter-4))
         (pkgs (list pkg1 pkg2 pkg3 pkg4 pkg5 pkg6 pkg7 pkg8)))
    (helper--add-layers
     `(,(cfgl-layer "layer-filter-4" :name 'layer-filter-4)) t)
    (oset pkg1 :location 'local)
    (oset pkg1 :excluded t)
    (oset pkg3 :location 'local)
    (oset pkg5 :excluded t)
    (oset pkg6 :location 'local)
    (oset pkg6 :excluded t)
    (should
     (equal (list (cfgl-package "pkg2"
                                :name 'pkg2
                                :owners '(layer-filter-4))
                  (cfgl-package "pkg3"
                                :name 'pkg3
                                :location 'local
                                :owners '(layer-filter-4))
                  (cfgl-package "pkg4" :name 'pkg4
                                :owners '(layer-filter-4))
                  (cfgl-package "pkg5" :name 'pkg5
                                :excluded t
                                :owners '(layer-filter-4))
                  (cfgl-package "pkg7" :name 'pkg7
                                :owners '(layer-filter-4))
                  (cfgl-package "pkg8" :name 'pkg8
                                :owners '(layer-filter-4)))
            (configuration-layer/filter-objects
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
  (let (configuration-layer--used-layers
        (configuration-layer--indexed-layers (make-hash-table :size 1024))
        configuration-layer--used-packages
        (configuration-layer--indexed-packages (make-hash-table :size 2048))
        (auto-mode-alist '(("\\.pkg1\\'" . pkg1)
                           ("\\.pkg2\\'" . pkg2))))
    (helper--add-layers
     (list (cfgl-layer "layer" :name 'layer :packages '(pkg1 pkg2))) t)
    (helper--add-packages
     (list (cfgl-package "pkg1" :name 'pkg1 :owners '(layer))
           (cfgl-package "pkg2" :name 'pkg2 :owners '(layer))) t)
    (should (equal '((pkg2 . "\\(\\.pkg2\\'\\)")
                     (pkg1 . "\\(\\.pkg1\\'\\)"))
                   (configuration-layer//lazy-install-extensions-for-layer
                    'layer)))))

(ert-deftest test-lazy-install-extensions-for-layer--not-owned-package ()
  (let ((configuration-layer--layers
         (list (cfgl-layer "layer" :name 'layer :packages '(pkg1))))
        (configuration-layer--used-packages
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
  (let (configuration-layer--used-packages
        (configuration-layer--indexed-packages (make-hash-table :size 2048)))
    (helper--add-packages
     (list (cfgl-package "pkg1" :name 'pkg1 :location 'built-in)
           (cfgl-package "pkg2" :name 'pkg2 :location 'built-in)
           (cfgl-package "pkg3" :name 'pkg3 :location 'elpa)
           (cfgl-package "pkg4" :name 'pkg4 :location 'elpa)
           (cfgl-package "pkg5" :name 'pkg5 :location 'elpa)
           (cfgl-package "pkg6" :name 'pkg6 :location 'local)
           (cfgl-package "pkg7" :name 'pkg7 :location '(recipe :foo bar))
           (cfgl-package "pkg8" :name 'pkg8 :location '(recipe :foo bar))) t)
    (should (equal '((total 8)
                     (elpa 3)
                     (recipe 2)
                     (local 1)
                     (built-in 2))
                   (configuration-layer/configured-packages-stats
                    configuration-layer--used-packages)))))

(ert-deftest test-configured-packages-stats--sum-is-correct ()
  (let (stats
        configuration-layer--used-packages
        (configuration-layer--indexed-packages (make-hash-table :size 2048)))
    (helper--add-packages
     (list (cfgl-package "pkg1" :name 'pkg1 :location 'built-in)
           (cfgl-package "pkg2" :name 'pkg2 :location 'built-in)
           (cfgl-package "pkg3" :name 'pkg3 :location 'elpa)
           (cfgl-package "pkg4" :name 'pkg4 :location 'elpa)
           (cfgl-package "pkg5" :name 'pkg5 :location 'elpa)
           (cfgl-package "pkg6" :name 'pkg6 :location 'local)
           (cfgl-package "pkg7" :name 'pkg7 :location '(recipe :foo bar))
           (cfgl-package "pkg8" :name 'pkg8 :location '(recipe :foo bar))) t)
    (setq stats (configuration-layer/configured-packages-stats
                 configuration-layer--used-packages))
    (should (equal 8 (+ (cadr (assq 'elpa stats))
                        (cadr (assq 'recipe stats))
                        (cadr (assq 'local stats))
                        (cadr (assq 'built-in stats)))))))

;; ---------------------------------------------------------------------------
;; configuration-layer//stable-elpa-verify-archive
;; ---------------------------------------------------------------------------

(ert-deftest test-stable-elpa-verify-archive--archive-not-found-is-fatal-error ()
  (mocker-let
   ((configuration-layer//stable-elpa-tarball-local-file
     nil ((:record-cls 'mocker-stub-record
                       :output
                       (concat spacemacs-test-directory
                               "core/data/not-found.tar.gz")
                       :occur 1)))
    (configuration-layer//stable-elpa-tarball-local-sign-file
     nil ((:record-cls 'mocker-stub-record
                       :output
                       (concat spacemacs-test-directory
                               "core/data/stable-elpa.sig")
                       :occur 1)))
    (configuration-layer//error
     (msg &rest args) ((:record-cls 'mocker-stub-record :occur 1))))
   (should (null (configuration-layer//stable-elpa-verify-archive)))))


(ert-deftest test-stable-elpa-verify-archive--signature-not-found-is-fatal-error ()
  (mocker-let
   ((configuration-layer//stable-elpa-tarball-local-file
     nil ((:record-cls 'mocker-stub-record
                       :output
                       (concat spacemacs-test-directory
                               "core/data/signed-stable-elpa.tar.gz")
                       :occur 1)))
    (configuration-layer//stable-elpa-tarball-local-sign-file
     nil ((:record-cls 'mocker-stub-record
                       :output
                       (concat spacemacs-test-directory
                               "core/data/not-found.sig")
                       :occur 1)))
    (configuration-layer//error
     (msg &rest args) ((:record-cls 'mocker-stub-record :occur 1))))
   (should (null (configuration-layer//stable-elpa-verify-archive)))))
