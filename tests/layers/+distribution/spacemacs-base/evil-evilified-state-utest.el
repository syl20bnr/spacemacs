;;; evil-evilified-state-utest.el --- Spacemacs Unit Test File
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
;; ---------------------------------------------------------------------------
;; evilified-state--find-new-event
;; ---------------------------------------------------------------------------

(ert-deftest test-evilify-find-new-event--s-to-S ()
  (let ((input ?s))
    (should (equal ?S (evilified-state--find-new-event input)))))

(ert-deftest test-evilify-find-new-event--S-to-C-s ()
  (let ((input ?S))
    (should (equal ?\C-s (evilified-state--find-new-event input)))))

(ert-deftest test-evilify-find-new-event--C-s-to-C-S-s ()
  (let ((input ?\C-s)
        (output (+ (expt 2 25) ?\C-s)))
    (should (equal output (evilified-state--find-new-event input)))))

(ert-deftest test-evilify-find-new-event--C-S-s-error-return-nil ()
  (let ((input (+ (expt 2 25) ?\C-s)))
    (should (equal nil (evilified-state--find-new-event input)))))

(ert-deftest test-evilify-find-new-event--Space-remap ()
  (let ((input 32))
    (should (equal ?' (evilified-state--find-new-event input)))))

(ert-deftest test-evilify-find-new-event--/-remap ()
  (let ((input ?/))
    (should (equal ?\\ (evilified-state--find-new-event input)))))

(ert-deftest test-evilify-find-new-event--:-remap ()
  (let ((input ?:))
    (should (equal ?| (evilified-state--find-new-event input)))))

;; ---------------------------------------------------------------------------
;; evilified-state--sort-keymap
;; ---------------------------------------------------------------------------

(ert-deftest test-evilify-sort-keymap-1 ()
  (let ((map '(keymap
               (menu-bar keymap
                         (Menu menu-item "Title"
                               (keymap "Map"
                                       (Action1 menu-item "Action1" action1)
                                       (Action2 menu-item "Action2" action2))))
               (23 . func1)
               (M-tab . func9)
               (remap keymap (func1 . func4) (func2. func5) (func3 . func6))
               (s-tab . func8)
               (24 keymap (52 keymap (97 . func2)) (97 . func3))
               (33 . func4)
               (58 . func5)
               (122 . func6)
               (M-return . func7)
               (C-tab . func10))))
    (should (equal '((122 . func6)
                     (58 . func5)
                     (33 . func4)
                     (24 keymap (52 keymap (97 . func2)) (97 . func3))
                     (23 . func1)
                     (C-tab . func10)
                     (M-return . func7)
                     (M-tab . func9)
                     (menu-bar
                      keymap
                      (Menu menu-item "Title"
                            (keymap "Map"
                                    (Action1 menu-item "Action1" action1)
                                    (Action2 menu-item "Action2" action2))))
                     (remap keymap (func1 . func4) (func2. func5) (func3 . func6))
                     (s-tab . func8))
                   (evilified-state--sort-keymap map)))))


(ert-deftest test-evilify-sort-keymap-2 ()
  (let ((map '(keymap
               (s-tab . func8)
               (23 . func1)
               (122 . func6)
               (remap keymap (func1 . func4) (func2. func5) (func3 . func6))
               (33 . func4)
               (M-return . func7)
               (menu-bar keymap
                         (Menu menu-item "Title"
                               (keymap "Map"
                                       (Action1 menu-item "Action1" action1)
                                       (Action2 menu-item "Action2" action2))))
               (58 . func5)
               (M-tab . func9)
               (24 keymap (52 keymap (97 . func2)) (97 . func3))
               (C-tab . func10))))
    (should (equal '((122 . func6)
                     (58 . func5)
                     (33 . func4)
                     (24 keymap (52 keymap (97 . func2)) (97 . func3))
                     (23 . func1)
                     (C-tab . func10)
                     (M-return . func7)
                     (M-tab . func9)
                     (menu-bar
                      keymap
                      (Menu menu-item "Title"
                            (keymap "Map"
                                    (Action1 menu-item "Action1" action1)
                                    (Action2 menu-item "Action2" action2))))
                     (remap keymap (func1 . func4) (func2. func5) (func3 . func6))
                     (s-tab . func8))
                   (evilified-state--sort-keymap map)))))
