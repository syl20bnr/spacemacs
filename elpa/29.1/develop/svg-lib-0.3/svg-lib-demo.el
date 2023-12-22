;; Copyright (C) 2021 Free Software Foundation, Inc. -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This file demonstrates some usage examples of the svg-lib package.
:; Each line should insert some svg objets into the current buffer.

(require 'svg-lib)

(dotimes (i 5)
  (insert-image (svg-lib-tag "TODO" nil
                             :font-weight (* (+ i 2) 100))))


(dotimes (i 10)
  (insert-image (svg-lib-tag "TODO" nil :padding 1 :stroke (/ i 4.0))))
          

(dotimes (i 10)
  (insert-image (svg-lib-tag "TODO" nil :stroke 2 :radius i)))
          

(dotimes (i 10)
  (insert-image (svg-lib-progress-bar (/ (+ i 1) 10.0) nil
                    :width 5 :margin 1 :stroke 2 :padding 2)))
          

(insert-image (svg-lib-progress-bar 0.75 nil :radius 8 :stroke 2 :padding 0))
 

(dotimes (i 10)
  (insert-image (svg-lib-progress-pie (/ (+ i 1) 10.0) nil
                    :margin 1 :stroke 2 :padding 1)))
          

(dotimes (i 10)
  (insert-image (svg-lib-icon "star" nil :scale (/ (+ i 1) 10.0))))
          

(insert-image (svg-lib-icon+tag "check-bold" "DONE" nil
                              :font-family "Roboto Mono"
                              :font-weight 500
                              :stroke 0 :background "#673AB7" :foreground "white"))
 

(insert-image (svg-lib-icon "gnuemacs" nil :collection "simple"
                            :stroke 0 :scale 1 :padding 0))
  

(insert-image (svg-lib-date nil nil :font-family "Roboto" :radius 5
                                    :foreground "#673AB7"))
 
(svg-lib-button-mode 1)
(insert (svg-lib-button "[check-bold] OK"
                        (lambda () (interactive) (message "OK"))))



