;;; svg-lib.el --- SVG tags, progress bars & icons -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; URL: https://github.com/rougier/svg-lib
;; Version: 0.3
;; Package-Requires: ((emacs "27.1"))
;; Keywords: svg, icons, tags, convenience

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
;; Usage example:
;;
;; (insert-image (svg-lib-tag "TODO"))
;; (insert-image (svg-lib-progress-bar 0.33))
;; (insert-image (svg-lib-icon "star"))
;;
;; Icons ares created by parsing remote collections whose license are
;; compatibles with GNU Emacs:
;;
;; - Boxicons (https://github.com/atisawd/boxicons), available under a
;;   Creative Commons 4.0 license. As of version 2.07 (December 2020),
;;   this collection offers 1500 icons in two styles (regular & solid).
;;   Gallery is available at https://boxicons.com/
;;
;; - Octicons (https://github.com/primer/octicons), available under a
;;   MIT License with some usage restriction for the GitHub logo. As of
;;   version 11.2.0 (December 2020), this collection offers 201 icons.
;;   Gallery available at https://primer.style/octicons/
;;
;; - Material (https://github.com/google/material-design-icons),
;;   available under an Apache 2.0 license. As of version 4.0.0
;;   (December 2020), this collection offers 500+ icons in 4 styles
;;   (filled, outlined, rounded, sharp). Gallery available at
;;   https://material.io/resources/icons/?style=baseline
;;
;; - Bootstrap (https://github.com/twbs/icons), available under a MIT
;;   license.  As of version 1.2.1 (December 2020), this collection
;;   offers 1200+ icons in 2 styles (regular & filled).  Gallery
;;   available at https://icons.getbootstrap.com/
;;
;; The default size of an icon is exactly 2x1 characters such that it
;; can be inserted inside a text without disturbing alignment.
;;
;; Note: Each icon is cached locally to speed-up loading the next time
;;       you use it. If for some reason the cache is corrupted you can
;;       force reload using the svg-icon-get-data function.
;;
;; If you want to add new collections (i.e. URL), make sure the icons
;; are monochrome and that their size is consistent.

;;; NEWS:

;; Version 0.3
;; - Renamed 'svg-lib-button' to 'svg-lib-icon+tag'
;; - Added interactive 'svg-lib-button' with associtated 'svg-lib-button-mode'
;; - Added proper documentation in the README

;; Version 0.2.8
;; - No background for icon when background color is nil
;; - Refactored date icons

;; Version 0.2.7
;; - Added a dynamic date icon

;; Version 0.2.6
;; - Bug fix with bootstrap icon directory

;; Version 0.2.5
;; - Bug fix in text size computation

;; Version 0.2.4
;; - Better error handling if SVG support is missing

;; Version 0.2.2
;; - Added a left/righ crop style argument to allow for tags collage.

;; Version 0.2.1
;; - Added an alignment parameter for moving tags inside margins.

;; Version 0.2
;; - Fix most of the warnings.

;; Version 0.1:
;; - Submission to ELPA


;;; Code:
(require 'svg)
(require 'xml)
(require 'cl-lib)
(require 'color)

;; Check if Emacs has been compiled with svg support
(defun svg-lib--image (&rest args)
  ;; FIXME: Should `svg-image' perform this check instead?
  (unless (image-type-available-p 'svg)
     (error "svg-lib.el requires Emacs to be compiled with svg support.\n"))
  (apply #'svg-image args))

(defgroup svg-lib nil
  "SVG tags, bars & icons."
  :group 'convenience
  :prefix "svg-lib-")

(defface svg-lib-button-active-face
  `((t :foreground ,(face-foreground 'default)
       :background ,(face-background 'default)
       :family "RobotoMono Nerd Font"
       :weight regular
       :box (:line-width 2 :style nil)))
  "Default face for active button"
  :group 'svg-lib)

(defface svg-lib-button-hover-face
  `((t :foreground ,(face-background 'font-lock-comment-face nil 'default)
       :background ,(face-foreground 'font-lock-comment-face nil 'default)
       :family "RobotoMono Nerd Font"
       :weight semibold
       :box nil))
  "default face for when mouse is over the button"
  :group 'svg-lib)

(defface svg-lib-button-press-face
  `((t :foreground ,(face-background 'default)
       :background ,(face-foreground 'default)
       :family "RobotoMono Nerd Font"
       :weight semibold
       :box nil))
  "Default face for when button is prssed (mouse click or keyboard)"
  :group 'svg-lib)

;; Default icon collections
;; ---------------------------------------------------------------------
(defcustom  svg-lib-icon-collections
  '(("bootstrap" .
     "https://icons.getbootstrap.com/assets/icons/%s.svg")
    ("simple" .
     "https://raw.githubusercontent.com/simple-icons/simple-icons/develop/icons/%s.svg")
    ("material" .
     "https://raw.githubusercontent.com/Templarian/MaterialDesign/master/svg/%s.svg")
    ("octicons" .
     "https://raw.githubusercontent.com/primer/octicons/main/icons/%s-24.svg")
    ("boxicons" .
     "https://boxicons.com/static/img/svg/regular/bx-%s.svg")
    ("vscode" .
     "https://raw.githubusercontent.com/microsoft/vscode-icons/main/icons/light/%s.svg"))
    
  "Various icons collections stored as (name . base-url).

The name of the collection is used as a pointer for the various
icon creation methods.  The base-url is a string containing a %s
such that is can be replaced with the name of a specific icon.
User is responsible for finding/giving proper names for a given
collection (there are way too many to store them)."

  :type '(alist :key-type (string :tag "Name")
                :value-type (string :tag "URL"))
  :group 'svg-lib)

(defcustom svg-lib-icons-dir
  (expand-file-name (concat user-emacs-directory ".cache/svg-lib/"))
  "svg-lib icons directory."
  :group 'svg-lib
  :type 'directory)


;; Default style for all objects
(defun svg-lib-style-compute-default (&optional face)
  "Compute the default style according to face (which defaults
to the default face)."

  (let* ((face        (or face 'default))
         (font-family (face-attribute face :family nil 'default))
         (font-weight (face-attribute face :weight nil 'default))
         (font-size   (face-attribute face :height nil 'default))
         (font-size   (round (* font-size 0.085)))
         (foreground  (face-attribute face :foreground nil 'default))
         (background  (face-attribute face :background nil 'default)))

    `(:background    ,background
      :foreground    ,foreground

      :padding       1      ;; In characters (tag and icons) or pixels (progress)
      :margin        0      ;; In characters
      :stroke        2      ;; In pixels
      :radius        3      ;; In pixels
      :alignment     0.5    ;; Horizontal alignment (in fraction of margin)
      :width         20     ;; In characters
      :height        0.90   ;; Ratio of text line height
      :scale         0.75   ;; Icon scaling
      :ascent        center ;; Position / baseline
      :crop-left     nil    ;; Whether to crop on left (for collage with other tags)
      :crop-right    nil    ;; Whether to crop on righ (for collage with other tags)

      :collection    "material" ;; Icon collection
      
      :font-family   ,font-family
      :font-size     ,font-size
      :font-weight   ,font-weight)))

(defcustom svg-lib-style-default
  (svg-lib-style-compute-default)
  "Default style"
  :type '(plist :key-type (choice (const :tag "Background" :background)
                                  (const :tag "Foreground" :foreground)
                                  (const :tag "Padding" :padding)
                                  (const :tag "Margin" :margin)
                                  (const :tag "Stroke" :stroke)
                                  (const :tag "Radius" :radius)
                                  (const :tag "Ascent" :ascent)
                                  (const :tag "Alignment" :alignment)
                                  (const :tag "Width" :width)
                                  (const :tag "Height" :height)
                                  (const :tag "Scale" :scale)
                                  (const :tag "Crop Left" :crop-left)
                                  (const :tag "Crop Right" :crop-right)
                                  (const :tag "Collection" :collection)
                                  (const :tag "Font Family" :font-family)
                                  (const :tag "Font Size" :font-size)
                                  (const :tag "Font Weight" :font-weight))
                :value-type (choice (const :tag "None" nil)
                                    (number)
                                    (string)
                                    (boolean)
                                    (symbol)))
  :group 'svg-lib)


;; Convert Emacs color to SVG color
(defun svg-lib-convert-color (color-name)
  "Convert Emacs COLOR-NAME to #rrggbb form.
If COLOR-NAME is unknown to Emacs, then return COLOR-NAME as-is."
  (when color-name
    (let ((rgb-color (color-name-to-rgb color-name)))
      (if rgb-color
          (apply #'color-rgb-to-hex (append rgb-color '(2)))
	color-name))))

;; SVG Library style build from partial specification
(defun svg-lib-style (&optional base &rest args)
  "Build a news style using BASE and style elements ARGS."
  
  (let* ((default svg-lib-style-default)
         (base (or base default))
         (keys (cl-loop for (key _value) on default by 'cddr
                        collect key))
         (style '()))

    (dolist (key keys)
      (setq style (if (plist-member args key)
                      (plist-put style key (plist-get args key))
                    (plist-put style key (plist-get base key)))))

    ;; Convert emacs colors to SVG colors
    (plist-put style :foreground
               (svg-lib-convert-color (plist-get style :foreground)))
    (plist-put style :background
               (svg-lib-convert-color (plist-get style :background)))

    ;; Convert emacs font weights to SVG font weights
    (let ((weights
           '((thin       . 100) (ultralight . 200) (light      . 300)
             (regular    . 400) (medium     . 500) (semibold   . 600)
             (bold       . 700) (extrabold  . 800) (black      . 900))))
      (plist-put style :font-weight
                 (or (cdr (assoc (plist-get style :font-weight) weights))
                     (plist-get style :font-weight))))
    style))


;; Create an image displaying LABEL in a rounded box.
(defun svg-lib-tag (label &optional style &rest args)
  "Create an image displaying LABEL in a rounded box using given STYLE
and style elements ARGS."

  (let* ((default svg-lib-style-default)
         (style (if style (apply #'svg-lib-style nil style) default))
         (style (if args  (apply #'svg-lib-style style args) style))

         (foreground  (plist-get style :foreground))
         (background  (plist-get style :background))

         (crop-left   (plist-get style :crop-left))
         (crop-right  (plist-get style :crop-right))

         (alignment   (plist-get style :alignment))
         (stroke      (plist-get style :stroke))
         ;; (width       (plist-get style :width))
         (height      (plist-get style :height))
         (radius      (plist-get style :radius))
         ;; (scale       (plist-get style :scale))
         (margin      (plist-get style :margin))
         (padding     (plist-get style :padding))
         (font-size   (plist-get style :font-size))
         (font-family (plist-get style :font-family))
         (font-weight (plist-get style :font-weight))

         (txt-char-width  (window-font-width))
         (txt-char-height (window-font-height))
         (txt-char-height (if line-spacing
                              (+ txt-char-height line-spacing)
                            txt-char-height))
         (font-info       (font-info (format "%s-%d" font-family font-size)))
         (font-size       (aref font-info 2)) ;; redefine font-size
         (ascent          (aref font-info 8))
         (tag-char-width  (aref font-info 11))
         ;; (tag-char-height (aref font-info 3))
         (tag-width       (* (+ (length label) padding) txt-char-width))
         (tag-height      (* txt-char-height height))

         (svg-width       (+ tag-width (* margin txt-char-width)))
         (svg-height      tag-height)
         (svg-ascent      (plist-get style :ascent))
         
         (tag-x  (* (- svg-width tag-width)  alignment))
         (text-x (+ tag-x (/ (- tag-width (* (length label) tag-char-width)) 2)))
         (text-y ascent)

         (tag-x      (if crop-left  (- tag-x     txt-char-width) tag-x))
         (tag-width  (if crop-left  (+ tag-width txt-char-width) tag-width))
         (text-x     (if crop-left  (- text-x (/ stroke 2)) text-x))
         (tag-width  (if crop-right (+ tag-width txt-char-width) tag-width))
         (text-x     (if crop-right (+ text-x (/ stroke 2)) text-x))
         
         (svg (svg-create svg-width svg-height)))

    (when (>= stroke 0.25)
      (svg-rectangle svg tag-x 0 tag-width tag-height
                     :fill foreground :rx radius))
    (svg-rectangle svg (+ tag-x (/ stroke 2.0)) (/ stroke 2.0)
                       (- tag-width stroke) (- tag-height stroke)
                       :fill background :rx (- radius (/ stroke 2.0)))
    (svg-text svg label
              :font-family font-family :font-weight font-weight  :font-size font-size
              :fill foreground :x text-x :y  text-y)
    (svg-lib--image svg :ascent svg-ascent)))


;; Create a progress pie
(defun svg-lib-progress-pie (value &optional style &rest args)
  "Create a progress pie image with value VALUE using given STYLE
and style elements ARGS."

  (let* ((default svg-lib-style-default)
         (style (if style (apply #'svg-lib-style nil style) default))
         (style (if args  (apply #'svg-lib-style style args) style))

         (foreground  (plist-get style :foreground))
         (background  (plist-get style :background))
         (stroke      (plist-get style :stroke))
         ;; (width       (plist-get style :width))
         (height      (plist-get style :height))
         ;;  (scale       (plist-get style :scale))
         (margin      (plist-get style :margin))
         (padding     (plist-get style :padding))
         ;; (font-size   (plist-get style :font-size))
         ;; (font-family (plist-get style :font-family))
         ;; (font-weight (plist-get style :font-weight))
         
         (txt-char-width  (window-font-width))
         (txt-char-height (window-font-height))
         
         ;; (font-info       (font-info (format "%s-%d" font-family font-size)))
         ;; (ascent          (aref font-info 8))
         ;; (tag-char-width  (aref font-info 11))
         ;; (tag-char-height (aref font-info 3))

         (tag-width       (* 2 txt-char-width))
         (tag-height      (* txt-char-height height))

         (svg-width       (+ tag-width (* margin txt-char-width)))
         (svg-height      tag-height)
         (svg-ascent      (plist-get style :ascent))
         
         ;; (tag-x           (/ (- svg-width tag-width) 2))

         (cx              (/ svg-width  2))
         (cy              (/ svg-height 2))
         (radius          (- (/ tag-height 2) (/ stroke 2)))

         (iradius         (- radius stroke (/ padding 2)))

         (angle0          (- (/ float-pi 2)))
         (x0              (+ cx (* iradius (cos angle0))))
         (y0              (+ cy (* iradius (sin angle0))))

         (angle1          (+ angle0 (* value 2 float-pi)))
         (x1              (+ cx (* iradius (cos angle1))))
         (y1              (+ cy (* iradius (sin angle1))))

         (large-arc       (>= (- angle1 angle0) float-pi))
         (svg (svg-create svg-width svg-height)))

    (when (>= stroke 0.25)
      (svg-circle svg cx cy radius :fill foreground))

    (svg-circle svg cx cy (- radius (/ stroke 2.0)) :fill background)

    (if (>= (- angle1 angle0) (* float-pi 2))
        (svg-circle svg cx cy iradius :fill foreground)
      (svg-path svg `((moveto ((,cx . ,cy)))
                    (lineto ((,x0 . ,y0)))
                    (elliptical-arc ((,iradius ,iradius ,x1 ,y1
                                      :sweep t :large-arc ,large-arc))))
              :fill foreground))
    (svg-lib--image svg :ascent svg-ascent)))



;; Create a progress bar
(defun svg-lib-progress-bar (value &optional style &rest args)
  "Create a progress bar image with value VALUE using given STYLE
and style elements ARGS."

  (let* ((default svg-lib-style-default)
         (style (if style (apply #'svg-lib-style nil style) default))
         (style (if args  (apply #'svg-lib-style style args) style))

         (foreground  (plist-get style :foreground))
         (background  (plist-get style :background))
         (stroke      (plist-get style :stroke))
         (width       (plist-get style :width))
         (height      (plist-get style :height))
         (radius      (plist-get style :radius))
         ;; (scale       (plist-get style :scale))
         (margin      (plist-get style :margin))
         (padding     (plist-get style :padding))
         ;; (font-size   (plist-get style :font-size))
         ;; (font-family (plist-get style :font-family))
         ;; (font-weight (plist-get style :font-weight))

         (txt-char-width  (window-font-width))
         (txt-char-height (window-font-height))
         
         ;; (font-info       (font-info (format "%s-%d" font-family font-size)))
         ;; (ascent          (aref font-info 8))
         ;; (tag-char-width  (aref font-info 11))
         ;; (tag-char-height (aref font-info 3))

         (tag-width       (* width txt-char-width))
         (tag-height      (* txt-char-height height))

         (svg-width       (+ tag-width (* margin txt-char-width)))
         (svg-height      tag-height)
         (svg-ascent      (plist-get style :ascent))
         
         (tag-x (/ (- svg-width tag-width) 2))
         (svg (svg-create svg-width svg-height)))

    (when (>= stroke 0.25)
      (svg-rectangle svg tag-x 0 tag-width tag-height
                     :fill foreground :rx radius))
    (svg-rectangle svg (+ tag-x (/ stroke 2.0))
                       (/ stroke 2.0)
                       (- tag-width stroke)
                       (- tag-height stroke)
                       :fill background :rx (- radius (/ stroke 2.0)))
    (svg-rectangle svg (+ tag-x (/ stroke 2.0) padding)
                       (+ (/ stroke 2.0) padding)
                       (- (* value tag-width) stroke (* 2 padding))
                       (- tag-height stroke (* 2 padding))
                       :fill foreground :rx (- radius (/ stroke 2.0)))
    
    (svg-lib--image svg :ascent svg-ascent)))



;; Create a rounded box icon
(defun svg-lib--icon-get-data (collection name &optional force-reload)
  "Retrieve icon NAME from COLLECTION.

Cached version is returned if it exists unless FORCE-RELOAD is t."

  ;; Build url from collection and name without checking for error
  (let ((url (format (cdr (assoc collection svg-lib-icon-collections)) name)))
    ;; create the svg-lib-icons-dir if not exists
    (unless (file-exists-p svg-lib-icons-dir)
      (make-directory svg-lib-icons-dir t))
    (let* ((filename (expand-file-name (format "%s_%s.svg" collection name) svg-lib-icons-dir))
           (buffer (if (or force-reload (not (file-exists-p filename)))
                       (with-current-buffer (url-retrieve-synchronously url)
                         (goto-char (point-min))
                         (search-forward "\n\n")
                         (write-region (point) (point-max) filename)
                         (current-buffer))
                     (with-current-buffer (generate-new-buffer " *temp*")
                       (insert-file-contents filename)
                       (current-buffer)))))
      (with-current-buffer buffer
        (xml-parse-region (point-min) (point-max))))))


(defun svg-lib-icon (icon &optional style &rest args)
  "Create a SVG image displaying icon NAME from COLLECTION using
given STYLE and style elements ARGS."

  (let* ((default svg-lib-style-default)
         (style (if style (apply #'svg-lib-style nil style) default))
         (style (if args  (apply #'svg-lib-style style args) style))

         (collection  (plist-get style :collection))
         (root (svg-lib--icon-get-data collection icon))
         
         (foreground  (plist-get style :foreground))
         (background  (plist-get style :background))
         (stroke      (plist-get style :stroke))
         (height      (plist-get style :height))
         (radius      (plist-get style :radius))
         (scale       (plist-get style :scale))
         (margin      (plist-get style :margin))
         (padding     (plist-get style :padding))
         ;; (font-size   (plist-get style :font-size))
         ;; (font-family (plist-get style :font-family))
         ;; (font-weight (plist-get style :font-weight))
         (width      (+ 2 padding))
         
         (txt-char-width  (window-font-width))
         (txt-char-height (window-font-height))
         (box-width       (* width txt-char-width))
         (box-height      (* height txt-char-height))
         (svg-width       (+ box-width (* margin txt-char-width)))
         (svg-height      box-height)
         (svg-ascent      (plist-get style :ascent))
         (box-x           (/ (- svg-width box-width) 2))
         (box-y           0)

         ;; Read original viewbox
         (viewbox (cdr (assq 'viewBox (xml-node-attributes (car root)))))
         (viewbox (mapcar #'string-to-number (split-string viewbox)))
         (icon-x      (nth 0 viewbox))
         (icon-y      (nth 1 viewbox))
         (icon-width  (nth 2 viewbox))
         (icon-height (nth 3 viewbox))
         (scale       (* scale (/ (float box-height) (float icon-height))))
         (icon-transform
          (format "translate(%f,%f) scale(%f) translate(%f,%f)"
                  (- icon-x )
                  (- icon-y )
                  scale
                  (- (/ svg-width 2 scale) (/ icon-width 2))
                  (- (/ svg-height 2 scale) (/ icon-height 2))))

         (svg (svg-create svg-width svg-height)))

    (when (>= stroke 0.25)
      (svg-rectangle svg box-x box-y box-width box-height
                     :fill foreground :rx radius))
    (when background
      (svg-rectangle svg (+ box-x (/ stroke 2.0))
                     (+ box-y (/ stroke 2.0))
                     (- box-width stroke)
                     (- box-height stroke)
                     :fill background :rx (- radius (/ stroke 2.0))))
    
    (dolist (item (xml-get-children (car root) 'path))
      (let* ((attrs (xml-node-attributes item))
             (path (cdr (assoc 'd attrs)))
             ;; (fill (or (cdr (assoc 'fill attrs)) foreground))
             )
        (svg-node svg 'path :d path
                            :fill foreground
                            :transform icon-transform)))
    (svg-lib--image svg :ascent svg-ascent)))



;; Create an image displaying LABEL in a rounded box.
(defun svg-lib-icon+tag (icon label &optional style &rest args)
  "Create an image displaying LABEL in a rounded box using given STYLE
and style elements ARGS."

  (let* ((default svg-lib-style-default)
         (style (if style (apply #'svg-lib-style nil style) default))
         (style (if args  (apply #'svg-lib-style style args) style))

         (collection (plist-get style :collection))
         (root (svg-lib--icon-get-data collection icon))
         
         (foreground  (plist-get style :foreground))
         (background  (plist-get style :background))
         (stroke      (plist-get style :stroke))
         ;; (width       (plist-get style :width))
         (height      (plist-get style :height))
         (radius      (plist-get style :radius))
         (scale       (plist-get style :scale))
         (margin      (plist-get style :margin))
         (padding     (plist-get style :padding))
         (font-size   (plist-get style :font-size))
         (font-family (plist-get style :font-family))
         (font-weight (plist-get style :font-weight))

         (label-length    (+ (length label) 2))
                          
         (txt-char-width  (window-font-width))
         (txt-char-height (window-font-height))
         ;; (box-width       (* width txt-char-width))
         ;; (box-height      (* height txt-char-height))

         (font-info       (font-info (format "%s-%d" font-family font-size)))
         (ascent          (aref font-info 8))
         (tag-char-width  (aref font-info 11))
         ;; (tag-char-height (aref font-info 3))
         (tag-width       (* (+ label-length padding) txt-char-width))
         (tag-height      (* txt-char-height height))

         (svg-width       (+ tag-width (* margin txt-char-width)))
         (svg-height      tag-height)
         (svg-ascent      (plist-get style :ascent))
         
         (tag-x (/ (- svg-width tag-width) 2))
         (text-x (+ tag-x (/ (- tag-width (* (length label) tag-char-width)) 2)))
         (text-x (+ text-x tag-char-width))
         (text-y ascent)

         ;; ;; Read original viewbox
         (viewbox (cdr (assq 'viewBox (xml-node-attributes (car root)))))
         (viewbox (mapcar 'string-to-number (split-string viewbox)))
         (icon-x      (nth 0 viewbox))
         (icon-y      (nth 1 viewbox))
         (icon-width  (nth 2 viewbox))
         (icon-height (nth 3 viewbox))
         (scale       (* scale (/ (float tag-height) (float icon-height))))
         (icon-transform
          (format "translate(%f,%f) scale(%f) translate(%f,%f)"
                  (- icon-x )
                  (- icon-y )
                  scale
                  (- (/ (- text-x (* tag-char-width 1.25)) scale) (/ icon-width 2))
                  (- (/ svg-height 2 scale) (/ icon-height 2))))
         (svg (svg-create svg-width svg-height)))

    (when (>= stroke 0.25)
      (svg-rectangle svg tag-x 0 tag-width tag-height
                     :fill foreground :rx radius))
    (svg-rectangle svg (+ tag-x (/ stroke 2.0)) (/ stroke 2.0)
                       (- tag-width stroke) (- tag-height stroke)
                       :fill background :rx (- radius (/ stroke 2.0)))
    (svg-text svg label
              :font-family font-family :font-weight font-weight  :font-size font-size
              :fill foreground :x text-x :y text-y)


    (dolist (item (xml-get-children (car root) 'path))
      (let* ((attrs (xml-node-attributes item))
             (path (cdr (assoc 'd attrs)))
             ;; (fill (or (cdr (assoc 'fill attrs)) foreground))
             )
        (svg-node svg 'path :d path
                            :fill foreground
                            :transform icon-transform)))
    (svg-lib--image svg :ascent svg-ascent)))



(defun svg-lib-date (&optional date style &rest args)
  "Create a two lines date icon showing given DATE, using given
STYLE and style elements ARGS."

  (let* ((date (or date (current-time)))
         (month (upcase (format-time-string "%b" date)))
         (day (format-time-string "%d" date)))
    (apply 'svg-lib-box month day style args)))

(defun svg-lib-week-date (&optional date style &rest args)
  "Create a two lines date icon showing given DATE, using given
STYLE and style elements ARGS."

  (let* ((date (or date (current-time)))
         (week (format-time-string "%W" date)))
    (apply 'svg-lib-box "WEEK" week style args)))

(defun svg-lib-day-date (&optional date style &rest args)
  "Create a two lines date icon showing given DATE, using given
STYLE and style elements ARGS."

  (let* ((weekday (upcase (format-time-string "%a" date)))
         (day (format-time-string "%d" date)))
    (apply 'svg-lib-box weekday day style args)))

        
(defun svg-lib-box (top bottom &optional style &rest args)
  "Create a two lines icon showing given TOP and BOTTOM text, using
given STYLE and style elements ARGS."

  (let* ((default svg-lib-style-default)
         (style (if style (apply #'svg-lib-style nil style) default))
         (style (if args  (apply #'svg-lib-style style args) style))

         (foreground  (plist-get style :foreground))
         (background  (plist-get style :background))
         (alignment   (plist-get style :alignment))
         (stroke      (plist-get style :stroke))
         (width       (or (plist-get args :width) 5))
         (height      (or (plist-get args :height) 2))
         (radius      (plist-get style :radius))
         (margin      (plist-get style :margin))
         
         (font-size   (plist-get style :font-size))
         (font-family (plist-get style :font-family))
         (font-weight (plist-get style :font-weight))

         (txt-char-width  (window-font-width))
         (txt-char-height (window-font-height))
         
         (font-info       (font-info (format "%s-%d" font-family font-size)))
         (ascent          (aref font-info 8))
         (tag-char-width  (aref font-info 11))
         (tag-char-height (aref font-info 3))
         (tag-width       (* width txt-char-width))
         
         (tag-height      (* height txt-char-height))
         (svg-width       (+ tag-width (* margin txt-char-width)))
         (svg-height      tag-height)
         (svg-ascent      (or (plist-get style :ascent) 'center))
         (tag-x           (/ (- svg-width tag-width) 2) )

         (svg (svg-create svg-width svg-height)))
    
    (when (>= stroke 0.25)
      (svg-rectangle svg tag-x 0 tag-width tag-height
                     :fill foreground :rx radius))
    (svg-rectangle svg (+ tag-x (/ stroke 2.0))
                       (/ stroke 2.0)
                       (- tag-width stroke)
                       (- tag-height stroke)
                       :fill background :rx (- radius (/ stroke 2.0)))
    (svg-rectangle svg (+ tag-x (/ stroke 2.0))
                       (/ stroke 2.0)
                       (- tag-width stroke)
                       (- (/ tag-height 2) stroke)
                       :fill foreground :rx (- radius (/ stroke 2.0)))
    (svg-rectangle svg (+ tag-x (/ stroke 2.0))
                       (+ (/ stroke 2.0) (/ tag-height 3))
                       (- tag-width stroke)
                       (- (/ tag-height 2) stroke)
                       :fill background :rx 0)
    (svg-text svg top
              :font-family font-family
              :font-weight "bold"
              :font-size (* font-size 0.9)
              :fill background
              :text-anchor "middle"
              :x (/ svg-width 2)
              :y "+0.95em")
    (svg-text svg bottom
              :font-family font-family
              :font-weight "bold"
              :font-size (* font-size 1.7)
              :fill foreground ;;(face-foreground 'default)
              :text-anchor "middle"
              :x (/ svg-width 2)
              :y "+1.6em")
    (svg-lib--image svg :ascent svg-ascent)))

(defun svg-lib-concat (svg-image-1 svg-image-2)
  "Concatenate two svg images horizontally."

 (let* ((svg (car (with-temp-buffer
 	                (insert (plist-get (cdr svg-image-1) :data))
 	                (xml-parse-region (point-min) (point-max)))))
        (attrs (xml-node-attributes svg))
        (width-1 (string-to-number (cdr (assq 'width attrs))))
        (height-1 (string-to-number (cdr (assq 'height attrs))))
        (children-1 (xml-node-children svg))
 
        (svg (car (with-temp-buffer
 	                (insert (plist-get (cdr svg-image-2) :data))
 	                (xml-parse-region (point-min) (point-max)))))
        (attrs (xml-node-attributes svg))
        (width-2 (string-to-number (cdr (assq 'width attrs))))
        (height-2 (string-to-number (cdr (assq 'height attrs))))
        (children-2 (xml-node-children svg))

        (width (+ width-1 width-2))
        (height (max height-1 height-2))
        (transform (format "translate(%f,0)" width-1))
        (svg (svg-create width height)))

   (dolist (child children-1)
     (dom-append-child svg child))

   (dolist (child children-2)
     (unless (stringp child)
       (dom-set-attribute child 'transform transform))
     (dom-append-child svg child))
   svg))


(defvar svg-lib-button--id-counter 0
  "SVG button unique id counter")

(defun svg-lib-button--search (id)
  "Return region for the button with given ID"

  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (when-let* ((match (text-property-search-forward 'button-id id t)))
        (cons (prop-match-beginning match)
              (prop-match-end match))))))

(defun svg-lib-button--at-point (&optional pos)
  "Return the button at point"
  (get-text-property (or pos (point)) 'button-id))

(defun svg-lib-button--get-state (id &optional region)
  "Return the state of button ID"
  
  (when-let* ((region (or region (svg-lib-button--search id))))
    (get-text-property (car region) 'button-state)))

(defun svg-lib-button--set-state (id state &optional no-reset)
  "Set the state of button ID to STATE, reset the state of any
hovered button unless NO-RESET is t"

  ;; Reset previous hover button state (if any)
  (when (and (boundp 'svg-lib-button--hover-id) svg-lib-button--hover-id (not no-reset))
    (let ((prev-id svg-lib-button--hover-id))
      (setq-local svg-lib-button--hover-id nil)
      (svg-lib-button--set-state prev-id 'active)))

  ;; Set new state
  (when-let* ((region (svg-lib-button--search id))
              (cur-state (svg-lib-button--get-state id region))
              (button-list (get-text-property (car region) 'button-list))
              (display (cdr (assoc state button-list))))
        (put-text-property (car region) (cdr region) 'display display)
        (put-text-property (car region) (cdr region) 'button-state state)
        (cond ((eq state 'hover)
               (setq-local svg-lib-button--hover-id id))
              ((eq state 'press)
               (setq-local svg-lib-button--press-id id)))))

(defun svg-lib-button--tooltip-hide (&rest args)
  "Set currently press or hightlighted button to default
state (active) and hover button at point if any."

  (when (boundp 'svg-lib-button--press-id)
    (svg-lib-button--set-state svg-lib-button--press-id 'active))
  (when (boundp 'svg-lib-button--hover-id)
    (svg-lib-button--set-state svg-lib-button--hover-id 'active))

  ;; Hover button at point (if any)
  (svg-lib-button--set-state (svg-lib-button--at-point) 'hover)
  (advice-remove 'tooltip-hide #'svg-lib-button--tooltip-hide))

(defun svg-lib-button--tooltip-show (pos)
  "Set button under mouse state to hover or press depending
on whether mouse button 1 is down (press) or up (hover)"

  (if (and (consp last-input-event)
           (string-match-p "down-mouse-1" (format "%s" (car last-input-event))))
      (svg-lib-button--set-state (svg-lib-button--at-point pos) 'press)
    (svg-lib-button--set-state (svg-lib-button--at-point pos) 'hover))
  (advice-add 'tooltip-hide :before #'svg-lib-button--tooltip-hide))

(defun svg-lib-button--mouse-down ()
  "Set button under mouse state to press."

  (interactive)
  (save-excursion
    (mouse-set-point last-input-event)
    (svg-lib-button--set-state (svg-lib-button--at-point) 'press))
  (advice-add 'tooltip-hide :before #'svg-lib-button--tooltip-hide))

(defun svg-lib-button--mouse-press ()
    "Set button under mouse state to default state (active) and call
button hook. If current buffer is minibuffer, it aborts it. It
would be better to simply exit minibuffer but this leads to focus
problem if the hook creates a frame."

  (interactive)

  ;; Here we check if mouse is still over the button
  (let ((mouse-point (save-excursion
                       (mouse-set-point last-input-event)
                       (point))))
    (if-let ((id (svg-lib-button--at-point mouse-point)))
        (svg-lib-button--set-state svg-lib-button--press-id 'hover)
      (svg-lib-button--set-state svg-lib-button--press-id 'active)))
  
  (when-let* ((region (svg-lib-button--search svg-lib-button--press-id))
              (hook (get-text-property (car region) 'button-hook)))
    (if (minibufferp nil t)
        (unwind-protect
            (abort-minibuffers)
          (funcall hook))
      (funcall hook))))
 
(defun svg-lib-button--mouse-drag ()
  "Update the state of the button under mouse"
  
  (interactive)
  (save-excursion
    (mouse-set-point last-input-event)
    (svg-lib-button--set-state (svg-lib-button--at-point) 'press)))

(defun svg-lib-button--make (label &optional face)
  "Return a svg tag with given LABEL and FACE. LABEL can be composed
as \"[collection:icon] label\" resulting in an icon+tag button."

  (save-match-data
    (let* ((face (or face 'default))
           (label-regex "\\[\\([a-zA-Z0-9]+:\\)?\\([a-zA-Z0-9 _-]+\\)\\] *\\(.+\\)"))
      (if (string-match label-regex label)
          (let* ((collection (match-string 1 label))
                 (collection (if (stringp collection)
                                 (substring collection 0 -1)
                               (plist-get svg-lib-style-default ':collection)))
                 (icon       (match-string 2 label))
                 (label      (match-string 3 label)))
            (svg-lib-icon+tag icon label nil
                              :collection collection
                              :stroke (or (plist-get (face-attribute face :box) ':line-width) 0)
                              :font-family (face-attribute face :family nil t)
                              :font-weight (face-attribute face :weight nil t)
                              :foreground (face-foreground face nil 'default)
                              :background (face-background face nil 'default)))
        (svg-lib-tag label nil
                     :stroke (or (plist-get (face-attribute face :box) ':line-width) 0)
                     :font-family (face-attribute face :family nil t)
                     :font-weight (face-attribute face :weight nil t)
                     :foreground (face-foreground face nil 'default)
                     :background (face-background face nil 'default))))))


(defun svg-lib-button (label &optional hook help active-face hover-face press-face)
  "Make a button with given LABEL that will call HOOK when
pressed. The HELP text is displatyed when mouse is hovering the
button. ACTIVE-FACE, HOVER-FACE and PRESS-FACE correspond to the
different states of the button. LABEL can be composed as
\"[collection:icon] label\" resulting in an icon+tag button.

For proper highlighting, `svg-lib-button-mode' needs to be
activated before inserting a button into a buffer."

  ;; Having a SVG button highlighted when mouse cursor is hovering is
  ;; not totally straightforward because Emacs lacks the proper
  ;; machinery to do so. There is actually a mouse-face property but it
  ;; only changes the face and cannot change the display property (that
  ;; is needed for SVG). To solve the problem, we can take advantage of
  ;; the tooltip machinery because it offers a hackable enter/exit event
  ;; that can be used to update the display at the proper time.
  ;;
  ;; To make this works, a few properties needs to be removed from
  ;; `font-lock-extra-managed-props' and `yank-excluded-properties'.
  ;; For org-mode, another hack is necessary because when org-mode
  ;; unfontifies a region (see `org-unfontify-region'), it removes the
  ;; local keymap that is used. You thus need to activate the
  ;; svg-lib-button-mode to have this set for you.
  
  (let* ((active (svg-lib-button--make label
                          (or active-face 'svg-lib-button-active-face)))
         (hover (svg-lib-button--make label
                          (or hover-face 'svg-lib-button-hover-face)))
         (press (svg-lib-button--make label
                          (or press-face 'svg-lib-button-press-face)))
         (buttons `((active . ,active)
                    (hover . ,hover)
                    (press . ,press)))
         (state 'active))
    (setq svg-lib-button--id-counter (1+ svg-lib-button--id-counter))
    (propertize (concat label " ")
                'display (cdr (assoc state buttons))
                'svg-lib-button t
                'button-id svg-lib-button--id-counter
                'button-state state
                'button-list buttons
                'button-hook hook
                'front-sticky nil
                'rear-nonsticky t
                'keymap (define-keymap :suppress t
                          "<down-mouse-1>" #'svg-lib-button--mouse-down
                          "<mouse-1>"      #'svg-lib-button--mouse-press
                          "<drag-mouse-1>" #'svg-lib-button--mouse-drag)
                'help-echo `(lambda (_window _object pos)
                              (svg-lib-button--tooltip-show pos)
                               ,help)
                'pointer 'hand)))

(defun svg-lib-button--remove-text-properties (orig-fun beg end properties &optional object)
  "This advice function ensures keymap is not removed when in svg-lib-button-mode"

  (let ((properties (if (and svg-lib-button-mode (derived-mode-p 'org-mode))
                        (org-plist-delete properties 'keymap)
                       properties)))
    (apply orig-fun (list beg end properties object))))

(define-minor-mode svg-lib-button-mode
  "Activate svg-lib-button-mode that takes care of activating tooltip
mode and removing some properties from `yank-excluded-properties'
and `font-lock-extra-managed-props' in order for highlight to
work properly. This mode also installs an advice on
`remove-text-properties' in org-mode in order to not delete the
`keymap' property that is necessary to detect mouse press events."

  :lighter "B"

  (when svg-lib-button-mode
    ;; This is necessary for detecting when mouse cursor enter or
    ;; leave a button
    (require 'tooltip)
    (tooltip-mode 1)

    ;; This is necessary for preventing org-mode to remove keymap when
    ;; unfontiying a region
    (advice-add #'remove-text-properties :around #'svg-lib-button--remove-text-properties)

    ;; This prevents help-echo to be removed.when button is copied/yanked
    (dolist (property '(help-echo keymap))
      (setq-local yank-excluded-properties
                  (remove property yank-excluded-properties)))

    ;; This prevents help-echo to be removed.by font-lock
    (dolist (property '(help-echo keymap display))
      (setq-local font-lock-extra-managed-props
                  (remove property font-lock-extra-managed-props))))
  
  (unless svg-lib-button-mode
    (advice-remove #'remove-text-properties #'svg-lib-button--remove-text-properties)))

(provide 'svg-lib)
;;; svg-lib.el ends here



      
