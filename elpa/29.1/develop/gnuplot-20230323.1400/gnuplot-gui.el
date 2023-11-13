;;;; gnuplot-gui.el -- GUI interface to setting options in gnuplot-mode -*- lexical-binding: t -*-

;; Copyright (C) 1998-2000 Bruce Ravel

;; Author:     Bruce Ravel <ravel@phys.washington.edu>
;; URL:        https://github.com/emacs-gnuplot/gnuplot

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This file provides a graphical user interface to setting arguments
;; to gnuplot commands.  Positioning point near a command and invoking
;; `gnuplot-gui-set-options-and-insert' (C-c C-c or shift-mouse-2)
;; will pop open a frame with widgets for setting the various
;; arguments appropriate the the item that was near point.  The goal
;; is to provide point-and-click functionality to gnuplot-mode.
;;
;; gnuplot-gui.el was developed using GNU Emacs 25 and should be
;; compatible with GNU Emacs 24.3 and above.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; To do:
;;
;; Widgets I need:
;; -- 'position: two or three comma separated numbers used to denote a
;;               position or a tic start/end/increment (see arrow,
;;               need a prefix)
;; -- 'modifier: colon separated fields used for datafile modifiers
;;
;; command types which are currently unsupported or contain mistakes
;; -- unsupported: cntrparam
;; -- plot, splot, fit: rather lame
;; -- label: position information missing
;; -- label: font string handled in overly simple manner
;; -- hidden3d: not really suited to 'list, but all options are exclusive...
;; -- pointstyle argument to "set label"
;;
;; overall:
;; -- continuation lines (ugh!)
;; -- multiple frames end up displaying same window after setting options
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(require 'gnuplot)
(require 'widget)
(require 'wid-edit)
(require 'cl-lib)


;;; customizable variables

(defgroup gnuplot-gui nil
  "Graphical interface to setting arguments in gnuplot scrips."
  :prefix "gnuplot-gui-"
  :group 'gnuplot)

(defcustom gnuplot-gui-popup-flag nil
  "Non-nil means to open arguments pop-ups automatically.
This would be done after menu insertion of Gnuplot commands."
  :group 'gnuplot-gui
  :type 'boolean)

(defvar gnuplot-gui-frame nil
  "Frame used to hold the buffer for setting options.")

(defcustom gnuplot-gui-frame-parameters
  '((height . 18)
    (width . 65)
    (user-position . t)
    (top . 150)
    (left . 150)
    (border-width . 0)
    (menu-bar-lines . 0)
    (unsplittable . t))
  "Frame parameters for the input run-time display frame in Emacs."
  :group 'gnuplot-gui
  :type '(repeat (sexp :tag "Parameter:")))

(defcustom gnuplot-gui-fontname-list
  '(" " "\"Helvetica\"" "\"Times-Roman\"")
  "List of known font names.
These *must* be quoted, like so \"\\\"Helvetica\\\"\".  This allows
for fonts with names like \"\\\"Arial Bold Italic\\\"\" to be treated
as single entries in the menu-buttons.  And it is really important that
the first entry in the list be a blank string."
  :group 'gnuplot-gui
  :type '(repeat (string :tag "Font name:")))

;; some global variables
(defvar gnuplot-current-frame nil)
(defvar gnuplot-current-buffer nil)
(defvar gnuplot-current-buffer-point nil)
(defvar gnuplot-gui-alist nil)
(defvar gnuplot-gui-current-string nil)


;;; various tools for handling data structures and text in the buffer

;; tools for accessing the elements of the lists in `gnuplot-gui-all-types'
(defsubst gnuplot-gui-type-tag     (obj) (elt obj 0))
(defsubst gnuplot-gui-type-symbol  (obj) (elt obj 1))
(defsubst gnuplot-gui-type-default (obj) (elt obj 2))
(defsubst gnuplot-gui-type-prefix  (obj) (elt obj 3)) ; also 'range seperator
(defsubst gnuplot-gui-type-fourth  (obj) (elt obj 4))
(defsubst gnuplot-gui-type-list    (obj) (cddr obj))

(defun gnuplot-this-word ()
  "Return the word under point."
  (let ((begin (save-excursion (beginning-of-line) (point-marker)))
        (end   (save-excursion (end-of-line)       (point-marker))))
    (save-excursion
      (or (looking-at "\\<") (= (current-column) 0) (forward-word -1))
      (if (> (point) begin) (setq begin (point-marker)))
      (forward-word 1)
      (if (> (point) end) (goto-char end))
      (buffer-substring-no-properties begin (point)))))



;;; data structures containing regarding options in Gnuplot 3.7

;; various constants used for options that take the same sorts of arguments
(defconst gnuplot-gui-mtics-list
  '(("FREQUENCY"   'number " ")
    ("DEFAULT"     'list " " "default")))
(defconst gnuplot-gui-data-list
  '(("DATA TYPE"   'list " " "time")))
(defconst gnuplot-gui-label-list
  '(("LABEL"       'string " ")
    ("POSITION"    'position " " "" 2)
    ("FONTNAME"    'list " " gnuplot-gui-fontname-list)
    ("FONTSIZE"    'fontsize " ")))
(defconst gnuplot-gui-range-list
  '(("RANGE"       'range (" " . " ") ":")
    ("REVERSE"     'list " " "reverse" "noreverse")
    ("WRITEBACK"   'list " " "writeback" "nowriteback")))
(defconst gnuplot-gui-tics-list
  '(("WHERE"       'list " " "axis" "border")
    ("MIRROR"      'list " " "mirror" "nomirror")
    ("ROTATE"      'list " " "rotate" "norotate")
    ("SERIES"      'position " " "" 3)
    ("LABEL ARRAY" 'labels () )))
(defconst gnuplot-gui-zeroaxis-list
  '(("LINETYPE"    'number " ")))

(defvar gnuplot-gui-terminal-types nil
  "Associated list of terminal descriptions.
See the doc-string for `gnuplot-gui-all-types'.")
(setq gnuplot-gui-terminal-types
      (list (cons "aifm"
                  '(("COLOR"     'list " " "monochrome" "gray" "color")
                    ("FONTNAME"  'list " " gnuplot-gui-fontname-list)
                    ("FONTSIZE"  'fontsize " ")))
            (cons "cgm"
                  '(("MODE"      'list   " " "landscape" "portrait" "default")
                    ("COLOR"     'list   " " "color" "monochrome")
                    ("ROTATION"  'list   " " "rotate" "norotate")
                    ("WIDTH"     'number " " "width")
                    ("LINEWIDTH" 'number " " "linewidth")
                    ("FONTNAME"  'list   " " "\"Arial\"" "\"Arial Italic\""
                     "\"Arial Bold\"" "\"Arial Bold Italic\""
                     "\"Times Roman\"" "\"Times Roman Italic\""
                     "\"Times Roman Bold\"" "\"Times Roman Bold Italic\""
                     "\"Helvetica\"" "\"Roman\"")
                    ("FONTSIZE"  'fontsize " ")))
            (cons "corel"
                  '(("COLOR"            'list " " "default" "color" "monochrome")
                    ("FONTNAME"         'list " " "\"SwitzerlandLight\""
                     "\"Helvetica\"" "\"Times-Roman\"")
                    ("FONTSIZE "        'number " ")
                    ("X-SIZE   "        'number " ")
                    ("Y-SIZE   "        'number " ")
                    ("LINEWIDTH"        'number " ")))
            (cons "dumb"
                  '(("LINEFEED"         'list   " " "feed" "nofeed")
                    ("X-SIZE"           'number " ")
                    ("Y-SIZE"           'number " ")))
            (cons "emf"
                  '(("COLOR"            'list " " "color" "monochrome")
                    ("LINE"             'list " " "solid" "dashed")
                    ("FONTNAME"         'string " ")
                    ("FONTSIZE"         'number " ")))
            (cons "emtex"
                  '(("FONTNAME"         'list     " " "courier" "roman")
                    ("FONTSIZE"         'fontsize " ")))
            (cons "fig"
                  '(("COLOR"            'list   " " "color" "monochrome")
                    ("FRAMESIZE"        'list   " " "small" "big")
                    ("POINTSMAX"        'number " " "pointsmax")
                    ("ORIENTATION"      'list   " " "landscape" "portrait")
                    ("UNITS"            'list   " " "metric" "inches")
                    ("FONT SIZE"        'number " " "fontsize")
                    ("SIZE"             'pair   (" " . " ") "size")
                    ("LINE THICKNESS"   'number " " "thickness")
                    ("LAYER DEPTH"      'number " " "depth")))
            (cons "hp500c"
                  '(("RESOLUTION"       'list " " "75" "100" "150" "300")
                    ("COMPRESSION"      'list " " "rle" "tiff")))
            (cons "hpgl"
                  '(("PENS"             'number " ")
                    ("EJECT"            'list   " " "eject")))
            (cons "hpdj"
                  '(("RESOLUTION"       'list " " "75" "100" "150" "300")))
            (cons "hpljii"
                  '(("RESOLUTION"       'list " " "75" "100" "150" "300")))
            (cons "hppj"
                  '(("FONT"             'list " " "FNT9X17" "FNT5X9" "FNT13X25")))
            (cons "imagen"
                  '(("FONT SIZE"        'number " ")
                    ("LAYOUT"           'list   " " "portrait" "landscape")
                    ("NUMBER OF GRAPHS" 'range (" " . " ") ",")))
            (cons "gpic"
                  '(("X ORIGIN"         'number " ")
                    ("Y ORIGIN"         'number " " ",")))
            (cons "latex"
                  '(("FONTNAME"         'list " " "courier" "roman")
                    ("FONTSIZE"         'fontsize " ")))
            (cons "mif"
                  '(("COLOUR"           'list " " "colour" "monochrome")
                    ("LINETYPE"         'list " " "polyline" "vectors")))
            (cons "nec-cp6"
                  '(("MODE"             'list " " "monochrome" "colour" "draft")))
            (cons "pbm"
                  '(("SIZE"      'list " " "small" "medium" "large")
                    ("COLOR"     'list " " "monochrome" "gray" "color")))
            (cons "pcl5L"
                  '(("MODE"      'list " " "landscape" "portrait")
                    ("FONTNAME"  'list " " "stick" "univers" "cg_times")
                    ("FONTSIZE"  'fontsize " ")))
            (cons "png"
                  '(("SIZE"      'list " " "small" "medium" "large")
                    ("COLOR"     'list " " "monochrome" "gray" "color")))
            (cons "postscript"
                  '(("MODE"      'list " " "landscape" "portrait" "eps" "default")
                    ("ENHANCED"  'list " " "enhanced" "noenhanced")
                    ("COLOR"     'list " " "color" "monochrome")
                    ("SOLID"     'list " " "solid" "dashed")
                    ("DUPLEXING" 'list " " "defaultplex" "simplex" "duplex")
                    ("FONTNAME"  'list " " gnuplot-gui-fontname-list)
                    ("FONTSIZE"  'fontsize " ")))
            (cons "pslatex"
                  '(("COLOR"     'list " " "monochrome" "color")
                    ("DASHED"    'list " " "dashed")
                    ("ROTATION"  'list " " "rotate" "norotate")
                    ("AUXFILE"   'list " " "auxfile")))
            (cons "pstex"
                  '(("COLOR"     'list " " "monochrome" "color")
                    ("DASHED"    'list " " "dashed")
                    ("ROTATION"  'list " " "rotate" "norotate")
                    ("AUXFILE"   'list " " "auxfile")))
            (cons "pstricks"
                  '(("HACK TEXT"        'list " " "hacktext" "nohacktext")
                    ("PLOT SCALING"     'list " " "nounit" "unit")))
            (cons "regis"
                  '(("COLOR DEPTH"      'list "4" "16")))
            (cons "tgif"
                  '(("LAYOUT"           'list   " " "portrait" "landscape")
                    ("NUMBER OF GRAPHS" 'range (" " . " ") ",")
                    ("LINE TYPE"        'list " " "solid" "dashed")
                    ("FONTNAME"         'list " " gnuplot-gui-fontname-list)
                    ("FONTSIZE"         'fontsize " ")))
            (cons "tpic"
                  '(("POINTSIZE"        'number " ")
                    ("LINEWIDTH"        'number " ")
                    ("INTERVAL "        'number " ")))
            (cons "vgagl"       ; for pm3d patch (also persist, raise in x11) <MT>
                  '(("BACKGROUND"       'position  " " "background" 3)
                    ("INTERPOLATION"    'list " " "uniform" "interpolate")
                    ("DUMP"             'file " ")
                    ("MODE"             'string  " " "")))
            (cons "x11"
                  '(("RESET"            'list " " "reset")
                    ("TERMINAL NUMBER"  'number " ")
                    ("PERSIST"          'list " " "persist" "nopersist")
                    ("RAISE"            'list " " "raise" "noraise"))) ))

(defvar gnuplot-gui-terminal-list nil)
(setq gnuplot-gui-terminal-list
      (append (list " ") (mapcar 'car gnuplot-gui-terminal-types)))

(defvar gnuplot-gui-set-types nil
  "Associated list of set option descriptions.
See the doc-string for `gnuplot-gui-all-types'.")
(setq gnuplot-gui-set-types
      (list (cons "angles"
                  '(("UNITS" 'list " " "degrees" "radians")))
            (cons "arrow"
                  '(("TAG"       'tag " ")
                    ("FROM"      'position " " "from" 3)
                    ("TO"        'position " " "to" 3)
                    ("HEAD"      'list " " "head" "nohead")
                    ("LINESTYLE" 'number " " "ls")
                    ("LINETYPE " 'number " " "lt")
                    ("LINEWIDTH" 'number " " "lw")))
            (cons "noarrow"
                  '(("TAG" 'tag " ")))
            (cons "autoscale"
                  '(("AXIS" 'list " " "x" "y" "z" "x2" "y2" "xy"
                     "xmin" "ymin" "zmin" "x2min" "y2min" "xymin"
                     "xmax" "ymax" "zmax" "x2max" "y2max" "xymax")))
            (cons "noautoscale"
                  '(("AXIS" 'list " " "x" "y" "z" "x2" "y2" "xy"
                     "xmin" "ymin" "zmin" "x2min" "y2min" "xymin"
                     "xmax" "ymax" "zmax" "x2max" "y2max" "xymax")))
            (cons "bar"
                  '(("SIZE" 'list " " "small" "large")))
            (cons "border"
                  '(("BORDER CODE" 'number " ")
                    ("LINE STYLE"  'list   " " "lines"
                     "dots" "points" "linespoints")
                    ("LINESTYLE"   'number " " "ls")
                    ("LINETYPE"    'number " " "lt")
                    ("LINEWIDTH"   'number " " "lw")))
            (cons "boxwidth"
                  '(("WIDTH" 'number " ")))
            (cons "clabel"
                  '(("FORMAT" 'format " ")))
            (cons "clip"
                  '(("CLIP TYPE" 'list " " "points" "one" "two")))
            (cons "noclip"
                  '(("CLIP TYPE" 'list " " "points" "one" "two")))
            ;;(cons "cntrparam"
            ;;  '(("INTERPOLATION" 'list " " "linear" "cubicspline" "bspline")
            ;;    ("POINTS" 'number " " "points")
            ;;    ("ORDER"  'number " " "order")))
            (cons "contour"
                  '(("WHERE"     'list " " "base" "surface" "both")))
            (cons "dgrid3d"
                  '(("ROW,COLUMN,NORM" 'position " " "" 3)))
            (cons "encoding"
                  '(("ENCODING" 'list " " "default" "iso_8859_1"
                     "cp850" "cp437")))
            (cons "format"
                  '(("AXIS"   'list " " "x" "y" "z" "xy" "x2" "y2")
                    ("FORMAT" 'format  " ")))
            (cons "dummy"
                  '(("VAR 1" 'string " " "")
                    ("VAR 2" 'string " " ",")))
            (cons "grid"
                  '(("XTICS"  'list " " "xtics" "mxtics" "noxtics" "nomxtics")
                    ("YTICS"  'list " " "ytics" "mytics" "noytics" "nomytics")
                    ("ZTICS"  'list " " "ztics" "mztics" "noztics" "nomztics")
                    ("X2TICS" 'list " " "x2tics" "mx2tics" "nox2tics" "nomx2tics")
                    ("Y2TICS" 'list " " "y2tics" "my2tics" "noy2tics" "nomy2tics")
                    ("POLAR"  'number " " "polar")
                    ("MAJOR LINETYPE" 'number " ")
                    ("MINOR LINETYPE" 'number " ")))
            (cons "hidden3d"
                  '(("ALGORITHM" 'list " " "defaults"
                     "offset"
                     "nooffset"
                     ;;"trianglepattern  # bitpattern between 0 and 7"
                     "trianglepattern 0" "trianglepattern 1"
                     "trianglepattern 2" "trianglepattern 3"
                     "trianglepattern 4" "trianglepattern 5"
                     "trianglepattern 6" "trianglepattern 7"
                     ;;"undefined        # level between 0 and 3"
                     "undefined 0" "undefined 1" "undefined 2" "undefined 3"
                     "noundefined" "altdiagonal" "noaltdiagonal"
                     "bentover" "nobentover")))
            (cons "historysize"
                  '(("SIZE" 'number " ")))
            (cons "isosamples"
                  '(("ISO_U LINES" 'number " ")
                    ("ISO_V LINES" 'number " " ",")))
            (cons "key"
                  '(("LOCATION"      'list " " "left" "right" "top" "bottom"
                     "outside" "below")
                    ("POSITION"      'position  " " "" 3)
                    ("JUSTIFICATION" 'list " " "Left" "Right")
                    ("REVERSE"       'list " " "reverse" "noreverse")
                    ("SAMPLE LENGTH" 'number " " "samplen")
                    ("SPACING"       'number " " "spacing")
                    ("WIDTH"         'number " " "width")
                    ("TITLE"         'string " " "title ")
                    ("BOX LINETYPE"  'number " " "box") ;; linetype data
                    ("NOBOX"         'list   " " "nobox")))
            (cons "label"
                  '(("TAG" 'tag " ")
                    ("LABEL TEXT" 'string " ")
                    ("POSITION"   'position " " "at" 3)
                    ;; first, second, graph, screen
                    ("JUSTIFICATION" 'list " " "left" "right" "center")
                    ("ROTATE"        'list " " "rotate" "norotate")
                    ("FONT"          'string " " "font"))) ;; font "name,size"
            (cons "nolabel"
                  '(("TAG"        'tag " ")))
            (cons "linestyle"
                  '(("TAG      "  'tag " ")
                    ("LINE STYLE" 'list " " "boxerrorbars" "boxes"
                     "boxxyerrorbars" "candlesticks" "dots"
                     "financebars" "fsteps" "histeps" "impulses"
                     "lines" "linespoints" "points" "steps" "vector"
                     "xerrorbars" "xyerrorbars" "yerrorbars")
                    ("LINETYPE " 'number " " "lt")
                    ("LINEWIDTH" 'number " " "lw")
                    ("POINTTYPE" 'number " " "pt")
                    ("POINTSIZE" 'number " " "ps")))
            (cons "locale"
                  '(("LOCALE" 'string " ")))
            (cons "logscale"
                  '(("AXIS" 'list " " "x" "y" "z" "xy" "xz" "yz" "xyz"
                     "x2" "y2")
                    ("BASE" 'number " ")))
            (cons "nologscale"
                  '(("AXIS" 'list " " "x" "y" "z" "xy" "xz" "yz" "xyz"
                     "x2" "y2")))
            (cons "mapping"
                  '(("COORDINATE SYSTEM" 'list " " "cartesian" "spherical"
                     "cylindrical")))
                                        ; _margin
            (cons "bmargin"
                  '(("BOTTOM MARGIN" 'number " ")))
            (cons "lmargin"
                  '(("LEFT MARGIN"   'number " ")))
            (cons "rmargin"
                  '(("RIGHT MARGIN"  'number " ")))
            (cons "tmargin"
                  '(("TOP MARGIN"    'number " ")))

            (cons "missing"
                  '(("CHARACTER" 'string " " 1)))
                                        ; m_tics
            (cons "mxtics"  gnuplot-gui-mtics-list)
            (cons "mytics"  gnuplot-gui-mtics-list)
            (cons "mztics"  gnuplot-gui-mtics-list)
            (cons "mx2tics" gnuplot-gui-mtics-list)
            (cons "my2tics" gnuplot-gui-mtics-list)

                                        ; pm3d additions <MT>
            (cons "mouse"
                  '(("DOUBLECLICK"     'number " " "doubleclick")
                    ("ZOOM"            'list   " " "zoomcoordinates" "nozoomcoordinates")
                    ("POLAR"           'list   " " "polarcoordinates" "nopolarcoordinates")
                    ("FORMAT"          'string " " "format")
                    ("CLIPBOARDFORMAT" 'string " " "clipboardformat")
                    ("MOUSEFORMAT"     'string " " "mouseformat")
                    ("LABELS"          'list   " " "labels" "nolabels")
                    ("LABELOPTIONS"    'string " " "labeloptions")
                    ("ZOOMJUMP"        'list   " " "zoomjump" "nozoomjump")
                    ("VERBOSE"         'list   " " "verbose" "noverbose")))
            (cons "palette"
                  '(("COLOR"       'list     " " "gray" "color")
                    ("RGBFORMULAE" 'position " " "rgbformulae" 3)
                    ("PARITY"      'list     " " "positive" "negative")
                    ("FORMULAE"    'list     " " "nops_allcF" "ps_allcF")
                    ("MAXCOLORS"   'number   " ")
                    ("COLOR_BOX"   'list     " " "nocb" "cbdefault" "cbuser")
                    ("ORIENTATION" 'list     " " "cbvertical" "cbhorizontal")
                    ("ORIGIN"      'position " " "origin" 2)
                    ("SIZE"        'position " " "size" 2)
                    ("BORDER"      'number   " ")
                    ("NOBORDER"    'list     " " "bdefault" "noborder")))
            (cons "pm3d"
                  '(("AT"         'list*  " " "b" "s" "t" "bs" "bt" "st" "bst")
                    ("SCANS"      'list   " " "scansautomatic" "scansforward" "scansbackward")
                    ("FLUSH"      'list*  " " "begin" "center" "end")
                    ("CLIP"       'list   " " "clip1in" "clip4in")
                    ("ZRANGE"     'range (" " . " ") ":")
                    ("HIDDEN3D"   'number " ")
                    ("NOHIDDEN3D" 'list   " " "nohidden3d")
                    ("FILLING"    'list   " " "transparent" "solid")
                    ("MAP"        'list   " " "map")))

            (cons "offsets"
                  '(("LEFT  " 'number " ")
                    ("RIGHT " 'number " " ",")
                    ("TOP   " 'number " " ",")
                    ("BOTTOM" 'number " " ",")))
            (cons "origin"
                  '(("X ORIGIN"   'number " ")
                    ("Y ORIGIN"   'number " " ",")))
            (cons "output"
                  '(("FILENAME"   'file " ")))
            (cons "pointsize"
                  '(("MULTIPLIER" 'number " ")))
            (cons "samples"
                  '(("2D PLOT"    'number " ")
                    ("3D PLOT"    'number " " ",")))
            (cons "size"
                  '(("ASPECT"           'list " " "square" "nosquare"
                     "ratio" "noratio")
                    ("X-SCALE OR RATIO" 'number " ")
                    ("Y-SCALE"          'number " " ",")))
            (cons "style"
                  '(("DATA TYPE"  'list " " "data" "function")
                    ("PLOT STYLE" 'list " " "boxerrorbars" "boxes"
                     "boxxyerrorbars" "candlesticks" "dots"
                     "financebars" "fsteps" "histeps" "impulses"
                     "lines" "linespoints" "points" "steps" "vector"
                     "xerrorbars" "xyerrorbars" "yerrorbars")))
            (cons "terminal"
                  '(("TERMINAL TYPE" 'list " " gnuplot-gui-terminal-list)))
            (cons "tics"
                  '(("DIRECTION"  'list " " "in" "out")))
            (cons "ticslevel"
                  '(("RELATIVE HEIGHT" 'number " ")))
            (cons "ticscale"
                  '(("MAJOR" 'number " ")
                    ("MINOR" 'number " ")))
            (cons "timestamp"
                  '(("FORMAT STRING" 'format " ")
                    ("WHERE"         'list " " "top" "bottom")
                    ("ROTATE"        'list " " "rotate" "norotate")
                    ("X-OFFSET"      'number " ")
                    ("Y-OFFSET"      'number " " ",")
                    ("FONTNAME"      'list " " gnuplot-gui-fontname-list)))
            (cons "timefmt"
                  '(("FORMAT STRING" 'string " ")))
            (cons "title"
                  '(("TITLE" 'string " ")))
            (cons "view"
                  '(("X-ROTATION" 'number " ")
                    ("Z-ROTATION" 'number " " ",")
                    ("SCALE"      'number " " ",")
                    ("Z-SCALE"    'number " " ",")))
            ;; ("SCALE" 'position " " "," 4)
                                        ; _data
            (cons "xdata"      gnuplot-gui-data-list)
            (cons "ydata"      gnuplot-gui-data-list)
            (cons "zdata"      gnuplot-gui-data-list)
            (cons "x2data"     gnuplot-gui-data-list)
            (cons "y2data"     gnuplot-gui-data-list)
                                        ; _label
            (cons "xlabel"     gnuplot-gui-label-list)
            (cons "ylabel"     gnuplot-gui-label-list)
            (cons "zlabel"     gnuplot-gui-label-list)
            (cons "x2label"    gnuplot-gui-label-list)
            (cons "y2label"    gnuplot-gui-label-list)
                                        ; _range, note that the [] syntax for
                                        ;         the writeback argument is
                                        ;         not properly supported
            (cons "xrange"     gnuplot-gui-range-list)
            (cons "yrange"     gnuplot-gui-range-list)
            (cons "zrange"     gnuplot-gui-range-list)
            (cons "x2range"    gnuplot-gui-range-list)
            (cons "y2range"    gnuplot-gui-range-list)
            (cons "trange"     gnuplot-gui-range-list)
            (cons "rrange"     gnuplot-gui-range-list)
            (cons "urange"     gnuplot-gui-range-list)
            (cons "vrange"     gnuplot-gui-range-list)
                                        ; _tics
            (cons "xtics"      gnuplot-gui-tics-list)
            (cons "ytics"      gnuplot-gui-tics-list)
            (cons "ztics"      gnuplot-gui-tics-list)
            (cons "x2tics"     gnuplot-gui-tics-list)
            (cons "y2tics"     gnuplot-gui-tics-list)
                                        ; zeroaxis
            (cons "zeroaxis"   gnuplot-gui-zeroaxis-list)
            (cons "xzeroaxis"  gnuplot-gui-zeroaxis-list)
            (cons "yzeroaxis"  gnuplot-gui-zeroaxis-list)
            (cons "y2zeroaxis" gnuplot-gui-zeroaxis-list)
            (cons "x2zeroaxis" gnuplot-gui-zeroaxis-list)

            (cons "zero"
                  '(("THRESHOLD" 'number " ")))))

(defvar gnuplot-gui-command-types nil
  "Associated list of command descriptions.
See the doc-string for `gnuplot-gui-all-types'.")
(setq gnuplot-gui-command-types
      (list (cons "cd"
                  '(("FILENAME"       'file   " ")))
            (cons "call"
                  '(("INPUT FILE"     'file   " ")
                    ("PARAMETER LIST" 'string " ")))
            (cons "load"
                  '(("INPUT FILE"     'file   " ")))
            (cons "pause"
                  '(("TIME"           'number " ")
                    ("MESSAGE"        'string " ")))
            (cons "print"
                  '(("EXPRESSION"     'string " ")))
            (cons "save"
                  '(("SAVE"           'list   " " "functions" "variables" "set")
                    ("FILE"           'file   " ")))
            (cons "update"
                  '(("INITIAL FILE"   'file   " " t)
                    ("UPDATED FILE"   'file   " " t))) ))


(defcustom gnuplot-gui-plot-splot-fit-style 'simple
  "Control the complexity of the GUI display for plot, splot, and fit.
The values are 'simple, which causes a limited set of plot, splot, or
fit options to be displayed, and 'complete, which attempts to display
all options.  The 'complete setting is prone to making errors when
parsing values already in the script buffer."
  :group 'gnuplot-gui
  :type '(radio (const :tag "Simple listing"   simple)
                (const :tag "Complete listing" complete)))


(defconst gnuplot-gui-plot-simple-list
  '(("X RANGE"     'range (" " . " ") ":")
    ("Y RANGE"     'range (" " . " ") ":")
    ("DATA FILE"   'file   " ")
    ("THRU"        'string* " " "thru")
    ("USING"       'modifier " ")
    ("TITLE"       'string " ")
    ("WITH"        'list* " " "boxerrorbars" "boxes"
     "boxxyerrorbars" "candlesticks" "dots" "financebars"
     "fsteps" "histeps" "impulses" "lines" "linespoints"
     "points" "steps" "vector" "xerrorbars" "xyerrorbars"
     "yerrorbars")))
(defconst gnuplot-gui-plot-full-list
  '(;;("T RANGE"     'range (" " . " ") ":")
    ("X RANGE"     'range (" " . " ") ":")
    ("Y RANGE"     'range (" " . " ") ":")
    ("xa"          'text   "\t---------------------")
    ("FUNCTION"    'string " ")
    ("xc"          'text   "   or")
    ("DATA FILE"   'file   " ")
    ("INDEX"       'modifier " ")
    ("EVERY"       'modifier " ")
    ("THRU"        'string* " " "thru")
    ("USING"       'modifier " ")
    ("SMOOTH"      'list* " " "unique" "csplines" "acsplines"
     "bezier" "sbezier")
    ;; datafile modifiers
    ("AXES"        'list* " " "x1y1" "x2y2" "x1y2" "x2y1")
    ("TITLE"       'string " ")
    ("NOTITLE"     'list   " " "notitle")
    ("xf"          'text   "\t---------------------")
    ("xi"          'text   "Select a standard plotting style")
    ("WITH"        'list* " " "boxerrorbars" "boxes"
     "boxxyerrorbars" "candlesticks" "dots" "financebars"
     "fsteps" "histeps" "impulses" "lines" "linespoints"
     "points" "steps" "vector" "xerrorbars" "xyerrorbars"
     "yerrorbars")
    ("xo"          'text   "     or a previously defined style")
    ("LINE STYLE " 'number " " "ls")
    ("xr"          'text   "     or specify a style in-line")
    ("LINE TYPE  " 'number " " "lt")
    ("LINE WIDTH " 'number " " "lw")
    ("POINT TYPE " 'number " " "pt")
    ("POINT STYLE" 'number " " "ps")))
(defconst gnuplot-gui-splot-simple-list
  '(("DATA FILE"   'file   " ")
    ("TITLE"       'string " ")
    ("WITH"        'list* " " "lines" "linespoints" "points" "dots" "impulses")))
(defconst gnuplot-gui-splot-full-list
  '(;;("U RANGE"     'range (" " . " ") ":")
    ;;("V RANGE"     'range (" " . " ") ":")
    ("X RANGE"     'range (" " . " ") ":")
    ("Y RANGE"     'range (" " . " ") ":")
    ("Z RANGE"     'range (" " . " ") ":")
    ("xa"          'text   "\t---------------------")
    ("FUNCTION"    'string " ")
    ("xc"          'text   "   or")
    ("DATA FILE"   'file   " ")
    ("INDEX"       'modifier " ")
    ("EVERY"       'modifier " ")
    ("THRU"        'string* " " "thru")
    ("USING"       'modifier " ")
    ("SMOOTH"      'list* " " "unique" "csplines" "acsplines"
     "bezier" "sbezier")
    ("TITLE"       'string " ")
    ("NOTITLE"     'list   " " "notitle")
    ("WITH"        'list* " " "lines" "linespoints" "points" "dots" "impulses")))
(defconst gnuplot-gui-fit-simple-list
  '(("FUNCTION"     'string* " " "")
    ("DATA FILE"    'file    " ")
    ("VIA (params)" 'string* " " "via") ))
(defconst gnuplot-gui-fit-full-list
  '(("X RANGE"      'range  (" " . " ") ":")
    ("Y RANGE"      'range  (" " . " ") ":")
    ("xa"           'text    "----- fitting functionn and file --------")
    ("FUNCTION"     'string* " " "")
    ("DATA FILE"    'file    " ")
    ("xb"           'text    "----- datafile modifiers ----------------")
    ("INDEX"        'modifier " ")
    ("EVERY"        'modifier " ")
    ("THRU"         'string* " " "thru")
    ("USING"        'modifier " ")
    ("SMOOTH"       'list* " " "unique" "csplines" "acsplines"
     "bezier" "sbezier")
    ("xc"           'text    "----- parameters (file or parameters) ---")
    ("VIA (file)"   'string  " " "via")
    ("VIA (params)" 'string* " " "via") ))

(defvar gnuplot-gui-plot-splot-fit nil
  "Associated list of plot, splot, and fit descriptions.
See the doc-string for `gnuplot-gui-all-types'.")
(setq gnuplot-gui-plot-splot-fit
      (list (cons "plot"  (if (equal gnuplot-gui-plot-splot-fit-style 'complete)
                              gnuplot-gui-plot-full-list
                            gnuplot-gui-plot-simple-list))
            (cons "splot" (if (equal gnuplot-gui-plot-splot-fit-style 'complete)
                              gnuplot-gui-splot-full-list
                            gnuplot-gui-splot-simple-list))
            (cons "fit"   (if (equal gnuplot-gui-plot-splot-fit-style 'complete)
                              gnuplot-gui-fit-full-list
                            gnuplot-gui-fit-simple-list))) )


(defvar gnuplot-gui-test-type nil)
(setq gnuplot-gui-test-type
      (list (cons "test"
                  '(("TAG"      'tag      " ")
                    ("LIST"     'list     " " "1" "2" "3")
                    ("LIST*"    'list*    " " "1" "2" "3")
                    ("NUMBER"   'number   " " "number")
                    ("RANGE"    'range   (" " . " ") ":")
                    ("PAIR"     'pair    (" " . " ") "pair")
                    ("LABELS"   'labels   ())
                    ("FILE"     'file     " ")
                    ("TEXT"     'text     "this is text")
                    ("STRING"   'string   " ")
                    ("STRING*"  'string*  " " "string*")
                    ("FORMAT"   'format   " ")
                    ("POSITION" 'position " " "at" 3)
                    ("FONTSIZE" 'fontsize " ") ))))

(defvar gnuplot-gui-all-types nil
  "Associated list of terminal, set option, and command arguments.

Each entry in the list is a cons cell of the form
      (OPTION . ALIST)
where OPTION is one of the recognized options in Gnuplot, either a
command, something that is set, or a terminal type.  Only those
commands, set options, and terminal types that actually take arguments
are in this associated list.

ALIST is itself an associated list where each entry is of the form:

      (TAG TYPE DEFAULT REST)

TAG is the name used on the widget and indicates one of the options
for this command, set option, or terminal type.

TYPE is one of
     'list       a menu-list of strings
     'list*      a menu-list of strings with a prefix
     'number     a number with an optional prefix
     'tag        like number but must be the first argument
     'fontsize   like number but must be the last argument
     'range      a pair of numbers like [#,#] or [#:#]
     'pair       a pair of numbers with no punctuation and a prefix
     'file       a quoted string and a file browser
     'string     a quoted string with an optional prefix
     'string*    an unquoted string with a prefix
     'format     a quoted string and an info-link to (gnuplot)format
     'labels     an array as needed for xtics, ytics, etc
     'position   2 or 3 comma separated numbers with an optional prefix

DEFAULT is the default value for this option.  Note that the default
for 'range and 'pair is a cons cell and the default for 'labels is a
list.  For most things, the best choice of DEFAULT is a string of
white space or a cons cell of two strings of white space.  Strings of
white space are better defaults than empty strings or nil.

The value of REST depends upon TYPE:

  For 'list &    REST is the list of options that will go into the
      'list*       menu-button.  This can also be a symbol which
                   evaluates to a list containing the options to go into
                   the menu-button.  This list variable must contain the
                   DEFAULT.
  For 'number    REST is the prefix string (if it exists) for that number.
  For 'range     REST is the separator, \":\" for plot ranges and
                   \",\" for plot dimensions (see for example the tgif
                   terminal type)
  For 'string &  REST may a number denoting the width of the editable-text
      'string*     field or it may be a string denoting a prefix.  By
                   default, the width is half the width of the frame
                   and there is no prefix.  It may be useful to
                   specify \"1\" when the input is a single character
                   as in 'set missing'.
  For 'file      REST determines the label placed before the file insertion
                   field.  If non-nil, then TAG is used.  If nil, then
                   the default \"File\" is used.
  For 'position  REST is the prefix and the number of comma separated numbers
  For others     REST is not used.

Here is an example entry for the png terminal type:

  (cons \"png\"
    '((\"SIZE\"  'list \" \" \"small\" \"medium\" \"large\")
      (\"COLOR\" 'list \" \" \"monochrome\" \"gray\" \"color\")))

This alist is formed at load time by appending together
`gnuplot-gui-terminal-types', `gnuplot-gui-set-types' and
`gnuplot-gui-command-types'.")

(setq gnuplot-gui-all-types (append gnuplot-gui-terminal-types
                                    gnuplot-gui-set-types
                                    gnuplot-gui-command-types
                                    gnuplot-gui-plot-splot-fit
                                    gnuplot-gui-test-type))


;;;###autoload
(defun gnuplot-gui-swap-simple-complete ()
  (interactive)
  (setq gnuplot-gui-plot-splot-fit-style
        (if (equal gnuplot-gui-plot-splot-fit-style 'complete)
            'simple 'complete))
  (if (equal gnuplot-gui-plot-splot-fit-style 'complete)
      (progn
        (setcdr (assoc "plot"  gnuplot-gui-all-types) gnuplot-gui-plot-full-list)
        (setcdr (assoc "splot" gnuplot-gui-all-types) gnuplot-gui-splot-full-list)
        (setcdr (assoc "fit"   gnuplot-gui-all-types) gnuplot-gui-fit-full-list))
    (setcdr (assoc "plot"  gnuplot-gui-all-types) gnuplot-gui-plot-simple-list)
    (setcdr (assoc "splot" gnuplot-gui-all-types) gnuplot-gui-splot-simple-list)
    (setcdr (assoc "fit"   gnuplot-gui-all-types) gnuplot-gui-fit-simple-list))
  (message "Using %s lists for plot, splot, and fit."
           gnuplot-gui-plot-splot-fit-style) )




;;; user interface to the widget-y stuff

;;;###autoload
(defun gnuplot-gui-mouse-set (event)
  "Use the mouse to begin setting options using a GUI interface.
EVENT is a mouse event.  Bound to \\[gnuplot-gui-mouse-set]
Note that \"plot\", \"splot\", \"fit\", and \"cntrparam\" are not
currently supported."
  (interactive "@e")
  (save-excursion
    (mouse-set-point event)
    (gnuplot-gui-set-options-and-insert)))

(defun gnuplot-gui-get-frame-param (param)
  (cdr (assoc param gnuplot-gui-frame-parameters)))

(defun gnuplot-gui-set-frame-param (param value)
  (setcdr (assoc param gnuplot-gui-frame-parameters) value))

;;;###autoload
(defun gnuplot-gui-set-options-and-insert ()
  "Insert arguments using a GUI interface.
Determine contents of current line and set up the appropriate GUI
frame.  Bound to \\[gnuplot-gui-set-options-and-insert]
Note that \"cntrparam\" is not currently supported."
  (interactive)
  (let ((begin  (gnuplot-point-at-beginning-of-command))
        (end    (save-excursion (end-of-line)       (point-marker)))
        (termin (concat "\\(,\\s-*" (regexp-quote "\\") "\\|;\\)"))
        (set nil) (term nil))
    (save-excursion
      ;; there can be more then one command per line
      (if (re-search-forward termin end "to_limit")
          (progn (backward-char (length (match-string 1)))
                 (setq end (point-marker))))
      (goto-char begin)
      (skip-syntax-forward "-" end)
      ;; various constructions are recognized here. at the end of this
      ;; cond, point should be just after the word whose arguments are
      ;; to be set
      (cond ((looking-at "set\\s-+")
             (setq set t)
             (goto-char (match-end 0))
             (if (looking-at "\\sw+") (goto-char (match-end 0)))
             (when (string-match "^ter" (gnuplot-this-word)) ; terminal?
               (setq term t)
               (forward-word 1))
             (when (string-match "^\\(da\\|fu\\)" (gnuplot-this-word))
               (unless (looking-at "\\s-+st")
                 (insert " style") (forward-word 1))
               (forward-word 1)))
            ((looking-at (concat "\\(cd\\|ca\\|lo\\|pa\\|pr\\|sa\\|u\\)"
                                 "\\w*"
                                 "[\\s-\\']"))
             (forward-word 1))
            ;;(goto-char (match-end 0)))
            (t
             (forward-word 1)))
      (if (> (point) end) (goto-char end))
      (let* ((w (gnuplot-this-word))
             (wd (try-completion w gnuplot-gui-all-types))
             (word "") wrd list)
        (cond ((equal wd t)                     (setq word w))
              ((equal wd nil)                   (setq word w))
              ((assoc wd gnuplot-gui-all-types) (setq word wd))
              (t                                (setq wd nil)))
        (cond ((equal (string-match "^\\s-*$" w) 0)
               (message "Blank line"))
              ((and wd (stringp word))
               (gnuplot-gui-correct-command word set term begin)
               (setq gnuplot-gui-alist nil
                     gnuplot-gui-current-string
                     (buffer-substring-no-properties (point) end))
               (gnuplot-gui-set-alist word gnuplot-gui-current-string)
               (let* ((old-height (gnuplot-gui-get-frame-param 'height))
                      (old-top    (gnuplot-gui-get-frame-param 'top)))
                 (when (or
                        (and (equal gnuplot-gui-plot-splot-fit-style 'complete)
                             (cl-member word '("plot" "splot" "fit")
                                        :test 'string=))
                        (equal word "test"))
                   (gnuplot-gui-set-frame-param 'height 32)
                   (gnuplot-gui-set-frame-param 'top    50))
                 (gnuplot-gui-prompt-for-frame word)
                 (when (or
                        (and (equal gnuplot-gui-plot-splot-fit-style 'complete)
                             (cl-member word '("plot" "splot" "fit")
                                        :test 'string=))
                        (equal word "test"))
                   (gnuplot-gui-set-frame-param 'height old-height)
                   (gnuplot-gui-set-frame-param 'top    old-top)) ))
              ((setq wrd (car (all-completions w '(("cntrparam")))))
               (message
                "Setting arguments for %S is currently unsuported in gnuplot-mode"
                wrd))
              ((setq list (all-completions w gnuplot-gui-all-types))
               (message "%S could be one of %S" w list))
              (t
               (message
                "%S is not a gnuplot command which takes options" w)))) )))

;;;###autoload
(defun gnuplot-gui-toggle-popup ()
  (interactive)
  (setq gnuplot-gui-popup-flag (not gnuplot-gui-popup-flag))
  (message (if gnuplot-gui-popup-flag
               "Argument popup will appear after insertions."
             "Argument popup will no longer appear after insertions.")))


(defalias 'gnuplot-gui-y-n 'y-or-n-p)

(defun gnuplot-gui-correct-command (word set term begin)
  "Check syntax of set command and terminal specifications.
WORD is the item being set. SET and TERM are non-nil if the words
\"set\" and \"terminal\" were found preceding WORD in the buffer.
BEGIN is the beginning of the command."
  (save-excursion
    (cond ((assoc word gnuplot-gui-terminal-types)
           (when (and (not (and set term))
                      (gnuplot-gui-y-n
                       (format
                        "%S must be preceded by \"set terminal\".  Add it? "
                        word)))
             (backward-word 1)
             (let ((e (point-marker)))
               (goto-char begin)
               (skip-syntax-forward "-" e)
               (delete-region (point) e)
               (insert "set terminal "))))
          ((assoc word gnuplot-gui-set-types)
           (when (and (not set)
                      (gnuplot-gui-y-n
                       (format
                        "%S must be preceded by \"set\".  Add \"set\"? " word)))
             (backward-word 1)
             (let ((e (point-marker)))
               (goto-char begin)
               (skip-syntax-forward "-" e)
               (delete-region (point) e)
               (insert "set "))))))
  (message nil))



;;; handle the actual arguments

(defun gnuplot-gui-fix-arg-list (list)
  "Correct the result of splitting `gnuplot-gui-current-string'.
LIST is the split string.  This removes empty and all-blank strings
from the list and concatenates the strings that are part of a quoted
argument, for example an axis label or a font name.  It also replaces
bounding single quotes with double quotes, since double quotes are
used in `gnuplot-gui-all-types'."
  (let (fixed-list quote quoted)    ; remove blanks
    (setq list (cl-remove "\\s-+" list :test 'string-match)
          list (cl-remove ""      list :test 'string=))
    (while list             ; concatinate parts of quoted string
      (if (not (string-match "^\\([\]\[()'\"]\\)" (car list)))
          (setq fixed-list (append fixed-list (list (car list))))
        (setq quote (match-string 1 (car list))
              quoted (car list))
        (if (string= quote "[") (setq quote "]"))
        (if (string= quote "(") (setq quote ")"))
        (while (and list
                    (or (equal (length quoted) 1)
                        (not (string-match (concat (regexp-quote quote) "$")
                                           quoted))))
          (setq quoted (concat quoted " " (cadr list))
                list (cdr list)))
        (if (string= quote "'")
            (setq quoted (concat "\"" (substring quoted 1))
                  quoted (concat (substring quoted 0 -1) "\"")))
        (setq fixed-list (append fixed-list (list quoted))))
      (setq list (cdr list)) )
    fixed-list))

(defun gnuplot-gui-set-alist (word string)
  "Set defaults for arguments, using text from buffer if appropriate.
WORD is the Gnuplot expression whose arguments are being set.  STRING
is text from the buffer containing the previous values for WORD's
arguments."
  (let ((alist    (cdr (assoc word gnuplot-gui-all-types)))
        (arg-list (gnuplot-gui-fix-arg-list (split-string string)) ))
    ;; arg-list contains the arguments taken from the buffer
    (setq gnuplot-gui-alist nil)
    (while alist
      (let* ((list      (car alist))
             (tag       (gnuplot-gui-type-tag     list))
             (symbol    (eval (gnuplot-gui-type-symbol list)))
             (default   (gnuplot-gui-type-default list))
             (prefix    (gnuplot-gui-type-prefix  list))
             (values    (gnuplot-gui-type-list    list))
             (this-cons (cond ((stringp default) (cons tag default))
                              ((consp default) ; set cons valued default w/care
                               (cons tag (cons (car default) (cdr default))))
                              (t (cons tag default))))
             (temp-list arg-list) )
        ;;(message "%S" temp-list)  ; want to lop values off arg-list
                                        ; as they are found
        (if (symbolp (cadr values))
            (setq values (symbol-value (cadr values))))
        ;; check if an argument of this type is in arg-list
        ;; set the current cons cell if it is
        (while temp-list
          (cond
           ;; ---------------------------- list
           ((cl-member symbol '(list list*) :test 'equal)
            (let* ((case-fold-search nil)
                   (match-cons (cl-member (concat "^" (car temp-list))
                                        values :test 'string-match)))
              (if (and (car match-cons) ; " " may be first elem. of list
                       (not (string= " " (car match-cons))))
                  (setq this-cons (cons tag (car match-cons))
                        arg-list (cl-remove (car temp-list) arg-list
                                          :test 'string= :count 1)
                        temp-list nil)
                (setq temp-list (cdr temp-list)))))
           ;; ---------------------------- tag (first number in list)
           ((equal symbol 'tag)
            (if (string-match "^[-0-9.]+$" (car arg-list))
                (setq this-cons (cons  tag (car arg-list))
                      temp-list nil)
              (setq temp-list (cdr temp-list))) )
           ;; ---------------------------- fontsize (last number in list)
           ((equal symbol 'fontsize)
            (if (string-match "^[-0-9.]+$" (car (last arg-list)))
                (setq this-cons (cons  tag (car (last arg-list)))
                      temp-list nil)
              (setq temp-list (cdr temp-list))) )
           ;; ---------------------------- number with prefix
           ((equal symbol 'number)
            (cond ((and (string= prefix (car temp-list))
                        (string-match "^[-0-9.]+$" (cadr temp-list)))
                   (setq this-cons (cons tag (cadr temp-list))
                         arg-list (cl-remove (car temp-list) arg-list
                                           :test 'string= :count 1)
                         arg-list (cl-remove (cadr temp-list) arg-list
                                           :test 'string= :count 1)
                         temp-list nil))
                  ;; --------------------- number without prefix
                  ((and (not prefix)
                        (string-match "^[-0-9.]+$" (car temp-list)))
                   (setq this-cons (cons tag (car temp-list))
                         arg-list (cl-remove (car temp-list) arg-list
                                           :test 'string= :count 1)
                         temp-list nil))
                  (t
                   (setq temp-list (cdr temp-list)))))
           ;; ---------------------------- pair with prefix
           ((equal symbol 'pair)
            (if (and (string= prefix (car temp-list))
                     (string-match "^[-0-9.]+$" (cadr temp-list)))
                (let ((this-car (cadr temp-list))
                      (this-cdr (if (string-match "^[-0-9.]+$" (cl-caddr temp-list))
                                    (cl-caddr temp-list) "")))
                  (setq this-cons (cons tag (cons this-car this-cdr))
                        temp-list nil))
              (setq temp-list (cdr temp-list))))
           ;; ---------------------------- range
           ((equal symbol 'range)
            (if (string-match (concat "\\[\\s-*" ; opening bracket
                                      "\\([^:, \t]*\\)" ; first argument
                                      "\\s-*[:,]\\s-*" ; separator
                                      "\\([^\] \t]*\\)" ; second argument
                                      "\\s-*\\]") ; closing bracket
                              (car temp-list))
                (setq this-cons
                      (cons tag (cons (match-string 1 (car temp-list))
                                      (match-string 2 (car temp-list))))
                      arg-list (cl-remove (car temp-list) arg-list
                                        :test 'string= :count 1)
                      temp-list nil)
              (setq temp-list (cdr temp-list)) ))
           ;; ---------------------------- labels
           ((equal symbol 'labels)
            (if (string-match (concat "(" ; opening paren
                                      "\\([^\)]*\\)" ; string
                                      ")") ; closing paren
                              (car temp-list))
                (let* ((list (split-string (car temp-list) "[ \t(),]+"))
                       (list (cl-remove "" list :test 'string=))
                       (return ()))
                  (while list
                    (if (string-match "['\"]\\([^'\"]*\\)['\"]" (car list))
                        (setq return (append return
                                             (list (match-string 1 (car list))))
                              list (cdr list)
                              return (append return (list (car list))) )
                      (setq return (append return (list "" (car list)))))
                    (setq list (cdr list)) )
                  (setq this-cons (cons tag return)
                        arg-list (cl-remove (car temp-list) arg-list
                                          :test 'string= :count 1)
                        temp-list nil))
              (setq temp-list (cdr temp-list))) )
           ;; ---------------------------- string, file, format
           ((cl-member symbol '(string file format) :test 'equal)
            (if (string-match (concat "['\"]" ; opening quote
                                      "\\([^'\"]*\\)" ; string
                                      "['\"]") ; closing quote
                              (car temp-list))
                (setq this-cons (cons tag (match-string 0 (car temp-list)))
                      arg-list (cl-remove (car temp-list) arg-list
                                        :test 'string= :count 1)
                      temp-list nil)
              (setq temp-list (cdr temp-list)) ))
           ;; ---------------------------- string*
           ((equal symbol 'string*)
            (if (string= prefix (car temp-list))
                (setq this-cons (cons tag (cadr temp-list))
                      arg-list (cl-remove (car temp-list) arg-list
                                        :test 'string= :count 1)
                      arg-list (cl-remove (cadr temp-list) arg-list
                                        :test 'string= :count 1)
                      temp-list nil)
              (setq temp-list (cdr temp-list)) ) )
           ;; ---------------------------- other or unknown
           (t
            (setq temp-list nil))))
        (setq gnuplot-gui-alist
              (append gnuplot-gui-alist (list this-cons))))
      (setq alist (cdr alist))) ))


(defun gnuplot-gui-post-process-alist (type)
  "A few types need some additional processing.
'range, 'pair, and 'labels are cons or list valued and need to b made
into strings.  This is called right before inserting the arguments
into the buffer.  TYPE is the object whose arguments are being set."
  (let ((alist gnuplot-gui-alist)
        (types (cdr (assoc type gnuplot-gui-all-types))) )
    (while alist  ;; loop thru alist looking for tyeps needing post-processing
      (let* ((list   (assoc (caar alist) types))
             (value  (cdr (assoc (caar alist) gnuplot-gui-alist)))
             (prefix (gnuplot-gui-type-prefix list))
             (symb   (gnuplot-gui-type-symbol list)) )
        (cond
         ;;-------------------------- flat text
         ((equal (eval symb) 'text)
          (setcdr (assoc (caar alist) gnuplot-gui-alist) ""))
         ;;-------------------------- range [#:#] or [#,#]
         ((equal (eval symb) 'range)
          (if (and (string-match "^\\s-*$" (car value))
                   (string-match "^\\s-*$" (cdr value)))
              (setcdr (assoc (caar alist) gnuplot-gui-alist) "")
            (setcdr (assoc (caar alist) gnuplot-gui-alist)
                    (concat "[" (car value) prefix (cdr value) "]")) ) )
         ;;-------------------------- pair
         ((equal (eval symb) 'pair)
          (if (and (string-match "^\\s-*$" (car value))
                   (string-match "^\\s-*$" (cdr value)))
              (setcdr (assoc (caar alist) gnuplot-gui-alist) "")
            (setcdr (assoc (caar alist) gnuplot-gui-alist)
                    (concat prefix " " (car value) " " (cdr value) )) ) )
         ;;-------------------------- labels
         ((equal (eval symb) 'labels)
          (if (consp value)
              (let ((word "") (list value))
                (while list
                  (if (string-match "^\\s-*$" (car list))
                      (setq word (concat word (format "%s, " (cadr list))))
                    (setq word (concat word (format "%S %s, " (car list)
                                                    (cadr list)))))
                  (setq list (cddr list)) )
                (setq value (concat "(" (substring word 0 -2) ")")))
            (setq value "") )
          (setcdr (assoc (caar alist) gnuplot-gui-alist) value) ))

        (setq alist (cdr alist))) )))


;;; GUI frames

(defun gnuplot-gui-prompt-for-frame (&optional option save-frame)
  (setq option (or option (completing-read "Option: " gnuplot-gui-all-types
                                           nil t nil t)))
  (gnuplot-gui-make-frame
   option (cdr (assoc option gnuplot-gui-all-types)) save-frame) )


(defface gnuplot-gui-error-face '((((class color) (background light))
                                   (:foreground "grey30"))
                                  (((class color) (background dark))
                                   (:foreground "grey70")))
  "Face used to display message about unknown widget types."
  :group 'gnuplot-faces)

(defface gnuplot-gui-flat-text-face '((((class color) (background light))
                                       (:foreground "MediumBlue"))
                                      (((class color) (background dark))
                                       (:foreground "LightSteelBlue")))
  "Face used to display message about unknown widget types."
  :group 'gnuplot-faces)

(defun gnuplot-gui-make-frame (item alist &optional save-frame)
  "Open the frame and populate it with widgets.
ITEM is the object for which arguments are being set.  ALIST is
the alist of arguments for ITEM taken from `gnuplot-gui-all-types'.
SAVE-FRAME is non-nil when the widgets are being reset."
  (unless save-frame
    (setq gnuplot-current-frame (selected-frame)
          gnuplot-current-buffer (current-buffer)
          gnuplot-current-buffer-point (point-marker))
    (unless (and gnuplot-gui-frame (frame-live-p gnuplot-gui-frame))
      (setq gnuplot-gui-frame (make-frame gnuplot-gui-frame-parameters)))
    (select-frame gnuplot-gui-frame)
    ;;(set-frame-position gnuplot-gui-frame 150 150) ;; so herky-jerky
    (set-mouse-position gnuplot-gui-frame 0 0))
  (kill-buffer (get-buffer-create "*Gnuplot GUI*"))
  (switch-to-buffer (get-buffer-create "*Gnuplot GUI*"))
  (kill-all-local-variables)
  (modify-frame-parameters (selected-frame)
                           '((title . "Set Gnuplot Options")))
  (widget-insert "\nSet options for \"" item "\"  ")
  (let (tag help val)
    (cond ((string-match "^[xyz]2?tics" item)
           (setq tag  "info on tic labels"
                 help "Open a frame displaying the info entry for tic labels"
                 val  "xtics"))
          ((string-match "^no" item)
           (setq tag  (concat "info on " (substring item 2))
                 help (format "Open a frame displaying the info entry for %S"
                              item)
                 val  item))
          (t
           (setq tag  (concat "info on " item)
                 help (format "Open a frame displaying the info entry for %S"
                              item)
                 val  item)))
    (widget-create 'gnuplot-gui-info-link :tag tag :help-echo help :value val))

  (widget-insert "\n\n")
  (while alist
    (let* ((this    (car   alist))
           (tag     (gnuplot-gui-type-tag    this))
           (wtype   (gnuplot-gui-type-symbol this))
           (prefix  (gnuplot-gui-type-prefix this))
           (default (cdr (assoc tag gnuplot-gui-alist)))
           (list    (gnuplot-gui-type-list   this)))
      (if (symbolp (cadr list))
          (setq list (symbol-value (cadr list))))
      (widget-insert "\t")      ; insert the appropriate widget
      (cond
       ;;------------------------------ list, list* ------------
       ((cl-member (eval wtype) '(list list*) :test 'equal)
        (let ((starred (if (equal (eval wtype) 'list*) t nil)))
          (gnuplot-gui-menu-choice tag default list starred)))
       ;;------------------------------ number, tag, fontsize --
       ((cl-member (eval wtype) '(number tag fontsize) :test 'equal)
        (gnuplot-gui-number tag default prefix))
       ;;------------------------------ position ---------------
       ;;------------------------------ range, pair ------------
       ((cl-member (eval wtype) '(range pair) :test 'equal)
        (let ((is-range (equal (eval wtype) 'range)))
          (gnuplot-gui-range tag default prefix is-range)))
       ;;------------------------------ string, string* --------
       ((cl-member (eval wtype) '(string string*) :test 'equal)
        (let ((starred (if (equal (eval wtype) 'string) nil t)))
          (gnuplot-gui-string tag default prefix starred)))
       ;;------------------------------ format -----------------
       ((equal (eval wtype) 'format)
        (gnuplot-gui-format tag default))
       ;;------------------------------ file -------------------
       ((equal (eval wtype) 'file)
        (gnuplot-gui-file tag default prefix))
       ;;------------------------------ labels -----------------
       ((equal (eval wtype) 'labels)
        (gnuplot-gui-labels tag default))
       ;;------------------------------ text -------------------
       ((equal (eval wtype) 'text)
        (let ((str (gnuplot-gui-type-default this)))
          (put-text-property 0 (length str) 'face 'gnuplot-gui-flat-text-face str)
          (widget-insert str "\n")))
       ;;------------------------------ unknown ----------------
       (t
        (let ((str (concat "<" (downcase tag) "> ('"
                           (symbol-name (eval wtype))
                           " arguments are not yet supported)\n")))
          (put-text-property 0 (length str) 'face 'gnuplot-gui-error-face str)
          (widget-insert str)) )))
    (setq alist (cdr alist)))
  ;; insert control buttons: [Set options]   [Reset]   [Clear]   [Cancel]
  (widget-insert "\n\t")
  (widget-create 'push-button
                 :value "Set options"
                 :doc item
                 :button-face 'gnuplot-gui-button-face
                 :help-echo "Push this button to set options"
                 :notify
                 (lambda (widget &rest _ignore)
                   (kill-buffer (get-buffer-create "*Gnuplot GUI*"))
                   (delete-frame)
                   (select-frame gnuplot-current-frame)
                   (switch-to-buffer gnuplot-current-buffer)
                   (goto-char gnuplot-current-buffer-point)
                   (gnuplot-gui-post-process-alist
                    (widget-get widget :doc))
                   (let ((alist gnuplot-gui-alist) marker
                         (eol (save-excursion (end-of-line) (point-marker) )) )
                     (if (re-search-forward ";" eol "to_limit")
                         (backward-char 1))
                     (delete-region gnuplot-current-buffer-point (point-marker))
                     (delete-horizontal-space)
                     (setq marker (point-marker))
                     (while alist
                       (let ((val (cdar alist)))
                         (if (string-match "^\\s-+$" val) ()
                           (if (string-match "^['\"]\\(.*\\)['\"]$" val)
                               (setq val (concat gnuplot-quote-character
                                                 (match-string 1 val)
                                                 gnuplot-quote-character)))
                           (insert (format " %s" val))))
                       (setq alist (cdr alist)))
                     (setq eol (point-marker))
                     (goto-char marker)
                     (while (< (point) eol) ; a few odd cases
                       (unless (looking-at (concat "[" (regexp-quote "(")
                                                   (regexp-quote "*") ",]"))
                         (just-one-space))
                       (forward-sexp)))
                   (delete-horizontal-space)
                   (if (string= "terminal" (widget-get widget :doc))
                       (gnuplot-gui-set-options-and-insert)) ))
  (widget-insert "   ")
  (widget-create 'push-button :value "Reset"
                 :help-echo "Push this button to reset all values"
                 :button-face 'gnuplot-gui-button-face
                 :doc item
                 :notify
                 (lambda (widget &rest _ignore)
                   (let ((word (widget-get widget :doc)))
                     (gnuplot-gui-set-alist word gnuplot-gui-current-string)
                     (gnuplot-gui-prompt-for-frame word t))))
  (widget-insert "   ")
  (widget-create 'push-button :value "Clear"
                 :help-echo "Push this button to clear all values"
                 :button-face 'gnuplot-gui-button-face
                 :doc item
                 :notify
                 (lambda (widget &rest _ignore)
                   (let* ((word (widget-get widget :doc))
                          (alist (cdr (assoc word gnuplot-gui-all-types))))
                     (while alist
                       (setcdr (assoc (gnuplot-gui-type-tag (car alist))
                                      gnuplot-gui-alist)
                               (gnuplot-gui-type-default (car alist)))
                       (setq alist (cdr alist)))
                     (gnuplot-gui-prompt-for-frame word t))) )
  (widget-insert "   ")
  (widget-create 'push-button :value "Cancel"
                 :help-echo "Quit setting options and dismiss frame"
                 :button-face 'gnuplot-gui-button-face
                 :notify (lambda (_widget &rest _ignore)
                           (kill-buffer (get-buffer-create "*Gnuplot GUI*"))
                           (setq gnuplot-gui-alist nil
                                 gnuplot-gui-current-string nil)
                           (delete-frame)
                           (select-frame gnuplot-current-frame)))
  (goto-char (point-min))
  (use-local-map widget-keymap)
  (widget-setup))


;;; widgets

(defface gnuplot-gui-menu-face '((((class color) (background light))
                                  (:bold t :foreground "darkolivegreen"))
                                 (((class color) (background dark))
                                  (:bold t :foreground "seagreen"))
                                 (t
                                  (:italic t)))
  "Face used for menu-buttons."
  :group 'gnuplot-faces)
(defface gnuplot-gui-button-face '((((class color) (background light))
                                    (:bold t :foreground "sienna"))
                                   (((class color) (background dark))
                                    (:bold t :foreground "tan"))
                                   (t
                                    (:italic t)))
  "Face used for push-buttons."
  :group 'gnuplot-faces)
(defface gnuplot-gui-labels-face '((((class color) (background light))
                                    (:bold t :foreground "darkslateblue"))
                                   (((class color) (background dark))
                                    (:bold t :foreground "lightslateblue"))
                                   (t
                                    (:italic t)))
  "Face used for insert and delete button in the labels widget."
  :group 'gnuplot-faces)

(defun gnuplot-gui-menu-choice (item default list &optional starred)
  "Create a menu widget for the Gnuplot GUI.
ITEM is the object whose arguments are set by this widget, DEFAULT
is the default argument value, LIST contains the items for the pop-up
menu.  STARRED is true if this a 'list* widget."
  (let ((widget
         (apply 'widget-create
                'menu-choice :value default :tag item :doc starred
                :button-face 'gnuplot-gui-menu-face
                :button-prefix "[" :button-suffix "]"
                :help-echo (format "Mouse-2 to view the %S menu" (downcase item))
                :notify
                (lambda (widget &rest _ignore)
                  (let ((lab (if (widget-get widget :doc)
                                 (concat (downcase (widget-get widget :tag)) " ")
                               "" )))
                    (setcdr (assoc (widget-get widget :tag) gnuplot-gui-alist)
                            (if (string= (widget-value widget) " ") ""
                              (format "%s%s" lab (widget-value widget))) )))
                (mapcar (lambda (x) (list 'item :value x))
                        list))))
    (widget-value-set widget default)
    (if (and starred (not (string-match "^\\s-*$" default)))
        (setcdr (assoc item gnuplot-gui-alist)
                (format "%s %s" (downcase item) default)))
    widget))

(defun gnuplot-gui-number (item default &optional prefix)
  "Create a number widget for the Gnuplot GUI.
ITEM is the object whose arguments are set by this widget, DEFAULT
is the default value for the widget, PREFIX is a text string preceding
the numerical argument."
  (let ((help-label (or prefix (downcase item))))
    (widget-insert (capitalize item) ": ")
    (widget-create 'editable-field
                   :size 2 :tag item :value default :doc prefix
                   :help-echo (format "Insert new value of %S here" help-label)
                   :notify (lambda (widget &rest _ignore)
                             (let ((val (widget-value widget))
                                   (pre (concat (widget-get widget :doc) " ")))
                               (setcdr (assoc (widget-get widget :tag)
                                              gnuplot-gui-alist)
                                       (if (string-match
                                            "^\\s-*[-0-9.*]+\\s-*$" val)
                                           (format "%s%s" pre val) "") )))))
  (unless (string-match "^\\s-*$" default)
    (setcdr (assoc item gnuplot-gui-alist) (format "%s %s" prefix default)))
  (widget-insert " " (make-string (- 40 (current-column)) ?.)
                 " (numeric value)\n"))

(defun gnuplot-gui-string (item default &optional width_or_prefix starred)
  "Create a string widget for the Gnuplot GUI.
ITEM is the object whose arguments are set by this widget, DEFAULT is
the default value for the widget, and WIDTH_OR_PREFIX is the width of
the text entry field (which defaults to half the frame width) or the
prefix for the string.  STARRED is t if quotes are not to be used."
  (let ((help-label (downcase item)) width (prefix "") (pp ""))
    (cond ((stringp width_or_prefix)
           (setq prefix width_or_prefix
                 pp prefix)
           (if starred (setq prefix (concat prefix "_star"))) )
          ((numberp width_or_prefix)
           (setq width width_or_prefix)))
    (setq width (or width (/ (frame-width) 2)))
    (if (string-match "^['\"]" default)
        (setq default (replace-match "" nil nil default)))
    (if (string-match "['\"]$" default)
        (setq default (replace-match "" nil nil default)))
    (widget-insert (capitalize item) ": ")
    (widget-create
     'editable-field
     :size width :tag item :doc prefix :value default
     :help-echo (format "Insert new value of %S here" help-label)
     :notify (lambda (widget &rest _ignore)
               (let ((val (widget-value widget))
                     (q gnuplot-quote-character)
                     (p (widget-get widget :doc)) )
                 (setcdr (assoc (widget-get widget :tag) gnuplot-gui-alist)
                         (if (string-match "^\\s-*$" val)
                             ""
                           (progn
                             (if (string-match "_star$" p)
                                 (setq p (concat (substring p 0 -5) " ")
                                       q ""))
                             (if (string-match "^\\s-+" val)
                                 (setq val (replace-match "" nil nil val)))
                             (if (string-match "\\s-+$" val)
                                 (setq val (replace-match "" nil nil val)))
                             (format "%s%s%s%s" p q val q)))))))
    (unless (string-match "^\\s-*$" default)
      (setcdr (assoc item gnuplot-gui-alist) (format "%s %s" pp default)))
    (widget-insert "\n")))

(defun gnuplot-gui-format (item default)
  "Create a string widget for the Gnuplot GUI.
ITEM is the object whose arguments are set by this widget, DEFAULT is
the default value for the widget, and WIDTH_OR_PREFIX is the width of
the text entry field (which defaults to half the frame width) or the
prefix for the string."
  (if (string-match "^['\"]" default)
      (setq default (replace-match "" nil nil default)))
  (if (string-match "['\"]$" default)
      (setq default (replace-match "" nil nil default)))
  (widget-insert (capitalize item) ": ")
  (widget-create 'editable-field
                 :size (/ (frame-width) 3) :tag item :value default
                 :help-echo (format "Insert new format string here")
                 :notify (lambda (widget &rest _ignore)
                           (let ((val (widget-value widget)))
                             (setcdr (assoc (widget-get widget :tag)
                                            gnuplot-gui-alist)
                                     (format "%s%s%s"
                                             gnuplot-quote-character
                                             val
                                             gnuplot-quote-character)))))
  (widget-insert "   ")
  (widget-create 'gnuplot-gui-info-link
                 :tag (concat "info on format")
                 :help-echo "Open a frame displaying the info entry for format"
                 :value "format")
  (widget-insert "\n"))


;; swiped from widget-color-complete
(defun gnuplot-gui-file-completion (widget)
  "Complete the filename in WIDGET."
  (let* ((str (buffer-substring-no-properties (widget-field-start widget)
                                              (point)))
         (file (or (file-name-nondirectory str) ""))
         (dir  (or (file-name-directory str) "./"))
         (val  (file-name-completion file dir)) )
    (cond ((eq val t)
           (message "Exact match"))
          ((null val)
           (error "Can't find completion for \"%s\"" str))
          ((not (string-equal str val))
           (insert (substring val (length file))))
          (t
           (message "Making completion list...")
           (let ((list (file-name-all-completions file dir)))
             (with-output-to-temp-buffer "*Completions*"
               (display-completion-list list)))
           (message "Making completion list...done")))))

(defun gnuplot-gui-file (item default &optional tag)
  "Create a file widget for the Gnuplot GUI.
ITEM is the object whose arguments is set by this widget, DEFAULT is
the default value for the argument.  TAG is non-nil if ITEM rather than
\"File:\" is to be used as the tag."
  (setq tag (if tag (capitalize item) "File"))
  (if (string-match "^['\"]" default)
      (setq default (replace-match "" nil nil default)))
  (if (string-match "['\"]$" default)
      (setq default (replace-match "" nil nil default)))
  (let ((widg (widget-create
               'file
               :value default :tag tag
               :size (- (/ (frame-width) 2) 3)
               :doc item :help-echo "Insert a filename here"
               :complete 'gnuplot-gui-file-completion
               :notify
               (lambda (widget &rest _ignore)
                 (setcdr (assoc (widget-get widget :doc) gnuplot-gui-alist)
                         (format "%s%s%s" gnuplot-quote-character
                                 (widget-value widget)
                                 gnuplot-quote-character)) )) ))
    (widget-insert " ")
    (widget-create
     'push-button :value "Browse"
     :doc item :help-echo "Browse directories for a filename."
     :parent widg
     :notify (lambda (widget &rest _ignore)
               (let ((fname (file-relative-name (read-file-name "File: ")
                                                default-directory))
                     (q gnuplot-quote-character))
                 (widget-value-set (widget-get widget :parent) fname)
                 (setcdr (assoc (widget-get widget :doc) gnuplot-gui-alist)
                         (format "%s%s%s" q fname q))
                 (widget-setup))))
    (widget-insert "\n")))

(defun gnuplot-gui-labels (item default)
  "Create a labels widget for the Gnuplot GUI.
ITEM is the object whose arguments is set by this widget, DEFAULT is
the default value for the argument."
  (widget-create
   '(editable-list
     (list :inline t :tag "Tic label"
           (string :tag "label" :size 10
                   :help-echo "Enter the tic label here" )
           (string :tag "  position" :size 10
                   :help-echo "Enter an expression for the tic location here" )))
   :tag (capitalize item)
   :value default
   :format "%{%t%}:\n%v\t  %i\n"
   :entry-format "\t  %i %d %v\n"
   :button-face 'gnuplot-gui-labels-face
   :notify (lambda (widget &rest _ignore)
             (setcdr (assoc (upcase (widget-get widget :tag))
                            gnuplot-gui-alist)
                     (widget-value widget)))))

(defun gnuplot-gui-range (item default separator is-range)
  "Create a range or pair widget for the Gnuplot GUI.
ITEM is the object whose arguments are set by this widget, DEFAULT is
the default value for the widget, SEPARATOR is a text string preceding
the numerical argument, or the prefix for a pair operator.  IS-RANGE
is non-nil if this is a 'range widget."
  (widget-insert (capitalize item) ": ")
  (if is-range (widget-insert "["))
  (widget-create 'editable-field
                 :size 4 :tag item :value (car default)
                 :help-echo (format "Insert the first value of the %S here"
                                    (downcase item))
                 :notify (lambda (widget &rest _ignore)
                           (setcar (cdr (assoc (widget-get widget :tag)
                                               gnuplot-gui-alist))
                                   (format "%s" (widget-value widget)))))
  (if is-range (widget-insert separator) (widget-insert "  "))
  (widget-create 'editable-field
                 :size 4 :tag item :value (cdr default)
                 :help-echo (format "Insert the second value of the %S here"
                                    (downcase item))
                 :notify (lambda (widget &rest _ignore)
                           (setcdr (cdr (assoc (widget-get widget :tag)
                                               gnuplot-gui-alist))
                                   (format "%s" (widget-value widget)))))
  (if is-range (widget-insert "]"))
  (widget-insert " " (make-string (- 39 (current-column)) ?.)
                 " (numeric values)\n"))


;; suppress compiler warning
;;(eval-when-compile (defun gnuplot-info-lookup-symbol (sym mode)))
(define-widget 'gnuplot-gui-info-link 'info-link
  "A link to an info file for the Gnuplot GUI."
  :action '(lambda (widget &optional event)
             (let ((gnuplot-info-display 'frame))
               (if gnuplot-keywords-pending     ; <HW>
                   (gnuplot-setup-info-look))
               (gnuplot-info-lookup-symbol (widget-value widget)
                                           'gnuplot-mode))))


;;; just about done

(provide 'gnuplot-gui)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; gnuplot-gui.el ends here
