;;; gnuplot.el --- Major-mode and interactive frontend for gnuplot -*- lexical-binding: t -*-

;; Copyright (C) 1998, 2011 Phil Type and Bruce Ravel, 1999-2012 Bruce Ravel

;; Author:           Jon Oddie, Bruce Ravel, Phil Type
;; Maintainer:       Maxime Tréca <maxime@gmail.com>, Daniel Mendler <mail@daniel-mendler.de>
;; Created:          1998
;; Version:          0.8.1
;; Keywords:         data gnuplot plotting
;; URL:              https://github.com/emacs-gnuplot/gnuplot
;; Package-Requires: ((emacs "25.1"))

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

;; This is a major mode for composing gnuplot scripts and displaying
;; their results using gnuplot.  It supports features of recent Gnuplot
;; versions (5.0 and up), but should also work fine with older
;; versions.
;;
;; This version of gnuplot-mode has been tested mostly on GNU Emacs
;; 25.
;;
;; Gnuplot-mode now includes context-sensitive support for keyword
;; completion and, optionally, eldoc-mode help text.  See the
;; commentary in gnuplot-context.el for more information.  If you
;; don't find it useful, it can be turned off by customizing
;; `gnuplot-context-sensitive-mode'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Acknowledgements:
;;    David Batty       <DB> (numerous corrections)
;;    Laurent Bonnaud   <LB> (suggestions regarding font-lock rules)
;;    Markus Dickebohm  <MD> (suggested `gnuplot-send-line-and-forward')
;;    Stephen Eglan     <SE> (suggested the use of info-look,
;;                            contributed a bug fix regarding shutting
;;                            down the gnuplot process, improvement to
;;                            `gnuplot-send-line-and-forward')
;;    Robert Fenk       <RF> (suggested respecting continuation lines)
;;    Michael Karbach   <MK> (suggested trimming the gnuplot process buffer)
;;    Alex Chan Libchen <AL> (suggested font-lock for plotting words)
;;    Kuang-Yu Liu      <KL> (pointed out buggy dependence on font-lock)
;;    Hrvoje Niksic     <HN> (help with defcustom arguments for insertions)
;;    Andreas Rechtsteiner <AR> (pointed out problem with C-c C-v)
;;    Michael Sanders   <MS> (help with the info-look interface)
;;    Jinwei Shen       <JS> (suggested functionality in comint buffer)
;;    Michael M. Tung   <MT> (prompted me to add pm3d support)
;;    Holger Wenzel     <HW> (suggested using `gnuplot-keywords-when')
;;    Wolfgang Zocher   <WZ> (pointed out problem with gnuplot-mode + speedbar)
;;    Jon Oddie         <jjo> (indentation, inline images, context mode)
;;    Maxime F. Treca   <MFT> (package update, XEmacs deprecation)
;;
;;  and especially to Lars Hecking <LH> for including gnuplot-mode
;;  with the gnuplot 3.7-beta distribution and for providing me with
;;  installation materials
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(require 'cl-lib)
(require 'comint)
(require 'custom)
(require 'easymenu)
(require 'info)
(require 'info-look)

(declare-function 'eldoc-add-command "eldoc")

(defgroup gnuplot nil
  "Gnuplot-mode for Emacs."
  :prefix "gnuplot-"
  :group 'processes
  :group 'applications
  :group 'local
  :link '(emacs-library-link :tag "Lisp File" "gnuplot.el")
  :link '(url-link :tag "Homepage"
                   "https://github.com/emacs-gnuplot/gnuplot/")
  :link '(custom-manual "(gnuplot)Top")
  :link '(emacs-commentary-link :tag "Commentary" "gnuplot.el"))

(defgroup gnuplot-insertions nil
  "Insert commands into gnuplot-scripts from a pull-down menu."
  :prefix "gnuplot-insertions-"
  :group 'gnuplot)

(defgroup gnuplot-hooks nil
  "Hook variables used by `gnuplot-mode'."
  :prefix "gnuplot-"
  :group 'gnuplot)

(defcustom gnuplot-mode-hook nil
  "Hook run when `gnuplot-mode' is entered."
  :group 'gnuplot-hooks
  :type 'hook)

(defcustom gnuplot-after-plot-hook (list #'gnuplot-trim-gnuplot-buffer)
  "Hook run after gnuplot plots something.
This is the last thing done by the functions for plotting a line, a
region, a buffer, or a file."
  :group 'gnuplot-hooks
  :type 'hook)

;; comint hook suggested by <DB>
(defcustom gnuplot-comint-setup-hook nil
  "Hook run after setting up the gnuplot buffer in comint mode.
So the configuration can be customised by the user."
  :group 'gnuplot-hooks
  :type 'hook)

(defcustom gnuplot-comint-mode-hook nil
  "Hook run after setting up the gnuplot buffer in `gnuplot-comint-mode'.
By default this runs the hook named `gnuplot-comint-setup-hook',
for backward compatibility."
  :group 'gnuplot-hooks
  :type 'hook)

(defvar-local gnuplot-recently-sent nil
  "This is a record of the most recent kind of text sent to gnuplot.
It takes as its value nil, `line', `region', `buffer', or `file'.  It is
useful for functions included in `gnuplot-after-plot-hook'.")

(defcustom gnuplot-program "gnuplot"
  "The name of the gnuplot executable."
  :group 'gnuplot
  :type 'string)

(defcustom gnuplot-program-args nil
  "Whitespace-separated flags to pass to the gnuplot executable."
  :group 'gnuplot
  :type 'string)

(defcustom gnuplot-process-name "gnuplot"
  "Name given to the gnuplot buffer and process."
  :group 'gnuplot
  :type 'string)

(defvar gnuplot-buffer nil
  "The name of the buffer displaying the gnuplot process.")

(defvar gnuplot-process nil
  "Variable holding the process handle.")

(defvar gnuplot-process-frame nil
  "The frame for displaying the gnuplot process.
This is used when `gnuplot-display-process' is equal to `frame'.")

(defvar gnuplot-comint-recent-buffer nil
  "The most recently plotted gnuplot script buffer.
This is used by the function that plot from the comint buffer.  It is
reset every time something is plotted from a script buffer.")

(defcustom gnuplot-gnuplot-buffer "plot.gp"
  "The name of the gnuplot scratch buffer opened by `gnuplot-make-buffer'."
  :group 'gnuplot
  :type 'string)

(defcustom gnuplot-display-process 'window
  "This controls how the gnuplot process buffer is displayed.
The values are
   \\='frame    display gnuplot process in a separate frame
   \\='window   display gnuplot process in this frame but in another window
   nil       `gnuplot-process' is in the current frame but not displayed"
  :group 'gnuplot
  :type '(radio (const :tag "Separate frame"  frame)
                (const :tag "Separate window" window)
                (const :tag "Not displayed"   nil)))

(defcustom gnuplot-info-display 'window
  "Determines how `gnuplot-info-lookup-symbol' displays the info file.
The values are
   \\='frame    display info file in a separate frame
   \\='window   display info file in another window
   nil       display info file in the current window"
  :group 'gnuplot
  :type '(radio (const :tag "Separate frame"  frame)
                (const :tag "Separate window" window)
                (const :tag "This window"     nil)))

(defcustom gnuplot-echo-command-line-flag
  (not (string-match "msvc" (emacs-version)))
  "Non-nil means the gnuplot subprocess echoes any input.
This sets the fall-back value of `comint-process-echoes'.
If `gnuplot-mode' cannot figure out what version number of gnuplot
this is, then the value of this variable will be used for
`comint-process-echos'.  It seems that gnuplot 3.5 wants this to be
nil and 3.7 wants it to be t.  If lines that you send to gnuplot from
the `gnuplot-mode' buffer are not appearing at the gnuplot prompt in
the process buffer, try toggling it.  Also see the document string for
`comint-process-echos'.  If you change this, kill the gnuplot process
and start it again."
  :group 'gnuplot
  :type 'boolean)

(defcustom gnuplot-insertions-show-help-flag nil
  "Non-nil means to display certain help messages automatically.
These messages are shown after menu insertion of gnuplot commands."
  :group 'gnuplot-insertions
  :type 'boolean)

(defcustom gnuplot-delay 0.01
  "Amount of time to delay before sending a new line to gnuplot.
This is needed so that the the line is not written in the gnuplot
buffer in advance of its prompt.  Increase this number if the
prompts and lines are displayed out of order."
  :group 'gnuplot
  :type 'number)

(defcustom gnuplot-buffer-max-size 1000
  "The maximum size in lines of the gnuplot process buffer.
Each time text is written in the gnuplot process buffer, lines are
trimmed from the beginning of the buffer so that the buffer is this
many lines long.  The lines are deleted after the most recent lines
were interpretted by gnuplot.  Setting to 0 turns off this feature."
  :group 'gnuplot
  :type 'integer)

(defcustom gnuplot-quote-character "\'"
  "Quotation character used for inserting quoted strings.
Gnuplot can use single or double quotes.  If you prefer to have the
filename insertion function never insert quotes for you, set this
to the empty string."
  :group 'gnuplot
  :type '(radio (const :tag "double quote"  "\"")
                (const :tag "single quote"  "\'")
                (const :tag "none"          ""  )))
(defcustom gnuplot-basic-offset 4
  "Number of columns to indent lines inside a do- or if-else-block.

This applies only to new-style do- and if-statements using
braces.  Commands continued over a linebreak using a backslash
are always indented to line up with the second word on the line
beginning the continued command."
  :group 'gnuplot
  :type 'integer)

(defvar gnuplot-info-frame nil)

;; with info-look, there is no need to carry this list around -- it
;; can be generated on the fly appropriate to the currently installed
;; version of gnuplot.info
(defvar gnuplot-keywords nil
  "A list of keywords used in GNUPLOT.
These are set by `gnuplot-set-keywords-list' from the values in
`info-lookup-cache'.")
(defvar gnuplot-keywords-pending t      ;; <HW>
  "A boolean which gets toggled when the info file is probed.")
(defcustom gnuplot-keywords-when 'deferred ;; 'immediately
  "This variable controls when the info file is parsed.
The choices are immediately upon starting `gnuplot-mode' or the first
time that data is needed."
  :group 'gnuplot
  :type
  '(radio (const :tag "Parse info file when gnuplot-mode starts"    immediately)
          (const :tag "Parse info file the first time it is needed" deferred)))

(defcustom gnuplot-use-context-sensitive-completion t
  "Non-nil if `gnuplot-context-sensitive-mode' should be enabled by default.

In context-sensitive mode, `gnuplot-mode' parses the current
command line to provide smarter completion and documentation
suggestions."
  :group 'gnuplot
  :type 'boolean
  :set (lambda (sym value)
         (set sym value)
         (cond
          (value
           (add-hook 'gnuplot-mode-hook 'gnuplot-context-sensitive-mode nil nil)
           (add-hook 'gnuplot-comint-mode-hook 'gnuplot-context-sensitive-mode nil nil))
          (t
           (remove-hook 'gnuplot-mode-hook 'gnuplot-context-sensitive-mode)
           (remove-hook 'gnuplot-comint-mode-hook 'gnuplot-context-sensitive-mode)))
         (dolist (buffer (buffer-list))
           (with-current-buffer buffer
             (when (and (derived-mode-p 'gnuplot-mode 'gnuplot-comint-mode)
                        (fboundp 'gnuplot-context-sensitive-mode))
               (gnuplot-context-sensitive-mode (if value 1 0))))))
  :link '(emacs-commentary-link "gnuplot-context"))

(defcustom gnuplot-eldoc-mode nil
  "Non-nil if ElDoc mode should be enabled by default in Gnuplot buffers.
ElDoc support requires `gnuplot-context-sensitive-mode' to be
on."
  :group 'gnuplot
  :type 'boolean)

(defcustom gnuplot-tab-completion nil
  "Non-nil if TAB should perform completion in `gnuplot-mode' buffers.

Setting this to non-nil sets the `tab-always-indent' variable to the
symbol `complete' in `gnuplot-mode' buffers."
  :group 'gnuplot
  :type 'boolean)

(defun gnuplot-set-display-mode (variable value &rest _args)
  "Customize :set function for `gnuplot-inline-image-mode'.
Set VARIABLE to VALUE.  ARGS is optional args."
  (if (and (eq variable 'gnuplot-inline-image-mode)
           value
           (not (display-images-p)))
      (progn
        (message "Displaying images is not supported.")
        (set variable nil))
    (set variable value))
  (gnuplot-setup-comint-for-image-mode))

(defcustom gnuplot-inline-image-mode nil
  "Whether to display Gnuplot output in Emacs.

Possible values are nil, `inline' and `dedicated'.

When this is nil, Gnuplot output is handled outside of Emacs in
the normal way.  Otherwise, Emacs attempts to capture Gnuplot's
output and display it in a buffer.  Output is inserted inline in
the Gnuplot interaction buffer it this is `inline', in a
separate dedicated buffer if it is `dedicated'.

Use Customize to set this variable, or the commands
`gnuplot-external-display-mode', `gnuplot-inline-display-mode',
and `gnuplot-dedicated-display-mode'."
  :group 'gnuplot
  :type '(radio
          (const :tag "No" nil)
          (const :tag "In Comint buffer" inline)
          (const :tag "In dedicated buffer" dedicated))
  :initialize #'custom-initialize-default
  :set #'gnuplot-set-display-mode)

(defcustom gnuplot-image-format "png"
  "Image format to use for displaying images within Emacs.

This will be sent directly to Gnuplot as a command of the form
\"set terminal <FORMAT>\".  Common values are \"png\" and
\"svg\".

This only has an effect when `gnuplot-inline-image-mode' is
non-nil."
  :group 'gnuplot
  :type 'string
  :initialize #'custom-initialize-default
  :set #'gnuplot-set-display-mode)

(defgroup gnuplot-faces nil
  "Text faces used by `gnuplot-mode'."
  :prefix "gnuplot-"
  :group 'gnuplot)

(defface gnuplot-prompt-face '((((class color))
                                (:foreground "firebrick"))
                               (t
                                (:bold t :underline t)))
  "Face used for the prompt in the gnuplot process buffer."
  :group 'gnuplot-faces)


;;; --- key bindings and menus

(defvar gnuplot-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-b"    #'gnuplot-send-buffer-to-gnuplot)
    (define-key map "\C-c\C-c"    #'comment-region) ; <RF>
    (define-key map "\C-c\C-o"    'gnuplot-gui-set-options-and-insert)
    (define-key map "\C-c\C-e"    #'gnuplot-show-gnuplot-buffer)
    (define-key map "\C-c\C-f"    #'gnuplot-send-file-to-gnuplot)
    (define-key map "\C-c\C-d"    #'gnuplot-info-lookup-symbol)
    (define-key map "\C-c\C-i"    #'gnuplot-insert-filename)
    (define-key map "\C-c\C-j"    #'gnuplot-forward-script-line)
    (define-key map "\C-c\C-k"    #'gnuplot-kill-gnuplot-buffer)
    (define-key map "\C-c\C-l"    #'gnuplot-send-line-to-gnuplot)
    (define-key map "\C-c\C-n"    #'gnuplot-negate-option)
    (define-key map "\C-c\C-r"    #'gnuplot-send-region-to-gnuplot)
    (define-key map (kbd "C-M-x") #'gnuplot-send-line-to-gnuplot)
    (define-key map "\C-c\C-v"    #'gnuplot-send-line-and-forward)
    (define-key map "\C-c\C-z"    #'gnuplot-customize)
    (define-key map "\C-i"        #'indent-for-tab-command)
    (define-key map "\C-m"        #'newline-and-indent)
    (define-key map (kbd "}")     #'gnuplot-electric-insert)
    (define-key map "\M-\r"       #'completion-at-point)
    (define-key map "\M-\t"       #'completion-at-point)
    (define-key map [S-mouse-2]   'gnuplot-gui-mouse-set)

    map))

(defvar gnuplot-mode-menu nil)

(defvar gnuplot-display-options-menu
  (cl-flet ((make-image-setter (type)
                            `[,(concat (upcase type) " images")
                              (lambda () (interactive) (gnuplot-set-image-format ,type))
                              :style toggle
                              :selected (eq gnuplot-image-format ,type)]))
    `("Display plot output"
      ["Externally" gnuplot-external-display-mode
       :style toggle
       :selected (null gnuplot-inline-image-mode)]
      ["In Comint buffer" gnuplot-inline-display-mode
       :active (display-images-p)
       :style toggle
       :selected (eq gnuplot-inline-image-mode 'comint)]
      ["In dedicated buffer" gnuplot-dedicated-display-mode
       :style toggle
       :selected (eq gnuplot-inline-image-mode 'dedicated)]
      "---"
      ,@(mapcar #'make-image-setter (list "png" "jpeg" "svg"))
      ["Other image type..." gnuplot-set-image-format])))

(defvar gnuplot-menu
  `("Gnuplot"
    ["Send line to gnuplot"             gnuplot-send-line-to-gnuplot   t]
    ["Send line & move forward"         gnuplot-send-line-and-forward (not (eobp))]
    ["Send region to gnuplot"           gnuplot-send-region-to-gnuplot
     (gnuplot-mark-active)]
    ["Send buffer to gnuplot"           gnuplot-send-buffer-to-gnuplot t]
    ["Send file to gnuplot"             gnuplot-send-file-to-gnuplot t]
    "---"
    ,gnuplot-display-options-menu
    ["Contextual completion and help"   gnuplot-context-sensitive-mode
     :style toggle
     :selected gnuplot-context-sensitive-mode]
    ["Echo area help (eldoc-mode)" eldoc-mode
     :active gnuplot-context-sensitive-mode
     :style toggle
     :selected eldoc-mode]
    "---"
    ["Insert filename at point"         gnuplot-insert-filename t]
    ["Negate set option"                gnuplot-negate-option t]
    ["Keyword help"                     gnuplot-info-lookup-symbol
     (or gnuplot-keywords gnuplot-keywords-pending)]
    ["Quick help for thing at point"    gnuplot-help-function
     gnuplot-context-sensitive-mode]
    ["Info documentation on thing at point"
     gnuplot-info-at-point
     gnuplot-context-sensitive-mode]
    ["Show gnuplot process buffer"      gnuplot-show-gnuplot-buffer t]
    ["Set arguments at point"           gnuplot-gui-set-options-and-insert t]
    ["Swap plot/splot/fit lists in GUI" gnuplot-gui-swap-simple-complete t]
    "---"
    ["Customize gnuplot"                gnuplot-customize t]
    "---"
    ["Kill gnuplot"                     gnuplot-kill-gnuplot-buffer t])
  "Menu for `gnuplot-mode'.")



;;; --- insertions variables and menus

(defvar gnuplot-mode-insertions-menu nil)
(defvar gnuplot-insertions-menu nil
  "Menu for insertions in `gnuplot-mode'.

The insertions menu is composed of several sub-menus.  The variables
describing the sub-menus are:
  `gnuplot-insertions-adornments'
  `gnuplot-insertions-plot-options'
  `gnuplot-insertions-terminal'
  `gnuplot-insertions-x-axis'
  `gnuplot-insertions-y-axis'
  `gnuplot-insertions-z-axis'
  `gnuplot-insertions-x2-axis'
  `gnuplot-insertions-y2-axis'
  `gnuplot-insertions-parametric-plots'
  `gnuplot-insertions-polar-plots'
  `gnuplot-insertions-surface-plots'
These variables can be customized by the user.  For example, there are
many terminal types which are not in the terminal submenu but which
may be compiled into your copy of gnuplot.

Each of these variables is a list whose first element is a string and
all the rest are vectors as described in the document string for
`easy-menu-define'.  The callback used throughout these menus is
`gnuplot-insert' which inserts the appropriate set expression and,
optionally, looks up that item in the gnuplot info file.

The easiest way to customize the submenus is to use the custom
package.  Just type \\[gnuplot-customize] and follow your nose.

You can also add new items to any of these sub-menus by adding to the
`with-eval-after-load' blocks in your .emacs file.  Here is an example of
adding the \"regis\" terminal type to the terminal sub-menu:

 (with-eval-after-load 'gnuplot
      (setq gnuplot-insertions-terminal
            (append gnuplot-insertions-terminal
                    (list
                     [\"regis\"
                      (gnuplot-insert \"set terminal regis\")
                       t]))))")

(defvar gnuplot-insertions-top ()
  "Top part of insertions menu.
See the document string for `gnuplot-insertions-menu'")

(defcustom gnuplot-insertions-menu-flag t
  "Non-nil means to place the insertion menu in the menubar.
Changing this will not effect a change in any currently existing
`gnuplot-mode' buffer.  You will see the change the next time you
create a `gnuplot-mode' buffer."
  :group 'gnuplot-insertions
  :type 'boolean)

(defcustom gnuplot-insertions-adornments ; this is icky...
  '("adornments"
    ["arrow"       (gnuplot-insert "set arrow ")          t]
    ["bar"         (gnuplot-insert "set bar")             t]
    ["border"      (gnuplot-insert "set border")          t]
    ["boxwidth"    (gnuplot-insert "set boxwidth ")       t]
    ["format"      (gnuplot-insert "set format ")         t]
    ["grid"        (gnuplot-insert "set grid")            t]
    ["key"         (gnuplot-insert "set key ")            t]
    ["label"       (gnuplot-insert "set label ")          t]
    ["pointsize"   (gnuplot-insert "set pointsize ")      t]
    ["samples"     (gnuplot-insert "set samples ")        t]
    ["size"        (gnuplot-insert "set size ")           t]
    ["style"       (gnuplot-insert "set style ")          t]
    ["tics"        (gnuplot-insert "set tics ")           t]
    ["timefmt"     (gnuplot-insert "set timefmt ")        t]
    ["timestamp"   (gnuplot-insert "set timestamp ")      t]
    ["title"       (gnuplot-insert "set title ")          t]
    ["zeroaxis"    (gnuplot-insert "set zeroaxis")        t])

  "Adornments submenu in the insertions menu.
See the document string for `gnuplot-insertions-menu'
Changing this will not effect a change in any currently existing
`gnuplot-mode' buffer.  You will see the change the next time you
create a `gnuplot-mode' buffer."
  :group 'gnuplot-insertions
  :type '(list (string :tag "Title")
               (repeat :inline t
                       (vector (string   :tag "Name")
                               (function :tag "Callback")
                               (boolean  :tag "Enabled" t)))))



(defcustom gnuplot-insertions-plot-options
  '("plot options"
    ["autoscale"  (gnuplot-insert "set autoscale ")          t]
    ["clip"       (gnuplot-insert "set clip ")               t]
    ["encoding"   (gnuplot-insert "set encoding ")           t]
    ["locale"     (gnuplot-insert "set locale ")             t]
    ["logscale"   (gnuplot-insert "set logscale ")           t]
    ["multiplot"  (gnuplot-insert "set multiplot")           t]
    ["missing"    (gnuplot-insert "set missing \"\"")        t]
    ["palette"    (gnuplot-insert "set palette ")            t]         ; <MT>
    ["pm3d"       (gnuplot-insert "set pm3d ")               t]
    ["offsets"    (gnuplot-insert "set offsets ")            t]
    ["output"     (gnuplot-insert "set output ")             t]
    ["zero"       (gnuplot-insert "set zero ")               t])
  "Plot options submenu in the insertions menu.
See the document string for `gnuplot-insertions-menu'
Changing this will not effect a change in any currently existing
`gnuplot-mode' buffer.  You will see the change the next time you
create a `gnuplot-mode' buffer."
  :group 'gnuplot-insertions
  :type '(list (string :tag "Title")
               (repeat :inline t
                       (vector (string   :tag "Name")
                               (function :tag "Callback")
                               (boolean  :tag "Enabled" t)))))


(defcustom gnuplot-insertions-terminal
  '("terminal"
    ["eepic"      (gnuplot-insert "set terminal eepic")      t]
    ["fig"        (gnuplot-insert "set terminal fig")        t]
    ["gpic"       (gnuplot-insert "set terminal gpic")       t]
    ["latex"      (gnuplot-insert "set terminal latex")      t]
    ["linux"      (gnuplot-insert "set terminal linux")      t]
    ["pbm"        (gnuplot-insert "set terminal pbm")        t]
    ["png"        (gnuplot-insert "set terminal png")        t]
    ["postscript" (gnuplot-insert "set terminal postscript") t]
    ["pslatex"    (gnuplot-insert "set terminal pslatex")    t]
    ["table"      (gnuplot-insert "set terminal table")      t]
    ["tek40xx"    (gnuplot-insert "set terminal tek40xx")    t]
    ["tkcanvas"   (gnuplot-insert "set terminal tkcanvas")   t]
    ["tpic"       (gnuplot-insert "set terminal tpic")       t]
    ["vgagl"      (gnuplot-insert "set terminal vgagl")      t]         ; for pm3d patch
    ["vttek"      (gnuplot-insert "set terminal vttek")      t]
    ["x11"        (gnuplot-insert "set terminal x11")        t])
  "Terminal submenu in the insertions menu.
See the document string for `gnuplot-insertions-menu'
Changing this will not effect a change in any currently existing
`gnuplot-mode' buffer.  You will see the change the next time you
create a `gnuplot-mode' buffer."
  :group 'gnuplot-insertions
  :type '(list (string :tag "Title")
               (repeat :inline t
                       (vector (string   :tag "Name")
                               (function :tag "Callback")
                               (boolean  :tag "Enabled" t)))))


(defcustom gnuplot-insertions-x-axis
  '("x-axis"
    ["xdata"      (gnuplot-insert "set xdata ")              t]
    ["xlabel"     (gnuplot-insert "set xlabel ")             t]
    ["xrange"     (gnuplot-insert "set xrange [:]")          t]
    ["xtics"      (gnuplot-insert "set xtics ")              t]
    ["mxtics"     (gnuplot-insert "set mxtics ")             t]
    ["xzeroaxis"  (gnuplot-insert "set xzeroaxis ")          t]
    ["xdtics"     (gnuplot-insert "set xdtics ")             t]
    ["xmtics"     (gnuplot-insert "set xmtics ")             t])
  "X-axis submenu in the insertions menu.
See the document string for `gnuplot-insertions-menu'
Changing this will not effect a change in any currently existing
`gnuplot-mode' buffer.  You will see the change the next time you
create a `gnuplot-mode' buffer."
  :group 'gnuplot-insertions
  :type '(list (string :tag "Title")
               (repeat :inline t
                       (vector (string   :tag "Name")
                               (function :tag "Callback")
                               (boolean  :tag "Enabled" t)))))


(defcustom gnuplot-insertions-x2-axis
  '("x2-axis"
    ["x2data"     (gnuplot-insert "set xdata ")              t]
    ["x2label"    (gnuplot-insert "set xlabel ")             t]
    ["x2range"    (gnuplot-insert "set xrange [:]")          t]
    ["x2tics"     (gnuplot-insert "set xtics ")              t]
    ["mx2tics"    (gnuplot-insert "set mxtics ")             t]
    ["x2zeroaxis" (gnuplot-insert "set xzeroaxis ")          t]
    ["x2dtics"    (gnuplot-insert "set xdtics ")             t]
    ["x2mtics"    (gnuplot-insert "set xmtics ")             t])
  "X2-axis submenu in the insertions menu.
See the document string for `gnuplot-insertions-menu'
Changing this will not effect a change in any currently existing
`gnuplot-mode' buffer.  You will see the change the next time you
create a `gnuplot-mode' buffer."
  :group 'gnuplot-insertions
  :type '(list (string :tag "Title")
               (repeat :inline t
                       (vector (string   :tag "Name")
                               (function :tag "Callback")
                               (boolean  :tag "Enabled" t)))))


(defcustom gnuplot-insertions-y-axis
  '("y-axis"
    ["ydata"      (gnuplot-insert "set ydata ")              t]
    ["ylabel"     (gnuplot-insert "set ylabel ")             t]
    ["ymtics"     (gnuplot-insert "set ymtics ")             t]
    ["yrange"     (gnuplot-insert "set yrange [:]")          t]
    ["ytics"      (gnuplot-insert "set ytics ")              t]
    ["yzeroaxis"  (gnuplot-insert "set yzeroaxis ")          t]
    ["ydtics"     (gnuplot-insert "set ydtics ")             t]
    ["mytics"     (gnuplot-insert "set mytics ")             t])
  "Y-axis submenu in the insertions menu.
See the document string for `gnuplot-insertions-menu'
Changing this will not effect a change in any currently existing
`gnuplot-mode' buffer.  You will see the change the next time you
create a `gnuplot-mode' buffer."
  :group 'gnuplot-insertions
  :type '(list (string :tag "Title")
               (repeat :inline t
                       (vector (string   :tag "Name")
                               (function :tag "Callback")
                               (boolean  :tag "Enabled" t)))))

(defcustom gnuplot-insertions-y2-axis
  '("y2-axis"
    ["y2data"     (gnuplot-insert "set ydata ")              t]
    ["y2label"    (gnuplot-insert "set ylabel ")             t]
    ["y2range"    (gnuplot-insert "set yrange [:]")          t]
    ["y2tics"     (gnuplot-insert "set ytics ")              t]
    ["my2tics"    (gnuplot-insert "set mytics ")             t]
    ["y2zeroaxis"  (gnuplot-insert "set yzeroaxis ")         t]
    ["y2mtics"    (gnuplot-insert "set ymtics ")             t]
    ["y2dtics"    (gnuplot-insert "set ydtics ")             t])
  "Y2-axis submenu in the insertions menu.
See the document string for `gnuplot-insertions-menu'
Changing this will not effect a change in any currently existing
`gnuplot-mode' buffer.  You will see the change the next time you
create a `gnuplot-mode' buffer."
  :group 'gnuplot-insertions
  :type '(list (string :tag "Title")
               (repeat :inline t
                       (vector (string   :tag "Name")
                               (function :tag "Callback")
                               (boolean  :tag "Enabled" t)))))



(defcustom gnuplot-insertions-z-axis
  '("z-axis"
    ["zdata"      (gnuplot-insert "set zdata ")              t]
    ["zlabel"     (gnuplot-insert "set zlabel ")             t]
    ["zrange"     (gnuplot-insert "set zrange [:]")          t]
    ["ztics"      (gnuplot-insert "set ztics ")              t]
    ["mztics"     (gnuplot-insert "set mztics ")             t]
    ["zdtics"     (gnuplot-insert "set zdtics ")             t]
    ["zmtics"     (gnuplot-insert "set zmtics ")             t])
  "Z-axis submenu in the insertions menu.
See the document string for `gnuplot-insertions-menu'
Changing this will not effect a change in any currently existing
`gnuplot-mode' buffer.  You will see the change the next time you
create a `gnuplot-mode' buffer."
  :group 'gnuplot-insertions
  :type '(list (string :tag "Title")
               (repeat :inline t
                       (vector (string   :tag "Name")
                               (function :tag "Callback")
                               (boolean  :tag "Enabled" t)))))


(defcustom gnuplot-insertions-parametric-plots
  '("parametric plots"
    ["parametric" (gnuplot-insert "set parametric")          t]
    ["isosamples" (gnuplot-insert "set isosamples ")         t]
    ["dummy"      (gnuplot-insert "set dummy ")              t]
    ["trange"     (gnuplot-insert "set trange [:]")          t]
    ["urange"     (gnuplot-insert "set urange [:]")          t]
    ["vrange"     (gnuplot-insert "set vrange [:]")          t])
  "Parametric plots submenu in the insertions menu.
See the document string for `gnuplot-insertions-menu'
Changing this will not effect a change in any currently existing
`gnuplot-mode' buffer.  You will see the change the next time you
create a `gnuplot-mode' buffer."
  :group 'gnuplot-insertions
  :type '(list (string :tag "Title")
               (repeat :inline t
                       (vector (string   :tag "Name")
                               (function :tag "Callback")
                               (boolean  :tag "Enabled" t)))))


(defcustom gnuplot-insertions-polar-plots
  '("polar plots"
    ["polar"      (gnuplot-insert "set polar")               t]
    ["angles"     (gnuplot-insert "set angles ")             t]
    ["rrange"     (gnuplot-insert "set rrange [:]")          t])
  "Polar plots submenu in the insertions menu.
See the document string for `gnuplot-insertions-menu'
Changing this will not effect a change in any currently existing
`gnuplot-mode' buffer.  You will see the change the next time you
create a `gnuplot-mode' buffer."
  :group 'gnuplot-insertions
  :type '(list (string :tag "Title")
               (repeat :inline t
                       (vector (string   :tag "Name")
                               (function :tag "Callback")
                               (boolean  :tag "Enabled" t)))))


(defcustom gnuplot-insertions-surface-plots
  '("surface plots"
    ["clabel"     (gnuplot-insert "set clabel ")             t]
    ["cntrparam"  (gnuplot-insert "set cntrparam ")          t]
    ["contour"    (gnuplot-insert "set contour")             t]
    ["dgrid3d"    (gnuplot-insert "set dgrid3d ")            t]
    ["hidden3d"   (gnuplot-insert "set hidden3d ")           t]
    ["mapping"    (gnuplot-insert "set mapping ")            t]
    ["surface"    (gnuplot-insert "set surface ")            t]
    ["view"       (gnuplot-insert "set view ")               t])
  "Surface plots submenu in the insertions menu.
See the document string for `gnuplot-insertions-menu'
Changing this will not effect a change in any currently existing
`gnuplot-mode' buffer.  You will see the change the next time you
create a `gnuplot-mode' buffer."
  :group 'gnuplot-insertions
  :type '(list (string :tag "Title")
               (repeat :inline t
                       (vector (string   :tag "Name")
                               (function :tag "Callback")
                               (boolean  :tag "Enabled" t)))))


(defvar gnuplot-insertions-bottom
  '("---"
    ["Display of info with insertion" gnuplot-toggle-info-display
     :style toggle :selected gnuplot-insertions-show-help-flag]
    ["Display GUI popup with insertion" gnuplot-gui-toggle-popup
     :active t
     :style toggle :selected gnuplot-gui-popup-flag])
  "Bottom part of the insertions menu.
This part contains the toggle buttons for displaying info or
opening an argument-setting popup.")

(defun gnuplot-setup-menubar ()
  "Initial setup of gnuplot and insertions menus."
  (if gnuplot-insertions-menu-flag      ; set up insertions menu
      (progn
        (setq gnuplot-insertions-top
              '("insert set expression" "---"))
        (setq gnuplot-insertions-menu
              (append (list "Insertions")
                      gnuplot-insertions-top
                      (list gnuplot-insertions-adornments)
                      (list gnuplot-insertions-plot-options)
                      (list gnuplot-insertions-terminal)
                      (list gnuplot-insertions-x-axis)
                      (list gnuplot-insertions-y-axis)
                      (list gnuplot-insertions-z-axis)
                      (list gnuplot-insertions-x2-axis)
                      (list gnuplot-insertions-y2-axis)
                      (list gnuplot-insertions-parametric-plots)
                      (list gnuplot-insertions-polar-plots)
                      (list gnuplot-insertions-surface-plots)
                      gnuplot-insertions-bottom))
        (easy-menu-define gnuplot-mode-insertions-menu gnuplot-mode-map
          "Insertions menu used in Gnuplot-mode"
          gnuplot-insertions-menu)))
  (easy-menu-define                     ; set up gnuplot menu
    gnuplot-mode-menu gnuplot-mode-map "Menu used in gnuplot-mode"
    gnuplot-menu))

(defun gnuplot-mark-active ()
  "Return non-nil if the mark is active and it is not equal to point."
  (condition-case nil
      (and (mark) (/= (mark) (point)))
    (error nil)))


;;; --- syntax colorization, syntax table

(defvar gnuplot-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?/ "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?& "." table )
    (modify-syntax-entry ?^ "." table )
    (modify-syntax-entry ?| "." table )
    (modify-syntax-entry ?& "." table )
    (modify-syntax-entry ?? "." table )
    (modify-syntax-entry ?~ "." table )
    (modify-syntax-entry ?_ "w" table )
    (modify-syntax-entry ?\" "." table)
    (modify-syntax-entry ?\' "." table)
    (modify-syntax-entry ?` "." table)
    (modify-syntax-entry ?\\ "." table)

    table)

  "Syntax table in use in `gnuplot-mode' buffers.
This is the same as the standard syntax table except that ` and _
are word characters, and math operators are punctuation
characters.")

;; Macro to generate efficient regexps for keyword matching
;;
;; These regular expressions treat the gnuplot vocabulary as complete
;; words.  Although gnuplot will recognise unique abbreviations, these
;; regular expressions will not.
(defmacro gnuplot-make-regexp (list)
  "Macro to generate efficient regexps for keyword matching from LIST."
  `(regexp-opt ,list 'words))

;; Lists of gnuplot keywords for syntax coloring etc.
(defvar gnuplot-keywords-builtin-functions
  '("abs" "acosh" "acos" "arg" "asinh" "asin" "atan" "atanh" "atan2" "besj1"
    "besj0" "besy1" "besy0" "ceil" "column" "cosh" "cos" "erfc" "erf" "exp"
    "floor" "gamma" "ibeta" "igamma" "imag" "int" "inverf" "invnorm" "lgamma"
    "log" "log10" "norm" "rand" "real" "sgn" "sinh" "sin" "sqrt" "tanh" "tan"
    "tm_hour" "tm_mday" "tm_min" "tm_mon" "tm_sec" "tm_wday" "tm_yday" "tm_year"
    "valid" "EllipticPi" "EllipticE" "EllipticK" "words" "word" "value"
    "timecolumn" "substr" "strstrt" "strptime" "strlen" "stringcolumn"
    "strftime" "sprintf" "lambertw" "gprintf" "exists" "defined" "columnhead")

  "List of GNUPLOT built-in functions, as strings.

These are highlighted using `font-lock-function-name-face'.")

(defvar gnuplot-keywords-plotting
  '("axes" "every" "index" "lw" "lt" "ls" "linestyle" "linetype" "linewidth"
    "notitle" "pt" "ps" "pointsize" "pointtype" "smooth" "thru" "title" "using"
    "with" "noautoscale" "volatile" "matrix" "nonuniform" "binary" "fillstyle"
    "linecolor" "pointinterval" "nosurface" "nocontours" "nohidden3d")
  "List of GNUPLOT keywords associated with plotting, as strings.

These are highlighted using `font-lock-type-face'.
This list does not include plotting styles -- for that, see
`gnuplot-keywords-plotting-styles'")

(defvar gnuplot-keywords-plotting-styles
  '("boxerrorbars" "boxes" "boxxyerrorbars" "candlesticks" "dots" "errorbars"
    "financebars" "fsteps" "histeps" "impulses" "lines" "linespoints" "points"
    "steps" "vector" "xerrorbars" "xyerrorbars" "yerrorbars" "vectors"
    "filledcurves" "labels" "rgbalpha" "rgbimage" "image" "circles" "pm3d"
    "histograms" "xyerrorlines" "xerrorlines" "errorlines" "yerrorlines")

  "List of GNUPLOT plotting styles, as strings.

These are highlighted using `font-lock-function-name-face'.")

(defvar gnuplot-keywords-misc
  '("bind" "cd" "clear" "exit" "fit" "help" "history" "load" "pause" "print"
    "pwd" "quit" "replot" "save" "set" "show" "unset" "if" "else" "do" "update"
    "undefine" "test" "system" "raise" "lower" "eval" "shell" "reset" "reread"
    "refresh" "call")
  "List of GNUPLOT miscellaneous commands, as strings.

These are highlighted using `font-lock-constant-face'.")

(defvar gnuplot-keywords-negatable-options
  '("arrow" "autoscale" "border" "clabel" "clip" "contour" "dgrid3d" "grid"
    "hidden3d" "historysize" "key" "label" "linestyle" "logscale" "mouse"
    "multiplot" "mx2tics" "mxtics" "my2tics" "mytics" "mztics" "offsets" "polar"
    "surface" "timestamp" "title" "x2dtics" "x2mtics" "x2tics" "x2zeroaxis"
    "xdtics" "xmtics" "xtics" "xzeroaxis" "y2dtics" "y2mtics" "y2tics"
    "y2zeroaxis" "ydtics" "ymtics" "ytics" "yzeroaxis" "zdtics" "zmtics" "ztics"
    "zzeroaxis")

  "List of gnuplot options which can be negated using `gnuplot-negate-option'.")

(defvar gnuplot-negatable-options-regexp
  (gnuplot-make-regexp gnuplot-keywords-negatable-options))

;; Set up colorization for gnuplot.
(defvar gnuplot-font-lock-keywords
  (list
   ;; stuff in brackets, sugg. by <LB>
   '("\\[\\([^]]+\\)\\]" 1 font-lock-constant-face)

   ;; variable/function definitions
   '("\\(\\(\\sw\\|\\s_\\)+\\s-*\\((\\s-*\\(\\sw\\|\\s_\\)*\\s-*\\(,\\s-*\\sw*\\)*\\s-*)\\)?\\s-*=\\)[^=]"
     1 font-lock-variable-name-face)

   ;; built-in function names
   (cons (gnuplot-make-regexp gnuplot-keywords-builtin-functions)
         font-lock-function-name-face)

   ;; reserved words associated with plotting <AL>
   (cons (gnuplot-make-regexp gnuplot-keywords-plotting)
         font-lock-type-face)
   (cons (gnuplot-make-regexp gnuplot-keywords-plotting-styles)
         font-lock-function-name-face)

   ;; (s)plot -- also thing (s)plotted
   '("\\<s?plot\\>" . font-lock-keyword-face)
   ;; '("\\<s?plot\\s-+\\([^'\" ]+\\)[) \n,\\\\]"
   ;;   1 font-lock-variable-name-face)

   ;; other common commands
   (cons (gnuplot-make-regexp gnuplot-keywords-misc)
         font-lock-constant-face)
   (cons "!.*$" font-lock-constant-face))) ; what is this for? jjo

(defvar gnuplot-font-lock-defaults
  '(gnuplot-font-lock-keywords
    nil                           ; Use syntactic fontification
    t                             ; Use case folding
    nil                           ; No extra syntax
    ;; calls `gnuplot-beginning-of-continuation'
    ;; to find a safe place to begin syntactic highlighting
    beginning-of-defun))

;; Some corner cases in Gnuplot's comment and string syntax are
;; difficult to handle accurately using Emacs's built-in syntax tables
;; and parser:
;;
;; - strings can continue over several lines, but only by using a
;;   backslash to escape the newline
;;
;; - double-quoted strings can contain escaped quotes, \", and escaped
;;   backslashes, \\; but in single-quoted strings the quote is
;;   escaped by doubling it, '', and backslash is only special at
;;   end-of-line
;;
;; - either type of string can end at newline without needing a
;; - closing delimiter
;;
;; - comments continue over continuation lines
;;
;; The following syntax-propertize rules should accurately mark string
;; and comment boundaries using the "generic string fence" and
;; "generic comment fence" syntax properties.
(defalias 'gnuplot-syntax-propertize
  (syntax-propertize-rules
   ;; Double quoted strings
   ((rx
     (group "\"")
     (* (or (seq "\\" anything)
            (not (any "\"" "\n"))))
     (group (or "\"" "\n" buffer-end)))
    (1 "|") (2 "|"))

   ;; Single quoted strings
   ((rx
     (group "'")
     (* (or (seq "\\" "\n")
            "''"
            (not (any "'" "\n"))))
     (group (or "'" "\n" buffer-end)))
    (1 "|") (2 "|"))

   ;; Comments
   ((rx
     (group "#")
     (* (or (seq "\\" "\n")
            any))
     (or (group "\n") buffer-end))
    (1 "!") (2 "!"))))

(defun gnuplot-syntax-propertize-extend-region (start end)
  "Expand the region to `syntax-propertize' for strings and comments.

Region range is START to END.
Ensures that the region being searched begins and ends outside of
any lines continued with a backslash.

This function is added to `syntax-propertize-extend-region-functions'
in `gnuplot-mode' buffers."
  (let ((continuation-start
         (min start
              (gnuplot-point-at-beginning-of-continuation start)))
        (continuation-end
         (max end
              (gnuplot-point-at-end-of-continuation end))))
    (if (and (= continuation-start start)
             (= continuation-end end))
        nil
      (cons continuation-start continuation-end))))

;; Parsing utilities to tell if we are inside a string or comment
(defun gnuplot-in-string (&optional where)
  "Return non-nil if the text at WHERE is within a string.

If WHERE is omitted, defaults to text at point.
This is a simple wrapper for `syntax-ppss'."
  (save-excursion
    (let ((parse-state (syntax-ppss where)))
      (nth 3 parse-state))))

(defun gnuplot-in-comment (&optional where)
  "Return non-nil if the text at WHERE is within a comment.

If WHERE is omitted, defaults to text at point.
This is a simple wrapper for `syntax-ppss'."
  (save-excursion
    (let ((parse-state (syntax-ppss where)))
      (nth 4 parse-state))))

(defun gnuplot-in-string-or-comment (&optional where)
  "Return non-nil if the text at WHERE is within a string or comment.

If WHERE is omitted, defaults to text at point.
This is a simple wrapper for `syntax-ppss'."

  (save-excursion
    (let ((parse-state (syntax-ppss where)))
      (or (nth 3 parse-state)
          (nth 4 parse-state)))))


;;; --- functions for sending commands to gnuplot

(defun gnuplot-split-string (string)
  "Break STRING at each carriage return, returning a list of lines."
  (let ((list ()) (line "") (index 0))
    (while (< index (length string))
      (if (char-equal (elt string index) ?\n)
          (setq list (append list (list line))
                line "")
        (setq line (concat line (char-to-string (elt string index)))))
      (setq index (1+ index)))
    list))

;; -- the calls to `sleep-for' are to allow enough time for gnuplot
;;    to write to the buffer before the next line is inserted
;; -- note that the input string is split into lines and each line is
;;    sent to gnuplot individually.  this is a bit slow, but it puts
;;    each line on the comint history.
(defun gnuplot-send-string-to-gnuplot (string text)
  "Sends STRING to the gnuplot program.
If no gnuplot process exists, a new one is created.  TEXT indicates
the type of text being sent to gnuplot and is typically one of
nil, `line', `region', `buffer', or `file'.  TEXT may be useful for
functions in `gnuplot-after-plot-hook'.  `gnuplot-after-plot-hook' is
called by this function after all of STRING is sent to gnuplot."
  (gnuplot-make-gnuplot-buffer)         ; make sure a gnuplot buffer exists
  (setq gnuplot-comint-recent-buffer (current-buffer))

  ;; Create a gnuplot frame if needed
  (if (equal gnuplot-display-process 'frame)
      (or (and gnuplot-process-frame
               (frame-live-p gnuplot-process-frame))
          (let ((frame (selected-frame)))
            (setq gnuplot-process-frame (make-frame))
            (select-frame gnuplot-process-frame)
            (switch-to-buffer gnuplot-buffer)
            (delete-other-windows)
            (select-frame frame))))

  (let ((list (gnuplot-split-string string)))
    (with-current-buffer (get-buffer gnuplot-buffer)
      (goto-char (point-max))
      ;; bruce asks: what is this next line for?
      (set-marker (process-mark gnuplot-process) (point-marker))
      (sleep-for (* 20 gnuplot-delay))
      (while list
        (insert (car list))
        (comint-send-input)
        (sleep-for gnuplot-delay)
        (setq list (cdr list))
        (goto-char (point-max))))

    (cond ((equal gnuplot-display-process 'window)
           (gnuplot-display-and-recenter-gnuplot-buffer))
          ((equal gnuplot-display-process 'frame)
           ;;(raise-frame gnuplot-process-frame)
           (with-selected-frame gnuplot-process-frame
             (gnuplot-display-and-recenter-gnuplot-buffer))))

    (setq gnuplot-recently-sent text)
    (run-hooks 'gnuplot-after-plot-hook)))

(defun gnuplot-display-and-recenter-gnuplot-buffer ()
  "Make sure the gnuplot comint buffer is displayed.
Move point to the end if necessary."
  (save-selected-window
    (select-window (display-buffer (get-buffer gnuplot-buffer)))
    (goto-char (point-max))
    (unless (pos-visible-in-window-p (point) (selected-window)) (recenter 5))))

(defun gnuplot-send-region-to-gnuplot (&optional begin end text)
  "Sends a selected region to the gnuplot program.
If BEGIN and END are not specified, point and mark are used.  TEXT
indicates the type of text being sent to gnuplot.  This will be
`region' unless explicitly set by a function calling this one.  Other
typical values are of nil, `line', `buffer', or `file'.  TEXT may be
useful for function in `gnuplot-after-plot-hook'."
  (interactive "r")
  (let (string (txt (or text 'region)))
    (cond ((equal major-mode 'gnuplot-mode)
           (setq string (buffer-substring-no-properties begin end))
           (if (string= (substring string -1) "\n") ()
             (setq string (concat string "\n")))
           (gnuplot-send-string-to-gnuplot string txt))
          (t
           (message (concat "You can only send regions from "
                            "gnuplot-mode buffers to gnuplot."))))))

(defun gnuplot-send-line-to-gnuplot ()
  "Sends the current line to the gnuplot program.
Respects continuation lines.
This sets `gnuplot-recently-sent' to `line'."
  (interactive)
  (cond ((equal major-mode 'gnuplot-mode)
         (let (start end)
           (save-excursion
             ;; go to start of continued command, or beginning of line
             ;; if this is not a continuation of a previous line <JJO>
             (gnuplot-beginning-of-continuation)
             (setq start (point))
             (end-of-line)
             (while (save-excursion
                      (backward-char)
                      (looking-at "\\\\"))      ; go to end of last continuation line
               (end-of-line 2))
             (beginning-of-line 2)
             (setq end (point)))
           (if (not (string-match "\\`\\s-*\\'"
                                  (buffer-substring-no-properties start end)))
               (gnuplot-send-region-to-gnuplot start end 'line))
           end))
        (t
         (message "You can only send lines in gnuplot-mode buffers to gnuplot.")
         nil)))

;; I chose a very easy to type but slightly non-mnemonic key-binding
;; for this (C-c C-v).  It seems like the kind of thing one would want
;; to do repeatedly without incurring RSI. 8^)
(defun gnuplot-send-line-and-forward (&optional num)
  "Call `gnuplot-send-line-to-gnuplot' and move forward 1 line.
You can use a numeric prefix to send more than one line.  Blank lines and
lines with only comments are skipped when moving forward.
NUM is optional arg."
  (interactive "p")
  (let (end)
    (while (> num 0)
      (setq end (gnuplot-send-line-to-gnuplot))
      (goto-char end)
      (backward-char 1)                         ; <AR>
      (gnuplot-forward-script-line 1)
      (setq num (1- num)))))

(defun gnuplot-send-line-and-newline ()
  "Call `gnuplot-send-line-to-gnuplot' and insert a new line."
  (interactive)
  (end-of-line)
  (gnuplot-send-line-to-gnuplot)
  (insert "\n"))

(defun gnuplot-forward-script-line (&optional num) ; <SE>
  "Move forward my NUM script lines.
Blank lines and commented lines are not included in the NUM count."
  (interactive "p")
  (while (> num 0)
    (and (not (eobp)) (forward-line 1))
    (while (and (not (eobp))
                (or (looking-at "^\\s-*$")
                    (looking-at "^\\s-*#")))
      (forward-line 1))
    (setq num (1- num))))

(defun gnuplot-send-buffer-to-gnuplot ()
  "Sends the entire buffer to the gnuplot program.
This sets `gnuplot-recently-sent' to `buffer'."
  (interactive)
  (if (equal major-mode 'gnuplot-mode)
      (gnuplot-send-region-to-gnuplot (point-min) (point-max) 'buffer)
    (message "You can only send gnuplot-mode buffers to gnuplot.")))

(defun gnuplot-send-file-to-gnuplot ()
  "Sends a selected file to the gnuplot program using the \"load\" command.
This sets `gnuplot-recently-sent' to `file'."
  (interactive)
  (let ((string (read-file-name "Name of file to send to gnuplot > " nil nil t)))
    (setq string (concat "load '" (expand-file-name string) "'\n"))
    (message "%S" string)
    (gnuplot-make-gnuplot-buffer)       ; make sure a gnuplot buffer exists
    (gnuplot-send-string-to-gnuplot string 'file)))

;; suggested by <JS>
(defun gnuplot-plot-from-comint ()
  "Send the contents of a script to gnuplot from the process buffer.
This inserts the contents of the most recently used gnuplot script
into the process buffer and sends those lines to gnuplot.  It does
this by copying the script line by line."
  (interactive)
  (if (not (buffer-live-p gnuplot-comint-recent-buffer))
      (message "Script buffer has been deleted.")
    (let (string list (buffer (current-buffer)))
      (set-buffer gnuplot-comint-recent-buffer)
      (setq string (buffer-substring-no-properties (point-min) (point-max))
            string (concat string "\n")
            list   (gnuplot-split-string string))
      (set-buffer buffer)
      (while list
        (insert (car list))
        (comint-send-input)
        (sleep-for gnuplot-delay)
        (setq list (cdr list)))
      (comint-send-input))))

(defun gnuplot-save-and-plot-from-comint ()
  "Send a current script to gnuplot from the process buffer.
This sends the most recently used gnuplot script to gnuplot using the
\"load\" command.  This function first saves the script buffer to a
file, prompting for a filename if one is not associated with the script
buffer.  Then it sends a load command to gnuplot using the name of the
file visited by the script buffer."
  (interactive)
  (if (not (buffer-live-p gnuplot-comint-recent-buffer))
      (message "Script buffer has been deleted.")
    (let (fname)
      (with-current-buffer gnuplot-comint-recent-buffer
        (save-buffer)
        (setq fname (buffer-file-name)))
      (goto-char (point-max))
      (insert (format "load '%s'" fname))
      (comint-send-input))))

(defun gnuplot-pop-to-recent-buffer ()
  "Switch to the most recently-plotted gnuplot script buffer."
  (interactive)
  (when (buffer-live-p gnuplot-comint-recent-buffer)
    (pop-to-buffer gnuplot-comint-recent-buffer)))

(defun gnuplot-trim-gnuplot-buffer ()
  "Trim lines from the beginning of the *gnuplot* buffer.
This keeps that buffer from growing excessively in size.  Normally,
this function is attached to `gnuplot-after-plot-hook'"
  (if (> gnuplot-buffer-max-size 0)
      (with-current-buffer gnuplot-buffer
        (let ((gnuplot-lines (count-lines (point-min) (point-max))))
          (dotimes (_n (- gnuplot-lines gnuplot-buffer-max-size))
            (goto-char (point-min))
            (delete-region (line-beginning-position) (1+ (line-end-position))))
          (goto-char (point-max))))))


;;; --- functions controlling the gnuplot process

;; Menu for the comint-mode buffer
(defvar gnuplot-comint-menu
  `("Gnuplot"
    ["Plot most recent gnuplot buffer"          gnuplot-plot-from-comint
     (buffer-live-p gnuplot-comint-recent-buffer)]
    ["Save and plot most recent gnuplot buffer"         gnuplot-save-and-plot-from-comint
     (buffer-live-p gnuplot-comint-recent-buffer)]
    "---"
    ,gnuplot-display-options-menu
    ["Contextual completion and help"           gnuplot-context-sensitive-mode
     :style toggle
     :selected gnuplot-context-sensitive-mode]
    ["Echo area help (eldoc-mode)" eldoc-mode
     :active gnuplot-context-sensitive-mode
     :style toggle
     :selected eldoc-mode]
    "---"
    ["Insert filename at point"                         gnuplot-insert-filename t]
    ["Negate set option"                        gnuplot-negate-option t]
    ["Keyword help"                             gnuplot-info-lookup-symbol
     (or gnuplot-keywords gnuplot-keywords-pending)]
    ["Quick help for thing at point"            gnuplot-help-function
     gnuplot-context-sensitive-mode]
    ["Info documentation on thing at point"
     gnuplot-info-at-point
     gnuplot-context-sensitive-mode]
    ["Switch to recent gnuplot script buffer"   gnuplot-pop-to-recent-buffer
     (buffer-live-p gnuplot-comint-recent-buffer)]
    "---"
    ["Customize gnuplot"                        gnuplot-customize t]
    "---"
    ["Kill gnuplot"                             gnuplot-kill-gnuplot-buffer t]))

;; Major mode `gnuplot-comint-mode' for the interaction buffer
(define-derived-mode gnuplot-comint-mode comint-mode "Gnuplot interaction"
  "Major mode for interacting with a gnuplot process in a buffer.

This sets font-lock and keyword completion in the comint/gnuplot
buffer."

  (set-syntax-table gnuplot-mode-syntax-table)

  (setq font-lock-defaults gnuplot-font-lock-defaults)
  (setq-local parse-sexp-lookup-properties t)
  (setq-local syntax-propertize-function #'gnuplot-syntax-propertize)

  (add-hook 'kill-buffer-hook #'gnuplot-close-down nil t)

  (add-hook 'comint-output-filter-functions
            #'comint-postoutput-scroll-to-bottom
            nil t)
  (add-hook 'comint-output-filter-functions
            #'gnuplot-protect-prompt-fn
            nil t)

  ;; Set up completion, using completion-at-point
  (add-hook 'completion-at-point-functions #'gnuplot-completion-at-point nil t)


  ;; Set up menu (see below)
  (easy-menu-define
    gnuplot-comint-mode-menu gnuplot-comint-mode-map "Menu used in gnuplot-comint-mode"
    gnuplot-comint-menu))

;; Key bindings for gnuplot-comint-mode
(define-key gnuplot-comint-mode-map "\M-\C-p"   #'gnuplot-plot-from-comint)
(define-key gnuplot-comint-mode-map "\M-\C-f"   #'gnuplot-save-and-plot-from-comint)
(define-key gnuplot-comint-mode-map "\C-d"      #'gnuplot-delchar-or-maybe-eof)
(define-key gnuplot-comint-mode-map "\M-\r"     #'completion-at-point)
(define-key gnuplot-comint-mode-map "\M-\t"     #'completion-at-point)
(define-key gnuplot-comint-mode-map "\C-c\C-d"  #'gnuplot-info-lookup-symbol)
(define-key gnuplot-comint-mode-map "\C-c\C-i"  #'gnuplot-insert-filename)
(define-key gnuplot-comint-mode-map "\C-c\C-n"  #'gnuplot-negate-option)
(define-key gnuplot-comint-mode-map "\C-c\C-z"  #'gnuplot-customize)
(define-key gnuplot-comint-mode-map "\C-c\C-e"  #'gnuplot-pop-to-recent-buffer)

;; Menu for gnuplot-comint-mode
(defvar gnuplot-comint-mode-menu nil
  "Menu for `gnuplot-comint-mode'.")

;; Switch to the gnuplot program buffer
(defun gnuplot-make-gnuplot-buffer ()
  "Switch to the gnuplot program buffer or create one if none exists."
  (unless (and gnuplot-process (eq (process-status gnuplot-process) 'run)
               gnuplot-buffer (buffer-live-p gnuplot-buffer))
    (message "Starting gnuplot plotting program...")
    (let ((gnuplot-cmd (list #'make-comint gnuplot-process-name gnuplot-program)))
      (when gnuplot-program-args
        (setq gnuplot-cmd (append gnuplot-cmd '(nil) (split-string gnuplot-program-args))))
      (setq gnuplot-buffer  (eval gnuplot-cmd)
            gnuplot-process (get-buffer-process gnuplot-buffer)))
    (set-process-query-on-exit-flag gnuplot-process nil)
    (with-current-buffer gnuplot-buffer
      (gnuplot-comint-mode)
      (when gnuplot-inline-image-mode
        (sleep-for gnuplot-delay)
        (gnuplot-setup-comint-for-image-mode)))
    (message "Starting gnuplot plotting program...Done")))

(defvar gnuplot-prompt-regexp
  (regexp-opt '("gnuplot> " "multiplot> "))
  "Regexp for recognizing the GNUPLOT prompt.")

(defun gnuplot-protect-prompt-fn (_string)
  "Prevent the Gnuplot prompt from being deleted or overwritten.
STRING is the text as originally inserted in the comint buffer."
  (save-excursion
    (let ((b (progn
               (goto-char (point-max))
               (beginning-of-line)
               (point)))
          e)
      (if (re-search-forward gnuplot-prompt-regexp (point-max) t)
          (progn
            (setq e (point))
            (put-text-property b e 'rear-nonsticky '(read-only intangible face))
            (put-text-property b e 'intangible t)
            (put-text-property b e 'face 'gnuplot-prompt-face)
            ;;(put-text-property b e 'read-only t)
            )))))

(defun gnuplot-close-down ()
  "Tidy up when deleting the gnuplot buffer."
  (if (and gnuplot-process
           (eq (process-status gnuplot-process) 'run)) ; <SE>
      (kill-process gnuplot-process))
  (setq gnuplot-process nil
        gnuplot-buffer nil))

(defun gnuplot-delchar-or-maybe-eof (arg)
  "Delete ARG characters forward, or (if at eob) send an EOF to subprocess.
This is very similar to `comint-delchar-or-maybe-eof'."
  (interactive "p")
  (if (eobp)
      (gnuplot-kill-gnuplot-buffer)
    (delete-char arg)))

(defun gnuplot-kill-gnuplot-buffer ()
  "Kill the gnuplot process and its display buffers."
  (interactive)
  (if (and gnuplot-process
           (eq (process-status gnuplot-process) 'run))  ;; <SE>
      (kill-process gnuplot-process))
  (if (and gnuplot-buffer (get-buffer gnuplot-buffer))
      (progn
        (if (one-window-p) ()
          (delete-window (get-buffer-window gnuplot-buffer)))
        (kill-buffer gnuplot-buffer)))
  (setq gnuplot-process nil
        gnuplot-buffer nil))


(defun gnuplot-show-gnuplot-buffer ()
  "Switch to the buffer containing the gnuplot process.
When `gnuplot-display-process' is nil this will switch to
the gnuplot process buffer.  When that variable is non-nil, the
gnuplot process buffer will be displayed in a window."
  (interactive)
  (unless (and gnuplot-buffer (get-buffer gnuplot-buffer))
    (gnuplot-make-gnuplot-buffer))
  (cond ((equal gnuplot-display-process 'window)
         (switch-to-buffer-other-window gnuplot-buffer))
        ((equal gnuplot-display-process 'frame)
         (or (and gnuplot-process-frame
                  (frame-live-p gnuplot-process-frame))
             (setq gnuplot-process-frame (make-frame)))
         (raise-frame gnuplot-process-frame)
         (select-frame gnuplot-process-frame)
         (switch-to-buffer gnuplot-buffer))
        (t
         (switch-to-buffer gnuplot-buffer))))


;;; Support for displaying plotted images within Emacs

(defvar gnuplot-inline-image-filename nil
  "Name of the current Gnuplot output file.")

(defvar gnuplot-image-buffer-name "*gnuplot output*")

(defun gnuplot-external-display-mode ()
  "Display image in external."
  (interactive)
  (gnuplot-set-display-mode 'gnuplot-inline-image-mode nil))

(defun gnuplot-inline-display-mode ()
  "Display image in inline."
  (interactive)
  (gnuplot-set-display-mode 'gnuplot-inline-image-mode 'inline))

(defun gnuplot-dedicated-display-mode ()
  "Display image in dedicated."
  (interactive)
  (gnuplot-set-display-mode 'gnuplot-inline-image-mode 'dedicated))

(defun gnuplot-set-image-format (format)
  "Display image in FORMAT."
  (interactive "sGnuplot image format: ")
  (gnuplot-set-display-mode 'gnuplot-image-format format)
  (unless gnuplot-inline-image-mode
    (message "Setting will take effect when plots are displayed in Emacs")))

(defun gnuplot-setup-comint-for-image-mode ()
  "Setup comint for image."
  (when (and gnuplot-buffer (buffer-live-p gnuplot-buffer)
             (get-buffer-process gnuplot-buffer))
    (with-current-buffer gnuplot-buffer
      (if gnuplot-inline-image-mode
          (progn
            (gnuplot-send-hiding-output
             (format "set terminal %s\n" gnuplot-image-format))
            (gnuplot-inline-image-set-output)
            (add-hook 'comint-output-filter-functions
                      #'gnuplot-insert-inline-image-output nil t))
        (gnuplot-send-hiding-output "set terminal pop\n")
        (remove-hook 'comint-output-filter-functions
                     #'gnuplot-insert-inline-image-output t)))))

(defun gnuplot-inline-image-set-output ()
  "Set Gnuplot's output file to `gnuplot-inline-image-filename'."
  (let ((tmp (make-temp-file "gnuplot")))
    (setq gnuplot-inline-image-filename tmp)
    (gnuplot-send-hiding-output (format "set output '%s'\n" tmp))))

(defvar gnuplot--inhibit-filter nil)

(defun gnuplot-insert-inline-image-output (_string)
  "Insert Gnuplot graphical output STRING in the gnuplot-comint buffer.

Called via `comint-preoutput-filter-functions' hook when
`gnuplot-inline-image-mode' is enabled.  Checks the status of the
file `gnuplot-inline-image-filename'; if it exists and has
nonzero size, inserts it as an inline image, stores a new
temporary filename in `gnuplot-inline-image-filename', and
updates Gnuplot with the appropriate \"set output\" command."
  (unless gnuplot--inhibit-filter        ; Prevent recursively entering this filter
    (let ((gnuplot--inhibit-filter t))   ; (causing an infinite loop)
      (save-excursion
        (goto-char (point-max))
        (beginning-of-line)
        (when (looking-at gnuplot-prompt-regexp)
          (let* ((filename gnuplot-inline-image-filename)
                 (size (nth 7 (file-attributes filename))))
            (when (and size (> size 0))
              (gnuplot-send-hiding-output "set output\n") ; Flush output file
              (sit-for 0.1)             ; Hack: wait for Gnuplot IO to finish
              (cl-ecase gnuplot-inline-image-mode
                (nil nil)
                (inline
                  (ignore-errors
                    (let ((image (create-image filename)))
                      (beginning-of-line)
                      (insert-image image)
                      (insert "\n")
                      (gnuplot-inline-image-set-output))))
                (dedicated
                 (with-current-buffer
                     (get-buffer-create gnuplot-image-buffer-name)
                   (let ((inhibit-read-only t))
                     (erase-buffer)
                     (insert-file-contents filename)
                     (ignore-errors (normal-mode))
                     (display-buffer (current-buffer))
                     (gnuplot-inline-image-set-output))))))))))))

;;; Send commands to GNUPLOT silently & without generating an extra prompt
(defvar gnuplot-hidden-output-buffer " *gnuplot output*")

(defun gnuplot-send-hiding-output (string)
  "Send STRING to the running Gnuplot process invisibly."
  (with-current-buffer gnuplot-buffer
    (add-hook 'comint-preoutput-filter-functions
              #'gnuplot-discard-output nil t))
  (with-current-buffer (get-buffer-create gnuplot-hidden-output-buffer)
    (erase-buffer))
  (comint-send-string (get-buffer-process gnuplot-buffer) string))

(defun gnuplot-discard-output (string)
  "Temporary preoutput filter for hiding Gnuplot output & prompt.
Accumulates output STRING in a buffer until it finds the next prompt,
then removes itself from `comint-preoutput-filter-functions'."
  (with-current-buffer
      (get-buffer-create gnuplot-hidden-output-buffer)
    (insert string)
    (when (looking-back gnuplot-prompt-regexp (point-min))
      (with-current-buffer gnuplot-buffer
        (remove-hook 'comint-preoutput-filter-functions
                     'gnuplot-discard-output t))))
  "")



;;; --- miscellaneous functions: insert file name, indentation, negation

(defun gnuplot-insert-filename ()
  "Insert a filename at point, prompting for name in the minibuffer.
This inserts a filename relative to the buffer's default directory.
Uses completion and the value of `gnuplot-quote-character'.
Bound to \\[gnuplot-insert-filename]"
  (interactive)
  (insert gnuplot-quote-character
          (file-relative-name (read-file-name "Filename > " "")
                              default-directory)
          gnuplot-quote-character))


;; Adjust indentation for the line containing point
(defun gnuplot-indent-line ()
  "Set indentation in gnuplot buffer.
For most lines, set indentation to previous level of indentation.
Add additional indentation for continuation lines."
  (interactive)
  (let (indent)
    (if (gnuplot-in-string (line-beginning-position))
        ;; Continued strings begin at left margin
        (setq indent 0)
      (save-excursion
        (if (gnuplot-continuation-line-p)
            ;; This is a continuation line. Indent to the same level as
            ;; the second word on the line beginning this command (i.e.,
            ;; the first non-whitespace character after whitespace)
            (progn
              (gnuplot-beginning-of-continuation)
              (back-to-indentation)
              (re-search-forward "\\S-+\\s-+" (line-end-position) 'end-at-limit)
              (setq indent (current-column)))

          ;; Not a continuation line; indent according to block
          ;; nesting depth
          (save-excursion
            (condition-case nil
                (progn
                  (beginning-of-line)
                  (skip-syntax-forward "-" (line-end-position))
                  (if (looking-at "\\s)") (forward-char))
                  (backward-up-list)
                  (gnuplot-beginning-of-continuation)
                  (setq indent (+ gnuplot-basic-offset (current-indentation))))
              (error
               (setq indent 0)))))))

    ;; Set indentation
    (save-excursion
      (indent-line-to indent))

    ;; Move point after indentation when at beginning of line
    (when (< (current-column) indent)
      (move-to-column indent))))

(defun gnuplot-electric-insert (BRACE)
  "Adjust indentation on inserting a close BRACE.
The blink-paren fix is stolen from cc-mode"
  (interactive "*p")
  (let ((old-blink-paren blink-paren-function)
        (blink-paren-function nil))
    (self-insert-command BRACE)
    (gnuplot-indent-line)
    (when old-blink-paren (funcall old-blink-paren))))

;;
;; Functions for finding the start and end of continuation blocks
;;

;; Check if line containing point is a continuation
(defun gnuplot-continuation-line-p ()
  "Return t if the line containing point is a continuation of the previous line."
  (save-excursion
    (condition-case ()
        (progn
          (end-of-line 0)
          (backward-char)
          (looking-at "\\\\"))
      (error nil))))

;; Move point to start of continuation block
(defun gnuplot-beginning-of-continuation ()
  "Move point to the beginning of the continuation lines containing point.

If not in a continuation line, move point to beginning of line."
  (beginning-of-line)
  (while (gnuplot-continuation-line-p)
    (beginning-of-line 0)))

;; Move point to end of continuation block
(defun gnuplot-end-of-continuation ()
  "Move point to the end of the continuation lines containing point.

If there are no continuation lines, move point to `end-of-line'."
  (end-of-line)
  (unless (bobp)
    (catch 'eob
      (while (save-excursion (backward-char)
                             (looking-at "\\\\"))
        (end-of-line 2)
        (if (eobp) (throw 'eob nil))))))

;; Save-excursion wrappers for the above to return point at beginning
;; or end of continuation
(defun gnuplot-point-at-beginning-of-continuation (&optional pos)
  "Return value of point at beginning of the continued block containing point.

If there are no continuation lines, returns `line-beginning-position'.
If specify POS, move POS befere execution."
  (save-excursion
    (when pos (goto-char pos))
    (gnuplot-beginning-of-continuation)
    (point)))

(defun gnuplot-point-at-end-of-continuation (&optional pos)
  "Return value of point at the end of the continued block containing point.

If there are no continuation lines, returns `line-end-position'.
If specify POS, move POS before execution."
  (save-excursion
    (when pos (goto-char pos))
    (gnuplot-end-of-continuation)
    (point)))

(defun gnuplot-beginning-of-defun (&optional arg)
  "We also treat a block of continuation lines as a `defun'.
ARG is optional arg."
  (if (not arg) (setq arg 1))
  (if (> arg 0)
      (catch 'bob               ; go to beginning of ARGth prev. defun
        (dotimes (_n arg)
          (when (= (point)
                   (gnuplot-point-at-beginning-of-continuation))
            (forward-line -1)
            (if (bobp) (throw 'bob t))
            (while (looking-at "^\\s-*$")
              (forward-line -1)
              (if (bobp) (throw 'bob t))))
          (gnuplot-beginning-of-continuation))
        t)

    (catch 'eob                   ; find beginning of (-ARG)th following defun
      (dotimes (_n (- arg))
        (gnuplot-end-of-continuation)
        (forward-line)
        (if (eobp) (throw 'eob t))
        (while (looking-at "^\\s-*$")
          (forward-line)
          (if (eobp) (throw 'eob t)))))))

;; Movement to start or end of command, including multiple commands
;; separated by semicolons
(defun gnuplot-beginning-of-command ()
  "Move point to beginning of command containing point."
  (let ((limit (gnuplot-point-at-beginning-of-continuation)))
    (while
        (and
         (search-backward ";" limit 'lim)
         (gnuplot-in-string-or-comment)))
    (skip-chars-forward ";")
    (skip-syntax-forward "-")))

(defun gnuplot-end-of-command ()
  "Move point to end of command containing point."
  (let ((limit (gnuplot-point-at-end-of-continuation)))
    (while
        (and
         (search-forward ";" limit 'lim)
         (gnuplot-in-string-or-comment)))
    (skip-chars-backward ";")
    (skip-syntax-backward "-")))

(defun gnuplot-point-at-beginning-of-command ()
  "Return position at the beginning of command containing point."
  (save-excursion (gnuplot-beginning-of-command) (point)))

(defun gnuplot-point-at-end-of-command ()
  "Return position at the end of command containing point."
  (save-excursion (gnuplot-end-of-command) (point)))

(defun gnuplot-negate-option ()
  "Append \"no\" to or remove \"no\" from the set option on the current line.
This checks if the set option is one which has a negated form.

Negatable options are defined in `gnuplot-keywords-negatable-options'."
  (interactive)
  (let ((begin (gnuplot-point-at-beginning-of-command))
        (end   (gnuplot-point-at-end-of-command))
        (regex gnuplot-negatable-options-regexp))
    (save-excursion
      (goto-char begin)
      (skip-syntax-forward "-" end)
      (if (looking-at "\\(un\\)?set\\s-+")
          (cond ((looking-at "unset")
                 (delete-char 2))
                ((looking-at (concat "set\\s-+\\(" regex "\\)"))
                 (insert "un"))
                (t
                 (message "There is not a negatable set option on this line")))
        (message "There is not a set option on this line")))))


(defun gnuplot-customize ()
  "Customize `gnuplot-mode'."
  (interactive)
  (customize-group "gnuplot"))



;;; --- help from the info file, keyword list + completion, insert function


;; set up stuff for info-look (as suggested by <SE>)
;; modified with suggestion from <MS>
(defun gnuplot-setup-info-look ()
  "Setup info-look in the gnuplot buffer.

Also set the variable `gnuplot-keywords' and do something sensible if
info-look was not available."
  (interactive)
  (setq gnuplot-keywords-pending nil)
  ;; TODO Update info layout
  (let ((doc-spec
         '(("(gnuplot)Command_Index"   nil "[_a-zA-Z0-9]+")
           ("(gnuplot)Options_Index"   nil "[_a-zA-Z0-9]+")
           ("(gnuplot)Function_Index"  nil "[_a-zA-Z0-9]+")
           ("(gnuplot)Terminal_Index"  nil "[_a-zA-Z0-9]+"))))
    (info-lookup-add-help
     :mode 'gnuplot-mode :topic 'symbol
     :regexp "[a-zA-Z][_a-zA-Z0-9]*"
     :doc-spec doc-spec)
    (info-lookup-add-help
     :mode 'gnuplot-comint-mode :topic 'symbol
     :regexp "[a-zA-Z][_a-zA-Z0-9]*"
     :doc-spec doc-spec))

  (let ((there (bufferp (get-buffer "*info*"))))
    (info-lookup-setup-mode 'symbol 'gnuplot-mode)
    (or there (and (get-buffer "*info*") (kill-buffer "*info*")))
    ;; why are these buffers here?  I think that the general
    ;; user will not want them lying around
    (and (get-buffer "info dir")    (kill-buffer "info dir"))
    (and (get-buffer "info dir<2>") (kill-buffer "info dir<2>")))
  (setq gnuplot-keywords (gnuplot-set-keywords-list)))

(defun gnuplot-set-keywords-list ()
  "Set `gnuplot-keywords' from `info-lookup-cache'.
Return a list of keywords."
  (let* ((list (cdr (assoc 'symbol info-lookup-cache)))
         (list (cdr (cdr (assoc 'gnuplot-mode list))))
         (list (car list))
         (store ()) item)
    (while list
      (setq item (car (car list))
            item (format "%s" item) ; keep this line for the sake of
            store (append (list item) store) ; info-look.el w/o my patch
            list  (cdr list)))
    (delete "nil" store)
    store ))


;;;; Completion at point and Eldoc.

;; There are two alternative completion-at-point mechanisms: the old
;; one using info-look and the new one (enabled by default) which
;; parses the command line to provide smarter completions.

;; `gnuplot-completion-at-point-function' defines which one is
;; used. `gnuplot-context-sensitive-mode' toggles between the two.

(defvar gnuplot-completion-at-point-function #'gnuplot-completion-at-point-info-look
  "Function to call to perform completion in Gnuplot buffers.")

(defun gnuplot-completion-at-point ()
  "Perform completion in Gnuplot buffers."
  (funcall gnuplot-completion-at-point-function))

;; Older completion method using info-look
(defun gnuplot-completion-at-point-info-look ()
  "Return completions of keyword preceding point.

Uses the cache of keywords generated by `info-lookup'.  See
`gnuplot-setup-info-look'.  If non-nil, the return value is in the form
\(BEGIN END COMPLETIONS) where BEGIN and END are buffer
positions and COMPLETIONS is a list."

  (if gnuplot-keywords-pending          ; <HW>
      (gnuplot-setup-info-look))
  (list (condition-case _err
            (save-excursion (backward-sexp 1) (point))
          (error (point)))
        (point) gnuplot-keywords))


(defun gnuplot-info-lookup-symbol (symbol &optional mode)
  "Wrapper for `info-lookup-symbol'.
Takes SYMBOL and MODE as arguments exactly as
`info-lookup-symbol'.  After doing the info lookup, calls
`gnuplot--adjust-info-display' to display the info buffer
according to the value of `gnuplot-info-display'."
  (interactive
   (cond (gnuplot-keywords
          (info-lookup-interactive-arguments 'symbol))
         (gnuplot-keywords-pending      ; <HW>
          (gnuplot-setup-info-look)
          (info-lookup-interactive-arguments 'symbol))
         (t
          (list nil (message
                     "Help is not available.  The gnuplot info file could not be found.")))))

  (when gnuplot-keywords
    (unless symbol (setq symbol "Commands"))
    (save-window-excursion
      (info-lookup-symbol symbol mode))
    (gnuplot--adjust-info-display)))

(defun gnuplot--adjust-info-display ()
  "Displays the *info* buffer in a window or frame.
Specified by the value of `gnuplot-info-display'.
If `gnuplot-info-display' is `window', then the window will be
shrunk to the size of the info entry if it is smaller than half
the height of the frame.

The *info* buffer should already exist when this function is
called."
  (cl-case gnuplot-info-display
    (window
     (switch-to-buffer-other-window "*info*")
     ;; Adjust window height only if the frame is split
     ;; horizontally, so as not to mess up the minibuffer <jjo>
     ;; we can't use shrink-window-if-larger-than-buffer here
     ;; because it doesn't work with Info mode's narrowing
     (with-selected-window (get-buffer-window "*info*")
       (unless (window-full-height-p)
         (enlarge-window
          (min (- (count-lines (point-min) (point-max)) (window-height) -1)
               (- (/ (frame-height) 2) (window-height)))))))

    (frame
     (unless (and gnuplot-info-frame
                  (frame-live-p gnuplot-info-frame))
       (setq gnuplot-info-frame (make-frame)))
     (select-frame gnuplot-info-frame)
     (raise-frame gnuplot-info-frame)
     (switch-to-buffer "*info*"))

    (t
     (switch-to-buffer "*info*"))))

(defun gnuplot-insert (string)
  "Insert STRING at point and display help for for STRING.
Help is not shown if `gnuplot-insertions-show-help-flag' is nil.  The
help shown is for STRING unless STRING begins with the word \"set\" or
\"show\", in which case help is shown for the thing being set or
shown."
  (interactive)
  (insert string)
  (let ((topic string) term)
    (if (string-match
         "\\(set\\|show\\)[ \t]+\\([^ \t]+\\)\\(\\s-+\\([^ \t]+\\)\\)?"
         string)
        (progn
          (setq topic (downcase (match-string 2 string))
                term            (match-string 4 string))
          (if (string= topic "terminal") (setq topic (downcase term)))))
    (cond ((and (bound-and-true-p gnuplot-gui-popup-flag)
                (fboundp 'gnuplot-gui-set-options-and-insert))
           (gnuplot-gui-set-options-and-insert))
          (gnuplot-insertions-show-help-flag
           (if gnuplot-keywords-pending          ; <HW>
               (gnuplot-setup-info-look))
           (gnuplot-info-lookup-symbol topic)))))

(defun gnuplot-toggle-info-display ()
  "Toggle info display."
  (interactive)
  (setq gnuplot-insertions-show-help-flag (not gnuplot-insertions-show-help-flag))
  (message (if gnuplot-insertions-show-help-flag
               "Help will be displayed after insertions."
             "Help no longer displayed after insertions.")))


;;; --- autoloaded functions: gnuplot-mode and gnuplot-make-buffer

;;;###autoload
(defun gnuplot-mode ()
  "Major mode for editing and executing GNUPLOT scripts.
This was written with version 4.6 of gnuplot in mind, but should
work with newer and older versions.

Report bugs at https://github.com/emacs-gnuplot/gnuplot/issues

                            ------O------

Gnuplot-mode includes two different systems for keyword
completion and documentation lookup: a newer one,
`gnuplot-context-sensitive-mode' (enabled by default), and a
older one which extracts keywords from gnuplot's Info file.  Both
systems allow looking up documentation in the Info file.  The
older system also depends having the info file properly installed
to make a list of keywords.

The info file should be installed by default with the Gnuplot
distribution, or is available at the `gnuplot-mode' web page:
https://github.com/emacs-gnuplot/gnuplot/

With the new context-sensitive mode active, `gnuplot-mode' can also
provide function/`eldoc-mode' syntax hints as you type.  This requires a
separate file of strings, `gnuplot-eldoc.el', which is also
provided by recent Gnuplot distributions.

                            ------O------

There are several known shortcomings of `gnuplot-mode', version 0.5g
and up.  Many of the shortcomings involve the graphical interface
\(refered to as the GUI) to setting arguments to plot options.  Here is
a list:

 1.  Currently there is no way for `gnuplot-mode' to know if information
     sent to gnuplot was correctly plotted.
 2.  \"plot\", \"splot\", and \"fit\" are handled in the GUI, but are
     a bit flaky.  Their arguments may not be read correctly from
     existing text, and continuation lines (common for plot and splot)
     are not supported.
 3.  The GUI does not know how to read from continuation lines.
 4.  Comma separated position arguments to plot options are
     unsupported in the GUI.  Colon separated datafile modifiers (used
     for plot, splot, and fit) are not supported either.  Arguments
     not yet supported by the GUI generate messages printed in grey
     text.
 5.  The GUI handling of \"hidden3d\" is flaky and \"cntrparam\" is
     unsupported.

                            ------O------

 Key bindings:
 \\{gnuplot-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map gnuplot-mode-map)
  (setq major-mode 'gnuplot-mode
        mode-name "Gnuplot")
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-column 32)
  (setq-local comment-start-skip "#[ \t]*")
  (setq-local indent-line-function #'gnuplot-indent-line)

  (setq-local beginning-of-defun-function #'gnuplot-beginning-of-defun)
  (setq-local end-of-defun-function #'gnuplot-end-of-continuation)

  (add-hook 'completion-at-point-functions #'gnuplot-completion-at-point nil t)

  (set-syntax-table gnuplot-mode-syntax-table)

  (when (eq gnuplot-keywords-when 'immediately) ; <HW>
    (gnuplot-setup-info-look)) ;; <SE>

  ;; Add syntax-propertizing functions to search for strings and comments
  (setq-local syntax-propertize-function #'gnuplot-syntax-propertize)
  (add-hook 'syntax-propertize-extend-region-functions
            #'gnuplot-syntax-propertize-extend-region nil t)

  ;; Set up font-lock
  (setq font-lock-defaults gnuplot-font-lock-defaults)
  (setq-local font-lock-multiline t)
  (setq-local parse-sexp-lookup-properties t)

  (setq gnuplot-comint-recent-buffer (current-buffer))
  (setq-local comint-process-echoes gnuplot-echo-command-line-flag)
  (run-hooks 'gnuplot-mode-hook)
  (gnuplot-setup-menubar))

;;;###autoload
(defun gnuplot-make-buffer ()
  "Open a new buffer in `gnuplot-mode'.
When invoked, it switches to a new, empty buffer visiting no file
and then starts `gnuplot-mode'.

It is convenient to bind this function to a global key sequence.  For
example, to make the F10 key open a gnuplot script buffer, put the
following in your .emacs file:
     (autoload 'gnuplot-make-buffer \"gnuplot\"
               \"open a buffer in gnuplot mode\" t)
     (global-set-key [(f10)] 'gnuplot-make-buffer)"
  (interactive)
  (switch-to-buffer gnuplot-gnuplot-buffer)
  (gnuplot-mode))

;;;###autoload
(defun run-gnuplot ()
  "Run an inferior Gnuplot process."
  (interactive)
  (gnuplot-make-gnuplot-buffer)
  (pop-to-buffer gnuplot-buffer))

;;; That's it! ----------------------------------------------------------------

(provide 'gnuplot)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; gnuplot.el ends here
