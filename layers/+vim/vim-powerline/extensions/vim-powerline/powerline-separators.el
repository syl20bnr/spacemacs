;;; powerline-separators.el --- Separators for Powerline

;; Copyright (C) 2012-2013 Donald Ephraim Curtis
;; Copyright (C) 2013 Jason Milkins
;; Copyright (C) 2012 Nicolas Rougier

;; Author: Donald Ephraim Curtis <dcurtis@milkbox.net>
;; URL: http://github.com/milkypostman/powerline/
;; Version: 2.0
;; Keywords: mode-line

;;; Commentary:
;;
;; Separators for Powerline.
;; Included separators: alternate, arrow, arrow-fade, bar, box, brace, butt,
;; chamfer, contour, curve, rounded, roundstub, slant, wave, zigzag, and nil.
;;

;;; Code:

(require 'cl-lib)
(require 'color)

(defun pl/interpolate (color1 color2)
  "Interpolate between COLOR1 and COLOR2.

COLOR1 and COLOR2 must be supplied as hex strings with a leading #."
  (let* ((c1 (color-name-to-rgb color1))
         (c2 (color-name-to-rgb color2))
         (red (/ (+ (nth 0 c1) (nth 0 c2)) 2))
         (green (/ (+ (nth 1 c1) (nth 1 c2)) 2))
         (blue (/ (+ (nth 2 c1) (nth 2 c2)) 2)))
    (color-rgb-to-hex red green blue)))

(defun pl/hex-color (color)
  "Get the hexadecimal value of COLOR."
  (when color
    (apply 'color-rgb-to-hex (color-name-to-rgb color))))

(defun pl/pattern (lst)
  "Turn LST into an infinite pattern."
  (when lst
    (let ((pattern (cl-copy-list lst)))
      (setcdr (last pattern) pattern))))

(defun pl/pattern-to-string (pattern)
  "Convert a PATTERN into a string that can be used in an XPM."
  (concat "\"" (mapconcat 'number-to-string pattern "") "\","))

(defun pl/reverse-pattern (pattern)
  "Reverse each line in PATTERN."
  (mapcar 'reverse pattern))

(defun pl/row-pattern (fill total &optional fade)
  "Make a list that has FILL 0s out of TOTAL 1s with FADE 2s to the right of the fill."
  (unless fade
    (setq fade 0))
  (let ((fill (min fill total))
        (fade (min fade (max (- total fill) 0))))
    (append (make-list fill 0)
            (make-list fade 2)
            (make-list (- total fill fade) 1))))


(defun pl/pattern-defun (name dir width &rest patterns)
  "Create a powerline function of NAME in DIR with WIDTH for PATTERNS.

PATTERNS is of the form (PATTERN HEADER FOOTER SECOND-PATTERN CENTER).
PATTERN is required, all other components are optional.

All generated functions generate the form:
HEADER
PATTERN ...
CENTER
SECOND-PATTERN ...
FOOTER

PATTERN and SECOND-PATTERN repeat infinitely to fill the space needed to generate a full height XPM.

PATTERN, HEADER, FOOTER, SECOND-PATTERN, CENTER are of the form ((COLOR ...) (COLOR ...) ...).

COLOR can be one of 0, 1, or 2, where 0 is the source color, 1 is the
destination color, and 2 is the interpolated color between 0 and 1."
  (when (eq dir 'right)
    (setq patterns (mapcar 'pl/reverse-pattern patterns)))
  (let* ((pattern (pl/pattern (mapcar 'pl/pattern-to-string (car patterns))))
         (header (mapcar 'pl/pattern-to-string (nth 1 patterns)))
         (footer (mapcar 'pl/pattern-to-string (nth 2 patterns)))
         (second-pattern (pl/pattern (mapcar 'pl/pattern-to-string (nth 3 patterns))))
         (center (mapcar 'pl/pattern-to-string (nth 4 patterns)))
         (reserve (+ (length header) (length footer) (length center))))
    (pl/wrap-defun name dir width
                   `((pattern-height (max (- height ,reserve) 0))
                     (second-pattern-height (/ pattern-height 2))
                     (pattern-height ,(if second-pattern '(ceiling pattern-height 2) 'pattern-height)))
                   `((mapconcat 'identity ',header "")
                     (mapconcat 'identity (cl-subseq ',pattern 0 pattern-height) "")
                     (mapconcat 'identity ',center "")
                     (mapconcat 'identity (cl-subseq ',second-pattern 0 second-pattern-height) "")
                     (mapconcat 'identity ',footer "")))))

(defun pl/wrap-defun (name dir width let-vars body)
  "Generate a powerline function of NAME in DIR with WIDTH using LET-VARS and BODY."
  (let* ((src-face (if (eq dir 'left) 'face1 'face2))
         (dst-face (if (eq dir 'left) 'face2 'face1)))
    `(defun ,(intern (format "powerline-%s-%s" name (symbol-name dir)))
       (face1 face2 &optional height)
       (when window-system
         (unless height (setq height (pl/separator-height)))
         (let* ,(append `((color1 (when ,src-face
                                    (pl/hex-color (face-background ,src-face))))
                          (color2 (when ,dst-face
                                    (pl/hex-color (face-background ,dst-face))))
                          (colori (when (and color1 color2) (pl/interpolate color1 color2)))
                          (color1 (or color1 "None"))
                          (color2 (or color2 "None"))
                          (colori (or colori "None")))
                        let-vars)
           (create-image ,(append `(concat (format "/* XPM */ static char * %s_%s[] = { \"%s %s 3 1\", \"0 c %s\", \"1 c %s\", \"2 c %s\","
                                                   ,(replace-regexp-in-string "-" "_" name)
                                                   (symbol-name ',dir)
                                                   ,width
                                                   height
                                                   color1
                                                   color2
                                                   colori))
                                  body
                                  '("};"))
                         'xpm t
                         :ascent 'center
                         :face (when (and face1 face2)
                                 ,dst-face)))))))


(defmacro pl/alternate (dir)
  "Generate an alternating pattern XPM function for DIR."
  (pl/pattern-defun "alternate" dir 4
                    '((2 2 1 1)
                      (0 0 2 2))))

(defmacro pl/arrow (dir)
  "Generate an arrow XPM function for DIR."
  (let ((row-modifier (if (eq dir 'left) 'identity 'reverse)))
    (pl/wrap-defun "arrow" dir 'middle-width
                   '((width (1- (/ height 2)))
                     (middle-width (1- (ceiling height 2))))
                   `((cl-loop for i from 0 to width
                              concat (pl/pattern-to-string (,row-modifier (pl/row-pattern i middle-width))))
                     (when (cl-oddp height)
                       (pl/pattern-to-string (make-list middle-width 0)))
                     (cl-loop for i from width downto 0
                              concat (pl/pattern-to-string (,row-modifier (pl/row-pattern i middle-width))))))))

(defmacro pl/arrow-fade (dir)
  "Generate an arrow-fade XPM function for DIR."
  (let* ((row-modifier (if (eq dir 'left) 'identity 'reverse)))
    (pl/wrap-defun "arrow-fade" dir 'middle-width
                   '((width (1- (/ height 2)))
                     (middle-width (1+ (ceiling height 2))))
                   `((cl-loop for i from 0 to width
                              concat (pl/pattern-to-string (,row-modifier (pl/row-pattern i middle-width 2))))
                     (when (cl-oddp height)
                       (pl/pattern-to-string (,row-modifier (pl/row-pattern (1+ width) middle-width 2))))
                     (cl-loop for i from width downto 0
                              concat (pl/pattern-to-string (,row-modifier (pl/row-pattern i middle-width 2))))))))

(defmacro pl/bar (dir)
  "Generate a bar XPM function for DIR."
  (pl/pattern-defun "bar" dir 2
                    '((2 2))))

(defmacro pl/box (dir)
  "Generate a box XPM function for DIR."
  (pl/pattern-defun "box" dir 2
                    '((0 0)
                      (0 0)
                      (1 1)
                      (1 1))))

(defmacro pl/brace (dir)
  "Generate a brace XPM function for DIR."
  (pl/pattern-defun "brace" dir 4
                    '((0 1 1 1))
                    '((1 1 1 1)
                      (2 1 1 1))
                    '((2 1 1 1)
                      (1 1 1 1))
                    '((0 1 1 1))
                    '((0 2 1 1)
                      (0 2 1 1)
                      (0 0 2 1)
                      (0 0 0 0)
                      (0 0 2 1)
                      (0 2 1 1)
                      (0 2 1 1))))

(defmacro pl/butt (dir)
  "Generate a butt XPM function for DIR."
  (pl/pattern-defun "butt" dir 3
                    '((0 0 0))
                    '((1 1 1)
                      (0 1 1)
                      (0 0 1))
                    '((0 0 1)
                      (0 1 1)
                      (1 1 1))))

(defmacro pl/chamfer (dir)
  "Generate a chamfer XPM function for DIR."
  (pl/pattern-defun "chamfer" dir 3
                    '((0 0 0))
                    '((1 1 1)
                      (0 1 1)
                      (0 0 1))))

(defmacro pl/contour (dir)
  "Generate a contour XPM function for DIR."
  (pl/pattern-defun "contour" dir 10
                    '((0 0 0 0 0 1 1 1 1 1))
                    '((1 1 1 1 1 1 1 1 1 1)
                      (0 2 1 1 1 1 1 1 1 1)
                      (0 0 2 1 1 1 1 1 1 1)
                      (0 0 0 2 1 1 1 1 1 1)
                      (0 0 0 0 1 1 1 1 1 1)
                      (0 0 0 0 2 1 1 1 1 1))
                    '((0 0 0 0 0 2 1 1 1 1)
                      (0 0 0 0 0 0 1 1 1 1)
                      (0 0 0 0 0 0 2 1 1 1)
                      (0 0 0 0 0 0 0 2 1 1)
                      (0 0 0 0 0 0 0 0 0 0))))

(defmacro pl/curve (dir)
  "Generate a curve XPM function for DIR."
  (pl/pattern-defun "curve" dir 4
                    '((0 0 0 0))
                    '((1 1 1 1)
                      (2 1 1 1)
                      (0 0 1 1)
                      (0 0 2 1)
                      (0 0 0 1)
                      (0 0 0 2))
                    '((0 0 0 2)
                      (0 0 0 1)
                      (0 0 2 1)
                      (0 0 1 1)
                      (2 1 1 1)
                      (1 1 1 1))))

(defmacro pl/rounded (dir)
  "Generate a rounded XPM function for DIR."
  (pl/pattern-defun "rounded" dir 6
                    '((0 0 0 0 0 0))
                    '((2 1 1 1 1 1)
                      (0 0 2 1 1 1)
                      (0 0 0 0 1 1)
                      (0 0 0 0 2 1)
                      (0 0 0 0 0 1)
                      (0 0 0 0 0 2))))

(defmacro pl/roundstub (dir)
  "Generate a roundstub XPM function for DIR."
  (pl/pattern-defun "roundstub" dir 3
                    '((0 0 0))
                    '((1 1 1)
                      (0 0 1)
                      (0 0 2))
                    '((0 0 2)
                      (0 0 1)
                      (1 1 1))))

(defmacro pl/slant (dir)
  "Generate a slant XPM function for DIR."
  (let* ((row-modifier (if (eq dir 'left) 'identity 'reverse)))
    (pl/wrap-defun "slant" dir 'width
                   '((width (1- (ceiling height 2))))
                   `((cl-loop for i from 0 to height
                              concat (pl/pattern-to-string (,row-modifier (pl/row-pattern (/ i 2) width))))))))

(defmacro pl/wave (dir)
  "Generate a wave XPM function for DIR."
  (pl/pattern-defun "wave" dir 11
                    '((0 0 0 0 0 0 1 1 1 1 1))
                    '((2 1 1 1 1 1 1 1 1 1 1)
                      (0 0 1 1 1 1 1 1 1 1 1)
                      (0 0 0 1 1 1 1 1 1 1 1)
                      (0 0 0 2 1 1 1 1 1 1 1)
                      (0 0 0 0 1 1 1 1 1 1 1)
                      (0 0 0 0 2 1 1 1 1 1 1)
                      (0 0 0 0 0 1 1 1 1 1 1)
                      (0 0 0 0 0 1 1 1 1 1 1)
                      (0 0 0 0 0 2 1 1 1 1 1))
                    '((0 0 0 0 0 0 2 1 1 1 1)
                      (0 0 0 0 0 0 0 1 1 1 1)
                      (0 0 0 0 0 0 0 1 1 1 1)
                      (0 0 0 0 0 0 0 2 1 1 1)
                      (0 0 0 0 0 0 0 0 1 1 1)
                      (0 0 0 0 0 0 0 0 2 1 1)
                      (0 0 0 0 0 0 0 0 0 0 2))))

(defmacro pl/zigzag (dir)
  "Generate a zigzag pattern XPM function for DIR."
  (pl/pattern-defun "zigzag" dir 3
                    '((1 1 1)
                      (0 1 1)
                      (0 0 1)
                      (0 0 0)
                      (0 0 1)
                      (0 1 1))))

(defmacro pl/nil (dir)
  "Generate a XPM function that returns nil for DIR."
  `(defun ,(intern (format "powerline-nil-%s" (symbol-name dir)))
     (face1 face2 &optional height)
     nil))

(defmacro pl/utf-8 (dir)
  "Generate function that returns raw utf-8 symbols."
  (let ((dir-name (symbol-name dir))
	(src-face (if (eq dir 'left) 'face1 'face2))
	(dst-face (if (eq dir 'left) 'face2 'face1)))
    `(defun ,(intern (format "powerline-utf-8-%s" dir-name))
       (face1 face2 &optional height)
       (powerline-raw
	(char-to-string ,(intern (format "powerline-utf-8-separator-%s"
					 dir-name)))
	(list :foreground (face-attribute ,src-face :background)
	      :background (face-attribute ,dst-face :background)
              :underline nil
              :box (face-attribute ,dst-face :background))))))


(provide 'powerline-separators)

;;; powerline-separators.el ends here
