(require 'dash)

(ert-deftest powerline-define-mode-line-segment ()
  (spacemacs|define-mode-line-segment string "string")
  (spacemacs|define-mode-line-segment number 4)
  (spacemacs|define-mode-line-segment list '("text" 9))
  (spacemacs|define-mode-line-segment variable some-var)
  (spacemacs|define-mode-line-segment func (some-func))
  (spacemacs|define-mode-line-segment image '(image data))
  (defun some-func () "" "hello there")
  (let ((some-var 'symbol))
    (should (equal '("string") (spacemacs//mode-line-string)))
    (should (equal '(4) (spacemacs//mode-line-number)))
    (should (equal '("text" 9) (spacemacs//mode-line-list)))
    (should (equal '(symbol) (spacemacs//mode-line-variable)))
    (should (equal '("hello there") (spacemacs//mode-line-func)))
    (should (equal '((image data)) (spacemacs//mode-line-image))))
  (dolist (s '(string number list variable func image))
    (fmakunbound (intern (format "spacemacs//mode-line-%s" s))))
  (fmakunbound 'some-func))

(ert-deftest powerline-define-mode-line-segment-when ()
  (spacemacs|define-mode-line-segment always "always" :when t)
  (spacemacs|define-mode-line-segment never "never" :when nil)
  (spacemacs|define-mode-line-segment maybe "maybe" :when condition)
  (should (equal '("always") (spacemacs//mode-line-always)))
  (should (equal nil (spacemacs//mode-line-never)))
  (let ((condition t))
    (should (equal '("maybe") (spacemacs//mode-line-maybe))))
  (let ((condition nil))
    (should (equal nil (spacemacs//mode-line-maybe))))
  (dolist (s '(always never maybe))
    (fmakunbound (intern (format "spacemacs//mode-line-%s" s)))))

;; Replace with `equal-including-properties' once Emacs bug #6581 is fixed
;; The function `ert-equal-including-properties' is good but only works for strings
;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=6581
(defun equal-segments (a b)
  (and (equal a b)
       (-all-p 'identity
               (--zip-with (if (stringp it)
                               (ert-equal-including-properties it other)
                             (equal it other))
                           (segment-objects a)
                           (segment-objects b)))))

(ert-deftest powerline-eval-mode-line-segment-face ()
  (spacemacs|define-mode-line-segment face1 (powerline-raw "1" 'some-face))
  (spacemacs|define-mode-line-segment face2 "2" :face some-face-var)
  (spacemacs|define-mode-line-segment face3 "3")
  (let ((default-face 'd)
        (some-face-var 's))
    (should (equal-segments
             [cl-struct-segment (#("yoyo" 0 4 (face (d)))) d d nil nil]
             (spacemacs//eval-mode-line-segment "yoyo")))
    (should (equal-segments
             [cl-struct-segment (#("1" 0 1 (face (some-face d)))) d d nil nil]
             (spacemacs//eval-mode-line-segment 'face1)))
    (should (equal-segments
             [cl-struct-segment (#("2" 0 1 (face (s)))) s s nil nil]
             (spacemacs//eval-mode-line-segment 'face2)))
    (should (equal-segments
             [cl-struct-segment (#("3" 0 1 (face (d)))) d d nil nil]
             (spacemacs//eval-mode-line-segment 'face3 :face 'default-face)))
    (should (equal-segments
             [cl-struct-segment (#("3" 0 1 (face (variable-pitch))))
                                variable-pitch variable-pitch nil nil]
             (spacemacs//eval-mode-line-segment 'face3 :face ''variable-pitch)))
    (should (equal-segments
             [cl-struct-segment (#("2" 0 1 (face (s)))) s s nil nil]
             (spacemacs//eval-mode-line-segment 'face2 :face 'blergh)))
    (should (equal-segments
             [cl-struct-segment (#("3" 0 1 (face (variable-pitch))))
                                variable-pitch variable-pitch nil nil]
             (spacemacs//eval-mode-line-segment '(face3 :face 'variable-pitch)))))
  (dolist (s '(face1 face2 face3))
    (fmakunbound (intern (format "spacemacs//mode-line-%s" s)))))

(ert-deftest powerline-eval-mode-line-segment-tight ()
  (spacemacs|define-mode-line-segment tight "tight" :tight t)
  (spacemacs|define-mode-line-segment tight-l "left" :tight-left t)
  (spacemacs|define-mode-line-segment tight-r "right" :tight-right t)
  (spacemacs|define-mode-line-segment none "none" :tight-left nil :tight-right nil)
  (let ((default-face 'd))
    (should (equal-segments
             [cl-struct-segment (#("tight" 0 5 (face (d)))) d d t t]
             (spacemacs//eval-mode-line-segment 'tight)))
    (should (equal-segments
             [cl-struct-segment (#("left" 0 4 (face (d)))) d d t nil]
             (spacemacs//eval-mode-line-segment 'tight-l)))
    (should (equal-segments
             [cl-struct-segment (#("right" 0 5 (face (d)))) d d nil t]
             (spacemacs//eval-mode-line-segment 'tight-r :tight nil)))
    (should (equal-segments
             [cl-struct-segment (#("none" 0 4 (face (d)))) d d t t]
             (spacemacs//eval-mode-line-segment '(none :tight t)))))
  (dolist (s '(tight tight-l tight-r none))
    (fmakunbound (intern (format "spacemacs//mode-line-%s" s)))))

(ert-deftest powerline-eval-mode-line-segment-when-fallback ()
  (spacemacs|define-mode-line-segment always "always" :when t)
  (spacemacs|define-mode-line-segment never "never" :when nil)
  (spacemacs|define-mode-line-segment maybe "maybe" :when condit)
  (let ((default-face 'd))
    (should (equal-segments
             [cl-struct-segment nil d d nil nil]
             (spacemacs//eval-mode-line-segment '(never :when t) :when t)))
    (should (equal-segments
             [cl-struct-segment (#("always" 0 6 (face (d)))) d d nil nil]
             (spacemacs//eval-mode-line-segment 'never :fallback 'always)))
    (should (equal-segments
             [cl-struct-segment nil d d nil nil]
             (spacemacs//eval-mode-line-segment 'never :fallback 'never)))
    (should (equal-segments
             [cl-struct-segment (#("always" 0 6 (face (d)))) d d nil nil]
             (spacemacs//eval-mode-line-segment 'always :fallback "something else")))
    (let ((condit nil))
      (should (equal-segments
               [cl-struct-segment (#("always" 0 6 (face (d)))) d d nil nil]
               (spacemacs//eval-mode-line-segment 'maybe :fallback '(never :fallback always)))))
    (let ((condit t))
      (should (equal-segments
               [cl-struct-segment (#("maybe" 0 5 (face (d)))) d d nil nil]
               (spacemacs//eval-mode-line-segment 'never :fallback 'maybe)))))
  (dolist (s '(never))
    (fmakunbound (intern (format "spacemacs//mode-line-%s" s)))))

(ert-deftest powerline-eval-mode-line-separator ()
  (let ((default-face 'd))
    (should (equal-segments
             [cl-struct-segment (#("a" 0 1 (face (d)))
                                 #(" " 0 1 (face (d)))
                                 #("b" 0 1 (face (d)))
                                 #(" " 0 1 (face (d)))
                                 #("c" 0 1 (face (d))))
                                d d nil nil]
             (spacemacs//eval-mode-line-segment '("a" "b" "c"))))
    (should (equal-segments
             [cl-struct-segment (#("a" 0 1 (face (d)))
                                 #("~" 0 1 (face (d)))
                                 #("b" 0 1 (face (d)))
                                 #("~" 0 1 (face (d)))
                                 #("c" 0 1 (face (d))))
                                d d nil nil]
             (spacemacs//eval-mode-line-segment '(("a" "b" "c") :separator "~"))))
    (should (equal-segments
             [cl-struct-segment (#("a" 0 1 (face (d)))
                                 #("~" 0 1 (face (d)))
                                 #("b" 0 1 (face (d)))
                                 #("|" 0 1 (face (d)))
                                 #("c" 0 1 (face (d))))
                                d d nil nil]
             (spacemacs//eval-mode-line-segment
              '(((("a" "b") :separator "~") "c") :separator "|"))))))
