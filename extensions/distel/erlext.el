;;; erlext.el --- Encoding and decoding of Erlang external term format

;; Copyleft (]) 2000-2002 Luke Gorrie <luke@bluetail.com>
;; Version: $Id: erlext.el,v 1.1 2004/10/25 19:55:57 lukeg Exp $
;; Keywords: erlang

;;; Commentary:
;;
;; Library for encoding/decoding elisp terms into erlang's external
;; term format.  For format details see erts/emulator/internal_doc/ in
;; the Erlang/OTP sources.
;;
;; Supported mappings from/to erlext to elisp:
;;   atom    -> symbol
;;   string  -> string
;;   integer -> integer
;;   list    -> list
;;   tuple   -> (vector ...)
;;   pid     -> (vector ERL-TAG 'pid node id serial creation)
;;   binary  -> string
;; Not mapped/supported yet:
;;   ref, port, float, bignum, function, ...
;;
;; ----------------------------------------------------------------------
;; Revision history:
;;
;; Originally written some time in 2000, borrowing lots of code that I
;; didn't understand from Lennart Staflin's nice elisp CORBA client.
;;
;; May 2001: Added asynchronous networking support for the "shbuf"
;; program that shares emacs buffers on the network via an erlang
;; server.
;;
;; March 2002: Big cleanup for use in distributed erlang. Removed the
;; old networking code.

(eval-when-compile (require 'cl))
(eval-when-compile (load "cl-extra"))

;; type tags

(defconst erlext-tag-alist
  '((smallInt	. 97)
    (int	. 98)
    (float	. 99)
    (atom	. 100)
    (cached	. 67)
    (ref	. 101)
    (port	. 102)
    (pid	. 103)
    (smallTuple . 104)
    (largeTuple . 105)
    (null	. 106)
    (string	. 107)
    (list	. 108)
    (bin	. 109)
    (smallBig	. 110)
    (largeBig	. 111)
    (newRef	. 114)))

(defconst erlext-max-atom-length 255 "The maximum length of an erlang atom.")
(defconst erlext-protocol-version 131)

(defconst empty-symbol (intern "")
  "The zero-length lisp symbol.")

(defvar erl-tag (make-symbol "TYPE")
  "Tag placed in the first element of a vector to indicate a non-tuple type.")

;; ------------------------------------------------------------
;; Encoding / decoding interface
;; ------------------------------------------------------------

(defun erlext-binary-to-term (string)
  "Decode and return the elisp representation of `string'."
  (assert (stringp string))
  (let (default-enable-multibyte-characters)
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (erlext-read-whole-obj))))

(defun erlext-term-to-binary (term)
  "Encode `term' as erlext and return the result as a string."
  (let (default-enable-multibyte-characters)
    (with-temp-buffer
      (insert erlext-protocol-version)
      (erlext-write-obj term)
      (buffer-string))))

;; Tuple datatype: (tuple X Y Z) => [X Y Z]

(defun tuple (&rest elems)
  (apply #'vector elems))

(defun tuple-to-list (x)
  (assert (tuplep x))
  (mapcar #'identity x))

(defun tuplep (x)
  (and (vectorp x)
       (or (zerop (length x))
	   (not (eq (elt x 0) erl-tag)))))

(defun tuple-arity (tuple)
  (1- (length tuple)))

(defmacro tuple-elt (tuple index)
  "Return element INDEX from TUPLE. Index starts from 1."
  ;; Defined as a macro just so that we get the setf of `elt' for free
  `(elt ,tuple (1- ,index)))

;; ------------------------------------------------------------
;; Encoding
;; ------------------------------------------------------------

(defun erlext-write-obj (obj)
  (cond ((listp obj)                    ; lists at top since (symbolp '()) => t
	 (erlext-write-list obj))
	((stringp obj)
	 (erlext-write-string obj))
	((symbolp obj)
	 (erlext-write-atom obj))
	((vectorp obj)
	 (if (tuplep obj)
	     (erlext-write-tuple (tuple-to-list obj))
	   (let* ((list (mapcar #'identity obj))
		  (type (cadr list))
		  (elts (cddr list)))
	     (ecase type
	       ((erl-pid)
		(apply #'erlext-write-pid elts))
	       ((erl-port)
		(apply #'erlext-write-port elts))
	       ((erl-ref)
		(apply #'erlext-write-ref elts))
	       ((erl-new-ref)
		(apply #'erlext-write-new-ref elts))
	       ((erl-binary)
		(erlext-write-binary (car elts)))))))
	((integerp obj)
	 (erlext-write-int obj))
	(t
	 (error "erlext can't marshal %S" obj))))

(defun erlext-write1 (n)
  (assert (integerp n))
  (insert n))
(defun erlext-write2 (n)
  (assert (integerp n))
  (insert (logand (ash n -8) 255)
	  (logand n 255)))
(defun erlext-write4 (n)
  (assert (integerp n))
  (insert (logand (ash n -24) 255)
	  (logand (ash n -16) 255)
	  (logand (ash n -8) 255)
	  (logand n 255)))
(defun erlext-writen (bytes)
  (assert (stringp bytes))
  (insert bytes))
(defun erlext-insert4 (n offset)
  (goto-char offset)
  (erlext-write4 n)
  (goto-char (point-max)))

(defun erlext-write-atom (atom)
  (assert (symbolp atom))
  (let* ((string (symbol-name atom))
	 (len    (length string)))
    (assert (<= len erlext-max-atom-length))
    (erlext-write1 (erlext-get-code 'atom))
    (erlext-write2 (length string))
    (erlext-writen string)))
(defun erlext-write-int (n)
  (assert (integerp n))
  (cond ((= n (logand n 255))
	 (erlext-write1 (erlext-get-code 'smallInt))
	 (erlext-write1 n))
	;; elisp has small numbers so 32bit on the wire is as far as
	;; we need bother supporting
	(t
	 (erlext-write1 (erlext-get-code 'int))
	 (erlext-write4 n))))
(defun erlext-write-list (lst)
  (assert (listp lst))
  (if (null lst)
      (erlext-write-null)
    (progn (erlext-write-list-head (length lst))
	   (mapc 'erlext-write-obj lst)
	   (erlext-write-null))))
(defun erlext-write-string (str)
  (assert (stringp str))
  (erlext-write1 (erlext-get-code 'string))
  (erlext-write2 (length str))
  (erlext-writen str))
(defun erlext-write-binary (str)
  (assert (stringp str))
  (erlext-write1 (erlext-get-code 'bin))
  (erlext-write4 (length str))
  (erlext-writen str))
(defun erlext-write-null ()
  (erlext-write1 (erlext-get-code 'null)))
(defun erlext-write-list-head (arity)
  (assert (> arity 0))
  (erlext-write1 (erlext-get-code 'list))
  (erlext-write4 arity))
(defun erlext-write-tuple (elts)
  (assert (listp elts))
  (let ((arity (length elts)))
    (if (< arity 256)
	(progn (erlext-write1 (erlext-get-code 'smallTuple))
	       (erlext-write1 arity))
      (progn (erlext-write1 (erlext-get-code 'largeTuple))
	     (erlext-write4 arity))))
  (mapc 'erlext-write-obj elts))
(defun erlext-write-pid (node id serial creation)
  (erlext-write1 (erlext-get-code 'pid))
  (erlext-write-obj node)
  (erlext-write4 id)
  (erlext-write4 serial)
  (erlext-write1 creation))
(defun erlext-write-port (node id creation)
  (erlext-write1 (erlext-get-code 'port))
  (erlext-write-obj node)
  (erlext-write4 id)
  (erlext-write1 creation))
(defun erlext-write-ref (node id creation)
  (erlext-write1 (erlext-get-code 'ref))
  (erlext-write-obj node)
  (erlext-write4 id)
  (erlext-write1 creation))
(defun erlext-write-new-ref (node creation id)
  (erlext-write1 (erlext-get-code 'newRef))
  (erlext-write2 (/ (length id) 4))
  (erlext-write-obj node)
  (erlext-write1 creation)
  (erlext-writen id))

;; ------------------------------------------------------------
;; Decoding
;; ------------------------------------------------------------

(eval-and-compile
  (if (fboundp 'char-int)
      ;; convert character to string
      (defsubst erlext-read1 ()
	(prog1 (char-int (following-char))
	  (forward-char 1)))
    (defsubst erlext-read1 ()
      (prog1 (following-char)
	(forward-char 1)))))

(defun erlext-read-whole-obj ()
  (let ((version (erlext-read1)))
    (assert (= version erlext-protocol-version))
    (erlext-read-obj)))

(defun erlext-read-obj ()
  (let ((tag (erlext-get-tag (erlext-read1))))
    (case tag
      ((smallInt)   (erlext-read1))
      ((int)        (erlext-read4))
      ((atom)       (erlext-read-atom))
      ((smallTuple) (erlext-read-small-tuple))
      ((largeTuple) (erlext-read-large-tuple))
      ((list)       (erlext-read-list))
      ((string)     (erlext-read-string))
      ((bin)        (erlext-read-binary))
      ((null)       nil)
      ((nil)        nil)
      ((pid)        (vector erl-tag
			    'erl-pid
			    (erlext-read-obj) ; node
			    (erlext-read4) ; id
			    (erlext-read4) ; serial
			    (erlext-read1))); creation
      ((port)       (vector erl-tag
			    'erl-port
			    (erlext-read-obj) ; node
			    (erlext-read4) ; id
			    (erlext-read1))) ; creation
      ((ref)        (vector erl-tag
			    'erl-ref
			    (erlext-read-obj) ; node
			    (erlext-read4) ;id
			    (erlext-read1))) ; creation
      ((newRef)     (erlext-read-newref))
      ((smallBig)   (erlext-read-small-bignum))
      ((largeBig)   (erlext-read-large-bignum))
     (t
      (error "Unknown tag: %S" tag)))))

(defun erlext-read (size)
  (case size
    ((1) (erlext-read1))
    ((2) (erlext-read2))
    ((4) (erlext-read4))))
;; read1 moved above so that it can be inlined
(defun erlext-read2 ()
  (logior (ash (erlext-read1) 8)
	  (erlext-read1)))
(defun erlext-read4 ()
  (logior (ash (erlext-read1) 24)
	  (ash (erlext-read1) 16)
	  (ash (erlext-read1) 8)
	  (erlext-read1)))
(defun erlext-readn (n)
  (assert (integerp n))
  (let ((start (point))
	(end   (+ (point) n)))
    (prog1 (let ((string (buffer-substring start end)))
	     (if (featurep 'xemacs)
		 string
	       (string-as-unibyte string))) ; fixme: should be
					    ; string-make-unibyte?
					    ; Why is it necessary
					    ; anyhow?
      (goto-char end))))
(defun erlext-read-atom ()
  (let ((length (erlext-read2)))
    (intern (erlext-readn length))))
(defun erlext-read-small-tuple ()
  (erlext-read-tuple (erlext-read1)))
(defun erlext-read-large-tuple ()
  (erlext-read-tuple (erlext-read4)))
(defun erlext-read-list ()
  (let ((arity (erlext-read4)))
    (prog1 (loop for x from 1 to arity
		 collect (erlext-read-obj))
      ;; This seems fishy, I find nil's at the end of lists, not
      ;; included as elements, and no mention of how it works in the
      ;; erl_ext_dist.txt
      (assert (eq (erlext-get-code 'null) (erlext-read1))))))
(defun erlext-read-tuple (arity)
  (apply #'vector (loop for x from 1 to arity
			collect (erlext-read-obj))))

(defun erlext-read-string ()
  (erlext-readn (erlext-read2)))

(defun erlext-read-binary ()
  (erlext-readn (erlext-read4)))

(defun erlext-read-newref ()
  (let* ((len (erlext-read2))
	 (node (erlext-read-obj))
	 (creation (erlext-read1))
	 (id (erlext-readn (* 4 len))))
    (vector erl-tag 'erl-new-ref node creation id)))

;; We don't actually support bignums. When we get one, we skip over it
;; and return the symbol {SMALL|LARGE}-BIGNUM.

(defun erlext-read-small-bignum ()
  (erlext-read (erlext-read1))
  'SMALL-BIGNUM)

(defun erlext-read-large-bignum ()
  (erlext-read (erlext-read4))
  'LARGE-BIGNUM)

;; ------------------------------------------------------------
;; Helpers
;; ------------------------------------------------------------

(defun erlext-get-tag (number)
  (car (rassq number erlext-tag-alist)))
(defun erlext-get-code (tag)
  (cdr (assq tag erlext-tag-alist)))

;; ------------------------------------------------------------
;; Testing
;; ------------------------------------------------------------

(defvar erlext-test-cases
  `(1 foo "bar" [bar baz] [,erl-tag erl-pid someone@somehost 0 0 0] (1 foo ())
      [,erl-tag erl-port someone@somehost 0 0]
      (([1 2]) ([1 2]))))

(defun erlext-test ()
  "Test each term in `erlext-test-cases' by encoding it and decoding
it and making sure that it's unchanged."
  (interactive)
  (mapc #'erlext-test-case erlext-test-cases)
  (message "Smooth sailing"))

(defun erlext-test-case (term)
  (condition-case x
      (assert (equal term (erlext-binary-to-term (erlext-term-to-binary term))))
    (error (error "test failed for %S: %S" term (error-message-string x)))))

(provide 'erlext)
