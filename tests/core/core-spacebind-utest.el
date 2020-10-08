;;; core-spacebind-utest.el --- Core Unit Test File -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(require 'core-spacebind)
(require 'cl-lib)

(defconst test-spacebind-moked-fns-sig
  '((spacemacs/add-key-based-replacements-for-minor-mode
     (MODE KEY-SEQUENCE REPLACEMENT &rest MORE))
    (spacemacs/declare-prefix
      (PREFIX NAME &optional LONG-NAME))
    (spacemacs/declare-prefix-for-mode
      (MODE PREFIX NAME &optional LONG-NAME))
    (spacemacs/set-leader-keys
      (KEY DEF &rest BINDINGS))
    (spacemacs/set-leader-keys-for-major-mode
      (MODE KEY DEF &rest BINDINGS))
    (spacemacs/set-leader-keys-for-minor-mode
      (MODE KEY DEF &rest BINDINGS))
    (which-key-add-key-based-replacements
      (KEY-SEQUENCE REPLACEMENT &rest MORE))
    (which-key-add-major-mode-key-based-replacements
      (MODE KEY-SEQUENCE REPLACEMENT &rest MORE))
    (spacemacs/add-which-key-fn-key-seq-override
     (FN-NAME KEY-REP LABEL)))
  "Signature of the functions that we will mock for the `spacebind' tests.")

;;;; Helpers:
(defmacro test-spacebind|subst-eval (rep-fun &rest body)
  "Substitute calls to binding functions with calls to REP-FUN.
REP-FUN applied to the form: (fn-sym (args)).
Binding functions are listed in `test-spacebind-moked-fns-sig'. "
  `(cl-letf
       (((symbol-function 'spacemacs/leader-key) (lambda () "SPC"))
        ((symbol-function 'spacemacs/major-mode-prefix) (lambda () "m"))
        ,@(mapcar
           (lambda (seg)
             (let* ((f-s (car seg))
                    (args (cadr seg))
                    (pl-args (cl-set-difference args '(&rest &optional))))
               `((symbol-function ',f-s)
                 (lambda ,args (,rep-fun ',f-s (list ,@pl-args))))))
           test-spacebind-moked-fns-sig))
     ,@body))

(defmacro test-spacebind|log-calls (&rest body)
  "Evaluate BODY while mocking and logging calls to the binding functions.
The log is returned.
Binding functions are listed in `test-spacebind-moked-fns-sig'.
NOTE: `spacebind--eager-bind' set to true. "
  `(let ((spacebind--eager-bind t)
         (acc nil))
     (test-spacebind|subst-eval
      (lambda (fn args)
        (push (cons fn args) acc))
      ,@body)
     acc))

;; Example:
(thread-last (spacemacs|spacebind
              :major
              (major-foo-mode
               ("a" "section a"
                ("a" foo-fn "execute foo-fn"))))
  (test-spacebind|log-calls)
  (format "%S")
  (insert)
  ;; Prevents execution
  (declare))

(defun test-spacebind/plist-diff (plist1 plist2)
  "Returns difference of PLIST1 and PLIST2 using `equal'."
  (cl-labels ((recur
               (acc plist)
               (let* ((key (car plist))
                      (val (cadr plist))
                      (rest (cddr plist))
                      (pair (list key val))
                      (new-acc (append (list pair) acc)))
                 (if rest
                     (recur new-acc rest)
                   new-acc))))
    (let ((set-1 (recur '() plist1))
          (set-2 (recur '() plist2)))
      (cl-set-exclusive-or set-1 set-2 :test 'equal))))

(defmacro test-spacebind|log-stack-eval (&rest body)
  "Evaluate BODY while mocking `spacebind//process-bind-stack'.
Stack values after the evaluation are returned and the stacks cleaned.

The return value is a plist of the shape:

(:minor-mode-replacements <STACK>
 :major-mode-replacements <STACK>
 :declare-prefix <STACK>
 :declare-prefix-for-mode <STACK>
 :set-leader-keys <STACK>
 :set-leader-keys-for-major-mode <STACK>
 :set-leader-keys-for-minor-mode <STACK>
 :global-replacements <STACK>
 :fn-key-seq-override <STACK>)

<STACK> is a corresponding binding stack.

NOTE: `spacebind--eager-bind' set to true."
  `(let ((spacebind--eager-bind t)
         (ret-plist nil))
     (cl-labels ((spacebind//process-bind-stack
                  ()
                  (setq ret-plist (list
                                   :minor-mode-replacements
                                   spacebind--bs-add-minor-mode-replacements
                                   :major-mode-replacements
                                   spacebind--bs-add-major-mode-replacements
                                   :declare-prefix
                                   spacebind--bs-declare-prefix
                                   :declare-prefix-for-mode
                                   spacebind--bs-declare-prefix-for-mode
                                   :set-leader-keys
                                   spacebind--bs-set-leader-keys
                                   :set-leader-keys-for-major-mode
                                   spacebind--bs-set-leader-keys-for-major-mode
                                   :set-leader-keys-for-minor-mode
                                   spacebind--bs-set-leader-keys-for-minor-mode
                                   :global-replacements
                                   spacebind--bs-global-replacements
                                   :fn-key-seq-override
                                   spacebind--bs-add-fn-key-seq-override)
                        spacebind--bs-global-replacements nil
                        spacebind--bs-set-leader-keys-for-minor-mode nil
                        spacebind--bs-set-leader-keys-for-major-mode nil
                        spacebind--bs-set-leader-keys nil
                        spacebind--bs-declare-prefix-for-mode nil
                        spacebind--bs-declare-prefix nil
                        spacebind--bs-add-major-mode-replacements nil
                        spacebind--bs-add-minor-mode-replacements nil
                        spacebind--bs-add-fn-key-seq-override nil
                        spacebind--timer [t])))
       (progn
         ,@body
         ret-plist))))

;; Example:
(thread-last (spacemacs|spacebind
              :major
              (major-foo-mode
               ("a" "section a"
                ("a" foo-fn "execute foo-fn"))))
  (test-spacebind|log-stack-eval)
  (format "%S")
  (insert)
  ;; Prevents execution
  (declare))

(defmacro test-spacebind|validate-keys (&rest body)
  "Mocks `spacebind//process-bind-stack' and validate key sequences.
NOTE: `spacebind--eager-bind' set to true. "
  `(cl-letf* ((invalid-key-seqs '())
              (spacebind--eager-bind t)
              (spacebind--bs-add-minor-mode-replacements '())
              (spacebind--bs-add-major-mode-replacements '())
              (spacebind--bs-declare-prefix '())
              (spacebind--bs-declare-prefix-for-mode '())
              (spacebind--bs-set-leader-keys '())
              (spacebind--bs-set-leader-keys-for-major-mode '())
              (spacebind--bs-set-leader-keys-for-minor-mode '())
              (spacebind--bs-global-replacements '())
              (spacebind--bs-add-fn-key-seq-override '())
              (spacebind--timer [t])
              (called nil)
              ((symbol-function 'spacebind//process-bind-stack)
               (lambda () (progn))))
     ,@body
     (dolist (el (apply 'append
                        (append spacebind--bs-add-minor-mode-replacements
                                spacebind--bs-add-major-mode-replacements
                                spacebind--bs-declare-prefix
                                spacebind--bs-declare-prefix-for-mode
                                spacebind--bs-set-leader-keys
                                spacebind--bs-set-leader-keys-for-major-mode
                                spacebind--bs-set-leader-keys-for-minor-mode
                                spacebind--bs-global-replacements
                                spacebind--bs-add-fn-key-seq-override)))
       (when (listp el) ;; all list arguments are key sequences.
         (condition-case err (kbd (string-join el " "))
           ((error nil) (push err invalid-key-seqs)))))
     (cl-remove-duplicates invalid-key-seqs :test 'equal)))

;;;; Tests:
(ert-deftest test-spacebind-major-mode-always-generates-right-stack ()
  (thread-last (spacemacs|spacebind
                :major
                (py-mode
                 "with a description"
                 ("c" "compile/execute"
                  ("c" spacemacs/python-execute-file "execute file"))))
    (test-spacebind|log-stack-eval)
    (test-spacebind/plist-diff
     '(:major-mode-replacements
       ((py-mode ("c" "c") "execute file"))
       :declare-prefix-for-mode
       ((py-mode ("c") "compile/execute"))
       :set-leader-keys-for-major-mode
       ((py-mode ("c" "c") spacemacs/python-execute-file))
       :minor-mode-replacements nil
       :declare-prefix nil
       :set-leader-keys nil
       :set-leader-keys-for-minor-mode nil
       :fn-key-seq-override nil
       :global-replacements nil))
    (eq nil)
    (should)))

(ert-deftest test-spacebind-minor-mode-always-generates-right-stack ()
  (thread-last (spacemacs|spacebind
                :minor
                (foo-mode
                 "With a description"
                 ("a" "section under a key"
                  ("b" baz-fn "call baz-fn"))))
    (test-spacebind|log-stack-eval)
    (test-spacebind/plist-diff
     '(:minor-mode-replacements
       ((foo-mode ("a" "b") "call baz-fn"))
       :set-leader-keys-for-minor-mode
       ((foo-mode ("a" "b") baz-fn))
       :declare-prefix-for-mode
       ((foo-mode ("a") "section under a key"))
       :major-mode-replacements nil
       :declare-prefix nil
       :set-leader-keys nil
       :set-leader-keys-for-major-mode nil
       :fn-key-seq-override nil
       :global-replacements nil))
    (eq nil)
    (should)))

(ert-deftest test-spacebind-global-always-generates-right-stack ()
  (thread-last (spacemacs|spacebind
                "With a description"
                :global
                (("a" "section under a key"
                  ("b" bar-fn "call bar-fn"))))
    (test-spacebind|log-stack-eval)
    (test-spacebind/plist-diff
     '(:declare-prefix
       ((("a") "section under a key"))
       :set-leader-keys
       ((("a" "b") bar-fn))
       :global-replacements
       ((("a" "b") "call bar-fn"))
       :fn-key-seq-override nil
       :minor-mode-replacements nil
       :major-mode-replacements nil
       :declare-prefix-for-mode nil
       :set-leader-keys-for-major-mode nil
       :set-leader-keys-for-minor-mode nil))
    (eq nil)
    (should)))

(ert-deftest test-spacebind-doc-string-always-ignored ()
  (should (equal (test-spacebind|log-stack-eval
                  (spacemacs|spacebind
                   :major
                   (py-mode
                    "With a doc-string"
                    ("c" "compile/execute"
                     ("c" spacemacs/python-execute-file "execute file")))))
                 (test-spacebind|log-stack-eval
                  (spacemacs|spacebind
                   :major
                   (py-mode
                    ("c" "compile/execute"
                     ("c" spacemacs/python-execute-file "execute file")))))))
  (should (equal (test-spacebind|log-stack-eval
                  (spacemacs|spacebind
                   :minor
                   (foo-mode
                    "With a doc-string"
                    ("a" "section under a key"
                     ("b" bar-fn "call bar-fn")))
                   (baz-mode
                    "With a doc-string"
                    ("c" "section under a key"
                     ("d" qux-fn "call qux-fn")))))
                 (test-spacebind|log-stack-eval
                  (spacemacs|spacebind
                   :minor
                   (foo-mode
                    ("a" "section under a key"
                     ("b" bar-fn "call bar-fn")))
                   (baz-mode
                    ("c" "section under a key"
                     ("d" qux-fn "call qux-fn")))))))
  (should (equal (test-spacebind|log-stack-eval
                  (spacemacs|spacebind
                   :global
                   ("With a doc-string"
                    ("a" "section under a key"
                     ("b" bar-fn "call bar-fn")))))
                 (test-spacebind|log-stack-eval
                  (spacemacs|spacebind
                   :global
                   (("a" "section under a key"
                     ("b" bar-fn "call bar-fn"))))))))

(ert-deftest test-spacebind-desc-overrides-always-applied ()
  (thread-first (spacemacs|spacebind
                 :global
                 (("a" foo-fn ("ignored(used for docs)" :label "used label"))))
    (test-spacebind|log-stack-eval)
    (plist-get :global-replacements)
    (car)
    (equal '(("a") "used label"))
    (should))
  (thread-first (spacemacs|spacebind
                 :minor
                 (foo-mode
                  ("a" foo-fn ("ignored(used for docs)" :label "used label"))))
    (test-spacebind|log-stack-eval)
    (plist-get :minor-mode-replacements)
    (car)
    (equal '(foo-mode ("a") "used label"))
    (should))
  (thread-first (spacemacs|spacebind
                 :major
                 (foo-mode
                  ("a" foo-fn ("ignored(used for docs)" :label "used label"))))
    (test-spacebind|log-stack-eval)
    (plist-get :major-mode-replacements)
    (car)
    (equal '(foo-mode ("a") "used label"))
    (should)))

(ert-deftest test-spacebind-key-overrides-always-applied ()
  (thread-first (spacemacs|spacebind
                 :global
                 ((("a" :label "press a") foo-fn "label")))
    (test-spacebind|log-stack-eval)
    (plist-get :fn-key-seq-override)
    (car)
    (equal '("foo-fn" "press a" "label"))
    (should))
  (thread-first (spacemacs|spacebind
                 :minor
                 (foo-mode
                  (("a" :label "press a") foo-fn "label")))
    (test-spacebind|log-stack-eval)
    (plist-get :fn-key-seq-override)
    (car)
    (equal '("foo-fn" "press a" "label"))
    (should))
  (thread-first (spacemacs|spacebind
                 :major
                 (foo-mode
                  (("a" :label "press a") foo-fn "label")))
    (test-spacebind|log-stack-eval)
    (plist-get :fn-key-seq-override)
    (car)
    (equal '("foo-fn" "press a" "label"))
    (should)))

(ert-deftest test-spacebind-labels-multi-line-strings-always-joined ()
  (thread-first (spacemacs|spacebind
                 :global
                 (("a" foo-fn "This is a
                               multi line string")))
    (test-spacebind|log-stack-eval)
    (plist-get :global-replacements)
    (car)
    (equal '(("a") "This is a multi line string"))
    (should))
  (thread-first (spacemacs|spacebind
                 :minor
                 (foo-mode
                  ("a" foo-fn "This is a
                               multi line string")))
    (test-spacebind|log-stack-eval)
    (plist-get :minor-mode-replacements)
    (car)
    (equal '(foo-mode ("a") "This is a multi line string"))
    (should))
  (thread-first (spacemacs|spacebind
                 :major
                 (foo-mode
                  ("a" foo-fn "This is a
                               multi line string")))
    (test-spacebind|log-stack-eval)
    (plist-get :major-mode-replacements)
    (car)
    (equal '(foo-mode ("a") "This is a multi line string"))
    (should))
  (thread-first (spacemacs|spacebind
                 :global
                 (("a" foo-fn ("ignored" :label "This is a
                                                 multi line string"))))
    (test-spacebind|log-stack-eval)
    (plist-get :global-replacements)
    (car)
    (equal '(("a") "This is a multi line string"))
    (should)))

(ert-deftest test-spacebind-labels-pipe-slicing-always-works ()
  (thread-first (spacemacs|spacebind
                 :global
                 (("a" foo-fn "this part goes into label | that part omitted")))
    (test-spacebind|log-stack-eval)
    (plist-get :global-replacements)
    (car)
    (equal '(("a") "this part goes into label"))
    (should))
  (thread-first (spacemacs|spacebind
                 :minor
                 (foo-mode
                  ("a" foo-fn "this part goes into label | that part omitted")))
    (test-spacebind|log-stack-eval)
    (plist-get :minor-mode-replacements)
    (car)
    (equal '(foo-mode ("a") "this part goes into label"))
    (should))
  (thread-first (spacemacs|spacebind
                 :major
                 (foo-mode
                  ("a" foo-fn "this part goes into label | that part omitted")))
    (test-spacebind|log-stack-eval)
    (plist-get :major-mode-replacements)
    (car)
    (equal '(foo-mode ("a") "this part goes into label"))
    (should))
  (thread-first (spacemacs|spacebind
                 :global
                 (("a" foo-fn ("ignored"
                               :label "this part goes into label
                                       | that part omitted"))))
    (test-spacebind|log-stack-eval)
    (plist-get :global-replacements)
    (car)
    (equal '(("a") "this part goes into label"))
    (should)))

(ert-deftest test-spacebind-always-generates-right-stack ()
  (thread-last
      (spacemacs|spacebind
       :major
       (py-mode
        "Docstring for documentation"
        ("C-p" "compile/execute"
         ("TAB" spacemacs/python-execute-file "execute file")
         ("C" spacemacs/python-execute-file-focus "execute file and focus"))
        ("d" "debug"
         (("b" :label "->b") spacemacs/python-toggle-breakpoint "toggle
                                                                 breakpoint"))
        ("r" "refactor"
         ("i" spacemacs/python-remove-unused-imports "remove unused import"))
        ("s" "REPL"
         ("s" spacemacs/python-shell-send-buffer-switch
          "send buffer to REPL and focus | on the buffer")
         ("S" python-shell-send-buffer
          ("send buffer to REPL" :label "buffer -> REPL | without
                                                          focusing"))
         ("d" spacemacs/python-shell-send-defun-switch
          "send function around point to REPL and focus")
         ("D" python-shell-send-defun
          "send function around point to REPL")
         ("r" spacemacs/python-shell-send-region-switch
          "send region to REPL and focus")
         ("R" python-shell-send-region "send region to REPL")))
       :minor
       (some-minor-mode
        ("a" "section under a key"
         ("b" foo-fn "call foo-fn")
         ("c" "sub section under c key"
          ("d" "sub sub section under d key"
           ("e" baz-fn "call baz-fn")))))
       (some-another-minor-mode
        ("a" "section under a key"
         ("b" "sub section under b key"
          ("c" "sub sub section under c key"
           ("b" baz-fn "call baz-fn")))))
       :global
       (("C-v" "section under a key"
         ("b" bar-fn "call bar-fn"))))
    (test-spacebind|log-stack-eval)
    (test-spacebind/plist-diff
     '(:minor-mode-replacements
       ((some-another-minor-mode ("a" "b" "c" "b") "call baz-fn")
        (some-minor-mode ("a" "c" "d" "e") "call baz-fn")
        (some-minor-mode ("a" "b") "call foo-fn"))
       :major-mode-replacements
       ((py-mode ("s" "R") "send region to REPL")
        (py-mode ("s" "r") "send region to REPL and focus")
        (py-mode ("s" "D") "send function around point to REPL")
        (py-mode ("s" "d") "send function around point to REPL and focus")
        (py-mode ("s" "S") "buffer -> REPL")
        (py-mode ("s" "s") "send buffer to REPL and focus")
        (py-mode ("r" "i") "remove unused import")
        (py-mode ("d" "b") "toggle breakpoint")
        (py-mode ("C-p" "C") "execute file and focus")
        (py-mode ("C-p" "TAB") "execute file"))
       :declare-prefix
       ((("C-v") "section under a key"))
       :declare-prefix-for-mode
       ((some-another-minor-mode ("a" "b" "c") "sub sub section under c key")
        (some-another-minor-mode ("a" "b") "sub section under b key")
        (some-another-minor-mode ("a") "section under a key")
        (some-minor-mode ("a" "c" "d") "sub sub section under d key")
        (some-minor-mode ("a" "c") "sub section under c key")
        (some-minor-mode ("a") "section under a key")
        (py-mode ("s") "REPL") (py-mode ("r") "refactor")
        (py-mode ("d") "debug") (py-mode ("C-p") "compile/execute"))
       :set-leader-keys
       ((("C-v" "b") bar-fn))
       :set-leader-keys-for-major-mode
       ((py-mode ("s" "R") python-shell-send-region)
        (py-mode ("s" "r") spacemacs/python-shell-send-region-switch)
        (py-mode ("s" "D") python-shell-send-defun)
        (py-mode ("s" "d") spacemacs/python-shell-send-defun-switch)
        (py-mode ("s" "S") python-shell-send-buffer)
        (py-mode ("s" "s") spacemacs/python-shell-send-buffer-switch)
        (py-mode ("r" "i") spacemacs/python-remove-unused-imports)
        (py-mode ("d" "b") spacemacs/python-toggle-breakpoint)
        (py-mode ("C-p" "C") spacemacs/python-execute-file-focus)
        (py-mode ("C-p" "TAB") spacemacs/python-execute-file))
       :set-leader-keys-for-minor-mode
       ((some-another-minor-mode ("a" "b" "c" "b") baz-fn)
        (some-minor-mode ("a" "c" "d" "e") baz-fn)
        (some-minor-mode ("a" "b") foo-fn))
       :global-replacements
       ((("C-v" "b") "call bar-fn"))
       :fn-key-seq-override
       (("spacemacs/python-toggle-breakpoint" "->b" "toggle breakpoint"))))
    (eq nil)
    (should)))

(ert-deftest test-spacebind-always-generates-right-calls ()
  (thread-first
      (spacemacs|spacebind
       :major
       (py-mode
        "Docstring for documentation"
        ("C-p" "compile/execute"
         ("TAB" spacemacs/python-execute-file "execute file")
         ("C" spacemacs/python-execute-file-focus "execute file and focus"))
        ("d" "debug"
         (("b" :label "->b") spacemacs/python-toggle-breakpoint "toggle
                                                                 breakpoint"))
        ("r" "refactor"
         ("i" spacemacs/python-remove-unused-imports "remove unused import"))
        ("s" "REPL"
         ("s" spacemacs/python-shell-send-buffer-switch
          "send buffer to REPL and focus | on the buffer")
         ("S" python-shell-send-buffer
          ("send buffer to REPL" :label "buffer -> REPL | without
                                                          focusing"))
         ("d" spacemacs/python-shell-send-defun-switch
          "send function around point to REPL and focus")
         ("D" python-shell-send-defun
          "send function around point to REPL")
         ("r" spacemacs/python-shell-send-region-switch
          "send region to REPL and focus")
         ("R" python-shell-send-region "send region to REPL")))
       :minor
       (some-minor-mode
        ("a" "section under a key"
         ("b" foo-fn "call foo-fn")
         ("c" "sub section under c key"
          ("d" "sub sub section under d key"
           ("e" baz-fn "call baz-fn")))))
       (some-another-minor-mode
        ("a" "section under a key"
         ("b" "sub section under b key"
          ("c" "sub sub section under c key"
           ("b" baz-fn "call baz-fn")))))
       :global
       (("C-v" "section under a key"
         ("b" bar-fn "call bar-fn"))))
    (test-spacebind|log-calls)
    (cl-set-exclusive-or
     '((spacemacs/add-which-key-fn-key-seq-override
        "spacemacs/python-toggle-breakpoint" "->b" "toggle breakpoint")
       (which-key-add-key-based-replacements
         "SPC C-v b" "call bar-fn" nil)
       (spacemacs/set-leader-keys-for-minor-mode
         some-minor-mode "a b" foo-fn nil)
       (spacemacs/set-leader-keys-for-minor-mode
         some-minor-mode "a c d e" baz-fn nil)
       (spacemacs/set-leader-keys-for-minor-mode
         some-another-minor-mode "a b c b" baz-fn nil)
       (spacemacs/set-leader-keys-for-major-mode
         py-mode "C-p TAB" spacemacs/python-execute-file nil)
       (spacemacs/set-leader-keys-for-major-mode
         py-mode "C-p C" spacemacs/python-execute-file-focus nil)
       (spacemacs/set-leader-keys-for-major-mode
         py-mode "d b" spacemacs/python-toggle-breakpoint nil)
       (spacemacs/set-leader-keys-for-major-mode
         py-mode "r i" spacemacs/python-remove-unused-imports nil)
       (spacemacs/set-leader-keys-for-major-mode
         py-mode "s s" spacemacs/python-shell-send-buffer-switch nil)
       (spacemacs/set-leader-keys-for-major-mode
         py-mode "s S" python-shell-send-buffer nil)
       (spacemacs/set-leader-keys-for-major-mode
         py-mode "s d" spacemacs/python-shell-send-defun-switch nil)
       (spacemacs/set-leader-keys-for-major-mode
         py-mode "s D" python-shell-send-defun nil)
       (spacemacs/set-leader-keys-for-major-mode
         py-mode "s r" spacemacs/python-shell-send-region-switch nil)
       (spacemacs/set-leader-keys-for-major-mode
         py-mode "s R" python-shell-send-region nil)
       (spacemacs/set-leader-keys
         "C-v b" bar-fn nil)
       (spacemacs/declare-prefix-for-mode
         py-mode "C-p" "compile/execute" nil)
       (spacemacs/declare-prefix-for-mode
         py-mode "d" "debug" nil)
       (spacemacs/declare-prefix-for-mode
         py-mode "r" "refactor" nil)
       (spacemacs/declare-prefix-for-mode
         py-mode "s" "REPL" nil)
       (spacemacs/declare-prefix-for-mode
         some-minor-mode "a" "section under a key" nil)
       (spacemacs/declare-prefix-for-mode
         some-minor-mode "a c" "sub section under c key" nil)
       (spacemacs/declare-prefix-for-mode
         some-minor-mode "a c d" "sub sub section under d key" nil)
       (spacemacs/declare-prefix-for-mode
         some-another-minor-mode "a" "section under a key" nil)
       (spacemacs/declare-prefix-for-mode
         some-another-minor-mode "a b" "sub section under b key" nil)
       (spacemacs/declare-prefix-for-mode
         some-another-minor-mode "a b c" "sub sub section under c key" nil)
       (spacemacs/declare-prefix
         "C-v" "section under a key" nil)
       (which-key-add-major-mode-key-based-replacements
         py-mode "SPC m C-p TAB" "execute file" nil)
       (which-key-add-major-mode-key-based-replacements
         py-mode "SPC m C-p C" "execute file and focus" nil)
       (which-key-add-major-mode-key-based-replacements
         py-mode "SPC m d b" "toggle breakpoint" nil)
       (which-key-add-major-mode-key-based-replacements
         py-mode "SPC m r i" "remove unused import" nil)
       (which-key-add-major-mode-key-based-replacements
         py-mode "SPC m s s" "send buffer to REPL and focus" nil)
       (which-key-add-major-mode-key-based-replacements
         py-mode "SPC m s S" "buffer -> REPL" nil)
       (which-key-add-major-mode-key-based-replacements
         py-mode "SPC m s d" "send function around point to REPL and focus" nil)
       (which-key-add-major-mode-key-based-replacements
         py-mode "SPC m s D" "send function around point to REPL" nil)
       (which-key-add-major-mode-key-based-replacements
         py-mode "SPC m s r" "send region to REPL and focus" nil)
       (which-key-add-major-mode-key-based-replacements
         py-mode "SPC m s R" "send region to REPL" nil)
       (spacemacs/add-key-based-replacements-for-minor-mode
        some-minor-mode "SPC a b" "call foo-fn" nil)
       (spacemacs/add-key-based-replacements-for-minor-mode
        some-minor-mode "SPC a c d e" "call baz-fn" nil)
       (spacemacs/add-key-based-replacements-for-minor-mode
        some-another-minor-mode "SPC a b c b" "call baz-fn" nil))
     :test 'equal)
    (eq nil)
    (should)))

(ert-deftest test-spacebind-always-generate-valid-key-seqs ()
  (thread-first
      (spacemacs|spacebind
       :major
       (py-mode
        "Docstring for documentation"
        ("C-p" "compile/execute"
         ("TAB" spacemacs/python-execute-file "execute file")
         ("C" spacemacs/python-execute-file-focus "execute file and focus"))
        ("d" "debug"
         (("b" :label "->b") spacemacs/python-toggle-breakpoint "toggle
                                                                 breakpoint"))
        ("r" "refactor"
         ("i" spacemacs/python-remove-unused-imports "remove unused import"))
        ("s" "REPL"
         ("s" spacemacs/python-shell-send-buffer-switch
          "send buffer to REPL and focus | on the buffer")
         ("S" python-shell-send-buffer
          ("send buffer to REPL" :label "buffer -> REPL | without
                                                          focusing"))
         ("d" spacemacs/python-shell-send-defun-switch
          "send function around point to REPL and focus")
         ("D" python-shell-send-defun
          "send function around point to REPL")
         ("r" spacemacs/python-shell-send-region-switch
          "send region to REPL and focus")
         ("R" python-shell-send-region "send region to REPL")))
       :minor
       (some-minor-mode
        ("a" "section under a key"
         ("b" foo-fn "call foo-fn")
         ("c" "sub section under c key"
          ("d" "sub sub section under d key"
           ("e" baz-fn "call baz-fn")))))
       (some-another-minor-mode
        ("a" "section under a key"
         ("b" "sub section under b key"
          ("c" "sub sub section under c key"
           ("b" baz-fn "call baz-fn")))))
       :global
       (("C-v" "section under a key"
         ("b" bar-fn "call bar-fn"))))
    (test-spacebind|validate-keys)
    (eq '())
    (should)))
