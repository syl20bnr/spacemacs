;;; core-spacebind-utest.el --- Spacemacs Unit Test File
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
      (MODE KEY-SEQUENCE REPLACEMENT &rest MORE)))
  "Signature of the functions that we will mock for the `spacebind' tests.")

;;;; Helpers:
(defmacro test-spacebind/subst-eval (rep-fun &rest body)
  "Substitute calls to binding functions with calls to REP-FUN.
REP-FUN applied to the form: (fn-sym (args)).
Binding functions are listed in `test-spacebind-moked-fns-sig'. "
  `(cl-labels
       ((spacemacs/leader-key () "SPC")
        (spacemacs/major-mode-prefix () "m")
        ,@(mapcar
           (lambda (seg)
             (let* ((f-s (car seg))
                    (args (cadr seg))
                    (pl-args (cl-set-difference args '(&rest &optional))))
               `(,f-s ,args (funcall ,rep-fun ',f-s (list ,@pl-args)))))
           test-spacebind-moked-fns-sig))
     ,@body))

(defmacro test-spacebind/log-calls (&rest body)
  "Evaluate BODY while mocking and logging calls to the binding functions.
The log is returned.
Binding functions are listed in `test-spacebind-moked-fns-sig'."
  `(let ((acc nil))
     (test-spacebind/subst-eval
         (lambda (fn args) (push (list* fn args) acc))
       ,@body)
     acc))

;; Example:
(thread-last (spacebind
              :major
              (major-foo-mode
               ("a" "section a"
                ("a" foo-fn "execute foo-fn"))))
  (test-spacebind/log-calls)
  (format "%S")
  (insert)
  ;; Prevents execution
  (declare))

;;;; Tests:
(ert-deftest test-spacebind-major-mode-always-generates-right-calls ()
  (should
   (eq '()
       (cl-set-exclusive-or
        (test-spacebind/log-calls
         (spacebind
          :major
          (python-mode
           "with a description"
           ("c" "compile/execute"
            ("c" spacemacs/python-execute-file "execute file")))))

        '((spacemacs/set-leader-keys-for-major-mode
            python-mode "mcc" spacemacs/python-execute-file nil)
          (which-key-add-major-mode-key-based-replacements
            python-mode "SPC m c c" "execute file" nil)
          (spacemacs/declare-prefix-for-mode
            python-mode "mc" "compile/execute" nil))
        :test 'equal))))

(ert-deftest test-spacebind-minor-mode-always-generates-right-calls ()
  (should
   (eq '()
       (cl-set-exclusive-or
        (test-spacebind/log-calls
         (spacebind
          :minor
          (foo-mode
           "With a description"
           ("a" "section under a key"
            ("b" baz-fn "call baz-fn")))))

        '((spacemacs/set-leader-keys-for-minor-mode
            foo-mode "ab" baz-fn nil)
          (spacemacs/add-key-based-replacements-for-minor-mode
           foo-mode "SPC a b" "call baz-fn" nil)
          (spacemacs/declare-prefix-for-mode
            foo-mode "a" "section under a key" nil))
        :test 'equal))))

(ert-deftest test-spacebind-global-always-generates-right-calls ()
  (should
   (eq '()
       (cl-set-exclusive-or
        (test-spacebind/log-calls
         (spacebind
          "With a description"
          :global
          (("a" "section under a key"
            ("b" bar-fn "call bar-fn")))))
        '((spacemacs/set-leader-keys "ab" bar-fn nil)
          (which-key-add-key-based-replacements "SPC a b" "call bar-fn" nil)
          (spacemacs/declare-prefix "a" "section under a key" nil))
        :test 'equal))))

(ert-deftest test-spacebind-doc-string-always-ignored ()
  (should (equal (test-spacebind/log-calls
                  (spacebind
                   :major
                   (python-mode
                    "With a doc-string"
                    ("c" "compile/execute"
                     ("c" spacemacs/python-execute-file "execute file")))))
                 (test-spacebind/log-calls
                  (spacebind
                   :major
                   (python-mode
                    ("c" "compile/execute"
                     ("c" spacemacs/python-execute-file "execute file")))))))
  (should (equal (test-spacebind/log-calls
                  (spacebind
                   :minor
                   (foo-mode
                    "With a doc-string"
                    ("a" "section under a key"
                     ("b" baz-fn "call baz-fn")))))
                 (test-spacebind/log-calls
                  (spacebind
                   :minor
                   (foo-mode
                    "With a description"
                    ("a" "section under a key"
                     ("b" baz-fn "call baz-fn")))))))
  (should (equal (test-spacebind/log-calls
                  (spacebind
                   "With a doc-string"
                   :global
                   (("a" "section under a key"
                     ("b" bar-fn "call bar-fn")))))
                 (test-spacebind/log-calls
                  (spacebind
                   :global
                   (("a" "section under a key"
                     ("b" bar-fn "call bar-fn"))))))))

(ert-deftest test-spacebind-multy-section-always-work ()
  (should (eq '()
              (cl-set-exclusive-or
               (test-spacebind/log-calls
                (spacebind
                 :major
                 (major-foo-mode
                  ("a" "section a"
                   ("a" foo-fn "execute foo-fn")))
                 :minor
                 (minor-foo-mode
                  ("b" "section b"
                   ("b" bar-fn "execute bar-fn")))
                 :major
                 (major-bar-mode
                  ("c" "section c"
                   ("c" baz-fn "execute baz-fn")))
                 :minor
                 (minor-bar-mode
                  ("d" "section d"
                   ("d" qux-fn "execute qux-fn")))
                 :global
                 (("e" "section e"
                   ("e" quux-fn "execute quux-fn")))
                 :global
                 (("f" "section f"
                   ("f" quuz-fn "execute quuz-fn")))))

               '((spacemacs/set-leader-keys "ff" quuz-fn nil)
                 (which-key-add-key-based-replacements
                   "SPC f f" "execute quuz-fn" nil)
                 (spacemacs/declare-prefix
                   "f" "section f" nil)
                 (spacemacs/set-leader-keys
                   "ee" quux-fn nil)
                 (which-key-add-key-based-replacements
                   "SPC e e" "execute quux-fn" nil)
                 (spacemacs/declare-prefix
                   "e" "section e" nil)
                 (spacemacs/set-leader-keys-for-minor-mode
                   minor-bar-mode "dd" qux-fn nil)
                 (spacemacs/add-key-based-replacements-for-minor-mode
                  minor-bar-mode "SPC d d" "execute qux-fn" nil)
                 (spacemacs/declare-prefix-for-mode
                   minor-bar-mode "d" "section d" nil)
                 (spacemacs/set-leader-keys-for-major-mode
                   major-bar-mode "mcc" baz-fn nil)
                 (which-key-add-major-mode-key-based-replacements
                   major-bar-mode "SPC m c c" "execute baz-fn" nil)
                 (spacemacs/declare-prefix-for-mode
                   major-bar-mode "mc" "section c" nil)
                 (spacemacs/set-leader-keys-for-minor-mode
                   minor-foo-mode "bb" bar-fn nil)
                 (spacemacs/add-key-based-replacements-for-minor-mode
                  minor-foo-mode "SPC b b" "execute bar-fn" nil)
                 (spacemacs/declare-prefix-for-mode
                   minor-foo-mode "b" "section b" nil)
                 (spacemacs/set-leader-keys-for-major-mode
                   major-foo-mode "maa" foo-fn nil)
                 (which-key-add-major-mode-key-based-replacements
                   major-foo-mode "SPC m a a" "execute foo-fn" nil)
                 (spacemacs/declare-prefix-for-mode
                   major-foo-mode "ma" "section a" nil))
               :test 'equal))))

(ert-deftest test-spacebind-complex-always-generates-right-calls ()
  (should
   (eq
    '()
    (cl-set-exclusive-or
     (test-spacebind/log-calls
      (spacebind
       :major
       (python-mode
        "Docstring for documentation"
        ("c" "compile/execute"
         ("c" spacemacs/python-execute-file "execute file")
         ("C" spacemacs/python-execute-file-focus "execute file and focus"))
        ("d" "debug"
         ("b" spacemacs/python-toggle-breakpoint "toggle breakpoint"))
        ("r" "refactor"
         ("i" spacemacs/python-remove-unused-imports "remove unused import"))
        ("s" "REPL"
         ("s" spacemacs/python-shell-send-buffer-switch
          "send buffer to REPL and focus")
         ("S" python-shell-send-buffer
          "send buffer to REPL")
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
         ("b" "sub section under b key"
          ("c" "sub sub section under c key"
           ("b" baz-fn "call baz-fn")))))
       :global
       (("a" "section under a key"
         ("b" bar-fn "call bar-fn")))))

     '((spacemacs/set-leader-keys
         "ab" bar-fn nil)
       (which-key-add-key-based-replacements
         "SPC a b" "call bar-fn" nil)
       (spacemacs/declare-prefix
         "a" "section under a key" nil)
       (spacemacs/set-leader-keys-for-minor-mode
         some-minor-mode "abcb" baz-fn nil)
       (spacemacs/add-key-based-replacements-for-minor-mode
        some-minor-mode "SPC abc b" "call baz-fn" nil)
       (spacemacs/declare-prefix-for-mode
         some-minor-mode "abc" "sub sub section under c key" nil)
       (spacemacs/declare-prefix-for-mode
         some-minor-mode "ab" "sub section under b key" nil)
       (spacemacs/declare-prefix-for-mode
         some-minor-mode "a" "section under a key" nil)
       (spacemacs/set-leader-keys-for-major-mode
         python-mode "msR" python-shell-send-region nil)
       (which-key-add-major-mode-key-based-replacements
         python-mode "SPC m s R" "send region to REPL" nil)
       (spacemacs/set-leader-keys-for-major-mode
         python-mode "msr" spacemacs/python-shell-send-region-switch nil)
       (which-key-add-major-mode-key-based-replacements
         python-mode "SPC m s r" "send region to REPL and focus" nil)
       (spacemacs/set-leader-keys-for-major-mode
         python-mode "msD" python-shell-send-defun nil)
       (which-key-add-major-mode-key-based-replacements
         python-mode "SPC m s D" "send function around point to REPL" nil)
       (spacemacs/set-leader-keys-for-major-mode
         python-mode "msd" spacemacs/python-shell-send-defun-switch nil)
       (which-key-add-major-mode-key-based-replacements
         python-mode
         "SPC m s d"
         "send function around point to REPL and focus"
         nil)
       (spacemacs/set-leader-keys-for-major-mode
         python-mode "msS" python-shell-send-buffer nil)
       (which-key-add-major-mode-key-based-replacements
         python-mode "SPC m s S" "send buffer to REPL" nil)
       (spacemacs/set-leader-keys-for-major-mode
         python-mode "mss" spacemacs/python-shell-send-buffer-switch nil)
       (which-key-add-major-mode-key-based-replacements
         python-mode "SPC m s s" "send buffer to REPL and focus" nil)
       (spacemacs/declare-prefix-for-mode
         python-mode "ms" "REPL" nil)
       (spacemacs/set-leader-keys-for-major-mode
         python-mode "mri" spacemacs/python-remove-unused-imports nil)
       (which-key-add-major-mode-key-based-replacements
         python-mode "SPC m r i" "remove unused import" nil)
       (spacemacs/declare-prefix-for-mode
         python-mode "mr" "refactor" nil)
       (spacemacs/set-leader-keys-for-major-mode
         python-mode "mdb" spacemacs/python-toggle-breakpoint nil)
       (which-key-add-major-mode-key-based-replacements
         python-mode "SPC m d b" "toggle breakpoint" nil)
       (spacemacs/declare-prefix-for-mode
         python-mode "md" "debug" nil)
       (spacemacs/set-leader-keys-for-major-mode
         python-mode "mcC" spacemacs/python-execute-file-focus nil)
       (which-key-add-major-mode-key-based-replacements
         python-mode "SPC m c C" "execute file and focus" nil)
       (spacemacs/set-leader-keys-for-major-mode
         python-mode "mcc" spacemacs/python-execute-file nil)
       (which-key-add-major-mode-key-based-replacements
         python-mode "SPC m c c" "execute file" nil)
       (spacemacs/declare-prefix-for-mode
         python-mode "mc" "compile/execute" nil))
     :test 'equal))))
