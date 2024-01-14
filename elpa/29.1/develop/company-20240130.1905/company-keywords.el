;;; company-keywords.el --- A company backend for programming language keywords  -*- lexical-binding: t -*-

;; Copyright (C) 2009-2011, 2013-2018, 2020-2023  Free Software Foundation, Inc.

;; Author: Nikolaj Schumacher

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:
;;

;;; Code:

(require 'company)
(require 'cl-lib)
(eval-when-compile (require 'make-mode))

(defgroup company-keywords nil
  "Completion backend for keywords."
  :group 'company)

(defcustom company-keywords-ignore-case nil
  "Non-nil to ignore case in completion candidates."
  :type 'boolean)

(defun company-keywords-upper-lower (&rest lst)
  ;; Upcase order is different for _.
  (nconc (sort (mapcar 'upcase lst) 'string<) lst))

(defvar company-keywords-alist
  ;; Please contribute corrections or additions.
  `((c++-mode
     ;; from https://en.cppreference.com/w/cpp/keyword
     "alignas" "alignof" "and" "and_eq" "asm" "atomic_cancel" "atomic_commit"
     "atomic_noexcept" "auto" "bitand" "bitor" "bool" "break" "case" "catch"
     "char" "char16_t" "char32_t" "char8_t" "class" "co_await" "co_return"
     "co_yield" "compl" "concept" "const" "const_cast" "consteval" "constexpr"
     "constinit" "continue" "decltype" "default" "delete" "do" "double"
     "dynamic_cast" "else" "enum" "explicit" "export" "extern" "false" "final"
     "float" "for" "friend" "goto" "if" "import" "inline" "int" "long" "module"
     "mutable" "namespace" "new" "noexcept" "not" "not_eq" "nullptr" "operator"
     "or" "or_eq" "override" "private" "protected" "public" "reflexpr" "register"
     "reinterpret_cast" "requires" "return" "short" "signed" "sizeof" "static"
     "static_assert" "static_cast" "struct" "switch" "synchronized" "template"
     "this" "thread_local" "throw" "true" "try" "typedef" "typeid" "typename"
     "union" "unsigned" "using" "virtual" "void" "volatile" "wchar_t" "while"
     "xor" "xor_eq")
    (c-mode
     ;; from https://en.cppreference.com/w/c/keyword
     "_Alignas" "_Alignof" "_Atomic" "_Bool" "_Complex"
     "_Decimal128" "_Decimal32" "_Decimal64" "_Generic" "_Imaginary"
     "_Noreturn" "_Static_assert" "_Thread_local" "__asm__" "asm"
     "auto" "break" "case" "char" "const" "continue" "default" "do"
     "double" "else" "enum" "extern" "float" "for" "goto" "if" "inline"
     "int" "long" "register" "restrict" "return" "short" "signed" "sizeof"
     "static" "struct" "switch" "typedef" "union" "unsigned" "void" "volatile"
     "while")
    (crystal-mode
     ;; from https://github.com/crystal-lang/crystal-book/issues/124#issuecomment-1008311227
    "abstract" "alias" "annotation" "as" "as?" "asm" "begin" "break" "case" "class"
    "def" "do" "else" "elsif" "end" "ensure" "enum" "extend" "false" "for" "fun"
    "if" "in" "include" "instance_sizeof" "is_a?" "lib" "macro" "module" "next"
    "nil" "nil?" "of" "offsetof" "out" "pointerof" "private" "protected" "require"
    "rescue" "responds_to?" "return" "select" "self" "sizeof" "struct" "super"
    "then" "true" "type" "typeof" "uninitialized" "union" "unless" "until" "verbatim"
    "when" "while" "with" "yield")
    (csharp-mode
     "abstract" "add" "alias" "as" "base" "bool" "break" "byte" "case"
     "catch" "char" "checked" "class" "const" "continue" "decimal" "default"
     "delegate" "do" "double" "else" "enum" "event" "explicit" "extern"
     "false" "finally" "fixed" "float" "for" "foreach" "get" "global" "goto"
     "if" "implicit" "in" "int" "interface" "internal" "is" "lock" "long"
     "namespace" "new" "null" "object" "operator" "out" "override" "params"
     "partial" "private" "protected" "public" "readonly" "ref" "remove"
     "return" "sbyte" "sealed" "set" "short" "sizeof" "stackalloc" "static"
     "string" "struct" "switch" "this" "throw" "true" "try" "typeof" "uint"
     "ulong" "unchecked" "unsafe" "ushort" "using" "value" "var" "virtual"
     "void" "volatile" "where" "while" "yield")
    (d-mode
     ;; from http://www.digitalmars.com/d/2.0/lex.html
     "abstract" "alias" "align" "asm"
     "assert" "auto" "body" "bool" "break" "byte" "case" "cast" "catch"
     "cdouble" "cent" "cfloat" "char" "class" "const" "continue" "creal"
     "dchar" "debug" "default" "delegate" "delete" "deprecated" "do"
     "double" "else" "enum" "export" "extern" "false" "final" "finally"
     "float" "for" "foreach" "foreach_reverse" "function" "goto" "idouble"
     "if" "ifloat" "import" "in" "inout" "int" "interface" "invariant"
     "ireal" "is" "lazy" "long" "macro" "mixin" "module" "new" "nothrow"
     "null" "out" "override" "package" "pragma" "private" "protected"
     "public" "pure" "real" "ref" "return" "scope" "short" "static" "struct"
     "super" "switch" "synchronized" "template" "this" "throw" "true" "try"
     "typedef" "typeid" "typeof" "ubyte" "ucent" "uint" "ulong" "union"
     "unittest" "ushort" "version" "void" "volatile" "wchar" "while" "with")
    (elixir-mode
     ;; from https://hexdocs.pm/elixir/Kernel.html
     "__CALLER__" "__DIR__" "__ENV__" "__MODULE__" "__STACKTRACE__"
     "__aliases__" "__block__" "abs" "alias" "alias!" "and" "apply"
     "binary_part" "binary_slice" "binding" "bit_size" "byte_size" "case" "ceil"
     "cond" "dbg" "def" "defdelegate" "defexception" "defguard" "defguardp"
     "defimpl" "defmacro" "defmacrop" "defmodule" "defoverridable" "defp"
     "defprotocol" "defstruct" "destructure" "div" "elem" "exit" "floor" "fn"
     "for" "function_exported?" "get_and_update_in" "get_in" "hd" "if" "import"
     "in" "inspect" "is_atom" "is_binary" "is_bitstring" "is_boolean"
     "is_exception" "is_float" "is_function" "is_integer" "is_list" "is_map"
     "is_map_key" "is_nil" "is_number" "is_pid" "is_port" "is_reference"
     "is_struct" "is_tuple" "length" "macro_exported?" "make_ref" "map_size"
     "match?" "max" "min" "node" "not" "or" "pop_in" "put_elem" "put_in" "quote"
     "raise" "receive" "rem" "require" "reraise" "round" "self" "send" "spawn"
     "spawn_link" "spawn_monitor" "struct" "struct!" "super" "tap" "then"
     "throw" "tl" "to_charlist" "to_string" "trunc" "try" "tuple_size" "unless"
     "unquote" "unquote_splicing" "update_in" "use" "var!" "with")
    (erlang-mode
     ;; from https://www.erlang.org/docs/20/reference_manual/introduction.html#id63536
     "after" "and" "andalso" "band" "begin" "bnot" "bor" "bsl" "bsr" "bxor"
     "case" "catch" "cond" "div" "end" "fun" "if" "let" "not" "of" "or" "orelse"
     "receive" "rem" "try" "when" "xor")
    (f90-mode .
     ;; from f90.el
     ;; ".AND." ".GE." ".GT." ".LT." ".LE." ".NE." ".OR." ".TRUE." ".FALSE."
     ,(company-keywords-upper-lower
      "abs" "abstract" "achar" "acos" "adjustl" "adjustr" "aimag" "aint"
      "align" "all" "all_prefix" "all_scatter" "all_suffix" "allocatable"
      "allocate" "allocated" "and" "anint" "any" "any_prefix" "any_scatter"
      "any_suffix" "asin" "assign" "assignment" "associate" "associated"
      "asynchronous" "atan" "atan2" "backspace" "bind" "bit_size" "block"
      "btest" "c_alert" "c_associated" "c_backspace" "c_bool"
      "c_carriage_return" "c_char" "c_double" "c_double_complex" "c_f_pointer"
      "c_f_procpointer" "c_float" "c_float_complex" "c_form_feed" "c_funloc"
      "c_funptr" "c_horizontal_tab" "c_int" "c_int16_t" "c_int32_t" "c_int64_t"
      "c_int8_t" "c_int_fast16_t" "c_int_fast32_t" "c_int_fast64_t"
      "c_int_fast8_t" "c_int_least16_t" "c_int_least32_t" "c_int_least64_t"
      "c_int_least8_t" "c_intmax_t" "c_intptr_t" "c_loc" "c_long"
      "c_long_double" "c_long_double_complex" "c_long_long" "c_new_line"
      "c_null_char" "c_null_funptr" "c_null_ptr" "c_ptr" "c_short"
      "c_signed_char" "c_size_t" "c_vertical_tab" "call" "case" "ceiling"
      "char" "character" "character_storage_size" "class" "close" "cmplx"
      "command_argument_count" "common" "complex" "conjg" "contains" "continue"
      "copy_prefix" "copy_scatter" "copy_suffix" "cos" "cosh" "count"
      "count_prefix" "count_scatter" "count_suffix" "cpu_time" "cshift"
      "cycle" "cyclic" "data" "date_and_time" "dble" "deallocate" "deferred"
      "digits" "dim" "dimension" "distribute" "do" "dot_product" "double"
      "dprod" "dynamic" "elemental" "else" "elseif" "elsewhere" "end" "enddo"
      "endfile" "endif" "entry" "enum" "enumerator" "eoshift" "epsilon" "eq"
      "equivalence" "eqv" "error_unit" "exit" "exp" "exponent" "extends"
      "extends_type_of" "external" "extrinsic" "false" "file_storage_size"
      "final" "floor" "flush" "forall" "format" "fraction" "function" "ge"
      "generic" "get_command" "get_command_argument" "get_environment_variable"
      "goto" "grade_down" "grade_up" "gt" "hpf_alignment" "hpf_distribution"
      "hpf_template" "huge" "iachar" "iall" "iall_prefix" "iall_scatter"
      "iall_suffix" "iand" "iany" "iany_prefix" "iany_scatter" "iany_suffix"
      "ibclr" "ibits" "ibset" "ichar" "ieee_arithmetic" "ieee_exceptions"
      "ieee_features" "ieee_get_underflow_mode" "ieee_set_underflow_mode"
      "ieee_support_underflow_control" "ieor" "if" "ilen" "implicit"
      "import" "include" "independent" "index" "inherit" "input_unit"
      "inquire" "int" "integer" "intent" "interface" "intrinsic" "ior"
      "iostat_end" "iostat_eor" "iparity" "iparity_prefix" "iparity_scatter"
      "iparity_suffix" "ishft" "ishftc" "iso_c_binding" "iso_fortran_env"
      "kind" "lbound" "le" "leadz" "len" "len_trim" "lge" "lgt" "lle" "llt"
      "log" "log10" "logical" "lt" "matmul" "max" "maxexponent" "maxloc"
      "maxval" "maxval_prefix" "maxval_scatter" "maxval_suffix" "merge"
      "min" "minexponent" "minloc" "minval" "minval_prefix" "minval_scatter"
      "minval_suffix" "mod" "module" "modulo" "move_alloc" "mvbits" "namelist"
      "ne" "nearest" "neqv" "new" "new_line" "nint" "non_intrinsic"
      "non_overridable" "none" "nopass" "not" "null" "nullify"
      "number_of_processors" "numeric_storage_size" "only" "onto" "open"
      "operator" "optional" "or" "output_unit" "pack" "parameter" "parity"
      "parity_prefix" "parity_scatter" "parity_suffix" "pass" "pause"
      "pointer" "popcnt" "poppar" "precision" "present" "print" "private"
      "procedure" "processors" "processors_shape" "product" "product_prefix"
      "product_scatter" "product_suffix" "program" "protected" "public"
      "pure" "radix" "random_number" "random_seed" "range" "read" "real"
      "realign" "recursive" "redistribute" "repeat" "reshape" "result"
      "return" "rewind" "rrspacing" "same_type_as" "save" "scale" "scan"
      "select" "selected_char_kind" "selected_int_kind" "selected_real_kind"
      "sequence" "set_exponent" "shape" "sign" "sin" "sinh" "size" "spacing"
      "spread" "sqrt" "stop" "subroutine" "sum" "sum_prefix" "sum_scatter"
      "sum_suffix" "system_clock" "tan" "tanh" "target" "template" "then"
      "tiny" "transfer" "transpose" "trim" "true" "type" "ubound" "unpack"
      "use" "value" "verify" "volatile" "wait" "where" "while" "with" "write"))
    (go-mode
     ;; 1. Keywords ref: https://golang.org/ref/spec#Keywords
     ;; 2. Builtin functions and types ref: https://golang.org/pkg/builtin/
     "append" "bool" "break" "byte" "cap" "case" "chan" "close" "complex" "complex128"
     "complex64" "const" "continue" "copy" "default" "defer" "delete" "else" "error"
     "fallthrough" "false" "float32" "float64" "for" "func" "go" "goto" "if" "imag"
     "import" "int" "int16" "int32" "int64" "int8" "interface" "len" "make"
     "map" "new" "nil" "package" "panic" "print" "println" "range" "real" "recover"
     "return" "rune" "select" "string" "struct" "switch" "true" "type" "uint" "uint16"
     "uint32" "uint64" "uint8" "uintptr" "var")
    (java-mode
     "abstract" "assert" "boolean" "break" "byte" "case" "catch" "char" "class"
     "continue" "default" "do" "double" "else" "enum" "extends" "final"
     "finally" "float" "for" "if" "implements" "import" "instanceof" "int"
     "interface" "long" "native" "new" "package" "private" "protected" "public"
     "return" "short" "static" "strictfp" "super" "switch" "synchronized"
     "this" "throw" "throws" "transient" "try" "void" "volatile" "while")
    (javascript-mode
     ;; https://tc39.github.io/ecma262/ + async, static and undefined
     "async" "await" "break" "case" "catch" "class" "const" "continue"
     "debugger" "default" "delete" "do" "else" "enum" "export" "extends" "false"
     "finally" "for" "function" "if" "import" "in" "instanceof" "let" "new"
     "null" "return" "static" "super" "switch" "this" "throw" "true" "try"
     "typeof" "undefined" "var" "void" "while" "with" "yield")
    (kotlin-mode
     "abstract" "annotation" "as" "break" "by" "catch" "class" "companion"
     "const" "constructor" "continue" "data" "do" "else" "enum" "false" "final"
     "finally" "for" "fun" "if" "import" "in" "init" "inner" "interface"
     "internal" "is" "lateinit" "nested" "null" "object" "open" "out" "override"
     "package" "private" "protected" "public" "return" "super" "this" "throw"
     "trait" "true" "try" "typealias" "val" "var" "when" "while")
    (lua-mode
     ;; https://www.lua.org/manual/5.3/manual.html
     "and" "break" "do" "else" "elseif" "end" "false" "for" "function" "goto" "if"
     "in" "local" "nil" "not" "or" "repeat" "return" "then" "true" "until" "while")
    (nim-mode
     ;; https://nim-lang.org/docs/manual.html#lexical-analysis-identifiers-amp-keywords
     "addr" "and" "as" "asm" "bind" "block" "break" "case" "cast" "concept" "const" "continue"
     "converter" "defer" "discard" "distinct" "div" "do" "elif" "else" "end" "enum" "except"
     "export" "finally" "for" "from" "func" "if" "import" "in" "include" "interface" "is" "isnot"
     "iterator" "let" "macro" "method" "mixin" "mod" "nil" "not" "notin" "object" "of" "or" "out"
     "proc" "ptr" "raise" "ref" "return" "shl" "shr" "static" "template" "try" "tuple" "type"
     "using" "var" "when" "while" "xor" "yield")
    (objc-mode
     "@catch" "@class" "@encode" "@end" "@finally" "@implementation"
     "@interface" "@private" "@protected" "@protocol" "@public"
     "@selector" "@synchronized" "@throw" "@try" "alloc" "autorelease"
     "bycopy" "byref" "in" "inout" "oneway" "out" "release" "retain")
    (perl-mode
     ;; from cperl.el
     "AUTOLOAD" "BEGIN" "CHECK" "CORE" "DESTROY" "END" "INIT" "__END__"
     "__FILE__" "__LINE__" "abs" "accept" "alarm" "and" "atan2" "bind"
     "binmode" "bless" "caller" "chdir" "chmod" "chomp" "chop" "chown" "chr"
     "chroot" "close" "closedir" "cmp" "connect" "continue" "cos"
     "crypt" "dbmclose" "dbmopen" "defined" "delete" "die" "do" "dump" "each"
     "else" "elsif" "endgrent" "endhostent" "endnetent" "endprotoent"
     "endpwent" "endservent" "eof" "eq" "eval" "exec" "exists" "exit" "exp"
     "fcntl" "fileno" "flock" "for" "foreach" "fork" "format" "formline"
     "ge" "getc" "getgrent" "getgrgid" "getgrnam" "gethostbyaddr"
     "gethostbyname" "gethostent" "getlogin" "getnetbyaddr" "getnetbyname"
     "getnetent" "getpeername" "getpgrp" "getppid" "getpriority"
     "getprotobyname" "getprotobynumber" "getprotoent" "getpwent" "getpwnam"
     "getpwuid" "getservbyname" "getservbyport" "getservent" "getsockname"
     "getsockopt" "glob" "gmtime" "goto" "grep" "gt" "hex" "if" "index" "int"
     "ioctl" "join" "keys" "kill" "last" "lc" "lcfirst" "le" "length"
     "link" "listen" "local" "localtime" "lock" "log" "lstat" "lt" "map"
     "mkdir" "msgctl" "msgget" "msgrcv" "msgsnd" "my" "ne" "next" "no"
     "not" "oct" "open" "opendir" "or" "ord" "our" "pack" "package" "pipe"
     "pop" "pos" "print" "printf" "push" "q" "qq" "quotemeta" "qw" "qx"
     "rand" "read" "readdir" "readline" "readlink" "readpipe" "recv" "redo"
     "ref" "rename" "require" "reset" "return" "reverse" "rewinddir" "rindex"
     "rmdir" "scalar" "seek" "seekdir" "select" "semctl" "semget" "semop"
     "send" "setgrent" "sethostent" "setnetent" "setpgrp" "setpriority"
     "setprotoent" "setpwent" "setservent" "setsockopt" "shift" "shmctl"
     "shmget" "shmread" "shmwrite" "shutdown" "sin" "sleep" "socket"
     "socketpair" "sort" "splice" "split" "sprintf" "sqrt" "srand" "stat"
     "study" "sub" "substr" "symlink" "syscall" "sysopen" "sysread" "system"
     "syswrite" "tell" "telldir" "tie" "time" "times" "tr" "truncate" "uc"
     "ucfirst" "umask" "undef" "unless" "unlink" "unpack" "unshift" "untie"
     "until" "use" "utime" "values" "vec" "wait" "waitpid"
     "wantarray" "warn" "while" "write" "x" "xor" "y")
    (php-mode ;; https://www.php.net/manual/reserved.php
     "Closure" "Error" "Exception" "Generator" "Throwable"
     "__CLASS__" "__DIR__" "__FILE__" "__FUNCTION__" "__LINE__" "__METHOD__"
     "__NAMESPACE__" "__TRAIT__"
     "abstract" "and" "array" "as" "bool" "break" "callable" "case" "catch"
     "class" "clone" "const" "continue" "declare" "default" "die" "do" "echo"
     "else" "elseif" "empty" "enddeclare" "endfor" "endforeach" "endif"
     "endswitch" "endwhile" "enum" "eval" "exit" "extends" "false" "final" "finally"
     "float" "fn" "for" "foreach" "function" "global" "goto" "if"
     "implements" "include" "include_once" "instanceof" "insteadof" "interface"
     "isset" "iterable" "list" "match" "namespace" "new" "null" "object" "or"
     "print" "private" "protected" "public" "readonly" "require" "require_once"
     "return" "self" "static" "string" "switch" "this" "throw" "trait" "true"
     "try" "unset" "use" "var" "void" "while" "xor" "yield" "yield from")
    (purescript-mode ;; purescript-font-lock.el
     "ado" "case" "class" "data" "default" "deriving" "do" "else" "if" "import"
     "in" "infix" "infixl" "infixr" "instance" "let" "module" "newtype" "of"
     "then" "type" "where")
    (python-mode
     ;; https://docs.python.org/3/reference/lexical_analysis.html#keywords
     "False" "None" "True" "and" "as" "assert" "break" "class" "continue" "def"
     "del" "elif" "else" "except" "exec" "finally" "for" "from" "global" "if"
     "import" "in" "is" "lambda" "nonlocal" "not" "or" "pass" "print" "raise"
     "return" "try" "while" "with" "yield")
    (ruby-mode
     "BEGIN" "END" "alias" "and"  "begin" "break" "case" "class" "def" "defined?"
     "do" "else" "elsif"  "end" "ensure" "false" "for" "if" "in" "module"
     "next" "nil" "not" "or" "redo" "rescue" "retry" "return" "self" "super"
     "then" "true" "undef" "unless" "until" "when" "while" "yield")
    ;; From https://doc.rust-lang.org/grammar.html#keywords
    ;; but excluding unused reserved words: https://www.reddit.com/r/rust/comments/34fq0k/is_there_a_good_list_of_rusts_keywords/cqucvnj
    (rust-mode
     "Self"
     "as" "box" "break" "const" "continue" "crate" "else" "enum" "extern"
     "false" "fn" "for" "if" "impl" "in" "let" "loop" "macro" "match" "mod"
     "move" "mut" "pub" "ref" "return" "self" "static" "struct" "super"
     "trait" "true" "type" "unsafe" "use" "where" "while")
    ; Extract from R7RS-small Tex: https://small.r7rs.org/
    (scheme-mode
     "abs" "acos" "angle" "append" "apply" "asin" "assoc" "assq" "assv"
     "atan" "binary-port?" "body" "boolean=?" "boolean?" "bytevector"
     "bytevector-append" "bytevector-copy" "bytevector-copy!"
     "bytevector-length" "bytevector-u8-ref" "bytevector-u8-set!"
     "bytevector?" "caaaar" "caaadr" "caaar" "caadar" "caaddr" "caadr"
     "caar" "cadaar" "cadadr" "cadar" "caddar" "cadddr" "caddr" "cadr"
     "call-with-port" "call-with-values" "car" "car-internal" "cdaaar"
     "cdaadr" "cdaar" "cdadar" "cdaddr" "cdadr" "cdar" "cddaar" "cddadr"
     "cddar" "cdddar" "cddddr" "cdddr" "cddr" "cdr" "ceiling"
     "char->integer" "char-alphabetic?" "char-ci<=?" "char-ci<?"
     "char-ci=?" "char-ci>=?" "char-ci>?" "char-downcase" "char-foldcase"
     "char-lower-case?" "char-numeric?" "char-ready?" "char-upcase"
     "char-upper-case?" "char-whitespace?" "char<=?" "char<?" "char=?"
     "char>=?" "char>?" "char?" "close-input-port" "close-output-port"
     "close-port" "command-line" "complex?" "cons" "cos"
     "current-error-port" "current-input-port" "current-jiffy"
     "current-output-port" "current-second" "delete-file" "denominator"
     "digit-value" "display" "dynamic-wind" "emergency-exit" "environment"
     "eof-object" "eof-object?" "eq?" "equal?" "eqv?" "error"
     "error-object-irritants" "error-object-message" "error-object?" "eval"
     "even?" "exact" "exact-integer-sqrt" "exact-integer?" "exact?" "exit"
     "exp" "expt" "features" "file-error?" "file-exists?" "finite?" "floor"
     "floor-quotient" "floor-remainder" "floor/" "flush-output-port" "gcd"
     "get-environment-variable" "get-environment-variables"
     "get-output-bytevector" "get-output-string" "imag-part" "inexact"
     "inexact?" "infinite?" "input-port-open?" "input-port?"
     "integer->char" "integer?" "interaction-environment"
     "jiffies-per-second" "lcm" "length" "list" "list->string"
     "list->vector" "list-copy" "list-ref" "list-set!" "list-tail" "list?"
     "load" "log" "magnitude" "make-bytevector" "make-list"
     "make-parameter" "make-polar" "make-promise" "make-rectangular"
     "make-string" "make-vector" "max" "member" "memq" "memv" "min"
     "modulo" "nan?" "negative?" "newline" "nil" "not" "null-environment"
     "null?" "number->string" "number?" "numerator" "odd?"
     "open-binary-input-file" "open-binary-output-file"
     "open-input-bytevector" "open-input-file" "open-input-string"
     "open-output-bytevector" "open-output-file" "open-output-string"
     "output-port-open?" "output-port?" "pair?" "peek-char" "peek-u8"
     "port?" "positive?" "procedure?" "promise?" "quasiquote" "quote"
     "quotient" "raise" "raise-continuable" "rational?" "rationalize"
     "read" "read-bytevector" "read-bytevector!" "read-char" "read-error?"
     "read-line" "read-string" "read-u8" "real-part" "real?" "remainder"
     "reverse" "round" "scheme-report-environment" "set!" "set-car!"
     "set-cdr!" "setcar" "sin" "sqrt" "square" "string" "string->list"
     "string->number" "string->symbol" "string->utf" "string->vector"
     "string-append" "string-ci<=?" "string-ci<?" "string-ci=?"
     "string-ci>=?" "string-ci>?" "string-copy" "string-copy!"
     "string-downcase" "string-fill!" "string-foldcase" "string-for-each"
     "string-length" "string-map" "string-ref" "string-set!"
     "string-upcase" "string<=?" "string<?" "string=?" "string>=?"
     "string>?" "string?" "substring" "symbol->string" "symbol=?" "symbol?"
     "tan" "textual-port?" "truncate" "truncate-quotient"
     "truncate-remainder" "truncate/" "u8-ready?" "unquote"
     "unquote-splicing" "utf->string" "values" "vector" "vector->list"
     "vector->string" "vector-append" "vector-copy" "vector-copy!"
     "vector-fill!" "vector-for-each" "vector-length" "vector-map"
     "vector-ref" "vector-set!" "vector?" "with-exception-handler"
     "with-input-from-file" "with-output-to-file" "write"
     "write-bytevector" "write-char" "write-shared" "write-simple"
     "write-string" "write-u8" "zero?")
    (scala-mode
     "abstract" "case" "catch" "class" "def" "do" "else" "extends" "false"
     "final" "finally" "for" "forSome" "if" "implicit" "import" "lazy" "match"
     "new" "null" "object" "override" "package" "private" "protected"
     "return" "sealed" "super" "this" "throw" "trait" "true" "try" "type" "val"
     "var" "while" "with" "yield")
    (swift-mode
     "Protocol" "Self" "Type" "and" "as" "assignment" "associatedtype"
     "associativity" "available" "break" "case" "catch" "class" "column" "continue"
     "convenience" "default" "defer" "deinit" "didSet" "do" "dynamic" "dynamicType"
     "else" "elseif" "endif" "enum" "extension" "fallthrough" "false" "file"
     "fileprivate" "final" "for" "func" "function" "get" "guard" "higherThan" "if"
     "import" "in" "indirect" "infix" "init" "inout" "internal" "is" "lazy" "left"
     "let" "line" "lowerThan" "mutating" "nil" "none" "nonmutating" "open"
     "operator" "optional" "override" "postfix" "precedence" "precedencegroup"
     "prefix" "private" "protocol" "public" "repeat" "required" "rethrows" "return"
     "right" "selector" "self" "set" "static" "struct" "subscript" "super" "switch"
     "throw" "throws" "true" "try" "typealias" "unowned" "var" "weak" "where"
     "while" "willSet")
    (julia-mode
     "abstract" "break" "case" "catch" "const" "continue" "do" "else" "elseif"
     "end" "eval" "export" "false" "finally" "for" "function" "global" "if"
     "ifelse" "immutable" "import" "importall" "in" "let" "macro" "module"
     "otherwise" "quote" "return" "switch" "throw" "true" "try" "type"
     "typealias" "using" "while"
     )
    ;; From https://github.com/apache/thrift/blob/master/contrib/thrift.el
    (thrift-mode
     "binary" "bool" "byte" "const" "double" "enum" "exception" "extends"
     "i16" "i32" "i64" "include" "list" "map" "oneway" "optional" "required"
     "service" "set" "string" "struct" "throws" "typedef" "void"
     )
    (tuareg-mode 
     ;; ocaml, from https://v2.ocaml.org/manual/lex.html#sss:keywords
     "and" "as" "asr" "assert" "begin" "class"
     "constraint" "do" "done" "downto" "else" "end" 
     "exception" "external" "false" "for" "fun" "function"
     "functor" "if" "in" "include" "inherit" "initializer"
     "land" "lazy" "let" "lor" "lsl" "lsr"
     "lxor" "match" "method" "mod" "module" "mutable"
     "new" "nonrec" "object" "of" "open" "or"
     "private" "rec" "sig" "struct" "then" "to" 
     "true" "try" "type" "val" "virtual" "when" 
     "while" "with" 
    )
    ;; aliases
    (caml-mode . tuareg-mode)
    (js2-mode . javascript-mode)
    (js2-jsx-mode . javascript-mode)
    (espresso-mode . javascript-mode)
    (js-mode . javascript-mode)
    (js-jsx-mode . javascript-mode)
    (rjsx-mode . javascript-mode)
    (cperl-mode . perl-mode)
    (jde-mode . java-mode)
    (ess-julia-mode . julia-mode)
    (php-ts-mode . php-mode)
    (phps-mode . php-mode)
    (enh-ruby-mode . ruby-mode))
  "Alist mapping major-modes to sorted keywords for `company-keywords'.")

(with-eval-after-load 'make-mode
  (mapc
   (lambda (mode-stmnts)
     (setf (alist-get (car mode-stmnts) company-keywords-alist)
           (cl-remove-duplicates
            (sort (append makefile-special-targets-list
                          (cl-mapcan #'identity
                                     (mapcar
                                      #'split-string
                                      (cl-remove-if-not
                                       #'stringp
                                       (symbol-value (cdr mode-stmnts))))))
                  #'string<)
            :test #'string=)))
   '((makefile-automake-mode . makefile-automake-statements)
     (makefile-gmake-mode    . makefile-gmake-statements)
     (makefile-makepp-mode   . makefile-makepp-statements)
     (makefile-bsdmake-mode  . makefile-bsdmake-statements)
     (makefile-imake-mode    . makefile-statements)
     (makefile-mode          . makefile-statements))))

;;;###autoload
(defun company-keywords (command &optional arg &rest _ignored)
  "`company-mode' backend for programming language keywords."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-keywords))
    (prefix (and (assq major-mode company-keywords-alist)
                 (not (company-in-string-or-comment))
                 (or (company-grab-symbol) 'stop)))
    (candidates
     (let ((completion-ignore-case company-keywords-ignore-case)
           (symbols (cdr (assq major-mode company-keywords-alist))))
       (all-completions arg (if (consp symbols)
                                symbols
                              (cdr (assq symbols company-keywords-alist))))))
    (kind 'keyword)
    (sorted t)
    (ignore-case company-keywords-ignore-case)))

(provide 'company-keywords)
;;; company-keywords.el ends here
