(require 'auto-complete-clang)

(setq ac-clang-flags
      (mapcar (lambda (item)(concat "-I" item))
              (split-string
               "
 /usr/include/c++/4.7
 /usr/include/i386-linux-gnu/c++/4.7/.
 /usr/include/c++/4.7/backward
 /usr/lib/gcc/i686-linux-gnu/4.7/include
 /usr/local/include
 /usr/lib/gcc/i686-linux-gnu/4.7/include-fixed
 /usr/include/i386-linux-gnu
 /usr/include
"
               )))
