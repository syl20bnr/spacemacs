;;; f-shortdoc.el --- Shortdoc for f.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; Author: Lucien Cartier-Tilet <lucien@phundrak.com>
;; Maintainer: Lucien Cartier-Tilet <lucien@phundrak.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Homepage: https://github.com/rejeep/f.el

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Shortdoc implementation for f.el

;;; Code:

(when (version<= "28.1" emacs-version)
  (require 'shortdoc)

  (define-short-documentation-group f
    "Paths"
    (f-join
     :eval (f-join "path")
     :eval (f-join "path" "to")
     :eval (f-join "/" "path" "to" "heaven")
     :eval (f-join "path" "/to" "file"))

    (f-split
     :eval (f-split "path")
     :eval (f-split "path/to")
     :eval (f-split "/path/to/heaven")
     :eval (f-split "~/back/to/earth"))

    (f-expand
     :no-eval (f-expand "name")
     :result-string "/default/directory/name"
     :no-eval (f-expand "name" "other/directory")
     :result-string "other/directory/name")

    (f-filename
     :eval (f-filename "path/to/file.ext")
     :eval (f-filename "path/to/directory"))

    (f-dirname
     :eval (f-dirname "path/to/file.ext")
     :eval (f-dirname "path/to/directory")
     :eval (f-dirname "/"))

    (f-common-parent
     :eval (f-common-parent '("foo/bar/baz" "foo/bar/qux" "foo/bar/mux"))
     :eval (f-common-parent '("/foo/bar/baz" "/foo/bar/qux" "/foo/bax/mux"))
     :eval (f-common-parent '("foo/bar/baz" "quack/bar/qux" "lack/bar/mux")))

    (f-ext
     :eval (f-ext "path/to/file")
     :eval (f-ext "path/to/file.txt")
     :eval (f-ext "path/to/file.txt.org"))

    (f-no-ext
     :eval (f-no-ext "path/to/file")
     :eval (f-no-ext "path/to/file.txt")
     :eval (f-no-ext "path/to/file.txt.org"))

    (f-swap-ext
     :eval (f-swap-ext "path/to/file.ext" "org"))

    (f-base
     :eval (f-base "path/to/file.ext")
     :eval (f-base "path/to/directory"))

    (f-relative
     :eval (f-relative "/some/path/relative/to/my/file.txt" "/some/path/")
     :eval (f-relative "/default/directory/my/file.txt"))

    (f-short
     :no-eval (f-short "/Users/foo/Code/on/macOS")
     :result-string "~/Code/on/macOS"
     :no-eval (f-short "/home/foo/Code/on/linux")
     :result-string "~/Code/on/linux"
     :eval (f-short "/path/to/Code/bar"))

    (f-long
     :eval (f-long "~/Code/bar")
     :eval (f-long "/path/to/Code/bar"))

    (f-canonical
     :eval (f-canonical "/path/to/real/file")
     :no-eval (f-canonical "/link/to/file")
     :result-string "/path/to/real/file")

    (f-slash
     :no-eval (f-slash "/path/to/file")
     :result-string "/path/to/file"
     :no-eval (f-slash "/path/to/dir")
     :result-string "/path/to/dir/"
     :no-eval (f-slash "/path/to/dir/")
     :result-string "/path/to/dir/")

    (f-full
     :eval (f-full "~/path/to/file")
     :eval (f-full "~/path/to/dir")
     :eval (f-full "~/path/to/dir/"))

    (f-uniquify
     :eval (f-uniquify '("/foo/bar" "/foo/baz" "/foo/quux"))
     :eval (f-uniquify '("/foo/bar" "/www/bar" "/foo/quux"))
     :eval (f-uniquify '("/foo/bar" "/www/bar" "/www/bar/quux"))
     :eval (f-uniquify '("/foo/bar" "/foo/baz" "/home/www/bar" "/home/www/baz" "/var/foo" "/opt/foo/www/baz")))

    (f-uniquify-alist
     :eval (f-uniquify-alist '("/foo/bar" "/foo/baz" "/foo/quux"))
     :eval (f-uniquify-alist '("/foo/bar" "/www/bar" "/foo/quux"))
     :eval (f-uniquify-alist '("/foo/bar" "/www/bar" "/www/bar/quux"))
     :eval (f-uniquify-alist '("/foo/bar" "/foo/baz" "/home/www/bar" "/home/www/baz" "/var/foo" "/opt/foo/www/baz")))

    "I/O"
    (f-read-bytes
     :no-eval* (f-read-bytes "path/to/binary/data"))

    (f-write-bytes
     :no-eval* (f-write-bytes (unibyte-string 72 101 108 108 111 32 119 111 114 108 100) "path/to/binary/data"))

    (f-append-bytes
     :no-eval* (f-append-bytes "path/to/file" (unibyte-string 72 101 108 108 111 32 119 111 114 108 100)))

    (f-read-text
     :no-eval* (f-read-text "path/to/file.txt" 'utf-8)
     :no-eval* (f-read "path/to/file.txt" 'utf-8))

    (f-write-text
     :no-eval* (f-write-text "Hello world" 'utf-8 "path/to/file.txt")
     :no-eval* (f-write "Hello world" 'utf-8 "path/to/file.txt"))

    (f-append-text
     :no-eval* (f-append-text "Hello world" 'utf-8 "path/to/file.txt")
     :no-eval* (f-append "Hello world" 'utf-8 "path/to/file.txt"))

    "Destructive"
    (f-mkdir
     :no-eval (f-mkdir "dir")
     :result-string "creates /default/directory/dir"
     :no-eval (f-mkdir "other" "dir")
     :result-string "creates /default/directory/other/dir"
     :no-eval (f-mkdir "/" "some" "path")
     :result-string "creates /some/path"
     :no-eval (f-mkdir "~" "yet" "another" "dir")
     :result-string "creates ~/yet/another/dir")

    (f-mkdir-full-path
     :no-eval (f-mkdir-full-path "dir")
     :result-string "creates /default/directory/dir"
     :no-eval (f-mkdir-full-path "other/dir")
     :result-string "creates /default/directory/other/dir"
     :no-eval (f-mkdir-full-path "/some/path")
     :result-string "creates /some/path"
     :no-eval (f-mkdir-full-path "~/yet/another/dir")
     :result-string "creates ~/yet/another/dir")

    (f-delete
     :no-eval* (f-delete "dir")
     :no-eval* (f-delete "other/dir" t)
     :no-eval* (f-delete "path/to/file.txt"))

    (f-symlink
     :no-eval* (f-symlink "path/to/source" "path/to/link"))

    (f-move
     :no-eval* (f-move "path/to/file.txt" "new-file.txt")
     :no-eval* (f-move "path/to/file.txt" "other/path"))

    (f-copy
     :no-eval* (f-copy "path/to/file.txt" "new-file.txt")
     :no-eval* (f-copy "path/to/dir" "other/dir"))

    (f-copy-contents
     :no-eval* (f-copy-contents "path/to/dir" "path/to/other/dir"))

    (f-touch
     :no-eval* (f-touch "path/to/existing/file.txt")
     :no-eval* (f-touch "path/to/non/existing/file.txt"))

    "Predicates"
    (f-exists-p
     :no-eval* (f-exists-p "path/to/file.txt")
     :no-eval* (f-exists-p "path/to/dir"))

    (f-directory-p
     :no-eval* (f-directory-p "path/to/file.txt")
     :no-eval* (f-directory-p "path/to/dir"))

    (f-file-p
     :no-eval* (f-file-p "path/to/file.txt")
     :no-eval* (f-file-p "path/to/dir"))

    (f-symlink-p
     :no-eval* (f-symlink-p "path/to/file.txt")
     :no-eval* (f-symlink-p "path/to/dir")
     :no-eval* (f-symlink-p "path/to/link"))

    (f-readable-p
     :no-eval* (f-readable-p "path/to/file.txt")
     :no-eval* (f-readable-p "path/to/dir"))

    (f-writable-p
     :no-eval* (f-writable-p "path/to/file.txt")
     :no-eval* (f-writable-p "path/to/dir"))

    (f-executable-p
     :no-eval* (f-executable-p "path/to/file.txt")
     :no-eval* (f-executable-p "path/to/dir"))

    (f-absolute-p
     :eval (f-absolute-p "path/to/dir")
     :eval (f-absolute-p "/full/path/to/dir"))

    (f-relative-p
     :eval (f-relative-p "path/to/dir")
     :eval (f-relative-p "/full/path/to/dir"))

    (f-root-p
     :eval (f-root-p "/")
     :eval (f-root-p "/not/root"))

    (f-ext-p
     :eval (f-ext-p "path/to/file.el" "el")
     :eval (f-ext-p "path/to/file.el" "txt")
     :eval (f-ext-p "path/to/file.el")
     :eval (f-ext-p "path/to/file"))

    (f-same-p
     :eval (f-same-p "foo.txt" "foo.txt")
     :eval (f-same-p "foo/bar/../baz" "foo/baz")
     :eval (f-same-p "/path/to/foo.txt" "/path/to/bar.txt"))

    (f-parent-of-p
     :no-eval (f-parent-of-p "/path/to" "/path/to/dir")
     :result t
     :no-eval (f-parent-of-p "/path/to/dir" "/path/to")
     :result nil
     :no-eval (f-parent-of-p "/path/to" "/path/to")
     :result nil)

    (f-child-of-p
     :no-eval (f-child-of-p "/path/to" "/path/to/dir")
     :result nil
     :no-eval (f-child-of-p "/path/to/dir" "/path/to")
     :result t
     :no-eval (f-child-of-p "/path/to" "/path/to")
     :result nil)

    (f-ancestor-of-p
     :no-eval (f-ancestor-of-p "/path/to" "/path/to/dir")
     :result t
     :no-eval (f-ancestor-of-p "/path" "/path/to/dir")
     :result t
     :no-eval (f-ancestor-of-p "/path/to/dir" "/path/to")
     :result nil
     :no-eval (f-ancestor-of-p "/path/to" "/path/to")
     :result nil)

    (f-descendant-of-p
     :no-eval (f-descendant-of-p "/path/to/dir" "/path/to")
     :result t
     :no-eval (f-descendant-of-p "/path/to/dir" "/path")
     :result t
     :no-eval (f-descendant-of-p "/path/to" "/path/to/dir")
     :result nil
     :no-eval (f-descendant-of-p "/path/to" "/path/to")
     :result nil)

    (f-hidden-p
     :eval (f-hidden-p "path/to/foo")
     :eval (f-hidden-p ".path/to/foo")
     :eval (f-hidden-p "path/.to/foo")
     :eval (f-hidden-p "path/to/.foo")
     :eval (f-hidden-p ".path/to/foo" 'any)
     :eval (f-hidden-p "path/.to/foo" 'any)
     :eval (f-hidden-p "path/to/.foo" 'any)
     :eval (f-hidden-p ".path/to/foo" 'last)
     :eval (f-hidden-p "path/.to/foo" 'last)
     :eval (f-hidden-p "path/to/.foo" 'last))

    (f-empty-p
     :no-eval (f-empty-p "/path/to/empty-file")
     :result t
     :no-eval (f-empty-p "/path/to/file-with-contents")
     :result nil
     :no-eval (f-empty-p "/path/to/empty-dir/")
     :result t
     :no-eval (f-empty-p "/path/to/dir-with-contents/")
     :result nil)

    (f-older-p
     :noeval (f-older-p "older-file.txt" "newer-file.txt")
     :result t
     :noeval (f-older-p "newer-file.txt" "older-file.txt")
     :result nil
     :noeval (f-older-p "same-time1.txt" "same-time2.txt")
     :result nil)

    (f-newer-p
     :noeval (f-newer-p "newer-file.txt" "older-file.txt")
     :result t
     :noeval (f-newer-p "older-file.txt" "newer-file.txt")
     :result nil
     :noeval (f-newer-p "same-time1.txt" "same-time2.txt")
     :result nil)

    (f-same-time-p
     :noeval (f-same-time-p "same-time1.txt" "same-time2.txt")
     :result t
     :noeval (f-same-time-p "newer-file.txt" "older-file.txt")
     :result nil
     :noeval (f-same-time-p "older-file.txt" "newer-file.txt")
     :result nil)

    "Stats"
    (f-size
     :no-eval* (f-size "path/to/file.txt")
     :no-eval* (f-size "path/to/dir"))

    (f-depth
     :eval (f-depth "/")
     :eval (f-depth "/var/")
     :eval (f-depth "/usr/local/bin"))

    (f-change-time
     :no-eval (f-change-time "path/to/file.txt")
     :result (25517 48756 26337 111000)
     :no-eval (f-change-time "path/to/dir")
     :result (25517 57887 344657 210000)
     :no-eval (f-change-time "path/to/file.txt" t)
     :result (1672330868026337111 . 1000000000)
     :no-eval (f-change-time "path/to/dir" t)
     :result (1672339999344657210 . 1000000000)
     :no-eval (f-change-time "path/to/file.txt" 'seconds)
     :result 1672330868
     :no-eval (f-change-time "path/to/dir" 'seconds)
     :result 1672339999)

    (f-modification-time
     :no-eval (f-modification-time "path/to/file.txt")
     :result (25517 48756 26337 111000)
     :no-eval (f-modification-time "path/to/dir")
     :result (25517 57887 344657 210000)
     :no-eval (f-modification-time "path/to/file.txt" t)
     :result (1672330868026337111 . 1000000000)
     :no-eval (f-modification-time "path/to/dir" t)
     :result (1672339999344657210 . 1000000000)
     :no-eval (f-modification-time "path/to/file.txt" 'seconds)
     :result 1672330868
     :no-eval (f-modification-time "path/to/dir" 'seconds)
     :result 1672339999)

    (f-access-time
     :no-eval (f-access-time "path/to/file.txt")
     :result (25517 48756 26337 111000)
     :no-eval (f-access-time "path/to/dir")
     :result (25517 57887 344657 210000)
     :no-eval (f-access-time "path/to/file.txt" t)
     :result (1672330868026337111 . 1000000000)
     :no-eval (f-access-time "path/to/dir" t)
     :result (1672339999344657210 . 1000000000)
     :no-eval (f-access-time "path/to/file.txt" 'seconds)
     :result 1672330868
     :no-eval (f-access-time "path/to/dir" 'seconds)
     :result 1672339999)

    "Misc"
    (f-this-file
     :no-eval* (f-this-file))

    (f-path-separator
     :eval (f-path-separator))

    (f-glob
     :no-eval* (f-glob "path/to/*.el")
     :no-eval* (f-glob "*.el" "path/to"))

    (f-entries
     :no-eval* (f-entries "path/to/dir")
     :no-eval* (f-entries "path/to/dir" (lambda (file) (s-matches? "test" file)))
     :no-eval* (f-entries "path/to/dir" nil t)
     :no-eval* (f--entries "path/to/dir" (s-matches? "test" it)))

    (f-directories
     :no-eval* (f-directories "path/to/dir")
     :no-eval* (f-directories "path/to/dir" (lambda (dir) (equal (f-filename dir) "test")))
     :no-eval* (f-directories "path/to/dir" nil t)
     :no-eval* (f--directories "path/to/dir" (equal (f-filename it) "test")))

    (f-files
     :no-eval* (f-files "path/to/dir")
     :no-eval* (f-files "path/to/dir" (lambda (file) (equal (f-ext file) "el")))
     :no-eval* (f-files "path/to/dir" nil t)
     :no-eval* (f--files "path/to/dir" (equal (f-ext it) "el")))

    (f-root
     :eval (f-root))

    (f-traverse-upwards
     :no-eval* (f-traverse-upwards
                (lambda (path)
                  (f-exists? (f-expand ".git" path)))
                start-path)

     :no-eval* (f--traverse-upwards (f-exists? (f-expand ".git" it)) start-path))

    (f-with-sandbox
     :no-eval (f-with-sandbox foo-path
                (f-touch (f-expand "foo" foo-path)))
     :no-eval (f-with-sandbox (list foo-path bar-path)
                (f-touch (f-expand "foo" foo-path))
                (f-touch (f-expand "bar" bar-path)))
     :no-eval (f-with-sandbox foo-path
                (f-touch (f-expand "bar" bar-path)))))) ;; "Destructive operation outside sandbox"

(eval-when-compile
  (when (version< emacs-version "28.1")
    (warn "Emacs should not be compiling this file")))

(provide 'f-shortdoc)

;;; f-shortdoc.el ends here
