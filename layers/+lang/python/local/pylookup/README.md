# Author

 Taesoo Kim (taesoo@mit.edu)

 Michael Markert (markert.michael@gmail.com)
 Austin Bingham (austin.bingham@gmail.com)
 Takafumi Arakaki <aka.tkf@gmail.com>

# README

 Pylookup stole idea from 'http://furius.ca/haddoc', one of my favorite
 emacs mode for python documentation lookup. I reimplemented python code and
 elisp code not just to support new version of python 2.7 but also to extend
 it for other documentation lookup interfaces with easy. Importantly, pylookup
 mode is much faster and supports fancy highlighting.

 Please check,
    Web  : http://taesoo.org/proj/pylookup.html
    Repo : https://github.com/tsgates/pylookup

# INSTALL

## Create database

 You can browse python documents from either online or offline. Since I prefer
 offline, here is an easy step:

     make download

 It will download python document, and construct database for you. If you get in
 any trouble, follow the below steps manually:

 1. Download your own version of python document
   (i.e. http://docs.python.org/archives/python-2.7.2-docs-html.zip)
 2. Unzip: 'unzip python-2.7.2-docs-html.zip'
 3. Index: './pylookup.py -u python-2.7.1-docs-html'
 4. Test : './pylookup.py -l ljust'

 (see updateing database section for more options)

## Elisp

 Here is lisp part for emacs.

 - [PATH] parameter depends on your environment (i.e. "~/.emacs.d/pylookup")

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.cl}
;; add pylookup to your loadpath, ex) ~/.emacs.d/pylookup
(setq pylookup-dir "[PATH]")
(add-to-list 'load-path pylookup-dir)

;; load pylookup when compile time
(eval-when-compile (require 'pylookup))

;; set executable file and db file
(setq pylookup-program (concat pylookup-dir "/pylookup.py"))
(setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))

;; set search option if you want
;; (setq pylookup-search-options '("--insensitive" "0" "--desc" "0"))

;; to speedup, just load it on demand
(autoload 'pylookup-lookup "pylookup"
  "Lookup SEARCH-TERM in the Python HTML indexes." t)

(autoload 'pylookup-update "pylookup"
  "Run pylookup-update and create the database at `pylookup-db-file'." t)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Updating Databases

 You can easily accumulate many sources into single database. For example, you
 can index python and scipy at the same time. Here are the examples:

 - Single source
  ./pylookup.py -u http://docs.python.org
 - Multiple sources, remote and local
  ./pylookup.py -u http://docs.python.org -u ~/doc/python2.7
 - Adding local source to existing database (duplicate entries are not checked)
  ./pylookup.py -a -u ~/doc/python
 - Example online documents of python, scipy, numpy, and matplotlib
  (you can append new indexes into the current db with '-a' option)
  ./pylookup.py -u http://docs.python.org
  ./pylookup.py -u http://docs.scipy.org/doc/numpy/genindex.html
  ./pylookup.py -u http://docs.scipy.org/doc/scipy/reference/genindex.html
  ./pylookup.py -u http://matplotlib.sourceforge.net/genindex.html

 You probably like to type './pylookup.py -h' to see more options.
