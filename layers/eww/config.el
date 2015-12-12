;;; config.el --- eww Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Andrea Moretti <axyzxp@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variables

(defvar eww--search-engines
  '((google . "https://www.google.com/search?q=")
    (stackoverflow . "http://stackoverflow.com/search?q=")
    (bing . "https://www.bing.com/search?q=")
    (answers . "https://answers.yahoo.com/search/search_result?p=")
    (yahoo . "https://search.yahoo.com/search?p=")
    (duckduckgo . "https://duckduckgo.com/html/?q=")))

(defvar default-search-engine 'google)
