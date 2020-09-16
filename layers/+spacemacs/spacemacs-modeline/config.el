;;; config.el --- Spacemacs Mode-line Visual Layer configuration File
;;
;; Copyright (c) 2020 Sylvain Benner & Contributors
;;
;; Author: Riccardo Murri <riccardo.murri@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defcustom spacemacs-spaceline-additional-segments
  '((new-version :when active))
  "Additional segments for the Spacemacs modeline.

They are inserted in the modeline between `global' and
`buffer-position'.

Must be a list of valid segments; see `spaceline-install' for
more information on what constitutes a valid segment."
  :type '(repeat sexp)
  :group 'spacemacs)
