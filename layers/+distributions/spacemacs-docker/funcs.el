;;; funcs.el --- Spacemacs Base Layer functions File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs-docker//dump-layers-data ()
  "Save used layers and their configurations to
the `spacemacs-docker-dump-layer-data-fp' file"
  (with-temp-file spacemacs-docker-dump-layer-data-fp
    (let ((used-layers (mapcar (lambda (el)
                                 (or (car-safe el)
                                     el))
                               dotspacemacs-configuration-layers)))
      (insert (format "(setq used-layers '(%-20s%s))\n\n"
                      "\n"
                      (mapconcat 'symbol-name
                                 used-layers
                                 (format "%-20s"
                                         "\n"))))
      (insert "(setq used-layers-configs '(\n")
      (dolist (layer used-layers)
        (let ((names (all-completions (format "%s-"
                                              layer)
                                      obarray)))
          (dolist (name names)
            (let* ((symbol (intern-soft name))
                   (fp (when (boundp symbol) (symbol-file symbol)))
                   (sym-val (bound-and-true-p symbol)))
              (when (or (and fp
                             (string-prefix-p
                              (configuration-layer/get-layer-path layer)
                              fp))
                        (string-match-p (regexp-quote "spacemacs-docker")
                                        name))
                (insert (format "%-28s(%s . %s)\n"
                                ""
                                name
                                (if (stringp sym-val)
                                    (format "\"%s\"" sym-val)
                                  sym-val))))))))
      (insert "))\n\n"))))
