;;; funcs.el --- Spacemacs Base Layer functions File
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs-docker//dump-layers-data ()
  "Save used layers and their configurations to
the `spacemacs-docker--default-dump-layer-data-file-path' file"
  (with-temp-file docker-spacemacs--default-dump-layer-data-file-path
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
                   (fp (when (boundp symbol) (symbol-file symbol))))
              (when (or (and fp
                             (string-prefix-p
                              (configuration-layer/get-layer-path layer)
                              fp))
                        (string-match-p (regexp-quote "docker-spacemacs-")
                                        name))
                (insert (format "%-28s(%s . %s)\n"
                                ""
                                name
                                (symbol-value symbol))))))))
      (insert "))\n\n"))))

(defun spacemacs-docker//test-dotfile ()
  "Run `dotspacemacs/test-dotfile' and `kill-emacs' with
the code 1 if tests failed."
  (unless (dotspacemacs/test-dotfile)
    (message "dotspacemacs/test-dotfile FAILED")
    (kill-emacs 1)))
