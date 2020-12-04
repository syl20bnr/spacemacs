;;; funcs.el --- search-engine Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs//search-engine-source (engines)
  "return a source for helm selection"
  `((name . "Search Engines")
    (candidates . ,(mapcar (lambda (engine)
                             (cons (plist-get (cdr engine) :name)
                                   (intern (format "engine/search-%S"
                                                   (car engine)))))
                           engines))
    (action . (lambda (candidate) (call-interactively candidate)))))

(defun space-macs/helm-search-engine-select ()
  "Set search engine to use with helm."
  (interactive)
  (helm :sources (list (space-macs//search-engine-source
                        search-engine-alist))))

(defun space-macs/ivy-search-engine-select ()
  "Set search engine to use with ivy."
  (interactive)
  (ivy-read "Search Engines: "
            (mapcar (lambda (engine)
                      (cons (plist-get (cdr engine) :name)
                            (intern (format "engine/search-%S"
                                            (car engine)))))
                    search-engine-alist)
            :action (lambda (candidate) (call-interactively (cdr candidate)))))

(defun space-macs/search-engine-select ()
  "Set search engine to use."
  (interactive)
  (if (configuration-layer/layer-used-p 'ivy)
      (call-interactively 'space-macs/ivy-search-engine-select)
    (call-interactively 'space-macs/helm-search-engine-select)))


