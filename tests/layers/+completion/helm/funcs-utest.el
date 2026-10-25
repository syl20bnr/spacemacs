;;; funcs-utest.el --- Helm layer unit tests -*- lexical-binding: nil; -*-

(require 'ert)
(require 'cl-lib)

(load (expand-file-name "../../../../layers/+completion/helm/funcs.el"
                         (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(provide 'helm-ag)
(defvar helm-ag-insert-at-point nil)

(ert-deftest helm-do-ag-region-or-symbol-forwards-no-properties ()
  (let (thing-at-point-arguments)
    (with-temp-buffer
      (cl-letf (((symbol-function 'thing-at-point)
                 (lambda (thing &optional no-properties)
                   (setq thing-at-point-arguments (list thing no-properties))
                   "search input"))
                ((symbol-function 'rxt-quote-pcre) #'identity))
        (should
         (equal "search input"
                (spacemacs//helm-do-ag-region-or-symbol
                 (lambda (_dir) (thing-at-point 'symbol t)))))
        (should (equal '(symbol t) thing-at-point-arguments))))))

(provide 'funcs-utest)
