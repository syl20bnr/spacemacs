;; Pre extensions are loaded *before* the packages
(defvar syl20bnr-pre-extensions '())

;; Pre extensions are loaded *after* the packages
(defvar syl20bnr-post-extensions '(o-blog))

(defun syl20bnr/init-o-blog ()
  (let* ((dir (contribsys/get-layer-property 'syl20bnr :ext-dir)))
    (add-to-list 'load-path (format "%so-blog/lisp/" dir)))
  (use-package o-blog
    :commands o-blog-publish))
