;;; pdf-cache.el --- Cache time-critical or frequent epdfinfo queries. -*- lexical-binding:t -*-

;; Copyright (C) 2013  Andreas Politz

;; Author: Andreas Politz <politza@fh-trier.de>
;; Keywords: files, doc-view, pdf

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;; Code:
;;

(require 'pdf-macs)
(require 'pdf-info)
(require 'pdf-util)


;; * ================================================================== *
;; * Customiazations
;; * ================================================================== *

(defcustom pdf-cache-image-limit 64
  "Maximum number of cached PNG images per buffer."
  :type 'integer
  :group 'pdf-cache
  :group 'pdf-view)

(defcustom pdf-cache-prefetch-delay 0.5
  "Idle time in seconds before prefetching images starts."
  :group 'pdf-view
  :type 'number)

(defcustom pdf-cache-prefetch-pages-function
  'pdf-cache-prefetch-pages-function-default
  "A function returning a list of pages to be prefetched.

It is called with no arguments in the PDF window and should
return a list of page-numbers, determining the pages that should
be prefetched and their order."
  :group 'pdf-view
  :type 'function)


;; * ================================================================== *
;; * Simple Value cache
;; * ================================================================== *

(defvar-local pdf-cache--data nil)

(defvar pdf-annot-modified-functions)

(defun pdf-cache--initialize ()
  "Initialize the cache to store document data.

Note: The cache is only initialized once. After that it needs to
be cleared before this function makes any changes to it. This is
an internal function and not meant to be directly used."
  (unless pdf-cache--data
    (setq pdf-cache--data (make-hash-table))
    (add-hook 'pdf-info-close-document-hook #'pdf-cache-clear-data nil t)
    (add-hook 'pdf-annot-modified-functions
              #'pdf-cache--clear-data-of-annotations
              nil t)))

(defun pdf-cache--clear-data-of-annotations (fn)
  "Clear the data cache when annotations are modified.

FN is a closure as described in `pdf-annot-modified-functions'.

Note: This is an internal function and not meant to be directly used."
  (apply #'pdf-cache-clear-data-of-pages
         (mapcar (lambda (a)
                   (cdr (assq 'page a)))
                 (funcall fn t))))

(defun pdf-cache--data-put (key value &optional page)
  "Put KEY with VALUE in the cache of PAGE, return value."
  (pdf-cache--initialize)
  (puthash page (cons (cons key value)
                      (assq-delete-all
                       key
                       (gethash page pdf-cache--data)))
           pdf-cache--data)
  value)

(defun pdf-cache--data-get (key &optional page)
  "Get value of KEY in the cache of PAGE.

Returns a cons \(HIT . VALUE\), where HIT is non-nil if KEY was
stored previously for PAGE and VALUE its value.  Otherwise HIT
is nil and VALUE undefined."
  (pdf-cache--initialize)
  (let ((elt (assq key (gethash page pdf-cache--data))))
    (if elt
        (cons t (cdr elt))
      (cons nil nil))))

(defun pdf-cache--data-clear (key &optional page)
  "Remove KEY from the cache of PAGE."
  (pdf-cache--initialize)
  (puthash page
           (assq-delete-all key (gethash page pdf-cache--data))
           pdf-cache--data)
  nil)

(defun pdf-cache-clear-data-of-pages (&rest pages)
  "Remove all PAGES from the cache."
  (when pdf-cache--data
    (dolist (page pages)
      (remhash page pdf-cache--data))))

(defun pdf-cache-clear-data ()
  "Remove the entire cache."
  (interactive)
  (when pdf-cache--data
    (clrhash pdf-cache--data)))

(defmacro define-pdf-cache-function (command &optional page-arg-p)
  "Define a simple data cache function.

COMMAND is the name of the command, e.g. number-of-pages.  It
should have a corresponding pdf-info function.  If PAGE-ARG-P is
non-nil, define a one-dimensional cache indexed by the page
number. Otherwise the value is constant for each document, like
e.g. number-of-pages.

Both args are unevaluated."

  (let ((args (if page-arg-p (list 'page)))
        (fn (intern (format "pdf-cache-%s" command)))
        (ifn (intern (format "pdf-info-%s" command)))
        (doc (format "Cached version of `pdf-info-%s', which see.

Make sure, not to modify its return value." command)))
    `(defun ,fn ,args
       ,doc
       (let ((hit-value (pdf-cache--data-get ',command ,(if page-arg-p 'page))))
         (if (car hit-value)
             (cdr hit-value)
           (pdf-cache--data-put
            ',command
            ,(if page-arg-p
                 (list ifn 'page)
               (list ifn))
            ,(if page-arg-p 'page)))))))

(define-pdf-cache-function pagelinks t)
(define-pdf-cache-function number-of-pages)
;; The boundingbox may change if annotations change.
(define-pdf-cache-function boundingbox t)
(define-pdf-cache-function textregions t)
(define-pdf-cache-function pagesize t)


;; * ================================================================== *
;; * PNG image LRU cache
;; * ================================================================== *

(defvar pdf-cache-image-inihibit nil
  "Non-nil, if the image cache should be bypassed.")

(defvar-local pdf-cache--image-cache nil)

(defmacro pdf-cache--make-image (page width data hash)
  "Make the image that we store in the image cache.

An image is a tuple of PAGE WIDTH DATA HASH."
  `(list ,page ,width ,data ,hash))
(defmacro pdf-cache--image/page (img)
  "Return the page value for IMG."
  `(nth 0 ,img))
(defmacro pdf-cache--image/width (img)
  "Return the width value for IMG."
  `(nth 1 ,img))
(defmacro pdf-cache--image/data (img)
  "Return the data value for IMG."
  `(nth 2 ,img))
(defmacro pdf-cache--image/hash (img)
  "Return the hash value for IMG."
  `(nth 3 ,img))

(defun pdf-cache--image-match (image page min-width &optional max-width hash)
  "Match IMAGE with specs.

IMAGE should be a list as created by `pdf-cache--make-image'.

Return non-nil, if IMAGE's page is the same as PAGE, its width
is at least MIN-WIDTH and at most MAX-WIDTH and its stored
hash-value is `eql' to HASH."
  (and (= (pdf-cache--image/page image)
          page)
       (or (null min-width)
           (>= (pdf-cache--image/width image)
               min-width))
       (or (null max-width)
           (<= (pdf-cache--image/width image)
               max-width))
       (eql (pdf-cache--image/hash image)
            hash)))

(defun pdf-cache-lookup-image (page min-width &optional max-width hash)
  "Return PAGE's cached PNG data as a string or nil.

Return an image of at least MIN-WIDTH and, if non-nil, maximum
width MAX-WIDTH and `eql' HASH value.

Does not modify the cache.  See also `pdf-cache-get-image'."
  (let ((image (car (cl-member
                     (list page min-width max-width hash)
                     pdf-cache--image-cache
                     :test (lambda (spec image)
                             (apply #'pdf-cache--image-match image spec))))))
    (and image
         (pdf-cache--image/data image))))

(defun pdf-cache-get-image (page min-width &optional max-width hash)
  "Return PAGE's PNG data as a string.

Return an image of at least MIN-WIDTH and, if non-nil, maximum
width MAX-WIDTH and `eql' HASH value.

Remember that image was recently used.

Returns nil, if no matching image was found."
  (let ((cache pdf-cache--image-cache)
        image)
    ;; Find it in the cache.
    (while (and (setq image (pop cache))
                (not (pdf-cache--image-match
                      image page min-width max-width hash))))
    ;; Remove it and push it to the front.
    (when image
      (setq pdf-cache--image-cache
            (cons image (delq image pdf-cache--image-cache)))
      (pdf-cache--image/data image))))

(defun pdf-cache-put-image (page width data &optional hash)
  "Cache image of PAGE with WIDTH, DATA and HASH.

DATA should the string of a PNG image of width WIDTH and from
page PAGE in the current buffer.  See `pdf-cache-get-image' for
the HASH argument.

This function always returns nil."
  (unless pdf-cache--image-cache
    (add-hook 'pdf-info-close-document-hook #'pdf-cache-clear-images nil t)
    (add-hook 'pdf-annot-modified-functions
              #'pdf-cache--clear-images-of-annotations nil t))
  (push (pdf-cache--make-image page width data hash)
        pdf-cache--image-cache)
  ;; Forget old image(s).
  (when (> (length pdf-cache--image-cache)
           pdf-cache-image-limit)
    (if (> pdf-cache-image-limit 1)
        (setcdr (nthcdr (1- pdf-cache-image-limit)
                        pdf-cache--image-cache)
                nil)
      (setq pdf-cache--image-cache nil)))
  nil)

(defun pdf-cache-clear-images ()
  "Clear the image cache."
  (setq pdf-cache--image-cache nil))

(defun pdf-cache-clear-images-if (fn)
  "Remove images from the cache according to FN.

FN should be function accepting 4 Arguments \(PAGE WIDTH DATA
HASH\).  It should return non-nil, if the image should be removed
from the cache."
  (setq pdf-cache--image-cache
        (cl-remove-if
         (lambda (image)
           (funcall
            fn
            (pdf-cache--image/page image)
            (pdf-cache--image/width image)
            (pdf-cache--image/data image)
            (pdf-cache--image/hash image)))
         pdf-cache--image-cache)))


(defun pdf-cache--clear-images-of-annotations (fn)
  "Clear the images cache when annotations are modified.

FN is a closure as described in `pdf-annot-modified-functions'.

Note: This is an internal function and not meant to be directly used."
  (apply #'pdf-cache-clear-images-of-pages
         (mapcar (lambda (a)
                   (cdr (assq 'page a)))
                 (funcall fn t))))

(defun pdf-cache-clear-images-of-pages (&rest pages)
  "Remove all images of PAGES from the image cache."
  (pdf-cache-clear-images-if
   (lambda (page &rest _) (memq page pages))))

(defun pdf-cache-renderpage (page min-width &optional max-width)
  "Render PAGE according to MIN-WIDTH and MAX-WIDTH.

Return the PNG data of an image as a string, such that its width
is at least MIN-WIDTH and, if non-nil, at most MAX-WIDTH.

If such an image is not available in the cache, call
`pdf-info-renderpage' to create one."
  (if pdf-cache-image-inihibit
      (pdf-info-renderpage page min-width)
    (or (pdf-cache-get-image page min-width max-width)
        (let ((data (pdf-info-renderpage page min-width)))
          (pdf-cache-put-image page min-width data)
          data))))

(defun pdf-cache-renderpage-text-regions (page width single-line-p
                                               &rest selection)
  "Render PAGE according to WIDTH, SINGLE-LINE-P and SELECTION.

See also `pdf-info-renderpage-text-regions' and
`pdf-cache-renderpage'."
  (if pdf-cache-image-inihibit
      (apply #'pdf-info-renderpage-text-regions
             page width single-line-p nil nil selection)
    (let ((hash (sxhash
                 (format "%S" (cons 'renderpage-text-regions
                                    (cons single-line-p selection))))))
      (or (pdf-cache-get-image page width width hash)
          (let ((data (apply #'pdf-info-renderpage-text-regions
                             page width single-line-p nil nil selection)))
            (pdf-cache-put-image page width data hash)
            data)))))

(defun pdf-cache-renderpage-highlight (page width &rest regions)
  "Highlight PAGE according to WIDTH and REGIONS.

See also `pdf-info-renderpage-highlight' and
`pdf-cache-renderpage'."
  (if pdf-cache-image-inihibit
      (apply #'pdf-info-renderpage-highlight
             page width nil regions)
    (let ((hash (sxhash
                 (format "%S" (cons 'renderpage-highlight
                                    regions)))))
      (or (pdf-cache-get-image page width width hash)
          (let ((data (apply #'pdf-info-renderpage-highlight
                             page width nil regions)))
            (pdf-cache-put-image page width data hash)
            data)))))


;; * ================================================================== *
;; * Prefetching images
;; * ================================================================== *

(defvar-local pdf-cache--prefetch-pages nil
  "Pages to be prefetched.")

(defvar-local pdf-cache--prefetch-timer nil
  "Timer used when prefetching images.")

(define-minor-mode pdf-cache-prefetch-minor-mode
  "Try to load images which will probably be needed in a while."
  :group 'pdf-cache
  (pdf-cache--prefetch-cancel)
  (cond
   (pdf-cache-prefetch-minor-mode
    (pdf-util-assert-pdf-buffer)
    (add-hook 'pre-command-hook #'pdf-cache--prefetch-stop nil t)
    ;; FIXME: Disable the time when the buffer is killed or its
    ;; major-mode changes.
    (setq pdf-cache--prefetch-timer
          (run-with-idle-timer (or pdf-cache-prefetch-delay 1) t
                               #'pdf-cache--prefetch-start (current-buffer))))
   (t
    (remove-hook 'pre-command-hook #'pdf-cache--prefetch-stop t))))

(defun pdf-cache-prefetch-pages-function-default ()
  "The default function to prefetch pages.

See `pdf-cache-prefetch-pages-function' for an explanation of
what this function does."
  (let ((page (pdf-view-current-page)))
    (pdf-util-remove-duplicates
     (cl-remove-if-not
      (lambda (page)
        (and (>= page 1)
             (<= page (pdf-cache-number-of-pages))))
      (append
       ;; +1, -1, +2, -2, ...
       (let ((sign 1)
             (incr 1))
         (mapcar (lambda (_)
                   (setq page (+ page (* sign incr))
                         sign (- sign)
                         incr (1+ incr))
                   page)
                 (number-sequence 1 16)))
       ;; First and last
       (list 1 (pdf-cache-number-of-pages))
       ;; Links
       (mapcar
        (apply-partially 'alist-get 'page)
        (cl-remove-if-not
         (lambda (link) (eq (alist-get 'type link) 'goto-dest))
         (pdf-cache-pagelinks
          (pdf-view-current-page)))))))))

(defvar pdf-view-use-scaling)
(defun pdf-cache--prefetch-pages (window image-width)
  "Internal function to prefetch pages and store them in the cache.

WINDOW and IMAGE-WIDTH decide the page and scale of the final image."
  (when (and (eq window (selected-window))
             (pdf-util-pdf-buffer-p))
    (let ((page (pop pdf-cache--prefetch-pages)))
      (while (and page
                  (pdf-cache-lookup-image
                   page
                   image-width
                   (if pdf-view-use-scaling
                       (* 2 image-width)
                     image-width)))
        (setq page (pop pdf-cache--prefetch-pages)))
      (pdf-util-debug
        (when (null page)
          (message  "Prefetching done.")))
      (when page
        (let* ((buffer (current-buffer))
               (pdf-info-asynchronous
                (lambda (status data)
                  (when (and (null status)
                             (eq window
                                 (selected-window))
                             (eq buffer (window-buffer)))
                    (with-current-buffer (window-buffer)
                      (when (derived-mode-p 'pdf-view-mode)
                        (pdf-cache-put-image
                         page image-width data)
                        (image-size (pdf-view-create-page page))
                        (pdf-util-debug
                          (message "Prefetched page %s." page))
                        ;; Avoid max-lisp-eval-depth
                        (run-with-timer
                         0.001 nil
                         #'pdf-cache--prefetch-pages window image-width)))))))
          (condition-case err
              (pdf-info-renderpage page image-width)
            (error
             (pdf-cache-prefetch-minor-mode -1)
             (signal (car err) (cdr err)))))))))

(defvar pdf-cache--prefetch-started-p nil
  "Guard against multiple prefetch starts.

Used solely in `pdf-cache--prefetch-start'.")

(defun pdf-cache--prefetch-start (buffer)
  "Start prefetching images in BUFFER."
  (when (and pdf-cache-prefetch-minor-mode
             (not pdf-cache--prefetch-started-p)
             (pdf-util-pdf-buffer-p)
             (not isearch-mode)
             (null pdf-cache--prefetch-pages)
             (eq (window-buffer) buffer)
             (fboundp pdf-cache-prefetch-pages-function))
    (let* ((pdf-cache--prefetch-started-p t)
           (pages (funcall pdf-cache-prefetch-pages-function)))
      (setq pdf-cache--prefetch-pages
            (butlast pages (max 0 (- (length pages)
                                     pdf-cache-image-limit))))
      (pdf-cache--prefetch-pages
       (selected-window)
       (car (pdf-view-desired-image-size))))))

(defun pdf-cache--prefetch-stop ()
  "Stop prefetching images in current buffer."
  (setq pdf-cache--prefetch-pages nil))

(defun pdf-cache--prefetch-cancel ()
  "Cancel prefetching images in current buffer."
  (pdf-cache--prefetch-stop)
  (when pdf-cache--prefetch-timer
    (cancel-timer pdf-cache--prefetch-timer))
  (setq pdf-cache--prefetch-timer nil))

(provide 'pdf-cache)
;;; pdf-cache.el ends here
