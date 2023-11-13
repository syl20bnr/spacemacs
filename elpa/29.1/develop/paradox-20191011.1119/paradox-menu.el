;;; paradox-menu.el --- defining the Packages menu -*- lexical-binding:t -*-

;; Copyright (C) 2014-2015 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; Prefix: paradox
;; Separator: -

;;; License:
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;

;;; Code:
(require 'cl-lib)
(require 'cus-edit)
(require 'package)
(require 'subr-x)
(require 'hydra)

(require 'paradox-core)
(require 'paradox-github)
(require 'paradox-commit-list)
(require 'paradox-execute)

(defgroup paradox-menu nil
  "Paradox Packages Menu configurations."
  :prefix "paradox-"
  :package-version '(paradox . "2.0")
  :group 'paradox)


;;; Customization Variables
(defcustom paradox-column-width-package  18
  "Width of the \"Package\" column."
  :type 'integer
  :group 'paradox-menu
  :package-version '(paradox . "0.1"))

(defcustom paradox-column-width-version  9
  "Width of the \"Version\" column."
  :type 'integer
  :group 'paradox-menu
  :package-version '(paradox . "0.1"))

(defcustom paradox-column-width-status  10
  "Width of the \"Status\" column."
  :type 'integer
  :group 'paradox-menu
  :package-version '(paradox . "0.1"))

(defcustom paradox-column-width-star 4
  "Width of the \"Star\" column."
  :type 'integer
  :group 'paradox-menu
  :package-version '(paradox . "0.1"))

(defcustom paradox-column-width-download 4
  "Width of the \"Download Count\" column."
  :type 'integer
  :group 'paradox-menu
  :package-version '(paradox . "1.1"))

(defcustom paradox-display-star-count t
  "If non-nil, adds a \"Star\" column to the Package Menu."
  :type 'boolean
  :group 'paradox-menu
  :package-version '(paradox . "1.1"))

(defcustom paradox-display-download-count nil
  "If non-nil, adds a \"Download\" column to the Package Menu."
  :type 'boolean
  :group 'paradox-menu
  :package-version '(paradox . "1.2.3"))

(defface paradox-mode-line-face
  '((t :inherit (font-lock-keyword-face mode-line-buffer-id)
       :weight normal))
  "Face used on mode line statuses."
  :group 'paradox)
(defface paradox-name-face
  '((t :inherit link))
  "Face used on the package's name."
  :group 'paradox)
(defface paradox-homepage-button-face
  '((t :underline t :inherit font-lock-comment-face))
  "Face used on the homepage button."
  :group 'paradox)
;; (defface paradox-version-face
;;   '((t :inherit default))
;;   "Face used on the version column."
;;   :group 'paradox)
(defface paradox-archive-face
  '((t :inherit paradox-comment-face))
  "Face used on the archive column."
  :group 'paradox)
(defface paradox-star-face
  '((t :inherit font-lock-string-face))
  "Face used on the star column, for packages you haven't starred."
  :group 'paradox)
(defface paradox-starred-face
  '((t :inherit font-lock-variable-name-face))
  "Face used on the star column, for packages you have starred."
  :group 'paradox)
(defface paradox-download-face
  '((t :inherit font-lock-keyword-face))
  "Face used on the Downloads column."
  :group 'paradox)
(defface paradox-description-face
  '((t :inherit default))
  "Face used on the description column.
If `paradox-lines-per-entry' > 1, the face
`paradox-description-face-multiline' is used instead."
  :group 'paradox)
(defface paradox-description-face-multiline
  '((t :inherit font-lock-doc-face))
  "Face used on the description column when `paradox-lines-per-entry' > 1.
If `paradox-lines-per-entry' = 1, the face
`paradox-description-face' is used instead."
  :group 'paradox)

(defcustom paradox-status-face-alist
  '(("built-in"  . font-lock-builtin-face)
    ("available" . default)
    ("new"       . bold)
    ("held"      . font-lock-constant-face)
    ("disabled"  . font-lock-warning-face)
    ("avail-obso" . font-lock-comment-face)
    ("installed" . font-lock-comment-face)
    ("dependency" . font-lock-comment-face)
    ("incompat"  . font-lock-comment-face)
    ("deleted"   . font-lock-comment-face)
    ("unsigned"  . font-lock-warning-face))
  "List of (\"STATUS\" . FACE) cons cells.
When displaying the package menu, FACE will be used to paint the
Version, Status, and Description columns of each package whose
status is STATUS."
  :type '(repeat (cons string face))
  :group 'paradox-menu
  :package-version '(paradox . "2.0"))

(defcustom paradox-homepage-button-string "h"
  "String used to for the link that takes you to a package's homepage."
  :type 'string
  :group 'paradox-menu
  :package-version '(paradox . "0.10"))

(defcustom paradox-use-homepage-buttons t
  "If non-nil a button will be added after the name of each package.
This button takes you to the package's homepage."
  :type 'boolean
  :group 'paradox-menu
  :package-version '(paradox . "0.10"))

(defcustom paradox-lines-per-entry 1
  "Number of lines used to display each entry in the Package Menu.
1 Gives you the regular package menu.
2 Displays the description on a separate line below the entry.
3+ Adds empty lines separating the entries."
  :type 'integer
  :group 'paradox-menu
  :package-version '(paradox . "0.10"))


;;; Internal
(defvar-local paradox--current-filter nil)

(defvar paradox--column-name-star
  (if (char-displayable-p ?\x2605) "\x2605" "*"))

(defvar paradox--column-name-download
  (if (char-displayable-p ?\x2193) "\x2193" "DC"))

(defvar paradox--upgradeable-packages nil)
(defvar paradox--upgradeable-packages-number nil)
(defvar paradox--upgradeable-packages-any? nil)

(defvar paradox--column-index-star nil)
(defvar paradox--column-index-download nil)

(defvar paradox--desc-suffix nil)
(defvar paradox--desc-prefix nil)

(defvar paradox--commit-list-buffer "*Package Commit List*")

(define-button-type 'paradox-name
  'action      #'package-menu-describe-package
  'follow-link t)

(define-button-type 'paradox-homepage
  'action      #'paradox-menu-visit-homepage
  'follow-link t
  'mouse-face  'custom-button-mouse)

;; Use `font-lock-face' on creation instead.
(button-type-put 'paradox-name     'face nil)
(button-type-put 'paradox-homepage 'face nil)


;;; Building the packages buffer.
(defun paradox-refresh-upgradeable-packages ()
  "Refresh the list of upgradeable packages."
  (interactive)
  (setq paradox--upgradeable-packages (package-menu--find-upgrades))
  (setq paradox--upgradeable-packages-number
        (length paradox--upgradeable-packages))
  (setq paradox--upgradeable-packages-any?
        (> paradox--upgradeable-packages-number 0)))

(defun paradox--print-info (pkg)
  "Return a package entry suitable for `tabulated-list-entries'.
PKG has the form (PKG-DESC . STATUS).
Return (PKG-DESC [STAR NAME VERSION STATUS DOC])."
  (let* ((pkg-desc (if (consp pkg) (car pkg) pkg))
         (status   (if (consp pkg) (cdr pkg) (package-desc-status pkg)))
         (face (or (cdr (assoc-string status paradox-status-face-alist))
                   'font-lock-warning-face))
         (url (paradox--package-homepage pkg-desc))
         (name (symbol-name (package-desc-name pkg-desc)))
         (name-length (length name))
         (counts (paradox--count-print (package-desc-name pkg-desc)))
         (button-length (if paradox-use-homepage-buttons (length paradox-homepage-button-string) 0)))
    (paradox--incf status)
    (let ((cell (assq :stars (package-desc-extras pkg-desc))))
      (if cell
          (setcdr cell counts)
        (push (cons :stars counts) (package-desc-extras pkg-desc))))
    (list pkg-desc
          `[,(concat
              (make-text-button
               (truncate-string-to-width
                name (- paradox-column-width-package button-length) 0 nil t)
               nil
               'type           'paradox-name
               'font-lock-face 'paradox-name-face
               'help-echo      (concat "Package: " name)
               'package-desc   pkg-desc)
              (when (and paradox-use-homepage-buttons url)
                (make-string (max 0 (- paradox-column-width-package name-length button-length)) ?\s))
              (when (and paradox-use-homepage-buttons url)
                (make-text-button
                 (copy-sequence paradox-homepage-button-string) nil
                 'type           'paradox-homepage
                 'font-lock-face 'paradox-homepage-button-face
                 'help-echo      (format "Visit %s" url))))
            ,(propertize (package-version-join
                          (package-desc-version pkg-desc))
                         'font-lock-face face)
            ,(propertize status 'font-lock-face face)
            ,@(if (cdr package-archives)
                  (list (propertize (or (package-desc-archive pkg-desc) "")
                                    'font-lock-face 'paradox-archive-face)))
            ,@counts
            ,(propertize
              (concat (propertize " " 'display paradox--desc-prefix)
                      (package-desc-summary pkg-desc)
                      (propertize " " 'display paradox--desc-suffix)) ;└╰
              'font-lock-face
              (if (> paradox-lines-per-entry 1)
                  'paradox-description-face-multiline
                'paradox-description-face))])))

(defun paradox--count-print (pkg)
  "Return counts of PKG as a package-desc list."
  (append
   (when (and paradox-display-star-count (hash-table-p paradox--star-count))
     (list (paradox--package-star-count pkg)))
   (when (and paradox-display-download-count (hash-table-p paradox--download-count))
     (list (paradox--package-download-count pkg)))))

(defun paradox--package-download-count (pkg)
  "Return propertized string with the download count of PKG."
  (let ((c (gethash pkg paradox--download-count nil)))
    (propertize
     (if (numberp c)
         (if (> c 999) (format "%sK" (truncate c 1000)) (format "%s" c))
       " ")
     'font-lock-face 'paradox-download-face
     'value (or c 0))))

(defun paradox--package-homepage (pkg)
  "PKG can be the package-name symbol or a package-desc object."
  (let* ((object   (if (symbolp pkg) (cadr (assoc pkg package-archive-contents)) pkg))
         (name     (if (symbolp pkg) pkg (package-desc-name pkg)))
         (extras   (package-desc-extras object))
         (homepage (and (listp extras) (cdr-safe (assoc :url extras)))))
    (or homepage
        (and (setq extras (gethash name paradox--package-repo-list))
             (format "https://github.com/%s" extras)))))

(defun paradox--get-or-return-package (pkg)
  "Take a marker or package name PKG and return a package name."
  (if (or (markerp pkg) (null pkg))
      (if (derived-mode-p 'package-menu-mode)
          (package-desc-name (tabulated-list-get-id))
        (error "Not in Package Menu"))
    pkg))

(defun paradox--incf (status)
  "Increment the count for STATUS on `paradox--package-count'.
Also increments the count for \"total\"."
  (paradox--inc-count status)
  (unless (member status '("obsolete" "avail-obso" "incompat"))
    (paradox--inc-count "total")))

(defun paradox--inc-count (string)
  "Increment the cdr of (assoc-string STRING paradox--package-count)."
  (let ((cons (assoc-string string paradox--package-count)))
    (setcdr cons (1+ (cdr cons)))))

(defun paradox--entry-star-count (entry)
  "Get the star count of the package in ENTRY."
  (paradox--package-star-count
   ;; The package symbol should be in the ID field, but that's not mandatory,
   (or (ignore-errors (elt (car entry) 1))
       ;; So we also try interning the package name.
       (intern (car (elt (cadr entry) 0))))))

(defun paradox--handle-failed-download (&rest _)
  "Handle the case when Emacs fails to download Github data."
  (paradox--update-downloads-in-progress 'paradox--data)
  (unless (hash-table-p paradox--download-count)
    (setq paradox--download-count (make-hash-table)))
  (unless (hash-table-p paradox--package-repo-list)
    (setq paradox--package-repo-list (make-hash-table)))
  (unless (hash-table-p paradox--star-count)
    (setq paradox--star-count (make-hash-table)))
  (unless (hash-table-p paradox--wiki-packages)
    (setq paradox--wiki-packages (make-hash-table)))
  (message "[Paradox] Error downloading Github data"))

(defmacro paradox--with-work-buffer (location file &rest body)
  "Run BODY in a buffer containing the contents of FILE at LOCATION.
This is the same as `package--with-work-buffer-async', except it
automatically decides whether to download asynchronously based on
`package-menu-async'."
  (declare (indent 2) (debug t))
  (require 'package)
  (if (fboundp 'package--with-response-buffer)
      `(package--with-response-buffer
         ,location :file ,file
         :async package-menu-async
         :error-form (paradox--handle-failed-download)
         ,@body
         (paradox--update-downloads-in-progress 'paradox--data))
    `(package--with-work-buffer ,location ,file ,@body)))

(defun paradox--refresh-remote-data ()
  "Download metadata and populate the respective variables."
  (interactive)
  (when (boundp 'package--downloads-in-progress)
    (add-to-list 'package--downloads-in-progress 'paradox--data))
  (condition-case-unless-debug nil
      (paradox--with-work-buffer paradox--data-url "data-hashtables"
        (setq paradox--star-count (read (current-buffer)))
        (setq paradox--package-repo-list (read (current-buffer)))
        (setq paradox--download-count (read (current-buffer)))
        (setq paradox--wiki-packages (read (current-buffer))))
    (error (paradox--handle-failed-download))))

(defun paradox--package-star-count (package)
  "Get the star count of PACKAGE."
  (let ((count (gethash package paradox--star-count nil))
        (repo  (gethash package paradox--package-repo-list nil)))
    (propertize
     (format "%s" (or count ""))
     'font-lock-face
     (if (and repo (paradox--starred-repo-p repo))
         'paradox-starred-face
       'paradox-star-face))))

(defun paradox--star-predicate (A B)
  "Non-nil t if star count of A is larger than B."
  (> (string-to-number (elt (cadr A) paradox--column-index-star))
     (string-to-number (elt (cadr B) paradox--column-index-star))))
(defun paradox--download-predicate (A B)
  "Non-nil t if download count of A is larger than B."
  (> (get-text-property 0 'value (elt (cadr A) paradox--column-index-download))
     (get-text-property 0 'value (elt (cadr B) paradox--column-index-download))))

(defun paradox--generate-menu (remember-pos packages &optional keywords)
  "Populate the Package Menu, without hacking into the header-format.
If REMEMBER-POS is non-nil, keep point on the same entry.
PACKAGES should be t, which means to display all known packages,
or a list of package names (symbols) to display.

With KEYWORDS given, only packages with those keywords are
shown."
  (paradox-menu--refresh packages keywords)
  (setq paradox--current-filter
        (if keywords (mapconcat 'identity keywords ",")
          nil))
  (let ((idx (paradox--column-index "Package")))
    (when (integerp idx)
      (setcar (elt tabulated-list-format idx)
              (if keywords
                  (concat "Package[" paradox--current-filter "]")
                "Package"))))
  (tabulated-list-print remember-pos)
  (tabulated-list-init-header)
  (paradox--update-mode-line))

(defcustom paradox-hide-wiki-packages nil
  "If non-nil, don't display packages from the emacswiki."
  :type 'boolean)

(defun paradox--maybe-remove-wiki-packages (pkgs)
  "Remove wiki packages from PKGS.
If `paradox-hide-wiki-packages' is nil, just return PKGS."
  (if (not paradox-hide-wiki-packages)
      pkgs
    (remq nil
          (mapcar
           (lambda (entry)
             (let ((name (or (car-safe entry) entry)))
               (unless (gethash name paradox--wiki-packages)
                 name)))
           (if (or (not pkgs) (eq t pkgs))
               package-archive-contents
             pkgs)))))

(defun paradox-menu--refresh (&optional packages keywords)
  "Call `package-menu--refresh' retaining current filter.
PACKAGES and KEYWORDS are passed to `package-menu--refresh'.  If
KEYWORDS is nil and `paradox--current-filter' is non-nil, it is
used to define keywords."
  (mapc (lambda (x) (setf (cdr x) 0)) paradox--package-count)
  (let ((paradox--desc-prefix (if (> paradox-lines-per-entry 1) " \n      " ""))
        (paradox--desc-suffix (make-string (max 0 (- paradox-lines-per-entry 2)) ?\n)))
    (cond
     ((or packages keywords (not paradox--current-filter))
      (package-menu--refresh
       (paradox--maybe-remove-wiki-packages packages)
       keywords)
      (paradox-refresh-upgradeable-packages))
     ((string= paradox--current-filter "Upgradable")
      (paradox-refresh-upgradeable-packages)
      (paradox-filter-upgrades))
     ((string= paradox--current-filter "Starred")
      (paradox-filter-stars)
      (paradox-refresh-upgradeable-packages))
     ((string-match "\\`Regexp:\\(.*\\)\\'" paradox--current-filter)
      (paradox-filter-regexp (match-string 1 paradox--current-filter))
      (paradox-refresh-upgradeable-packages))
     (t
      (paradox-menu--refresh
       packages (split-string paradox--current-filter ","))))))

(defun paradox--column-index (regexp)
  "Find the index of the column that matches REGEXP."
  (cl-position (format "\\`%s\\'" (regexp-quote regexp)) tabulated-list-format
               :test (lambda (x y) (string-match x (or (car-safe y) "")))))

(defun paradox--count-format ()
  "List of star/download counts to be used as part of the entry."
  (remove
   nil
   (list
    (when paradox-display-star-count
      (list paradox--column-name-star paradox-column-width-star
            'paradox--star-predicate :right-align t))
    (when paradox-display-download-count
      (list paradox--column-name-download paradox-column-width-download
            'paradox--download-predicate :right-align t)))))

(defun paradox--archive-format ()
  "List containing archive to be used as part of the entry."
  (when (cdr package-archives)
    (list (list "Archive"
                (apply 'max (mapcar 'length (mapcar 'car package-archives)))
                'package-menu--archive-predicate))))

(add-hook 'paradox-menu-mode-hook 'paradox-refresh-upgradeable-packages)


;;; Mode Definition
(define-derived-mode paradox-menu-mode tabulated-list-mode "Paradox Menu"
  "Major mode for browsing a list of packages.
Letters do not insert themselves; instead, they are commands.
\\<paradox-menu-mode-map>
\\{paradox-menu-mode-map}"
  (hl-line-mode 1)
  (when (boundp 'package--post-download-archives-hook)
    (add-hook 'package--post-download-archives-hook
              #'paradox--stop-spinner))
  (if (boundp 'package--downloads-in-progress)
      (setq mode-line-process
            '("" (package--downloads-in-progress
                  (":Loading "
                   (paradox--spinner
                    (:eval (spinner-print paradox--spinner))
                    (:eval (paradox--start-spinner))))
                  (paradox--spinner
                   (":Executing " (:eval (spinner-print paradox--spinner)))))))
    (setq mode-line-process
          '(paradox--spinner
            (":Executing " (:eval (spinner-print paradox--spinner))))))
  (paradox--update-mode-line)
  (setq tabulated-list-format
        `[("Package" ,paradox-column-width-package package-menu--name-predicate)
          ("Version" ,paradox-column-width-version paradox--version-predicate)
          ("Status" ,paradox-column-width-status package-menu--status-predicate)
          ,@(paradox--archive-format)
          ,@(paradox--count-format)
          ("Description" 0 nil)])
  (setq paradox--column-index-star
        (paradox--column-index paradox--column-name-star))
  (setq paradox--column-index-download
        (paradox--column-index paradox--column-name-download))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Status" nil))
  (add-hook 'tabulated-list-revert-hook #'paradox-menu--refresh nil t)
  (add-hook 'tabulated-list-revert-hook #'paradox-refresh-upgradeable-packages nil t)
  ;; (add-hook 'tabulated-list-revert-hook #'paradox--refresh-remote-data nil t)
  (add-hook 'tabulated-list-revert-hook #'paradox--update-mode-line 'append t)
  (tabulated-list-init-header)
  ;; We need package-menu-mode to be our parent, otherwise some
  ;; commands throw errors.  But we can't actually derive from it,
  ;; otherwise its initialization will screw up the header-format.  So
  ;; we "patch" it like this.
  (put 'paradox-menu-mode 'derived-mode-parent 'package-menu-mode)
  (run-hooks 'package-menu-mode-hook))

(put 'paradox-menu-mode 'derived-mode-parent 'package-menu-mode)

(defun paradox--define-sort (name &optional key)
  "Define sorting by column NAME and bind it to KEY.
Defines a function called paradox-sort-by-NAME."
  (let ((symb (intern (format "paradox-sort-by-%s" (downcase name))))
        (key (or key (substring name 0 1))))
    (eval
     `(progn
        (defun ,symb
            (invert)
          ,(format "Sort Package Menu by the %s column." name)
          (interactive "P")
          (when invert
            (setq tabulated-list-sort-key (cons ,name nil)))
          (tabulated-list--sort-by-column-name ,name))
        (define-key paradox-menu-mode-map ,(concat "S" (upcase key)) ',symb)
        (define-key paradox-menu-mode-map ,(concat "S" (downcase key)) ',symb)))))

(paradox--define-sort "Package")
(paradox--define-sort "Status")
(paradox--define-sort paradox--column-name-star "*")
(paradox--define-sort "Version")
(declare-function paradox-sort-by-package "paradox-menu")
(declare-function paradox-sort-by-version "paradox-menu")

(defun paradox--version-predicate (package-a package-b)
  "Predicate for sorting by the Version column.
Versions are compared semantically in descending order."
  (let ((a (package-desc-version (car package-a)))
        (b (package-desc-version (car package-b))))
    (cond ((version-list-= a b)
           (package-menu--name-predicate package-a package-b))
          (t
           (version-list-< b a)))))

(defalias 'paradox-filter-clear #'package-show-package-list
  "Clear current Package filter.
Redisplay the Packages buffer listing all packages, without
fetching the list.")

(defmacro paradox--apply-filter (name packages &optional nil-message)
  "Apply filter called NAME (a string) listing only PACKAGES.
PACKAGES should be a list of symbols (the names of packages to
display) or a list of cons cells whose `car's are symbols.
NIL-MESSAGE is the message to show if PACKAGES is nil, and
defaults to: \"No %s packages\"."
  (declare (debug t)
           (indent 1))
  (let* ((n (format "%s" name))
         (cn (capitalize n))
         (dn (downcase n)))
    (macroexp-let2 macroexp-copyable-p pl packages
      `(if (null ,pl)
           (user-error ,(or nil-message (format "No %s packages." dn)))
         (package-show-package-list
          (mapcar (lambda (p) (or (car-safe p) p)) ,pl))
         (setq paradox--current-filter ,cn)))))

(defun paradox-filter-upgrades ()
  "Show only upgradable packages."
  (interactive)
  (paradox--apply-filter Upgradable
    paradox--upgradeable-packages)
  (paradox-sort-by-package nil))

(defun paradox-filter-stars ()
  "Show only starred packages."
  (interactive)
  (let ((list))
    (maphash (lambda (pkg repo)
               (when (paradox--starred-repo-p repo)
                 (push pkg list)))
             paradox--package-repo-list)
    (paradox--apply-filter Starred list)))

(defun paradox-filter-regexp (regexp)
  "Show only packages matching REGEXP.
Test match against name and summary."
  (interactive (list (read-regexp "Enter Regular Expression: ")))
  (paradox--apply-filter Regexp
    (cl-remove-if-not
     (lambda (package)
       (or (string-match-p regexp (symbol-name (car package)))
           (string-match-p regexp (package-desc-summary (cadr package)))))
     package-archive-contents)
    "No packages match this regexp.")
  (setq paradox--current-filter (concat "Regexp:" regexp)))

(set-keymap-parent paradox-menu-mode-map package-menu-mode-map)
(define-key paradox-menu-mode-map "q" #'paradox-quit-and-close)
(define-key paradox-menu-mode-map "p" #'paradox-previous-entry)
(define-key paradox-menu-mode-map "n" #'paradox-next-entry)
(define-key paradox-menu-mode-map "k" #'paradox-previous-describe)
(define-key paradox-menu-mode-map "j" #'paradox-next-describe)
(define-key paradox-menu-mode-map "s" #'paradox-menu-mark-star-unstar)
(define-key paradox-menu-mode-map "h" #'paradox-menu-quick-help)
(define-key paradox-menu-mode-map "v" #'paradox-menu-visit-homepage)
(define-key paradox-menu-mode-map "w" #'paradox-menu-copy-homepage-as-kill)
(define-key paradox-menu-mode-map "l" #'paradox-menu-view-commit-list)
(define-key paradox-menu-mode-map "x" #'paradox-menu-execute)
(define-key paradox-menu-mode-map "\r" #'paradox-push-button)
(define-key paradox-menu-mode-map "F" 'package-menu-filter)
(if (version< emacs-version "25")
    (defhydra hydra-paradox-filter (:color blue :hint nil)
      "
Filter by:
_u_pgrades _r_egexp      _k_eyword   _s_tarred    _c_lear
"
      ("f" package-menu-filter)
      ("k" package-menu-filter)
      ("r" paradox-filter-regexp)
      ("u" paradox-filter-upgrades)
      ("s" paradox-filter-stars)
      ("c" paradox-filter-clear)
      ("g" paradox-filter-clear)
      ("q" nil "cancel" :color blue))
  (defhydra hydra-paradox-filter (:color blue :hint nil)
    "
Filter by:
_u_pgrades _r_egexp    _k_eyword   _s_tarred    _c_lear
Archive: g_n_u       _o_ther
Status:  _i_nstalled _a_vailable _d_ependency _b_uilt-in
"
    ("f" package-menu-filter)
    ("k" package-menu-filter)
    ("n" (package-menu-filter "arc:gnu"))
    ("o" (package-menu-filter
          (remove "arc:gnu"
                  (mapcar (lambda (e) (concat "arc:" (car e)))
                          package-archives))))
    ("r" paradox-filter-regexp)
    ("u" paradox-filter-upgrades)
    ("s" paradox-filter-stars)
    ("i" (package-menu-filter "status:installed"))
    ("a" (package-menu-filter "status:available"))
    ("b" (package-menu-filter "status:built-in"))
    ("d" (package-menu-filter "status:dependency"))
    ("c" paradox-filter-clear)
    ("g" paradox-filter-clear)
    ("q" nil "cancel" :color blue)))
(define-key paradox-menu-mode-map "f" #'hydra-paradox-filter/body)

;;; for those who don't want a hydra
(defvar paradox--filter-map)
(define-prefix-command 'paradox--filter-map)
(define-key paradox--filter-map "k" #'package-menu-filter)
(define-key paradox--filter-map "f" #'package-menu-filter)
(define-key paradox--filter-map "r" #'paradox-filter-regexp)
(define-key paradox--filter-map "u" #'paradox-filter-upgrades)
(define-key paradox--filter-map "s" #'paradox-filter-stars)
(define-key paradox--filter-map "c" #'paradox-filter-clear)

(easy-menu-define paradox-menu-mode-menu paradox-menu-mode-map
  "Menu for `paradox-menu-mode'."
  `("Paradox"
    ["Describe Package" package-menu-describe-package :help "Display information about this package"]
    ["Help" paradox-menu-quick-help :help "Show short key binding help for package-menu-mode"]

    "--"
    ["Refresh Package List" package-menu-refresh
     :help "Redownload the ELPA archive"
     :active (not package--downloads-in-progress)]
    ["Execute Marked Actions" paradox-menu-execute :help "Perform all the marked actions"]
    ["Mark All Available Upgrades" package-menu-mark-upgrades
     :help "Mark packages that have a newer version for upgrading"
     :active (not package--downloads-in-progress)]

    ("Other Mark Actions"
     ["Mark All Obsolete for Deletion" package-menu-mark-obsolete-for-deletion :help "Mark all obsolete packages for deletion"]
     ["Mark for Install" package-menu-mark-install :help "Mark a package for installation and move to the next line"]
     ["Mark for Deletion" package-menu-mark-delete :help "Mark a package for deletion and move to the next line"]
     ["Unmark" package-menu-mark-unmark :help "Clear any marks on a package and move to the next line"])

    "--"
    ("Github" :visible (stringp paradox-github-token)
     ["Star or unstar this package" paradox-menu-mark-star-unstar]
     ["Star all installed packages" paradox-star-all-installed-packages]
     ["Star packages when installing" (customize-save-variable 'paradox-automatically-star (not paradox-automatically-star))
      :help "Automatically star packages that you install (and unstar packages you delete)"
      :style toggle :selected paradox-automatically-star])
    ["Configure Github Inegration" (paradox--check-github-token) :visible (not paradox-github-token)]
    ["View Changelog" paradox-menu-view-commit-list :help "Show a package's commit list on Github"]
    ["Visit Homepage" paradox-menu-visit-homepage :help "Visit a package's Homepage on a browser"]

    "--"
    ("Filter Package List"
     ["Clear filter" paradox-filter-clear :help "Go back to unfiltered list"]
     ["By Keyword" package-menu-filter :help "Filter by package keyword"]
     ["By Upgrades" paradox-filter-upgrades :help "List only upgradeable packages"]
     ["By Regexp" paradox-filter-regexp :help "Filter packages matching a regexp"]
     ["By Starred" paradox-filter-stars :help "List only packages starred by the user"])
    ("Sort Package List"
     ["By Package Name" paradox-sort-by-package]
     ["By Status (default)" paradox-sort-by-status]
     ["By Number of Stars" paradox-sort-by-★]
     ["By Version" paradox-sort-by-version])
    ["Hide by Regexp" package-menu-hide-package :help "Permanently hide all packages matching a regexp"]
    ["Display Older Versions" package-menu-toggle-hiding
     :style toggle :selected (not package-menu--hide-packages)
     :help "Display package even if a newer version is already installed"]

    "--"
    ["Quit" quit-window :help "Quit package selection"]
    ["Customize" (customize-group 'package)]))


;;; Menu Mode Commands
(defun paradox-previous-entry (&optional n)
  "Move to previous entry, which might not be the previous line.
With prefix N, move to the N-th previous entry."
  (interactive "p")
  (paradox-next-entry (- (prefix-numeric-value n)))
  (forward-line 0)
  (forward-button 1))

(defun paradox-next-entry (&optional n)
  "Move to next entry, which might not be the next line.
With prefix N, move to the N-th next entry."
  (interactive "p")
  (setq n (prefix-numeric-value n))
  (let ((d (cl-signum n)))
    (dotimes (_ (abs n))
      (forward-line (max d 0))
      (if (eobp) (forward-line -1))
      (forward-button d))))

(defun paradox-next-describe (&optional n)
  "Move to the next package and describe it.
With prefix N, move to the N-th next package instead."
  (interactive "p")
  (paradox-next-entry n)
  (call-interactively 'package-menu-describe-package))

(defun paradox-previous-describe (&optional n)
  "Move to the previous package and describe it.
With prefix N, move to the N-th previous package instead."
  (interactive "p")
  (paradox-previous-entry n)
  (call-interactively 'package-menu-describe-package))

(defun paradox-push-button ()
  "Push button under point, or describe package."
  (interactive)
  (or (push-button)
      (call-interactively #'package-menu-describe-package)))

(defvar paradox--key-descriptors
  '(("next," "previous," "install," "delete," ("execute," . 1) "refresh," "help")
    ("star," "visit homepage," "unmark," ("mark Upgrades," . 5) "~delete obsolete")
    ("list commits")
    ("filter by" "+" "upgrades" "regexp" "keyword" "starred" "clear")
    ("Sort by" "+" "Package name" "Status" "*(star)")))

(defun paradox-menu-quick-help ()
  "Show short key binding help for `paradox-menu-mode'.
The full list of keys can be viewed with \\[describe-mode]."
  (interactive)
  (message (mapconcat 'paradox--prettify-key-descriptor
                      paradox--key-descriptors "\n")))

(defun paradox-quit-and-close (kill)
  "Bury this buffer and close the window.
With prefix KILL, kill the buffer instead of burying."
  (interactive "P")
  (let ((log (get-buffer-window paradox--commit-list-buffer)))
    (when (window-live-p log)
      (quit-window kill log))
    (quit-window kill)))

(defun paradox-menu-visit-homepage (pkg)
  "Visit the homepage of package named PKG.
PKG is a symbol.  Interactively it is the package under point."
  (interactive '(nil))
  (let ((url (paradox--package-homepage
              (paradox--get-or-return-package pkg))))
    (if (stringp url)
        (browse-url url)
      (message "Package %s has no homepage."
               (propertize (symbol-name pkg)
                           'face 'font-lock-keyword-face)))))

(defun paradox-menu-copy-homepage-as-kill (pkg)
  "Save the homepage of package named PKG as kill.
PKG is a symbol.  Interactively it is the package under point."
  (interactive '(nil))
  (let ((url (paradox--package-homepage
	       (paradox--get-or-return-package pkg))))
    (if (stringp url)
	(progn (kill-new url)
	       (message "copied \"%s\"" url))
      (message "Package %s has no homepage."
	       (propertize (symbol-name pkg)
			   'face 'font-lock-keyword-face)))))

(defun paradox-menu-mark-star-unstar ()
  "Star or unstar a package and move to the next line."
  (interactive)
  (paradox--enforce-github-token
   (unless paradox--user-starred-repos
     (paradox--refresh-user-starred-list))
   ;; Get package name
   (let* ((pkg (paradox--get-or-return-package nil))
          (repo (gethash pkg paradox--package-repo-list))
          will-delete)
     (unless pkg (error "Couldn't find package-name for this entry"))
     ;; (Un)Star repo
     (if (not repo)
         (message "This package is not a GitHub repo.")
       (setq will-delete (paradox--starred-repo-p repo))
       (paradox--star-repo repo will-delete)
       (cl-incf (gethash pkg paradox--star-count 0)
                (if will-delete -1 1))
       (tabulated-list-set-col paradox--column-name-star
                               (paradox--package-star-count pkg)))))
  (forward-line 1))

(defun paradox-menu-view-commit-list (pkg)
  "Visit the commit list of package named PKG.
PKG is a symbol.  Interactively it is the package under point."
  (interactive '(nil))
  (let* ((name (paradox--get-or-return-package pkg))
         (repo (gethash name paradox--package-repo-list)))
    (if repo
        (with-selected-window
            (display-buffer (get-buffer-create paradox--commit-list-buffer))
          (paradox-commit-list-mode)
          (setq paradox--package-repo repo)
          (setq paradox--package-name name)
          (setq paradox--package-version
                (paradox--get-installed-version name))
          (setq paradox--package-tag-commit-alist
                (paradox--get-tag-commit-alist repo))
          (paradox--commit-list-update-entries)
          (tabulated-list-print))
      (message "Package %s is not a GitHub repo." pkg))))


;;; Mode-line Construction
(defcustom paradox-local-variables
  '(mode-line-mule-info
    mode-line-client
    mode-line-remote mode-line-position
    column-number-mode size-indication-mode)
  "Variables which will take special values on the Packages buffer.
This is a list, where each element is either SYMBOL or (SYMBOL . VALUE).

Each SYMBOL (if it is bound) will be locally set to VALUE (or
nil) on the Packages buffer."
  :type '(repeat (choice symbol (cons symbol sexp)))
  :group 'paradox-menu
  :package-version '(paradox . "0.1"))

(defcustom paradox-display-buffer-name nil
  "If nil, *Packages* buffer name won't be displayed in the mode-line."
  :type 'boolean
  :group 'paradox-menu
  :package-version '(paradox . "0.2"))

(defun paradox--build-buffer-id (st n)
  "Return a list that propertizes ST and N for the mode-line."
  `((:propertize ,st
                 face paradox-mode-line-face)
    (:propertize ,(int-to-string n)
                 face mode-line-buffer-id)))

(defun paradox--update-mode-line ()
  "Update `mode-line-format'."
  (mapc #'paradox--set-local-value paradox-local-variables)
  (let ((total-lines (int-to-string (length tabulated-list-entries))))
    (paradox--update-mode-line-front-space total-lines)
    (paradox--update-mode-line-buffer-identification total-lines)))

(defun paradox--update-mode-line-buffer-identification (_total-lines)
  "Update `mode-line-buffer-identification'.
TOTAL-LINES is currently unused."
  (require 'spinner)
  (setq mode-line-buffer-identification
        `((paradox-display-buffer-name
           ,(propertized-buffer-identification
             (format "%%%sb" (length (buffer-name)))))
          (paradox--current-filter (:propertize ("[" paradox--current-filter "]") face paradox-mode-line-face))
          (paradox--upgradeable-packages-any?
           (:eval (paradox--build-buffer-id " Upgrade:" paradox--upgradeable-packages-number)))
          (package-menu--new-package-list
           (:eval (paradox--build-buffer-id " New:" (paradox--cas "new"))))
          ,(paradox--build-buffer-id " Installed:" (+ (paradox--cas "installed")
                                                      (paradox--cas "dependency")
                                                      (paradox--cas "unsigned")))
          (paradox--current-filter
           "" ,(paradox--build-buffer-id " Total:" (length package-archive-contents))))))

(defvar sml/col-number)
(defvar sml/numbers-separator)
(defvar sml/col-number-format)
(defvar sml/line-number-format)
(defvar sml/position-construct)
(declare-function sml/compile-position-construct "sml")
(defvar sml/post-id-separator)
(defun paradox--update-mode-line-front-space (total-lines)
  "Update `mode-line-front-space'.
TOTAL-LINES is the number of lines in the buffer."
  (if (memq 'sml/post-id-separator mode-line-format)
      (progn
        (add-to-list (make-local-variable 'mode-line-front-space)
                     (propertize " (" 'face 'sml/col-number))
        (setq column-number-mode line-number-mode)
        (set (make-local-variable 'sml/numbers-separator) "")
        (set (make-local-variable 'sml/col-number-format)
             (format "/%s)" total-lines))
        (set (make-local-variable 'sml/line-number-format)
             (format "%%%sl" (length total-lines)))
        (make-local-variable 'sml/position-construct)
        (sml/compile-position-construct))
    (set (make-local-variable 'mode-line-front-space)
         `(line-number-mode
           ("(" (:propertize ,(format "%%%sl" (length total-lines)) face mode-line-buffer-id) "/"
            ,total-lines ")")))
    (set (make-local-variable 'mode-line-modified) nil)))

(defun paradox--set-local-value (x)
  "Locally set value of (car X) to (cdr X)."
  (let ((sym (or (car-safe x) x)))
    (when (boundp sym)
      (set (make-local-variable sym) (cdr-safe x)))))

(defun paradox--prettify-key-descriptor (desc)
  "Prettify DESC to be displayed as a help menu."
  (if (listp desc)
      (if (listp (cdr desc))
          (mapconcat 'paradox--prettify-key-descriptor desc "   ")
        (let ((place (cdr desc))
              (out (car desc)))
          (setq out (propertize out 'face 'paradox-comment-face))
          (add-text-properties place (1+ place) '(face paradox-highlight-face) out)
          out))
    (paradox--prettify-key-descriptor (cons desc 0))))

(provide 'paradox-menu)
;;; paradox-menu.el ends here
