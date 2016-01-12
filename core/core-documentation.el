;;; core-spacemacs.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(require 'org)
(require 'ox-publish)
(require 's)
(require 'dash)

(defvar spacemacs--category-names
  '(("config-files" . "Configuration files")
    ("email" . "E-mail")
    ("irc" . "IRC")
    ("lang" . "Programming languages"))
  "Special names for categories. Used to generate the layers list.")

(defun spacemacs//generate-layers-from-path (path level)
  "Add all layers found in PATH to the current buffer, at org level LEVEL."
  (let* ((all-subs (directory-files path t nil nil))
         (layers (-filter (lambda (p)
                            (eq 'layer (configuration-layer//directory-type p)))
                          all-subs))
         (categories (-filter (lambda (p)
                                (eq 'category (configuration-layer//directory-type p)))
                              all-subs)))
    (message "%S" layers)
    (dolist (l layers)
      (let ((layer-name (file-name-nondirectory l))
            (target-path (concat (file-relative-name
                                  l (concat user-emacs-directory "layers"))
                                 "/README.org")))
        (insert (format "- [[file:%s][%s]]\n" target-path layer-name))))
    (dolist (c categories)
      (let* ((category-name (substring (file-name-nondirectory c) 1))
             (pretty-name (or (cdr (assoc category-name spacemacs--category-names))
                              (s-capitalize (replace-regexp-in-string
                                             "-" " " category-name)))))
        (message "%S" category-name)
        (unless (string= "distribution" category-name)
          (insert (format "\n%s %s\n" level pretty-name))
          (spacemacs//generate-layers-from-path c (concat level "*"))
          )))))

(defun spacemacs//generate-layers-file ()
  "Generate the layers list file."
  (interactive)
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Configuration layers\n")
    (insert "#+HTML_HEAD_EXTRA: <link rel=\"stylesheet\" type=\"text/css\" href=\"../css/readtheorg.css\" />\n\n")
    (insert "* Table of Contents\n")
    (org-set-tags-to '("TOC_4_org" "noexport"))
    (insert "* General layers\n")
    (spacemacs//generate-layers-from-path configuration-layer-directory "*")
    (write-file (concat user-emacs-directory "layers/LAYERS.org"))))

(defun spacemacs//format-toc (&rest r)
  (if (not (null (car r)))
      (let* ((toc (car r))
             (heading-pos (s-index-of "Contents</h" toc)))
        (if (not (null heading-pos))
            (let* ((end-of-heading-pos (+ (length "Contents") heading-pos))
                   (beginning-of-heading (substring toc 0 end-of-heading-pos))
                   (rest-of-toc (substring toc end-of-heading-pos)))
              (format "%s<a href=\"#\">Close</a>%s" beginning-of-heading rest-of-toc))
          toc))
    (car r)))

(defun spacemacs//format-content (&rest r)
  (let* ((content (car r))
         (div-string "<div id=\"content\">")
         (toc-string "<div id=\"toggle-sidebar\"><a href=\"#table-of-contents\"><h2>Table of Contents</h2></a></div>")
         (has-toc (s-index-of "Table of Contents" content))
         (beginning-of-content-div-pos (+ (length div-string) (s-index-of div-string content)))
         (beginning-of-content (substring content 0 beginning-of-content-div-pos))
         (rest-of-content (substring content beginning-of-content-div-pos)))
    (if (not (null has-toc))
        (format "%s\n%s%s" beginning-of-content toc-string rest-of-content)
      content)))

(defun spacemacs/publish-doc ()
  "Publishe the documentation to doc/export/."
  (interactive)
  (advice-add 'org-html-toc :filter-return #'spacemacs//format-toc)
  (advice-add 'org-html-template :filter-return #'spacemacs//format-content)
  (let* ((header
          "<link rel=\"stylesheet\" type=\"text/css\"
                 href=\"http://www.pirilampo.org/styles/readtheorg/css/htmlize.css\"/>
          <script src=\"https://ajax.googleapis.com/ajax/libs/jquery/2.1.3/jquery.min.js\"></script>
          <script src=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/js/bootstrap.min.js\"></script>
          <script type=\"text/javascript\"
                  src=\"http://www.pirilampo.org/styles/lib/js/jquery.stickytableheaders.js\"></script>
          <script type=\"text/javascript\"
                  src=\"http://www.pirilampo.org/styles/readtheorg/js/readtheorg.js\"></script>")
         (publish-target (concat user-emacs-directory "export/"))
         (org-html-htmlize-output-type 'css)
         (org-publish-project-alist
          `(("spacemacs"
             :components ("spacemacs-doc"
                          "spacemacs-doc-static"
                          "layers-doc"
                          "layers-doc-static"))
            ("spacemacs-doc"
             :base-directory ,spacemacs-docs-directory
             :base-extension "org"
             :publishing-directory ,(concat publish-target "doc/")
             :publishing-function org-html-publish-to-html
             :headline-levels 4
             :html-head ,header)
            ("layers-doc"
             :base-directory ,(concat user-emacs-directory "layers/")
             :base-extension "org"
             :recursive t
             :publishing-directory ,(concat publish-target "layers/")
             :publishing-function org-html-publish-to-html
             :preparation-function spacemacs//generate-layers-file
             :exclude "extensions"
             :html-head ,header)
            ("spacemacs-doc-static"
             :base-directory ,spacemacs-docs-directory
             :base-extension "png"
             :recursive t
             :publishing-directory ,(concat publish-target "doc/")
             :publishing-function org-publish-attachment)
            ("layers-doc-static"
             :base-directory ,(concat user-emacs-directory "layers/")
             :base-extension "jpg\\|png\\|gif"
             :recursive t
             :publishing-directory ,(concat publish-target "layers/")
             :publishing-function org-publish-attachment))))
    (org-publish-project "spacemacs"))
  (advice-remove 'org-html-toc #'spacemacs//format-toc)
  (advice-remove 'org-html-template #'spacemacs//format-content))

(provide 'core-documentation)
