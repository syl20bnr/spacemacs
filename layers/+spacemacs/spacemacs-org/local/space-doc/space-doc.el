;;; space-doc.el --- Space-macs org minor mode. -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;; Description:
;; This package provides:
;;   - `space-doc-mode' - buffer local minor mode
;; for viewing the Space-macs documentation files.
;; The mode hides org meta tags to improve readability.
;;   - `org-mode' link-type "https" that opens the local
;; copies of the Space-macs documentation files with
;; `space-macs/view-org-file' and supports GitHub style
;; heading links.
;;
;; For example, the link:
;;  https://github.com/syl20bnr/space-macs/blob/develop/layers/org/README.org#lnk
;; Will be handled similary to as if it was:
;; file:~/.e-macs.d/layers/org/README.org::*links
;; Also the `space-doc' mode will be applied.

;;; License: GPLv3
;;; Code:
(require 'face-remap)
(require 'org)
(require 'org-compat)

(defgroup space-doc nil "Minor mode for viewing Space-macs documentation files."
  :group 'convenience)

;; NOTE: Dont forget to update Space-macs FAQ if you modify this list!
(defcustom space-macs-space-doc-modificators
  '(org-indent-mode
    view-mode
    hide-line-numbers
    alternative-emphasis
    alternative-tags-look
    link-protocol
    org-block-line-face-remap
    org-kbd-face-remap
    resize-inline-images)
  "List of `space-doc' modificators."
  :type '(set (const org-indent-mode)
              (const view-mode)
              (const hide-line-numbers)
              (const alternative-emphasis)
              (const alternative-tags-look)
              (const link-protocol)
              (const org-bl-face-remap)
              (const org-kbd-face-remap)
              (const resize-inline-images))
  :group 'space-doc)

(defvar space-macs-space-doc-modificators-functions
  '((org-indent-mode              . space-macs//space-doc-org-indent-mode)
    (view-mode                    . space-macs//space-doc-view-mode)
    (hide-line-numbers            . space-macs//space-doc-hide-line-numbers)
    (alternative-emphasis         . space-macs//space-doc-alternative-emphasis)
    (alternative-tags-look        . space-macs//space-doc-alternative-tags-look)
    (link-protocol                . space-macs//space-doc-link-protocol)
    (org-block-line-face-remap    . space-macs//space-doc-org-bl-face-remap)
    (org-kbd-face-remap           . space-macs//space-doc-org-kbd-face-remap)
    (resize-inline-images         . space-macs//space-doc-resize-inline-images))
  "alist of `space-doc' modificator (tag . function) for `org-mode' buffers.
The functions work with a current buffer and accept ENABLE(flag) argument.
If the argument has non-nil value - enable the modifications introduced
by the function. Otherwise - disable. The tags used in
`space-macs-space-doc-modificators'")

(define-minor-mode space-doc-mode
  "Buffer local minor mode for viewing Space-macs documentation files.
This mode:
 - hides `org-mode' meta tags like #+TITLE: while
keeping their content visible.
 - Improves emphasized region apparence.
 - enables buffer local link  opening with `space-macs//space-doc-open'.
=================================================
= THE MODE IS CUSTOMIZABLE - read Space-macs FAQ =
================================================="
  :init-value nil
  :lighter " SD"
  :group 'space-doc
  (if (derived-mode-p 'org-mode)
      (let ((inhibit-read-only t))
        (space-macs//space-doc-set-cache +1)
        (dolist (modificator space-macs-space-doc-modificators-functions)
          (when (member (car modificator)
                        space-macs-space-doc-modificators)
            (funcall (cdr modificator) space-doc-mode))))
    ;; Force `org-mode' to replace font text properties with the default ones.
    (unless space-doc-mode (org-font-lock-ensure))
    (message (format "space-doc-mode error:%s isn't an org-mode buffer"
                     (buffer-name)))
    (setq space-doc-mode nil)))

(defvar-local space-macs--space-doc-org-kbd-face-remap-cookie nil
  "Cookie for org-kbd-face remapping.")

(defvar-local space-macs--space-doc-org-block-begin-line-face-remap-cookie nil
  "Cookie for org-block-begin-line-face remapping.")

(defvar-local space-macs--space-doc-org-block-end-line-face-remap-cookie nil
  "Cookie for org-block-end-line-face ")

(defun space-macs//space-doc-org-indent-mode (&optional flag)
  "Enable `org-indent-mode' if flag is non nil, disable it otherwise.
This functions is aimed to be used with `space-macs-space-doc-modificators'."
  (org-indent-mode (if flag 1 -1)))

(defun space-macs//space-doc-view-mode (&optional flag)
  "Enable `view-mode' if flag is non nil, disable it otherwise.
This functions is aimed to be used with `space-macs-space-doc-modificators'."
  (view-mode (if flag 1 -1)))

(cl-defstruct space-macs--space-doc-cache-struct
  marker-face
  btn-marker-face
  kbd-marker)

(defvar-local space-macs--space-doc-cache nil
  "Global variable of struct `space-macs-space-doc-cache-struct'.
It is set by `space-macs//space-doc-set-cache'.")

(defun space-macs//space-doc-set-cache (&optional flag)
  "Set `space-macs--space-doc-cache'.
This functions is aimed to be used with `space-macs-space-doc-modificators'."
  (setq space-macs--space-doc-cache
        (if flag
            (let* ((kbd-bg (or (face-background 'org-kbd)
                               (face-background 'region)
                               'unspecified))
                   (table-bg (or (face-background 'org-table)
                                 (face-background 'default)
                                 'unspecified))
                   (marker-face
                    `(:inherit     org-table
                      :foreground ,table-bg))
                   (btn-marker-face
                    `(:inherit             org-kbd
                      :distant-foreground ,kbd-bg
                      :foreground         ,kbd-bg))
                   (kbd-marker
                    (dolist (el org-emphasis-alist)
                      (when (member 'org-kbd el)
                        (return (car el))))))
              (make-space-macs--space-doc-cache-struct
               :marker-face     marker-face
               :btn-marker-face btn-marker-face
               :kbd-marker      kbd-marker)))))

(defun space-macs//space-doc-hide-line-numbers (&optional enable)
  "If ENABLE is non-nil then toggle off the line numbers.
This functions is aimed to be used with `space-macs-space-doc-modificators'."
  (if enable
      (space-macs/toggle-line-numbers-off)
    (when dotspace-macs-line-numbers
      (space-macs/toggle-line-numbers-on))))

(defun space-macs//space-doc-org-do-emphasis-faces-advice (found)
  "If FOUND has non-nil value then modify emphasized regions
appearances in the current buffer. The function uses
`match-data' set by `org-do-emphasis-faces' function."
  ;; `org-do-emphasis-faces' returns non-nil value when it
  ;; found a region to emphasize.
  (when (and space-doc-mode
             found
             (not (and
                   (match-string 4)
                   (string-empty-p
                    (replace-regexp-in-string "\\*+"
                                              ""
                                              (match-string 4))))))
    (space-macs//space-doc-emphasis-region
     (match-beginning 2)
     (match-end 2)))
  found)

(defun space-macs//space-doc-advice-org-do-emphasis-faces (&optional enable)
  "Advise org-do-emphasis-faces.
If ENABLE is non-nil, add advice `org-do-emphasis-faces' function with
`space-macs//space-doc-org-do-emphasis-faces-advice'.
NOTE: `org-do-emphasis-faces' is lazy and will emphasize only part of the
current buffer so piggybacking it should be pretty performant solution."
  (when enable
    (advice-add 'org-do-emphasis-faces
                :after
                #'space-macs//space-doc-org-do-emphasis-faces-advice)))

(defsubst space-macs//space-doc-add-region-edge-text-property
    (begin end property &optional face)
  "Add text PROPERTY to the first and last character of the BEGIN END
 text region with `add-text-properties' or if FACE has non-nil value
`add-face-text-property'."
  (let ((edge-sub-regs (list (list (1+ begin) begin)
                             (list (1- end)   end))))
    (dolist (edge-sub-reg edge-sub-regs)
      (funcall (if face
                   'add-face-text-property
                 'add-text-properties)
               (car edge-sub-reg)
               (cadr edge-sub-reg)
               property))))

(defun space-macs//space-doc-emphasis-region (begin end)
  "Emphasis region based on its leading character.
The character should be one of the markers from `org-emphasis-alist'."
  (let ((kbd-face (space-macs--space-doc-cache-struct-btn-marker-face
                   space-macs--space-doc-cache))
        (marker-face (space-macs--space-doc-cache-struct-marker-face
                      space-macs--space-doc-cache))
        (begin (or begin (point-min)))
        (end   (or end (point-max))))
    (if (string= (buffer-substring-no-properties begin
                                                 (1+ begin))
                 (space-macs--space-doc-cache-struct-kbd-marker
                  space-macs--space-doc-cache))
        (space-macs//space-doc-add-region-edge-text-property begin
                                                            end
                                                            kbd-face
                                                            t)
      (if (save-excursion
            (goto-char begin)
            (beginning-of-line)
            (looking-at-p org-table-any-line-regexp))
          ;; If inside table.
          (space-macs//space-doc-add-region-edge-text-property begin
                                                              end
                                                              marker-face
                                                              t)
        (space-macs//space-doc-add-region-edge-text-property
         begin
         end
         '(invisible space-macs--space-doc-invisible-marker))))))
(byte-compile 'space-macs//space-doc-emphasis-region)

(defun space-macs//space-doc-alternative-emphasis (&optional enable)
  "Emphasis overlays.
If ENABLE is non-nil, change the look of regions which have already
been emphasized by `org-do-emphasis-faces' in the current buffer.
Otherwise revert to the normal look.
This functions is aimed to be used with `space-macs-space-doc-modificators'."
  (if enable
      (progn
        (make-local-variable 'org-emphasis-regexp-components)
        (setcar (nthcdr 2 org-emphasis-regexp-components)
                " \t\n")
        (org-set-emph-re 'org-emphasis-regexp-components
                         org-emphasis-regexp-components)
        (setq-local org-emphasis-alist '(("*" bold)
                                         ("/" italic)
                                         ("_" underline)
                                         ("=" org-verbatim verbatim)
                                         ("~" org-kbd)
                                         ("+" (:strike-through t))))
        (space-macs//space-doc-advice-org-do-emphasis-faces enable)
        (add-to-invisibility-spec 'space-macs--space-doc-invisible-marker)
        (dolist (emphasized-region
                 (space-macs//space-doc-find-regions-by-text-property
                  'org-emphasis t))
          (space-macs//space-doc-emphasis-region
           (car  emphasized-region)
           (cadr emphasized-region))))
    (kill-local-variable 'org-emphasis-alist)
    (kill-local-variable 'org-emphasis-regexp-components)
    (remove-from-invisibility-spec 'space-macs--space-doc-invisible-marker))
  (space-macs//space-doc-set-cache +1))

(defun space-macs//space-doc-org-kbd-face-remap (&optional enable)
  "Remove boxes from key bindings.
If ENABLE is non-nil, removes boxes from the `org-kbd'face in the current
`org-mode' buffer.
Otherwise, reverts them to default.
This functions is aimed to be used with `space-macs-space-doc-modificators'."
  (if enable
      (setq space-macs--space-doc-org-kbd-face-remap-cookie
           (face-remap-add-relative 'org-kbd
                                    `(:box nil)))
    (when (bound-and-true-p space-macs--space-doc-org-kbd-face-remap-cookie)
      (face-remap-remove-relative
       space-macs--space-doc-org-kbd-face-remap-cookie))))

(defun space-macs//space-doc-resize-inline-images (&optional enable)
  "Resize inline images.
If ENABLE is non nil then resize inline images.
This functions is aimed to be used with `space-macs-space-doc-modificators'."
  ;; resizing is always performed even when the image is smaller
  ;; so we don't resize in README.org buffers for now
  (let ((org-image-actual-width
         (and enable
              (not (string-match-p ".*README.org\\'" (buffer-file-name)))
              600)))
    (org-display-inline-images)))

(defsubst space-macs//space-doc-tags-fontify (startish endish)
  "Fontify `org-mode' tags in the fuzzy region that starts
 before STARTISH and end after ENDISH."
  ;; TODO add more types of tags or meta-line if needed.
  (let ((invisible-org-meta-tags-list
         `(;; Hide TITLE tag.
           "\\([ \t]*\\#\\+TITLE\\:\[ \t]*\\)"
           ;; Hide CAPTION logo meta line.
           "\\(\n.*\\#\\+CAPTION\\:.*\\)"
           ;; Hide PROPERTIES lines.
           "\\(\n.*\\:PROPERTIES\\:.*\\)"
           "\\(\n.*\\:CUSTOM_ID\\:.*\\)"
           "\\(\n.*\\:END\\:.*\\)"
           ;; Hide TOC-ORG tag and spaces before it.
           ;; Use modified `toc-org-toc-org-regexp' because
           ;; the original one matches whole string.
           ,(concat "\\([ \t]*:toc\\([@_][0-9]\\|\\([@_][0-9]"
                    "[@_][a-zA-Z]+\\)\\)?:\\($\\|[^ ]*:$\\)\\)")
           ;; Hide empty line before #+BEGIN_SRC tag if
           ;; background color of the `org-block-begin-line'
           ;; face is unspecified.
           ,(unless (face-background 'org-block-begin-line)
              "\n\\(\n\\)[ \t]*\\#\\+begin_src.*$")
           ;; Hide empty line after #+END_SRC tag if
           ;; background color of the `org-block-end-line'
           ;; face is unspecified and the next line isn't
           ;;an org headline.
           ,(unless (face-background 'org-block-end-line)
              "^[ \t]*\\#\\+end_src.*\n\\(\n\\)[^\\*]")))
        (start (save-excursion (goto-char (or startish
                                              (point-min)))
                               (point-at-bol -2)))
        (end   (save-excursion (goto-char (or endish
                                              (point-max)))
                               (point-at-eol 2))))
    ;; Remove nils.
    (setq invisible-org-meta-tags-list
          (remove nil invisible-org-meta-tags-list))
    ;; Make `org-mode' meta tags invisible.
    (dolist (tag invisible-org-meta-tags-list)
      (save-excursion
        (goto-char start)
        (while (re-search-forward tag end t)
          (add-text-properties
           (match-beginning 1)
           (match-end 1)
           (list 'invisible
                 'space-macs--space-doc-invisible-marker)))))))

(defun space-macs//space-doc-font-lock-fontify-region-function
    (start end &optional verbose)
  "Wrapper around `font-lock-default-fontify-region' function for
the buffer local value of `font-lock-fontify-region-function'.
Makes sure that `font-lock-default-fontify-region' text property
persist after `org-mode' shenanigans.
NOTE: Not using `advice-add' because it is global modification.
FIXME: Find cleaner solution."
  (font-lock-default-fontify-region  start end verbose)
  (space-macs//space-doc-tags-fontify start end))
(byte-compile 'space-macs//space-doc-font-lock-fontify-region-function)

(defun space-macs//space-doc-alternative-tags-look (&optional enable)
  "Modify meta tag appearance.
If ENABLE is non-nil, modify `org-mode' meta tags appearance in the current
buffer.
This functions is aimed to be used with `space-macs-space-doc-modificators'."
  (if enable
      (setq-local font-lock-fontify-region-function
                  'space-macs//space-doc-font-lock-fontify-region-function)
    (kill-local-variable 'font-lock-fontify-region-function)))

(defun space-macs//space-doc-org-bl-face-remap (&optional enable)
  "Hide drawers.
If ENABLE is non-nil, hide text of the code block meta lines in the current
buffer. If the blocks have background color text won't be masked because it
makes them look ugly with some themes.
If ENABLE has nil, revert to the default.
This functions is aimed to be used with `space-macs-space-doc-modificators'."
  (if enable
      (let* ((default-bg (or (face-background 'default)
                             'unspecified))
             (org-bb-bg (or (face-background 'org-block-begin-line)
                            (face-background 'org-meta-line)))
             (hide-bb-text-face `(:inherit org-block-begin-line
                                  :foreground         ,default-bg
                                  :distant-foreground ,default-bg))
             (org-bn-bg (or (face-background 'org-block-end-line)
                            (face-background 'org-meta-line)))
             (hide-bn-text-face `(:inherit org-block-end-line
                                  :foreground         ,default-bg
                                  :distant-foreground ,default-bg)))
        (unless org-bb-bg
          (setq space-macs--space-doc-org-block-begin-line-face-remap-cookie
               (face-remap-add-relative 'org-block-begin-line
                                        hide-bb-text-face)))
        (unless org-bn-bg
          (setq space-macs--space-doc-org-block-end-line-face-remap-cookie
               (face-remap-add-relative 'org-block-end-line
                                        hide-bn-text-face))))
    (when (bound-and-true-p
           space-macs--space-doc-org-block-begin-line-face-remap-cookie)
      (face-remap-remove-relative
       space-macs--space-doc-org-block-begin-line-face-remap-cookie))
    (when (bound-and-true-p
           space-macs--space-doc-org-block-end-line-face-remap-cookie)
      (face-remap-remove-relative
       space-macs--space-doc-org-block-end-line-face-remap-cookie))))

(defun space-macs//space-doc-link-protocol (&optional enable)
  "Open HTTPS links in the current buffer.
If ENABLE is non-nil, use `space-macs//space-doc-open' to open HTTPS links
in the current `org-mode' buffer.
Otherwise open them in the browser (default behavior).
This functions is aimed to be used with `space-macs-space-doc-modificators'."
  (if enable
      (progn
        ;; Make `space-doc' https link opener buffer local
        ;; and enable it only when `space-doc' mode is enabled.
        (make-local-variable 'org-link-types)
        (make-local-variable 'org-link-protocols)
        (org-link-set-parameters "https"
                                 :follow #'space-macs//space-doc-open))
    (kill-local-variable 'org-link-types)
    (kill-local-variable 'org-link-protocols)))

(defun space-macs//space-doc-open (path)
  "Open PATH link.
If PATH argument is a link to an .org file that is located in the Space-macs
GitHub repository then visit the local copy of the file with
`space-macs/view-org-file'.
Open all other links with `browse-url'."
  (let ((git-url-root-regexp
         (concat "\\/\\/github\\.com\\/syl20bnr\\/space-macs\\/blob"
                 "\\/[^/]+\\/\\(.*\\.org\\)\\(\\#.*\\)?")))
    (if (string-match git-url-root-regexp path)
        (space-macs/view-org-file (concat space-macs-start-directory
                                         (match-string 1 path))
                                 (or (match-string 2 path)
                                     "^")
                                 'subtree)
      (browse-url (concat "https:" path)))))

(defun space-macs//space-doc-find-regions-by-text-property
    (property value &optional start end)
  "Return a list of pairs (region-beginning region-end) in
the current buffer. If START or END has non-nil value - use them as
boundaries.
NOTE: It can find only fontified regions."
  (let ((p-min (or start (point-min)))
        (p-max (or end (point-max)))
        (r-end nil)
        (ret (list)))
    (while (not (= p-min p-max))
      (setq p-min (or (text-property-any p-min p-max property value)
                      (point-max))
            r-end (or (text-property-not-all p-min p-max property value)
                      (point-max))
            ret (append ret (unless (= p-min r-end)
                              (list(list p-min r-end))))
            p-min r-end))
    ret))

(provide 'space-doc)
;;; space-doc.el ends here.


