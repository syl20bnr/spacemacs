;;; pdf-outline.el --- Outline for PDF buffer -*- lexical-binding: t -*-

;; Copyright (C) 2013, 2014  Andreas Politz

;; Author: Andreas Politz <politza@fh-trier.de>
;; Keywords: files, multimedia

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

(require 'outline)
(require 'pdf-links)
(require 'pdf-view)
(require 'pdf-util)
(require 'cl-lib)
(require 'imenu)
(require 'let-alist)

;;; Code:

;;
;; User options
;;

(defgroup pdf-outline nil
  "Display a navigatable outline of a PDF document."
  :group 'pdf-tools)

(defcustom pdf-outline-buffer-indent 2
  "The level of indent in the Outline buffer."
  :type 'integer)

(defcustom pdf-outline-enable-imenu t
  "Whether `imenu' should be enabled in PDF documents."
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)))

(defcustom pdf-outline-imenu-keep-order t
  "Whether `imenu' should be advised not to reorder the outline."
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)))

(defcustom pdf-outline-imenu-use-flat-menus nil
  "Whether the constructed Imenu should be a list, rather than a tree."
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)))

(defcustom pdf-outline-display-buffer-action '(nil . nil)
  "The display action used, when displaying the outline buffer."
  :type display-buffer--action-custom-type)

(defcustom pdf-outline-display-labels nil
  "Whether the outline should display labels instead of page numbers.

Usually a page's label is its displayed page number."
  :type 'boolean)

(defcustom pdf-outline-fill-column fill-column
  "The value of `fill-column' in pdf outline buffers.

Set to nil to disable line wrapping."
  :type 'integer)

(defvar pdf-outline-minor-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "o") #'pdf-outline)
    km)
  "Keymap used for `pdf-outline-minor-mode'.")

(defvar pdf-outline-buffer-mode-map
  (let ((kmap (make-sparse-keymap)))
    (dotimes (i 10)
      (define-key kmap (vector (+ i ?0)) #'digit-argument))
    (define-key kmap "-" #'negative-argument)
    (define-key kmap (kbd "p") #'previous-line)
    (define-key kmap (kbd "n") #'next-line)
    (define-key kmap (kbd "b") #'outline-backward-same-level)
    (define-key kmap (kbd "d") #'hide-subtree)
    (define-key kmap (kbd "a") #'show-all)
    (define-key kmap (kbd "s") #'show-subtree)
    (define-key kmap (kbd "f") #'outline-forward-same-level)
    (define-key kmap (kbd "u") #'pdf-outline-up-heading)
    (define-key kmap (kbd "Q") #'hide-sublevels)
    (define-key kmap (kbd "<") #'beginning-of-buffer)
    (define-key kmap (kbd ">") #'pdf-outline-end-of-buffer)
    (define-key kmap (kbd "TAB") #'outline-toggle-children)
    (define-key kmap (kbd "RET") #'pdf-outline-follow-link)
    (define-key kmap (kbd "C-o") #'pdf-outline-display-link)
    (define-key kmap (kbd "SPC") #'pdf-outline-display-link)
    (define-key kmap [mouse-1] #'pdf-outline-mouse-display-link)
    (define-key kmap (kbd "o") #'pdf-outline-select-pdf-window)
    (define-key kmap (kbd ".") #'pdf-outline-move-to-current-page)
    ;; (define-key kmap (kbd "Q") #'pdf-outline-quit)
    (define-key kmap (kbd "C-c C-q") #'pdf-outline-quit-and-kill)
    (define-key kmap (kbd "q") #'quit-window)
    (define-key kmap (kbd "M-RET") #'pdf-outline-follow-link-and-quit)
    (define-key kmap (kbd "C-c C-f") #'pdf-outline-follow-mode)
    kmap)
  "Keymap used in `pdf-outline-buffer-mode'.")

;;
;; Internal Variables
;;

(define-button-type 'pdf-outline
  'face nil
  'keymap nil)

(defvar-local pdf-outline-pdf-window nil
  "The PDF window corresponding to this outline buffer.")

(defvar-local pdf-outline-pdf-document nil
  "The PDF filename or buffer corresponding to this outline
  buffer.")

(defvar-local pdf-outline-follow-mode-last-link nil)

;;
;; Functions
;;

;;;###autoload
(define-minor-mode pdf-outline-minor-mode
  "Display an outline of a PDF document.

This provides a PDF's outline on the menu bar via imenu.
Additionally the same outline may be viewed in a designated
buffer.

\\{pdf-outline-minor-mode-map}"
  :group 'pdf-outline
  (pdf-util-assert-pdf-buffer)
  (cond
   (pdf-outline-minor-mode
    (when pdf-outline-enable-imenu
      (pdf-outline-imenu-enable)))
   (t
    (when pdf-outline-enable-imenu
      (pdf-outline-imenu-disable)))))

(define-derived-mode pdf-outline-buffer-mode outline-mode "PDF Outline"
  "View and traverse the outline of a PDF file.

Press \\[pdf-outline-display-link] to display the PDF document,
\\[pdf-outline-select-pdf-window] to select its window,
\\[pdf-outline-move-to-current-page] to move to the outline item
of the current page, \\[pdf-outline-follow-link] to goto the
corresponding page or \\[pdf-outline-follow-link-and-quit] to
additionally quit the Outline.

\\[pdf-outline-follow-mode] enters a variant of
`next-error-follow-mode'.  Most `outline-mode' commands are
rebound to their respective last character.

\\{pdf-outline-buffer-mode-map}"
  (setq-local outline-regexp "\\( *\\).")
  (setq-local outline-level
              (lambda nil (1+ (/ (length (match-string 1))
                                 pdf-outline-buffer-indent))))

  (toggle-truncate-lines 1)
  (setq buffer-read-only t)
  (when (> (count-lines 1 (point-max))
           (* 1.5 (frame-height)))
    (hide-sublevels 1))
  (message "%s"
           (substitute-command-keys
            (concat
             "Try \\[pdf-outline-display-link], "
             "\\[pdf-outline-select-pdf-window], "
             "\\[pdf-outline-move-to-current-page] or "
             "\\[pdf-outline-follow-link-and-quit]"))))

(define-minor-mode pdf-outline-follow-mode
  "Display links as point moves."
  :group 'pdf-outline
  (setq pdf-outline-follow-mode-last-link nil)
  (cond
   (pdf-outline-follow-mode
    (add-hook 'post-command-hook #'pdf-outline-follow-mode-pch nil t))
   (t
    (remove-hook 'post-command-hook #'pdf-outline-follow-mode-pch t))))

(defun pdf-outline-follow-mode-pch ()
  (let ((link (pdf-outline-link-at-pos (point))))
    (when (and link
               (not (eq link pdf-outline-follow-mode-last-link)))
      (setq pdf-outline-follow-mode-last-link link)
      (pdf-outline-display-link (point)))))

;;;###autoload
(defun pdf-outline (&optional buffer no-select-window-p)
  "Display an PDF outline of BUFFER.

BUFFER defaults to the current buffer.  Select the outline
buffer, unless NO-SELECT-WINDOW-P is non-nil."
  (interactive (list nil (or current-prefix-arg
                             (consp last-nonmenu-event))))
  (let ((win
         (display-buffer
          (pdf-outline-noselect buffer)
          pdf-outline-display-buffer-action)))
    (unless no-select-window-p
      (select-window win))))

(defun pdf-outline-noselect (&optional buffer)
  "Create an PDF outline of BUFFER, but don't display it."
  (save-current-buffer
    (and buffer (set-buffer buffer))
    (pdf-util-assert-pdf-buffer)
    (let* ((pdf-buffer (current-buffer))
           (pdf-file (pdf-view-buffer-file-name))
           (pdf-window (and (eq pdf-buffer (window-buffer))
                            (selected-window)))
           (bname (pdf-outline-buffer-name))
           (buffer-exists-p (get-buffer bname))
           (buffer (get-buffer-create bname)))
      (with-current-buffer buffer
        (setq-local fill-column pdf-outline-fill-column)
        (unless buffer-exists-p
          (when (= 0 (save-excursion
                       (pdf-outline-insert-outline pdf-buffer)))
            (kill-buffer buffer)
            (error "PDF has no outline"))
          (pdf-outline-buffer-mode))
        (set (make-local-variable 'other-window-scroll-buffer)
             pdf-buffer)
        (setq pdf-outline-pdf-window pdf-window
              pdf-outline-pdf-document (or pdf-file pdf-buffer))
        (current-buffer)))))

(defun pdf-outline-buffer-name (&optional pdf-buffer)
  (unless pdf-buffer (setq pdf-buffer (current-buffer)))
  (let ((buf (format "*Outline %s*" (buffer-name pdf-buffer))))
    ;; (when (buffer-live-p (get-buffer buf))
    ;;   (kill-buffer buf))
    buf))

(defun pdf-outline-insert-outline (pdf-buffer)
  (let ((labels (and pdf-outline-display-labels
                     (pdf-info-pagelabels pdf-buffer)))
        (nitems 0))
    (dolist (item (pdf-info-outline pdf-buffer))
      (let-alist item
        (when (eq .type 'goto-dest)
          (insert-text-button
           (concat
            (make-string (* (1- .depth) pdf-outline-buffer-indent) ?\s)
            .title
            (if (> .page 0)
                (format " (%s)"
                        (if labels
                            (nth (1- .page) labels)
                          .page))
              "(invalid)"))
           'type 'pdf-outline
           'help-echo (pdf-links-action-to-string item)
           'pdf-outline-link item)
          (newline)
          (cl-incf nitems))))
    nitems))

(defun pdf-outline-get-pdf-window (&optional if-visible-p)
  (save-selected-window
    (let* ((buffer (cond
                    ((buffer-live-p pdf-outline-pdf-document)
                     pdf-outline-pdf-document)
                    ((bufferp pdf-outline-pdf-document)
                     (error "PDF buffer was killed"))
                    (t
                     (or
                      (find-buffer-visiting
                       pdf-outline-pdf-document)
                      (find-file-noselect
                       pdf-outline-pdf-document)))))
           (pdf-window
            (if (and (window-live-p pdf-outline-pdf-window)
                     (eq buffer
                         (window-buffer pdf-outline-pdf-window)))
                pdf-outline-pdf-window
              (or (get-buffer-window buffer)
                  (and (null if-visible-p)
                       (display-buffer
                        buffer
                        '(nil (inhibit-same-window . t))))))))
      (setq pdf-outline-pdf-window pdf-window))))


;;
;; Commands
;;

(defun pdf-outline-move-to-current-page ()
  "Move to the item corresponding to the current page.

Open nodes as necessary."
  (interactive)
  (let (page)
    (with-selected-window (pdf-outline-get-pdf-window)
      (setq page (pdf-view-current-page)))
    (pdf-outline-move-to-page page)))

(defun pdf-outline-quit-and-kill ()
  "Quit browsing the outline and kill its buffer."
  (interactive)
  (pdf-outline-quit t))

(defun pdf-outline-quit (&optional kill)
  "Quit browsing the outline buffer."
  (interactive "P")
  (let ((win (selected-window)))
    (pdf-outline-select-pdf-window t)
    (quit-window kill win)))

(defun pdf-outline-up-heading (arg &optional invisible-ok)
  "Like `outline-up-heading', but `push-mark' first."
  (interactive "p")
  (let ((pos (point)))
    (outline-up-heading arg invisible-ok)
    (unless (= pos (point))
      (push-mark pos))))

(defun pdf-outline-end-of-buffer ()
  "Move to the end of the outline buffer."
  (interactive)
  (let ((pos (point)))
    (goto-char (point-max))
    (when (and (eobp)
               (not (bobp))
               (null (button-at (point))))
      (forward-line -1))
    (unless (= pos (point))
      (push-mark pos))))

(defun pdf-outline-link-at-pos (&optional pos)
  (unless pos (setq pos (point)))
  (let ((button (or (button-at pos)
                    (button-at (1- pos)))))
    (and button
         (button-get button
                     'pdf-outline-link))))

(defun pdf-outline-follow-link (&optional pos)
  "Select PDF window and move to the page corresponding to POS."
  (interactive)
  (unless pos (setq pos (point)))
  (let ((link (pdf-outline-link-at-pos pos)))
    (unless link
      (error "Nothing to follow here"))
    (select-window (pdf-outline-get-pdf-window))
    (pdf-links-action-perform link)))

(defun pdf-outline-follow-link-and-quit (&optional pos)
  "Select PDF window and move to the page corresponding to POS.

Then quit the outline window."
  (interactive)
  (let ((link (pdf-outline-link-at-pos (or pos (point)))))
    (pdf-outline-quit)
    (unless link
      (error "Nothing to follow here"))
    (pdf-links-action-perform link)))

(defun pdf-outline-display-link (&optional pos)
  "Display the page corresponding to the link at POS."
  (interactive)
  (unless pos (setq pos (point)))
  (let ((inhibit-redisplay t)
        (link (pdf-outline-link-at-pos pos)))
    (unless link
      (error "Nothing to follow here"))
    (with-selected-window (pdf-outline-get-pdf-window)
      (pdf-links-action-perform link))
    (force-mode-line-update t)))

(defun pdf-outline-mouse-display-link (event)
  "Display the page corresponding to the position of EVENT."
  (interactive "@e")
  (pdf-outline-display-link
   (posn-point (event-start event))))

(defun pdf-outline-select-pdf-window (&optional no-create-p)
  "Display and select the PDF document window."
  (interactive)
  (let ((win (pdf-outline-get-pdf-window no-create-p)))
    (and (window-live-p win)
         (select-window win))))

(defun pdf-outline-toggle-subtree ()
  "Toggle hidden state of the current complete subtree."
  (interactive)
  (save-excursion
    (outline-back-to-heading)
    (if (not (outline-invisible-p (line-end-position)))
        (hide-subtree)
      (show-subtree))))

(defun pdf-outline-move-to-page (page)
  "Move to an outline item corresponding to PAGE."
  (interactive
   (list (or (and current-prefix-arg
                  (prefix-numeric-value current-prefix-arg))
             (read-number "Page: "))))
  (goto-char (pdf-outline-position-of-page page))
  (save-excursion
    (while (outline-invisible-p)
      (outline-up-heading 1 t)
      (show-children)))
  (save-excursion
    (when (outline-invisible-p)
      (outline-up-heading 1 t)
      (show-children)))
  (back-to-indentation))

(defun pdf-outline-position-of-page (page)
  (let (curpage)
    (save-excursion
      (goto-char (point-min))
      (while (and (setq curpage (alist-get 'page (pdf-outline-link-at-pos)))
                  (< curpage page))
        (forward-line))
      (point))))



;;
;; Imenu Support
;;


;;;###autoload
(defun pdf-outline-imenu-enable ()
  "Enable imenu in the current PDF buffer."
  (interactive)
  (pdf-util-assert-pdf-buffer)
  (setq-local imenu-create-index-function
              (if pdf-outline-imenu-use-flat-menus
                  'pdf-outline-imenu-create-index-flat
                'pdf-outline-imenu-create-index-tree))
  (imenu-add-to-menubar "PDF Outline"))

(defun pdf-outline-imenu-disable ()
  "Disable imenu in the current PDF buffer."
  (interactive)
  (pdf-util-assert-pdf-buffer)
  (setq-local imenu-create-index-function nil)
  (local-set-key [menu-bar index] nil)
  (when (eq pdf-view-mode-map
            (keymap-parent (current-local-map)))
    (use-local-map (keymap-parent (current-local-map)))))


(defun pdf-outline-imenu-create-item (link &optional labels)
  (let-alist link
    (list (format "%s (%s)" .title (if labels
                                       (nth (1- .page) labels)
                                     .page))
          0
          'pdf-outline-imenu-activate-link
          link)))

(defun pdf-outline-imenu-create-index-flat ()
  (let ((labels (and pdf-outline-display-labels
                     (pdf-info-pagelabels)))
        index)
    (dolist (item (pdf-info-outline))
      (let-alist item
        (when (eq .type 'goto-dest)
          (push (pdf-outline-imenu-create-item item labels)
                index))))
    (nreverse index)))


(defun pdf-outline-imenu-create-index-tree ()
  (pdf-outline-imenu-create-index-tree-1
   (pdf-outline-treeify-outline-list
    (cl-remove-if-not
     (lambda (type)
       (eq type 'goto-dest))
     (pdf-info-outline)
     :key (apply-partially 'alist-get 'type)))
   (and pdf-outline-display-labels
        (pdf-info-pagelabels))))

(defun pdf-outline-imenu-create-index-tree-1 (nodes &optional labels)
  (mapcar (lambda (node)
            (let (children)
              (when (consp (caar node))
                (setq children (cdr node)
                      node (car node)))
              (let ((item
                     (pdf-outline-imenu-create-item node labels)))
                (if children
                    (cons (alist-get 'title node)
                          (cons item (pdf-outline-imenu-create-index-tree-1
                                      children labels)))
                  item))))
          nodes))

(defun pdf-outline-treeify-outline-list (list)
  (when list
    (let ((depth (alist-get 'depth (car list)))
          result)
      (while (and list
                  (>= (alist-get 'depth (car list))
                      depth))
        (when (= (alist-get 'depth (car list)) depth)
          (let ((item (car list)))
            (when (and (cdr list)
                       (>  (alist-get 'depth (cadr list))
                           depth))
              (setq item
                    (cons
                     item
                     (pdf-outline-treeify-outline-list (cdr list)))))
            (push item result)))
        (setq list (cdr list)))
      (reverse result))))

(defun pdf-outline-imenu-activate-link (&rest args)
  ;; bug #14029
  (when (eq (nth 2 args) 'pdf-outline-imenu-activate-link)
    (setq args (cdr args)))
  (pdf-links-action-perform (nth 2 args)))

(defadvice imenu--split-menu (around pdf-outline activate)
  "Advice to keep the original outline order.

 Calls `pdf-outline-imenu--split-menu' instead, if in a PDF
 buffer and `pdf-outline-imenu-keep-order' is non-nil."
  (if (not (and (pdf-util-pdf-buffer-p)
                pdf-outline-imenu-keep-order))
      ad-do-it
    (setq ad-return-value
          (pdf-outline-imenu--split-menu menulist title))))

(defvar imenu--rescan-item)
(defvar imenu-sort-function)
(defvar imenu-create-index-function)
(defvar imenu-max-items)

(defun pdf-outline-imenu--split-menu (menulist title)
  "Replacement function for `imenu--split-menu'.

This function does not move sub-menus to the top, therefore
keeping the original outline order of the document.  Also it does
not call `imenu-sort-function'."
  (let ((menulist (copy-sequence menulist))
        keep-at-top)
    (if (memq imenu--rescan-item menulist)
        (setq keep-at-top (list imenu--rescan-item)
              menulist (delq imenu--rescan-item menulist)))
    (if (> (length menulist) imenu-max-items)
        (setq menulist
              (mapcar
               (lambda (menu)
                 (cons (format "From: %s" (caar menu)) menu))
               (imenu--split menulist imenu-max-items))))
    (cons title
          (nconc (nreverse keep-at-top) menulist))))


(provide 'pdf-outline)

;;; pdf-outline.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; End:
