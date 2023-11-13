;;; org-download.el --- Image drag-and-drop for Org-mode. -*- lexical-binding: t -*-

;; Copyright (C) 2014-2019 Free Software Foundation, Inc.

;; Author: Oleh Krehel
;; URL: https://github.com/abo-abo/org-download
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3") (async "1.2"))
;; Keywords: multimedia images screenshots download

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This extension facilitates moving images from point A to point B.
;;
;; Point A (the source) can be:
;; 1. An image inside your browser that you can drag to Emacs.
;; 2. An image on your file system that you can drag to Emacs.
;; 3. A local or remote image address in kill-ring.
;;    Use the `org-download-yank' command for this.
;;    Remember that you can use "0 w" in `dired' to get an address.
;; 4. An screenshot taken using `gnome-screenshot' or `scrot' or `gm'.
;;    Use the `org-download-screenshot' command for this.
;;    Customize the backend with  `org-download-screenshot-method'.
;;
;; Point B (the target) is an Emacs `org-mode' buffer where the inline
;; link will be inserted.  Several customization options will determine
;; where exactly on the file system the file will be stored.
;;
;; They are:
;; `org-download-method':
;; a. 'attach => use `org-mode' attachment machinery
;; b. 'directory => construct the directory in two stages:
;;    1. first part of the folder name is:
;;       * either "." (current folder)
;;       * or `org-download-image-dir' (if it's not nil).
;;         `org-download-image-dir' becomes buffer-local when set,
;;         so each file can customize this value, e.g with:
;;         # -*- mode: Org; org-download-image-dir: "~/Pictures/foo"; -*-
;;    2. second part is:
;;       * `org-download-heading-lvl' is nil => ""
;;       * `org-download-heading-lvl' is n => the name of current
;;         heading with level n. Level count starts with 0,
;;         i.e. * is 0, ** is 1, *** is 2 etc.
;;         `org-download-heading-lvl' becomes buffer-local when set,
;;         so each file can customize this value, e.g with:
;;         # -*- mode: Org; org-download-heading-lvl: nil; -*-
;;
;; `org-download-timestamp':
;; optionally add a timestamp to the file name.
;;
;; Customize `org-download-backend' to choose between `url-retrieve'
;; (the default) or `wget' or `curl'.
;;
;;; Code:


(require 'cl-lib)
(require 'async)
(require 'url-parse)
(require 'url-http)
(require 'org)
(require 'org-attach)
(require 'org-element)

(defgroup org-download nil
  "Image drag-and-drop for org-mode."
  :group 'org
  :prefix "org-download-")

(defcustom org-download-method 'directory
  "The way images should be stored."
  :type '(choice
          (const :tag "Directory" directory)
          (const :tag "Attachment" attach)
          (function :tag "Custom function")))

(defcustom org-download-image-dir nil
  "If set, images will be stored in this directory instead of \".\".
See `org-download--dir-1' for more info."
  :type '(choice
          (const :tag "Default" nil)
          (string :tag "Directory")))
(make-variable-buffer-local 'org-download-image-dir)

(defcustom org-download-heading-lvl 0
  "Heading level to be used in `org-download--dir-2'."
  :type
  '(choice integer (const :tag "None" nil)))
(make-variable-buffer-local 'org-download-heading-lvl)

(defvar org-download-path-last-file nil
  "Variable to hold the full path of the last downloaded file.
See `org-download-rename-last-file'.")

(defcustom org-download-backend t
  "Method to use for downloading."
  :type '(choice
          (const :tag "wget" "wget \"%s\" -O \"%s\"")
          (const :tag "curl" "curl \"%s\" -o \"%s\"")
          (const :tag "url-retrieve" t)))

(defcustom org-download-timestamp "%Y-%m-%d_%H-%M-%S_"
  "This `format-time-string'-style string will be appended to the file name.
Set this to \"\" if you don't want time stamps."
  :type 'string)

(defcustom org-download-img-regex-list
  '("<img +src=\"" "<img +\\(class=\"[^\"]+\"\\)? *src=\"")
  "This regex is used to unalias links that look like images.
The html to which the links points will be searched for these
regexes, one by one, until one succeeds.  The found image address
will be used."
  :type '(repeat string))

(defcustom org-download-screenshot-method "gnome-screenshot -a -f %s"
  "The tool to capture screenshots."
  :type '(choice
          (const :tag "gnome-screenshot" "gnome-screenshot -a -f %s")
          (const :tag "scrot" "scrot -s %s")
          (const :tag "flameshot" "flameshot gui --raw > %s")
          (const :tag "gm" "gm import %s")
          (const :tag "imagemagick/import" "import %s")
          (const :tag "imagemagick/import + xclip to save to clipboard"
           "export filename=\"%s\"; import png:\"$filename\" ;xclip -selection clipboard -target image/png -filter < \"$filename\" &>/dev/null")
          (const :tag "xfce4-screenshooter" "xfce4-screenshooter -r -o cat > %s")
          ;; screenshot method in ms-windows, /capture=4 stands for interactive.
          (const :tag "IrfanView" "i_view64 /capture=4 /convert=\"%s\"")
          ;; screenshot script in osx, -i stands for interactive,
          ;; press space key to toggle between selection and
          ;; window/application mode.
          (const :tag "screencapture" "screencapture -i %s")
          ;; KDE screenshot application
          (const :tag "spectacle" "spectacle -br -o %s")
          ;; take an image that is already on the clipboard, for Linux
          (const :tag "xclip"
           "xclip -selection clipboard -t image/png -o > %s")
          ;; take an image that is already on the clipboard, for Windows
          (const :tag "imagemagick/convert" "convert clipboard: %s")
          ; capture region, for Wayland
          (const :tag "grim + slurp" "grim -g \"$(slurp)\" %s")
          (function :tag "Custom function")))

(defcustom org-download-screenshot-basename "screenshot.png"
  "Default base filename to use for screenshots."
  :type 'string)

(defcustom org-download-screenshot-file (expand-file-name org-download-screenshot-basename temporary-file-directory)
  "The file to capture screenshots."
  :type 'string)

(defcustom org-download-image-html-width 0
  "When non-zero add #+attr_html: :width tag to the image."
  :type 'integer)

(defcustom org-download-image-latex-width 0
  "When non-zero add #+attr_latex: :width tag to the image."
  :type 'integer)

(defcustom org-download-image-org-width 0
  "When non-zero add #+attr_org: :width tag to the image."
  :type 'integer)

(defcustom org-download-image-attr-list nil
  "Add attr info to the image.
For example:

  (\"#+attr_html: :width 80% :align center\"
   \"#+attr_org: :width 100px\")"
  :type '(repeat string))

(defcustom org-download-delete-image-after-download nil
  "When non-nil delete local image after download."
  :type 'boolean)

(defcustom org-download-display-inline-images t
  "When non-nil display inline images in org buffer after download."
  :type
  '(choice
    (const :tag "On" t)
    (const :tag "Off" nil)
    (const :tag "Posframe" posframe)))

(defvar org-download-posframe-show-params
  '(;; Please do not remove :timeout or set it to large.
    :timeout 1
    :internal-border-width 1
    :internal-border-color "red"
    :min-width 40
    :min-height 10
    :poshandler posframe-poshandler-window-center)
  "List of parameters passed to `posframe-show'.")

(declare-function posframe-workable-p "ext:posframe")
(declare-function posframe-show "ext:posframe")

(defun org-download-org-mode-p ()
  "Return `t' if major-mode or derived-mode-p equals 'org-mode, otherwise `nil'."
  (or (eq major-mode 'org-mode) (when (derived-mode-p 'org-mode) t)))

(defun org-download--display-inline-images ()
  (cond
   ((eq org-download-display-inline-images t)
    (org-display-inline-images))
   ((eq org-download-display-inline-images 'posframe)
    (require 'posframe)
    (when (posframe-workable-p)
      (let ((buffer (get-buffer-create " *org-download-image")))
        (with-current-buffer buffer
          (erase-buffer)
          (insert-image-file org-download-path-last-file))
        (apply #'posframe-show
               buffer
               org-download-posframe-show-params))))))

(defun org-download-get-heading (lvl)
  "Return the heading of the current entry's LVL level parent."
  (save-excursion
    (let ((cur-lvl (org-current-level)))
      (if cur-lvl
          (progn
            (unless (= cur-lvl 1)
              (org-up-heading-all (- (1- (org-current-level)) lvl)))
            (let ((heading (nth 4 (org-heading-components))))
              (if heading
                  (replace-regexp-in-string
                   " " "_"
                   heading)
                "")))
        ""))))

(defun org-download--dir-1 ()
  "Return the first part of the directory path for `org-download--dir'.
It's `org-download-image-dir', unless it's nil.  Then it's \".\"."
  (or org-download-image-dir "."))

(defun org-download--dir-2 ()
  "Return the second part of the directory path for `org-download--dir'.
Unless `org-download-heading-lvl' is nil, it's the name of the current
`org-download-heading-lvl'-leveled heading."
  (when org-download-heading-lvl
    (org-download-get-heading
     org-download-heading-lvl)))

(defun org-download--dir ()
  "Return the directory path for image storage.

The path is composed from `org-download--dir-1' and `org-download--dir-2'.
The directory is created if it didn't exist before."
  (if (org-download-org-mode-p)
      (let* ((part1 (org-download--dir-1))
             (part2 (org-download--dir-2))
             (dir (if part2
                      (expand-file-name part2 part1)
                    part1)))
        (unless (file-exists-p dir)
          (make-directory dir t))
        dir)
    default-directory))

(defvar org-download-file-format-function #'org-download-file-format-default)

(defun org-download--fullname (link &optional ext)
  "Return the file name where LINK will be saved to.

It's affected by `org-download--dir'.
EXT can hold the file extension, in case LINK doesn't provide it."
  (let ((filename
         (replace-regexp-in-string
          "%20" " "
          (file-name-nondirectory
           (car (url-path-and-query
                 (url-generic-parse-url link))))))
        (dir (org-download--dir)))
    (when (string-match ".*?\\.\\(?:png\\|jpg\\)\\(.*\\)$" filename)
      (setq filename (replace-match "" nil nil filename 1)))
    (when ext
      (setq filename (concat filename "." ext)))
    (abbreviate-file-name
     (expand-file-name
      (funcall org-download-file-format-function filename)
      dir))))

(defun org-download-file-format-default (filename)
  "It's affected by `org-download-timestamp'."
  (concat
   (format-time-string org-download-timestamp)
   filename))

(defvar org-download--file-content nil
  "When non-nil, store the file name of an already downloaded file.")

(defun org-download--image (link filename)
  "Save LINK to FILENAME asynchronously and show inline images in current buffer."
  (when (string= "file" (url-type (url-generic-parse-url link)))
    (setq link (url-unhex-string (url-filename (url-generic-parse-url link)))))
  (cond ((and (not (file-remote-p link))
              (file-exists-p link))
         (copy-file link (expand-file-name filename)))
        (org-download--file-content
         (copy-file org-download--file-content (expand-file-name filename))
         (setq org-download--file-content nil))
        ((eq org-download-backend t)
         (org-download--image/url-retrieve link filename))
        (t
         (org-download--image/command org-download-backend link filename))))

(defun org-download--image/command (command link filename)
  "Using COMMAND, save LINK to FILENAME.
COMMAND is a format-style string with two slots for LINK and FILENAME."
  (async-start
   `(lambda () (shell-command
                ,(format command link
                         (expand-file-name filename))))
   (let ((cur-buf (current-buffer)))
     (lambda (_x)
       (with-current-buffer cur-buf
         (org-download--display-inline-images))))))

(defun org-download--write-image (status filename)
  "Write current buffer STATUS to FILENAME."
  (let ((err (plist-get status :error)))
    (when err
      (error
       "HTTP error %s"
       (downcase (nth 2 (assq (nth 2 err) url-http-codes))))))
  (delete-region
   (point-min)
   (progn
     (re-search-forward "\n\n" nil 'move)
     (point)))
  (let ((coding-system-for-write 'no-conversion))
    (write-region nil nil filename nil nil nil 'confirm)))

(defun org-download--image/url-retrieve (link filename)
  "Save LINK to FILENAME using `url-retrieve'."
  (url-retrieve
   link
   (lambda (status filename buffer)
     (org-download--write-image status filename)
     (cond ((org-download-org-mode-p)
            (with-current-buffer buffer
              (org-download--display-inline-images)))
           ((eq major-mode 'dired-mode)
            (let ((inhibit-message t))
              (with-current-buffer (dired (file-name-directory filename))
                (revert-buffer nil t))))))
   (list
    (expand-file-name filename)
    (current-buffer))
   nil t))

(defun org-download-yank ()
  "Call `org-download-image' with current kill."
  (interactive)
  (let ((k (current-kill 0)))
    (unless (url-type (url-generic-parse-url k))
      (user-error "Not a URL: %s" k))
    (org-download-image
     (replace-regexp-in-string
      "\n+$" "" k))))

(defun org-download-screenshot (&optional basename)
  "Capture screenshot and insert the resulting file.
The screenshot tool is determined by `org-download-screenshot-method'."
  (interactive)
  (let* ((screenshot-dir (file-name-directory org-download-screenshot-file))
         (org-download-screenshot-file
          (if basename
              (concat screenshot-dir basename) org-download-screenshot-file)))
    (make-directory screenshot-dir t)
    (if (functionp org-download-screenshot-method)
        (funcall org-download-screenshot-method
                 org-download-screenshot-file)
      (shell-command-to-string
       (format org-download-screenshot-method
               org-download-screenshot-file)))
    (when (file-exists-p org-download-screenshot-file)
      (org-download-image org-download-screenshot-file)
      (delete-file org-download-screenshot-file))))

(defun org-download-clipboard (&optional basename)
  "Capture the image from the clipboard and insert the resulting file."
  (interactive)
  (let ((org-download-screenshot-method
         (cl-case system-type
           (gnu/linux
            (if (string= "wayland" (getenv "XDG_SESSION_TYPE"))
                (if (executable-find "wl-paste")
                    "wl-paste -t image/png > %s"
                  (user-error
                   "Please install the \"wl-paste\" program included in wl-clipboard"))
              (if (executable-find "xclip")
                  "xclip -selection clipboard -t image/png -o > %s"
                (user-error
                 "Please install the \"xclip\" program"))))
           ((windows-nt cygwin)
            (if (executable-find "convert")
                "convert clipboard: %s"
              (user-error
               "Please install the \"convert\" program included in ImageMagick")))
           ((darwin berkeley-unix)
            (if (executable-find "pngpaste")
                "pngpaste %s"
              (user-error
               "Please install the \"pngpaste\" program from Homebrew."))))))
    (org-download-screenshot basename)))

(declare-function org-attach-dir "org-attach")
(declare-function org-attach-attach "org-attach")
(declare-function org-attach-sync "org-attach")

(defun org-download-annotate-default (link)
  "Annotate LINK with the time of download."
  (format "#+DOWNLOADED: %s @ %s\n"
          (if (equal link org-download-screenshot-file)
              "screenshot"
            link)
          (format-time-string "%Y-%m-%d %H:%M:%S")))

(defvar org-download-annotate-function
  #'org-download-annotate-default
  "Function that takes LINK and returns a string.
It's inserted before the image link and is used to annotate it.")

(defvar org-download-link-format
  "[[file:%s]]\n"
  "Format of the file link to insert.")

(defcustom org-download-link-format-function #'org-download-link-format-function-default
  "Function that takes FILENAME and returns a org link."
  :type 'function)

(defcustom org-download-abbreviate-filename-function #'file-relative-name
  "Function that takes FILENAME and returns an abbreviated file name."
  :type '(choice
          (const :tag "relative" file-relative-name)
          (const :tag "absolute" expand-file-name)))

(defun org-download-link-format-function-default (filename)
  "The default function of `org-download-link-format-function'."
  (if (and (>= (string-to-number org-version) 9.3)
           (eq org-download-method 'attach))
      (format "[[attachment:%s]]\n"
              (org-link-escape
               (file-relative-name filename (org-attach-dir))))
    (format org-download-link-format
            (org-link-escape
             (funcall org-download-abbreviate-filename-function filename)))))

(defun org-download--detect-ext (link buffer)
  (let (ext)
    (with-current-buffer buffer
      (cond ((let ((regexes org-download-img-regex-list)
                   lnk)
               (while (and (not lnk) regexes)
                 (goto-char (point-min))
                 (when (re-search-forward (pop regexes) nil t)
                   (backward-char)
                   (setq lnk (read (current-buffer)))))
               (when lnk
                 (setq link lnk))))
            ((progn
               (goto-char (point-min))
               (when (re-search-forward "^Content-Type: image/\\(.*\\)$" nil t)
                 (setq ext (match-string 1)))))
            ((progn
               (goto-char (point-min))
               (when (re-search-forward "^Content-Type: application/pdf" nil t)
                 (setq ext "pdf"))
               (re-search-forward "^%PDF")
               (beginning-of-line)
               (write-region
                (point) (point-max)
                (setq org-download--file-content "/tmp/org-download.pdf"))
               t))
            (t
             (error "Link %s does not point to an image; unaliasing failed" link)))
      (list link ext))))

(defun org-download--parse-link (link)
  (cond ((image-type-from-file-name link)
         (list link nil))
        ((string-match "^file:/+" link)
         (list link nil))
        (t
         (let ((buffer (url-retrieve-synchronously link t)))
           (org-download--detect-ext link buffer)))))

(defun org-download-image (link)
  "Save image at address LINK to `org-download--dir'."
  (interactive "sUrl: ")
  (let* ((link-and-ext (org-download--parse-link link))
         (filename
          (cond ((and (derived-mode-p 'org-mode)
                      (eq org-download-method 'attach))
                 (let ((org-download-image-dir (org-attach-dir t))
                       org-download-heading-lvl)
                   (apply #'org-download--fullname link-and-ext)))
                ((fboundp org-download-method)
                 (funcall org-download-method link))
                (t
                 (apply #'org-download--fullname link-and-ext)))))
    (setq org-download-path-last-file filename)
    (org-download--image link filename)
    (when (org-download-org-mode-p)
      (when (eq org-download-method 'attach)
        (org-attach-attach filename nil 'none))
      (org-download-insert-link link filename))
    (when (and (eq org-download-delete-image-after-download t)
               (not (url-handler-file-remote-p (current-kill 0))))
      (delete-file link delete-by-moving-to-trash))))

(defun org-download-rename-at-point ()
  "Rename image at point."
  (interactive)
  (let* ((dir-path (org-download--dir))
         (current-name (file-name-nondirectory
                        (org-element-property :path (org-element-context))))
         (current-path (concat dir-path "/" current-name))
         (ext (file-name-extension current-name))
         (new-name (read-string "Rename file at point to: " (file-name-sans-extension current-name)))
         (new-path (concat dir-path "/" new-name "." ext)))
    (rename-file current-path new-path)
    (message "File successfully renamed...")
    (org-download-replace-all current-name (concat new-name "." ext))))

(defun org-download-rename-last-file ()
  "Rename the last downloaded file saved in your computer."
  (interactive)
  (let* ((dir-path (org-download--dir))
         (newname (read-string "Rename last file to: " (file-name-base org-download-path-last-file)))
         (ext (file-name-extension org-download-path-last-file))
         (newpath (concat dir-path "/" newname "." ext)))
    (when org-download-path-last-file
      (rename-file org-download-path-last-file newpath 1)
      (org-download-replace-all
       (file-name-nondirectory org-download-path-last-file)
       (concat newname "." ext))
      (setq org-download-path-last-file newpath)
      (org-download--display-inline-images))))

(defun org-download-replace-all (oldpath newpath)
  "Function to search for the OLDPATH inside the buffer and replace it by the NEWPATH."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward oldpath nil t)
      (replace-match newpath))))

(defun org-download-insert-link (link filename)
  (let* ((beg (point))
         (line-beg (line-beginning-position))
         (indent (- beg line-beg))
         (in-item-p (org-in-item-p))
         str)
    (if (looking-back "^[ \t]+" line-beg)
        (delete-region (match-beginning 0) (match-end 0))
      (newline))
    (insert (funcall org-download-annotate-function link))
    (dolist (attr org-download-image-attr-list)
      (insert attr "\n"))
    (insert (if (= org-download-image-html-width 0)
                ""
              (format "#+attr_html: :width %dpx\n" org-download-image-html-width)))
    (insert (if (= org-download-image-latex-width 0)
                ""
              (format "#+attr_latex: :width %dcm\n" org-download-image-latex-width)))
    (insert (if (= org-download-image-org-width 0)
                ""
              (format "#+attr_org: :width %dpx\n" org-download-image-org-width)))
    (insert (funcall org-download-link-format-function filename))
    (org-download--display-inline-images)
    (setq str (buffer-substring-no-properties line-beg (point)))
    (when in-item-p
      (indent-region line-beg (point) indent))
    str))

(defun org-download--at-comment-p ()
  "Check if current line begins with #+DOWLOADED:."
  (save-excursion
    (move-beginning-of-line nil)
    (looking-at "#\\+DOWNLOADED:")))

(defvar org-link-any-re)

(defun org-download-delete ()
  "Delete inline image link on current line, and the file that it points to."
  (interactive)
  (cond ((org-download--at-comment-p)
         (delete-region (line-beginning-position)
                        (line-end-position))
         (org-download--delete (line-beginning-position)
                               nil
                               1))
        ((region-active-p)
         (org-download--delete (region-beginning)
                               (region-end))
         (delete-region (region-beginning)
                        (region-end)))

        ((looking-at org-link-any-re)
         (let ((fname (org-link-unescape
                       (match-string-no-properties 2))))
           (when (file-exists-p fname)
             (delete-file fname)
             (delete-region (match-beginning 0)
                            (match-end 0))
             (when (eolp)
               (delete-char 1)))))

        (t (org-download--delete (line-beginning-position)
                                 (line-end-position))))
  (when (eq org-download-method 'attach)
    (org-attach-sync)))

(defcustom org-download-edit-cmd "gimp %s"
  "Command for editing an image link."
  :type 'string)

(defun org-download-edit ()
  "Open the image at point for editing."
  (interactive)
  (let ((context (org-element-context)))
    (if (not (eq (car-safe context) 'link))
        (user-error "Not on a link")
      (start-process-shell-command
       "org-download-edit"
       "org-download-edit"
       (format org-download-edit-cmd
               (shell-quote-wildcard-pattern
                (url-unhex-string (plist-get (cadr context) :path))))))))

(defun org-download--delete (beg end &optional times)
  "Delete inline image links and the files they point to between BEG and END.

When TIMES isn't nil, delete only TIMES links."
  (unless times
    (setq times most-positive-fixnum))
  (save-excursion
    (goto-char beg)
    (while (and (>= (cl-decf times) 0)
                (and (string-match "\\[\\[\\(\\w+\\)" org-download-link-format)
                     (let ((link-name (match-string 1 org-download-link-format)))
                       (re-search-forward (format "\\[\\[%s:\\([^]]*\\)\\]\\]" link-name) end t))))
      (let ((str (match-string-no-properties 1)))
        (delete-region beg
                       (match-end 0))
        (when (file-exists-p str)
          (delete-file str))))))

(defun org-download-dnd-fallback (uri action)
  (let ((dnd-protocol-alist
         (rassq-delete-all
          'org-download-dnd
          (copy-alist dnd-protocol-alist))))
    (dnd-handle-one-url nil action uri)))

(defun org-download-dnd (uri action)
  "When in `org-mode' and URI points to image, download it.
Otherwise, pass URI and ACTION back to dnd dispatch."
  (cond ((org-download-org-mode-p)
         (condition-case nil
             (org-download-image uri)
           (error
            (org-download-dnd-fallback uri action))))
        ((eq major-mode 'dired-mode)
         (org-download-dired uri))
        ;; redirect to someone else
        (t
         (org-download-dnd-fallback uri action))))

(defun org-download-dired (uri)
  "Download URI to current directory."
  (raise-frame)
  (org-download-image uri))

(defun org-download-dnd-base64 (uri _action)
  (when (org-download-org-mode-p)
    (when (string-match "^data:image/png;base64," uri)
      (let* ((me (match-end 0))
             (filename (org-download--fullname
                        (substring-no-properties uri me (+ me 10))
                        "png")))
        (with-temp-buffer
          (insert (base64-decode-string (substring uri me)))
          (write-file filename))
        (org-download-insert-link filename filename)))))

;;;###autoload
(defun org-download-enable ()
  "Enable org-download."
  (unless (eq (cdr (assoc "^\\(https?\\|ftp\\|file\\|nfs\\):" dnd-protocol-alist))
              'org-download-dnd)
    (setq dnd-protocol-alist
          `(("^\\(https?\\|ftp\\|file\\|nfs\\):" . org-download-dnd)
            ("^data:" . org-download-dnd-base64)
            ,@dnd-protocol-alist))))

(defun org-download-disable ()
  "Disable org-download."
  (rassq-delete-all 'org-download-dnd dnd-protocol-alist))

(org-download-enable)

(provide 'org-download)

;;; org-download.el ends here
