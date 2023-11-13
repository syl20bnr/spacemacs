;;; pdf-tools.el --- Support library for PDF documents -*- lexical-binding:t -*-

;; Copyright (C) 2013, 2014  Andreas Politz

;; Author: Andreas Politz <mail@andreas-politz.de>
;; Maintainer: Vedang Manerikar <vedang.manerikar@gmail.com>
;; URL: http://github.com/vedang/pdf-tools/
;; Keywords: files, multimedia
;; Package: pdf-tools
;; Version: 1.1.0
;; Package-Requires: ((emacs "26.3") (tablist "1.0") (let-alist "1.0.4"))

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
;; PDF Tools is, among other things, a replacement of DocView for PDF
;; files.  The key difference is, that pages are not prerendered by
;; e.g. ghostscript and stored in the file-system, but rather created
;; on-demand and stored in memory.
;;
;; Note: This package is built and tested on GNU/Linux systems. It
;; works on macOS and Windows, but is officially supported only on
;; GNU/Linux systems. This package will not make macOS or Windows
;; specific functionality changes, behaviour on these systems is
;; provided as-is.
;;
;; Note: If you ever update it, you need to restart Emacs afterwards.
;;
;; To activate the package put
;;
;; (pdf-tools-install)
;;
;; somewhere in your .emacs.el .
;;
;; M-x pdf-tools-help RET
;;
;; gives some help on using the package and
;;
;; M-x pdf-tools-customize RET
;;
;; offers some customization options.

;; Features:
;;
;; * View
;;   View PDF documents in a buffer with DocView-like bindings.
;;
;; * Isearch
;;   Interactively search PDF documents like any other buffer. (Though
;;   there is currently no regexp support.)
;;
;; * Follow links
;;   Click on highlighted links, moving to some part of a different
;;   page, some external file, a website or any other URI.  Links may
;;   also be followed by keyboard commands.
;;
;; * Annotations
;;   Display and list text and markup annotations (like underline),
;;   edit their contents and attributes (e.g. color), move them around,
;;   delete them or create new ones and then save the modifications
;;   back to the PDF file.
;;
;; * Attachments
;;   Save files attached to the PDF-file or list them in a Dired buffer.
;;
;; * Outline
;;   Use imenu or a special buffer to examine and navigate the PDF's
;;   outline.
;;
;; * SyncTeX
;;   Jump from a position on a page directly to the TeX source and
;;   vice-versa.
;;
;; * Misc
;;    + Display PDF's metadata.
;;    + Mark a region and kill the text from the PDF.
;;    + Search for occurrences of a string.
;;    + Keep track of visited pages via a history.

;;; Code:

(require 'pdf-view)
(require 'pdf-util)
(require 'pdf-info)
(require 'cus-edit)
(require 'compile)
(require 'cl-lib)
(require 'package)



;; * ================================================================== *
;; * Customizables
;; * ================================================================== *

(defgroup pdf-tools nil
  "Support library for PDF documents."
  :group 'data)

(defgroup pdf-tools-faces nil
  "Faces determining the colors used in the pdf-tools package.

In order to customize dark and light colors use
`pdf-tools-customize-faces', or set `custom-face-default-form' to
'all."
  :group 'pdf-tools)

(defconst pdf-tools-modes
  '(pdf-history-minor-mode
    pdf-isearch-minor-mode
    pdf-links-minor-mode
    pdf-misc-minor-mode
    pdf-outline-minor-mode
    pdf-misc-size-indication-minor-mode
    pdf-misc-menu-bar-minor-mode
    pdf-annot-minor-mode
    pdf-sync-minor-mode
    pdf-misc-context-menu-minor-mode
    pdf-cache-prefetch-minor-mode
    pdf-view-auto-slice-minor-mode
    pdf-occur-global-minor-mode
    pdf-virtual-global-minor-mode))

(defcustom pdf-tools-enabled-modes
  '(pdf-history-minor-mode
    pdf-isearch-minor-mode
    pdf-links-minor-mode
    pdf-misc-minor-mode
    pdf-outline-minor-mode
    pdf-misc-size-indication-minor-mode
    pdf-misc-menu-bar-minor-mode
    pdf-annot-minor-mode
    pdf-sync-minor-mode
    pdf-misc-context-menu-minor-mode
    pdf-cache-prefetch-minor-mode
    pdf-occur-global-minor-mode)
  "A list of automatically enabled minor-modes.

PDF Tools is build as a series of minor-modes.  This variable and
the function `pdf-tools-install' merely serve as a convenient
wrapper in order to load these modes in current and newly created
PDF buffers."
  :group 'pdf-tools
  :type `(set ,@(mapcar (lambda (mode)
                          `(function-item ,mode))
                        pdf-tools-modes)))

(defcustom pdf-tools-enabled-hook nil
  "A hook ran after PDF Tools is enabled in a buffer."
  :group 'pdf-tools
  :type 'hook)

(defconst pdf-tools-auto-mode-alist-entry
  '("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  "The entry to use for `auto-mode-alist'.")

(defconst pdf-tools-magic-mode-alist-entry
  '("%PDF" . pdf-view-mode)
  "The entry to use for `magic-mode-alist'.")

(defun pdf-tools-customize ()
  "Customize Pdf Tools."
  (interactive)
  (customize-group 'pdf-tools))

(defun pdf-tools-customize-faces ()
  "Customize PDF Tool's faces."
  (interactive)
  (let ((buffer (format "*Customize Group: %s*"
                        (custom-unlispify-tag-name 'pdf-tools-faces))))
    (when (buffer-live-p (get-buffer buffer))
      (with-current-buffer (get-buffer buffer)
        (rename-uniquely)))
    (customize-group 'pdf-tools-faces)
    (with-current-buffer buffer
      (set (make-local-variable 'custom-face-default-form) 'all))))


;; * ================================================================== *
;; * Installation
;; * ================================================================== *

;;;###autoload
(defcustom pdf-tools-handle-upgrades t
  "Whether PDF Tools should handle upgrading itself."
  :group 'pdf-tools
  :type 'boolean)

(make-obsolete-variable 'pdf-tools-handle-upgrades
                        "Not used anymore" "0.90")

(defconst pdf-tools-directory
  (or (and load-file-name
           (file-name-directory load-file-name))
      default-directory)
  "The directory from where this library was first loaded.")

(defvar pdf-tools-msys2-directory nil)

(defcustom pdf-tools-installer-os nil
  "Specifies which installer to use.

If nil the installer is chosen automatically. This variable is
useful if you have multiple installers present on your
system (e.g. nix on arch linux)"
  :group 'pdf-tools
  :type 'string)

(defun pdf-tools-identify-build-directory (directory)
  "Return non-nil, if DIRECTORY appears to contain the epdfinfo source.

Returns the expanded directory-name of DIRECTORY or nil."
  (setq directory (file-name-as-directory
                   (expand-file-name directory)))
  (and (file-exists-p (expand-file-name "autobuild" directory))
       (file-exists-p (expand-file-name "epdfinfo.c" directory))
       directory))

(defun pdf-tools-locate-build-directory ()
  "Attempt to locate a source directory.

Returns a appropriate directory or nil.  See also
`pdf-tools-identify-build-directory'."
  (cl-some #'pdf-tools-identify-build-directory
           (list default-directory
                 (expand-file-name "build/server" pdf-tools-directory)
                 (expand-file-name "server")
                 (expand-file-name "server" pdf-tools-directory)
                 (expand-file-name "../server" pdf-tools-directory))))

(defun pdf-tools-msys2-directory (&optional noninteractive-p)
  "Locate the Msys2 installation directory.

Ask the user if necessary and NONINTERACTIVE-P is nil.
Returns always nil, unless `system-type' equals windows-nt."
  (cl-labels ((if-msys2-directory (directory)
                (and (stringp directory)
                     (file-directory-p directory)
                     (file-exists-p
                      (expand-file-name "usr/bin/bash.exe" directory))
                     directory)))
    (when (eq system-type 'windows-nt)
      (setq pdf-tools-msys2-directory
            (or pdf-tools-msys2-directory
                (cl-some #'if-msys2-directory
                         (cl-mapcan
                          (lambda (drive)
                            (list (format "%c:/msys64" drive)
                                  (format "%c:/msys32" drive)))
                          (number-sequence ?c ?z)))
                (unless (or noninteractive-p
                            (not (y-or-n-p "Do you have Msys2 installed ? ")))
                  (if-msys2-directory
                   (read-directory-name
                    "Please enter Msys2 installation directory: " nil nil t))))))))

(defun pdf-tools-msys2-mingw-bin ()
  "Return the location of /mingw*/bin."
  (when (pdf-tools-msys2-directory)
    (let ((arch (intern (car (split-string system-configuration "-" t)))))
    (expand-file-name
     (format "./mingw%s/bin" (if (eq arch 'x86_64) "64" "32"))
     (pdf-tools-msys2-directory)))))

(defun pdf-tools-find-bourne-shell ()
  "Locate a usable sh."
  (or (and (eq system-type 'windows-nt)
           (let* ((directory (pdf-tools-msys2-directory)))
             (when directory
               (expand-file-name "usr/bin/bash.exe" directory))))
      (executable-find "sh")))

(defun pdf-tools-build-server (target-directory
                               &optional
                               skip-dependencies-p
                               force-dependencies-p
                               callback
                               build-directory)
  "Build the epdfinfo program in the background.

Install into TARGET-DIRECTORY, which should be a directory.

If CALLBACK is non-nil, it should be a function.  It is called
with the compiled executable as the single argument or nil, if
the build failed.

Expect sources to be in BUILD-DIRECTORY.  If nil, search for it
using `pdf-tools-locate-build-directory'.

See `pdf-tools-install' for the SKIP-DEPENDENCIES-P and
FORCE-DEPENDENCIES-P arguments.

Returns the buffer of the compilation process."

  (unless callback (setq callback #'ignore))
  (unless build-directory
    (setq build-directory (pdf-tools-locate-build-directory)))
  (cl-check-type target-directory (satisfies file-directory-p))
  (setq target-directory (file-name-as-directory
                          (expand-file-name target-directory)))
  (cl-check-type build-directory (and (not null)
                                      (satisfies file-directory-p)))
  (when (and skip-dependencies-p force-dependencies-p)
    (error "Can't simultaneously skip and force dependencies"))
  (let* ((compilation-auto-jump-to-first-error nil)
         (compilation-scroll-output t)
         (shell-file-name (pdf-tools-find-bourne-shell))
         (shell-command-switch "-c")
         (process-environment process-environment)
         (default-directory build-directory)
         (autobuild (shell-quote-argument
                     (expand-file-name "autobuild" build-directory)))
         (msys2-p (equal "bash.exe" (file-name-nondirectory shell-file-name))))
    (unless shell-file-name
      (error "No suitable shell found"))
    (when msys2-p
      (push "BASH_ENV=/etc/profile" process-environment))
    (let ((executable
           (expand-file-name
            (concat "epdfinfo" (and (eq system-type 'windows-nt) ".exe"))
            target-directory))
          (compilation-buffer
           (compilation-start
            (format "%s -i %s%s%s"
                    autobuild
                    (shell-quote-argument target-directory)
                    (cond
                     (skip-dependencies-p " -D")
                     (force-dependencies-p " -d")
                     (t ""))
                    (if pdf-tools-installer-os (concat " --os " pdf-tools-installer-os) ""))
            t)))
      ;; In most cases user-input is required, so select the window.
      (if (get-buffer-window compilation-buffer)
          (select-window (get-buffer-window compilation-buffer))
        (pop-to-buffer compilation-buffer))
      (with-current-buffer compilation-buffer
        (setq-local compilation-error-regexp-alist nil)
        (add-hook 'compilation-finish-functions
                  (lambda (_buffer status)
                    (funcall callback
                             (and (equal status "finished\n")
                                  executable)))
                  nil t)
        (current-buffer)))))


;; * ================================================================== *
;; * Initialization
;; * ================================================================== *

;;;###autoload
(defun pdf-tools-install (&optional no-query-p skip-dependencies-p
                                    no-error-p force-dependencies-p)
  "Install PDF-Tools in all current and future PDF buffers.

If the `pdf-info-epdfinfo-program' is not running or does not
appear to be working, attempt to rebuild it.  If this build
succeeded, continue with the activation of the package.
Otherwise fail silently, i.e. no error is signaled.

Build the program (if necessary) without asking first, if
NO-QUERY-P is non-nil.

Don't attempt to install system packages, if SKIP-DEPENDENCIES-P
is non-nil.

Do not signal an error in case the build failed, if NO-ERROR-P is
non-nil.

Attempt to install system packages (even if it is deemed
unnecessary), if FORCE-DEPENDENCIES-P is non-nil.

Note that SKIP-DEPENDENCIES-P and FORCE-DEPENDENCIES-P are
mutually exclusive.

Note further, that you can influence the installation directory
by setting `pdf-info-epdfinfo-program' to an appropriate
value (e.g. ~/bin/epdfinfo) before calling this function.

See `pdf-view-mode' and `pdf-tools-enabled-modes'."
  (interactive)
  (if (or (pdf-info-running-p)
          (ignore-errors (pdf-info-check-epdfinfo) t))
      (pdf-tools-install-noverify)
    (let ((target-directory
           (or (and (stringp pdf-info-epdfinfo-program)
                    (file-name-directory
                     pdf-info-epdfinfo-program))
               pdf-tools-directory)))
      (if (or no-query-p
              (y-or-n-p "Need to (re)build the epdfinfo program, do it now ?"))
        (pdf-tools-build-server
         target-directory
         skip-dependencies-p
         force-dependencies-p
         (lambda (executable)
           (let ((msg (format
                       "Building the PDF Tools server %s"
                       (if executable "succeeded" "failed"))))
             (if (not executable)
                 (funcall (if no-error-p #'message #'error) "%s" msg)
               (message "%s" msg)
               (setq pdf-info-epdfinfo-program executable)
               (let ((pdf-info-restart-process-p t))
                 (pdf-tools-install-noverify))))))
        (message "PDF Tools not activated")))))

(defun pdf-tools-install-noverify ()
  "Like `pdf-tools-install', but skip checking `pdf-info-epdfinfo-program'."
  (add-to-list 'auto-mode-alist pdf-tools-auto-mode-alist-entry)
  (add-to-list 'magic-mode-alist pdf-tools-magic-mode-alist-entry)
  ;; FIXME: Generalize this sometime.
  (when (memq 'pdf-occur-global-minor-mode
              pdf-tools-enabled-modes)
    (pdf-occur-global-minor-mode 1))
  (when (memq 'pdf-virtual-global-minor-mode
              pdf-tools-enabled-modes)
    (pdf-virtual-global-minor-mode 1))
  (add-hook 'pdf-view-mode-hook #'pdf-tools-enable-minor-modes)
  (dolist (buf (buffer-list))
    ;; This when check should not be necessary, but somehow dead
    ;; buffers are showing up here. See
    ;; https://github.com/vedang/pdf-tools/pull/93
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (and (not (derived-mode-p 'pdf-view-mode))
                   (pdf-tools-pdf-buffer-p)
                   (buffer-file-name))
          (pdf-view-mode))))))

(defun pdf-tools-uninstall ()
  "Uninstall PDF-Tools in all current and future PDF buffers."
  (interactive)
  (pdf-info-quit)
  (setq-default auto-mode-alist
    (remove pdf-tools-auto-mode-alist-entry auto-mode-alist))
  (setq-default magic-mode-alist
    (remove pdf-tools-magic-mode-alist-entry magic-mode-alist))
  (pdf-occur-global-minor-mode -1)
  (pdf-virtual-global-minor-mode -1)
  (remove-hook 'pdf-view-mode-hook #'pdf-tools-enable-minor-modes)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (pdf-util-pdf-buffer-p buf)
        (pdf-tools-disable-minor-modes pdf-tools-modes)
        (normal-mode)))))

(defun pdf-tools-pdf-buffer-p (&optional buffer)
  "Check if the current buffer is a PDF document.

Optionally, take BUFFER as an argument and check if it is a PDF document."
  (save-current-buffer
    (when buffer (set-buffer buffer))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char 1)
        (looking-at "%PDF")))))

(defun pdf-tools-assert-pdf-buffer (&optional buffer)
  "Throw an error if the current BUFFER does not contain a PDF document."
  (unless (pdf-tools-pdf-buffer-p buffer)
    (error "Buffer does not contain a PDF document")))

(defun pdf-tools-set-modes-enabled (enable &optional modes)
  "Enable/Disable all the pdf-tools modes on the current buffer based on ENABLE.

Accepts MODES as a optional argument to enable/disable specific modes."
  (dolist (m (or modes pdf-tools-enabled-modes))
    (let ((enabled-p (and (boundp m)
                          (symbol-value m))))
      (unless (or (and enabled-p enable)
                  (and (not enabled-p) (not enable)))
        (funcall m (if enable 1 -1))))))

;;;###autoload
(defun pdf-tools-enable-minor-modes (&optional modes)
  "Enable MODES in the current buffer.

MODES defaults to `pdf-tools-enabled-modes'."
  (interactive)
  (pdf-util-assert-pdf-buffer)
  (pdf-tools-set-modes-enabled t modes)
  (run-hooks 'pdf-tools-enabled-hook))

(defun pdf-tools-disable-minor-modes (&optional modes)
  "Disable MODES in the current buffer.

MODES defaults to `pdf-tools-enabled-modes'."
  (interactive)
  (pdf-tools-set-modes-enabled nil modes))

(declare-function pdf-occur-global-minor-mode "pdf-occur.el")
(declare-function pdf-virtual-global-minor-mode "pdf-virtual.el")

;;;###autoload
(defun pdf-tools-help ()
  "Show a Help buffer for `pdf-tools'."
  (interactive)
  (help-setup-xref (list #'pdf-tools-help)
                   (called-interactively-p 'interactive))
  (with-help-window (help-buffer)
    (princ "PDF Tools Help\n\n")
    (princ "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    (dolist (m (cons 'pdf-view-mode
                     (sort (copy-sequence pdf-tools-modes) #'string<)))
      (princ (format "`%s' is " m))
      (describe-function-1 m)
      (terpri) (terpri)
      (princ "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"))))


;; * ================================================================== *
;; * Debugging
;; * ================================================================== *

(defvar pdf-tools-debug nil
  "Non-nil, if debugging PDF Tools.")

(defun pdf-tools-toggle-debug ()
  "Turn debugging on/off for pdf-tools."
  (interactive)
  (setq pdf-tools-debug (not pdf-tools-debug))
  (when (called-interactively-p 'any)
    (message "Toggled debugging %s" (if pdf-tools-debug "on" "off"))))

(provide 'pdf-tools)

;;; pdf-tools.el ends here
