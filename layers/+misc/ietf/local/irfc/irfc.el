;;; irfc.el --- Interface for IETF RFC document.

;; Filename: irfc.el
;; Description: Interface for IETF RFC document.
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;;         Juanma Barranquero <lekktu@gmail.com>
;; Maintainer: Niels Widger <niels.widger@gmail.com>
;; Copyright (C) 2009, Andy Stewart, all rights reserved.
;; Copyright (C) 2009, Juanma Barranquero, all rights reserved.
;; Copyright (C) 2010, Niels Widger, all rights reserved.
;; Created: 2009-01-14 08:13:15
;; Version: 0.5.6
;; Last-Updated: Fri Aug 17 19:42:29 2012 (-0400)
;;           By: Samuel Bronson
;; URL: https://www.emacswiki.org/emacs/download/irfc.el
;; Keywords: RFC, IETF
;; Compatibility: GNU Emacs 22 ~ 23
;;
;; Features that might be required by this library:
;;
;; `cl' `url-vars' `thingatpt'
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;; Introduction:
;;
;; For historical reasons, IETF Internet RFCs are required to be in a plain
;; ASCII text format that's best-suited for sending directly to a 6-lpi
;; US-letter-size printer.  This makes them suboptimal for viewing on-screen,
;; as you will be doing for countless hours if you're ever doing network
;; programming to one of them.  Fortunately, the ASCII format is usually
;; close to what you, the Emacs zealot, *truly* want -- which is a format
;; suited to more pleasurably viewing the RFC in Emacs.
;;
;; The `irfc' package uses Emacs overlays to add some fortification and
;; hide the page headers and footers (which it replaces with one-line page
;; number references that look like "(p.1)", right-justified).  The file is
;; never modified, and you can see the raw ASCII text by pressing `T'.
;;

;;; Commentary:
;;
;; Interface for IETF RFC document.
;;
;; This package use some code from `rfcview.el'.
;; Thanks "Neil W.  Van Dyke"!
;;
;; The features this package provide:
;;
;; * Format RFC document for easy reading.
;; * Single keystroke for fast view.
;; * Render status switch.
;; * Smart table and content switch.
;; * Visit RFC link around point.
;; * Jump to RFC reference around point.
;; * Download RFC document *asynchronous*.
;;
;; Below are commands you can use:
;;
;; `irfc-render-toggle'         Toggle render status with RFC buffer.
;; `irfc-quit'                  Quit RFC buffer.
;; `irfc-visit'                 Ask for RFC number and visit document.
;; `irfc-reference-goto'        Ask for RFC reference and jump to it.
;; `irfc-head-goto'             Ask for heading name and jump to it.
;; `irfc-head-number-goto'      Ask for heading number and jump to it.
;; `irfc-follow'                Visit RFC document around point.
;; `irfc-table-jump'            Switch between table and content.
;; `irfc-page-goto'             Goto page.
;; `irfc-page-next'             Jump next page.
;; `irfc-page-prev'             Jump previous page.
;; `irfc-page-first'            Jump first page.
;; `irfc-page-last'             Jump last page.
;; `irfc-page-table'            Jump table page.
;; `irfc-head-next'             Jump next heading.
;; `irfc-head-prev'             Jump previous heading.
;; `irfc-rfc-link-next'         Jump next RFC link.
;; `irfc-rfc-link-prev'         Jump previous RFC link.
;; `irfc-scroll-up-one-line'    Scroll up one line.
;; `irfc-scroll-down-one-line'  Scroll down one line.
;;
;; Tips:
;;
;; You can use command `irfc-render-toggle' to toggle render status.
;;
;; Command `irfc-table-jump' can switch between table and content,
;; example you stay cursor at *table*, and type "G" will jump corresponding
;; content in buffer, alike, you can stay at any content and type "G"
;; will jump corresponding table item.
;;
;; Command `irfc-follow' will visit RFC document around point,
;; example you stay cursor at "[RFC3986]", and type "o" will
;; open rfc3986.txt in storage directory.  If have not found
;; this file in directory, will download from `https://www.ietf.org/rfc/'
;; and open it when download complete.
;;
;; And command ‘irfc-follow’ can also use at title of RFC document.
;; Example rfc3986.txt contain “Obsoletes: 2732, 2396, 1808” at title,
;; you can move cursor to “2732” and type “o” will visit RFC 2732 document.
;; ‘irfc-follow’ support below keywords in title:
;;
;;        “Request for Comments:”
;;        “Updates:”
;;        “Obsoletes:”
;;
;; You can use command `irfc-rfc-link-next' or `irfc-rfc-link-prev'
;; to jump next or previous RFC link in document.
;;
;; Command `irfc-visit' will ask the user for a RFC number and will
;; visit that document, either from `irfc-directory', if exists, or by
;; downloading it.  This command can serve as entry point for Irfc,
;; to go to a RFC without having to visit the file or remember
;; whether it is already in `irfc-directory'.
;; And if you visit same document with your previous type, so just
;; hit RET, and don't need type RFC document number.
;;
;; Command `irfc-reference-goto' will ask the user for a reference
;; number and will jump to that citation in the Normative
;; References/Informative References heading.
;;
;; Command `irfc-head-goto' will ask the user for a heading name and
;; will jump to that heading.  Completion list in minibuffer is
;; available.
;;
;; Command `irfc-head-number-goto' will ask the user for a heading
;; number and will jump to that heading.  Completion list in minibuffer
;; is available.
;;


;;; Installation:
;;
;; Put irfc.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'irfc)
;;
;; Setup your storage directory for RFC documents.
;;
;; (setq irfc-directory "YourStorageDirectory")
;;
;; If you want make RFC document load `irfc-mode' automatically,
;; setup like below:
;;
;; (setq irfc-assoc-mode t)
;;

;;; Customize:
;;
;; `irfc-assoc-mode' whether assoc RFC document with `irfc-mode'.
;; `irfc-directory' the storage directory for RFC document.
;; `irfc-download-base-url' the base url for download RFC document.
;; `irfc-buffer-name-includes-title' whether buffer name should
;;  include the RFC document's title
;; `irfc-highlight-requirement-keywords' whether RFC requirement
;;  keywords specified in `irfc-requirement-keywords' list
;;  should be highlighted using the face specified by
;;  `irfc-requirement-keyword-face'.
;; `irfc-requirement-keywords' list of RFC requirement keywords to
;;  highlight if `irfc-highlight-requirement-keywords' is t.
;; `irfc-highlight-references' whether RFC references should be
;;  highlighted using the face specified by `irfc-reference-face'.
;;
;; All of the above can customize by:
;;      M-x customize-group RET irfc RET
;;

;;; Change log:
;; 2012/08/17
;;   * Samuel Bronson:
;;      * Added `autoload' cookies in key locations.
;;      * Modified `irfc-open' to create the `irfc-directory' if it's
;;        missing (after verifying that the user is okay with this).
;;      * Now, the user can install using e.g. `el-get' and never have
;;        to touch init!
;;
;; 2011/07/12
;;   * Juanma Barranquero:
;;      * Fixed let-binding in `irfc-render-buffer'.
;;      * Use `string-to-number' instead of `string-to-int'.
;;      * Use `when' whenever appropriate.
;;      * Lowercase arg of `irfc-current-head'.
;;      * Remove unused local variable in `irfc-rfc-link-prev'.
;;      * Add leading underscore to unused parameters in `irfc-fill-tables'
;;        (that removes bytecompiler warnings if compiled under lexical
;;        binding in Emacs 24+).
;;      * Remove `function' from lambda expressions in
;;        `irfc-overlay-put-alist' and `irfc-overlay-remove-all', and use
;;        `mapc' instead of `mapcar'.
;;      * Use `nconc' in `irfc-overlay-remove-all' to avoid a bit of
;;        unnecessary consing when dealing with temporary lists.
;;      * Remove unused local variable in `irfc-get-buffer' and simplify
;;        format call (no need to pass as argument a constant string).
;;      * Several fixes in docstrings and error messages.
;;
;; 2010/09/28
;;   * Niels Widger:
;;      * Added new mappings: "f" to `irfc-head-goto' and "F"
;;        to `irfc-head-number-goto'.
;;      * Added new variables: `irfc-heading-names-table',
;;        `irfc-heading-names-list', `irfc-heading-numbers-table'
;;        and `irfc-heading-numbers-list'.
;;      * Added new functions: `irfc-read-heading-name',
;;        `irfc-read-heading-number', `irfc-heading-number-at-point',
;;        `irfc-head-goto', `irfc-head-number-goto' and
;;        `irfc-fill-tables'.
;;      * `irfc-render-buffer' makes new call to `irfc-fill-tables'.
;;
;; 2010/09/24
;;   * Niels Widger:
;;      * Added new function `irfc-reference-goto' that prompts a user
;;        for a reference number and jumps to that citation.
;;      * Added mapping from "r" to `irfc-reference-goto' in keymap.
;;      * Added several helper functions used by `irfc-reference-goto':
;;        `irfc-read-reference', `irfc-reference-at-point' and
;;        `irfc-current-head'.
;;      * Added requirement for `thingatpt'.
;;
;; 2010/09/23
;;   * Niels Widger:
;;      * Added new RFC requirement keyword overlay and RFC reference
;;        overlay.
;;      * Several new variables: `irfc-highlight-requirement-keywords',
;;        `irfc-highlight-references' and `irfc-requirement-keywords'.
;;      * New faces: `irfc-requirement-keyword-face' and
;;        `irfc-reference-face'.
;;      * New overlays: `irfc-requirement-keyword-overlay' and
;;        `irfc-reference-overlay'.
;;      * Modified `irfc-render-buffer' to call
;;        `irfc-render-buffer-overlay-requirement-keyword' and
;;        `irfc-render-buffer-overlay-reference'.
;;
;; 2010/09/20
;;   * Niels Widger:
;;      * New variable `irfc-buffer-name-includes-title'.
;;      * Add new function `irfc-rename-buffer' to include RFC document
;;        title in buffer name if `irfc-buffer-name-includes-title' is t.
;;      * Modify `irfc-render-buffer' to call `irfc-rename-buffer'
;;        function.
;;
;; 2009/02/13
;;   * Andy Stewart:
;;      * New variable `irfc-table-regex'.
;;      * Fix bug of `irfc-table-jump'.
;;      * Fix doc.
;;
;; 2009/01/29
;;   * Andy Stewart:
;;      * Fix overlay RFC link (as format [RFC-number]) in RFC1034.txt.
;;      * Fix RFC link jump bug.
;;
;; 2009/01/22
;;   * Juanma Barranquero
;;      * Add require information to avoid compile warning.
;;      * Add new function `irfc-unload-function' to cleanup
;;        when execute command `unload-feature'.
;;
;; 2009/01/21
;;   * Juanma Barranquero
;;      * Add new command `irfc-visit' for fast open or download RFC
;;        document.
;;      * Fix doc.
;;   * Andy Stewart:
;;      * Applied Juanma's patch (with slightly modified). Thanks!
;;      * Add variable `irfc-last-visit-number' to record last input
;;        RFC document number, save type if have to visit same document
;;        with previous times.
;;      * Fix bug of function `irfc-download-callback'.
;;        Display error information when download RFC document failed.
;;
;; 2009/01/18
;;   * Andy Stewart:
;;      * Make `irfc-follow' can open RFC link at title.
;;        Now support below keyword in title:
;;              "Request for Comments:"
;;              "Updates:"
;;              "Obsoletes:"
;;      * Add new commands: `irfc-rfc-link-next' and `irfc-rfc-link-prev'.
;;      * Fix doc.
;;
;;   * Juanma Barranquero:
;;      * Fix defface error, and improve document.
;;
;; 2009/01/17
;;   * Andy Stewart:
;;      * Fix doc.
;;      * Remove function `irfc-render-buffer-hide-cr'.
;;      * Thanks "Juanma Barranquero" improve document and advices. :)
;;
;; 2009/01/16
;;   * Andy Stewart:
;;      * Modified code for 22 compatibility.
;;      * Fix doc.
;;
;; 2009/01/14
;;   * Andy Stewart:
;;      * First released.
;;

;;; Acknowledgements:
;;
;;      Neil W. Van Dyke        <neil@neilvandyke.org>
;;              For create rfcview.el
;;      Juanma Barranquero      <lekktu@gmail.com>
;;              Thanks Juanma Barranquero send many patches.
;;              Juanma, thank you very much! :)
;;

;;; TODO
;;
;;
;;

;;; Require
(eval-when-compile (require 'cl))
(require 'url-vars)
(require 'thingatpt)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defgroup irfc nil
  "Interface for IETF RFC documents."
  :group 'edit)

;; This is autoloaded so that it will take effect without users having
;; to `load'/`require' this package in their init file.
;;;###autoload
(defcustom irfc-assoc-mode nil
  "If non-nil, RFC documents are associated with `irfc-mode'.
Default is nil."
  :type 'boolean
  :set (lambda (symbol value)
         (set symbol value)
         (if value
             (add-to-list 'auto-mode-alist
                          '("/rfc[0-9]+\\.txt\\'" . irfc-mode))
           (remove-hook 'auto-mode-alist
                        '("/rfc[0-9]+\\.txt\\'" . irfc-mode))))
  :group 'irfc)

(defcustom irfc-directory "~/.emacs.d/RFC/"
  "The storage directory for RFC document download and search."
  :type 'string
  :group 'irfc)

(defcustom irfc-download-base-url "https://www.ietf.org/rfc/"
  "The base URL for downloading RFC documents."
  :type 'string
  :group 'irfc)

(defcustom irfc-buffer-name-includes-title t
  "If non-nil, buffer names for RFC documents will include the RFC title.
The format for the buffer name will be 'RFCTITLE (RFCNUM.TXT)'.
If nil, the buffer name is just RFCNUM.TXT.  Default is t."
  :type 'boolean
  :group 'irfc)

(defcustom irfc-highlight-requirement-keywords t
  "If non-nil, requirement keywords specified by
`irfc-requirement-keywords' list will be highlighted using the
face specified by `irfc-requirement-keyword-face'.
Default is t."
  :type 'boolean
  :group 'irfc)

(defcustom irfc-highlight-references t
  "If non-nil, RFC document references specified by the
`irfc-reference-regex' regular expression will be highlighted
using the face specified by `irfc-reference-face'.  Default is
t."
  :type 'boolean
  :group 'irfc)

(defcustom irfc-requirement-keywords '("MUST" "MUST NOT"
                                       "REQUIRED"
                                       "SHALL" "SHALL NOT"
                                       "SHOULD" "SHOULD NOT"
                                       "RECOMMENDED" "NOT RECOMMENDED"
                                       "MAY" "OPTIONAL" "NOT")
  "List of requirement keyword strings to be highlighted if
`irfc-highlight-requirement-keywords' is t."
  :type '(repeat (string))
  :group 'irfc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Faces ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defface irfc-title-face
  '((t (:foreground "Gold" :bold t)))
  "Face used for titles."
  :group 'irfc)
(defvar irfc-title-overlay nil
  "Overlay for `irfc-title-face'.")

(defface irfc-head-name-face
  '((t (:foreground "DarkRed" :bold t :underline t)))
  "Face used for heading names."
  :group 'irfc)
(defvar irfc-head-name-overlay nil
  "Overlay for `irfc-head-name-face'.")

(defface irfc-head-number-face
  '((t (:foreground "DarkRed" :bold t)))
  "Face used for heading numbers."
  :group 'irfc)
(defvar irfc-head-number-overlay nil
  "Overlay for `irfc-head-number-face'.")

(defface irfc-rfc-number-face
  '((t (:foreground "Green3" :bold t)))
  "Face used for RFC number in the header."
  :group 'irfc)
(defvar irfc-rfc-number-overlay nil
  "Overlay for `irfc-rfc-number-face'.")

(defface irfc-std-number-face
  '((t (:foreground "Grey" :bold t)))
  "Face used for STD number in the header."
  :group 'irfc)
(defvar irfc-std-number-overlay nil
  "Overlay for `irfc-std-number-face'.")

(defface irfc-rfc-link-face
  '((t (:foreground "Grey30" :bold t)))
  "Face used for RFC link in the header."
  :group 'irfc)
(defvar irfc-rfc-link-overlay nil
  "Overlay for `irfc-rfc-link-face'.")

(defface irfc-table-item-face
  '((t (:foreground "LawnGreen")))
  "Face used for Table item."
  :group 'irfc)
(defvar irfc-table-item-overlay nil
  "Overlay for `irfc-table-item-face'.")

(defface irfc-requirement-keyword-face
  '((t (:foreground "red1" :bold t)))
  "Face used for requirement keywords."
  :group 'irfc)
(defvar irfc-requirement-keyword-overlay nil
  "Overlay for `irfc-requirement-keyword-face'.")

(defface irfc-reference-face
  '((t (:foreground "blue1" :bold t)))
  "Face used for RFC document references."
  :group 'irfc)
(defvar irfc-reference-overlay nil
  "Overlay for `irfc-reference-face'.")

(defvar irfc-hide-overlay nil
  "Overlay for hiding whitespace or blank lines.")

(defvar irfc-page-number-overlay nil
  "Overlay for page number.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar irfc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j") 'next-line)
    (define-key map (kbd "k") 'previous-line)
    (define-key map (kbd "h") 'backward-char)
    (define-key map (kbd "l") 'forward-char)
    (define-key map (kbd "e") 'scroll-down)
    (define-key map (kbd "SPC") 'scroll-up)
    (define-key map (kbd "J") 'irfc-scroll-up-one-line)
    (define-key map (kbd "K") 'irfc-scroll-down-one-line)
    (define-key map (kbd ",") 'end-of-buffer)
    (define-key map (kbd ".") 'beginning-of-buffer)
    (define-key map (kbd "T") 'irfc-render-toggle)
    (define-key map (kbd "q") 'irfc-quit)
    (define-key map (kbd "o") 'irfc-follow)
    (define-key map (kbd "v") 'irfc-visit)
    (define-key map (kbd "r") 'irfc-reference-goto)
    (define-key map (kbd "f") 'irfc-head-goto)
    (define-key map (kbd "F") 'irfc-head-number-goto)
    (define-key map (kbd "g") 'irfc-page-goto)
    (define-key map (kbd "N") 'irfc-page-next)
    (define-key map (kbd "P") 'irfc-page-prev)
    (define-key map (kbd ">") 'irfc-page-last)
    (define-key map (kbd "<") 'irfc-page-first)
    (define-key map (kbd "b") 'irfc-page-table)
    (define-key map (kbd "H") 'irfc-head-next)
    (define-key map (kbd "L") 'irfc-head-prev)
    (define-key map (kbd "G") 'irfc-table-jump)
    (define-key map (kbd "<tab>") 'irfc-rfc-link-next)
    (define-key map (kbd "<backtab>") 'irfc-rfc-link-prev)
    map)
  "Keymap used by `irfc-mode'.")

(defvar irfc-stock-section-names
  '("abstract"
    "acknowledgement"
    "acknowledgements"
    "acknowledgment"
    "acknowledgments"
    "appendices"
    "author's address"
    "authors' addresses"
    "bibliography"
    "chair's address"
    "copyright notice"
    "copyright statement"
    "editor's address"
    "editors' addresses"
    "full copyright notice"
    "full copyright statement"
    "iesg note"
    "index"
    "introduction"
    "references and bibliography"
    "references"
    "security considerations"
    "status of this memo"
    "table of contents")
  "The stock name for overlay heading.")

(defvar irfc-download-buffer nil
  "Download buffer used by `url-retrieve'.
This variable is always buffer-local.")
(make-variable-buffer-local 'irfc-download-buffer)

(defvar irfc-download-url nil
  "URL from which to download files.
This variable is always buffer-local.")
(make-variable-buffer-local 'irfc-download-url)

(defvar irfc-render-p t
  "Render status for RFC buffer.
This variable is always buffer-local.")
(make-variable-buffer-local 'irfc-render-p)

(defvar irfc-total-pages 0
  "Total number of pages in RFC buffer.
This variable is always buffer-local.")
(make-variable-buffer-local 'irfc-total-pages)

(defvar irfc-heading-names-list nil
  "List of heading names in RFC buffer.
This variable is buffer-local in buffers where `irfc-mode' has
been called.")

(defvar irfc-heading-numbers-list nil
  "List of heading numbers in RFC buffer.
This variable is buffer-local in buffers where `irfc-mode' has
been called.")

(defvar irfc-heading-names-table nil
  "Table mapping heading names to position in RFC buffer.
This variable is buffer-local in buffers where `irfc-mode' has
been called.")

(defvar irfc-heading-numbers-table nil
  "Table mapping heading numbers to position in RFC buffer.
This variable is buffer-local in buffers where `irfc-mode' has
been called.")

(defvar irfc-last-visit-number nil
  "Number of the last RFC document visited.")

(defvar irfc-table-regex "^[ ]+\\([A-Z]?[0-9\\.]*\\)[ ]+\\([^\\.\n]+\\)[\\. ]+\\([0-9]+\\)$"
  "The regular-expression that match table item.")

(defvar irfc-reference-regex "\\[[0-9]+]"
  "The regular-expression that matches normative/informative
references.")

(defvar irfc-reference-format-regex "\\[%d]"
  "The format string for use with `format' function for creating
regular-expressions that match a normative/informative
reference.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(define-derived-mode irfc-mode text-mode "Irfc"
  "Major mode for IETF RFC documents."
  ;; Setup.
  (use-local-map irfc-mode-map)
  (setq buffer-read-only t)
  (setq font-lock-defaults nil)
  (auto-save-mode 0)
  (make-local-variable 'irfc-heading-names-list)
  (make-local-variable 'irfc-heading-numbers-list)
  (make-local-variable 'irfc-heading-names-table)
  (make-local-variable 'irfc-heading-numbers-table)
  (setq irfc-heading-names-table (make-hash-table :test 'equal))
  (setq irfc-heading-numbers-table (make-hash-table :test 'equal))
  (setq irfc-heading-names-list nil)
  (setq irfc-heading-numbers-list nil)
  ;; Render.
  (irfc-render-buffer))

(defun irfc-render-buffer ()
  "Render RFC buffer."
  (interactive)
  (save-excursion
    (let ((case-fold-search nil)
          (top-point (point-min))
          (title-line-point nil)
          temp-point)
      ;; Clean up overlays.
      (irfc-overlay-remove-all)
      ;; Hide whitespace at start of file.
      (setq temp-point (irfc-render-buffer-hide-whitespace-at-start))
      (when temp-point (setq top-point temp-point))
      ;; Hide any extraneous blank lines.
      (setq title-line-point (irfc-render-buffer-hide-blank-line top-point))
      ;; Add overlays for page headers and footers.
      (irfc-render-buffer-overlay-page-number)
      ;; Add overlay for the RFC number.
      (irfc-render-buffer-overlay-rfc-number top-point title-line-point)
      ;; Add overlay for the STD number.
      (irfc-render-buffer-overlay-std-number top-point title-line-point)
      ;; Add overlay for the table item.
      (irfc-render-buffer-overlay-table-item top-point)
      ;; Add overlay for the RFC link.
      (irfc-render-buffer-overlay-rfc-link top-point)
      ;; Add overlay for the title.
      (irfc-render-buffer-overlay-title title-line-point)
      ;; Add overlay for the heading.
      (irfc-render-buffer-overlay-head title-line-point)
      ;; Add overlay for requirement keywords.
      (when irfc-highlight-requirement-keywords
        (irfc-render-buffer-overlay-requirement-keyword top-point))
      ;; Add overlay for references.
      (when irfc-highlight-references
        (irfc-render-buffer-overlay-reference top-point))
      ;; Rename buffer
      (irfc-rename-buffer title-line-point)
      ;; Fill heading names/numbers tables
      (irfc-fill-tables title-line-point))))

(defun irfc-render-toggle ()
  "Toggle RFC buffer render status."
  (interactive)
  (if irfc-render-p
      (irfc-render-turn-off)
    (irfc-render-turn-on)))

(defun irfc-render-turn-on ()
  "Turn on RFC buffer render status."
  (irfc-render-buffer)
  (setq irfc-render-p t))

(defun irfc-render-turn-off ()
  "Turn off RFC buffer render status."
  (irfc-overlay-remove-all)
  (setq irfc-render-p nil))

(defun irfc-quit ()
  "Quit RFC buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun irfc-table-jump ()
  "Jump between table and content.
You can jump to the corresponding table item when you are at content.
You can jump to the corresponding content when you are at table."
  (interactive)
  (if (irfc-have-table-p)
      (let ((original-position (point))
            head-name
            page-number
            match-list)
        (cond ((irfc-in-table-p)
               ;; When in table.
               (beginning-of-line)
               (if (search-forward-regexp irfc-table-regex (line-end-position) t)
                   ;; Jump content when find valid table item.
                   (progn
                     ;; Get head name and page number.
                     (setq head-name (match-string 0))
                     (setq head-name (replace-regexp-in-string "[\\. ]+\\([0-9]+\\)$" "" head-name))
                     (setq head-name (replace-regexp-in-string "^[ ]+" "" head-name))
                     (setq page-number (string-to-number (match-string 3)))
                     ;; Jump page.
                     (irfc-page-goto page-number)
                     ;; Search head.
                     (re-search-forward head-name nil t)
                     ;; Indent.
                     (back-to-indentation))
                 ;; Restore original position and output message
                 ;; when at invalid table item.
                 (message "Invalid table item.")
                 (goto-char original-position)))
              ;; Do nothing when at front of table.
              ((irfc-front-table-p)
               (message "In front of table."))
              ;; Jump corresponding table item from content.
              (t
               ;; Get head name and page number.
               (end-of-line)
               (setq match-list (irfc-head-move t))
               (setq head-name (buffer-substring-no-properties (nth 2 match-list)
                                                               (nth 3 match-list)))
               (setq page-number (irfc-current-page))
               ;; Jump table.
               (irfc-page-table)
               ;; Search head.
               (re-search-forward (concat (regexp-quote head-name) "[\\. ]+"
                                          (regexp-quote (number-to-string page-number))))
               ;; Indent.
               (back-to-indentation))))
    ;; Do nothing when haven't table in this RFC document.
    (message "This RFC document contains no Table of Contents.")))

(defun irfc-reference-goto (&optional number)
  "Goto reference NUMBER."
  (interactive (list (irfc-read-reference)))
  (let ((original-position (point)) (done nil) (found nil) (beg) (end))
    (goto-char (point-min))
    (while (not done)
      (if (not (re-search-forward
                (concat "^[ \t]*"
                        (format irfc-reference-format-regex number))
                (point-max) t))
          (setq done t)
        (setq beg (match-beginning 0))
        (setq end (match-end 0))
        (let ((name (irfc-current-head)))
          (if (not (or (string= name "References")
                       (string= name "Normative References")
                       (string= name "Informative References")))
              (goto-char end)
            (goto-char beg)
            (setq found t)
            (setq done t)))))
    (when (not found)
      (goto-char original-position)
      (message "Cannot find reference %d" number))))

(defun irfc-read-reference ()
  "Read reference as a number using a reference found at point as
default."
  (let ((default (irfc-reference-at-point)))
    (if (eq default nil)
        (read-number "Reference number: ")
      (read-number "Reference number: " default))))

(defun irfc-reference-at-point ()
  "Returns reference at point as a number or nil if one is not
found."
  (if (not (thing-at-point-looking-at irfc-reference-regex))
      nil
    (let* ((match (buffer-substring (match-beginning 0) (match-end 0)))
           (len (length match)))
      (string-to-number (substring match 1 (1- len))))))

(defun irfc-read-heading-name ()
  "Read heading name as a string."
  (completing-read "Heading name: " irfc-heading-names-list nil t))

(defun irfc-read-heading-number ()
  "Read heading number as a string using a heading number found
at point as default."
  (let ((default (irfc-heading-number-at-point)))
    (completing-read
     (concat "Heading number" (if (eq default nil) "" (format " (default %s)" default)) ": ")
     irfc-heading-numbers-list nil t nil nil default nil)))

(defun irfc-heading-number-at-point ()
  "Returns heading number at point as a string or nil if one is
not found."
  (if (not (thing-at-point-looking-at "\\([0-9]+\\.\\)*[0-9]+"))
      nil
    (let ((match (match-string 0)))
      (cond ((member match irfc-heading-numbers-list) match)
            ((member (concat match ".") irfc-heading-numbers-list) (concat match "."))))))

(defun irfc-page-goto (number)
  "Goto page NUMBER."
  (interactive "nPage number: ")
  (cond ((<= number 1)
         ;; Move beginning of buffer when page number
         ;; is equal or below 1.
         (call-interactively 'beginning-of-buffer)
         (when (< number 1)
           (message "Top page reached.")))
        (t
         ;; Move special page.
         (let ((original-position (point))
               (original-render-status irfc-render-p)
               reach-bottom-p)
           ;; Set max page number when
           ;; query page is above max limit.
           (when (> number irfc-total-pages)
             (setq number irfc-total-pages)
             (setq reach-bottom-p t))
           ;; Turn off render.
           (irfc-render-turn-off)
           ;; Search page number.
           (goto-char (point-min))
           (if (re-search-forward (concat "\\[Page "
                                          (regexp-quote (number-to-string (1- number)))
                                          "\\]$")
                                  nil t)
               ;; Move special page when search successful.
               (progn
                 ;; Adjust cursor position.
                 (forward-line +3)
                 (re-search-forward "^.+$" nil t)
                 (back-to-indentation)
                 ;; Recenter when reach bottom page.
                 (when reach-bottom-p
                   (recenter 0)
                   (message "Bottom page reached.")))
             ;; Restore original position when search failed.
             (goto-char original-position))
           ;; Revert render status.
           (unless (equal original-render-status irfc-render-p)
             (irfc-render-toggle))))))

(defun irfc-page-next (arg)
  "Move to next ARGth page.
ARG defaults to 1."
  (interactive "P")
  (irfc-page-goto (+ (irfc-current-page) (or arg 1))))

(defun irfc-page-prev (arg)
  "Move to previous ARGth page.
ARG defaults to 1."
  (interactive "P")
  (irfc-page-goto (- (irfc-current-page) (or arg 1))))

(defun irfc-page-first ()
  "Move to first page."
  (interactive)
  (irfc-page-goto 1))

(defun irfc-page-last ()
  "Move to last page."
  (interactive)
  (irfc-page-goto irfc-total-pages))

(defun irfc-page-table ()
  "Move to Table of Contents."
  (interactive)
  (if (irfc-have-table-p)
      (progn
        (goto-char (point-min))
        (re-search-forward "^Table of Contents$" nil t)
        (back-to-indentation))
    (message "This RFC document has no Table of Contents.")))

;;;###autoload
(defun irfc-follow ()
  "Open RFC document around point.
Download and open RFC document if it
does not exist in `irfc-directory'."
  (interactive)
  (let ((rfc-file-name (irfc-get-rfc-filename)))
    (if rfc-file-name
        ;; Open RFC document.
        (irfc-open rfc-file-name)
      (message "No valid RFC link found at cursor."))))

;;;###autoload
(defun irfc-visit (&optional rfc-number)
  "Open RFC document RFC-NUMBER.
Download and open RFC document if it
does not exist in `irfc-directory'."
  (interactive)
  (or rfc-number
      (setq rfc-number (read-number
                        "RFC document to visit: "
                        irfc-last-visit-number)))
  (setq irfc-last-visit-number rfc-number)
  (irfc-open (format "rfc%s.txt" rfc-number)))

(defun irfc-head-goto (NAME)
  "Goto heading NAME."
  (interactive (list (irfc-read-heading-name)))
  (let ((new-point (gethash NAME irfc-heading-names-table)))
    (if (eq new-point nil)
        (message "Cannot find heading \"%s\"" NAME)
      (goto-char new-point)
      (back-to-indentation))))

(defun irfc-head-next ()
  "Move to next heading."
  (interactive)
  (let ((original-position (point)))
    (end-of-line)
    (if (irfc-head-move)
        ;; Move to next heading,
        ;; when search successful.
        (beginning-of-line)
      ;; Restore original position
      ;; when search failed.
      (goto-char original-position)
      (message "No next heading."))))

(defun irfc-head-prev ()
  "Move to previous heading."
  (interactive)
  (let ((original-position (point)))
    (beginning-of-line)
    (unless (irfc-head-move t)
      ;; Restore original position
      ;; when search failed.
      (goto-char original-position)
      (message "No previous heading."))))

(defun irfc-current-head (&optional print)
  "Returns name of the current heading.
If optional argument PRINT is non-nil, print the name before returning it."
  (interactive)
  (save-excursion
    (irfc-head-prev)
    (re-search-forward "^\\([0-9]+\\.\?\\)+[ \t]+" (line-end-position) t)
    (let ((name (buffer-substring (point) (line-end-position))))
      (when print
        (message "%s" name))
      name)))

(defun irfc-head-number-goto (NAME)
  "Goto heading number NAME."
  (interactive (list (irfc-read-heading-number)))
  (let ((new-point (gethash NAME irfc-heading-numbers-table)))
    (if (eq new-point nil)
        (irfc-head-number-goto (concat NAME "."))
      (goto-char new-point)
      (back-to-indentation))))

(defun irfc-scroll-up-one-line ()
  "Scroll up one line."
  (interactive)
  (scroll-up 1))

(defun irfc-scroll-down-one-line ()
  "Scroll down one line."
  (interactive)
  (scroll-down 1))

(defun irfc-rfc-link-next ()
  "Move the point to the next RFC link."
  (interactive)
  (let ((original-point (point)))
    (when (re-search-forward "\\(\\B\\[RFC-?[0-9]+\\]\\B\\|[ \t]+[0-9]+\\)" nil t)
      (catch 'match
        (while (and (not (string-match "\\[\\(RFC-?[0-9]+\\)\\]" (irfc-get-symbol-non-blank)))
                    (or (not (irfc-title-rfc-link-p)) ;not valid RFC link number
                        (eolp)))                      ;number at end of line is invalid RFC number
          (unless (re-search-forward "\\(\\B\\[RFC-?[0-9]+\\]\\B\\|[ \t]+[0-9]+\\)" nil t)
            (goto-char original-point)
            (message "No next RFC link.")
            (throw 'match "Match last one.")))))))

(defun irfc-rfc-link-prev ()
  "Move the point to the previous RFC link."
  (interactive)
  (when (re-search-backward "\\(\\B\\[RFC-?[0-9]+\\]\\B\\|[ \t]+[0-9]+\\)" nil t)
    (catch 'match
      (while
          ;; Not match [RFCnnn] format.
          (not (string-match "\\[\\(RFC-?[0-9]+\\)\\]" (irfc-get-symbol-non-blank)))
        (skip-chars-forward " ")
        (if (and (irfc-title-rfc-link-p)                    ;is valid RFC link number
                 (save-excursion                            ;skip number at end of line.
                   (search-forward-regexp "[0-9]+" nil t)
                   (not (eolp))))
            (progn
              (when (string-match "^request for comments:[ \t]+$"
                                  (buffer-substring-no-properties (line-beginning-position)
                                                                  (point)))
                (message "No previous RFC link."))
              (throw 'match "Match title RFC link."))
          (re-search-backward "\\(\\B\\[RFC-?[0-9]+\\]\\B\\|[ \t]+[0-9]+\\)" nil t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun irfc-open (rfc-file-name)
  "Open RFC document with RFC-FILE-NAME."

  ;; Make sure `irfc-directory' exists.
  (unless (file-directory-p irfc-directory)
    (if (y-or-n-p (format "Create directory %s to hold RFCs? "
                          irfc-directory))
        (make-directory irfc-directory t)
      (error "Customize `irfc-directory', then!")))

  (let (filepath)
    (if (string-equal rfc-file-name (buffer-name))
        ;; Notify user if current buffer is search RFC document.
        (message "Current RFC document.")
      ;; Get full file path.
      (setq filepath (expand-file-name rfc-file-name irfc-directory))
      (cond ((file-exists-p filepath)
             ;; Open corresponding RFC document.
             (if (get-buffer rfc-file-name)
                 ;; Switch corresponding buffer when it open.
                 (switch-to-buffer rfc-file-name)
               ;; Or open that file.
               (find-file filepath)))
            (t
             ;; Otherwise download and open corresponding RFC document.
             (irfc-download (concat irfc-download-base-url rfc-file-name)))))))

(defun irfc-render-buffer-hide-whitespace-at-start ()
  "Hide whitespace at start of file.
Return adjusted point."
  (goto-char (point-min))
  (if (re-search-forward "\\`\\([ \t\f]*\r?\n\\)+" nil t)
      (progn
        (irfc-overlay-hide-region (match-beginning 0) (match-end 0))
        (point))
    nil))

(defun irfc-render-buffer-overlay-page-number ()
  "Add overlays for page headers and footers."
  (let ((headerfooter-re (concat "^[ \t]*"
                                 "\\(\r?\n\\)"        ; #1
                                 "\\([ \t]*\r?\n\\)*" ; #2
                                 "[^ \t\f].*\\[Page "
                                 "\\([0-9iIvVxX]+\\)" ; #3
                                 "\\][ ]*\r?\n?"
                                 "\\("  ; <#4
                                 "\f"
                                 "\\([ \t]*\r?\n\\)?" ; #5
                                 "\\("                ; <#6
                                 "\\("                ; <#7
                                 "RFC [0-9]+"
                                 "\\|"  ; |#7
                                 "Internet-Draft[ \t]"
                                 "\\)"  ; >#7
                                 ".*\r?\n"
                                 "\\([ \t]*\r?\n\\)*" ; #8
                                 "\\)?"               ; >#6
                                 "\\)?"               ; >#4
                                 )))
    (while (re-search-forward headerfooter-re nil t)
      ;; Hide old page information for clear reading.
      (irfc-overlay-hide-region (match-end 1) (match-end 0))
      (when (match-beginning 6)
        (let* ((overlay (irfc-overlay-add (match-beginning 1)
                                          (match-end 1)
                                          'irfc-page-number-overlay))
               ;; Get page number.
               (page-num (1+ (string-to-number (match-string 3))))
               ;; Generate page string.
               (page-str (format "(p.%s)" (number-to-string page-num)))
               ;; Generate new page string.
               (new-str (concat (make-string (max (- 79
                                                     (- (match-beginning 1)
                                                        (match-beginning 0))
                                                     (length page-str))
                                                  0)
                                             32)
                                page-str)))
          ;; Record total pages number.
          (setq irfc-total-pages page-num)
          ;; Overlay new page string instead old one.
          (overlay-put overlay
                       'before-string
                       new-str))))))

(defun irfc-render-buffer-hide-blank-line (top-point)
  "Hide any extraneous blank lines between top header and before title.
Argument TOP-POINT is the top point of RFC buffer after render."
  (goto-char top-point)
  (unless (re-search-forward (concat "^[ \t]*\r?\n"
                                     "\\(\\([ \t]*\r?\n\\)+\\)?")
                             nil t)
    (error "This doesn't seem to be an RFC - no blank line before title"))
  (when (match-beginning 1)
    ;; Hide blanks lines between top header and before title.
    (irfc-overlay-hide-region (match-beginning 1) (match-end 1)))
  (point))

(defun irfc-render-buffer-overlay-rfc-number (top-point title-line-point)
  "Overlay RFC number.
Argument TOP-POINT is the top point of RFC buffer after render.
Argument TITLE-LINE-POINT is the title line point of RFC buffer after render."
  (goto-char top-point)
  (while (let ((case-fold-search t))
           (re-search-forward "^\\(request for comments\\|updates\\|obsoletes\\):\\( RFCs\\)?[ \t]+\\(\\([0-9X]+\\)\\(,[ \t]+[0-9]+\\)*\\)"
                              title-line-point t))
    ;; Overlay RFC number.
    (irfc-overlay-add (match-beginning 3)
                      (match-end 3)
                      'irfc-rfc-number-overlay)))

(defun irfc-render-buffer-overlay-std-number (top-point title-line-point)
  "Overlay STD number.
Argument TOP-POINT is the top point of RFC buffer after render.
Argument TITLE-LINE-POINT is the title line point of RFC buffer after render."
  (goto-char top-point)
  (when (let ((case-fold-search nil))
          (re-search-forward "^STD:[ \t]+[0-9]+"
                             title-line-point t))
    ;; Overlay STD number.
    (irfc-overlay-add (match-beginning 0)
                      (match-end 0)
                      'irfc-std-number-overlay)))

(defun irfc-render-buffer-overlay-table-item (top-point)
  "Overlay valid item in table for jump.
Argument TOP-POINT is the top point of RFC buffer after render."
  (when (irfc-have-table-p)             ;whether have table in current buffer
    (goto-char top-point)
    (let* ((case-fold-search t)
           (start-position (re-search-forward "^Table of Contents$" nil t))
           (end-position (re-search-forward "^[0-9\\.]+" nil t)))
      (goto-char start-position)
      (while (re-search-forward irfc-table-regex end-position t)
        ;; Overlay valid table item.
        (irfc-overlay-add (match-beginning 2)
                          (match-end 2)
                          'irfc-table-item-overlay)))))

(defun irfc-render-buffer-overlay-rfc-link (top-point)
  "Overlay valid RFC link.
Argument TOP-POINT is the top point of RFC buffer after render."
  (goto-char top-point)
  (while (let ((case-fold-search nil))
           (re-search-forward "\\[RFC-?[0-9]+\\]"
                              nil t))
    ;; Overlay valid RFC link.
    (irfc-overlay-add (match-beginning 0)
                      (match-end 0)
                      'irfc-rfc-link-overlay)))

(defun irfc-render-buffer-overlay-title (title-line-point)
  "Add overlays to the title line(s).
Note that we currently assume no blank lines in the title; otherwise
we have to do a perfect job of identifying the first non-title line
\(usually a section heading, which some some RFCs make difficult to
always identify).
Argument TITLE-LINE-POINT is the title line point of RFC buffer after render."
  (goto-char title-line-point)
  (when (re-search-forward (concat
                            "\\([^ \t\f\r\n].*[^ \t\f\r\n]\\)"
                            "\\(\r?\n[ \t]*[^ \t\f\r\n].*[^ \t\f\r\n]\\)*"))
    ;; Overlay title.
    (irfc-overlay-add (match-beginning 0)
                      (match-end       0)
                      'irfc-title-overlay)))

(defun irfc-render-buffer-overlay-head (title-line-point)
  "Overlay heading.
Argument TITLE-LINE-POINT is the title line point of RFC buffer after render."
  (goto-char title-line-point)
  (let (match-list)
    (while (setq match-list (irfc-head-move))
      (when (and (nth 0 match-list) (nth 1 match-list))
        ;; Overlay heading number.
        (irfc-overlay-add (nth 0 match-list)
                          (nth 1 match-list)
                          'irfc-head-number-overlay))
      ;; Overlay heading name.
      (irfc-overlay-add (nth 2 match-list)
                        (nth 3 match-list)
                        'irfc-head-name-overlay))))

(defun irfc-render-buffer-overlay-requirement-keyword (top-point)
  "Overlay RFC specification requirements.
Argument TOP-POINT is the top point of RFC buffer after render."
  (goto-char top-point)
  (while (let ((case-fold-search nil))
           (re-search-forward (regexp-opt irfc-requirement-keywords 'words)
                              nil t))
    ;; Overlay RFC requirement keyword.
    (irfc-overlay-add (match-beginning 0)
                      (match-end 0)
                      'irfc-requirement-keyword-overlay)))


(defun irfc-render-buffer-overlay-reference (top-point)
  "Overlay RFC references.
Argument TOP-POINT is the top point of RFC buffer after render."
  (goto-char top-point)
  (while (let ((case-fold-search nil))
           (re-search-forward irfc-reference-regex
                              nil t))
    ;; Overlay RFC reference.
    (irfc-overlay-add (match-beginning 0)
                      (match-end 0)
                      'irfc-reference-overlay)))

(defun irfc-rename-buffer (title-line-point)
  "Rename buffer to include RFC title.
Argument TITLE-LINE-POINT is the title line point of RFC buffer after render."
  (goto-char title-line-point)
  (let ((rfc-txt "")
        (rfc-title ""))
    ;; Set RFC title
    (when (re-search-forward (concat
                              "\\([^ \t\f\r\n].*[^ \t\f\r\n]\\)"
                              "\\(\r?\n[ \t]*[^ \t\f\r\n].*[^ \t\f\r\n]\\)*"))
      (setq rfc-title (match-string 0))
      (setq rfc-title (replace-regexp-in-string "[\r\n\t\f ]+" " " rfc-title)))
    ;; Set RFC txt
    (when (string-match "\\(rfc[0-9]+\.txt\\)" (buffer-name))
      (setq rfc-txt (match-string 1 (buffer-name))))
    ;; Set buffer name
    (if irfc-buffer-name-includes-title
        (rename-buffer (concat rfc-title " (" rfc-txt ")"))
      (rename-buffer rfc-txt))))

(defun irfc-fill-tables (title-line-point)
  "Fill heading names/numbers tables and lists.
Argument TITLE-LINE-POINT is the title line point of RFC buffer after render."
  (clrhash irfc-heading-numbers-table)
  (clrhash irfc-heading-names-table)
  (setq irfc-heading-numbers-list nil)
  (setq irfc-heading-names-list nil)
  (goto-char title-line-point)
  (let (match-list)
    ;; Populate irfc-heading-numbers-table and irfc-heading-names-table
    (while (setq match-list (irfc-head-move))
      (when (and (nth 0 match-list) (nth 1 match-list))
        (puthash (buffer-substring (nth 0 match-list) (nth 1 match-list))
                 (nth 0 match-list) irfc-heading-numbers-table))
      (puthash (buffer-substring (nth 2 match-list) (nth 3 match-list))
               (nth 2 match-list) irfc-heading-names-table))
    ;; Generate irfc-heading-numbers-list
    (maphash (lambda (number _point)
               (setq irfc-heading-numbers-list (cons number irfc-heading-numbers-list)))
             irfc-heading-numbers-table)
    ;; Generate irfc-heading-names-list
    (maphash (lambda (name _point)
               (setq irfc-heading-names-list (cons name irfc-heading-names-list)))
             irfc-heading-names-table)))

(defun irfc-head-move (&optional reverse)
  "Move to special heading.
Return heading list for overlay.
Default is to move to next heading;
move to previous heading if REVERSE is non-nil."
  (let ((case-fold-search t)
        ;; Note: We can't just look for lines that begin in column 0, since
        ;; some RFCs put source code, ASCII-art, description list headings,
        ;; body text, and other stuff in column 0.
        ;; So we look for stock headings and ones that appear to
        ;; begin with section numbers.
        (heading-re (concat
                     "^"
                     "\\("                         ; <#1
                     "\\("                         ; <#2 = numbered section
                     "\\("                         ; <#3 = number
                     "\\([0-9]+\\.?\\|[A-Z]\\.\\)" ; #4
                     "\\([0-9]+\\.?\\)*"           ; #5
                     "\\)"                         ; >#3 = number
                     "[ \t]+"
                     "\\([^\r\n]+\\)"   ; #6 = name
                     "\\)"              ; >#2 = numbered section
                     "\\|"              ; |#1
                     "\\("              ; <#7 = stock section
                     "\\("              ; <#8
                     (mapconcat 'identity irfc-stock-section-names "\\|")
                     "\\)"              ; >#8
                     ":?[ \t]*$"
                     "\\)"              ; >#7 = stock section
                     "\\|"              ; |#1
                     "\\("              ; <#9 = lit-appendix

                     "appendix[ \t]+"
                     "\\([A-Z]\\)"      ; #10 = number

                     "\\(\\.\\|:\\|[ \t]+-\\)" ; #11
                     "[ \t]+"
                     "\\([^\r\n]+\\)"   ; #12 = name

                     "\\)"              ; >#9 = lit-appendix
                     "\\)"              ; >#1
                     )))
    (if (if reverse
            ;; Search backward.
            (re-search-backward heading-re nil t)
          ;; Search forward.
          (re-search-forward heading-re nil t))
        (let ((num-match nil)
              (num-highlight-begin nil)
              (num-highlight-end nil)
              (name-match nil))
          ;; Get the match data numbers.
          (cond ((match-beginning 3) (setq num-match 3
                                           name-match 6))
                ((match-beginning 8) (setq num-match nil
                                           name-match 8))
                ((match-beginning 9) (setq num-match 10
                                           name-match 12)
                 (setq num-highlight-begin (match-beginning 9)
                       num-highlight-end (match-end 11)))
                (t (error " should never happen")))
          ;; Return heading list for overlay.
          (list
           (if num-match
               (or num-highlight-begin
                   (match-beginning num-match))
             nil)
           (if num-match
               (or num-highlight-end
                   (match-end num-match))
             nil)
           (match-beginning name-match)
           (match-end name-match)))
      nil)))

(defun irfc-overlay-put-alist (symbol alist)
  "Put special overlay prop with value.
SYMBOL is overlay variable.
ALIST contain special properties for overlay."
  (mapc (lambda (cell)
          (put symbol (nth 0 cell) (cdr cell)))
        alist))

(defun irfc-overlay-remove-all ()
  "Remove all overlays from current buffer."
  (mapc (lambda (lst)
          (when lst
            (delete-overlay lst)
            ))
        (let ((lists (overlay-lists)))
          (nconc (car lists) (cdr lists)))))

(defun irfc-overlay-add (begin end category)
  "Add overlay.
BEGIN is start position to overlay.
END is end position to overlay.
CATEGORY is special overlay variable."
  (or category (error "Irfc-overlay-add nil category"))
  (let ((overlay (make-overlay begin end)))
    (overlay-put overlay 'category category)
    overlay))

(defun irfc-overlay-hide-region (start end)
  "Use overlay to hide region.
START is start position to hide.
END is end position to hide."
  (irfc-overlay-add start end 'irfc-hide-overlay))

(defun irfc-have-table-p ()
  "Return non-nil if the RFC contain a Table of Contents."
  (save-excursion
    (let ((case-fold-search t))
      (goto-char (point-min))
      (re-search-forward "^Table of Contents$" nil t))))

(defun irfc-front-table-p ()
  "Return t when point is before the Table of Contents."
  (let ((case-fold-search t)
        (original-position (point))
        table-start-position)
    (save-excursion
      (goto-char (point-min))
      (setq table-start-position (re-search-forward "^Table of Contents$" nil t))
      (< original-position table-start-position))))

(defun irfc-in-table-p ()
  "Return t when point is in the Table of Contents."
  (let ((case-fold-search t)
        (original-position (point))
        table-start-position
        table-end-position)
    (save-excursion
      ;; Get start and end position of table.
      (goto-char (point-min))
      (re-search-forward "^Table of Contents$" nil t)
      (beginning-of-line)
      (setq table-start-position (point))
      (re-search-forward "^[0-9\\.]+" nil t)
      (beginning-of-line)
      (forward-char -1)
      ;; Compare current cursor with table scope.
      (setq table-end-position (point))
      (and (>= original-position table-start-position)
           (<= original-position table-end-position)))))

(defun irfc-current-page ()
  "Return current page number at point."
  (let ((original-render-status irfc-render-p)
        current-page)
    (save-excursion
      ;; Turn off render.
      (irfc-render-turn-off)
      (if (re-search-forward "\\[Page \\([0-9]+\\)\\]$" nil t)
          ;; Set current page number when search successful.
          (setq current-page (string-to-number (match-string 1)))
        ;; Set max page number when search failed.
        (setq current-page irfc-total-pages))
      ;; Revert render status.
      (unless (equal original-render-status irfc-render-p)
        (irfc-render-toggle)))
    current-page))

(defun irfc-download (url)
  "Download RFC document URL.
URL is download URL that base on `irfc-download-base-url'."
  (let* ((url-request-method "GET")
         (url-request-extra-headers nil)
         (url-mime-accept-string "*/*")
         (parsed-url (url-generic-parse-url url))
         download-buffer
         download-buffer-name)
    ;; Get unique buffer for handle download information.
    (setq download-buffer (irfc-get-buffer))
    (setq download-buffer-name (buffer-name download-buffer))
    (with-current-buffer (get-buffer download-buffer-name)
      ;; Bind download url with local buffer.
      (setq irfc-download-url url)
      (setq irfc-download-buffer (url-retrieve url
                                               'irfc-download-callback
                                               (list download-buffer-name))))))

(defun irfc-download-callback (&optional redirect download-buffer-name)
  "Callback for `irfc-download'.
With `irfc-download', this downloads RFC files asynchronously.
REDIRECT is default return argument for `url-retrieve'.
DOWNLOAD-BUFFER-NAME is the buffer name for handling download content."
  (if (eq (car redirect) ':error)
      ;; Output error information if download RFC document failed.
      (with-current-buffer (get-buffer download-buffer-name)
        (message "Not found %s." irfc-download-url)
        (kill-buffer download-buffer-name))
    ;; Decode retrieve information.
    (irfc-retrieve-decode download-buffer-name 'utf-8)
    (with-current-buffer (get-buffer download-buffer-name)
      ;; Write file.
      (write-file (expand-file-name (url-file-nondirectory irfc-download-url) irfc-directory))
      ;; Switch buffer.
      (switch-to-buffer (current-buffer)))))

(defun irfc-retrieve-decode (retrieve-buffer-name coding)
  "Decode the retrieve buffer RETRIEVE-BUFFER-NAME with coding CODING."
  (declare (special url-http-end-of-headers))
  (with-current-buffer (get-buffer retrieve-buffer-name)
    (insert
     (with-current-buffer irfc-download-buffer
       (set-buffer-multibyte t)
       (goto-char (1+ url-http-end-of-headers))
       (decode-coding-region
        (point) (point-max)
        (coding-system-change-eol-conversion coding 'dos))
       (buffer-substring (point) (point-max))))
    (goto-char (point-min))))

(defun irfc-get-buffer ()
  "Get a buffer for temporary storage of downloaded content.
Uses `current-time' to make buffer name unique."
  (let ((time-now (current-time)))
    (get-buffer-create (format "*irfc<%s-%s-%s>*"
                               (nth 0 time-now) (nth 1 time-now) (nth 2 time-now)))))

(defun irfc-get-rfc-filename ()
  "Return filename for RFC file.
Look at point and extract an RFC number: either a string `[RFCnnn]',
or a RFC number in a standard header field (`Updates:', etc.).
In that case, return `rfcnnn.txt'; otherwise return nil."
  (let (case-fold-search
        (symbol (irfc-get-symbol-non-blank)))
    (cond ((string-match "\\[\\(RFC-?[0-9]+\\)\\]" symbol)
           (format "%s.txt" (replace-regexp-in-string "-" "" (downcase (match-string 1 symbol)))))
          ((and (string-match "^\\([0-9]+\\),*$" symbol)
                (irfc-title-rfc-link-p))
           (string-match "^\\([0-9]+\\),*$" symbol)
           (format "rfc%s.txt" (match-string 1 symbol)))
          (t
           nil))))

(defun irfc-title-rfc-link-p ()
  "Return t if current point is at title RFC link.
Otherwise return nil."
  (save-excursion
    (let ((case-fold-search t))
      (search-forward-regexp " \\|$" nil t)
      (skip-chars-backward " ")
      (if (string-match "^\\(request for comments\\|updates\\|obsoletes\\):\\( RFCs\\)?[ \t]+\\(\\([0-9X]+\\)\\(,[ \t]+[0-9]+\\)*\\)\\b"
                        (buffer-substring-no-properties (line-beginning-position) (point)))
          t
        nil))))

(defun irfc-get-symbol-non-blank ()
  "Return symbol between `blank'."
  (save-excursion
    (let (start end)
      (search-backward-regexp " \\|^" nil t)
      (skip-chars-forward " ")
      (setq start (point))
      (search-forward-regexp " \\|$" nil t)
      (skip-chars-backward " ")
      (setq end (point))
      (and start
           end
           (>= end start)
           (buffer-substring-no-properties start end)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Overlay setup.
(irfc-overlay-put-alist 'irfc-title-overlay '((face . irfc-title-face)))
(irfc-overlay-put-alist 'irfc-head-name-overlay '((face . irfc-head-name-face)))
(irfc-overlay-put-alist 'irfc-head-number-overlay '((face . irfc-head-number-face)))
(irfc-overlay-put-alist 'irfc-rfc-number-overlay '((face . irfc-rfc-number-face)))
(irfc-overlay-put-alist 'irfc-std-number-overlay '((face . irfc-std-number-face)))
(irfc-overlay-put-alist 'irfc-rfc-link-overlay '((face . irfc-rfc-link-face)))
(irfc-overlay-put-alist 'irfc-table-item-overlay '((face . irfc-table-item-face)))
(irfc-overlay-put-alist 'irfc-requirement-keyword-overlay '((face . irfc-requirement-keyword-face)))
(irfc-overlay-put-alist 'irfc-reference-overlay '((face . irfc-reference-face)))
(irfc-overlay-put-alist 'irfc-hide-overlay
                        '((face . default)
                          (intangible . t)
                          (invisible . t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Cleanup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun irfc-unload-function ()
  "Unload the Irfc library."
  (dolist (buffer (buffer-list))
    (set-buffer buffer)
    (when (eq major-mode 'irfc-mode)
      (irfc-render-turn-off)
      (text-mode)))
  ;; `nil' means continue standard unloading.
  nil)

(provide 'irfc)

;;; irfc.el ends here

;;; LocalWords:  irfc IETF lpi rfcview Dyke txt YourStorageDirectory DarkRed cr
;;; LocalWords:  LawnGreen iesg nPage filepath headerfooter iIvVxX num str SPC
;;; LocalWords:  lst eol Juanma Barranquero ARGth RFCnnn backtab rfcnnn regex
;;; LocalWords:  Juanma's
