;;; ebib-ivy.el --- Part of Ebib, a BibTeX database manager  -*- lexical-binding: t -*-

;; Copyright (c) 2022 Hugo Heagren
;; All rights reserved.

;; Author: Hugo Heagren <hugo@heagren.com>
;; Maintainer: Joost Kremers <joostkremers@fastmail.fm>

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. The name of the author may not be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES ; LOSS OF USE,
;; DATA, OR PROFITS ; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; This file is part of Ebib, a BibTeX database manager for Emacs.  It contains
;; the code to integrate ivy's features more tightly into the interface.
;; To use this integration, add (require 'ebib-ivy) to init.el file.

;;; Code:

(require 'ivy nil 'noerror)
(require 'ebib)

(declare-function ivy-add-actions "ext:ivy.el" (cmd actions))

(ivy-add-actions
 'ebib-jump-to-field
 '(("c" ebib-copy-field-contents "copy field contents")
   ("k" ebib-kill-field-contents "kill field contents")
   ("d" ebib-delete-field-contents "delete field contents")
   ("y" ebib-yank-field-contents "yank field contents")
   ("e" ebib-edit-current-field "edit field")
   ("m" ebib--edit-field-as-multiline "edit multiline field")
   ("v" ebib-view-field-as-help "view field as help")
   ("s" ebib-insert-abbreviation "insert abbreviation")
   ("r" ebib-toggle-raw "toggle raw")
   ("a" ebib-add-field "add field")
   ("u" (lambda (_) (ebib-browse-url)) "browse url")
   ("I" (lambda (_) (ebib-browse-doi)) "browse doi")))

(provide 'ebib-ivy)

;;; ebib-notes.el ends here
