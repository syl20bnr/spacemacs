;;; funcs.el --- cquery layer function definitions file for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Cormac Cannon <cormacc-public@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(require 'cl-lib)
(require 'subr-x)

(defun cquery//enable ()
  (condition-case nil
    (lsp-cquery-enable)
    (user-error nil)))


(defun cquery/customise-lsp-ui-peek ()
  (defun cquery/base () (interactive) (lsp-ui-peek-find-custom 'base "$cquery/base"))
  (defun cquery/callers () (interactive) (lsp-ui-peek-find-custom 'callers "$cquery/callers"))
  (defun cquery/derived () (interactive) (lsp-ui-peek-find-custom 'derived "$cquery/derived"))
  (defun cquery/vars () (interactive) (lsp-ui-peek-find-custom 'vars "$cquery/vars"))
  (defun cquery/random () (interactive) (lsp-ui-peek-find-custom 'random "$cquery/random"))

  (defun cquery/references-address ()
    (interactive)
    (lsp-ui-peek-find-custom
      'address "textDocument/references"
      (plist-put (lsp--text-document-position-params) :context
        '(:role 128))))

  (defun cquery/references-read ()
    (interactive)
    (lsp-ui-peek-find-custom
      'read "textDocument/references"
      (plist-put (lsp--text-document-position-params) :context
        '(:role 8))))

  (defun cquery/references-write ()
    (interactive)
    (lsp-ui-peek-find-custom
      'write "textDocument/references"
      (plist-put (lsp--text-document-position-params) :context
        '(:role 16))))

  )

(defun cquery/define-keys-for-mode (mode)
  "Define key bindings for the specific MODE."

  (spacemacs/set-leader-keys-for-major-mode mode
    ;; hierarchy
    "hb" #'cquery/base
    "hd" #'cquery/derived
    "hc" #'cquery-call-hierarchy
    "hC" (lambda () (interactive) (cquery-call-hierarchy t))
    "hi" #'cquery-inheritance-hierarchy
    "hI" (lambda () (interactive) (cquery-inheritance-hierarchy t))
    "hm" #'cquery-member-hierarchy
    "hM" (lambda () (interactive) (cquery-member-hierarchy t))
    ;; lsp/peek
    "j&" #'cquery/references-address
    "jR" #'cquery/references-read
    "jW" #'cquery/references-write
    "jc" #'cquery/callers
    "jv" #'cquery/vars
    )
  )
