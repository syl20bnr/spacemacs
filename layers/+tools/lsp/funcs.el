;;; funcs.el --- Language Server Protocol Layer functions file for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Fangrui Song <i@maskray.me>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs//setup-lsp-jump-handler ()
  "Set jump handler for LSP with the given MODE."
    (add-to-list 'space-macs-jump-handlers '(lsp-ui-peek-find-definitions :async t)))


;; Key bindings

;; Used for lsp-ui-peek-mode, but may be able to use some space-macs fn. instead?
(defun space-macs/lsp-define-key (keymap key def &rest bindings)
  "Define multiple key bindings with KEYMAP KEY DEF BINDINGS."
  (interactive)
  (while key
    (define-key keymap (kbd key) def)
    (setq key (pop bindings)
          def (pop bindings))))

(defun space-macs/lsp-bind-keys ()
  "Define key bindings for the lsp minor mode."
  (cl-ecase lsp-navigation
    ('simple (space-macs//lsp-bind-simple-navigation-functions "g"))
    ('peek (space-macs//lsp-bind-peek-navigation-functions "g"))
    ('both
     (space-macs//lsp-bind-simple-navigation-functions "g")
     (space-macs//lsp-bind-peek-navigation-functions "G")))

  (space-macs/set-leader-keys-for-minor-mode 'lsp-mode
    ;; format
    "=" "format"
    "=b" #'lsp-format-buffer
    "=r" #'lsp-format-region
    "=o" #'lsp-organize-imports
    ;; code actions
    "a" "code actions"
    "aa" #'lsp-execute-code-action
    "af" #'space-macs//lsp-action-placeholder
    "ar" #'space-macs//lsp-action-placeholder
    "as" #'space-macs//lsp-action-placeholder
    ;; goto
    ;; N.B. implementation and references covered by xref bindings / lsp provider...
    "g" "goto"
    "gt" #'lsp-find-type-definition
    "gk" #'space-macs/lsp-avy-goto-word
    "gK" #'space-macs/lsp-avy-goto-symbol
    "gM" #'lsp-ui-imenu
    ;; help
    "h" "help"
    "hh" #'lsp-describe-thing-at-point
    ;; jump
    ;; backend
    "b" "backend"
    "bd" #'lsp-describe-session
    "br" #'lsp-workspace-restart
    "bs" #'lsp-workspace-shutdown
    "bv" #'lsp-version
    ;; refactor
    "r" "refactor"
    "rr" #'lsp-rename
    ;; toggles
    "T" "toggle"
    "Td" #'lsp-ui-doc-mode
    "Ts" #'lsp-ui-sideline-mode
    "TF" #'space-macs/lsp-ui-doc-func
    "TS" #'space-macs/lsp-ui-sideline-symb
    "TI" #'space-macs/lsp-ui-sideline-ignore-duplicate
    "Tl" #'lsp-lens-mode
    ;; folders
    "F" "folder"
    "Fs" #'lsp-workspace-folders-switch
    "Fr" #'lsp-workspace-folders-remove
    "Fa" #'lsp-workspace-folders-add
    ;; text/code
    "x" "text/code"
    "xh" #'lsp-document-highlight
    "xl" #'lsp-lens-show
    "xL" #'lsp-lens-hide))

(defun space-macs//lsp-bind-simple-navigation-functions (prefix-char)
  (space-macs/set-leader-keys-for-minor-mode 'lsp-mode
    (concat prefix-char "i") #'lsp-find-implementation
    (concat prefix-char "d") #'xref-find-definitions
    (concat prefix-char "r") #'xref-find-references
    (concat prefix-char "e") #'lsp-tree-macs-errors-list
    (concat prefix-char "b") #'xref-pop-marker-stack)
  (cond
   ((configuration-layer/package-usedp 'helm)
    (space-macs/set-leader-keys-for-minor-mode 'lsp-mode
      (concat prefix-char "s") #'helm-lsp-workspace-symbol
      (concat prefix-char "S") #'helm-lsp-global-workspace-symbol))
   ((configuration-layer/package-usedp 'ivy)
    (space-macs/set-leader-keys-for-minor-mode 'lsp-mode
      (concat prefix-char "s") #'lsp-ivy-workspace-symbol
      (concat prefix-char "S") #'lsp-ivy-global-workspace-symbol))
   (t (space-macs/set-leader-keys-for-minor-mode 'lsp-mode
        (concat prefix-char "s") #'lsp-ui-find-workspace-symbol))))

(defun space-macs//lsp-bind-peek-navigation-functions (prefix-char)
  (space-macs/set-leader-keys-for-minor-mode 'lsp-mode
    "G" "peek"
    (concat prefix-char "i") #'lsp-ui-peek-find-implementation
    (concat prefix-char "d") #'lsp-ui-peek-find-definitions
    (concat prefix-char "r") #'lsp-ui-peek-find-references
    (concat prefix-char "s") #'lsp-ui-peek-find-workspace-symbol
    (concat prefix-char "S") #'lsp-tree-macs-symbols
    (concat prefix-char "b") #'lsp-ui-peek-jump-backward
    (concat prefix-char "e") #'lsp-ui-flycheck-list
    (concat prefix-char "n") #'lsp-ui-peek-jump-forward))

(defun space-macs//lsp-bind-extensions-for-mode (mode
                                                layer-name
                                                backend-name
                                                key
                                                kind)
  "Bind extensions under the appropriate prefix(es) for the major-mode MODE.
MODE should be a quoted symbol corresponding to a valid major mode.

LAYER-NAME is a string, the name of the layer
BACKEND-NAME is a string, the name of the backend that's set for the layer
KEY is a string corresponding to a key sequence
KIND is a quoted symbol corresponding to an extension defined using
`lsp-define-extensions'."
  (cl-ecase lsp-navigation
    ('simple (space-macs/set-leader-keys-for-major-mode mode
               (concat "g" key)
               (space-macs//lsp-extension-name
                layer-name backend-name "find" kind)))
    ('peek (space-macs/set-leader-keys-for-major-mode mode
             (concat "g" key)
             (space-macs//lsp-extension-name
              layer-name backend-name "peek" kind)))
    ('both (space-macs/set-leader-keys-for-major-mode mode
             (concat "g" key)
             (space-macs//lsp-extension-name
              layer-name backend-name "find" kind)
             (concat "G" key)
             (space-macs//lsp-extension-name
              layer-name backend-name "peek" kind)))))

(defun space-macs/lsp-bind-extensions-for-mode (mode
                                               layer-name
                                               backend-name
                                               key
                                               kind
                                               &rest bindings)
  "Bind extensions under the appropriate prefix(es) for the major-mode MODE.

MODE is a quoted symbol corresponding to a valid major mode.
LAYER-NAME is a string, the name of the layer
BACKEND-NAME is a string, the name of the backend that's set for the layer
KEY is a string corresponding to a key sequence
KIND is a quoted symbol corresponding to an extension defined using
`lsp-define-extensions'.
BINDINGS is other KEY and KIND to create other key bindings."
  (while key
    (space-macs//lsp-bind-extensions-for-mode mode layer-name backend-name key kind)
    (setq key (pop bindings)
          kind (pop bindings))))


;; Extensions

(defun space-macs/lsp-define-extensions (layer-name
                                        backend-name
                                        kind
                                        request
                                        &optional extra)
  "Wrap backend-specific LSP extensions.

This function uses `lsp-find-custom' and `lsp-ui-peek-find-custom'.
The function names are defined in `space-macs//lsp-extension-name.'"
  (dolist (nav-mode '("find" "peek"))
    (if extra
        (space-macs//lsp-define-custom-extension
         layer-name backend-name nav-mode kind request extra)
      (space-macs//lsp-define-custom-extension
       layer-name backend-name nav-mode kind request))))

(defun space-macs//lsp-extension-name (layer-name backend-name nav-mode kind)
  "Return the extension name.

Pattern is `space-macs/<layer-name>-<backend-end>-<nav-mode>-<kind>'.

Examples of return name:
  - space-macs/c-c++-lsp-clangd-find-clangd-other-file
  - space-macs/c-c++-lsp-clangd-peek-clangd-other-file

LAYER-NAME is a string, the name of the layer
BACKEND-NAME is a string, the name of the backend that's set for the layer
NAV-MODE is a string with value `peek' or `find'
KIND is a quoted symbol corresponding to an extension defined using
`lsp-define-extensions'."
  (intern
   (concat "space-macs/"
           layer-name "-" backend-name "-" nav-mode "-" (symbol-name kind))))

(defun space-macs//lsp-define-custom-extension (layer-name
                                               backend-name
                                               nav-mode
                                               kind
                                               request
                                               &optional extra)
  "Helper function to define custom LSP extensions.

LAYER-NAME is a string, the name of the layer
BACKEND-NAME is a string, the name of the backend that's set for the layer
NAV-MODE is a string with value `peek' or `find'
KIND is a quoted symbol corresponding to an extension defined using
`lsp-define-extensions'.
REQUEST is a string defining the request
EXTRA is an additional parameter that's passed to the LSP function"
  (let ((lsp-extension-fn (if (equal nav-mode "find")
                              'lsp-find-locations
                            'lsp-ui-peek-find-custom))
        (extension-name (space-macs//lsp-extension-name
                         layer-name backend-name nav-mode kind))
        (extension-descriptor (format (concat nav-mode " %s")
                                      (symbol-name kind))))
    (if extra
        (defalias extension-name
          `(lambda ()
             ,extension-descriptor
             (interactive)
             (funcall ',lsp-extension-fn ,request ',extra)))
      (defalias extension-name
        `(lambda ()
           ,extension-descriptor
           (interactive)
           (funcall ',lsp-extension-fn ,request))))))


;; Utils

(defun space-macs/lsp-ui-doc-func ()
  "Toggle the function signature in the lsp-ui-doc overlay"
  (interactive)
  (setq lsp-ui-doc-include-signature (not lsp-ui-doc-include-signature)))

(defun space-macs/lsp-ui-sideline-symb ()
  "Toggle the symbol in the lsp-ui-sideline overlay.
(generally redundant in C modes)"
  (interactive)
  (setq lsp-ui-sideline-show-symbol (not lsp-ui-sideline-show-symbol)))

(defun space-macs/lsp-ui-sideline-ignore-duplicate ()
  "Toggle ignore duplicates for lsp-ui-sideline overlay"
  (interactive)
  (setq lsp-ui-sideline-ignore-duplicate
        (not lsp-ui-sideline-ignore-duplicate)))

(defun space-macs//lsp-action-placeholder ()
  (interactive)
  (message "Not supported yet... (to be implemented in 'lsp-mode')"))


;; ivy integration

(defun space-macs//lsp-avy-document-symbol (all)
  (interactive)
  (let ((line 0) (col 0) (w (selected-window))
        (ccls (and (memq major-mode '(c-mode c++-mode objc-mode)) (eq c-c++-backend 'lsp-ccls)))
        (start-line (1- (line-number-at-pos (window-start))))
        (end-line (1- (line-number-at-pos (window-end))))
        ranges point0 point1
        candidates)
    (save-excursion
      (goto-char 1)
      (cl-loop for loc in
               (lsp--send-request
                (lsp--make-request
                 "textDocument/documentSymbol"
                 `(:textDocument ,(lsp--text-document-identifier)
                                 :all ,(if all t :json-false)
                                 :startLine ,start-line :endLine ,end-line)))
               for range = (if ccls
                               loc
                             (->> loc (gethash "location") (gethash "range")))
               for range_start = (gethash "start" range)
               for range_end = (gethash "end" range)
               for l0 = (gethash "line" range_start)
               for c0 = (gethash "character" range_start)
               for l1 = (gethash "line" range_end)
               for c1 = (gethash "character" range_end)
               while (<= l0 end-line)
               when (>= l0 start-line)
               do
               (forward-line (- l0 line))
               (forward-char c0)
               (setq point0 (point))
               (forward-line (- l1 l0))
               (forward-char c1)
               (setq point1 (point))
               (setq line l1 col c1)
               (push `((,point0 . ,point1) . ,w) candidates)))
    (avy-with avy-document-symbol
      (avy--process candidates
                    (avy--style-fn avy-style)))))

(defun space-macs/lsp-avy-goto-word ()
  (interactive)
  (space-macs//lsp-avy-document-symbol t))

(defun space-macs/lsp-avy-goto-symbol ()
  (interactive)
  (space-macs//lsp-avy-document-symbol nil))


