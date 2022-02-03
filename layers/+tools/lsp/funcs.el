;;; funcs.el --- Language Server Protocol Layer functions file for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Fangrui Song <i@maskray.me>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(defun spacemacs//setup-lsp-jump-handler ()
  "Set jump handler for LSP with the given MODE."
  (add-to-list 'spacemacs-jump-handlers '(lsp-ui-peek-find-definitions :async t)))


;; Key bindings

;; Used for lsp-ui-peek-mode, but may be able to use some spacemacs fn. instead?
(defun spacemacs/lsp-define-key (keymap key def &rest bindings)
  "Define multiple key bindings with KEYMAP KEY DEF BINDINGS."
  (while key
    (define-key keymap (kbd key) def)
    (setq key (pop bindings)
          def (pop bindings))))

(defun spacemacs/lsp-bind-upstream-keys ()
  "Bind upstream `lsp-command-map' behind \"SPC m\" and the likes."
  (bind-map lsp-command-map
    :minor-modes (lsp-mode)
    :keys ((concat dotspacemacs-emacs-leader-key " m") dotspacemacs-major-mode-emacs-leader-key)
    :evil-keys ((concat dotspacemacs-leader-key " m") dotspacemacs-major-mode-leader-key)
    :evil-states (normal motion visual evilified))
  (dolist (it '(("=" . "format")
                ("F" . "folder")
                ("T" . "toggle")
                ("g" . "goto")
                ("h" . "help")
                ("r" . "refactor")
                ("w" . "workspace")
                ("a" . "actions")
                ("G" . "peek")))
    (which-key-add-keymap-based-replacements lsp-command-map (car it) (cdr it)))
  ;; we still have to bind keys for `lsp-ivy', `consult-lsp' and `helm-lsp'
  (cond
   ((configuration-layer/package-usedp 'ivy)
    (spacemacs/lsp-define-key lsp-command-map
                              "gs" #'lsp-ivy-workspace-symbol
                              "gS" #'lsp-ivy-global-workspace-symbol
                              "FR" #'lsp-ivy-workspace-folders-remove))
   ((configuration-layer/package-usedp 'helm)
    (spacemacs/lsp-define-key lsp-command-map
                              "gs" #'helm-lsp-workspace-symbol
                              "gS" #'helm-lsp-global-workspace-symbol))
   ((configuration-layer/package-usedp 'consult)
    (define-key lsp-mode-map
      [remap lsp-treemacs-errors-list]
      #'consult-lsp-diagnostics)
    (spacemacs/lsp-define-key lsp-command-map
                              "gs" #'consult-lsp-symbols
                              "gf" #'consult-lsp-file-symbols))))

(defun spacemacs/lsp-bind-keys ()
  "Define key bindings for the lsp minor mode."
  (cl-ecase lsp-navigation
    ('simple (spacemacs//lsp-bind-simple-navigation-functions "g"))
    ('peek (spacemacs//lsp-bind-peek-navigation-functions "g"))
    ('both
     (spacemacs//lsp-bind-simple-navigation-functions "g")
     (spacemacs//lsp-bind-peek-navigation-functions "G")))

  (spacemacs/set-leader-keys-for-minor-mode 'lsp-mode
    ;; format
    "=" "format"
    "=b" #'lsp-format-buffer
    "=r" #'lsp-format-region
    "=o" #'lsp-organize-imports
    ;; code actions
    "a" "code actions"
    "aa" #'lsp-execute-code-action
    "af" #'spacemacs//lsp-action-placeholder
    "ar" #'spacemacs//lsp-action-placeholder
    "as" #'spacemacs//lsp-action-placeholder
    ;; goto
    ;; N.B. implementation and references covered by xref bindings / lsp provider...
    "g" "goto"
    "gt" #'lsp-find-type-definition
    "gk" #'spacemacs/lsp-avy-goto-word
    "gK" #'spacemacs/lsp-avy-goto-symbol
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
    "Tl" "lsp"
    "Tld" #'lsp-ui-doc-mode
    "Tls" #'lsp-ui-sideline-mode
    "TlF" #'spacemacs/lsp-ui-doc-func
    "TlS" #'spacemacs/lsp-ui-sideline-symb
    "TlI" #'spacemacs/lsp-ui-sideline-ignore-duplicate
    "Tll" #'lsp-lens-mode
    ;; folders
    "F" "folder"
    "Fs" #'lsp-workspace-folders-switch
    "Fr" #'lsp-workspace-folders-remove
    "Fa" #'lsp-workspace-folders-add
    ;; text/code
    "x" "text/code"
    "xh" #'lsp-document-highlight
    "xl" #'lsp-lens-show
    "xL" #'lsp-lens-hide)
  (when (configuration-layer/package-used-p 'lsp-treemacs)
    (spacemacs/set-leader-keys-for-minor-mode 'lsp-mode
      "gh" #'lsp-treemacs-call-hierarchy
      "gT" #'lsp-treemacs-type-hierarchy)))

(defun spacemacs//lsp-bind-simple-navigation-functions (prefix-char)
  (spacemacs/set-leader-keys-for-minor-mode 'lsp-mode
    (concat prefix-char "i") #'lsp-find-implementation
    (concat prefix-char "d") #'xref-find-definitions
    (concat prefix-char "r") #'xref-find-references
    (concat prefix-char "e") #'lsp-treemacs-errors-list
    (concat prefix-char "b") #'xref-pop-marker-stack)
  (cond
   ((configuration-layer/package-usedp 'helm)
    (spacemacs/set-leader-keys-for-minor-mode 'lsp-mode
      (concat prefix-char "s") #'helm-lsp-workspace-symbol
      (concat prefix-char "S") #'helm-lsp-global-workspace-symbol)
    (spacemacs/set-leader-keys "pE" #'helm-lsp-diagnostics))
   ((configuration-layer/package-usedp 'ivy)
    (spacemacs/set-leader-keys-for-minor-mode 'lsp-mode
      (concat prefix-char "s") #'lsp-ivy-workspace-symbol
      (concat prefix-char "S") #'lsp-ivy-global-workspace-symbol))
   (t (spacemacs/set-leader-keys-for-minor-mode 'lsp-mode
        (concat prefix-char "s") #'lsp-ui-find-workspace-symbol))))

(defun spacemacs//lsp-bind-peek-navigation-functions (prefix-char)
  (spacemacs/set-leader-keys-for-minor-mode 'lsp-mode
    "G" "peek"
    (concat prefix-char "i") #'lsp-ui-peek-find-implementation
    (concat prefix-char "d") #'lsp-ui-peek-find-definitions
    (concat prefix-char "r") #'lsp-ui-peek-find-references
    (concat prefix-char "s") #'lsp-ui-peek-find-workspace-symbol
    (concat prefix-char "S") #'lsp-treemacs-symbols
    (concat prefix-char "b") #'lsp-ui-peek-jump-backward
    (concat prefix-char "e") #'lsp-ui-flycheck-list
    (concat prefix-char "n") #'lsp-ui-peek-jump-forward))

(defun spacemacs//lsp-bind-extensions-for-mode (mode
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
    ('simple (spacemacs/set-leader-keys-for-major-mode mode
               (concat "g" key)
               (spacemacs//lsp-extension-name
                layer-name backend-name "find" kind)))
    ('peek (spacemacs/set-leader-keys-for-major-mode mode
             (concat "g" key)
             (spacemacs//lsp-extension-name
              layer-name backend-name "peek" kind)))
    ('both (spacemacs/set-leader-keys-for-major-mode mode
             (concat "g" key)
             (spacemacs//lsp-extension-name
              layer-name backend-name "find" kind)
             (concat "G" key)
             (spacemacs//lsp-extension-name
              layer-name backend-name "peek" kind)))))

(defun spacemacs/lsp-bind-extensions-for-mode (mode
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
    (spacemacs//lsp-bind-extensions-for-mode mode layer-name backend-name key kind)
    (setq key (pop bindings)
          kind (pop bindings))))


;; Extensions

(defun spacemacs/lsp-define-extensions (layer-name
                                        backend-name
                                        kind
                                        request
                                        &optional extra)
  "Wrap backend-specific LSP extensions.

This function uses `lsp-find-custom' and `lsp-ui-peek-find-custom'.
The function names are defined in `spacemacs//lsp-extension-name.'"
  (dolist (nav-mode '("find" "peek"))
    (if extra
        (spacemacs//lsp-define-custom-extension
         layer-name backend-name nav-mode kind request extra)
      (spacemacs//lsp-define-custom-extension
       layer-name backend-name nav-mode kind request))))

(defun spacemacs//lsp-extension-name (layer-name backend-name nav-mode kind)
  "Return the extension name.

Pattern is `spacemacs/<layer-name>-<backend-end>-<nav-mode>-<kind>'.

Examples of return name:
  - spacemacs/c-c++-lsp-clangd-find-clangd-other-file
  - spacemacs/c-c++-lsp-clangd-peek-clangd-other-file

LAYER-NAME is a string, the name of the layer
BACKEND-NAME is a string, the name of the backend that's set for the layer
NAV-MODE is a string with value `peek' or `find'
KIND is a quoted symbol corresponding to an extension defined using
`lsp-define-extensions'."
  (intern
   (concat "spacemacs/"
           layer-name "-" backend-name "-" nav-mode "-" (symbol-name kind))))

(defun spacemacs//lsp-define-custom-extension (layer-name
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
        (extension-name (spacemacs//lsp-extension-name
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

(defun spacemacs/lsp-ui-doc-func ()
  "Toggle the function signature in the lsp-ui-doc overlay"
  (interactive)
  (setq lsp-ui-doc-include-signature (not lsp-ui-doc-include-signature)))

(defun spacemacs/lsp-ui-sideline-symb ()
  "Toggle the symbol in the lsp-ui-sideline overlay.
(generally redundant in C modes)"
  (interactive)
  (setq lsp-ui-sideline-show-symbol (not lsp-ui-sideline-show-symbol)))

(defun spacemacs/lsp-ui-sideline-ignore-duplicate ()
  "Toggle ignore duplicates for lsp-ui-sideline overlay"
  (interactive)
  (setq lsp-ui-sideline-ignore-duplicate
        (not lsp-ui-sideline-ignore-duplicate)))

(defun spacemacs//lsp-action-placeholder ()
  (interactive)
  (message "Not supported yet... (to be implemented in 'lsp-mode')"))

(defun spacemacs//lsp-client-server-id ()
  "Return the ID of the LSP server associated with current project."
  (mapcar 'lsp--client-server-id (mapcar 'lsp--workspace-client (lsp-workspaces))))


;; ivy integration

(defun spacemacs//lsp-avy-document-symbol (all)
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

(defun spacemacs/lsp-avy-goto-word ()
  (interactive)
  (spacemacs//lsp-avy-document-symbol t))

(defun spacemacs/lsp-avy-goto-symbol ()
  (interactive)
  (spacemacs//lsp-avy-document-symbol nil))
