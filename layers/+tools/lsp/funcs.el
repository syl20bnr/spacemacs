;;; funcs.el --- Language Server Protocol Layer functions file for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Fangrui Song <i@maskray.me>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//lsp-sync-peek-face ()
  "Synchronize the face used in `lsp-ui' peek window according to the theme."
  (set-face-attribute 'lsp-ui-peek-list nil
                      :background (face-attribute 'hl-line :background nil t))
  (set-face-attribute 'lsp-ui-peek-peek nil
                      :background (face-attribute 'hl-line :background nil t))
  (set-face-attribute 'lsp-ui-peek-selection nil
                      :background (face-attribute 'highlight :background nil t)
                      :foreground (face-attribute 'default :foreground nil t))
  (set-face-attribute 'lsp-ui-peek-filename nil
                      :foreground (face-attribute 'font-lock-constant-face
                                                  :foreground nil t))
  (set-face-attribute 'lsp-ui-peek-highlight nil
                      :background (face-attribute 'highlight :background nil t)
                      :foreground (face-attribute 'highlight :foreground nil t)
                      :distant-foreground (face-attribute 'highlight
                                                          :foreground nil t))
  (set-face-attribute 'lsp-ui-peek-header nil
                      :background (face-attribute 'highlight :background nil t)
                      :foreground (face-attribute 'default :foreground nil t))
  )

(defun spacemacs//setup-lsp-jump-handler (&rest modes)
  "Set jump handler for LSP with the given MODE."
  (dolist (m modes)
    (add-to-list (intern (format "spacemacs-jump-handlers-%S" m))
                 '(lsp-ui-peek-find-definitions :async t))))

(defun fix-lsp-company-prefix ()
  "fix lsp-javascript company prefix
https://github.com/emacs-lsp/lsp-javascript/issues/9#issuecomment-379515379"
  (interactive)
  (defun lsp-prefix-company-transformer (candidates)
    (let ((completion-ignore-case t))
      (if (and (car candidates)
               (get-text-property 0 'lsp-completion-prefix (car candidates)))
          (all-completions (company-grab-symbol) candidates)
        candidates)))
  (make-local-variable 'company-transformers)
  (add-to-list 'company-transformers 'lsp-prefix-company-transformer))

(defun spacemacs/lsp-bind-keys-for-mode (mode)
  "Define key bindings for the specific MODE."
  (spacemacs/declare-prefix-for-mode mode "m=" "format")
  (spacemacs/declare-prefix-for-mode mode "mh" "help")
  (spacemacs/declare-prefix-for-mode mode "mb" "backend")
  (spacemacs/declare-prefix-for-mode mode "mr" "refactor")
  (spacemacs/declare-prefix-for-mode mode "mT" "toggle")
  (spacemacs/declare-prefix-for-mode mode "mg" "goto")
  (spacemacs/declare-prefix-for-mode mode "mG" "peek")
  (dolist (prefix '("mg" "mG"))
    (spacemacs/declare-prefix-for-mode mode (concat prefix "h") "hierarchy")
    (spacemacs/declare-prefix-for-mode mode (concat prefix "m") "members"))

  (spacemacs//lsp-bind-navigation-keys-for-mode mode)

  (spacemacs/set-leader-keys-for-major-mode mode
    ;;format
    "=b" #'lsp-format-buffer
    ;;goto
    "gt" #'lsp-goto-type-definition
    "gk" #'spacemacs/lsp-avy-document-symbol
    "ge" #'lsp-ui-flycheck-list
    "gM" #'lsp-ui-imenu
    ;;help
    "hh" #'lsp-describe-thing-at-point
    ;;jump
    ;;backend
    "ba" #'lsp-execute-code-action
    "bc" #'lsp-capabilities
    "br" #'lsp-restart-workspace
    ;;refactor
    "rr" #'lsp-rename
    ;;toggles
    "Td" #'lsp-ui-doc-mode
    "Ts" #'lsp-ui-sideline-mode
    "TF" #'spacemacs/lsp-ui-doc-func
    "TS" #'spacemacs/lsp-ui-sideline-symb
    "TI" #'spacemacs/lsp-ui-sideline-ignore-duplicate))

(defun spacemacs//lsp-bind-navigation-keys-for-mode (mode)
  (ecase lsp-navigation
    ('simple (spacemacs//lsp-bind-simple-navigation-functions-for-mode mode "g"))
    ('peek (spacemacs//lsp-bind-peek-navigation-functions-for-mode mode "g"))
    ('both
      (spacemacs//lsp-bind-simple-navigation-functions-for-mode mode "g")
      (spacemacs//lsp-bind-peek-navigation-functions-for-mode mode "G"))))

(defun spacemacs//lsp-bind-simple-navigation-functions-for-mode (mode prefix-char)
  (spacemacs/set-leader-keys-for-major-mode mode
    (concat prefix-char "i") #'lsp-goto-implementation
    (concat prefix-char "d") #'xref-find-definitions
    (concat prefix-char "r") #'xref-find-references
    (concat prefix-char "s") #'lsp-ui-find-workspace-symbol
    (concat prefix-char "p") #'xref-pop-marker-stack))

(defun spacemacs//lsp-bind-peek-navigation-functions-for-mode (mode prefix-char)
  (spacemacs/set-leader-keys-for-major-mode mode
    (concat prefix-char "i") #'lsp-ui-peek-find-implementation
    (concat prefix-char "d") #'lsp-ui-peek-find-definitions
    (concat prefix-char "r") #'lsp-ui-peek-find-references
    (concat prefix-char "s") #'lsp-ui-peek-find-workspace-symbol
    (concat prefix-char "p") #'lsp-ui-peek-jump-backward
    (concat prefix-char "n") #'lsp-ui-peek-jump-forward))

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
  (setq lsp-ui-sideline-ignore-duplicate (not lsp-ui-sideline-ignore-duplicate)))

;; Used for lsp-ui-peek-mode, but may be able to use some spacemacs fn. instead?
(defun spacemacs/lsp-define-key (keymap key def &rest bindings)
  "Define multiple key bindings with KEYMAP KEY DEF BINDINGS."
  (interactive)
  (while key
    (define-key keymap (kbd key) def)
    (setq key (pop bindings)
      def (pop bindings))))

;; From https://github.com/MaskRay/Config/blob/master/home/.config/doom/autoload/misc.el#L118
;;;###autoload
(defun spacemacs/lsp-avy-document-symbol ()
  (interactive)
  (let (ranges point0 point1 (line 0) (col 0) (w (selected-window)) candidates)
    (save-excursion
      (goto-char 1)
      (dolist (loc
               (lsp--send-request (lsp--make-request
                                   "textDocument/documentSymbol"
;;;;;; I added :all t in ccls to return all symbols in the document
                                   `(:textDocument ,(lsp--text-document-identifier) :all t))))
        (let ((range (->> loc (gethash "location") (gethash "range"))))
          (-let* [((&hash "line" l0 "character" c0) (gethash "start" range))
                  ((&hash "line" l1 "character" c1) (gethash "end" range))]
            (when (or (< line l0) (and (= line l0) (<= col c0)))
              (forward-line (- l0 line))
              (forward-char c0)
              (setq point0 (point))
              (forward-line (- l1 l0))
              (forward-char c1)
              (setq point1 (point))
              (setq line l1 col c1)
              (push `((,point0 . ,point1) . ,w) candidates))))))
    (avy-with avy-document-symbol
      (avy--process candidates
        (avy--style-fn avy-style)))))


;; These functions facilitate extension of the navigation-mode keybindings in derived layers
;; See c/c++ layer for a usage example
(defun spacemacs//lsp-get-extension-name (layer-name nav-mode kind)
  (intern (concat layer-name "/" nav-mode "-" (symbol-name kind))))

(defun spacemacs//lsp-define-custom-extension (layer-name nav-mode kind request &optional extra)
  (let ((lsp-extension-fn (if (eq nav-mode "find")
                            'lsp-find-custom
                            'lsp-ui-peek-find-custom))
         (extension-name (spacemacs//lsp-get-extension-name layer-name nav-mode kind))
         (extension-descriptor (format (concat nav-mode " %s") (symbol-name kind))))
    (if extra
      (defalias extension-name `(lambda () ,extension-descriptor (interactive) (funcall ',lsp-extension-fn ,request ',extra)))
      (defalias extension-name `(lambda () ,extension-descriptor (interactive) (funcall ',lsp-extension-fn ,request))))))

(defun spacemacs/lsp-define-extensions (layer-name kind request &optional extra)
  "Wrap backend-specific LSP extensions using lsp-find-custom and lsp-ui-peek-find-custom.
The function names will be <layer-name>/find-<kind> and <layer-name>/peek-<kind>, respectively."
  (dolist (nav-mode '("find" "peek"))
    (if extra
      (spacemacs//lsp-define-custom-extension layer-name nav-mode kind request extra)
      (spacemacs//lsp-define-custom-extension layer-name nav-mode kind request))))

(defun spacemacs//lsp-bind-extensions (mode layer-name key kind)
  (ecase lsp-navigation
    ('simple (spacemacs/set-leader-keys-for-major-mode mode
               (concat "g" key) (spacemacs//lsp-get-extension-name layer-name "find" kind)))
    ('peek (spacemacs/set-leader-keys-for-major-mode mode
             (concat "g" key) (spacemacs//lsp-get-extension-name layer-name "peek" kind)))
    ('both (spacemacs/set-leader-keys-for-major-mode mode
             (concat "g" key) (spacemacs//lsp-get-extension-name layer-name "find" kind)
             (concat "G" key) (spacemacs//lsp-get-extension-name layer-name "peek" kind)))))

(defun spacemacs/lsp-bind-extensions-for-mode (mode layer-name key kind &rest bindings)
  "Bind find/peek extensions under the appropriate prefix(es) for the major-mode
MODE. MODE should be a quoted symbol corresponding to a valid major mode.
LAYER-NAME and KEY should be quoted strings. KIND should be quoted symbol corresponding to
a find extension defined using `lsp-define-extensions'"
  (while key
    (spacemacs//lsp-bind-extensions mode layer-name key kind)
    (setq key (pop bindings) kind (pop bindings))))
