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
                 '(lsp-ui-peek-find-definitions))))

(defun fix-lsp-company-prefix ()
  "fix lsp-javascript company prefix
https://github.com/emacs-lsp/lsp-javascript/issues/9#issuecomment-379515379"
  (interactive)
  (defun lsp-prefix-company-transformer (candidates)
    (let ((completion-ignore-case t))
      (all-completions (company-grab-symbol) candidates)))
  (make-local-variable 'company-transformers)
  (add-to-list 'company-transformers 'lsp-prefix-company-transformer))

(defun spacemacs/lsp-bind-keys-for-mode (mode)
  "Define key bindings for the specific MODE."
  (spacemacs/declare-prefix-for-mode mode "m=" "format")
  (spacemacs/declare-prefix-for-mode mode "me" "execute")
  (spacemacs/declare-prefix-for-mode mode "mg" "goto")
  (spacemacs/declare-prefix-for-mode mode "mh" "hierarchy")
  (spacemacs/declare-prefix-for-mode mode "mj" "jump")
  (spacemacs/declare-prefix-for-mode mode "ml" "lsp")
  (spacemacs/declare-prefix-for-mode mode "mr" "refactor")
  (spacemacs/declare-prefix-for-mode mode "mT" "toggle")

  (spacemacs/set-leader-keys-for-major-mode mode
    ;;format
    "=b" #'lsp-format-buffer
    ;;execute
    "ea" #'lsp-execute-code-action
    ;;goto
    "gi" #'lsp-goto-implementation
    "gt" #'lsp-goto-type-definition
    ;;hierarchy
    "hh" #'lsp-describe-thing-at-point
    ;;jump
    "ja" #'spacemacs/lsp-avy-document-symbol
    "ji" #'lsp-ui-imenu
    "jd" #'lsp-ui-peek-find-definitions
    "jr" #'lsp-ui-peek-find-references
    "js" #'lsp-ui-peek-find-workspace-symbol
    "jb" #'lsp-ui-peek-jump-backward
    "jf" #'lsp-ui-peek-jump-forward
    "jf" #'lsp-ui-flycheck-list
    ;;lsp
    "lr" #'lsp-restart-workspace
    ;;refactor
    "rr" #'lsp-rename
    ;;toggles
    "Td" #'lsp-ui-doc-mode
    "Ts" #'lsp-ui-sideline-mode
    "TF" #'spacemacs/lsp-ui-doc-func
    "TS" #'spacemacs/lsp-ui-sideline-symb
    "TI" #'spacemacs/lsp-ui-sideline-ignore-duplicate
    )

  )


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
