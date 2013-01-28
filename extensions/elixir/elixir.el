;; Keywords ===================================================================
(defvar elixir-keywords
  '("defmodule" "defmacro" "def" "function" "fn" "do" "end")
  "Elixir keywords.")

;; (defvar elixir-types
;;   '("float" "integer" "key" "list" "rotation" "string" "vector")
;;   "LSL types.")

;; (defvar elixir-constants
;;   '("ACTIVE" "AGENT" "ALL_SIDES" "ATTACH_BACK")
;;   "LSL constants.")

;; (defvar elixir-events
;;   '("at_rot_target" "at_target" "attach")
;;   "LSL events.")

;; (defvar elixir-functions
;;   '("llAbs" "llAcos" "llAddToLandBanList" "llAddToLandPassList")
;;   "LSL functions.")

;; Regular expressions ========================================================
(defvar elixir-keywords-regexp (regexp-opt elixir-keywords 'words))
;;(defvar elixir-type-regexp (regexp-opt elixir-types 'words))
;;(defvar elixir-constant-regexp (regexp-opt elixir-constants 'words))
;;(defvar elixir-event-regexp (regexp-opt elixir-events 'words))
;;(defvar elixir-functions-regexp (regexp-opt elixir-functions 'words))

;; font-lock lists ============================================================
(setq elixir-font-lock-keywords
  `(
    ;; (,elixir-type-regexp . font-lock-type-face)
    ;; (,elixir-constant-regexp . font-lock-constant-face)
    ;; (,elixir-event-regexp . font-lock-builtin-face)
    ;; (,elixir-functions-regexp . font-lock-function-name-face)
    (,elixir-keywords-regexp . font-lock-keyword-face)
))

;; define the mode ============================================================
(define-derived-mode elixir-mode fundamental-mode "Elixir"
  "Major mode for editing Elixir files."
  (setq font-lock-defaults '((elixir-font-lock-keywords)))
  ;; clear memory
  (setq elixir-keywords-regexp nil)
;;  (setq elixir-types-regexp nil)
;;  (setq elixir-constants-regexp nil)
;;  (setq elixir-events-regexp nil)
;;  (setq elixir-functions-regexp nil)
)

(let ((a '("\\.ex\\'" . elixir-mode))
      (b '("\\.exs\\'" . elixir-mode)))
  (or (assoc (car a) auto-mode-alist)
      (setq auto-mode-alist (cons a auto-mode-alist)))
  (or (assoc (car b) auto-mode-alist)
      (setq auto-mode-alist (cons b auto-mode-alist))))

(provide 'elixir)
