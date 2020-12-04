;;; core-keybindings.el --- Space-macs Core File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(require 'core-funcs)

(defvar space-macs/prefix-titles nil
  "alist for mapping command prefixes to long names.")

(defvar space-macs-default-map (make-sparse-keymap)
  "Base keymap for all space-macs leader key commands.")

(defun space-macs/translate-C-i (_)
  "If `dotspace-macs-distinguish-gui-tab' is non nil, the raw key
sequence does not include <tab> or <kp-tab>, and we are in the
gui, translate to [C-i]. Otherwise, [9] (TAB)."
  (interactive)
  (if (and (not (cl-position 'tab (this-single-command-raw-keys)))
           (not (cl-position 'kp-tab (this-single-command-raw-keys)))
           dotspace-macs-distinguish-gui-tab
           (display-graphic-p))
      [C-i] [?\C-i]))
(define-key key-translation-map [?\C-i] 'space-macs/translate-C-i)

;; (defun space-macs/translate-C-m (_)
;;   "If `dotspace-macs-distinguish-gui-ret' is non nil, the raw key
;; sequence does not include <ret>, and we are in the gui, translate
;; to [C-m]. Otherwise, [9] (TAB)."
;;   (interactive)
;;   (if (and
;;        (not (cl-position 'return (this-single-command-raw-keys)))
;;        (not (cl-position 'kp-enter (this-single-command-raw-keys)))
;;        dotspace-macs-distinguish-gui-ret
;;        (display-graphic-p))
;;     [C-m] [?\C-m]))
;; (define-key key-translation-map [?\C-m] 'space-macs/translate-C-m)

(defun space-macs/declare-prefix (prefix name &optional long-name)
  "Declare a prefix PREFIX. PREFIX is a string describing a key
sequence. NAME is a string used as the prefix command.
LONG-NAME if given is stored in `space-macs/prefix-titles'."
  (let* ((command name)
         (full-prefix (concat dotspace-macs-leader-key " " prefix))
         (full-prefix-e-macs (concat dotspace-macs-e-macs-leader-key " " prefix))
         (full-prefix-lst (listify-key-sequence (kbd full-prefix)))
         (full-prefix-e-macs-lst (listify-key-sequence
                                 (kbd full-prefix-e-macs))))
    ;; define the prefix command only if it does not already exist
    (unless long-name (setq long-name name))
    (which-key-add-key-based-replacements
      full-prefix-e-macs (cons name long-name)
      full-prefix (cons name long-name))))
(put 'space-macs/declare-prefix 'lisp-indent-function 'defun)

(defun space-macs/declare-prefix-for-mode (mode prefix name &optional long-name)
  "Declare a prefix PREFIX. MODE is the mode in which this prefix command should
be added. PREFIX is a string describing a key sequence. NAME is a symbol name
used as the prefix command."
  (let  ((command (intern (concat (symbol-name mode) name)))
         (full-prefix (concat dotspace-macs-leader-key " " prefix))
         (full-prefix-e-macs (concat dotspace-macs-e-macs-leader-key " " prefix))
         (is-major-mode-prefix (string-prefix-p "m" prefix))
         (major-mode-prefix (concat dotspace-macs-major-mode-leader-key
                                    " " (substring prefix 1)))
         (major-mode-prefix-e-macs
          (concat dotspace-macs-major-mode-e-macs-leader-key
                  " " (substring prefix 1))))
    (unless long-name (setq long-name name))
    (let ((prefix-name (cons name long-name)))
      (which-key-add-major-mode-key-based-replacements mode
        full-prefix-e-macs prefix-name
        full-prefix prefix-name)
      (when (and is-major-mode-prefix dotspace-macs-major-mode-leader-key)
        (which-key-add-major-mode-key-based-replacements mode major-mode-prefix prefix-name))
      (when (and is-major-mode-prefix dotspace-macs-major-mode-e-macs-leader-key)
        (which-key-add-major-mode-key-based-replacements
          mode major-mode-prefix-e-macs prefix-name)))))
(put 'space-macs/declare-prefix-for-mode 'lisp-indent-function 'defun)

(defun space-macs/set-leader-keys (key def &rest bindings)
  "Add KEY and DEF as key bindings under
`dotspace-macs-leader-key' and `dotspace-macs-e-macs-leader-key'.
KEY should be a string suitable for passing to `kbd', and it
should not include the leaders. DEF is most likely a quoted
command. See `define-key' for more information about the possible
choices for DEF. This function simply uses `define-key' to add
the bindings.

For convenience, this function will accept additional KEY DEF
pairs. For example,

\(space-macs/set-leader-keys
   \"a\" 'command1
   \"C-c\" 'command2
   \"bb\" 'command3\)"
  (while key
    (define-key space-macs-default-map (kbd key) def)
    (setq key (pop bindings) def (pop bindings))))
(put 'space-macs/set-leader-keys 'lisp-indent-function 'defun)

(defalias 'evil-leader/set-key 'space-macs/set-leader-keys)

(defun space-macs//acceptable-leader-p (key)
  "Return t if key is a string and non-empty."
  (and (stringp key) (not (string= key ""))))

(defun space-macs//init-leader-mode-map (mode map &optional minor)
  "Check for MAP-prefix. If it doesn't exist yet, use `bind-map'
to create it and bind it to `dotspace-macs-major-mode-leader-key'
and `dotspace-macs-major-mode-e-macs-leader-key'. If MODE is a
minor-mode, the third argument should be non nil."
  (let* ((prefix (intern (format "%s-prefix" map)))
         (leader1 (when (space-macs//acceptable-leader-p
                         dotspace-macs-major-mode-leader-key)
                    dotspace-macs-major-mode-leader-key))
         (leader2 (when (space-macs//acceptable-leader-p
                         dotspace-macs-leader-key)
                    (concat dotspace-macs-leader-key " m")))
         (e-macs-leader1 (when (space-macs//acceptable-leader-p
                               dotspace-macs-major-mode-e-macs-leader-key)
                          dotspace-macs-major-mode-e-macs-leader-key))
         (e-macs-leader2 (when (space-macs//acceptable-leader-p
                               dotspace-macs-e-macs-leader-key)
                          (concat dotspace-macs-e-macs-leader-key " m")))
         (leaders (delq nil (list leader1 leader2)))
         (e-macs-leaders (delq nil (list e-macs-leader1 e-macs-leader2))))
    (or (boundp prefix)
        (progn
          (eval
           `(bind-map ,map
              :prefix-cmd ,prefix
              ,(if minor :minor-modes :major-modes) (,mode)
              :keys ,e-macs-leaders
              :evil-keys ,leaders
              :evil-states (normal motion visual evilified)))
          (boundp prefix)))))

(defun space-macs/set-leader-keys-for-major-mode (mode key def &rest bindings)
  "Add KEY and DEF as key bindings under
`dotspace-macs-major-mode-leader-key' and
`dotspace-macs-major-mode-e-macs-leader-key' for the major-mode
MODE. MODE should be a quoted symbol corresponding to a valid
major mode. The rest of the arguments are treated exactly like
they are in `space-macs/set-leader-keys'."
  (let* ((map (intern (format "space-macs-%s-map" mode))))
    (when (space-macs//init-leader-mode-map mode map)
      (while key
        (define-key (symbol-value map) (kbd key) def)
        (setq key (pop bindings) def (pop bindings))))))
(put 'space-macs/set-leader-keys-for-major-mode 'lisp-indent-function 'defun)

(defalias
  'evil-leader/set-key-for-mode
  'space-macs/set-leader-keys-for-major-mode)

(defun space-macs/set-leader-keys-for-minor-mode (mode key def &rest bindings)
  "Add KEY and DEF as key bindings under
`dotspace-macs-major-mode-leader-key' and
`dotspace-macs-major-mode-e-macs-leader-key' for the minor-mode
MODE. MODE should be a quoted symbol corresponding to a valid
minor mode. The rest of the arguments are treated exactly like
they are in `space-macs/set-leader-keys'. If DEF is string, then
it is treated as a prefix not a command."
  (let* ((map (intern (format "space-macs-%s-map" mode))))
    (when (space-macs//init-leader-mode-map mode map t)
      (let ((map-value (symbol-value map)))
        (while key
          (if (stringp def)
              (which-key-add-keymap-based-replacements map-value key def)
            (define-key map-value (kbd key) def))
          (setq key (pop bindings) def (pop bindings)))))))
(put 'space-macs/set-leader-keys-for-minor-mode 'lisp-indent-function 'defun)

(defun space-macs/declare-prefix-for-minor-mode (mode prefix name)
  "Declare a prefix PREFIX. MODE is the mode in which this prefix command should
be added. PREFIX is a string describing a key sequence. NAME is a symbol name
used as the prefix command.

Example:
  \(space-macs/declare-prefix-for-minor-mode 'tide-mode \"E\" \"errors\"\)"

  (let* ((map (intern (format "space-macs-%s-map" mode))))
    (when (space-macs//init-leader-mode-map mode map t)
      (which-key-add-keymap-based-replacements (symbol-value map) prefix name))))

(provide 'core-keybindings)


