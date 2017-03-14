;;; core-keybindings.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(require 'core-funcs)

(defvar spacemacs/prefix-titles nil
  "alist for mapping command prefixes to long names.")

(defvar spacemacs-default-map (make-sparse-keymap)
  "Base keymap for all spacemacs leader key commands.")

(defvar spacemacs--keybinding-mode-prefix-ht (make-hash-table :test 'equal)
  "hash table of prefixes declared with `spacemacs/declare-prefix-for-mode'.
KEY is string made by `spacemacs//make-keybinding-prefix-key'.
VALUE is a plist and has format:
(:prefix <prefix> :mode <mode> :name <name> :scope <scope> :file <file>)
prefix, name and mode are from `spacemacs/declare-prefix-for-mode'.
scope documented in `spacemacs--config-scope'.")

(defvar spacemacs--keybinding-global-from-mode-prefix-ht
  (make-hash-table :test 'equal)
  "hash table of prefixes declared with `spacemacs/declare-prefix-for-mode'.
KEY is PREFIX from `spacemacs/declare-prefix-for-mode'.
VALUE is a plist and has format:
(:prefix <prefix> :mode <mode> :name <name> :scope <scope> :file <file>)
scope documented in `spacemacs--config-scope'.
NOTE: this hash table used to detect collision between global and mode
dependent prefixes.")

(defvar spacemacs--keybinding-global-prefix-ht (make-hash-table :test 'equal)
  "Hash table where KEY is PREFIX from `spacemacs/declare-prefix'
VALUE is a plist and has format:
(:prefix <PREFIX> :mode nil :name <NAME> :scope <SCOPE> :file <FILE>)
PREFIX and NAME are from `spacemacs/declare-prefix'.
SCOPE documented in `spacemacs--config-scope'.")

(defvar spacemacs--keybinding-prefix-collision-list nil
  "List of collisions in prefixes declared with `spacemacs/declare-prefix'
and `spacemacs/declare-prefix-for-mode' functions.
The value is a plist and has format:
((:from <OLD_BINDING> :to <NEW_BINDING>) ...)
<OLD_BINDING> and <NEW_BINDING> are old and new values from
`spacemacs--keybinding-global-prefix-ht'.
NOTE: See `dotspecemacs-keybindings-collision-report' for collision
reporting levels.")

(defvar spacemacs--keybinding-sequences-collision-list nil
  "FIXME: NOT IMPLEMENTED")

(defvar spacemacs--report-keybindings-collisions-timer nil
  "Timer used in `spacemacs/report-keybindings-collisions'.")

(defun spacemacs//schedule-report-keybindings-collisions ()
  "Schedule `spacemacs/report-keybindings-collisions' run.
NOTE: We use `run-with-idle-timer' to be asynchronous and
less intrusive. Calls do not stack."
  (when spacemacs--report-keybindings-collisions-timer
    (cancel-timer spacemacs--report-keybindings-collisions-timer))
  (setq spacemacs--report-keybindings-collisions-timer
        (run-with-idle-timer 0 nil 'spacemacs/report-keybindings-collisions)))

(defun spacemacs//make-keybinding-prefix-key (prefix &optional mode)
  "Make key for the `spacemacs--keybinding-global-prefix-ht' hash table.)
PREFIX - prefix used in `spacemacs/declare-prefix-for-mode' or
`spacemacs/declare-prefix'.
MODE - major mode used in `spacemacs/declare-prefix-for-mode'."
  (format "%s:%s" mode prefix))

(defun spacemacs/translate-C-i (_)
  "If `dotspacemacs-distinguish-gui-tab' is non nil, the raw key
sequence does not include <tab> or <kp-tab>, and we are in the
gui, translate to [C-i]. Otherwise, [9] (TAB)."
  (interactive)
  (if (and (not (cl-position 'tab (this-single-command-raw-keys)))
           (not (cl-position 'kp-tab (this-single-command-raw-keys)))
           dotspacemacs-distinguish-gui-tab
           (display-graphic-p))
      [C-i] [?\C-i]))
(define-key key-translation-map [?\C-i] 'spacemacs/translate-C-i)

;; (defun spacemacs/translate-C-m (_)
;;   "If `dotspacemacs-distinguish-gui-ret' is non nil, the raw key
;; sequence does not include <ret>, and we are in the gui, translate
;; to [C-m]. Otherwise, [9] (TAB)."
;;   (interactive)
;;   (if (and
;;        (not (cl-position 'return (this-single-command-raw-keys)))
;;        (not (cl-position 'kp-enter (this-single-command-raw-keys)))
;;        dotspacemacs-distinguish-gui-ret
;;        (display-graphic-p))
;;     [C-m] [?\C-m]))
;; (define-key key-translation-map [?\C-m] 'spacemacs/translate-C-m)

(defun spacemacs/declare-prefix (prefix name &optional long-name redeclare)
  "Declare a prefix PREFIX. PREFIX is a string describing a key
sequence. NAME is a string used as the prefix command.
LONG-NAME if given is stored in `spacemacs/prefix-titles'.
REDECLARE is a string of NAME that should be replaced or a list of string.
NOTE: See `dotspecemacs-keybindings-collision-report' for collision
reporting levels."
  (let* ((command name)
         (full-prefix (concat dotspacemacs-leader-key " " prefix))
         (full-prefix-emacs (concat dotspacemacs-emacs-leader-key " " prefix))
         (full-prefix-lst (listify-key-sequence (kbd full-prefix)))
         (full-prefix-emacs-lst (listify-key-sequence
                                 (kbd full-prefix-emacs))))
    (unless long-name (setq long-name name))
    (let* ((old-binding
            (gethash prefix spacemacs--keybinding-global-prefix-ht))
           (old-binding-from-mode
            (gethash prefix spacemacs--keybinding-global-from-mode-prefix-ht))
           (new-binding
            (puthash prefix
                     `(:prefix ,prefix
                       :mode    nil
                       :name   ,long-name
                       :scope  ,spacemacs--config-scope
                       :file   ,load-file-name)
                     spacemacs--keybinding-global-prefix-ht))
           (check? (or (eq dotspecemacs-keybindings-collision-report 'all)
                       (and (eq dotspecemacs-keybindings-collision-report
                                'internal)
                            load-file-name
                            (string-prefix-p
                             (expand-file-name
                              spacemacs-start-directory)
                             (expand-file-name
                              (file-name-directory load-file-name)))))))
      (when check?
        (dolist (binding (list old-binding old-binding-from-mode))
          (when (and binding
                     new-binding
                     (not (let ((old-name (plist-get binding :name)))
                            (or (and (stringp redeclare)
                                     (string= old-name redeclare))
                                (and (listp redeclare)
                                     (member old-name redeclare))))))
            (push `(:from ,binding
                    :to   ,new-binding)
                  spacemacs--keybinding-prefix-collision-list)
            (spacemacs//schedule-report-keybindings-collisions)))))
    (which-key-add-key-based-replacements
      full-prefix-emacs (cons name long-name)
      full-prefix (cons name long-name))))
(put 'spacemacs/declare-prefix 'lisp-indent-function 'defun)

(defun spacemacs/declare-prefix-for-mode (mode prefix name &optional long-name redeclare)
  "Declare a prefix PREFIX. MODE is the mode in which this prefix command should
be added. PREFIX is a string describing a key sequence. NAME is a symbol name
used as the prefix command.
REDECLARE is a string of NAME that should be replaced or a list of string.
NOTE: See `dotspecemacs-keybindings-collision-report' for collision
reporting levels."
  (let  ((command (intern (concat (symbol-name mode) name)))
         (full-prefix (concat dotspacemacs-leader-key " " prefix))
         (full-prefix-emacs (concat dotspacemacs-emacs-leader-key " " prefix))
         (is-major-mode-prefix (string-prefix-p "m" prefix))
         (major-mode-prefix (concat dotspacemacs-major-mode-leader-key
                                    " " (substring prefix 1)))
         (major-mode-prefix-emacs
          (concat dotspacemacs-major-mode-emacs-leader-key
                  " " (substring prefix 1))))
    (unless long-name (setq long-name name))
    (let* ((kbpl-key (spacemacs//make-keybinding-prefix-key prefix mode))
           (old-binding (gethash kbpl-key
                                 spacemacs--keybinding-mode-prefix-ht))
           (old-global-binding (gethash prefix
                                spacemacs--keybinding-global-prefix-ht))
           (new-binding
            (puthash kbpl-key
                     `(:prefix ,prefix
                       :mode   ,mode
                       :name   ,long-name
                       :scope  ,spacemacs--config-scope
                       :file   ,load-file-name)
                     spacemacs--keybinding-mode-prefix-ht))
           (check? (or (eq dotspecemacs-keybindings-collision-report 'all)
                       (and (eq dotspecemacs-keybindings-collision-report
                                'internal)
                            load-file-name
                            (string-prefix-p
                             (expand-file-name
                              spacemacs-start-directory)
                             (expand-file-name
                              (file-name-directory load-file-name)))))))

      (puthash prefix
               new-binding
               spacemacs--keybinding-global-from-mode-prefix-ht)
      (when check?
        (dolist (binding (list old-binding old-global-binding))
          (when (and binding
                     new-binding
                     (not (let ((old-name (plist-get binding :name)))
                            (or (and (stringp redeclare)
                                     (string= old-name redeclare))
                                (and (listp redeclare)
                                     (member old-name redeclare))))))
            (push `(:from ,binding
                    :to   ,new-binding)
                  spacemacs--keybinding-prefix-collision-list)
            (spacemacs//schedule-report-keybindings-collisions)))))
    (let ((prefix-name (cons name long-name)))
      (which-key-add-major-mode-key-based-replacements mode
        full-prefix-emacs prefix-name
        full-prefix prefix-name)
      (when (and is-major-mode-prefix dotspacemacs-major-mode-leader-key)
        (which-key-add-major-mode-key-based-replacements mode major-mode-prefix prefix-name))
      (when (and is-major-mode-prefix dotspacemacs-major-mode-emacs-leader-key)
        (which-key-add-major-mode-key-based-replacements
          mode major-mode-prefix-emacs prefix-name)))))
(put 'spacemacs/declare-prefix-for-mode 'lisp-indent-function 'defun)

(defun spacemacs/set-leader-keys (key def &rest bindings)
  "Add KEY and DEF as key bindings under
`dotspacemacs-leader-key' and `dotspacemacs-emacs-leader-key'.
KEY should be a string suitable for passing to `kbd', and it
should not include the leaders. DEF is most likely a quoted
command. See `define-key' for more information about the possible
choices for DEF. This function simply uses `define-key' to add
the bindings.

For convenience, this function will accept additional KEY DEF
pairs. For example,

\(spacemacs/set-leader-keys
   \"a\" 'command1
   \"C-c\" 'command2
   \"bb\" 'command3\)"
  (while key
    (define-key spacemacs-default-map (kbd key) def)
    (setq key (pop bindings) def (pop bindings))))
(put 'spacemacs/set-leader-keys 'lisp-indent-function 'defun)

(defalias 'evil-leader/set-key 'spacemacs/set-leader-keys)

(defun spacemacs//acceptable-leader-p (key)
  "Return t if key is a string and non-empty."
  (and (stringp key) (not (string= key ""))))

(defun spacemacs//init-leader-mode-map (mode map &optional minor)
  "Check for MAP-prefix. If it doesn't exist yet, use `bind-map'
to create it and bind it to `dotspacemacs-major-mode-leader-key'
and `dotspacemacs-major-mode-emacs-leader-key'. If MODE is a
minor-mode, the third argument should be non nil."
  (let* ((prefix (intern (format "%s-prefix" map)))
         (leader1 (when (spacemacs//acceptable-leader-p
                         dotspacemacs-major-mode-leader-key)
                    dotspacemacs-major-mode-leader-key))
         (leader2 (when (spacemacs//acceptable-leader-p
                         dotspacemacs-leader-key)
                    (concat dotspacemacs-leader-key " m")))
         (emacs-leader1 (when (spacemacs//acceptable-leader-p
                               dotspacemacs-major-mode-emacs-leader-key)
                          dotspacemacs-major-mode-emacs-leader-key))
         (emacs-leader2 (when (spacemacs//acceptable-leader-p
                               dotspacemacs-emacs-leader-key)
                          (concat dotspacemacs-emacs-leader-key " m")))
         (leaders (delq nil (list leader1 leader2)))
         (emacs-leaders (delq nil (list emacs-leader1 emacs-leader2))))
    (or (boundp prefix)
        (progn
          (eval
           `(bind-map ,map
              :prefix-cmd ,prefix
              ,(if minor :minor-modes :major-modes) (,mode)
              :keys ,emacs-leaders
              :evil-keys ,leaders
              :evil-states (normal motion visual evilified)))
          (boundp prefix)))))

(defun spacemacs/set-leader-keys-for-major-mode (mode key def &rest bindings)
  "Add KEY and DEF as key bindings under
`dotspacemacs-major-mode-leader-key' and
`dotspacemacs-major-mode-emacs-leader-key' for the major-mode
MODE. MODE should be a quoted symbol corresponding to a valid
major mode. The rest of the arguments are treated exactly like
they are in `spacemacs/set-leader-keys'."
  (let* ((map (intern (format "spacemacs-%s-map" mode))))
    (when (spacemacs//init-leader-mode-map mode map)
      (while key
        (define-key (symbol-value map) (kbd key) def)
        (setq key (pop bindings) def (pop bindings))))))
(put 'spacemacs/set-leader-keys-for-major-mode 'lisp-indent-function 'defun)

(defalias
  'evil-leader/set-key-for-mode
  'spacemacs/set-leader-keys-for-major-mode)

(defun spacemacs/set-leader-keys-for-minor-mode (mode key def &rest bindings)
  "Add KEY and DEF as key bindings under
`dotspacemacs-major-mode-leader-key' and
`dotspacemacs-major-mode-emacs-leader-key' for the minor-mode
MODE. MODE should be a quoted symbol corresponding to a valid
minor mode. The rest of the arguments are treated exactly like
they are in `spacemacs/set-leader-keys'."
  (let* ((map (intern (format "spacemacs-%s-map" mode))))
    (when (spacemacs//init-leader-mode-map mode map t)
      (while key
        (define-key (symbol-value map) (kbd key) def)
        (setq key (pop bindings) def (pop bindings))))))
(put 'spacemacs/set-leader-keys-for-minor-mode 'lisp-indent-function 'defun)

(provide 'core-keybindings)
