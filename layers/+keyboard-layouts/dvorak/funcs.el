;;; funcs.el --- dvorak Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Alejandro Catalina <alecatfel@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Map multiple states at once. Courtesy of Michael Markert;
;; http://permalink.gmane.org/gmane.emacs.vim-emulation/1674

;; This functions have been taken from bepo layer, courtesy of
;; Fabien Dubosson <fabien.dubosson@gmail.com>>

;;------------------------------------------------------------------------------
;; FILE-PRIVATE FUNCTIONS
;;------------------------------------------------------------------------------

(defun dvorak//define-key (maps key def bindings)
  "Define a list of KEYS to their associated DEFINITIONS in all
the given MAPS."
  (declare (indent 1))
  (while key
    ;; Define the key
    (dolist (map maps)
      (define-key map (kbd key) def))
    ;; Get next keybinding
    (setq key (pop bindings)
          def (pop bindings))))

(defun dvorak//remap-key-as (map bindings)
  "Define keys to the associated definitions of other ones. All
remapping are done atomically, i.e. if `a' -> `b' and `c' -> `a',
then `c' will be defined to the old `a' function, not to `b'."
  (declare (indent 1))
  (let ((map-original (copy-tree map)))
    (dolist (binding bindings)
      (let ((key1 (kbd (car binding)))
            (key2 (kbd (cdr binding))))
        (define-key map key1 (lookup-key map-original key2))))))

(defun dvorak//replace-in-list-rec (lst elem repl)
  "Replace recursively all occurrences of `elem' by `repl' in the
list `lst'."
  (declare (indent 0))
  (if (typep lst 'list)
      (let* ((body-position (cl-position elem lst)))
        (if body-position
            ;; The element is in the list, replace it
            (progn
              (setf (nth body-position lst) repl)
              lst)
          ;; The element is not in the list, recurse
          (dolist (l lst)
            (dvorak//replace-in-list-rec l elem repl))))))

(defun dvorak//guess-rebindings (key)
  "Tries to guess the rebindings needed to correct the given
key."
  (let* ((key1 key)
         (prefix nil))
    ;; If key not existing as-is in the dvorak-rebinding-map, try on last letter.
    (when (not (assoc key1 dvorak--rebinding-map))
      (setq key1 (substring key -1))
      (setq prefix (substring key 0 -1)))
    (let* ((key2 (cdr (assoc key1 dvorak--rebinding-map)))
           (bind1 (assoc key1 dvorak--rebinding-map))
           (bind2 (assoc key2 dvorak--rebinding-map)))
      (when prefix
        (defun dvorak//guess-prefixit (bind)
          `(,(concat prefix (car bind)) . ,(concat prefix (cdr bind))))
        (setq bind1 (dvorak//guess-prefixit bind1))
        (setq bind2 (dvorak//guess-prefixit bind2)))
      `(,bind1 ,bind2))))

;;------------------------------------------------------------------------------
;; HELPER FUNCTIONS
;;------------------------------------------------------------------------------

(defun dvorak/set-in-state (map key def &rest bindings)
  "Define a list of keys with their associated functions in a
given state map."
  (declare (indent 1))
  (dvorak//define-key (list map) key def bindings))

(defun dvorak/set-in-states (maps key def &rest bindings)
  "Define a list of keys with their associated functions in all
given state maps."
  (declare (indent 1))
  (dvorak//define-key maps key def bindings))

(defun dvorak/set-in-all-evil-states (key def &rest bindings)
  "Define a list of keys with their associated functions in all
evil states."
  (declare (indent 0))
  (dvorak//define-key dvorak--all-evil-states key def bindings))

(defun dvorak/set-in-all-evil-states-but-insert (key def &rest bindings)
  "Define a list of keys with their associated functions in all
evil states, except insert."
  (declare (indent 0))
  (dvorak//define-key dvorak--all-evil-states-but-insert key def bindings))

(defun dvorak/leader-alias-of (key1 key2)
  "Define a leader key as an alias of another one."
  (spacemacs/set-leader-keys key1 (lookup-key spacemacs-default-map key2)))

(defun dvorak/leader-swap-keys (key1 key2)
  "Invert the behaviour of two leader keys."
  (let ((map1 (lookup-key spacemacs-default-map key1))
        (map2 (lookup-key spacemacs-default-map key2)))
    (spacemacs/set-leader-keys key1 map2 key2 map1)))

(defun dvorak/swap-keys (map key1 key2)
  "Invert the behaviour of two leader keys."
  (let ((map1 (lookup-key map key1))
        (map2 (lookup-key map key2)))
    (define-key map key1 map2)
    (define-key map key2 map1)))

;;------------------------------------------------------------------------------
;; CORRECTION FUNCTIONS
;;------------------------------------------------------------------------------

(defun dvorak/correct-keys (map &rest keys)
  (declare (indent 1))
  (let ((bindings (mapcan #'dvorak//guess-rebindings keys)))
    (dvorak//remap-key-as map (remove-if #'null bindings))))

(defun dvorak/evil-correct-keys (state map &rest keys)
  (declare (indent 2))
  (apply #'dvorak/correct-keys (evil-get-auxiliary-keymap map state) keys))

(defun dvorak/leader-correct-keys (&rest keys)
  (declare (indent 0))
  (apply #'dvorak/correct-keys spacemacs-default-map keys))

;;------------------------------------------------------------------------------
;; MAIN MACRO
;;------------------------------------------------------------------------------

(defmacro dvorak|config (name &rest props)
  "Macro used for structuring dvorak configuration changes.

Usage:

    (dvorak|config configuration-name
       [:keyword option]...)

:disable       Boolean, whether the configuration is disabled or not.
:description   String, documents what the configuration does.
:functions     Code, functions definitions.
:loader        Code, used to load the configuration. Must contains `BODY'
               where the real configuration must be placed.
:config        Code, the configuration code.
:special       Code executed as-is at the end, without being wrapped inside
               the `:loader'.

All keywords are optional, except for `:config'.

These configurations can be overridden by the user using a
`dvorak/pre-config-<name>' or `dvorak/post-config-<name>'
function (taking no argument). These functions will be called just
before or after the dvorak's configurations."
  (declare (indent 1))
  (let* ((disable (plist-get props :disable))
         (description (plist-get props :description))
         (functions (plist-get props :functions))
         (loader (plist-get props :loader))
         (config (plist-get props :config))
         (special (plist-get props :special))
         (preconf (intern (format "dvorak/pre-config-%s" name)))
         (postconf (intern (format "dvorak/post-config-%s" name)))
         (body `(progn
                  (when (fboundp ',preconf) (funcall ',preconf))
                  ,config
                  (when (fboundp ',postconf) (funcall ',postconf))
                  )))
    ;; Use loader if defined
    (when loader
      (dvorak//replace-in-list-rec loader 'BODY body)
      (setq body loader))
    ;; If the configuration is not disabled
    (when (not disable)
      ;; If the configuration is not in disabled-list
      (when (not (member name dvorak-set-disabled-configurations))
        ;; If the package is in enabled-list, if any.
        (when (or (not dvorak-set-enabled-configurations) (member name dvorak-set-enabled-configurations))
          (when dotspacemacs-verbose-loading
            (message (format "[dvorak] Configuration enabled: '%s'" name)))
          `(progn
             ,functions
             ,body
             ,special
             ,description
             ))))))
