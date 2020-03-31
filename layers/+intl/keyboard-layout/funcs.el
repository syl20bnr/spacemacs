;;; funcs.el --- keyboard-layout Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Fabien Dubosson <fabien.dubosson@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Map multiple states at once. Courtesy of Michael Markert;
;; http://permalink.gmane.org/gmane.emacs.vim-emulation/1674

;;------------------------------------------------------------------------------
;; PRIVATE FUNCTIONS
;;------------------------------------------------------------------------------

(defun kl//generate-full-rebinding-map (basemap)
  "Generate the full rebinding map from a base map."
  (mapcan (lambda (binding)
            (let ((key1 (car binding))
                  (key2 (cdr binding)))
              (append
               (list  (cons (upcase key1) (upcase key2))
                      (cons key1 key2))
               (mapcar
                (lambda (modifier)
                  (cons (concat modifier key1) (concat modifier key2)))
                '("C-" "M-" "C-S-")))))
          basemap))

(defun kl//define-key (maps key def bindings)
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

(defun kl//remap-key-as (map bindings)
  "Define keys to the associated definitions of other ones. All
remapping are done atomically, i.e. if `a' -> `b' and `c' -> `a',
then `c' will be defined to the old `a' function, not to `b'."
  (if (keymapp map)
      (progn
        (declare (indent 1))
        (let ((map-original (copy-tree map)))
          (dolist (binding bindings)
            (let ((key1 (kbd (car binding)))
                  (key2 (kbd (cdr binding))))
              (define-key map key1 (lookup-key map-original key2))))))))

(defun kl//replace-in-list-rec (lst elem repl)
  "Replace recursively all occurrences of `elem' by `repl' in the
list `lst'."
  (declare (indent 0))
  (if (cl-typep lst 'list)
      (let* ((body-position (cl-position elem lst)))
        (if body-position
            ;; The element is in the list, replace it
            (progn
              (setf (nth body-position lst) repl)
              lst)
          ;; The element is not in the list, recurse
          (dolist (l lst)
            (kl//replace-in-list-rec l elem repl))))))

(defun kl//guess-rebindings (key)
  "Tries to guess the rebindings needed to correct the given
key."
  (let* ((key1 key)
         (prefix nil)
         (rebinding-map (cdr (assoc kl-layout kl--rebinding-maps))))
    ;; If key not existing as-is in the kl--rebinding-maps, try on last letter.
    (when (not (assoc key1 rebinding-map))
      (setq key1 (substring key -1))
      (setq prefix (substring key 0 -1)))
    (let* ((key2 (cdr (assoc key1 rebinding-map)))
           (bind1 (assoc key1 rebinding-map))
           (bind2 (assoc key2 rebinding-map)))
      (when (and prefix
                 (not (string-empty-p prefix)))
        (defun kl//guess-prefixit (bind)
          `(,(concat prefix (car bind)) . ,(concat prefix (cdr bind))))
        (setq bind1 (kl//guess-prefixit bind1))
        (setq bind2 (kl//guess-prefixit bind2)))
      `(,bind1 ,bind2))))

;;------------------------------------------------------------------------------
;; HELPER FUNCTIONS
;;------------------------------------------------------------------------------

(defun kl/set-in-state (map key def &rest bindings)
  "Define a list of keys with their associated functions in a
given state map."
  (declare (indent 1))
  (kl//define-key (list map) key def bindings))

(defun kl/set-in-states (maps key def &rest bindings)
  "Define a list of keys with their associated functions in all
given state maps."
  (declare (indent 1))
  (kl//define-key maps key def bindings))

(defun kl/set-in-all-evil-states (key def &rest bindings)
  "Define a list of keys with their associated functions in all
evil states."
  (declare (indent 0))
  (kl//define-key kl--all-evil-states key def bindings))

(defun kl/set-in-all-evil-states-but-insert (key def &rest bindings)
  "Define a list of keys with their associated functions in all
evil states, except insert."
  (declare (indent 0))
  (kl//define-key kl--all-evil-states-but-insert key def bindings))

(defun kl/leader-alias-of (key1 key2)
  "Define a leader key as an alias of another one."
  (spacemacs/set-leader-keys key1 (lookup-key spacemacs-default-map key2)))

(defun kl/leader-swap-keys (key1 key2)
  "Invert the behaviour of two leader keys."
  (let ((map1 (lookup-key spacemacs-default-map key1))
        (map2 (lookup-key spacemacs-default-map key2)))
    (spacemacs/set-leader-keys key1 map2 key2 map1)))

;;------------------------------------------------------------------------------
;; CORRECTION FUNCTIONS
;;------------------------------------------------------------------------------

(defun kl/correct-keys (map &rest keys)
  (declare (indent 1))
  (let ((bindings (mapcan #'kl//guess-rebindings keys)))
    (kl//remap-key-as map (cl-remove-if #'null bindings))))

(defun kl/evil-correct-keys (state map &rest keys)
  (declare (indent 2))
  (apply #'kl/correct-keys (evil-get-auxiliary-keymap map state) keys))

(defun kl/leader-correct-keys (&rest keys)
  (declare (indent 0))
  (apply #'kl/correct-keys spacemacs-default-map keys))

;;------------------------------------------------------------------------------
;; MAIN MACRO
;;------------------------------------------------------------------------------

(defmacro kl|config (name &rest props)
  "Macro used for structuring `keyboard-layout' configuration changes.

Usage:

    (kl|config configuration-name
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
`kl/pre-config-<name>' or `kl/post-config-<name>'
function (taking no argument). These functions will be called just
before or after the keyboard-layout's configurations."
  (declare (indent 1))
  (let* ((disable (plist-get props :disable))
         (description (plist-get props :description))
         (functions (plist-get props :functions))
         (loader (plist-get props :loader))
         (common (plist-get props :common))
         (specific (plist-get props (intern (format ":%s" kl-layout))))
         (special (plist-get props :special))
         (preconf (intern (format "kl/pre-config-%s" name)))
         (postconf (intern (format "kl/post-config-%s" name)))
         (body `(progn
                  (when (fboundp ',preconf) (funcall ',preconf))
                  ,common
                  ,specific
                  (when (fboundp ',postconf) (funcall ',postconf))
                  )))
    ;; Use loader if defined
    (when loader
      (kl//replace-in-list-rec loader 'BODY body)
      (setq body loader))
    ;; If the configuration is not disabled
    (when (not disable)
      ;; If the configuration is not in disabled-list
      (when (not (member name kl-disabled-configurations))
        ;; If the package is in enabled-list, if any.
        (when (or (not kl-enabled-configurations) (member name kl-enabled-configurations))
          (when init-file-debug
            (message (format "[kl] Configuration enabled: '%s'" name)))
          `(progn
             ,functions
             ,body
             ,special
             ,description
             ))))))
