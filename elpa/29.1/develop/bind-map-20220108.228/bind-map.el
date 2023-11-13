;;; bind-map.el --- Bind personal keymaps in multiple locations -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Justin Burkett

;; Author: Justin Burkett <justin@burkett.cc>
;; URL: https://github.com/justbur/emacs-bind-map
;; Version: 1.1.2
;; Keywords:
;; Package-Requires: ((emacs "24.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; bind-map is an Emacs package providing the macro bind-map which can be used
;; to make a keymap available across different "leader keys" including ones tied
;; to evil states. It is essentially a generalization of the idea of a leader
;; key as used in vim or the Emacs https://github.com/cofi/evil-leader package,
;; and allows for an arbitrary number of "leader keys". This is probably best
;; explained with an example.

;; (bind-map my-base-leader-map
;;   :keys ("M-m")
;;   :evil-keys ("SPC")
;;   :evil-states (normal motion visual))

;; (bind-map my-elisp-map
;;   :keys ("M-m m" "M-RET")
;;   :evil-keys ("SPC m" ",")
;;   :major-modes (emacs-lisp-mode
;;                 lisp-interaction-mode))

;; This will make my-base-leader-map (automatically creating the map if it's not
;; defined yet) available under the prefixes (or leaders) M-m and SPC, where the
;; latter is only bound in evil's normal, motion or visual states. The second
;; declaration makes my-elisp-map available under the specified keys when one of
;; the specified major modes is active. In the second case, the evil states used
;; are also normal motion and visual because this is the default as specified in
;; bind-map-default-evil-states. It is possible to make the bindings conditional
;; on minor modes being loaded, or a mix of major and minor modes. Since the
;; symbols of the modes are used, it is not necessary to ensure that any of the
;; mode's packages are loaded prior to this declaration. See the docstring of
;; bind-map for more options.

;; This package will only make use of evil if one of the evil related keywords
;; is specified. This declaration, for example, makes no use of the evil
;; package.

;; (bind-map my-elisp-map
;;   :keys ("M-m m" "M-RET")
;;   :major-modes (emacs-lisp-mode
;;                 lisp-interaction-mode))

;; The idea behind this package is that you want to organize your personal
;; bindings in a series of keymaps separate from built-in mode maps. You can
;; simply add keys using the built-in define-key to my-elisp-map for example,
;; and a declaration like the one above will take care of ensuring that these
;; bindings are available in the correct places.

;; Binding keys in the maps

;; You may use the built-in define-key which will function as intended. bind-key
;; (part of https://github.com/jwiegley/use-package) is another option. For
;; those who want a different interface, the following functions are also
;; provided, which both just use define-key internally, but allow for multiple
;; bindings without much syntax.

;;   (bind-map-set-keys my-base-leader-map
;;     "c" 'compile
;;     "C" 'check
;;     ;; ...
;;     )
;;   ;; is the same as
;;   ;; (define-key my-base-leader-map (kbd "c") 'compile)
;;   ;; (define-key my-base-leader-map (kbd "C") 'check)
;;   ;; ...

;;   (bind-map-set-key-defaults my-base-leader-map
;;     "c" 'compile
;;     ;; ...
;;     )
;;   ;; is the same as
;;   ;; (unless (lookup-key my-base-leader-map (kbd "c"))
;;   ;;   (define-key my-base-leader-map (kbd "c") 'compile))
;;   ;; ...

;; The second function only adds the bindings if there is no existing binding
;; for that key. It is probably only useful for shared configurations, where you
;; want to provide a default binding but don't want that binding to overwrite
;; one made by the user. Note the keys in both functions are strings that are
;; passed to kbd before binding them.

;;; Code:

(defgroup bind-map nil
  "Bind personal keymaps in multiple locations."
  :group 'emacs)

(defcustom bind-map-default-keys nil
  "Default for :keys when unspecified."
  :group 'bind-map
  :type  '(repeat string))

(defcustom bind-map-default-evil-states '(normal motion visual)
  "Default states for evil bindings."
  :group 'bind-map
  :type  '(repeat symbol))

(defcustom bind-map-default-evil-keys nil
  "Default for :evil-keys when unspecified."
  :group 'bind-map
  :type  '(repeat string))

(defcustom bind-map-default-map-suffix "-bm-map"
  "Default suffix to use for `bind-map-for-major-mode' and
`bind-map-for-minor-mode'."
  :group 'bind-map
  :type  'string)

(defvar bind-map-evil-local-bindings '()
  "Each element takes the form (OVERRIDE-MODE STATE KEY DEF) and
corresponds to a binding for an evil local state map.
OVERRIDE-MODE is the minor mode that must be enabled for these to
be activated.")
(defvaralias 'bind-map-local-bindings 'bind-map-evil-local-bindings)
(make-obsolete-variable 'bind-map-local-bindings
                        'bind-map-evil-local-bindings "2015-12-2")

(defun bind-map-put-map-properties (map-sym &rest properties)
  "Use put to add symbol properties to MAP-SYM."
  (declare (indent 1))
  (while properties
    (put map-sym (pop properties)
         (when properties (pop properties)))))

(defun bind-map-evil-local-mode-hook ()
  "Called to activate local state maps in a buffer."
  ;; format is (OVERRIDE-MODE STATE KEY DEF)
  (dolist (entry bind-map-evil-local-bindings)
    (let* ((map (intern (format "evil-%s-state-local-map" (nth 1 entry))))
           (mode (nth 0 entry))
           (global-mode (intern (format "global-%s" (nth 0 entry))))
           (set-explicitly (intern (format "%s-set-explicitly" mode))))
      (when (and (boundp global-mode) (boundp mode)
                 (boundp set-explicitly) (boundp map)
                 (keymapp (symbol-value map))
                 (symbol-value global-mode)
                 (not (and (symbol-value set-explicitly)
                           (null (symbol-value mode)))))
        (define-key (symbol-value map) (nth 2 entry) (nth 3 entry))))))
(add-hook 'evil-local-mode-hook 'bind-map-evil-local-mode-hook)

(defvar bind-map-major-modes-alist '()
  "Each element takes the form (MAP-ACTIVE (MAJOR-MODE1
MAJOR-MODE2 ...)). The car is the variable used to activate a map
when the major mode is an element of the cdr. See
`bind-map-change-major-mode-after-body-hook'.")

(defun bind-map-change-major-mode-after-body-hook ()
  "Called to activate major mode maps in a buffer."
  ;; format is (ACTIVATE-VAR MAJOR-MODES-LIST)
  (dolist (entry bind-map-major-modes-alist)
    (if (boundp (car entry))
      (setf (symbol-value (car entry))
            (not (null (member major-mode (cdr entry)))))
      (message "bind-map: %s is void in change major mode hook" (car entry)))))
(add-hook 'change-major-mode-after-body-hook
          'bind-map-change-major-mode-after-body-hook)

(defun bind-map-add-to-major-mode-list (activate-var major-mode-list)
  "Add (ACTIVATE-VAR . MAJOR-MODE-LIST) to
`bind-map-major-modes-alist'. If ACTIVATE-VAR is already a key,
then append MAJOR-MODE-LIST to the existing cdr."
  (let ((current (assq activate-var bind-map-major-modes-alist)))
    (if current
        (setcdr current (append (cdr current)
                                major-mode-list))
      (push (cons activate-var major-mode-list)
            bind-map-major-modes-alist))))

(defun bind-map-kbd-keys (keys)
  "Apply `kbd' to KEYS filtering out nil and empty strings."
  (let (res)
    (dolist (key keys (nreverse res))
      (when (and (stringp key)
                 (not (string= "" key)))
        (push (kbd key) res)))))

;;;###autoload
(defmacro bind-map (map &rest args)
  "Bind keymap MAP in multiple locations.
If MAP is not defined, this will create a new sparse keymap with
the name MAP. Supports binding in evil states and conditioning
the bindings on major and/or minor modes being active. The
options are controlled through the keyword arguments ARGS, all of
which are optional.

Keys for evil-mode are bound using `eval-after-load', so they
will only take effect after evil is loaded.

:keys \(KEY1 KEY2 ...\)

The keys to use for the leader binding. These are strings
suitable for use in `kbd'.

:override-minor-modes BOOL

If non nil, make keys in :keys override the minor-mode maps, by
using `emulation-mode-map-alists' instead of the `global-map'.
This is done for the :evil-keys using evil local state maps. If
either :major-modes or :minor-modes is specified, this setting
has no effect.

The overriding behavior can be toggled using the minor mode
MAP-overriding-mode (the name of the minor mode can be customized
in the next keyword). It is enabled by default when you specify
this keyword.

:override-mode-name SYMBOL

The name to use for the minor mode described for the previous
keyword (a default name will be given if this is left
unspecificied). This setting as no effect
if :override-minor-modes is nil or unspecified.

:evil-keys \(KEY1 KEY2 ...\)

Like :keys but these bindings are only active in certain evil
states.

:evil-states \(STATE1 STATE2 ...\)

Symbols representing the states to use for :evil-keys. If nil,
use `bind-map-default-evil-states'.

:evil-use-local BOOL

\(Deprecated\) This is now equivalent to setting
`:override-minor-modes' to t, which handles evil and non-evil
keys now.

:major-modes \(MODE1 MODE2 ...\)

If specified, the keys will only be bound when these major modes
are active. If both :major-modes and :minor-modes are nil or
unspecified the bindings are global.

:minor-modes \(MODE1 MODE2 ...\)

If specified, the keys will only be bound when these minor modes
are active. If both :major-modes and :minor-modes are nil or
unspecified the bindings are global.

:prefix-cmd COMMAND-NAME

Declare a prefix command for MAP named COMMAND-NAME.

:bindings \(KEY1 BINDING1 KEY2 BINDING2 ...\)

Bind keys when declaring the map. This is optional, but added as
a convenience."
  (let* ((root-map (intern (format "%s-root-map" map)))
         (active-var (intern (format "%s-active" map)))
         (prefix-cmd (or (plist-get args :prefix-cmd)
                         (intern (format "%s-prefix" map))))
         (keys (or (plist-get args :keys)
                   bind-map-default-keys))
         (override-minor-modes (or (plist-get args :override-minor-modes)
                                   (plist-get args :evil-use-local)))
         (override-mode (if (plist-get args :override-mode-name)
                            (plist-get args :override-mode-name)
                          (intern (format "%s-override-mode" map))))
         (override-mode-doc (format "Minor mode that makes %s override minor \
mode maps. Set up by bind-map.el." map))
         (global-override-mode (intern (format "global-%s" override-mode)))
         (turn-on-override-mode (intern (format "turn-on-%s" override-mode)))
         (turn-on-override-mode-doc (format "Enable `%s' except in minibuffer"
                                            override-mode))
         (evil-keys (or (plist-get args :evil-keys)
                        bind-map-default-evil-keys))
         (evil-states (or (plist-get args :evil-states)
                          bind-map-default-evil-states))
         (minor-modes (plist-get args :minor-modes))
         (major-modes (plist-get args :major-modes))
         (bindings (plist-get args :bindings)))
    (append
     '(progn)

     `((defvar ,map (make-sparse-keymap))
       (unless (keymapp ,map)
         (error "bind-map: %s is not a keymap" ',map))
       (defvar ,prefix-cmd nil)
       (setq ,prefix-cmd ,map)
       (setf (symbol-function ',prefix-cmd) ,map)
       (defvar ,root-map (make-sparse-keymap))
       (bind-map-put-map-properties ',map
                                    :root-map ',root-map
                                    :active-var ',active-var
                                    :prefix-cmd ',prefix-cmd
                                    :override-minor-modes ',override-minor-modes
                                    :override-mode-name ',override-mode
                                    :keys ',keys
                                    :evil-keys ',evil-keys
                                    :evil-states ',evil-states
                                    :minor-modes ',minor-modes
                                    :major-modes ',major-modes))

     (when minor-modes
       `((dolist (mode ',minor-modes)
           (push (cons mode ,root-map) minor-mode-map-alist))))

     (when major-modes
       ;; compiler warns about making a local var below the top-level
       `((with-no-warnings (defvar-local ,active-var nil))
         (add-to-list 'minor-mode-map-alist (cons ',active-var ,root-map))
         (bind-map-add-to-major-mode-list ',active-var ',major-modes)
         ;; call once in case we are already in the relevant major mode
         (bind-map-change-major-mode-after-body-hook)))

     (when (and override-minor-modes
                (null major-modes)
                (null minor-modes))
       `((with-no-warnings
           (defun ,turn-on-override-mode ()
             ,turn-on-override-mode-doc
             (unless (minibufferp) (,override-mode 1)))
           (define-globalized-minor-mode ,global-override-mode
             ,override-mode ,turn-on-override-mode)
           (define-minor-mode ,override-mode
             ,override-mode-doc)
           (,global-override-mode 1))
         (add-to-list 'emulation-mode-map-alists
                      (list (cons ',override-mode ,root-map)))))

     (if (or minor-modes major-modes)
         ;; only bind keys in root-map
         `((dolist (key (bind-map-kbd-keys (list ,@keys)))
             (define-key ,root-map key ',prefix-cmd)))
       ;; bind in global maps and possibly root-map
       `((dolist (key (bind-map-kbd-keys (list ,@keys)))
           (when ,override-minor-modes
             (define-key ,root-map key ',prefix-cmd))
           (global-set-key key ',prefix-cmd))))

     (when evil-keys
       (if (or minor-modes major-modes)
	   `((eval-after-load 'evil
	       '(progn
		  (dolist (key (bind-map-kbd-keys (list ,@evil-keys)))
		    (dolist (state ',evil-states)
		      (when ',major-modes
			(define-key
			  (evil-get-auxiliary-keymap ,root-map state t)
			  key ',prefix-cmd))
		      (dolist (mode ',minor-modes)
			(when (fboundp 'evil-define-minor-mode-key)
			  (evil-define-minor-mode-key
			   state mode key ',prefix-cmd)))))
		  (evil-normalize-keymaps))))
	 `((eval-after-load 'evil
	     '(progn
		(dolist (key (bind-map-kbd-keys (list ,@evil-keys)))
		  (dolist (state ',evil-states)
		    (when ,override-minor-modes
		      (push (list ',override-mode state key ',prefix-cmd)
			    bind-map-evil-local-bindings))
		    (evil-global-set-key state key ',prefix-cmd)))
		(evil-normalize-keymaps))))))

     (when bindings
       `((bind-map-set-keys ,map
           ,@bindings)))

     `(',map))))
(put 'bind-map 'lisp-indent-function 'defun)

(defun bind-map--get-prop (keyword args parent-args)
  (list keyword
        (or (plist-get args keyword)
            (plist-get parent-args keyword))))

;;;###autoload
(defmacro bind-map-for-mode-inherit (map parent &rest args)
  "Same as `bind-map' for MAP, except use some arguments from
PARENT as defaults, which must be another map declared with
`bind-map'. This is intended to be used with :major-modes
or :minor-modes and will throw an error if not.

The arguments that get recycled from PARENT (unless a new value
is provided) are :override-minor-modes, :keys, :evil-keys,
and :evil-states. All others must be declared explicitly."
  (declare (indent 2))
  (let* ((parent-args (symbol-plist parent))
         (minor-modes (plist-get args :minor-modes))
         (major-modes (plist-get args :major-modes))
         (bindings (plist-get args :bindings)))
    (when (and (null minor-modes)
               (null major-modes))
      (user-error "bind-map-for-modes-derived called without \
reference to :major-modes or :minor-modes"))
    `(bind-map ,map
       :prefix-cmd ,(plist-get args :prefix-cmd)
       :override-mode-name ,(plist-get args :override-mode-name)
       :minor-modes ,minor-modes
       :major-modes ,major-modes
       :bindings ,bindings
       ,@(bind-map--get-prop :override-minor-modes args parent-args)
       ,@(bind-map--get-prop :keys args parent-args)
       ,@(bind-map--get-prop :evil-keys args parent-args)
       ,@(bind-map--get-prop :evil-states args parent-args))))

;;;###autoload
(defmacro bind-map-for-major-mode (major-mode-sym &rest args)
  "Short version of `bind-map' if you want to bind a map for a
single major mode. MAJOR-MODE-SYM is the unquoted symbol
representing a major mode. This macro makes the call

\(bind-map map-name
  :major-modes \(MAJOR-MODE-SYM\)
  ARGS\)

where ARGS should include :keys or :evil-keys. The name of the
generated keymap is returned, which is MAJOR-MODE-SYM concatenated
with `bind-map-default-map-suffix'."
  (let ((map-name (intern (concat (symbol-name major-mode-sym)
                                  bind-map-default-map-suffix))))
    `(bind-map ,map-name
       :major-modes (,major-mode-sym)
       ,@args)))
(put 'bind-map-for-major-mode 'lisp-indent-function 'defun)

;;;###autoload
(defmacro bind-map-for-minor-mode (minor-mode-sym &rest args)
  "Short version of `bind-map' if you want to bind a map for a
single minor mode. MINOR-MODE-SYM is the unquoted symbol
representing a minor mode. This macro makes the call

\(bind-map map-name
  :minor-modes \(MINOR-MODE-SYM\)
  ARGS\)

where ARGS should include :keys or :evil-keys. The name of the
generated keymap is returned, which is MINOR-MODE-SYM
concatenated with `bind-map-default-map-suffix'."
  (let ((map-name (intern (concat (symbol-name minor-mode-sym)
                                  bind-map-default-map-suffix))))
    `(bind-map ,map-name
       :minor-modes (,minor-mode-sym)
       ,@args)))
(put 'bind-map-for-minor-mode 'lisp-indent-function 'defun)

;;;###autoload
(defun bind-map-set-keys (map key def &rest bindings)
  "Add a series of bindings to MAP.
BINDINGS is a series of KEY DEF pairs. Each KEY should be a
string suitable for `kbd'."
  (while key
    (define-key map (kbd key) def)
    (setq key (pop bindings) def (pop bindings))))
(put 'bind-map-set-keys 'lisp-indent-function 'defun)

;;;###autoload
(defun bind-map-set-key-defaults (map key def &rest bindings)
  "Add a series of default bindings to MAP.
Default bindings never override existing ones. BINDINGS is a
series of KEY DEF pairs. Each KEY should be a string suitable for
`kbd'."
  (while key
    (unless (lookup-key map (kbd key))
      (define-key map (kbd key) def))
    (setq key (pop bindings) def (pop bindings))))
(put 'bind-map-set-key-defaults 'lisp-indent-function 'defun)

(provide 'bind-map)
;;; bind-map.el ends here
