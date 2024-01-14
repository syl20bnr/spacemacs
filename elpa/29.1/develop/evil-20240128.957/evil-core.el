;;; evil-core.el --- Core functionality -*- lexical-binding: t -*-
;; Author: Vegard Øye <vegard_oye at hotmail.com>
;; Maintainer: Vegard Øye <vegard_oye at hotmail.com>

;; Version: 1.15.0

;;
;; This file is NOT part of GNU Emacs.

;;; License:

;; This file is part of Evil.
;;
;; Evil is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Evil is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Evil.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Evil is defined as a globalized minor mode, enabled with the toggle
;; function `evil-mode'.  This in turn enables `evil-local-mode' in
;; every buffer, which sets up the buffer's state.
;;
;; Each state has its own keymaps, and these keymaps have status as
;; "emulation keymaps" with priority over regular keymaps.  Emacs
;; maintains the following keymap hierarchy (highest priority first):
;;
;;     * Overriding keymaps/overlay keymaps...
;;     * Emulation mode keymaps...
;;       - Evil keymaps...
;;     * Minor mode keymaps...
;;     * Local keymap (`local-set-key')
;;     * Global keymap (`global-set-key')
;;
;; Within this hierarchy, Evil arranges the keymaps for the current
;; state as shown below:
;;
;;     * Intercept keymaps...
;;     * Local state keymap
;;     * Minor-mode keymaps...
;;     * Auxiliary keymaps...
;;     * Overriding keymaps...
;;     * Global state keymap
;;     * Keymaps for other states...
;;
;; These keymaps are listed in `evil-mode-map-alist', which is listed
;; in `emulation-mode-map-alist'.
;;
;; Most of the key bindings for a state are stored in its global
;; keymap, which has a name such as `evil-normal-state-map'. (See the
;; file evil-maps.el, which contains all the default key bindings.) A
;; state also has a local keymap (`evil-normal-state-local-map'),
;; which may contain user customizations for the current buffer.
;; Furthermore, any Emacs mode may be assigned state bindings of its
;; own by passing the mode's keymap to the function `evil-define-key'
;; or `evil-define-minor-mode-key'. The former uses a specific map to
;; define the key in while the latter associates the key with a
;; particular mode. These mode-specific bindings are ultimately stored
;; in so-called auxiliary and minor-mode keymaps respectively, which
;; are sandwiched between the local keymap and the global keymap.
;; Finally, the state may also activate the keymaps of other states
;; (e.g., Normal state inherits bindings from Motion state).
;;
;; For integration purposes, a regular Emacs keymap may be "elevated"
;; to emulation status by passing it to `evil-make-intercept-map' or
;; `evil-make-overriding-map'.  An "intercept" keymap has priority over
;; all other Evil keymaps.  (Evil uses this facility when debugging and
;; for handling the "ESC" key in the terminal.) More common is the
;; "overriding" keymap, which only has priority over the global state
;; keymap.  (This is useful for adapting key-heavy modes such as Dired,
;; where all but a few keys should be left as-is and should not be
;; shadowed by Evil's default bindings.)
;;
;; States are defined with the macro `evil-define-state', which
;; creates a command for switching to the state.  This command,
;; for example `evil-normal-state' for Normal state, performs
;; the following tasks:
;;
;;     * Setting `evil-state' to the new state.
;;     * Refreshing the keymaps in `evil-mode-map-alist'.
;;     * Updating the mode line.
;;       - Normal state depends on `evil-normal-state-tag'.
;;     * Adjusting the cursor's appearance.
;;       - Normal state depends on `evil-normal-state-cursor'.
;;     * Displaying a message in the echo area.
;;       - Normal state depends on `evil-normal-state-message'.
;;     * Running hooks.
;;       - Normal state runs `evil-normal-state-entry-hook' when
;;         entering, and `evil-normal-state-exit-hook' when exiting.
;;
;; The various properties of a state can be accessed through their
;; respective variables, or by passing a keyword and the state's name
;; to the `evil-state-property' function.  Evil defines the states
;; Normal state ("normal"), Insert state ("insert"), Visual state
;; ("visual"), Replace state ("replace"), Operator-Pending state
;; ("operator"), Motion state ("motion") and Emacs state ("emacs").

;;; Code:

(require 'advice)
(require 'evil-common)

(declare-function evil-emacs-state-p "evil-states")
(declare-function evil-ex-p "evil-ex")

(define-minor-mode evil-local-mode
  "Minor mode for setting up Evil in a single buffer."
  :init-value nil
  (if evil-local-mode
      (progn
        (cl-pushnew 'evil-mode-map-alist emulation-mode-map-alists)
        (evil-initialize-local-keymaps)
        (when (minibufferp)
          (setq-local evil-default-state 'insert)
          (setq-local evil-echo-state nil))
        (setq evil-input-method current-input-method)
        (evil-initialize-state)
        (add-hook 'input-method-activate-hook #'evil-activate-input-method t t)
        (add-hook 'input-method-deactivate-hook #'evil-deactivate-input-method t t)
        (add-hook 'activate-mark-hook 'evil-visual-activate-hook nil t)
        ;; FIXME: Add these hooks buffer-locally and remove when disabling
        (add-hook 'pre-command-hook 'evil-repeat-pre-hook)
        (add-hook 'post-command-hook 'evil-repeat-post-hook))
    (evil-refresh-mode-line)
    (remove-hook 'activate-mark-hook 'evil-visual-activate-hook t)
    (remove-hook 'input-method-activate-hook #'evil-activate-input-method t)
    (remove-hook 'input-method-deactivate-hook #'evil-deactivate-input-method t)
    (activate-input-method evil-input-method)
    (evil-change-state nil)))

;; Make the variable permanent local.  This is particular useful in
;; conjunction with nXhtml/mumamo because mumamo does not touch these
;; variables.
(put 'evil-local-mode 'permanent-local t)

(defun turn-on-evil-mode ()
  "Turn on Evil in the current buffer."
  (interactive)
  (evil-local-mode))

(defun turn-off-evil-mode ()
  "Turn off Evil in the current buffer."
  (interactive)
  (evil-local-mode -1))

;; The function `evil-initialize' should only be used to initialize
;; `evil-local-mode' from the globalized minor-mode `evil-mode'. It is
;; called whenever evil is enabled in a buffer for the first time or
;; when evil is active and the major-mode of the buffer changes.
(defun evil-initialize ()
  "Enable Evil in the current buffer, if appropriate.
To enable Evil globally, do (evil-mode)."
  (if evil-local-mode
      ;; Set Evil state according to new major-mode
      (evil-initialize-state)
    (or (and (minibufferp) (not evil-want-minibuffer))
        (evil-disabled-buffer-p)
        (evil-local-mode))))

(defalias 'evil--fundamental-mode #'fundamental-mode)

;;;###autoload (autoload 'evil-mode "evil" nil t)
(define-globalized-minor-mode evil-mode evil-local-mode evil-initialize
  :group 'evil)

(defadvice evil-mode (after start-evil activate)
  ;; Hooks used to not run in Fundamental buffers (bug#23827), so
  ;; other measures are necessary to initialize Evil there. When Evil
  ;; is enabled globally, the default value of `major-mode' is set to
  ;; the `evil--fundamental-mode' alias, sidestepping the restriction.
  (if evil-mode
      (progn
        (and (eval-when-compile (version< emacs-version "26.1"))
             (eq (default-value 'major-mode) 'fundamental-mode)
             (setq-default major-mode 'evil--fundamental-mode))
        (ad-enable-regexp "^evil")
        (ad-activate-regexp "^evil")
        (with-no-warnings (evil-esc-mode 1)))
    (when (eq (default-value 'major-mode) 'evil--fundamental-mode)
      (setq-default major-mode 'fundamental-mode))
    (ad-disable-regexp "^evil")
    (ad-update-regexp "^evil")
    (with-no-warnings (evil-esc-mode -1))))

(defun evil-change-state (state &optional message)
  "Change the state to STATE.
If STATE is nil, disable all states."
  (let ((func (evil-state-property (or state evil-state) :toggle)))
    (when (and (functionp func)
               (or message (not (eq state evil-state))))
      (funcall func (if state (and message 1) -1)))))

(defmacro evil-save-state (&rest body)
  "Save the current state; execute BODY; restore the state."
  (declare (indent defun)
           (debug t))
  `(let ((evil-state evil-state)
         (evil-previous-state evil-previous-state)
         (evil-previous-state-alist (copy-tree evil-previous-state-alist))
         (evil-next-state evil-next-state)
         (old-state evil-state)
         (inhibit-quit t)
         (buf (current-buffer)))
     (unwind-protect
         (progn ,@body)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (evil-change-state old-state))))))

(defmacro evil-with-state (state &rest body)
  "Change to STATE and execute BODY without refreshing the display.
Restore the previous state afterwards."
  (declare (indent defun)
           (debug t))
  `(evil-without-display
     (evil-save-state
       (evil-change-state ',state)
       ,@body)))

(defun evil-initialize-state ()
  "Set up the initial state for the current buffer.
See also `evil-set-initial-state'."
  (evil-change-state (evil-initial-state-for-buffer)))

(defun evil-initial-state-for-buffer-name (&optional name default)
  "Return the initial Evil state to use for a buffer with name NAME.
Matches the name against the regular expressions in
`evil-buffer-regexps'. If none matches, returns DEFAULT."
  (let ((name (if (stringp name) name (buffer-name name))))
    (when name
      (catch 'done
        (dolist (entry evil-buffer-regexps default)
          (let ((regexp (car entry))
                (state (cdr entry)))
            (when (string-match-p regexp name)
              (throw 'done state))))))))

(defun evil-disabled-buffer-p (&optional buffer)
  "Whether Evil should be disabled in BUFFER."
  (null (evil-initial-state-for-buffer-name buffer 'undefined)))

(defun evil-initial-state-for-buffer (&optional buffer)
  "Return the initial Evil state to use for BUFFER.
BUFFER defaults to the current buffer. See also `evil-initial-state'."
  (with-current-buffer (or buffer (current-buffer))
    (or (evil-initial-state-for-buffer-name)
        (cl-loop for (mode) in minor-mode-map-alist
                 when (and (boundp mode) (symbol-value mode))
                 thereis (evil-initial-state mode))
        (evil-initial-state major-mode nil t)
        evil-default-state)))

(defun evil-initial-state (mode &optional default follow-parent checked-modes)
  "Return the Evil state to use for MODE or its alias.
Return DEFAULT if no initial state is associated with MODE.
The initial state for a mode can be set with
`evil-set-initial-state'.

If FOLLOW-PARENT is non-nil, also check parent modes of MODE and
its alias. CHECKED-MODES is used internally and should not be set
initially."
  (when (memq mode checked-modes)
    (error "Circular reference detected in ancestors of `%s'\n%s"
           major-mode checked-modes))
  (let ((mode-alias (let ((func (symbol-function mode)))
                      (when (symbolp func) func))))
    (or (cl-dolist (entry (evil-state-property t :modes) default)
          (let ((state (car entry))
                (modes (symbol-value (cdr entry))))
            (when (or (memq mode modes)
                      (and mode-alias (memq mode-alias modes)))
              (cl-return state))))
        (and follow-parent (get mode 'derived-mode-parent)
             (evil-initial-state (get mode 'derived-mode-parent)
                                 nil t (cons mode checked-modes)))
        (and follow-parent mode-alias
             (get mode-alias 'derived-mode-parent)
             (evil-initial-state (get mode-alias 'derived-mode-parent)
                                 nil t (cons mode-alias checked-modes))))))

(defun evil-set-initial-state (mode state)
  "Set the initial state for major mode MODE to STATE.
This is the state the buffer comes up in."
  (dolist (modes (evil-state-property t :modes))
    (setq modes (cdr-safe modes))
    (set modes (delq mode (symbol-value modes))))
  (when state
    (add-to-list (evil-state-property state :modes) mode)))

(evil-define-command evil-change-to-initial-state
  (&optional buffer message)
  "Change the state of BUFFER to its initial state.
This is the state the buffer came up in. If Evil is not activated
then this function does nothing."
  :keep-visual t
  :suppress-operator t
  (with-current-buffer (or buffer (current-buffer))
    (when evil-local-mode
      (evil-change-state (evil-initial-state-for-buffer buffer)
                         message))))

(evil-define-command evil-change-to-previous-state
  (&optional buffer message)
  "Change the state of BUFFER to its previous state."
  :keep-visual t
  :repeat abort
  :suppress-operator t
  (with-current-buffer (or buffer (current-buffer))
    (let ((prev-state evil-previous-state)
          (prev-prev-state (cdr-safe (assoc evil-previous-state
                                            evil-previous-state-alist))))
      (evil-change-state nil)
      (when prev-prev-state
        (setq evil-previous-state prev-prev-state))
      (evil-change-state (or prev-state evil-default-state 'normal)
                         message))))

;; When a buffer is created in a low-level way, it is invisible to
;; Evil (as well as other globalized minor modes) because no hooks are
;; run. This is appropriate since many buffers are used for throwaway
;; purposes. Passing the buffer to `set-window-buffer' indicates
;; otherwise, though, so advise this function to initialize Evil.
(defadvice set-window-buffer (before evil)
  "Initialize Evil in the displayed buffer."
  (when (and evil-mode (get-buffer (ad-get-arg 1)))
    (with-current-buffer (ad-get-arg 1)
      (unless evil-local-mode
        (save-match-data (evil-initialize))))))

;; Refresh cursor color.
;; Cursor color can only be set for each frame but not for each buffer.
(add-hook 'window-configuration-change-hook #'evil-refresh-cursor)
(defadvice select-window (after evil activate)
  (evil-refresh-cursor))

(defun evil-generate-mode-line-tag (&optional state)
  "Generate the evil mode-line tag for STATE."
  (let ((tag (evil-state-property state :tag t)))
    (when (functionp tag)
      (setq tag (funcall tag)))
    ;; prepare mode-line: add tooltip
    (if (stringp tag)
        (propertize tag
                    'help-echo (evil-state-property state :name)
                    'mouse-face 'mode-line-highlight)
      tag)))

(defun evil-refresh-mode-line (&optional state)
  "Refresh mode line tag."
  (when (listp mode-line-format)
    (setq evil-mode-line-tag (evil-generate-mode-line-tag state))
    ;; refresh mode line data structure
    ;; first remove evil from mode-line
    (setq mode-line-format (delq 'evil-mode-line-tag mode-line-format))
    (let ((mlpos mode-line-format)
          pred which where)
      ;; determine before/after which symbol the tag should be placed
      (cond
       ((eq evil-mode-line-format 'before)
        (setq where 'after which 'mode-line-position))
       ((eq evil-mode-line-format 'after)
        (setq where 'after which 'mode-line-modes))
       ((consp evil-mode-line-format)
        (setq where (car evil-mode-line-format)
              which (cdr evil-mode-line-format))))
      ;; find the cons-cell of the symbol before/after which the tag
      ;; should be placed
      (while (and mlpos
                  (let ((sym (or (car-safe (car mlpos)) (car mlpos))))
                    (not (eq which sym))))
        (setq pred mlpos
              mlpos (cdr mlpos)))
      ;; put evil tag at the right position in the mode line
      (cond
       ((not mlpos)) ;; position not found, so do not add the tag
       ((eq where 'before)
        (if pred
            (setcdr pred (cons 'evil-mode-line-tag mlpos))
          (setq mode-line-format
                (cons 'evil-mode-line-tag mode-line-format))))
       ((eq where 'after)
        (setcdr mlpos (cons 'evil-mode-line-tag (cdr mlpos)))))
      (force-mode-line-update))))

;; input methods should be disabled in non-insertion states
(defun evil-activate-input-method ()
  "Enable input method in states with :input-method non-nil."
  (let (input-method-activate-hook
        input-method-deactivate-hook)
    (when (and evil-local-mode evil-state)
      (setq evil-input-method current-input-method)
      (unless (evil-state-property evil-state :input-method)
        (deactivate-input-method)))))
(put 'evil-activate-input-method 'permanent-local-hook t)

(defun evil-deactivate-input-method ()
  "Disable input method in all states."
  (let (input-method-activate-hook
        input-method-deactivate-hook)
    (when (and evil-local-mode evil-state)
      (setq evil-input-method nil))))
(put 'evil-deactivate-input-method 'permanent-local-hook t)

(defmacro evil-without-input-method-hooks (&rest body)
  "Execute body with evil's activate/deactivate-input-method hooks deactivated.
This allows input methods to be used in normal-state."
  (declare (indent defun))
  `(unwind-protect
       (progn
         (remove-hook 'input-method-activate-hook #'evil-activate-input-method t)
         (remove-hook 'input-method-deactivate-hook #'evil-deactivate-input-method t)
         ,@body)
     (add-hook 'input-method-activate-hook #'evil-activate-input-method nil t)
     (add-hook 'input-method-deactivate-hook #'evil-deactivate-input-method nil t)))

(defadvice toggle-input-method (around evil)
  "Refresh `evil-input-method'."
  (cond
   ((not evil-local-mode)
    ad-do-it)
   ((evil-state-property evil-state :input-method)
    ad-do-it)
   (t
    (let ((current-input-method evil-input-method))
      ad-do-it))))

;; Local keymaps are implemented using buffer-local variables.
;; However, unless a buffer-local value already exists,
;; `define-key' acts on the variable's default (global) value.
;; So we need to initialize the variable whenever we enter a
;; new buffer or when the buffer-local values are reset.
(defun evil-initialize-local-keymaps ()
  "Initialize a buffer-local value for local keymaps as necessary.
The initial value is that of `make-sparse-keymap'."
  (dolist (entry evil-local-keymaps-alist)
    (let ((map (cdr entry)))
      (unless (and (keymapp (symbol-value map))
                   (local-variable-p map))
        (set map (make-sparse-keymap))))))

(defun evil-make-overriding-map (keymap &optional state copy)
  "Give KEYMAP precedence over the global keymap of STATE.
The keymap will have lower precedence than custom STATE bindings.
If STATE is nil, give it precedence over all states.
If COPY is t, create a copy of KEYMAP and give that
higher precedence. See also `evil-make-intercept-map'."
  (let ((key [override-state]))
    (if (not copy)
        (define-key keymap key (or state 'all))
      (unless (keymapp copy)
        (setq copy (assq-delete-all 'menu-bar (copy-keymap keymap))))
      (define-key copy key (or state 'all))
      (define-key keymap key copy))))

(defun evil-make-intercept-map (keymap &optional state aux)
  "Give KEYMAP precedence over all Evil keymaps in STATE.
If STATE is nil, give it precedence over all states. If AUX is non-nil, make the
auxiliary keymap corresponding to KEYMAP in STATE an intercept keymap instead of
KEYMAP itself. See also `evil-make-overriding-map'."
  (let ((key [intercept-state])
        (keymap (if aux
                    (evil-get-auxiliary-keymap keymap state t t)
                  keymap)))
    (define-key keymap key (or state 'all))))

(defmacro evil-define-keymap (keymap doc &rest body)
  "Define a keymap KEYMAP listed in `evil-mode-map-alist'.
That means it will have precedence over regular keymaps.

DOC is the documentation for the variable. BODY, if specified,
is executed after toggling the mode. Optional keyword arguments
may be specified before the body code:

:mode VAR       Mode variable. If unspecified, the variable
                is based on the keymap name.
:local BOOLEAN  Whether the keymap should be buffer-local, that is,
                reinitialized for each buffer.
:func BOOLEAN   Create a toggle function even if BODY is empty.

\(fn KEYMAP DOC [[KEY VAL]...] BODY...)"
  (declare (indent defun)
           (doc-string 2)
           (debug (&define name
                           [&optional stringp]
                           [&rest [keywordp sexp]]
                           def-body)))
  (let ((func t)
        arg intercept key local mode overriding)
    (while (keywordp (car-safe body))
      (setq key (pop body)
            arg (pop body))
      (cond
       ((eq key :mode) (setq mode arg))
       ((eq key :local) (setq local arg))
       ((eq key :func) (setq func arg))
       ((eq key :intercept) (setq intercept arg))
       ((eq key :overriding) (setq overriding arg))))
    (setq mode (or mode
                   (intern (replace-regexp-in-string
                            "\\(?:-\\(?:mode-\\)?\\(?:key\\)?map\\)?$"
                            "-mode"
                            (symbol-name keymap)))))
    `(progn
       (defvar ,keymap ,(unless local '(make-sparse-keymap)))
       (unless (get ',keymap 'variable-documentation)
         (put ',keymap 'variable-documentation ,doc))
       (defvar ,mode nil)
       (unless (get ',mode 'variable-documentation)
         (put ',mode 'variable-documentation ,doc))
       (make-variable-buffer-local ',mode)
       (put ',mode 'permanent-local t)
       (when ,intercept
         (evil-make-intercept-map ,keymap))
       (when ,overriding
         (evil-make-overriding-map ,keymap))
       ,@(if local
             `((make-variable-buffer-local ',keymap)
               (put ',keymap 'permanent-local t)
               (evil--add-to-alist evil-local-keymaps-alist ',mode ',keymap))
           `((evil--add-to-alist evil-global-keymaps-alist ',mode ',keymap)
             (evil--add-to-alist evil-mode-map-alist ',mode ,keymap)))
       ,(when (or body func)
          `(defun ,mode (&optional arg)
             ,@(when doc `(,doc))
             (interactive)
             (cond
              ((numberp arg) (setq ,mode (> arg 0)))
              (t (setq ,mode (not ,mode))))
             ,@body))
       ',keymap)))

;; The ESC -> escape translation code has been provided by Stefan
;; Monnier in the discussion of GNU Emacs bug #13793.
(defun evil-esc-mode (&optional arg)
  "Toggle interception of \\e (escape).
Enable with positive ARG and disable with negative ARG.

When enabled, `evil-esc-mode' modifies the entry of \\e in
`input-decode-map'. If such an event arrives, it is translated to
a plain `escape' event if no further event occurs within
`evil-esc-delay' seconds. Otherwise no translation happens and
the ESC prefix map (i.e. the map originally bound to \\e in
`input-decode-map`) is returned."
  (cond
   ((or (null arg) (eq arg 0))
    (evil-esc-mode (if evil-esc-mode -1 +1)))
   ((> arg 0)
    (unless evil-esc-mode
      (setq evil-esc-mode t)
      (add-hook 'after-make-frame-functions #'evil-init-esc)
      (mapc #'evil-init-esc (frame-list))))
   ((< arg 0)
    (when evil-esc-mode
      (remove-hook 'after-make-frame-functions #'evil-init-esc)
      (mapc #'evil-deinit-esc (frame-list))
      (setq evil-esc-mode nil)))))

(defun evil-init-esc (frame)
  "Update `input-decode-map' in terminal."
  (with-selected-frame frame
    (let ((term (frame-terminal frame)))
      (when (and
             (or (eq evil-intercept-esc 'always)
                 (and evil-intercept-esc
                      (eq (terminal-live-p term) t))) ; only patch tty
             (not (terminal-parameter term 'evil-esc-map)))
        (let ((evil-esc-map (lookup-key input-decode-map [?\e])))
          (set-terminal-parameter term 'evil-esc-map evil-esc-map)
          (define-key input-decode-map [?\e]
            `(menu-item "" ,evil-esc-map :filter ,#'evil-esc)))))))

(defun evil-deinit-esc (frame)
  "Restore `input-decode-map' in terminal."
  (with-selected-frame frame
    (let ((term (frame-terminal frame)))
      (when (terminal-live-p term)
        (let ((evil-esc-map (terminal-parameter term 'evil-esc-map)))
          (when evil-esc-map
            (define-key input-decode-map [?\e] evil-esc-map)
            (set-terminal-parameter term 'evil-esc-map nil)))))))

(defun evil-esc (map)
  "Translate \\e to `escape' if no further event arrives.
This function is used to translate a \\e event either to `escape'
or to the standard ESC prefix translation map. If \\e arrives,
this function waits for `evil-esc-delay' seconds for another
event. If no other event arrives, the event is translated to
`escape', otherwise it is translated to the standard ESC prefix
map stored in `input-decode-map'. If `evil-inhibit-esc' is
non-nil or if evil is in emacs state, the event is always
translated to the ESC prefix.

The translation to `escape' happens only if the current command
has indeed been triggered by \\e. In other words, this will only
happen when the keymap is accessed from `read-key-sequence'. In
particular, if it is access from `define-key' the returned
mapping will always be the ESC prefix map."
  (if (and (not evil-inhibit-esc)
           (or evil-local-mode (evil-ex-p)
               (active-minibuffer-window))
           (not (evil-emacs-state-p))
           (let ((keys (this-single-command-keys)))
             (and (> (length keys) 0)
                  (= (aref keys (1- (length keys))) ?\e)))
           (sit-for evil-esc-delay))
      (prog1 [escape]
        (when defining-kbd-macro
          (end-kbd-macro)
          (setq last-kbd-macro (vconcat last-kbd-macro [escape]))
          (start-kbd-macro t t)))
    map))

(defun evil-state-p (sym)
  "Whether SYM is the name of a state."
  (assq sym evil-state-properties))

(defun evil-state-keymaps (state &rest excluded)
  "Return a keymap alist of keymaps activated by STATE.
If STATE references other states in its :enable property,
these states are recursively processed and added to the list.
\(The EXCLUDED argument is an internal safeguard against
infinite recursion, keeping track of processed states.)"
  (let* ((state (or state evil-state))
         (enable (evil-state-property state :enable))
         (map (cons
               (evil-state-property state :mode)
               (evil-state-property state :keymap t)))
         (local-map (cons
                     (evil-state-property state :local)
                     (evil-state-property state :local-keymap t)))
         (minor-mode-maps (evil-state-minor-mode-keymaps state))
         (aux-maps (evil-state-auxiliary-keymaps state))
         (overriding-maps
          (evil-state-overriding-keymaps state))
         (intercept-maps
          (evil-state-intercept-keymaps state))
         (result `(,intercept-maps))
         (remove-duplicates (null excluded)))
    (unless (memq state enable)
      (setq enable (cons state enable)))
    ;; process STATE's :enable property
    (dolist (entry enable)
      (cond
       ((memq entry excluded))
       ;; the keymaps for STATE
       ((eq entry state)
        (setq result `(,@result
                       (,local-map)
                       ,minor-mode-maps
                       ,aux-maps
                       ,overriding-maps
                       (,map)))
        (push state excluded))
       ;; the keymaps for another state: call `evil-state-keymaps'
       ;; recursively, but keep track of processed states
       ((evil-state-p entry)
        (setq result `(,@result
                       ,(apply #'evil-state-keymaps entry excluded))))
       ;; a single keymap
       ((or (keymapp entry)
            (and (keymapp (symbol-value entry))
                 (setq entry (symbol-value entry)))
            (setq entry (evil-keymap-for-mode entry)))
        (setq result `(,@result
                       ((,(evil-mode-for-keymap entry t) .
                         ,entry)))))))
    ;; postpone the expensive filtering of duplicates to the top level
    (if remove-duplicates
        (apply #'evil-concat-keymap-alists result)
      (apply #'append result))))

(defun evil-normalize-keymaps (&optional state)
  "Create a buffer-local value for `evil-mode-map-alist'.
This is a keymap alist, determined by the current state
\(or by STATE if specified)."
  (let ((state (or state evil-state))
        (excluded '(nil t))
        map mode temp)
    ;; initialize buffer-local keymaps as necessary
    (evil-initialize-local-keymaps)
    ;; deactivate keymaps of previous state
    (dolist (entry evil-mode-map-alist)
      (setq mode (car entry)
            map (cdr entry))
      ;; don't deactivate overriding keymaps;
      ;; they are toggled by their associated mode
      (if (or (memq mode excluded)
              (evil-intercept-keymap-p map)
              (evil-overriding-keymap-p map)
              (evil-auxiliary-keymap-p map)
              (evil-minor-mode-keymap-p map))
          (push mode excluded)
        (and (fboundp mode) (symbol-value mode) (funcall mode -1))
        (set mode nil)))
    (setq evil-mode-map-alist nil)
    ;; activate keymaps of current state
    (when state
      (setq temp (evil-state-keymaps state))
      (dolist (entry temp)
        (setq mode (car entry)
              map (cdr entry))
        (unless (or (and (boundp mode) (symbol-value mode))
                    ;; the minor-mode keymaps include modes that are not
                    ;; necessarily active
                    (evil-minor-mode-keymap-p map))
          (when (fboundp mode)
            (funcall mode 1))
          (set mode t))
        ;; refresh the keymap in case it has changed
        ;; (e.g., `evil-operator-shortcut-map' is
        ;; reset on toggling)
        (if (or (memq mode excluded)
                (evil-intercept-keymap-p map)
                (evil-overriding-keymap-p map)
                (evil-auxiliary-keymap-p map)
                (evil-minor-mode-keymap-p map))
            (push mode excluded)
          (setcdr entry (or (evil-keymap-for-mode mode) map))))
      ;; update `evil-mode-map-alist'
      (setq evil-mode-map-alist temp))))

(defun evil-mode-for-keymap (keymap &optional default)
  "Return the minor mode associated with KEYMAP.
Return DEFAULT if no mode is found.
See also `evil-keymap-for-mode'."
  (let ((map (if (keymapp keymap) keymap (symbol-value keymap)))
        (var (when (symbolp keymap) keymap)))
    ;; Check Evil variables first for speed purposes.
    ;; If all else fails, check `minor-mode-map-alist'.
    (or (when var
          (or (car (rassq var evil-global-keymaps-alist))
              (car (rassq var evil-local-keymaps-alist))))
        (car (rassq map (mapcar #'(lambda (e)
                                    ;; from (MODE-VAR . MAP-VAR)
                                    ;; to (MODE-VAR . MAP)
                                    (cons (car-safe e)
                                          (symbol-value (cdr-safe e))))
                                (append evil-global-keymaps-alist
                                        evil-local-keymaps-alist))))
        (car (rassq map minor-mode-map-alist))
        default)))

(defun evil-keymap-for-mode (mode &optional variable)
  "Return the keymap associated with MODE.
Return the keymap variable if VARIABLE is non-nil.
See also `evil-mode-for-keymap'."
  (let* ((var (or (cdr (assq mode evil-global-keymaps-alist))
                  (cdr (assq mode evil-local-keymaps-alist))))
         (map (or (symbol-value var)
                  (cdr (assq mode minor-mode-map-alist)))))
    (if variable var map)))

(defun evil-state-auxiliary-keymaps (state)
  "Return a keymap alist of auxiliary keymaps for STATE."
  (let ((state (or state evil-state))
        aux result)
    (dolist (map (current-active-maps) (nreverse result))
      (when (setq aux (evil-get-auxiliary-keymap map state))
        (push (cons (evil-mode-for-keymap map t) aux) result)))))

(defun evil-state-minor-mode-keymaps (state)
  "Return a keymap alist of minor-mode keymaps for STATE."
  (cdr (assq (or state evil-state) evil-minor-mode-keymaps-alist)))

(defun evil-state-overriding-keymaps (&optional state)
  "Return a keymap alist of overriding keymaps for STATE."
  (let* ((state (or state evil-state))
         result)
    (dolist (map (current-active-maps))
      (when (setq map (evil-overriding-keymap-p map state))
        (push (cons (evil-mode-for-keymap map t) map) result)))
    (nreverse result)))

(defun evil-state-intercept-keymaps (&optional state)
  "Return a keymap alist of intercept keymaps for STATE."
  (let* ((state (or state evil-state))
         result)
    (dolist (map (current-active-maps))
      (when (setq map (or (evil-intercept-keymap-p map state)
                          (evil-intercept-keymap-p
                           (evil-get-auxiliary-keymap map state) state)))
        (push (cons (evil-mode-for-keymap map t) map) result)))
    (setq result (nreverse result))
    result))

(defun evil-set-auxiliary-keymap (map state &optional aux)
  "Set the auxiliary keymap for MAP in STATE to AUX.
If AUX is nil, create a new auxiliary keymap."
  (unless aux (setq aux (make-sparse-keymap)))
  (unless (evil-auxiliary-keymap-p aux)
    (evil-set-keymap-prompt
     aux (format "Auxiliary keymap for %s"
                 (or (evil-state-property state :name)
                     (format "%s state" state)))))
  (define-key map (vector (intern (format "%s-state" state))) aux)
  aux)

(defun evil-get-auxiliary-keymap (map state &optional create ignore-parent)
  "Get the auxiliary keymap for MAP in STATE.
If CREATE is non-nil, create an auxiliary keymap
if MAP does not have one. If CREATE and
IGNORE-PARENT are non-nil then a new auxiliary
keymap is created even if the parent of MAP has
one already."
  (when state
    (let* ((key (vector (intern (format "%s-state" state))))
           (parent-aux (when (and ignore-parent
                                  (keymap-parent map))
                         (lookup-key (keymap-parent map) key)))
           (aux (lookup-key map key)))
      (cond
       ((and ignore-parent
             (equal parent-aux aux)
             create)
        (evil-set-auxiliary-keymap map state))
       ((evil-auxiliary-keymap-p aux)
        aux)
       (create
        (evil-set-auxiliary-keymap map state))))))

(defun evil-get-minor-mode-keymap (state mode)
  "Get the auxiliary keymap for MODE in STATE, creating one if it
does not already exist."
  (let ((state-entry (assq state evil-minor-mode-keymaps-alist)))
    (if (and state-entry
             (assq mode state-entry))
        (cdr (assq mode state-entry))
      (let ((map (make-sparse-keymap)))
        (evil-set-keymap-prompt
         map (format "Minor-mode keymap for %s in %s"
                     mode
                     (or (evil-state-property state :name)
                         (format "%s state" state))))
        (if state-entry
            (setcdr state-entry
                    (append (list (cons mode map)) (cdr state-entry)))
          (push (cons state (list (cons mode map)))
                evil-minor-mode-keymaps-alist))
        map))))

(defun evil-auxiliary-keymap-p (map)
  "Whether MAP is an auxiliary keymap."
  (let ((prompt (keymap-prompt map)))
    (when prompt (string-prefix-p "Auxiliary keymap" prompt))))

(defun evil-minor-mode-keymap-p (map)
  "Whether MAP is a minor-mode keymap."
  (let ((prompt (keymap-prompt map)))
    (when prompt (string-prefix-p "Minor-mode keymap" prompt))))

(defun evil-intercept-keymap-p (map &optional state)
  "Whether MAP is an intercept keymap for STATE.
If STATE is nil, it means any state."
  (let ((entry (and (keymapp map)
                    (lookup-key map [intercept-state]))))
    (cond
     ((null entry) nil)
     ((null state) map)
     ((eq entry state) map)
     ((eq entry 'all) map))))

(defun evil-overriding-keymap-p (map &optional state)
  "Whether MAP is an overriding keymap for STATE.
If STATE is nil, it means any state."
  (let ((entry (and (keymapp map)
                    (lookup-key map [override-state]))))
    (cond
     ((null entry) nil)
     ((keymapp entry) (evil-overriding-keymap-p entry state))
     ((null state) map)
     ((eq entry state) map)
     ((eq entry 'all) map))))

(defun evil-intercept-keymap-state (map)
  "Return the state for the intercept keymap MAP.
A return value of t means all states."
  (let ((state (lookup-key map [intercept-state] map)))
    (cond
     ((keymapp state)
      (evil-intercept-keymap-state state))
     ((eq state 'all)
      t)
     (t
      state))))

(defun evil-overriding-keymap-state (map)
  "Return the state for the overriding keymap MAP.
A return value of t means all states."
  (let ((state (lookup-key map [override-state] map)))
    (cond
     ((keymapp state)
      (evil-overriding-keymap-state state))
     ((eq state 'all)
      t)
     (t
      state))))

(defun evil-set-leader (state key &optional localleader)
  "Set KEY to trigger leader bindings in STATE.
KEY should be in the form produced by `kbd'. STATE is one of
`normal', `insert', `visual', `replace', `operator', `motion',
`emacs', a list of one or more of these, or `nil', which means
all of the above. If LOCALLEADER is non-nil, set the local leader
instead."
  (let* ((all-states '(normal insert visual replace operator motion emacs))
         (states (cond ((null state) all-states)
                       ((consp state) state)
                       (t (list state))))
         (leaderkey (if localleader [localleader] [leader]))
         (binding
          `(menu-item "" nil :filter ,(lambda (_cmd) (key-binding leaderkey)))))
    (dolist (state states)
      (evil-global-set-key state key binding))))

(defmacro evil-define-key (state keymap key def &rest bindings)
  "Create a STATE binding from KEY to DEF for KEYMAP.
STATE is one of `normal', `insert', `visual', `replace',
`operator', `motion', `emacs', or a list of one or more of
these. Omitting a state by using `nil' corresponds to a standard
Emacs binding using `define-key'. The remaining arguments are
like those of `define-key'. For example:

    (evil-define-key \\='normal foo-map \"a\" \\='bar)

This creates a binding from `a' to `bar' in normal state, which
is active whenever `foo-map' is active. Using nil for the state,
the following lead to identical bindings:

    (evil-define-key nil foo-map \"a\" \\='bar)
    (define-key foo-map \"a\" \\='bar)

It is possible to specify multiple states and/or bindings at
once:

    (evil-define-key \\='(normal visual) foo-map
      \"a\" \\='bar
      \"b\" \\='foo)

If `foo-map' has not been initialized yet, this macro adds an
entry to `after-load-functions', delaying execution as necessary.

KEYMAP may also be a quoted symbol. If the symbol is `global', the
global evil keymap corresponding to the state(s) is used, meaning
the following lead to identical bindings:

    (evil-define-key \\='normal \\='global \"a\" \\='bar)
    (evil-global-set-key \\='normal \"a\" \\='bar)

The symbol `local' may also be used, which corresponds to using
`evil-local-set-key'. If a quoted symbol is used that is not
`global' or `local', it is assumed to be the name of a minor
mode, in which case `evil-define-minor-mode-key' is used.

KEY is an internal Emacs representation of a key, as for
`define-key'. To bind key sequences that use modifier keys such
as \"C-a\" or \"M-a\", convert the key sequences using `kbd'.
For example:

    (evil-define-key \\='normal foo-map (kbd \"C-a\") \\='bar)"
  (declare (indent defun))
  (cond
   ((member keymap '('global 'local))
    `(evil-define-key* ,state ,keymap ,key ,def ,@bindings))
   ((eq (car-safe keymap) 'quote)
    `(evil-define-minor-mode-key ,state ,keymap ,key ,def ,@bindings))
   (t `(evil-with-delay ,(if (symbolp keymap)
                             ;; BEWARE: Can't work for lexically scoped vars
                             `(and (boundp ',keymap) (keymapp ,keymap))
                           `(keymapp ,keymap))
           (after-load-functions
            t nil ,(format "evil-define-key-in-%s"
                           (if (symbolp keymap) keymap 'keymap)))
         (with-demoted-errors "Error in evil-define-key: %S"
           (evil-define-key* ,state ,keymap ,key ,def ,@bindings))))))
(defalias 'evil-declare-key #'evil-define-key)

(defun evil-define-key* (state keymap key def &rest bindings)
  "Create a STATE binding from KEY to DEF for KEYMAP.
STATE is one of normal, insert, visual, replace, operator,
motion, emacs, or a list of one or more of these. Omitting a
state by using nil corresponds to a standard Emacs binding using
`define-key' The remaining arguments are like those of
`define-key'. For example:

    (evil-define-key* \\='normal foo-map \"a\" \\='bar)

This creates a binding from \"a\" to bar in Normal state, which
is active whenever foo-map is active. Using nil for the state,
the following are equivalent:

    (evil-define-key* nil foo-map \"a\" \\='bar)

    (define-key foo-map \"a\" \\='bar)

 It is possible to specify multiple states and/or bindings at
 once:

    (evil-define-key* \\='(normal visual) foo-map
      \"a\" \\='bar
      \"b\" \\='foo)

KEYMAP may also be a quoted symbol. If the symbol is global, the
global evil keymap corresponding to the state(s) is used, meaning
the following are equivalent:

    (evil-define-key* \\='normal \\='global \"a\" \\='bar)

    (evil-global-set-key \\='normal \"a\" \\='bar)

The symbol local may also be used, which corresponds to using
`evil-local-set-key'.

The use is nearly identical to `evil-define-key' with the
exception that this is a function and not a macro (and so will
not be expanded when compiled which can have unintended
consequences). `evil-define-key*' also does not defer any
bindings like `evil-define-key' does using `evil-with-delay'.  This
allows errors in the bindings to be caught immediately, and makes
its behavior more predictable."
  (declare (indent defun))
  (let ((maps
         (if state
             (mapcar
              (lambda (st)
                (cond ((eq keymap 'global)
                       (evil-state-property st :keymap t))
                      ((eq keymap 'local)
                       (evil-state-property st :local-keymap t))
                      (t
                       (evil-get-auxiliary-keymap keymap st t t))))
              (if (listp state) state (list state)))
           (list
            (cond ((eq keymap 'global)
                   global-map)
                  ((eq keymap 'local)
                   ;; see `local-set-key'
                   (or (current-local-map)
                       (let ((map (make-sparse-keymap)))
                         (use-local-map map)
                         map)))
                  (t
                   keymap))))))
    (while key
      (dolist (map maps)
        (define-key map key def))
      (setq key (pop bindings)
            def (pop bindings)))
    ;; ensure the prompt string comes first
    (dolist (map maps)
      (evil-set-keymap-prompt map (keymap-prompt map)))))

(defun evil-define-minor-mode-key (state mode key def &rest bindings)
  "Similar to `evil-define-key' but the bindings are associated
with the minor-mode symbol MODE instead of a particular map.
Associating bindings with a mode symbol instead of a map allows
evil to use Emacs' built-in mechanisms to enable the bindings
automatically when MODE is active without relying on calling
`evil-normalize-keymaps'. Another less significant difference is
that the bindings can be created immediately, because this
function only uses the symbol MODE and does not rely on its
value.

See `evil-define-key' for the usage of STATE, KEY, DEF and
BINDINGS."
  (declare (indent defun))
  (let ((maps (mapcar
               (lambda (st)
                 (evil-get-minor-mode-keymap st mode))
               (if (listp state) state (list state)))))
    (while key
      (dolist (map maps)
        (define-key map key def))
      (setq key (pop bindings)
            def (pop bindings)))))

(defmacro evil-add-hjkl-bindings (keymap &optional state &rest bindings)
  "Add \"h\", \"j\", \"k\", \"l\" bindings to KEYMAP in STATE.
Add additional BINDINGS if specified."
  (declare (indent defun))
  `(evil-define-key ,state ,keymap
     "h" (lookup-key evil-motion-state-map "h")
     "j" (lookup-key evil-motion-state-map "j")
     "k" (lookup-key evil-motion-state-map "k")
     "l" (lookup-key evil-motion-state-map "l")
     ":" (lookup-key evil-motion-state-map ":")
     ,@bindings))

;; may be useful for programmatic purposes
(defun evil-global-set-key (state key def)
  "Bind KEY to DEF in STATE."
  (define-key (evil-state-property state :keymap t) key def))

(defun evil-local-set-key (state key def)
  "Bind KEY to DEF in STATE in the current buffer."
  (define-key (evil-state-property state :local-keymap t) key def))

;; Advise these functions as they may activate an overriding keymap or
;; a keymap with state bindings; if so, refresh `evil-mode-map-alist'.
(defadvice use-global-map (after evil activate)
  "Refresh Evil keymaps."
  (evil-normalize-keymaps))

(defadvice use-local-map (after evil activate)
  "Refresh Evil keymaps."
  (evil-normalize-keymaps))

(defmacro evil-define-state (state doc &rest body)
  "Define an Evil state STATE.
DOC is a general description and shows up in all docstrings;
the first line of the string should be the full name of the state.

BODY is executed each time the state is enabled or disabled.

Optional keyword arguments:
- `:tag' - the mode line indicator, e.g. \"<T>\".
- `:message' - string shown in the echo area when the state is
  activated.
- `:cursor' - default cursor specification.
- `:enable' - list of other state keymaps to enable when in this
  state.
- `:entry-hook' - list of functions to run when entering this state.
- `:exit-hook' - list of functions to run when exiting this state.
- `:suppress-keymap' - if non-nil, effectively disables bindings to
  `self-insert-command' by making `evil-suppress-map' the parent of
  the global state keymap.

The global keymap of this state will be `evil-test-state-map',
the local keymap will be `evil-test-state-local-map', and so on.

\(fn STATE DOC [[KEY VAL]...] BODY...)"
  (declare (indent defun)
           (doc-string 2)
           (debug (&define name
                           [&optional stringp]
                           [&rest [keywordp sexp]]
                           def-body)))
  (let* ((name (and (string-match "^\\(.+\\)\\(\\(?:.\\|\n\\)*\\)" doc)
                    (match-string 1 doc)))
         (doc (match-string 2 doc))
         (name (and (string-match "^\\(.+?\\)\\.?$" name)
                    (match-string 1 name)))
         (doc (if (or (null doc) (string= doc "")) ""
                (format "\n%s" doc)))
         (toggle (intern (format "evil-%s-state" state)))
         (mode (intern (format "%s-minor-mode" toggle)))
         (keymap (intern (format "%s-map" toggle)))
         (local (intern (format "%s-local-minor-mode" toggle)))
         (local-keymap (intern (format "%s-local-map" toggle)))
         (tag (intern (format "%s-tag" toggle)))
         (message (intern (format "%s-message" toggle)))
         (cursor (intern (format "%s-cursor" toggle)))
         (entry-hook (intern (format "%s-entry-hook" toggle)))
         (exit-hook (intern (format "%s-exit-hook" toggle)))
         (modes (intern (format "%s-modes" toggle)))
         (predicate (intern (format "%s-p" toggle)))
         arg cursor-value enable entry-hook-value exit-hook-value
         input-method key message-value suppress-keymap tag-value)
    ;; collect keywords
    (while (keywordp (car-safe body))
      (setq key (pop body)
            arg (pop body))
      (cond
       ((eq key :tag)
        (setq tag-value arg))
       ((eq key :message)
        (setq message-value arg))
       ((eq key :cursor)
        (setq cursor-value arg))
       ((eq key :entry-hook)
        (setq entry-hook-value arg)
        (unless (listp entry-hook-value)
          (setq entry-hook-value (list entry-hook-value))))
       ((eq key :exit-hook)
        (setq exit-hook-value arg)
        (unless (listp exit-hook-value)
          (setq exit-hook-value (list exit-hook-value))))
       ((eq key :enable)
        (setq enable arg))
       ((eq key :input-method)
        (setq input-method arg))
       ((eq key :suppress-keymap)
        (setq suppress-keymap arg))))

    ;; macro expansion
    `(progn
       ;; Save the state's properties in `evil-state-properties' for
       ;; runtime lookup. Among other things, this information is used
       ;; to determine what keymaps should be activated by the state
       ;; (and, when processing :enable, what keymaps are activated by
       ;; other states). We cannot know this at compile time because
       ;; it depends on the current buffer and its active keymaps
       ;; (to which we may have assigned state bindings), as well as
       ;; states whose definitions may not have been processed yet.
       (let ((plist (list
                     :name ',name
                     :toggle ',toggle
                     :mode (defvar ,mode nil
                             ,(format "Non-nil if %s is enabled.
Use the command `%s' to change this variable." name toggle))
                     :keymap (defvar ,keymap (make-sparse-keymap)
                               ,(format "Keymap for %s." name))
                     :local (defvar ,local nil
                              ,(format "Non-nil if %s is enabled.
Use the command `%s' to change this variable." name toggle))
                     :local-keymap (defvar ,local-keymap nil
                                     ,(format "Buffer-local keymap for %s." name))
                     :tag (defvar ,tag ,tag-value
                            ,(format "Mode line tag for %s." name))
                     :message (defvar ,message ,message-value
                                ,(format "Echo area message for %s." name))
                     :cursor (defvar ,cursor ',cursor-value
                               ,(format "Cursor for %s.
May be a cursor type as per `cursor-type', a color string as passed
to `set-cursor-color', a zero-argument function for changing the
cursor, or a list of the above." name))
                     :entry-hook (defvar ,entry-hook nil
                                   ,(format "Hooks to run when entering %s." name))
                     :exit-hook (defvar ,exit-hook nil
                                  ,(format "Hooks to run when exiting %s." name))
                     :modes (defvar ,modes nil
                              ,(format "Modes that should come up in %s." name))
                     :input-method ',input-method
                     :predicate ',predicate
                     :enable ',enable)))
       (evil--add-to-alist evil-state-properties ',state plist))

       ,@(when suppress-keymap
           `((set-keymap-parent ,keymap evil-suppress-map)))

       (dolist (func ',entry-hook-value) (add-hook ',entry-hook func))
       (dolist (func ',exit-hook-value) (add-hook ',exit-hook func))

       (defun ,predicate (&optional state)
         ,(format "Whether the current state is %s.
\(That is, whether `evil-state' is `%s'.)" name state)
         (and evil-local-mode
              (eq (or state evil-state) ',state)))

       ;; define state function
       (defun ,toggle (&optional arg)
         ,(format "Enable %s. Disable with negative ARG.
If ARG is nil, don't display a message in the echo area.%s" name doc)
         (interactive)
         (cond
          ((and (numberp arg) (< arg 1))
           (setq evil-previous-state evil-state
                 evil-state nil)
           (let ((evil-state ',state))
             (run-hooks ',exit-hook)
             (setq evil-state nil)
             (evil-normalize-keymaps)
             ,@body))
          (t
           (unless evil-local-mode (evil-local-mode))
           (let ((evil-next-state ',state)
                 input-method-activate-hook input-method-deactivate-hook)
             (evil-change-state nil)
             (setq evil-state ',state)
             (evil--add-to-alist evil-previous-state-alist
                                 ',state evil-previous-state)
             (let ((evil-state ',state))
               (evil-normalize-keymaps)
               (if ',input-method
                   (activate-input-method evil-input-method)
                 ;; BUG #475: Deactivate the current input method only
                 ;; if there is a function to deactivate it, otherwise
                 ;; an error would be raised. This strange situation
                 ;; should not arise in general and there should
                 ;; probably be a better way to handle this situation.
                 (when deactivate-current-input-method-function
                   (deactivate-input-method)))
               (unless evil-no-display
                 (evil-refresh-cursor ',state)
                 (evil-refresh-mode-line ',state))
               ,@body
               (run-hooks ',entry-hook)
               (when (and evil-echo-state
                          arg (not evil-no-display) ,message)
                 (if (functionp ,message)
                     (funcall ,message)
                   (evil-echo "%s" ,message))))))))

       (evil-add-command-properties ',toggle :keep-visual t :suppress-operator t)

       (evil-define-keymap ,keymap nil
         :mode ,mode
         :func nil)

       (evil-define-keymap ,local-keymap nil
         :mode ,local
         :local t
         :func nil)

       ',state)))

(provide 'evil-core)

;;; evil-core.el ends here
