;;; funcs.el --- Space-macs Layouts Layer functions File -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3


;; General Persp functions

(defun space-macs//activate-persp-mode ()
  "Always activate persp-mode, unless it is already active.
 (e.g. don't re-activate during `dotspace-macs/sync-configuration-layers' -
 see issues #5925 and #3875)"
  (unless (bound-and-true-p persp-mode)
    (persp-mode)
    ;; eyebrowse's advice for rename-buffer only updates workspace window
    ;; configurations that are stored in frame properties, but Space-macs's
    ;; persp-mode integration saves workspace window configurations in
    ;; perspective parameters.  We need to replace eyebrowse's advice with
    ;; perspective-aware advice in order to ensure that window
    ;; configurations for inactive perspectives get updated.
    (when (ad-find-advice 'rename-buffer 'around 'eyebrowse-fixup-window-configs)
      (ad-disable-advice 'rename-buffer 'around 'eyebrowse-fixup-window-configs)
      (ad-activate 'rename-buffer))
    (advice-add 'rename-buffer :around #'space-macs//fixup-window-configs)))

(defun space-macs//layout-wait-for-modeline (&rest _)
  "Assure the mode-line is loaded before restoring the layouts."
  (advice-remove 'persp-load-state-from-file 'space-macs//layout-wait-for-modeline)
  (when (and (configuration-layer/package-used-p 'spaceline)
             (memq (space-macs/get-mode-line-theme-name) '(space-macs all-the-icons custom)))
    (require 'spaceline-config)))

(defun space-macs//current-layout-name ()
  "Get name of the current perspective."
  (safe-persp-name (get-frame-persp)))

(defun space-macs//layout-autosave ()
  "Perspectives mode autosave.
Autosaves perspectives layouts every `persp-autosave-interal' seconds.
Cancels autosave on exiting perspectives mode."
  (if (and persp-mode layouts-enable-autosave)
      (progn
        (message "Perspectives mode autosaving enabled.")
        (setq space-macs--layouts-autosave-timer
              (run-with-timer
               layouts-autosave-delay
               layouts-autosave-delay
               (lambda ()
                 (message "Saving perspectives to file.")
                 (persp-save-state-to-file)))))
    (when space-macs--layouts-autosave-timer
      (cancel-timer space-macs--layouts-autosave-timer)
      (setq space-macs--layouts-autosave-timer nil))))

(defun space-macs//layout-not-contains-buffer-p (buffer)
  "Return non-nil if current layout doesn't contain BUFFER."
  (not (persp-contain-buffer-p buffer)))

(defun space-macs/jump-to-last-layout ()
  "Open the previously selected layout, if it exists."
  (interactive)
  (unless (eq 'non-existent
              (gethash space-macs--last-selected-layout
                       *persp-hash* 'non-existent))
    (persp-switch space-macs--last-selected-layout)))

(defun space-macs-layouts/non-restricted-buffer-list-helm ()
  "Show all buffers accross all layouts."
  (interactive)
  (let ((helm-buffer-list-reorder-fn #'helm-buffers-reorder-buffer-list))
    (helm-mini)))

(defun space-macs-layouts/non-restricted-buffer-list-ivy ()
  (interactive)
  (let ((ivy-ignore-buffers (remove #'space-macs//layout-not-contains-buffer-p ivy-ignore-buffers)))
    (ivy-switch-buffer)))

(defun space-macs-layouts//advice-with-persp-buffer-list (orig-fun &rest args)
  "Advice to provide perp buffer list."
  (with-persp-buffer-list () (apply orig-fun args)))


;; Persp transient-state

(defvar space-macs--persp-display-buffers-func 'ignore
  "Function to display buffers in the perspective.")
(defun space-macs/persp-buffers ()
  "Call the function defined in `space-macs--persp-display-buffers-func'"
  (interactive)
  (call-interactively space-macs--persp-display-buffers-func))

(defvar space-macs--persp-display-perspectives-func 'ignore
  "Function to display perspectives.")
(defun space-macs/persp-perspectives ()
  "Call the function defined in `space-macs--persp-display-perspectives-func'"
  (interactive)
  (call-interactively space-macs--persp-display-perspectives-func))

(defun space-macs//layouts-ts-toggle-hint ()
  "Toggle the full hint docstring for the layouts transient-state."
  (interactive)
  (setq space-macs--layouts-ts-full-hint-toggle
        (not space-macs--layouts-ts-full-hint-toggle)))

(defun space-macs//layout-format-name (name pos)
  "Format the layout name given by NAME for display in mode-line."
  (let* ((layout-name (if (file-directory-p name)
                          (file-name-nondirectory (directory-file-name name))
                        name))
         (string-name (format "%s" layout-name))
         (current (equal name (space-macs//current-layout-name)))
         (caption (concat (number-to-string (if (eq 9 pos) 0 (1+ pos)))
                          ":" string-name)))
    (if current
        (propertize (concat "[" caption "]") 'face 'warning)
      caption)))

(defun space-macs//layouts-ts-hint ()
  "Return a one liner string containing all the layout names."
  (let* ((persp-list (or (persp-names-current-frame-fast-ordered)
                         (list persp-nil-name)))
         (formatted-persp-list
          (concat " "
                  (mapconcat (lambda (persp)
                               (space-macs//layout-format-name
                                persp (cl-position persp persp-list)))
                             persp-list " | "))))
    (concat
     formatted-persp-list
     (if space-macs--layouts-ts-full-hint-toggle
         space-macs--layouts-ts-full-hint
       (concat "  (["
               (propertize "?" 'face 'hydra-face-red)
               "] help)")))))

(defun space-macs//generate-layout-name (pos)
  "Generate name for layout of position POS.
POS should be a number between 1 and 9, where 1 represents the
2nd layout, 2 represents the 3rd and so on. 9 represents the 10th
layout, which is also knows as the 0th layout.

 If no name can be generated, return nil."
  (catch 'found
    ;; return 1st available name
    (dolist (name (nth pos space-macs-generic-layout-names))
      (unless (persp-p (persp-get-by-name name))
        (throw 'found name)))

    ;; return 1st available name from grab-bag
    (dolist (name (car space-macs-generic-layout-names))
      (unless (persp-p (persp-get-by-name name))
        (throw 'found name)))))

(defun space-macs/layout-switch-by-pos (pos)
  "Switch to perspective of position POS.
If POS has no layout, and `dotspace-macs-auto-generate-layout-names'
is non-nil, create layout with auto-generated name. Otherwise,
ask the user if a new layout should be created."
  (let ((persp-to-switch
         (nth pos (persp-names-current-frame-fast-ordered))))
    (if persp-to-switch
        (persp-switch persp-to-switch)
      (let ((persp-reset-windows-on-nil-window-conf t)
            (generated-name (and dotspace-macs-auto-generate-layout-names
                                 (space-macs//generate-layout-name pos))))
        (if generated-name
            (persp-switch generated-name) ; select an existing layout
          (persp-switch nil)              ; create a new layout
          (space-macs/home-delete-other-windows))))))

;; Define all `space-macs/persp-switch-to-X' functions
(dolist (i (number-sequence 9 0 -1))
  (eval `(defun ,(intern (format "space-macs/persp-switch-to-%s" i)) nil
           ,(format "Switch to layout %s.\n%s"
                    i "See `space-macs/layout-switch-by-pos' for details.")
           (interactive)
           (space-macs/layout-switch-by-pos ,(if (eq 0 i) 9 (1- i))))))

(defun space-macs/layout-switch-to (pos)
  "Switch to perspective but ask for POS.
If POS has no layout, and `dotspace-macs-auto-generate-layout-names'
is non-nil, create layout with auto-generated name. Otherwise,
ask the user if a new layout should be created."
  (interactive "NLayout to switch to/create: ")
  (space-macs/layout-switch-by-pos (1- pos)))

(defun space-macs/layout-goto-default ()
  "Go to `dotspace-macs-default-layout-name` layout"
  (interactive)
  (when dotspace-macs-default-layout-name
    (persp-switch dotspace-macs-default-layout-name)))

(defun space-macs/layouts-ts-rename ()
  "Rename a layout and get back to the perspectives transient-state."
  (interactive)
  (call-interactively 'persp-rename)
  (space-macs/layouts-transient-state/body))

(defun space-macs/layouts-ts-close ()
  "Kill current perspective"
  (interactive)
  (persp-kill-without-buffers (space-macs//current-layout-name)))

(defun space-macs/layouts-ts-close-other ()
  (interactive)
  (cond ((configuration-layer/layer-used-p 'helm)
         (space-macs/helm-persp-close))
        ((configuration-layer/layer-used-p 'ivy)
         (space-macs/ivy-space-macs-layout-close-other)))
  (space-macs/layouts-transient-state/body))

(defun space-macs/layouts-ts-kill ()
  "Kill current perspective"
  (interactive)
  (persp-kill (space-macs//current-layout-name)))

(defun space-macs/layouts-ts-kill-other ()
  (interactive)
  (call-interactively 'space-macs/helm-persp-kill)
  (space-macs/layouts-transient-state/body))

(defun space-macs/move-element-left (element list)
  "Move ELEMENT one step to the left in LIST."
  (let (value)
    (dolist (name list value)
      (if (and (equal name element) (car value))
          (setq value (cons (car value) (cons name (cdr value))))
        (setq value (cons name value))))
    (nreverse value)))

(defun space-macs/move-element-right (element list)
  "Move ELEMENT one step to the right in LIST."
  (nreverse (space-macs/move-element-left element (reverse list))))

(defun space-macs/move-current-persp-right ()
  "Moves the current perspective one step to the right"
  (interactive)
  (setq persp-names-cache (space-macs/move-element-right
                           (space-macs//current-layout-name)
                           persp-names-cache)))

(defun space-macs/move-current-persp-left ()
  "Moves the current perspective one step to the left"
  (interactive)
  (setq persp-names-cache (space-macs/move-element-left
                           (space-macs//current-layout-name)
                           persp-names-cache)))


;; Custom Persp transient-state

(defun space-macs//custom-layout-func-name (name)
  "Return the name of the custom-perspective function for NAME."
  (intern (concat "space-macs/custom-perspective-" name)))

(defmacro space-macs|define-custom-layout (name &rest props)
  "Define a custom-perspective called NAME.

FUNC is a FUNCTION defined using NAME and the result of
`space-macs//custom-layout-func-name', it takes care of
creating the perspective NAME and executing the expressions given
in the :body property to this macro.

NAME is a STRING.

Available PROPS:

`:binding STRING'
   Key to be bound to the function FUNC

`:body EXPRESSIONS'
  One or several EXPRESSIONS that are going to be evaluated after
  we change into the perspective NAME."
  (declare (indent 1))
  (let* ((name (if (symbolp name)
                   (symbol-value name)
                 name))
         (func (space-macs//custom-layout-func-name name))
         (binding-prop (car (space-macs/mplist-get-values props :binding)))
         (binding (if (symbolp binding-prop)
                      (symbol-value binding-prop)
                    binding-prop))
         (body (space-macs/mplist-get-values props :body))
         (already-defined? (cdr (assoc binding
                                       space-macs--custom-layout-alist))))
    `(progn
       (defun ,func ()
         ,(format "Open custom perspective %s" name)
         (interactive)
         (let ((initialize (not (gethash ,name *persp-hash*))))
           (persp-switch ,name)
           (when initialize
             (delete-other-windows)
             ,@body)))
       ;; Check for Clashes
       (if ,already-defined?
           (unless (equal ,already-defined? ,name)
             (space-macs-buffer/message "Replacing existing binding \"%s\" for %s with %s"
                                       ,binding ,already-defined? ,name)
             (setq space-macs--custom-layout-alist
                   (delete (assoc ,binding space-macs--custom-layout-alist)
                           space-macs--custom-layout-alist))
             (push '(,binding . ,name) space-macs--custom-layout-alist))
         (push '(,binding . ,name) space-macs--custom-layout-alist)))))

(defun space-macs/select-custom-layout ()
  "Update the custom-perspectives transient-state and then activate it."
  (interactive)
  (space-macs//update-custom-layouts)
  (space-macs/custom-layouts-transient-state/body))

(defun space-macs//custom-layouts-ms-documentation ()
  "Return the docstring for the custom perspectives transient-state."
  (if space-macs--custom-layout-alist
      (mapconcat (lambda (custom-persp)
                   (format "[%s] %s"
                           (car custom-persp) (cdr custom-persp)))
                 space-macs--custom-layout-alist " ")
    (space-macs-buffer/warning (format "`space-macs--custom-layout-alist' variable is empty" ))))

(defun space-macs//update-custom-layouts ()
  "Ensure the custom-perspectives transient-state is updated.
Takes each element in the list `space-macs--custom-layout-alist'
format so they are supported by the
`space-macs/custom-layouts-transient-state' macro."
  (let (bindings)
    (dolist (custom-persp space-macs--custom-layout-alist bindings)
      (let* ((binding (car custom-persp))
             (name (cdr custom-persp))
             (func-name (space-macs//custom-layout-func-name name)))
        (push (list binding func-name :exit t) bindings)))
    (eval `(space-macs|define-transient-state custom-layouts
             :doc (concat (space-macs//custom-layouts-ms-documentation))
             :bindings
             ,@bindings))))


;; Persp and Projectile integration

(defmacro space-macs||switch-layout (name &rest props)
  "Switch to the perspective called NAME.

Available PROPS:

`:init EXPRESSIONS'
    One or more forms, which will be evaluated after switching to perspective
    NAME if the perspective did not already exist."
  (declare (indent 1))
  (let ((init (space-macs/mplist-get-values props :init)))
    `(let ((persp-reset-windows-on-nil-window-conf t)
           (persp-already-exists (persp-with-name-exists-p ,name)))
       (persp-switch ,name)
       (unless persp-already-exists
         ,@init))))

(defun space-macs//create-persp-with-current-project-buffers (name)
  "Create new perspective with project buffers.

If perspective NAME does not already exist, create it and add any
buffers that belong to the current buffer's project."
  (if (persp-with-name-exists-p name)
      (message "There is already a perspective named %s" name)
    (if-let ((project (projectile-project-p)))
        (space-macs||switch-layout name
          :init
          (persp-add-buffer (projectile-project-buffers project)
                            (persp-get-by-name name) nil nil))
      (message "Current buffer does not belong to a project"))))

(defmacro space-macs||switch-project-persp (name &rest body)
  "Switch to persp and execute BODY with hook to add project buffers.

Switch to perspective NAME, and then evaluate the forms in BODY.
If the perspective did not already exist, then BODY will be
evaluated with `projectile-after-switch-project-hook' bound to
add a hook that adds the current project's buffers to the
perspective.  If the user quits during the evaluation of BODY,
the new perspective will be killed."
  (declare (indent 1))
  `(let ((projectile-after-switch-project-hook
          projectile-after-switch-project-hook))
     (space-macs||switch-layout ,name
       :init
       (add-hook 'projectile-after-switch-project-hook
                 (lambda ()
                   (let ((persp (persp-get-by-name ,name)))
                     (when (persp-p persp)
                       (persp-add-buffer (projectile-project-buffers
                                          (expand-file-name ,name))
                                         persp nil nil)))))
       (condition-case nil
           (progn
             ,@body)
         (quit (persp-kill-without-buffers ,name))))))


;; Helm and Ivy common functions

(defun space-macs//create-persp-with-home-buffer (name)
  "Switch to perspective and display the Space-macs home buffer.

If perspective NAME does not already exist, create it and display
the Space-macs home buffer.  If the perspective already exists,
just switch to it."
  (space-macs||switch-layout name :init (space-macs/home)))


;; Helm integration

(defun space-macs/persp-helm-mini ()
  "As `helm-mini' but restricts visible buffers by perspective."
  (interactive)
  (with-persp-buffer-list ()
                          (helm-mini)))

(defun space-macs//helm-perspectives-source ()
  (helm-build-in-buffer-source
      (concat "Current Perspective: " (space-macs//current-layout-name))
    :data (persp-names)
    :fuzzy-match t
    :action
    '(("Switch to perspective" . persp-switch)
      ("Close perspective(s)" . (lambda (candidate)
                                  (mapcar
                                   'persp-kill-without-buffers
                                   (helm-marked-candidates))))
      ("Kill perspective(s)" . (lambda (candidate)
                                 (mapcar 'persp-kill
                                         (helm-marked-candidates)))))))
(defun space-macs/helm-perspectives ()
  "Control Panel for perspectives. Has many actions.
If match is found
f1: (default) Select perspective
f2: Close Perspective(s) <- mark with C-SPC to close more than one-window
f3: Kill Perspective(s)

If match is not found
<enter> Creates perspective

Closing doesn't kill buffers inside the perspective while killing
perspectives does."
  (interactive)
  (helm
   :buffer "*Helm Perspectives*"
   :sources
   `(,(space-macs//helm-perspectives-source)
     ,(helm-build-dummy-source "Create new perspective"
        :action
        '(("Create new perspective" .
           space-macs//create-persp-with-home-buffer)
          ("Create new perspective with buffers from current project" .
           space-macs//create-persp-with-current-project-buffers)
          ("Create new perspective with buffers from current perspective" .
           persp-copy))))))

;; ability to use helm find files but also adds to current perspective
(defun space-macs/helm-persp-close ()
  "Kills perspectives without killing the buffers"
  (interactive)
  (helm
   :buffer "*Helm Kill Perspectives (without killing buffers)*"
   :sources
   (helm-build-in-buffer-source
       (concat "Current Perspective: " (space-macs//current-layout-name))
     :data (persp-names)
     :fuzzy-match t
     :action
     '(("Close perspective(s)" . (lambda (candidate)
                                   (mapcar
                                    'persp-kill-without-buffers
                                    (helm-marked-candidates))))))))

(defun space-macs/helm-persp-kill ()
  "Kills perspectives with all their buffers"
  (interactive)
  (helm
   :buffer "*Helm Kill Perspectives with all their buffers*"
   :sources (helm-build-in-buffer-source
                (s-concat "Current Perspective: "
                          (space-macs//current-layout-name))
              :data (persp-names)
              :fuzzy-match t
              :action
              '(("Kill perspective(s)" .
                 (lambda (candidate)
                   (mapcar 'persp-kill
                           (helm-marked-candidates))))))))

(defun space-macs//helm-persp-switch-project-action (project)
  "Default action for `space-macs/helm-persp-switch-project'."
  (space-macs||switch-project-persp project
    (let ((projectile-completion-system 'helm)
          (helm-quit-hook (append helm-quit-hook
                                  (lambda ()
                                    (persp-kill-without-buffers project)))))
      (projectile-switch-project-by-name project))))

(defun space-macs//helm-persp-switch-project-action-maker (project-action)
  "Make persistent actions for `space-macs/helm-persp-switch-project'.
Run PROJECT-ACTION on project."
  (lambda (project)
    (space-macs||switch-project-persp project
      (let ((projectile-completion-system 'helm)
            (projectile-switch-project-action project-action)
            (helm-quit-hook (append helm-quit-hook
                                    (lambda ()
                                      (persp-kill-without-buffers project)))))
        (projectile-switch-project-by-name project)))))

(defun space-macs/helm-persp-switch-project (arg)
  "Select a project layout using Helm."
  (interactive "P")
  (helm
   :sources
   (helm-build-in-buffer-source "*Helm Switch Project Layout*"
     :data (lambda ()
             (if (projectile-project-p)
                 (cons (abbreviate-file-name (projectile-project-root))
                       (projectile-relevant-known-projects))
               projectile-known-projects))
     :fuzzy-match helm-projectile-fuzzy-match
     :mode-line helm-read-file-name-mode-line-string
     :keymap (let ((map (make-sparse-keymap)))
               (define-key map
                 (kbd "C-d") #'(lambda () (interactive)
                                 (helm-exit-and-execute-action
                                  (lambda (project)
                                    (space-macs||switch-project-persp project
                                      (dired project))))))
               map)
     :action `(("Switch to Project Perspective" .
                space-macs//helm-persp-switch-project-action)
               ("Switch to Project Perspective and Open Dired `C-d'" .
                ,(space-macs//helm-persp-switch-project-action-maker
                  (lambda () (dired "."))))
               ("Switch to Project Perspective and Show Recent Files" .
                ,(space-macs//helm-persp-switch-project-action-maker
                  'helm-projectile-recentf))
               ("Switch to Project Perspective and Search" .
                ,(space-macs//helm-persp-switch-project-action-maker
                  'space-macs/helm-project-smart-do-search))))
   :buffer "*Helm Projectile Layouts*"))

(defun space-macs//make-helm-list-reorder-fn (fn)
  "Take a function `helm-buffer-list-reorder-fn' and return a
`helm-buffer-list-reorder-fn' function.
This the return function will filter out buffers not in layout and then
pass results to FN."
  (lambda (visibles others)
    (funcall fn
             (seq-remove #'space-macs//layout-not-contains-buffer-p visibles)
             (seq-remove #'space-macs//layout-not-contains-buffer-p others))))

(defun space-macs//persp-helm-setup ()
  "Set new `helm-buffer-list-reorder-fn'.
Compose it with a new one that will filter out a buffers on in current layout."
  (let ((my-wrapper (space-macs//make-helm-list-reorder-fn helm-buffer-list-reorder-fn)))
    (setq helm-buffer-list-reorder-fn my-wrapper)))

;; Ivy integration
(defun space-macs//ivy-persp-switch-project-action (project)
  "Default action for `space-macs/ivy-persp-switch-project'."
  (space-macs||switch-project-persp project
    (counsel-projectile-switch-project-action project)))

(defun space-macs/ivy-persp-switch-project (arg)
  "Select a project layout using Ivy."
  (interactive "P")
  (require 'counsel-projectile)
  (ivy-read "Switch to Project Perspective: "
            (if (projectile-project-p)
                (cons (abbreviate-file-name (projectile-project-root))
                      (projectile-relevant-known-projects))
              projectile-known-projects)
            :action #'space-macs//ivy-persp-switch-project-action
            :caller 'space-macs/ivy-persp-switch-project))

(defun space-macs/ivy-switch-project-open-dired (project)
  (interactive)
  (space-macs||switch-project-persp project
    (dired project)))


;; Eyebrowse

;; Eyebrowse uses window-state objects (as returned by `window-state-get') to
;; store window configurations, so here are some utility functions to help us
;; analyse window-states.
;; it might make more sense to move these functions to a more general place

(defun space-macs/window-state-window-p (object)
  "Return t if OBJECT is a window, as represented in window-state objects.
Note: this function doesn't test for real window objects, but for
representations of a window in a window-state object as returned by
`window-state-get'."
  (and (listp object)
       (memq (car object) '(leaf vc hc))))

(defun space-macs/window-state-get-buffer (window)
  "Get WINDOW's buffer.
WINDOW is the representation of a window in a window-state object.
The returned value is the representation of a buffer in a window-state
object."
  (cdr (assq 'buffer window)))

(defun space-macs/window-state-get-buffer-name (window)
  "Get WINDOW's buffer's name.
WINDOW is the representation of a window in a window-state object."
  (car (space-macs/window-state-get-buffer window)))

(defun space-macs/window-state-walk-windows-1 (window fn)
  "Helper function for `space-macs/window-state-walk-windows'."
  ;; WINDOW is a misleading name. WINDOW is a list that can represent a window,
  ;; or a concatenation of several windows. window-state objects are weird.
  (let ((child-windows
         (-filter #'space-macs/window-state-window-p window))
        (bare-window
         ;; if WINDOW contains more than one window, take only the first window
         (--take-while (not (space-macs/window-state-window-p it))
                       window)))
    (--each child-windows
      (space-macs/window-state-walk-windows-1 it fn))
    (push (funcall fn bare-window) result)))

(defun space-macs/window-state-walk-windows (state fn)
  "Execute FN once for each window in STATE and make a list of the results.
FN is a function to execute.
STATE is a window-state object."
  (defvar result) ;; use dynamic binding
  (let (result)
    (space-macs/window-state-walk-windows-1 (cdr state) fn)
    result))

(defun space-macs/window-state-all-windows (state)
  "Get all windows contained in STATE.
STATE is a window-state object.
The returned windows are not actual window objects. They are windows as
represented in window-state objects."
  (space-macs/window-state-walk-windows state #'identity))

(defun space-macs/window-state-get-buffer-names (state)
  "Get names of all buffers saved in STATE.
STATE is a window-state object as returned by `window-state-get'."
  (delq nil (space-macs/window-state-walk-windows state #'space-macs/window-state-get-buffer-name)))

(defun space-macs/window-state-get-buffers (state)
  "Get all buffers saved in STATE.
STATE is a window-state object as returned by `window-state-get'."
  ;; delq nil - removes buffers stored in STATE that don't exist anymore
  (delq nil (mapcar #'get-buffer (space-macs/window-state-get-buffer-names state))))

(defun space-macs/find-workspace (buffer)
  "Find Eyebrowse workspace containing BUFFER.
 If several workspaces contain BUFFER, return the first one. Workspaces are
 ordered by slot number.
 If no workspace contains
 BUFFER, return nil."
  ;; the second element of a workspace is its window-state object
  (--find (memq buffer (space-macs/window-state-get-buffers (cadr it)))
          (eyebrowse--get 'window-configs)))

(defun space-macs/display-in-workspace (buffer alist)
  "Display BUFFER's workspace.
 Return BUFFER's window, if exists, otherwise nil.
 If BUFFER is already visible in current workspace, just return its window
 without switching workspaces."
  (or (get-buffer-window buffer)
      (-when-let (workspace (space-macs/find-workspace buffer))
        (eyebrowse-switch-to-window-config (car workspace))
        (get-buffer-window buffer))))

(defun space-macs/goto-buffer-workspace (buffer)
  "Switch to BUFFER's window in BUFFER's workspace.
 If BUFFER isn't displayed in any workspace, display it in the current
 workspace, preferably in the current window."
  (interactive "B")
  (pop-to-buffer buffer '((;; reuse buffer window from some workspace
                           space-macs/display-in-workspace
                           ;; fallback to display in current window
                           display-buffer-same-window)
                          (inhibit-same-window . nil))))


;; Eyebrowse transient state

(defun space-macs/single-win-workspace ()
  "Create a new single window workspace, and show the Space-macs home buffer."
  (interactive)
  (let ((eyebrowse-new-workspace 'space-macs/home))
    (eyebrowse-create-window-config)))

(defun space-macs//workspaces-ts-toggle-hint ()
  "Toggle the full hint docstring for the workspaces transient-state."
  (interactive)
  (setq space-macs--workspaces-ts-full-hint-toggle
        (not space-macs--workspaces-ts-full-hint-toggle)))

(defun space-macs/workspaces-ts-rename ()
  "Rename a workspace and get back to transient-state."
  (interactive)
  (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) nil)
  (space-macs/workspaces-transient-state/body))

(defun space-macs//workspace-format-name (workspace)
  "Return a propertized string given a WORKSPACE name."
  (let* ((current (eq (eyebrowse--get 'current-slot) (car workspace)))
         (name (nth 2 workspace))
         (number (car workspace))
         (caption (if (< 0 (length name))
                      (concat (int-to-string number) ":" name)
                    (int-to-string number))))
    (if current
        (propertize (concat "[" caption "]") 'face 'warning)
      caption)))

(defun space-macs//workspaces-ts-hint ()
  "Return a one liner string containing all the workspaces names."
  (concat
   " "
   (mapconcat 'space-macs//workspace-format-name
              (eyebrowse--get 'window-configs) " | ")
   (if space-macs--workspaces-ts-full-hint-toggle
       space-macs--workspaces-ts-full-hint
     (concat "  (["
             (propertize "?" 'face 'hydra-face-red)
             "] help)"))))


;; Eyebrowse and Persp integration

(defun space-macs//get-persp-workspace (&optional persp frame)
  "Get the correct workspace parameters for perspective.
PERSP is the perspective, and defaults to the current perspective.
FRAME is the frame where the parameters are expected to be used, and
defaults to the current frame."
  (let ((param-names (if (display-graphic-p frame)
                         '(gui-eyebrowse-window-configs
                           gui-eyebrowse-current-slot
                           gui-eyebrowse-last-slot)
                       '(term-eyebrowse-window-configs
                         term-eyebrowse-current-slot
                         term-eyebrowse-last-slot))))
    (--map (persp-parameter it persp) param-names)))

(defun space-macs//set-persp-workspace (workspace-params &optional persp frame)
  "Set workspace parameters for perspective.
WORKSPACE-PARAMS should be a list containing 3 elements in this order:
- window-configs, as returned by (eyebrowse--get 'window-configs)
- current-slot, as returned by (eyebrowse--get 'current-slot)
- last-slot, as returned by (eyebrowse--get 'last-slot)
PERSP is the perspective, and defaults to the current perspective.
FRAME is the frame where the parameters came from, and defaults to the
current frame.

Each perspective has two sets of workspace parameters: one set for
graphical frames, and one set for terminal frames."
  (let ((param-names (if (display-graphic-p frame)
                         '(gui-eyebrowse-window-configs
                           gui-eyebrowse-current-slot
                           gui-eyebrowse-last-slot)
                       '(term-eyebrowse-window-configs
                         term-eyebrowse-current-slot
                         term-eyebrowse-last-slot))))
    (--zip-with (set-persp-parameter it other persp)
                param-names workspace-params)))

(defun space-macs/load-eyebrowse-for-perspective (type &optional frame)
  "Load an eyebrowse workspace according to a perspective's parameters.
 FRAME's perspective is the perspective that is considered, defaulting to
 the current frame's perspective.
 If the perspective doesn't have a workspace, create one."
  (when (eq type 'frame)
    (let* ((workspace-params (space-macs//get-persp-workspace (get-frame-persp frame) frame))
           (window-configs (nth 0 workspace-params))
           (current-slot (nth 1 workspace-params))
           (last-slot (nth 2 workspace-params)))
      (if window-configs
          (progn
            (eyebrowse--set 'window-configs window-configs frame)
            (eyebrowse--set 'current-slot current-slot frame)
            (eyebrowse--set 'last-slot last-slot frame)
            (eyebrowse--load-window-config current-slot))
        (eyebrowse--set 'window-configs nil frame)
        (eyebrowse-init frame)
        (space-macs/save-eyebrowse-for-perspective frame)))))

(defun space-macs/load-eyebrowse-after-loading-layout (_state-file _phash persp-names)
  "Bridge between `persp-after-load-state-functions' and
`space-macs/load-eyebrowse-for-perspective'.

_PHASH is the hash were the loaded perspectives were placed, and
PERSP-NAMES are the names of these perspectives."
  (let ((cur-persp (get-current-persp)))
    ;; load eyebrowse for current perspective only if it was one of the loaded
    ;; perspectives
    (when (member (or (and cur-persp (persp-name cur-persp))
                      persp-nil-name)
                  persp-names)
      (space-macs/load-eyebrowse-for-perspective 'frame))))

(defun space-macs/update-eyebrowse-for-perspective (&rest _args)
  "Update and save current frame's eyebrowse workspace to its perspective."
  (let* ((current-slot (eyebrowse--get 'current-slot))
         (current-tag (nth 2 (assoc current-slot (eyebrowse--get 'window-configs)))))
    (eyebrowse--update-window-config-element
     (eyebrowse--current-window-config current-slot current-tag)))
  (space-macs/save-eyebrowse-for-perspective))

(defun space-macs/save-eyebrowse-for-perspective (&optional frame)
  "Save FRAME's eyebrowse workspace to FRAME's perspective.
FRAME defaults to the current frame."
  (space-macs//set-persp-workspace (list (eyebrowse--get 'window-configs frame)
                                        (eyebrowse--get 'current-slot frame)
                                        (eyebrowse--get 'last-slot frame))
                                  (get-frame-persp frame)
                                  frame))

(defun space-macs//fixup-window-configs (orig-fn newname &optional unique)
  "Update the buffer's name in the eyebrowse window-configs of any perspectives
containing the buffer."
  (let* ((old (buffer-name))
         (new (funcall orig-fn newname unique)))
    (dolist (persp (persp--buffer-in-persps (current-buffer)))
      (dolist (window-config
               (append (persp-parameter 'gui-eyebrowse-window-configs persp)
                       (persp-parameter 'term-eyebrowse-window-configs persp)))
        (eyebrowse--rename-window-config-buffers window-config old new)))
    new))


;; layout local variables

(defun space-macs/make-variable-layout-local (&rest vars)
  "Make variables become layout-local whenever they are set.
Accepts a list of VARIABLE, DEFAULT-VALUE pairs.

(space-macs/make-variable-layout-local 'foo 1 'bar 2)"
  (cl-loop for (symbol default-value) on vars by 'cddr
           do (add-to-list 'space-macs--layout-local-variables (cons symbol default-value))))

(defun space-macs//load-layout-local-vars (persp-name &rest _)
  "Load the layout-local values of variables for PERSP-NAME."
  (let ((layout-local-vars (-filter 'boundp
                                    (-map 'car
                                          space-macs--layout-local-variables))))
    ;; save the current layout
    (ht-set! space-macs--layout-local-map
             (space-macs//current-layout-name)
             (--map (cons it (symbol-value it))
                    layout-local-vars))
    ;; load the default values into the new layout
    (--each layout-local-vars
      (set it (alist-get it space-macs--layout-local-variables)))
    ;; override with the previously bound values for the new layout
    (--when-let (ht-get space-macs--layout-local-map persp-name)
      (-each it
        (-lambda ((var . val)) (set var val))))))


