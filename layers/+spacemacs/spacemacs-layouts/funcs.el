;;; funcs.el --- Spacemacs Layouts Layer functions File
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; General Persp functions

(defun spacemacs//current-layout-name ()
  "Get name of the current perspective."
  (safe-persp-name (get-frame-persp)))

(defun spacemacs//layout-autosave ()
  "Perspectives mode autosave.
Autosaves perspectives layouts every `persp-autosave-interal' seconds.
Cancels autosave on exiting perspectives mode."
  (if (and persp-mode layouts-enable-autosave)
      (progn
        (message "Perspectives mode autosaving enabled.")
        (setq spacemacs--layouts-autosave-timer
              (run-with-timer
               layouts-autosave-delay
               layouts-autosave-delay
               (lambda ()
                 (message "Saving perspectives to file.")
                 (persp-save-state-to-file)))))
    (when spacemacs--layouts-autosave-timer
      (cancel-timer spacemacs--layouts-autosave-timer)
      (setq spacemacs--layouts-autosave-timer nil))))

(defun spacemacs//layout-not-contains-buffer-p (buffer)
  "Return non-nil if current layout doesn't contain BUFFER."
  (not (persp-contain-buffer-p buffer)))

(defun spacemacs/jump-to-last-layout ()
  "Open the previously selected layout, if it exists."
  (interactive)
  (unless (eq 'non-existent
              (gethash spacemacs--last-selected-layout
                       *persp-hash* 'non-existent))
    (persp-switch spacemacs--last-selected-layout)))

(defun spacemacs-layouts/non-restricted-buffer-list-helm ()
  (interactive)
  (let ((ido-make-buffer-list-hook (remove #'persp-restrict-ido-buffers ido-make-buffer-list-hook)))
    (helm-mini)))

(defun spacemacs-layouts/non-restricted-buffer-list-ivy ()
  (interactive)
  (let ((ivy-ignore-buffers (remove #'spacemacs//layout-not-contains-buffer-p ivy-ignore-buffers)))
    (ivy-switch-buffer)))


;; Persp transient-state

(defun spacemacs//layouts-ts-toggle-hint ()
  "Toggle the full hint docstring for the layouts transient-state."
  (interactive)
  (setq spacemacs--ts-full-hint-toggle
        (logxor spacemacs--ts-full-hint-toggle 1)))

(defun spacemacs//layout-format-name (name pos)
  "Format the layout name given by NAME for display in mode-line."
  (let* ((layout-name (if (file-directory-p name)
                          (file-name-nondirectory (directory-file-name name))
                        name))
         (string-name (format "%s" layout-name))
         (current (equal name (spacemacs//current-layout-name)))
         (caption (concat (number-to-string (if (eq 9 pos) 0 (1+ pos)))
                          ":" string-name)))
    (if current
        (propertize (concat "[" caption "]") 'face 'warning)
      caption)))

(defun spacemacs//layouts-ts-hint ()
  "Return a one liner string containing all the layout names."
  (let* ((persp-list (or (persp-names-current-frame-fast-ordered)
                         (list persp-nil-name)))
         (formatted-persp-list
          (concat " "
                  (mapconcat (lambda (persp)
                               (spacemacs//layout-format-name
                                persp (position persp persp-list)))
                             persp-list " | "))))
    (concat
     formatted-persp-list
     (if (equal 1 spacemacs--ts-full-hint-toggle)
         spacemacs--layouts-ts-full-hint
       (concat "  (["
               (propertize "?" 'face 'hydra-face-red)
               "] help)")))))

(defun spacemacs/layout-switch-by-pos (pos)
  "Switch to perspective of position POS."
  (let ((persp-to-switch
         (nth pos (persp-names-current-frame-fast-ordered))))
    (if persp-to-switch
        (persp-switch persp-to-switch)
      (when (y-or-n-p
             (concat "Perspective in this position doesn't exist.\n"
                     "Do you want to create one? "))
        (let ((persp-reset-windows-on-nil-window-conf t))
          (persp-switch nil)
          (spacemacs/home-delete-other-windows))))))

;; Define all `spacemacs/persp-switch-to-X' functions
(dolist (i (number-sequence 9 0 -1))
  (eval `(defun ,(intern (format "spacemacs/persp-switch-to-%s" i)) nil
           ,(format "Switch to layout %s." i)
           (interactive)
           (spacemacs/layout-switch-by-pos ,(if (eq 0 i) 9 (1- i))))))

(defun spacemacs/layout-goto-default ()
  "Go to `dotspacemacs-default-layout-name` layout"
  (interactive)
  (when dotspacemacs-default-layout-name
    (persp-switch dotspacemacs-default-layout-name)))

(defun spacemacs/layouts-ts-rename ()
  "Rename a layout and get back to the perspectives transient-state."
  (interactive)
  (call-interactively 'persp-rename)
  (spacemacs/layouts-transient-state/body))

(defun spacemacs/layouts-ts-close ()
  "Kill current perspective"
  (interactive)
  (persp-kill-without-buffers (spacemacs//current-layout-name)))

(defun spacemacs/layouts-ts-close-other ()
  (interactive)
  (call-interactively 'spacemacs/helm-persp-close)
  (spacemacs/layouts-transient-state/body))

(defun spacemacs/layouts-ts-kill ()
  "Kill current perspective"
  (interactive)
  (persp-kill (spacemacs//current-layout-name)))

(defun spacemacs/layouts-ts-kill-other ()
  (interactive)
  (call-interactively 'spacemacs/helm-persp-kill)
  (spacemacs/layouts-transient-state/body))


;; Custom Persp transient-state

(defun spacemacs//custom-layout-func-name (name)
  "Return the name of the custom-perspective function for NAME."
  (intern (concat "spacemacs/custom-perspective-" name)))

(defmacro spacemacs|define-custom-layout (name &rest props)
  "Define a custom-perspective called NAME.

FUNC is a FUNCTION defined using NAME and the result of
`spacemacs//custom-layout-func-name', it takes care of
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
         (func (spacemacs//custom-layout-func-name name))
         (binding-prop (car (spacemacs/mplist-get props :binding)))
         (binding (if (symbolp binding-prop)
                      (symbol-value binding-prop)
                    binding-prop))
         (body (spacemacs/mplist-get props :body))
         (already-defined? (cdr (assoc binding
                                       spacemacs--custom-layout-alist))))
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
             (spacemacs-buffer/warning "Replacing existing binding \"%s\" for %s with %s"
                                       ,binding ,already-defined? ,name)
             (setq spacemacs--custom-layout-alist
                   (delete (assoc ,binding spacemacs--custom-layout-alist)
                     spacemacs--custom-layout-alist))
             (push '(,binding . ,name) spacemacs--custom-layout-alist))
         (push '(,binding . ,name) spacemacs--custom-layout-alist)))))

(defun spacemacs/select-custom-layout ()
  "Update the custom-perspectives transient-state and then activate it."
  (interactive)
  (spacemacs//update-custom-layouts)
  (spacemacs/custom-layouts-transient-state/body))

(defun spacemacs//custom-layouts-ms-documentation ()
  "Return the docstring for the custom perspectives transient-state."
  (if spacemacs--custom-layout-alist
      (mapconcat (lambda (custom-persp)
                   (format "[%s] %s"
                           (car custom-persp) (cdr custom-persp)))
                 spacemacs--custom-layout-alist " ")
    (spacemacs-buffer/warning (format "`spacemacs--custom-layout-alist' variable is empty" ))))

(defun spacemacs//update-custom-layouts ()
  "Ensure the custom-perspectives transient-state is updated.
Takes each element in the list `spacemacs--custom-layout-alist'
format so they are supported by the
`spacemacs/custom-layouts-transient-state' macro."
  (let (bindings)
    (dolist (custom-persp spacemacs--custom-layout-alist bindings)
      (let* ((binding (car custom-persp))
             (name (cdr custom-persp))
             (func-name (spacemacs//custom-layout-func-name name)))
        (push (list binding func-name :exit t) bindings)))
    (eval `(spacemacs|define-transient-state custom-layouts
             :doc (concat (spacemacs//custom-layouts-ms-documentation))
             :bindings
             ,@bindings))))


;; Helm integration

(defun spacemacs/persp-helm-mini ()
  "As `helm-mini' but restricts visible buffers by perspective."
  (interactive)
  (with-persp-buffer-list ()
                          (helm-mini)))

(defun spacemacs//helm-perspectives-source ()
  (helm-build-in-buffer-source
      (concat "Current Perspective: " (spacemacs//current-layout-name))
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
(defun spacemacs/helm-perspectives ()
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
   `(,(spacemacs//helm-perspectives-source)
     ,(helm-build-dummy-source "Create new perspective"
        :requires-pattern t
        :action
        '(("Create new perspective" .
           (lambda (name)
             (let ((persp-reset-windows-on-nil-window-conf t))
               (persp-switch name)
               (unless (member name (persp-names-current-frame-fast-ordered))
                 (spacemacs/home))))))))))

;; ability to use helm find files but also adds to current perspective
(defun spacemacs/helm-persp-close ()
  "Kills perspectives without killing the buffers"
  (interactive)
  (helm
   :buffer "*Helm Kill Perspectives (without killing buffers)*"
   :sources
   (helm-build-in-buffer-source
       (concat "Current Perspective: " (spacemacs//current-layout-name))
     :data (persp-names)
     :fuzzy-match t
     :action
     '(("Close perspective(s)" . (lambda (candidate)
                                   (mapcar
                                    'persp-kill-without-buffers
                                    (helm-marked-candidates))))))))

(defun spacemacs/helm-persp-kill ()
  "Kills perspectives with all their buffers"
  (interactive)
  (helm
   :buffer "*Helm Kill Perspectives with all their buffers*"
   :sources (helm-build-in-buffer-source
                (s-concat "Current Perspective: "
                          (spacemacs//current-layout-name))
              :data (persp-names)
              :fuzzy-match t
              :action
              '(("Kill perspective(s)" .
                 (lambda (candidate)
                   (mapcar 'persp-kill
                           (helm-marked-candidates))))))))

(defun spacemacs/helm-persp-switch-project (arg)
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
     :action '(("Switch to Project Perspective" .
                (lambda (project)
                  (let ((persp-reset-windows-on-nil-window-conf t))
                    (persp-switch project)
                    (let ((projectile-completion-system 'helm))
                      (projectile-switch-project-by-name project)))))))
   :buffer "*Helm Projectile Layouts*"))


;; Ivy integration

(defun spacemacs/ivy-persp-switch-project (arg)
  (interactive "P")
  (ivy-read "Switch to Project Perspective: "
            (if (projectile-project-p)
                (cons (abbreviate-file-name (projectile-project-root))
                      (projectile-relevant-known-projects))
              projectile-known-projects)
            :action (lambda (project)
                      (let ((persp-reset-windows-on-nil-window-conf t))
                        (persp-switch project)
                        (let ((projectile-completion-system 'ivy))
                          (projectile-switch-project-by-name project))))))


;; Eyebrowse

;; Eyebrowse uses window-state objects (as returned by `window-state-get') to
;; store window configurations, so here are some utility functions to help us
;; analyse window-states.
;; it might make more sense to move these functions to a more general place

(defun spacemacs/window-state-window-p (object)
  "Return t if OBJECT is a window, as represented in window-state objects.
Note: this function doesn't test for real window objects, but for
representations of a window in a window-state object as returned by
`window-state-get'."
  (and (listp object)
       (memq (car object) '(leaf vc hc))))

(defun spacemacs/window-state-get-buffer (window)
  "Get WINDOW's buffer.
WINDOW is the representation of a window in a window-state object.
The returned value is the representation of a buffer in a window-state
object."
  (cdr (assq 'buffer window)))

(defun spacemacs/window-state-get-buffer-name (window)
  "Get WINDOW's buffer's name.
WINDOW is the representation of a window in a window-state object."
  (car (spacemacs/window-state-get-buffer window)))

(defun spacemacs/window-state-walk-windows-1 (window fn)
  "Helper function for `spacemacs/window-state-walk-windows'."
  ;; WINDOW is a misleading name. WINDOW is a list that can represent a window,
  ;; or a concatenation of several windows. window-state objects are weird.
  (let ((child-windows
         (-filter #'spacemacs/window-state-window-p window))
        (bare-window
         ;; if WINDOW contains more than one window, take only the first window
         (--take-while (not (spacemacs/window-state-window-p it))
                       window)))
    (--each child-windows
      (spacemacs/window-state-walk-windows-1 it fn))
    (push (funcall fn bare-window) result)))

(defun spacemacs/window-state-walk-windows (state fn)
  "Execute FN once for each window in STATE and make a list of the results.
FN is a function to execute.
STATE is a window-state object."
  (let (result)
    (spacemacs/window-state-walk-windows-1 (cdr state) fn)
    result))

(defun spacemacs/window-state-all-windows (state)
  "Get all windows contained in STATE.
STATE is a window-state object.
The returned windows are not actual window objects. They are windows as
represented in window-state objects."
  (spacemacs/window-state-walk-windows state #'identity))

(defun spacemacs/window-state-get-buffer-names (state)
  "Get names of all buffers saved in STATE.
STATE is a window-state object as returned by `window-state-get'."
  (delq nil (spacemacs/window-state-walk-windows state #'spacemacs/window-state-get-buffer-name)))

(defun spacemacs/window-state-get-buffers (state)
  "Get all buffers saved in STATE.
STATE is a window-state object as returned by `window-state-get'."
  ;; delq nil - removes buffers stored in STATE that don't exist anymore
  (delq nil (mapcar #'get-buffer (spacemacs/window-state-get-buffer-names state))))

(defun spacemacs/find-workspace (buffer)
  "Find Eyebrowse workspace containing BUFFER.
 If several workspaces contain BUFFER, return the first one. Workspaces are
 ordered by slot number.
 If no workspace contains
 BUFFER, return nil."
  ;; the second element of a workspace is its window-state object
  (--find (memq buffer (spacemacs/window-state-get-buffers (cadr it)))
          (eyebrowse--get 'window-configs)))

(defun spacemacs/display-in-workspace (buffer alist)
  "Display BUFFER's workspace.
 Return BUFFER's window, if exists, otherwise nil.
 If BUFFER is already visible in current workspace, just return its window
 without switching workspaces."
  (or (get-buffer-window buffer)
      (-when-let (workspace (spacemacs/find-workspace buffer))
        (eyebrowse-switch-to-window-config (car workspace))
        (get-buffer-window buffer))))

(defun spacemacs/goto-buffer-workspace (buffer)
  "Switch to BUFFER's window in BUFFER's workspace.
 If BUFFER isn't displayed in any workspace, display it in the current
 workspace, preferably in the current window."
  (interactive "B")
  (pop-to-buffer buffer '((;; reuse buffer window from some workspace
                           spacemacs/display-in-workspace
                           ;; fallback to display in current window
                           display-buffer-same-window)
                          (inhibit-same-window . nil))))


;; Eyebrowse transient state

(defun spacemacs//workspaces-ts-toggle-hint ()
  "Toggle the full hint docstring for the workspaces transient-state."
  (interactive)
  (setq spacemacs--ts-full-hint-toggle
        (logxor spacemacs--ts-full-hint-toggle 1)))

(defun spacemacs/workspaces-ts-rename ()
  "Rename a workspace and get back to transient-state."
  (interactive)
  (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) nil)
  (spacemacs/workspaces-transient-state/body))

(defun spacemacs//workspace-format-name (workspace)
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

(defun spacemacs//workspaces-ts-hint ()
  "Return a one liner string containing all the workspaces names."
  (concat
   " "
   (mapconcat 'spacemacs//workspace-format-name
              (eyebrowse--get 'window-configs) " | ")
   (if (equal 1 spacemacs--ts-full-hint-toggle)
       spacemacs--workspaces-ts-full-hint
     (concat "  (["
             (propertize "?" 'face 'hydra-face-red)
             "] help)"))))


;; Eyebrowse and Persp integration

(defun spacemacs//get-persp-workspace (&optional persp frame)
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

(defun spacemacs//set-persp-workspace (workspace-params &optional persp frame)
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

(defun spacemacs/load-eyebrowse-for-perspective (type &optional frame)
  "Load an eyebrowse workspace according to a perspective's parameters.
 FRAME's perspective is the perspective that is considered, defaulting to
 the current frame's perspective.
 If the perspective doesn't have a workspace, create one."
  (when (eq type 'frame)
    (let* ((workspace-params (spacemacs//get-persp-workspace (get-frame-persp frame) frame))
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
        (spacemacs/save-eyebrowse-for-perspective frame)))))

(defun spacemacs/load-eyebrowse-after-loading-layout (_state-file _phash persp-names)
  "Bridge between `persp-after-load-state-functions' and
`spacemacs/load-eyebrowse-for-perspective'.

_PHASH is the hash were the loaded perspectives were placed, and
PERSP-NAMES are the names of these perspectives."
  (let ((cur-persp (get-current-persp)))
    ;; load eyebrowse for current perspective only if it was one of the loaded
    ;; perspectives
    (when (member (or (and cur-persp (persp-name cur-persp))
                      persp-nil-name)
                  persp-names)
      (spacemacs/load-eyebrowse-for-perspective 'frame))))

(defun spacemacs/update-eyebrowse-for-perspective (&rest _args)
  "Update and save current frame's eyebrowse workspace to its perspective."
  (let* ((current-slot (eyebrowse--get 'current-slot))
         (current-tag (nth 2 (assoc current-slot (eyebrowse--get 'window-configs)))))
    (eyebrowse--update-window-config-element
     (eyebrowse--current-window-config current-slot current-tag)))
  (spacemacs/save-eyebrowse-for-perspective))

(defun spacemacs/save-eyebrowse-for-perspective (&optional frame)
  "Save FRAME's eyebrowse workspace to FRAME's perspective.
FRAME defaults to the current frame."
  (spacemacs//set-persp-workspace (list (eyebrowse--get 'window-configs frame)
                                        (eyebrowse--get 'current-slot frame)
                                        (eyebrowse--get 'last-slot frame))
                                  (get-frame-persp frame)
                                  frame))
