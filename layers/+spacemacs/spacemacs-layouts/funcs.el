;;; funcs.el --- Spacemacs Layouts Layer functions File -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; Parts of this file are used with permission under the terms of other
;; GPL-compatible licenses. Specifically, the functions
;; `spacemacs//ediff-in-comparison-buffer-p' and
;; `spacemacs/ediff-balance-windows' are included under the terms of the MIT
;; license: <https://github.com/roman/golden-ratio.el/blob/master/LICENSE>



;; General Persp functions

(defun spacemacs//activate-persp-mode ()
  "Always activate persp-mode, unless it is already active.
 (e.g. don't re-activate during `dotspacemacs/sync-configuration-layers' -
 see issues #5925 and #3875)"
  (unless (bound-and-true-p persp-mode)
    (persp-mode)
    ;; eyebrowse's advice for rename-buffer only updates workspace window
    ;; configurations that are stored in frame properties, but Spacemacs's
    ;; persp-mode integration saves workspace window configurations in
    ;; perspective parameters.  We need to replace eyebrowse's advice with
    ;; perspective-aware advice in order to ensure that window
    ;; configurations for inactive perspectives get updated.
    (when (ad-find-advice 'rename-buffer 'around 'eyebrowse-fixup-window-configs)
      (ad-disable-advice 'rename-buffer 'around 'eyebrowse-fixup-window-configs)
      (ad-activate 'rename-buffer))
    (advice-add 'rename-buffer :around #'spacemacs//fixup-window-configs)))

(defun spacemacs//layout-wait-for-modeline (&rest _)
  "Assure the mode-line is loaded before restoring the layouts."
  (advice-remove 'persp-load-state-from-file 'spacemacs//layout-wait-for-modeline)
  (when (and (configuration-layer/package-used-p 'spaceline)
             (memq (spacemacs/get-mode-line-theme-name) '(spacemacs all-the-icons custom)))
    (require 'spaceline-config)))

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

(defun spacemacs//ediff-in-comparison-buffer-p (&optional buffer)
  "Return non-nil if BUFFER is part of an ediff comparison."
  (with-current-buffer (or buffer (current-buffer))
    (and (boundp 'ediff-this-buffer-ediff-sessions)
         ediff-this-buffer-ediff-sessions)))

(defun spacemacs/ediff-balance-windows ()
  "Balance the width of ediff windows."
  (interactive)
  (ediff-toggle-split)
  (ediff-toggle-split))

(defun spacemacs/jump-to-last-layout ()
  "Open the previously selected layout, if it exists."
  (interactive)
  (unless (eq 'non-existent
              (gethash spacemacs--last-selected-layout
                       *persp-hash* 'non-existent))
    (persp-switch spacemacs--last-selected-layout)))

(defun spacemacs-layouts/non-restricted-buffer-list-helm ()
  "Show all buffers across all layouts."
  (interactive)
  (let ((helm-buffer-list-reorder-fn #'helm-buffers-reorder-buffer-list))
    (helm-mini)))

(defun spacemacs-layouts/non-restricted-buffer-list-ivy ()
  (interactive)
  (let ((ivy-ignore-buffers (remove #'spacemacs//layout-not-contains-buffer-p ivy-ignore-buffers)))
    (ivy-switch-buffer)))

(defmacro spacemacs||with-persp-buffer-list (&rest body)
  "This one is a brute force version of `with-persp-buffer-list'.
It maitains the order of the original `buffer-list'"
  `(cl-letf* ((org-buffer-list
               (symbol-function 'buffer-list))
              ((symbol-function 'buffer-list)
               #'(lambda (&optional frame)
                   (seq-filter
                    #'persp-contain-buffer-p
                    (funcall org-buffer-list frame)))))
     ,@body))

(defun spacemacs-layouts//advice-with-persp-buffer-list (orig-fun &rest args)
  "Advice to provide persp buffer list."
  (spacemacs||with-persp-buffer-list () (apply orig-fun args)))


;; Persp transient-state

(defvar spacemacs--persp-display-buffers-func 'ignore
  "Function to display buffers in the perspective.")
(defun spacemacs/persp-buffers ()
  "Call the function defined in `spacemacs--persp-display-buffers-func'"
  (interactive)
  (call-interactively spacemacs--persp-display-buffers-func))

(defvar spacemacs--persp-display-perspectives-func 'ignore
  "Function to display perspectives.")
(defun spacemacs/persp-perspectives ()
  "Call the function defined in `spacemacs--persp-display-perspectives-func'"
  (interactive)
  (call-interactively spacemacs--persp-display-perspectives-func))

(defun spacemacs//layouts-ts-toggle-hint ()
  "Toggle the full hint docstring for the layouts transient-state."
  (interactive)
  (setq spacemacs--layouts-ts-full-hint-toggle
        (not spacemacs--layouts-ts-full-hint-toggle)))

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
                                persp (cl-position persp persp-list)))
                             persp-list " | "))))
    (concat
     formatted-persp-list
     (if spacemacs--layouts-ts-full-hint-toggle
         spacemacs--layouts-ts-full-hint
       (concat "  (["
               (propertize "?" 'face 'hydra-face-red)
               "] help)")))))

(defun spacemacs//generate-layout-name (pos)
  "Generate name for layout of position POS.
POS should be a number between 1 and 9, where 1 represents the
2nd layout, 2 represents the 3rd and so on. 9 represents the 10th
layout, which is also knows as the 0th layout.

 If no name can be generated, return nil."
  (catch 'found
    ;; return 1st available name
    (dolist (name (nth pos spacemacs-generic-layout-names))
      (unless (persp-p (persp-get-by-name name))
        (throw 'found name)))

    ;; return 1st available name from grab-bag
    (dolist (name (car spacemacs-generic-layout-names))
      (unless (persp-p (persp-get-by-name name))
        (throw 'found name)))))

(defun spacemacs/layout-switch-by-pos (pos)
  "Switch to perspective of position POS.
If POS has no layout, and `dotspacemacs-auto-generate-layout-names'
is non-nil, create layout with auto-generated name. Otherwise,
ask the user if a new layout should be created."
  (let ((persp-to-switch
         (nth pos (persp-names-current-frame-fast-ordered))))
    (if persp-to-switch
        (persp-switch persp-to-switch)
      (let ((persp-reset-windows-on-nil-window-conf t)
            (generated-name (and dotspacemacs-auto-generate-layout-names
                                 (spacemacs//generate-layout-name pos))))
        (if generated-name
            (persp-switch generated-name) ; select an existing layout
          (persp-switch nil)              ; create a new layout
          (spacemacs/home-delete-other-windows))))))

;; Define all `spacemacs/persp-switch-to-X' functions
(dolist (i (number-sequence 9 0 -1))
  (eval `(defun ,(intern (format "spacemacs/persp-switch-to-%s" i)) nil
           ,(format "Switch to layout %s.\n%s"
                    i "See `spacemacs/layout-switch-by-pos' for details.")
           (interactive)
           (spacemacs/layout-switch-by-pos ,(if (eq 0 i) 9 (1- i))))))

(defun spacemacs/layout-switch-to (pos)
  "Switch to perspective but ask for POS.
If POS has no layout, and `dotspacemacs-auto-generate-layout-names'
is non-nil, create layout with auto-generated name. Otherwise,
ask the user if a new layout should be created."
  (interactive "NLayout to switch to/create: ")
  (spacemacs/layout-switch-by-pos (1- pos)))

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
  (cond ((configuration-layer/layer-used-p 'helm)
         (spacemacs/helm-persp-close))
        ((configuration-layer/layer-used-p 'ivy)
         (spacemacs/ivy-spacemacs-layout-close-other)))
  (spacemacs/layouts-transient-state/body))

(defun spacemacs/layouts-ts-kill ()
  "Kill current perspective"
  (interactive)
  (persp-kill (spacemacs//current-layout-name)))

(defun spacemacs/layouts-ts-kill-other ()
  (interactive)
  (call-interactively 'spacemacs/helm-persp-kill)
  (spacemacs/layouts-transient-state/body))

(defun spacemacs/move-element-left (element list)
  "Move ELEMENT one step to the left in LIST."
  (let (value)
    (dolist (name list value)
      (if (and (equal name element) (car value))
          (setq value (cons (car value) (cons name (cdr value))))
        (setq value (cons name value))))
    (nreverse value)))

(defun spacemacs/move-element-right (element list)
  "Move ELEMENT one step to the right in LIST."
  (nreverse (spacemacs/move-element-left element (reverse list))))

(defun spacemacs/move-current-persp-right ()
  "Moves the current perspective one step to the right"
  (interactive)
  (setq persp-names-cache (spacemacs/move-element-right
                           (spacemacs//current-layout-name)
                           persp-names-cache)))

(defun spacemacs/move-current-persp-left ()
  "Moves the current perspective one step to the left"
  (interactive)
  (setq persp-names-cache (spacemacs/move-element-left
                           (spacemacs//current-layout-name)
                           persp-names-cache)))


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
  (when-let* ((name (if (symbolp name)
                        (and (boundp name) (symbol-value name))
                      name))
              (binding-prop (car (spacemacs/mplist-get-values props :binding)))
              (binding (if (symbolp binding-prop)
                           (and (boundp binding-prop) (symbol-value binding-prop))
                         binding-prop)))
    (let* ((func (spacemacs//custom-layout-func-name name))
           (body (spacemacs/mplist-get-values props :body))
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
               (spacemacs-buffer/message "Replacing existing binding \"%s\" for %s with %s"
                                         ,binding ,already-defined? ,name)
               (setq spacemacs--custom-layout-alist
                     (delete (assoc ,binding spacemacs--custom-layout-alist)
                             spacemacs--custom-layout-alist))
               (push '(,binding . ,name) spacemacs--custom-layout-alist))
           (push '(,binding . ,name) spacemacs--custom-layout-alist))))))

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


;; Persp and Projectile integration

(defmacro spacemacs||switch-layout (name &rest props)
  "Switch to the perspective called NAME.

Available PROPS:

`:init EXPRESSIONS'
    One or more forms, which will be evaluated after switching to perspective
    NAME if the perspective did not already exist."
  (declare (indent 1))
  (let ((init (spacemacs/mplist-get-values props :init)))
    `(let ((persp-reset-windows-on-nil-window-conf t)
           (persp-already-exists (persp-with-name-exists-p ,name)))
       (persp-switch ,name)
       (unless persp-already-exists
         ,@init))))

(defun spacemacs//create-persp-with-current-project-buffers (name)
  "Create new perspective with project buffers.

If perspective NAME does not already exist, create it and add any
buffers that belong to the current buffer's project."
  (if (persp-with-name-exists-p name)
      (message "There is already a perspective named %s" name)
    (if-let ((project (projectile-project-p)))
        (spacemacs||switch-layout name
          :init
          (persp-add-buffer (projectile-project-buffers project)
                            (persp-get-by-name name) nil nil))
      (message "Current buffer does not belong to a project"))))

(defmacro spacemacs||switch-project-persp (name &rest body)
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
     (spacemacs||switch-layout ,name
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

(defun spacemacs//create-persp-with-home-buffer (name)
  "Switch to perspective and display the Spacemacs home buffer.

If perspective NAME does not already exist, create it and display
the Spacemacs home buffer.  If the perspective already exists,
just switch to it."
  (spacemacs||switch-layout name :init (spacemacs/home)))


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
        :action
        '(("Create new perspective" .
           spacemacs//create-persp-with-home-buffer)
          ("Create new perspective with buffers from current project" .
           spacemacs//create-persp-with-current-project-buffers)
          ("Create new perspective with buffers from current perspective" .
           persp-copy))))))

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

(defun spacemacs//helm-persp-switch-project-action (project)
  "Default action for `spacemacs/helm-persp-switch-project'."
  (spacemacs||switch-project-persp project
    (let ((projectile-completion-system 'helm)
          (helm-quit-hook (append helm-quit-hook
                                  (lambda ()
                                    (persp-kill-without-buffers project)))))
      (projectile-switch-project-by-name project))))

(defun spacemacs//helm-persp-switch-project-action-maker (project-action)
  "Make persistent actions for `spacemacs/helm-persp-switch-project'.
Run PROJECT-ACTION on project."
  (lambda (project)
    (spacemacs||switch-project-persp project
      (let ((projectile-completion-system 'helm)
            (projectile-switch-project-action project-action)
            (helm-quit-hook (append helm-quit-hook
                                    (lambda ()
                                      (persp-kill-without-buffers project)))))
        (projectile-switch-project-by-name project)))))

(defun spacemacs/helm-persp-switch-project (arg)
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
                 (kbd "C-d") (lambda () (interactive)
                                 (helm-exit-and-execute-action
                                  (lambda (project)
                                    (spacemacs||switch-project-persp project
                                      (dired project))))))
               map)
     :action `(("Switch to Project Perspective" .
                spacemacs//helm-persp-switch-project-action)
               ("Switch to Project Perspective and Open Dired `C-d'" .
                ,(spacemacs//helm-persp-switch-project-action-maker
                  (lambda () (dired "."))))
               ("Switch to Project Perspective and Show Recent Files" .
                ,(spacemacs//helm-persp-switch-project-action-maker
                  'helm-projectile-recentf))
               ("Switch to Project Perspective and Search" .
                ,(spacemacs//helm-persp-switch-project-action-maker
                  'spacemacs/helm-project-smart-do-search))))
   :buffer "*Helm Projectile Layouts*"))

(defun spacemacs//make-helm-list-reorder-fn (fn)
  "Take a function `helm-buffer-list-reorder-fn' and return a
`helm-buffer-list-reorder-fn' function.
This the return function will filter out buffers not in layout and then
pass results to FN."
  (lambda (visibles others)
    (funcall fn
             (seq-remove #'spacemacs//layout-not-contains-buffer-p visibles)
             (seq-remove #'spacemacs//layout-not-contains-buffer-p others))))

(defun spacemacs//persp-helm-setup ()
  "Set new `helm-buffer-list-reorder-fn'.
Compose it with a new one that will filter out a buffers on in current layout."
  (let ((my-wrapper (spacemacs//make-helm-list-reorder-fn helm-buffer-list-reorder-fn)))
    (setq helm-buffer-list-reorder-fn my-wrapper)))

;; Ivy integration
(defun spacemacs//ivy-persp-switch-project-action (project)
  "Default action for `spacemacs/ivy-persp-switch-project'."
  (spacemacs||switch-project-persp project
    (counsel-projectile-switch-project-action project)))

(defun spacemacs/ivy-persp-switch-project (arg)
  "Select a project layout using Ivy."
  (interactive "P")
  (require 'counsel-projectile)
  (ivy-read "Switch to Project Perspective: "
            (if (projectile-project-p)
                (cons (abbreviate-file-name (projectile-project-root))
                      (projectile-relevant-known-projects))
              projectile-known-projects)
            :action #'spacemacs//ivy-persp-switch-project-action
            :caller 'spacemacs/ivy-persp-switch-project))

(defun spacemacs/ivy-switch-project-open-dired (project)
  (interactive)
  (spacemacs||switch-project-persp project
    (dired project)))


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
  (defvar result) ;; use dynamic binding
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

(defun spacemacs//workspace-get-used-slots ()
  (mapcar 'car (eyebrowse--get 'window-configs)))

(defun spacemacs//workspace-next-free-slot ()
  "Get the next free workspace slot."
  (eyebrowse-free-slot (spacemacs//workspace-get-used-slots)))

(defun spacemacs/clone-workspace ()
  "Clone the current workspace."
  (interactive)
  (let ((eyebrowse-new-workspace nil) ; nil = clone current workspace
        (current-slot (eyebrowse--get 'current-slot))
        (next-free-slot (spacemacs//workspace-next-free-slot)))
    (eyebrowse-switch-to-window-config next-free-slot)
    (message "Workspace %s cloned to %s" current-slot next-free-slot)))

(defun spacemacs/new-workspace (&optional slot)
  "Create a new workspace, showing the Spacemacs home buffer.
If a optional SLOT (number) was provided,
then create the new workspace at that slot.
Otherwise create it at the next free slot."
  (let ((eyebrowse-new-workspace 'spacemacs/home)
        (slot (or slot (spacemacs//workspace-next-free-slot))))
    (eyebrowse-switch-to-window-config slot)
    (message "Workspace %s created" slot)))

(defun spacemacs/single-win-workspace ()
  "Create a new single window workspace,
showing the Spacemacs home buffer."
  (interactive)
  (spacemacs/new-workspace))

(defun spacemacs/workspace-switch-or-create (slot)
  "Given a workspace SLOT number.
If SLOT is current, show a message.
If SLOT exists, switch to it.
Otherwise create a new workspace at the next free slot."
  (let* ((slot-current-p (= slot (eyebrowse--get 'current-slot)))
         (slot-exists-p (and (not slot-current-p)
                             (memq slot (spacemacs//workspace-get-used-slots)))))
    (cond (slot-current-p (message "Already on Workspace: %s" slot))
          (slot-exists-p (eyebrowse-switch-to-window-config slot)
                         (message "Workspace switched to: %s" slot))
          (t (spacemacs/new-workspace slot)))))

(defun spacemacs/eyebrowse-switch-to-window-config-0 ()
  (interactive)
  (spacemacs/workspace-switch-or-create 0))

(defun spacemacs/eyebrowse-switch-to-window-config-1 ()
  (interactive)
  (spacemacs/workspace-switch-or-create 1))

(defun spacemacs/eyebrowse-switch-to-window-config-2 ()
  (interactive)
  (spacemacs/workspace-switch-or-create 2))

(defun spacemacs/eyebrowse-switch-to-window-config-3 ()
  (interactive)
  (spacemacs/workspace-switch-or-create 3))

(defun spacemacs/eyebrowse-switch-to-window-config-4 ()
  (interactive)
  (spacemacs/workspace-switch-or-create 4))

(defun spacemacs/eyebrowse-switch-to-window-config-5 ()
  (interactive)
  (spacemacs/workspace-switch-or-create 5))

(defun spacemacs/eyebrowse-switch-to-window-config-6 ()
  (interactive)
  (spacemacs/workspace-switch-or-create 6))

(defun spacemacs/eyebrowse-switch-to-window-config-7 ()
  (interactive)
  (spacemacs/workspace-switch-or-create 7))

(defun spacemacs/eyebrowse-switch-to-window-config-8 ()
  (interactive)
  (spacemacs/workspace-switch-or-create 8))

(defun spacemacs/eyebrowse-switch-to-window-config-9 ()
  (interactive)
  (spacemacs/workspace-switch-or-create 9))

(defun spacemacs/eyebrowse-close-window-config ()
  (interactive)
  (let ((current-workspace (eyebrowse--get 'current-slot))
        (last-workspace-p (= (length (eyebrowse--get 'window-configs)) 1)))
    (if last-workspace-p
        (message "The last workspace can not be closed")
      (eyebrowse-close-window-config)
      (message "Workspace %s closed" current-workspace))))

(defun spacemacs//workspaces-ts-toggle-hint ()
  "Toggle the full hint docstring for the workspaces transient-state."
  (interactive)
  (setq spacemacs--workspaces-ts-full-hint-toggle
        (not spacemacs--workspaces-ts-full-hint-toggle)))

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
   (if spacemacs--workspaces-ts-full-hint-toggle
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

(defun spacemacs//fixup-window-configs (orig-fn newname &optional unique)
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



;; consult compleseus stuff
(defun spacemacs/compleseus-pers-switch-project (arg)
  "Select a project layout using consult."
  (interactive "P")
  (let* ((current-project-maybe (if (projectile-project-p)
                                    (abbreviate-file-name (projectile-project-root))
                                  nil))
         (project (completing-read
                   "Switch to Project Perspective: "
                   (if current-project-maybe
                       (cons current-project-maybe projectile-known-projects)
                     projectile-known-projects)
                   nil
                   nil
                   nil
                   nil
                   current-project-maybe)))
    (spacemacs||switch-project-persp project
      (let ((projectile-switch-project-action (if (string= project current-project-maybe)
                                                  (lambda () nil)
                                                projectile-switch-project-action)))
        (projectile-switch-project-by-name project arg)))))


;; layout local variables

(defun spacemacs/make-variable-layout-local (&rest vars)
  "Make variables become layout-local whenever they are set.
Accepts a list of VARIABLE, DEFAULT-VALUE pairs.

(spacemacs/make-variable-layout-local 'foo 1 'bar 2)"
  (cl-loop for (symbol default-value) on vars by 'cddr
           do (add-to-list 'spacemacs--layout-local-variables (cons symbol default-value))))

(defun spacemacs//load-layout-local-vars (persp-name &rest _)
  "Load the layout-local values of variables for PERSP-NAME."
  (let ((layout-local-vars (-filter 'boundp
                                    (-map 'car
                                          spacemacs--layout-local-variables))))
    ;; save the current layout
    (spacemacs-ht-set! spacemacs--layout-local-map
             (spacemacs//current-layout-name)
             (--map (cons it (symbol-value it))
                    layout-local-vars))
    ;; load the default values into the new layout
    (--each layout-local-vars
      (set it (alist-get it spacemacs--layout-local-variables)))
    ;; override with the previously bound values for the new layout
    (--when-let (spacemacs-ht-get spacemacs--layout-local-map persp-name)
      (-each it
        (-lambda ((var . val)) (set var val))))))
