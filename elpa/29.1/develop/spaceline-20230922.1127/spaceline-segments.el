;;; spaceline-segments.el --- Segments for spaceline

;; This file is not part of GNU Emacs.

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

;; This file contains a variety of segments that may be of general interest in
;; many people's modelines.  It contains both "stock" segments which are usable
;; without any additional packages, as well as a number of segments which depend
;; on optional third-party packages.

;; Note: The `global' segment is defined in spaceline.el, not here.  It's the
;; only exception.

;;; Code:

(require 'spaceline)
(require 's)

(defvar evil-state)
(defvar evil-visual-selection)

;;; Stock segments - no optional dependencies
;;  =========================================

(defvar spaceline-minor-modes-separator "|")
(spaceline-define-segment minor-modes
  "A list of minor modes. Configure the separator with
`spaceline-minor-modes-separator'."
  (-filter
   (lambda (k) (and k (not (string= k ""))))
   (mapcar (lambda (mm)
             (let* ((displayp (and (boundp (car mm))
                                   (symbol-value (car mm))))
                    (lighter (when displayp
                               (s-trim (format-mode-line (cadr mm)))))
                    (displayp (and lighter (not (string= "" lighter)))))
               (when displayp
                 (propertize
                  lighter
                  'mouse-face 'mode-line-highlight
                  'help-echo (concat (symbol-name (car mm))
                                     "\nmouse-1: Display minor mode menu"
                                     "\nmouse-2: Show help for minor mode"
                                     "\nmouse-3: Toggle minor mode")
                  'local-map (let ((map (make-sparse-keymap)))
                               (define-key map
                                 [mode-line mouse-1]
                                 (powerline-mouse 'minor 'menu lighter))
                               (define-key map
                                 [mode-line mouse-2]
                                 (powerline-mouse 'minor 'help lighter))
                               (define-key map
                                 [mode-line mouse-3]
                                 (powerline-mouse 'minor 'menu lighter))
                               (define-key map
                                 [header-line down-mouse-3]
                                 (powerline-mouse 'minor 'menu lighter))
                               map)))))
           minor-mode-alist))
  :separator spaceline-minor-modes-separator)

(spaceline-define-segment buffer-modified
  "Buffer modified marker."
  "%*")

(spaceline-define-segment buffer-size
  "Size of buffer."
  (if (bound-and-true-p total-lines-mode)
      (format "%d" total-lines)
    (powerline-buffer-size)))

(defcustom spaceline-buffer-id-max-length 45
  "The maximum displayed length of the buffer-id segment."
  :type 'integer
  :group 'spaceline)

(spaceline-define-segment buffer-id
  "Name of buffer."
  (spaceline--string-trim-from-center
   (s-trim (powerline-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive)))
   spaceline-buffer-id-max-length))

(spaceline-define-segment remote-host
  "Hostname for remote buffers."
  (when (and default-directory
             (file-remote-p default-directory 'host))
    (concat "@" (file-remote-p default-directory 'host))))

(spaceline-define-segment major-mode
  "The name of the major mode."
  (powerline-major-mode))

(spaceline-define-segment process
  "The process associated with this buffer, if any."
  (when (spaceline--mode-line-nonempty mode-line-process)
    (s-trim (powerline-raw mode-line-process))))

(spaceline-define-segment version-control-simple
  "Simplified version control information."
  (when vc-mode
    (powerline-raw
     (when (buffer-file-name)
       (pcase (vc-state (buffer-file-name))
         (`up-to-date " ")
         (`ignored " ")
         (`edited "~")
         (`added "+")
         (`removed "-")
         (`needs-merge "!")
         (`needs-update "!")
         (_ "?"))))))

(spaceline-define-segment version-control
  "Version control information."
  (when vc-mode
    (powerline-raw
     (s-trim (concat vc-mode
                     (when (buffer-file-name)
                       (pcase (vc-state (buffer-file-name))
                         (`up-to-date " ")
                         (`edited " Mod")
                         (`added " Add")
                         (`unregistered " ??")
                         (`removed " Del")
                         (`needs-merge " Con")
                         (`needs-update " Upd")
                         (`ignored " Ign")
                         (_ " Unk"))))))))

(spaceline-define-segment buffer-encoding
  "The full `buffer-file-coding-system'."
  (format "%s" buffer-file-coding-system))

(spaceline-define-segment buffer-encoding-abbrev
  "The line ending convention used in the buffer."
  (let ((buf-coding (format "%s" buffer-file-coding-system)))
    (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
        (match-string 1 buf-coding)
      buf-coding)))

(spaceline-define-segment point-position
  "The value of `(point)'."
  (format "%d" (point))
  :enabled nil)

(spaceline-define-segment line
  "The current line number."
  "%l")

(spaceline-define-segment column
  "The current column number."
  (if (and
        (boundp 'column-number-indicator-zero-based)
        (not column-number-indicator-zero-based))
    "%2C"
    "%2c"))

(defun spaceline--docview-page-number ()
  "The current `doc-view-mode' page number to display in the mode-line.
Return a formated string containing the current and last page number for the
currently displayed pdf file in `doc-view-mode'."
  (format "(%d/%d)"
          (image-mode-window-get 'page nil)
          (doc-view-last-page-number)))

(declare-function pdf-view-current-page 'pdf-view)
(declare-function pdf-cache-number-of-pages 'pdf-view)

(defun spaceline--pdfview-page-number ()
  "The current `pdf-view-mode' page number to display in the mode-line.
Return a formated string containing the current and last page number for the
currently displayed pdf file in `pdf-view-mode'."
  (format "(%d/%d)"
          ;; `pdf-view-current-page' is a macro in an optional dependency
          ;; any better solutions?
          (eval `(pdf-view-current-page))
          (pdf-cache-number-of-pages)))

(spaceline-define-segment line-column
  "The current line and column numbers, or `(current page/number of pages)`
in pdf-view mode (enabled by the `pdf-tools' package)."
  (cond ((eq major-mode 'doc-view-mode)
         (spaceline--docview-page-number))
        ((eq major-mode 'pdf-view-mode)
         (spaceline--pdfview-page-number))
        (t (if (and
                (boundp 'column-number-indicator-zero-based)
                (not column-number-indicator-zero-based))
               "%l:%2C"
             "%l:%2c"))))

(spaceline-define-segment buffer-position
  "The current approximate buffer position, in percent."
  "%p")

(defun spaceline--column-number-at-pos (pos)
  "Column number at POS.  Analog to `line-number-at-pos'."
  (save-excursion (goto-char pos) (current-column)))

(spaceline-define-segment selection-info
  "Information about the size of the current selection, when applicable.
Supports both Emacs and Evil cursor conventions."
  (when (or mark-active
            (and (bound-and-true-p evil-local-mode)
                 (eq 'visual evil-state)))
    (let* ((lines (count-lines (region-beginning) (min (1+ (region-end)) (point-max))))
           (chars (- (1+ (region-end)) (region-beginning)))
           (cols (1+ (abs (- (spaceline--column-number-at-pos (region-end))
                             (spaceline--column-number-at-pos (region-beginning))))))
           (evil (and (bound-and-true-p evil-state) (eq 'visual evil-state)))
           (rect (or (bound-and-true-p rectangle-mark-mode)
                     (and evil (eq 'block evil-visual-selection))))
           (multi-line (or (> lines 1) (and evil (eq 'line evil-visual-selection)))))
      (cond
       (rect (format "%d×%d block" lines (if evil cols (1- cols))))
       (multi-line (format "%d lines" lines))
       (t (format "%d chars" (if evil chars (1- chars))))))))

(defcustom spaceline-show-default-input-method nil
  "Whether to show the default input method in `input-method'.

When non-nil, show the default input method in the `input-method'
segment.  Otherwise only show the active input method, if any."
  :type 'boolean
  :group 'spaceline
  :risky t)

(spaceline-define-segment input-method
  "The current input method, or the default input method."
  (when (or current-input-method
            (and (bound-and-true-p evil-mode)
                 (bound-and-true-p evil-input-method))
            (and spaceline-show-default-input-method
                 default-input-method))
    (cond
     (current-input-method
      (propertize current-input-method-title 'face 'bold))
     ;; `evil-input-method' is where evil remembers the input method while in
     ;; normal state.  The input method is not active in normal state, but Evil
     ;; will enable this input method again when switching to insert/emacs state.
     ((and (bound-and-true-p evil-mode) (bound-and-true-p evil-input-method))
      (nth 3 (assoc default-input-method input-method-alist)))
     ((and spaceline-show-default-input-method default-input-method)
      (propertize (nth 3 (assoc default-input-method input-method-alist))
                  'face 'italic)))))

(spaceline-define-segment hud
  "A HUD that shows which part of the buffer is currently visible."
  (powerline-hud highlight-face default-face)
  :tight t)

;;; Helm segments
;;  =============

(declare-function helm-candidate-number-at-point 'helm)
(declare-function helm-get-candidate-number 'helm)

(defvar spaceline--helm-buffer-ids
  '(("*helm*" . "HELM")
    ("*helm M-x*" . "HELM M-x")
    ("*swiper*" . "SWIPER")
    ("*Projectile Perspectives*" . "HELM Projectile Perspectives")
    ("*Projectile Layouts*" . "HELM Projectile Layouts")
    ("*helm-ag*" . (lambda ()
                     (format "HELM Ag: Using %s"
                             (car (split-string helm-ag-base-command))))))
  "Alist of custom helm buffer names to use.
The cdr can also be a function that returns a name to use.")
(spaceline-define-segment helm-buffer-id
  "Helm session identifier."
  (when (bound-and-true-p helm-alive-p)
    (propertize
     (let ((custom (cdr (assoc (buffer-name) spaceline--helm-buffer-ids)))
           (case-fold-search t)
           (name (replace-regexp-in-string "-" " " (buffer-name))))
       (cond ((stringp custom) custom)
             ((functionp custom) (funcall custom))
             (t
              (string-match "\\*helm:? \\(mode \\)?\\([^\\*]+\\)\\*" name)
              (concat "HELM " (capitalize (match-string 2 name))))))
     'face 'bold)))

(spaceline-define-segment helm-done
  "Done."
  (propertize "(DONE)" 'face 'bold))

(spaceline-define-segment helm-number
  "Number of helm candidates."
  (when (bound-and-true-p helm-alive-p)
    (format "%d/%s (%s total)"
            (helm-candidate-number-at-point)
            (helm-get-candidate-number t)
            (helm-get-candidate-number))))

(spaceline-define-segment helm-help
  "Helm keybindings help."
  (when (bound-and-true-p helm-alive-p)
    (-interleave
     (mapcar (lambda (s)
               (propertize (substitute-command-keys s) 'face 'bold))
             '("\\<helm-map>\\[helm-help]"
               "\\<helm-map>\\[helm-select-action]"
               "\\<helm-map>\\[helm-maybe-exit-minibuffer]/F1/F2..."))
     '("(help)" "(actions)" "(action)"))))

(spaceline-define-segment helm-prefix-argument
  "Helm prefix argument."
  (when (and (bound-and-true-p helm-alive-p)
             helm--mode-line-display-prefarg)
    (let ((arg (prefix-numeric-value (or prefix-arg current-prefix-arg))))
      (unless (= arg 1)
        (propertize (format "C-u %s" arg) 'face 'helm-prefarg)))))

(defvar spaceline--helm-current-source nil
  "The currently active helm source.")
(spaceline-define-segment helm-follow
  "Helm follow indicator."
  (when (and (bound-and-true-p helm-alive-p)
             spaceline--helm-current-source
             (eq 1 (cdr (assq 'follow spaceline--helm-current-source))))
    "HF"))

;;; Info segments
;;  =============

(defvar spaceline--info-topic nil
  "Topic for the info modeline.")
(spaceline-define-segment info-topic
  "Topic for the info modeline."
  spaceline--info-topic)
(defvar spaceline--info-nodes nil
  "Breadcrumbs for the info modeline.")
(spaceline-define-segment info-nodes
  "Breadcrumbs for the info modeline."
  spaceline--info-nodes)

;;; Segments requiring optional dependencies
;;  ========================================

(defvar conda-env-current-name)
(defvar erc-modified-channels-object)
(defvar erc-track-position-in-mode-line)
(defvar fancy-battery-last-status)
(defvar fancy-battery-show-percentage)
(defvar mu4e-alert-mode-line)
(defvar org-pomodoro-mode-line)
(defvar pyvenv-virtual-env)
(defvar pyvenv-virtual-env-name)
(defvar which-func-current)
(defvar which-func-keymap)

(declare-function projectile-project-name 'projectile)
(declare-function anzu--update-mode-line 'anzu)
(declare-function evil-state-property 'evil-common)
(declare-function eyebrowse--get 'eyebrowse)
(declare-function mode-line-auto-compile-control 'auto-compile)
(declare-function nyan-create 'nyan-mode)
(declare-function safe-persp-name 'persp-mode)
(declare-function get-frame-persp 'persp-mode)
(declare-function winum-get-number 'winum)
(declare-function window-numbering-get-number 'window-numbering)
(declare-function purpose--modeline-string 'window-purpose)
(declare-function purpose-window-purpose-dedicated-p 'window-purpose)
(declare-function pyenv-mode-version 'pyenv-mode)
(declare-function pyenv-mode-full-path 'pyenv-mode)
(declare-function doc-view-last-page-number 'doc-view)
(declare-function image-mode-window-get 'image-mode)

(spaceline-define-segment projectile-root
  "Show the current projectile root."
  (when (fboundp 'projectile-project-name)
    (let ((project-name (projectile-project-name)))
      (unless (or (string= project-name "-")
                  (string= project-name (buffer-name)))
        project-name))))

(spaceline-define-segment anzu
  "Show the current match number and the total number of matches.
Requires anzu to be enabled."
  (when (and active (bound-and-true-p anzu--state))
    (anzu--update-mode-line)))

(spaceline-define-segment auto-compile
  "Show the count of byte-compiler warnings from the auto-compile
package."
  (when (and active (boundp 'auto-compile-warnings) (> auto-compile-warnings 0))
    (mode-line-auto-compile-control)))

(spaceline-define-segment erc-track
  "Show the ERC buffers with new messages. Requires
`erc-track-mode' to be enabled and
`erc-track-position-in-mode-line' to be set to true."
  (when (and (bound-and-true-p erc-track-mode)
             erc-track-position-in-mode-line
             erc-modified-channels-object)
    (s-trim erc-modified-channels-object))
  :global-override erc-modified-channels-object)

(defun spaceline--fancy-battery-percentage ()
  "Return the load percentage or an empty string."
  (let ((p (cdr (assq ?p fancy-battery-last-status))))
    (if (and fancy-battery-show-percentage
             p (not (string= "N/A" p))) (concat " " p "%%") "")))

(defun spaceline--fancy-battery-time ()
  "Return the remaining time complete load or discharge."
  (let ((time (cdr (assq ?t fancy-battery-last-status))))
    (cond
     ((string= "0:00" time) "")
     ((string= "N/A" time) "")
     ((string= time "") "")
     (t (concat " (" time ")")))))

(defun spaceline--fancy-battery-mode-line ()
  "Assemble a mode line string for Fancy Battery Mode."
  (when fancy-battery-last-status
    (let* ((type (cdr (assq ?L fancy-battery-last-status)))
           (percentage (spaceline--fancy-battery-percentage))
           (time (spaceline--fancy-battery-time)))
      (cond
       ((string= "on-line" type) " No Battery")
       ((string= type "") " No Battery")
       (t (concat (if (string= "AC" type) " AC" "") percentage time))))))

(defun spaceline--fancy-battery-face ()
  "Return a face appropriate for powerline."
  (let ((type (cdr (assq ?L fancy-battery-last-status))))
    (if (and type (string= "AC" type))
        'fancy-battery-charging
      (pcase (cdr (assq ?b fancy-battery-last-status))
        ("!"  'fancy-battery-critical)
        ("+"  'fancy-battery-charging)
        ("-"  'fancy-battery-discharging)
        (_ 'fancy-battery-discharging)))))

(spaceline-define-segment battery
  "Show battery information.  Requires `fancy-battery-mode' to be enabled.

This segment overrides the modeline functionality of
`fancy-battery-mode'."
  (when (bound-and-true-p fancy-battery-mode)
    (let ((text (spaceline--fancy-battery-mode-line)))
      (and text (powerline-raw (s-trim text)
                               (spaceline--fancy-battery-face)))))
  :global-override fancy-battery-mode-line)

(defvar spaceline-org-clock-format-function
  'org-clock-get-clock-string
  "The function called by the `org-clock' segment to determine what to show.")

(spaceline-define-segment org-clock
  "Show information about the current org clock task.  Configure
`spaceline-org-clock-format-function' to configure. Requires a currently running
org clock.

This segment overrides the modeline functionality of `org-mode-line-string'."
  (when (and (fboundp 'org-clocking-p)
             (org-clocking-p))
    (substring-no-properties (funcall spaceline-org-clock-format-function)))
  :global-override org-mode-line-string)

(spaceline-define-segment org-pomodoro
  "Shows the current pomodoro.  Requires `org-pomodoro' to be active.

This segment overrides the modeline functionality of `org-pomodoro' itself."
  (when (and (fboundp 'org-pomodoro-active-p)
             (org-pomodoro-active-p))
    (nth 1 org-pomodoro-mode-line))
  :global-override org-pomodoro-mode-line)

(spaceline-define-segment nyan-cat
  "Shows the infamous nyan cat.  Requires `nyan-mode' to be enabled."
  (when (bound-and-true-p nyan-mode)
    (powerline-raw (nyan-create) default-face)))

(defun spaceline--unicode-number (str)
  "Return a nice unicode representation of a single-digit number STR."
  (cond
   ((string= "1" str) "➊")
   ((string= "2" str) "➋")
   ((string= "3" str) "➌")
   ((string= "4" str) "➍")
   ((string= "5" str) "➎")
   ((string= "6" str) "➏")
   ((string= "7" str) "➐")
   ((string= "8" str) "➑")
   ((string= "9" str) "➒")
   ((string= "10" str) "➓")
   (t str)))

(defvar spaceline-window-numbers-unicode nil
  "Set to true to enable unicode display in the `window-number' segment.")

(spaceline-define-segment window-number
  "The current window number.
Requires either `winum-mode' or `window-numbering-mode' to be enabled."
  (let* ((num (cond
               ((bound-and-true-p winum-mode)
                (winum-get-number))
               ((bound-and-true-p window-numbering-mode)
                (window-numbering-get-number))
               (t nil)))
         (str (when num (int-to-string num))))
    (when num
      (if spaceline-window-numbers-unicode
          (spaceline--unicode-number str)
        (propertize str 'face 'bold)))))

(defvar spaceline-workspace-numbers-unicode nil
  "Set to true to enable unicode display in the `workspace-number' segment.")

(spaceline-define-segment workspace-number
  "The current workspace name or number. Requires `eyebrowse-mode' to be
enabled."
  (when (and (bound-and-true-p eyebrowse-mode)
             (< 1 (length (eyebrowse--get 'window-configs))))
    (let* ((num (eyebrowse--get 'current-slot))
           (tag (when num (nth 2 (assoc num (eyebrowse--get 'window-configs)))))
           (str (if (and tag (< 0 (length tag)))
                    tag
                  (when num (int-to-string num)))))
      (or (when spaceline-workspace-numbers-unicode
            (spaceline--unicode-number str))
          (propertize str 'face 'bold)))))

(defvar spaceline-display-default-perspective nil
  "If non-nil, the default perspective name is displayed in the mode-line.")

(spaceline-define-segment persp-name
  "The current perspective name."
  (when (and active
             (bound-and-true-p persp-mode)
             ;; There are multiple implementations of
             ;; persp-mode with different APIs
             (fboundp 'safe-persp-name)
             (fboundp 'get-frame-persp)
             ;; Display the nil persp only if specified
             (or (not (string= persp-nil-name (safe-persp-name (get-frame-persp))))
                 spaceline-display-default-perspective))
    (let ((name (safe-persp-name (get-frame-persp))))
      (propertize
       (if (file-directory-p name)
           (file-name-nondirectory (directory-file-name name))
         name)
       'face 'bold))))

(defface spaceline-flycheck-error
  '((t (:foreground "#FC5C94" :distant-foreground "#A20C41")))
  "Face for flycheck error feedback in the modeline."
  :group 'spaceline)
(defface spaceline-flycheck-warning
  '((t (:foreground "#F3EA98" :distant-foreground "#968B26")))
  "Face for flycheck warning feedback in the modeline."
  :group 'spaceline)
(defface spaceline-flycheck-info
  '((t (:foreground "#8DE6F7" :distant-foreground "#21889B")))
  "Face for flycheck info feedback in the modeline."
  :group 'spaceline)

(defvar spaceline-flycheck-bullet "•%s"
  "The bullet used for the flycheck segment.
This should be a format string with a single `%s'-expression corresponding to
the number of errors.")

(defmacro spaceline--flycheck-lighter (state)
  "Return flycheck information for the given error type STATE."
  `(let* ((counts (flycheck-count-errors flycheck-current-errors))
          (errorp (flycheck-has-current-errors-p ',state))
          (err (or (cdr (assq ',state counts)) "?"))
          (running (eq 'running flycheck-last-status-change)))
     (if (or errorp running) (format spaceline-flycheck-bullet err))))

(dolist (state '(error warning info))
  (let ((segment-name (intern (format "flycheck-%S" state)))
        (face (intern (format "spaceline-flycheck-%S" state))))
    (eval
     `(spaceline-define-segment ,segment-name
        ,(format "Information about flycheck %Ss. Requires `flycheck-mode' to be enabled" state)
        (when (and (bound-and-true-p flycheck-mode)
                   (or flycheck-current-errors
                       (eq 'running flycheck-last-status-change)))
          (let ((lighter (spaceline--flycheck-lighter ,state)))
            (when lighter (powerline-raw (s-trim lighter) ',face))))))))

(spaceline-define-segment evil-state
  "The current evil state.  Requires `evil-mode' to be enabled."
  (when (bound-and-true-p evil-local-mode)
    (let ((tag (evil-state-property evil-state :tag t)))
      (s-trim (if (stringp tag) tag (funcall tag))))))

(defface spaceline-python-venv
  '((t (:foreground "plum1" :distant-foreground "DarkMagenta")))
  "Face for highlighting the python venv."
  :group 'spaceline)

(defvar spaceline-purpose-hide-if-not-dedicated nil
  "Hide the purpose segment if the window is not dedicated in
some way.")

(spaceline-define-segment purpose
  "The current window purpose. Requires `purpose-mode' to be
enabled."
  (when (and (bound-and-true-p purpose-mode)
             (or (not spaceline-purpose-hide-if-not-dedicated)
                 (purpose-window-purpose-dedicated-p)
                 (window-dedicated-p)))
    (propertize (substring (purpose--modeline-string) 2 -1)
                'face 'spaceline-python-venv
                'help-echo "Window purpose")))

(spaceline-define-segment python-env
  "The current python env.  Works with `pyvenv', `pyenv' and `conda'."
  (when (and active (eq 'python-mode major-mode))
    (let (name source)
      (cond
       ((bound-and-true-p pyvenv-virtual-env-name)
        (setq name pyvenv-virtual-env-name source "pyvenv"))
       ((bound-and-true-p conda-env-current-name)
        (setq name conda-env-current-name source "conda"))
       ((and (fboundp 'pyenv-mode) (setq name (pyenv-mode-version)))
        (setq source "pyenv")))
      (when name
        (propertize name
                    'face 'spaceline-python-venv
                    'help-echo (format "Virtual environment (via %s): %s" source name))))))

(spaceline-define-segment python-pyvenv
  "The current python venv.  Works with `pyvenv'."
  (when (and active
             (eq 'python-mode major-mode)
             (bound-and-true-p pyvenv-virtual-env-name))
    (propertize pyvenv-virtual-env-name
                'face 'spaceline-python-venv
                'help-echo (format "Virtual environment (via pyvenv): %s"
                                   pyvenv-virtual-env))))

(spaceline-define-segment python-pyenv
  "The current python venv.  Works with `pyenv'."
  (when (and active
             (eq 'python-mode major-mode)
             (fboundp 'pyenv-mode-version)
             (pyenv-mode-version))
    (let ((name (pyenv-mode-version)))
      (propertize name
                  'face 'spaceline-python-venv
                  'help-echo "Virtual environment (via pyenv)"))))

(spaceline-define-segment paradox-menu
  "The current package info including upgradable, new, installed
and total packages"
  (when (and active (derived-mode-p 'paradox-menu-mode))
    mode-line-buffer-identification))

(spaceline-define-segment which-function
  (when (and active
             (bound-and-true-p which-function-mode)
             (bound-and-true-p which-func-mode))
    (let* ((current (format-mode-line which-func-current)))
      (when (string-match "{\\(.*\\)}" current)
        (setq current (match-string 1 current)))
      (setq current (replace-regexp-in-string "%" "%%" current))
      (propertize current
                  'local-map which-func-keymap
                  'face 'which-func
                  'mouse-face 'mode-line-highlight
                  'help-echo "mouse-1: go to beginning\n\
mouse-2: toggle rest visibility\n\
mouse-3: go to end"))))

(spaceline-define-segment mu4e-alert-segment
  "Show the number of unread mails using mu. Requires mu4e-alert"
  (when (and active (featurep 'mu4e-alert))
    mu4e-alert-mode-line)
  :global-override ((:eval mu4e-alert-mode-line)))

(spaceline-define-segment recursive-edit
  "Shows the current recursive-edit depth."
  (format "↻%s" (recursion-depth))
  :when (> (recursion-depth) 0))

(spaceline-define-segment macrodef
  "Shows when defining a keyboard macro."
  "•REC"
  :when defining-kbd-macro)

(spaceline-define-segment treesit-inspect
  "Show tree-sitter node at point."
  (when (and active
             (fboundp 'treesit-available-p)
             (treesit-available-p)
             (bound-and-true-p treesit-inspect-mode))
    '((:eval treesit--inspect-name))))

(spaceline-define-segment so-long
  "Show `so-long-mode-line-info'."
  (when (and active
             (bound-and-true-p so-long-mode-line-info))
    '(("" so-long-mode-line-info))))

(provide 'spaceline-segments)

;;; spaceline-segments.el ends here
