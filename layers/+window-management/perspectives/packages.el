;;; packages.el --- Perspectives Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(setq perspectives-packages
      '(
        spaceline
        persp-mode
        helm
        helm-projectile))

(defun perspectives/pre-init-spaceline ()
  (spacemacs|use-package-add-hook spaceline-config
    :post-config
    (progn
      (spaceline-define-segment persp-number
        (spacemacs/persp-number)
        :when (and (bound-and-true-p persp-mode)
                   (spacemacs/persp-number)))
      (setq spaceline-left
            (cons '((persp-number workspace-number window-number)
                    :fallback evil-state
                    :separator "|"
                    :face highlight-face)
                  (cdr spaceline-left))))))

(defun perspectives/init-persp-mode ()
  (use-package persp-mode
    :no-require t
    :diminish persp-mode
    :init
    (setq persp-nil-name spacemacs-persp-nil-name
          persp-save-dir spacemacs-persp-save-dir)
    :config
    (push #'(lambda (b) (with-current-buffer b
                     (cond ((eq major-mode 'erc-mode) t)
                           ((eq major-mode 'rcirc-mode) t)
                           ((eq major-mode 'exwm-mode) t)
                           (t nil)))) persp-filter-save-buffers-functions)
    ;; By default, persp mode wont affect either helm or ido
    (defadvice persp-mode
        (after suppress-buffer-isolation activate)
      (remove-hook 'ido-make-buffer-list-hook #'persp-restrict-ido-buffers))

    ;; quick swtiching between perspectives
    (defvar persp-toggle-perspective persp-nil-name
      "Previously selected perspective. Used with `persp-jump-to-last-persp'.")

    (defun persp-jump-to-last-persp ()
      (interactive)
      (persp-switch persp-toggle-perspective))

    (defadvice persp-activate
        (before save-toggle-persp activate)
      (setq persp-toggle-perspective persp-last-persp-name))

    (defun persp-autosave ()
      "Perspectives mode autosave.
Autosaves perspectives layouts every `persp-autosave-interal' seconds.
Cancels autosave on exiting perspectives mode."
      (if (and persp-mode persp-mode-autosave)
          (progn
            (message "Perspectives mode autosaving enabled.")
            (setq persp-autosave-timer
                  (run-with-timer
                   persp-autosave-interval
                   persp-autosave-interval
                   (lambda ()
                     (message "Saving perspectives to file.")
                     (persp-save-state-to-file)))))
        (when persp-autosave-timer
          (cancel-timer persp-autosave-timer)
          (setq persp-autosave-timer nil))))

    (add-hook 'persp-mode-hook #'persp-autosave)

    ;; activate persp mode
    (persp-mode 1)

    (when spacemacs-persp-show-home-at-startup
      (defadvice dotspacemacs/user-config (after show-spacemacs-home activate)
        "Show Spacemacs Home Buffer after perspectives load."
        (persp-switch persp-nil-name)
        (switch-to-buffer "*spacemacs*")
        (delete-other-windows)))

    (defun persp-curr-name ()
      (interactive)
      (safe-persp-name (get-frame-persp)))

    (defface persp-selected-face
      '((t (:inherit font-lock-keyword-face)))
      "The face used to highlight the current perspective on the modeline.")

    (defun persp-format-name (name)
      "Format the perspective name given by NAME for display in `persp-modestring'."
      (let ((string-name (format "%s" name)))
        (if (equal name (persp-curr-name))
            (propertize string-name 'face 'persp-selected-face)
          string-name)))

    (defun spacemacs/persp-number ()
      "Return the number of the current workspace."
      (let* ((num (position (persp-curr-name)
                            (persp-names-current-frame-fast-ordered)))
             (str (if num (int-to-string num))))
        (cond
         ((not (dotspacemacs|symbol-value
                dotspacemacs-mode-line-unicode-symbols)) str)
         ((equal str "0") "➊")
         ((equal str "1") "➋")
         ((equal str "2") "➌")
         ((equal str "3") "➍")
         ((equal str "4") "➎")
         ((equal str "5") "❻")
         ((equal str "6") "➐")
         ((equal str "7") "➑")
         ((equal str "8") "➒")
         ((equal str "9") "➓"))))

    (defun spacemacs/persp-switch-by-pos (pos)
      "Switch to perspective of position (1-index)."
      (let ((persp-to-switch
             (nth (1- pos) (persp-names-current-frame-fast-ordered))))
        (if persp-to-switch
            (persp-switch persp-to-switch)
          (when (y-or-n-p (format
                           (concat "Perspective in position %s doesn't exist.\n"
                                   "Do you want to create one? ") pos))
            (persp-switch nil)))))

    (defun spacemacs/persp-ms-switch-by-pos (pos)
      "Switch to perspective by position POS (1-index) and return to micro-state"
      (spacemacs/persp-switch-by-pos pos)
      (spacemacs/perspectives-micro-state))

    (defun spacemacs/persp-switch-to-1 ()
      (interactive)
      (spacemacs/persp-ms-switch-by-pos 1))
    (defun spacemacs/persp-switch-to-2 ()
      (interactive)
      (spacemacs/persp-ms-switch-by-pos 2))
    (defun spacemacs/persp-switch-to-3 ()
      (interactive)
      (spacemacs/persp-ms-switch-by-pos 3))
    (defun spacemacs/persp-switch-to-4 ()
      (interactive)
      (spacemacs/persp-ms-switch-by-pos 4))
    (defun spacemacs/persp-switch-to-5 ()
      (interactive)
      (spacemacs/persp-ms-switch-by-pos 5))
    (defun spacemacs/persp-switch-to-6 ()
      (interactive)
      (spacemacs/persp-ms-switch-by-pos 6))
    (defun spacemacs/persp-switch-to-7 ()
      (interactive)
      (spacemacs/persp-ms-switch-by-pos 7))
    (defun spacemacs/persp-switch-to-8 ()
      (interactive)
      (spacemacs/persp-ms-switch-by-pos 8))
    (defun spacemacs/persp-switch-to-9 ()
      (interactive)
      (spacemacs/persp-ms-switch-by-pos 9))

    (defvar spacemacs--perspectives-ms-doc-toggle 1
      "Display a short doc when nil, full doc otherwise.")

    (defun spacemacs//perspectives-ms-doc ()
      "Return the docstring for the perspectives micro-state."
      (let* ((persp-list           (persp-names-current-frame-fast-ordered))
             (formatted-persp-list (mapconcat
                                    (lambda (persp)
                                      (persp-format-name persp))
                                    (or persp-list (list persp-nil-name)) " | ")))
        (concat formatted-persp-list
                (when (and perspectives-display-help
                           (equal 0 spacemacs--perspectives-ms-doc-toggle))
                  (concat
                   "\n"
                   "[n|p] [next|previous] perspective, "
                   "[tab] jump to last perspective \n"
                   "[s|r] [switch to or create|rename] perspective\n"
                   "[P] Projectile switch to perspective, [o] open custom perspective\n"
                   "[c|C] kill [current|other] perspective, "
                   "[t|K] switch [to|remove] buffer from [other|current] perspective\n"
                   "[a|i] import [one|all] buffer(s) from other perspective\n"
                   "[w|l] [save|load] configuration [to|from] file")))))

    (defun spacemacs//perspectives-ms-toggle-doc ()
      (interactive)
      (setq spacemacs--perspectives-ms-doc-toggle
            (logxor spacemacs--perspectives-ms-doc-toggle 1)))

    (spacemacs|define-micro-state perspectives
      :doc (spacemacs//perspectives-ms-doc)
      :use-minibuffer t
      :evil-leader "L"
      :bindings
      ;; need to exit in case number doesn't exist
      ("?" spacemacs//perspectives-ms-toggle-doc)
      ("1" spacemacs/persp-switch-to-1 :exit t)
      ("2" spacemacs/persp-switch-to-2 :exit t)
      ("3" spacemacs/persp-switch-to-3 :exit t)
      ("4" spacemacs/persp-switch-to-4 :exit t)
      ("5" spacemacs/persp-switch-to-5 :exit t)
      ("6" spacemacs/persp-switch-to-6 :exit t)
      ("7" spacemacs/persp-switch-to-7 :exit t)
      ("8" spacemacs/persp-switch-to-8 :exit t)
      ("9" spacemacs/persp-switch-to-9 :exit t)
      ("<tab>" persp-jump-to-last-persp)
      ("n" persp-next)
      ("N" persp-prev)
      ("p" persp-prev)
      ("P" spacemacs/helm-persp-switch-project :exit t)
      ("L" spacemacs/helm-perspectives :exit t)
      ("s" spacemacs/helm-persp-switch :exit t)
      ("b" spacemacs/persp-helm-mini :exit t)
      ("r" spacemacs/persp-ms-rename :exit t)
      ("x" spacemacs/persp-ms-kill)
      ("X" spacemacs/persp-ms-kill-other :exit t)
      ("c" spacemacs/persp-ms-close)
      ("C" spacemacs/persp-ms-close-other :exit t)
      ("a" persp-add-buffer :exit t)
      ("t" persp-temporarily-display-buffer :exit t)
      ("i" persp-import-buffers :exit t)
      ("K" persp-remove-buffer :exit t)
      ("w" persp-save-state-to-file :exit t)
      ("l" persp-load-state-from-file :exit t)
      ("o" spacemacs/select-custom-persps :exit t))

    (defun spacemacs/persp-ms-rename ()
      "Rename a perspective and get back to the perspectives micro-state."
      (interactive)
      (call-interactively #'persp-rename)
      (spacemacs/perspectives-micro-state))

    (defun spacemacs/persp-ms-close ()
      "Kill current perspective"
      (interactive)
      (persp-kill-without-buffers (persp-curr-name)))

    (defun spacemacs/persp-ms-close-other ()
      (interactive)
      (call-interactively 'spacemacs/helm-persp-close)
      (spacemacs/perspectives-micro-state))

    (defun spacemacs/persp-ms-kill ()
      "Kill current perspective"
      (interactive)
      (persp-kill (persp-curr-name)))

    (defun spacemacs/persp-ms-kill-other ()
      (interactive)
      (call-interactively 'spacemacs/helm-persp-kill)
      (spacemacs/perspectives-micro-state))

    ;; (defun spacemacs/persp-ms-last ()
    ;;   "Switch to the last active perspective"
    ;;   (interactive)
    ;;   (persp-switch persp-last-persp-name))

;;; Custom Perspectives Micro State

    (defvar spacemacs-custom-persp-alist '()
      "List of custom perspectives with their bindkeys.
Do not modify directly, use provided `spacemacs|define-custom-persp'")

    (defun spacemacs/select-custom-persps ()
      "Update the custom-persps microstate and then activate it."
      (interactive)
      (eval (macroexpand '(spacemacs|update-custom-persps)))
      (call-interactively #'spacemacs/custom-persps-micro-state))

    (defun spacemacs//custom-persps-ms-documentation ()
      "Return the docstring for the custom perspectives micro-state."
      (if spacemacs-custom-persp-alist
          (concat (mapconcat (lambda (custom-persp)
                               (format "[%s] %s" (car custom-persp) (cdr custom-persp)))
                             spacemacs-custom-persp-alist
                             " ")
                  "\n[q] quit to perspectives-micro-state")
        (warn (format "`spacemacs-custom-persp-alist' variable is empty" ))))

    (defmacro spacemacs|update-custom-persps ()
      "Ensure the custom-persps micro-state is updated.
Takes each element in the list `spacemacs-custom-persp-alist'
format so they are supported by the
`spacemacs/custom-persps-micro-state' macro."
      (let ((bindings '(("q" spacemacs/perspectives-micro-state :exit t))))
        (dolist (custom-persp spacemacs-custom-persp-alist bindings)
          (let* ((binding (car custom-persp))
                 (name (cdr custom-persp))
                 (func-name (spacemacs//custom-persps-func-name name)))
            (push (list binding func-name) bindings)))
        `(spacemacs|define-micro-state custom-persps
           :doc (spacemacs//custom-persps-ms-documentation)
           :use-minibuffer t
           :bindings
           ,@bindings)))

    (defun spacemacs//custom-persps-func-name (name)
      "Return the name of the custom-perspective function."
      (intern (concat "spacemacs/custom-perspective-" name)))

    (defmacro spacemacs|define-custom-persp (name &rest props)
      "Define a custom-perspective called NAME.

FUNC is a FUNCTION defined using NAME and the result of
`spacemacs//custom-persps-func-name', it takes care of
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
      (let* ((func             (spacemacs//custom-persps-func-name name))
             (binding          (car (spacemacs/mplist-get props :binding)))
             (body             (spacemacs/mplist-get props :body))
             (already-defined? (cdr
                                (assoc binding spacemacs-custom-persp-alist))))
        `(progn
           (defun ,func ()
             ,(format "Open custom perspective %s" name)
             (interactive)
             (let ((initialize (not (gethash ,name *persp-hash*))))
               (persp-switch ,name)
               (when initialize ,@body)))
           ;; Check for Clashes
           (if ,already-defined?
               (unless (equal ,already-defined? ,name)
                 (warn "Replacing existing binding \"%s\" for %s with %s"
                       ,binding ,already-defined? ,name )
                 (push '(,binding . ,name) spacemacs-custom-persp-alist))
             (push '(,binding . ,name) spacemacs-custom-persp-alist)))))

    (spacemacs|define-custom-persp "@Spacemacs"
      :binding "e"
      :body
      (spacemacs/find-dotfile))

    (when (configuration-layer/layer-usedp 'org)
      (spacemacs|define-custom-persp "@Org"
        :binding "o"
        :body
        (find-file (first org-agenda-files))))

    (when (configuration-layer/layer-usedp 'erc)
      (spacemacs|define-custom-persp "@ERC"
        :binding "E"
        :body
        (call-interactively 'erc)))))


(defun perspectives/post-init-helm ()
  (defun spacemacs/persp-helm-mini ()
    "As `helm-mini' but restricts visible buffers by perspective."
    (interactive)
    (with-persp-buffer-list ()
                            (helm-mini)))
  (defun spacemacs/helm-perspectives-source ()
    (helm-build-in-buffer-source
        (concat "Current Perspective: " (persp-curr-name))
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
     :sources `(,(spacemacs/helm-perspectives-source)
                ,(helm-build-dummy-source "Create new perspective"
                   :requires-pattern t
                   :action #'persp-switch))))
  (defun spacemacs/helm-persp-switch ()
    "Selects or creates perspective."
    (interactive)
    (helm
     :buffer "*Helm Switch Perspectives*"
     :sources `(,(helm-build-in-buffer-source
                     (concat "Current Perspective: " (persp-curr-name))
                   :data (persp-names)
                   :fuzzy-match t
                   :action #'persp-switch)
                ,(helm-build-dummy-source "Create new perspective"
                   :requires-pattern t
                   :action #'persp-switch))))
  ;; ability to use helm find files but also adds to current perspective
  (defun spacemacs/helm-persp-close ()
    "Kills perspectives without killing the buffers"
    (interactive)
    (helm
     :buffer "*Helm Kill Perspectives (without killing buffers)*"
     :sources (helm-build-in-buffer-source
                  (concat "Current Perspective: " (persp-curr-name))
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
                            (persp-curr-name))
                :data (persp-names)
                :fuzzy-match t
                :action
                '(("Kill perspective(s)" . (lambda (candidate)
                                             (mapcar 'persp-kill
                                                     (helm-marked-candidates)))))))))

(defun perspectives/post-init-helm-projectile ()
  (defun spacemacs/helm-persp-switch-project (arg)
    (interactive "P")
    (helm
     :sources (helm-build-in-buffer-source "*Helm Switch Project Perspective*"
                :data (lambda ()
                        (if (projectile-project-p)
                            (cons (abbreviate-file-name (projectile-project-root))
                                  (projectile-relevant-known-projects))
                          projectile-known-projects))
                :fuzzy-match helm-projectile-fuzzy-match
                :mode-line helm-read-file-name-mode-line-string
                :action '(("Switch to Project Perspective" .
                           (lambda (project)
                             (persp-switch project)
                             (let ((projectile-completion-system 'helm))
                               (projectile-switch-project-by-name project)))
                           )))
     :buffer "*Projectile Perspectives*")))
