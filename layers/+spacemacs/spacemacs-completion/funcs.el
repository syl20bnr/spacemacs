;;; funcs.el --- Spacemacs Completion Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3



;; Helm

(defun spacemacs/helm-faces ()
  "Describe face."
  (interactive)
  (require 'helm-elisp)
  (let ((default (or (face-at-point) (thing-at-point 'symbol))))
    (helm :sources (helm-def-source--emacs-faces
                    (format "%s" (or default "default")))
          :buffer "*helm faces*")))

(defun spacemacs//hide-cursor-in-helm-buffer ()
  "Hide the cursor in helm buffers."
  (with-helm-buffer
    (setq cursor-in-non-selected-windows nil)))

(defun spacemacs//set-dotted-directory ()
  "Set the face of diretories for `.' and `..'"
  (set-face-attribute 'helm-ff-dotted-directory
                      nil
                      :foreground nil
                      :background nil
                      :inherit 'helm-ff-directory))

(defun spacemacs//helm-make-source (f &rest args)
  "Function to be used as advice to activate fuzzy matching for all sources."
  (let ((source-type (cadr args))
        (props (cddr args)))
    ;; fuzzy matching is not supported in async sources
    (unless (child-of-class-p source-type helm-source-async)
      (plist-put props :fuzzy-match (eq 'always dotspacemacs-helm-use-fuzzy))))
  (apply f args))

;; Helm Header line

(defun spacemacs//helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))

(defun helm-toggle-header-line ()
  "Hide the `helm' header if there is only one source."
  (when dotspacemacs-helm-no-header
    (if (> (length helm-sources) 1)
        (set-face-attribute
         'helm-source-header
         nil
         :foreground helm-source-header-default-foreground
         :background helm-source-header-default-background
         :box helm-source-header-default-box
         :height helm-source-header-default-height)
      (set-face-attribute
       'helm-source-header
       nil
       :foreground (face-attribute 'default :background)
       :background (face-attribute 'default :background)
       :box nil
       :height 0.1))))

;; helm navigation on hjkl
(defun spacemacs//helm-hjkl-navigation (style)
  "Set navigation on 'hjkl' for the given editing STYLE."
  (cond
   ((or (eq 'vim style)
        (and (eq 'hybrid style)
             hybrid-mode-enable-hjkl-bindings))
    (define-key helm-map (kbd "C-j") 'helm-next-line)
    (define-key helm-map (kbd "C-k") 'helm-previous-line)
    (define-key helm-map (kbd "C-h") 'helm-next-source)
    (define-key helm-map (kbd "C-S-h") 'describe-key)
    (define-key helm-map (kbd "C-l") (kbd "RET"))
    (with-eval-after-load 'helm-files
      (dolist (keymap (list helm-find-files-map helm-read-file-map))
        (define-key keymap (kbd "C-l") 'helm-execute-persistent-action)
        (define-key keymap (kbd "C-h") 'helm-find-files-up-one-level)
        ;; rebind `describe-key' for convenience
        (define-key keymap (kbd "C-S-h") 'describe-key))))
   (t
    (define-key helm-map (kbd "C-j") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-k") 'helm-delete-minibuffer-contents)
    (define-key helm-map (kbd "C-h") nil)
    (define-key helm-map
      (kbd "C-l") 'helm-recenter-top-bottom-other-window))))

;; Helm Window position

(defvar spacemacs-helm-display-help-buffer-regexp '("*.*Helm.*Help.**"))
(defvar spacemacs-helm-display-buffer-regexp
  `("*.*helm.**"
    (display-buffer-in-side-window)
    (inhibit-same-window . t)
    (side . ,dotspacemacs-helm-position)
    (window-width . 0.6)
    (window-height . 0.4)))
(defvar spacemacs-display-buffer-alist nil)

(defun spacemacs//display-helm-window (buffer)
  "Display the Helm window respecting `dotspacemacs-helm-position'."
  (let ((display-buffer-alist
         (list spacemacs-helm-display-help-buffer-regexp
               ;; this or any specialized case of Helm buffer must be
               ;; added AFTER `spacemacs-helm-display-buffer-regexp'.
               ;; Otherwise, `spacemacs-helm-display-buffer-regexp' will
               ;; be used before
               ;; `spacemacs-helm-display-help-buffer-regexp' and display
               ;; configuration for normal Helm buffer is applied for helm
               ;; help buffer, making the help buffer unable to be
               ;; displayed.
               spacemacs-helm-display-buffer-regexp)))
    (helm-default-display-buffer buffer)))

(defun spacemacs//unprevent-minibuffer-escape ()
  "Workaround for a helm-evil incompatibility.
See https://github.com/syl20bnr/spacemacs/issues/3700"
  (when helm-prevent-escaping-from-minibuffer
    (define-key evil-motion-state-map
      [down-mouse-1] 'evil-mouse-drag-region)))

(defun spacemacs//prevent-minibuffer-escape ()
  "Workaround for a helm-evil incompatibility.
See https://github.com/syl20bnr/spacemacs/issues/3700"
  (when helm-prevent-escaping-from-minibuffer
    (define-key evil-motion-state-map [down-mouse-1] nil)))

;; Helm Transient state

(defun spacemacs//define-helm-action-functions ()
  "Define Spacemacs functions to pick actions."
  (dotimes (n 10)
    (let ((func (intern (format "spacemacs/helm-action-%d" n)))
          (doc (format "Select helm action #%d" n)))
      (eval `(defun ,func ()
               ,doc
               (intern)
               (helm-select-nth-action ,(1- n)))))))

(defun spacemacs/helm-ts-edit ()
  "Switch in edit mode depending on the current helm buffer."
  (interactive)
  (cond
   ((string-equal "*helm-ag*" helm-buffer)
    (helm-ag-edit))))

(defun spacemacs//helm-navigation-ts-on-enter ()
  "Initialization of helm transient-state."
  ;; faces
  (spacemacs//helm-navigation-ts-set-face)
  (setq spacemacs--helm-navigation-ts-face-cookie-minibuffer
        (face-remap-add-relative
         'minibuffer-prompt
         'spacemacs-helm-navigation-ts-face)))

(defun spacemacs//helm-navigation-ts-set-face ()
  "Set the face for helm header in helm navigation transient-state"
  (with-helm-window
    (setq spacemacs--helm-navigation-ts-face-cookie-header
          (face-remap-add-relative
           'helm-header
           'spacemacs-helm-navigation-ts-face))))

(defun spacemacs//helm-navigation-ts-on-exit ()
  "Action to perform when exiting helm transient-state."
  (with-helm-window
    (face-remap-remove-relative
     spacemacs--helm-navigation-ts-face-cookie-header))
  (face-remap-remove-relative
   spacemacs--helm-navigation-ts-face-cookie-minibuffer))

(defun spacemacs/helm-transient-state-select-action ()
  "Display the Helm actions page."
  (interactive)
  (call-interactively 'helm-select-action)
  (spacemacs//helm-navigation-ts-set-face))


;; Ivy

(defun spacemacs//ivy-hjkl-navigation (style)
  "Set navigation on 'hjkl' for the given editing STYLE."
  (cond
   ((or (eq 'vim style)
        (and (eq 'hybrid style)
             hybrid-mode-enable-hjkl-bindings))
    (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
    (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
    (define-key ivy-minibuffer-map (kbd "C-h") (kbd "DEL"))
    ;; Move C-h to C-S-h
    (define-key ivy-minibuffer-map (kbd "C-S-h") help-map)
    (define-key ivy-minibuffer-map (kbd "C-l") 'ivy-alt-done)
    (define-key ivy-minibuffer-map (kbd "<escape>")
      'minibuffer-keyboard-quit))
   (t
    (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-alt-done)
    (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-kill-line)
    (define-key ivy-minibuffer-map (kbd "C-h") nil)
    (define-key ivy-minibuffer-map (kbd "C-l") nil))))
