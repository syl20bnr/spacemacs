;;; funcs.el --- Spacemacs Navigation Layer functions File
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; auto-highlight symbol

(defun spacemacs/goto-last-searched-ahs-symbol ()
  "Go to the last known occurrence of the last symbol searched with
`auto-highlight-symbol'."
  (interactive)
  (if spacemacs-last-ahs-highlight-p
      (progn (goto-char (nth 1 spacemacs-last-ahs-highlight-p))
             (spacemacs/ahs-highlight-now-wrapper)
             (spacemacs/symbol-highlight-transient-state/body))
    (message "No symbol has been searched for now.")))

(defun spacemacs/integrate-evil-search (forward)
        ;; isearch-string is last searched item.  Next time
        ;; "n" is hit we will use this.
        (let* ((symbol (evil-find-thing forward 'symbol))
               (regexp (concat "\\<" symbol "\\>")))
          (setq isearch-string regexp
                isearch-regexp regexp
                evil-ex-search-pattern (evil-ex-make-search-pattern regexp)))
        ;; Next time "n" is hit, go the correct direction.
        (setq isearch-forward forward)
        ;; ahs does a case sensitive search.  We could set
        ;; this, but it would break the user's current
        ;; sensitivity settings.  We could save the setting,
        ;; then next time the user starts a search we could
        ;; restore the setting.
        ;;(setq case-fold-search nil)
        ;; Place the search term into the search rings.
        (isearch-update-ring isearch-string t)
        (evil-push-search-history isearch-string forward)
        ;; Use this search term for empty pattern "%s//replacement/"
        ;; Append case sensitivity
        (setq evil-ex-last-was-search nil
              evil-ex-substitute-pattern `(,(concat isearch-string "\\C")
                                           nil (0 0))))

(defun spacemacs/ensure-ahs-enabled-locally ()
  "Ensures ahs is enabled for the local buffer."
  (unless
      (bound-and-true-p ahs-mode-line)
    (auto-highlight-symbol-mode)
    ))

(defun spacemacs/ahs-highlight-now-wrapper ()
  "Safe wrapper for ahs-highlight-now"
  (eval '(progn
           (spacemacs/ensure-ahs-enabled-locally)
           (ahs-highlight-now)) nil))

(defun spacemacs/enter-ahs-forward ()
  "Go to the next occurrence of symbol under point with
 `auto-highlight-symbol'"
  (interactive)
  (setq spacemacs--ahs-searching-forward t)
  (spacemacs/quick-ahs-forward))

(defun spacemacs/enter-ahs-backward ()
  "Go to the previous occurrence of symbol under point with
 `auto-highlight-symbol'"
  (interactive)
  (setq spacemacs--ahs-searching-forward nil)
  (spacemacs/quick-ahs-forward))

(defun spacemacs/quick-ahs-forward ()
  "Go to the next occurrence of symbol under point with
 `auto-highlight-symbol'"
  (interactive)
  (spacemacs//quick-ahs-move t))

(defun spacemacs/quick-ahs-backward ()
  "Go to the previous occurrence of symbol under point with
 `auto-highlight-symbol'"
  (interactive)
  (spacemacs//quick-ahs-move nil))

(defun spacemacs//quick-ahs-move (forward)
  "Go to the next occurrence of symbol under point with
 `auto-highlight-symbol'"

  (if (eq forward spacemacs--ahs-searching-forward)
      (progn
        (spacemacs/integrate-evil-search t)
        (spacemacs/ahs-highlight-now-wrapper)
        (evil-set-jump)
        (spacemacs/symbol-highlight-transient-state/body)
        (ahs-forward))
    (progn
      (spacemacs/integrate-evil-search nil)
      (spacemacs/ahs-highlight-now-wrapper)
      (evil-set-jump)
      (spacemacs/symbol-highlight-transient-state/body)
      (ahs-backward))))

(defun spacemacs/symbol-highlight ()
  "Highlight the symbol under point with `auto-highlight-symbol'."
  (interactive)
  (spacemacs/ahs-highlight-now-wrapper)
  (setq spacemacs-last-ahs-highlight-p (ahs-highlight-p))
  (spacemacs/symbol-highlight-transient-state/body)
  (spacemacs/integrate-evil-search nil))

(defun spacemacs//ahs-ms-on-exit ()
  ;; Restore user search direction state as ahs has exitted in a state
  ;; good for <C-s>, but not for 'n' and 'N'"
  (setq isearch-forward spacemacs--ahs-searching-forward))

(defun spacemacs/symbol-highlight-reset-range ()
  "Reset the range for `auto-highlight-symbol'."
  (interactive)
  (ahs-change-range ahs-default-range))

;; transient state
(defun spacemacs//symbol-highlight-doc ()
        (let* ((i 0)
               (overlay-count (length ahs-overlay-list))
               (overlay (format "%s" (nth i ahs-overlay-list)))
               (current-overlay (format "%s" ahs-current-overlay))
               (st (ahs-stat))
               (plighter (ahs-current-plugin-prop 'lighter))
               (plugin (format "%s"
                               (cond ((string= plighter "HS")  "Display")
                                     ((string= plighter "HSA") "Buffer")
                                     ((string= plighter "HSD") "Function"))))
               (face (cond ((string= plighter "HS")  ahs-plugin-defalt-face)
                           ((string= plighter "HSA") ahs-plugin-whole-buffer-face)
                           ((string= plighter "HSD") ahs-plugin-bod-face))))
          (while (not (string= overlay current-overlay))
            (setq i (1+ i))
            (setq overlay (format "%s" (nth i ahs-overlay-list))))
          (let* ((x/y (format "[%s/%s]" (- overlay-count i) overlay-count))
                 (hidden (if (< 0 (- overlay-count (nth 4 st))) "*" "")))
            (concat
             (propertize (format " %s " plugin) 'face face)
             (propertize (format " %s%s " x/y hidden) 'face
                         `(:foreground "#ffffff" :background "#000000"))))))

(defun spacemacs/ahs-to-iedit ()
  "Trigger iedit from ahs."
  (interactive)
  (cond
   ((and (not (eq dotspacemacs-editing-style 'emacs))
         (configuration-layer/package-usedp 'evil-iedit-state))
    (evil-iedit-state/iedit-mode)
    (iedit-restrict-region (ahs-current-plugin-prop 'start)
                           (ahs-current-plugin-prop 'end)))
   ((and (eq dotspacemacs-editing-style 'emacs)
         (configuration-layer/package-usedp 'iedit))
    (iedit-mode)
    (iedit-restrict-region (ahs-current-plugin-prop 'start)
                           (ahs-current-plugin-prop 'end)))
   (t (ahs-edit-mode t))))

(defun spacemacs//symbol-highlight-ts-doc ()
  (spacemacs//transient-state-make-doc
   'symbol-highlight
   (format spacemacs--symbol-highlight-transient-state-doc
           (spacemacs//symbol-highlight-doc)
           (make-string (length (spacemacs//symbol-highlight-doc)) 32))))


;; golden ratio

(defun spacemacs/no-golden-ratio-for-buffers (bufname)
  "Disable golden-ratio if BUFNAME is the name of a visible buffer."
  (and (get-buffer bufname) (get-buffer-window bufname 'visible)))

(defun spacemacs/no-golden-ratio-guide-key ()
  "Disable golden-ratio for guide-key popwin buffer."
  (or (spacemacs/no-golden-ratio-for-buffers " *guide-key*")
      (spacemacs/no-golden-ratio-for-buffers " *popwin-dummy*")))


;; neotree

(defun spacemacs/neotree-expand-or-open ()
  "Expand or open a neotree node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (if (file-directory-p node)
          (progn
            (neo-buffer--set-expand node t)
            (neo-buffer--refresh t)
            (when neo-auto-indent-point
              (next-line)
              (neo-point-auto-indent)))
        (call-interactively 'neotree-enter)))))

(defun spacemacs/neotree-collapse ()
  "Collapse a neotree node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (when (file-directory-p node)
        (neo-buffer--set-expand node nil)
        (neo-buffer--refresh t))
      (when neo-auto-indent-point
        (neo-point-auto-indent)))))

(defun spacemacs/neotree-collapse-or-up ()
  "Collapse an expanded directory node or go to the parent node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (if (file-directory-p node)
          (if (neo-buffer--expanded-node-p node)
              (spacemacs/neotree-collapse)
            (neotree-select-up-node))
        (neotree-select-up-node)))))

(defun neotree-find-project-root ()
  (interactive)
  (if (neo-global--window-exists-p)
      (neotree-hide)
    (let ((origin-buffer-file-name (buffer-file-name)))
      (neotree-find (projectile-project-root))
      (neotree-find origin-buffer-file-name))))

(defun spacemacs//neotree-maybe-attach-window ()
  (when (get-buffer-window (neo-global--get-buffer))
    (neo-global--attach)))


;; smooth scrolling

(defun spacemacs/enable-smooth-scrolling ()
  "Enable smooth scrolling."
  (interactive)
  (setq scroll-conservatively 101))

(defun spacemacs/disable-smooth-scrolling ()
  "Disable smooth scrolling."
  (interactive)
  (setq scroll-conservatively 0))


;; zoom

(defun spacemacs//zoom-frm-powerline-reset ()
  (when (fboundp 'powerline-reset)
    (setq-default powerline-height (spacemacs/compute-powerline-height))
    (powerline-reset)))

(defun spacemacs//zoom-frm-do (arg)
  "Perform a zoom action depending on ARG value."
  (let ((zoom-action (cond ((eq arg 0) 'zoom-frm-unzoom)
                           ((< arg 0) 'zoom-frm-out)
                           ((> arg 0) 'zoom-frm-in)))
        (fm (cdr (assoc 'fullscreen (frame-parameters))))
        (fwp (* (frame-char-width) (frame-width)))
        (fhp (* (frame-char-height) (frame-height))))
    (when (equal fm 'maximized)
      (toggle-frame-maximized))
    (funcall zoom-action)
    (set-frame-size nil fwp fhp t)
    (when (equal fm 'maximized)
      (toggle-frame-maximized))))

(defun spacemacs/zoom-frm-in ()
  "zoom in frame, but keep the same pixel size"
  (interactive)
  (spacemacs//zoom-frm-do 1)
  (spacemacs//zoom-frm-powerline-reset))

(defun spacemacs/zoom-frm-out ()
  "zoom out frame, but keep the same pixel size"
  (interactive)
  (spacemacs//zoom-frm-do -1)
  (spacemacs//zoom-frm-powerline-reset))

(defun spacemacs/zoom-frm-unzoom ()
  "Unzoom current frame, keeping the same pixel size"
  (interactive)
  (spacemacs//zoom-frm-do 0)
  (spacemacs//zoom-frm-powerline-reset))
