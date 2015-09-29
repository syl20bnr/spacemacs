;;; packages.el --- Spacemacs Core Layer packages File
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

(setq spacemacs-base-packages
      '(
        bind-key
        bookmark
        diminish
        (electric-indent-mode :location built-in)
        ediff
        eldoc
        evil
        evil-escape
        evil-leader
        evil-surround
        evil-visualstar
        exec-path-from-shell
        fill-column-indicator
        helm
        helm-descbinds
        helm-projectile
        (helm-spacemacs :location local)
        (hs-minor-mode :location built-in)
        (holy-mode :location local :step pre)
        (hybrid-mode :location local :step pre)
        (ido :location built-in)
        ido-vertical-mode
        page-break-lines
        popup
        popwin
        (process-menu :location built-in)
        projectile
        quelpa
        recentf
        savehist
        saveplace
        spacemacs-theme
        subword
        undo-tree
        (uniquify :location built-in)
        use-package
        which-key
        whitespace
        winner))

;; Initialization of packages

(defun spacemacs-base/init-bind-key ())

(defun spacemacs-base/init-bookmark ()
  (use-package bookmark
    :defer t
    :init
    (setq bookmark-default-file (concat spacemacs-cache-directory "bookmarks")
          ;; autosave each change
          bookmark-save-flag 1)))

(defun spacemacs-base/init-diminish ()
  (use-package diminish
    :init
    (progn
      ;; Minor modes abbrev --------------------------------------------------------
      (when (display-graphic-p)
        (eval-after-load "eproject"
          '(diminish 'eproject-mode " eⓅ"))
        (eval-after-load "flymake"
          '(diminish 'flymake-mode " Ⓕ2")))
      ;; Minor Mode (hidden) ------------------------------------------------------
      (eval-after-load 'elisp-slime-nav
        '(diminish 'elisp-slime-nav-mode))
      (eval-after-load "hi-lock"
        '(diminish 'hi-lock-mode))
      (eval-after-load "abbrev"
        '(diminish 'abbrev-mode))
      (eval-after-load "subword"
        '(when (eval-when-compile (version< "24.3.1" emacs-version))
           (diminish 'subword-mode))))))

(defun spacemacs-base/init-eldoc ()
  (use-package eldoc
    :defer t
    :config
    (progn
      ;; enable eldoc in `eval-expression'
      (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
      ;; enable eldoc in IELM
      (add-hook 'ielm-mode-hook #'eldoc-mode)
      ;; don't display eldoc on modeline
      (spacemacs|hide-lighter eldoc-mode))))

(defun spacemacs-base/init-electric-indent-mode ()
  (electric-indent-mode))

;; notes from mijoharas
;; We currently just set a few variables to make it look nicer.
;; Here is my first attempt at evilifying the buffer, does not work correctly, help is very much welcome.

;; ```
;; (defun ediff/setup-ediff-keymaps ()
;;   "setup the evil ediff keymap"
;;     (progn
;;      (add-to-list 'evil-emacs-state-modes 'Ediff)
;;      (spacemacs|evilify ediff-mode-map)
;;      (spacemacs/activate-evil-leader-for-map 'ediff-mode-map)
;;       )
;;   )

;; ;; inside the use-package function
;; (add-hook 'ediff-keymap-setup-hook 'ediff/setup-ediff-keymaps)
;; ```
(defun spacemacs-base/init-ediff ()
  (use-package ediff
    :defer t
    :init
    (progn
      ;; first we set some sane defaults
      (setq-default
       ediff-window-setup-function 'ediff-setup-windows-plain
       ;; emacs is evil and decrees that vertical shall henceforth be horizontal
       ediff-split-window-function 'split-window-horizontally
       ediff-merge-split-window-function 'split-window-horizontally))))

(defun spacemacs-base/init-eldoc ()
  (use-package eldoc
    :defer t
    :config
    (progn
      ;; enable eldoc in `eval-expression'
      (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
      ;; enable eldoc in IELM
      (add-hook 'ielm-mode-hook #'eldoc-mode)
      ;; don't display eldoc on modeline
      (spacemacs|hide-lighter eldoc-mode))))

(defun spacemacs-base/init-evil ()
  (use-package evil
    :init
    (progn
      (defvar spacemacs-evil-cursors '(("normal" "DarkGoldenrod2" box)
                                       ("insert" "chartreuse3" (bar . 2))
                                       ("emacs" "SkyBlue2" box)
                                       ("hybrid" "SkyBlue2" (bar . 2))
                                       ("replace" "chocolate" (hbar . 2))
                                       ("evilified" "LightGoldenrod3" box)
                                       ("visual" "gray" (hbar . 2))
                                       ("motion" "plum3" box)
                                       ("lisp" "HotPink1" box)
                                       ("iedit" "firebrick1" box)
                                       ("iedit-insert" "firebrick1" (bar . 2)))
        "Colors assigned to evil states with cursor definitions.")

      (loop for (state color cursor) in spacemacs-evil-cursors
            do
            (eval `(defface ,(intern (format "spacemacs-%s-face" state))
                     `((t (:background ,color
                                      :foreground ,(face-background 'mode-line)
                                      :box ,(face-attribute 'mode-line :box)
                                      :inherit 'mode-line)))
                     (format "%s state face." state)
                     :group 'spacemacs))
            (eval `(setq ,(intern (format "evil-%s-state-cursor" state))
                         (list (when dotspacemacs-colorize-cursor-according-to-state color)
                               cursor))))

      ;; put back refresh of the cursor on post-command-hook see status of:
      ;; https://bitbucket.org/lyro/evil/issue/502/cursor-is-not-refreshed-in-some-cases
      ;; (add-hook 'post-command-hook 'evil-refresh-cursor)

      (defun spacemacs/state-color-face (state)
        "Return the symbol of the face for the given STATE."
        (intern (format "spacemacs-%s-face" (symbol-name state))))

      (defun spacemacs/state-color (state)
        "Return the color string associated to STATE."
        (face-background (spacemacs/state-color-face state)))

      (defun spacemacs/current-state-color ()
        "Return the color string associated to the current state."
        (face-background (spacemacs/state-color-face evil-state)))

      (defun spacemacs/state-face (state)
        "Return the face associated to the STATE."
        (spacemacs/state-color-face state))

      (defun spacemacs/current-state-face ()
        "Return the face associated to the current state."
        (let ((state (if (eq evil-state 'operator)
                         evil-previous-state
                       evil-state)))
          (spacemacs/state-color-face state)))

      (defun evil-insert-state-cursor-hide ()
        (setq evil-insert-state-cursor '((hbar . 0))))

      (evil-mode 1))
    :config
    (progn
      ;; evil ex-command key
      (define-key evil-normal-state-map (kbd dotspacemacs-command-key) 'evil-ex)
      (define-key evil-visual-state-map (kbd dotspacemacs-command-key) 'evil-ex)
      (define-key evil-motion-state-map (kbd dotspacemacs-command-key) 'evil-ex)
      ;; Make the current definition and/or comment visible.
      (define-key evil-normal-state-map "zf" 'reposition-window)
      ;; toggle maximize buffer
      (define-key evil-window-map (kbd "o") 'spacemacs/toggle-maximize-buffer)
      (define-key evil-window-map (kbd "C-o") 'spacemacs/toggle-maximize-buffer)
      ;; make cursor keys work
      (define-key evil-window-map (kbd "<left>") 'evil-window-left)
      (define-key evil-window-map (kbd "<right>") 'evil-window-right)
      (define-key evil-window-map (kbd "<up>") 'evil-window-up)
      (define-key evil-window-map (kbd "<down>") 'evil-window-down)
      ;; Make Y equivalent to y$
      (defun spacemacs/evil-yank-to-end-of-line ()
        "Yank from point to end of line."
        (interactive)
        (evil-yank (point) (point-at-eol)))
      (when dotspacemacs-remap-Y-to-y$
        (define-key evil-normal-state-map (kbd "Y")
          'spacemacs/evil-yank-to-end-of-line)
        (define-key evil-motion-state-map (kbd "Y")
          'spacemacs/evil-yank-to-end-of-line))

      (evil-leader/set-key "re" 'evil-show-registers)

      (defmacro evil-map (state key seq)
        "Map for a given STATE a KEY to a sequence SEQ of keys.

Can handle recursive definition only if KEY is the first key of SEQ.
Example: (evil-map visual \"<\" \"<gv\")"
        (let ((map (intern (format "evil-%S-state-map" state))))
          `(define-key ,map ,key
             (lambda ()
               (interactive)
               ,(if (string-equal key (substring seq 0 1))
                    `(progn
                       (call-interactively ',(lookup-key evil-normal-state-map key))
                       (execute-kbd-macro ,(substring seq 1)))
                  (execute-kbd-macro ,seq))))))
      ;; Keep the region active when shifting
      (evil-map visual "<" "<gv")
      (evil-map visual ">" ">gv")

      (defun spacemacs/evil-smart-doc-lookup ()
        "Version of `evil-lookup' that attempts to use
        the mode specific goto-definition binding,
        i.e. `SPC m h h`, to lookup the source of the definition,
        while falling back to `evil-lookup'"
        (interactive)
        (condition-case nil
            (execute-kbd-macro (kbd (concat dotspacemacs-leader-key " mhh")))
          (error (evil-lookup))))
      (define-key evil-normal-state-map (kbd "K") 'spacemacs/evil-smart-doc-lookup)

      (defun spacemacs/evil-smart-goto-definition ()
        "Version of `evil-goto-definition' that attempts to use
        the mode specific goto-definition binding,
        i.e. `SPC m g g`, to lookup the source of the definition,
        while falling back to `evil-goto-definition'"
        (interactive)
        (condition-case nil
            (execute-kbd-macro (kbd (concat dotspacemacs-leader-key " mgg")))
          (error (evil-goto-definition))))
      (define-key evil-normal-state-map
        (kbd "gd") 'spacemacs/evil-smart-goto-definition)

      ;; scrolling micro state
      (defun spacemacs/scroll-half-page-up ()
        "Scroll half a page up while keeping cursor in middle of page."
        (interactive)
        (evil-window-top)
        (let ((recenter-redisplay nil))
          (recenter nil)))
      (defun spacemacs/scroll-half-page-down ()
        "Scroll half a page down while keeping cursor in middle of page."
        (interactive)
        (evil-window-bottom)
        ;; required to make repeated presses idempotent
        (evil-next-visual-line)
        (let ((recenter-redisplay nil))
          (recenter nil)))
      (spacemacs|define-micro-state scroll
        :doc "[,] page up [.] page down [<] half page up [>] half page down"
        :execute-binding-on-enter t
        :evil-leader "n." "n," "n<" "n>"
        :bindings
        ;; page
        ("," evil-scroll-page-up)
        ("." evil-scroll-page-down)
        ;; half page
        ("<" spacemacs/scroll-half-page-up)
        (">" spacemacs/scroll-half-page-down))

      ;; support for auto-indentation inhibition on universal argument
      (spacemacs|advise-commands
       "handle-indent" (evil-paste-before evil-paste-after) around
       "Handle the universal prefix argument for auto-indentation."
       (let ((prefix (ad-get-arg 0)))
         (ad-set-arg 0 (unless (equal '(4) prefix) prefix))
         ad-do-it
         (ad-set-arg 0 prefix)))

      ;; pasting micro-state
      (spacemacs|advise-commands
       "paste-micro-state"
       (evil-paste-before evil-paste-after evil-visual-paste) after
       "Initate the paste micro-state."
       (unless (or (evil-ex-p)
                   (eq 'evil-paste-from-register this-command))
         (spacemacs/paste-micro-state)))
      (defun spacemacs//paste-ms-doc ()
        "The documentation for the paste micro-state."
        (format (concat "[%s/%s] Type [p] or [P] to paste the previous or "
                        "next copied text, [.] to paste the same text")
                (length kill-ring-yank-pointer) (length kill-ring)))
      (spacemacs|define-micro-state paste
        :doc (spacemacs//paste-ms-doc)
        :use-minibuffer t
        :bindings
        ("p" evil-paste-pop)
        ("P" evil-paste-pop-next))
      (unless dotspacemacs-enable-paste-micro-state
        (ad-disable-advice 'evil-paste-before 'after
                           'evil-paste-before-paste-micro-state)
        (ad-activate 'evil-paste-before)
        (ad-disable-advice 'evil-paste-after 'after
                           'evil-paste-after-paste-micro-state)
        (ad-activate 'evil-paste-after)
        (ad-disable-advice 'evil-visual-paste 'after
                           'evil-visual-paste-paste-micro-state)
        (ad-activate 'evil-visual-paste))

      ;; define text objects
      (defmacro spacemacs|define-text-object (key name start end)
        (let ((inner-name (make-symbol (concat "evil-inner-" name)))
              (outer-name (make-symbol (concat "evil-outer-" name)))
              (start-regex (regexp-opt (list start)))
              (end-regex (regexp-opt (list end))))
          `(progn
             (evil-define-text-object ,inner-name (count &optional beg end type)
               (evil-select-paren ,start-regex ,end-regex beg end type count nil))
             (evil-define-text-object ,outer-name (count &optional beg end type)
               (evil-select-paren ,start-regex ,end-regex beg end type count t))
             (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
             (define-key evil-outer-text-objects-map ,key (quote ,outer-name))
             (when (configuration-layer/package-usedp 'evil-surround)
               (push (cons (string-to-char ,key)
                           (if ,end
                               (cons ,start ,end)
                             ,start))
                     evil-surround-pairs-alist)))))

      (defun spacemacs//standard-text-objects ()
        ;; between dollars sign:
        (spacemacs|define-text-object "$" "dollar" "$" "$")
        ;; define stars
        (spacemacs|define-text-object "*" "star" "*" "*")
        ;; define block star text object
        (spacemacs|define-text-object "8" "block-star" "/*" "*/")
        ;; between pipe characters:
        (spacemacs|define-text-object "|" "bar" "|" "|")
        ;; between percent signs:
        (spacemacs|define-text-object "%" "percent" "%" "%"))

      (spacemacs/add-to-hook 'prog-mode-hook '(spacemacs//standard-text-objects))

      ;; define text-object for entire buffer
      (evil-define-text-object evil-inner-buffer (count &optional beg end type)
        (evil-select-paren "\\`" "\\'" beg end type count nil))
      (define-key evil-inner-text-objects-map "g" 'evil-inner-buffer)

      ;; support smart 1parens-strict-mode
      (when (configuration-layer/package-usedp 'smartparens)
        (defadvice evil-delete-backward-char-and-join
            (around spacemacs/evil-delete-backward-char-and-join activate)
          (defvar smartparens-strict-mode)
          ;; defadvice compiles this sexp generating a compiler warning for a
          ;; free variable reference. The line above fixes this
          (if smartparens-strict-mode
              (call-interactively 'sp-backward-delete-char)
            ad-do-it))))))

(defun spacemacs-base/init-evil-escape ()
  (use-package evil-escape
    :init
    (evil-escape-mode)
    :config
    (spacemacs|hide-lighter evil-escape-mode)))

(defun spacemacs-base/init-evil-leader ()
  (use-package evil-leader
    :init
    (progn
      (setq evil-leader/leader dotspacemacs-leader-key)
      (global-evil-leader-mode)
      ;; This is the same hook used by evil-leader. We make sure that this
      ;; function is called after `evil-leader-mode' using the last argument
      (add-hook 'evil-local-mode-hook
                #'spacemacs-additional-leader-mode t))
    :config
    (progn
      ;; Unset shortcuts which shadow evil leader
      (eval-after-load "compile"
        '(progn
           ;; (define-key compilation-mode-map (kbd dotspacemacs-leader-key) nil)
           (define-key compilation-mode-map (kbd "h") nil)))
      ;; (eval-after-load "dired" '(define-key dired-mode-map (kbd
      ;;   dotspacemacs-leader-key) nil))
      ;; evil-leader does not get activated in existing buffers, so we have to
      ;; force it here
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (evil-leader-mode 1)
          (spacemacs-additional-leader-mode 1))))))

(defun spacemacs-base/init-evil-surround ()
  (use-package evil-surround
    :init
    (progn
      (global-evil-surround-mode 1)
      ;; `s' for surround instead of `substitute'
      ;; see motivation for this change in the documentation
      (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
      (evil-define-key 'visual evil-surround-mode-map "S" 'evil-substitute))))

(defun spacemacs-base/init-evil-visualstar ()
  (use-package evil-visualstar
    :commands (evil-visualstar/begin-search-forward
               evil-visualstar/begin-search-backward)
    :init
    (progn
      (define-key evil-visual-state-map (kbd "*")
        'evil-visualstar/begin-search-forward)
      (define-key evil-visual-state-map (kbd "#")
        'evil-visualstar/begin-search-backward))))

(defun spacemacs-base/init-exec-path-from-shell ()
  (use-package exec-path-from-shell
    :init (when (memq window-system '(mac ns x))
            (exec-path-from-shell-initialize))))

(defun spacemacs-base/init-fill-column-indicator ()
  (use-package fill-column-indicator
    :defer t
    :init
    (progn
      (setq fci-rule-width 1)
      (setq fci-rule-color "#D0BF8F")
      ;; manually register the minor mode since it does not define any
      ;; lighter
      (push '(fci-mode "") minor-mode-alist)
      (spacemacs|add-toggle fill-column-indicator
        :status fci-mode
        :on (turn-on-fci-mode)
        :off (turn-off-fci-mode)
        :documentation "Display the fill column indicator."
        :evil-leader "tf"))
    :config
    (spacemacs|hide-lighter fci-mode)))

(defun spacemacs-base/init-helm ()
  (use-package helm
    :defer 1
    :commands (spacemacs/helm-find-files)
    :config
    (progn
      (when (and dotspacemacs-helm-resize
                  (or (eq dotspacemacs-helm-position 'bottom)
                      (eq dotspacemacs-helm-position 'top)))
        (setq helm-autoresize-min-height 10)
        (helm-autoresize-mode 1))

      ;; from https://www.reddit.com/r/emacs/comments/2z7nbv/lean_helm_window/
      (defvar helm-source-header-default-background (face-attribute 'helm-source-header :background))
      (defvar helm-source-header-default-foreground (face-attribute 'helm-source-header :foreground))
      (defvar helm-source-header-default-box (face-attribute 'helm-source-header :box))
      (defvar helm-source-header-default-height (face-attribute 'helm-source-header :height) )

      (defun helm-toggle-header-line ()
        "Hide the `helm' header is there is only one source."
        (when dotspacemacs-helm-no-header
          (if (> (length helm-sources) 1)
              (set-face-attribute 'helm-source-header
                                  nil
                                  :foreground helm-source-header-default-foreground
                                  :background helm-source-header-default-background
                                  :box helm-source-header-default-box
                                  :height helm-source-header-default-height)
            (set-face-attribute 'helm-source-header
                                nil
                                :foreground (face-attribute 'helm-selection :background)
                                :background (face-attribute 'helm-selection :background)
                                :box nil
                                :height 0.1))))
      (add-hook 'helm-before-initialize-hook 'helm-toggle-header-line)

      (defun spacemacs/helm-find-files (arg)
        "Custom spacemacs implementation for calling helm-find-files-1.

Removes the automatic guessing of the initial value based on thing at point. "
        (interactive "P")
        (let* ((hist          (and arg helm-ff-history (helm-find-files-history)))
                (default-input hist )
                (input         (cond ((and (eq major-mode 'dired-mode) default-input)
                                    (file-name-directory default-input))
                                    ((and (not (string= default-input ""))
                                            default-input))
                                    (t (expand-file-name (helm-current-directory))))))
            (set-text-properties 0 (length input) nil input)
            (helm-find-files-1 input ))))
    :init
    (progn
      (setq helm-prevent-escaping-from-minibuffer t
            helm-bookmark-show-location t
            helm-display-header-line nil
            helm-split-window-in-side-p t
            helm-always-two-windows t
            helm-echo-input-in-header-line t
            helm-imenu-execute-action-at-once-if-one nil)

      ;; hide minibuffer in Helm session, since we use the header line already
      (defun helm-hide-minibuffer-maybe ()
        (when (with-helm-buffer helm-echo-input-in-header-line)
          (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
            (overlay-put ov 'window (selected-window))
            (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                                    `(:background ,bg-color :foreground ,bg-color)))
            (setq-local cursor-type nil))))
      (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

      ;; fuzzy matching setting
      (setq helm-M-x-fuzzy-match t
            helm-apropos-fuzzy-match t
            helm-file-cache-fuzzy-match t
            helm-imenu-fuzzy-match t
            helm-lisp-fuzzy-completion t
            helm-recentf-fuzzy-match t
            helm-semantic-fuzzy-match t
            helm-buffers-fuzzy-matching t)

      ;; helm-locate uses es (from everything on windows, which doesnt like fuzzy)
      (setq helm-locate-fuzzy-match (executable-find "locate"))

      (defun spacemacs//helm-do-grep-region-or-symbol (&optional targs use-region-or-symbol-p)
        "Version of `helm-do-grep' with a default input."
        (interactive)
        (require 'helm)
        (cl-letf*
            (((symbol-function 'this-fn) (symbol-function 'helm-do-grep-1))
             ((symbol-function 'helm-do-grep-1)
              (lambda (targets &optional recurse zgrep exts default-input region-or-symbol-p)
                (let* ((new-input (when region-or-symbol-p
                                   (if (region-active-p)
                                       (buffer-substring-no-properties
                                        (region-beginning) (region-end))
                                     (thing-at-point 'symbol t))))
                      (quoted-input (when new-input (rxt-quote-pcre new-input))))
                  (this-fn targets recurse zgrep exts default-input quoted-input))))
             (preselection (or (dired-get-filename nil t)
                               (buffer-file-name (current-buffer))))
             (targets   (if targs
                            targs
                          (helm-read-file-name
                          "Search in file(s): "
                          :marked-candidates t
                          :preselect (and helm-do-grep-preselect-candidate
                                          (if helm-ff-transformer-show-only-basename
                                              (helm-basename preselection)
                                            preselection))))))
          (helm-do-grep-1 targets nil nil nil nil use-region-or-symbol-p)))

      (defun spacemacs/helm-file-do-grep ()
        "Search in current file with `grep' using a default input."
        (interactive)
        (spacemacs//helm-do-grep-region-or-symbol
         (list (buffer-file-name (current-buffer))) nil))

      (defun spacemacs/helm-file-do-grep-region-or-symbol ()
        "Search in current file with `grep' using a default input."
        (interactive)
        (spacemacs//helm-do-grep-region-or-symbol
         (list (buffer-file-name (current-buffer))) t))

      (defun spacemacs/helm-files-do-grep ()
        "Search in files with `grep'."
        (interactive)
        (spacemacs//helm-do-grep-region-or-symbol nil nil))

      (defun spacemacs/helm-files-do-grep-region-or-symbol ()
        "Search in files with `grep' using a default input."
        (interactive)
        (spacemacs//helm-do-grep-region-or-symbol nil t))

      (defun spacemacs/helm-buffers-do-grep ()
        "Search in opened buffers with `grep'."
        (interactive)
        (let ((buffers (cl-loop for buffer in (buffer-list)
                                when (buffer-file-name buffer)
                                collect (buffer-file-name buffer))))
          (spacemacs//helm-do-grep-region-or-symbol buffers nil)))

      (defun spacemacs/helm-buffers-do-grep-region-or-symbol ()
        "Search in opened buffers with `grep' with a default input."
        (interactive)
        (let ((buffers (cl-loop for buffer in (buffer-list)
                                when (buffer-file-name buffer)
                                collect (buffer-file-name buffer))))
          (spacemacs//helm-do-grep-region-or-symbol buffers t)))

      (defun spacemacs/last-search-buffer ()
        "open last helm-ag or hgrep buffer."
        (interactive)
        (if (get-buffer "*helm ag results*")
            (switch-to-buffer-other-window "*helm ag results*")
            (if (get-buffer "*hgrep*")
                (switch-to-buffer-other-window "*hgrep*")
                (message "No previous search buffer found"))))

      ;; use helm by default for M-x
      (unless (configuration-layer/package-usedp 'smex)
        (global-set-key (kbd "M-x") 'helm-M-x))

      (evil-leader/set-key
        "<f1>" 'helm-apropos
        "bb"   'helm-mini
        "Cl"   'helm-colors
        "ff"   'spacemacs/helm-find-files
        "fF"   'helm-find-files
        "fL"   'helm-locate
        "fr"   'helm-recentf
        "hb"   'helm-filtered-bookmarks
        "hi"   'helm-info-at-point
        "hl"   'helm-resume
        "hm"   'helm-man-woman
        "iu"   'helm-ucs
        "ry"   'helm-show-kill-ring
        "rr"   'helm-register
        "rm"   'helm-all-mark-rings
        "sL"   'spacemacs/last-search-buffer
        "sl"   'spacemacs/jump-in-buffer)

      ;; search with grep
      (evil-leader/set-key
        "sgb"  'spacemacs/helm-buffers-do-grep
        "sgB"  'spacemacs/helm-buffers-do-grep-region-or-symbol
        "sgf"  'spacemacs/helm-files-do-grep
        "sgF"  'spacemacs/helm-files-do-grep-region-or-symbol
        "sgg"  'spacemacs/helm-file-do-grep
        "sgG"  'spacemacs/helm-file-do-grep-region-or-symbol)

      ;; define the key binding at the very end in order to allow the user
      ;; to overwrite any key binding
      (add-hook 'emacs-startup-hook
                (lambda ()
                  (unless (configuration-layer/package-usedp 'smex)
                    (evil-leader/set-key dotspacemacs-command-key 'helm-M-x))))

      (defvar spacemacs-helm-display-help-buffer-regexp '("*.*Helm.*Help.**"))
      (defvar spacemacs-helm-display-buffer-regexp `("*.*helm.**"
                                                     (display-buffer-in-side-window)
                                                     (inhibit-same-window . t)
                                                     (side . ,dotspacemacs-helm-position)
                                                     (window-width . 0.6)
                                                     (window-height . 0.4)))
      (defvar spacemacs-display-buffer-alist nil)
      (defun spacemacs//helm-prepare-display ()
        "Prepare necessary settings to make Helm display properly."
        ;; avoid Helm buffer being diplaye twice when user
        ;; sets this variable to some function that pop buffer to
        ;; a window. See https://github.com/syl20bnr/spacemacs/issues/1396
        (let ((display-buffer-base-action '(nil)))
          (setq spacemacs-display-buffer-alist display-buffer-alist)
          ;; the only buffer to display is Helm, nothing else we must set this
          ;; otherwise Helm cannot reuse its own windows for copyinng/deleting
          ;; etc... because of existing popwin buffers in the alist
          (setq display-buffer-alist nil)
          (popwin-mode -1)))

      (defun spacemacs//display-helm-window (buffer)
        (let ((display-buffer-alist (list spacemacs-helm-display-help-buffer-regexp
                                          ;; this or any specialized case of Helm buffer must be added AFTER
                                          ;; `spacemacs-helm-display-buffer-regexp'. Otherwise,
                                          ;; `spacemacs-helm-display-buffer-regexp' will be used before
                                          ;; `spacemacs-helm-display-help-buffer-regexp' and display
                                          ;; configuration for normal Helm buffer is applied for helm help
                                          ;; buffer, making the help buffer unable to be displayed.
                                          spacemacs-helm-display-buffer-regexp)))
          (helm-default-display-buffer buffer)))

      (setq helm-display-function 'spacemacs//display-helm-window)

      (defun spacemacs//restore-previous-display-config ()
        (popwin-mode 1)
        ;; we must enable popwin-mode first then restore `display-buffer-alist'
        ;; Otherwise, popwin keeps adding up its own buffers to `display-buffer-alist'
        ;; and could slow down Emacs as the list grows
        (setq display-buffer-alist spacemacs-display-buffer-alist))

      (add-hook 'helm-after-initialize-hook 'spacemacs//helm-prepare-display)
      ;;  Restore popwin-mode after a Helm session finishes.
      (add-hook 'helm-cleanup-hook 'spacemacs//restore-previous-display-config)

      ;; Add minibuffer history with `helm-minibuffer-history'
      (define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

      (defun spacemacs//helm-cleanup ()
        "Cleanup some helm related states when quitting."
        ;; deactivate any running transient map (micro-state)
        (setq overriding-terminal-local-map nil))
      (add-hook 'helm-cleanup-hook 'spacemacs//helm-cleanup)

      (defface spacemacs-helm-navigation-ms-face
        `((t :background ,(face-attribute 'error :foreground) :foreground "black"))
        "Face for helm heder when helm micro-state is activated."
        :group 'spacemacs))

    :config
    (progn
      (helm-mode +1)
      (defun spacemacs//set-dotted-directory ()
        "Set the face of diretories for `.' and `..'"
        (set-face-attribute 'helm-ff-dotted-directory
                            nil
                            :foreground nil
                            :background nil
                            :inherit 'helm-ff-directory))
      (add-hook 'helm-find-files-before-init-hook 'spacemacs//set-dotted-directory)

      ;; alter helm-bookmark key bindings to be simpler
      (defun simpler-helm-bookmark-keybindings ()
        (define-key helm-bookmark-map (kbd "C-d") 'helm-bookmark-run-delete)
        (define-key helm-bookmark-map (kbd "C-e") 'helm-bookmark-run-edit)
        (define-key helm-bookmark-map (kbd "C-f") 'helm-bookmark-toggle-filename)
        (define-key helm-bookmark-map (kbd "C-o") 'helm-bookmark-run-jump-other-window)
        (define-key helm-bookmark-map (kbd "C-/") 'helm-bookmark-help))
      (add-hook 'helm-mode-hook 'simpler-helm-bookmark-keybindings)

      ;; helm navigation on hjkl
      (defun spacemacs//helm-hjkl-navigation (&optional arg)
        "Set navigation in helm on `jklh'.
ARG non nil means that the editing style is `vim'."
        (cond
         (arg
          ;; better navigation on homerow
          ;; rebind `describe-key' for convenience
          (define-key helm-map (kbd "C-j") 'helm-next-line)
          (define-key helm-map (kbd "C-k") 'helm-previous-line)
          (define-key helm-map (kbd "C-h") 'helm-next-source)
          (define-key helm-map (kbd "C-S-h") 'describe-key)
          (define-key helm-map (kbd "C-l") (kbd "RET"))
          (dolist (keymap (list helm-find-files-map helm-read-file-map))
            (define-key keymap (kbd "C-l") 'helm-execute-persistent-action)
            (define-key keymap (kbd "C-h") 'helm-find-files-up-one-level)
            (define-key keymap (kbd "C-S-h") 'describe-key)))
         (t
          (define-key helm-map (kbd "C-j") 'helm-execute-persistent-action)
          (define-key helm-map (kbd "C-k") 'helm-delete-minibuffer-contents)
          (define-key helm-map (kbd "C-h") nil)
          (define-key helm-map (kbd "C-l") 'helm-recenter-top-bottom-other-window))))
      (spacemacs//helm-hjkl-navigation (member dotspacemacs-editing-style '(vim hybrid)))

      (defun spacemacs/helm-edit ()
        "Switch in edit mode depending on the current helm buffer."
        (interactive)
        (cond
         ((string-equal "*helm-ag*" helm-buffer)
          (helm-ag-edit))))

      (defun spacemacs//helm-navigation-ms-on-enter ()
        "Initialization of helm micro-state."
        ;; faces
        (spacemacs//helm-navigation-ms-set-face)
        (setq spacemacs--helm-navigation-ms-face-cookie-minibuffer
              (face-remap-add-relative
               'minibuffer-prompt
               'spacemacs-helm-navigation-ms-face))
        ;; bind actions on numbers starting from 1 which executes action 0
        (dotimes (n 10)
          (define-key helm-map (number-to-string n)
            `(lambda () (interactive) (helm-select-nth-action
                                       ,(% (+ n 9) 10))))))

      (defun spacemacs//helm-navigation-ms-set-face ()
        "Set the face for helm header in helm navigation micro-state"
        (with-helm-window
          (setq spacemacs--helm-navigation-ms-face-cookie-header
                (face-remap-add-relative
                 'helm-header
                 'spacemacs-helm-navigation-ms-face))))

      (defun spacemacs//helm-navigation-ms-on-exit ()
        "Action to perform when exiting helm micro-state."
        ;; restore helm key map
        (dotimes (n 10) (define-key helm-map (number-to-string n) nil))
        ;; restore faces
        (with-helm-window
          (face-remap-remove-relative
           spacemacs--helm-navigation-ms-face-cookie-header))
        (face-remap-remove-relative
         spacemacs--helm-navigation-ms-face-cookie-minibuffer))

      (defun spacemacs//helm-navigation-ms-full-doc ()
        "Full documentation for helm navigation micro-state."
        "
  [?]          display this help
  [a]          toggle action selection page
  [e]          edit occurrences if supported
  [j] [k]      next/previous candidate
  [h] [l]      previous/next source
  [t]          toggle visible mark
  [T]          toggle all mark
  [v]          persistent action
  [q]          quit")

      (spacemacs|define-micro-state helm-navigation
        :persistent t
        :disable-evil-leader t
        :define-key (helm-map . "M-SPC") (helm-map . "s-M-SPC")
        :on-enter (spacemacs//helm-navigation-ms-on-enter)
        :on-exit  (spacemacs//helm-navigation-ms-on-exit)
        :bindings
        ("<tab>" helm-select-action :exit t)
        ("C-i" helm-select-action :exit t)
        ("<RET>" helm-maybe-exit-minibuffer :exit t)
        ("?" nil :doc (spacemacs//helm-navigation-ms-full-doc))
        ("a" helm-select-action :post (spacemacs//helm-navigation-ms-set-face))
        ("e" spacemacs/helm-edit)
        ("h" helm-previous-source)
        ("j" helm-next-line)
        ("k" helm-previous-line)
        ("l" helm-next-source)
        ("q" nil :exit t)
        ("t" helm-toggle-visible-mark)
        ("T" helm-toggle-all-marks)
        ("v" helm-execute-persistent-action))

      ;; Swap default TAB and C-z commands.
      ;; For GUI.
      (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
      ;; For terminal.
      (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
      (define-key helm-map (kbd "C-z") 'helm-select-action)

      (eval-after-load "helm-mode" ; required
        '(spacemacs|hide-lighter helm-mode)))))

(defun spacemacs-base/init-helm-descbinds ()
  (use-package helm-descbinds
    :defer t
    :init
    (progn
      (setq helm-descbinds-window-style 'split)
      (add-hook 'helm-mode-hook 'helm-descbinds-mode)
      (evil-leader/set-key "?" 'helm-descbinds))))

(defun spacemacs-base/init-helm-projectile ()
  (use-package helm-projectile
    :commands (helm-projectile-switch-to-buffer
               helm-projectile-find-dir
               helm-projectile-dired-find-dir
               helm-projectile-recentf
               helm-projectile-find-file
               helm-projectile-grep
               helm-projectile
               helm-projectile-switch-project)
    :init
    (progn
      (setq projectile-switch-project-action 'helm-projectile)

      (defconst spacemacs-use-helm-projectile t
        "This variable is only defined if helm-projectile is used.")

      ;; needed for smart search if user's default tool is grep
      (defalias 'spacemacs/helm-project-do-grep 'helm-projectile-grep)
      (defalias
        'spacemacs/helm-project-do-grep-region-or-symbol 'helm-projectile-grep)

      (evil-leader/set-key
        "pb"  'helm-projectile-switch-to-buffer
        "pd"  'helm-projectile-find-dir
        "pf"  'helm-projectile-find-file
        "ph"  'helm-projectile
        "pp"  'helm-projectile-switch-project
        "pr"  'helm-projectile-recentf
        "pv"  'projectile-vc
        "sgp" 'helm-projectile-grep))))

(defun spacemacs-base/init-helm-spacemacs ()
  (use-package helm-spacemacs
    :commands helm-spacemacs
    :init
    (evil-leader/set-key "feh" 'helm-spacemacs)))

(defun spacemacs-base/init-hs-minor-mode ()
  ;; required for evil folding
  (defun spacemacs//enable-hs-minor-mode ()
    "Enable hs-minor-mode for code folding."
    (ignore-errors
      (hs-minor-mode)
      (spacemacs|hide-lighter hs-minor-mode)))
  (add-hook 'prog-mode-hook 'spacemacs//enable-hs-minor-mode))

(defun spacemacs-base/init-holy-mode ()
  (use-package holy-mode
    :commands holy-mode
    :init
    (progn
      (when (eq 'emacs dotspacemacs-editing-style)
        (holy-mode))
      (spacemacs|add-toggle holy-mode
        :status holy-mode
        :on (progn (when (bound-and-true-p hybrid-mode)
                     (hybrid-mode -1))
                   (holy-mode))
        :off (holy-mode -1)
        :documentation "Globally toggle holy mode."
        :evil-leader "tEe")
      (spacemacs|diminish holy-mode " Ⓔe" " Ee"))))

(defun spacemacs-base/init-hybrid-mode ()
  (use-package hybrid-mode
    :config
    (progn
      (when (eq 'hybrid dotspacemacs-editing-style) (hybrid-mode))
      (spacemacs|add-toggle hybrid-mode
        :status hybrid-mode
        :on (progn (when (bound-and-true-p holy-mode)
                     (holy-mode -1))
                   (hybrid-mode))
        :off (hybrid-mode -1)
        :documentation "Globally toggle hybrid mode."
        :evil-leader "tEh")
      (spacemacs|diminish hybrid-mode " Ⓔh" " Eh"))))

(defun spacemacs-base/init-ido ()
  (ido-mode t)
  (setq ido-save-directory-list-file (concat spacemacs-cache-directory
                                             "ido.last")
        ;; enable fuzzy matching
        ido-enable-flex-matching t))

(defun spacemacs-base/init-ido-vertical-mode ()
  (use-package ido-vertical-mode
    :init
    (progn
      (ido-vertical-mode t)
      (when dotspacemacs-use-ido
        (evil-leader/set-key "ff" 'ido-find-file))
      (defun spacemacs//ido-minibuffer-setup ()
        "Setup the minibuffer."
        ;; Since ido is implemented in a while loop where each
        ;; iteration setup a whole new minibuffer, we have to keep
        ;; track of any activated ido navigation micro-state and force
        ;; the reactivation at each iteration.
        (when spacemacs--ido-navigation-ms-enabled
          (spacemacs/ido-navigation-micro-state)))
      (add-hook 'ido-minibuffer-setup-hook 'spacemacs//ido-minibuffer-setup)

      (defun spacemacs//ido-setup ()
        (when spacemacs--ido-navigation-ms-face-cookie-minibuffer
          (face-remap-remove-relative
           spacemacs--ido-navigation-ms-face-cookie-minibuffer))
        ;; be sure to wipe any previous micro-state flag
        (setq spacemacs--ido-navigation-ms-enabled nil)
        ;; overwrite the key bindings for ido vertical mode only
        (define-key ido-completion-map (kbd "C-<return>") 'ido-select-text)
        ;; use M-RET in terminal
        (define-key ido-completion-map "\M-\r" 'ido-select-text)
        (define-key ido-completion-map (kbd "C-h") 'ido-delete-backward-updir)
        (define-key ido-completion-map (kbd "C-j") 'ido-next-match)
        (define-key ido-completion-map (kbd "C-k") 'ido-prev-match)
        (define-key ido-completion-map (kbd "C-l") 'ido-exit-minibuffer)
        (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
        (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
        (define-key ido-completion-map (kbd "C-S-h") 'ido-prev-match-dir)
        (define-key ido-completion-map (kbd "C-S-j") 'next-history-element)
        (define-key ido-completion-map (kbd "C-S-k") 'previous-history-element)
        (define-key ido-completion-map (kbd "C-S-l") 'ido-next-match-dir)
        (define-key ido-completion-map (kbd "C-S-n") 'next-history-element)
        (define-key ido-completion-map (kbd "C-S-p") 'previous-history-element)
        ;; ido-other window maps
        (define-key ido-completion-map (kbd "C-o") 'spacemacs/ido-invoke-in-other-window)
        (define-key ido-completion-map (kbd "C-s") 'spacemacs/ido-invoke-in-vertical-split)
        (define-key ido-completion-map (kbd "C-t") 'spacemacs/ido-invoke-in-new-frame)
        (define-key ido-completion-map (kbd "C-v") 'spacemacs/ido-invoke-in-horizontal-split)
        ;; more natural navigation keys: up, down to change current item
        ;; left to go up dir
        ;; right to open the selected item
        (define-key ido-completion-map (kbd "<up>") 'ido-prev-match)
        (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
        (define-key ido-completion-map (kbd "<left>") 'ido-delete-backward-updir)
        (define-key ido-completion-map (kbd "<right>") 'ido-exit-minibuffer)
        ;; initiate micro-state
        (define-key ido-completion-map (kbd "M-SPC") 'spacemacs/ido-navigation-micro-state)
        (define-key ido-completion-map (kbd "s-M-SPC") 'spacemacs/ido-navigation-micro-state)
        )
      (add-hook 'ido-setup-hook 'spacemacs//ido-setup)

      (defun spacemacs/ido-invoke-in-other-window ()
        "signals ido mode to switch to (or create) another window after exiting"
        (interactive)
        (setq ido-exit-minibuffer-target-window 'other)
        (ido-exit-minibuffer))

      (defun spacemacs/ido-invoke-in-horizontal-split ()
        "signals ido mode to split horizontally and switch after exiting"
        (interactive)
        (setq ido-exit-minibuffer-target-window 'horizontal)
        (ido-exit-minibuffer))

      (defun spacemacs/ido-invoke-in-vertical-split ()
        "signals ido mode to split vertically and switch after exiting"
        (interactive)
        (setq ido-exit-minibuffer-target-window 'vertical)
        (ido-exit-minibuffer))

      (defun spacemacs/ido-invoke-in-new-frame ()
        "signals ido mode to create a new frame after exiting"
        (interactive)
        (setq ido-exit-minibuffer-target-window 'frame)
        (ido-exit-minibuffer))

      (defadvice ido-read-internal
          (around ido-read-internal-with-minibuffer-other-window activate)
        (let* (ido-exit-minibuffer-target-window
               (this-buffer (current-buffer))
               (result ad-do-it))
          (cond
           ((equal ido-exit-minibuffer-target-window 'other)
            (if (= 1 (count-windows))
                (spacemacs/split-window-horizontally-and-switch)
              (other-window 1)))
           ((equal ido-exit-minibuffer-target-window 'horizontal)
            (spacemacs/split-window-horizontally-and-switch))

           ((equal ido-exit-minibuffer-target-window 'vertical)
            (spacemacs/split-window-vertically-and-switch))
           ((equal ido-exit-minibuffer-target-window 'frame)
            (make-frame)))
          ;; why? Some ido commands, such as textmate.el's
          ;; textmate-goto-symbol don't switch the current buffer
          (switch-to-buffer this-buffer)
          result))

      (defvar spacemacs--ido-navigation-ms-enabled nil
        "Flag which is non nil when ido navigation micro-state is enabled.")

      (defvar spacemacs--ido-navigation-ms-face-cookie-minibuffer nil
        "Cookie pointing to the local face remapping.")

      (defface spacemacs-ido-navigation-ms-face
        `((t :background ,(face-attribute 'error :foreground)
             :foreground "black"
             :weight bold))
        "Face for ido minibuffer prompt when ido micro-state is activated."
        :group 'spacemacs)

      (defun spacemacs//ido-navigation-ms-set-face ()
        "Set faces for ido navigation micro-state."
        (setq spacemacs--ido-navigation-ms-face-cookie-minibuffer
              (face-remap-add-relative
               'minibuffer-prompt
               'spacemacs-ido-navigation-ms-face)))

      (defun spacemacs//ido-navigation-ms-on-enter ()
        "Initialization of ido micro-state."
        (setq spacemacs--ido-navigation-ms-enabled t)
        (spacemacs//ido-navigation-ms-set-face))

      (defun spacemacs//ido-navigation-ms-on-exit ()
        "Action to perform when exiting ido micro-state."
        (face-remap-remove-relative
         spacemacs--ido-navigation-ms-face-cookie-minibuffer))

      (defun spacemacs//ido-navigation-ms-full-doc ()
        "Full documentation for ido navigation micro-state."
        "
  [?]          display this help
  [e]          enter dired
  [j] [k]      next/previous match
  [J] [K]      sub/parent directory
  [h]          delete backward or parent directory
  [l]          select match
  [n] [p]      next/previous directory in history
  [o]          open in other window
  [s]          open in a new horizontal split
  [t]          open in other frame
  [v]          open in a new vertical split
  [q]          quit")

      (spacemacs|define-micro-state ido-navigation
        :persistent t
        :disable-evil-leader t
        :on-enter (spacemacs//ido-navigation-ms-on-enter)
        :on-exit  (spacemacs//ido-navigation-ms-on-exit)
        :bindings
        ("?" nil :doc (spacemacs//ido-navigation-ms-full-doc))
        ("<RET>" ido-exit-minibuffer :exit t)
        ("<escape>" nil :exit t)
        ("e" ido-select-text :exit t)
        ("h" ido-delete-backward-updir)
        ("j" ido-next-match)
        ("J" ido-next-match-dir)
        ("k" ido-prev-match)
        ("K" ido-prev-match-dir)
        ("l" ido-exit-minibuffer :exit t)
        ("n" ido-next-match-dir)
        ("o" spacemacs/ido-invoke-in-other-window :exit t)
        ("p" ido-prev-match-dir)
        ("q" nil :exit t)
        ("s" spacemacs/ido-invoke-in-vertical-split :exit t)
        ("t" spacemacs/ido-invoke-in-new-frame :exit t)
        ("v" spacemacs/ido-invoke-in-horizontal-split :exit t)))))

(defun spacemacs-base/init-page-break-lines ()
  (use-package page-break-lines
    :init
    (global-page-break-lines-mode t)
    (spacemacs|hide-lighter page-break-lines-mode)))

(defun spacemacs-base/init-popup ()
  (use-package popup
    :defer t))

(defun spacemacs-base/init-popwin ()
  (use-package popwin
    :config
    (progn
      (popwin-mode 1)
      (evil-leader/set-key "wpm" 'popwin:messages)
      (evil-leader/set-key "wpp" 'popwin:close-popup-window)

      ;; don't use default value but manage it ourselves
      (setq popwin:special-display-config nil)

      ;; buffers that we manage
      (push '("*Help*"                 :dedicated t :position bottom :stick t :noselect nil :height 0.4) popwin:special-display-config)
      (push '("*compilation*"          :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
      (push '("*Shell Command Output*" :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
      (push '("*Async Shell Command*"  :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
      (push '(" *undo-tree*"           :dedicated t :position bottom :stick t :noselect nil :height 0.4) popwin:special-display-config)
      (push '("*ert*"                  :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
      (push '("*grep*"                 :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
      (push '("*nosetests*"            :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
      (push '("^\*WoMan.+\*$" :regexp t             :position bottom                                   ) popwin:special-display-config)

      (defun spacemacs/remove-popwin-display-config (str)
        "Removes the popwin display configurations that matches the passed STR"
        (setq popwin:special-display-config
              (-remove (lambda (x) (if (and (listp x) (stringp (car x)))
                                       (string-match str (car x))))
                       popwin:special-display-config))))))

(defun spacemacs-base/init-process-menu ()
  (evilify process-menu-mode process-menu-mode-map))

(defun spacemacs-base/init-projectile ()
  (use-package projectile
    :commands (projectile-ack
               projectile-ag
               projectile-compile-project
               projectile-dired
               projectile-grep
               projectile-find-dir
               projectile-find-file
               projectile-find-tag
               projectile-find-test-file
               projectile-invalidate-cache
               projectile-kill-buffers
               projectile-multi-occur
               projectile-project-root
               projectile-recentf
               projectile-regenerate-tags
               projectile-replace
               projectile-run-async-shell-command-in-root
               projectile-run-shell-command-in-root
               projectile-switch-project
               projectile-switch-to-buffer
               projectile-vc)
    :init
    (progn
      ;; note for Windows: GNU find or Cygwin find must be in path
      ;; default parameters are not supported on Windows, we default
      ;; to simplest call to find.
      (when (spacemacs/system-is-mswindows)
        (setq projectile-generic-command "find . -type f"))
      (setq projectile-enable-caching t
            projectile-indexing-method 'alien
            projectile-sort-order 'recentf
            projectile-cache-file (concat spacemacs-cache-directory
                                          "projectile.cache")
            projectile-known-projects-file (concat spacemacs-cache-directory
                                                   "projectile-bookmarks.eld"))
      (unless (configuration-layer/package-usedp 'helm-projectile)
        (evil-leader/set-key
          "pb" 'projectile-switch-to-buffer
          "pd" 'projectile-find-dir
          "pf" 'projectile-find-file
          "ph" 'helm-projectile
          "pr" 'projectile-recentf
          "ps" 'projectile-switch-project))
      (evil-leader/set-key
        "p!" 'projectile-run-shell-command-in-root
        "p&" 'projectile-run-async-shell-command-in-root
        "pa" 'projectile-toggle-between-implementation-and-test
        "pc" 'projectile-compile-project
        "pD" 'projectile-dired
        "pG" 'projectile-regenerate-tags
        "pI" 'projectile-invalidate-cache
        "pk" 'projectile-kill-buffers
        "po" 'projectile-multi-occur
        "pR" 'projectile-replace
        "pT" 'projectile-find-test-file
        "py" 'projectile-find-tag))
    :config
    (progn
      (projectile-global-mode)
      (spacemacs|hide-lighter projectile-mode))))

(defun spacemacs-base/init-quelpa ())

(defun spacemacs-base/init-recentf ()
  (use-package recentf
    :defer t
    :init
    ;; lazy load recentf
    (add-hook 'find-file-hook (lambda () (unless recentf-mode
                                           (recentf-mode)
                                           (recentf-track-opened-file))))
    :config
    (add-to-list 'recentf-exclude (expand-file-name spacemacs-cache-directory))
    (add-to-list 'recentf-exclude (expand-file-name package-user-dir))
    (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
    (setq recentf-save-file (concat spacemacs-cache-directory "recentf"))
    (setq recentf-max-saved-items 100)
    (setq recentf-auto-cleanup 'never)
    (setq recentf-auto-save-timer (run-with-idle-timer 600 t 'recentf-save-list))))

(defun spacemacs-base/init-savehist ()
  (use-package savehist
    :init
    (progn
      ;; Minibuffer history
      (setq savehist-file (concat spacemacs-cache-directory "savehist")
            enable-recursive-minibuffers t ; Allow commands in minibuffers
            history-length 1000
            savehist-additional-variables '(mark-ring
                                            global-mark-ring
                                            search-ring
                                            regexp-search-ring
                                            extended-command-history)
            savehist-autosave-interval 60)
      (savehist-mode t))))

(defun spacemacs-base/init-saveplace ()
  (use-package saveplace
    :init
    (progn
      ;; Save point position between sessions
      (setq save-place t
            save-place-file (concat spacemacs-cache-directory "places")))))

(defun spacemacs-base/init-spacemacs-theme ()
  (use-package spacemacs-theme
    :defer t
    :init
    (progn
      (setq spacemacs-theme-comment-bg t)
      (setq spacemacs-theme-org-height t))))

(defun spacemacs-base/init-subword ()
  (unless (version< emacs-version "24.4")
    (use-package subword
      :defer t
      :init
      (progn
        (unless (category-docstring ?U)
          (define-category ?U "Uppercase")
          (define-category ?u "Lowercase"))
        (modify-category-entry (cons ?A ?Z) ?U)
        (modify-category-entry (cons ?a ?z) ?u)
        (make-variable-buffer-local 'evil-cjk-word-separating-categories)
        (defun spacemacs//subword-enable-camel-case ()
          "Add support for camel case to subword."
          (if subword-mode
              (push '(?u . ?U) evil-cjk-word-separating-categories)
            (setq evil-cjk-word-separating-categories
                  (default-value 'evil-cjk-word-separating-categories))))
        (add-hook 'subword-mode-hook 'spacemacs//subword-enable-camel-case)
        (spacemacs|add-toggle camel-case-motion
          :status subword-mode
          :on (subword-mode +1)
          :off (subword-mode -1)
          :documentation "Toggle CamelCase motions."
          :evil-leader "tc")
        (spacemacs|add-toggle camel-case-motion-globally
          :status subword-mode
          :on (global-subword-mode +1)
          :off (global-subword-mode -1)
          :documentation "Globally toggle CamelCase motions."
          :evil-leader "t C-c"))
      :config
      (spacemacs|diminish subword-mode " ⓒ" " c"))))

(defun spacemacs-base/init-undo-tree ()
  (use-package undo-tree
    :init
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)
    :config
    (spacemacs|hide-lighter undo-tree-mode)))

(defun spacemacs-base/init-uniquify ()
  (require 'uniquify)
  ;; When having windows with repeated filenames, uniquify them
  ;; by the folder they are in rather those annoying <2>,<3>,.. etc
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets
        ;; don't screw special buffers
        uniquify-ignore-buffers-re "^\\*"))

(defun spacemacs-base/init-use-package ())

(defun spacemacs-base/init-which-key ()
  (use-package which-key
    :init
    (progn
      (spacemacs|add-toggle which-key
        :status which-key-mode
        :on (which-key-mode)
        :off (which-key-mode -1)
        :documentation
        "Display a buffer with available key bindings."
        :evil-leader "tK")
      (let ((new-descriptions
             ;; being higher in this list means the replacement is applied later
             '(
               ("spacemacs/\\(.+\\)" . "\\1")
               ("spacemacs/toggle-\\(.+\\)" . "\\1")
               ("select-window-\\([0-9]\\)" . "window \\1")
               ("spacemacs/alternate-buffer" . "last buffer")
               ("spacemacs/toggle-mode-line-\\(.+\\)" . "\\1")
               ("avy-goto-word-or-subword-1" . "avy word")
               ("shell-command" . "shell cmd")
               ("spacemacs/default-pop-shell" . "open shell")
               ("spacemacs/helm-project-smart-do-search-region-or-symbol" . "smart search")
               ("helm-descbinds" . "show keybindings")
               ("sp-split-sexp" . "split sexp")
               ("avy-goto-line" . "avy line")
               ("universal-argument" . "universal arg")
               ("er/expand-region" . "expand region")
               ("helm-apropos" . "apropos")
               ("spacemacs/toggle-hybrid-mode" . "hybrid (hybrid-mode)")
               ("spacemacs/toggle-holy-mode" . "emacs (holy-mode)")
               ("evil-lisp-state-\\(.+\\)" . "\\1"))))
        (dolist (nd new-descriptions)
          ;; ensure the target matches the whole string
          (push (cons (concat "\\`" (car nd) "\\'") (cdr nd))
                which-key-description-replacement-alist)))
      (dolist (leader-key `(,dotspacemacs-leader-key ,dotspacemacs-emacs-leader-key))
        (which-key-add-key-based-replacements
         (concat leader-key " m")    "major mode commands"
         (concat leader-key " " dotspacemacs-command-key) "M-x"))
      (if (fboundp 'which-key-declare-prefixes)
          (which-key-declare-prefixes
            dotspacemacs-leader-key '("root" . "Spacemacs root")
            dotspacemacs-emacs-leader-key '("root" . "Spacemacs root")
            (concat dotspacemacs-leader-key " m")
            '("major-mode-cmd" . "Major mode commands")
            (concat dotspacemacs-emacs-leader-key " m")
            '("major-mode-cmd" . "Major mode commands"))
        ;; no need to use this after everyone updates which-key
        (setq which-key-prefix-title-alist
              `((,(listify-key-sequence
                   (kbd (concat dotspacemacs-leader-key " m"))) . "Major mode commands")
                (,(listify-key-sequence
                   (kbd (concat dotspacemacs-emacs-leader-key " m"))) . "Major mode commands")
                (,(listify-key-sequence
                   (kbd dotspacemacs-leader-key)) . "Spacemacs root")
                (,(listify-key-sequence
                   (kbd dotspacemacs-emacs-leader-key)) . "Spacemacs root")))
        (nconc which-key-prefix-title-alist spacemacs/prefix-titles))
      ;; disable special key handling for spacemacs, since it can be
      ;; disorienting if you don't understand it
      (pcase dotspacemacs-which-key-position
        (`right (which-key-setup-side-window-right))
        (`bottom (which-key-setup-side-window-bottom))
        (`right-then-bottom (which-key-setup-side-window-right-bottom)))
      (setq which-key-special-keys nil
            which-key-use-C-h-for-paging t
            which-key-prevent-C-h-from-cycling t
            which-key-echo-keystrokes 0.02
            which-key-max-description-length 32
            which-key-idle-delay dotspacemacs-which-key-delay)
      (which-key-mode)
      (spacemacs|diminish which-key-mode " Ⓚ" " K"))))

(defun spacemacs-base/init-whitespace ()
  (use-package whitespace
    :defer t
    :init
    (progn
      (setq spacemacs-show-trailing-whitespace t)
      (defun spacemacs//show-trailing-whitespace ()
        (when spacemacs-show-trailing-whitespace
          (set-face-attribute 'trailing-whitespace nil
                              :background
                              (face-attribute 'font-lock-comment-face
                                              :foreground))
          (setq show-trailing-whitespace 1)))
      (add-hook 'prog-mode-hook 'spacemacs//show-trailing-whitespace)

      (spacemacs|add-toggle whitespace
        :status whitespace-mode
        :on (whitespace-mode)
        :off (whitespace-mode -1)
        :documentation "Display whitespace."
        :evil-leader "tw")
      (spacemacs|add-toggle whitespace-globally
        :status global-whitespace-mode
        :on (global-whitespace-mode)
        :off (global-whitespace-mode -1)
        :documentation "Display whitespace globally."
        :evil-leader "t C-w")

      (defun spacemacs//set-whitespace-style-for-diff ()
        "Whitespace configuration for `diff-mode'"
        (setq-local whitespace-style '(face
                                       tabs
                                       tab-mark
                                       spaces
                                       space-mark
                                       trailing
                                       indentation::space
                                       indentation::tab
                                       newline
                                       newline-mark)))
      (add-hook 'diff-mode-hook 'whitespace-mode)
      (add-hook 'diff-mode-hook 'spacemacs//set-whitespace-style-for-diff))
    :config
    (progn
      (set-face-attribute 'whitespace-space nil
                          :background nil
                          :foreground (face-attribute 'font-lock-warning-face
                                                      :foreground))
      (set-face-attribute 'whitespace-tab nil
                          :background nil)
      (set-face-attribute 'whitespace-indentation nil
                          :background nil)
      (spacemacs|diminish whitespace-mode " ⓦ" " w")
      (spacemacs|diminish global-whitespace-mode " Ⓦ" " W"))))

(defun spacemacs-base/init-winner ()
  (use-package winner
    :init
    (progn
      (winner-mode t)
      (setq spacemacs/winner-boring-buffers '("*Completions*"
                                              "*Compile-Log*"
                                              "*inferior-lisp*"
                                              "*Fuzzy Completions*"
                                              "*Apropos*"
                                              "*Help*"
                                              "*cvs*"
                                              "*Buffer List*"
                                              "*Ibuffer*"
                                              "*esh command on file*"
                                              ))
      (setq winner-boring-buffers
            (append winner-boring-buffers spacemacs/winner-boring-buffers))
      (winner-mode t))))
