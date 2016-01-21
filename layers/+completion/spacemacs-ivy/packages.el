;;; packages.el --- Spacemacs Core Layer packages File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq spacemacs-ivy-packages
      '(counsel
        flx
        ;; hack since ivy is part for swiper but I like to
        ;; treat it as a stand-alone package
        (ivy :location built-in)
        (ivy-spacemacs-help :location local)
        smex
        swiper))

(defun spacemacs-ivy/init-counsel ()
  (defvar spacemacs--counsel-commands
    '(("ag" . "ag --vimgrep %s %S .")
      ("pt" . "pt -e --nocolor --nogroup --column %s %S .")
      ("ack" . "ack --nocolor --nogroup --column %s %S .")
      ("grep" . "grep -nrP %s %S ."))
    "Alist of search commands and their corresponding commands
with options to run in the shell.")

  (defvar spacemacs--counsel-search-max-path-length 30
    "Truncate the current path in counsel search if it is longer
than this amount.")

  (defvar spacemacs--counsel-initial-cands-shown nil)
  (defvar spacemacs--counsel-initial-number-cand 100)

  (defun spacemacs//counsel-async-command (cmd)
    (let* ((counsel--process " *counsel*")
           (proc (get-process counsel--process))
           (buff (get-buffer counsel--process)))
      (when proc
        (delete-process proc))
      (when buff
        (kill-buffer buff))
      (setq proc (start-process-shell-command
                  counsel--process
                  counsel--process
                  cmd))
      (setq spacemacs--counsel-initial-cands-shown nil)
      (setq counsel--async-time (current-time))
      (set-process-sentinel proc #'counsel--async-sentinel)
      (set-process-filter proc #'spacemacs//counsel-async-filter)))

  (defun spacemacs//counsel-async-filter (process str)
    (with-current-buffer (process-buffer process)
      (insert str))
    (when (or (null spacemacs--counsel-initial-cands-shown)
              (time-less-p
               ;; 0.5s
               '(0 0 500000 0)
               (time-since counsel--async-time)))
      (let (size display-now)
        (with-current-buffer (process-buffer process)
          (goto-char (point-min))
          (setq size (- (buffer-size) (forward-line (buffer-size))))
          (when (and (null spacemacs--counsel-initial-cands-shown)
                     (> size spacemacs--counsel-initial-number-cand))
            (setq ivy--all-candidates
                  (split-string (buffer-string) "\n" t))
            (setq display-now t)
            (setq spacemacs--counsel-initial-cands-shown t)))
        (let ((ivy--prompt
               (format (ivy-state-prompt ivy-last)
                       size)))
          (if display-now
              (ivy--insert-minibuffer
               (ivy--format ivy--all-candidates))
            (ivy--insert-prompt))))
      (setq counsel--async-time (current-time))))

  ;; see `counsel-ag-function'
  (defun spacemacs//make-counsel-search-function (tool)
    (lexical-let ((base-cmd
                   (cdr (assoc-string tool spacemacs--counsel-commands))))
      (lambda (string &optional _pred &rest _unused)
        "Grep in the current directory for STRING."
        (if (< (length string) 3)
            (counsel-more-chars 3)
          (let* ((default-directory counsel--git-grep-dir)
                 (args (if (string-match-p " -- " string)
                           (let ((split (split-string string " -- ")))
                             (prog1 (pop split)
                               (setq string (mapconcat #'identity split " -- "))))
                         ""))
                 (regex (counsel-unquote-regex-parens
                         (setq ivy--old-re
                               (ivy--regex string)))))
            (spacemacs//counsel-async-command (format base-cmd args regex))
            nil)))))

  ;; see `counsel-ag'
  (defun spacemacs/counsel-search
      (&optional tools use-initial-input initial-directory)
    "Search using the first available tool in TOOLS. Default tool
to try is grep. If INPUT is non nil, use the region or the symbol
around point as the initial input. If DIR is non nil start in
that directory."
    (interactive)
    (require 'counsel)
    (letf* ((initial-input (when use-initial-input
                             (if (region-active-p)
                                 (buffer-substring-no-properties
                                  (region-beginning) (region-end))
                               (thing-at-point 'symbol t))))
            (tool (catch 'tool
                    (dolist (tool tools)
                      (when (and (assoc-string tool spacemacs--counsel-commands)
                                 (executable-find tool))
                        (throw 'tool tool)))
                    (throw 'tool "grep"))))
      (setq counsel--git-grep-dir
            (or initial-directory
                (read-directory-name "Start from directory: ")))
      (ivy-read
       (concat "%-5d "
               (format "%s from [%s]: "
                       tool
                       (if (< (length counsel--git-grep-dir)
                              spacemacs--counsel-search-max-path-length)
                           counsel--git-grep-dir
                         (concat
                          "..." (substring counsel--git-grep-dir
                                           (- (length counsel--git-grep-dir)
                                              spacemacs--counsel-search-max-path-length)
                                           (length counsel--git-grep-dir))))))
       (spacemacs//make-counsel-search-function tool)
       :initial-input initial-input
       :dynamic-collection t
       :history 'counsel-git-grep-history
       :action #'counsel-git-grep-action
       :unwind (lambda ()
                 (counsel-delete-process)
                 (swiper--cleanup)))))

  (cl-loop
   for (tools tool-name) in '((dotspacemacs-search-tools  "auto")
                              ((list "ag") "ag")
                              ((list "pt") "pt")
                              ((list "ack") "ack")
                              ((list "grep") "grep"))
   do
   (eval
    `(progn
       (defun ,(intern (format "spacemacs/search-%s" tool-name)) ()
         ,(format
           "Use `spacemacs/counsel-search' to search in the current
 directory with %s." (if (string= tool-name "auto")
                         "a tool selected from `dotspacemacs-search-tools'."
                       tool-name))
         (interactive)
         (spacemacs/counsel-search ,tools))
       (defun ,(intern (format "spacemacs/search-%s-region-or-symbol"
                               tool-name)) ()
         ,(format
           "Use `spacemacs/counsel-search' to search for
 the selected region or the symbol around point in the current
 directory with %s." (if (string= tool-name "auto")
                         "a tool selected from `dotspacemacs-search-tools'."
                       tool-name))
         (interactive)
         (spacemacs/counsel-search ,tools t))
       (defun ,(intern (format "spacemacs/search-project-%s" tool-name)) ()
         ,(format
           "Use `spacemacs/counsel-search' to search in the current
 project with %s." (if (string= tool-name "auto")
                       "a tool selected from `dotspacemacs-search-tools'."
                     tool-name))
         (interactive)
         (spacemacs/counsel-search ,tools nil (projectile-project-root)))
       (defun ,(intern (format "spacemacs/search-project-%s-region-or-symbol"
                               tool-name)) ()
         ,(format
           "Use `spacemacs/counsel-search' to search for
 the selected region or the symbol around point in the current
 project with %s." (if (string= tool-name "auto")
                       "a tool selected from `dotspacemacs-search-tools'."
                     tool-name))
         (interactive)
         (spacemacs/counsel-search ,tools t (projectile-project-root))))))

  (defun spacemacs/counsel-search-docs ()
    "Search spacemacs docs using `spacemacs/counsel-search'"
    (interactive)
    (spacemacs/counsel-search dotspacemacs-search-tools
                              nil spacemacs-docs-directory))

  (defun spacemacs/counsel-git-grep-region-or-symbol ()
    "Use `counsel-git-grep' to search for the selected region or
 the symbol around point in the current project with git grep."
    (let ((input (if (region-active-p)
                     (buffer-substring-no-properties
                      (region-beginning) (region-end))
                   (thing-at-point 'symbol t))))
      (counsel-git-grep nil input)))

  (defun spacemacs//ivy-command-not-implemented-yet (key)
    (lexical-let ((-key key))
      (spacemacs/set-leader-keys
        -key (lambda ()
               (interactive)
               (message "The command usually bound to %s %s has \
 not been implemented for the spacemacs-ivy layer yet."
                        dotspacemacs-leader-key -key)))))

  (use-package counsel
    :config
    (progn
      (defun spacemacs/describe-mode ()
        "Dummy wrapper to prevent an key binding error from helm.

By default the emacs leader is M-m, turns out that Helm does this:

   (cl-dolist (k (where-is-internal 'describe-mode global-map))
        (define-key map k 'helm-help))

after doing this:

   (define-key map (kbd \"M-m\") 'helm-toggle-all-marks)

So when Helm is loaded we get the error:

   Key sequence M-m h d m starts with non-prefix key M-m

To prevent this error we just wrap `describe-mode' to defeat the
Helm hack."
        (interactive)
        (call-interactively 'describe-mode))

      (define-key counsel-find-file-map (kbd "C-h") 'counsel-up-directory)
      (spacemacs/set-leader-keys
        dotspacemacs-emacs-command-key 'counsel-M-x
        ;; files
        "ff"  'counsel-find-file
        "fL"  'counsel-locate
        ;; help
        "?"   'counsel-descbinds
        "hdf" 'counsel-describe-function
        "hdm" 'spacemacs/describe-mode
        "hdv" 'counsel-describe-variable
        "hR"  'spacemacs/counsel-search-docs
        ;; insert
        "iu"  'counsel-unicode-char
        ;; jump
        ;; projects
        "pp"  'projectile-switch-project
        "pv"  'projectile-vc
        ;; register/ring
        "ry"  'counsel-yank-pop
        ;; jumping
        "sj"  'counsel-imenu
        ;; themes
        "Tc"  'counsel-load-theme
        ;; search
        "/"   'spacemacs/search-project-auto
        "*"   'spacemacs/search-project-auto-region-or-symbol
        "sf"  'spacemacs/search-auto
        "sF"  'spacemacs/search-auto-region-or-symbol
        "sp"  'spacemacs/search-project-auto
        "sP"  'spacemacs/search-project-auto-region-or-symbol
        "saf" 'spacemacs/search-ag
        "saF" 'spacemacs/search-ag-region-or-symbol
        "sap" 'spacemacs/search-project-ag
        "saP" 'spacemacs/search-project-ag-region-or-symbol
        "stf" 'spacemacs/search-pt
        "stF" 'spacemacs/search-pt-region-or-symbol
        "stp" 'spacemacs/search-project-pt
        "stP" 'spacemacs/search-project-pt-region-or-symbol
        "sgf" 'spacemacs/search-grep
        "sgF" 'spacemacs/search-grep-region-or-symbol
        "sgp" 'counsel-git-grep
        "sgP" 'spacemacs/counsel-git-grep-region-or-symbol
        "skf" 'spacemacs/search-ack
        "skF" 'spacemacs/search-ack-region-or-symbol
        "skp" 'spacemacs/search-project-ack
        "skP" 'spacemacs/search-project-ack-region-or-symbol)
      (global-set-key (kbd "M-x") 'counsel-M-x)

      ;; Note: Must be set before which-key is loaded.
      (setq prefix-help-command 'counsel-descbinds)
      ;; TODO: Commands to port
      (spacemacs//ivy-command-not-implemented-yet "jI"))))

(defun spacemacs-ivy/init-flx ())

(defun spacemacs-ivy/init-ivy ()
  (use-package ivy
    :config
    (progn
      (spacemacs/set-leader-keys
        "a'" 'spacemacs/ivy-available-repls
        "fr" 'ivy-recentf
        "ir" 'ivy-resume
        "bb" 'ivy-switch-buffer)
      (setq ivy-height 15
            ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
      (with-eval-after-load 'recentf
        ;; merge recentf and bookmarks into buffer switching. If we set this
        ;; before recentf loads, then ivy-mode loads recentf for us,
        ;; which messes up the spacemacs version of recentf.
        (setq ivy-use-virtual-buffers t))
      (when (configuration-layer/package-usedp 'projectile)
        (setq projectile-completion-system 'ivy))
      (spacemacs|hide-lighter ivy-mode)
      (ivy-mode 1)
      (global-set-key (kbd "C-c C-r") 'ivy-resume)
      (global-set-key (kbd "<f6>") 'ivy-resume)

      (defun spacemacs/ivy-available-repls ()
        "Show available repls."
        (interactive)
        (ivy-read "Repls: "
                  (mapcar #'car spacemacs-repl-list)
                  :action (lambda (candidate)
                            (let ((repl (cdr (assoc candidate spacemacs-repl-list))))
                              (require (car repl))
                              (call-interactively (cdr repl))))))

      (defun spacemacs//hjkl-completion-navigation (&optional arg)
        "Set navigation on `jklh'. ARG non nil means Vim like movements."
        (cond
         (arg
          ;; better navigation on homerow
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
      (spacemacs//hjkl-completion-navigation
       (member dotspacemacs-editing-style '(vim hybrid))))))

(defun spacemacs-ivy/init-smex ()
  (use-package smex
    :defer t
    :init (setq-default smex-history-length 32
                        smex-save-file (concat spacemacs-cache-directory
                                               ".smex-items"))))

(defun spacemacs-ivy/init-ivy-spacemacs-help ()
  (use-package ivy-spacemacs-help
    :commands (ivy-spacemacs-help-dotspacemacs
               ivy-spacemacs-help
               ivy-spacemacs-help-faq
               ivy-spacemacs-help-layers
               ivy-spacemacs-help-packages
               ivy-spacemacs-help-docs
               ivy-spacemacs-help-toggles)
    :init (spacemacs/set-leader-keys
            "h ."   'ivy-spacemacs-help-dotspacemacs
            "h SPC" 'ivy-spacemacs-help
            "h f"   'ivy-spacemacs-help-faq
            "h l"   'ivy-spacemacs-help-layers
            "h p"   'ivy-spacemacs-help-packages
            "h r"   'ivy-spacemacs-help-docs
            "h t"   'ivy-spacemacs-help-toggles)))

(defun spacemacs-ivy/init-swiper ()
  (use-package swiper
    :config
    (progn
      (defun spacemacs/swiper-region-or-symbol ()
        "Run `swiper' with the selected region or the symbol
around point as the initial input."
        (interactive)
        (let ((input (if (region-active-p)
                         (buffer-substring-no-properties
                          (region-beginning) (region-end))
                       (thing-at-point 'symbol t))))
          (swiper--ivy input)))

      (defun spacemacs/swiper-all-region-or-symbol ()
        "Run `swiper-all' with the selected region or the symbol
around point as the initial input."
        (interactive)
        (ivy-read "Swiper: " (swiper--multi-candidates
                              (cl-remove-if-not
                               #'buffer-file-name
                               (buffer-list)))
                  :initial-input (if (region-active-p)
                                     (buffer-substring-no-properties
                                      (region-beginning) (region-end))
                                   (thing-at-point 'symbol t))
                  :action 'swiper-multi-action-2
                  :unwind #'swiper--cleanup
                  :caller 'swiper-multi))

      (spacemacs/set-leader-keys
        "ss" 'swiper
        "sS" 'spacemacs/swiper-region-or-symbol
        "sb" 'swiper-all
        "sB" 'spacemacs/swiper-all-region-or-symbol)
      (global-set-key "\C-s" 'swiper))))
