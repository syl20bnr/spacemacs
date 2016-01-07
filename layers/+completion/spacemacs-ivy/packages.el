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

(setq spacemacs-ivy-packages
      '(counsel
        flx
        hydra
        (ivy-spacemacs-help :location local)
        smex
        swiper))

;; Initialization of packages

(defun spacemacs-ivy/init-counsel ()
  (defvar spacemacs--counsel-commands
    '(("ag" . "ag --vimgrep %S .")
      ("pt" . "pt.exe -e --nocolor --nogroup --column %S .")
      ("ack" . "ack --nocolor --nogroup --column %S .")
      ("grep" . "grep -nrP %S ."))
    "Alist of search commands and their corresponding commands
with options to run in the shell.")

  (defvar spacemacs--counsel-search-max-path-length 30
    "Truncate the current path in counsel search if it is longer
than this amount.")

  ;; see `counsel-ag-function'
  (defun spacemacs//make-counsel-search-function (tool)
    (lexical-let ((base-cmd
                   (cdr (assoc-string tool spacemacs--counsel-commands))))
      (lambda (string &optional _pred &rest _unused)
        "Grep in the current directory for STRING."
        (if (< (length string) 3)
            (counsel-more-chars 3)
          (let ((default-directory counsel--git-grep-dir)
                (regex (counsel-unquote-regex-parens
                        (setq ivy--old-re
                              (ivy--regex string)))))
            (counsel--async-command (format base-cmd regex))
            nil)))))

  ;; see `counsel-ag'
  (defun spacemacs/counsel-search
      (&optional tools use-initial-input initial-directory)
    "Search using the first available tool in TOOLS. Default tool
to try is grep. If INPUT is non nil, use the region or the symbol
at the point as the initial input. If DIR is non nil start in
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
      (setq counsel--git-grep-dir (or initial-directory default-directory))
      (ivy-read
       (format "%s from [%s]: "
               tool
               (if (< (length counsel--git-grep-dir)
                      spacemacs--counsel-search-max-path-length)
                   counsel--git-grep-dir
                 (concat "..."
                         (substring counsel--git-grep-dir
                                    (- (length counsel--git-grep-dir)
                                       spacemacs--counsel-search-max-path-length)
                                    (length counsel--git-grep-dir)))))
       (spacemacs//make-counsel-search-function tool)
       :initial-input initial-input
       :dynamic-collection t
       :history 'counsel-git-grep-history
       :action #'counsel-git-grep-action
       :unwind (lambda ()
                 (counsel-delete-process)
                 (swiper--cleanup)))))

  (cl-loop for (tools tool-name) in '((dotspacemacs-search-tools  "auto")
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
the selected region or the symbol under the point in the current
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
the selected region or the symbol under the point in the current
project with %s." (if (string= tool-name "auto")
                      "a tool selected from `dotspacemacs-search-tools'."
                    tool-name))
                 (interactive)
                 (spacemacs/counsel-search ,tools t (projectile-project-root))))))

  (defun spacemacs/counsel-git-grep-region-or-symbol ()
    "Use `counsel-git-grep' to search for the selected region or
the symbol under the point in the current project with git grep."
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
    (spacemacs/set-leader-keys
      dotspacemacs-command-key 'counsel-M-x
      ;; files
      "ff"  'counsel-find-file
      "fL"  'counsel-locate
      ;; help
      "hdf" 'counsel-describe-function
      "hdv" 'counsel-describe-variable
      ;; insert
      "iu"  'counsel-unicode-char
      ;; projects
      "pp"  'projectile-switch-project
      ;; jumping
      "sj"  'counsel-imenu
      ;; themes
      "Tc"  'counsel-load-theme
      ;; search
      "/"   'spacemacs/search-project-auto
      "*"   'spacemacs/search-project-auto-region-or-symbol
      "sf"  'spacemacs/search-auto
      "sF"  'spacemacs/search-auto-region-or-symbol
      "sp"  'spacemacs/search-project
      "sP"  'spacemacs/search-project-region-or-symbol
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
    ;; Commands to port
    (spacemacs//ivy-command-not-implemented-yet "?")))

(defun spacemacs-ivy/init-flx ())

(defun spacemacs-ivy/init-hydra ())

(defun spacemacs-ivy/init-ivy-spacemacs-help ()
  (use-package ivy-spacemacs-help
    :init
    (progn
      (spacemacs/set-leader-keys
        "h SPC d"    'ivy-spacemacs-help-docs
        "h SPC ."    'ivy-spacemacs-help-dotspacemacs
        ;; "h SPC f" 'ivy-spacemacs-help-faq
        "h SPC l"    'ivy-spacemacs-help-layers
        "h SPC SPC"  'ivy-spacemacs-help
        "h SPC p"    'ivy-spacemacs-help-packages
        "h SPC t"    'ivy-spacemacs-help-toggles))))

(defun spacemacs-ivy/init-smex ()
  (use-package smex
    :defer t
    :init
    (progn
      (setq-default smex-history-length 32
                    smex-save-file (concat spacemacs-cache-directory
                                           ".smex-items")))))

(defun spacemacs-ivy/init-swiper ()
  (use-package ivy
    :config
    (spacemacs/set-leader-keys
      "fr" 'ivy-recentf
      "ir" 'ivy-resume
      "bb" 'ivy-switch-buffer)
    (setq ivy-height 15)
    (with-eval-after-load 'recentf
      ;; merge recentf and bookmarks into buffer switching. If we set this
      ;; before recentf loads, then ivy-mode loads recentf for us, which messes
      ;; up the spacemacs version of recentf.
      (setq ivy-use-virtual-buffers t))
    (when (configuration-layer/package-usedp 'projectile)
      (setq projectile-completion-system 'ivy))
    (ivy-mode 1)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (spacemacs//hjkl-completion-navigation))

  (use-package swiper
    :config
    (defun spacemacs/swiper-region-or-symbol ()
      "Run `swiper' with the selected region or the symbol under
the point as the initial input."
      (interactive)
      (let ((input (if (region-active-p)
                       (buffer-substring-no-properties
                        (region-beginning) (region-end))
                     (thing-at-point 'symbol t))))
        (swiper--ivy input)))

    (defun spacemacs/swiper-all-region-or-symbol ()
      "Run `swiper-all' with the selected region or the symbol
under the point as the initial input."
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
    (global-set-key "\C-s" 'swiper)))
