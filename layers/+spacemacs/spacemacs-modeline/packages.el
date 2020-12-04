;;; packages.el --- Space-macs Mode-line Visual Layer packages File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(setq space-macs-modeline-packages
      '(
        anzu
        (doom-modeline :toggle (eq (space-macs/get-mode-line-theme-name) 'doom))
        fancy-battery
        ;; dependency of spaceline-all-the-icons which came from
        ;; the e-macs wiki, we fetch it from e-macs Mirror for now.
        ;; TODO eventually remove this if font-lock+ is available
        ;; in an ELPA repository.
        (font-lock+ :step pre
                    :location (recipe :fetcher github
                                      :repo e-macsmirror/font-lock-plus))
        neotree
        spaceline
        spaceline-all-the-icons
        symon
        (vim-powerline :location local)))

(defun space-macs-modeline/post-init-anzu ()
  (when (eq 'all-the-icons (space-macs/get-mode-line-theme-name))
    (spaceline-all-the-icons--setup-anzu)))

(defun space-macs-modeline/init-doom-modeline ()
  ;; doom modeline depends on `display-graphic-p' so we delay its initialization
  ;; as when dumping we don't know yet wether we are using a graphical e-macs or
  ;; not.
  (space-macs|unless-dumping-and-eval-after-loaded-dump doom-modeline
    (use-package doom-modeline
      :defer t
      :init (doom-modeline-mode))))

(defun space-macs-modeline/init-fancy-battery ()
  (use-package fancy-battery
    :defer t
    :init
    (progn
      (space-macs|add-toggle mode-line-battery
        :mode fancy-battery-mode
        :documentation "Display battery info in mode-line."
        :evil-leader "tmb")
      (setq-default fancy-battery-show-percentage t))))

(defun space-macs-modeline/init-font-lock+ ())

(defun space-macs-modeline/post-init-neotree ()
  (when (eq 'all-the-icons (space-macs/get-mode-line-theme-name))
    (spaceline-all-the-icons--setup-neotree)))

(defun space-macs-modeline/init-spaceline ()
  (use-package spaceline-config
    :if (memq (space-macs/get-mode-line-theme-name)
              '(space-macs all-the-icons custom))
    :init
    (progn
      (space-macs|require-when-dumping 'spaceline)
      (space-macs|when-dumping-strict
        (space-macs/spaceline-config-startup))
      (space-macs|unless-dumping
        (add-hook 'e-macs-startup-hook 'space-macs/spaceline-config-startup-hook))
      (add-hook 'space-macs-post-theme-change-hook
                'space-macs/customize-powerline-faces)
      (add-hook 'space-macs-post-theme-change-hook 'powerline-reset)
      (space-macs|add-toggle mode-line-responsive
        :status spaceline-responsive
        :on (progn (setq spaceline-responsive t)
                   (powerline-reset))
        :off (progn (setq spaceline-responsive nil)
                    ;; seems necessary to recompile when turning off
                    (spaceline-compile))
        :documentation "Make the mode-line responsive."
        :evil-leader "tmr")
      ;; Segment toggles
      (dolist (spec '((minor-modes "tmm")
                      (major-mode "tmM")
                      (version-control "tmv")
                      (new-version "tmV")
                      (point-position "tmp")
                      (org-clock "tmc")))
        (let* ((segment (car spec))
               (status-var (intern (format "spaceline-%S-p" segment))))
          (eval `(space-macs|add-toggle ,(intern (format "mode-line-%S" segment))
                   :status ,status-var
                   :on (setq ,status-var t)
                   :off (setq ,status-var nil)
                   :documentation ,(format "Show %s in the mode-line."
                                           (replace-regexp-in-string
                                            "-" " " (format "%S" segment)))
                   :evil-leader ,(cadr spec)))))
      (setq powerline-default-separator
            (cond
             ((space-macs-is-dumping-p) 'utf-8)
             ((memq (space-macs/get-mode-line-theme-name)
                    '(space-macs custom))
              (space-macs/mode-line-separator))
             (t 'wave))
            powerline-image-apple-rgb (eq window-system 'ns)
            powerline-scale (or (space-macs/mode-line-separator-scale) 1.5)
            spaceline-byte-compile nil))
    :config
    (progn
      (space-macs/customize-powerline-faces)
      (setq spaceline-org-clock-p nil
            spaceline-highlight-face-func 'space-macs//evil-state-face)
      ;; unicode
      (let ((unicodep (dotspace-macs|symbol-value
                       dotspace-macs-mode-line-unicode-symbols)))
        (setq spaceline-window-numbers-unicode unicodep
              spaceline-workspace-numbers-unicode unicodep))
      (add-hook 'spaceline-pre-hook 'space-macs//prepare-diminish)
      ;; New space-macs version segment
      (defpowerline space-macs-powerline-new-version
        (propertize
         space-macs-version-check-lighter
         'mouse-face 'mode-line-highlight
         'help-echo (format "New version %s | Click with mouse-1 to update"
                            space-macs-new-version)
         'local-map (let ((map (make-sparse-keymap)))
                      (define-key map
                        [mode-line down-mouse-1]
                        (lambda (event)
                          (interactive "@e")
                          (if (yes-or-no-p
                               (format
                                (concat "Do you want to update to the newest "
                                        "version %s ?")
                                space-macs-new-version))
                              (progn
                                (space-macs/switch-to-version
                                 space-macs-new-version))
                            (message "Update aborted."))))
                      map)))
      (spaceline-define-segment
          new-version
        (when space-macs-new-version
          (space-macs-powerline-new-version
           (space-macs/get-new-version-lighter-face
            space-macs-version space-macs-new-version))))
      (let ((theme (intern (format "spaceline-%S-theme"
                                   (space-macs/get-mode-line-theme-name)))))
        (apply theme space-macs-spaceline-additional-segments))
      ;; Additional spacelines
      (when (package-installed-p 'helm)
        (spaceline-helm-mode t))
      (when (configuration-layer/package-used-p 'info+)
        (spaceline-info-mode t))
      ;; Enable spaceline for buffers created before the configuration of
      ;; spaceline
      (space-macs//restore-buffers-powerline))))

(defun space-macs-modeline/pre-init-spaceline-all-the-icons ()
  (when (eq 'all-the-icons (space-macs/get-mode-line-theme-name))
    (space-macs|use-package-add-hook spaceline-config
      :pre-config
      (progn
        (require 'spaceline-all-the-icons)
        ;; responsivness does not play well with all-the-icons theme
        ;; let's disable it for now
        ;; https://github.com/domtronn/spaceline-all-the-icons.el/issues/51#issuecomment-316686790
        (setq spaceline-responsive nil)
        (spaceline-all-the-icons--setup-git-ahead)))))

(defun space-macs-modeline/init-spaceline-all-the-icons ()
  (use-package spaceline-all-the-icons
    :defer t
    :init
    (progn
      (setq
       spaceline-all-the-icons-separator-type
       (or (space-macs/mode-line-separator) 'wave)
       spaceline-all-the-icons-separator-scale
       (or (space-macs/mode-line-separator-scale) 1.6)))))

(defun space-macs-modeline/init-symon ()
  (use-package symon
    :defer t
    :init
    (progn
      (setq symon-delay 0
            symon-refresh-rate 2)
      (space-macs|add-toggle minibuffer-system-monitor
        :mode symon-mode
        :documentation "Tiny graphical system monitor."
        :evil-leader "tms"))))

(defun space-macs-modeline/init-vim-powerline ()
  (when (eq 'vim-powerline (space-macs/get-mode-line-theme-name))
    (require 'powerline)
    (if (display-graphic-p)
        (setq powerline-default-separator 'arrow)
      (setq powerline-default-separator 'utf-8))
    (defun powerline-raw (str &optional face pad)
      "Render STR as mode-line data using FACE and optionally
PAD import on left (l) or right (r) or left-right (lr)."
      (when str
        (let* ((rendered-str (format-mode-line str))
               (padded-str (concat
                            (when (and (> (length rendered-str) 0)
                                       (or (eq pad 'l) (eq pad 'lr))) " ")
                            (if (listp str) rendered-str str)
                            (when (and (> (length rendered-str) 0)
                                       (or (eq pad 'r) (eq pad 'lr))) " "))))
          (if face
              (pl/add-text-property padded-str 'face face)
            padded-str))))
    (require 'vim-powerline-theme)
    (powerline-vimish-theme)
    (add-hook 'e-macs-startup-hook
              'space-macs//set-vimish-powerline-for-startup-buffers)))


