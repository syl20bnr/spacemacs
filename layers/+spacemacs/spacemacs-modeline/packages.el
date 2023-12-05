;;; packages.el --- Spacemacs Mode-line Visual Layer packages File
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
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


(setq spacemacs-modeline-packages
      '(
        anzu
        (doom-modeline :toggle (eq (spacemacs/get-mode-line-theme-name) 'doom))
        fancy-battery
        spaceline
        (spaceline-all-the-icons :toggle (eq (spacemacs/get-mode-line-theme-name) 'all-the-icons))
        symon
        (vim-powerline :location (recipe :fetcher local))))

(defun spacemacs-modeline/post-init-anzu ()
  (when (eq 'all-the-icons (spacemacs/get-mode-line-theme-name))
    (spaceline-all-the-icons--setup-anzu)))

(defun spacemacs-modeline/init-doom-modeline ()
  ;; doom modeline depends on `display-graphic-p' so we delay its initialization
  ;; as when dumping we don't know yet wether we are using a graphical emacs or
  ;; not.
  (spacemacs|unless-dumping-and-eval-after-loaded-dump doom-modeline
    (use-package doom-modeline
      :defer t
      :init (doom-modeline-mode))))

(defun spacemacs-modeline/init-fancy-battery ()
  (use-package fancy-battery
    :defer t
    :init
    (spacemacs|add-toggle mode-line-battery
      :mode fancy-battery-mode
      :documentation "Display battery info in mode-line."
      :evil-leader "tmb")
    (setq-default fancy-battery-show-percentage t)))

(defun spacemacs-modeline/init-spaceline ()
  (use-package spaceline-config
    :if (memq (spacemacs/get-mode-line-theme-name)
              '(spacemacs all-the-icons custom))
    :init
    (spacemacs|require-when-dumping 'spaceline)
    (spacemacs|when-dumping-strict
      (spacemacs/spaceline-config-startup))
    (spacemacs|unless-dumping
      (add-hook 'emacs-startup-hook 'spacemacs/spaceline-config-startup-hook))
    (add-hook 'spacemacs-post-theme-change-hook
              'spacemacs/customize-powerline-faces)
    (add-hook 'spacemacs-post-theme-change-hook 'powerline-reset)
    (spacemacs|add-toggle mode-line-responsive
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
        (eval `(spacemacs|add-toggle ,(intern (format "mode-line-%S" segment))
                 :status ,status-var
                 :on (setq ,status-var t)
                 :off (setq ,status-var nil)
                 :documentation ,(format "Show %s in the mode-line."
                                         (replace-regexp-in-string
                                          "-" " " (format "%S" segment)))
                 :evil-leader ,(cadr spec)))))
    (setq powerline-default-separator
          (cond
           ((spacemacs-is-dumping-p) 'utf-8)
           ((memq (spacemacs/get-mode-line-theme-name)
                  '(spacemacs custom))
            (spacemacs/mode-line-separator))
           (t 'wave))
          powerline-image-apple-rgb (eq window-system 'ns)
          powerline-scale (or (spacemacs/mode-line-separator-scale) 1.5)
          spaceline-byte-compile nil)
    :config
    (spacemacs/customize-powerline-faces)
    (setq spaceline-org-clock-p nil
          spaceline-highlight-face-func 'spacemacs//evil-state-face)
    ;; unicode
    (let ((unicodep (dotspacemacs|symbol-value
                     dotspacemacs-mode-line-unicode-symbols)))
      (setq spaceline-window-numbers-unicode unicodep
            spaceline-workspace-numbers-unicode unicodep))
    (add-hook 'spaceline-pre-hook 'spacemacs//prepare-diminish)
    ;; New spacemacs version segment
    (defpowerline spacemacs-powerline-new-version
      (propertize
       spacemacs-version-check-lighter
       'mouse-face 'mode-line-highlight
       'help-echo (format "New version %s | Click with mouse-1 to update"
                          spacemacs-new-version)
       'local-map (let ((map (make-sparse-keymap)))
                    (define-key map
                      [mode-line down-mouse-1]
                      (lambda (event)
                        (interactive "@e")
                        (if (yes-or-no-p
                             (format
                              (concat "Do you want to update to the newest "
                                      "version %s ?")
                              spacemacs-new-version))
                            (progn
                              (spacemacs/switch-to-version
                               spacemacs-new-version))
                          (message "Update aborted."))))
                    map)))
    (spaceline-define-segment
        new-version
      (when spacemacs-new-version
        (spacemacs-powerline-new-version
         (spacemacs/get-new-version-lighter-face
          spacemacs-version spacemacs-new-version))))
    (let ((theme (intern (format "spaceline-%S-theme"
                                 (spacemacs/get-mode-line-theme-name)))))
      (apply theme spacemacs-spaceline-additional-segments))
    ;; Additional spacelines
    (when (package-installed-p 'helm)
      (spaceline-helm-mode t))
    (when (configuration-layer/package-used-p 'info+)
      (spaceline-info-mode t))
    ;; Enable spaceline for buffers created before the configuration of
    ;; spaceline
    (spacemacs//restore-buffers-powerline)))

(defun spacemacs-modeline/pre-init-spaceline-all-the-icons ()
  (when (eq 'all-the-icons (spacemacs/get-mode-line-theme-name))
    (spacemacs|use-package-add-hook spaceline-config
      :pre-config
      (progn
        (require 'spaceline-all-the-icons)
        ;; responsivness does not play well with all-the-icons theme
        ;; let's disable it for now
        ;; https://github.com/domtronn/spaceline-all-the-icons.el/issues/51#issuecomment-316686790
        (setq spaceline-responsive nil)
        (spaceline-all-the-icons--setup-git-ahead)))))

(defun spacemacs-modeline/init-spaceline-all-the-icons ()
  (use-package spaceline-all-the-icons
    :defer t
    :init
    (setq
     spaceline-all-the-icons-separator-type
     (or (spacemacs/mode-line-separator) 'wave)
     spaceline-all-the-icons-separator-scale
     (or (spacemacs/mode-line-separator-scale) 1.6))
    :config
    (when (and (eq 'all-the-icons (spacemacs/get-mode-line-theme-name))
               (configuration-layer/package-used-p 'neotree))
      (spaceline-all-the-icons--setup-neotree))))

(defun spacemacs-modeline/init-symon ()
  (use-package symon
    :defer t
    :init
    (setq symon-delay 0
          symon-refresh-rate 2)
    (spacemacs|add-toggle minibuffer-system-monitor
      :mode symon-mode
      :documentation "Tiny graphical system monitor."
      :evil-leader "tms")))

(defun spacemacs-modeline/init-vim-powerline ()
  (when (eq 'vim-powerline (spacemacs/get-mode-line-theme-name))
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
    (add-hook 'emacs-startup-hook
              'spacemacs//set-vimish-powerline-for-startup-buffers)))
