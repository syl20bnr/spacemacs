;;; spaceline-config.el --- Spaceline themes

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

;; This file contains ready-to-use modeline themes for powerline.

;;; Code:

(require 'spaceline-segments)

(defun spaceline--theme (left second-left &rest additional-segments)
  "Convenience function for the spacemacs and emacs themes."
  (spaceline-compile
    `(,left
      (anzu :priority 95)
      auto-compile
      ,second-left
      (major-mode :priority 79)
      (process :when active)
      ((flycheck-error flycheck-warning flycheck-info)
       :when active
       :priority 89)
      (minor-modes :when active
                   :priority 9)
      (treesit-inspect :when active)
      (mu4e-alert-segment :when active)
      (erc-track :when active)
      (version-control :when active
                       :priority 78)
      (org-pomodoro :when active)
      (org-clock :when active)
      nyan-cat)
    `(which-function
      (python-pyvenv :fallback python-pyenv)
      (purpose :priority 94)
      (battery :when active)
      (selection-info :priority 95)
      input-method
      ((buffer-encoding-abbrev
        point-position
        line-column)
       :separator " | "
       :priority 96)
      (so-long :when active)
      (global :when active)
      ,@additional-segments
      (buffer-position :priority 99)
      (hud :priority 99)))

  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))

;;;###autoload
(defun spaceline-spacemacs-theme (&rest additional-segments)
  "Install the modeline used by Spacemacs.

ADDITIONAL-SEGMENTS are inserted on the right, between `global' and
`buffer-position'."
  (apply 'spaceline--theme
         '((persp-name
            workspace-number
            window-number)
           :fallback evil-state
           :face highlight-face
           :priority 100)
         '((buffer-modified buffer-size buffer-id remote-host)
           :priority 98)
         additional-segments))

;;;###autoload
(defun spaceline-emacs-theme (&rest additional-segments)
  "Install a modeline close to the one used by Spacemacs, but which
looks better without third-party dependencies.

ADDITIONAL-SEGMENTS are inserted on the right, between `global' and
`buffer-position'."
  (apply 'spaceline--theme
         '(((((persp-name :fallback workspace-number)
              window-number) :separator "|")
            buffer-modified
            buffer-size)
           :face highlight-face
           :priority 100)
         '((buffer-id remote-host)
           :priority 98)
         additional-segments))

;;; Helm custom mode
;;  ================

(defvar helm-ag-show-status-function)

(defun spaceline--helm-ag-update ()
  (setq mode-line-format '("%e" (:eval (spaceline-ml-helm-done)))))

;;;###autoload
(define-minor-mode spaceline-helm-mode
  "Customize the mode-line in helm."
  :group 'spaceline
  :init-value nil
  :global t
  (if spaceline-helm-mode
      (progn
        (spaceline-compile 'helm
          '((helm-buffer-id :face highlight-face)
            helm-number
            helm-follow
            helm-prefix-argument)
          '(helm-help))
        (spaceline-compile 'helm-done
          '(((helm-buffer-id helm-done) :face highlight-face)
            helm-number
            helm-follow
            helm-prefix-argument)
          '(helm-help))
        (defadvice helm-display-mode-line (after spaceline-helm)
          "Set up a custom helm modeline."
          (setq spaceline--helm-current-source source
                mode-line-format '("%e" (:eval (spaceline-ml-helm))))
          (when force (force-mode-line-update)))
        (setq helm-ag-show-status-function 'spaceline--helm-ag-update)
        (ad-activate 'helm-display-mode-line))
    (setq helm-ag-show-status-function 'helm-ag-show-status-default-mode-line)
    (ad-deactivate 'helm-display-mode-line)))

;;; Info custom mode
;;  ================

;;;###autoload
(define-minor-mode spaceline-info-mode
  "Customize the mode-line in info.
This minor mode requires info+."
  :init-value nil
  :group 'spaceline
  :global t
  (if spaceline-info-mode
      (progn
        (spaceline-compile 'info '(info-topic (info-nodes :separator " > ")) nil)
        (defadvice Info-set-mode-line (after spaceline-info)
          "Set up a custom info modeline."
          (if (featurep 'info+)
              (let* ((nodes (s-split " > " mode-line-format))
                     (topic (prog2
                                (string-match "(\\(.+\\))\\(.+\\)" (car nodes))
                                (propertize (concat "INFO "
                                                    (match-string 1 (car nodes)))
                                            'face 'bold)
                              (setcar nodes (match-string 2 (car nodes))))))
                (setq spaceline--info-nodes nodes)
                (setq spaceline--info-topic topic)
                (setq-local mode-line-format '("%e" (:eval (spaceline-ml-info)))))
            (message "info+ is not available: spaceline-info-mode disabled")
            (spaceline-info-mode -1)))
        (ad-activate 'Info-set-mode-line))
    (ad-deactivate 'Info-set-mode-line)))

(provide 'spaceline-config)

;;; spaceline-config.el ends here
