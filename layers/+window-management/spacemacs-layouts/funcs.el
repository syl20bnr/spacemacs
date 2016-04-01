;;; funcs.el --- Spacemacs Layouts Layer functions File
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

(defun spacemacs//current-layout-name ()
  "Get name of the current perspective."
  (safe-persp-name (get-frame-persp)))

;; Helm related functions --------------------------------------------------

(defun spacemacs/persp-helm-mini ()
  "As `helm-mini' but restricts visible buffers by perspective."
  (interactive)
  (with-persp-buffer-list ()
                          (helm-mini)))

(defun spacemacs//helm-perspectives-source ()
  (helm-build-in-buffer-source
      (concat "Current Perspective: " (spacemacs//current-layout-name))
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
   :sources `(,(spacemacs//helm-perspectives-source)
              ,(helm-build-dummy-source "Create new perspective"
                 :requires-pattern t
                 :action
                 '(("Create new perspective" .
                    (lambda (name)
                      (let ((persp-reset-windows-on-nil-window-conf t))
                        (persp-switch name)))))))))

;; ability to use helm find files but also adds to current perspective
(defun spacemacs/helm-persp-close ()
  "Kills perspectives without killing the buffers"
  (interactive)
  (helm
   :buffer "*Helm Kill Perspectives (without killing buffers)*"
   :sources (helm-build-in-buffer-source
                (concat "Current Perspective: " (spacemacs//current-layout-name))
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
                          (spacemacs//current-layout-name))
              :data (persp-names)
              :fuzzy-match t
              :action
              '(("Kill perspective(s)" .
                 (lambda (candidate)
                   (mapcar 'persp-kill
                           (helm-marked-candidates))))))))

;; Helm Projectile related functions ---------------------------------------

(defun spacemacs/helm-persp-switch-project (arg)
  (interactive "P")
  (helm
   :sources
   (helm-build-in-buffer-source "*Helm Switch Project Layout*"
     :data (lambda ()
             (if (projectile-project-p)
                 (cons (abbreviate-file-name (projectile-project-root))
                       (projectile-relevant-known-projects))
               projectile-known-projects))
     :fuzzy-match helm-projectile-fuzzy-match
     :mode-line helm-read-file-name-mode-line-string
     :action '(("Switch to Project Perspective" .
                (lambda (project)
                  (let ((persp-reset-windows-on-nil-window-conf t))
                    (persp-switch project)
                    (let ((projectile-completion-system 'helm))
                      (projectile-switch-project-by-name project)))))))
   :buffer "*Projectile Layouts*"))

;; Autosave ----------------------------------------------------------------

(defun spacemacs//layout-autosave ()
  "Perspectives mode autosave.
Autosaves perspectives layouts every `persp-autosave-interal' seconds.
Cancels autosave on exiting perspectives mode."
  (if (and persp-mode layouts-enable-autosave)
      (progn
        (message "Perspectives mode autosaving enabled.")
        (setq spacemacs--layouts-autosave-timer
              (run-with-timer
               layouts-autosave-delay
               layouts-autosave-delay
               (lambda ()
                 (message "Saving perspectives to file.")
                 (persp-save-state-to-file)))))
    (when spacemacs--layouts-autosave-timer
      (cancel-timer spacemacs--layouts-autosave-timer)
      (setq spacemacs--layouts-autosave-timer nil))))

;; Eyebrowse - allow perspective-local workspaces --------------------------

(defun spacemacs/load-eyebrowse-for-perspective (&optional frame)
  "Load an eyebrowse workspace according to a perspective's parameters.
FRAME's perspective is the perspective that is considered, defaulting to
the current frame's perspective.
If the perspective doesn't have a workspace, create one."
  (let* ((persp (get-frame-persp frame))
         (window-configs (persp-parameter 'eyebrowse-window-configs persp))
         (current-slot (persp-parameter 'eyebrowse-current-slot persp))
         (last-slot (persp-parameter 'eyebrowse-last-slot persp)))
    (if window-configs
        (progn
          (eyebrowse--set 'window-configs window-configs frame)
          (eyebrowse--set 'current-slot current-slot frame)
          (eyebrowse--set 'last-slot last-slot frame)
          (eyebrowse--load-window-config current-slot))
      (eyebrowse--set 'window-configs nil frame)
      (eyebrowse-init frame)
      (spacemacs/save-eyebrowse-for-perspective frame))))

(defun spacemacs/update-eyebrowse-for-perspective (_new-persp-name)
  "Update and save current frame's eyebrowse workspace to its perspective.
Parameter _NEW-PERSP-NAME is ignored, and exists only for compatibility with
`persp-before-switch-functions'."
  (eyebrowse--update-window-config-element
   (eyebrowse--current-window-config (eyebrowse--get 'current-slot)
                                     (eyebrowse--get 'current-tag)))
  (spacemacs/save-eyebrowse-for-perspective))

(defun spacemacs/save-eyebrowse-for-perspective (&optional frame)
  "Save FRAME's eyebrowse workspace to FRAME's perspective.
FRAME defaults to the current frame."
  (let ((persp (get-frame-persp frame)))
    (set-persp-parameter
     'eyebrowse-window-configs (eyebrowse--get 'window-configs frame) persp)
    (set-persp-parameter
     'eyebrowse-current-slot (eyebrowse--get 'current-slot frame) persp)
    (set-persp-parameter
     'eyebrowse-last-slot (eyebrowse--get 'last-slot frame) persp)))
