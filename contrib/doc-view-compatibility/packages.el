;;; packages.el --- doc-view-compatibility Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Christopher McCloud <mccloud.christopher@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq doc-view-compatibility-packages
      '(doc-view
        popwin
        eyebrowse
        persp-mode
        ace-jump-mode))

(defun doc-view-compatibility/pre-init-doc-view ()
  (spacemacs|use-package-add-hook doc-view
    :pre-init
    (progn
      (defun doc-view/doc-view-save-current-page-to-buffer (&optional args)
        "For any doc-view windows, saves the current page to buffer.
Doc-view Mode uses image-mode-winprops to track state. Other packages,
particularly those involving window management, sometimes delete windows
automatically. On restoration, the associated window-props are gone, and
doc-view mode's default behavior is to pop to the first page. This function
introduces a fix for that behavior by saving the current page property
from the winprops-alist as a buffer local variable.

Also see `doc-view/doc-view-restore-current-page-from-buffer'."
        (cl-loop for win in (window-list)
                 when (eql 'doc-view-mode
                           (buffer-local-value 'major-mode (window-buffer win)))
                 do (with-selected-window win
                      (setq-local doc-view-last-visited-page
                                  (doc-view-current-page)))))
      (defun doc-view/doc-view-restore-current-page-from-buffer (&optional args)
        "For any doc-view windows, restores the current page from buffer.
Doc-view Mode uses image-mode-winprops to track state. Other packages,
particularly those involving window management, sometimes delete windows
automatically. On restoration, the associated window-props are gone, and
doc-view mode's default behavior is to pop to the first page. This function
introduces a fix for that behavior by restoring the current page property
from the winprops-alist as a buffer local variable.

Also see `doc-view/doc-view-save-current-page-to-buffer'."
        (cl-loop for win in (window-list)
                 when (eql 'doc-view-mode
                           (buffer-local-value 'major-mode (window-buffer win)))
                 do (with-selected-window win
                      (doc-view-goto-page doc-view-last-visited-page)))))))

(defun doc-view-compatibility/pre-init-popwin ()
  "Adds doc-view page saving/restoration support to popwin."
  (spacemacs|use-package-add-hook popwin
    :post-config
    (progn
      (add-hook 'popwin:before-popup-hook
                #'doc-view/doc-view-save-current-page-to-buffer)

      (add-hook 'popwin:after-popup-hook
                #'doc-view/doc-view-restore-current-page-from-buffer))))

(defun doc-view-compatibility/pre-init-persp-mode ()
  "Adds doc-view page saving/restoration support to perspectives."
  (spacemacs|use-package-add-hook persp-mode
    :post-config
    (progn
      ;; check for which version of the advice api to use
      (if (version< emacs-version "24.4")
          (progn
            (defadvice persp-switch
                (before doc-view/doc-view-save-current-page-to-buffer activate)
              (funcall #'doc-view/doc-view-save-current-page-to-buffer))
            (defadvice persp-switch
                (after doc-view/doc-view-restore-current-page-from-buffer activate)
              (funcall #'doc-view/doc-view-restore-current-page-from-buffer)))
        (advice-add 'persp-switch
                    :before
                    #'doc-view/doc-view-save-current-page-to-buffer)
        (advice-add 'persp-switch
                    :after
                    #'doc-view/doc-view-restore-current-page-from-buffer)))))

(defun doc-view-compatibility/pre-init-eyebrowse ()
  "Adds doc-view page saving/restoration support to eyebrowse."
  (spacemacs|use-package-add-hook eyebrowse
    :post-config
    (progn
      (add-hook 'eyebrowse-pre-window-switch-hook
                #'doc-view/doc-view-save-current-page-to-buffer)
      (add-hook 'eyebrowse-post-window-switch-hook
                #'doc-view/doc-view-restore-current-page-from-buffer))))

(defun doc-view-compatibility/pre-init-ace-jump-mode ()
  "Removes active doc-view windows from ace jump mode scope."
  (spacemacs|use-package-add-hook ace-jump-mode
    :post-config
    (progn
      (defun doc-view/filtered-ace-scope (visual-areas)
        "Filters ace-jump visual-areas, removing those belonging to doc-view
windows."
        (--filter (let* ((buf (window-buffer (aj-visual-area-window it)))
                         (mmode (buffer-local-value 'major-mode buf)))
                    (not (eql 'doc-view-mode mmode)))
                  visual-areas))
      (advice-add
       'ace-jump-list-visual-area
       :filter-return
       #'doc-view/filtered-ace-scope))))
