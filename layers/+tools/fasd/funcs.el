;;; funcs.el --- fasd Layer packages File for Spacemacs
;;
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


(defun ivy-search-from-action (x)
  (if (file-directory-p x)
      (spacemacs/counsel-search dotspacemacs-search-tools nil x)
    (message "Selected item is not a directory path")))

(defun fasd-find-file-make-persp ()
  "Use fasd to open file or directory in a Spacemacs layout (persp).

 If fasd item's project root already in a layout, switches to that layout. If
multiple layouts contain the same project root - lets you choose one of them."
  (interactive)
  (let* ((lexical-binding t)
         (query (if fasd-enable-initial-prompt
                    (read-from-minibuffer "Fasd query: ")
                  ""))
         (results
          (split-string
           (shell-command-to-string
            (concat "fasd -l -R -a " query))
           "\n" t))
         (fpath (when results
                  ;; set `this-command' to `fasd-find-file' is required because
                  ;; `read-from-minibuffer' modifies its value, while `ivy-completing-read'
                  ;; assumes it to be its caller
                  (setq this-command 'fasd-find-file-make-persp)
                  (completing-read "Fasd query: " results nil t)))
         (proj-dir (projectile-project-root fpath))
         (get-fname (lambda (buf)
                      (when (buffer-live-p buf)
                        (with-current-buffer buf
                          (cond ((eq major-mode 'dired-mode) (dired-get-filename nil :no-error))
                                (t (buffer-file-name)))))))
         ;; find the perspective with a matching project-root by traversing every
         ;; persp and analyzing every buffer (if buffer's file belongs to the project
         ;; directory - that's the persp we need)
         (persps (or (seq-filter
                      (lambda (p)
                        (when p
                          (seq-filter
                           (lambda (f)
                             (when (and proj-dir f)
                               (string=
                                proj-dir
                                (projectile-project-root f))))
                           (seq-remove 'null (seq-map get-fname (persp-buffers p))))))
                      (persp-persps))
                     ;; if there isn't a single persp that matches - create a new one
                     (let ((new-persp (persp-add-new
                                       (file-name-nondirectory
                                        (directory-file-name
                                         (or proj-dir fpath))))))
                       (list new-persp))))
         ;; if multiple matching persps found - prompt to choose
         (layout-name (if (< 1 (length persps))
                          (completing-read "Select layout " (seq-map 'persp-name persps))
                        (persp-name (car persps)))))
    (when layout-name
      (persp-switch layout-name)
      (find-file fpath)
      (delete-other-windows)
      ;; flash layouts transient for a moment, to indicate the layout switch
      (let ((spacemacs--layouts-ts-full-hint-toggle nil))
        (spacemacs/layouts-transient-state/body)
        (run-at-time "1 sec" nil #'spacemacs/layouts-transient-state/nil)))))
