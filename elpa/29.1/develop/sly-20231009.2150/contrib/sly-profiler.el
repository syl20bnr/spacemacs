;;; -*- coding: utf-8; lexical-binding: t -*-
;;;
;;; sly-profiler.el -- a navigable dialog of inspectable timing entries
;;;
(eval-and-compile
  (require 'sly)
  (require 'sly-parse "lib/sly-parse"))

(define-sly-contrib sly-profiler
  "Provide an interfactive timing dialog buffer for managing and
inspecting details of timing functions. Invoke this dialog with C-c Y."
  (:authors "João Távora <joaotavora@gmail.com>")
  (:license "GPL")
  (:slynk-dependencies slynk/profiler)
  (:on-load (add-hook 'sly-mode-hook 'sly-profiler-enable))
  (:on-unload (remove-hook 'sly-mode-hook 'sly-profiler-enable)))


;;;; Modes and mode maps
;;;
(defvar sly-profiler-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "G") 'sly-profiler-fetch-timings)
    (define-key map (kbd "C-k") 'sly-profiler-clear-fetched-timings)
    (define-key map (kbd "g") 'sly-profiler-fetch-status)
    (define-key map (kbd "q") 'quit-window)
    map))

(define-derived-mode sly-profiler-mode fundamental-mode
  "SLY Timing Dialog" "Mode for controlling SLY's Timing Dialog"
  (set-syntax-table lisp-mode-syntax-table)
  (read-only-mode 1))

(defvar sly-profiler-shortcut-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c Y") 'sly-profiler)
    (define-key map (kbd "C-c C-y") 'sly-profiler-toggle-timing)
    map))

(define-minor-mode sly-profiler-shortcut-mode
  "Add keybindings for accessing SLY's Profiler.")

(defun sly-profiler-enable () (sly-profiler-shortcut-mode 1))


;;;; Helpers
;;;
(defun sly-profiler--get-buffer ()
  (let* ((name (format "*profiler for %s*"
                       (sly-connection-name sly-default-connection)))
         (existing (get-buffer name)))
    (cond ((and existing
                (buffer-live-p existing)
                (with-current-buffer existing
                  (memq sly-buffer-connection sly-net-processes)))
           existing)
          (t
           (if existing (kill-buffer existing))
           (with-current-buffer (get-buffer-create name)
             (sly-profiler-mode)
             (setq sly-buffer-connection sly-default-connection)
             (pop-to-buffer (current-buffer)))))))

(defun sly-profiler--clear-local-tree ()
  (erase-buffer)
  (insert "Cleared timings!"))

(defun sly-profiler--render-timings (timing-specs)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (let ((standard-output (current-buffer)))
      (cl-loop for spec in timing-specs
               do (princ spec) (terpri)))))

;;;; Interactive functions
;;;
;; (defun sly-profiler-fetch-specs ()
;;   "Refresh just list of timing specs."
;;   (interactive)
;;   (sly-eval-async `(slynk-profiler:report-specs)
;;     #'sly-profiler--open-specs))

(defun sly-profiler-clear-fetched-timings (&optional interactive)
  "Clear local and remote timings collected so far"
  (interactive "p")
  (when (or (not interactive)
            (y-or-n-p "Clear all collected and fetched timings?"))
    (sly-eval-async
        '(slynk-profiler:clear-timing-tree)
      #'sly-profiler--clear-local-tree)))

(defun sly-profiler-fetch-timings ()
  (interactive)
  (sly-eval-async `(slynk-profiler:report-latest-timings)
    #'sly-profiler--render-timings))

(defun sly-profiler-fetch-status ()
  (interactive)
  (sly-profiler-fetch-timings))

(defun sly-profiler-toggle-timing (&optional using-context-p)
  "Toggle the dialog-timing of the spec at point.

When USING-CONTEXT-P, attempt to decipher lambdas. methods and
other complicated function specs."
  (interactive "P")
  ;; Notice the use of "spec strings" here as opposed to the
  ;; proper cons specs we use on the slynk side.
  ;;
  ;; Notice the conditional use of `sly-trace-query' found in
  ;; slynk-fancy-trace.el
  ;;
  (let* ((spec-string (if using-context-p
                          (sly-extract-context)
                        (sly-symbol-at-point)))
         (spec-string (read-from-minibuffer "(Un)time: " (format "%s" spec-string))))
    (message "%s" (sly-eval `(slynk-profiler:toggle-timing
                                (slynk::from-string ,spec-string))))))

(defun sly-profiler (&optional refresh)
  "Show timing dialog and refresh timing collection status.

With optional CLEAR-AND-FETCH prefix arg, clear the current tree
and fetch a first batch of timings."
  (interactive "P")
  (sly-with-popup-buffer ((sly-buffer-name :profiler :connection sly-default-connection)
                          :mode 'sly-profiler-mode
                          :select t)
    (when refresh (sly-profiler-fetch-timings))))


;;;; Menu
;;;
(easy-menu-define sly-profiler--shortcut-menu nil
  "Menu setting traces from anywhere in SLY."
  (let* ((in-dialog '(eq major-mode 'sly-profiler-mode))
         (_dialog-live `(and ,in-dialog
                             (memq sly-buffer-connection sly-net-processes)))
         (connected '(sly-connected-p)))
    `("Profiling"
      ["(Un)Profile definition" sly-profiler-toggle-timing ,connected]
      ["Open Profiler Dialog" sly-profiler (and ,connected (not ,in-dialog))])))

(easy-menu-add-item sly-menu nil sly-profiler--shortcut-menu "Documentation")

(defvar sly-profiler--easy-menu
  (let ((condition '(memq sly-buffer-connection sly-net-processes)))
    `("Timing"
      [ "Clear fetched timings" sly-profiler-clear-fetched-timings ,condition]
      [ "Fetch timings" sly-profiler-fetch-timings ,condition])))

(easy-menu-define my-menu sly-profiler-mode-map "Timing"
  sly-profiler--easy-menu)

(provide 'sly-profiler)
