;;; funcs.el --- Org-agenda Layer funcs file for Spacemacs
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

(require 'org-agenda)

(defun org-agenda/spacemacs-agenda-map ()
  "add the org-agenda-map to 'a' in the spacemacs-mode-map.
~a SPC~ gets nerfed as it contains the spacemacs map etc etc and thus gives us an infitiely recursive keymap.
[menu-bar] was also in the org-agenda-mode-map which causes problems (most probably you can't say ~a menu-bar~)
"
  (defvar spacemacs-agenda-map)
  (setq spacemacs-agenda-map (copy-keymap org-agenda-mode-map))
  (define-key spacemacs-agenda-map (kbd "SPC") 'nil)
  (define-key spacemacs-agenda-map [menu-bar] 'nil)
  (define-key spacemacs-mode-map "a" spacemacs-agenda-map)
  )

(defun org-agenda/append-todo-list ()
  "append the org agenda list to *spacemacs*"
  (interactive)
  (org-agenda/spacemacs-agenda-map)
  (with-current-buffer (get-buffer-create "*spacemacs*")
    (let ((buffer-read-only nil)
          (list-separator "\n\n"))
      (goto-char (point-max))
      (insert "Todo:")
      (insert "\n")
      (org-agenda/org-todo-list)
      (spacemacs//insert--shortcut "d" "Todo:")
      (insert "\n\n"))))

(defun org-agenda/append-agenda-list ()
  "append the org agenda list to *spacemacs*"
  (interactive)
  (org-agenda/spacemacs-agenda-map)
  (with-current-buffer (get-buffer-create "*spacemacs*")
    (let ((buffer-read-only nil)
          (list-separator "\n\n"))
      (goto-char (point-max))
      (insert "Agenda:")
      (insert "\n")
      (org-agenda/org-agenda-list)
      (spacemacs//insert--shortcut "c" "Agenda:")
      (insert "\n\n"))))

(defun org-agenda/org-todo-list ()
  "Heavily hacked version of org-todo-list"
  (org-agenda-prepare-buffers (org-agenda-files nil 'ifmode))
  (org-compile-prefix-format 'todo)
  (org-set-sorting-strategy 'todo)
  (let ((rtnall nil)
        (rtn nil)
        (files (org-agenda-files nil 'ifmode))
        (date (calendar-gregorian-from-absolute (org-today))))
    (while (setq file (pop files))
      (setq rtn (org-agenda-get-day-entries file date :todo))
      (setq rtnall (append rtnall rtn)))
    (insert (org-agenda-finalize-entries
             ;;(mapcar (lambda (x) (concat (make-string 2 32) x))
             rtnall
             'todo) "\n")))

(defun org-agenda/org-agenda-list ()
  "Heavily hacked version of org-agenda-list"
  (org-agenda-prepare-buffers (org-agenda-files nil 'ifmode))
  (org-compile-prefix-format 'agenda)
  (org-set-sorting-strategy 'agenda)

  (let* ((span (org-agenda-ndays-to-span org-agenda-span))
         (today (org-today))
         (sd today)
         (ndays (org-agenda-span-to-ndays span sd))
         (org-agenda-start-on-weekday
          (if (or (eq ndays 7) (eq ndays 14))
              org-agenda-start-on-weekday))
         (thefiles (org-agenda-files nil 'ifmode))
         (files thefiles)
         (start (if (or (null org-agenda-start-on-weekday)
                        (< ndays 7))
                    sd
                  (let* ((nt (calendar-day-of-week
                              (calendar-gregorian-from-absolute sd)))
                         (n1 org-agenda-start-on-weekday)
                         (d (- nt n1)))
                    (- sd (+ (if (< d 0) 7 0) d)))))
         (day-numbers (list start))
         (day-cnt 0)
         (inhibit-redisplay (not debug-on-error))
         (org-agenda-show-log-scoped org-agenda-show-log)
         s e rtn rtnall file date d start-pos end-pos todayp
         clocktable-start clocktable-end filter)
    (dotimes (n (1- ndays))
      (push (1+ (car day-numbers)) day-numbers))
    (setq day-numbers (nreverse day-numbers))
    (setq clocktable-start (car day-numbers)
          clocktable-end (1+ (or (org-last day-numbers) 0)))
    (org-set-local 'org-starting-day (car day-numbers))
    ;;(org-set-local 'org-arg-loc arg)
    (org-set-local 'org-agenda-current-span (org-agenda-ndays-to-span span))
    (unless org-agenda-compact-blocks
      (let* ((d1 (car day-numbers))
             (d2 (org-last day-numbers))
             (w1 (org-days-to-iso-week d1))
             (w2 (org-days-to-iso-week d2)))
        (setq s (point))
        (if org-agenda-overriding-header
            (insert (org-add-props (copy-sequence org-agenda-overriding-header)
                        nil 'face 'org-agenda-structure) "\n")
          (insert (org-agenda-span-name span)
                  "-agenda"
                  (if (< (- d2 d1) 350)
                      (if (= w1 w2)
                          (format " (W%02d)" w1)
                        (format " (W%02d-W%02d)" w1 w2))
                    "")
                  ":\n")))
      (add-text-properties s (1- (point)) (list 'face 'org-agenda-structure
                                                'org-date-line t))
      (org-agenda-mark-header-line s))
    (while (setq d (pop day-numbers))
      (setq date (calendar-gregorian-from-absolute d)
            s (point))
      (if (or (setq todayp (= d today))
              (and (not start-pos) (= d sd)))
          (setq start-pos (point))
        (if (and start-pos (not end-pos))
            (setq end-pos (point))))
      (setq files thefiles
            rtnall nil)
      (while (setq file (pop files))
        (catch 'nextfile
          (org-check-agenda-file file)
          (let ((org-agenda-entry-types org-agenda-entry-types))
            ;; Starred types override non-starred equivalents
            (when (member :deadline* org-agenda-entry-types)
              (setq org-agenda-entry-types
                    (delq :deadline org-agenda-entry-types)))
            (when (member :scheduled* org-agenda-entry-types)
              (setq org-agenda-entry-types
                    (delq :scheduled org-agenda-entry-types)))
            ;; Honor with-hour
            (setq with-hour nil)
            (when with-hour
              (when (member :deadline org-agenda-entry-types)
                (setq org-agenda-entry-types
                      (delq :deadline org-agenda-entry-types))
                (push :deadline* org-agenda-entry-types))
              (when (member :scheduled org-agenda-entry-types)
                (setq org-agenda-entry-types
                      (delq :scheduled org-agenda-entry-types))
                (push :scheduled* org-agenda-entry-types)))
            (unless org-agenda-include-deadlines
              (setq org-agenda-entry-types
                    (delq :deadline* (delq :deadline org-agenda-entry-types))))
            (cond
             ((memq org-agenda-show-log-scoped '(only clockcheck))
              (setq rtn (org-agenda-get-day-entries
                         file date :closed)))
             (org-agenda-show-log-scoped
              (setq rtn (apply 'org-agenda-get-day-entries
                               file date
                               (append '(:closed) org-agenda-entry-types))))
             (t
              (setq rtn (apply 'org-agenda-get-day-entries
                               file date
                               org-agenda-entry-types)))))
          (setq rtnall (append rtnall rtn)))) ;; all entries
      (if org-agenda-include-diary
          (let ((org-agenda-search-headline-for-time t))
            (require 'diary-lib)
            (setq rtn (org-get-entries-from-diary date))
            (setq rtnall (append rtnall rtn))))
      (if (or rtnall org-agenda-show-all-dates)
          (progn
            (setq day-cnt (1+ day-cnt))
            (insert
             (if (stringp org-agenda-format-date)
                 (format-time-string org-agenda-format-date
                                     (org-time-from-absolute date))
               (funcall org-agenda-format-date date))
             "\n")
            (put-text-property s (1- (point)) 'face
                               (org-agenda-get-day-face date))
            (put-text-property s (1- (point)) 'org-date-line t)
            (put-text-property s (1- (point)) 'org-agenda-date-header t)
            (put-text-property s (1- (point)) 'org-day-cnt day-cnt)
            (when todayp
              (put-text-property s (1- (point)) 'org-today t))
            (setq rtnall
                  (org-agenda-add-time-grid-maybe rtnall ndays todayp))
            (if rtnall (insert ;; all entries
                        (org-agenda-finalize-entries rtnall 'agenda)
                        "\n"))
            (put-text-property s (1- (point)) 'day d)
            (put-text-property s (1- (point)) 'org-day-cnt day-cnt))))
    (when (and org-agenda-clockreport-mode clocktable-start)
      (let ((org-agenda-files (org-agenda-files nil 'ifmode))
            ;; the above line is to ensure the restricted range!
            (p (copy-sequence org-agenda-clockreport-parameter-plist))
            tbl)
        (setq p (org-plist-delete p :block))
        (setq p (plist-put p :tstart clocktable-start))
        (setq p (plist-put p :tend clocktable-end))
        (setq p (plist-put p :scope 'agenda))
        (setq tbl (apply 'org-clock-get-clocktable p))
        (insert tbl)))
    (if (eq org-agenda-show-log-scoped 'clockcheck)
        (org-agenda-show-clocking-issues))
    (org-agenda-finalize)
    (setq buffer-read-only t)
    (message "")))
