;;; -*- coding: utf-8; lexical-binding: t -*-
;;;
;;; sly-trace-dialog.el -- a navigable dialog of inspectable trace entries
;;;
;;; TODO: implement better wrap interface for sbcl method, labels and such
;;; TODO: backtrace printing is very slow
;;;
(require 'sly)
(require 'sly-parse "lib/sly-parse")
(require 'cl-lib)

(define-sly-contrib sly-trace-dialog
  "Provide an interactive trace dialog buffer for managing and
inspecting details of traced functions. Invoke this dialog with C-c T."
  (:authors "João Távora <joaotavora@gmail.com>")
  (:license "GPL")
  (:slynk-dependencies slynk/trace-dialog)
  (:on-load (add-hook 'sly-mode-hook 'sly-trace-dialog-shortcut-mode)
            (define-key sly-selector-map (kbd "T") 'sly-trace-dialog))
  (:on-unload (remove-hook 'sly-mode-hook 'sly-trace-dialogn-shortcut-mode)))


;;;; Variables
;;;
(defvar sly-trace-dialog-flash t
  "Non-nil means flash the updated region of the SLY Trace Dialog. ")

(defvar sly-trace-dialog--specs-overlay nil)

(defvar sly-trace-dialog--progress-overlay nil)

(defvar sly-trace-dialog--tree-overlay nil)

(defvar sly-trace-dialog--collapse-chars (cons "-" "+"))


;;;; Local trace entry model
(defvar sly-trace-dialog--traces nil)

(cl-defstruct (sly-trace-dialog--trace
               (:constructor sly-trace-dialog--make-trace))
  id
  parent
  spec
  args
  retlist
  depth
  beg
  end
  collapse-button-marker
  summary-beg
  children-end
  collapsed-p)

(defun sly-trace-dialog--find-trace (id)
  (gethash id sly-trace-dialog--traces))


;;;; Modes and mode maps
;;;
(defvar sly-trace-dialog-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "G") 'sly-trace-dialog-fetch-traces)
    (define-key map (kbd "C-k") 'sly-trace-dialog-clear-fetched-traces)
    (define-key map (kbd "g") 'sly-trace-dialog-fetch-status)

    (define-key map (kbd "q")     'quit-window)

    (set-keymap-parent map button-buffer-map)
    map))

(define-derived-mode sly-trace-dialog-mode fundamental-mode
  "SLY Trace Dialog" "Mode for controlling SLY's Trace Dialog"
  (set-syntax-table lisp-mode-syntax-table)
  (read-only-mode 1)
  (sly-mode 1)
  (add-to-list (make-local-variable 'sly-trace-dialog-after-toggle-hook)
               'sly-trace-dialog-fetch-status))

(defvar sly-trace-dialog-shortcut-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c T") 'sly-trace-dialog)
    (define-key map (kbd "C-c C-t") 'sly-trace-dialog-toggle-trace)
    (define-key map (kbd "C-c M-t")
      (if (featurep 'sly-fancy-trace)
          'sly-toggle-fancy-trace
          'sly-toggle-trace-fdefinition))
    map))

(define-minor-mode sly-trace-dialog-shortcut-mode
  "Add keybindings for accessing SLY's Trace Dialog.")

(easy-menu-define sly-trace-dialog--shortcut-menu nil
  "Menu setting traces from anywhere in SLY."
  (let* ((in-dialog '(eq major-mode 'sly-trace-dialog-mode))
         (_dialog-live `(and ,in-dialog
                             (memq sly-buffer-connection sly-net-processes)))
         (connected '(sly-connected-p)))
    `("Trace"
      ["Toggle trace.." sly-trace-dialog-toggle-trace ,connected]
      ["Untrace all" sly-trace-dialog-untrace-all ,connected]
      ["Trace complex spec" sly-trace-dialog-toggle-complex-trace ,connected]
      ["Open Trace dialog" sly-trace-dialog (and ,connected (not ,in-dialog))]
      "--"
      [ "Regular lisp trace..."         sly-toggle-fancy-trace ,connected])))

(easy-menu-add-item sly-menu nil sly-trace-dialog--shortcut-menu "Documentation")

(easy-menu-define sly-trace-dialog--menu sly-trace-dialog-mode-map
  "Menu for SLY's Trace Dialog"
  (let* ((in-dialog '(eq major-mode 'sly-trace-dialog-mode))
         (dialog-live `(and ,in-dialog
                            (memq sly-buffer-connection sly-net-processes))))
    `("SLY-Trace"
      [ "Refresh traces and progress" sly-trace-dialog-fetch-status
        ,dialog-live]
      [ "Fetch next batch" sly-trace-dialog-fetch-traces ,dialog-live]
      [ "Clear all fetched traces" sly-trace-dialog-clear-fetched-traces
        ,dialog-live]
      [ "Toggle details" sly-trace-dialog-hide-details-mode ,in-dialog]
      [ "Toggle autofollow" sly-trace-dialog-autofollow-mode ,in-dialog])))

(define-minor-mode sly-trace-dialog-hide-details-mode
  "Hide details in `sly-trace-dialog-mode'"
  nil " Brief"
  :group 'sly-trace-dialog
  (unless (derived-mode-p 'sly-trace-dialog-mode)
    (error "Not a SLY Trace Dialog buffer"))
  (sly-trace-dialog--set-hide-details-mode))

(define-minor-mode sly-trace-dialog-autofollow-mode
  "Automatically inspect trace entries from `sly-trace-dialog-mode'"
  nil " Autofollow"
  :group 'sly-trace-dialog
  (unless (derived-mode-p 'sly-trace-dialog-mode)
    (error "Not a SLY Trace Dialog buffer")))


;;;; Helper functions
;;;
(defmacro sly-trace-dialog--insert-and-overlay (string overlay)
  `(save-restriction
     (let ((inhibit-read-only t))
       (narrow-to-region (point) (point))
       (insert ,string "\n")
       (set (make-local-variable ',overlay)
            (let ((overlay (make-overlay (point-min)
                                         (point-max)
                                         (current-buffer)
                                         nil
                                         t)))
              (move-overlay overlay (overlay-start overlay)
                            (1- (overlay-end overlay)))
              overlay)))))

(defun sly-trace-dialog--buffer-name ()
  (sly-buffer-name :traces :connection (sly-current-connection)))

(defun sly-trace-dialog--live-dialog (&optional buffer-or-name)
  (let ((buffer-or-name (or buffer-or-name
                            (sly-trace-dialog--buffer-name))))
    (and (buffer-live-p (get-buffer buffer-or-name))
       (with-current-buffer buffer-or-name
         (memq sly-buffer-connection sly-net-processes))
       buffer-or-name)))

(defun sly-trace-dialog--ensure-buffer ()
  (let ((name (sly-trace-dialog--buffer-name)))
    (or (sly-trace-dialog--live-dialog name)
        (let ((connection (sly-current-connection)))
          (with-current-buffer (get-buffer-create name)
            (let ((inhibit-read-only t))
              (erase-buffer))
            (sly-trace-dialog-mode)
            (save-excursion
              (buffer-disable-undo)
              (sly-trace-dialog--insert-and-overlay
               "[waiting for the traced specs to be available]"
               sly-trace-dialog--specs-overlay)
              (sly-trace-dialog--insert-and-overlay
               "[waiting for some info on trace download progress ]"
               sly-trace-dialog--progress-overlay)
              (sly-trace-dialog--insert-and-overlay
               "[waiting for the actual traces to be available]"
               sly-trace-dialog--tree-overlay)
              (current-buffer))
            (setq sly-buffer-connection connection)
            (current-buffer))))))

(defun sly-trace-dialog--set-collapsed (collapsed-p trace button)
  (save-excursion
    (setf (sly-trace-dialog--trace-collapsed-p trace) collapsed-p)
    (sly-trace-dialog--go-replace-char-at
     button
     (if collapsed-p
         (cdr sly-trace-dialog--collapse-chars)
       (car sly-trace-dialog--collapse-chars)))
    (sly-trace-dialog--hide-unhide
     (sly-trace-dialog--trace-summary-beg trace)
     (sly-trace-dialog--trace-end trace)
     (if collapsed-p 1 -1))
    (sly-trace-dialog--hide-unhide
     (sly-trace-dialog--trace-end trace)
     (sly-trace-dialog--trace-children-end trace)
     (if collapsed-p 1 -1))))

(defun sly-trace-dialog--hide-unhide (start-pos end-pos delta)
  (cl-loop with inhibit-read-only = t
           for pos = start-pos then next
           for next = (next-single-property-change
                       pos
                       'sly-trace-dialog--hidden-level
                       nil
                       end-pos)
           for hidden-level = (+ (or (get-text-property
                                      pos
                                      'sly-trace-dialog--hidden-level)
                                     0)
                                 delta)
           do (add-text-properties pos next
                                   (list 'sly-trace-dialog--hidden-level
                                         hidden-level
                                         'invisible
                                         (cl-plusp hidden-level)))
           while (< next end-pos)))

(defun sly-trace-dialog--set-hide-details-mode ()
  (cl-loop for trace being the hash-values of sly-trace-dialog--traces
           do (sly-trace-dialog--hide-unhide
               (sly-trace-dialog--trace-summary-beg trace)
               (sly-trace-dialog--trace-end trace)
               (if sly-trace-dialog-hide-details-mode 1 -1))))

(defun sly-trace-dialog--format (fmt-string &rest args)
  (let* ((string (apply #'format fmt-string args))
         (indent (make-string (max 2
                                   (- 50 (length string))) ? )))
    (format "%s%s" string indent)))

(defun sly-trace-dialog--call-maintaining-properties (pos fn)
  (save-excursion
    (goto-char pos)
    (let* ((saved-props (text-properties-at pos))
           (saved-point (point))
           (inhibit-read-only t)
           (inhibit-point-motion-hooks t))
      (funcall fn)
      (add-text-properties saved-point (point) saved-props)
      (if (markerp pos) (set-marker pos saved-point)))))

(cl-defmacro sly-trace-dialog--maintaining-properties (pos
                                                         &body body)
  (declare (indent 1))
  `(sly-trace-dialog--call-maintaining-properties ,pos #'(lambda () ,@body)))

(defun sly-trace-dialog--go-replace-char-at (pos char)
  (sly-trace-dialog--maintaining-properties pos
    (delete-char 1)
    (insert char)))


;;;; Handlers for the *trace-dialog* buffer
;;;
(defun sly-trace-dialog--open-specs (traced-specs)
  (let ((make-report-spec-fn-fn
         (lambda (&optional form)
           (lambda (_button)
             (sly-eval-async
                 `(cl:progn
                   ,form
                   (slynk-trace-dialog:report-specs))
               #'(lambda (results)
                   (sly-trace-dialog--open-specs results)))))))
    (sly-refreshing
        (:overlay sly-trace-dialog--specs-overlay
                  :recover-point-p t)
      (insert
       (sly-trace-dialog--format "Traced specs (%s)" (length traced-specs))
       (sly-make-action-button "[refresh]"
                               (funcall make-report-spec-fn-fn))
       "\n" (make-string 50 ? )
       (sly-make-action-button
        "[untrace all]"
        (funcall make-report-spec-fn-fn `(slynk-trace-dialog:dialog-untrace-all)))
       "\n\n")
      (cl-loop for (spec-pretty . spec) in traced-specs
               do (insert
                   "  "
                   (sly-make-action-button
                    "[untrace]"
                    (funcall make-report-spec-fn-fn
                     `(slynk-trace-dialog:dialog-untrace ',spec)))
                   (format " %s" spec-pretty)
                   "\n")))))

(defvar sly-trace-dialog--fetch-key nil)

(defvar sly-trace-dialog--stop-fetching nil)

(defun sly-trace-dialog--update-progress (total &optional show-stop-p remaining-p)
  ;; `remaining-p' indicates `total' is the number of remaining traces.
  (sly-refreshing
      (:overlay sly-trace-dialog--progress-overlay
                :recover-point-p t)
    (let* ((done (hash-table-count sly-trace-dialog--traces))
           (total (if remaining-p (+ done total) total)))
      (insert
       (sly-trace-dialog--format "Trace collection status (%d/%s)"
                                   done
                                   (or total "0"))
       (sly-make-action-button "[refresh]"
                                   #'(lambda (_button)
                                       (sly-trace-dialog-fetch-progress))))

      (when (and total (cl-plusp (- total done)))
        (insert "\n" (make-string 50 ? )
                (sly-make-action-button
                 "[fetch next batch]"
                 #'(lambda (_button)
                     (sly-trace-dialog-fetch-traces nil)))
                "\n" (make-string 50 ? )
                (sly-make-action-button
                 "[fetch all]"
                 #'(lambda (_button)
                     (sly-trace-dialog-fetch-traces t)))))
      (when total
        (insert "\n" (make-string 50 ? )
                (sly-make-action-button
                 "[clear]"
                 #'(lambda (_button)
                     (sly-trace-dialog-clear-fetched-traces)))))
      (when show-stop-p
        (insert "\n" (make-string 50 ? )
                (sly-make-action-button
                 "[stop]"
                 #'(lambda (_button)
                     (setq sly-trace-dialog--stop-fetching t)))))
      (insert "\n\n"))))


;;;; Rendering traces
;;;

(define-button-type 'sly-trace-dialog-part :supertype 'sly-part
  'sly-button-inspect
  #'(lambda (trace-id part-id type)
      (sly-eval-for-inspector
       `(slynk-trace-dialog:inspect-trace-part ,trace-id ,part-id ,type)
       :inspector-name (sly-maybe-read-inspector-name)))
  'sly-button-pretty-print
  #'(lambda (trace-id part-id type)
      (sly-eval-describe
       `(slynk-trace-dialog:pprint-trace-part ,trace-id ,part-id ,type)))
  'sly-button-describe
  #'(lambda (trace-id part-id type)
      (sly-eval-describe
       `(slynk-trace-dialog:describe-trace-part ,trace-id ,part-id ,type))))

(defun sly-trace-dialog-part-button (part-id part-text trace-id type)
  (sly--make-text-button part-text nil
                         :type 'sly-trace-dialog-part
                         'part-args (list trace-id part-id type)
                         'part-label (format "%s %s"
                                             (capitalize
                                              (substring (symbol-name type) 1))
                                             part-id)))

(define-button-type 'sly-trace-dialog-spec :supertype 'sly-part
  'action 'sly-button-show-source
  'sly-button-inspect
  #'(lambda (trace-id _spec)
      (sly-eval-for-inspector `(slynk-trace-dialog:inspect-trace ,trace-id)
                              :inspector-name "trace-entries"))
  'sly-button-show-source
  #'(lambda (trace-id _spec)
      (sly-eval-async
          `(slynk-trace-dialog:trace-location ,trace-id)
        #'(lambda (location)
            (sly--display-source-location location 'noerror))))
  'point-entered
  #'(lambda (before after)
      (let ((button (sly-button-at after nil 'no-error)))
        (when (and (not (sly-button-at before nil 'no-error))
                   button
                   sly-trace-dialog-autofollow-mode)
          ;; we can't quite `push-button' here, because
          ;; of the need for `save-selected-window'
          ;;
          (let ((id (button-get button 'trace-id)))
            (sly-eval-for-inspector
             `(slynk-trace-dialog:inspect-trace ,id)
             :inspector-name "trace-entries"
             :save-selected-window t))))))

(defun sly-trace-dialog-spec-button (label trace &rest props)
  (let ((id (sly-trace-dialog--trace-id trace)))
    (apply #'sly--make-text-button label nil
           :type 'sly-trace-dialog-spec
           'trace-id id
           'part-args (list id
                            (cdr (sly-trace-dialog--trace-spec trace)))
           'part-label (format "Trace entry: %s" id)
           props)))

(defun sly-trace-dialog--draw-tree-lines (start offset direction)
  (save-excursion
    (let ((inhibit-point-motion-hooks t))
      (goto-char start)
      (cl-loop with replace-set = (if (eq direction 'down)
                                      '(? )
                                    '(?  ?`))
               for line-beginning = (line-beginning-position
                                     (if (eq direction 'down)
                                         2 0))
               for pos = (+ line-beginning offset)
               while (and (< (point-min) line-beginning)
                          (< line-beginning (point-max))
                          (memq (char-after pos) replace-set))
               do
               (sly-trace-dialog--go-replace-char-at pos "|")
               (goto-char pos)))))

(defun sly-trace-dialog--make-indent (depth suffix)
  (concat (make-string (* 3 (max 0 (1- depth))) ? )
          (if (cl-plusp depth) suffix)))

(defun sly-trace-dialog--make-collapse-button (trace)
  (sly-make-action-button (if (sly-trace-dialog--trace-collapsed-p trace)
                              (cdr sly-trace-dialog--collapse-chars)
                            (car sly-trace-dialog--collapse-chars))
                          #'(lambda (button)
                              (sly-trace-dialog--set-collapsed
                               (not (sly-trace-dialog--trace-collapsed-p
                                     trace))
                               trace
                               button))))

(defun sly-trace-dialog--insert-trace (trace)
  (let* ((id (sly-trace-dialog--trace-id trace))
         (parent (sly-trace-dialog--trace-parent trace))
         (has-children-p (sly-trace-dialog--trace-children-end trace))
         (indent-spec (sly-trace-dialog--make-indent
                       (sly-trace-dialog--trace-depth trace)
                       "`--"))
         (indent-summary (sly-trace-dialog--make-indent
                          (sly-trace-dialog--trace-depth trace)
                          "   "))
         (id-string
          (sly-trace-dialog-spec-button
           (format "%4s" id) trace 'skip t 'action 'sly-button-inspect))
         (spec-button (sly-trace-dialog-spec-button
                       (format "%s" (car (sly-trace-dialog--trace-spec trace)))
                       trace))
         (summary (cl-loop for (type objects marker) in
                           `((:arg    ,(sly-trace-dialog--trace-args trace)
                                      " > ")
                             (:retval ,(sly-trace-dialog--trace-retlist trace)
                                      " < "))
                           concat (cl-loop for object in objects
                                           concat "      "
                                           concat indent-summary
                                           concat marker
                                           concat (sly-trace-dialog-part-button
                                                   (cl-first object)
                                                   (cl-second object)
                                                   id
                                                   type)
                                           concat "\n"))))
    (puthash id trace sly-trace-dialog--traces)
    ;; insert and propertize the text
    ;;
    (setf (sly-trace-dialog--trace-beg trace) (point-marker))
    (insert id-string " ")
    (insert indent-spec)
    (if has-children-p
        (insert (sly-trace-dialog--make-collapse-button trace))
      (setf (sly-trace-dialog--trace-collapse-button-marker trace)
            (point-marker))
      (insert "-"))
    (insert " " spec-button "\n")
    (setf (sly-trace-dialog--trace-summary-beg trace) (point-marker))
    (insert summary)
    (setf (sly-trace-dialog--trace-end trace) (point-marker))
    (set-marker-insertion-type (sly-trace-dialog--trace-beg trace) t)

    ;; respect brief mode and collapsed state
    ;;
    (cl-loop for condition in (list sly-trace-dialog-hide-details-mode
                                    (sly-trace-dialog--trace-collapsed-p trace))
             when condition
             do (sly-trace-dialog--hide-unhide
                 (sly-trace-dialog--trace-summary-beg
                  trace)
                 (sly-trace-dialog--trace-end trace)
                 1))
    (cl-loop for tr = trace then parent
             for parent = (sly-trace-dialog--trace-parent tr)
             while parent
             when (sly-trace-dialog--trace-collapsed-p parent)
             do (sly-trace-dialog--hide-unhide
                 (sly-trace-dialog--trace-beg trace)
                 (sly-trace-dialog--trace-end trace)
                 (+ 1
                    (or (get-text-property (sly-trace-dialog--trace-beg parent)
                                           'sly-trace-dialog--hidden-level)
                        0)))
             (cl-return))
    ;; maybe add the collapse-button to the parent in case it didn't
    ;; have one already
    ;;
    (when (and parent
               (sly-trace-dialog--trace-collapse-button-marker parent))
      (sly-trace-dialog--maintaining-properties
          (sly-trace-dialog--trace-collapse-button-marker parent)
        (delete-char 1)
        (insert (sly-trace-dialog--make-collapse-button parent))
        (setf (sly-trace-dialog--trace-collapse-button-marker parent)
              nil)))
    ;; draw the tree lines
    ;;
    (when parent
      (sly-trace-dialog--draw-tree-lines (sly-trace-dialog--trace-beg trace)
                                         (+ 2 (length indent-spec))
                                         'up))
    (when has-children-p
      (sly-trace-dialog--draw-tree-lines (sly-trace-dialog--trace-beg trace)
                                         (+ 5 (length indent-spec))
                                         'down))
    ;; set the "children-end" slot
    ;;
    (unless (sly-trace-dialog--trace-children-end trace)
      (cl-loop for parent = trace
               then (sly-trace-dialog--trace-parent parent)
               while parent
               do
               (setf (sly-trace-dialog--trace-children-end parent)
                     (sly-trace-dialog--trace-end trace))))))

(defun sly-trace-dialog--render-trace (trace)
  ;; Render the trace entry in the appropriate place.
  ;;
  ;; A trace becomes a few lines of slightly propertized text in the
  ;; buffer, inserted by `sly-trace-dialog--insert-trace', bound by
  ;; point markers that we use here.
  ;;
  ;; The new trace might be replacing an existing one, or otherwise
  ;; must be placed under its existing parent which might or might not
  ;; be the last entry inserted.
  ;;
  (let ((existing (sly-trace-dialog--find-trace
                   (sly-trace-dialog--trace-id trace)))
        (parent (sly-trace-dialog--trace-parent trace)))
    (cond (existing
           ;; Other traces might already reference `existing' and with
           ;; need to maintain that eqness. Best way to do that is
           ;; destructively modify `existing' with the new retlist...
           ;;
           (setf (sly-trace-dialog--trace-retlist existing)
                 (sly-trace-dialog--trace-retlist trace))
           ;; Now, before deleting and re-inserting `existing' at an
           ;; arbitrary point in the tree, note that it's
           ;; "children-end" marker is already non-nil, and informs us
           ;; about its parenthood status. We want to 1. leave it
           ;; alone if it's already a parent, or 2. set it to nil if
           ;; it's a leaf, thus forcing the needed update of the
           ;; parents' "children-end" marker.
           ;;
           (when (= (sly-trace-dialog--trace-children-end existing)
                    (sly-trace-dialog--trace-end existing))
             (setf (sly-trace-dialog--trace-children-end existing) nil))
           (delete-region (sly-trace-dialog--trace-beg existing)
                          (sly-trace-dialog--trace-end existing))
           (goto-char (sly-trace-dialog--trace-end existing))
           ;; Remember to set `trace' to be `existing'
           ;;
           (setq trace existing))
          (parent
           (goto-char (1+ (sly-trace-dialog--trace-children-end parent))))
          (;; top level trace
           t
           (goto-char (point-max))))
    (goto-char (line-beginning-position))
    (sly-trace-dialog--insert-trace trace)))

(defun sly-trace-dialog--update-tree (tuples)
  (save-excursion
    (sly-refreshing
        (:overlay sly-trace-dialog--tree-overlay
                  :dont-erase t)
      (cl-loop for tuple in tuples
               for parent = (sly-trace-dialog--find-trace (cl-second tuple))
               for trace = (sly-trace-dialog--make-trace
                            :id (cl-first tuple)
                            :parent parent
                            :spec (cl-third tuple)
                            :args (cl-fourth tuple)
                            :retlist (cl-fifth tuple)
                            :depth (if parent
                                       (1+ (sly-trace-dialog--trace-depth
                                            parent))
                                     0))
               do (sly-trace-dialog--render-trace trace)))))

(defun sly-trace-dialog--clear-local-tree ()
  (set (make-local-variable 'sly-trace-dialog--fetch-key)
       (cl-gensym "sly-trace-dialog-fetch-key-"))
  (set (make-local-variable 'sly-trace-dialog--traces)
       (make-hash-table))
  (sly-refreshing
      (:overlay sly-trace-dialog--tree-overlay))
  (sly-trace-dialog--update-progress nil))

(defun sly-trace-dialog--on-new-results (results &optional recurse)
  (cl-destructuring-bind (tuples remaining reply-key)
      results
    (cond ((and sly-trace-dialog--fetch-key
                (string= (symbol-name sly-trace-dialog--fetch-key)
                         (symbol-name reply-key)))
           (sly-trace-dialog--update-tree tuples)
           (sly-trace-dialog--update-progress
            remaining
            (and recurse
                 (cl-plusp remaining))
            t)
           (when (and recurse
                      (not (prog1 sly-trace-dialog--stop-fetching
                             (setq sly-trace-dialog--stop-fetching nil)))
                      (cl-plusp remaining))
             (sly-eval-async `(slynk-trace-dialog:report-partial-tree
                                 ',reply-key)
               #'(lambda (results) (sly-trace-dialog--on-new-results
                                    results
                                    recurse))))))))


;;;; Interactive functions
;;;
(defun sly-trace-dialog-fetch-specs ()
  "Refresh just list of traced specs."
  (interactive)
  (sly-eval-async `(slynk-trace-dialog:report-specs)
    #'sly-trace-dialog--open-specs))

(defun sly-trace-dialog-fetch-progress ()
  (interactive)
  (sly-eval-async
      '(slynk-trace-dialog:report-total)
    #'(lambda (total)
        (sly-trace-dialog--update-progress
         total))))

(defun sly-trace-dialog-fetch-status ()
  "Refresh just the status part of the SLY Trace Dialog"
  (interactive)
  (sly-trace-dialog-fetch-specs)
  (sly-trace-dialog-fetch-progress))

(defun sly-trace-dialog-clear-fetched-traces (&optional interactive)
  "Clear local and remote traces collected so far"
  (interactive "p")
  (when (or (not interactive)
            (y-or-n-p "Clear all collected and fetched traces?"))
    (sly-eval-async
        '(slynk-trace-dialog:clear-trace-tree)
      #'(lambda (_ignored)
          (sly-trace-dialog--clear-local-tree)))))

(defun sly-trace-dialog-fetch-traces (&optional recurse)
  (interactive "P")
  (setq sly-trace-dialog--stop-fetching nil)
  (sly-eval-async `(slynk-trace-dialog:report-partial-tree
                      ',sly-trace-dialog--fetch-key)
    #'(lambda (results) (sly-trace-dialog--on-new-results results
                                                            recurse))))

(defvar sly-trace-dialog-after-toggle-hook nil
  "Hooks run after toggling a dialog-trace")

(defun sly-trace-dialog-toggle-trace (&optional using-context-p)
  "Toggle the dialog-trace of the spec at point.

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
         (spec-string (if (fboundp 'sly-trace-query)
                          (sly-trace-query spec-string)
                        spec-string)))
    (sly-message "%s" (sly-eval `(slynk-trace-dialog:dialog-toggle-trace
                                  (slynk::from-string ,spec-string))))
    (run-hooks 'sly-trace-dialog-after-toggle-hook)))

(defun sly-trace-dialog-untrace-all ()
  "Untrace all specs traced for the Trace Dialog."
  (interactive)
  (sly-eval-async `(slynk-trace-dialog:dialog-untrace-all)
    #'(lambda (results)
        (sly-message "%s dialog specs and %s regular specs untraced"
                       (cdr results) (car results) )))
  (run-hooks 'sly-trace-dialog-after-toggle-hook))

(defun sly-trace-dialog--update-existing-dialog ()
  (let ((existing (sly-trace-dialog--live-dialog)))
    (when existing
      (with-current-buffer existing
        (sly-trace-dialog-fetch-status)))))

(add-hook 'sly-trace-dialog-after-toggle-hook
          'sly-trace-dialog--update-existing-dialog)

(defun sly-trace-dialog-toggle-complex-trace ()
  "Toggle the dialog-trace of the complex spec at point.

See `sly-trace-dialog-toggle-trace'."
  (interactive)
  (sly-trace-dialog-toggle-trace t))

(defun sly-trace-dialog (&optional clear-and-fetch)
  "Show trace dialog and refresh trace collection status.

With optional CLEAR-AND-FETCH prefix arg, clear the current tree
and fetch a first batch of traces."
  (interactive "P")
  (with-current-buffer
      ;; FIXME: refactor with `sly-with-popup-buffer'
      (pop-to-buffer
       (sly-trace-dialog--ensure-buffer)
       `(display-buffer-reuse-window . ((inhibit-same-window . t))))
    (sly-trace-dialog-fetch-status)
    (when (or clear-and-fetch
              (null sly-trace-dialog--fetch-key))
      (sly-trace-dialog--clear-local-tree))
    (when clear-and-fetch
      (sly-trace-dialog-fetch-traces nil))))

(provide 'sly-trace-dialog)
