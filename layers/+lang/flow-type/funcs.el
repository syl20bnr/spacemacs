(defun flow-type/known-type-at-pos ()
  ;; You'll get '(unknown)' while cursoring over comments, whitespace, keywords, etc
  ;; Don't bother reporting type information for those instances:
  (let ((type (flow-minor-get-type-at-pos)))
    (if (not (string-match "^\\(flow is still initializing\\|(unknown)\\)" type))
        type)))

(defun flow-type/enable-eldoc ()
  (if (and flow-type-enable-eldoc-type-info (flow-minor-configured-p))
      (set (make-local-variable 'eldoc-documentation-function) 'flow-type/known-type-at-pos)))

