(require 'rcirc)

(defun helm-rcirc-auto-join-channels-alist ()
  "Return an alist where key is a server and value is a list of defined
channels to auto join."
  (mapcar (lambda (server)
            (cons (car server) (plist-get (cdr server) :channels)))
          rcirc-server-alist)
)

(defun helm-rcirc-auto-join-channels-source (entry)
  "Construct the Helm source given an ENTRY.

ENTRY is a key value pair where key is the server and value is a list of
channels."
  (let ((server (car entry))
        (chans (cdr entry)))
    (list (cons 'name (format "Server: %s" (car entry)))
          (cons 'candidates (mapcar (lambda (chan)
                                      (cons chan (cons server chan))) chans))
          (cons 'action 'helm-rcirc-open-channel-buffer))))

(defun helm-rcirc-open-channel-buffer (selected-value)
  "Open the buffer corresponding to SELECTED-VALUE if any.

SELECTED-VALUE is a key value pair where key is the server and value is the
channel name."
  (let ((buf (format "%s@%s" (cdr selected-value) (car selected-value))))
    (if (bufferp (get-buffer buf))
        (switch-to-buffer buf)
      (message (format "Cannot find buffer %s" buf)))))

;;;###autoload
(defun helm-rcirc-auto-join-channels ()
  "rcirc auto join channels selection with helm interface."
  (interactive)
  (helm :buffer "*helm: Auto Join Channels (rcirc)*"
        :sources (mapcar 'helm-rcirc-auto-join-channels-source
                         (helm-rcirc-auto-join-channels-alist))))
