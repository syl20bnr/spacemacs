(eval-when-compile (require 'cl))
(require 'net-fsm)

(defvar epmd-hosts '("localhost")
  "Hosts to query EPMDs on.")

(defvar epmd-port 4369)

(defun epmd-process (event arg)
  (check-event event 'init)
  ;; Arg is the request
  (let* ((len (length arg))
	 (len-msb (ash len -8))
	 (len-lsb (logand len 255)))
    (fsm-send-string (concat (string len-msb len-lsb)
			     arg)))
  (ecase (elt arg 0)
    ((?n) (fsm-change-state #'epmd-recv-names-resp))
    ((?z) (fsm-change-state #'epmd-recv-port-resp))
    ((?a) (fsm-change-state #'epmd-recv-alive-resp))))

(defun epmd-recv-names-resp (event data)
  (check-event event 'data)
  (assert (>= (length data) 4))
  (fsm-terminate (substring arg 4)))

(defun epmd-recv-port-resp (event data)
  (message "Event: %s" event)
  (message "data: %s" data)
  (message "arg: %s" arg)
  (ecase event
    ((data)
     (assert (> (length arg) 2))
     (assert (= 119 (elt data 0)))
     (fsm-terminate (+ (ash    (elt arg 2) 8)
		       (logand (elt arg 3) 255))))
    ((closed)
     (fsm-fail))))

(defun epmd-recv-alive-resp (event data)
  (ecase event
    ((closed)
     (fsm-fail))
    ((data)
     (if (equal (string ?y 0) (substring data 0 2))
	 (let ((creation (substring data 2)))
	   ;; Cheat by calling the success continuation without
	   ;; terminating, since we need to keep the socket open.
	   (when fsm-cont
	     (funcall fsm-cont creation))
	   (fsm-change-state #'epmd-alive))
       (fsm-fail)))))
	 
(defun epmd-alive (event data)
  (check-event event 'close)
  (fsm-fail))

(defun epmd-show-nodes ()
  (interactive)
  (epmd-collect-names epmd-hosts ""))

(defun epmd-collect-names (hosts string)
  (if (null hosts)
      (epmd-show-names string)
    (lexical-let* ((host (car hosts))
		   (remaining-hosts (cdr hosts))
		   (string string))
      (let ((cont
	     (lambda (new-result)
	       (epmd-collect-names remaining-hosts
				   (format "%s[%s]\n%s\n"
					   string host new-result))))
	    (fail
	     (lambda ()
	       (epmd-collect-names remaining-hosts
				   (format "%s[%s]\nUnable to connect.\n\n"
					   string host)))))
	(fsm-connect host epmd-port #'epmd-process "n" cont fail)))))

(defun epmd-port-please (node host cont &optional fail-cont)
  (fsm-connect host epmd-port #'epmd-process (concat "z" node) cont fail-cont))

;; (defun epmd-login (nodename &optional cont fail-cont)
;;   (fsm-connect host epmd-port #'epmd-process (epmd-make-alive-req nodename)
;; 	       cont fail-cont))

;; (defun epmd-make-alive-req (nodename)
;;   (with-temp-buffer
;;     (insert ?x)

(defun epmd-show-names (string)
  (with-current-buffer (get-buffer-create "*epmd nodes*")
    (erase-buffer)
    (insert string)
    (view-buffer-other-window (current-buffer))))

;; testing

(defun epmd-test (node host)
  (interactive "sNode: \nsHost: ")
  (epmd-port-please node host (lambda (x) (message "X = %S" x))))

(provide 'epmd)

