;; Testing

(require 'distel)

(defun erl-test ()
  (interactive)
  (erl-message-test)
  (erl-exit-test)
  (message "Smooth sailing"))

(defun erl-message-test ()
  "Some message send/receive checks."
  (let (v)
    (erl-spawn-async
      (erl-spawn-async (push 1 v))
      (erl-spawn (push 2 v))
      (push 3 v)
      (erl-send erl-self 'x)		; make ourselves schedulable
      (erl-continue (lambda () (push 4 v))))
    (unless (equal (sort v #'<) '(1 2 3 4))
      (error "Error, v = %S" v))))

(make-local-variable 'erl-test-thingo)

(defun erl-exit-test ()
  "Check that exits propagate along links."
  (let (a b c d)
    (setq a (erl-spawn
	      (setq erl-test-thingo 'a)
	      (setq c (erl-spawn (erl-continue 'nofun)))
	      (erl-link c)
	      (setq d (erl-spawn-link (setq erl-test-thingo 'd)
				      (setq erl-trap-exit t)
				      (erl-continue-forever)))
	      (setq b (erl-spawn-link-async (setq erl-test-thingo 'b)
					    (erl-exit 'stop)))
	      (erl-continue-forever)))
    (assert (not (erl-local-pid-alive-p a)))
    (assert (not (erl-local-pid-alive-p b)))
    (assert (not (erl-local-pid-alive-p c)))
    (assert (erl-local-pid-alive-p d))
    (with-erl-process d
      (assert (equal (pop erl-mailbox)
		     (tuple 'EXIT a 'stop))))
    t))

(defun erl-binding-capture-test ()
  (interactive)
  (let (bs
	(x 1)
	(y 'foo)
	(z '(error "z")))
    (setq bs (capture-bindings x y z))
    (equal '(1 foo (error "z"))
	   (with-bindings bs
	     (list x y z)))))
    

(defun erl-continue-forever ()
  (erl-continue #'erl-continue-forever))

(defun erl-spawn-tag-srv (tag)
  (erl-spawn
    (erl-register 'tag-srv)
    (erl-tag-srv-loop tag)))

(defun erl-tag-srv-loop (tag)
  (erl-receive (tag)
      (([,tag msg]
	(message "Tagged: %S" msg))
       ([other msg]
	(message "Other: %S %S" other msg)))
    (erl-tag-srv-loop tag)))

;; Interactive testing for high level features

(defvar erl-interactive-test-cases
  (list (lambda (node) (erl-process-list node))
	(lambda (node)
	  (find-file "/home/luke/devel/erlang/foo.erl")
	  (erlang-mode)
	  (erlang-extended-mode t)
	  (message "DebugMe"))))

(defvar erl-interactive-remaining-cases
  erl-interactive-test-cases)

(defun erl-interactive-next-test (node)
  (interactive (list (erl-target-node)))
  (when current-prefix-arg
    (setq erl-interactive-remaining-cases erl-interactive-test-cases))
  (funcall (pop erl-interactive-remaining-cases) node))

