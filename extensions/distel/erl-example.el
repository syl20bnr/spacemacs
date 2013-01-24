;;; erl-example.el

(eval-when-compile (require 'cl))
(require 'erl)

;; Echo

(defun erlex-spawn-echo (recipient)
  "Start a server process that forwards all of its messages to
RECIPIENT, except for the symbol `exit' which is a shutdown message.
The server registers the name `echo'."
  (erl-spawn
    (erl-register 'echo)
    (erlex-echo-loop recipient)))

(defun erlex-echo-loop (recipient)
  "The 'receive loop' of the echo server."
  (erl-receive (recipient)
      (('exit t)
       (msg (erl-send recipient msg)
	    (erlex-echo-loop recipient)))))

(defun erlex-echo-test ()
  "Test an echo server by having it forward some messages to the null
process. These messages will end up in the *erl-lost-msgs* buffer -
check for them there!"
  (interactive)
  (erl-spawn
    (erlex-spawn-echo erl-null-pid)
    (erl-send 'echo "Hey,")
    (erl-send 'echo "it works!")
    (erl-send 'echo 'exit)))

;; Counter

(defun spawn-counter ()
  (erl-spawn
    (erl-register 'counter)
    (counter-loop 1)))

(defun counter-loop (count)
  (erl-receive (count)
      ((msg (message "Got message #%S: %S" count msg)))
    (counter-loop (1+ count))))

(defun counter-test ()
  (interactive)
  (erl-spawn
    ;; Start count server if its not already running
    (unless (erl-whereis 'counter)
      (spawn-counter))
    (erl-send 'counter 'x)
    (erl-send 'counter 'y)
    (erl-send 'counter 'z)
    ;; counter is left alive. It's easy to kill manually because of
    ;; its registered name - it's buffer is "*reg counter*", and you
    ;; can safely C-x k it.
    ))

