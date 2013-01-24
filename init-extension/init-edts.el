(require 'edts-start)

(defconst edts-rest-host "localhost"
  "The host where the edts erlang node is running.")

(setq edts-log-level 'debug)

(setq edts-projects
      '(( ;; Erlang tests.
         (name       . "erlang")
         (root       . "/home/sbenner/dev/projects/erlang")
         (node-sname . "erltests"))
        ))
