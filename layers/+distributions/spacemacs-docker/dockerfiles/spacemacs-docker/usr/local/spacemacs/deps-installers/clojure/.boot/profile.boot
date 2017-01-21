(swap! boot.repl/*default-dependencies* conj
       (quote [refactor-nrepl "1.2.0-SNAPSHOT"])
       (quote [cider/cider-nrepl "0.11.0-SNAPSHOT"]))
(swap! boot.repl/*default-middleware* conj
       (quote refactor-nrepl.middleware/wrap-refactor))
