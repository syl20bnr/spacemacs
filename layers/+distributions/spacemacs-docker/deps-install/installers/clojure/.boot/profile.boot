(swap! boot.repl/*default-dependencies* conj
       (quote [refactor-nrepl "2.2.0-SNAPSHOT"])
       (quote [cljfmt "0.5.6"])
       (quote [cider/cider-nrepl "0.14.0-SNAPSHOT"]))
(swap! boot.repl/*default-middleware* conj
       (quote refactor-nrepl.middleware/wrap-refactor))
