(use-package pylookup
  :defer t
  :config
  (progn
    (setq pylookup-dir (concat user-extensions-directory "/pylookup"))
    ;; set executable file and db file
    (setq pylookup-program (concat pylookup-dir "/pylookup.py"))
    (setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))
))
