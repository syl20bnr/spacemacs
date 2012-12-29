(require 'find-file-in-project)

;; Use eproject to find project root
(setq ffip-project-root-function 'eproject-root)
;; No need to be stingy
(setq ffip-limit 4096)

;; Helper methods to create local settings
(defun ffip--create-exclude-find-options (names)
  (mapconcat (lambda (name)
               (concat "-not -regex \".*" name ".*\"")) names " "))

(defun ffip-local-excludes (&rest names)
  "Given a set of names, will exclude results with those names in the path.
Example:
(ffip-local-excludes \"target\" \"overlays\")
"
  (set (make-local-variable 'ffip-find-options)
       (ffip--create-exclude-find-options names)))

(defun ffip-local-patterns (&rest patterns)
  "An exhaustive list of file name patterns to look for.
Example:
(ffip-local-patterns \"*.js\" \"*.jsp\" \"*.css\")
"
  (set (make-local-variable 'ffip-patterns) patterns))

;; Function to create new functions that look for a specific pattern
(defun ffip-create-pattern-file-finder (&rest patterns)
  (lexical-let ((patterns patterns))
    (lambda ()
      (interactive)
      (let ((ffip-patterns patterns))
        (find-file-in-project)))))

;; Default excludes - override with ffip-local-excludes
(setq ffip-find-options
      (ffip--create-exclude-find-options
       '("node_modules"
         "target"
         "overlays"
         "vendor")))

;; key bindings
(global-set-key (kbd "C-x o") 'find-file-in-project)
;; Find file in project, with specific patterns
(global-unset-key (kbd "C-x C-o")) ;; which used to be delete-blank-lines
                                   ;; (also bound to C-c C-<return>)
(global-set-key (kbd "C-x C-o cl") (ffip-create-pattern-file-finder "*.clj"))
(global-set-key (kbd "C-x C-o cs") (ffip-create-pattern-file-finder "*.css"))
(global-set-key (kbd "C-x C-o el") (ffip-create-pattern-file-finder "*.el"))
(global-set-key (kbd "C-x C-o ja") (ffip-create-pattern-file-finder "*.java"))
(global-set-key (kbd "C-x C-o jp") (ffip-create-pattern-file-finder "*.jsp"))
(global-set-key (kbd "C-x C-o js") (ffip-create-pattern-file-finder "*.js"))
(global-set-key (kbd "C-x C-o md") (ffip-create-pattern-file-finder "*.md"))
(global-set-key (kbd "C-x C-o or") (ffip-create-pattern-file-finder "*.org"))
(global-set-key (kbd "C-x C-o ph") (ffip-create-pattern-file-finder "*.php"))
(global-set-key (kbd "C-x C-o py") (ffip-create-pattern-file-finder "*.py"))
(global-set-key (kbd "C-x C-o rb") (ffip-create-pattern-file-finder "*.rb"))
(global-set-key (kbd "C-x C-o tx") (ffip-create-pattern-file-finder "*.txt"))
(global-set-key (kbd "C-x C-o vm") (ffip-create-pattern-file-finder "*.vm"))
