(defvar fsharp-pre-extensions
  '(
    ;; pre extension fsharps go here
    )
  "List of all extensions to load before the packages.")

(defvar fsharp-post-extensions
  '(
    ;; post extension fsharps go here
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function fsharp/init-<extension-fsharp>
;;
;; (defun fsharp/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
