(require 'eproject)
(require 'eproject-extras)

(define-project-type python (generic)
  (look-for "dummy_dummy")
  :relevant-files ("\\.py$")
  :irrevelant-files ("\\.pyc$"))

(define-project-type Wrappy (python) (look-for "wrappy.py"))
(define-project-type Mappy (python) (look-for "mappy.py"))

(require 'eproject-anything)
(global-set-key (kbd "s-L") 'anything-eproject-files)
(global-set-key (kbd "s-b") 'anything-eproject-buffers)
