(require 'yasnippet)

(defun yas-c++-class-name (str)
  "Search for a class name like `DerivedClass' in STR
(which may look like `DerivedClass : ParentClass1, ParentClass2, ...')

If found, the class name is returned, otherwise STR is returned"
  (yas-substr str "[^: ]*"))

(defun yas-c++-class-method-declare-choice ()
  "Choose and return the end of a C++11 class method declaration"
  (yas-choose-value '(";" " = default;" " = delete;")))
