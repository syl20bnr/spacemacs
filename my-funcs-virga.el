;; Mappy files
(defvar mappyl_base_path (concat "/home/sbenner/dev/p4/virga1-sbenner-linux/"
                                "virga/users/sylvain/dev/services/API/XCloud/"
                                "Python/"))
(defvar mappyl_working_path (concat mappyl_base_path "src/tools/mappy"))
(defvar mappyl_output_path (concat mappyl_base_path
                                  "src/xcloud/compute/resource/"))
(defun mappyl ()
  (interactive)
  (shell-command (concat "cd " mappyl_working_path
                         "&& python mappy.py -t wrapper -o " mappyl_output_path
                         " " mappyl_output_path "states.graphml")))

(provide 'my-funcs-virga)
