;; =============================================================================
;; Work bindings
;; =============================================================================

;; Mappy files
(defvar mappy_base_path (concat "/home/sbenner/dev/p4/virga1-sbenner-linux/"
                                "virga/users/sylvain/dev/services/API/XCloud/"
                                "Python/"))
(defvar mappy_working_path (concat mappy_base_path "src/tools/mappy"))
(defvar mappy_output_path (concat mappy_base_path
                                  "src/xcloud/compute/resource/"))
(global-set-key (kbd "C-c s m")
                (lambda ()
                  (interactive)
                  (shell-command (concat "cd " mappy_working_path
                                         "&& python mappy.py -t wrapper -o "
                                         mappy_output_path " " mappy_output_path
                                         "states.graphml"))))

(provide 'my-keybindings-work)
