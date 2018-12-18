(define-minor-mode pulp-mode "pulp-mode")

(defun pulp-mode/get-project-dir ()
  (locate-dominating-file (or (buffer-file-name nil) default-directory) "node_modules"))

(defun pulp-mode/project-dir ()
  (pulp-mode/get-project-dir))

(defun pulp-mode/node-modules-dir ()
  (expand-directory-name "node_modules" (pulp-mode/project-dir)))

(defun pulp-mode/pulp-binary-location ()
  (expand-file-name "pulp" (expand-directory-name ".bin" (pulp-mode/node-modules-dir))))

(provide 'pulp-mode)
