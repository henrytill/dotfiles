(require 'pulp-mode)

(define-derived-mode inferior-psci-mode comint-mode "Inferior PSCi")

(defun run-psci ()
  (interactive)
  (when (pulp-mode/project-dir)
    (let ((pulp (pulp-mode/pulp-binary-location)))
      (when (file-exists-p pulp)
        (switch-to-buffer-other-window (make-comint "PSCi" pulp nil "psci"))
        (inferior-psci-mode)))))

(provide 'inferior-psci-mode)
