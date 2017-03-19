(defun ht/agda2-load-path ()
  (when (executable-find "agda-mode")
    (file-name-directory (shell-command-to-string "agda-mode locate")))))

(use-package agda2-mode
  :if (executable-find "agda-mode")
  :load-path (lambda () (ht/agda2-load-path))
  :mode "\\.agda\\'")

(provide 'feature-agda)
