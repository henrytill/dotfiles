(defun ht/agda2-load-path ()
  (when (executable-find "agda-mode")
    (file-name-directory (shell-command-to-string "agda-mode locate"))))

(defun ht/agda2-mode ()
  (interactive)
  (custom-set-variables '(agda2-highlight-face-groups 'default-faces)))

(use-package agda2-mode
  :if (executable-find "agda-mode")
  :load-path (lambda () (ht/agda2-load-path))
  :mode "\\.agda\\'"
  :init
  (add-hook 'agda2-mode-hook #'ht/agda2-mode))

(provide 'feature-agda)
