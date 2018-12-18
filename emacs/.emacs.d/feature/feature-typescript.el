(use-package tide
  :ensure t
  :commands tide-setup
  :init
  (defun ht/tide-mode ()
    (tide-setup)
    (flycheck-mode 1)
    (eldoc-mode 1)
    (define-key evil-normal-state-local-map (kbd "C-]") 'tide-jump-to-definition)
    (define-key evil-normal-state-local-map (kbd "C-t") 'tide-jump-back)))

(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :init
  (add-hook 'typescript-mode-hook 'ht/tide-mode))

(use-package web-mode
  :ensure t
  :mode "\\.tsx\\'"
  :init
  (defun ht/web-mode ()
    (when (string-equal "tsx" (file-name-extension buffer-file-name))
      (ht/tide-mode)))
  (add-hook 'web-mode-hook 'ht/web-mode))

(provide 'feature-typescript)
