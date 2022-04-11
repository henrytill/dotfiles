(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :init
  (add-hook 'rust-mode-hook 'auto-revert-mode)
  (add-hook 'rust-mode-hook 'electric-pair-mode))

(provide 'feature-rust)
