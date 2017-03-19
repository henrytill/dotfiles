(use-package idris-mode
  :ensure t
  :mode "\\.idr\\'"
  :init
  (add-hook 'idris-mode-hook 'electric-pair-mode))

(provide 'feature-idris)
