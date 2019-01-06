(use-package prog-mode
  :defer t
  :init
  (add-hook 'prog-mode-hook 'page-break-lines-mode)
  (add-hook 'prog-mode-hook 'undo-tree-mode))

(provide 'feature-prog-mode)
