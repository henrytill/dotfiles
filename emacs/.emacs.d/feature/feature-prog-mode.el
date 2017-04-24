(defun ht/prog-mode ()
  (linum-mode 1))

(use-package prog-mode
  :defer t
  :init
  (add-hook 'prog-mode-hook 'ht/prog-mode)
  (add-hook 'prog-mode-hook 'page-break-lines-mode)
  (add-hook 'prog-mode-hook 'undo-tree-mode)
  (add-hook 'prog-mode-hook 'whitespace-mode))

(provide 'feature-prog-mode)
