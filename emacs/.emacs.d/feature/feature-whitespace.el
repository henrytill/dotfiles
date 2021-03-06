(use-package whitespace
  :commands whitespace-mode

  :init
  (setq whitespace-style '(face lines-tail trailing)
        whitespace-line-column 100)

  (defun ht/whitespace-mode ()
    (when (derived-mode-p 'prog-mode)
      (whitespace-mode 1)))

  (add-hook 'hack-local-variables-hook 'ht/whitespace-mode)

  :config
  (defun ht/toggle-tabs-display ()
    (interactive)
    (whitespace-mode -1)
    (if (memq 'tabs whitespace-style)
        (setq whitespace-style (remove 'tabs whitespace-style))
      (add-to-list 'whitespace-style 'tabs))
    (whitespace-mode 1))

  (defun ht/toggle-lines-tail-display ()
    (interactive)
    (whitespace-mode -1)
    (if (memq 'lines-tail whitespace-style)
        (setq whitespace-style (remove 'lines-tail whitespace-style))
      (add-to-list 'whitespace-style 'lines-tail))
    (whitespace-mode 1))

  nil)

(provide 'feature-whitespace)
