(use-package whitespace
  :commands whitespace-mode
  :diminish whitespace-mode
  :init
  (setq whitespace-style '(face lines-tail trailing)
        whitespace-line-column 100)
  :config
  (defun ht/toggle-tabs-display ()
    (interactive)
    (whitespace-mode -1)
    (if (memq 'tabs whitespace-style)
        (setq whitespace-style (remove 'tabs whitespace-style))
      (add-to-list 'whitespace-style 'tabs))
    (whitespace-mode 1)))

(provide 'feature-whitespace)
