(defun ht/fsharp-mode ()
  (setq fsharp-indent-level 2
        fsharp-indent-offset 2))

(use-package fsharp-mode
  :ensure t
  :mode (("\\.fs[iylx]?$" . fsharp-mode))
  :init
  (add-hook 'fsharp-mode-hook #'ht/fsharp-mode))

(provide 'feature-fsharp)
