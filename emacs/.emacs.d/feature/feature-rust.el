(defun ht/racer-mode ()
  (let ((cmd (executable-find "racer")))
    (when cmd
      (setq racer-cmd cmd))))

(use-package racer
  :ensure t
  :if (executable-find "racer")
  :commands racer-mode
  :init
  (add-hook 'racer-mode-hook 'ht/racer-mode)
  (add-hook 'racer-mode-hook 'company-mode)
  (add-hook 'racer-mode-hook 'eldoc-mode))

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :init
  (use-package flycheck-rust
    :ensure t
    :defer t
    :init
    (add-hook 'flycheck-mode-hook 'flycheck-rust-setup))
  (add-hook 'rust-mode-hook 'auto-revert-mode)
  (add-hook 'rust-mode-hook 'electric-pair-mode)
  (add-hook 'rust-mode-hook 'flycheck-mode)
  (add-hook 'rust-mode-hook 'racer-mode))

(provide 'feature-rust)
