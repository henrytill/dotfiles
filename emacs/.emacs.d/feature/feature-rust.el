(defhydra ht/hydra-rust (:idle 1)
  "
rust
----
_gd_: racer-find-definition
"
  ("gd" racer-find-definition nil :exit t))

(defun ht/racer-mode ()
  (progn
    (exec-path-from-shell-copy-env "RUST_SRC_PATH")
    (let ((cmd (executable-find "racer")))
      (when cmd
        (setq racer-cmd cmd)))))

(use-package racer
  :disabled t
  :ensure t
  :if (executable-find "racer")
  :commands racer-mode
  :init
  (add-hook 'racer-mode-hook 'ht/racer-mode)
  (add-hook 'racer-mode-hook 'company-mode)
  (add-hook 'racer-mode-hook 'eldoc-mode))

(defun ht/rust-mode ()
  (setq-local ht/hydra-mode-specific 'ht/hydra-rust/body))

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :init
  (add-hook 'rust-mode-hook 'auto-revert-mode)
  (add-hook 'rust-mode-hook 'electric-pair-mode))

(provide 'feature-rust)
