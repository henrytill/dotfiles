(defun ht/npm-local-executable-path (name)
  (let* ((npm-bin-path (string-trim (shell-command-to-string "npm bin")))
         (npm-exe-path (expand-file-name name npm-bin-path)))
    (when (file-executable-p npm-exe-path) npm-exe-path)))

(defun ht/js2-init ()
  (setq js2-additional-externs '("describe" "it"))
  (setq flycheck-javascript-eslint-executable (ht/npm-local-executable-path "eslint"))
  (flycheck-mode 1))

(use-package js
  :mode (("\\.json\\'" . js-mode))
  :init
  (add-hook 'js-mode-hook 'electric-pair-mode)
  :config
  (setq js-indent-level 2))

(use-package js2-mode
  :ensure t
  :mode (("\\.js\\'"  . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :interpreter ("node" . js2-mode)
  :init
  (add-hook 'js2-init-hook 'ht/js2-init)
  :config
  (setq js2-basic-offset 2
        js2-include-node-externs t
        js2-indent-switch-body t))

(provide 'feature-javascript)
