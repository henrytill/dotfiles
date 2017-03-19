(eval-and-compile
  (defun ht/oz-home ()
    (cond ((is-darwin-p) (let ((oz-app-path "/Applications/Mozart2.app"))
                           (when (file-directory-p oz-app-path)
                             (concat oz-app-path "/Contents/Resources"))))
          ((is-linux-p)  (let ((oz-binary-path (executable-find "oz")))
                           (when oz-binary-path
                             (car (split-string oz-binary-path "/bin/oz")))))))
  (defun ht/oz-load-path ()
    (cond ((is-darwin-p) "~/src/other/mozart-elisp")
          ((is-linux-p)  (expand-directory-name "share/mozart/elisp" (ht/oz-home))))))

(use-package oz
  :if (executable-find "oz")
  :load-path (lambda () (list (ht/oz-load-path)))
  :mode ("\\.oz\\'" . oz-mode)
  :commands run-oz
  :init
  (setenv "OZHOME" (ht/oz-home))
  (add-hook 'oz-mode-hook 'electric-pair-mode)
  (add-hook 'oz-mode-hook 'page-break-lines-mode)
  (add-hook 'oz-mode-hook 'undo-tree-mode)
  (add-hook 'oz-mode-hook 'whitespace-mode))

(provide 'feature-oz)
