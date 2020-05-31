(use-package compile
  :commands compile
  :init
  (ht/comment
    ;; https://stackoverflow.com/questions/4556368/compiling-c-with-emacs-on-windows-system
    (add-to-list 'compilation-error-regexp-alist
                 '(msvc "^[ \t]*\\([A-Za-z0-9\\.][^(]*\\.\\(cpp\\|c\\|h\\|hpp\\)\\)(\\([0-9]+\\)) *: +\\(error\\|fatal error\\|warning\\) C[0-9]+:" 1 3))
    nil)
  ;; http://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html
  (require 'ansi-color)
  (defun ht/colorize-compilation-buffer ()
    "Colorize from `compilation-filter-start' to `point'."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point))))
  (add-hook 'compilation-filter-hook 'ht/colorize-compilation-buffer))

(bind-key "<f6>" 'recompile)

(provide 'feature-compile)
