(use-package compile
  :commands compile
  :init
  ;; http://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html
  (require 'ansi-color)
  (defun ht/colorize-compilation-buffer ()
    "Colorize from `compilation-filter-start' to `point'."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point))))
  (add-hook 'compilation-filter-hook 'ht/colorize-compilation-buffer))

(bind-key "<f6>" 'recompile)

(provide 'feature-compile)
