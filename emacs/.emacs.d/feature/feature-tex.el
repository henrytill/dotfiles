(use-package auctex
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :config
  (setq TeX-auto-save t
        TeX-parse-self t)
  (setq-default TeX-master nil))

(with-eval-after-load 'tex
  (when (and (is-linux-p) (executable-find "mupdf-x11"))
    (add-to-list 'TeX-view-program-list
                 '("mupdf" ("mupdf-x11" (mode-io-correlate " -p %(outpage)") " %o")))
    (add-to-list 'TeX-view-program-selection
                 '(output-pdf "mupdf"))))

(provide 'feature-tex)
