(defun ht/ats-load-path ()
  (expand-directory-name "utils/emacs" (getenv "PATSHOME")))

(use-package ats2-mode
  :if (file-directory-p (ht/ats-load-path))
  :load-path (lambda () (list (ht/ats-load-path)))
  :mode (("\\.cats\\'" . c-mode)
         ("\\.dats\\'" . ats-mode)
         ("\\.hats\\'" . ats-mode)
         ("\\.sats\\'" . ats-mode)))

(provide 'feature-ats)
