(defun ht/dired-pwd ()
  (interactive)
  (dired default-directory))

(define-key evil-normal-state-map (kbd "-") #'ht/dired-pwd)

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "-") #'dired-up-directory))

(provide 'feature-dired)
