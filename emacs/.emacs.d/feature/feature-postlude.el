(with-eval-after-load 'tramp-sh
  (add-to-list 'tramp-remote-path "/run/current-system/sw/bin"))

(when (string-equal "thaumas" (ht/hostname))
  (setq doc-view-resolution 150
        doc-view-scale-internally nil))

(when (is-windows-p)
  (let ((home (getenv "HOME")))
    (when home
      (setq default-directory home))))

(provide 'feature-postlude)
