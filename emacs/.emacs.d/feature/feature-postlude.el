(with-eval-after-load 'tramp-sh
  (add-to-list 'tramp-remote-path "/run/current-system/sw/bin"))

(when (string-equal "thaumas" (ht/hostname))
  (setq doc-view-resolution 150
        doc-view-scale-internally nil))

(provide 'feature-postlude)
