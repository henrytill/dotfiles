(use-package magit
  :ensure t
  :commands magit-status
  :config
  (put 'magit-clean 'disabled nil)
  (setq magit-last-seen-setup-instructions "1.4.0"))

(defhydra ht/hydra-magit (:idle 1.0)
  "
magit
-----
_s_: magit-status
"
  ("s" magit-status nil :exit t))

(provide 'feature-magit)
