(use-package magit
  :ensure t
  :commands magit-status
  :config
  (put 'magit-clean 'disabled nil)
  (setq magit-last-seen-setup-instructions "1.4.0"))

(bind-map ht/magit-leader-map
  :keys ("M-m m")
  :evil-keys ("SPC m"))

(bind-map-set-keys ht/magit-leader-map
  "s" 'magit-status)

(provide 'feature-magit)
