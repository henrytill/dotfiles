;;;; Machine-specific settings

;;; Darwin machines
(when (and (is-darwin-p) (window-system))
  (setq mac-command-modifier 'super
        mac-option-modifier 'meta)
  (when (file-exists-p (expand-file-name "mplus-1mn-light.ttf" "~/Library/Fonts"))
    (set-default-font "M+ 1mn 14"))
  (invert-face 'default)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) 100 40)
  (setq explicit-shell-file-name (expand-file-name "ansi-term" user-emacs-directory))
  (server-mode 1))

;;; NixOS machines
(when (and (is-linux-p) (file-directory-p "/etc/nixos"))
  (require 'tramp)
  (add-to-list 'tramp-remote-path "/run/current-system/sw/bin"))
