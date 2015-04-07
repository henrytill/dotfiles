;;;; Machine-specific settings

;;; Darwin machines
(when (and (is-darwin-p) (window-system))
  (let ((ansi-term (expand-file-name "ansi-term" user-emacs-directory))
        (aspell-dir (expand-directory-name "~/.nix-profile/lib/aspell/"))
        (mplus-font (expand-file-name "mplus-1mn-regular.ttf" "~/Library/Fonts")))
    (setq explicit-shell-file-name ansi-term
          mac-command-modifier 'super
          mac-option-modifier 'meta)
    (when (file-directory-p aspell-dir)
      (setenv "ASPELL_CONF" (concat "dict-dir " aspell-dir)))
    (when (file-exists-p mplus-font)
      (set-face-attribute 'default nil :font "M+ 1mn 14"))
    (add-to-list 'default-frame-alist '(height . 40))
    (add-to-list 'default-frame-alist '(width . 100))))

;;; NixOS machines
(when (and (is-linux-p) (file-directory-p "/etc/nixos"))
  (require 'tramp)
  (add-to-list 'tramp-remote-path "/run/current-system/sw/bin"))
