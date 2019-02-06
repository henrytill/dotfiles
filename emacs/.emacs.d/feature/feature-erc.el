(defun ht/erc-match (&optional match-type nick message)
  (let (alert-log-messages)
    (alert (or message (buffer-string))
           :title (concat "ERC: " (or nick (buffer-name)))
           :severity 'high
           :data message)))

(defun ht/erc-sesh ()
  (interactive)
  (let ((erc-sesh (expand-file-name "erc-sesh.el.gpg" "~/prv/emacs")))
    (when (file-exists-p erc-sesh)
      (load-file erc-sesh))))

(use-package erc
  :defer t
  :config
  (setq erc-rename-buffers t
        erc-fill-function 'erc-fill-static
        erc-fill-static-center 19
        erc-hide-list '("JOIN" "PART" "QUIT")
        erc-keywords '("henrytill" "musnix")
        erc-prompt ">"
        erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                  "324" "329" "332" "333" "353" "477"))
  (add-hook 'erc-text-matched-hook 'ht/erc-match)
  (use-package erc-hl-nicks :ensure t)
  (require 'erc-spelling)
  (add-to-list 'erc-modules 'hl-nicks)
  (add-to-list 'erc-modules 'spelling))

(provide 'feature-erc)
