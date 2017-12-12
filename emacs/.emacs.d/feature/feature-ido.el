(defun ht/ido-define-keys ()
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))

(defun ht/ido-disable-line-truncation ()
  (set (make-local-variable 'truncate-lines) nil))

(use-package ido
  :config
  (use-package flx-ido
    :ensure t
    :commands flx-ido-mode
    :config
    (setq flx-ido-use-faces nil))
  ;; Vertical Ido Results
  (setq ido-decorations '("\n-> " "" "\n   " "\n   ..." "[" "]"
                          " [No match]" " [Matched]" " [Not readable]"
                          " [Too big]" " [Confirm]")
        ido-enable-flex-matching t)
  (add-hook 'ido-minibuffer-setup-hook #'ht/ido-disable-line-truncation)
  (add-hook 'ido-setup-hook            #'ht/ido-define-keys)
  (add-hook 'ido-setup-hook            #'flx-ido-mode)
  (ido-mode t))

(provide 'feature-ido)
