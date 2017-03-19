(use-package ace-window          :ensure t :defer t)
(use-package dash                :ensure t :defer t)
(use-package idle-highlight-mode :ensure t :defer t)
(use-package pkg-info            :ensure t :defer t)
(use-package queue               :ensure t :defer t)
(use-package spinner             :ensure t :defer t)
(use-package docker-tramp        :ensure t :defer t)

(use-package eldoc
  :commands eldoc-mode
  :diminish eldoc-mode)

(use-package alert
  :ensure t
  :defer t
  :config
  (cond
   ((is-darwin-p) (setq-default alert-default-style 'ignore))
   ((is-linux-p)  (setq-default alert-default-style 'libnotify))))

(use-package auth-source
  :defer t
  :config
  (let ((authinfo (expand-file-name "authinfo.gpg" "~/Dropbox/doc")))
    (when (file-exists-p authinfo)
      (add-to-list 'auth-sources authinfo))))

(use-package avy
  :ensure t
  :bind (:map isearch-mode-map ("C-c '" . avy-isearch))
  :commands (avy-goto-char
             avy-goto-char-2
             avy-goto-word-1
             avy-goto-line
             avy-isearch))

(use-package exec-path-from-shell
  :ensure t
  :if (is-darwin-p)
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

(use-package grep
  :commands (grep find-grep find-grep-dired)
  :config
  (add-to-list 'grep-find-ignored-directories "target"))

(use-package hideshow
  :bind (("<f5>"     . ht/hs-toggle-hiding)
         ("M-<f5>"   . ht/hs-hide-all)
         ("M-S-<f5>" . ht/hs-show-all))
  :functions ht/hs-add-xml-mode
  :preface
  (defun ht/hs-minor-mode ()
    (when (not (bound-and-true-p hs-minor-mode))
      (hs-minor-mode)))
  (defun ht/hs-toggle-hiding ()
    (interactive)
    (ht/hs-minor-mode)
    (hs-toggle-hiding))
  (defun ht/hs-hide-all ()
    (interactive)
    (ht/hs-minor-mode)
    (hs-hide-all))
  (defun ht/hs-show-all ()
    (interactive)
    (ht/hs-minor-mode)
    (hs-show-all))
  :config
  (defun ht/hs-add-xml-mode (mode forward-sexp-func)
    (let ((start         "<!--\\|<[^/>]*[^/]>")
          (end           "-->\\|</[^/>]*[^/]>")
          (comment-start "<!--"))
      (push (list mode start end comment-start forward-sexp-func nil)
            hs-special-modes-alist)))
  (ht/hs-add-xml-mode 'nxml-mode 'nxml-forward-element)
  (ht/hs-add-xml-mode 'html-mode 'sgml-skip-tag-forward)
  (ht/hs-add-xml-mode 'sgml-mode 'sgml-skip-tag-forward))

(use-package hl-line-mode
  :commands hl-line-mode
  :init
  (defun ht/hl-line-mode ()
    (set-face-attribute 'hl-line nil :background "grey20"))
  (add-hook 'hl-line-mode-hook 'ht/hl-line-mode))

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
  (add-hook 'ido-minibuffer-setup-hook
            (defun ido-disable-line-truncation ()
              (set (make-local-variable 'truncate-lines) nil)))
  (defun ht/ido-define-keys ()
    (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
    (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
  (add-hook 'ido-setup-hook 'ht/ido-define-keys)
  (add-hook 'ido-setup-hook 'flx-ido-mode)
  (ido-mode t))

(use-package linum
  :init
  (use-package linum-relative :ensure t)
  (defun ht/linum-mode ()
    (setq linum-format "%4d ")
    (setq linum-relative-format "%4s ")
    (set-face-foreground 'linum "grey30")
    (set-face-foreground 'linum-relative-current-face "grey30")
    (set-face-background 'linum-relative-current-face (face-attribute 'default :background))
    (set-face-attribute 'linum-relative-current-face nil :weight 'normal)
    (linum-relative-on))
  (add-hook 'linum-mode-hook 'ht/linum-mode))

(use-package page-break-lines
  :ensure t
  :commands page-break-lines-mode
  :diminish page-break-lines-mode
  :config
  (setq page-break-lines-char ?-))

(use-package smex
  :ensure t
  :bind ("M-x" . smex)
  :config
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  (smex-initialize))

(use-package undo-tree
  :ensure t
  :commands undo-tree-mode
  :diminish undo-tree-mode)

(use-package uniquify
  :defer t
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package whitespace
  :commands whitespace-mode
  :diminish whitespace-mode
  :init
  (setq whitespace-style '(face tabs lines-tail trailing empty)
        whitespace-line-column 100)
  (ht/comment
    (defun ht/style-whitespace-mode ()
      (set-face-attribute 'whitespace-line nil
                          :foreground nil
                          :background "gray90"))
    (add-hook 'whitespace-mode-hook 'ht/style-whitespace-mode)))

(provide 'feature-general)
