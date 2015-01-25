;;; REPL-style prompt
(setq eshell-prompt-function
      (lambda nil
        (concat "\n"
                (user-login-name) "@"
                (abbreviate-file-name (eshell/pwd)) "> "))
      eshell-prompt-regexp "^[^>]*> ")

;;; Cyan prompt
(add-hook 'eshell-prompt-load-hook
          (defun my-color-eshell-prompt ()
            (set-face-foreground 'eshell-prompt "#2aa198")))

;;; Visual Commands
(add-hook 'eshell-mode-hook
          (defun my-eshell-visual-commands ()
            (add-to-list 'eshell-visual-commands "ssh")
            (add-to-list 'eshell-visual-commands "bash")))

(defun eshell-term ()
  (interactive)
  (eshell)
  (setq-local mode-line-format nil))

;;; Functions
(defun eshell/clear ()
  "Clear the Eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun eshell/rgrep (&rest args)
  "Use Emacs grep facility instead of calling external grep."
  (eshell-grep "rgrep" args t))

(defun eshell/scheme ()
  (interactive)
  (call-interactively 'run-scheme))

(when (file-exists-p "~/bin/rc-shell")
  (defun eshell/rc ()
    (interactive)
    (term "~/bin/rc-shell")
    (rename-buffer "*rc*")))

(defalias 'eshell/view 'view-file)
