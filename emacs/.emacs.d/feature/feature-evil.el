(defun ht/other-window ()
  (interactive)
  (other-window 1))

(defconst ht/evil-emacs-state-modes
  '(cider-repl-mode
    cider-stacktrace-mode
    flycheck-error-list-mode
    geiser-repl-mode
    haskell-error-mode
    haskell-interactive-mode
    idris-repl-mode
    inferior-caml-mode
    inferior-emacs-lisp-mode
    inferior-forth-mode
    inferior-haskell-mode
    inferior-python-mode
    inferior-scheme-mode
    inferior-sml-mode
    ocamldebug-mode
    prolog-inferior-mode
    sbt-mode
    shell-mode
    slime-repl-mode
    term-mode
    tuareg-interactive-mode
    utop-mode
    xref--xref-buffer-mode))

(defun ht/setup-evil-emacs-state-modes ()
  (dolist (mode ht/evil-emacs-state-modes)
    (progn (when (member mode evil-insert-state-modes)
             (delete mode evil-insert-state-modes))
           (when (member mode evil-normal-state-modes)
             (delete mode evil-normal-state-modes))
           (add-to-list 'evil-emacs-state-modes mode))))

(defconst ht/evil-emacs-state-bindings
  '(("C-w C-w" . ht/other-window)
    ("C-w s"   . split-window-below)
    ("C-w v"   . split-window-right)
    ("C-w o"   . delete-other-windows)
    ("C-w c"   . delete-window)
    ("C-w q"   . ido-kill-buffer)
    ("C-o"     . evil-execute-in-normal-state)))

(defconst ht/evil-normal-state-bindings
  '(("C-w C-]" . find-tag-other-window)
    ("g x"     . browse-url-at-point)))

(defun ht/setup-evil-bindings ()
  (dolist (binding ht/evil-emacs-state-bindings)
    (let ((key (car binding))
          (cmd (cdr binding)))
      (define-key evil-emacs-state-map (kbd key) cmd)))
  (dolist (binding ht/evil-normal-state-bindings)
    (let ((key (car binding))
          (cmd (cdr binding)))
      (define-key evil-normal-state-map (kbd key) cmd))))

(defconst evil-paredit-state-bindings
  '(("j"     . paredit-forward)
    ("k"     . paredit-backward)
    ("h"     . paredit-backward-up)
    ("l"     . paredit-forward-down)
    ("C-b"   . paredit-backward-down)
    ("C-f"   . paredit-forward-up)
    ("J"     . evil-next-line)
    ("K"     . evil-previous-line)
    ("H"     . evil-backward-char)
    ("L"     . evil-forward-char)
    ("M-r"   . paredit-raise-sexp)
    ("M-c"   . paredit-convolute-sexp)
    (")"     . paredit-forward-slurp-sexp)
    ("}"     . paredit-forward-barf-sexp)
    ("("     . paredit-backward-slurp-sexp)
    ("{"     . paredit-backward-barf-sexp)
    ("C-d"   . paredit-forward-delete)
    ("DEL"   . paredit-backward-delete)
    ("M-d"   . paredit-forward-kill-word)
    ("M-DEL" . paredit-backward-kill-word)
    ("M-j"   . paredit-splice-sexp-killing-forward)
    ("M-k"   . paredit-splice-sexp-killing-backward)
    ("C-o"   . evil-execute-in-normal-state)))

(defun ht/setup-evil-paredit-state ()
  (evil-define-state paredit "Paredit state." :tag " <PAR> "
    :enable (paredit normal)
    :intercept-esc nil)
  (dolist (binding evil-paredit-state-bindings)
    (let ((key (car binding))
          (cmd (cdr binding)))
      (define-key evil-paredit-state-map (kbd key) cmd))))

(defun ht/setup-evil-ex-commands ()
  (evil-define-command cfile     () (flycheck-buffer))
  (evil-define-command tnext     () (find-tag nil t))
  (evil-define-command tprevious () (find-tag nil '-))
  (evil-ex-define-cmd "cf[ile]"     'cfile)
  (evil-ex-define-cmd "tn[ext]"     'tnext)
  (evil-ex-define-cmd "tp[revious]" 'tprevious))

(use-package hydra
  :ensure t)

(defvar ht/hydra-mode-specific nil "mode-specific hydra")

(defun ht/call-hydra-mode-specific ()
  (interactive)
  (if (boundp 'ht/hydra-mode-specific)
      (funcall ht/hydra-mode-specific)
    (message "no mode-specific hydra specified")))

(defhydra ht/hydra-base (:idle 1.0)
  "
base
----
_w_: ace-window           _p_: projectile    _!_: shell-command
_x_: counsel-M-x          _a_: avy           _&_: async-shell-command
_b_: ivy-switch-buffer    _g_: magit
_f_: counsel-find-file    _c_: flycheck
_k_: kill-buffer          _m_: mode-specific
_l_: evil-paredit-state
"
  ("w" ace-window                  nil :exit t)
  ("x" counsel-M-x                 nil :exit t)
  ("b" ivy-switch-buffer           nil :exit t)
  ("f" counsel-find-file           nil :exit t)
  ("k" kill-buffer                 nil :exit t)
  ("l" evil-paredit-state          nil :exit t)
  ("p" ht/hydra-projectile/body    nil :exit t)
  ("a" ht/hydra-avy/body           nil :exit t)
  ("g" ht/hydra-magit/body         nil :exit t)
  ("c" ht/hydra-flycheck/body      nil :exit t)
  ("m" ht/call-hydra-mode-specific nil :exit t)
  ("!" shell-command               nil :exit t)
  ("&" async-shell-command         nil :exit t))

(use-package avy
  :ensure t
  :bind (:map isearch-mode-map ("C-c '" . avy-isearch))
  :commands (avy-goto-char
             avy-goto-char-2
             avy-goto-word-1
             avy-goto-line
             avy-isearch))

(defhydra ht/hydra-avy (:idle 1.0)
  "
avy
---
_;_: evil-avy-goto-char
_'_: evil-avy-goto-char-2
_w_: evil-avy-goto-word-1
_l_: evil-avy-goto-line
"
  (";" evil-avy-goto-char   nil :exit t)
  ("'" evil-avy-goto-char-2 nil :exit t)
  ("w" evil-avy-goto-word-1 nil :exit t)
  ("l" evil-avy-goto-line   nil :exit t))

(with-eval-after-load 'compile
  (define-key compilation-mode-map (kbd "SPC") nil))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "SPC") nil))

(with-eval-after-load 'magit
  (define-key magit-mode-map (kbd "SPC") nil)
  (define-key magit-mode-map (kbd "x") nil))

(use-package evil
  :ensure t
  :init
  (setq evil-want-abbrev-expand-on-insert-exit nil)
  :config
  (use-package evil-surround
    :ensure t
    :commands global-evil-surround-mode)
  (ht/setup-evil-emacs-state-modes)
  (ht/setup-evil-bindings)
  (ht/setup-evil-paredit-state)
  (ht/setup-evil-ex-commands)
  (define-key evil-normal-state-map (kbd "SPC") 'ht/hydra-base/body)
  (define-key evil-visual-state-map (kbd "SPC") 'ht/hydra-base/body)
  (define-key evil-emacs-state-map  (kbd "M-m") 'ht/hydra-base/body)
  (evil-mode 1)
  (global-evil-surround-mode 1))

(provide 'feature-evil)
