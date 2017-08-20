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
    utop-mode))

(defun ht/setup-evil-emacs-state-modes ()
  (dolist (mode ht/evil-emacs-state-modes)
    (progn (when (member mode evil-insert-state-modes)
             (delete mode evil-insert-state-modes))
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

(use-package evil
  :ensure t
  :config
  (use-package evil-surround
    :ensure t
    :commands global-evil-surround-mode)
  (ht/setup-evil-emacs-state-modes)
  (ht/setup-evil-bindings)
  (ht/setup-evil-paredit-state)
  (ht/setup-evil-ex-commands)
  (evil-mode 1)
  (global-evil-surround-mode 1))

(use-package bind-map
  :ensure t
  :config
  (with-eval-after-load 'compile
    (define-key compilation-mode-map (kbd "SPC") nil))
  (bind-map ht/base-leader-map
    :keys ("M-m")
    :evil-keys ("SPC")
    :evil-states (motion normal visual paredit))
  (bind-map-set-keys ht/base-leader-map
    "]" 'forward-page
    "[" 'backward-page
    "w" 'ace-window
    "x" 'smex
    "l" 'evil-paredit-state)
  (ht/comment
    (bind-map ht/avy-leader-map
      :keys ("M-m g")
      :evil-keys ("SPC g"))
    (bind-map-set-keys ht/avy-leader-map
      ";" 'evil-avy-goto-char
      "'" 'evil-avy-goto-char-2
      "w" 'evil-avy-goto-word-1
      "l" 'evil-avy-goto-line)
    (bind-map ht/flycheck-leader-map
      :keys ("M-m f")
      :evil-keys ("SPC f"))
    (bind-map-set-keys ht/flycheck-leader-map
      "l" 'flycheck-list-errors
      "j" 'flycheck-next-error
      "k" 'flycheck-previous-error)
    nil))

(provide 'feature-evil)
