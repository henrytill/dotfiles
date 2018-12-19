;;; init.el

(defconst emacs-start-time (current-time))

(defun ht/elapsed-msg ()
  (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
    (message "Load time: %.3fs"  elapsed)))

(add-hook 'after-init-hook 'ht/elapsed-msg)

(eval-and-compile
  (mapc #'(lambda (path)
            (push (expand-file-name path user-emacs-directory) load-path))
        '("feature" "site-lisp" "site-lisp/use-package"))
  (mapc #'(lambda (path)
            (when (file-directory-p path)
              (push (expand-file-name path) load-path)))
        '("/usr/local/share/emacs/site-lisp" "~/.nix-profile/share/emacs/site-lisp/")))

(require 'ht-prelude)
(require 'use-package)
(require 'bind-key)
(require 'diminish "diminish-0.44.el")
(require 'package)

(dolist (archive '(("melpa" . "https://melpa.org/packages/")
                   ("org"   . "http://orgmode.org/elpa/")))
  (add-to-list 'package-archives archive))

(package-initialize)

(when (null package-archive-contents)
  (package-refresh-contents))

(setq use-package-verbose t)

(setq ht/features '(feature-cosmetics
                    feature-general
                    feature-ivy
                    feature-paredit
                    feature-evil
                    ;; general modes
                    feature-align
                    feature-company
                    feature-compile
                    feature-dired
                    feature-erc
                    feature-flycheck
                    feature-ido
                    feature-lsp
                    feature-magit
                    feature-mmm
                    feature-misc-editing
                    feature-notmuch
                    feature-org
                    feature-prog-mode
                    feature-projectile
                    feature-shell
                    feature-tex
                    feature-yasnippet
                    feature-whitespace
                    ;; language-specific modes
                    feature-agda
                    feature-ats
                    feature-c-cpp
                    feature-coq
                    feature-forth
                    feature-fsharp
                    feature-haskell
                    feature-idris
                    feature-javascript
                    feature-lisp
                    feature-nix
                    feature-ocaml
                    feature-oz
                    feature-purescript
                    feature-rust
                    feature-scala
                    feature-sml
                    feature-terraform
                    feature-typescript
                    ;; the "postlude"
                    feature-postlude))

(dolist (feature ht/features)
  (require feature nil t))

(set-register ?i `(file . ,(concat user-emacs-directory "init.el")))
(set-register ?f `(file . ,(concat user-emacs-directory "feature")))
