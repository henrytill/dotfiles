;;; ht-prelude.el --- Henry Till's Emacs Prelude     -*- lexical-binding: t; -*-

(defun is-darwin-p ()
  (string-equal system-type "darwin"))

(defun is-linux-p ()
  (string-equal system-type "gnu/linux"))

(defun is-windows-p ()
  (or (string-equal system-type "windows-nt")
      (string-equal system-type "cygwin")))

(defun in-nix-shell-p ()
  (string-equal (getenv "IN_NIX_SHELL") "1"))

(defun expand-directory-name (dir &optional parent-dir)
  (file-name-as-directory (expand-file-name dir parent-dir)))

(defmacro ht/comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

(put 'ht/comment 'lisp-indent-function 'defun)

(defun ht/s-trim-left (s)
  "Remove whitespace at the beginning of S."
  (save-match-data
    (if (string-match "\\`[ \t\n\r]+" s)
        (replace-match "" t t s)
      s)))

(defun ht/s-trim-right (s)
  "Remove whitespace at the end of S."
  (save-match-data
    (if (string-match "[ \t\n\r]+\\'" s)
        (replace-match "" t t s)
      s)))

(defun ht/s-trim (s)
  "Remove whitespace at the beginning and end of S."
  (ht/s-trim-left (ht/s-trim-right s)))

(defun ht/replace-item-in-alist (alist key value)
  (cons `(,key . ,value) (assq-delete-all key alist)))

(defun ht/hostname ()
  (when (executable-find "hostname")
    (ht/s-trim (shell-command-to-string "hostname -s"))))

;;; https://www.emacswiki.org/emacs/ElispCookbook
(defun ht/list-subdirs (dir exclude)
  "Find all directories in DIR."
  (unless (file-directory-p dir)
    (error "Not a directory `%s'" dir))
  (let ((dir   (directory-file-name dir))
        (files (directory-files dir nil nil t))
        (dirs  '()))
    (dolist (file files)
      (unless (member file (append '("." "..") exclude))
        (let ((file (concat (file-name-as-directory dir) file)))
          (when (file-directory-p file)
            (setq dirs (append (cons file (ht/list-subdirs file exclude)) dirs))))))
    dirs))

(setq apropos-do-all t
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      custom-file (expand-file-name "custom.el" user-emacs-directory)
      epa-armor t
      gnutls-min-prime-bits 1024
      ido-handle-duplicate-virtual-buffers 2
      ido-use-virtual-buffers t
      inhibit-startup-message t
      initial-scratch-message nil
      load-prefer-newer t
      mouse-yank-at-point t
      require-final-newline t
      ring-bell-function 'ignore
      save-interprogram-paste-before-kill t
      save-place-file (concat user-emacs-directory "places")
      scroll-conservatively 1
      visible-bell t
      x-select-enable-primary t
      x-select-enable-clipboard t)

(setq-default fill-column 80
              indent-tabs-mode nil
              ispell-program-name "aspell")

(when (is-darwin-p)
  (setq browse-url-browser-function 'browse-url-default-browser
        dired-use-ls-dired nil))

(load custom-file t)

(put 'dired-find-alternate-file 'disabled nil)

(defconst ht/global-bindings
  '(("M-/"     . hippie-expand)
    ("C-x C-b" . ibuffer)
    ("C-s"     . isearch-forward-regexp)
    ("C-r"     . isearch-backward-regexp)
    ("C-M-s"   . isearch-forward)
    ("C-M-r"   . isearch-backward)
    ("M-%"     . query-replace-regexp)
    ("C-M-%"   . query-replace)))

(dolist (binding ht/global-bindings)
  (let ((key (car binding))
        (cmd (cdr binding)))
    (global-set-key (kbd key) cmd)))

(provide 'ht-prelude)
;;; ht-prelude.el ends here
