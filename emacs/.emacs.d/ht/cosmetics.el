;;; Frame Titles
(setq frame-title-format
      '("" invocation-name ": "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;;; Mode Line
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)
(set-face-attribute 'header-line nil :box nil)

;;; Column Numbers
(column-number-mode 1)

;;; Diminish
(eval-after-load 'company '(diminish 'company-mode))
(eval-after-load 'eldoc '(diminish 'eldoc-mode))
(eval-after-load 'page-break-lines '(diminish 'page-break-lines-mode))
(eval-after-load 'paredit '(diminish 'paredit-mode))

;;; Display ido results vertically, rather than horizontally
(setq ido-decorations '("\n-> " "" "\n   " "\n   ..." "[" "]"
                        " [No match]" " [Matched]" " [Not readable]"
                        " [Too big]" " [Confirm]"))

(add-hook 'ido-minibuffer-setup-hook
          (defun ido-disable-line-truncation ()
            (set (make-local-variable 'truncate-lines) nil)))

(defun jf-ido-define-keys () ;; C-n/p is more intuitive in vertical layout
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))

(add-hook 'ido-setup-hook 'jf-ido-define-keys)

;;; Cursor
(setq visible-cursor nil)
(setq-default cursor-type 'box)
(blink-cursor-mode 0)

;;; hl-line
(defun select-line-mode ()
  (hl-line-mode 1)
  (setq cursor-type 'nil))

(add-hook 'dired-mode-hook 'select-line-mode)
(add-hook 'ibuffer-mode-hook 'select-line-mode)
(add-hook 'gnus-group-mode-hook 'select-line-mode)
(add-hook 'gnus-summary-mode-hook 'select-line-mode)
(add-hook 'gnus-server-mode-hook 'select-line-mode)
(add-hook 'package-menu-mode-hook 'select-line-mode)

;;; Whitespace Handling
(setq whitespace-style '(face tabs lines-tail trailing empty))
(setq whitespace-line-column 80)

;;; Prettify Symbols
(when (and (>= emacs-major-version 24)
           (>= emacs-minor-version 4))
  (global-prettify-symbols-mode)
  (defun scheme-prettify-symbols ()
    (push '("lambda" . "\u03bb") prettify-symbols-alist))
  (add-hook 'scheme-mode-hook 'scheme-prettify-symbols))

;;; paren-face
(setq paren-face-regexp "[][(){}]")
(global-paren-face-mode)
