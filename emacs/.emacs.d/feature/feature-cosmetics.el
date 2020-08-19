(defun ht/style-fringe ()
  (set-face-foreground 'fringe (face-attribute 'default :foreground))
  (set-face-background 'fringe (face-attribute 'default :background)))

(defun ht/reset-frame ()
  (interactive)
  (let ((height (cdr (assq 'height default-frame-alist)))
        (width  (cdr (assq 'width  default-frame-alist))))
    (set-frame-height (selected-frame) height)
    (set-frame-width  (selected-frame) width)))

(defun ht/double-width-frame ()
  (interactive)
  (let ((height (cdr (assq 'height default-frame-alist)))
        (width  (cdr (assq 'width  default-frame-alist))))
    (set-frame-height (selected-frame) height)
    (set-frame-width  (selected-frame) (* 2 width))))

(defun ht/adjust-frame ()
  (add-to-list 'default-frame-alist '(internal-border-width . 14))
  (add-to-list 'default-frame-alist '(height . 60))
  (add-to-list 'default-frame-alist '(width . 100)))

(defun ht/style-mode-line ()
  (set-face-attribute 'mode-line
                      nil
                      :box `(:line-width 3 :color ,(face-attribute 'mode-line :background)))
  (set-face-attribute 'mode-line-inactive
                      nil
                      :box `(:line-width 3 :color ,(face-attribute 'mode-line-inactive :background))))


(defun ht/frame-setup (frame)
  (with-selected-frame frame
    (when (display-graphic-p frame)
      (ht/style-fringe)
      (ht/style-mode-line))))

(unless (and (is-linux-p) (window-system))
  (menu-bar-mode -1)
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (tool-bar-mode -1))

(setq frame-background-mode 'light)

(when (is-linux-p)
  (add-hook 'after-make-frame-functions 'ht/frame-setup))

(when (and (is-linux-p) (string-equal (window-system) "x"))
  (set-face-attribute 'region nil :background "lightgoldenrod2")
  (ht/style-fringe))

(when (is-darwin-p)
  (add-hook 'after-make-frame-functions 'ht/frame-setup))

(when (and (is-darwin-p) (string-equal (window-system) "ns"))
  (ht/adjust-frame)
  (ht/style-fringe)
  (when (member "Fira Mono" (font-family-list))
    (set-frame-font "Fira Mono-12" nil t)))

(when (and (is-windows-p) (window-system))
  (ht/adjust-frame)
  (ht/style-fringe)
  (when (member "Consolas" (font-family-list))
    (set-face-attribute 'default nil
                        :family "Consolas"
                        :foundry 'outline
                        :width 'normal
                        :height 98)))

(use-package linum
  :if (version< emacs-version "26.1")
  :after (prog-mode)
  :init
  (setq linum-format "%4d ")
  (add-hook 'prog-mode-hook 'linum-on))

(use-package display-line-numbers
  :if (version<= "26.1" emacs-version)
  :after (prog-mode)
  :init
  (setq display-line-numbers-width 4)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

(setq frame-title-format
      '("" invocation-name ": " (:eval (if (buffer-file-name)
                                           (abbreviate-file-name (buffer-file-name))
                                         "%b"))))

(ht/style-mode-line)

(show-paren-mode 1)
(column-number-mode 1)

;; cursor
(blink-cursor-mode 0)
(setq visible-cursor nil)
(setq-default cursor-type 'box)

;;; truncate lines
(defun ht/truncate-lines ()
  (setq truncate-lines t))

(add-hook 'compilation-mode-hook     'ht/truncate-lines)
(add-hook 'dired-mode-hook           'ht/truncate-lines)
(add-hook 'prog-mode-hook            'ht/truncate-lines)
(add-hook 'rtags-mode-hook           'ht/truncate-lines)
(add-hook 'shell-mode-hook           'ht/truncate-lines)
(add-hook 'sql-interactive-mode-hook 'ht/truncate-lines)

;;; warning keywords
(defun ht/add-watchwords ()
  (font-lock-add-keywords nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\)" 1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'ht/add-watchwords)

(provide 'feature-cosmetics)
