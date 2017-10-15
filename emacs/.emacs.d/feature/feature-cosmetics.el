(unless (and (is-linux-p) (window-system))
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1))

(setq frame-background-mode 'light)

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

(defun ht/frame-setup (frame)
  (with-selected-frame frame
    (when (display-graphic-p frame)
      (set-face-foreground 'fringe (face-attribute 'default :foreground))
      (set-face-background 'fringe (face-attribute 'default :background)))))

(when (is-linux-p)
  (add-hook 'after-make-frame-functions 'ht/frame-setup))

(when (and (is-linux-p) (string-equal (window-system) "x"))
  (set-face-foreground 'fringe (face-attribute 'default :foreground))
  (set-face-background 'fringe (face-attribute 'default :background)))

(when (is-darwin-p)
  (add-to-list 'default-frame-alist '(height . 60))
  (add-to-list 'default-frame-alist '(width . 100))
  (add-hook 'after-make-frame-functions 'ht/frame-setup))

(when (and (is-darwin-p) (string-equal (window-system) "ns"))
  (add-to-list 'default-frame-alist '(internal-border-width . 14))
  (set-face-foreground 'fringe (face-attribute 'default :foreground))
  (set-face-background 'fringe (face-attribute 'default :background))
  (when (member "Fira Mono" (font-family-list))
    (set-face-attribute 'default nil :font "Fira Mono 12")
    (setq-default line-spacing 2)))

(when (and (is-windows-p) (window-system))
  (add-to-list 'default-frame-alist '(internal-border-width . 14))
  (add-to-list 'default-frame-alist '(height . 60))
  (add-to-list 'default-frame-alist '(width . 100))
  (set-face-foreground 'fringe (face-attribute 'default :foreground))
  (set-face-background 'fringe (face-attribute 'default :background))
  (when (member "Consolas" (font-family-list))
    (set-face-attribute 'default nil
                        :family "Consolas"
                        :foundry 'outline
                        :width 'normal
                        :height 98)))

(when (window-system)
  (set-face-attribute 'region nil :background "lightgoldenrod2")
  (set-face-attribute 'region nil :foreground "black"))

(defconst ht/fixed-font
  (cond
   ((is-linux-p)  '(:font "Go Mono"))
   ((is-darwin-p) '(:font "Go Mono"))))

(defconst ht/variable-font
  (cond
   ((is-linux-p)  '(:font "Go"))
   ((is-darwin-p) '(:font "Go"))))

(defun ht/custom-set-faces ()
  (custom-set-faces `(fixed-pitch    ((t ,ht/fixed-font)))
                    `(variable-pitch ((t ,ht/variable-font)))))

(ht/custom-set-faces)

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

(show-paren-mode 1)

(setq frame-title-format
      '("" invocation-name ": "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(set-face-attribute 'mode-line          nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)
(set-face-attribute 'header-line        nil :box nil)

(column-number-mode 1)

(blink-cursor-mode 0)
(setq visible-cursor nil)
(setq-default cursor-type 'box)

;;; hl-line
(defun ht/select-line-mode ()
  (hl-line-mode 1)
  (set-face-attribute 'hl-line nil :background "lightgoldenrod2")
  (set-face-attribute 'hl-line nil :foreground "black")
  (setq cursor-type nil))

(add-hook 'dired-mode-hook        'ht/select-line-mode)
(add-hook 'ibuffer-mode-hook      'ht/select-line-mode)
(add-hook 'gnus-group-mode-hook   'ht/select-line-mode)
(add-hook 'gnus-summary-mode-hook 'ht/select-line-mode)
(add-hook 'gnus-server-mode-hook  'ht/select-line-mode)
(add-hook 'package-menu-mode-hook 'ht/select-line-mode)

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
