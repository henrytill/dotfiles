;;;; my-themes.el
;;;;
;;;; a WIP

(defvar my-themes-alist nil)
(defvar my-themes-current-theme nil)

(setq my-themes-alist
      '((solarized-light . ((default-background-color . "#fdf6e3")
                            (fringe-background-color . "#fdf6e3")
                            ;; (hl-line-background-color . "#2aa198")
                            ;; (hl-line-foreground-color . "#eee8d5")
                            (mode-line-outline-color . "#eee8d5")
                            (mode-line-inactive-background-color . "#eee8d5")
                            (terminal-default-background-color . "unspecified-bg")
                            (terminal-mode-line-inactive-background-color . "#eee8d5")
                            (font-lock-builtin-face-slant . normal)
                            (font-lock-keyword-face-weight . normal)
                            (font-lock-type-face-weight . normal)))
        (solarized-dark  . ((default-background-color . "#002b36")
                            (fringe-background-color . "#002b36")
                            (mode-line-outline-color . "#586e75")
                            (mode-line-inactive-background-color . "#002b36")
                            (terminal-default-background-color . "unspecified-bg")
                            (terminal-mode-line-inactive-background-color . "#073642")
                            (font-lock-builtin-face-slant . normal)
                            (font-lock-keyword-face-weight . normal)
                            (font-lock-type-face-weight . normal)))))

;; (setq my-themes-current-theme 'solarized-light)

(defun my-themes-get-param (theme param)
  (cdr (assoc param (cdr (assoc theme my-themes-alist)))))

(defun my-themes-customizations (theme &optional frame)
  (when (member theme (mapcar (lambda (element) (car element))
                              my-themes-alist))
    (let* ((bgcol (my-themes-get-param theme 'default-background-color))
           (fbcol (my-themes-get-param theme 'fringe-background-color))
           (hlcol (my-themes-get-param theme 'hl-line-background-color))
           (hfcol (my-themes-get-param theme 'hl-line-foreground-color))
           (olcol (my-themes-get-param theme 'mode-line-outline-color))
           (iacol (my-themes-get-param theme 'mode-line-inactive-background-color))
           (flbfs (my-themes-get-param theme 'font-lock-builtin-face-slant))
           (flkfw (my-themes-get-param theme 'font-lock-keyword-face-weight))
           (fltfw (my-themes-get-param theme 'font-lock-type-face-weight))
           (boxen `(:line-width 1 :color ,olcol)))
      (when bgcol
        (set-face-attribute 'default frame :background bgcol))
      (when fbcol
        (set-face-attribute 'fringe frame :background fbcol))
      (when hlcol
        (set-face-attribute 'hl-line frame :background hlcol))
      (when hfcol
        (set-face-attribute 'hl-line frame :foreground hfcol))
      (when olcol
        (if iacol
            (set-face-attribute 'mode-line-inactive frame :box boxen :background iacol)
          (set-face-attribute 'mode-line-inactive frame :box boxen))
        (set-face-attribute 'header-line frame :box boxen)
        (set-face-attribute 'mode-line frame :box boxen))
      (when flbfs
        (set-face-attribute 'font-lock-builtin-face frame :slant flbfs))
      (when flkfw
        (set-face-attribute 'font-lock-keyword-face frame :weight flkfw))
      (when fltfw
        (set-face-attribute 'font-lock-type-face frame :weight fltfw)))))

(defun my-themes-terminal-customizations (theme &optional frame)
  (when (member theme (mapcar (lambda (element) (car element))
                              my-themes-alist))
    (let ((iacol (my-themes-get-param theme 'terminal-mode-line-inactive-background-color))
          (bgcol (my-themes-get-param theme 'terminal-default-background-color)))
      (when iacol
        (set-face-attribute 'mode-line-inactive frame :background iacol))
      (when bgcol
        (set-face-attribute 'default frame :background bgcol)))))

(defun my-themes-frame-loader (frame)
  ;; (select-frame frame)
  (my-themes-customizations my-themes-current-theme frame)
  (unless (display-graphic-p frame)
    (my-themes-terminal-customizations my-themes-current-theme frame)))

(defun my-load-theme (theme &optional no-confirm no-enable)
  (interactive
   (list (intern (completing-read "Load custom theme: "
                                  (mapcar (lambda (element) (car element))
                                          my-themes-alist)))
         nil nil))
  (load-theme theme no-confirm no-enable)
  (when (not (eq my-themes-current-theme theme))
    (setq my-themes-current-theme theme))
  (my-themes-frame-loader (selected-frame)))

(defun my-themes-loader ()
  (my-load-theme my-themes-current-theme t))

(add-hook 'window-setup-hook 'my-themes-loader)
(add-hook 'after-make-frame-functions 'my-themes-frame-loader)

(provide 'my-themes)
