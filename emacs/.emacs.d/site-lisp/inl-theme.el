;;; inl-theme.el --- A minimal light theme  -*- lexical-binding: t; -*-

;; Author: Henry Till <henrytill@gmail.com>

;;; Commentary:

;; A port of inl.vim to Emacs.

;;; Code:

(deftheme inl
  "A minimal light theme ported from inl.vim.")

(let ((fg           "black")
      (bg           "#ffffff")
      (comment-fg   "#595959")
      (light-gray   "#bfbfbf")
      (folded-bg    "#f5f5f5")
      (visual-fg    "black")
      (visual-bg    "cyan")
      (error-fg     "red"))
  (custom-theme-set-faces
   'inl
   `(default ((t (:foreground ,fg :background ,bg))))
   `(cursor  ((t (:background ,fg))))

   ;; Chrome
   `(hl-line                    ((t (:background ,light-gray))))
   `(line-number                ((t (:foreground ,light-gray :background unspecified))))
   `(line-number-current-line   ((t (:inherit line-number))))
   `(fringe                     ((t (:foreground unspecified :background unspecified))))
   `(vertical-border            ((t (:foreground ,light-gray))))
   `(window-divider             ((t (:foreground ,light-gray))))
   `(window-divider-first-pixel ((t (:foreground ,light-gray))))
   `(window-divider-last-pixel  ((t (:foreground ,light-gray))))
   `(region                     ((t (:foreground ,visual-fg :background ,visual-bg))))
   `(error                      ((t (:foreground ,error-fg))))

   ;; Folded
   `(outline-1 ((t (:foreground unspecified :background ,folded-bg))))

   ;; NonText analogues
   `(escape-glyph        ((t (:foreground ,light-gray))))
   `(homoglyph           ((t (:foreground ,light-gray))))
   `(nobreak-space       ((t (:foreground ,light-gray :underline nil))))
   `(nobreak-hyphen      ((t (:foreground ,light-gray))))
   `(trailing-whitespace ((t (:foreground unspecified :background unspecified))))
   `(whitespace-trailing ((t (:foreground unspecified :background unspecified))))

   ;; Font-lock — clear so syntax has no extra color (Comment is the exception)
   `(font-lock-comment-face              ((t (:foreground ,comment-fg))))
   `(font-lock-comment-delimiter-face    ((t (:foreground ,comment-fg))))
   `(font-lock-doc-face                  ((t (:foreground ,comment-fg))))
   `(font-lock-doc-markup-face           ((t (:foreground ,comment-fg))))
   `(font-lock-builtin-face              ((t (:inherit default))))
   `(font-lock-constant-face             ((t (:inherit default))))
   `(font-lock-function-name-face        ((t (:inherit default))))
   `(font-lock-function-call-face        ((t (:inherit default))))
   `(font-lock-keyword-face              ((t (:inherit default))))
   `(font-lock-negation-char-face        ((t (:inherit default))))
   `(font-lock-number-face               ((t (:inherit default))))
   `(font-lock-operator-face             ((t (:inherit default))))
   `(font-lock-preprocessor-face         ((t (:inherit default))))
   `(font-lock-property-name-face        ((t (:inherit default))))
   `(font-lock-property-use-face         ((t (:inherit default))))
   `(font-lock-punctuation-face          ((t (:inherit default))))
   `(font-lock-regexp-face               ((t (:inherit default))))
   `(font-lock-regexp-grouping-backslash ((t (:inherit default))))
   `(font-lock-regexp-grouping-construct ((t (:inherit default))))
   `(font-lock-string-face               ((t (:inherit default))))
   `(font-lock-type-face                 ((t (:inherit default))))
   `(font-lock-variable-name-face        ((t (:inherit default))))
   `(font-lock-variable-use-face         ((t (:inherit default))))
   `(font-lock-warning-face              ((t (:inherit default))))

   ;; Eglot / LSP — clear (mirrors the vim @lsp* clearing loop)
   `(eglot-highlight-symbol-face           ((t (:inherit default))))
   `(eglot-mode-line                       ((t (:inherit mode-line))))
   `(eglot-inlay-hint-face                 ((t (:inherit default))))
   `(eglot-parameter-hint-face             ((t (:inherit default))))
   `(eglot-type-hint-face                  ((t (:inherit default))))
   `(eglot-diagnostic-tag-unnecessary-face ((t (:inherit default))))
   `(eglot-diagnostic-tag-deprecated-face  ((t (:inherit default :strike-through t))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'inl)

;;; inl-theme.el ends here
