;;;; Machine-specific settings

;;; Darwin machines
(when (and (is-darwin-p) (window-system))
  (if (file-exists-p (expand-file-name "SourceCodePro-Regular.ttf" "~/Library/Fonts"))
      (set-default-font "Source Code Pro 13")
    (set-default-font "Menlo-12"))
  (invert-face 'default))
