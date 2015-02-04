;;;; Machine-specific settings

;;; Darwin machines
(when (and (is-darwin-p) (window-system))
  (when (file-exists-p (expand-file-name "SourceCodePro-Light.ttf" "~/Library/Fonts"))
    (set-default-font "Source Code Pro Light 13"))
  (invert-face 'default)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) 100 48)
  (server-mode 1))
