;;;; Key Bindings

;;; Org
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cb" 'org-iswitchb)

(when (file-directory-p org-directory)
  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-ca" 'org-agenda))

;;; hs-minor-mode
(require 'hideshow)
(define-key hs-minor-mode-map (kbd "<f5>")     'hs-toggle-hiding)
(define-key hs-minor-mode-map (kbd "M-<f5>")   'hs-hide-all)
(define-key hs-minor-mode-map (kbd "M-S-<f5>") 'hs-show-all)
