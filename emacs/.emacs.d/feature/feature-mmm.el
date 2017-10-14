(use-package mmm-mode
  :ensure t
  :commands mmm-mode
  :config
  (mmm-add-group 'haskell-inline-c
                 '((inline-c
                    :submode c-mode
                    :face mmm-code-submode-face
                    :front "\\[C\\.block| \\w+ \{"
                    :back "\} |\\]"
                    :include-front nil
                    :include-back nil
                    ;; :front-offset 6
                    ;; :back-offset 2
                    )))
  (mmm-add-group 'html-js2
                 '((js-script-cdata
                    :submode js2-mode
                    :face mmm-code-submode-face
                    :front "<script[^>]*>[ \t\n]*\\(//\\)?<!\\[CDATA\\[[ \t]*\n?"
                    :back "[ \t]*\\(//\\)?]]>[ \t\n]*</script>")
                   (js-script
                    :submode js2-mode
                    :face mmm-code-submode-face
                    :front "<script[^>]*>[ \t]*\n?"
                    :back "[ \t]*</script>"
                    :insert ((?j js-tag nil @ "<script type=\"text/javascript\">\n"
                                 @ "" _ "" @ "\n</script>" @)))))
  (mmm-add-mode-ext-class 'haskell-mode nil 'haskell-inline-c)
  (mmm-add-mode-ext-class 'html-mode nil 'html-css)
  (mmm-add-mode-ext-class 'html-mode nil 'html-js2))

(provide 'feature-mmm)
