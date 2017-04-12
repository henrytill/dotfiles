(use-package notmuch
  :if (locate-library "notmuch")
  :commands notmuch)

(provide 'feature-notmuch)
