(when (file-directory-p org-directory)
  (set-register ?n `(file . ,(expand-file-name "notes.org" org-directory))))
