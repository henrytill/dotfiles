(defun org-babel-scheme-execute-with-geiser (code output impl repl)
  "Execute code in specified REPL. If the REPL doesn't exist, create it
using the given scheme implementation.

Returns the output of executing the code if the output parameter
is true; otherwise returns the last value."
  (let ((result nil))
    (with-temp-buffer
      (insert (format ";; -*- geiser-scheme-implementation: %s -*-" impl))
      (newline)
      (insert (if output
                  (format "(with-output-to-string (lambda () %s))" code)
                code))
      (geiser-mode)
      (let ((repl-buffer (save-current-buffer
                           (org-babel-scheme-get-repl impl repl))))
        (when (not (eq impl (org-babel-scheme-get-buffer-impl
                             (current-buffer))))
          (message "Implementation mismatch: %s (%s) %s (%s)" impl (symbolp impl)
                   (org-babel-scheme-get-buffer-impl (current-buffer))
                   (symbolp (org-babel-scheme-get-buffer-impl
                             (current-buffer)))))
        (setq geiser-repl--repl repl-buffer)
        (setq geiser-impl--implementation nil)
        (setq result (org-babel-scheme-capture-current-message
                      (geiser-eval-region (point-min) (point-max))))
        (setq result
              (if (and (stringp result) (equal (substring result 0 3) "=> "))
                  (replace-regexp-in-string "^=> " "" result)
                "\"An error occurred.\""))
        (when (not repl)
          (save-current-buffer (set-buffer repl-buffer)
                               (geiser-repl-exit))
          (set-process-query-on-exit-flag (get-buffer-process repl-buffer) nil)
          (kill-buffer repl-buffer))
        ;; (setq result (if (or (string= result "#<void>")
        ;;                      (string= result "#<unspecified>"))
        ;;                  nil
        ;;                (read result)))
        ))
    result))
