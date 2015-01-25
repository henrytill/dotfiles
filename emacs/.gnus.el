(setq gnus-select-method '(nnnil "")
      gnus-directory "~/.emacs.d/news/"
      gnus-home-directory "~/.emacs.d/"
      gnus-dribble-directory "~/.emacs.d/"
      gnus-kill-files-directory "~/.emacs.d/news/"
      gnus-always-dribble-file t
      user-mail-address "henrytill@gmail.com"
      user-full-name "Henry Till"
      gnus-ignored-from-addresses "Henry Till"

      mail-source-directory "~/.emacs.d/mail"
      message-directory "~/.emacs.d/mail"

      message-kill-buffer-on-exit t
      gnus-treat-display-smileys nil
      gnus-fetch-old-headers 'some
      nnmail-crosspost nil
      mail-source-delete-incoming nil
      gnus-gcc-mark-as-read t

      send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "henrytill@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service  587
      
      gnus-asynchronous t
      gnus-agent-expire-days 0
      gnus-agent-synchronize-flags t
      gnus-agent-enable-expiration 'DISABLE
      gnus-secondary-select-methods 
      '((nntp "gmane" (nntp-address "news.gmane.org"))
        (nntp "eternal-september" (nntp-address "news.eternal-september.org"))))

(when (file-directory-p "~/mail/")
  (add-to-list 'gnus-secondary-select-methods
               '(nnmaildir "GMail"
                           (directory "~/mail/henrytill@gmail.com/")
                           (directory-files nnheader-directory-files-safe)
                           (get-new-mail nil))))

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(gnus-demon-add-handler 'gnus-group-get-new-news 10 t)
(gnus-demon-init)
      
(setq gnus-use-full-window nil)
