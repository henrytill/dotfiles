;;;; Gnus

(setq gnus-agent-enable-expiration 'DISABLE
      gnus-agent-expire-days 0
      gnus-agent-synchronize-flags t
      gnus-always-dribble-file t
      gnus-asynchronous t
      gnus-directory (concat user-emacs-directory "news")
      gnus-dribble-directory user-emacs-directory
      gnus-fetch-old-headers 'some
      gnus-gcc-mark-as-read t
      gnus-home-directory user-emacs-directory
      gnus-ignored-from-addresses "Henry Till"
      gnus-kill-files-directory (concat user-emacs-directory "news")
      gnus-message-archive-group "sent"
      gnus-select-method '(nnnil "")
      gnus-treat-display-smileys nil
      gnus-use-full-window nil
      mail-source-delete-incoming nil
      mail-source-directory (concat user-emacs-directory "mail")
      message-directory (concat user-emacs-directory "mail")
      message-kill-buffer-on-exit t
      nnmail-crosspost nil
      user-full-name "Henry Till"
      user-mail-address "henrytill@gmail.com"
      send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

(setq gnus-secondary-select-methods
      '((nnimap "henrytill@gmail.com"
                (nnimap-address "imap.gmail.com")
                (nnimap-server-port 993)
                (nnimap-stream ssl))
        (nnimap "ht@xngns.net"
                (nnimap-address "mail.xngns.net")
                (nnimap-server-port 993)
                (nnimap-stream ssl))
        (nnimap "admin@xngns.net"
                (nnimap-address "mail.xngns.net")
                (nnimap-server-port 993)
                (nnimap-stream ssl))
        (nntp "gmane" (nntp-address "news.gmane.org"))
        (nntp "eternal-september" (nntp-address "news.eternal-september.org"))))

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(gnus-demon-add-handler 'gnus-group-get-new-news 10 t)
(gnus-demon-init)
