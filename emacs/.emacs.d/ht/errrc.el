;;;; ERC

(setq erc-fill-function 'erc-fill-static
      erc-fill-static-center 19
      erc-hide-list '("JOIN" "PART" "QUIT")
      erc-prompt ">"
      erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))

(eval-after-load 'erc
  '(progn
     (when (not (package-installed-p 'erc-hl-nicks))
       (package-install 'erc-hl-nicks))
     (require 'erc-hl-nicks)
     (require 'erc-spelling)
     (add-to-list 'erc-modules 'hl-nicks)
     (add-to-list 'erc-modules 'spelling)))
