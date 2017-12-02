# defined hosts
MY_HOSTS = glaucus nereus thalassa thaumas

# hosts and their package sets
glaucus  = emacs git nixpkgs tmux vim zsh
nereus   = ctags emacs git ideavim leiningen \
           nixpkgs npm ocaml tmux vim zsh \
           x11-osx
thaumas  = ctags emacs git ideavim leiningen \
           nixpkgs npm ocaml tmux vim zsh \
           dunst x11 xdg

# base package set for undefined hosts
BASE_PKGS = emacs git tmux vim zsh

# target directories
TARG_DIRS = ../.config

# get host name
HOST = $(shell hostname -s)

# define package set based on host's name
ifneq (,$(findstring $(HOST),$(MY_HOSTS)))
  PKG_SET = $($(HOST))
else
  PKG_SET = $(BASE_PKGS)
endif

# ghc
ifneq (,$(findstring ghc,$(PKG_SET)))
  TARG_DIRS += ../.ghc
endif

# leiningen
ifneq (,$(findstring leiningen,$(PKG_SET)))
  TARG_DIRS += ../.lein
endif

# sbt
ifneq (,$(findstring sbt,$(PKG_SET)))
  TARG_DIRS += ../.sbt/0.13/plugins
endif

# vim
ifneq (,$(findstring vim,$(PKG_SET)))
  TARG_DIRS += ../.vim
endif

all: install

list:
	@echo Packages for $(HOST):
	@echo $(PKG_SET)

$(TARG_DIRS):
	@mkdir -p $@

install: $(TARG_DIRS)
	@stow -v $(PKG_SET)
	@$(INSTALL_HOOK)

reinstall: $(TARG_DIRS)
	@stow -Rv $(PKG_SET)
	@$(REINSTALL_HOOK)

clean:
	@$(CLEAN_HOOK)
	@stow -Dv $(PKG_SET)

.PHONY: all list install reinstall clean
