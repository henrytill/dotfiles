# defined hosts
MY_HOSTS  = glaucus nereus proteus tethys thalassa thaumas

# hosts and their package sets
glaucus   = emacs git nixpkgs tmux zile zsh
nereus    = asdf emacs git keysnail leiningen nixpkgs sbcl tmux x11-osx zsh
proteus   = dunst emacs git keysnail leiningen nixpkgs tmux x11-nixos xdg \
            xmonad zile zsh
tethys    = dunst emacs git keysnail leiningen nixpkgs tmux x11-nixos xdg \
            zile zsh
thalassa  = emacs git keysnail leiningen tmux x11-gentoo xdg zile zsh
thaumas   = emacs git keysnail leiningen tmux x11-gentoo xdg zile zsh

# base package set for undefined hosts
BASE_PKGS = emacs git tmux zile zsh

# target directories
TARG_DIRS = ../.config

# get host name
ifeq ($(shell uname),Darwin)
  HOST = $(shell hostname -s)
else
  HOST ?= $(shell hostname)
endif

# define package set based on host's name
ifneq (,$(findstring $(HOST),$(MY_HOSTS)))
  PKG_SET = $($(HOST))
else
  PKG_SET = $(BASE_PKGS)
endif

# keysnail
ifneq (,$(findstring keysnail,$(PKG_SET)))
  INSTALL_HOOK   += $(MAKE) -C keysnail/plugins;
  REINSTALL_HOOK += $(MAKE) -C keysnail/plugins;
  CLEAN_HOOK     += $(MAKE) clean -C keysnail/plugins;
endif

# leiningen
ifneq (,$(findstring leiningen,$(PKG_SET)))
  TARG_DIRS += ../.lein
endif

all: install

list:
	@echo Packages for $(HOST):
	@echo $(PKG_SET)

$(TARG_DIRS):
	@mkdir $@

install: $(TARG_DIRS)
	@stow -v $(PKG_SET)
	@$(INSTALL_HOOK)

reinstall:
	@stow -Rv $(PKG_SET)
	@$(REINSTALL_HOOK)

clean:
	@stow -Dv $(PKG_SET)
	@$(CLEAN_HOOK)

.PHONY: all list install reinstall clean
