# defined hosts
MY_HOSTS = glaucus nereus tethys thaumas

# hosts and their package sets
glaucus = emacs git nixpkgs tmux vim zsh
nereus  = asdf boot emacs git keysnail leiningen nixpkgs sbcl tmux \
          vim x11-osx zsh
tethys  = boot dunst emacs git keysnail leiningen nixpkgs systemd tmux \
          vim x11-nixos xdg zsh
thaumas = boot dunst emacs git keysnail leiningen nixpkgs systemd tmux \
          vim x11-nixos xdg zsh

# base package set for undefined hosts
BASE_PKGS = emacs git tmux vim zsh

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

# boot
ifneq (,$(findstring boot,$(PKG_SET)))
  TARG_DIRS += ../.boot
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

# systemd
ifneq (,$(findstring systemd,$(PKG_SET)))
  INSTALL_HOOK += systemctl --user enable ~/etc/systemd/*.service;
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
	@stow -Dv $(PKG_SET)
	@$(CLEAN_HOOK)

.PHONY: all list install reinstall clean
