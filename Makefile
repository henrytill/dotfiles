# defined hosts
MY_HOSTS = proteus thalassa tethys

# hosts and their package sets
proteus  = bash emacs gdb git git-unix gtk ideavim nixpkgs ocaml \
	   sway-proteus tmux vim x11-gnome xdg-gnome
thalassa = bash emacs gdb git git-unix gtk ideavim nixpkgs ocaml tmux vim \
           x11-gnome xdg-gnome
tethys   = bash emacs gdb git git-unix tmux vim

# base package set for undefined hosts
BASE_PKGS = bash gdb git git-unix tmux vim

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

# systemd
ifneq (,$(findstring systemd,$(PKG_SET)))
  TARG_DIRS += ../.config/systemd/user/default.target.wants
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
	mkdir -p $@

install: $(TARG_DIRS)
	stow -v $(PKG_SET)
	$(INSTALL_HOOK)

reinstall: $(TARG_DIRS)
	stow -Rv $(PKG_SET)
	$(REINSTALL_HOOK)

clean:
	$(CLEAN_HOOK)
	stow -Dv $(PKG_SET)

.PHONY: all list install reinstall clean
