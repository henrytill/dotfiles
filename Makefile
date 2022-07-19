# defined hosts
MY_HOSTS = proteus thalassa tethys

# hosts and their package sets
proteus  = emacs foot gdb git git-unix oksh \
           profile sway sway-proteus tmux vim
thalassa = emacs foot gdb git git-unix gtk nixpkgs ocaml oksh \
           profile sway sway-thalassa systemd tmux vim x11 xdg
tethys   = bash emacs gdb git git-unix profile tmux vim

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

sway-thalassa::
	install --b -S .old -C sway/sway.desktop /usr/local/share/wayland-sessions
	install --b -S .old -C sway/sway-session /usr/local/bin

$(PKG_SET)::
	stow -v $@

install: $(PKG_SET) $(TARG_DIRS)
	$(INSTALL_HOOK)

reinstall: $(TARG_DIRS)
	stow -Rv $(PKG_SET)
	$(REINSTALL_HOOK)

clean:
	$(CLEAN_HOOK)
	stow -Dv $(PKG_SET)

.PHONY: all list install reinstall clean $(PKG_SET)
