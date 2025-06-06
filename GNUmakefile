# defined hosts
MY_HOSTS = proteus thalassa thaumas

# hosts and their package sets
proteus  = bash code dune emacs foot gdb git git-unix nix \
           sway sway-proteus tmux xdg zathura
thalassa = bash code dune emacs foot gdb git git-unix nix \
           sway sway-thalassa tmux x11 xdg zathura
thaumas  = bash emacs foot gdb git git-unix \
           sway sway-thaumas xdg

# base package set for undefined hosts
BASE_PKGS = git git-unix

# target directories
DIRS =
DIRS += ../.config
DIRS += ../.cache/gdb

# get host name
HOST = $(shell hostname -s)

# define package set based on host's name
ifneq (,$(findstring $(HOST),$(MY_HOSTS)))
PKG_SET = $($(HOST))
else
PKG_SET = $(BASE_PKGS)
endif

all: install

list:
	@echo Packages for $(HOST):
	@echo $(PKG_SET)

$(DIRS):
	mkdir -p $@

sway-proteus sway-thaumas::
	install -bv -S .old sway/sway-session /usr/local/bin

$(PKG_SET)::
	stow -v $@

install: $(PKG_SET) | $(DIRS)
	$(INSTALL_HOOK)

reinstall: | $(DIRS)
	stow -Rv $(PKG_SET)
	$(REINSTALL_HOOK)

clean:
	$(CLEAN_HOOK)
	stow -Dv $(PKG_SET)

.PHONY: all list install reinstall clean $(PKG_SET)
