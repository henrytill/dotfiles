# defined hosts
MY_HOSTS = proteus thalassa tethys

# hosts and their package sets
proteus  = emacs foot gdb git git-unix oksh \
           profile sway sway-proteus tmux
thalassa = emacs foot gdb git git-unix oksh \
           profile sway sway-thalassa tmux xdg
tethys   = emacs gdb git git-unix oksh profile tmux

# base package set for undefined hosts
BASE_PKGS = git git-unix oksh profile tmux

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

all: install

list:
	@echo Packages for $(HOST):
	@echo $(PKG_SET)

$(TARG_DIRS):
	mkdir -p $@

sway-proteus sway-thalassa::
	install --b -S .old -C $@/sway-session /usr/local/bin

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