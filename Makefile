# List of defined hosts
MY_HOSTS  = glaucus nereus proteus tethys thalassa thaumas

# Hosts and their package sets
glaucus   = emacs git nixpkgs tmux zile zsh
nereus    = asdf emacs git keysnail leiningen nixpkgs sbcl tmux x11-osx zsh
proteus   = dunst emacs git keysnail leiningen nixpkgs tmux x11-nixos xdg \
            xmonad zile zsh
tethys    = dunst emacs git keysnail leiningen nixpkgs tmux x11-nixos xdg \
            xmonad zile zsh
thalassa  = emacs git keysnail leiningen tmux x11-gentoo xdg xmonad zile zsh
thaumas   = emacs git keysnail leiningen tmux x11-gentoo xdg xmonad zile zsh

# Base package set for undefined hosts
BASE_PKGS = emacs git tmux zile zsh

# Target Directories
TARG_DIRS = .config

# Get host name
ifeq ($(shell uname),Darwin)
  HOST = $(shell hostname -s)
else
  HOST ?= $(shell hostname)
endif

# Define package set for Stow based on host's name
ifneq (,$(findstring $(HOST),$(MY_HOSTS)))
  PKG_SET = $($(HOST))
else
  PKG_SET = $(BASE_PKGS)
endif

# Add .lein to TARG_DIRS if leiningen is in PKG_SET
ifneq (,$(findstring leiningen,$(PKG_SET)))
  TARG_DIRS += .lein
endif

# Targets

all: install

install: dirs $(PKG_SET:=.install)

reinstall: $(PKG_SET:=.reinstall)

clean: $(PKG_SET:=.clean)

dirs: $(addprefix ../,$(TARG_DIRS))

../%:
	mkdir $@

%.install:
	stow -v $*

%.reinstall:
	stow -Rv $*

%.clean:
	stow -Dv $*

keysnail.install keysnail.reinstall:
	$(MAKE) -C keysnail/plugins

keysnail.clean:
	$(MAKE) clean -C keysnail/plugins
