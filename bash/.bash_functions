stripwhitespace() {
    sed -i 's/[[:space:]]\+$//' "$@"
}

cls() {
    if [ -n "$(command -v tput)" ]; then
        tput reset
    else
        clear
    fi
}

# Local Variables:
# mode: sh
# End:
#
# vim: set filetype=sh:
