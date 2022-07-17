stripwhitespace() {
    sed -i 's/[[:space:]]\+$//' "$@"
}

# Local Variables:
# mode: sh
# End:
#
# vim: set filetype=sh:
