stripwhitespace() {
	sed -i 's/[[:space:]]\+$//' "$@"
}

# https://superuser.com/questions/380772/removing-ansi-color-codes-from-text-stream
stripcolor() {
	sed 's/\x1b\[[0-9;]*m//g'
}

cls() {
	if test -n "$(command -v tput)"
	then
		tput reset
	else
		clear
	fi
}

# Local Variables:
# mode: sh
# sh-basic-offset: 8
# indent-tabs-mode: t
# End:
