# ~/.bash_logout: executed by bash(1) when login shell exits.

# when leaving the console clear the screen to increase privacy

if test "$SHLVL" = 1
then
	if test -x /usr/bin/clear_console
	then
		/usr/bin/clear_console -q
	fi
fi

# Local Variables:
# sh-basic-offset: 8
# indent-tabs-mode: t
# End:
