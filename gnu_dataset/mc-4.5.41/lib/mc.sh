mc ()
{
        mkdir -p ~/.mc/tmp 2> /dev/null
	chmod 700 ~/.mc/tmp
	MC=~/.mc/tmp/mc-$$
	/opt/gnome/bin/mc -P "$@" > "$MC"
	cd "`cat $MC`"
	rm "$MC"
        unset MC;
}
