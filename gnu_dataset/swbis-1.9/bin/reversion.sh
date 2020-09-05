case "$1" in
	"")
		echo "Usage: sh reversion.sh NEW_VERSION" 1>&2
		exit 1;
		;;
	*)
		;;
esac

version="$1"
set -vx
cat configure.ac |
sed -e "s/^AC_INIT(\\[swbis\\], \\[[^\\]*\\], \\[bug-swbis@gnu.org\\])/AC_INIT(\\[swbis\\], \\[$version\\], \\[bug-swbis@gnu.org\\])/" |
cat > configure.ac.new
diff configure.ac configure.ac.new
cat < configure.ac.new >configure.ac
rm configure.ac.new
echo "$version" >VERSION
autoconf && automake
exit $?
