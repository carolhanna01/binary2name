TCL_LIB_NAME = $(shell if [ -e /usr/lib/libtcl8.0.so ] ; then echo "tcl8.0" ; else echo "tcl"; fi)
TK_LIB_NAME = $(shell if [ -e /usr/lib/libtk8.0.so ] ; then echo "tk8.0" ; else echo "tk"; fi)
