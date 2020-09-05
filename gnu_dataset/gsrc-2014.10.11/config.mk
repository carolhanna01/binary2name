# Installation directories.
# These are the standard directory name variables from all GNU
# makefiles.  They're also used by autoconf, and can be adapted
# for a variety of build systems.

prefix ?= /usr/local
sysconfdir ?= ${prefix}/etc
vardir ?= ${prefix}/var
bootdir ?= $(prefix)/boot

CONFIGURE_ARGS = 

BOOTSTRAP = 1
