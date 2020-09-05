guiledir=$(exec_prefix)$(GUILE_SITE)
guile_DATA = seal.scm seal.go
EXTRA_DIST = $(guile_DATA)

%.go: @abs_srcdir@/%.scm
	$(GUILD) compile $< -o $@
