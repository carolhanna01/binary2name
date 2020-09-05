## Process this file with automake to create Makefile.in

AWK=@AWK@

TESTS = boolean.sh context.sh empty.sh opt_b.sh opt_c.sh opt_d.sh \
        opt_f.sh opt_h.sh opt_i.sh opt_l.sh opt_n.sh opt_p.sh opt_q.sh \
        opt_s.sh single.sh status.sh
EXTRA_DIST = $(TESTS) empty.txt context.txt single.txt
TESTS_ENVIRONMENT = BOOL=$(top_builddir)/src/bool AWK=$(AWK)

