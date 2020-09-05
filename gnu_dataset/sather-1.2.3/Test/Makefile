#------------------------------->  Makefile  <--------------------------------#
#- Copyright (C) 199x by International Computer Science Institute            -#
#- This file is part of the GNU Sather package. It is free software; you may -#
#- redistribute  and/or modify it under the terms of the  GNU General Public -#
#- License (GPL)  as  published  by the  Free  Software  Foundation;  either -#
#- version 3 of the license, or (at your option) any later version.          -#
#- This  program  is distributed  in the  hope that it will  be  useful, but -#
#- WITHOUT ANY WARRANTY without even the implied warranty of MERCHANTABILITY -#
#- or FITNESS FOR A PARTICULAR PURPOSE. See Doc/GPL for more details.        -#
#- The license text is also available from:  Free Software Foundation, Inc., -#
#- 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA                     -#
#------------->  Please email comments to <bug-sather@gnu.org>  <-------------#

SACOMP         =Bin/sacomp
CS_OPT         =-chk -verbose -output_C
CS_PSATHER_OPT =-linux/lwp

TESTCS1     =./test-cs-first
TESTCS2     =./test-cs-second
TESTALL     =./test-all
TESTFULL    =./test-full
TESTPSATHER =./test-psather

####################### TESTING ########################

test: test-all

# There are two tests.  The first is the test classes in the library,
# which is pretty straightforward.  The second is a full 
# double compilation of the compiler, which can take quite a while.

test-all:
	@echo; echo Testing...; echo
	../$(SACOMP) $(CS_OPT) test-all.module -main TEST_OBLIG -o $(TESTALL)
	$(TESTALL) > $(TESTALL).output

# The full test does the following:
#
#    1. Compile using Bin/sacomp, with full checking on.
#    2. Compile using result.
#    3. Clean up.

test-full:
	@echo; echo "Doing full test..." ; echo
	@echo; echo "Compiling 1st..." ; echo
	../$(SACOMP) $(CS_OPT) ../Compiler/sacomp.module -o $(TESTCS1)
	@echo; echo "Compiling 2nd..." ; echo
	$(TESTCS1) $(CS_OPT) ../Compiler/sacomp.module -o $(TESTCS2)
	
	$(TESTCS2) $(CS_OPT) test-all.module -o $(TESTFULL)
	$(TESTFULL) > $(TESTFULL).output
	
	@ CC="$(CC)" RANLIB="$(RANLIB)" CPP="$(CPP)" MV="$(MV)"	\
	  AR="$(AR)" RM="$(RM)" LN="$(LN)" CP="$(CP)"		\
	  export CC RANLIB CPP MV AR RM LN CP;			\
	for i in `echo $(PLATFORMS) | tr , \ ` ; do		\
	  echo "Testing platform $$i..." ;			\
	  $(MAKE) -C ../System/Platforms/$$i test ;		\
	done
	
	@echo; echo "Everything okay" ; echo

test-psather:
	../$(SACOMP) $(CS_OPT) $(CS_PSATHER_OPT) test-psather.module -o $(TESTPSATHER)
	$(TESTPSATHER) > $(TESTPSATHER).output

clean:
	rm -fr $(TESTCS1) $(TESTCS1).code
	rm -fr $(TESTCS2) $(TESTCS2).code
	rm -fr $(TESTALL) $(TESTALL).code $(TESTALL).output
	rm -fr $(TESTFULL) $(TESTFULL).code $(TESTFULL).output
	rm -fr $(TESTPSATHER) $(TESTPSATHER).code $(TESTPSATHER).output

# These test rules are used at the ICSI. If you work on the compiler they
# may be useful for you too, but you will probably have to hack the perl
# scripts in Test and Test/TestPrg/PROG (they are adapted to the 
# specific way files and home directories are set up at ICSI).

testcommit: SYSTEM prepare_test 
	@ CC="$(CC)" RANLIB="$(RANLIB)" CPP="$(CPP)" MV="$(MV)"	\
	  AR="$(AR)" RM="$(RM)" LN="$(LN)" CP="$(CP)"		\
	  export CC RANLIB CPP MV AR RM LN CP;			\
	for i in solaris					\
	         solaris/sparc_smp				\
		 solaris/sparc_tcpip				\
	         solaris_at/sparc_smp				\
		 solaris_at/sparc_myrinet; do			\
	  echo "Making platform $$i..." ;			\
	  $(MAKE) -C System/Platforms/$$i			\
	done
	
	./do_test solaris solaris/sparc_smp solaris/sparc_tcpip \
	          solaris_at/sparc_smp solaris_at/sparc_myrinet

testcomplete:
	./do_test `echo $(PLATFORMS) | tr '",' '  '`

testplatform:
	@ CC="$(CC)" RANLIB="$(RANLIB)" CPP="$(CPP)" MV="$(MV)"	\
	  AR="$(AR)" RM="$(RM)" LN="$(LN)" CP="$(CP)"		\
	  export CC RANLIB CPP MV AR RM LN CP;			\
	$(MAKE) -C ../System/Platforms/$(PLATFORM)
	./do_test `echo $(PLATFORM)`
