#
# To Build tgrep, just enter "make"
#
######################### STOP EDITING NOW ###########################
PROGRAM=tgrep
PROGRAM_SMP=at_grep
include ../makefile.master

AT		= $(SATHER_HOME)/System/Common/ActiveThreads
AT_LIB_DIR	= $(AT)/lib
AT_LIBS         = -lqt -lat -lschedulers
AT_COMMON_DIR	= $(SATHER_HOME)/System/Common/Brahma/at_common  

BRAHMA		= $(SATHER_HOME)/System/Common/Brahma
MYRI_HOME	= /u/sather/network/myrinet
MYRI_SRC_DIR	= $(SATHER_HOME)/System/Common/Brahma/myrinet_solaris/src
MYRI_LIB_DIR	= $(SATHER_HOME)/System/Common/Brahma/lib
MYRI_LIB_DIRS = -L$(MYRI_LIB_DIR) -L/usr/ucb/lib -L${MYRI_HOME}/lib/sparc_solaris
MYRI_LIBS	= -lLanaiDevice -lbfd -liberty 
ARCH	= sparc-solaris

LDFLAGS		+= $(MYRI_LIB_DIRS) $(MYRI_LIB_DIR)/solaris_at_sparc_myrinet.a $(MYRI_LIBS) -L$(AT_LIB_DIR) $(AT_LIBS)

INC_DIRS	= -I$(BRAHMA) -I. -I$(MYRI_SRC_DIR) -I$(MYRI_HOME)/include -I$(AT_COMMON_DIR) -I$(AT)/src -I$(AT)/md/$(ARCH)


CFLAGS		= -g -DAT_NO_INLINE -DDEBUG -DAT_THREADS -DBR_MYRINET_SOLARIS_AT  $(INC_DIRS)
.KEEP_STATE: 

SRC_COMMON=../Common/pmatch.c ../Common/bmpmatch.c
SRC_FILES=main.c $(MYRI_SRC_DIR)/migration.c
SRC_ALL=$(SRC_COMMON) $(SRC_FILES)
OBJ_ALL=$(SRC_ALL:%.c=%.o)
#WARLOCK_FILES=$(OBJ_ALL:%.o=%.ll)
HEADER_FILES=version.h ../Common/debug.h ../Common/proto.h
OBJ_FILES=$(SRC_FILES:%.c=%.o)

$(PROGRAM): $(OBJ_FILES) ../Common/libpat.a
	$(CC) $(OBJ_FILES) $(LDFLAGS) -L../Common -lpat -o $(PROGRAM)

$(PROGRAM_SMP): at_main.o ../Common/libpat.a
	$(CC) at_main.o -L$(AT_LIB_DIR) $(AT_LIBS) -L../Common -lpat -o $(PROGRAM_SMP)

#warlock: $(WARLOCK_FILES)

%.o : %.c $(HEADER_FILES)
	$(CC) $(CFLAGS) -I../Common -c $< -o $@

../Common/libpat.a: 
	cd ../Common; make

clean: 
	rm -f $(OBJ_FILES) core *~ *%

clobber: clean
	rm -f $(PROGRAM) *.flc

locklint: 
	$(CC) -Zll $(CFLAGS) -I../Common main.c -o main.ll


%.ll: %.c 
	$(WLCC) $(CFLAGS) -I../Common -o $@ $<


