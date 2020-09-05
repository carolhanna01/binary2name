#
# To Build tgrep, just enter "make"
#
######################### STOP EDITING NOW ###########################
PROGRAM=libpat.a
include ../makefile.master

.KEEP_STATE: 

SRC_FILES= pmatch.c bmpmatch.c
HEADER_FILES=pmatch.h proto.h debug.h
OBJ_FILES=$(SRC_FILES:%.c=%.o)

all: $(PROGRAM) tgrep.man

tgrep.man: tgrep.1
	nroff -man tgrep.1 > tgrep.man

$(PROGRAM): $(OBJ_FILES)
	ar -r $(PROGRAM) $(OBJ_FILES)

%.o : %.c $(HEADER_FILES)
	$(CC) $(CFLAGS) -c $< -o $@

clean: 
	rm -f $(OBJ_FILES) core *~ *%

clobber: clean
	rm -f $(PROGRAM) *.flc tgrep.man

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

#WARLOCK_FILES=$(OBJ_ALL:%.o=%.ll)

%.o : %.c $(HEADER_FILES)
	$(CC) $(CFLAGS) -I../Common -c $< -o $@









