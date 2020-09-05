all:    plat

libs:
	cd src; make

egs:
	cd examples; make

dems:
	cd demos; make

clean:
	cd src; make clean
	cd examples; make clean
	cd demos; make clean

distclean:
	cd src; make distclean
	cd examples; make distclean
	cd demos; make distclean
	

plat:
	make libs \
	"CC= gcc" \
	"CFLAG= -DMAV_LINUX -fPIC -Wall -ansi -ggdb -O2 -finline-functions -fomit-frame-pointer -funroll-loops -ffast-math -march=native" \
	"LD= ld -shared" \
	"DLLIBS= -ldl" \
	"LEX= flex" \
	"YACC= bison -y" \
	"CXX= g++" \
	"CXXFLAG= -DMAV_LINUX -fPIC -Wall -ansi -ggdb -O2 -finline-functions -fomit-frame-pointer -funroll-loops -ffast-math -march=native" \
	"VRML97COMP= 0" \
	"VVCOMP= 0" \
	"TDMINCL= -DMAV_TDM -I/home/rosch/Maverik/TDM-2.2/incl" \
	"PNGINCL= -DMAV_PNG -I/usr/include" \
	"PNGLIBS= -L/usr/lib64 -lpng" \
	"OPENGLINCL= -I/usr/include" \
	"OPENGLLIBS= -L/usr/lib64 -lGL -lGLU -L/usr/lib64 -lX11 -lXext"
	make egs \
	MAV_HOME=`pwd` \
	"CC= gcc   -Wall -ansi -ggdb -O2 -finline-functions -fomit-frame-pointer -funroll-loops -ffast-math -march=native" \
	"CXX= g++   -Wall -ansi -ggdb -O2 -finline-functions -fomit-frame-pointer -funroll-loops -ffast-math -march=native" 
	make dems \
	MAV_HOME=`pwd` \
	"CC= gcc   -Wall -ansi -ggdb -O2 -finline-functions -fomit-frame-pointer -funroll-loops -ffast-math -march=native" \
	"OPENGLINCL= -I/usr/include" \
	"OPENGLLIBS= -L/usr/lib64 -lGL -lGLU -L/usr/lib64 -lX11 -lXext"
