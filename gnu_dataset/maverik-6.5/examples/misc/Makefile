
all:	examples

examples:
	cd allobjs; make
	cd composites; make
	cd text; make
	cd textures; make
	cd chess; make
	cd firefox; make
	cd transparent; make
	cd align; make
	cd stereo; make
	cd avatar; make
	cd windows; make
	cd fonts; make
	cd overview; make
	cd TDM; make
	cd TR; make
	cd LOD; make
	cd lights; make
	cd interpolate; make
	cd hud; make
	if [ -f ../../src/gfx/mav_gfxWMOpenGLGTK.o ] ; then cd GTK ; make ; fi
	if [ -f ../../src/gfx/mav_gfxWMOpenGLQt.o ] ; then cd Qt ; make ; fi
	cd dynamic; make
	cd SR; make
# xforms is unaveilable, so I comment it out
#	cd xforms; make   # Needs Xforms path filling in

.PHONY: clean distclean

clean:
	rm -f *.bck *.*~ core
	cd allobjs; make clean
	cd composites; make clean
	cd text; make clean
	cd textures; make clean
	cd chess; make clean
	cd firefox; make clean
	cd transparent; make clean
	cd align; make clean
	cd stereo; make clean
	cd avatar; make clean
	cd windows; make clean
	cd fonts; make clean
	cd overview; make clean
	cd TDM; make clean
	cd TR; make clean
	cd LOD; make clean
	cd lights; make clean
	cd interpolate; make clean
	cd hud; make clean
	cd GTK; make clean
	cd Qt; make clean
	cd dynamic; make clean
	cd SR; make clean
#	cd xforms; make clean

distclean: clean
	cd allobjs; make distclean
	cd composites; make distclean
	cd text; make distclean
	cd textures; make distclean
	cd chess; make distclean
	cd firefox; make distclean
	cd transparent; make distclean
	cd align; make distclean
	cd stereo; make distclean
	cd avatar; make distclean
	cd windows; make distclean
	cd fonts; make distclean
	cd overview; make distclean
	cd TDM; make distclean
	cd TR; make distclean
	cd LOD; make distclean
	cd lights; make distclean
	cd interpolate; make distclean
	cd hud; make distclean
	cd GTK; make distclean
	cd Qt; make distclean
	cd dynamic; make distclean
	cd SR; make distclean
