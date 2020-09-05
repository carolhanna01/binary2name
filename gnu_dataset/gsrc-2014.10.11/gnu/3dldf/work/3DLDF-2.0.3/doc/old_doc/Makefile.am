## 3DLDF-2.0/doc/old_doc/Makefile.am
#### Created by Laurence D. Finston (LDF)

#### This file is part of GNU 3DLDF, a package for three-dimensional drawing. 
#### Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013 The Free Software Foundation

#### GNU 3DLDF is free software; you can redistribute it and/or modify 
#### it under the terms of the GNU General Public License as published by 
#### the Free Software Foundation; either version 3 of the License, or 
#### (at your option) any later version. 

#### GNU 3DLDF is distributed in the hope that it will be useful, 
#### but WITHOUT ANY WARRANTY; without even the implied warranty of 
#### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
#### GNU General Public License for more details. 

#### You should have received a copy of the GNU General Public License 
#### along with GNU 3DLDF; if not, write to the Free Software 
#### Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA 

#### GNU 3DLDF is a GNU package.  
#### It is part of the GNU Project of the  
#### Free Software Foundation 
#### and is published under the GNU General Public License. 
#### See the website http://www.gnu.org 
#### for more information.   
#### GNU 3DLDF is available for downloading from 
#### http://www.gnu.org/software/3dldf/LDF.html. 

#### Please send bug reports to Laurence.Finston@gmx.de
#### The mailing list help-3dldf@gnu.org is available for people to 
#### ask other users for help.  
#### The mailing list info-3dldf@gnu.org is for sending 
#### announcements to users. To subscribe to these mailing lists, send an 
#### email with "subscribe <email-address>" as the subject.  

#### The author can be contacted at: 

#### Laurence D. Finston                  
#### c/o Free Software Foundation, Inc.  
#### 51 Franklin St, Fifth Floor          
#### Boston, MA  02110-1301              
#### USA                                  

#### Laurence.Finston@gmx.de



# html-split
# 3DLDF.dvi 3DLDF.ps 3DLDF.pdf 3DLDF.info 3DLDF.html 

all: 

3dldf_TEXINFOS = 3DLDF.texi intro.texi contrib.texi using.texi gspoint.texi gstranpt.texi  \
                 gstransf.texi gsdlpt.texi gspaths.texi gsplane.texi gssolfig.texi \
                 gspict.texi intersct.texi instlrun.texi typeglb.texi glbcnvar.texi \
                 creatnew.texi system.texi color.texi io.texi shape.texi transfor.texi \
                 label.texi picture.texi point.texi focus.texi line.texi plane.texi \
                 path.texi polygon.texi regpolyg.texi rectang.texi rcpcurve.texi \
                 ellipse.texi circle.texi pattern.texi solid.texi solfaced.texi \
                 cuboid.texi polyhed.texi utility.texi addfile.texi future.texi \
                 changes.texi biblio.texi fdl.texi varidx.texi funcidx.texi \
                 concidx.texi 

3dldf_EPS =   ./graphics/eps/3DLDFmp.1 \
              ./graphics/eps/3DLDFmp.2 \
              ./graphics/eps/3DLDFmp.3 \
              ./graphics/eps/3DLDFmp.4 \
              ./graphics/eps/3DLDFmp.5 \
              ./graphics/eps/3DLDFmp.6 \
              ./graphics/eps/3DLDFmp.7 \
              ./graphics/eps/3DLDFmp.8 \
              ./graphics/eps/3DLDFmp.9 \
              ./graphics/eps/3DLDFmp.1? \
              ./graphics/eps/3DLDFmp.2? \
              ./graphics/eps/3DLDFmp.3? \
              ./graphics/eps/3DLDFmp.4? \
              ./graphics/eps/3DLDFmp.5? \
              ./graphics/eps/3DLDFmp.6? \
              ./graphics/eps/3DLDFmp.7? \
              ./graphics/eps/3DLDFmp.8? \
              ./graphics/eps/3DLDFmp.9? \
              ./graphics/eps/3DLDFmp.1??   

3dldf_PNG = ./graphics/png/3DLDF1.png \
	    ./graphics/png/3DLDF2.png \
	    ./graphics/png/3DLDF3.png \
	    ./graphics/png/3DLDF4.png \
	    ./graphics/png/3DLDF5.png \
	    ./graphics/png/3DLDF6.png \
	    ./graphics/png/3DLDF7.png \
	    ./graphics/png/3DLDF8.png \
	    ./graphics/png/3DLDF9.png \
	    ./graphics/png/3DLDF1?.png \
	    ./graphics/png/3DLDF2?.png \
	    ./graphics/png/3DLDF3?.png \
	    ./graphics/png/3DLDF4?.png \
	    ./graphics/png/3DLDF5?.png \
	    ./graphics/png/3DLDF6?.png \
	    ./graphics/png/3DLDF7?.png \
	    ./graphics/png/3DLDF8?.png \
	    ./graphics/png/3DLDF9?.png \
	    ./graphics/png/3DLDF1??.png

.PHONY: pdf

pdf: 3DLDF.pdf

3DLDF.pdf: 3DLDF.ps
	ps2pdf 3DLDF.ps

EXTRA_DIST = 3DLDF.ps 3DLDF.pdf 3DLDF.dvi 3DLDF.info 3DLDF.html \
              ChangeLog.dev $(3dldf_TEXINFOS) 3DLDF/ \
              $(3dldf_EPS) $(3dldf_PNG) 

.PHONY : dummy
dummy:
	echo "distdir = $(distdir)"

# .PHONY : ps
# ps: 3DLDF.ps 

# 3DLDF.ps : 3DLDF.dvi
# 	@echo Running \"dvips -o 3DLDF.ps 3DLDF.dvi\"
# 	dvips -o 3DLDF.ps 3DLDF.dvi


# 3DLDF.dvi: $(3dldf_TEXINFOS)
# 	texi2dvi 3DLDF.texi

# 3DLDF.html: $(3dldf_TEXINFOS)
# 	makeinfo --html --no-split 3DLDF.texi

# 3DLDF.info: $(3dldf_TEXINFOS)
# 	makeinfo 3DLDF.texi

# .PHONY: html-split

# html-split: $(3dldf_TEXINFOS)
# 	makeinfo --html 3DLDF.texi


.PHONY : purge
purge:
	@echo "Deleting files generated by Texinfo."
	-rm -f 3DLDF.aux
	-rm -f 3DLDF.cp
	-rm -f 3DLDF.cps
	-rm -f 3DLDF.fn
	-rm -f 3DLDF.ky
	-rm -f 3DLDF.log
	-rm -f 3DLDF.pg
	-rm -f 3DLDF.tmp
	-rm -f 3DLDF.toc
	-rm -f 3DLDF.tp
	-rm -f 3DLDF.vr
	-rm -f 3DLDF.fns
	-rm -f 3DLDF.vrs

TAGS : $(3dldf_TEXINFOS) 
	@echo "Regenerating the tags table for the TEXINFO files."
	etags $(3dldf_TEXINFOS) 


.PHONY : show
show:
	@echo "3dldf_PNG: $(3dldf_PNG)"
	@echo "3dldf_EPS: $(3dldf_EPS)"

## Local Variables:
## mode:Makefile
## End:










