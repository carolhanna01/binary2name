## 3DLDF-2.0/doc/Makefile.am
#### Created by Laurence D. Finston (LDF)

#### This file is part of GNU 3DLDF, a package for three-dimensional drawing. 
#### Copyright (C) 2013 The Free Software Foundation

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



#### Automatic Variables.  For more, see Section 10.5.3, "Automatic Variables"
#### of the Make info manual.
#### LDF 2009.12.29.

#### $@:  Target
#### $<:  First prerequisite
#### $^:  The names of all the prerequisites, with spaces between them.

SOURCES_MANUAL :=  3dldf.texi concidx.texi dttypes.texi dttypidx.texi fdl.texi \
                   funcidx.texi getstart.texi gpl.texi install.texi intro.texi \
                   invoking.texi varidx.texi

EXTRA_DIST = 3dldf.ps 3dldf.pdf 3dldf.dvi 3dldf.html 3dldf.info $(SOURCES_MANUAL) texinfo.tex

all: 3dldf.dvi 3dldf.ps 3dldf.pdf 3dldf.info 3dldf.html html-split

.PHONY: dvi

dvi: 3dldf.dvi

.PHONY: ps

ps: 3dldf.ps

.PHONY: pdf

pdf: 3dldf.pdf

.PHONY: html

html: 3dldf.html

.PHONY: info

info: 3dldf.info

3dldf.dvi: $(SOURCES_MANUAL)
	texi2dvi 3dldf.texi

3dldf.ps: 3dldf.dvi
	dvips -o $@ $<

3dldf.pdf: 3dldf.ps
	ps2pdf $<

3dldf.info: $(SOURCES_MANUAL)
	makeinfo 3dldf.texi

3dldf.html: $(SOURCES_MANUAL)
	makeinfo --html --no-split 3dldf.texi

#### .PHONY: html-split

html-split: $(SOURCES_MANUAL)
	makeinfo --html 3dldf.texi
	touch html-split

#### ** (2) Tags

.PHONY: tags

tags: TAGS

TAGS: $(SOURCES_MANUAL)
	etags $(SOURCES_MANUAL)

.PHONY: query

query:
	echo "SOURCES_MANUAL == $(SOURCES_MANUAL)"


## Local Variables:
## mode:Makefile
## End:












