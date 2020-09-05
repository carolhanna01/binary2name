%%%% 3DLDFmp.mp
%%%% Created by Laurence D. Finston (LDF) 

%% * (1) Copyright and License.

%%%% This file is part of GNU 3DLDF, a package for three-dimensional drawing.  
%%%% Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013 The Free Software Foundation

%%%% GNU 3DLDF is free software; you can redistribute it and/or modify 
%%%% it under the terms of the GNU General Public License as published by 
%%%% the Free Software Foundation; either version 3 of the License, or 
%%%% (at your option) any later version.  

%%%% GNU 3DLDF is distributed in the hope that it will be useful, 
%%%% but WITHOUT ANY WARRANTY; without even the implied warranty of 
%%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
%%%% GNU General Public License for more details.  

%%%% You should have received a copy of the GNU General Public License 
%%%% along with GNU 3DLDF; if not, write to the Free Software 
%%%% Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA 

%%%% GNU 3DLDF is a GNU package.  
%%%% It is part of the GNU Project of the  
%%%% Free Software Foundation 
%%%% and is published under the GNU General Public License. 
%%%% See the website http://www.gnu.org 
%%%% for more information.   
%%%% GNU 3DLDF is available for downloading from 
%%%% http://www.gnu.org/software/3dldf/LDF.html.
  

%%%% Please send bug reports to Laurence.Finston@gmx.de
%%%% The mailing list help-3dldf@gnu.org is available for people to 
%%%% ask other users for help.  
%%%% The mailing list info-3dldf@gnu.org is for sending 
%%%% announcements to users. To subscribe to these mailing lists, send an 
%%%% email with ``subscribe <email-address>'' as the subject.  

%%%% The author can be contacted at: 

%%%% Laurence D. Finston 
%%%% c/o Free Software Foundation, Inc. 
%%%% 51 Franklin St, Fifth Floor 
%%%% Boston, MA  02110-1301  
%%%% USA

%%%% Laurence.Finston@gmx.de
%%%% 

%% * (1) 

input boxes;

pickup pencircle scaled .25mm;

%% LDF 2003.01.15.  Added the following two lines.  This sets the
%% margins of the boxes around the examples.  The original default is
%% 3. 
defaultdx:=.75cm;
defaultdy:=.75cm;
  
%input 3DLDFput.mp 

%% * (1) Now inputting `sample2.mp' instead of `sample1.mp' or `sample0.mp'.
%%       `sample0.ldf' has gotten so large that I've switched 
%%       to using `sample2.ldf' for the current sample code.  When I'm done with it,
%%       I'm now putting it into `sample1.ldf'.  It's easier to search `sample2.ldf'
%%       if the older samples are removed.
%% 
%%       LDF 2007.09.19.

%input sample0.mp 
%input sample1.mp 
input sample2.mp 

%input stars.mp 


bye;

%% Local Variables:
%% mode:Metafont
%% eval:(if metapost-keymap nil (load "metapost-keymap"))
%% eval:(use-local-map metapost-mode-map) 
%% eval:(local-set-key [f9] 'mp-file)
%% eval:(read-abbrev-file "~/.abbrev_defs")
%% run-mp-on-file:"3DLDFmp.mp"
%% run-tex-on-file:"3DLDFtex.tex"
%% run-dvips-on-file:"3DLDFtex.dvi"
%% End:

