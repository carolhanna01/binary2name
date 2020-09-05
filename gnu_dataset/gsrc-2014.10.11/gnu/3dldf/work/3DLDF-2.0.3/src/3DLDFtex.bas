%%%% 3DLDFtex.bas

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


%% * (1) Fonts

\font\large=cmr12
\font\Large=cmr17
\font\huge=cmr17 scaled \magstep2

\font\largebx=cmbx12
%\font\Largebx=cmbx17
%\font\hugebx=cmbx17 scaled \magstep2

\font\small=cmr8
\font\smalltt=cmtt8

%% * (1) Conditionals (\newif)

\newif\ifxlrg
\newif\ifavier
\newif\ifadrei
\newif\iflandscape

\newif\iftesting

%% * (1) Macros

\def\leaderfill{\leaders\hbox to 1em{\hss.\hss}\hfill}

%% * (1) Local variables for Emacs.

%% Local Variables:
%% mode:tex
%% eval:(local-set-key [C-kp-add] 'vc-diff) 
%% eval:(local-set-key "\"" 'self-insert-command)
%% eval:(outline-minor-mode t)
%% eval:(read-abbrev-file abbrev-file-name)
%% abbrev-mode:t
%% outline-regexp:"%% [*\f]+"
%% End:

