%%%% raster_0.mp
%%%% Created by Laurence D. Finston (LDF) Fr Jun 17 10:55:09 CEST 2005

%% DO NOT DELETE!  It was written by hand, _not_ generated!  

%%%% * Copyright and License.

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

%%%% Please send bug reports to bug-3dldf@gnu.org 
%%%% The mailing list help-3dldf@gnu.org is available for people to 
%%%% ask other users for help.  
%%%% The mailing list info-3dldf@gnu.org is for the maintainer of 
%%%% GNU 3DLDF to send announcements to users. 
%%%% To subscribe to these mailing lists, send an 
%%%% email with ``subscribe <email-address>'' as the subject.  

%%%% The author can be contacted at: 

%%%% Laurence D. Finston 
%%%% c/o The Free Software Foundation, Inc.
%%%% 51 Franklin St, Fifth Floor 
%%%% Boston, MA  02110-1301 
%%%% USA 

%%%% Laurence.Finston@gmx.de 
 



%% * (1)

%% LDF 2005.09.07.
%% Run these commands:

%% mpost raster_0
%% convert raster_0.1 raster_0.png
%% cp raster_0.png ~/www
%% chmod go+r ~/www/raster_0.png

prologues := 1;  %% 0

beginfig(1);

pickup pencircle scaled .25mm;
tiny_pen = savepen;

end_value    := 115;
indent_value := 5;


step_value0 := 1;
step_value1 := 10;

for i = 0 step step_value0 until end_value:
   for j = 0 step step_value0 until end_value:
      drawdot (i*mm, j*mm);
   endfor;
endfor;


pickup pencircle scaled 1mm;
medium_pen = savepen;

path c;
c := fullcircle scaled .5cm;

for i = 0 step step_value1 until end_value:
   for j = 0 step step_value1 until end_value:
      pickup medium_pen;
      drawdot (i*mm, j*mm);
      pickup tiny_pen;
      draw c shifted (i*mm, j*mm);
   endfor;
endfor;

pickup pencircle scaled 2mm;

for i = 5 step step_value1 until (end_value - indent_value):
   for j = 5 step step_value1 until (end_value - indent_value):
      drawdot (i*mm, j*mm);
   endfor;
endfor;

clip_value := 11.5cm;

clip currentpicture to    (0, 0) -- (clip_value, 0)
                       -- (clip_value, clip_value) -- (0, clip_value)
                       -- cycle;

endfig;
end;

%% ** End of MetaPost code.

%% * Local variables for Emacs.

%% Local Variables:
%% mode:MetaPost
%% eval:(outline-minor-mode t)
%% eval:(read-abbrev-file abbrev-file-name)
%% outline-regexp:"%% [*\f]+"
%% End:
