%%%% stlltn02.mp
%% Created by Laurence D. Finston (LDF) Mi Jul  1 19:04:29 CEST 2009


%% DO NOT DELETE!  It was written by hand, _not_ generated!  
%% LDF 2010.08.06.

%% * (1) Copyright and License.

%%%% This file is part of GNU 3DLDF, a package for three-dimensional drawing.  
%%%% Copyright (C) 2010, 2011, 2012, 2013 The Free Software Foundation  

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


%% Run the following commands:
%% mpost stlltn02.mp
%% tex stlltn02.txt
%% dvips -o stlltn02.ps stlltn02.dvi

%% * (1) Top

%% * (1) Stellation Pattern 2


%% ** (2) Declarations

pair p[];

string s;

f = .75;  % Scaling factor for circles

% diam_0 =  8cm * f;
% diam_1 = 13cm * f;
% diam_2 = 18cm * f;
% diam_3 = 23cm * f;

diam_0 =  6cm * f;
diam_1 = 12cm * f;
diam_2 = 18cm * f;
diam_3 = 24cm * f;

offset = .20cm;  %% Setting this value too low results in paths
                 %% failing to intersect.

diam_offset = diam_0 + 2 * offset;


%% *** (3) Pens

%pen_diam = .5mm;
%dot_diam = .5mm;

pen_diam = .375mm;
dot_diam = .75mm;

pen dot_pen;
pickup pencircle scaled dot_diam;
dot_pen = currentpen;

pickup pencircle scaled pen_diam;
pen line_pen;
line_pen = currentpen;

pen big_pen;
pickup pencircle scaled 1mm;
big_pen = currentpen;

pickup line_pen;

%% *** (3) Paths

path q[];

%% *** (3) Pictures

picture temp_picture;

temp_picture = nullpicture;

picture circle_picture;
picture north_picture;
picture northeast_picture;
picture north_northeast_picture;
picture north_northwest_picture;

picture circle_label_picture;
picture north_label_picture;
picture northeast_label_picture;
picture north_northeast_label_picture;
picture north_northwest_label_picture;

picture pie_slice_picture;
picture pie_slice_label_picture;

picture cross_hair_picture;

%% ** (2) Circles

q0 = fullcircle scaled diam_0;
q1 = fullcircle scaled diam_1;
q2 = fullcircle scaled diam_2;
q3 = fullcircle scaled diam_3;

path offset_circle;

offset_circle = fullcircle scaled diam_offset;

%% *** (3) Draw circles

draw q0; 
draw q1;
draw q2;
draw q3; 

%draw offset_circle;


%% *** (3) Dots and lines on inner circle


for i = 0 upto 15:
   p[i] = (point 0 of q0) rotated (22.5 * i);
%   drawdot p[i] withpen dot_pen;
endfor;

% for i = 1 upto 31:
%    drawdot p0 rotated (11.25 * i) withpen dot_pen;
% endfor;  

%drawdot origin withpen dot_pen;

%% *** (3) Lines on inner circle

pickup line_pen;

for i = 1 step 2 until 15:
   draw origin -- p[i];
endfor;  


circle_picture = currentpicture;
currentpicture := nullpicture;

for i = 1 upto 7:
  s := decimal i;
  dotlabel.top(s, p[i]);
endfor;

for i = 9 upto 15:
  s := decimal i;
  dotlabel.bot(s, p[i]);
endfor;

dotlabel.rt("0", p0);
dotlabel.lft("8", p8);

circle_label_picture = currentpicture;

currentpicture := nullpicture;

%% ** (2) North Triangles

pickup line_pen;

p16 = p4 shifted (0, .5(diam_3 - diam_0));

q4 = p3 -- p16 -- p5;

draw q4;

draw p4 -- p16;


p17 = (p5 -- p16) intersectionpoint offset_circle;

p18 = p5 rotatedaround (p17, 90);

p19 = p18 shifted (2 * (p17 - p5));

p20 = (p18 -- p19) intersectionpoint offset_circle;

p21 = 30[p20, p19]; %% !! The coefficient here may need to be increased,
                    %% otherwise, the lines may not intersect.
                    %% LDF 2009.07.05. 

p22 = (p20 -- p21)  intersectionpoint (p4 -- p16);

p23 = p20 reflectedabout (p4, p16);

pair i;

i = p23 intersectiontimes offset_circle;

a = ypart i;

i := p20 intersectiontimes offset_circle;
b = ypart i;

q5 = subpath(a, b) of offset_circle;

draw q5;

q6 = p23 -- p22 -- p20;

draw q6;

p33 =  point 1 of q5;

for i = 1 upto 11:
   drawdot (i/12)[p33, p16] withpen dot_pen;
endfor;

%% **** (4) Add dots to lines

for i = 0 upto 10:
   drawdot (i/10)[p20, p22] withpen dot_pen;
   drawdot (i/10)[p23, p22] withpen dot_pen;
endfor;

for i = 1 upto 3:
  drawdot point (i*.5) of q5 withpen dot_pen;
endfor;

%% *** (3) Finish North picture

north_picture = currentpicture;

currentpicture := nullpicture;

%% *** (3) North labels

dotlabel.top("16", p16);
dotlabel.lft("17", p17);
dotlabel.top("18", p18);
dotlabel.top("19", p19);
dotlabel.top("20", p20);
dotlabel.top("21", p21);
dotlabel.top("22", p22);
dotlabel.top("23", p23);
dotlabel.urt("33", p33);


north_label_picture = currentpicture;
currentpicture := nullpicture;

%% ** (2) Northeast Triangles

%% Start with p24

pickup line_pen;

p24 = (p4 shifted (0, .5(diam_2 - diam_0))) rotated -45;

draw p2 -- p24;

q7 = p1 -- p24 -- p3;

draw q7;

%draw offset_circle;

p25 = (p3 -- p24) intersectionpoint offset_circle;

p26 = p3 rotatedaround (p25, 90);

p27 = .5[p25, p24];

p28 = p26 shifted (p27 - p25);

p29 = 6[p26, p28];

p30 = (p26 -- p29) intersectionpoint (p2 -- p24);
p31 = (p26 -- p29) intersectionpoint offset_circle;

draw p30 -- p31;

p32 = p31 reflectedabout (p2, p24);

i := p31 intersectiontimes offset_circle;

a := ypart i;

i := p32 intersectiontimes offset_circle;
b := ypart i;

q8 = subpath(a, b) of offset_circle;

draw q8;

draw p30 -- p32;

%% **** (4) Add dots to lines

for i = 0 upto 10:
   drawdot (i/10)[p30, p31] withpen dot_pen;
   drawdot (i/10)[p30, p32] withpen dot_pen;
endfor;

for i = 1 upto 3:
  drawdot point (i*.5) of q8 withpen dot_pen;
endfor;


p34 =  point 1 of q8;

for i = 1 upto 11:
   drawdot (i/12)[p34, p24] withpen dot_pen;
endfor;

%% *** (3) Finish Northeast picture

northeast_picture = currentpicture;

currentpicture := nullpicture;

%% *** (3) Northeast labels

dotlabel.top("24", p24);
dotlabel.top("25", p25);
%dotlabel.top("26", p26);
%dotlabel.top("27", p27);
%dotlabel.top("28", p28);
%dotlabel.top("29", p29);
dotlabel.top("30", p30);
dotlabel.top("31", p31);
dotlabel.top("32", p32);

northeast_label_picture = currentpicture;
currentpicture := nullpicture;

%% *** (3) North_northeast picture

p60 =  (p4 shifted (0, .5(diam_1 - diam_0))) rotated -22.5;

p61 = (p3 -- p16) intersectionpoint (p4 -- p60);

p62 = (p3 -- p24) intersectionpoint (p2 -- p60);

draw p61 -- p60 -- p62;

draw p3 -- p60;

a := angle (p60 - p61);

p63 = (p61 shifted (offset, 0)) rotatedaround (p61, a - 90);

p64 = p60 shifted (p63 - p61);

p65 = 1.5[p64, p63];

p66 = (p65 -- p64) intersectionpoint (p3 -- p60);

p67 = (p65 -- p64) intersectionpoint (p3 -- p16);

a := angle (p60 - p62);

p68 = (p62 shifted (-offset, 0)) rotatedaround (p62, 90 - a);

p69 = whatever[p3, p62];
p69 = whatever[p68, p66];

draw p67 -- p66 -- p69;

%% *** (3) Dots on lines

pickup line_pen;


for i = 0 upto 3:
   drawdot (i/3)[p67, p66] withpen dot_pen;
   drawdot (i/3)[p69, p66] withpen dot_pen;
endfor;   

for i = 1 upto 4:
   drawdot (i/4)[p3, p66] withpen dot_pen;
endfor;   


north_northeast_picture = currentpicture;

currentpicture := nullpicture;

%% *** (3) North_northeast labels

%if false:
if true:
dotlabel.bot("1", p1);
dotlabel.bot("2", p2);
dotlabel.bot("3", p3);
dotlabel.bot("4", p4);
dotlabel.bot("5", p5);
dotlabel.top("60", p60);
dotlabel.lft("61", p61);
dotlabel.top("16", p16);
dotlabel.rt("62", p62);
%dotlabel.urt("63", p63);
%dotlabel.top("64", p64);
%dotlabel.top("65", p65);
dotlabel.top("66", p66);
dotlabel.top("67", p67);
%dotlabel.top("68", p68);
dotlabel.top("69", p69);
fi

north_northeast_label_picture = currentpicture;
currentpicture := nullpicture;

%% ** (2) North_northwest picture
%%
%% I've had to add these pictures because of inaccuracies when 
%% rotating north_northeast_picture.
%% LDF 2009.07.06

pickup line_pen;

p110 = 3[origin, p5];
p111 = (origin -- p110) intersectionpoint q1;

draw p5 -- p111;

p112 = 4[origin, p6];
p113 = (origin -- p112) intersectionpoint q2;

p114 = (p113 -- p5) intersectionpoint (p6 -- p111);

p115 = (p5 -- p16) intersectionpoint (p4 -- p111);

draw p114 -- p111 -- p115;

a := angle (p111 - p114);

p116 = (p114 shifted (offset, 0)) rotatedaround (p114, a - 90);

p117 = (p111 shifted (offset, 0)) rotatedaround (p111, a - 90);

p118 = whatever[p116, p117];
p118 = whatever[p5, p113];

p119 = whatever[p116, p117];
p119 = whatever[p5, p111];

a := angle (p111 - p115);

p120 = (p115 shifted (-offset, 0)) rotatedaround (p115, a - 90);

%show (p119 - p120) / length (p119 - p120);
%show (p111 - p115) / length (p111 - p115);

p121 = whatever[p119, p120];
p121 = whatever[p5, p16];

draw p118 -- p119 -- p121;


%% *** (3) Dots on lines

pickup line_pen;

for i = 0 upto 3:
   drawdot (i/3)[p118, p119] withpen dot_pen;
   drawdot (i/3)[p121, p119] withpen dot_pen;
endfor;   

for i = 1 upto 4:
   drawdot (i/4)[p5, p119] withpen dot_pen;
endfor;   


north_northwest_picture = currentpicture;

currentpicture := nullpicture;


%% *** (3) North_northwest labels

%if false:
if true:
dotlabel.top("110", p110);
dotlabel.lft("111", p111);
dotlabel.top("113", p113);
dotlabel.lft("114", p114);
dotlabel.rt("115", p115);
%dotlabel.top("116", p116);
%dotlabel.top("117", p117);
dotlabel.top("118", p118);
dotlabel.ulft("119", p119);
%dotlabel.top("120", p120);
dotlabel.top("121", p121);
fi

north_northwest_label_picture = currentpicture;
currentpicture := nullpicture;

%% *** (2) Pie-slices (inner circle)

p80 = p4 shifted (0, -offset);

q20 = fullcircle scaled (2 * length p80);

%draw q20 dashed withdots;

p81 = .5[origin, p5];

p82 = ((0, offset) rotated 22.5 shifted p81) rotatedabout (p81, -90);

p83 - p82 = p5 - p81;

p84 = q20 intersectionpoint (p83 -- p82);

p85 = whatever[origin, p80];
p85 = whatever[p84, p82];

p86 = p84 reflectedabout (origin, p80);

draw p84 -- p85 -- p86;

i := p86 intersectiontimes q20;
a := ypart i;

i := p84 intersectiontimes q20;
b := ypart i;

draw subpath (a, b) of q20;


%% *** (3) Dots on lines

for i = 0 upto 4:
   drawdot (i/4)[p85, p84] withpen dot_pen;
   drawdot (i/4)[p85, p86] withpen dot_pen;
endfor;

for i = 1 upto 3:
   drawdot point (a + i * (b - a)/4) of q20 withpen dot_pen;
endfor;


%% *** (3) End of pie-slice picture

pie_slice_picture = currentpicture;
currentpicture := nullpicture;

%% *** (3) Pie-slice labels 

label.top("80", p80);
%dotlabel.lft("81", p81);
%dotlabel.rt("82", p82);
%dotlabel.top("83", p83);
label.top("84", p84);
label.top("85", p85);
label.top("86", p86);

pie_slice_label_picture = currentpicture;
currentpicture := nullpicture;

%% *** (3) Cross-hairs

cross_hair_length = .5cm;

p90 = (-cross_hair_length, 0);
p91 = (cross_hair_length, 0);
p92 = (0, -cross_hair_length);
p93 = (0, cross_hair_length);

pickup line_pen;

draw p90 -- p91;
draw p92 -- p93;

cross_hair_picture = currentpicture;
currentpicture := nullpicture;

%% ** (2) Figure 1

pickup line_pen;

beginfig(1);

  addto currentpicture also circle_picture;
%  addto currentpicture also circle_label_picture;

  addto currentpicture also north_picture;
%  addto currentpicture also north_label_picture;

  addto currentpicture also northeast_picture;
%  addto currentpicture also northeast_label_picture;

  addto currentpicture also north_northeast_picture;
%  addto currentpicture also north_northeast_label_picture;

  addto currentpicture also north_northwest_picture;
%  addto currentpicture also north_northwest_label_picture;

  addto currentpicture also pie_slice_picture;
%  addto currentpicture also pie_slice_label_picture;

  addto currentpicture also cross_hair_picture shifted (0cm, -11cm);
  addto currentpicture also cross_hair_picture shifted (-8cm, -11cm);
  addto currentpicture also cross_hair_picture shifted (8cm, -11cm);

  addto currentpicture also cross_hair_picture shifted (0cm,  11cm);
  addto currentpicture also cross_hair_picture shifted (-8cm, 11cm);
  addto currentpicture also cross_hair_picture shifted (8cm,  11cm);


%% *** (3) Copy 

for i = 1 upto 4:
   addto currentpicture also north_picture rotated (i * 90);
   addto currentpicture also northeast_picture rotated (i * 90);
   addto currentpicture also north_northeast_picture rotated (i * 90);
   addto currentpicture also north_northwest_picture rotated (i * 90);
   addto currentpicture also pie_slice_picture rotated (i * 90);
      addto currentpicture also pie_slice_picture rotated (i * 90 + 45);
 endfor;

%% *** (3) End of Figure 1

endfig;

%% ** (2) Figure 2.  Triangles and pie-slices for cutting out individually

pickup line_pen;

picture north_left_triangle_picture;
picture north_left_triangle_label_picture;

picture north_right_triangle_picture;
picture north_right_triangle_label_picture;

picture northeast_left_triangle_picture;
picture northeast_left_triangle_label_picture;

picture northeast_right_triangle_picture;
picture northeast_right_triangle_label_picture;

picture north_northeast_left_triangle_picture;
picture north_northeast_left_triangle_label_picture;

picture north_northeast_right_triangle_picture;
picture north_northeast_right_triangle_label_picture;

beginfig(2)

%draw q0;  %% Inner circle

draw p5 -- p16;
draw p4 -- p16;

%draw q5;

p50 = p16 shifted (.25cm, 0);
xpart p51 = xpart p50;
ypart p51 = ypart p4 - .5cm;

p52 = (p50 -- p51) intersectionpoint q0;
p53 = (p50 -- p51) intersectionpoint q5;

i := p53 intersectiontimes q5;

a := ypart i;

q9 = subpath(1, b) of q5;

draw q9;

draw p20 -- p22;

drawdot point 1.5 of q5 withpen dot_pen;

%% **** (4) Add dots to lines

drawdot p20 withpen dot_pen;
drawdot p33 withpen dot_pen;

for i = 1 upto 9:
   drawdot (i/10)[p20, p22] withpen dot_pen;
endfor;

for i = 1 upto 11:
   drawdot (i/12)[p33, p16] withpen dot_pen;
endfor;


p54 = (p3 -- p16) intersectionpoint (p50 -- p52);

draw p16 -- p54 -- p52;

q10 = subpath(1.5, 2.75) of q0;

draw q10;

north_left_triangle_picture = currentpicture;
currentpicture := nullpicture;

%% *** (3) Labels (North left triangle)

dotlabel.lrt("3", p3);
dotlabel.bot("4", p4);
dotlabel.ulft("5", p5);
dotlabel.top("6", p6);
dotlabel.top("16", p16);
dotlabel.lft("20", p20);
dotlabel.lft("22", p22);
%dotlabel.rt("23", p23);
dotlabel.ulft("33", p33);
dotlabel.urt("50", p50);
%dotlabel.bot("51", p51);
dotlabel.lrt("52", p52);
dotlabel.urt("53", p53);
dotlabel.rt("54", p54);

north_left_triangle_label_picture  = currentpicture;
currentpicture := nullpicture;

%% *** (3) 

%addto currentpicture also north_left_triangle_picture;
%addto currentpicture also north_left_triangle_label_picture;

temp_picture := north_left_triangle_picture rotated 180 shifted (1.75*(p16 - p4)) shifted (1cm, 0);

addto currentpicture also north_left_triangle_picture;
addto currentpicture also temp_picture;

temp_picture := currentpicture;

for i = 0 upto 5:  
   addto currentpicture also temp_picture shifted (i * 2.5cm, 0cm);
endfor;


%% *** (3) End of Figure 2 (North left triangles)

endfig;

%% ** (2) Figure 3 (North right triangles)

beginfig(3)

north_right_triangle_picture = north_left_triangle_picture reflectedabout (origin, (0, 1));


temp_picture := north_right_triangle_picture rotated 180 shifted (1.75*(p16 - p4)) shifted (1.5cm, 0);

addto currentpicture also north_right_triangle_picture;
addto currentpicture also temp_picture;

temp_picture := currentpicture;

for i = 0 upto 5:  
   addto currentpicture also temp_picture shifted (i * 2.5cm, 0cm);
endfor;


%% *** (3) End of Figure 3 (North right triangles)

endfig;


%% ** (2) Figure 4 (Northeast left triangles)



beginfig(4)

northeast_left_triangle_picture = nullpicture;

addto currentpicture also northeast_left_triangle_picture;

%% *** (3) End of Figure 4 (Northeast left triangles)

endfig;

%% ** (2) Figure 5 (Northeast right triangles)

beginfig(5)

northeast_right_triangle_picture = nullpicture;

addto currentpicture also northeast_right_triangle_picture;

%% *** (3) End of Figure 5 (Northeast right triangles)

endfig;

%% ** (2) Figure 6 (North_northeast left triangles)

beginfig(6)

north_northeast_left_triangle_picture = nullpicture;

addto currentpicture also north_northeast_left_triangle_picture;

%% *** (3) End of Figure 6 (North_northeast left triangles)

endfig;


%% ** (2) Figure 7 (North_northeast right triangles)

beginfig(7)

north_northeast_right_triangle_picture = nullpicture;

addto currentpicture also north_northeast_right_triangle_picture;


%% *** (3) End of Figure 7 (North_northeast right triangles)

endfig;


%% ** (2) Figure 8 (Pie-slices == inner circle)

beginfig(8)

pickup line_pen;

%draw q0;


draw p3 -- origin -- p5;

draw subpath (1, 3) of q0 dashed evenly;
draw subpath (1.5, 2.5) of q0;


p100 = 1.25[origin, p5];
p101 = 1.25[p5, origin];

draw p100 -- p101 dashed evenly;

p102 = 1.25[origin, p3];
p103 = 1.25[p3, origin];

draw p102 -- p103 dashed evenly;


% label.urt("3", p3);
% label.top("4", p4);
% label.ulft("5", p5);


addto pie_slice_picture also currentpicture;

currentpicture := nullpicture;

temp_picture := pie_slice_picture rotated 180 shifted (1cm, .75cm);

for i = 0 upto 5:  
   addto currentpicture also pie_slice_picture shifted (i * 3cm, 0cm);
   addto currentpicture also temp_picture shifted (i * 3cm, 0cm);
endfor;




%% *** (3) End of Figure 8 (Pie-slices == inner circle)

endfig;




%% ** (2) End

end;

%% * (1) Local Variables for Emacs

%% * (1) End of stlln02.mp

