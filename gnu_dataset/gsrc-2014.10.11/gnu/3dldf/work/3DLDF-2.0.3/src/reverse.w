@q reverse.w @> 
@q Created by Laurence Finston Thu Aug 16 12:56:37 CEST 2007  @>
     
@q * Copyright and License.@>

@q This file is part of GNU 3DLDF, a package for three-dimensional drawing. @>
@q Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, @>
@q 2011, 2012, 2013 The Free Software Foundation @>

@q GNU 3DLDF is free software; you can redistribute it and/or modify @>
@q it under the terms of the GNU General Public License as published by @>
@q the Free Software Foundation; either version 3 of the License, or @>
@q (at your option) any later version. @>

@q GNU 3DLDF is distributed in the hope that it will be useful, @>
@q but WITHOUT ANY WARRANTY; without even the implied warranty of @>
@q MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the @>
@q GNU General Public License for more details. @>

@q You should have received a copy of the GNU General Public License @>
@q along with GNU 3DLDF; if not, write to the Free Software @>
@q Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA @>

@q GNU 3DLDF is a GNU package.  @>
@q It is part of the GNU Project of the  @>
@q Free Software Foundation @>
@q and is published under the GNU General Public License. @>
@q See the website http://www.gnu.org @>
@q for more information.   @>
@q GNU 3DLDF is available for downloading from @>
@q http://www.gnu.org/software/3dldf/LDF.html. @>

@q (``@@'' stands for a single at-sign in the following paragraph.) @>

@q Please send bug reports to Laurence.Finston@@gmx.de @>
@q The mailing list help-3dldf@@gnu.org is available for people to @>
@q ask other users for help.  @>
@q The mailing list info-3dldf@@gnu.org is for sending @>
@q announcements to users. To subscribe to these mailing lists, send an @>
@q email with ``subscribe <email-address>'' as the subject.  @>

@q The author can be contacted at: @>

@q Laurence D. Finston                 @> 
@q c/o Free Software Foundation, Inc.  @>
@q 51 Franklin St, Fifth Floor         @> 
@q Boston, MA  02110-1301              @>
@q USA                                 @>

@q Laurence.Finston@@gmx.de (@@ stands for a single ``at'' sign.)@>



@q * (0) Reverse Command.  @>
@** Reverse Command.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Created this file.  
\ENDLOG

@q * (1) command --> REVERSE boolean_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§boolean vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: REVERSE boolean_vector_variable@>@/
{

   Scan_Parse::reverse_func<bool>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE string_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§string vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: REVERSE string_vector_variable@>@/
{

   Scan_Parse::reverse_func<string>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE numeric_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§numeric vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: REVERSE numeric_vector_variable@>@/
{

   Scan_Parse::reverse_func<real>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE ulong_long_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§ulong long vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: REVERSE ulong_long_vector_variable@>@/
{

   Scan_Parse::reverse_func<unsigned long long>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE transform_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§transform vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: REVERSE transform_vector_variable@>@/
{

   Scan_Parse::reverse_func<Transform>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE color_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§color vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: REVERSE color_vector_variable@>@/
{

   Scan_Parse::reverse_func<Color>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE pen_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§pen vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: REVERSE pen_vector_variable@>@/
{

   Scan_Parse::reverse_func<Pen>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE dash_pattern_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§dash pattern vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: REVERSE dash_pattern_vector_variable@>@/
{

   Scan_Parse::reverse_func<Dash_Pattern>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE focus_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§focus vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: REVERSE focus_vector_variable@>@/
{

   Scan_Parse::reverse_func<Focus>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE picture_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§picture vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: REVERSE picture_vector_variable@>@/
{

   Scan_Parse::reverse_func<Picture>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE point_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§point vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: REVERSE point_vector_variable@>@/
{

   Scan_Parse::reverse_func<Point>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE bool_point_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§bool point vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: REVERSE bool_point_vector_variable@>@/
{

   Scan_Parse::reverse_func<Bool_Point>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE nurb_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§nurb vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: REVERSE nurb_vector_variable@>@/
{

   Scan_Parse::reverse_func<Nurb>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE path_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§path vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: REVERSE path_vector_variable@>@/
{

   Scan_Parse::reverse_func<Path>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE triangle_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§triangle vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: REVERSE triangle_vector_variable@>@/
{

   Scan_Parse::reverse_func<Triangle>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE polygon_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§polygon vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: REVERSE polygon_vector_variable@>@/
{

   Scan_Parse::reverse_func<Polygon>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE reg_polygon_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§reg polygon vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: REVERSE reg_polygon_vector_variable@>@/
{

   Scan_Parse::reverse_func<Reg_Polygon>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE rectangle_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§rectangle vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: REVERSE rectangle_vector_variable@>@/
{

   Scan_Parse::reverse_func<Rectangle>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE ellipse_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§ellipse vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: REVERSE ellipse_vector_variable@>@/
{

   Scan_Parse::reverse_func<Ellipse>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE circle_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§circle vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: REVERSE circle_vector_variable@>@/
{

   Scan_Parse::reverse_func<Circle>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE conic_section_lattice_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§conic section lattice vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: REVERSE conic_section_lattice_vector_variable@>@/
{

   Scan_Parse::reverse_func<Conic_Section_Lattice>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE helix_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§helix vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: REVERSE helix_vector_variable@>@/
{

   Scan_Parse::reverse_func<Helix>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE cuboid_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§cuboid vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: REVERSE cuboid_vector_variable@>@/
{

   Scan_Parse::reverse_func<Cuboid>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE polyhedron_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§polyhedron vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: REVERSE polyhedron_vector_variable@>@/
{

   Scan_Parse::reverse_func<Polyhedron>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE cone_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§cone vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: REVERSE cone_vector_variable@>@/
{

   Scan_Parse::reverse_func<Cone>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE cylinder_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§cylinder vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: REVERSE cylinder_vector_variable@>@/
{

   Scan_Parse::reverse_func<Cylinder>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE ellipsoid_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§ellipsoid vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: REVERSE ellipsoid_vector_variable@>@/
{

   Scan_Parse::reverse_func<Ellipsoid>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE sphere_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§sphere vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: REVERSE sphere_vector_variable@>@/
{

   Scan_Parse::reverse_func<Sphere>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE parabola_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§parabola vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: REVERSE parabola_vector_variable@>@/
{

   Scan_Parse::reverse_func<Parabola>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE hyperbola_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§hyperbola vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: REVERSE hyperbola_vector_variable@>@/
{

   Scan_Parse::reverse_func<Hyperbola>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE paraboloid_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§paraboloid vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: REVERSE paraboloid_vector_variable@>@/
{

   Scan_Parse::reverse_func<Paraboloid>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE hyperboloid_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§hyperboloid vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.  It's currently commented-out.
\ENDLOG

@q **** (4) Definition.@> 

@<Garbage@>=
@=command: REVERSE hyperboloid_vector_variable@>@/
{

   Scan_Parse::reverse_func<Hyperboloid>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE parabola_slice_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§parabola slice vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.  It's currently commented-out.
\ENDLOG

@q **** (4) Definition.@> 

@<Garbage@>=
@=command: REVERSE parabola_slice_vector_variable@>@/
{

   Scan_Parse::reverse_func<Parabola_Slice>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE hyperbola_slice_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§hyperbola slice vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.  It's currently commented-out.
\ENDLOG

@q **** (4) Definition.@> 

@<Garbage@>=
@=command: REVERSE hyperbola_slice_vector_variable@>@/
{

   Scan_Parse::reverse_func<Hyperbola_Slice>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE paraboloid_slice_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§paraboloid slice vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.  It's currently commented-out.
\ENDLOG

@q **** (4) Definition.@> 

@<Garbage@>=
@=command: REVERSE paraboloid_slice_vector_variable@>@/
{

   Scan_Parse::reverse_func<Paraboloid_Slice>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE hyperboloid_slice_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§hyperboloid slice vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.  It's currently commented-out.
\ENDLOG

@q **** (4) Definition.@> 

@<Garbage@>=
@=command: REVERSE hyperboloid_slice_vector_variable@>@/
{

   Scan_Parse::reverse_func<Hyperboloid_Slice>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE glyph_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§glyph vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: REVERSE glyph_vector_variable@>@/
{

   Scan_Parse::reverse_func<Glyph>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE plane_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§plane vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: REVERSE plane_vector_variable@>@/
{

   Scan_Parse::reverse_func<Plane>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE origami_figure_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§origami figure vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: REVERSE origami_figure_vector_variable@>@/
{

   Scan_Parse::reverse_func<Origami_Figure>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE ellipse_slice_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§ellipse slice vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: REVERSE ellipse_slice_vector_variable@>@/
{

   Scan_Parse::reverse_func<Ellipse_Slice>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE circle_slice_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§circle slice vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: REVERSE circle_slice_vector_variable@>@/
{

   Scan_Parse::reverse_func<Circle_Slice>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE helix_slice_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§helix slice vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.  It's currently commented-out.
\ENDLOG

@q **** (4) Definition.@> 

@<Garbage@>=
@=command: REVERSE helix_slice_vector_variable@>@/
{

   Scan_Parse::reverse_func<Helix_Slice>(@=$2@>, parameter);

};
@q * (1) command --> REVERSE polyhedron_slice_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§polyhedron slice vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: REVERSE polyhedron_slice_vector_variable@>@/
{

   Scan_Parse::reverse_func<Polyhedron_Slice>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE cone_slice_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§cone slice vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.  It's currently commented-out.
\ENDLOG

@q **** (4) Definition.@> 

@<Garbage@>=
@=command: REVERSE cone_slice_vector_variable@>@/
{

   Scan_Parse::reverse_func<Cone_Slice>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE cylinder_slice_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§cylinder slice vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.  It's currently commented-out.
\ENDLOG

@q **** (4) Definition.@> 

@<Garbage@>=
@=command: REVERSE cylinder_slice_vector_variable@>@/
{

   Scan_Parse::reverse_func<Cylinder_Slice>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE ellipsoid_slice_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§ellipsoid slice vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.  It's currently commented-out.
\ENDLOG

@q **** (4) Definition.@> 

@<Garbage@>=
@=command: REVERSE ellipsoid_slice_vector_variable@>@/
{

   Scan_Parse::reverse_func<Ellipsoid_Slice>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE sphere_slice_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§sphere slice vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.  It's currently commented-out.
\ENDLOG

@q **** (4) Definition.@> 

@<Garbage@>=
@=command: REVERSE sphere_slice_vector_variable@>@/
{

   Scan_Parse::reverse_func<Sphere_Slice>(@=$2@>, parameter);

};

@q * (1) command --> REVERSE glyph_slice_vector_variable.@> 
@* \§command> $\longrightarrow$ \.{REVERSE} 
\§glyph slice vector variable>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.  It's currently commented-out.
\ENDLOG

@q **** (4) Definition.@> 

@<Garbage@>=
@=command: REVERSE glyph_slice_vector_variable@>@/
{

   Scan_Parse::reverse_func<Glyph_Slice>(@=$2@>, parameter);

};


@q * Emacs-Lisp code for use in indirect buffers when using the          @>
@q   GNU Emacs editor.  The local variable list is not evaluated when an @>
@q   indirect buffer is visited, so it's necessary to evaluate the       @>
@q   following s-expression in order to use the facilities normally      @>
@q   accessed via the local variables list.                              @>
@q   \initials{LDF 2004.02.12}.                                          @>
@q   (progn (cweb-mode) (outline-minor-mode t) (setq fill-column 80))    @>


@q Local Variables: @>
@q mode:CWEB  @>
@q eval:(outline-minor-mode t)  @>
@q abbrev-file-name:"~/.abbrev_defs" @>
@q eval:(read-abbrev-file)  @>
@q fill-column:70 @>
@q run-cweave-on-file:"3DLDFprg.web"  @>
@q End: @>
