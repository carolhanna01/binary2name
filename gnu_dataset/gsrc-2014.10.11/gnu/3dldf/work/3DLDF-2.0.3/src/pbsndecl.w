@q pbsndecl.w @> 
@q Created by Laurence Finston Thu Apr 29 13:12:18 MEST 2004 @>
     
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



@q * (0) Declarations.  @>
@** Declarations.  

\LOG
\initials{LDF 2004.04.29.}  Created this file.  It contains code formerly
contained in \filename{parser.w} and is included in that file by means of 
CWEB's ``\.{@@i}'' command.
\ENDLOG 

@q * (1) Include files.  @>
@* Include files.

\LOG
\initials{LDF 2004.04.16.}  No longer including the library header
file \filename{map} here.  It's now included in
\filename{loader.web}.

\initials{LDF 2004.05.21.}  Now including \filename{pens.web}.

\initials{LDF 2004.07.18.}
@:BUG FIX@> BUG FIX: 
No longer including \filename{main.h}.  \filename{parser.w} is 
processed before \filename{main.web}, so this caused an error 
when I tried to build 3DLDF from scratch.
\ENDLOG 

@<Include files@>=
#include "config.h"

#include <string.h>

#include "loader.h++"

#include <time.h>
 
#include "pspglb.h++"
#include "io.h++"
#include "gsltmplt.h++"
#include "creatnew.h++"
#include "pntrvctr.h++"
#include "primes.h++"
#include "complex.h++"
#include "matrices.h++"
#include "colors.h++"
#include "transfor.h++"
#include "pens.h++"
#include "dashptrn.h++"
#include "shapes.h++" 
#include "pictures.h++"
#include "points.h++"
#include "lines.h++"
#include "planes.h++"
#include "nurbs.h++"
#include "paths.h++"
#include "curves.h++"
#include "polygons.h++"
#include "triangle.h++"
#include "rectangs.h++"
#include "conicsct.h++"
#include "ellipses.h++"
#include "circles.h++"
#include "ellpsslc.h++"
#include "crclslc.h++"
#include "parabola.h++"
#include "hyprbola.h++"
#include "cncsctlt.h++"
#include "arc.h++"
#include "helices.h++"
#include "origami.h++"

#if 0 
   #include "patterns.h++"
#endif 

#include "solids.h++"
#include "solfaced.h++"
#include "cuboid.h++"
#include "polyhed.h++"
#include "ddchdrn.h++"
#include "rhtchdrn.h++"
#include "plyhdslc.h++"
#include "cones.h++"
#include "cylinder.h++" 
#include "ellpsoid.h++"
#include "spheres.h++" 
#include "sphrdevl.h++"
#include "parabold.h++"
#include "paraellp.h++"
#include "parahypr.h++" 
#include "glyphs.h++"
#include "pctfncs0.h++"
#include "utility.h++"
#include "pntrvcf0.h++"
#include "predctes.h++"
#include "scanprse.h++"

@q * (1) Using declarations.  @>
@* Using declarations.
@<Using declarations@>=
using namespace std;
using namespace Scan_Parse;@/


@q * (1) Location type.  @>
@* Location type.
@<Declare location type@>=
struct 
LDF_LOCATION_TYPE
{
  unsigned int first_line; 
  unsigned int first_column; 
  unsigned int last_line; 
  unsigned int last_column; 
  unsigned int position;
  @<Declare |LDF_LOCATION_TYPE| functions@>@;
};

@q ** (2) |LDF_LOCATION_TYPE| functions.  @>
@*1 {\bf LDF\_LOCATION\_TYPE} functions.

@q *** (3) Constructor.  @>
@*2 Constructor.
@<Declare |LDF_LOCATION_TYPE| functions@>=
LDF_LOCATION_TYPE(void);

@
@<Define |LDF_LOCATION_TYPE| functions@>=
LDF_LOCATION_TYPE::LDF_LOCATION_TYPE(void)
{
 first_line = last_line = position = 1;
 first_column = last_column = 0;
 return;
}
 
@q * (1) Preprocessor macros not only for the parser.  @>
@* Preprocessor macros not only for the parser.
\initials{LDF Undated.}

\LOG
\initials{LDF 2005.10.20.}
Now setting |DEBUG_COMPILE_SAVE| to |DEBUG_COMPILE| and |DEBUG_COMPILE| to 0.

\initials{LDF 2005.10.22.}
@:BUG FIX@> BUG FIX:  Fixed the way I set |DEBUG_COMPILE_SAVE| 
to |DEBUG_COMPILE|.  The way it was before didn't work, 
because the bodies of macros are not expanded in 
definitions, but only when the macros are expanded.
\ENDLOG

@<Preprocessor macros not only for the parser@>=

#if 0 
#define YYPARSE_PARAM parameter
#define YYLEX_PARAM parameter
#define YYLTYPE LDF_LOCATION_TYPE
#define YYLOC_DEFAULT(Current, Rhs, N) /* Do nothing.  */
#endif 

@q #if DEBUG_COMPILE @>
@q #define DEBUG_COMPILE_SAVE 1 @>
@q #else @>
@q #define DEBUG_COMPILE_SAVE 0 @>
@q #endif @>

@q #undef DEBUG_COMPILE @>
@q #define DEBUG_COMPILE 0 @>


@q * (1) Preprocessor macros for the parser only.  @>
@* Preprocessor macros for the parser only.
@<Preprocessor macros for the parser only@>=

#if 0 
#define YYERROR_VERBOSE
#endif 


@q * (1) Bison declarations.  @>
@* Bison declarations.

If |@=%token_table@>| is declared, Bison generates 
``an array of token names in the parser file.''%
%
\fnote{Donnelly, Charles, and Richard Stallman. 
{\it Bison.  The YACC-compatible Parser Generator}, p.~39.}
%
Unfortunately, it's |static|, so it's not usable in files that include
\filename{parser.h} and \filename{parser.h++}, like \filename{scan.web}.
Therefore it's still necessary to declare |name_map| in 
|@<Declare |name_map| and |type_name_map|@>|.  However, |@=%token_table@>| also 
generates macro definitions for the following macros: |YYNTOKENS|, |YYNNTS|, 
|YYNRULES|, and |YYNSTATES|, so it might be worthwhile to declare it after all.  
It is currently commented out.%
\fnote{Ibid, p.~40.}
%
\initials{LDF 2004.04.25.}

\LOG
\initials{LDF 2004.03.09.}  
Added this section.
\ENDLOG 

@<Bison declarations@>=
@=%defines@>@/
@=%debug@>@/
@q =%expect 1@>@/
@q =%locations@>@/
@q =%glr-parser@>@/
@=%pure_parser@>@/
@=%parse-param {yyscan_t parameter}@>@;@/
@=%lex-param {yyscan_t parameter}@>@;@/
@=/* %token_table */@>@/ 
@=%verbose@>

@q * (1) Type declarations.  @>
@* Type declarations.

@q ** (2) Union declaration.  This is the type of |YYSTYPE|.  @>
@*1 Union declaration for |YYSTYPE|.
\initials{LDF Undated.}

\LOG
\initials{LDF 2005.12.05.}
Added |unsigned long long ulong_long_value|.
\ENDLOG 

@<Union declaration for |YYSTYPE|@>=

@=%union@>
{
  char string_value[64];
  double real_value;
  signed int int_value;
  unsigned long long ulong_long_value;
  void* pointer_value;
};


@q * (1) Token and precedence declarations.  @>
@* Token and precedence declarations.

I don't believe it's necessary to specify the precedence of operators, because
this is implicit in the structure of the rules, with ``the four-fold 
`primary, secondary, tertiary, expression' hierarchy $[\ldots]$''%
%
\fnote{Knuth, {\it The METAFONTbook}, p.~209.}.
%
However, it may be necessary to specify their associativity, and the ordering of
the |@=%left@>|, |@=%right@>|, and |@=%nonassoc@>| declarations automatically
specify their precedence.
\initials{LDF 2004.04.27.}

\TODO
@q { @>
@:TO DO}{{\bf TO DO}@>
@q } @> 
@q !! TO DO:@>  
Add formatting commands.  
\initials{LDF 2004.04.22.}

\initials{LDF 2004.04.27.}
Make sure these declarations work properly.
\ENDTODO 
 

@f terminal_symbol int
@f non_terminal_symbol terminal_symbol 

@q ** (2) Basic types.  @>
@*1 Basic types.

\LOG
\initials{LDF 2004.05.13.}  
Added token declarations for
|AMPERSAND|, |CYCLE|, |PERIOD_PAIR|, |PERIOD_TRIPLE|, |HYPHEN_PAIR|, and  
|HYPHEN_TRIPLE|,

\initials{LDF 2004.05.13.}  
Added token declarations for |REVERSE|, |SUBPATH|, and |OF|.  

\initials{LDF 2004.06.07.}  
Changed |DASH_PATTERN| to |DASH_PATTERN_DECLARATOR|
in token declaration.

\initials{LDF 2004.09.10.}
Added token declaration for |COLON|.

\initials{LDF 2005.01.02.}
Replaced token declaration for |CURLY_BRACES| with  
ones for |LEFT_BRACE| and |RIGHT_BRACE|.

\initials{LDF 2005.12.06.}
Added token declaration for |ULONG_LONG_TYPE|.
\ENDLOG 

@f END terminal_symbol
@f numeric_token non_terminal_symbol

@<Token and precedence declarations@>=


@=%token <int_value> NULL_VALUE@>@/
@=%token <int_value> TERMINATE@>@/      
@=%token <int_value> INVALID_CATCODE@>@/
@=%token <int_value> INVALID_CHAR@>@/

@=%token <string_value> SPACE@>@/

@=%token <string_value> AMPERSAND@>@/
@=%token <string_value> PERIOD@>@/
@=%token <string_value> PERIOD_PAIR@>@/
@=%token <string_value> PERIOD_TRIPLE@>@/
@=%token <string_value> HYPHEN@>@/
@=%token <string_value> HYPHEN_PAIR@>@/
@=%token <string_value> HYPHEN_TRIPLE@>@/

@=%token <int_value>    CYCLE@>@/
@=%token <int_value>    REVERSE@>@/
@=%token <int_value>    SUBPATH@>@/
@=%token <int_value>    OF@>@/

@=%token <string_value> COMMA@>@/
@=%token <string_value> COLON@>@/ 
@=%token <string_value> SEMI_COLON@>@/ 
@=%token <string_value> LEFT_PARENTHESIS@>@/
@=%token <string_value> RIGHT_PARENTHESIS@>@/
@=%token <string_value> DOUBLE_QUOTE@>@/
@=%token <string_value> LEFT_BRACKET@>@/
@=%token <string_value> RIGHT_BRACKET@>@/
@=%token <string_value> LEFT_BRACE@>@/
@=%token <string_value> RIGHT_BRACE@>@/
@=%token <string_value> DIGIT@>@/
@=%token <int_value>    INTEGER@>@/        
@=%token <ulong_long_value> ULONG_LONG_TYPE@>@/
@=%token <real_value>   REAL@>@/          
@=%token <string_value> PERCENT@>@/
@=%token <string_value> SYMBOL_0@>@/
@=%token <string_value> SYMBOL_1@>@/
@=%token <string_value> SYMBOL_2@>@/
@=%token <string_value> SYMBOL_3@>@/
@=%token <string_value> SYMBOL_4@>@/
@=%token <string_value> SYMBOL_5@>@/
@=%token <string_value> SYMBOL_6@>@/
@=%token <string_value> SYMBOL_7@>@/

@q ** (2)  Object types defined in 3DLDF and |UNDECLARED|.@>  

@*1 Object types defined in 3DLDF and |UNDECLARED|.  
I plan to use the latter in 
{\bf save} commands. 
\initials{LDF 2004.04.23.}

\LOG
\initials{LDF 2004.04.22.}  
Added this section.

\initials{LDF 2004.04.23.}  
Added |UNKNOWN_TYPE|.

\initials{LDF 2004.05.03.}  
Changed |UNKNOWN_TYPE| to |UNDECLARED| and 
values of the types in this section from |string_value| to |pointer_value|.
|NUMERIC| was already a |pointer_value|.

\initials{LDF 2004.05.17.}  
Added token declaration for |BOOLEAN|.

\initials{LDF 2004.05.21.}  
Added token declarations for |PEN|, |COLOR|,
|FOCUS|, and |PICTURE|.

\initials{LDF 2004.07.01.}
Added token declaration for |SYNONYM|.

\initials{LDF 2004.08.23.}
Added token declaration for |COLOR_VECTOR|.

\initials{LDF 2004.08.27.}
Added token declaration for |POINT_VECTOR|.

\initials{LDF 2004.09.01.}
Added token declarations for |BOOL_POINT| and |BOOL_POINT_VECTOR|.

\initials{LDF 2004.12.10.}
Added token declarations for |PATH_VECTOR|, 
|TRIANGLE_VECTOR|, |REG_POLYGON_VECTOR|, 
|RECTANGLE_VECTOR|, |SQUARE_VECTOR|, 
|ELLIPSE_VECTOR|, |CIRCLE_VECTOR|,  |CUBOID_VECTOR|, 
|TETRAHEDRON_VECTOR|, |OCTAHEDRON_VECTOR|, |DODECAHEDRON_VECTOR|, 
|ICOSAHEDRON_VECTOR|, and |TRUNC_OCTAHEDRON_VECTOR|.

\initials{LDF 2004.12.13.}
Added token declarations for 
|BOOLEAN_VECTOR|, 
|STRING_VECTOR|, 
|NUMERIC_VECTOR|, 
|PEN_VECTOR|, 
|DASH_PATTERN_VECTOR|, 
|PICTURE_VECTOR|, 
|TRANSFORM_VECTOR|, and
|FOCUS_VECTOR|.

\initials{LDF 2005.01.26.}
Added token declarations for |NURB| and |NURB_VECTOR|.

\initials{LDF 2005.02.03.}
Added token declarations for |ORIGAMI_FIGURE| and 
|ORIGAMI_FIGURE_VECTOR|.

\initials{LDF 2005.02.11.}
Added token declarations for |POLYGON| and 
|POLYGON_VECTOR|.

\initials{LDF 2005.03.29.}
Added token declarations for 
|ELLIPSE_SLICE|, |CIRCLE_SLICE|, |POLYHEDRON_SLICE|, 
|EL\-LIPSE_SLICE_VEC\-TOR|, |CIRCLE_SLICE_VECTOR|, and 
|POLYHEDRON_SLICE_VECTOR|.

\initials{LDF 2005.05.18.}
Added token declarations for |HELIX| and |HELIX_VECTOR|.

\initials{LDF 2005.05.24.}
Added token declarations for 
|HELIX_SLICE|, |HELIX_SLICE_VECTOR|,
|CONE|, |CONE_VECTOR|, |CONE_SLICE|, |CONE_SLICE_VECTOR|,
|CYLINDER|, |CYLINDER_VECTOR|,
|CYLINDER_SLICE|, |CYLINDER_SLICE_VECTOR|,
|ELLIPSOID|, |ELLIPSOID_VECTOR|, |ELLIPSOID_SLICE|, 
|ELLIPSOID_SLICE_VECTOR|,
|SPHERE|, |SPHERE_VECTOR|, |SPHERE_SLICE|, 
and |SPHERE_SLICE_VECTOR|.

\initials{LDF 2005.06.08.}
Added token declarations for |GLYPH|, |GLYPH_VECTOR|,
|GLYPH_SLICE|, and |GLYPH_SLICE_VECTOR|.

\initials{LDF 2005.10.30.}
Added token declarations for |PLANE| and |PLANE_VECTOR|.

\initials{LDF 2005.11.07.}
Added token declarations for |PARABOLA|, |HYPERBOLA|,
|PARABOLOID|,  |HYPERBOLOID|, |PARABOLA_VECTOR|, |HYPERBOLA_VECTOR|,
|PARABOLOID_VECTOR|, |HYPERBOLOID_VECTOR|,
|PARABOLA_SLICE|, |HYPERBOLA_SLICE|, |PARABOLOID_SLICE|, 
|HYPERBOLOID_SLICE|, |PARABOLA_SLICE_VECTOR|, 
|HYPERBOLA_SLICE_VECTOR|, |PARABOLOID_SLICE_VECTOR|, 
and |HYPERBOLOID_SLICE_VECTOR|.

\initials{LDF 2005.12.05.}
Added token declarations for |ULONG_LONG| and |ULONG_LONG_VECTOR|.

\initials{LDF 2007.07.29.}
Added token declarations for |CONIC_SECTION_LATTICE| and 
|CONIC_SECTION_LATTICE_VECTOR|.

\initials{LDF 2007.10.13.}
Added token declarations for |ARC| and |ARC_VECTOR|.

\initials{LDF 2007.11.28.}
Added token declaration for |PREDICATE|.

\initials{LDF 2007.11.28.}
Added token declarations for |MATRIX| and |MATRIX_VECTOR|.

\initials{LDF 2007.12.02.}
Added token declarations for |COMPLEX| and |COMPLEX_VECTOR|.

\initials{LDF 2009.11.04.}
Added token declarations for |SPHERE_DEVELOPMENT| and |SPHERE_DEVELOPMENT_VECTOR|.
\ENDLOG 

@q ** (2) Code.@> 

@<Token and precedence declarations@>=

@=%token <pointer_value> SYNONYM@>@/
@=%token <pointer_value> UNDECLARED@>@/

@=%token <pointer_value> PREDICATE@>@/

@=%token <pointer_value> BOOLEAN@>@/
@=%token <pointer_value> COMPLEX@>@/
@=%token <pointer_value> MATRIX@>@/
@=%token <pointer_value> STRING@>@/
@=%token <pointer_value> NUMERIC@>@/
@=%token <pointer_value> ULONG_LONG@>@/
@=%token <pointer_value> PEN@>@/
@=%token <pointer_value> DASH_PATTERN@>@/
@=%token <pointer_value> COLOR@>@/
@=%token <pointer_value> PICTURE@>@/
@=%token <pointer_value> TRANSFORM@>@/
@=%token <pointer_value> FOCUS@>@/

@=%token <pointer_value> POINT@>@/
@=%token <pointer_value> BOOL_POINT@>@/
@=%token <pointer_value> NURB@>@/
@=%token <pointer_value> PATH@>@/
@=%token <pointer_value> TRIANGLE@>@/
@=%token <pointer_value> POLYGON@>@/
@=%token <pointer_value> REG_POLYGON@>@/
@=%token <pointer_value> RECTANGLE@>@/
@=%token <pointer_value> SQUARE@>@/
@=%token <pointer_value> ELLIPSE@>@/
@=%token <pointer_value> CIRCLE@>@/
@=%token <pointer_value> CONIC_SECTION_LATTICE@>@/
@=%token <pointer_value> HELIX@>@/
@=%token <pointer_value> CUBOID@>@/
@=%token <pointer_value> POLYHEDRON@>@/
@=%token <pointer_value> CONE@>@/
@=%token <pointer_value> CYLINDER@>@/
@=%token <pointer_value> ELLIPSOID@>@/
@=%token <pointer_value> SPHERE@>@/
@=%token <pointer_value> SPHERE_DEVELOPMENT@>@/

@=%token <pointer_value> PARABOLA@>@/
@=%token <pointer_value> HYPERBOLA@>@/

@=%token <pointer_value> ARC@>@/

@=%token <pointer_value> PARABOLOID@>@/
@=%token <pointer_value> HYPERBOLOID@>@/

@=%token <pointer_value> GLYPH@>@/

@=%token <pointer_value> PLANE@>@/

@=%token <pointer_value> ELLIPSE_SLICE@>@/
@=%token <pointer_value> CIRCLE_SLICE@>@/
@=%token <pointer_value> HELIX_SLICE@>@/
@=%token <pointer_value> POLYHEDRON_SLICE@>@/
@=%token <pointer_value> CONE_SLICE@>@/
@=%token <pointer_value> CYLINDER_SLICE@>@/
@=%token <pointer_value> ELLIPSOID_SLICE @>@/
@=%token <pointer_value> SPHERE_SLICE@>@/


@=%token <pointer_value> PARABOLA_SLICE@>@/
@=%token <pointer_value> HYPERBOLA_SLICE@>@/
@=%token <pointer_value> PARABOLOID_SLICE@>@/
@=%token <pointer_value> HYPERBOLOID_SLICE@>@/

@=%token <pointer_value> GLYPH_SLICE@>@/

@=%token <pointer_value> ELLIPSE_SLICE_VECTOR@>@/
@=%token <pointer_value> CIRCLE_SLICE_VECTOR@>@/
@=%token <pointer_value> HELIX_SLICE_VECTOR@>@/
@=%token <pointer_value> POLYHEDRON_SLICE_VECTOR@>@/
@=%token <pointer_value> CONE_SLICE_VECTOR@>@/
@=%token <pointer_value> CYLINDER_SLICE_VECTOR@>@/
@=%token <pointer_value> ELLIPSOID_SLICE_VECTOR@>@/
@=%token <pointer_value> SPHERE_SLICE_VECTOR@>@/

@=%token <pointer_value> PARABOLA_SLICE_VECTOR@>@/
@=%token <pointer_value> HYPERBOLA_SLICE_VECTOR@>@/
@=%token <pointer_value> PARABOLOID_SLICE_VECTOR@>@/
@=%token <pointer_value> HYPERBOLOID_SLICE_VECTOR@>@/

@=%token <pointer_value> GLYPH_SLICE_VECTOR@>@/

@=%token <pointer_value> BOOLEAN_VECTOR@>@/ 
@=%token <pointer_value> COMPLEX_VECTOR@>@/ 
@=%token <pointer_value> MATRIX_VECTOR@>@/ 

@=%token <pointer_value> STRING_VECTOR@>@/ 
@=%token <pointer_value> NUMERIC_VECTOR@>@/ 
@=%token <pointer_value> ULONG_LONG_VECTOR@>@/ 
@=%token <pointer_value> COLOR_VECTOR@>@/
@=%token <pointer_value> PEN_VECTOR@>@/ 
@=%token <pointer_value> DASH_PATTERN_VECTOR@>@/ 
@=%token <pointer_value> PICTURE_VECTOR@>@/ 
@=%token <pointer_value> TRANSFORM_VECTOR@>@/ 
@=%token <pointer_value> FOCUS_VECTOR@>@/ 

@=%token <pointer_value> POINT_VECTOR@>@/
@=%token <pointer_value> BOOL_POINT_VECTOR@>@/
@=%token <pointer_value> NURB_VECTOR@>@/
@=%token <pointer_value> PATH_VECTOR@>@/
@=%token <pointer_value> TRIANGLE_VECTOR@>@/
@=%token <pointer_value> POLYGON_VECTOR@>@/
@=%token <pointer_value> REG_POLYGON_VECTOR@>@/
@=%token <pointer_value> RECTANGLE_VECTOR@>@/
@=%token <pointer_value> SQUARE_VECTOR@>@/
@=%token <pointer_value> ELLIPSE_VECTOR@>@/
@=%token <pointer_value> CIRCLE_VECTOR@>@/
@=%token <pointer_value> CONIC_SECTION_LATTICE_VECTOR@>@/
@=%token <pointer_value> HELIX_VECTOR@>@/
@=%token <pointer_value> CUBOID_VECTOR@>@/
@=%token <pointer_value> POLYHEDRON_VECTOR@>@/
@=%token <pointer_value> CONE_VECTOR@>@/
@=%token <pointer_value> CYLINDER_VECTOR@>@/
@=%token <pointer_value> ELLIPSOID_VECTOR@>@/
@=%token <pointer_value> SPHERE_VECTOR@>@/
@=%token <pointer_value> SPHERE_DEVELOPMENT_VECTOR@>@/


@=%token <pointer_value> PARABOLA_VECTOR@>@/
@=%token <pointer_value> HYPERBOLA_VECTOR@>@/

@=%token <pointer_value> ARC_VECTOR@>@/

@=%token <pointer_value> PARABOLOID_VECTOR@>@/
@=%token <pointer_value> HYPERBOLOID_VECTOR@>@/



@=%token <pointer_value> GLYPH_VECTOR@>@/

@=%token <pointer_value> PLANE_VECTOR@>@/

@=%token <pointer_value> ORIGAMI_FIGURE@>@/
@=%token <pointer_value> ORIGAMI_FIGURE_VECTOR@>@/


@q ** (2)  Declarators for object types defined in 3DLDF.@>
@*1 Declarators for object types defined in 3DLDF.

\LOG
\initials{LDF 2004.04.21.}  
Added this section.

\initials{LDF 2004.05.17.}  
Added token declaration for |BOOLEAN_DECLARATOR|.

\initials{LDF 2004.05.19.}  
Added token declaration for |STRING_DECLARATOR|.

\initials{LDF 2004.05.19.}  
Added token declarations for |COLOR_DECLARATOR|,
|PEN_DECLARATOR|, |FOCUS_DECLARATOR|, and |PICTURE_DECLARATOR|.

\initials{LDF 2004.08.23.}
Added token declaration for |COLOR_VECTOR_DECLARATOR|.

\initials{LDF 2004.08.27.}
Added token declaration for |POINT_VECTOR_DECLARATOR|.

\initials{LDF 2004.09.01.}
Added token declarations for |BOOL_POINT_DECLARATOR|
and |BOOL_POINT_VECTOR_DECLARATOR|.

\initials{LDF 2004.12.10.}
Added token declarations for |PATH_VECTOR_DECLARATOR|, 
|TRIANGLE_VECTOR_DECLARATOR|, |REG_POLYGON_VECTOR_DECLARATOR|, 
|RECTANGLE_VECTOR_DECLARATOR|, |SQUARE_VECTOR_DECLARATOR|, 
|ELLIPSE_VECTOR_DECLARATOR|, |CIRCLE_VECTOR_DECLARATOR|,  
|CUBOID_VECTOR_DECLARATOR|, |TETRAHEDRON_VECTOR_DECLARATOR|, 
|OCTAHEDRON_VECTOR_DECLARATOR|, |DODECAHEDRON_VECTOR_DECLARATOR|, 
|ICOSAHEDRON_VECTOR_DECLARATOR|, and 
|TRUNC_OCTAHEDRON_VECTOR_DECLARATOR|.

\initials{LDF 2004.12.11.}
Moved tokens declarations for vector-type declarators to their 
own section, below. 

\initials{LDF 2005.01.26.}
Added token declaration for |NURB_DECLARATOR|.

\initials{LDF 2005.02.03.}
Added token declaration for |ORIGAMI_FIGURE_DECLARATOR|.

\initials{LDF 2005.02.11.}
Added token declaration for |POLYGON__DECLARATOR|.

\initials{LDF 2005.03.29.}
Added token declarations for |ELLIPSE_SLICE_DECLARATOR|,
|CIRCLE_SLICE_DECLARATOR|, |POLYHEDRON_SLICE_DECLARATOR|,
|ELLIPSE_SLICE_VECTOR_DECLARATOR|,
|CIRCLE_SLICE_VECTOR_DECLARATOR|, |POLYHEDRON_SLICE_VECTOR_DECLARATOR|.

\initials{LDF 2005.05.18.}
Added token declarations for |HELIX_DECLARATOR| and 
|HELIX_VECTOR_DECLARATOR|.

\initials{LDF 2005.05.24.}
Added token declarations for 
|HELIX_SLICE_DECLARATOR|, |HELIX_SLICE_VECTOR_DECLARATOR|,
|CONE_DECLARATOR|, |CONE_VECTOR_DECLARATOR|, |CONE_SLICE_DECLARATOR|, 
|CONE_SLICE_VECTOR_DECLARATOR|,
|CYLINDER_DECLARATOR|, |CYLINDER_VECTOR_DECLARATOR|,
|CYLINDER_SLICE_DECLARATOR|, |CYLINDER_SLICE_VECTOR_DECLARATOR|,
|ELLIPSOID_DECLARATOR|, |ELLIPSOID_VECTOR_DECLARATOR|, 
|ELLIPSOID_SLICE_DECLARATOR|, |ELLIPSOID_SLICE_VECTOR_DECLARATOR|,
|SPHERE_DECLARATOR|, |SPHERE_VECTOR_DECLARATOR|, |SPHERE_SLICE_DECLARATOR|, 
and |SPHERE_SLICE_VECTOR_DECLARATOR|.

\initials{LDF 2005.06.08.}
Added token declarations for 
|GLYPH_DECLARATOR|, |GLYPH_VECTOR_DECLARATOR|, |GLYPH_SLICE_DECLARATOR|, 
and |GLYPH_SLICE_VECTOR_DECLARATOR|.

\initials{LDF 2005.10.30.}
Added token declarations for 
|PLANE_DECLARATOR| and |PLANE_VECTOR_DECLARATOR|.

\initials{LDF 2005.11.07.}
Added token declarations for 
|PARABOLA_DECLARATOR|, |HYPERBOLA_DECLARATOR|,
|PARABOLOID_DECLARATOR|,  |HYPERBOLOID_DECLARATOR|, 
|PARABOLA_VECTOR_DECLARATOR|, |HYPERBOLA_VECTOR_DECLARATOR|,
|PARABOLOID_VECTOR_DECLARATOR|, |HYPERBOLOID_VECTOR_DECLARATOR|,
|PARABOLA_SLICE_DECLARATOR|, |HYPERBOLA_SLICE_DECLARATOR|, |PARABOLOID_SLICE_DECLARATOR|, 
|HYPERBOLOID_SLICE_DECLARATOR|, |PARABOLA_SLICE_VECTOR_DECLARATOR|, 
|HYPERBOLA_SLICE_VECTOR_DECLARATOR|, |PARABOLOID_SLICE_VECTOR_DECLARATOR|, 
and |HYPERBOLOID_SLICE_VECTOR_DECLARATOR|.

\initials{LDF 2005.12.05.}
Added token declaration for |ULONG_LONG_DECLARATOR|.

\initials{LDF 2007.07.29.}
Added token declaration for |CONIC_SECTION_LATTICE_DECLARATOR|.

\initials{LDF 2007.10.13.}
Added token declaration for |ARC_DECLARATOR|.

\initials{LDF 2007.11.28.}
Added token declaration for |PREDICATE_DECLARATOR|.

\initials{LDF 2007.11.28.}
Added token declaration for |MATRIX_DECLARATOR|.

\initials{LDF 2007.12.02.}
Added token declaration for |COMPLEX_DECLARATOR|.

\initials{LDF 2009.11.04.}
Added token declaration for |SPHERE_DEVELOPMENT_DECLARATOR|
\ENDLOG 

@q *** (3) Code.@> 

@<Token and precedence declarations@>=

@=%token <string_value> PREDICATE_DECLARATOR@>@/
@=%token <string_value> BOOLEAN_DECLARATOR@>@/
@=%token <string_value> COMPLEX_DECLARATOR@>@/
@=%token <string_value> MATRIX_DECLARATOR@>@/
@=%token <string_value> STRING_DECLARATOR@>@/
@=%token <string_value> NUMERIC_DECLARATOR@>@/
@=%token <string_value> ULONG_LONG_DECLARATOR@>@/
@=%token <string_value> TRANSFORM_DECLARATOR@>@/
@=%token <string_value> COLOR_DECLARATOR@>@/
@=%token <string_value> PEN_DECLARATOR@>@/
@=%token <string_value> DASH_PATTERN_DECLARATOR@>@/
@=%token <string_value> FOCUS_DECLARATOR@>@/
@=%token <string_value> PICTURE_DECLARATOR@>@/
@=%token <string_value> POINT_DECLARATOR@>@/
@=%token <string_value> BOOL_POINT_DECLARATOR@>@/
@=%token <string_value> NURB_DECLARATOR@>@/
@=%token <string_value> PATH_DECLARATOR@>@/
@=%token <string_value> TRIANGLE_DECLARATOR@>@/
@=%token <string_value> POLYGON_DECLARATOR@>@/
@=%token <string_value> REG_POLYGON_DECLARATOR@>@/
@=%token <string_value> RECTANGLE_DECLARATOR@>@/
@=%token <string_value> SQUARE_DECLARATOR@>@/
@=%token <string_value> ELLIPSE_DECLARATOR@>@/
@=%token <string_value> CIRCLE_DECLARATOR@>@/
@=%token <string_value> CONIC_SECTION_LATTICE_DECLARATOR@>@/
@=%token <string_value> HELIX_DECLARATOR@>@/
@=%token <string_value> CUBOID_DECLARATOR@>@/
@=%token <string_value> POLYHEDRON_DECLARATOR@>@/
@=%token <string_value> CONE_DECLARATOR@>@/
@=%token <string_value> CYLINDER_DECLARATOR@>@/
@=%token <string_value> ELLIPSOID_DECLARATOR@>@/
@=%token <string_value> SPHERE_DECLARATOR@>@/
@=%token <string_value> SPHERE_DEVELOPMENT_DECLARATOR@>@/

@=%token <string_value> PARABOLA_DECLARATOR@>@/
@=%token <string_value> HYPERBOLA_DECLARATOR@>@/

@=%token <string_value> ARC_DECLARATOR@>@/

@=%token <string_value> PARABOLOID_DECLARATOR@>@/ 
@=%token <string_value> HYPERBOLOID_DECLARATOR@>@/ 
@=%token <string_value> PARABOLA_VECTOR_DECLARATOR@>@/ 
@=%token <string_value> HYPERBOLA_VECTOR_DECLARATOR@>@/

@=%token <string_value> ARC_VECTOR_DECLARATOR@>@/

@=%token <string_value> PARABOLOID_VECTOR_DECLARATOR@>@/ 
@=%token <string_value> HYPERBOLOID_VECTOR_DECLARATOR@>@/
@=%token <string_value> PARABOLA_SLICE_DECLARATOR@>@/ 
@=%token <string_value> HYPERBOLA_SLICE_DECLARATOR@>@/ 
@=%token <string_value> PARABOLOID_SLICE_DECLARATOR@>@/ 
@=%token <string_value> HYPERBOLOID_SLICE_DECLARATOR@>@/ 
@=%token <string_value> PARABOLA_SLICE_VECTOR_DECLARATOR@>@/ 
@=%token <string_value> HYPERBOLA_SLICE_VECTOR_DECLARATOR@>@/ 
@=%token <string_value> PARABOLOID_SLICE_VECTOR_DECLARATOR@>@/ 
@=%token <string_value> HYPERBOLOID_SLICE_VECTOR_DECLARATOR@>@/

@=%token <string_value> GLYPH_DECLARATOR@>@/
@=%token <string_value> PLANE_DECLARATOR@>@/

@=%token <string_value> ORIGAMI_FIGURE_DECLARATOR@>@/

@=%token <string_value> ELLIPSE_SLICE_DECLARATOR@>@/
@=%token <string_value> CIRCLE_SLICE_DECLARATOR@>@/
@=%token <string_value> HELIX_SLICE_DECLARATOR@>@/
@=%token <string_value> POLYHEDRON_SLICE_DECLARATOR@>@/
@=%token <string_value> CONE_SLICE_DECLARATOR@>@/ 
@=%token <string_value> CYLINDER_SLICE_DECLARATOR@>@/
@=%token <string_value> ELLIPSOID_SLICE_DECLARATOR@>@/
@=%token <string_value> SPHERE_SLICE_DECLARATOR@>@/
@=%token <string_value> GLYPH_SLICE_DECLARATOR@>@/

@=%token <string_value> ELLIPSE_SLICE_VECTOR_DECLARATOR@>@/
@=%token <string_value> CIRCLE_SLICE_VECTOR_DECLARATOR@>@/
@=%token <string_value> POLYHEDRON_SLICE_VECTOR_DECLARATOR@>@/

@q ** (2)  Declarators for object types defined in 3DLDF.@>
@*1 Declarators for object types defined in 3DLDF.

\LOG
\initials{LDF 2004.12.11.}
Added this section.  Moved tokens declarations for vector-type 
declarators here.

\initials{LDF 2004.12.11.}
Added token declarations for 
|BOOLEAN_VECTOR_DECLARATOR|, 
|STRING_VECTOR_DECLARATOR|, 
|NUMERIC_VECTOR_DECLARATOR|, 
|TRANSFORM_VECTOR_DECLARATOR|, 
|PEN_VECTOR_DECLARATOR|, 
|DASH_PATTERN_VECTOR_DECLARATOR|, 
|FOCUS_VECTOR_DECLARATOR|, and
|PICTURE_VECTOR_DECLARATOR|.

\initials{LDF 2005.01.26.}
Added token declaration for |NURB_VECTOR_DECLARATOR|.

\initials{LDF 2005.02.03.}
Added token declaration for |ORIGAMI_FIGURE_VECTOR_DECLARATOR|.

\initials{LDF 2005.02.11.}
Added token declaration for |POLYGON_VECTOR_DECLARATOR|.

\initials{LDF 2005.12.05.}
Added token declaration for |ULONG_LONG_VECTOR_DECLARATOR|.

\initials{LDF 2007.07.29.}
Added token declaration for |CONIC_SECTION_LATTICE_VECTOR_DECLARATOR|.

\initials{LDF 2007.11.28.}
Added token declaration for |MATRIX_VECTOR_DECLARATOR|.

\initials{LDF 2007.12.02.}
Added token declaration for |COMPLEX_VECTOR_DECLARATOR|.

\initials{LDF 2009.11.04.}
Added token declaration for |SPHERE_DEVELOPMENT_VECTOR_DECLARATOR|.
\ENDLOG

@q *** (3).@> 

@<Token and precedence declarations@>=

@=%token <string_value> BOOLEAN_VECTOR_DECLARATOR@>@/
@=%token <string_value> COMPLEX_VECTOR_DECLARATOR@>@/
@=%token <string_value> MATRIX_VECTOR_DECLARATOR@>@/
@=%token <string_value> STRING_VECTOR_DECLARATOR@>@/
@=%token <string_value> NUMERIC_VECTOR_DECLARATOR@>@/
@=%token <string_value> ULONG_LONG_VECTOR_DECLARATOR@>@/
@=%token <string_value> TRANSFORM_VECTOR_DECLARATOR@>@/
@=%token <string_value> COLOR_VECTOR_DECLARATOR@>@/
@=%token <string_value> PEN_VECTOR_DECLARATOR@>@/
@=%token <string_value> DASH_PATTERN_VECTOR_DECLARATOR@>@/
@=%token <string_value> FOCUS_VECTOR_DECLARATOR@>@/
@=%token <string_value> PICTURE_VECTOR_DECLARATOR@>@/
@=%token <string_value> POINT_VECTOR_DECLARATOR@>@/
@=%token <string_value> BOOL_POINT_VECTOR_DECLARATOR@>@/
@=%token <string_value> NURB_VECTOR_DECLARATOR@>@/
@=%token <string_value> PATH_VECTOR_DECLARATOR@>@/
@=%token <string_value> TRIANGLE_VECTOR_DECLARATOR@>@/
@=%token <string_value> POLYGON_VECTOR_DECLARATOR@>@/
@=%token <string_value> REG_POLYGON_VECTOR_DECLARATOR@>@/
@=%token <string_value> RECTANGLE_VECTOR_DECLARATOR@>@/
@=%token <string_value> SQUARE_VECTOR_DECLARATOR@>@/
@=%token <string_value> ELLIPSE_VECTOR_DECLARATOR@>@/
@=%token <string_value> CIRCLE_VECTOR_DECLARATOR@>@/
@=%token <string_value> CONIC_SECTION_LATTICE_VECTOR_DECLARATOR@>@/
@=%token <string_value> HELIX_VECTOR_DECLARATOR@>@/
@=%token <string_value> CUBOID_VECTOR_DECLARATOR@>@/
@=%token <string_value> POLYHEDRON_VECTOR_DECLARATOR@>@/
@=%token <string_value> CONE_VECTOR_DECLARATOR@>@/
@=%token <string_value> CONE_SLICE_VECTOR_DECLARATOR@>@/
@=%token <string_value> CYLINDER_VECTOR_DECLARATOR@>@/
@=%token <string_value> CYLINDER_SLICE_VECTOR_DECLARATOR@>@/
@=%token <string_value> ELLIPSOID_VECTOR_DECLARATOR@>@/ 
@=%token <string_value> ELLIPSOID_SLICE_VECTOR_DECLARATOR@>@/
@=%token <string_value> SPHERE_VECTOR_DECLARATOR@>@/
@=%token <string_value> SPHERE_SLICE_VECTOR_DECLARATOR@>@/
@=%token <string_value> SPHERE_DEVELOPMENT_VECTOR_DECLARATOR@>@/

@=%token <string_value> GLYPH_VECTOR_DECLARATOR@>@/
@=%token <string_value> GLYPH_SLICE_VECTOR_DECLARATOR@>@/

@=%token <string_value> PLANE_VECTOR_DECLARATOR@>@/

@=%token <string_value> ORIGAMI_FIGURE_VECTOR_DECLARATOR@>@/

@=%token <string_value> HELIX_SLICE_VECTOR_DECLARATOR@>@/


@q *** (3) General-Purpose Words and ``Syntactic Sugar''.@>
@*2 General-Purpose Words and ``Syntactic Sugar''.
\initials{LDF 2004.09.24.}

\LOG
\initials{LDF 2004.09.24.}
Added this section with token declarations for 
|IN|, |BY|, and |AROUND|.

\initials{LDF 2004.12.03.}
Added token declaration for |OFF|.
\ENDLOG

@<Token and precedence declarations@>=

@=%token <int_value> IN@>@/
@=%token <int_value> OFF@>@/
@=%token <int_value> BY@>@/
@=%token <int_value> AROUND@>@/


@q *** (3) System information.@>
@*2 System information.
\initials{LDF 2004.11.15.}

\LOG
\initials{LDF 2004.11.15.}
Added this section with token declarations for 
|IS_BIG_ENDIAN| and |IS_LITTLE_ENDIAN|.
\ENDLOG

@<Token and precedence declarations@>=

@=%token <int_value> IS_BIG_ENDIAN@>@/
@=%token <int_value> IS_LITTLE_ENDIAN@>@/


@q *** (3) Transformers and Transform Commands.  @>
@*2 Transformers and Transform Commands.

\LOG
\initials{LDF 2004.05.09.}  
Added this section.

\initials{LDF 2004.09.24.}
Added token declarations for the transform commands.

\initials{LDF 2004.10.04.}
Changed |REFLECTED_IN| to |REFLECTED|.

\initials{LDF 2004.10.13.}
Added token declaration for |ROTATE_AROUND|.  This was necessary, 
because using the token |ROTATE| for both rotation about the main axes 
and rotation around an arbitrary axis didn't work.  The parser rule for the
first kind of rotation was never reduced.

\initials{LDF 2004.12.03.}
Replaced |REFLECTED| with |REFLECTED_IN| and |REFLECTED_OFF|.

\initials{LDF 2006.10.01.}
Added token declaration for |ALIGN|.
\ENDLOG 


@<Token and precedence declarations@>=

@=%token <pointer_value> REFLECTED_IN@>@/
@=%token <pointer_value> REFLECTED_OFF@>@/
@=%token <pointer_value> ROTATED@>@/
@=%token <pointer_value> ROTATED_AROUND@>@/
@=%token <pointer_value> SCALED@>@/
@=%token <pointer_value> SHEARED@>@/
@=%token <pointer_value> SHIFTED@>@/
@=%token <pointer_value> TRANSFORMED@>@/
@=%token <pointer_value> XSCALED@>@/
@=%token <pointer_value> YSCALED@>@/
@=%token <pointer_value> ZSCALED@>@/

@=%token <int_value> ALIGN@>@/
@=%token <int_value> REFLECT@>@/
@=%token <int_value> ROTATE@>@/
@=%token <int_value> ROTATE_AROUND@>@/
@=%token <int_value> SCALE@>@/
@=%token <int_value> SHEAR@>@/
@=%token <int_value> SHIFT@>@/
@=%token <int_value> TRANSFORM_BY@>@/
@=%token <int_value> XSCALE@>@/
@=%token <int_value> YSCALE@>@/
@=%token <int_value> ZSCALE@>@/


@q *** (3) Operators.  @>
@*2 Operators.

Please note that in each declaration where multiple tokens are declared, the
tokens that appear earlier in the declaration are assigned {\it higher\/} values
than the tokens that appear later.  However, the values assigned to the tokens
ascend from declaration to declaration.  For example, |MINUS < PLUS < TIMES|.
\initials{LDF 2004.05.05.}

\LOG
\initials{LDF 2004.04.28.}  
Added declaration for |EQUALS|.

\initials{LDF 2004.05.05.}  
Raised the priority of |LENGTH| and
|UNARY_PLUS_OR_MINUS|.  Added |%left|
declarations for |XPART|, |YPART|, |ZPART|, and |WPART|.

\initials{LDF 2004.05.05.}  
Changed the order of the declarations of 
|XPART|, |YPART|, |ZPART|, and |WPART|.  They are now in reverse alphabetical
order, so that |ZPART == YPART + 1 == XPART + 2 == WPART + 3|.
The rule ``\§numeric primary> $\longrightarrow$ \§point part> \§point primary>'' in 
\filename{pnumexpr.w} depends on this.  

\initials{LDF 2004.05.05.}  
Added declaration of |MAGNITUDE|.

\initials{LDF 2004.05.05.}  
Added declaration of |ANGLE|.

\initials{LDF 2004.05.17.}  
Added declarations for 
|OR|, |AND|, |EQUAL|, |NOT_EQUAL|, |LESS|, 
|LESS_OR_EQUAL|, |GREATER|, |GREATER_OR_EQUAL|, 
|NOT|, and |EQUATE|.  Removed the declaration for |EQUALS|.  

\initials{LDF 2004.08.25.}
Added |%right| declaration of |PLUS_ASSIGN|, |MINUS_ASSIGN|, 
|TIMES_ASSIGN|, and |DIVIDE_ASSIGN|.

\initials{LDF 2004.10.05.}
Added |%left| declarations of |DOT_PRODUCT| and |CROSS_PRODUCT|.


\initials{LDF 2004.10.06.}
Added |%left| declarations of |NORMAL| and |UNIT_VECTOR|.

\initials{LDF 2004.10.06.}
Added |%left| declarations for |XXPART|, |XYPART|, |XZPART|, 
|YXPART|, |YYPART|, |YZPART|, |ZXPART|, |ZYPART|, 
|ZZPART|, and |WXPART|, |WYPART|, |WZPART|. 



\initials{LDF 2004.10.07.}
@:BUG FIX@> BUG FIX: 
Added |WXPART|, |WYPART|, and |WZPART|.  
I'd forgotten to before.

\initials{LDF 2004.11.09.}
Added |left| declaration of |SIZE|.

\initials{LDF 2004.12.16.}
Added |%left| declarations for |RED_PART|, 
|GREEN_PART|, |BLUE_PART|, |YELLOW_PART|, |MAGENTA_PART|,
|CYAN_PART|, |BLACK_PART|, |WHITE_PART|, |BLUE_VIOLET_PART|, and
|RED_ORANGE_PART|.

\initials{LDF 2005.12.14.}
Changed |NORMAL| to |GET_NORMAL|.

\initials{LDF 2007.02.06.}
Added token declarations for |TAND|, |ARCSIND|, |ARCCOSD|, 
and |ARCTAND|.

\initials{LDF 2007.11.08.}
Added token declarations for |GET_PERPENDICULAR| and |GET_PERPENDICULAR_BASE|.
\ENDLOG 

@q **** (4) Declarations.@> 

@<Token and precedence declarations@>=

@=%right <int_value> ASSIGN EQUATE PLUS_ASSIGN MINUS_ASSIGN @>@/
@=TIMES_ASSIGN DIVIDE_ASSIGN@>@/
@=%left <int_value> OR@>@/
@=%left <int_value> AND@>@/
@=%left <int_value> EQUAL NOT_EQUAL@>@/
@=%left <int_value> LESS LESS_OR_EQUAL GREATER GREATER_OR_EQUAL@>@/
@=%left <int_value> PLUS MINUS PYTHAGOREAN_PLUS PYTHAGOREAN_MINUS@>@/
@=%left <int_value> TIMES OVER ANGLE DOT_PRODUCT CROSS_PRODUCT @>@/
@=GET_NORMAL UNIT_VECTOR@>@/
@=%left <int_value> LENGTH MAGNITUDE SIZE SQRT @>@/
@=SIND COSD TAND ARCSIND ARCCOSD ARCTAND @>@/
@=MLOG MEXP @>@/
@=UNIFORMDEVIATE FLOOR ZPART YPART XPART WPART @>@/
@=XXPART XYPART XZPART YXPART YYPART YZPART ZXPART ZYPART ZZPART @>@/
@=WXPART WYPART WZPART @>@/
@=RED_PART GREEN_PART BLUE_PART YELLOW_PART @>@/
@=MAGENTA_PART CYAN_PART BLACK_PART WHITE_PART @>@/
@=BLUE_VIOLET_PART RED_ORANGE_PART @>@/
@=%right <int_value> UNARY_PLUS_OR_MINUS NOT@>@/

@=%token <int_value> GET_PERPENDICULAR@>@/
@=%token <int_value> GET_PERPENDICULAR_BASE@>@/

@q *** (3) Tokens for numerics.@> 
@*2 Tokens for numerics.
\initials{LDF 2005.08.29.}

\LOG
\initials{LDF 2005.08.29.}
Added this section with the token declaration for |MEASURE_TEXT|.

\initials{LDF 2005.11.03.}
Added token declaration for |WITH_PRECISION|.

\initials{LDF 2007.07.22.}
Moved the token declaration for |WITH_PRECISION| to the new section 
for general options.
\ENDLOG

@<Token and precedence declarations@>=

@=%token <int_value> MEASURE_TEXT@>@/

@q *** (3) Tokens for prime numbers.@> 
@*2 Tokens for prime numbers.
\initials{LDF 2005.12.06.}

\LOG
\initials{LDF 2005.12.05.}
Added token declarations for 
|GET_PRIME|, |GET_NEAREST_PRIME|, and |GET_PRIME_FACTORS|.

\initials{LDF 2005.12.05.}
Added token declarations for |IS_PRIME| and |IS_COMPOSITE|.

\initials{LDF 2005.12.06.}
Added this section.  Moved the existing token declarations for 
the prime numbers here from where they were before.
here.

\initials{LDF 2005.12.06.}
Added token declarations for |GET_PRIME_VECTOR|,
|GET_FERMAT_PRIME|, |GET_FERMAT_PRIME_VECTOR|,
|GET_GAUSS_PRIME|, |GET_GAUSS_PRIME_VECTOR|,
|GET_MERSENNE_PRIME|, |GET_MERSENNE_PRIME_VECTOR|,
|IS_PRIME_VECTOR|, |IS_FERMAT_PRIME|, |IS_FERMAT_PRIME_VECTOR|,
|IS_GAUSS_PRIME|, |IS_GAUSS_PRIME_VECTOR|,
|IS_MERSENNE_PRIME|, and |IS_MERSENNE_PRIME_VECTOR|.
\ENDLOG

@<Token and precedence declarations@>=

@=%token <int_value> GET_PRIME@>@/
@=%token <int_value> GET_NEAREST_PRIME@>@/
@=%token <int_value> GET_PRIME_FACTORS@>@/
@=%token <int_value> GET_PRIME_VECTOR@>@/


@=%token <int_value> IS_PRIME@>@/
@=%token <int_value> IS_PRIME_VECTOR@>@/

@=%token <int_value> IS_COMPOSITE@>@/
@=%token <int_value> IS_COMPOSITE_VECTOR@>@/

@=%token <int_value> GET_FERMAT_PRIME@>@/
@=%token <int_value> GET_FERMAT_PRIME_VECTOR@>@/
@=%token <int_value> IS_FERMAT_PRIME@>@/
@=%token <int_value> IS_FERMAT_PRIME_VECTOR@>@/


@=%token <int_value> GET_GAUSS_PRIME@>@/
@=%token <int_value> GET_GAUSS_PRIME_VECTOR@>@/
@=%token <int_value> IS_GAUSS_PRIME@>@/
@=%token <int_value> IS_GAUSS_PRIME_VECTOR@>@/

@=%token <int_value> GET_MERSENNE_PRIME@>@/
@=%token <int_value> GET_MERSENNE_PRIME_VECTOR@>@/
@=%token <int_value> IS_MERSENNE_PRIME@>@/
@=%token <int_value> IS_MERSENNE_PRIME_VECTOR@>@/

@q *** (3) Boolean values and predicates.  @>
@*2 Boolean values and predicates.

\LOG
\initials{LDF 2004.05.17.}  
Added this section with 
token declarations for |TRUE|, |FALSE|, 
|IS_ODD|, |IS_EVEN|, |IS_CYCLE|, |IS_KNOWN|, |IS_UNKNOWN|, 
and |CHAREXISTS|. 

\initials{LDF 2004.10.02.}
Added token declarations for the following type predicates:\hfil\break
|IS_SYNONYM|, |IS_UNDECLARED|,
|IS_BOOLEAN|, |IS_BOOL_POINT|, |IS_BOOL_POINT_VECTOR|, |IS_STRING|,
|IS_NUMERIC|, |IS_PEN|, |IS_DASH_PATTERN|, |IS_COLOR|, |IS_COLOR_VECTOR|,
|IS_PICTURE|, |IS_TRANSFORM|, |IS_POINT|, |IS_POINT_VECTOR|, |IS_FOCUS|,
|IS_PATH|, |IS_TRIANGLE|, |IS_REG_POLYGON|, |IS_RECTANGLE|, |IS_SQUARE|,
|IS_ELLIPSE|, |IS_CIRCLE|, |IS_CUBOID|, |IS_TETRAHEDRON|, |IS_OCTAHEDRON|,
|IS_DODECAHEDRON|, |IS_ICOSAHEDRON|, and |IS_TRUNC_OCTAHEDRON|.


\initials{LDF 2004.10.14.}
Added token declarations for 
|IS_ELLIPTICAL|,
|IS_CIRCULAR|,
|IS_REG_POLYGONAL|,
|IS_TRIANGULAR|,
|IS_QUADRATICAL|,
|IS_PENTAGONAL|,
|IS_HEXAGONAL|,
|IS_HEPTAGONAL|,
|IS_OCTAGONAL|,
|IS_NONAGONAL|,
|IS_DECAGONAL|,
|IS_ONDECAGONAL|,
|IS_DODECAGONAL|,
|IS_RECTANGULAR|,
|IS_CONVEX|,
|IS_CONICAL|,
|IS_PARABOLIC|,
|IS_HYPERBOLIC|,
|IS_TRAPEZOIDAL|,
|IS_PRISMOIDAL|,
|IS_CUBOIDAL|,
|IS_ELLIPSOIDAL|,
|IS_SPHEROIDAL|, and
|IS_SPHERICAL|.         

\initials{LDF 2004.10.22.}
Added token declarations for |IS_ON_SEGMENT|, |IS_ON_LINE|, 
|IS_IN_TRIANGLE|, |IS_IN_RECTANGLE|, and |LOCATION|.

\initials{LDF 2004.10.23.}
Added token declarations for 
|IS_PARALLEL|, 
|IS_COPLANAR|, 
|IS_SKEW|.

\initials{LDF 2004.10.23.}
Added token declaration for |DISTANCE|.

\initials{LDF 2004.10.23.}
Changed |DISTANCE| to |DISTANCE_TO_PLANE|.

\initials{LDF 2004.10.25.}
Added token declarations for |IS_PLANAR| and |DISTANCE_ALONG_LINE|.

\initials{LDF 2004.10.25.}
Added token declaration for |IS_LINEAR|.

\initials{LDF 2004.10.25.}
Removed the token declarations for |IS_NON_PARALLEL|, 
|IS_NON_COPLANAR|, and |IS_NON_SKEW|.

\initials{LDF 2004.10.26.}
Added token declaration for |IS_COLINEAR|.

\initials{LDF 2004.12.10.}
Added token declarations for |IS_PATH_VECTOR|, 
|IS_TRIANGLE_VECTOR|, |IS_REG_POLYGON_VECTOR|, 
|IS_RECTANGLE_VECTOR|, |IS_SQUARE_VECTOR|, 
|IS_ELLIPSE_VECTOR|, |IS_CIRCLE_VECTOR|,  |IS_CUBOID_VECTOR|, 
|IS_TETRAHEDRON_VECTOR|, |IS_OCTAHEDRON_VECTOR|, |IS_DODECAHEDRON_VECTOR|, 
|IS_ICOSAHEDRON_VECTOR|, and |IS_TRUNC_OCTAHEDRON_VECTOR|.


\initials{LDF 2004.12.11.}
Added token declarations for 
|IS_BOOLEAN_VECTOR|, 
|IS_STRING_VECTOR|, 
|IS_NUMERIC_VECTOR|, 
|IS_TRANSFORM_VECTOR|, 
|IS_PEN_VECTOR|, 
|IS_DASH_PATTERN_VECTOR|, 
|IS_FOCUS_VECTOR|, and
|IS_PICTURE_VECTOR|.

\initials{LDF 2005.01.25.}
Added token declaration for |IS_QUADRILATERAL|.

\initials{LDF 2005.01.26.}
Added token declarations for |IS_NURB| and |IS_NURB_VECTOR|.

\initials{LDF 2005.02.03.}
Added token declarations for |IS_ORIGAMI_FIGURE| and 
|IS_ORIGAMI_FIGURE_VECTOR|.

\initials{LDF 2005.02.07.}
Added token declaration for |IS_CONVEX_POLYGONAL|.

\initials{LDF 2005.02.11.}
Added token declarations for |IS_POLYGON|,
|IS_POLYGON_VECTOR|, and |IS_POLYGONAL|.

\initials{LDF 2005.03.29.}
Added token declarations for |IS_ELLIPSE_SLICE|, 
|IS_CIRCLE_SLICE|, |IS_POLYHEDRON_SLICE|,
|IS_ELLIPSE_SLICE_VECTOR|, 
|IS_CIRCLE_SLICE_VECTOR|, and |IS_POLYHEDRON_SLICE_VECTOR|.

\initials{LDF 2005.05.18.}
Added token declarations for |IS_HELIX|, |IS_HELIX_VECTOR|,
and |IS_HELICAL|.

\initials{LDF 2005.05.24.}
Added token declarations for 
|IS_HELIX_SLICE|, |IS_HELIX_SLICE_VECTOR|,
|IS_CONE|, |IS_CONE_VECTOR|, 
|IS_CONE_SLICE|, |IS_CONE_SLICE_VECTOR|,
|IS_CYLINDER|, |IS_CYLINDER_VECTOR|,
|IS_CYLINDER_SLICE|, |IS_CYLINDER_SLICE_VECTOR|, |IS_CYLINDRICAL|,
|IS_ELLIPSOID|, |IS_ELLIPSOID_VECTOR|, |IS_ELLIPSOID_SLICE|, 
|IS_ELLIPSOID_SLICE_VECTOR|, 
|IS_SPHERE|, |IS_SPHERE_VECTOR|, |IS_SPHERE_SLICE|, and
|IS_SPHERE_SLICE_VECTOR|.

\initials{LDF 2005.06.08.}
Added token declarations for 
|IS_GLYPH|, |IS_GLYPH_VECTOR|, |IS_GLYPH_SLICE|, and
|IS_GLYPH_SLICE_VECTOR|.

\initials{LDF 2005.10.26.}
Added token declarations for |IS_ON_SPHERE| and |WITH_TOLERANCE|. 

\initials{LDF 2005.10.30.}
Added token declarations for 
|IS_PLANE| and |IS_PLANE_VECTOR|.

\initials{LDF 2005.11.02.}
Added token declarations for 
|IS_ON_ELLIPSE|, |IS_ON_CIRCLE|, |IS_ON_PARABOLA|, and
|IS_ON_HYPERBOLA|.


\initials{LDF 2005.11.07.}
Added token declarations for 
|IS_PARABOLA|, |IS_HYPERBOLA|,
|IS_PARABOLOID|,  |IS_HYPERBOLOID|, |IS_PARABOLA_VECTOR|, |IS_HYPERBOLA_VECTOR|,
|IS_PARABOLOID_VECTOR|, |IS_HYPERBOLOID_VECTOR|,
|IS_PARABOLA_SLICE|, |IS_HYPERBOLA_SLICE|, |IS_PARABOLOID_SLICE|, 
|IS_HYPERBOLOID_SLICE|, |IS_PARABOLA_SLICE_VECTOR|, 
|IS_HYPERBOLA_SLICE_VECTOR|, |IS_PARABOLOID_SLICE_VECTOR|, 
|IS_HYPERBOLOID_SLICE_VECTOR|, 
|IS_ON_PARABOLA|, |IS_ON_HYPERBOLA|, 

\initials{LDF 2005.11.07.}
Added token declarations for 
|IS_PARABOLOIDAL| and |IS_HYPERBOLOIDAL|.

\initials{LDF 2005.11.21.}
Added token declarations for |IS_VALID| and |IS_INVALID|.

\initials{LDF 2005.12.05.}
Added token declarations for |IS_ULONG_LONG| and |IS_ULONG_LONG_VECTOR|.

\initials{LDF 2007.07.09.}
Added token declaration for |ARE_ON_CONIC_SECTION|.

\initials{LDF 2007.07.22.}
Added token declaration for |ARE_DISTINCT|.
Moved the token declaration for |WITH_TOLERANCE| to the new section for options.

\initials{LDF 2007.07.29.}
Added token declarations for |IS_CONIC_SECTION_LATTICE| and 
|IS_CONIC_SECTION_LATTICE_VECTOR|.

\initials{LDF 2007.08.03.}
Added token declarations for 
|IS_NOT_PARALLEL|, 
|ARE_PARALLEL|, 
|ARE_NOT_PARALLEL|, 
|IS_PERPENDICULAR|,
|IS_NOT_PERPENDICULAR|,
|ARE_PERPENDICULAR|,
|ARE_NOT_PERPENDICULAR|,
|IS_NOT_COPLANAR|,
|ARE_COPLANAR|,
|ARE_NOT_COPLANAR|,
|IS_NOT_SKEW|, 
|ARE_SKEW|,
|ARE_NOT_SKEW|,
|IS_NOT_COLINEAR|,
|ARE_COLINEAR|,
and 
|ARE_NOT_COLINEAR|.

\initials{LDF 2007.10.13.}
Added token declarations for |IS_ARC|, |IS_ARC_TYPE|,
|IS_ARC_VECTOR| and |IS_ON_ARC|.

\initials{LDF 2007.11.08.}
Added token declarations for |IS_ON_PLANE|,
|IS_IN_REG_POLYGON| and |IS_IN_POLYGON|.

\initials{LDF 2007.11.28.}
Added token declarations for |IS_MATRIX| and |IS_MATRIX_VECTOR|.

\initials{LDF 2007.11.29.}
Added token declarations for |IS_IDENTITY|, |IS_ZERO|, |IS_ONE|, |IS_MULTIPLIABLE|, 
and |IS_SINGULAR|.

\initials{LDF 2007.12.02.}
Added token declarations for |IS_COMPLEX| and |IS_COMPLEX_VECTOR|.

\initials{LDF 2009.11.04.}
Added token declarations for 
|IS_SPHERE_DEVELOPMENT| and |IS_SPHERE_DEVELOPMENT_VECTOR|.
\ENDLOG

@q **** (4) Code.@>   

@<Token and precedence declarations@>=

@=%token <int_value> TRUE@>@/
@=%token <int_value> FALSE@>@/
@=%token <int_value> IS_ODD@>@/
@=%token <int_value> IS_EVEN@>@/

@=%token <int_value> IS_CYCLE@>@/
@=%token <int_value> IS_KNOWN@>@/
@=%token <int_value> IS_UNKNOWN@>@/
@=%token <int_value> CHAREXISTS@>@/

@=%token <int_value> IS_VALID@>@/
@=%token <int_value> IS_INVALID@>@/

@=%token <int_value> IS_SYNONYM@>@/
@=%token <int_value> IS_UNDECLARED@>@/
@=%token <int_value> IS_BOOLEAN@>@/
@=%token <int_value> IS_BOOL_POINT@>@/
@=%token <int_value> IS_BOOL_POINT_VECTOR@>@/

@=%token <int_value> IS_COMPLEX@>@/
@=%token <int_value> IS_COMPLEX_VECTOR@>@/

@=%token <int_value> IS_MATRIX@>@/
@=%token <int_value> IS_MATRIX_VECTOR@>@/

@=%token <int_value> IS_IDENTITY@>@/
@=%token <int_value> IS_ZERO@>@/
@=%token <int_value> IS_ONE@>@/
@=%token <int_value> IS_MULTIPLIABLE@>@/
@=%token <int_value> IS_SINGULAR@>@/

@=%token <int_value> IS_STRING@>@/
@=%token <int_value> IS_NUMERIC@>@/
@=%token <int_value> IS_ULONG_LONG@>@/
@=%token <int_value> IS_PEN@>@/
@=%token <int_value> IS_DASH_PATTERN@>@/
@=%token <int_value> IS_COLOR@>@/
@=%token <int_value> IS_COLOR_VECTOR@>@/
@=%token <int_value> IS_PICTURE@>@/
@=%token <int_value> IS_TRANSFORM@>@/
@=%token <int_value> IS_POINT@>@/
@=%token <int_value> IS_POINT_VECTOR@>@/
@=%token <int_value> IS_FOCUS@>@/
@=%token <int_value> IS_NURB@>@/
@=%token <int_value> IS_PATH@>@/
@=%token <int_value> IS_TRIANGLE@>@/
@=%token <int_value> IS_POLYGON@>@/
@=%token <int_value> IS_REG_POLYGON@>@/
@=%token <int_value> IS_RECTANGLE@>@/
@=%token <int_value> IS_ELLIPSE@>@/
@=%token <int_value> IS_CIRCLE@>@/
@=%token <int_value> IS_CONIC_SECTION_LATTICE@>@/
@=%token <int_value> IS_HELIX@>@/
@=%token <int_value> IS_CUBOID@>@/
@=%token <int_value> IS_POLYHEDRON@>@/

@=%token <int_value> IS_ELLIPSE_SLICE@>@/
@=%token <int_value> IS_CIRCLE_SLICE@>@/
@=%token <int_value> IS_POLYHEDRON_SLICE@>@/

@=%token <int_value> IS_ELLIPSE_SLICE_VECTOR@>@/
@=%token <int_value> IS_CIRCLE_SLICE_VECTOR@>@/
@=%token <int_value> IS_POLYHEDRON_SLICE_VECTOR@>@/

@=%token <int_value> IS_HELIX_SLICE@>@/
@=%token <int_value> IS_HELIX_SLICE_VECTOR@>@/
@=%token <int_value> IS_CONE@>@/
@=%token <int_value> IS_CONE_VECTOR@>@/
@=%token <int_value> IS_CONE_SLICE@>@/
@=%token <int_value> IS_CONE_SLICE_VECTOR@>@/
@=%token <int_value> IS_CYLINDER@>@/
@=%token <int_value> IS_CYLINDER_VECTOR@>@/
@=%token <int_value> IS_CYLINDER_SLICE@>@/
@=%token <int_value> IS_CYLINDER_SLICE_VECTOR@>@/
@=%token <int_value> IS_CYLINDRICAL@>@/
@=%token <int_value> IS_ELLIPSOID@>@/
@=%token <int_value> IS_ELLIPSOID_VECTOR@>@/
@=%token <int_value> IS_ELLIPSOID_SLICE@>@/ 
@=%token <int_value> IS_ELLIPSOID_SLICE_VECTOR@>@/ 

@=%token <int_value> IS_PARABOLA@>@/
@=%token <int_value> IS_HYPERBOLA@>@/

@=%token <int_value> IS_ARC@>@/
@=%token <int_value> IS_ARC_TYPE@>@/

@=%token <int_value> IS_PARABOLOID@>@/
@=%token <int_value> IS_HYPERBOLOID@>@/
@=%token <int_value> IS_PARABOLA_VECTOR@>@/
@=%token <int_value> IS_HYPERBOLA_VECTOR@>@/

@=%token <int_value> IS_ARC_VECTOR@>@/

@=%token <int_value> IS_PARABOLOID_VECTOR@>@/
@=%token <int_value> IS_HYPERBOLOID_VECTOR@>@/
@=%token <int_value> IS_PARABOLA_SLICE@>@/
@=%token <int_value> IS_HYPERBOLA_SLICE@>@/
@=%token <int_value> IS_PARABOLOID_SLICE@>@/
@=%token <int_value> IS_HYPERBOLOID_SLICE@>@/
@=%token <int_value> IS_PARABOLA_SLICE_VECTOR@>@/ 
@=%token <int_value> IS_HYPERBOLA_SLICE_VECTOR@>@/
@=%token <int_value> IS_PARABOLOID_SLICE_VECTOR@>@/ 
@=%token <int_value> IS_HYPERBOLOID_SLICE_VECTOR@>@/ 
@=%token <int_value> IS_ON_PARABOLA@>@/
@=%token <int_value> IS_ON_HYPERBOLA@>@/

@=%token <int_value> IS_ON_ARC@>@/

@=%token <int_value> IS_PARABOLOIDAL@>@/ 
@=%token <int_value> IS_HYPERBOLOIDAL@>@/ 

@=%token <int_value> IS_SPHERE@>@/
@=%token <int_value> IS_SPHERE_VECTOR@>@/
@=%token <int_value> IS_SPHERE_SLICE@>@/ 
@=%token <int_value> IS_SPHERE_SLICE_VECTOR@>@/

@=%token <int_value> IS_SPHERE_DEVELOPMENT@>@/
@=%token <int_value> IS_SPHERE_DEVELOPMENT_VECTOR@>@/

@=%token <int_value> IS_GLYPH@>@/
@=%token <int_value> IS_GLYPH_VECTOR@>@/
@=%token <int_value> IS_GLYPH_SLICE@>@/ 
@=%token <int_value> IS_GLYPH_SLICE_VECTOR@>@/

@=%token <int_value> IS_PLANE@>@/
@=%token <int_value> IS_PLANE_VECTOR@>@/

@=%token <int_value> IS_ORIGAMI_FIGURE@>@/

@=%token <int_value> IS_LINEAR@>@/
@=%token <int_value> IS_PLANAR@>@/
@=%token <int_value> IS_SQUARE@>@/
@=%token <int_value> IS_ELLIPTICAL@>@/
@=%token <int_value> IS_CIRCULAR@>@/
@=%token <int_value> IS_HELICAL@>@/
@=%token <int_value> IS_POLYGONAL@>@/
@=%token <int_value> IS_REG_POLYGONAL@>@/
@=%token <int_value> IS_TRIANGULAR@>@/
@=%token <int_value> IS_QUADRATICAL@>@/
@=%token <int_value> IS_PENTAGONAL@>@/
@=%token <int_value> IS_HEXAGONAL@>@/
@=%token <int_value> IS_HEPTAGONAL@>@/
@=%token <int_value> IS_OCTAGONAL@>@/
@=%token <int_value> IS_NONAGONAL@>@/
@=%token <int_value> IS_DECAGONAL@>@/
@=%token <int_value> IS_ONDECAGONAL@>@/
@=%token <int_value> IS_DODECAGONAL@>@/
@=%token <int_value> IS_RECTANGULAR@>@/
@=%token <int_value> IS_QUADRILATERAL@>@/
@=%token <int_value> IS_CONVEX@>@/
@=%token <int_value> IS_CONVEX_POLYGONAL@>@/
@=%token <int_value> IS_CONICAL@>@/
@=%token <int_value> IS_PARABOLIC@>@/
@=%token <int_value> IS_HYPERBOLIC@>@/
@=%token <int_value> IS_TRAPEZOIDAL@>@/
@=%token <int_value> IS_PRISMOIDAL@>@/
@=%token <int_value> IS_CUBOIDAL@>@/
@=%token <int_value> IS_ELLIPSOIDAL@>@/
@=%token <int_value> IS_SPHEROIDAL@>@/ 
@=%token <int_value> IS_SPHERICAL@>@/         

@=%token <int_value> IS_ON_SEGMENT@>@/          
@=%token <int_value> IS_ON_LINE@>@/         
@=%token <int_value> IS_ON_PLANE@>@/         

@=%token <int_value> IS_IN_TRIANGLE@>@/         
@=%token <int_value> IS_IN_RECTANGLE@>@/     
@=%token <int_value> IS_IN_REG_POLYGON@>@/         
@=%token <int_value> IS_IN_POLYGON@>@/         
    
@=%token <int_value> LOCATION@>@/         

@=%token <int_value> IS_COLINEAR@>@/ 
@=%token <int_value> IS_NOT_COLINEAR@>@/ 
@=%token <int_value> ARE_COLINEAR@>@/ 
@=%token <int_value> ARE_NOT_COLINEAR@>@/ 

@=%token <int_value> IS_PARALLEL@>@/ 
@=%token <int_value> IS_NOT_PARALLEL@>@/ 
@=%token <int_value> ARE_PARALLEL@>@/ 
@=%token <int_value> ARE_NOT_PARALLEL@>@/ 

@=%token <int_value> IS_COPLANAR@>@/ 
@=%token <int_value> IS_NOT_COPLANAR@>@/ 
@=%token <int_value> ARE_COPLANAR@>@/ 
@=%token <int_value> ARE_NOT_COPLANAR@>@/ 

@=%token <int_value> IS_PERPENDICULAR@>@/ 
@=%token <int_value> IS_NOT_PERPENDICULAR@>@/ 
@=%token <int_value> ARE_PERPENDICULAR@>@/ 
@=%token <int_value> ARE_NOT_PERPENDICULAR@>@/ 

@=%token <int_value> IS_SKEW@>@/ 
@=%token <int_value> IS_NOT_SKEW@>@/ 
@=%token <int_value> ARE_SKEW@>@/ 
@=%token <int_value> ARE_NOT_SKEW@>@/ 

@=%token <int_value> DISTANCE_TO_PLANE@>@/
@=%token <int_value> DISTANCE_ALONG_LINE@>@/

@=%token <int_value> IS_NURB_VECTOR@>@/
@=%token <int_value> IS_PATH_VECTOR@>@/
@=%token <int_value> IS_TRIANGLE_VECTOR@>@/
@=%token <int_value> IS_POLYGON_VECTOR@>@/
@=%token <int_value> IS_REG_POLYGON_VECTOR@>@/
@=%token <int_value> IS_RECTANGLE_VECTOR@>@/
@=%token <int_value> IS_SQUARE_VECTOR@>@/
@=%token <int_value> IS_ELLIPSE_VECTOR@>@/
@=%token <int_value> IS_CIRCLE_VECTOR@>@/
@=%token <int_value> IS_CONIC_SECTION_LATTICE_VECTOR@>@/
@=%token <int_value> IS_HELIX_VECTOR@>@/
@=%token <int_value> IS_CUBOID_VECTOR@>@/
@=%token <int_value> IS_POLYHEDRON_VECTOR@>@/

@=%token <int_value> IS_ORIGAMI_FIGURE_VECTOR@>@/

@=%token <int_value> IS_BOOLEAN_VECTOR@>@/
@=%token <int_value> IS_STRING_VECTOR@>@/
@=%token <int_value> IS_NUMERIC_VECTOR@>@/
@=%token <int_value> IS_ULONG_LONG_VECTOR@>@/
@=%token <int_value> IS_TRANSFORM_VECTOR@>@/
@=%token <int_value> IS_PEN_VECTOR@>@/
@=%token <int_value> IS_DASH_PATTERN_VECTOR@>@/
@=%token <int_value> IS_FOCUS_VECTOR@>@/
@=%token <int_value> IS_PICTURE_VECTOR@>@/

@=%token <int_value> IS_ON_ELLIPSE@>@/
@=%token <int_value> IS_ON_CIRCLE@>@/

@=%token <int_value> ARE_ON_CONIC_SECTION@>@/

@=%token <int_value> IS_ON_SPHERE@>@/

@=%token <int_value> ARE_DISTINCT@>@/

@q *** (3) Tokens for complex types.@>
@*2 Tokens for complex types.  
\initials{LDF 2007.12.02.}

\LOG
\initials{LDF 2007.12.02.}
Added this section.  
\ENDLOG

@<Token and precedence declarations@>=


@q *** (3) Tokens for matrix types.@>
@*2 Tokens for matrix types.  
\initials{LDF 2007.11.28.}

\LOG
\initials{LDF 2007.11.28.}
Added this section.  

\initials{LDF 2007.11.29.}
Added the suffix ``|_MATRIX|'' to these tokens.  I couldn't use a prefix, 
because |class Matrix| defines constants named |MATRIX_DOUBLE_TYPE|, etc.

\initials{LDF 2007.11.29.}
Added token declarations for |IDENTITY_MATRIX|, |ZERO_MATRIX|, and
|ONE_MATRIX|.
\ENDLOG

@<Token and precedence declarations@>=

@=%token <int_value> DOUBLE_TYPE_MATRIX@>@/
@=%token <int_value> FLOAT_TYPE_MATRIX@>@/
@=%token <int_value> LONG_DOUBLE_TYPE_MATRIX@>@/
@=%token <int_value> INT_TYPE_MATRIX@>@/
@=%token <int_value> UINT_TYPE_MATRIX@>@/
@=%token <int_value> LONG_TYPE_MATRIX@>@/
@=%token <int_value> ULONG_TYPE_MATRIX@>@/
@=%token <int_value> SHORT_TYPE_MATRIX@>@/
@=%token <int_value> USHORT_TYPE_MATRIX@>@/
@=%token <int_value> CHAR_TYPE_MATRIX@>@/
@=%token <int_value> UCHAR_TYPE_MATRIX@>@/
@=%token <int_value> COMPLEX_TYPE_MATRIX@>@/
@=%token <int_value> COMPLEX_FLOAT_TYPE_MATRIX@>@/
@=%token <int_value> COMPLEX_LONG_DOUBLE_TYPE_MATRIX@>@/

@=%token <int_value> IDENTITY_MATRIX@>@/
@=%token <int_value> ZERO_MATRIX@>@/
@=%token <int_value> ONE_MATRIX@>@/



@q *** (3) Tokens for general options.  @>
@*2 Tokens for general options.  
\initials{LDF 2007.07.22.}

\LOG
\initials{LDF 2007.07.22.}
Added this section.  Moved token declarations to from 
other sections to here.
\ENDLOG

@<Token and precedence declarations@>=

@=%token <int_value> WITH_PRECISION@>@/
@=%token <int_value> WITH_TOLERANCE@>@/


@q *** (3) Tokens for |strings|.  @>
@*2 Tokens for {\bf strings}.
\initials{LDF 2005.09.08.}

\LOG
\initials{LDF 2005.09.08.}
Added this section with the declaration of |DECIMAL|.


\initials{LDF 2005.09.08.}
Added the declarations of |BINARY|, |OCTAL|, and |HEXADECIMAL|.
\ENDLOG

@<Token and precedence declarations@>=

@=%token <int_value> BINARY@>@/
@=%token <int_value> OCTAL@>@/
@=%token <int_value> DECIMAL@>@/
@=%token <int_value> HEXADECIMAL@>@/

@q *** (3) Types for drawing.@>
@*2 Types for drawing.
\initials{LDF 2004.05.21.}  

\LOG
\initials{LDF 2004.05.21.}  
Added this section.

\initials{LDF 2004.06.07.}  
Removed the token declaration for |WITH_DOTS|.
|with_dots| is now a predefined variable defined in |Scanner_Type::create()| in 
\filename{scan.web}.

\initials{LDF 2004.06.07.}  
Changed the name of the token |DASH_PATTERN| to 
|DASH_PATTERN_DECLARATOR| and moved the token declaration to where 
the types are declared, since |dash_pattern| is now a type.

\initials{LDF 2004.08.23.}
Added token declarations for 
|WITH_COLOR_VECTOR|, |WITH_DRAW_COLOR_VECTOR|, and 
|WITH_FILL_COLOR_VECTOR|.

\initials{LDF 2004.11.05.}
Added token declarations for |DRAWDOT_IF|, |DRAWDOT_UNLESS|, 
|UNDRAWDOT_IF|, and |UNDRAWDOT_UNLESS|.

\initials{LDF 2004.11.07.}
Added token declarations for |DRAWDBLARROW| and |UNDRAWDBLARROW|.

\initials{LDF 2004.11.11.}
Added token declarations for 
|PICKUP|, |DROP_COLOR|, |DROP_DASH_PATTERN|, and 
|DROP_PEN|.

\initials{LDF 2004.11.12.}
Added token declarations for |PICKUP_DRAW_COLOR|, 
|PICKUP_FILL_COLOR|, |DROP_DRAW_COLOR|, and |DROP_FILL_COLOR|.

\initials{LDF 2004.11.14.}
Added token declaration for |DROP_ALL|.

\initials{LDF 2005.01.29.}
Added token declarations for
|WITH_TEXT_COLOR| and |WITH_DOT_COLOR|.

\initials{LDF 2005.03.18.}
Added token declarations for
|WITH_PEN_VECTOR| and |WITH_DASH_PATTERN_VECTOR|.

\initials{LDF 2005.04.11.}
Added token declarations for |CMY|, |CMYK|, and |RGB|.

\initials{LDF 2005.08.16.}
Added token declaration for |CLIP_TO|.
\ENDLOG 

@q **** (4) Declarations.  @>

@<Token and precedence declarations@>=

@=%token <int_value> DRAWDOT@>@/
@=%token <int_value> DRAWDOT_IF@>@/
@=%token <int_value> DRAWDOT_UNLESS@>@/
@=%token <int_value> DRAW@>@/
@=%token <int_value> FILL@>@/
@=%token <int_value> FILLDRAW@>@/
@=%token <int_value> DRAWARROW@>@/
@=%token <int_value> DRAWDBLARROW@>@/
@=%token <int_value> CLIP_TO@>@/
@=%token <int_value> UNDRAWDOT@>@/
@=%token <int_value> UNDRAWDOT_IF@>@/
@=%token <int_value> UNDRAWDOT_UNLESS@>@/
@=%token <int_value> UNDRAW@>@/
@=%token <int_value> UNFILL@>@/
@=%token <int_value> UNFILLDRAW@>@/
@=%token <int_value> UNDRAWARROW@>@/
@=%token <int_value> UNDRAWDBLARROW@>@/
@=%token <int_value> WITH_PEN@>@/
@=%token <int_value> WITH_PEN_VECTOR@>@/
@=%token <int_value> PENCIRCLE@>@/
@=%token <int_value> PENRAZOR@>@/
@=%token <int_value> PENSPECK@>@/
@=%token <int_value> PENSQUARE@>@/
@=%token <int_value> WITH_COLOR@>@/
@=%token <int_value> WITH_DRAW_COLOR@>@/
@=%token <int_value> WITH_FILL_COLOR@>@/
@=%token <int_value> WITH_TEXT_COLOR@>@/
@=%token <int_value> WITH_DOT_COLOR@>@/
@=%token <int_value> WITH_COLOR_VECTOR@>@/
@=%token <int_value> WITH_DRAW_COLOR_VECTOR@>@/
@=%token <int_value> WITH_FILL_COLOR_VECTOR@>@/
@=%token <int_value> WITH_WEIGHT@>@/
@=%token <int_value> DASHED@>@/
@=%token <int_value> WITH_DASH_PATTERN_VECTOR@>@/
@=%token <int_value> ON_PICTURE@>@/

@=%token <int_value> PICKUP@>@/
@=%token <int_value> DROP_COLOR@>@/
@=%token <int_value> DROP_DASH_PATTERN@>@/
@=%token <int_value> DROP_PEN@>@/

@=%token <int_value> PICKUP_DRAW_COLOR@>@/ 
@=%token <int_value> PICKUP_FILL_COLOR@>@/
@=%token <int_value> DROP_DRAW_COLOR@>@/
@=%token <int_value> DROP_FILL_COLOR@>@/

@=%token <int_value> CMY@>@/
@=%token <int_value> CMYK@>@/
@=%token <int_value> RGB@>@/



@=%token <int_value> DROP_ALL@>@/

@q *** (3) Types for |Focuses|.  @>
@*2 Types for |Focuses|.
\initials{LDF 2004.06.08.}  

\LOG
\initials{LDF 2004.06.08.}  
Added this section, with token declarations for 
|WITH_POSITION|, |WITH_DIRECTION|, |WITH_DISTANCE|, |WITH_ANGLE|, 
|WITH_AXIS|, |X_AXIS|, |Y_AXIS|, and |Z_AXIS|.

\initials{LDF 2004.06.08.}  
Added token declarations for |PERSPECTIVE|
|PARALLEL_X_Y|, |PARALLEL_X_Z|, |PARALLEL_Z_Y|, |AXONOMETRIC|, 
|ISOMETRIC|, and |SUPPRESS|.

\initials{LDF 2007.09.02.}
Added token declarations for 
|GET_POSITION|,
|GET_DIRECTION|,
|GET_DISTANCE|,
|GET_UP|
and
|GET_PERSPECTIVE_TRANSFORM|.
|GET_TRANSFORM| is already declared elsewhere.

\initials{LDF 2007.09.02.}
Added token declaration for |RESET_ANGLE|.

\initials{LDF 2007.09.17.}
Added token declarations for |PAN|, |PANNED|, |ZOOM|, and |ZOOMED|.

\initials{LDF 2007.09.19.}
Added token declarations for |WITH_FOCUS_AXES| and |WITH_MAIN_AXES|.

\initials{LDF 2007.09.24.}
Added token declarations for |TILT| and |TILTED|.
\ENDLOG 

@q **** (4) Declarations.  @>

@<Token and precedence declarations@>=

@=%token <int_value> WITH_POSITION@>@/
@=%token <int_value> WITH_DIRECTION@>@/
@=%token <int_value> WITH_DISTANCE@>@/
@=%token <int_value> WITH_ANGLE@>@/
@=%token <int_value> WITH_AXIS@>@/
@=%token <int_value> X_AXIS@>@/
@=%token <int_value> Y_AXIS@>@/
@=%token <int_value> Z_AXIS@>@/

@=%token <int_value> WITH_FOCUS_AXES@>@/
@=%token <int_value> WITH_MAIN_AXES@>@/

@=%token <int_value> PERSPECTIVE@>@/
@=%token <int_value> PARALLEL_X_Y@>@/
@=%token <int_value> PARALLEL_X_Z@>@/
@=%token <int_value> PARALLEL_Z_Y@>@/
@=%token <int_value> AXONOMETRIC@>@/
@=%token <int_value> ISOMETRIC@>@/
@=%token <int_value> SUPPRESS@>@/

@=%token <int_value> GET_POSITION@>@/
@=%token <int_value> GET_DIRECTION@>@/
@=%token <int_value> GET_DISTANCE@>@/
@=%token <int_value> GET_UP@>@/
/* |GET_TRANSFORM| is already declared elsewhere.  \initials{LDF 2007.09.02.}  */@/
@=%token <int_value> GET_PERSPECTIVE_TRANSFORM@>@/

@=%token <int_value> RESET_ANGLE@>@/

@=%token <int_value> PAN@>@/
@=%token <int_value> PANNED@>@/

@=%token <int_value> ZOOM@>@/
@=%token <int_value> ZOOMED@>@/

@=%token <int_value> TILT@>@/
@=%token <int_value> TILTED@>@/

@q **** (4) Declarations.  @>

@q *** (4) Types for outputting |Pictures|.  @>
@*3 Types for outputting |Pictures|.

\LOG
\initials{LDF 2004.06.08.}  
Added this section.

\initials{LDF 2004.06.08.}  
Added token declaration for |WITH_PROJECTION|.

\initials{LDF 2004.06.26.}  
Added token declaration for |WITH_FACTOR|.

\initials{LDF 2004.06.26.}  
Added token declarations for 
|NO_SORT|, |MAX_Z_SORT|, |MIN_Z_SORT|, |MEAN_Z_SORT|, 
|WITH_MIN_X_PROJ|, |WITH_MAX_X_PROJ|, 
|WITH_MIN_Y_PROJ|, |WITH_MAX_Y_PROJ|, 
|WITH_MIN_Z_PROJ|, and |WITH_MAX_Z_PROJ|. 


\initials{LDF 2004.06.29.}
Changed |WITH_MIN_X_PROJ| to |MIN_Y_PROJ|.  Did the same for
|MAX_X_PROJ|, |MIN_Y_PROJ|, etc.

\initials{LDF 2005.11.14.}
Added token declarations for |WITH_FOCUS_0| and |WITH_FOCUS_1|.
\ENDLOG 

@q **** (4) Declarations.  @>

@<Token and precedence declarations@>=

@=%token <int_value> WITH_FOCUS@>@/
@=%token <int_value> WITH_FOCUS_0@>@/
@=%token <int_value> WITH_FOCUS_1@>@/

@=%token <int_value> WITH_PROJECTION@>@/
@=%token <real_value> WITH_FACTOR@>@/
@=%token <int_value> NO_SORT@>@/ 

@=%token <int_value> MAX_Z_SORT@>@/ 
@=%token <int_value> MIN_Z_SORT@>@/ 
@=%token <int_value> MEAN_Z_SORT@>@/ 

@=%token <int_value> MIN_X_PROJ@>@/ 
@=%token <int_value> MAX_X_PROJ@>@/ 
@=%token <int_value> MIN_Y_PROJ@>@/ 
@=%token <int_value> MAX_Y_PROJ@>@/ 
@=%token <int_value> MIN_Z_PROJ@>@/ 
@=%token <int_value> MAX_Z_PROJ@>@/ 

@q *** (3) Types for projections.@> 
@*2 Types for projections.
LDF 2005.04.15.

\LOG
\initials{LDF 2005.04.15.}
Added this section with the token declaration for |PROJECT|.

\initials{LDF 2005.04.18.}
Added the token declaration for |PROJECTED|.

\initials{LDF 2005.04.19.}
Added the token declaration for |WITH_Z|.
\ENDLOG

@<Token and precedence declarations@>=

@=%token <int_value> PROJECT@>@/ 
@=%token <int_value> PROJECTED@>@/ 
@=%token <int_value> WITH_Z@>@/ 

@q *** (3) Commands.  @>
@*2 Commands.

\LOG
\initials{LDF 2004.06.08.}  
Added token declaration for |SET|.

\initials{LDF 2004.06.30.}  
Added token declarations for |LABEL|,
|RT|, |LFT|, |TOP|, |BOT|, |URT|, |ULFT|, |LRT|, and |LLFT|.

\initials{LDF 2004.06.30.}  
Added token declaration for |DOTLABEL|.

\initials{LDF 2004.09.22.}
Added token declarations for |MESSAGE| and |ERRMESSAGE|.

\initials{LDF 2004.09.22.}
Added token declaration for |PAUSE|.

\initials{LDF 2004.09.30.}
Added token declaration for |END_INPUT|.
Changed value type of |INPUT| and |END| from |string_value| 
to |int_value|. 


\initials{LDF 2004.10.20.}
Added token declaration for |MEDIATE|.

\initials{LDF 2004.10.22.}
Added token declarations for |SUPPRESS_LABELS| and 
|UNSUPPRESS_LABELS|.

\initials{LDF 2004.10.22.}
Added token declaration for |KILL_LABELS|.

\initials{LDF 2004.11.06.}
Added token declarations for |LABEL_IF|, |LABEL_UNLESS|,
|DOTLABEL_IF|, and |DOTLABEL_UNLESS|.

\initials{LDF 2004.11.10.}
Added token declaration for |LABELS|, 
|DOTLABELS|, |LABELS_IF|, |DOTLABELS_IF|, |LABELS_UNLESS|, and 
|DOTLABELS_UNLESS|.

\initials{LDF 2004.12.01.}
Added token declaration for |SCANTOKENS|. 

\initials{LDF 2004.12.13.}
Added token declaration for |VERBATIM_METAPOST|. 
\ENDLOG 

@q **** (4) Declarations.@>   

\LOG
\initials{LDF 2005.01.25.}
Added token declaration for |LET|.
\ENDLOG 

@<Token and precedence declarations@>=

@=%token <int_value> INPUT@>@/
@=%token <int_value> END@>@/
@=%token <int_value> END_INPUT@>@/
@=%token <int_value> SET@>@/
@=%token <int_value> LABEL@>@/
@=%token <int_value> DOTLABEL@>@/
@=%token <int_value> RT@>@/
@=%token <int_value> LFT@>@/
@=%token <int_value> TOP@>@/
@=%token <int_value> BOT@>@/
@=%token <int_value> URT@>@/
@=%token <int_value> ULFT@>@/
@=%token <int_value> LRT@>@/
@=%token <int_value> LLFT@>@/
@=%token <int_value> MESSAGE@>@/
@=%token <int_value> ERRMESSAGE@>@/
@=%token <int_value> PAUSE@>@/
@=%token <int_value> MEDIATE@>@/
@=%token <int_value> SUPPRESS_LABELS@>@/
@=%token <int_value> UNSUPPRESS_LABELS@>@/
@=%token <int_value> KILL_LABELS@>@/

@=%token <int_value> LABEL_IF@>@/
@=%token <int_value> LABEL_UNLESS@>@/
@=%token <int_value> DOTLABEL_IF@>@/
@=%token <int_value> DOTLABEL_UNLESS@>@/


@=%token <int_value> LABELS@>@/
@=%token <int_value> LABELS_IF@>@/
@=%token <int_value> LABELS_UNLESS@>@/
@=%token <int_value> DOTLABELS@>@/
@=%token <int_value> DOTLABELS_IF@>@/
@=%token <int_value> DOTLABELS_UNLESS@>@/

@=%token <int_value> SCANTOKENS@>@/

@=%token <int_value> VERBATIM_METAPOST@>@/

@=%token <int_value> LET@>@/

@q *** (3) Types for setting |Shapes|.@>
@ Types for setting |Shapes|.

\LOG
\initials{LDF 2004.11.01.}
Added this section with token declarations 
for |WITH_CENTER|, |WITH_POINT_COUNT|, and |WITH_NORMAL|.

\initials{LDF 2004.11.01.}
Added token declarations for 
|WITH_AXIS_H|, |WITH_AXIS_V|, and |WITH_DIAMETER|.

\initials{LDF 2004.11.02.}
Added token declaration for |WITH_SIDES|. 

\initials{LDF 2005.05.19.}
Added token declarations for |WITH_START_DIAMETER|, 
|WITH_POINTS_PER_CYCLE|, |WITH_CYCLES|, and |WITH_VECTOR|.

\initials{LDF 2005.05.21.}
Changed |WITH_VECTOR| to |WITH_DIRECTION_VECTOR|.

\initials{LDF 2005.05.21.}
Added token declarations for |WITH_TYPE|
|NULL_TYPE|, |LOGARITHMIC|, |ARCHIMEDEAN|,
|PARABOLIC|, and |HYPERBOLIC|.

\initials{LDF 2005.05.24.}
Added token declarations for |ELLIPTICAL|, |CYLINDRICAL|, 
|CONICAL|, |CIRCULAR|, |WITH_CURVE|, |WITH_PATH|, and 
|WITH_ENVELOPE|.

\initials{LDF 2005.10.19.}
Added token declarations for 
|WITH_DIVISIONS|, |WITH_DIVISIONS_VERTICAL|, and 
|WITH_DIVISIONS_HORIZONTAL|.

\initials{LDF 2005.10.28.}
Added token declaration for |WITH_RADIUS|.

\initials{LDF 2005.10.31.}
Added token declarations for 
|WITH_AXIS_X|, |WITH_AXIS_Y|, and |WITH_AXIS_Z|.

\initials{LDF 2005.10.31.}
Added token declarations for 
|WITH_DIVISIONS_X|, |WITH_DIVISIONS_Y|, and |WITH_DIVISIONS_Z|.  

\initials{LDF 2006.11.09.}
Added token declarations for |SINGLE_NAPPED| and |DOUBLE_NAPPED|.

\initials{LDF 2009.11.18.}
Added token declarations for |WITH_DIVISIONS_LATITUDE| 
|WITH_DIVISIONS_LONGITUDE| and |WITH_SEGMENT_COUNT|.
\ENDLOG

@q **** (4) Code.@> 

@<Token and precedence declarations@>=

@=%token <int_value> WITH_CENTER@>@/
@=%token <int_value> WITH_POINT_COUNT@>@/
@=%token <int_value> WITH_NORMAL@>@/
@=%token <int_value> WITH_AXIS_H@>@/
@=%token <int_value> WITH_AXIS_V@>@/
@=%token <int_value> WITH_RADIUS@>@/
@=%token <int_value> WITH_DIAMETER@>@/
@=%token <int_value> WITH_SIDES@>@/

@=%token <int_value> WITH_AXIS_X@>@/
@=%token <int_value> WITH_AXIS_Y@>@/
@=%token <int_value> WITH_AXIS_Z@>@/

@=%token <int_value> WITH_DIVISIONS@>@/
@=%token <int_value> WITH_DIVISIONS_VERTICAL@>@/
@=%token <int_value> WITH_DIVISIONS_HORIZONTAL@>@/

@=%token <int_value> WITH_DIVISIONS_LATITUDE@>@/
@=%token <int_value> WITH_DIVISIONS_LONGITUDE@>@/

@=%token <int_value> WITH_SEGMENT_COUNT@>@/

@=%token <int_value> WITH_DIVISIONS_X@>@/
@=%token <int_value> WITH_DIVISIONS_Y@>@/
@=%token <int_value> WITH_DIVISIONS_Z@>@/

@=%token <int_value> WITH_START_DIAMETER@>@/
@=%token <int_value> WITH_POINTS_PER_CYCLE@>@/
@=%token <int_value> WITH_CYCLES@>@/
@=%token <int_value> WITH_DIRECTION_VECTOR@>@/

@=%token <int_value> WITH_TYPE@>@/

@=%token <int_value> NULL_TYPE@>@/
@=%token <int_value> LOGARITHMIC@>@/
@=%token <int_value> ARCHIMEDEAN@>@/
@=%token <int_value> PARABOLIC@>@/
@=%token <int_value> HYPERBOLIC@>@/

@=%token <int_value> ELLIPTICAL@>@/
@=%token <int_value> CIRCULAR@>@/
@=%token <int_value> CYLINDRICAL@>@/
@=%token <int_value> CONICAL@>@/

@=%token <int_value> SINGLE_NAPPED@>@/
@=%token <int_value> DOUBLE_NAPPED@>@/

@=%token <int_value> WITH_CURVE@>@/ 
@=%token <int_value> WITH_PATH@>@/
@=%token <int_value> WITH_ENVELOPE@>@/


@q *** (3) Types for modifying |Shapes|.@>
@ Types for modifying |Shapes|.

\LOG
\initials{LDF 2004.11.01.}
Added this section with a token declaration 
for |CLEAR_CONNECTORS|.
\ENDLOG

@<Token and precedence declarations@>=

@=%token <int_value> CLEAR_CONNECTORS@>@/


@q *** (3) Types for outputting |Pictures|.  @>
@*2 Types for outputting |Pictures|.

\LOG
\initials{LDF 2004.06.07.}  
Added token declaration for |OUTPUT|.

\initials{LDF 2004.06.09.}  
Added token declaration for |SHOW|.

\initials{LDF 2004.06.09.}  
Added token declarations for |BEGINFIG| and
|ENDFIG|. 

\initials{LDF 2004.07.14.}
Added token declaration for |CLEAR|.

\initials{LDF 2005.03.13.}
Added token declarations for |WITH_SURFACE_HIDING| 
and |WITHOUT_SURFACE_HIDING|.

\initials{LDF 2005.06.11.}
Added token declarations for |BEGINCHAR| and
|ENDCHAR|. 

\initials{LDF 2005.09.13.}
Added token declaration for |FIXED|.
\ENDLOG 

@<Token and precedence declarations@>=

@=%token <int_value> OUTPUT@>@/
@=%token <int_value> CLEAR@>@/
@=%token <int_value> BEGINFIG@>@/
@=%token <int_value> ENDFIG@>@/
@=%token <int_value> BEGINCHAR@>@/
@=%token <int_value> ENDCHAR@>@/
@=%token <int_value> SHOW@>@/

@=%token <int_value> FIXED@>@/

@=%token <int_value> WITH_SURFACE_HIDING@>@/
@=%token <int_value> WITHOUT_SURFACE_HIDING@>@/


@q *** (3) Types for intersections.  @>
@*2 Types for intersections.

\LOG
\initials{LDF 2004.09.05.}
Added this section with token declarations for 
|INTERSECTION_POINT| and |INTERSECTION_POINTS|.

\initials{LDF 2005.10.28.}
Added token declaration for |INTERSECTION|.
\ENDLOG

@<Token and precedence declarations@>=

@=%token <int_value> INTERSECTION@>@/
@=%token <int_value> INTERSECTION_POINT@>@/
@=%token <int_value> INTERSECTION_POINTS@>@/

@q *** (3) Types for conditionals.  @>
@ Types for conditionals.

\LOG
\initials{LDF 2004.09.09.}
Added this section with token declarations for 
|IF|, |ELSE|, |ELSEIF|, and |FI|.
\ENDLOG

@<Token and precedence declarations@>=

@=%token <int_value> IF@>@/
@=%token <int_value> ELSE@>@/
@=%token <int_value> ELSEIF@>@/
@=%token <int_value> FI@>@/

@q *** (3) Types for loops.  @>
@ Types for loops.

\LOG
\initials{LDF 2004.09.09.}
Added this section with token declarations for 
|FOR|, |FORSUFFIXES|, |FOREVER|, |EXITIF|, |STEP|, |UNTIL|, 
and |ENDFOR|. 

\initials{LDF 2004.09.13.}
Added token declaration for |END_LOOP_PREFIX|.

\initials{LDF 2004.09.14.}
Removed token declaration for |END_LOOP_PREFIX|.

\initials{LDF 2004.09.22.}
Changed |EXITIF| to |EXIT_IF| and added |EXIT_UNLESS|.

\initials{LDF 2004.09.22.}
Changed |FORSUFFIXES| to |FOR_SUFFIXES| and |ENDFOR| to |END_FOR|. 

\initials{LDF 2004.11.26.}
Added token declarations for |UPTO|, |DOWNTO|, 
and |FOR_SUFFIXES_INTERNAL|.

\initials{LDF 2005.01.19.}
Added token declaration for |FOR_SUFFIXES_INTERNAL_START|.
\ENDLOG

@<Token and precedence declarations@>=

@=%token <int_value> FOR@>@/
@=%token <int_value> FOR_SUFFIXES@>@/
@=%token <int_value> FOR_SUFFIXES_INTERNAL@>@/
@=%token <int_value> FOR_SUFFIXES_INTERNAL_START@>@/
@=%token <int_value> FOREVER@>@/
@=%token <int_value> EXIT_IF@>@/
@=%token <int_value> EXIT_UNLESS@>@/
@=%token <int_value> STEP@>@/
@=%token <int_value> UNTIL@>@/
@=%token <int_value> END_FOR@>@/
@=%token <int_value> UPTO@>@/
@=%token <int_value> DOWNTO@>@/

@q *** (3) Types for groups.  @>
@ Types for groups.

\LOG
\initials{LDF 2004.09.12.}
Added this section with token declarations for 
|BEGIN_GROUP|, |END_GROUP|, and |SAVE|.

\ENDLOG

@<Token and precedence declarations@>=

@=%token <int_value> BEGIN_GROUP@>@/
@=%token <int_value> END_GROUP@>@/
@=%token <int_value> SAVE@>@/

@q *** (3) Types for transformations.@>
@*2 Types for transformations.
\initials{LDF 2005.04.16.}

\LOG
\initials{LDF 2005.04.16.}
Added this section with the token declarations for |INVERSE| 
and |INVERT|.

\initials{LDF 2007.10.14.}
Added the token declarations for |TRANSPOSE| and |TRANSPOSED|. 
\ENDLOG

@<Token and precedence declarations@>=

@=%token <int_value> INVERSE@>@/
@=%token <int_value> INVERT@>@/

@=%token <int_value> TRANSPOSE@>@/
@=%token <int_value> TRANSPOSED@>@/


@q *** (3) Types for extracting sub-objects.@>
@*2 Types for extracting sub-objects.
\initials{LDF 2004.10.08.}

\LOG
\initials{LDF 2004.10.08.}
Added this section with token declarations for 
|GET_TRANSFORM|,
|GET_POINT|,
|GET_POINTS|,
|GET_CONNECTOR|,
|GET_CONNECTORS|,
|GET_PEN|,
|GET_DASH_PATTERN|,
|GET_DRAWDOT_COLOR|,
|GET_DRAW_COLOR|,
|GET_FILL_COLOR|,
|GET_COLORS|,
|GET_CIRCLE|,
|GET_ELLIPSE|,
|GET_PATH|,
|GET_RECTANGLE|, and
|GET_REG_POLYGON|.

\initials{LDF 2004.10.09.}
Added token declaration for |GET_CENTER|.

\initials{LDF 2004.11.13.}
Added token declarations for |HALF|, |QUARTER|, 
and |SEGMENT|.

\initials{LDF 2004.11.25.}
Added token declarations for |WITH_START|, |WITH_END|, |WITH_SIZE|, 
and |WITH_FRACTION|.

\initials{LDF 2004.11.25.}
Added token declarations for |WITH_CLOSED|.

\initials{LDF 2005.03.17.}
Removed the token declaration for |GET_DRAWDOT_COLOR|.

\initials{LDF 2005.11.07.}
Added token declarations for |GET_PARABOLA| and |GET_HYPERBOLA|.

\initials{LDF 2005.11.09.}
Added token declarations for |GET_VERTEX|,|GET_FOCUS|, |GET_FOCUS_0|, 
|GET_FOCUS_1|, |GET_DIRECTRIX|, |GET_PARAMETER|, |GET_LINEAR_ECCENTRICITY|, 
|GET_NUMERICAL_ECCENTRICITY|, |GET_TYPE|, |GET_SHAPE_TYPE|, and
|GET_CONIC_SECTION_TYPE|.

\initials{LDF 2005.11.09.}
Added the token declaration for |GET_LAST_POINT|.

\initials{LDF 2005.11.15.}
Removed the token declarations for |GET_VERTEX_0|, |GET_VERTEX_1|, 
|GET_FOCUS_0|, and |GET_FOCUS_1|.  The matching strings can't be used
in input because of the numerals.

\initials{LDF 2005.11.17.}
Added token declarations for |DISCRIMINANT|, |TANGENT|, 
and |CURVATURE|.

\initials{LDF 2005.11.18.}
Added token declaration for |STANDARDIZE|.

\initials{LDF 2006.11.06.}
Added token declarations for |GET_BASE_CENTER| and 
|GET_CAP_CENTER|.

\initials{LDF 2006.11.09.}
Added token declarations for 
|GET_APEX|, |GET_BASE|, and |GET_CAP|.

\initials{LDF 2007.07.09.}
Added token declaration for |GET_BRANCH|.

\initials{LDF 2007.07.29.}
Added token declaration for |GET_CONIC_SECTION_LATTICE|.

\initials{LDF 2007.08.03.}
Added token declaration for |GET_PATH_POINT|.

\initials{LDF 2007.08.16.}
Added token declarations for |GET_APPROX_CENTER|,
|GET_TRUE_CENTER| and |GET_POINT_SIX|.

\initials{LDF 2007.08.17.}
Added token declarations for |GET_POLYGON|, 
|GET_PENTAGON|, |GET_HEXAGON|, |GET_HEPTAGON|, 
|GET_OCTAGON|, |GET_NONAGON|, |GET_DECAGON|,
|GET_HENDECAGON|, and |GET_DODECAGON|.

\initials{LDF 2007.08.19.}
Added token declarations for |GET_PASCAL_POINT|, |GET_PASCAL_POINTS|, and 
|GET_PASCAL_LINE|.

\initials{LDF 2007.08.21.}
Added token declarations for |SET_TRANSFORM|, |RESET_TRANSFORM|,
and |DELETE_TRANSFORM|.
\ENDLOG

@q **** (4) Code.@> 

@<Token and precedence declarations@>=

@=%token <int_value> GET_TRANSFORM@>@/
@=%token <int_value> SET_TRANSFORM@>@/
@=%token <int_value> RESET_TRANSFORM@>@/
@=%token <int_value> DELETE_TRANSFORM@>@/

@=%token <int_value> GET_POINT@>@/
@=%token <int_value> GET_PATH_POINT@>@/
@=%token <int_value> GET_LAST_POINT@>@/
@=%token <int_value> GET_POINTS@>@/
@=%token <int_value> GET_CONNECTOR@>@/
@=%token <int_value> GET_CONNECTORS@>@/
@=%token <int_value> GET_PEN@>@/
@=%token <int_value> GET_DASH_PATTERN@>@/
@=%token <int_value> GET_DRAW_COLOR@>@/
@=%token <int_value> GET_FILL_COLOR@>@/
@=%token <int_value> GET_COLORS@>@/

@=%token <int_value> GET_PASCAL_POINT@>@/
@=%token <int_value> GET_PASCAL_POINTS@>@/
@=%token <int_value> GET_PASCAL_LINE@>@/

@=%token <int_value> GET_CENTER@>@/
@=%token <int_value> GET_APPROX_CENTER@>@/
@=%token <int_value> GET_TRUE_CENTER@>@/
@=%token <int_value> GET_POINT_SIX@>@/

@=%token <int_value> GET_CIRCLE@>@/
@=%token <int_value> GET_ELLIPSE@>@/
@=%token <int_value> GET_PATH@>@/
@=%token <int_value> GET_RECTANGLE@>@/
@=%token <int_value> GET_REG_POLYGON@>@/

@=%token <int_value> GET_POLYGON@>@/
@=%token <int_value> GET_PENTAGON@>@/
@=%token <int_value> GET_HEXAGON@>@/
@=%token <int_value> GET_HEPTAGON@>@/ 
@=%token <int_value> GET_OCTAGON@>@/ 
@=%token <int_value> GET_NONAGON@>@/
@=%token <int_value> GET_DECAGON@>@/
@=%token <int_value> GET_HENDECAGON@>@/
@=%token <int_value> GET_DODECAGON@>@/

@=%token <int_value> GET_APEX@>@/
@=%token <int_value> GET_BASE@>@/
@=%token <int_value> GET_CAP@>@/

@=%token <int_value> GET_BASE_CENTER@>@/
@=%token <int_value> GET_CAP_CENTER@>@/


@=%token <int_value> GET_PARABOLA@>@/
@=%token <int_value> GET_HYPERBOLA@>@/

@=%token <int_value> GET_CONIC_SECTION_LATTICE@>@/

@=%token <int_value> GET_VERTEX@>@/

@=%token <int_value> GET_FOCUS@>@/

@=%token <int_value> GET_DIRECTRIX@>@/
@=%token <int_value> GET_PARAMETER@>@/
@=%token <int_value> GET_LINEAR_ECCENTRICITY@>@/
@=%token <int_value> GET_NUMERICAL_ECCENTRICITY@>@/
@=%token <int_value> GET_TYPE@>@/
@=%token <int_value> GET_SHAPE_TYPE@>@/
@=%token <int_value> GET_CONIC_SECTION_TYPE@>@/

@=%token <int_value> GET_BRANCH@>@/

@=%token <int_value> DISCRIMINANT@>@/
@=%token <int_value> TANGENT@>@/
@=%token <int_value> CURVATURE@>@/

@=%token <int_value> STANDARDIZE@>@/

@=%token <int_value> HALF@>@/
@=%token <int_value> QUARTER@>@/
@=%token <int_value> SEGMENT@>@/

@=%token <int_value> WITH_START@>@/
@=%token <int_value> WITH_END@>@/
@=%token <int_value> WITH_SIZE@>@/
@=%token <int_value> WITH_FRACTION@>@/
@=%token <int_value> WITH_CLOSED@>@/

@q *** (3) Types for generating objects.@>
@*2 Types for generating objects.
\initials{LDF 2007.07.20.}

\LOG
\initials{LDF 2007.07.20.}
Added this section with token declarations for 
|WITH_TEST|, |WITH_NO_TEST|, |WITH_RECTIFY|, and |WITH_NO_RECTIFY|.

\initials{LDF 2007.07.27.}
Added token declarations for |WITH_CULL| and |WITH_NO_CULL|.

\initials{LDF 2007.08.03.}
Added token declarations for |WITH_TRANSFORM| and |WITH_NO_TRANSFORM|.
\ENDLOG

@q **** (4) Code.@> 

@<Token and precedence declarations@>=

@=%token <int_value> WITH_TEST@>@/
@=%token <int_value> WITH_NO_TEST@>@/

@=%token <int_value> WITH_CULL@>@/
@=%token <int_value> WITH_NO_CULL@>@/

@=%token <int_value> WITH_RECTIFY@>@/
@=%token <int_value> WITH_NO_RECTIFY@>@/

@=%token <int_value> WITH_TRANSFORM@>@/
@=%token <int_value> WITH_NO_TRANSFORM@>@/


@q *** (3) Types for enclosed and enclosing figures.@>
@*2 Types for enclosed and enclosing figures.
\initials{LDF 2004.11.16.}

\LOG
\initials{LDF 2004.11.16.}
Added this section with token declarations for
|IN_CIRCLE|, 
|OUT_CIRCLE|, 
|IN_ELLIPSE|, 
|OUT_ELLIPSE|, 
|IN_RECTANGLE|, 
|OUT_RECTANGLE|, 
|IN_SPHERE|, 
|OUT_SPHERE|, 
|IN_ELLIPSOID|, 
|OUT_ELLIPSOID|, 
|IN_CUBOID|, and
|OUT_CUBOID|.
\ENDLOG

@q **** (4) Code.@>   

@<Token and precedence declarations@>=

@=%token <int_value> IN_CIRCLE@>@/ 
@=%token <int_value> OUT_CIRCLE@>@/ 

@=%token <int_value> IN_ELLIPSE@>@/ 
@=%token <int_value> OUT_ELLIPSE@>@/ 

@=%token <int_value> IN_RECTANGLE@>@/ 
@=%token <int_value> OUT_RECTANGLE@>@/ 

@=%token <int_value> IN_SPHERE@>@/ 
@=%token <int_value> OUT_SPHERE@>@/ 

@=%token <int_value> IN_ELLIPSOID@>@/ 
@=%token <int_value> OUT_ELLIPSOID@>@/ 

@=%token <int_value> IN_CUBOID@>@/
@=%token <int_value> OUT_CUBOID@>@/

@q *** (3) Types for macros.@>
@*2 Types for macros.

\LOG
\initials{LDF 2004.12.16.}
Added this section with token declarations for
|MACRO_TYPE|,
|VARDEF_TYPE|,
|PRIMARYDEF_TYPE|,
|SECONDARYDEF_TYPE|,
|TERTIARYDEF_TYPE|,
|DEFMACRO|,
|VARDEF|,
|PRIMARYDEF|, 
|SECONDARYDEF|, 
|TERTIARYDEF|, 
|ENDDEF|, 
|SUFFIX|, 
|TEXT|, and
|EXPR|.

\initials{LDF 2004.12.20.}
Changed |DEFMACRO| to |DEF|.

\initials{LDF 2004.12.21.}
Added token declaration for |DEFUN|.

\initials{LDF 2004.12.22.}
Changed the type of |DEFUN| from |int_value| to |pointer_value|.

\initials{LDF 2004.12.28.}
Removed the declarations for 
|MACRO_TYPE|, 
|VARDEF_TYPE|, 
|PRIMARYDEF_TYPE|, 
|SECONDARYDEF_TYPE|, 
|TERTIARYDEF_TYPE|, 
|DEF|, 
|VARDEF|, 
|SUFFIX|, 
|TEXT|, and
|EXPR|.
%
Changed the type of 
|PRIMARYDEF|, |SECONDARYDEF|, and |TERTIARYDEF|
from |int_value| to |pointer_value|.
%
Added declarations for 
|MACRO|, 
|MACRO_DECLARATOR|, 
|PRIMARYDEF_DECLARATOR|,
|SECONDARYDEF_DECLARATOR|, and
|TERTIARYDEF_DECLARATOR|.

\initials{LDF 2004.12.28.}
Added declarations for 
|MACRO_VECTOR|, 
|PRIMARYDEF_VECTOR|, 
|SECONDARYDEF_VECTOR|, 
|TERTIARYDEF_VECTOR|, 
|MACRO_VECTOR_DECLARATOR|, 
|PRIMARYDEF_VECTOR_DECLARATOR|,
|SECONDARYDEF_VECTOR_DECLARATOR|,
and 
|TERTIARYDEF_VECTOR_DECLARATOR|.

\initials{LDF 2004.12.29.}
Added token declaration for |DEF| again.

\initials{LDF 2005.06.09.}
Removed the token declaration for |DEFUN|.
\ENDLOG

@q **** (4) Code.@>   

@<Token and precedence declarations@>=

@=%token <pointer_value> MACRO@>@/ 
@=%token <pointer_value> PRIMARYDEF@>@/ 
@=%token <pointer_value> SECONDARYDEF@>@/ 
@=%token <pointer_value> TERTIARYDEF@>@/ 

@=%token <string_value> MACRO_DECLARATOR@>@/ 
@=%token <string_value> PRIMARYDEF_DECLARATOR@>@/
@=%token <string_value> SECONDARYDEF_DECLARATOR@>@/
@=%token <string_value> TERTIARYDEF_DECLARATOR@>@/

@=%token <pointer_value> MACRO_VECTOR@>@/ 
@=%token <pointer_value> PRIMARYDEF_VECTOR@>@/ 
@=%token <pointer_value> SECONDARYDEF_VECTOR@>@/ 
@=%token <pointer_value> TERTIARYDEF_VECTOR@>@/ 

@=%token <string_value> MACRO_VECTOR_DECLARATOR@>@/ 
@=%token <string_value> PRIMARYDEF_VECTOR_DECLARATOR@>@/
@=%token <string_value> SECONDARYDEF_VECTOR_DECLARATOR@>@/
@=%token <string_value> TERTIARYDEF_VECTOR_DECLARATOR@>@/

@=%token <int_value> DEF@>@/ 
@=%token <int_value> ENDDEF@>@/ 

@q *** (3) Types for vector-type objects.@>
@*2 Types for vector-type objects.
\initials{LDF 2005.01.05.}

\LOG
\initials{LDF 2005.01.05.}
Added this section with token declarations for
|PUSH_BACK| and |POP_BACK|.

\initials{LDF 2005.01.07.}
Added token declaration for |LAST|.

\initials{LDF 2007.07.22.}
Added token declaration for |CULL|.

\initials{LDF 2007.07.27.}
Added token declaration for |RECTIFY|.

\initials{LDF 2007.07.27.}
Added token declarations for |CULLED| and |RECTIFIED|.
\ENDLOG

@q **** (4) Code.@>   

@<Token and precedence declarations@>=

@=%token <int_value> CULL@>@/ 
@=%token <int_value> CULLED@>@/ 

@=%token <int_value> LAST@>@/ 

@=%token <int_value> PUSH_BACK@>@/ 
@=%token <int_value> POP_BACK@>@/ 

@=%token <int_value> RECTIFY@>@/ 
@=%token <int_value> RECTIFIED@>@/ 


@q *** (3) Tokens for |planes|.@>
@*2 Tokens for |planes|.
\initials{LDF 2007.10.02.}

\LOG
\initials{LDF 2007.10.02.}
Added this section with the token declarations for
|GET_PLANE|.
\ENDLOG

@q **** (4) Code.@>   

@<Token and precedence declarations@>=

@=%token <int_value> GET_PLANE@>@/ 

@q *** (3) Tokens for |triangles|.@>
@*2 Tokens for |triangles|.
\initials{LDF 2005.01.20.}

\LOG
\initials{LDF 2005.01.20.}
Added this section with token declarations for
|WITH_SSS|, |WITH_SAS|, |WITH_SAA|, |WITH_ASA|, 
|WITH_SSA_B|, |WITH_SSA_L|, and |WITH_SSA_S|. 

\initials{LDF 2005.01.23.}
Added token declaration for |WITH_SSA_A|.  
I'd forgotten it before.

\initials{LDF 2005.01.23.}
Removed the token declarations for |WITH_SSA_A| and 
|WITH_SSA_B|.

\initials{LDF 2005.01.23.}
Added token declaration for |WITH_POINTS|.

\initials{LDF 2005.10.25.}
Removed the token declarations for 
|WITH_SSS|, |WITH_SAS|, |WITH_SAA|, |WITH_ASA|, |WITH_SSA_L|,
and |WITH_SSA_S|.
\ENDLOG

@q **** (4) Code.@>   

@<Token and precedence declarations@>=

@=%token <int_value> WITH_POINTS@>@/ 

@q *** (3) Types for conic sections.@>
@*2 Types for conic sections.
\initials{LDF 2005.11.07.}

\LOG
\initials{LDF 2005.11.07.}
Added this section with the token declarations for 
|WITH_VERTEX|, |WITH_DIRECTRIX|, |WITH_PARAMETER|,
|WITH_LINEAR_ECCENTRICITY|, and |WITH_NUMERICAL_ECCENTRICITY|.

\initials{LDF 2005.11.07.}
Added the token declaration for |WITH_HALF_PARAMETER|.

\initials{LDF 2005.11.09.}
Added the token declarations for |WITH_INCREMENT_VALUE| 
and |WITH_MAX_EXTENT|.

\initials{LDF 2005.11.14.}
Added token declarations for |WITH_VERTEX_0| and |WITH_VERTEX_1|.

\initials{LDF 2005.11.15.}
Added token declarations for 
|WITH_MAJOR_AXIS_LENGTH|, |WITH_HALF_MAJOR_AXIS_LENGTH|,
|WITH_MINOR_AXIS_LENGTH|, and |WITH_HALF_MINOR_AXIS_LENGTH|.

\initials{LDF 2007.07.30.}
Added token declaration for |TRAVERSE|.

\initials{LDF 2007.08.03.}
Added token declarations for 
|GET_LATTICE_POINT|, 
|GET_LATTICE_POINTS|, 
|GET_NEXT_POINT|, 
|GET_CYCLE_VALUE|, 
|DRAW_LATTICE|, 
|LABEL_LATTICE|, 
and
|WITH_CYCLE_VALUE|.

\initials{LDF 2007.08.03.}
Changed |WITH_INCREMENT_VALUE| to |WITH_INCREMENT|.

\initials{LDF 2007.08.03.}
Added token declarations for |GET_ANGLE_POINT| and |GET_PARALLEL|.

\initials{LDF 2007.08.05.}
Added token declarations for |WITH_INCREMENTS|.

\initials{LDF 2007.08.07.}
Added token declarations for |SHIFT_LATTICE|,
|WITH_SHIFT| and |WITH_NO_SHIFT|.

\initials{LDF 2007.08.16.}
Added token declaration for |DOTLABEL_LATTICE|.

\initials{LDF 2007.11.04.}
Added token declarations for |GET_RADIUS| and |GET_DIAMETER|.

\initials{LDF 2009.09.16.}
Added token declaration for |CIRCUMFERENCE|.
\ENDLOG

@<Token and precedence declarations@>=

@=%token <int_value> GET_LATTICE_POINT@>@/ 
@=%token <int_value> GET_LATTICE_POINTS@>@/ 
@=%token <int_value> GET_NEXT_POINT@>@/ 
@=%token <int_value> GET_CYCLE_VALUE@>@/ 

@=%token <int_value> GET_ANGLE_POINT@>@/ 
@=%token <int_value> GET_PARALLEL@>@/ 

@=%token <int_value> CIRCUMFERENCE@>@/  

@=%token <int_value> GET_RADIUS@>@/  
@=%token <int_value> GET_DIAMETER@>@/ 

@=%token <int_value> DRAW_LATTICE@>@/ 

@=%token <int_value> SHIFT_LATTICE@>@/ 

@=%token <int_value> WITH_CYCLE_VALUE@>@/ 

@=%token <int_value> WITH_VERTEX@>@/ 
@=%token <int_value> WITH_VERTEX_0@>@/ 
@=%token <int_value> WITH_VERTEX_1@>@/ 

@=%token <int_value> WITH_DIRECTRIX@>@/ 
@=%token <int_value> WITH_PARAMETER@>@/ 
@=%token <int_value> WITH_HALF_PARAMETER@>@/ 

@=%token <int_value> WITH_MAJOR_AXIS_LENGTH@>@/ 
@=%token <int_value> WITH_HALF_MAJOR_AXIS_LENGTH@>@/ 
@=%token <int_value> WITH_MINOR_AXIS_LENGTH@>@/ 
@=%token <int_value> WITH_HALF_MINOR_AXIS_LENGTH@>@/ 

@=%token <int_value> WITH_LINEAR_ECCENTRICITY@>@/ 
@=%token <int_value> WITH_NUMERICAL_ECCENTRICITY@>@/ 
@=%token <int_value> WITH_INCREMENT@>@/ 
@=%token <int_value> WITH_INCREMENTS@>@/ 
@=%token <int_value> WITH_MAX_EXTENT@>@/ 

@=%token <int_value> WITH_SHIFT@>@/ 
@=%token <int_value> WITH_NO_SHIFT@>@/ 

@=%token <int_value> TRAVERSE@>@/ 

@=%token <int_value> LABEL_LATTICE@>@/ 
@=%token <int_value> DOTLABEL_LATTICE@>@/ 


@q *** (3) Types for quadric surfaces.@>
@*2 Types for quadric surfaces.
\initials{LDF 2005.10.25.}

\LOG
\initials{LDF 2005.10.25.}
Added this section with token declarations for
|GLOBE_TYPE| and |PANEL_TYPE|.

\initials{LDF 2005.10.25.}
Changed |GLOBE_TYPE| and |PANEL_TYPE| to |SPHERE_GLOBE_TYPE| 
and |SPHERE_PANEL_TYPE|, respectively.  

\initials{LDF 2007.08.06.}
Added token declarations for |INTRUDE|, |EXTRUDE|, |INTRUDED|, and 
|EXTRUDED|.

\initials{LDF 2009.11.03.}
Added token declaration for |MAP| and |DEVELOP|.
\ENDLOG

@q **** (4) Code.@>   

@<Token and precedence declarations@>=

@=%token <int_value> SPHERE_GLOBE_TYPE@>@/ 
@=%token <int_value> SPHERE_PANEL_TYPE@>@/ 

@=%token <int_value> INTRUDE@>@/  
@=%token <int_value> EXTRUDE@>@/  
@=%token <int_value> INTRUDED@>@/ 
@=%token <int_value> EXTRUDED@>@/ 

@=%token <int_value> DEVELOP@>@/ 
@=%token <int_value> MAP@>@/ 


@q *** (3) Tokens for |Polyhedra|.@> 
@*2 Tokens for {\bf Polyhedra}.
\initials{LDF 2005.12.20.}

\TODO
@q { @>
@:TO DO}{{\bf TO DO}@>
@q } @> 
@q !! TO DO:  @>
\initials{LDF 2007.10.12.}
Decide whether |WITH_TABS| is needed and delete it, if it isn't.
\ENDTODO 

\LOG
\initials{LDF 2005.12.20.}
Added this section with the token declarations for |GET_NET|,
|WITH_HALF|, and |GREAT_RHOMBICOSIDODECAHEDRON|.

\initials{LDF 2005.12.20.}
Added the token declarations for 
|TETRAHEDRON|, |CUBE|, |OCTAHEDRON|,
|DODECAHEDRON|, |ICOSAHEDRON|, and |TRUNC_OCTAHEDRON|.

\initials{LDF 2007.10.02.}
Added the token declaration for |WITH_TABS|.

\initials{LDF 2007.10.12.}
Added the token declaration for |GET_TABS|.  |WITH_TABS| may not be needed, 
but I'm leaving it here for the present.

\initials{LDF 2007.10.12.}
Added the token declarations for |WITH_STITCH_LINES|,
|WITH_NO_STITCH_LINES|, and |WITH_STITCH_DIVISIONS|.

\initials{LDF 2007.10.22.}
Added the token declaration for |RHOMBIC_TRIACONTAHEDRON|.

\initials{LDF 2007.10.24.}
Added the token declaration for |WITH_RHOMBUS_SIDE_LENGTH|.

\initials{LDF 2007.10.26.}
Added the token declarations for |GET_RHOMBUS| and |WITH_SIDE_LENGTH|.
\ENDLOG

@<Token and precedence declarations@>=

@=%token <int_value> GET_NET@>@/ 
@=%token <int_value> GET_TABS@>@/ 

@=%token <int_value> GET_RHOMBUS@>@/ 

@=%token <int_value> TETRAHEDRON@>@/ 
@=%token <int_value> CUBE@>@/ 
@=%token <int_value> OCTAHEDRON@>@/ 
@=%token <int_value> DODECAHEDRON@>@/ 
@=%token <int_value> ICOSAHEDRON@>@/ 
@=%token <int_value> TRUNC_OCTAHEDRON@>@/ 
@=%token <int_value> GREAT_RHOMBICOSIDODECAHEDRON@>@/ 
@=%token <int_value> RHOMBIC_TRIACONTAHEDRON@>@/ 

@=%token <int_value> WITH_HALF@>@/ 
@=%token <int_value> WITH_TABS@>@/ 
@=%token <int_value> WITH_SIDE_LENGTH@>@/ 
@=%token <int_value> WITH_RHOMBUS_SIDE_LENGTH@>@/ 


@=%token <int_value> WITH_STITCH_LINES@>@/ 
@=%token <int_value> WITH_NO_STITCH_LINES@>@/ 
@=%token <int_value> WITH_STITCH_DIVISIONS@>@/ 

@q *** (3) Tokens for surface hiding.@>
@*2 Tokens for surface hiding.
\initials{LDF 2005.01.24.}

\LOG
\initials{LDF 2005.01.24.}
Added this section with token declarations 
for |DECOMPOSE| and |WITH_LEVEL|.

\initials{LDF 2005.04.14.}
Added the token declaration for |WITH_RECTANGLES|.
\ENDLOG 

@q **** (4) Declarations.  @>

@<Token and precedence declarations@>=

@=%token <int_value> DECOMPOSE@>@/
@=%token <int_value> WITH_LEVEL@>@/
@=%token <int_value> WITH_RECTANGLES@>@/

@q *** (3) Tokens for |Nurbs|.@>
@*2 Tokens for |Nurbs|.
\initials{LDF 2005.01.26.}

\LOG
\initials{LDF 2005.01.26.}
Added this section with token declarations for
|KNOT|, |WEIGHT|, |WITH_KNOTS|, and |WITH_WEIGHTS|.

\initials{LDF 2005.01.27.}
Added token declarations for |WITH_TIME_PARAMETER|,
|WITH_ORDER|, and |GENERATE|.

\initials{LDF 2005.01.28.}
Changed |WITH_ORDER| to |WITH_POWER|.
\ENDLOG

@q **** (4) Code.@>   

@<Token and precedence declarations@>=

@=%token <int_value> KNOT@>@/ 
@=%token <int_value> WEIGHT@>@/ 
@=%token <int_value> WITH_KNOTS@>@/ 
@=%token <int_value> WITH_WEIGHTS@>@/ 
@=%token <int_value> WITH_TIME_PARAMETER@>@/ 
@=%token <int_value> WITH_POWER@>@/ 

@=%token <int_value> GENERATE@>@/ 

@q *** (3) Tokens for |Polygons|.@>
@*2 Tokens for |Polygons|.
\initials{LDF 2006.01.20.}

\LOG
\initials{LDF 2006.01.20.}
Added this section with the token declaration for
|DISENTANGLE|.
\ENDLOG

@q **** (4) Code.@>   

@<Token and precedence declarations@>=

@=%token <int_value> DISENTANGLE@>@/ 

@q *** (3) Tokens for |origami_figures|.@>
@*2 Tokens for |nurbs|.
\initials{LDF 2005.02.03.}

\LOG
\initials{LDF 2005.02.03.}
Added this section.  It's currently empty.
\ENDLOG

@q **** (4) Code.@>   

@<Token and precedence declarations@>=


@q ** (2) Constants in |namespace Scan_Parse|. @>
@*1 Constants in {\bf namespace Scan\_Parse}. 
It is safe to assume that |TRANSFORM == MIN_SHAPE - 1|.  This can be useful
for testing whether a particular operation can be performed on objects in an
expression.  Some operations can be performed on {\bf numerics}, |Transforms|,
and/or |Shapes|.
\initials{LDF 2004.04.23.}

\LOG
\initials{LDF 2004.04.23.}  Added this section.

\initials{LDF 2004.05.02.}
Added |MIN_INTERNAL_QUANTITY| and |MAX_INTERNAL_QUANTITY|.

\initials{LDF 2004.11.07.}
Removed |MIN_INTERNAL_QUANTITY| and |MAX_INTERNAL_QUANTITY|.
\ENDLOG 

@<Constants in |namespace Scan_Parse|@>=
namespace
Scan_Parse
{
  extern const unsigned int MIN_SHAPE = POINT; 
  extern const unsigned int MAX_SHAPE = POLYHEDRON;

};

@
@<|extern| declarations in |namespace Scan_Parse|@>=
namespace
Scan_Parse
{
  extern const unsigned int MIN_SHAPE; 
  extern const unsigned int MAX_SHAPE;
};


@q ** (2) |name_map| and |type_name_map|. @>
@*1 {\bf name\_map} and {\bf type\_name\_map}. 
\initials{LDF Undated.}

\LOG
\initials{LDF 2004.04.28.}  
Changed |int| to |const int|.  Changing 
|string| to |const string| causes errors.  

\initials{LDF 2004.12.21.}
Added declaration of |type_name_map|.
\ENDLOG 

@q *** (3) Declaration .@> 

@<Declare |name_map| and |type_name_map|@>=
map <const int, string> name_map;
map <const int, string> type_name_map;

@q *** (3) |extern| declaration.@> 

@
@<|extern| declaration of |name_map| and |type_name_map|@>=
extern 
map <const int, string> name_map;

extern 
map <const int, string> type_name_map;

@q *** (3) Initialize |name_map| and |type_name_map|. @>
@*2 Initialize {\bf name\_map} and {\bf type\_name\_map}. 

\LOG
\initials{LDF 2005.06.09.}
Removed the definition of this function 
to \filename{prrfnc0.web}.
\ENDLOG 

@q **** (4) Declaration. @>

@<Declare functions@>=
int
initialize_name_map(void);




@q * Emacs-Lisp code for use in indirect buffers when using the          @>
@q   GNU Emacs editor.  The local variable list is not evaluated when an @>
@q   indirect buffer is visited, so it's necessary to evaluate the       @>
@q   following s-expression in order to use the facilities normally      @>
@q   accessed via the local variables list.                              @>
@q   \initials{LDF 2004.02.12}.                                          @>
@q   (progn (cweb-mode) (outline-minor-mode t) (setq fill-column 80))    @>


@q Local Variables: @>
@q mode:CWEB @>
@q eval:(outline-minor-mode t) @>
@q abbrev-file-name:"~/.abbrev_defs" @>
@q eval:(read-abbrev-file) @>
@q fill-column:80 @>
@q run-cweave-on-file:"3DLDFprg.web" @>
@q End: @>
