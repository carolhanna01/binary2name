@q pbpvexpr.w @> 
@q Created by Laurence Finston Fr Nov  5 22:10:03 CET 2004  @>
     
@q * (0) Copyright and License.@>

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



@q * (0) |bool_point_vector| expressions.@>
@** {\bf bool\_point\_vector} expressions.
\initials{LDF 2004.11.05.}

\LOG
\initials{LDF 2004.11.05.}
Created this file and wrote quite a few rules.  
\ENDLOG 


@q * (1) |bool_point_vector| primary.  @>
@* \§bool-point vector primary>.
\initials{LDF 2004.11.05.}
  
\LOG
\initials{LDF 2004.11.05.}
Added this type declaration.
\ENDLOG


@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> bool_point_vector_primary@>@/

@q ** (2) bool_point_vector_primary --> bool_point_vector_variable.@>
@*1 \§bool-point vector primary> $\longrightarrow$ 
\§bool-point vector variable>.  
\initials{LDF 2004.11.05.}

\LOG
\initials{LDF 2004.11.05.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=bool_point_vector_primary: bool_point_vector_variable@>@/ 
{

  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

@q **** (4) Error handling:  |entry == 0 || entry->object == 0|.@> 

@ Error handling:  |entry == 0 || entry->object == 0|.
\initials{LDF 2004.11.05.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0) || entry->object == static_cast<void*>(0))
    {

      @=$$@> = static_cast<void*>(0);

    } /* |if (entry == 0 || entry->object == 0)|  */

@q **** (4) |!(entry == 0 || entry->object == 0)|.@> 

@ |!(entry == 0 || entry->object == 0)|.
\initials{LDF 2004.11.05.}

@<Define rules@>=
  else /* |!(entry == 0 || entry->object == 0)|  */

     {


        typedef Pointer_Vector<Bool_Point> BPV;

        BPV* bpv = new BPV;

        *bpv = *static_cast<BPV*>(entry->object);

        @=$$@> = static_cast<void*>(bpv);                    

     }  /* |else| (|!(entry == 0 || entry->object == 0)|)  */

};


@q ** (2) bool_point_vector_primary --> LEFT_PARENTHESIS  @>
@q ** (2) bool_point_vector_expression  RIGHT_PARENTHESIS.@>

@*1 \§bool-point vector primary> $\longrightarrow$ \.{LEFT\_PARENTHESIS}
\§bool-point vector expression> \.{RIGHT\_PARENTHESIS}.
\initials{LDF 2004.11.05.}

\LOG
\initials{LDF 2004.11.05.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=bool_point_vector_primary: LEFT_PARENTHESIS@>@/ 
@=bool_point_vector_expression RIGHT_PARENTHESIS@>@/ 
{
   
  @=$$@> = @=$1@>;

};

@q * (1) bool_point_vector secondary.@>

@* \§bool-point vector secondary>.
\initials{LDF 2004.11.05.}

\LOG
\initials{LDF 2004.11.05.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> bool_point_vector_secondary@>
  
@q ** (2) bool_point_vector secondary --> bool_point_vector_primary.@>
@*1 \§bool-point vector secondary> $\longrightarrow$ 
\§bool-point vector primary>.
\initials{LDF 2004.11.05.}

\LOG
\initials{LDF 2004.11.05.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=bool_point_vector_secondary: bool_point_vector_primary@>@/ 
{

  @=$$@> = @=$1@>;

};

@q * (1) bool_point_vector tertiary.  @>

@* \§bool-point vector tertiary>.
\initials{LDF 2004.11.05.}

\LOG
\initials{LDF 2004.11.05.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> bool_point_vector_tertiary@>

@q ***** (5) bool_point_vector tertiary --> bool_point_vector_secondary.@>
@ \§bool-point vector tertiary> $\longrightarrow$ 
\§bool-point vector secondary>.
\initials{LDF 2004.11.05.}

\LOG
\initials{LDF 2004.11.05.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=bool_point_vector_tertiary: bool_point_vector_secondary@>@/ 
{

  @=$$@> = @=$1@>;

};

@q **** (4) bool_point_vector_tertiary: path_tertiary  @> 
@q **** (4) INTERSECTION_POINTS ellipse_secondary.     @> 

@*3 \§bool-point vector tertiary> $\longrightarrow$ 
\§path tertiary>
\.{INTERSECTION\_POINTS} \§ellipse secondary>.
\initials{LDF 2004.11.05.}

\LOG
\initials{LDF 2004.11.05.}
Added this rule.

\initials{LDF 2004.11.06.}
Corrected errors.

\initials{LDF 2005.10.24.}
Changed |ellipse_like_secondary| to |ellipse_secondary|.
Removed debugging code.

\initials{LDF 2005.10.27.}
Removed code.  Now calling |Scan_Parse::intersection_points_func()|.

\initials{LDF 2005.10.27.}
Removed more code.  Now calling 
|Pointer_Vector<Bool_Point>::convert_pair_pointer()|.
|Ellipse::intersection_points(Path)| returns a |Bool_Point_Pair|.
|Scan_Parse::intersection_points_func()| creates a new |Bool_Point_Pair|
on the free store, assigns to it from the first one, and returns a 
|void*| that points to the new one.
|Pointer_Vector<Bool_Point>::convert_pair_pointer()| creates two new
|Bool_Points| on the free store, pushes pointers to them onto 
the |Pointer_Vector<Bool_Point>|, and assigns to them from the 
two |Bool_Points| in the |Bool_Point_Pair|.  Then it deletes
the |Bool_Point_Pair*|.
\ENDLOG
 
@q ***** (5) Definition.@> 

@<Define rules@>= 
@=bool_point_vector_tertiary: path_tertiary INTERSECTION_POINTS @> 
@=ellipse_secondary@>@/
{

    Pointer_Vector<Bool_Point>* bpv = new Pointer_Vector<Bool_Point>;
   
    bpv->convert_pair_pointer(static_cast<Bool_Point_Pair*>(
                                 Scan_Parse::intersection_points_func<Ellipse, 
                                                                      Path, 
                                                                      Bool_Point_Pair>
                                            (static_cast<Ellipse*>(@=$3@>),
                                             static_cast<Path*>(@=$1@>), 
                                             parameter)));   

    @=$$@> = static_cast<void*>(bpv); 

};

@q **** (4) bool_point_vector_tertiary: ellipse_tertiary  @> 
@q **** (4) INTERSECTION_POINTS path_secondary.@> 

@*3 \§bool-point vector tertiary> $\longrightarrow$ 
\§ellipse tertiary> \.{INTERSECTION\_POINTS} \§path secondary>.
\initials{LDF 2004.11.05.}

\LOG
\initials{LDF 2004.11.06.}
Added this rule.

\initials{LDF 2004.11.06.}
Changed |ellipse_like_tertiary| to |ellipse_tertiary| 
because the rule  \§ellipse-like tertiary> $\longrightarrow$ 
\§ellipse tertiary> was never reduced because of conflicts.

\initials{LDF 2005.10.27.}
Removed code.  Now calling |Scan_Parse::intersection_points_func()|
and |Pointer_Vector<Bool_Point>::convert_pair_pointer()|.
|Ellipse::intersection_points(Path)| returns a |Bool_Point_Pair|.
|Scan_Parse::intersection_points_func()| creates a new |Bool_Point_Pair|
on the free store, assigns to it from the first one, and returns a 
|void*| that points to the new one.
|Pointer_Vector<Bool_Point>::convert_pair_pointer()| creates two new
|Bool_Points| on the free store, pushes pointers to them onto 
the |Pointer_Vector<Bool_Point>|, and assigns to them from the 
two |Bool_Points| in the |Bool_Point_Pair|.  Then it deletes
the |Bool_Point_Pair*|.
\ENDLOG
 
@q ***** (5) Definition.@> 

@<Define rules@>= 
@=bool_point_vector_tertiary: ellipse_tertiary INTERSECTION_POINTS @> 
@=path_secondary@>@/
{

    Pointer_Vector<Bool_Point>* bpv = new Pointer_Vector<Bool_Point>;
   
    bpv->convert_pair_pointer(static_cast<Bool_Point_Pair*>(
                                 Scan_Parse::intersection_points_func<Ellipse, 
                                                                      Path, 
                                                                      Bool_Point_Pair>
                                            (static_cast<Ellipse*>(@=$1@>),
                                             static_cast<Path*>(@=$3@>), 
                                             parameter)));   

    @=$$@> = static_cast<void*>(bpv); 

};

@q **** (4) bool_point_vector_tertiary: ellipse_tertiary  @> 
@q **** (4) INTERSECTION_POINTS rectangle_secondary.      @> 

@*3 \§bool-point vector tertiary> $\longrightarrow$ 
\§ellipse tertiary> \.{INTERSECTION\_POINTS} \§rectangle secondary>.
\initials{LDF 2010.09.10.}

\LOG
\initials{LDF 2010.09.10.}
Added this rule.
\ENDLOG
 
@q ***** (5) Definition.@> 

@<Define rules@>= 
@=bool_point_vector_tertiary: ellipse_tertiary INTERSECTION_POINTS @> 
@=rectangle_secondary@>@/
{

    Pointer_Vector<Bool_Point>* bpv = new Pointer_Vector<Bool_Point>;
   
    bpv->convert_pair_pointer(static_cast<Bool_Point_Pair*>(
                                 Scan_Parse::intersection_points_func<Ellipse, 
                                                                      Rectangle, 
                                                                      Bool_Point_Pair>
                                            (static_cast<Ellipse*>(@=$1@>),
                                             static_cast<Rectangle*>(@=$3@>), 
                                             parameter)));   

    @=$$@> = static_cast<void*>(bpv); 

};




@q **** (4) bool_point_vector_tertiary: circle_tertiary  @> 
@q **** (4) INTERSECTION_POINTS path_secondary.@> 

@*3 \§bool-point vector tertiary> $\longrightarrow$ 
\§circle tertiary> \.{INTERSECTION\_POINTS} \§path secondary>.
\initials{LDF 2005.10.28.}

\LOG
\initials{LDF 2005.10.28.}
Added this rule.
\ENDLOG
 
@q ***** (5) Definition.@> 

@<Define rules@>= 
@=bool_point_vector_tertiary: circle_tertiary INTERSECTION_POINTS @> 
@=path_secondary@>@/
{

    Pointer_Vector<Bool_Point>* bpv = new Pointer_Vector<Bool_Point>;
   
    bpv->convert_pair_pointer(static_cast<Bool_Point_Pair*>(
                                 Scan_Parse::intersection_points_func<Circle, 
                                                                      Path, 
                                                                      Bool_Point_Pair>
                                            (static_cast<Circle*>(@=$1@>),
                                             static_cast<Path*>(@=$3@>), 
                                             parameter)));   

    @=$$@> = static_cast<void*>(bpv); 

};

@q **** (4) bool_point_vector_tertiary: path_tertiary  @> 
@q **** (4) INTERSECTION_POINTS circle_secondary.@> 

@*3 \§bool-point vector tertiary> $\longrightarrow$ 
\§path tertiary> \.{INTERSECTION\_POINTS} \§circle secondary>.
\initials{LDF 2005.10.28.}

\LOG
\initials{LDF 2005.10.28.}
Added this rule.
\ENDLOG
 
@q ***** (5) Definition.@> 

@<Define rules@>= 
@=bool_point_vector_tertiary: path_tertiary INTERSECTION_POINTS @> 
@=circle_secondary@>@/
{

    Pointer_Vector<Bool_Point>* bpv = new Pointer_Vector<Bool_Point>;
   
    bpv->convert_pair_pointer(static_cast<Bool_Point_Pair*>(
                                 Scan_Parse::intersection_points_func<Circle, 
                                                                      Path, 
                                                                      Bool_Point_Pair>
                                            (static_cast<Circle*>(@=$3@>),
                                             static_cast<Path*>(@=$1@>), 
                                             parameter)));   

    @=$$@> = static_cast<void*>(bpv); 

};




@q **** (4) bool_point_vector_tertiary: ellipse_tertiary  @> 
@q **** (4) INTERSECTION_POINTS ellipse_secondary.@> 

@*3 \§bool-point vector tertiary> $\longrightarrow$ 
\§ellipse tertiary>.
\.{INTERSECTION\_POINTS} \§ellipse secondary>
\initials{LDF 2004.11.06.}

\LOG
\initials{LDF 2004.11.06.}
Added this rule.

\initials{LDF 2005.10.27.}
Removed code.  Now calling |Scan_Parse::intersection_points_func()| and
|Scan_Parse::convert()|.

\initials{LDF 2005.10.28.}
Now calling |Scan_Parse::ellipse_intersection_func()|.

\initials{LDF 2005.10.28.}
Now calling |Scan_Parse::ellipse_like_intersection_func()|.
\ENDLOG
 
@q ***** (5) Definition.@> 

@<Define rules@>= 
@=bool_point_vector_tertiary: ellipse_tertiary INTERSECTION_POINTS @> 
@=ellipse_secondary@>@/
{
   @=$$@> 
      = Scan_Parse::ellipse_like_intersection_func<Ellipse, Ellipse>
           (static_cast<Ellipse*>(@=$1@>), 
            static_cast<Ellipse*>(@=$3@>), 
            parameter); 

};

@q **** (4) bool_point_vector_tertiary: circle_tertiary @> 
@q **** (4) INTERSECTION_POINTS circle_secondary.       @> 

@*3 \§bool-point vector tertiary> $\longrightarrow$ 
\§circle tertiary>.
\.{INTERSECTION\_POINTS} \§circle secondary>
\initials{LDF 2004.11.07.}

\LOG
\initials{LDF 2004.11.07.}
Added this rule.

\initials{LDF 2005.10.28.}
Removed code.  Now calling |Scan_Parse::ellipse_like_intersection_func()|.
\ENDLOG
 
@q ***** (5) Definition.@> 

@<Define rules@>= 
@=bool_point_vector_tertiary: circle_tertiary INTERSECTION_POINTS @> 
@=circle_secondary@>@/
{
   @=$$@> 
      = Scan_Parse::ellipse_like_intersection_func<Circle, Circle>
           (static_cast<Circle*>(@=$1@>), 
            static_cast<Circle*>(@=$3@>), 
            parameter); 


};

@q **** (4) bool_point_vector_tertiary: ellipse_tertiary @> 
@q **** (4) INTERSECTION_POINTS circle_secondary.       @> 

@*3 \§bool-point vector tertiary> $\longrightarrow$ 
\§circle tertiary>.
\.{INTERSECTION\_POINTS} \§circle secondary>
\initials{LDF 2004.11.07.}

\LOG
\initials{LDF 2004.11.07.}
Added this rule.

\initials{LDF 2005.10.28.}
Removed code.  Now calling |Scan_Parse::ellipse_like_intersection_func()|.
\ENDLOG
 
@q ***** (5) Definition.@> 

@<Define rules@>= 
@=bool_point_vector_tertiary: ellipse_tertiary INTERSECTION_POINTS @> 
@=circle_secondary@>@/
{
   @=$$@> 
      = Scan_Parse::ellipse_like_intersection_func<Ellipse, Ellipse>
           (static_cast<Ellipse*>(@=$1@>), 
            static_cast<Ellipse*>(@=$3@>), 
            parameter); 
};

@q **** (4) bool_point_vector_tertiary: circle_tertiary @> 
@q **** (4) INTERSECTION_POINTS ellipse_secondary.       @> 

@*3 \§bool-point vector tertiary> $\longrightarrow$ 
\§circle tertiary>.
\.{INTERSECTION\_POINTS} \§circle secondary>
\initials{LDF 2004.11.07.}

\LOG
\initials{LDF 2004.11.07.}
Added this rule.

\initials{LDF 2004.11.08.}
@:BUG FIX@> BUG FIX:  Changed |Bool_Point_Pair bpp| to 
|Bool_Point_Quadruple bpq|.  Now calling 
|c->Ellipse::intersection_points()| explicitly, since the 
compiler didn't resolve the call to |c->intersection_points()| 
this way.  Now allocating memory for four |Bool_Points| rather than
two and setting |bpv->ctr| accordingly.

\initials{LDF 2005.10.28.}
Removed code.  Now calling |Scan_Parse::ellipse_like_intersection_func()|.
\ENDLOG
 
@q ***** (5) Definition.@> 

@<Define rules@>= 
@=bool_point_vector_tertiary: circle_tertiary INTERSECTION_POINTS @> 
@=ellipse_secondary@>@/
{


   @=$$@> 
      = Scan_Parse::ellipse_like_intersection_func<Ellipse, Ellipse>
           (static_cast<Ellipse*>(@=$1@>), 
            static_cast<Ellipse*>(@=$3@>), 
            parameter); 

};


@q ** (2) bool_point_vector_tertiary: parabola_tertiary @> 
@q ** (2) INTERSECTION_POINTS path_secondary.  @> 

@*1 \§bool-point vector tertiary> $\longrightarrow$ 
\§parabola tertiary>
\.{INTERSECTION\_POINTS} \§path secondary>.
\initials{LDF 2005.11.17.}

\LOG
\initials{LDF 2005.11.17.}
Added this rule.

\initials{LDF 2005.11.21.}
Changed the left-hand side from \§point vector tertiary> to 
\§bool-point vector tertiary> and moved from \filename{pptvexpr.w} to 
this file (\filename{pbpvexpr.w}.

\initials{LDF 2005.11.21.}
Changed the name of |conic_section_line_intersection_func()|
to |conic_section_intersection_func()| and added a second 
template parameter.  Changed the |Path*| argument to 
a pointer to the type of the new template parameter.
Made the corresponding changes here.
\ENDLOG
 
@q *** (3) Definition.@> 

@<Define rules@>= 
@=bool_point_vector_tertiary: parabola_tertiary INTERSECTION_POINTS @> 
@=path_secondary@>@/
{

   @=$$@> = Scan_Parse::conic_section_intersection_func<Parabola, Path>(
                static_cast<Parabola*>(@=$1@>),
                static_cast<Path*>(@=$3@>),
                parameter);
};

@q ** (2) bool_point_vector_tertiary: path_tertiary @> 
@q ** (2) INTERSECTION_POINTS parabola_secondary.  @> 

@*1 \§bool-point vector tertiary> $\longrightarrow$ 
\§path tertiary>
\.{INTERSECTION\_POINTS} \§parabola secondary>.
\initials{LDF 2005.11.17.}

\LOG
\initials{LDF 2005.11.17.}
Added this rule.

\initials{LDF 2005.11.21.}
Changed the left-hand side from \§point vector tertiary> to 
\§bool-point vector tertiary> and moved from \filename{pptvexpr.w} to 
this file (\filename{pbpvexpr.w}.

\initials{LDF 2005.11.21.}
Changed the name of |conic_section_line_intersection_func()|
to |conic_section_intersection_func()| and added a second 
template parameter.  Changed the |Path*| argument to 
a pointer to the type of the new template parameter.
Made the corresponding changes here.
\ENDLOG
 
@q *** (3) Definition.@> 

@<Define rules@>= 
@=bool_point_vector_tertiary: path_tertiary INTERSECTION_POINTS @> 
@=parabola_secondary@>@/
{

   @=$$@> = Scan_Parse::conic_section_intersection_func<Parabola, Path>(
                static_cast<Parabola*>(@=$3@>),
                static_cast<Path*>(@=$1@>),
                parameter);
};

@q ** (2) bool_point_vector_tertiary: parabola_tertiary @> 
@q ** (2) INTERSECTION_POINTS plane_secondary.  @> 

@*1 \§bool-point vector tertiary> $\longrightarrow$ 
\§parabola tertiary>
\.{INTERSECTION\_POINTS} \§plane secondary>.
\initials{LDF 2005.11.21.}

\LOG
\initials{LDF 2005.11.21.}
Added this rule.
\ENDLOG
 
@q *** (3) Definition.@> 

@<Define rules@>= 
@=bool_point_vector_tertiary: parabola_tertiary INTERSECTION_POINTS @> 
@=plane_secondary@>@/
{

   @=$$@> = Scan_Parse::conic_section_intersection_func<Parabola, Plane>(
                static_cast<Parabola*>(@=$1@>),
                static_cast<Plane*>(@=$3@>),
                parameter);
};

@q ** (2) bool_point_vector_tertiary: plane_tertiary @> 
@q ** (2) INTERSECTION_POINTS parabola_secondary.  @> 

@*1 \§bool-point vector tertiary> $\longrightarrow$ 
\§plane tertiary>
\.{INTERSECTION\_POINTS} \§parabola secondary>.
\initials{LDF 2005.11.21.}

\LOG
\initials{LDF 2005.11.21.}
Added this rule.
\ENDLOG
 
@q *** (3) Definition.@> 

@<Define rules@>= 
@=bool_point_vector_tertiary: plane_tertiary INTERSECTION_POINTS @> 
@=parabola_secondary@>@/
{

   @=$$@> = Scan_Parse::conic_section_intersection_func<Parabola, Plane>(
                static_cast<Parabola*>(@=$3@>),
                static_cast<Plane*>(@=$1@>),
                parameter);
};

@q ** (2) bool_point_vector_tertiary: hyperbola_tertiary @> 
@q ** (2) INTERSECTION_POINTS path_secondary.  @> 

@*1 \§bool-point vector tertiary> $\longrightarrow$ 
\§hyperbola tertiary>
\.{INTERSECTION\_POINTS} \§path secondary>.
\initials{LDF 2005.11.25.}

\LOG
\initials{LDF 2005.11.25.}
Added this rule.

\ENDLOG
 
@q *** (3) Definition.@> 

@<Define rules@>= 
@=bool_point_vector_tertiary: hyperbola_tertiary INTERSECTION_POINTS @> 
@=path_secondary@>@/
{

   @=$$@> = Scan_Parse::conic_section_intersection_func<Hyperbola, Path>(
                static_cast<Hyperbola*>(@=$1@>),
                static_cast<Path*>(@=$3@>),
                parameter);
};

@q ** (2) bool_point_vector_tertiary: path_tertiary @> 
@q ** (2) INTERSECTION_POINTS hyperbola_secondary.  @> 

@*1 \§bool-point vector tertiary> $\longrightarrow$ 
\§path tertiary>
\.{INTERSECTION\_POINTS} \§hyperbola secondary>.
\initials{LDF 2005.11.25.}

\LOG
\initials{LDF 2005.11.25.}
\ENDLOG
 
@q *** (3) Definition.@> 

@<Define rules@>= 
@=bool_point_vector_tertiary: path_tertiary INTERSECTION_POINTS @> 
@=hyperbola_secondary@>@/
{

   @=$$@> = Scan_Parse::conic_section_intersection_func<Hyperbola, Path>(
                static_cast<Hyperbola*>(@=$3@>),
                static_cast<Path*>(@=$1@>),
                parameter);
};


@q ** (2) bool_point_vector_tertiary: hyperbola_tertiary @> 
@q ** (2) INTERSECTION_POINTS plane_secondary.  @> 

@*1 \§bool-point vector tertiary> $\longrightarrow$ 
\§hyperbola tertiary>
\.{INTERSECTION\_POINTS} \§plane secondary>.
\initials{LDF 2005.11.28.}

\LOG
\initials{LDF 2005.11.28.}
Added this rule.
\ENDLOG
 
@q *** (3) Definition.@> 

@<Define rules@>= 
@=bool_point_vector_tertiary: hyperbola_tertiary INTERSECTION_POINTS @> 
@=plane_secondary@>@/
{

   @=$$@> = Scan_Parse::conic_section_intersection_func<Hyperbola, Plane>(
                static_cast<Hyperbola*>(@=$1@>),
                static_cast<Plane*>(@=$3@>),
                parameter);
};

@q ** (2) bool_point_vector_tertiary: plane_tertiary @> 
@q ** (2) INTERSECTION_POINTS hyperbola_secondary.  @> 

@*1 \§bool-point vector tertiary> $\longrightarrow$ 
\§plane tertiary>
\.{INTERSECTION\_POINTS} \§hyperbola secondary>.
\initials{LDF 2005.11.28.}

\LOG
\initials{LDF 2005.11.28.}
Added this rule.
\ENDLOG
 
@q *** (3) Definition.@> 

@<Define rules@>= 
@=bool_point_vector_tertiary: plane_tertiary INTERSECTION_POINTS @> 
@=hyperbola_secondary@>@/
{

   @=$$@> = Scan_Parse::conic_section_intersection_func<Hyperbola, Plane>(
                static_cast<Hyperbola*>(@=$3@>),
                static_cast<Plane*>(@=$1@>),
                parameter);
};

@q **** (4) bool_point_vector_tertiary: path_tertiary  @> 
@q **** (4) INTERSECTION_POINTS polygon_secondary.@> 

@*3 \§bool-point vector tertiary> $\longrightarrow$ 
\§path tertiary>
\.{INTERSECTION\_POINTS} \§polygon secondary>.
\initials{LDF 2004.11.07.}

\LOG
\initials{LDF 2004.11.07.}
Added this rule.

\initials{LDF 2005.10.24.}
Changed |polygon_like_secondary| to |polygon_secondary|.
Removed debugging code.

\initials{LDF 2005.10.28.}
Removed code.  Now calling |Scan_Parse::intersection_points_func()|.
\ENDLOG
 
@q ***** (5) Definition.@> 

@<Define rules@>= 
@=bool_point_vector_tertiary: path_tertiary INTERSECTION_POINTS @> 
@=polygon_secondary@>@/
{

    Pointer_Vector<Bool_Point>* bpv = new Pointer_Vector<Bool_Point>;
   
    bpv->convert_pair_pointer(static_cast<Bool_Point_Pair*>(
                                 Scan_Parse::intersection_points_func<Polygon, 
                                                                      Path, 
                                                                      Bool_Point_Pair>
                                            (static_cast<Polygon*>(@=$3@>),
                                             static_cast<Path*>(@=$1@>), 
                                             parameter)));   
    @=$$@> = static_cast<void*>(bpv); 

};

@q **** (4) bool_point_vector_tertiary: polygon_tertiary @> 
@q **** (4) INTERSECTION_POINTS path_secondary.          @> 

@*3 \§bool-point vector tertiary> $\longrightarrow$ 
\§polygon tertiary> \.{INTERSECTION\_POINTS} \§path secondary>.
\initials{LDF 2004.11.09.}

\LOG
\initials{LDF 2004.11.09.}
Added this rule.

\initials{LDF 2005.10.24.}
Changed |polygon_like_tertiary| to |polygon_tertiary|.
Removed debugging code.

\initials{LDF 2005.10.28.}
Removed code.  Now calling |Scan_Parse::intersection_points_func()|.
\ENDLOG
 
@q ***** (5) Definition.@> 

@<Define rules@>= 
@=bool_point_vector_tertiary: polygon_tertiary INTERSECTION_POINTS @> 
@=path_secondary@>@/
{

    Pointer_Vector<Bool_Point>* bpv = new Pointer_Vector<Bool_Point>;
   
    bpv->convert_pair_pointer(static_cast<Bool_Point_Pair*>(
                                 Scan_Parse::intersection_points_func<Polygon, 
                                                                      Path, 
                                                                      Bool_Point_Pair>
                                            (static_cast<Polygon*>(@=$1@>),
                                             static_cast<Path*>(@=$3@>), 
                                             parameter)));   

    @=$$@> = static_cast<void*>(bpv); 

};

@q **** (4) bool_point_vector_tertiary: path_tertiary  @> 
@q **** (4) INTERSECTION_POINTS reg_polygon_secondary.@> 

@*3 \§bool-point vector tertiary> $\longrightarrow$ 
\§path tertiary>
\.{INTERSECTION\_POINTS} \§regular polygon secondary>.
\initials{LDF 2005.10.28.}

\LOG
\initials{LDF 2005.10.28.}
Added this rule.
\ENDLOG
 
@q ***** (5) Definition.@> 

@<Define rules@>= 
@=bool_point_vector_tertiary: path_tertiary INTERSECTION_POINTS @> 
@=reg_polygon_secondary@>@/
{

    Pointer_Vector<Bool_Point>* bpv = new Pointer_Vector<Bool_Point>;
   
    bpv->convert_pair_pointer(static_cast<Bool_Point_Pair*>(
                                 Scan_Parse::intersection_points_func<Reg_Polygon, 
                                                                      Path, 
                                                                      Bool_Point_Pair>
                                            (static_cast<Reg_Polygon*>(@=$3@>),
                                             static_cast<Path*>(@=$1@>), 
                                             parameter)));   
    @=$$@> = static_cast<void*>(bpv); 

};

@q **** (4) bool_point_vector_tertiary: reg_polygon_tertiary @> 
@q **** (4) INTERSECTION_POINTS path_secondary.          @> 

@*3 \§bool-point vector tertiary> $\longrightarrow$ 
\§regular polygon tertiary> \.{INTERSECTION\_POINTS} \§path secondary>.
\initials{LDF 2005.10.28.}

\LOG
\initials{LDF 2005.10.28.}
Added this rule.
\ENDLOG
 
@q ***** (5) Definition.@> 

@<Define rules@>= 
@=bool_point_vector_tertiary: reg_polygon_tertiary INTERSECTION_POINTS @> 
@=path_secondary@>@/
{



    Pointer_Vector<Bool_Point>* bpv = new Pointer_Vector<Bool_Point>;




   
    bpv->convert_pair_pointer(static_cast<Bool_Point_Pair*>(
                                 Scan_Parse::intersection_points_func<Reg_Polygon, 
                                                                      Path, 
                                                                      Bool_Point_Pair>
                                            (static_cast<Reg_Polygon*>(@=$1@>),
                                             static_cast<Path*>(@=$3@>), 
                                             parameter)));   
    @=$$@> = static_cast<void*>(bpv); 


};


@q **** (4) bool_point_vector_tertiary: path_tertiary  @> 
@q **** (4) INTERSECTION_POINTS rectangle_secondary.@> 

@*3 \§bool-point vector tertiary> $\longrightarrow$ 
\§path tertiary>
\.{INTERSECTION\_POINTS} \§rectangle secondary>.
\initials{LDF 2005.10.28.}

\LOG
\initials{LDF 2005.10.28.}
Added this rule.
\ENDLOG
 
@q ***** (5) Definition.@> 

@<Define rules@>= 
@=bool_point_vector_tertiary: path_tertiary INTERSECTION_POINTS @> 
@=rectangle_secondary@>@/
{

    Pointer_Vector<Bool_Point>* bpv = new Pointer_Vector<Bool_Point>;
   
    bpv->convert_pair_pointer(static_cast<Bool_Point_Pair*>(
                              Scan_Parse::intersection_points_func<Rectangle,
                                                                   Path, 
                                                                   Bool_Point_Pair>
                                                  (static_cast<Rectangle*>(@=$3@>),
                                                   static_cast<Path*>(@=$1@>), 
                                                   parameter)));


    @=$$@> = static_cast<void*>(bpv); 

};

@q **** (4) bool_point_vector_tertiary: rectangle_tertiary @> 
@q **** (4) INTERSECTION_POINTS path_secondary.          @> 

@*3 \§bool-point vector tertiary> $\longrightarrow$ 
\§rectangle tertiary> \.{INTERSECTION\_POINTS} \§path secondary>.
\initials{LDF 2005.10.28.}

\LOG
\initials{LDF 2005.10.28.}
Added this rule.
\ENDLOG
 
@q ***** (5) Definition.@> 

@<Define rules@>= 
@=bool_point_vector_tertiary: rectangle_tertiary INTERSECTION_POINTS @> 
@=path_secondary@>@/
{

    Pointer_Vector<Bool_Point>* bpv = new Pointer_Vector<Bool_Point>;
   
    bpv->convert_pair_pointer(static_cast<Bool_Point_Pair*>(
                                 Scan_Parse::intersection_points_func<Rectangle, 
                                                                      Path, 
                                                                      Bool_Point_Pair>
                                            (static_cast<Rectangle*>(@=$1@>),
                                             static_cast<Path*>(@=$3@>), 
                                             parameter)));   

    @=$$@> = static_cast<void*>(bpv); 

};

@q **** (4) bool_point_vector_tertiary: path_tertiary  @> 
@q **** (4) INTERSECTION_POINTS triangle_secondary.@> 

@*3 \§bool-point vector tertiary> $\longrightarrow$ 
\§path tertiary> \.{INTERSECTION\_POINTS} \§triangle secondary>.
\initials{LDF 2005.10.28.}

\LOG
\initials{LDF 2005.10.28.}
Added this rule.
\ENDLOG
 
@q ***** (5) Definition.@> 

@<Define rules@>= 
@=bool_point_vector_tertiary: path_tertiary INTERSECTION_POINTS @> 
@=triangle_secondary@>@/
{

    Pointer_Vector<Bool_Point>* bpv = new Pointer_Vector<Bool_Point>;
   
    bpv->convert_pair_pointer(static_cast<Bool_Point_Pair*>(
                                 Scan_Parse::intersection_points_func<Triangle, 
                                                                      Path, 
                                                                      Bool_Point_Pair>
                                            (static_cast<Triangle*>(@=$3@>),
                                             static_cast<Path*>(@=$1@>), 
                                             parameter)));   
    @=$$@> = static_cast<void*>(bpv); 

};

@q **** (4) bool_point_vector_tertiary: triangle_tertiary @> 
@q **** (4) INTERSECTION_POINTS path_secondary.          @> 

@*3 \§bool-point vector tertiary> $\longrightarrow$ 
\§triangle tertiary> \.{INTERSECTION\_POINTS} \§path secondary>.
\initials{LDF 2005.10.28.}

\LOG
\initials{LDF 2005.10.28.}
Added this rule.
\ENDLOG
 
@q ***** (5) Definition.@> 

@<Define rules@>= 
@=bool_point_vector_tertiary: triangle_tertiary INTERSECTION_POINTS @> 
@=path_secondary@>@/
{

    Pointer_Vector<Bool_Point>* bpv = new Pointer_Vector<Bool_Point>;
   
    bpv->convert_pair_pointer(static_cast<Bool_Point_Pair*>(
                                 Scan_Parse::intersection_points_func<Triangle, 
                                                                      Path, 
                                                                      Bool_Point_Pair>
                                            (static_cast<Triangle*>(@=$1@>),
                                             static_cast<Path*>(@=$3@>), 
                                             parameter)));   

    @=$$@> = static_cast<void*>(bpv); 

};

@q ** (2) bool_point_vector_tertiary: conic_section_lattice_tertiary @> 
@q ** (2) INTERSECTION_POINTS path_secondary with_tolerance_optional @> 
@q ** (2) with_increment_optional.                                   @>

@*1 \§bool-point vector tertiary> $\longrightarrow$ 
\§conic section lattice tertiary> \.{INTERSECTION\_POINTS} 
\§path secondary> \§with tolerance optional> 
\§with increment optional>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.

\initials{LDF 2007.08.21.}
Added \§with tolerance optional> and \§with increment optional>
\ENDLOG
 
@q ***** (5) Definition.@> 

@<Define rules@>= 
@=bool_point_vector_tertiary: conic_section_lattice_tertiary @> 
@=INTERSECTION_POINTS path_secondary with_tolerance_optional@>@/
@=with_increment_optional@>@/

{
    @=$$@> 
       = Scan_Parse::conic_section_lattice_intersection_func<Path>(
            static_cast<Scanner_Node>(parameter), 
            static_cast<Conic_Section_Lattice*>(@=$1@>),
            static_cast<Path*>(@=$3@>),
            @=$4@>,
            @=$5@>);
};


@q ** (2) bool_point_vector_tertiary: path_tertiary           @> 
@q ** (2) INTERSECTION_POINTS conic_section_lattice_secondary @> 
@q ** (2) with_tolerance_optional with_increment_optional     @>

@*1 \§bool-point vector tertiary> $\longrightarrow$ 
\§path tertiary> \.{INTERSECTION\_POINTS} 
\§conic section lattice secondary> \§with tolerance optional>
\§with increment optional>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.

\initials{LDF 2007.08.21.}
Added \§with tolerance optional> and \§with increment optional>.
\ENDLOG
 
@q ***** (5) Definition.@> 

@<Define rules@>= 
@=bool_point_vector_tertiary: path_tertiary INTERSECTION_POINTS @> 
@=conic_section_lattice_secondary with_tolerance_optional@>@/
@=with_increment_optional@>@/
{
    @=$$@> 
       = Scan_Parse::conic_section_lattice_intersection_func<Path>(
            static_cast<Scanner_Node>(parameter), 
            static_cast<Conic_Section_Lattice*>(@=$3@>),
            static_cast<Path*>(@=$1@>),
            @=$4@>,
            @=$5@>);
};

@q **** (4) bool_point_vector_tertiary: circle_tertiary         @> 
@q **** (4) INTERSECTION_POINTS conic_section_lattice_secondary @> 
@q **** (4) with_tolerance_optional with_increment_optional.    @> 

@*3 \§bool-point vector tertiary> $\longrightarrow$ 
\§circle tertiary> \.{INTERSECTION\_POINTS} 
\§conic section lattice secondary> 
\§with tolerance optional> \§with increment optional>.
\initials{LDF 2007.08.21.}

\LOG
\initials{LDF 2007.08.21.}
Added this rule.
\ENDLOG
 
@q ***** (5) Definition.@> 

@<Define rules@>= 
@=bool_point_vector_tertiary: circle_tertiary INTERSECTION_POINTS @> 
@=conic_section_lattice_secondary@>@/
@=with_tolerance_optional with_increment_optional@>@/
{

   @=$$@> = conic_section_lattice_intersection_func<Circle>(
                static_cast<Scanner_Node>(parameter),
                static_cast<Conic_Section_Lattice*>(@=$3@>),
                static_cast<Circle*>(@=$1@>),
                @=$4@>, 
                @=$5@>); 

 
};

@q **** (4) bool_point_vector_tertiary: conic_section_lattice_tertiary @> 
@q **** (4) INTERSECTION_POINTS circle_secondary.                      @> 
@q **** (4) with_tolerance_optional with_increment_optional.           @> 

@*3 \§bool-point vector tertiary> $\longrightarrow$ 
\§conic section lattice tertiary>.
\.{INTERSECTION\_POINTS} \§circle secondary>
\§with tolerance optional> \§with increment optional>.
\initials{LDF 2007.08.21.}

\LOG
\initials{LDF 2007.08.21.}
Added this rule.
\ENDLOG
 
@q ***** (5) Definition.@> 

@<Define rules@>= 
@=bool_point_vector_tertiary: conic_section_lattice_tertiary @> 
@=INTERSECTION_POINTS circle_secondary@>@/
@=with_tolerance_optional with_increment_optional@>@/
{
   @=$$@> = conic_section_lattice_intersection_func<Circle>(
                static_cast<Scanner_Node>(parameter),
                static_cast<Conic_Section_Lattice*>(@=$1@>),
                static_cast<Circle*>(@=$3@>),
                @=$4@>, 
                @=$5@>); 

};

@q **** (4) bool_point_vector_tertiary: ellipse_tertiary         @> 
@q **** (4) INTERSECTION_POINTS conic_section_lattice_secondary @> 
@q **** (4) with_tolerance_optional with_increment_optional.    @> 

@*3 \§bool-point vector tertiary> $\longrightarrow$ 
\§ellipse tertiary> \.{INTERSECTION\_POINTS} 
\§conic section lattice secondary> 
\§with tolerance optional> \§with increment optional>.
\initials{LDF 2007.08.26.}

\LOG
\initials{LDF 2007.08.26.}
Added this rule.
\ENDLOG
 
@q ***** (5) Definition.@> 

@<Define rules@>= 
@=bool_point_vector_tertiary: ellipse_tertiary INTERSECTION_POINTS @> 
@=conic_section_lattice_secondary@>@/
@=with_tolerance_optional with_increment_optional@>@/
{

   @=$$@> = conic_section_lattice_intersection_func<Ellipse>(
                static_cast<Scanner_Node>(parameter),
                static_cast<Conic_Section_Lattice*>(@=$3@>),
                static_cast<Ellipse*>(@=$1@>),
                @=$4@>, 
                @=$5@>); 

 
};

@q **** (4) bool_point_vector_tertiary: conic_section_lattice_tertiary @> 
@q **** (4) INTERSECTION_POINTS ellipse_secondary.                      @> 
@q **** (4) with_tolerance_optional with_increment_optional.           @> 

@*3 \§bool-point vector tertiary> $\longrightarrow$ 
\§conic section lattice tertiary>.
\.{INTERSECTION\_POINTS} \§ellipse secondary>
\§with tolerance optional> \§with increment optional>.
\initials{LDF 2007.08.26.}

\LOG
\initials{LDF 2007.08.26.}
Added this rule.
\ENDLOG
 
@q ***** (5) Definition.@> 

@<Define rules@>= 
@=bool_point_vector_tertiary: conic_section_lattice_tertiary @> 
@=INTERSECTION_POINTS ellipse_secondary@>@/
@=with_tolerance_optional with_increment_optional@>@/
{
   @=$$@> = conic_section_lattice_intersection_func<Ellipse>(
                static_cast<Scanner_Node>(parameter),
                static_cast<Conic_Section_Lattice*>(@=$1@>),
                static_cast<Ellipse*>(@=$3@>),
                @=$4@>, 
                @=$5@>); 

};

@q ** (2) bool_point_vector_tertiary: ellipsoid_tertiary @> 
@q ** (2) INTERSECTION_POINTS path_secondary.  @> 

@*1 \§bool-point vector tertiary> $\longrightarrow$ 
\§ellipsoid tertiary> \.{INTERSECTION\_POINTS} \§path secondary>.
\initials{LDF 2005.12.07.}

\LOG
\initials{LDF 2005.12.07.}
Added this rule.

\initials{LDF 2005.12.09.}
@:BUG FIX@> BUG FIX:  Now deleting |Ellipsoid* e| and |Path* p|.

\initials{LDF 2005.12.09.}
Changed \§point vector tertiary> to \§bool-point vector tertiary>, and
moved this rule from \filename{pptvexpr.w} to 
this file(\filename{pbpvexpr.w}).
\ENDLOG
 
@q *** (3) Definition.@> 

@<Define rules@>= 
@=bool_point_vector_tertiary: ellipsoid_tertiary INTERSECTION_POINTS @> 
@=path_secondary@>@/
{
     Ellipsoid* e = static_cast<Ellipsoid*>(@=$1@>);
     Path*      p = static_cast<Path*>(@=$3@>);

     if (!(e && p))
        @=$$@> = static_cast<void*>(0);

     else 
        @=$$@> = e->intersection_points(p, static_cast<Scanner_Node>(parameter));

     delete e;
     delete p;
     e = 0;
     p = 0;

};


@q ** (2) bool_point_vector_tertiary: path_tertiary @> 
@q ** (2) INTERSECTION_POINTS ellipsoid_secondary.  @> 

@*1 \§bool-point vector tertiary> $\longrightarrow$ 
\§path tertiary> \.{INTERSECTION\_POINTS} \§ellipsoid secondary>.
\initials{LDF 2005.12.07.}

\LOG
\initials{LDF 2005.12.07.}
Added this rule.

\initials{LDF 2005.12.09.}
Changed \§point vector tertiary> to \§bool-point vector tertiary>, and
moved this rule from \filename{pptvexpr.w} to 
this file(\filename{pbpvexpr.w}).
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>= 
@=bool_point_vector_tertiary: path_tertiary INTERSECTION_POINTS @> 
@=ellipsoid_secondary@>@/
{
     Ellipsoid* e = static_cast<Ellipsoid*>(@=$3@>);
     Path*      p = static_cast<Path*>(@=$1@>);

     if (!(e && p))
        @=$$@> = static_cast<void*>(0);

     else 
        @=$$@> = e->intersection_points(p, static_cast<Scanner_Node>(parameter));

     delete e;
     delete p;
     e = 0;
     p = 0;
};

@q **** (4) bool_point_vector_tertiary: sphere_tertiary @> 
@q **** (4) INTERSECTION_POINTS path_secondary.         @> 

@*3 \§bool-point vector tertiary> $\longrightarrow$ 
\§sphere tertiary> \.{INTERSECTION\_POINTS} \§path secondary>.
\initials{LDF 2005.10.28.}

\LOG
\initials{LDF 2005.10.28.}
Added this rule.
\ENDLOG
 
@q ***** (5) Definition.@> 

@<Define rules@>= 
@=bool_point_vector_tertiary: sphere_tertiary INTERSECTION_POINTS @> 
@=path_secondary@>@/
{

    @=$$@> = Scan_Parse::sphere_line_intersection_func(
                static_cast<Sphere*>(@=$1@>),
                static_cast<Path*>(@=$3@>),
                parameter);  

};

@q **** (4) bool_point_vector_tertiary: path_tertiary @> 
@q **** (4) INTERSECTION_POINTS sphere_secondary.     @> 

@*3 \§bool-point vector tertiary> $\longrightarrow$ 
\§path tertiary> \.{INTERSECTION\_POINTS} \§sphere secondary>.
\initials{LDF 2005.10.28.}

\LOG
\initials{LDF 2005.10.28.}
Added this rule.
\ENDLOG
 
@q ***** (5) Definition.@> 

@<Define rules@>= 
@=bool_point_vector_tertiary: path_tertiary INTERSECTION_POINTS @> 
@=sphere_secondary@>@/
{

    @=$$@> = Scan_Parse::sphere_line_intersection_func(
                static_cast<Sphere*>(@=$3@>),
                static_cast<Path*>(@=$1@>),
                parameter);  

};

@q ** (2) bool_point_vector_tertiary: cuboid_tertiary @> 
@q ** (2) INTERSECTION_POINTS path_secondary.  @> 

@*1 \§bool-point vector tertiary> $\longrightarrow$ 
\§cuboid tertiary> \.{INTERSECTION\_POINTS} \§path secondary>.
\initials{LDF 2005.12.09.}

\LOG
\initials{LDF 2005.12.09.}
Added this rule.
\ENDLOG
 
@q *** (3) Definition.@> 

@<Define rules@>= 
@=bool_point_vector_tertiary: cuboid_tertiary INTERSECTION_POINTS @> 
@=path_secondary@>@/
{
     Cuboid* c = static_cast<Cuboid*>(@=$1@>);
     Path*   p = static_cast<Path*>(@=$3@>);

     if (!(c && p))
        @=$$@> = static_cast<void*>(0);

     else 
        @=$$@> = c->intersection_points(*p, static_cast<Scanner_Node>(parameter));

     delete c;
     delete p;
     c = 0;
     p = 0;
};


@q ** (2) bool_point_vector_tertiary: path_tertiary @> 
@q ** (2) INTERSECTION_POINTS cuboid_secondary.  @> 

@*1 \§bool-point vector tertiary> $\longrightarrow$ 
\§path tertiary> \.{INTERSECTION\_POINTS} \§cuboid secondary>.
\initials{LDF 2005.12.09.}

\LOG
\initials{LDF 2005.12.09.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>= 
@=bool_point_vector_tertiary: path_tertiary INTERSECTION_POINTS @> 
@=cuboid_secondary@>@/
{
     Cuboid* c = static_cast<Cuboid*>(@=$3@>);
     Path*      p = static_cast<Path*>(@=$1@>);

     if (!(c && p))
         @=$$@> = static_cast<void*>(0);

     else 
        @=$$@> = c->intersection_points(*p, static_cast<Scanner_Node>(parameter));

     delete c;
     delete p;
     c = 0;
     p = 0;
};


@q ** (2) bool_point_vector_tertiary: polyhedron_tertiary @> 
@q ** (2) INTERSECTION_POINTS path_secondary.        @> 

@*1 \§bool-point vector tertiary> $\longrightarrow$ 
\§polyhedron tertiary>
\.{INTERSECTION\_POINTS} \§path secondary>.
\initials{LDF 2006.01.20.}

\LOG
\initials{LDF 2006.01.20.}
Added this rule.

\initials{LDF 2006.01.23.}
Changed \§point vector tertiary> to \§bool-point vector tertiary>.
\ENDLOG
 
@q *** (3) Definition.@> 

@<Define rules@>= 
@=bool_point_vector_tertiary: polyhedron_tertiary INTERSECTION_POINTS @> 
@=path_secondary@>@/
{
    @=$$@> = polyhedron_path_intersection_func(
                static_cast<Polyhedron*>(@=$1@>),
                static_cast<Path*>(@=$3@>),
                parameter);
};

@q ** (2) bool_point_vector_tertiary: path_tertiary      @> 
@q ** (2) INTERSECTION_POINTS polyhedron_secondary. @> 

@*1 \§bool-point vector tertiary> $\longrightarrow$ 
\§ path tertiary>
\.{INTERSECTION\_POINTS} \§polyhedron secondary>.
\initials{LDF 2006.01.20.}

\LOG
\initials{LDF 2006.01.20.}
Added this rule.

Changed \§point vector tertiary> to \§bool-point vector tertiary>.
\ENDLOG
 
@q *** (3) Definition.@> 

@<Define rules@>= 
@=bool_point_vector_tertiary: path_tertiary INTERSECTION_POINTS @> 
@=polyhedron_secondary@>@/
{
    @=$$@> = polyhedron_path_intersection_func(
                static_cast<Polyhedron*>(@=$3@>),
                static_cast<Path*>(@=$1@>),
                parameter);
};




@q * (1) bool_point_vector expression.@>
@* \§bool-point vector expression>.

\LOG
\initials{LDF 2004.11.05.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> bool_point_vector_expression@>

@q ** (2) bool_point_vector expression --> bool_point_vector_tertiary.  @>
@*1 \§bool-point vector expression> $\longrightarrow$ 
\§bool-point vector tertiary>.

\LOG
\initials{LDF 2004.11.05.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=bool_point_vector_expression: bool_point_vector_tertiary@>@/ 
{

  @=$$@> = @=$1@>;

};



@q * Emacs-Lisp code for use in indirect buffers when using the          @>
@q   GNU Emacs editor.  The local variable list is not evaluated when an @>
@q   indirect buffer is visited, so it's necessary to evaluate the       @>
@q   following s-expression in order to use the facilities normally      @>
@q   accessed via the local variables list.                              @>
@q   \initials{LDF 2004.02.12}.                                          @>
@q   (progn (cweb-mode) (outline-minor-mode t) (setq fill-column 80))    @>


@q Local Variables:                   @>
@q mode:CWEB                          @>
@q eval:(outline-minor-mode t)        @>
@q abbrev-file-name:"~/.abbrev_defs"  @>
@q eval:(read-abbrev-file)            @>
@q fill-column:80                     @>
@q run-cweave-on-file:"3DLDFprg.web"  @>
@q End:                               @>


