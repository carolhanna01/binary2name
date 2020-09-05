@q pptvexpr.w @> 
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



@q * (0) |point_vector| expressions.@>
@** {\bf point\_vector} expressions.
\initials{LDF 2004.11.10.}

\LOG
\initials{LDF 2004.11.10.}
Created this file and wrote quite a few rules.  
\ENDLOG 


@q * (1) |point_vector| primary.  @>
@* \§point vector primary>.
\initials{LDF 2004.11.10.}
  
\LOG
\initials{LDF 2004.11.10.}
Added this type declaration.
\ENDLOG


@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> point_vector_primary@>@/

@q ** (2) point_vector_primary --> point_vector_variable.@>
@*1 \§point vector primary> $\longrightarrow$ 
\§point vector variable>.  
\initials{LDF 2004.11.10.}

\LOG
\initials{LDF 2004.11.10.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=point_vector_primary: point_vector_variable@>@/ 
{

  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

@q **** (4) Error handling:  |entry == 0 || entry->object == 0|.@> 

@ Error handling:  |entry == 0 || entry->object == 0|.
\initials{LDF 2004.11.10.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0) || entry->object == static_cast<void*>(0))
    {

      @=$$@> = static_cast<void*>(0);

    } /* |if (entry == 0 || entry->object == 0)|  */

@q **** (4) |!(entry == 0 || entry->object == 0)|.@> 

@ |!(entry == 0 || entry->object == 0)|.
\initials{LDF 2004.11.10.}

@<Define rules@>=
  else /* |!(entry == 0 || entry->object == 0)|  */

     {

        typedef Pointer_Vector<Point> PV;

        PV* pv = new PV;

        *pv = *static_cast<PV*>(entry->object);

        @=$$@> = static_cast<void*>(pv);                    

     }  /* |else| (|!(entry == 0 || entry->object == 0)|)  */

};


@q ** (2) point_vector_primary --> LEFT_PARENTHESIS  @>
@q ** (2) point_vector_expression  RIGHT_PARENTHESIS.@>

@*1 \§point vector primary> $\longrightarrow$ \.{LEFT\_PARENTHESIS}
\§point vector expression> \.{RIGHT\_PARENTHESIS}.
\initials{LDF 2004.11.10.}

\LOG
\initials{LDF 2004.11.10.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=point_vector_primary: LEFT_PARENTHESIS@>@/ 
@=point_vector_expression RIGHT_PARENTHESIS@>@/ 
{
   
  @=$$@> = @=$1@>;

};

@q ** (2) point_vector_primary --> TRAVERSE conic_section_lattice_expression @> 
@q ** (2) LEFT_PARENTHESIS numeric_expression COMMA numeric_expression       @> 
@q ** (2) RIGHT_PARENTHESIS                                                  @> 
@q ** (2) with_conic section_lattice_get_point_option_list.                  @>

@*1 \§point vector primary> $\longrightarrow$ 
\.{TRAVERSE} \§conic section lattice expression> 
\.{LEFT\_PARENTHESIS }\§numeric expression> \.{COMMA} \§numeric expression>
\.{RIGHT\_PARENTHESIS} \§with conic section lattice option list>.
\initials{LDF 2007.08.07.}

\LOG
\initials{LDF 2007.08.07.}
Added this rule.

\initials{LDF 2007.08.08.}
Added |with_conic_section_lattice_get_point_option_list| to the right-hand side.

\initials{LDF 2007.08.19.}
Changed |with_conic_section_lattice_get_point_option_list| to 
|with_conic_section_lattice_option_list|.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=point_vector_secondary: TRAVERSE conic_section_lattice_expression@>@/
@=LEFT_PARENTHESIS numeric_expression COMMA numeric_expression@>@/
@=RIGHT_PARENTHESIS with_conic_section_lattice_option_list@>@/
{

   @=$$@> = Scan_Parse::traverse_conic_section_lattice_func(
               parameter,
               @=$2@>, 
               @=$4@>,
               @=$6@>);
               
};

@q ** (2) point_vector_primary --> GET_PASCAL_POINTS numeric_expression @>
@q ** (2) COMMA conic_section_lattice_expression                        @>
@q ** (2) with_conic_section_lattice_option_list.                       @>

@*1 \§point vector primary> $\longrightarrow$ \.{GET\_PASCAL\_POINT} 
\§numeric expression> \.{COMMA} \§conic section lattice expression>.
\§with conic section lattice option list>.
\initials{LDF 2007.08.19.}

\LOG
\initials{LDF 2007.08.19.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=point_vector_primary:  GET_PASCAL_POINTS numeric_expression @>
@=COMMA conic_section_lattice_expression@>@/
@=with_conic_section_lattice_option_list@>@/
{

   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);
  
   Conic_Section_Lattice* c 
      = static_cast<Conic_Section_Lattice*>(@=$4@>); 

   if (c)
   {
      @=$$@> = c->get_pascal_points(@=$2@>, 
                                    scanner_node);

      delete c;
      c = 0;
   }
   else
       @=$$@> = static_cast<void*>(0);    
};

@q ** (2) path_vector_primary --> GET_TABS DODECAHEDRON.@>

@*1 \§path vector primary> $\longrightarrow$ \.{GET\_TABS}
\.{DODECAHEDRON}.
\initials{LDF 2007.10.12.}

\LOG
\initials{LDF 2007.10.12.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=path_vector_primary: GET_TABS DODECAHEDRON with_tabs_option_list@>@/
{
 
    @=$$@> = Dodecahedron::get_tabs(parameter);

};


@*1 \§path vector primary> $\longrightarrow$ \.{GET\_TABS}
\.{RHOMBIC\_TRIACONTAHEDRON}.
\initials{LDF 2007.10.24.}

\LOG
\initials{LDF 2007.10.24.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=path_vector_primary: GET_TABS RHOMBIC_TRIACONTAHEDRON with_tabs_option_list@>@/
{
 
    @=$$@> = Rhombic_Triacontahedron::get_tabs(parameter);

};

@q ** (2) with_tabs_option_list.@>
@*1 \§with tabs option list>.
\initials{LDF 2007.10.12.}

\LOG
\initials{LDF 2007.10.12.}
Added this type declaration.  It is currently commented-out, since
the symbol \§with tabs option list> doesn't require a semantic value.
\ENDLOG

@q <Type declarations for non-terminal symbols@>
@q =%type <pointer_value> with_tabs_option_list@>

@q *** (3) with_tabs_option_list --> /* Empty  */@>
@*2 \§with tabs option list> $\longrightarrow$ \.{EMPTY}.
\initials{LDF 2007.10.12.}

\LOG
\initials{LDF 2007.10.12.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=with_tabs_option_list: /* Empty  */@>@/
{
   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

   if (scanner_node->polyhedron_options)
      scanner_node->polyhedron_options->clear();
   else
      scanner_node->polyhedron_options = new Polyhedron_Options;
     
};

@q *** (3) with_tabs_option_list: with_tabs_option_list @>
@q *** (3) WITH_DIAMETER numeric_expression.            @>
@*2 \§with tabs option list> $\longrightarrow$ 
\§with tabs option list> \.{WITH\_DIAMETER} \§numeric expression>.
\initials{LDF 2007.10.12.}

\LOG
\initials{LDF 2007.10.12.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=with_tabs_option_list: with_tabs_option_list @>
@=WITH_DIAMETER numeric_expression@>@/
{
    static_cast<Scanner_Node>(
       parameter)->polyhedron_options->polygon_0_diameter 
    = @=$3@>;

};

@q *** (3) with_tabs_option_list: with_tabs_option_list @>
@q *** (3) WITH_RHOMBUS_SIDE_LENGTH numeric_expression. @>
@*2 \§with tabs option list> $\longrightarrow$ 
\§with tabs option list> \.{WITH\_RHOMBUS\_SIDE\_LENGTH} 
\§numeric expression>.
\initials{LDF 2007.10.24.}

\LOG
\initials{LDF 2007.10.24.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=with_tabs_option_list: with_tabs_option_list @>
@=WITH_RHOMBUS_SIDE_LENGTH numeric_expression@>@/
{
    static_cast<Scanner_Node>(
       parameter)->polyhedron_options->rhombus_0_side_length 
    = @=$3@>;

};

@q *** (3) with_tabs_option_list: with_tabs_option_list WITH_HALF.@>
@*2 \§with tabs option list> $\longrightarrow$ 
\§with tabs option list> \.{WITH\_HALF}.
\initials{LDF 2007.10.12.}

\LOG
\initials{LDF 2007.10.12.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=with_tabs_option_list: with_tabs_option_list WITH_HALF@>@/
{
    static_cast<Scanner_Node>(
       parameter)->polyhedron_options->do_half = true;

};

@q *** (3) with_tabs_option_list: with_tabs_option_list WITH_STITCH_LINES.@>
@*2 \§with tabs option list> $\longrightarrow$ 
\§with tabs option list> \.{WITH\_STITCH\_LINES}.
\initials{LDF 2007.10.12.}

\LOG
\initials{LDF 2007.10.12.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=with_tabs_option_list: with_tabs_option_list WITH_STITCH_LINES@>@/
{
    static_cast<Scanner_Node>(
       parameter)->polyhedron_options->do_stitch_lines = true;

};

@q *** (3) with_tabs_option_list: with_tabs_option_list WITH_NO_STITCH_LINES.@>
@*2 \§with tabs option list> $\longrightarrow$ 
\§with tabs option list> \.{WITH\_NO\_STITCH\_LINES}.
\initials{LDF 2007.10.12.}

\LOG
\initials{LDF 2007.10.12.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=with_tabs_option_list: with_tabs_option_list WITH_NO_STITCH_LINES@>@/
{
    static_cast<Scanner_Node>(
       parameter)->polyhedron_options->do_stitch_lines = false;

};

@q *** (3) with_tabs_option_list: with_tabs_option_list @>
@q *** (3) WITH_STITCH_DIVISIONS numeric_expression.    @>
@*2 \§with tabs option list> $\longrightarrow$ 
\§with tabs option list> \.{WITH\_STITCH\_DIVISIONS}.
\initials{LDF 2007.10.12.}

\LOG
\initials{LDF 2007.10.12.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=with_tabs_option_list: with_tabs_option_list WITH_STITCH_DIVISIONS @>
@=numeric_expression@>@/
{
    static_cast<Scanner_Node>(
       parameter)->polyhedron_options->stitch_divisions 
    = (@=$3@> <= 0) ? 0 
                    : static_cast<unsigned short>(
                         floor(fabs(@=$3@>))); 
};

@q * (1) point_vector secondary.@>
@* \§point vector secondary>.
\initials{LDF 2004.11.10.}

\LOG
\initials{LDF 2004.11.10.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> point_vector_secondary@>
  
@q ** (2) point_vector secondary --> point_vector_primary.@>
@*1 \§point vector secondary> $\longrightarrow$ 
\§point vector primary>.
\initials{LDF 2004.11.10.}

\LOG
\initials{LDF 2004.11.10.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=point_vector_secondary: point_vector_primary@>@/ 
{

  @=$$@> = @=$1@>;

};

@q ***** (5) point_vector_secondary --> point_secondary REFLECTED_OFF @>
@q ***** (5) path_expression WITH_DIRECTION point_expression.         @>

@*4 \§point vector secondary> $\longrightarrow$ \§point secondary> 
\.{REFLECTED\_OFF} \§path expression>
\.{WITH\_DIRECTION} \§point expression>.

\LOG
\initials{LDF 2004.12.04.}
Added this rule.

\initials{LDF 2004.12.06.}
Working on this rule.

\initials{LDF 2004.12.07.}
This rule seems to work now.

\initials{LDF 2004.12.09.}
Changed |point_secondary| on the left-hand side to 
|point_vector_secondary| and moved this rule from 
\filename{ppntexpr.w} to this file (\filename{pptvexpr.w}).

\initials{LDF 2005.10.24.}
Changed |path_like_expression| to |path_expression|.
Removed debugging code.
\ENDLOG


@q ****** (6) Definition.@> 

@<Define rules@>=
@=point_vector_secondary: point_secondary REFLECTED_OFF@>@/
@=path_expression WITH_DIRECTION point_expression@>@/ 
{

    Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

    Pointer_Vector<Point>* pv = new Pointer_Vector<Point>;

    *pv += create_new<Point>(0, scanner_node);
    *pv += create_new<Point>(0, scanner_node);
    *pv += create_new<Point>(0, scanner_node);
    *pv += create_new<Point>(0, scanner_node);

@q ******* (7) Call |Scan_Parse::reflect_off_func()|.@> 

@ Call |Scan_Parse::reflect_off_func()|.
\initials{LDF 2004.12.06.}

\LOG
\initials{LDF 2004.12.08.}
No longer calling |Scan_Parse::reflect_off_func()| in a |try| 
block and catching |bad_alloc|.  
|Point::reflect_off()|, which |Scan_Parse::reflect_off_func<Point>()| 
calls, no longer tries to allocate
memory for |reflection| if it's null, 
so it never throws |bad_alloc|, so 
|Scan_Parse::reflect_off_func<Point>()| doesn't, either. 
\ENDLOG 

@<Define rules@>=

   int status = reflect_off_func<Point>(scanner_node,
                                        static_cast<Point*>(@=$1@>),
                                        static_cast<Path*>(@=$3@>),
                                        static_cast<Point*>(@=$5@>),
                                        pv);

@q ******** (8) Error handling:  |Scan_Parse::reflect_off_func<Point>()|@> 
@q ******** (8) failed, returning a non-zero value.@> 

@ Error handling:  |Scan_Parse::reflect_off_func<Point>()|
failed, returning a non-zero value.
\initials{LDF 2004.12.04.}

@<Define rules@>=


   if (status != 0)
      {

          @=$$@> = static_cast<void*>(0);
       
      }  /* |if (status != 0)|  */


@q ******** (8) Success!  |status == 0|.@> 

@ Success!  |status == 0|.
\initials{LDF 2004.12.04.}

@<Define rules@>=

   else /* |status == 0|  */
      {

         @=$$@> = static_cast<void*>(pv);

      }   /* |else| (|status == 0|)  */

};



@q * (1) point_vector tertiary.  @>

@* \§point vector tertiary>.
\initials{LDF 2004.11.10.}

\LOG
\initials{LDF 2004.11.10.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> point_vector_tertiary@>

@q ** (2) point_vector tertiary --> point_vector_secondary.@>
@*1 \§point vector tertiary> $\longrightarrow$ 
\§point vector secondary>.
\initials{LDF 2004.11.10.}

\LOG
\initials{LDF 2004.11.10.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=point_vector_tertiary: point_vector_secondary@>@/ 
{

  @=$$@> = @=$1@>;

};

@q ** (2) point_vector_tertiary: ellipse_tertiary @> 
@q ** (2) INTERSECTION_POINTS plane_secondary.    @> 

@*1 \§point vector tertiary> $\longrightarrow$ 
\§ellipse tertiary> \.{INTERSECTION\_POINTS} \§plane secondary>.
\initials{LDF 2005.11.02.}

\LOG
\initials{LDF 2005.11.02.}
Added this rule.

\initials{LDF 2005.11.03.}
Changed code to use the version of |Ellipse::intersection_points()| that
takes a |Path| argument.

\initials{LDF 2005.11.04.}
Changed |rectangle_secondary| to |plane_secondary|.

\initials{LDF 2005.11.04.}
Changed |bool_point_vector_tertiary| to |bool_point_vector_tertiary|.
\ENDLOG
 
@q ***** (5) Definition.@> 

@<Define rules@>= 
@=point_vector_tertiary: ellipse_tertiary INTERSECTION_POINTS @> 
@=plane_secondary@>@/
{
    @=$$@> = Scan_Parse::ellipse_like_plane_intersection_func<Ellipse>(
                               static_cast<Ellipse*>(@=$1@>), 
                               @=$3@>, 
                               parameter);
};





@q ** (2) point_vector_tertiary: polygon_tertiary @> 
@q ** (2) INTERSECTION_POINTS polygon_secondary.  @> 

@*1 \§point vector tertiary> $\longrightarrow$ 
\§polygon tertiary>
\.{INTERSECTION\_POINTS} \§polygon secondary>.
\initials{LDF 2004.11.10.}

\LOG
\initials{LDF 2004.11.10.}
Added this rule.

\initials{LDF 2005.10.24.}
Changed |polygon_like_tertiary| and |polygon_like_secondary| 
to |polygon_tertiary| and |polygon_secondary|, respectively.
Removed debugging code.

\initials{LDF 2005.10.28.}
Removed code.  Now calling |Scan_Parse::polygon_like_intersection_func()|.
\ENDLOG
 
@q *** (3) Definition.@> 

@<Define rules@>= 
@=point_vector_tertiary: polygon_tertiary INTERSECTION_POINTS @> 
@=polygon_secondary@>@/
{

   @=$$@> = Scan_Parse::polygon_like_intersection_func<Polygon, Polygon>
               (static_cast<Polygon*>(@=$1@>),
               static_cast<Polygon*>(@=$3@>),
               parameter);

};

@q ** (2) point_vector_tertiary: polygon_tertiary @> 
@q ** (2) INTERSECTION_POINTS reg_polygon_secondary.  @> 

@*1 \§point vector tertiary> $\longrightarrow$ 
\§polygon tertiary>
\.{INTERSECTION\_POINTS} \§regular polygon secondary>.
\initials{LDF 2005.10.28.}

\LOG
\initials{LDF 2005.10.28.}
Added this rule.
\ENDLOG
 
@q *** (3) Definition.@> 

@<Define rules@>= 
@=point_vector_tertiary: polygon_tertiary INTERSECTION_POINTS @> 
@=reg_polygon_secondary@>@/
{

   @=$$@> = Scan_Parse::polygon_like_intersection_func<Polygon, Reg_Polygon>
               (static_cast<Polygon*>(@=$1@>),
               static_cast<Reg_Polygon*>(@=$3@>),
               parameter);

};


@q ** (2) point_vector_tertiary: reg_polygon_tertiary @> 
@q ** (2) INTERSECTION_POINTS polygon_secondary.  @> 

@*1 \§point vector tertiary> $\longrightarrow$ 
\§regular polygon tertiary>
\.{INTERSECTION\_POINTS} \§polygon secondary>.
\initials{LDF 2005.10.28.}

\LOG
\initials{LDF 2005.10.28.}
Added this rule.
\ENDLOG
 
@q *** (3) Definition.@> 

@<Define rules@>= 
@=point_vector_tertiary: reg_polygon_tertiary INTERSECTION_POINTS @> 
@=polygon_secondary@>@/
{

   @=$$@> = Scan_Parse::polygon_like_intersection_func<Reg_Polygon, Polygon>
               (static_cast<Reg_Polygon*>(@=$1@>),
               static_cast<Polygon*>(@=$3@>),
               parameter);

};

@q START HERE!!  LDF 2005.10.28.  Add rules from `poly_int_rules.web'.@>

@q * (1) point_vector expression.@>
@* \§point vector expression>.

\LOG
\initials{LDF 2004.11.10.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> point_vector_expression@>

@q ** (2) point_vector expression --> point_vector_tertiary.  @>
@*1 \§point vector expression> $\longrightarrow$ 
\§point vector tertiary>.

\LOG
\initials{LDF 2004.11.10.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=point_vector_expression: point_vector_tertiary@>@/ 
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


