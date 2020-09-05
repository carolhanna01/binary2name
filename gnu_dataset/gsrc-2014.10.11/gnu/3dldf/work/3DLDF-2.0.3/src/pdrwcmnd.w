@q pdrwcmnd.w @> 
@q Created by Laurence Finston Sun Aug 29 18:22:07 CEST 2004  @>
     
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




@q * (0) Drawing Commands.@>
@** Drawing Commands.

\LOG
\initials{LDF 2004.08.29.}
Created this file.  It contains code removed from 
\filename{pcommand.w}, which was getting too large.
\ENDLOG 


@q * (1) command --> basic_drawing_command   @>
@q * (1) path_expression with_clause_draw_list. @>

@* \§command> $\longrightarrow$ \§basic drawing command> 
\§path expression> \§with clause list>.

\LOG
\initials{LDF 2004.06.07.}  
Changed the way |ddash_pattern| is handled, to
reflect the fact that I've replaced |string Path::dashed| with 
|Dash_Pattern* Path::dash_pattern|.

\initials{LDF 2004.06.28.}
Changed |with_clause| to |with_clause_draw| and |with_clause_list| to 
|with_clause_draw_list|.  I plan to add |with_clause_output| for the 
``{\bf output}'' and ``{\bf endfig}'' rules.

\initials{LDF 2004.08.17.}
Removed code from this rule to the function |Scan_Parse::drawing_command()|,
which is defined in \filename{scanprsf.web}.

\initials{LDF 2004.08.30.}
Changed name of |Scan_Parse::drawing_command()| to
|drawing_command_path()|. 

\initials{LDF 2005.10.24.}
Changed |path_like_expression| to |path_expression|.  Removed
all debugging code.
\ENDLOG 


@q ** (2) Definition.@> 
@
@<Define rules@>= 
  
@=command: basic_drawing_command path_expression @>
@=with_clause_draw_list@>
{


    drawing_command_path(static_cast<Scanner_Node>(parameter),
                         @=$1@>,
                         static_cast<Path*>(@=$2@>));

    @=$$@> = static_cast<void*>(0);

};

@q * (1) command --> basic_drawing_command   @>
@q * (1) ellipse_expression with_clause_draw_list. @>

@* \§command> $\longrightarrow$ \§basic drawing command> 
\§ellipse expression> \§with clause list>.
\initials{LDF 2005.10.24.}

\LOG
\initials{LDF 2005.10.24.}
Added this rule.

\initials{LDF 2005.11.09.}
Now casting |@=$2@>| to |Ellipse*| rather than |Path*| 
in the call to |Scan_Parse::drawing_command_path()|.
\ENDLOG

@q ** (2) Definition.@> 
@
@<Define rules@>= 
  
@=command: basic_drawing_command ellipse_expression @>
@=with_clause_draw_list@>
{

    drawing_command_path(static_cast<Scanner_Node>(parameter),
                         @=$1@>,
                         static_cast<Ellipse*>(@=$2@>));

    @=$$@> = static_cast<void*>(0);

};

@q * (1) command --> basic_drawing_command   @>
@q * (1) circle_expression with_clause_draw_list. @>

@* \§command> $\longrightarrow$ \§basic drawing command> 
\§circle expression> \§with clause list>.
\initials{LDF 2005.10.24.}

\LOG
\initials{LDF 2005.10.24.}
Added this rule.

\initials{LDF 2005.11.09.}
Now casting |@=$2@>| to |Circle*| rather than |Path*| 
in the call to |Scan_Parse::drawing_command_path()|.
\ENDLOG

@q ** (2) Definition.@> 
@
@<Define rules@>= 
  
@=command: basic_drawing_command circle_expression @>
@=with_clause_draw_list@>
{

    drawing_command_path(static_cast<Scanner_Node>(parameter),
                         @=$1@>,
                         static_cast<Circle*>(@=$2@>));

    @=$$@> = static_cast<void*>(0);

};

@q * (1) command --> basic_drawing_command  @>
@q * (1) parabola_expression with_clause_draw_list. @>

@* \§command> $\longrightarrow$ \§basic drawing command> 
\§parabola expression> \§with clause list>.
\initials{LDF 2005.11.09.}

\LOG
\initials{LDF 2005.11.09.}
Added this rule.
\ENDLOG

@q ** (2) Definition.@> 
@
@<Define rules@>= 
  
@=command: basic_drawing_command parabola_expression @>
@=with_clause_draw_list@>
{

    drawing_command_path(static_cast<Scanner_Node>(parameter),
                         @=$1@>,
                         static_cast<Parabola*>(@=$2@>));

    @=$$@> = static_cast<void*>(0);

};

@q * (1) command --> basic_drawing_command  @>
@q * (1) hyperbola_expression with_clause_draw_list. @>

@* \§command> $\longrightarrow$ \§basic drawing command> 
\§hyperbola expression> \§with clause list>.
\initials{LDF 2005.11.15.}

\LOG
\initials{LDF 2005.11.15.}
Added this rule.
\ENDLOG

@q ** (2) Definition.@> 
@
@<Define rules@>= 
  
@=command: basic_drawing_command hyperbola_expression @>
@=with_clause_draw_list@>
{

    drawing_command_path(static_cast<Scanner_Node>(parameter),
                         @=$1@>,
                         static_cast<Hyperbola*>(@=$2@>));

    @=$$@> = static_cast<void*>(0);

};

@q * (1) command --> basic_drawing_command   @>
@q * (1) helix_expression with_clause_draw_list. @>

@* \§command> $\longrightarrow$ \§basic drawing command> 
\§helix expression> \§with clause list>.
\initials{2009.01.14.}

\LOG
\initials{2009.01.14.}
Added this rule.
\ENDLOG

@q ** (2) Definition.@> 
@
@<Define rules@>= 
  
@=command: basic_drawing_command helix_expression @>
@=with_clause_draw_list@>
{

    drawing_command_path(static_cast<Scanner_Node>(parameter),
                         @=$1@>,
                         static_cast<Helix*>(@=$2@>));

    @=$$@> = static_cast<void*>(0);

};




@q * (1) command --> basic_drawing_command   @>
@q * (1) polygon_expression with_clause_draw_list. @>

@* \§command> $\longrightarrow$ \§basic drawing command> 
\§polygon expression> \§with clause list>.

\LOG
\initials{LDF 2005.10.24.}
Added this rule.
\ENDLOG

@q ** (2) Definition.@> 
@
@<Define rules@>= 
  
@=command: basic_drawing_command polygon_expression @>
@=with_clause_draw_list@>
{

    drawing_command_path(static_cast<Scanner_Node>(parameter),
                         @=$1@>,
                         static_cast<Path*>(@=$2@>));

    @=$$@> = static_cast<void*>(0);

};

@q * (1) command --> basic_drawing_command   @>
@q * (1) reg_polygon_expression with_clause_draw_list. @>

@* \§command> $\longrightarrow$ \§basic drawing command> 
\§regular polygon expression> \§with clause list>.

\LOG
\initials{LDF 2005.10.24.}
Added this rule.
\ENDLOG

@q ** (2) Definition.@> 
@
@<Define rules@>= 
  
@=command: basic_drawing_command reg_polygon_expression @>
@=with_clause_draw_list@>
{

    drawing_command_path(static_cast<Scanner_Node>(parameter),
                         @=$1@>,
                         static_cast<Path*>(@=$2@>));

    @=$$@> = static_cast<void*>(0);

};

@q * (1) command --> basic_drawing_command   @>
@q * (1) rectangle_expression with_clause_draw_list. @>

@* \§command> $\longrightarrow$ \§basic drawing command> 
\§rectangle expression> \§with clause list>.

\LOG
\initials{LDF 2005.10.24.}
Added this rule.
\ENDLOG

@q ** (2) Definition.@> 
@
@<Define rules@>= 
  
@=command: basic_drawing_command rectangle_expression @>
@=with_clause_draw_list@>
{

    drawing_command_path(static_cast<Scanner_Node>(parameter),
                         @=$1@>,
                         static_cast<Path*>(@=$2@>));

    @=$$@> = static_cast<void*>(0);

};

@q * (1) command --> basic_drawing_command   @>
@q * (1) triangle_expression with_clause_draw_list. @>

@* \§command> $\longrightarrow$ \§basic drawing command> 
\§triangle expression> \§with clause list>.

\LOG
\initials{LDF 2005.10.24.}
Added this rule.
\ENDLOG


@q ** (2) Definition.@> 
@
@<Define rules@>= 
  
@=command: basic_drawing_command triangle_expression @>
@=with_clause_draw_list@>
{

    drawing_command_path(static_cast<Scanner_Node>(parameter),
                         @=$1@>,
                         static_cast<Path*>(@=$2@>));

    @=$$@> = static_cast<void*>(0);

};

@q * (1) command --> basic_drawing_command   @>
@q * (1) path_vector_expression with_clause_draw_list. @>

@* \§command> $\longrightarrow$ \§basic drawing command> 
\§path vector expression> \§with clause list>.
\initials{LDF 2005.03.10.}

\LOG
\initials{LDF 2005.03.10.}
Added this rule.

\initials{LDF 2005.10.24.}
Changed |path_vector_like_expression| to |path_vector_expression|.  Removed
all debugging code.
\ENDLOG

@q ** (2) Definition.@> 
@
@<Define rules@>= 
  
@=command: basic_drawing_command path_vector_expression @>
@=with_clause_draw_list@>
{

    drawing_command_path_vector(static_cast<Scanner_Node>(parameter),
                                @=$1@>,
                                static_cast<Pointer_Vector<Path>*>(@=$2@>));

    @=$$@> = static_cast<void*>(0);

};

@q * (1) command --> basic_drawing_command   @>
@q * (1) circle_vector_expression with_clause_draw_list. @>

@* \§command> $\longrightarrow$ \§basic drawing command> 
\§circle vector expression> \§with clause list>.
\initials{LDF 2005.03.10.}

\LOG
\initials{LDF 2005.10.24.}
Added this rule.
\ENDLOG

@q ** (2) Definition.@> 
@
@<Define rules@>= 
  
@=command: basic_drawing_command circle_vector_expression @>
@=with_clause_draw_list@>
{

    drawing_command_path_vector(static_cast<Scanner_Node>(parameter),
                                @=$1@>,
                                static_cast<Pointer_Vector<Path>*>(@=$2@>));

    @=$$@> = static_cast<void*>(0);

};

@q * (1) command --> basic_drawing_command   @>
@q * (1) ellipse_vector_expression with_clause_draw_list. @>

@* \§command> $\longrightarrow$ \§basic drawing command> 
\§ellipse vector expression> \§with clause list>.
\initials{LDF 2005.03.10.}

\LOG
\initials{LDF 2005.10.24.}
Added this rule.
\ENDLOG

@q ** (2) Definition.@> 
@
@<Define rules@>= 
  
@=command: basic_drawing_command ellipse_vector_expression @>
@=with_clause_draw_list@>
{

    drawing_command_path_vector(static_cast<Scanner_Node>(parameter),
                                @=$1@>,
                                static_cast<Pointer_Vector<Path>*>(@=$2@>));

    @=$$@> = static_cast<void*>(0);

};

@q * (1) command --> basic_drawing_command   @>
@q * (1) polygon_vector_expression with_clause_draw_list. @>

@* \§command> $\longrightarrow$ \§basic drawing command> 
\§polygon vector expression> \§with clause list>.
\initials{LDF 2005.03.10.}

\LOG
\initials{LDF 2005.10.24.}
Added this rule.
\ENDLOG

@q ** (2) Definition.@> 
@
@<Define rules@>= 
  
@=command: basic_drawing_command polygon_vector_expression @>
@=with_clause_draw_list@>
{

    drawing_command_path_vector(static_cast<Scanner_Node>(parameter),
                                @=$1@>,
                                static_cast<Pointer_Vector<Path>*>(@=$2@>));

    @=$$@> = static_cast<void*>(0);

};

@q * (1) command --> basic_drawing_command   @>
@q * (1) reg_polygon_vector_expression with_clause_draw_list. @>

@* \§command> $\longrightarrow$ \§basic drawing command> 
\§regular polygon vector expression> \§with clause list>.
\initials{LDF 2005.03.10.}

\LOG
\initials{LDF 2005.10.24.}
Added this rule.
\ENDLOG

@q ** (2) Definition.@> 
@
@<Define rules@>= 
  
@=command: basic_drawing_command reg_polygon_vector_expression @>
@=with_clause_draw_list@>
{

    drawing_command_path_vector(static_cast<Scanner_Node>(parameter),
                                @=$1@>,
                                static_cast<Pointer_Vector<Path>*>(@=$2@>));

    @=$$@> = static_cast<void*>(0);

};

@q * (1) command --> basic_drawing_command   @>
@q * (1) rectangle_vector_expression with_clause_draw_list. @>

@* \§command> $\longrightarrow$ \§basic drawing command> 
\§rectangle vector expression> \§with clause list>.
\initials{LDF 2005.03.10.}

\LOG
\initials{LDF 2005.10.24.}
Added this rule.
\ENDLOG

@q ** (2) Definition.@> 
@
@<Define rules@>= 
  
@=command: basic_drawing_command rectangle_vector_expression @>
@=with_clause_draw_list@>
{

    drawing_command_path_vector(static_cast<Scanner_Node>(parameter),
                                @=$1@>,
                                static_cast<Pointer_Vector<Path>*>(@=$2@>));

    @=$$@> = static_cast<void*>(0);

};

@q * (1) command --> basic_drawing_command   @>
@q * (1) triangle_vector_expression with_clause_draw_list. @>

@* \§command> $\longrightarrow$ \§basic drawing command> 
\§triangle vector expression> \§with clause list>.
\initials{LDF 2005.03.10.}

\LOG
\initials{LDF 2005.10.24.}
Added this rule.
\ENDLOG

@q ** (2) Definition.@> 
@
@<Define rules@>= 
  
@=command: basic_drawing_command triangle_vector_expression @>
@=with_clause_draw_list@>
{

    drawing_command_path_vector(static_cast<Scanner_Node>(parameter),
                                @=$1@>,
                                static_cast<Pointer_Vector<Path>*>(@=$2@>));

    @=$$@> = static_cast<void*>(0);

};

@q *** (3) command --> basic_drawing_command   @>
@q *** (3) cuboid_expression with_clause_draw_list. @>

@*2 \§command> $\longrightarrow$ \§basic drawing command> 
\§cuboid expression> \§with clause list>.

\LOG
\initials{LDF 2004.08.18.}
Added this rule.

\initials{LDF 2004.08.30.}
Finished writing this rule.  It now calls 
|Scan_Parse::drawing_command_solid()|, which is defined in 
\filename{scanprsf.web}.

\initials{LDF 2005.10.22.}
Replaced |solid_like_expression| with |cuboid_expression|.

\initials{LDF 2005.10.24.}
Removed all debugging code.
\ENDLOG 

@q ***** (5) Definition.@> 
@
@<Define rules@>= 
  
@=command: basic_drawing_command cuboid_expression @>
@=with_clause_draw_list@>
{

    drawing_command_solid(static_cast<Scanner_Node>(parameter),
                          @=$1@>,
                          static_cast<Solid*>(@=$2@>));


  @=$$@> = static_cast<void*>(0);

};

@q *** (3) drawing_command --> basic_drawing_command   @>
@q *** (3) polyhedron_expression with_clause_draw_list. @>

@*2 \§command> $\longrightarrow$ \§basic drawing command> 
\§polyhedron expression> \§with clause list>.

\LOG
\initials{LDF 2005.10.24.}
Added this rule.
\ENDLOG 

@q ***** (5) Definition.@> 
@
@<Define rules@>= 
  
@=command: basic_drawing_command polyhedron_expression @>
@=with_clause_draw_list@>
{

    drawing_command_solid(static_cast<Scanner_Node>(parameter),
                          @=$1@>,
                          static_cast<Solid*>(@=$2@>));


  @=$$@> = static_cast<void*>(0);

};

@q *** (3) drawing_command --> basic_drawing_command  @>
@q *** (3) ellipsoid_expression with_clause_draw_list.@>

@*2 \§command> $\longrightarrow$ \§basic drawing command> 
\§ellipsoid expression> \§with clause list>.

\LOG
\initials{LDF 2005.10.31.}
Added this rule.
\ENDLOG 

@q ***** (5) Definition.@> 

@<Define rules@>= 
  
@=command: basic_drawing_command ellipsoid_expression @>
@=with_clause_draw_list@>
{

    drawing_command_solid(static_cast<Scanner_Node>(parameter),
                          @=$1@>,
                          static_cast<Solid*>(@=$2@>));


  @=$$@> = static_cast<void*>(0);

};

@q *** (3) drawing_command --> basic_drawing_command @>
@q *** (3) sphere_expression with_clause_draw_list.  @>

@*2 \§command> $\longrightarrow$ \§basic drawing command> 
\§sphere expression> \§with clause list>.

\LOG
\initials{LDF 2005.10.24.}
Added this rule.
\ENDLOG 

@q ***** (5) Definition.@> 

@<Define rules@>= 
  
@=command: basic_drawing_command sphere_expression @>
@=with_clause_draw_list@>
{

    drawing_command_solid(static_cast<Scanner_Node>(parameter),
                          @=$1@>,
                          static_cast<Solid*>(@=$2@>));


  @=$$@> = static_cast<void*>(0);

};

@q *** (3) drawing_command --> basic_drawing_command             @>
@q *** (3) sphere_development_expression with_clause_draw_list.  @>

@*2 \§command> $\longrightarrow$ \§basic drawing command> 
\§sphere development expression> \§with clause list>.
\initials{LDF 2009.11.10.}

\LOG
\initials{LDF 2009.11.10.}
Added this rule.
\ENDLOG 

@q ***** (5) Definition.@> 

@<Define rules@>= 
  
@=command: basic_drawing_command sphere_development_expression @>
@=with_clause_draw_list@>
{

    drawing_command_sphere_development(static_cast<Scanner_Node>(parameter),
                                       @=$1@>,
                                       static_cast<Sphere_Development*>(@=$2@>));

    @=$$@> = static_cast<void*>(0);

};

@q *** (3) drawing_command --> basic_drawing_command @>
@q *** (3) cylinder_expression with_clause_draw_list.  @>

@*2 \§command> $\longrightarrow$ \§basic drawing command> 
\§cylinder expression> \§with clause list>.
\initials{LDF 2006.11.06.}

\LOG
\initials{LDF 2006.11.06.}
Added this rule.
\ENDLOG 

@q ***** (5) Definition.@> 

@<Define rules@>= 
  
@=command: basic_drawing_command cylinder_expression @>
@=with_clause_draw_list@>
{

    drawing_command_solid(static_cast<Scanner_Node>(parameter),
                          @=$1@>,
                          static_cast<Solid*>(@=$2@>));


    @=$$@> = static_cast<void*>(0);

};

@q *** (3) drawing_command --> basic_drawing_command @>
@q *** (3) cone_expression with_clause_draw_list.  @>

@*2 \§command> $\longrightarrow$ \§basic drawing command> 
\§cone expression> \§with clause list>.
\initials{LDF 2006.11.08.}

\LOG
\initials{LDF 2006.11.08.}
Added this rule.
\ENDLOG 

@q ***** (5) Definition.@> 

@<Define rules@>= 
  
@=command: basic_drawing_command cone_expression @>
@=with_clause_draw_list@>
{

    drawing_command_solid(static_cast<Scanner_Node>(parameter),
                          @=$1@>,
                          static_cast<Solid*>(@=$2@>));


    @=$$@> = static_cast<void*>(0);

};

@q ** (2) command --> DRAW_LATTICE conic_section_lattice_expression @> 
@q ** (2) COMMA numeric_expression                                  @> 
@q ** (2) with_conic_section_lattice_option_list                    @>
@q ** (2) with_clause_draw_list.                                    @>

@*1 \§command> \.{DRAW\_LATTICE} \§conic section lattice expression>
\.{COMMA} \§numeric expression> 
\§with conic section lattice option list>
\§with clause draw list>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.

\initials{LDF 2007.08.19.}
Changed \§point expression> to \§numeric expression>.

\initials{LDF 2007.08.19.}
Added \§with conic section lattice option list>.

\initials{LDF 2007.08.20.}
Added \§with clause draw list>.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=command: DRAW_LATTICE conic_section_lattice_expression COMMA@>@/
@=numeric_expression with_conic_section_lattice_option_list@>@/
@=with_clause_draw_list@>@/
{

    if (@=$2@> != static_cast<void*>(0))
    {
        Conic_Section_Lattice* c = static_cast<Conic_Section_Lattice*>(@=$2@>);
        c->draw_lattice(@=$4@>, parameter);
        delete c;
    }

    @=$$@> = static_cast<void*>(0);

};

@q **** (4) basic_drawing_command @>
@*3 \§basic drawing command>.

@<Type declarations for non-terminal symbols@>=
@=%type <int_value> basic_drawing_command@>


@q ***** (5) basic_drawing_command --> DRAW.@>
@*4 \§basic drawing command> $\longrightarrow$ \.{DRAW}.

@<Define rules@>= 
  
@=basic_drawing_command: DRAW@>
{

  @=$$@> = DRAW;

};


@q ***** (5) basic_drawing_command --> DRAWARROW.@>
@*4 \§basic drawing command> $\longrightarrow$ \.{DRAWARROW}.

\LOG
\initials{LDF 2004.11.08.}
Added this rule.
\ENDLOG

@<Define rules@>= 
  
@=basic_drawing_command: DRAWARROW@>
{

  @=$$@> = DRAWARROW;

};

@q ***** (5) basic_drawing_command --> DRAWDBLARROW.@>
@*4 \§basic drawing command> $\longrightarrow$ \.{DRAWDBLARROW}.

\LOG
\initials{LDF 2004.11.08.}
Added this rule.
\ENDLOG

@<Define rules@>= 
  
@=basic_drawing_command: DRAWDBLARROW@>
{

  @=$$@> = DRAWDBLARROW;

};


@q ***** (5) basic_drawing_command --> FILL.@>
@*4 \§basic drawing command> $\longrightarrow$ \.{FILL}.

@
@<Define rules@>= 
  
@=basic_drawing_command: FILL@>
{
#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
      cerr << "\n*** Parser: basic_drawing_command --> FILL."
           << endl;
    }
#endif /* |DEBUG_COMPILE|  */@;

  @=$$@> = FILL;

}
;

@q ***** (5) basic_drawing_command --> FILLDRAW.@>
@*4 \§basic drawing command> $\longrightarrow$ \.{FILLDRAW}.

@
@<Define rules@>= 
  
@=basic_drawing_command: FILLDRAW@>
{
#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
      cerr << "\n*** Parser: basic_drawing_command --> FILLDRAW."
           << endl;
    }
#endif /* |DEBUG_COMPILE|  */@;

  @=$$@> = FILLDRAW;

}
;

@q ***** (5) basic_drawing_command --> UNDRAW.@>
@*4 \§basic drawing command> $\longrightarrow$ \.{UNDRAW}.

@
@<Define rules@>= 
  
@=basic_drawing_command: UNDRAW@>
{
#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
      cerr << "\n*** Parser: basic_drawing_command --> UNDRAW."
           << endl;
    }
#endif /* |DEBUG_COMPILE|  */@;

  @=$$@> = UNDRAW;

}
;

@q ***** (5) basic_drawing_command --> UNFILL.@>
@*4 \§basic drawing command> $\longrightarrow$ \.{UNFILL}.

@
@<Define rules@>= 
  
@=basic_drawing_command: UNFILL@>
{
#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
      cerr << "\n*** Parser: basic_drawing_command --> UNFILL."
           << endl;
    }
#endif /* |DEBUG_COMPILE|  */@;

  @=$$@> = UNFILL;

}
;


@q ***** (5) basic_drawing_command --> UNFILLDRAW.@>
@*4 \§basic drawing command> $\longrightarrow$ \.{UNFILLDRAW}.

@
@<Define rules@>= 
  
@=basic_drawing_command: UNFILLDRAW@>
{
#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
      cerr << "\n*** Parser: basic_drawing_command --> UNFILLDRAW."
           << endl;
    }
#endif /* |DEBUG_COMPILE|  */@;

  @=$$@> = UNFILLDRAW;

};

@q **** (4) drawing_command --> drawdot_or_undrawdot point_expression @>
@q **** (4) with_clause_draw_list                                     @>
@*4 \§command> $\longrightarrow$ \§drawdot or undrawdot>
\§point expression> \§with clause draw list>.

\LOG
\initials{LDF 2004.06.21.}
Added this rule.

\initials{LDF 2004.08.17.}
Removed code from this rule to the function |Scan_Parse::drawdot_command()|,
which is defined in \filename{scanprsf.web}.

\initials{LDF 2005.11.21.}
Removed debugging code.
\ENDLOG 

@<Define rules@>= 
  
@=command: drawdot_or_undrawdot point_expression@>
@= with_clause_draw_list@>@/
{


     Scan_Parse::drawdot_command(static_cast<Scanner_Node>(parameter),
                                 @=$1@>,
                                 static_cast<Point*>(@=$2@>));

     @=$$@> = static_cast<void*>(0);

};

@q **** (4) drawing_command --> drawdot_or_undrawdot bool_point_expression @>
@q **** (4) with_clause_draw_list                                     @>
@*4 \§command> $\longrightarrow$ \§drawdot or undrawdot>
\§bool-point expression> \§with clause draw list>.

\LOG
\initials{LDF 2004.09.09.}
Added this rule.

\initials{LDF 2005.11.21.}
Removed debugging code.  
\ENDLOG 

@<Define rules@>=
  
@=command: drawdot_or_undrawdot bool_point_expression @>
@=with_clause_draw_list@>
{

  Bool_Point* bp = static_cast<Bool_Point*>(@=$2@>);

  Point* p;

@q ***** (5) Error handling:  |bp == 0 || bp->pt == INVALID_POINT|.@>   
@q ***** (5) Not drawing or undrawing dot.@>   

@ Error handling:  |bp == 0 || bp->pt == INVALID_POINT|.
Not drawing or undrawing dot.
\initials{LDF 2004.09.09.}

@<Define rules@>=

  if (bp == static_cast<Bool_Point*>(0) || bp->pt == INVALID_POINT)
     ;       /* Do nothing  */

@q ***** (5) |bp != 0 && bp->pt != INVALID_POINT|.@>   

@ |!(bp == 0 || bp->pt == INVALID_POINT)|.
\initials{LDF 2004.09.09.}

@<Define rules@>=

  else  /* |!(bp == 0 || bp->pt == INVALID_POINT)|  */
    {
      p = create_new<Point>(bp->pt);

      drawdot_command(static_cast<Scanner_Node>(parameter),
                      @=$1@>,
                      p);

    }

@q ***** (5) Delete |bp|, set |$$|, and exit rule.@>

@ Delete |bp|, set |@=$$@>|, and exit rule.
\initials{LDF 2004.09.09.}

@<Define rules@>=

    delete bp;

    @=$$@> = static_cast<void*>(0);

};

@q **** (4) drawing_command --> conditional_drawdot_or_undrawdot @>
@q **** (4) bool_point_expression with_clause_draw_list.         @>
@*4 \§command> $\longrightarrow$ 
\§conditional drawdot or undrawdot>
\§bool-point expression> \§with clause draw list>.

\LOG
\initials{LDF 2004.11.05.}
Added this rule.
\ENDLOG 


@q ***** (5) Definition.@> 

@<Define rules@>=
  
@=command: conditional_drawdot_or_undrawdot@>
@= bool_point_expression with_clause_draw_list@>@/
{

  Bool_Point* bp = static_cast<Bool_Point*>(@=$2@>);
 
  Point* p;

  int command;

  bool condition;

@q ****** (6).@> 
@
@<Define rules@>=

   switch (@=$1@>)
     {
        case DRAWDOT_IF:
            command = DRAWDOT;
            condition = bp->b;
            break;

        case DRAWDOT_UNLESS:
            command = DRAWDOT;
            condition = !bp->b;
            break;

        case UNDRAWDOT_IF:
            command = UNDRAWDOT;
            condition = bp->b;
            break;

        case UNDRAWDOT_UNLESS:
            command = UNDRAWDOT;
            condition = !bp->b;
            break;

     } /* |switch (@=$1@>)|  */



   if (condition)
      {
       
      p = create_new<Point>(bp->pt);

      int status = drawdot_command(static_cast<Scanner_Node>(parameter),
                                   command,
                                   p);

@q ******* (7) Error handling: |Scan_Parse::drawing_command()| failed.@> 

@ Error handling: |Scan_Parse::drawing_command()| failed.
\initials{LDF 2004.11.05.}

@<Define rules@>=

      if (status != 0) /* |Scan_Parse::drawing_command()| failed.  */
        {
#if 0 
          cerr_strm << thread_name 
                    << "ERROR! In parser rule"
                    << endl
                    << "`drawing_command --> conditional_drawdot_or_undrawdot "
                    << "bool_point_expression with_clause_draw_list:"
                    << endl << "`Scan_Parse::drawdot_command()' failed. "
                    << "Will try to continue.";  

          log_message(cerr_strm); 
          cerr_message(cerr_strm, error_stop_value); 
          cerr_strm.str(""); 
#endif 
          
        }  /* |if (status != 0)| (|Scan_Parse::drawing_command()| failed.)  */

   }   /* |if (condition)|  */

@q ****** (6) Delete |bp|, set |$$| to 0, and exit rule.@>

@ Delete |bp|, set |@=$$@>| to 0, and exit rule.
\initials{LDF 2004.11.05.}

@<Define rules@>=

   delete bp;

   @=$$@> = static_cast<void*>(0);

};

@q **** (4) drawdot_or_undrawdot@>
@*3 \§drawdot or undrawdot>.

\LOG
\initials{LDF 2004.06.21.} 
Added this section.
\ENDLOG 

@<Type declarations for non-terminal symbols@>=
@=%type <int_value> drawdot_or_undrawdot@>

@q ***** (5) drawdot_or_undrawdot --> DRAWDOT@>
@*4 \§drawdot or undrawdot> $\longrightarrow$ `\.{DRAWDOT}'.

\LOG
\initials{LDF 2004.06.21.} 
Added this rule.
\ENDLOG 


@<Define rules@>= 

@=drawdot_or_undrawdot: DRAWDOT@>
{
#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
      cerr << "\n*** Parser: drawdot_or_undrawdot --> DRAWDOT."
           << endl;
    }
#endif /* |DEBUG_COMPILE|  */@;

  @=$$@> = DRAWDOT;

}
;

@q ***** (5) drawdot_or_undrawdot --> UNDRAWDOT@>
@*4 \§drawdot or undrawdot> $\longrightarrow$ `\.{UNDRAWDOT}'.

\LOG
\initials{LDF 2004.06.21.} 
Added this rule.
\ENDLOG 


@<Define rules@>= 
@=drawdot_or_undrawdot: UNDRAWDOT@>
{
#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
      cerr << "\n*** Parser: drawdot_or_undrawdot --> UNDRAWDOT."
           << endl;
    }
#endif /* |DEBUG_COMPILE|  */@;

  @=$$@> = UNDRAWDOT;

};

@q **** (4) conditional_drawdot_or_undrawdot@>
@*3 \§conditional drawdot or undrawdot>.

\LOG
\initials{LDF 2004.11.05.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <int_value> conditional_drawdot_or_undrawdot@>

@q ***** (5) conditional_drawdot_or_undrawdot --> DRAWDOT_IF@>
@*4 \§conditional drawdot or undrawdot> $\longrightarrow$ \.{DRAWDOT\_IF}.

\LOG
\initials{LDF 2004.11.05.}
Added this rule.
\ENDLOG 


@<Define rules@>= 

@=conditional_drawdot_or_undrawdot: DRAWDOT_IF@>
{

  @=$$@> = DRAWDOT_IF;

};

@q ***** (5) conditional_drawdot_or_undrawdot --> UNDRAWDOT_IF@>
@*4 \§conditional drawdot or undrawdot> $\longrightarrow$ \.{UNDRAWDOT\_IF}.

\LOG
\initials{LDF 2004.11.05.}
Added this rule.
\ENDLOG 


@<Define rules@>= 
@=conditional_drawdot_or_undrawdot: UNDRAWDOT_IF@>
{

  @=$$@> = UNDRAWDOT_IF;

};

@q ***** (5) conditional_drawdot_or_undrawdot --> DRAWDOT_UNLESS@>
@*4 \§conditional drawdot or undrawdot> $\longrightarrow$ \.{DRAWDOT\_UNLESS}.

\LOG
\initials{LDF 2004.11.05.}
Added this rule.
\ENDLOG 


@<Define rules@>= 

@=conditional_drawdot_or_undrawdot: DRAWDOT_UNLESS@>
{

  @=$$@> = DRAWDOT_UNLESS;

};

@q ***** (5) conditional_drawdot_or_undrawdot --> UNDRAWDOT_UNLESS@>
@*4 \§conditional drawdot or undrawdot> $\longrightarrow$ \.{UNDRAWDOT\_UNLESS}.

\LOG
\initials{LDF 2004.11.05.}
Added this rule.
\ENDLOG 


@<Define rules@>= 
@=conditional_drawdot_or_undrawdot: UNDRAWDOT_UNLESS@>
{

  @=$$@> = UNDRAWDOT_UNLESS;

};


@q **** (4) with_clause_draw_list @>
@*3 \§with clause draw list>.

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> with_clause_draw_list@>


@q ***** (5) with_clause_draw_list --> EMPTY.@>
@*4 \§with clause list> $\longrightarrow$ \.{EMPTY}.

@<Define rules@>= 
  
@=with_clause_draw_list: /* Empty.  */@>
{
#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
      cerr << "\n*** Parser: with_clause_draw_list --> EMPTY."
           << endl;
    }
#endif /* |DEBUG_COMPILE|  */@;

  @=$$@> = static_cast<void*>(0);

}
;

@q ***** (5) with_clause_draw_list --> with_clause_draw_list @>
@q ***** (5) with_clause_draw.                               @>

@*4 \§with clause list> $\longrightarrow$ \§with clause list> 
\§with clause>.

@<Define rules@>= 
  
@=with_clause_draw_list: with_clause_draw_list with_clause_draw@>
{

  @=$$@> = static_cast<void*>(0);

};

@q **** (4) with_clause_draw @>
@*3 \§with clause>.

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> with_clause_draw@>

@q ***** (5) with_clause_draw --> WITH_COLOR_VECTOR color_vector_variable.@>

@*4 \§with clause list> $\longrightarrow$ \.{WITH\_COLOR\_VECTOR}
\§color vector variable>.

\LOG
\initials{LDF 2004.08.29.}
Added this rule.

\initials{LDF 2004.09.05.}
@:BUG FIX@> BUG FIX:  Now setting 
|entry =  static_cast<Id_Map_Entry_Node>(@=$2@>)| and
|scanner_node->color_vector_ptr = entry->object|.  Previously, I set
|scanner_node->color_vector_ptr = @=$2@>|.  The effect is that now
|scanner_node->color_vector_ptr| is a |void| pointer to a |Pointer_Vector<const Color>|
rather than a |void| pointer to an |Id_Map_Entry_Node|.
\ENDLOG 

@<Define rules@>= 
  
@=with_clause_draw: WITH_COLOR_VECTOR color_vector_variable@>
{

  Id_Map_Entry_Node entry =  static_cast<Id_Map_Entry_Node>(@=$2@>);

  static_cast<Scanner_Node>(parameter)->color_vector_ptr = entry->object;

  @=$$@> = static_cast<void*>(0);

};


@q ***** (5) with_clause_draw --> WITH_DRAW_COLOR_VECTOR color_vector_variable.@>

@*4 \§with clause list> $\longrightarrow$ \.{WITH\_DRAW\_COLOR\_VECTOR}
\§color vector variable>.

\LOG
\initials{LDF 2004.08.29.}
Added this rule.

\initials{LDF 2004.09.03.}
@:BUG FIX@> BUG FIX:  
Now setting |scanner_node->draw_color_vector_ptr| to |entry->object|.
Previously, it was set to |@=$2@>|.
\ENDLOG 


@<Define rules@>= 
  
@=with_clause_draw: WITH_DRAW_COLOR_VECTOR color_vector_variable@>
{

  Id_Map_Entry_Node entry =  static_cast<Id_Map_Entry_Node>(@=$2@>);

  static_cast<Scanner_Node>(parameter)->draw_color_vector_ptr = entry->object;

  @=$$@> = static_cast<void*>(0);

};

@q ***** (5) with_clause_draw --> WITH_FILL_COLOR_VECTOR color_vector_variable.@>

@*4 \§with clause list> $\longrightarrow$ \.{WITH\_FILL\_COLOR\_VECTOR}
\§color vector variable>.

\LOG
\initials{LDF 2004.08.29.}
Added this rule.

\initials{LDF 2004.09.05.}
@:BUG FIX@> BUG FIX:  Now setting 
|entry =  static_cast<Id_Map_Entry_Node>(@=$2@>)| and
|scanner_node->fill_color_vector_ptr = entry->object|.  Previously, I set
|scanner_node->fill_color_vector_ptr = @=$2@>|.  The effect is that now
|scanner_node->fill_color_vector_ptr| is a |void| pointer to a 
|Pointer_Vector<const Color>| rather than a |void| pointer to an
|Id_Map_Entry_Node|. 
\ENDLOG 


@<Define rules@>= 
  
@=with_clause_draw: WITH_FILL_COLOR_VECTOR color_vector_variable@>
{

  Id_Map_Entry_Node entry =  static_cast<Id_Map_Entry_Node>(@=$2@>);

  static_cast<Scanner_Node>(parameter)->fill_color_vector_ptr = entry->object;

  @=$$@> = static_cast<void*>(0);

};


@q ***** (5) with_clause_draw --> WITH_PEN pen_expression.@>

@*4 \§with clause list> $\longrightarrow$ \.{WITH\_PEN}
\§pen expression>.
\initials{LDF Undated.}

\LOG
\initials{LDF 2005.03.18.}
Made debugging output thread-safe.  No longer casting |@=$2@>| to
a |void*|, since it already is one.
\ENDLOG 

@<Define rules@>= 
  
@=with_clause_draw: WITH_PEN pen_expression@>
{

   @=$$@> = static_cast<Scanner_Node>(parameter)->pen_ptr = @=$2@>;

};

@q ***** (5) with_clause_draw --> WITH_PEN_VECTOR pen_vector_expression.@>

@*4 \§with clause list> $\longrightarrow$ \.{WITH\_PEN\_VECTOR}
\§pen vector expression>.
\initials{LDF 2005.03.18.}

\LOG
\initials{LDF 2005.03.18.}
Added this rule.
\ENDLOG

@<Define rules@>= 
  
@=with_clause_draw: WITH_PEN_VECTOR pen_vector_expression@>
{

   @=$$@> = static_cast<Scanner_Node>(parameter)->pen_vector_ptr = @=$2@>;

};

@q ***** (5) with_clause_draw --> DASHED dash_pattern_expression.@>

@*4 \§with clause list> $\longrightarrow$ \.{DASHED}
\§dash pattern expression>.
\initials{LDF Undated.}

\LOG
\initials{LDF 2005.03.18.}
Made debugging output thread-safe.  No longer casting |@=$2@>| to
a |void*|, since it already is one.
\ENDLOG 

@<Define rules@>= 
  
@=with_clause_draw: DASHED dash_pattern_expression@>
{

  @=$$@> = static_cast<Scanner_Node>(parameter)->dash_pattern_ptr = @=$2@>;

};


@q ***** (5) with_clause_draw --> WITH_DASH_PATTERN_VECTOR @>
@q ***** (5) dash_pattern_vector_expression.               @>

@*4 \§with clause list> $\longrightarrow$ \.{WITH\_DASH\_PATTERN\_VECTOR}
\§dash pattern vector expression>.
\initials{LDF 2005.03.18.}

\LOG
\initials{LDF 2005.03.18.}
Added this rule.
\ENDLOG

@<Define rules@>= 
  
@=with_clause_draw: WITH_DASH_PATTERN_VECTOR dash_pattern_vector_expression@>
{

   @=$$@> = static_cast<Scanner_Node>(parameter)->dash_pattern_vector_ptr = @=$2@>;

};


@q ***** (5) with_clause_draw --> WITH_COLOR color_expression.@>

@*4 \§with clause list> $\longrightarrow$ \.{WITH\_COLOR}
\§color expression>.

@<Define rules@>= 
  
@=with_clause_draw: WITH_COLOR color_expression@>
{

  @=$$@> = static_cast<Scanner_Node>(parameter)->color_ptr = static_cast<void*>(@=$2@>);

};

@q ***** (5) with_clause_draw --> WITH_DRAW_COLOR color_expression.@>

@*4 \§with clause list> $\longrightarrow$ \.{WITH\_DRAW\_COLOR}
\§color expression>.

@<Define rules@>= 
  
@=with_clause_draw: WITH_DRAW_COLOR color_expression@>
{

  @=$$@> = static_cast<Scanner_Node>(parameter)->draw_color_ptr 
     = static_cast<void*>(@=$2@>);

};

@q ***** (5) with_clause_draw --> WITH_FILL_COLOR color_expression.@>

@*4 \§with clause list> $\longrightarrow$ \.{WITH\_FILL\_COLOR}
\§color expression>.

@<Define rules@>= 
  
@=with_clause_draw: WITH_FILL_COLOR color_expression@>
{

  @=$$@> = static_cast<Scanner_Node>(parameter)->fill_color_ptr 
     = static_cast<void*>(@=$2@>);

};

@q ***** (5) with_clause_draw --> ON_PICTURE picture_expression.@>

@*4 \§with clause list> $\longrightarrow$ \.{ON\_PICTURE}
\§picture expression>.


\LOG
\initials{LDF 2004.06.30.}
Changed the code in this rule to account for the fact that 
|picture_expression| is now a |Id_Map_Entry_Node|, cast to |void*|.
Formerly, it was a |Picture*|, cast to |void*|.
\ENDLOG 

\TODO
@q { @>
@:TO DO}{{\bf TO DO}@>
@q } @> 
@q !! TO DO:  @>
\initials{LDF 2004.06.30.}
Lock mutex for |picture_expression|, which is an |Id_Map_Entry_Node|.
\ENDTODO 

@<Define rules@>= 
  
@=with_clause_draw: ON_PICTURE picture_expression@>
{

  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$2@>); 


  @=$$@> = static_cast<Scanner_Node>(parameter)->picture_entry_ptr 
     = static_cast<void*>(entry->object);

};

@q ** (2) command --> PICKUP pen_expression.@> 
@* \§command> $\longrightarrow$ 
\.{PICKUP} \§pen expression>.
\initials{LDF 2004.11.11.}

\LOG
\initials{LDF 2004.11.11.}
Added this rule.

\initials{LDF 2004.11.12.}
Got this rule to work.

\initials{LDF 2004.11.12.}
Added error handling for the case that 
|pen_expression == 0|.

\initials{LDF 2004.11.12.}
Now trying to allocate memory for a new |Pen|, if 
|*(scanner_node->current_pen) == 0|.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=command: PICKUP pen_expression@>@/
{

   Pen* p = static_cast<Pen*>(@=$2@>); 

@q **** (4) Error handling:  |p == 0|.@>   

@ Error handling:  |p == 0|.
\initials{LDF 2004.11.12.}

\LOG
\initials{LDF 2004.11.12.}
Added this section.
\ENDLOG


@<Define rules@>=

   if (p == static_cast<Pen*>(0))
     {
#if 0 
        cerr_strm << thread_name << "ERROR! In `yyparse()', rule "
                  << "`command -->" << endl 
                  << "PICKUP pen_expression':"
                  << endl << "`pen_expression' is null. "
                  << "Not changing `current_pen'.";

        log_message(cerr_strm);
        cerr_message(cerr_strm, error_stop_value);
        cerr_strm.str("");
#endif 

     }  /* |if (p == 0)|  */

@q **** (4) |p != 0|.@>   

@ |p != 0|.
\initials{LDF 2004.11.12.}


@<Define rules@>=
   else /* |p != 0|  */
      {
         Pen* q = *(static_cast<Scanner_Node>(parameter)->current_pen);

@q ***** (5) |q == 0|.  Try to allocate memory for a new |Pen|.@> 
@ |q == 0|.  Try to allocate memory for a new |Pen|.
\initials{LDF 2004.11.12.}

\LOG
\initials{LDF 2004.11.12.}
Added this section.
\ENDLOG

@<Define rules@>=
         if (q == static_cast<Pen*>(0))
            {

                q = create_new<Pen>(0);

               *(static_cast<Scanner_Node>(parameter)->current_pen) = q;

            }  /* |if (q == 0)|  */


@q ***** (5).@> 

         *q = *p;

         delete p;

      }   /* |else| (|p != 0|)  */


   @=$$@> = static_cast<void*>(0);

};

@q ** (2) command --> PICKUP color_expression.@> 
@* \§command> $\longrightarrow$ 
\.{PICKUP} \§color expression>.
\initials{LDF 2004.11.12.}

\LOG
\initials{LDF 2004.11.12.}
Added this rule.

\initials{LDF 2004.11.13.}
Now calling |Scan_Parse::pickup_command()|.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=command: PICKUP color_expression@>@/
{


    int status = pickup_command<Color>(static_cast<Scanner_Node>(parameter),
                                       static_cast<Color*>(@=$2@>),
                                       static_cast<Scanner_Node>(parameter)->current_color);

 
    if (status != 0)
       {
#if 0 
           cerr_strm << thread_name 
                      << "ERROR! In `yyparse()', rule "
                      << "`pickup_command -->" << endl 
                      << "PICKUP color_expression':"
                      << endl 
                      << "`Scan_Parse::pickup_command<Color>()' "
                      << "failed, returning " << status << "."
                      << endl << "Will try to continue.";
 
            log_message(cerr_strm);
            cerr_message(cerr_strm, error_stop_value);
            cerr_strm.str("");                      
#endif 
       } /* |if (status != 0)|  */

    @=$$@> = static_cast<void*>(0);

};

@q ** (2) command --> PICKUP_DRAW_COLOR color_expression.@> 
@* \§command> $\longrightarrow$ 
\.{PICKUP\_DRAW\_COLOR} \§color expression>.
\initials{LDF 2004.11.12.}

\LOG
\initials{LDF 2004.11.12.}
Added this rule.

\initials{LDF 2004.11.13.}
Now calling |Scan_Parse::pickup_command()|.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=command: PICKUP_DRAW_COLOR color_expression@>@/
{


    int status = pickup_command<Color>(static_cast<Scanner_Node>(parameter), 
                                       static_cast<Color*>(@=$2@>),
                                       static_cast<Scanner_Node>(parameter)->current_draw_color);
 
    if (status != 0)
       {
#if 0 
           cerr_strm << thread_name 
                      << "ERROR! In `yyparse()', rule "
                      << "`pickup_command -->" << endl 
                      << "PICKUP_DRAW_COLOR color_expression':"
                      << endl 
                      << "`Scan_Parse::pickup_command<Color>()' "
                      << "failed, returning " << status << "."
                      << endl << "Will try to continue.";
 
            log_message(cerr_strm);
            cerr_message(cerr_strm, error_stop_value);
            cerr_strm.str("");                      
#endif 
       } /* |if (status != 0)|  */

    @=$$@> = static_cast<void*>(0);

};

@q ** (2) command --> PICKUP_FILL_COLOR color_expression.@> 
@* \§command> $\longrightarrow$ 
\.{PICKUP\_FILL\_COLOR} \§color expression>.
\initials{LDF 2004.11.12.}

\LOG
\initials{LDF 2004.11.12.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=command: PICKUP_FILL_COLOR color_expression@>@/
{

    int status = pickup_command<Color>(static_cast<Scanner_Node>(parameter), 
                                       static_cast<Color*>(@=$2@>),
                                       static_cast<Scanner_Node>(parameter)->current_fill_color);
 
    if (status != 0)
       {
#if 0 
           cerr_strm << thread_name 
                      << "ERROR! In `yyparse()', rule "
                      << "`pickup_command -->" << endl 
                      << "PICKUP_FILL_COLOR color_expression':"
                      << endl 
                      << "`Scan_Parse::pickup_command<Color>()' "
                      << "failed, returning " << status << "."
                      << endl << "Will try to continue.";
 
            log_message(cerr_strm);
            cerr_message(cerr_strm, error_stop_value);
            cerr_strm.str("");                      
#endif 
       } /* |if (status != 0)|  */

    @=$$@> = static_cast<void*>(0);

};


@q ** (2) command --> PICKUP dash_pattern_expression.@> 
@* \§command> $\longrightarrow$ 
\.{PICKUP} \§dash pattern expression>.
\initials{LDF 2004.11.11.}

\LOG
\initials{LDF 2004.11.12.}
Added this rule.

\initials{LDF 2004.11.13.}
Removed code from this rule to |Scan_Parse::pickup_command()|,
which is defined in \filename{scanprse.web}.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=command: PICKUP dash_pattern_expression@>@/
{

    int status = pickup_command<Dash_Pattern>(
                            static_cast<Scanner_Node>(parameter), 
                            static_cast<Dash_Pattern*>(@=$2@>),
                            static_cast<Scanner_Node>(parameter)->current_dash_pattern);

 
    if (status != 0)
       {
#if 0 
           cerr_strm << thread_name 
                      << "ERROR! In `yyparse()', rule "
                      << "`pickup_command -->" << endl 
                      << "PICKUP dash_pattern_expression':"
                      << endl 
                      << "`Scan_Parse::pickup_command<Dash_Pattern>()' "
                      << "failed, returning " << status << "."
                      << endl << "Will try to continue.";
 
            log_message(cerr_strm);
            cerr_message(cerr_strm, error_stop_value);
            cerr_strm.str("");                      
#endif 

       } /* |if (status != 0)|  */

    @=$$@> = static_cast<void*>(0);

};

@q ** (2) command --> DROP_PEN.@> 
@* \§command> $\longrightarrow$ \§DROP\_PEN>.
\initials{LDF 2004.11.12.}

\LOG
\initials{LDF 2004.11.12.}
Added this rule.

\initials{LDF 2004.11.12.}
Now issuing a warning if |*(scanner_node->current_pen) == 0|.

\initials{LDF 2004.11.14.}
Removed code from this rule.  Now calling 
|Scan_Parse::drop_command()|.  It currently only ever 
returns 0, so there's no need to test its return value.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=command: DROP_PEN@>@/
{

   drop_command<Pen>(static_cast<Scanner_Node>(parameter),
                     static_cast<Scanner_Node>(parameter)->current_pen,
                     "scanner_node->current_pen");

   @=$$@> = static_cast<void*>(0);


};

@q ** (2) command --> DROP_COLOR.@> 
@* \§command> $\longrightarrow$ \§DROP\_COLOR>.
\initials{LDF 2004.11.12.}

\LOG
\initials{LDF 2004.11.12.}
Added this rule.

\initials{LDF 2004.11.14.}
Removed code from this rule.  Now calling 
|Scan_Parse::drop_command()|.  It currently only ever 
returns 0, so there's no need to test its return value.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=command: DROP_COLOR@>@/
{

   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter); 

   drop_command<Color>(scanner_node,
                       scanner_node->current_color,
                       "scanner_node->current_color");

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) command --> DROP_DRAW_COLOR.@> 
@* \§command> $\longrightarrow$ \§DROP\_DRAW\_COLOR>.
\initials{LDF 2004.11.12.}

\LOG
\initials{LDF 2004.11.12.}
Added this rule.

\initials{LDF 2004.11.14.}
Removed code from this rule.  Now calling 
|Scan_Parse::drop_command()|.  It currently only ever 
returns 0, so there's no need to test its return value.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=command: DROP_DRAW_COLOR@>@/
{

   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter); 

   drop_command<Color>(scanner_node,
                       scanner_node->current_draw_color,
                       "scanner_node->current_draw_color");

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) command --> DROP_FILL_COLOR.@> 
@* \§command> $\longrightarrow$ \§DROP\_FILL\_COLOR>.
\initials{LDF 2004.11.12.}

\LOG
\initials{LDF 2004.11.12.}
Added this rule.

\initials{LDF 2004.11.14.}
Removed code from this rule.  Now calling 
|Scan_Parse::drop_command()|.  It currently only ever 
returns 0, so there's no need to test its return value.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=command: DROP_FILL_COLOR@>@/
{

   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter); 

   drop_command<Color>(scanner_node,
                       scanner_node->current_fill_color,
                       "scanner_node->current_fill_color");


   @=$$@> = static_cast<void*>(0);

};

@q ** (2) command --> DROP_DASH_PATTERN.@> 
@* \§drop dash pattern command> $\longrightarrow$ 
\§DROP\_DASH\_PATTERN>.
\initials{LDF 2004.11.12.}

\LOG
\initials{LDF 2004.11.12.}
Added this rule.

\initials{LDF 2004.11.14.}
Removed code from this rule.  Now calling 
|Scan_Parse::drop_command()|.  It currently only ever 
returns 0, so there's no need to test its return value.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=command: DROP_DASH_PATTERN@>@/
{

   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter); 

   drop_command<Dash_Pattern>(scanner_node,
                              scanner_node->current_dash_pattern,
                              "scanner_node->current_dash_pattern");


   @=$$@> = static_cast<void*>(0);

};


@q ** (2) command --> DROP_ALL.@> 
@*1 \§command> $\longrightarrow$ 
\§DROP\_ALL>.
\initials{LDF 2004.11.14.}

\LOG
\initials{LDF 2004.11.14.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=command: DROP_ALL@>@/
{

   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter); 

   drop_command<Pen>(scanner_node,
                     scanner_node->current_pen,
                     "scanner_node->current_pen");

   drop_command<Color>(scanner_node,
                       scanner_node->current_color,
                       "scanner_node->current_color");


   drop_command<Color>(scanner_node,
                       scanner_node->current_draw_color,
                       "scanner_node->current_draw_color");

   drop_command<Color>(scanner_node,
                       scanner_node->current_fill_color,
                       "scanner_node->current_fill_color");

   drop_command<Dash_Pattern>(scanner_node,
                              scanner_node->current_dash_pattern,
                              "scanner_node->current_dash_pattern");


   @=$$@> = static_cast<void*>(0);

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
@q fill-column:80 @>
@q End: @>
