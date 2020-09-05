@q passign.w @> 
@q Created by Laurence Finston Thu Apr 29 13:55:05 MEST 2004  @>

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




@q * (0) Assignments.  @>
@** Assignments.  


\LOG
\initials{LDF 2004.05.03.}  
Completely rewrote the rule for assignment to
|numerics|.  The way it was before failed when I tried to use |variable| with no
further qualification in rules for both |numeric| and |point| variables. 

\initials{LDF 2004.11.05.}
Added type declaration for |operation_assignment|.

\initials{LDF 2004.12.04.}
Removed type declaration for |operation_assignment| to 
\filename{popassgn.w}.
\ENDLOG 


\TODO
@q { @>
@:TO DO}{{\bf TO DO}@>
@q } @> 
@q !! TO DO:  @>
\initials{LDF 2004.06.30.}
Make it possible to chain assignments for all types.  
\ENDTODO 

@q * (1) assignment.  @>
@* \§assignment>.


@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> assignment@>

@q ** (2) Non-vector types.@> 
@*1 Non-vector types.
\initials{LDF 2005.01.07.}

\LOG
\initials{LDF 2005.01.07.}
Added this section.
\ENDLOG

@q *** (3) Non-|Shape| types.@> 
@*1 Non-{\bf Shape}vector types.
\initials{LDF 2005.01.07.}

\LOG
\initials{LDF 2005.01.07.}
Added this section.
\ENDLOG


@q **** (4) assignment --> boolean_assignment. @>
@*3 \§assignment> $\longrightarrow$ \§boolean assignment>.

@<Define rules@>= 
  
@=assignment: boolean_assignment@>
{

  @=$$@> = @=$1@>;

};

@q **** (4) assignment --> numeric_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§numeric assignment>.
\§assignment>.

@<Define rules@>= 
  
@=assignment: numeric_assignment@>
{
  @=$$@> = @=$1@>;

};

@q **** (4) assignment --> ulong_long_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§ulong long assignment>.
\§assignment>.
\initials{LDF 2005.12.05.}

\LOG
\initials{LDF 2005.12.05.}
Added this rule.
\ENDLOG

@<Define rules@>= 
  
@=assignment: ulong_long_assignment@>
{
  @=$$@> = @=$1@>;

};


@q *** (3) assignment --> string_assignment.@>
@*1 \§assignment> $\longrightarrow$ \§string assignment>.

\LOG
\initials{LDF 2004.05.19.}  
Added this rule.
\ENDLOG 

@<Define rules@>= 
  
@=assignment: string_assignment@>
{

  @=$$@> = @=$1@>;

};

@q *** (3) assignment --> pen_assignment.  @>
@*1 \§assignment> $\longrightarrow$ \§pen assignment>.

\LOG
\initials{LDF 2004.05.21.}  
Added this rule.
\ENDLOG 

@<Define rules@>= 
  
@=assignment: pen_assignment@>
{

  @=$$@> = @=$1@>;

};

@q *** (3) assignment --> dash_pattern_assignment.@>
@*1 \§assignment> $\longrightarrow$ \§dash pattern assignment>.

\LOG
\initials{LDF 2004.06.07.}  
Added this rule.
\ENDLOG 

@<Define rules@>= 
  
@=assignment: dash_pattern_assignment@>
{
  @=$$@> = @=$1@>;

};

@q *** (3) assignment --> color_assignment.@>
@*1 \§assignment> $\longrightarrow$ \§color assignment>.

\LOG
\initials{LDF 2004.05.25.}  
Added this rule.
\ENDLOG 

@<Define rules@>= 
  
@=assignment: color_assignment@>
{
  @=$$@> = @=$1@>;

};

@q **** (4) assignment --> transform_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§transform assignment>.

@<Define rules@>= 
@=assignment: transform_assignment@>
{
  @=$$@> = @=$1@>;

}; 

@q **** (4) assignment --> picture_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§picture assignment>.

\LOG
\initials{LDF 2004.05.21.}  
Added this rule.
\ENDLOG 

@<Define rules@>= 
  
@=assignment: picture_assignment@>
{

  @=$$@> = @=$1@>;

};

@q **** (4) assignment --> focus_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§focus assignment>.

\LOG
\initials{LDF 2004.06.08.}  
Added this rule.
\ENDLOG 

@<Define rules@>= 
  
@=assignment: focus_assignment@>
{

  @=$$@> = @=$1@>;

};

@q **** (4) assignment --> macro_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§macro assignment>.

\LOG
\initials{LDF 2004.12.30.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
  
@=assignment: macro_assignment@>
{
  @=$$@> = @=$1@>;

};

@q *** (3) |Shape| types.@> 
@*2 {\bf Shape} types.
\initials{LDF 2005.01.07.}

\LOG
\initials{LDF 2005.01.07.}
Added this section.
\ENDLOG

@q **** (4) assignment --> bool_point_assignment. @>
@*3 \§assignment> $\longrightarrow$ \§bool-point assignment>.

\LOG
\initials{LDF 2004.09.01.}
Added this rule.
\ENDLOG

@<Define rules@>= 
  
@=assignment: bool_point_assignment@>
{

  @=$$@> = @=$1@>;

};

@q **** (4) assignment --> point_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§point assignment>.
@<Define rules@>= 
@=assignment: point_assignment@>
{

  @=$$@> = @=$1@>;

}; 


@q **** (4) assignment --> path_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§path assignment>.
@<Define rules@>= 
@=assignment: path_assignment@>
{

 @=$$@> = @=$1@>;

}; 

@q **** (4) assignment --> ellipse_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§ellipse assignment>.

\LOG
\initials{LDF 2004.06.29.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=assignment: ellipse_assignment@>
{

  @=$$@> = @=$1@>;

}; 

@q **** (4) assignment --> circle_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§circle assignment>.
@<Define rules@>= 
@=assignment: circle_assignment@>
{
  @=$$@> = @=$1@>;
}; 

@q **** (4) assignment --> parabola_assignment.@>

@*3 \§assignment> $\longrightarrow$ \§parabola assignment>.
\initials{LDF 2005.11.07.}

\LOG
\initials{LDF 2005.11.07.}
Added this rule.
\ENDLOG

@<Define rules@>= 
@=assignment: parabola_assignment@>
{
  @=$$@> = @=$1@>;
}; 


@q **** (4) assignment --> hyperbola_assignment.@>

@*3 \§assignment> $\longrightarrow$ \§hyperbola assignment>.
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Added this rule.
\ENDLOG

@<Define rules@>= 
@=assignment: hyperbola_assignment@>
{
  @=$$@> = @=$1@>;
}; 


@q **** (4) assignment --> arc_assignment.@>

@*3 \§assignment> $\longrightarrow$ \§arc assignment>.
\initials{LDF 2007.10.13.}

\LOG
\initials{LDF 2007.10.13.}
Added this rule.
\ENDLOG

@<Define rules@>= 
@=assignment: arc_assignment@>
{
  @=$$@> = @=$1@>;
}; 


@q **** (4) assignment --> conic_section_lattice_assignment.@>

@*3 \§assignment> $\longrightarrow$ \§conic section lattice assignment>.
\initials{LDF 2007.07.29.}

\LOG
\initials{LDF 2007.07.29.}
Added this rule.
\ENDLOG

@<Define rules@>= 
@=assignment: conic_section_lattice_assignment@>
{
  @=$$@> = @=$1@>;
}; 



@q **** (4) assignment --> helix_assignment.@>

@*3 \§assignment> $\longrightarrow$ \§helix assignment>.
\initials{LDF 2005.05.20.}

\LOG
\initials{LDF 2005.05.20.}
Added this rule.
\ENDLOG

@<Define rules@>= 
@=assignment: helix_assignment@>
{
  @=$$@> = @=$1@>;
}; 


@q **** (4) assignment --> rectangle_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§rectangle assignment>.

\LOG
\initials{LDF 2004.06.30.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=assignment: rectangle_assignment@>
{
  @=$$@> = @=$1@>;

}; 

@q **** (4) assignment --> polygon_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§polygon assignment>.
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.02.11.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=assignment: polygon_assignment@>
{
  @=$$@> = @=$1@>;

}; 

@q **** (4) assignment --> reg_polygon_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§regular polygon assignment>.

\LOG
\initials{LDF 2004.07.06.}
Added this rule.

\initials{LDF 2004.08.18.}
Now setting |@=$$@> = @=$1@>|.
\ENDLOG 

@<Define rules@>= 
@=assignment: reg_polygon_assignment@>
{
  @=$$@> = @=$1@>;

}; 

@q **** (4) assignment --> triangle_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§triangle assignment>.
\initials{2009.01.06.}

\LOG
\initials{2009.01.06.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=assignment: triangle_assignment@>
{
  @=$$@> = @=$1@>;

}; 

@q **** (4) assignment --> cuboid_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§cuboid assignment>.

\LOG
\initials{LDF 2004.08.18.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=assignment: cuboid_assignment@>
{
      @=$$@> = @=$1@>;
}; 

@q **** (4) assignment --> polyhedron_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§polyhedron assignment>.

\LOG
\initials{LDF 2004.08.31.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=assignment: polyhedron_assignment@>
{

      @=$$@> = @=$1@>;

}; 

@q **** (4) assignment --> sphere_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§sphere assignment>.

\LOG
\initials{LDF 2005.10.30.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=assignment: sphere_assignment@>
{
      @=$$@> = @=$1@>;
}; 

@q **** (4) assignment --> sphere_development_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§sphere development assignment>.

\LOG
\initials{LDF 2009.11.09.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=assignment: sphere_development_assignment@>
{
      @=$$@> = @=$1@>;
}; 

@q **** (4) assignment --> plane_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§plane assignment>.

\LOG
\initials{LDF 2005.10.30.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=assignment: plane_assignment@>
{

      @=$$@> = @=$1@>;

}; 



@q **** (4) assignment --> origami_figure_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§origami figure assignment>.

\LOG
\initials{LDF 2005.10.30.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=assignment: origami_figure_assignment@>
{

      @=$$@> = @=$1@>;

}; 



@q ** (2) Vector types.@> 
@*1 Vector types.
\initials{LDF 2005.01.07.}

\LOG
\initials{LDF 2005.01.07.}
Added this section.
\ENDLOG

@q *** (3) Non-|Shape| types.@> 
@*2 Non-{\bf Shape} types.
\initials{LDF 2005.01.07.}

\LOG
\initials{LDF 2005.01.07.}
Added this section.
\ENDLOG

@q **** (4) assignment --> boolean_vector_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§boolean vector assignment>.
\initials{LDF 2005.01.07.}

\LOG
\initials{LDF 2005.01.07.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=assignment: boolean_vector_assignment@>
{

  @=$$@> = @=$1@>;

};

@q ** (2) numeric_vector_assignment.@> 
@*1 \§numeric vector assignment>.
\initials{LDF 2005.08.31.}

\LOG
\initials{LDF 2005.08.31.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> numeric_vector_assignment@>

@q *** (3) assignment: numeric_vector_assignment.@> 
@*2 \§assignment> $\longrightarrow$ \§numeric vector assignment>.
\initials{LDF 2005.08.31.}

\LOG
\initials{LDF 2005.08.31.}
Added this rule.  I believe it existed before, but I had gotten rid of
it in the course of debugging the rule
\§numeric vector assignment>
$\longrightarrow$ \§numeric vector variable> 
\.{ASSIGN} \§numeric vector expression>.      
\ENDLOG

@<Define rules@>=
@=assignment: numeric_vector_assignment@>  
{
   @=$$@> = @=$1@>;
};

@q *** (3) numeric_vector_assignment --> numeric_vector_variable @>  
@q *** (3) ASSIGN numeric_vector_expression.                     @> 

@*2 \§numeric vector assignment>
$\longrightarrow$ \§numeric vector variable> 
\.{ASSIGN} \§numeric vector expression>.      
\initials{LDF 2005.08.30.}

It is currently not possible to chain 
\§numeric vector assignments>.
\initials{LDF 2005.08.31.}

\LOG
\initials{LDF 2005.08.30.}
Added this rule.

\initials{LDF 2005.08.30.}
Changed |PLUS_ASSIGN| to |ASSIGN|.

\initials{LDF 2005.08.31.}
@:BUG FIX@> BUG FIX: 
Rewrote this rule.  There was a bug in the previous version.  
I've managed to get rid of it, but I don't know what it was.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=

@=numeric_vector_assignment: numeric_vector_variable @>  
@=ASSIGN numeric_vector_expression@>@/
{

   typedef Pointer_Vector<real> PV;

   PV* pv = static_cast<PV*>(@=$3@>); 

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ***** (5) Error handling for the case that |entry == 0|.@>

@ Error handling for the case that |entry == 0 |.
\initials{LDF 2005.08.30.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      delete pv;

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == static_cast<Id_Map_Entry_Node>(0))|  */

@q ***** (5) |entry != static_cast<Id_Map_Entry_Node>(0)|.@>   

@ |entry != static_cast<Id_Map_Entry_Node>(0)|.
\initials{LDF 2005.08.30.}


@<Define rules@>=

  else /* |entry != static_cast<Id_Map_Entry_Node>(0)|  */
    {

      int status = Scan_Parse::vector_type_assign<real, real>(
                      static_cast<Scanner_Node>(parameter),
                      entry,
                      pv);          

@q ****** (6) Error handling:                                @> 
@q ****** (6) |Scan_Parse::vector_type_assign| failed.@> 

@ Error handling:  |Scan_Parse::vector_type_assign| 
failed. 
\initials{LDF 2005.08.30.}

@<Define rules@>=
                            

    if (status != 0)
         {
            delete pv;

            @=$$@> = static_cast<void*>(0);

         } /* |if (status != 0)|  */

@q ****** (6) |Scan_Parse::vector_type_assign| succeeded.@> 

@ |Scan_Parse::vector_type_assign| succeeded. 
\initials{LDF 2005.08.30.}

@<Define rules@>=

   else /* |status == 0|  */
      {
         delete pv;

         @=$$@> = static_cast<void*>(0);
 
      }  /* |else| (|status == 0|)  */


   }   /* |else| (|entry != static_cast<Id_Map_Entry_Node>(0)|)  */


@q ***** (5).@> 

}; 


@q ** (2) ulong_long_vector_assignment.@> 
@*1 \§ulong long vector assignment>.
\initials{LDF 2005.12.05.}

\LOG
\initials{LDF 2005.12.05.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> ulong_long_vector_assignment@>

@q *** (3) assignment: ulong_long_vector_assignment.@> 
@*2 \§assignment> $\longrightarrow$ \§ulong long vector assignment>.
\initials{LDF 2005.12.05.}

\LOG
\initials{LDF 2005.12.05.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=assignment: ulong_long_vector_assignment@>  
{
   @=$$@> = @=$1@>;
};

@q *** (3) ulong_long_vector_assignment --> ulong_long_vector_variable @>  
@q *** (3) ASSIGN ulong_long_vector_expression.                     @> 

@*2 \§ulong long vector assignment>
$\longrightarrow$ \§ulong long vector variable> 
\.{ASSIGN} \§ulong long vector expression>.      
\initials{LDF 2005.12.05.}

It is currently not possible to chain 
\§ulong long vector assignments>.
\initials{LDF 2005.12.05.}

\LOG
\initials{LDF 2005.12.05.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=

@=ulong_long_vector_assignment: ulong_long_vector_variable @>  
@=ASSIGN ulong_long_vector_expression@>@/
{

   typedef Pointer_Vector<unsigned long long> PV;

   PV* pv = static_cast<PV*>(@=$3@>); 

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == static_cast<Id_Map_Entry_Node>(0))|  */

  else /* |entry != static_cast<Id_Map_Entry_Node>(0)|  */
    {


         Scan_Parse::vector_type_assign<unsigned long long, unsigned long long>(
               static_cast<Scanner_Node>(parameter),
               entry,
               pv);          


            delete pv;

            @=$$@> = static_cast<void*>(0);

    }  /* |else|  */

}; 

@q **** (4) assignment --> string_vector_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§string vector assignment>.
\initials{LDF 2005.01.09.}

\LOG
\initials{LDF 2005.01.09.}
Added this rule.
\ENDLOG 

@<Define rules@>=

@=assignment: string_vector_assignment@>
{

  @=$$@> = @=$1@>;

};

@q **** (4) assignment --> pen_vector_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§pen vector assignment>.
\initials{LDF 2005.01.13.}

\LOG
\initials{LDF 2005.01.13.}
Added this rule.
\ENDLOG 

@<Define rules@>=

@=assignment: pen_vector_assignment@>
{

  @=$$@> = @=$1@>;

};

@q **** (4) assignment --> dash_pattern_vector_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§dash pattern vector assignment>.
\initials{LDF 2005.01.13.}

\LOG
\initials{LDF 2005.01.13.}
Added this rule.
\ENDLOG 

@<Define rules@>=

@=assignment: dash_pattern_vector_assignment@>
{

  @=$$@> = @=$1@>;

};



@q **** (4) assignment --> color_vector_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§color vector assignment>.


\LOG
\initials{LDF 2004.08.23.}  
Added this rule.

\initials{LDF 2004.11.05.}
Commented-out this rule.

\initials{LDF 2005.01.07.}
Commented this rule back in.
\ENDLOG 

@<Define rules@>=

@=assignment: color_vector_assignment@>
{

  @=$$@> = @=$1@>;

};

@q **** (4) assignment --> transform_vector_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§transform vector assignment>.
\initials{LDF 2005.01.13.}

\LOG
\initials{LDF 2005.01.13.}
Added this rule.
\ENDLOG 

@<Define rules@>=

@=assignment: transform_vector_assignment@>
{

  @=$$@> = @=$1@>;

};

@q **** (4) assignment --> focus_vector_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§focus vector assignment>.
\initials{LDF 2005.01.18.}

\LOG
\initials{LDF 2005.01.18.}
Added this rule.
\ENDLOG 

@<Define rules@>=

@=assignment: focus_vector_assignment@>
{

  @=$$@> = @=$1@>;

};




@q *** (3) |Shape| types.@> 
@*2 {\bf Shape} types.
\initials{LDF 2005.01.07.}

\LOG
\initials{LDF 2005.01.07.}
Added this section.
\ENDLOG

@q **** (4) assignment --> point_vector_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§point vector assignment>.


\LOG
\initials{LDF 2004.09.01.}  
Added this rule.

\initials{LDF 2004.11.05.}
Commented-out this rule.

\initials{LDF 2004.11.10.}
Commented this rule back in.
\ENDLOG 

@<Define rules@>=
  

@=assignment: point_vector_assignment@>
{

  @=$$@> = @=$1@>;

};


@q **** (4) assignment --> bool_point_vector_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§bool-point vector assignment>.


\LOG
\initials{LDF 2004.09.01.}  
Added this rule.

\initials{LDF 2004.11.05.}
Commented-out this rule.

\initials{LDF 2004.11.06.}
Commented this rule in again.
\ENDLOG 

@<Define rules@>=
   
@=assignment: bool_point_vector_assignment@>
{

  @=$$@> = @=$1@>;

};


@q **** (4) assignment --> path_vector_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§path vector assignment>.
\initials{LDF 2004.12.10.}

\LOG
\initials{LDF 2004.12.10.}
Added this rule.
\ENDLOG 

@<Define rules@>=
  
@=assignment: path_vector_assignment@>
{

  @=$$@> = @=$1@>;

};


@q **** (4) assignment --> ellipse_vector_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§ellipse vector assignment>.
\initials{LDF 2004.12.14.}

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG 

@<Define rules@>=
  
@=assignment: ellipse_vector_assignment@>
{

  @=$$@> = @=$1@>;

};

@q **** (4) assignment --> circle_vector_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§circle vector assignment>.
\initials{LDF 2004.12.14.}

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG 

@<Define rules@>=
  
@=assignment: circle_vector_assignment@>
{

  @=$$@> = @=$1@>;

};

@q **** (4) assignment --> parabola_vector_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§parabola vector assignment>.
\initials{LDF 2005.11.07.}

\LOG
\initials{LDF 2005.11.07.}
Added this rule.
\ENDLOG 

@<Define rules@>=
  
@=assignment: parabola_vector_assignment@>
{

  @=$$@> = @=$1@>;

};

@q **** (4) assignment --> hyperbola_vector_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§hyperbola vector assignment>.
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Added this rule.
\ENDLOG 

@<Define rules@>=
  
@=assignment: hyperbola_vector_assignment@>
{

  @=$$@> = @=$1@>;

};


@q **** (4) assignment --> arc_vector_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§arc vector assignment>.
\initials{LDF 2007.10.13.}

\LOG
\initials{LDF 2007.10.13.}
Added this rule.
\ENDLOG 

@<Define rules@>=
  
@=assignment: arc_vector_assignment@>
{

  @=$$@> = @=$1@>;

};



@q **** (4) assignment --> conic_section_lattice_vector_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§conic section lattice vector assignment>.
\initials{LDF 2007.07.29.}

\LOG
\initials{LDF 2007.07.29.}
Added this rule.
\ENDLOG 

@<Define rules@>=
  
@=assignment: conic_section_lattice_vector_assignment@>
{

  @=$$@> = @=$1@>;

};


@q **** (4) assignment --> helix_vector_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§helix vector assignment>.
\initials{LDF 2005.05.20.}

\LOG
\initials{LDF 2005.05.20.}
Added this rule.
\ENDLOG 

@<Define rules@>=
  
@=assignment: helix_vector_assignment@>
{

  @=$$@> = @=$1@>;

};



@q **** (4) assignment --> nurb_vector_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§nurb vector assignment>.
\initials{LDF 2005.01.26.}

\LOG
\initials{LDF 2005.01.26.}
Added this rule.
\ENDLOG 

@<Define rules@>=
  
@=assignment: nurb_vector_assignment@>
{

   @=$$@> = @=$1@>;

};


@q **** (4) assignment --> triangle_vector_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§triangle vector assignment>.
\initials{LDF 2005.01.25.}

\LOG
\initials{LDF 2005.01.25.}
Added this rule.
\ENDLOG 

@<Define rules@>=
  
@=assignment: triangle_vector_assignment@>
{

   @=$$@> = @=$1@>;

};


@q **** (4) assignment --> rectangle_vector_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§rectangle vector assignment>.
\initials{LDF 2004.12.14.}

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG 

@<Define rules@>=
  
@=assignment: rectangle_vector_assignment@>
{

  @=$$@> = @=$1@>;

};

@q **** (4) assignment --> polygon_vector_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§polygon vector assignment>.
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.02.11.}
Added this rule.
\ENDLOG 

@<Define rules@>=
  
@=assignment: polygon_vector_assignment@>
{

  @=$$@> = @=$1@>;

};



@q **** (4) assignment --> reg_polygon_vector_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§regular polygon vector assignment>.
\initials{LDF 2004.12.14.}

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG 

@<Define rules@>=
  
@=assignment: reg_polygon_vector_assignment@>
{

  @=$$@> = @=$1@>;

};

@q **** (4) assignment --> cuboid_vector_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§cuboid vector assignment>.
\initials{LDF 2004.12.14.}

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG 

@<Define rules@>=
  
@=assignment: cuboid_vector_assignment@>
{

  @=$$@> = @=$1@>;

};

@q **** (4) assignment --> polyhedron_vector_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§polyhedron vector assignment>.
\initials{LDF 2005.01.14.}

\LOG
\initials{LDF 2005.01.14.}
Added this rule.
\ENDLOG 

@<Define rules@>=
  
@=assignment: polyhedron_vector_assignment@>
{

  @=$$@> = @=$1@>;

};

@q **** (4) assignment --> sphere_vector_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§sphere vector assignment>.
\initials{LDF 2005.10.30.}

\LOG
\initials{LDF 2005.10.30.}
Added this rule.
\ENDLOG 

@<Define rules@>=
  
@=assignment: sphere_vector_assignment@>
{

  @=$$@> = @=$1@>;

};

@q **** (4) assignment --> sphere_development_vector_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§sphere development vector assignment>.
\initials{LDF 2009.11.09.}

\LOG
\initials{LDF 2009.11.09.}
Added this rule.
\ENDLOG 

@<Define rules@>=
  
@=assignment: sphere_development_vector_assignment@>
{

  @=$$@> = @=$1@>;

};

@q **** (4) assignment --> plane_vector_assignment.@>
@*3 \§assignment> $\longrightarrow$ \§plane vector assignment>.
\initials{LDF 2005.10.30.}

\LOG
\initials{LDF 2005.10.30.}
Added this rule.
\ENDLOG 

@<Define rules@>=
  
@=assignment: plane_vector_assignment@>
{

  @=$$@> = @=$1@>;

};



@q **** (4) assignment --> origami_figure_vector_assignment.@>
@*3 \§assignment> $\longrightarrow$ 
\§origami figure vector assignment>.
\initials{LDF 2005.02.03.}

\LOG
\initials{LDF 2005.02.03.}
Added this rule.
\ENDLOG 

@<Define rules@>=
  
@=assignment: origami_figure_vector_assignment@>
{

  @=$$@> = @=$1@>;

};




@q **** (4) assignment --> assignment_command.@>
@*3 \§assignment> $\longrightarrow$ 
\§assignment command>.

\LOG
\initials{LDF 2004.09.05.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=assignment: assignment_command@>
{

      @=$$@> = @=$1@>;
}; 

@q * (1) Type assignments.@> 
@* Type assignments.
\initials{LDF 2005.01.07.}

\LOG
\initials{LDF 2005.01.07.}
Added this section.
\ENDLOG

@q ** (2) Non-vector types.@> 
@*1 Non-vector types.
\initials{LDF 2005.01.07.}

\LOG
\initials{LDF 2005.01.07.}
Added this section.
\ENDLOG


@q *** (3) Non-|Shape| types.@> 
@*1 Non-{\bf Shape} types.
\initials{LDF 2005.01.07.}

\LOG
\initials{LDF 2005.01.07.}
Added this section.
\ENDLOG


@q **** (4) |boolean_assignment|.@>
@*2 \§boolean assignment>. 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> boolean_assignment@>

@q ***** (5) boolean_assignment --> boolean_variable := boolean_expression.  @>
@*3 \§boolean assignment> $\longrightarrow$ \§boolean variable>
\.{:=} \§boolean expression>.  


\LOG
\initials{LDF 2004.07.26.}
Removed code from this rule to |Scan_Parse::assign_simple| in
\filename{scanprse.web}.

\initials{LDF 2004.07.26.}
@:BUG FIX@> BUG FIX:  
Now using |int i| rather than |int* int_ptr|.  This rule
failed because no memory had been allocated for |int_ptr|.

\initials{LDF 2004.10.26.}
Changed |int i| to |int* i|, because I've changed the type of 
|boolean_expression| from |int_value| to |pointer_value| in 
\filename{pblexpr.w}.
\ENDLOG 

 
@<Define rules@>= 
 
@=boolean_assignment: boolean_variable ASSIGN boolean_expression@>
{

  bool* b = static_cast<bool*>(@=$3@>);
  
  Int_Void_Ptr ivp = Scan_Parse::assign_simple<bool>(
                        static_cast<Scanner_Node>(parameter), 
                        "bool", 
                        @=$1@>, 
                        b);

  @=$$@> = ivp.v;
 
};

@q ***** (5) boolean_assignment --> boolean_variable := boolean_assignment.  @>

@*3 \§boolean assignment> $\longrightarrow$ \§boolean variable>
\.{:=} \§boolean assignment>.

This rule makes it possible to chain |boolean_assignments|.
\initials{LDF 2004.05.04.}

\LOG
\initials{LDF 2004.05.04.}  
Added this rule.

\initials{LDF 2004.07.27.} 
Removed code from this rule.  Now calling 
|Scan_Parse::assign_chained<int>|.
\ENDLOG 
  
@ 
@<Define rules@>= 
 
@=boolean_assignment: boolean_variable ASSIGN boolean_assignment@>
{

  Int_Void_Ptr ivp = Scan_Parse::assign_chained<int>(
                        static_cast<Scanner_Node>(parameter),
                        @=$1@>,
                        static_cast<int*>(@=$3@>));

  @=$$@> = ivp.v;

};

@q ***** (5) boolean_assignment --> boolean_variable := bool_point_expression.@>
@*3 \§boolean assignment> $\longrightarrow$ \§boolean variable>
\.{:=} \§bool-point expression>.  

\LOG
\initials{LDF 2004.11.05.}
Added this rule.

\initials{LDF 2004.12.01.}
Now using |bool| instead of |int| for the type of the object 
referenced by |boolean_variables|, |boolean_primaries|,
|secondaries|, |tertiaries|, and |expressions|.
\ENDLOG

@q ****** (6) Definition.@> 
 
@<Define rules@>= 
 
@=boolean_assignment: boolean_variable ASSIGN bool_point_expression@>
{

  Bool_Point* bp = static_cast<Bool_Point*>(@=$3@>);

  int* i; 

       i = new int;

  *i = (bp->b) ? 1 : 0;


  delete bp;

  Int_Void_Ptr ivp = Scan_Parse::assign_simple<int>(
                        static_cast<Scanner_Node>(parameter),
                        "int", 
                        @=$1@>, 
                        i);

  @=$$@> = ivp.v;
 
};




@q **** (4) string_assignment.  @>
@*2 \§string assignment>. 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> string_assignment@>

@q **** (4) string_assignment --> string_variable := string_expression.  @>

@ \§string assignment> $\longrightarrow$ \§string variable>
\.{:=} \§string expression>.

\LOG
\initials{LDF 2004.05.19.}  Added this rule.

\initials{LDF 2004.07.26.}
Removed code from this rule.  
Now calling |Scan_Parse::assign_simple|.
\ENDLOG 

@<Define rules@>= 
 
@=string_assignment: string_variable ASSIGN string_expression@>
{

  string* string_ptr = static_cast<string*>(@=$3@>); 

  Int_Void_Ptr ivp = Scan_Parse::assign_simple<string>(
                                   static_cast<Scanner_Node>(parameter),
                                   "string",
                                   @=$1@>,
                                   string_ptr);

  @=$$@> = ivp.v;

};

@q **** (4) string_assignment --> string_variable := string_assignment.  @>
@*3 \§string assignment> $\longrightarrow$ \§string variable>
\.{:=} \§string assignment>.

This rule makes it possible to chain |string_assignments|.
\initials{LDF 2004.05.19.}

\LOG
\initials{LDF 2004.05.19.}  
Added this rule.

\initials{LDF 2004.07.27.} 
Removed code from this rule.  Now calling 
|Scan_Parse::assign_chained<string>|.
\ENDLOG 
  
@<Define rules@>= 
 
@=string_assignment: string_variable ASSIGN string_assignment@>
{

  Int_Void_Ptr ivp = assign_chained<string>(static_cast<Scanner_Node>(parameter),
                                            @=$1@>,
					    static_cast<string*>(@=$3@>));

  @=$$@> = ivp.v;

};

@q **** (4) pen_assignment.@>
@*2 \§pen assignment>. 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> pen_assignment@>

@q **** (4) pen_assignment --> pen_variable := pen_expression.  @>
@*3 \§pen assignment> $\longrightarrow$ \§pen variable>
\.{:=} \§pen expression>.

\LOG
\initials{LDF 2004.05.21.}  Added this rule.

\initials{LDF 2004.06.07.}  Now calling |create_new<Pen>| rather than
|new Pen|.

\initials{LDF 2004.07.26.}
Removed code from this rule.  
Now calling |Scan_Parse::assign_simple|.
\ENDLOG 

@q **** Definition.@>

@<Define rules@>= 
 
@=pen_assignment: pen_variable ASSIGN pen_expression@>
{

  Pen* pen_ptr = static_cast<Pen*>(@=$3@>); 

  Int_Void_Ptr ivp = assign_simple<Pen>(static_cast<Scanner_Node>(parameter),
                                   "Pen",
                                   @=$1@>,
                                   pen_ptr);

  @=$$@> = ivp.v;


};

@q **** (4) pen_assignment --> pen_variable := pen_assignment.@>
@*3 \§pen assignment> $\longrightarrow$ \§pen variable>
\.{:=} \§pen assignment>.

This rule makes it possible to chain |pen_assignments|.
\initials{LDF 2004.05.21.}

\LOG
\initials{LDF 2004.05.21.}  Added this rule.

\initials{LDF 2004.06.07.}  Now calling |create_new<Pen>| rather than
|new Pen|.

\initials{LDF 2004.07.27.} 
Removed code from this rule.  Now calling 
|Scan_Parse::assign_chained<Pen>|.
\ENDLOG 

@q ***** (5) Definition.@>   

@<Define rules@>= 
 
@=pen_assignment: pen_variable ASSIGN pen_assignment@>
{

  Int_Void_Ptr ivp = assign_chained<Pen>(static_cast<Scanner_Node>(parameter),
                                         @=$1@>,
                                         static_cast<Pen*>(@=$3@>));

  @=$$@> = ivp.v;

  
};

@q **** (4) dash_pattern_assignment.  @>
@*2 \§dash pattern assignment>. 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> dash_pattern_assignment@>

@q **** (4) dash_pattern_assignment --> dash_pattern_variable @>
@q **** (4) := dash_pattern_expression.                       @>

@*3 \§dash pattern assignment> $\longrightarrow$ \§dash pattern variable>
\.{:=} \§dash pattern expression>.  

\LOG
\initials{LDF 2004.06.07.}  
Added this rule.

\initials{LDF 2004.07.26.}
Removed code from this rule.  
Now calling |Scan_Parse::assign_simple|.
\ENDLOG 

@<Define rules@>= 
 
@=dash_pattern_assignment: dash_pattern_variable ASSIGN dash_pattern_expression@>
{

  Dash_Pattern* dash_pattern_ptr = static_cast<Dash_Pattern*>(@=$3@>); 

  Int_Void_Ptr ivp = assign_simple<Dash_Pattern>(static_cast<Scanner_Node>(parameter),
                                   "Dash_Pattern",
                                   @=$1@>,
                                   dash_pattern_ptr);

  @=$$@> = ivp.v;

};

@q **** (4) dash_pattern_assignment --> dash_pattern_variable @>
@q **** (4) := dash_pattern_assignment.                       @>

@*3 \§dash pattern assignment> $\longrightarrow$ \§dash pattern variable>
\.{:=} \§dash pattern assignment>.

This rule makes it possible to chain |dash_pattern_assignments|.
\initials{LDF 2004.06.07.}

\LOG
\initials{LDF 2004.06.07.}  Added this rule.

\initials{LDF 2004.07.27.} 
Removed code from this rule.  Now calling 
|Scan_Parse::assign_chained<Dash_Pattern>|.
\ENDLOG 
  
@q ***** (5) Definition.@> 
 
@<Define rules@>= 
 
@=dash_pattern_assignment: dash_pattern_variable ASSIGN dash_pattern_assignment@>
{

  Int_Void_Ptr ivp = assign_chained<Dash_Pattern>(static_cast<Scanner_Node>(parameter),
                                                  @=$1@>,
                                                  static_cast<Dash_Pattern*>(@=$3@>));

  @=$$@> = ivp.v;

};


@q **** (4) color_assignment.  @>
@*2 \§color assignment>. 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> color_assignment@>

@q **** (4) color_assignment --> color_variable := color_expression.@>

@*3 \§color assignment> $\longrightarrow$ \§color variable>
\.{:=} \§color expression>.

\LOG
\initials{LDF 2004.05.21.}  
Added this rule.

\initials{LDF 2004.07.26.}
Removed code from this rule.  
Now calling |Scan_Parse::assign_simple|.
\ENDLOG 

@<Define rules@>= 
 
@=color_assignment: color_variable ASSIGN color_expression@>
{

  Color* color_ptr = static_cast<Color*>(@=$3@>); 

  Int_Void_Ptr ivp = assign_simple<Color>(static_cast<Scanner_Node>(parameter),
                                   "Color",
                                   @=$1@>,
                                   color_ptr);

  @=$$@> = ivp.v;

};

@q **** (4) color_assignment --> color_variable := color_assignment.@>
@*3 \§color assignment> $\longrightarrow$ \§color variable>
\.{:=} \§color assignment>.

This rule makes it possible to chain |color_assignments|.
\initials{LDF 2004.05.21.}

\LOG
\initials{LDF 2004.05.21.}  
Added this rule.

\initials{LDF 2004.07.27.}
Removed code from this rule.  Now calling 
|Scan_Parse::assign_chained<Color>|.
\ENDLOG 
  
@q ***** (5) Definition.@> 
 
@<Define rules@>= 
 
@=color_assignment: color_variable ASSIGN color_assignment@>
{

  Int_Void_Ptr ivp = assign_chained<Color>(static_cast<Scanner_Node>(parameter),
                                           @=$1@>,
                                           static_cast<Color*>(@=$3@>));

  @=$$@> = ivp.v;

};

@q **** (4) numeric_assignment.  @>
@*2 \§numeric assignment>. 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> numeric_assignment@>

@q **** (4) numeric_assignment --> numeric_variable := numeric_expression.@>

@*3 \§numeric assignment> $\longrightarrow$ \§numeric variable>
\.{:=} \§numeric expression>.  


\LOG
\initials{LDF 2004.07.26.}
Removed code from this rule.  
Now calling |Scan_Parse::assign_simple|.

\initials{LDF 2004.10.26.}
Now calling |Scan_Parse::assign_real| instead of
|Scan_Parse::assign_simple|.

\initials{LDF 2004.10.27.}
Now calling |Scan_Parse::assign_simple<real>| again 
instead of |Scan_Parse::assign_real|.
\ENDLOG 

@<Define rules@>= 
 
@=numeric_assignment: numeric_variable ASSIGN numeric_expression@>@/
{


  real r = @=$3@>; 

  Int_Void_Ptr ivp = assign_simple<real>(static_cast<Scanner_Node>(parameter),
                                         "real", 
                                         @=$1@>,
                                         &r,
                                         false);    
  @=$$@> = ivp.v;

};

@q **** (4) numeric_assignment --> numeric_variable := numeric_vector_expression.@>

@*3 \§numeric assignment> $\longrightarrow$ \§numeric variable>
\.{:=} \§numeric vector expression>.  
\initials{LDF 2006.01.23.}

If the \§numeric vector expression> is non-null and has at least one element, 
its first element is used for the assignment.  Otherwise, |INVALID_REAL| is used.
\initials{LDF 2006.01.23.}

\LOG
\initials{LDF 2006.01.23.}
Added this rule.
\ENDLOG

@<Define rules@>= 
 
@=numeric_assignment: numeric_variable ASSIGN numeric_vector_expression@>@/
{

    real r;

    Pointer_Vector<real>* pv = static_cast<Pointer_Vector<real>*>(@=$3@>);

    if (pv == static_cast<Pointer_Vector<real>*>(0)  || pv->ctr <= 0)
       {
           r = INVALID_REAL;
       }
    else
       {
           r = *(pv->v[0]);
           delete pv;
           pv = 0;
       }


    Int_Void_Ptr ivp = assign_simple<real>(static_cast<Scanner_Node>(parameter),
                                         "real", 
                                         @=$1@>,
                                         &r,
                                         false);    
    @=$$@> = ivp.v;

};

@q **** (4) numeric_assignment --> numeric_variable := numeric_assignment.  @>

@*3 \§numeric assignment> $\longrightarrow$ \§numeric variable>
\.{:=} \§numeric assignment>.

This rule makes it possible to chain |numeric_assignments|.
\initials{LDF 2004.05.04.}

\LOG
\initials{LDF 2004.05.04.}  Added this rule.

\initials{LDF 2004.07.27.}
Removed code from this rule.  Now calling 
|Scan_Parse::assign_chained<real>|.
\ENDLOG 
 
@q ***** (5) Definition.@> 
 
@<Define rules@>= 
 
@=numeric_assignment: numeric_variable ASSIGN numeric_assignment@>
{

  Int_Void_Ptr ivp = assign_chained<real>(static_cast<Scanner_Node>(parameter),
                                          @=$1@>,
                                          static_cast<real*>(@=$3@>));
  @=$$@> = ivp.v;

};

@q **** (4) ulong_long_assignment.  @>
@*2 \§ulong long assignment>. 
\initials{LDF 2005.12.05.}

\LOG
\initials{LDF 2005.12.05.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> ulong_long_assignment@>

@q **** (4) ulong_long_assignment --> ulong_long_variable := ulong_long_expression.@>

@*3 \§ulong long assignment> $\longrightarrow$ \§ulong long variable>
\.{:=} \§ulong long expression>.  
\initials{LDF 2005.12.05.}

\LOG
\initials{LDF 2005.12.05.}
Added this rule.
\ENDLOG

@<Define rules@>= 
 
@=ulong_long_assignment: ulong_long_variable ASSIGN ulong_long_expression@>@/
{


  unsigned long long r = @=$3@>; 

  Int_Void_Ptr ivp = assign_simple<unsigned long long>(static_cast<Scanner_Node>(parameter),
                                         "unsigned long long", 
                                         @=$1@>,
                                         &r,
                                         false);    
  @=$$@> = ivp.v;

};

@q **** (4) ulong_long_assignment --> ulong_long_variable := ulong_long_assignment.  @>

@*3 \§ulong long assignment> $\longrightarrow$ \§ulong long variable>
\.{:=} \§ulong long assignment>.

This rule makes it possible to chain |ulong_long_assignments|.
\initials{LDF 2005.12.05.}

\LOG
\initials{LDF 2005.12.05.}
Added this rule.
\ENDLOG
 
@q ***** (5) Definition.@> 
 
@<Define rules@>= 
 
@=ulong_long_assignment: ulong_long_variable ASSIGN ulong_long_assignment@>
{

  Int_Void_Ptr ivp = assign_chained<unsigned long long>(static_cast<Scanner_Node>(parameter),
                                          @=$1@>,
                                          static_cast<unsigned long long*>(@=$3@>));
  @=$$@> = ivp.v;

};

@q **** (4) transform_assignment.  @>
@*2 \§transform assignment>. 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> transform_assignment@>@/

@q **** (4) transform_assignment --> transform_variable @>   
@q **** (4) := transform_expression.                    @>

@*3 \§transform assignment> $\longrightarrow$ \§transform variable> 
\.{:=} \§transform expression>. 

\LOG
\initials{LDF 2004.05.06.}  
Added this rule.

\initials{LDF 2004.07.27.}
Removed code from this rule.  Now calling 
|Scan_Parse::assign_simple_1<Transform>|.

\initials{LDF 2004.07.28.}
Now calling |Scan_Parse::assign_simple| rather than 
|Scan_Parse::assign_simple_1<Transform>|, since I've gotten rid of 
the latter function. 

\initials{LDF 2004.11.05.}
@:BUG FIX@> BUG FIX:  No longer deleting |transform_expression|.
\ENDLOG 

@<Define rules@>= 
 
@=transform_assignment: transform_variable ASSIGN transform_expression@>
{

  Transform* t = static_cast<Transform*>(@=$3@>);


  Int_Void_Ptr ivp = Scan_Parse::assign_simple<Transform>(
                                   static_cast<Scanner_Node>(parameter),
                                   "Transform",
                                   @=$1@>,
                                   t);

  @=$$@> = ivp.v;

};

@q **** (4) picture_assignment.@>
@*2 \§picture assignment>. 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> picture_assignment@>

@q **** (4) picture_assignment --> picture_variable := picture_expression.@>
@*3 \§picture assignment> $\longrightarrow$ \§picture variable>
\.{:=} \§picture expression>.

\LOG
\initials{LDF 2004.05.21.}  
Added this rule.

\initials{LDF 2004.06.30.}
Changed the code in this rule to account for the fact that 
|picture_expression| is now a |Id_Map_Entry_Node|, cast to |void*|.
Formerly, it was a |Picture*|, cast to |void*|.

\initials{LDF 2004.06.30.}
Now locking and unlocking |entry->mutex|, |entry_0->mutex|, |s->mutex|, 
and |t->mutex|.

\initials{LDF 2004.08.08.}
Removed code from this rule and put it in 
|Scan_Parse::assign_picture_simple|.

\initials{LDF 2004.12.02.}
Rewrote this rule so that |picture_expression| is copied and the copy 
passed to |Scan_Parse::assign_picture_simple|.

\initials{LDF 2004.12.03.}
Reversed the change made in the last version, i.e.,
|picture_expression| itself is again passed to 
|Scan_Parse::assign_picture_simple|. 
\ENDLOG 

@q ***** (5) Definition.@> 

@<Define rules@>=
 
@=picture_assignment: picture_variable ASSIGN picture_expression@>
{

  Int_Void_Ptr ivp
    = assign_picture_simple(static_cast<Scanner_Node>(parameter),
                            static_cast<Id_Map_Entry_Node>($1),
                            static_cast<Id_Map_Entry_Node>($3));

  @=$$@> = static_cast<void*>(ivp.v); 

};

@q **** (4) picture_assignment --> picture_variable := picture_assignment.@>
@*3 \§picture assignment> $\longrightarrow$ \§picture variable>
\.{:=} \§picture assignment>.

This rule makes it possible to chain |picture_assignments|.
\initials{LDF 2004.05.21.}

\LOG
\initials{LDF 2004.05.21.}  
Added this rule.

\initials{LDF 2004.06.30.}
Changed the code in this rule to account for the fact that 
|picture_assignment| is now a |Id_Map_Entry_Node|, cast to |void*|.
Formerly, it was a |Picture*|, cast to |void*|.

\initials{LDF 2004.06.30.}
Now locking and unlocking |entry->mutex|, |entry_0->mutex|, |s->mutex|, 
and |t->mutex|.

\initials{LDF 2004.08.13.}
Removed code from this rule to the function 
|Scan_Parse::assign_picture_chained| in \filename{scanprse.web}.
\ENDLOG 

@q ***** (5) Definition.@>   
 
@<Define rules@>= 
 
@=picture_assignment: picture_variable ASSIGN picture_assignment@>
{

  Int_Void_Ptr ivp
    = assign_picture_chained(static_cast<Scanner_Node>(parameter),
                             static_cast<Id_Map_Entry_Node>($1),
                             static_cast<Id_Map_Entry_Node>($3));

  @=$$@> = static_cast<void*>(ivp.v); 

};

@q **** (4) focus_assignment.  @>
@*2 \§focus assignment>. 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> focus_assignment@>

@q **** (4) focus_assignment --> focus_variable := focus_expression.@>

@*3 \§focus assignment> $\longrightarrow$ \§focus variable>
\.{:=} \§focus expression>.  

\LOG
\initials{LDF 2004.06.08.}  Added this rule.

\initials{LDF 2004.07.28.}
Removed code from this rule.  
Now calling |Scan_Parse::assign_simple|.
\ENDLOG 

@<Define rules@>= 
 
@=focus_assignment: focus_variable ASSIGN focus_expression@>
{

  Focus* f = static_cast<Focus*>(@=$3@>);

  Int_Void_Ptr ivp = assign_simple<Focus>(static_cast<Scanner_Node>(parameter),
                                   "Focus",
                                   @=$1@>,
                                   f);

  @=$$@> = ivp.v;

};

@q **** (4) focus_assignment --> focus_variable := focus_assignment.  @>

@*3 \§focus assignment> $\longrightarrow$ \§focus variable>
\.{:=} \§focus assignment>.

This rule makes it possible to chain |focus_assignments|.
\initials{LDF 2004.06.08.}

\LOG
\initials{LDF 2004.06.08.}  Added this rule.

\initials{LDF 2004.08.08.}
Now using |assign_chained| in this rule.
\ENDLOG 
  
@ 
@<Define rules@>= 
 
@=focus_assignment: focus_variable ASSIGN focus_assignment@>
{

    Int_Void_Ptr ivp = assign_chained<Focus>(static_cast<Scanner_Node>(parameter),
                                             @=$1@>,
                                             static_cast<Focus*>(@=$3@>));

    @=$$@> = ivp.v;

};

@q **** (4) macro_assignment.  @>
@*2 \§macro assignment>. 
\initials{LDF 2004.12.30.}

\LOG
\initials{LDF 2004.12.30.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> macro_assignment@>

@q **** (4) macro_assignment --> macro_variable := macro_variable.@>

@*3 \§macro assignment> $\longrightarrow$ \§macro variable>
\.{ASSIGN} \§macro expression>.  
\initials{LDF 2004.12.30.}

\LOG
\initials{LDF 2004.12.30.}
Added this rule.
\ENDLOG 

@q ***** (5) Definition.@> 

@<Define rules@>= 
 
@=macro_assignment: macro_variable ASSIGN macro_variable@>
{

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);


@q ****** (6) Error handling:  |entry == static_cast<Id_Map_Entry_Node>(0)|.@> 

@ Error handling:  |entry == static_cast<Id_Map_Entry_Node>(0)|.
\initials{LDF 2004.12.30.}

@<Define rules@>=

   if (entry == static_cast<Id_Map_Entry_Node>(0))
      {

          @=$$@> = static_cast<void*>(0);
        
      }  /* |if (entry == static_cast<Id_Map_Entry_Node>(0))| */        

@q ****** (6) |entry != static_cast<Id_Map_Entry_Node>(0)|.@> 

@ |entry != static_cast<Id_Map_Entry_Node>(0)|.
\initials{LDF 2004.12.30.}

@<Define rules@>=
 
   else  /* |entry != static_cast<Id_Map_Entry_Node>(0)|  */
      {
               
         Id_Map_Entry_Node entry_0 = static_cast<Id_Map_Entry_Node>(@=$3@>);  
 
   
@q ******* (7) Error handling:                                                         @>
@q ******* (7) |entry_0 == static_cast<Id_Map_Entry_Node>(0) || entry_0->object == 0|. @>
@ Error handling:  |entry_0 == 0 || entry_0->object == 0|.
\initials{LDF 2004.12.30.}

@<Define rules@>=

         if (   entry_0 == static_cast<Id_Map_Entry_Node>(0) 
             || entry_0->object == static_cast<Id_Map_Entry_Node>(0))
            {

                @=$$@> = static_cast<void*>(0);

            }  /* |if| */


@q ******* (7) |entry_0 != 0 && entry_0->object != 0|.@> 

@ |entry_0 != 0 && entry_0->object != 0|.
\initials{LDF 2004.12.30.}

@<Define rules@>=

      else  /*  |entry_0 != 0 && entry_0->object != 0|  */
         {

            Definition_Info_Node d;

@q ******** (8) |entry->object == 0|.@> 

@ |entry->object == 0|.
\initials{LDF 2004.12.30.}

@<Define rules@>=

            if (entry->object == static_cast<Id_Map_Entry_Node>(0))
               {


                  d = new Definition_Info_Type;

                  entry->object = static_cast<void*>(d); 

               }  /* |if (entry->object == static_cast<Id_Map_Entry_Node>(0))|  */


@q ******** (8) |entry->object != static_cast<void*>(0)|.@> 

@ |entry->object == static_cast<void*>(0)|.
\initials{LDF 2004.12.30.}

@<Define rules@>=

            else  /* |entry->object != static_cast<void*>(0)|  */
               {
                   d = static_cast<Definition_Info_Node>(entry->object);

               }  /* |else| (|entry->object != static_cast<void*>(0)|)  */

@q ******** (8) @> 


            *d = *(static_cast<Definition_Info_Node>(entry_0->object));  

            @=$$@> = static_cast<void*>(entry); 

         }  /* |else| ( |entry_0 != 0 && entry_0->object != 0|)  */


@q ******* (7) @> 


   }  /* |else| (|entry != 0|)  */

@q ****** (6).@> 

};






@q *** (3) Shape types.@> 
@*1 {\bf Shape} types.
\initials{LDF 2005.01.07.}

\LOG
\initials{LDF 2005.01.07.}
Added this section.
\ENDLOG



@q **** (4) point_assignment.  @>
@*2 \§point assignment>. 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> point_assignment@>@/

@q ***** (5) point_assignment --> point_variable := point_expression.@>
@*3 \§point assignment> $\longrightarrow$ \§point variable> \.{:=} 
\§point expression>. 

\LOG
\initials{LDF 2004.05.04.}  Added this rule.

\initials{LDF 2004.07.27.}
Removed code from this rule.  Now calling 
|Scan_Parse::assign_simple_1<Point>|.

\initials{LDF 2004.07.28.}
Now calling |Scan_Parse::assign_simple| rather than 
|Scan_Parse::assign_simple_1<Transform>|, since I've gotten rid of 
the latter function. 
\ENDLOG 


@<Define rules@>= 
 
@=point_assignment: point_variable ASSIGN point_expression@>
{

  Point* p = static_cast<Point*>(@=$3@>); 
  
  Int_Void_Ptr ivp = assign_simple<Point>(static_cast<Scanner_Node>(parameter),
                                   "Point",
                                   @=$1@>,
                                   p);

  @=$$@> = ivp.v;

};


@q ***** (5) point_assignment --> point_variable ASSIGN bool_point_expression.@>
@*3 \§point assignment> $\longrightarrow$ \§point variable> \.{ASSIGN} 
\§bool-point expression>. 

\LOG
\initials{LDF 2004.11.05.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>= 
 
@=point_assignment: point_variable ASSIGN bool_point_expression@>
{

  Bool_Point* bp = static_cast<Bool_Point*>(@=$3@>); 
  
  Point* p;

  p = create_new<Point>(bp->pt);

  Int_Void_Ptr ivp = assign_simple<Point>(static_cast<Scanner_Node>(parameter),
                                          "Point",
                                          @=$1@>,
                                          p);

  @=$$@> = ivp.v;

};

@q ***** (5) point_assignment --> point_variable := numeric_expression.@>

@*3 \§point assignment> $\longrightarrow$ \§point variable> 
\.{:=} \§numeric expression>. 

We can't use |@<Common code for variable assignments@>| in this
rule, because that section assumes that the |expression| on the
right-hand side of the assignment is of the same type as the
|variable| on the left-hand side.   
\initials{LDF 2004.05.05.}

\LOG
\initials{LDF 2004.05.03.}  
Added this rule.

\initials{LDF 2004.05.05.}  
Added error handling for the cases that |entry == 0|.

\initials{LDF 2004.08.13.}
Removed code from this rule to the function 
|Scan_Parse::assign_point_numeric| in \filename{scanprse.web}.
\ENDLOG 

@q ***** (5)  @>
@<Define rules@>=
 
@=point_assignment: point_variable ASSIGN numeric_expression@>
{

  Int_Void_Ptr ivp
    = assign_point_numeric(static_cast<Scanner_Node>(parameter),
                           static_cast<Id_Map_Entry_Node>(@=$1@>),
                           @=$3@>);

  @=$$@> = ivp.v;

};


@q ***** (5) point_assignment --> point_variable := point_assignment.  @>

@*3 \§point assignment> $\longrightarrow$ \§point variable>
\.{:=} \§point assignment>.

This rule makes it possible to chain |point_assignments|.
\initials{LDF 2004.08.08.}

\LOG
\initials{LDF 2004.08.08.}  
Tried to add this rule.  I wasn't able to get it to work, though.

\initials{LDF 2004.10.25.}
Got this rule to work.
\ENDLOG 

@q ***** (5) Definition.@> 
 
@<Define rules@>=
@=point_assignment: point_variable ASSIGN point_assignment@>
{


    Int_Void_Ptr ivp = assign_chained<Point>(static_cast<Scanner_Node>(parameter),
                                             @=$1@>,
                                             static_cast<Point*>(@=$3@>));

    @=$$@> = ivp.v;


};




@q **** (4) bool_point_assignment.@>
@*2 \§bool-point assignment>. 
\initials{LDF 2004.09.01.}

\LOG
\initials{LDF 2004.09.01.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> bool_point_assignment@>

@q ***** (5) bool_point_assignment --> bool_point_variable @>
@q ***** (5) ASSIGN bool_point_expression.                 @>
                                                            
@*3 \§bool-point assignment> $\longrightarrow$ 
\§bool-point variable> \.{ASSIGN} \§bool-point expression>.

\LOG
\initials{LDF 2004.09.01.}
Added this rule.

\initials{LDF 2004.11.03.}
Changed this rule from 
``\§bool-point assignment> $\longrightarrow$ 
\§bool-point variable> \.{ASSIGN} \.{LEFT\_PARENTHESIS} 
\§boolean expression> \.{COMMA} \§point expression>
\.{RIGHT\_PARENTHESIS}''
to 
``\§bool-point assignment> $\longrightarrow$ 
\§bool-point variable> \.{ASSIGN} \§bool-point expression>''.
\ENDLOG

@q ****** (6) Definition.@> 

 
@<Define rules@>=
 
@=bool_point_assignment: bool_point_variable ASSIGN @>@/
@=bool_point_expression@>@/
{


  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);


  
@q ******* (7) Error handling for the case that |entry == 0|.@>

@ Error handling for the case that |entry == 0 |.
\initials{LDF 2004.09.01.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == static_cast<Id_Map_Entry_Node>(0))|  */




@q ******* (7) |entry != static_cast<Id_Map_Entry_Node>(0)|.@>   

@ |entry != 0|.
\initials{LDF 2004.09.01.}

@<Define rules@>=

  else /* |entry != 0|  */
    {


         Bool_Point* bp = static_cast<Bool_Point*>(entry->object);

         Bool_Point* bpe = static_cast<Bool_Point*>(@=$3@>); 

@q ******** (8) |bp == 0|.@> 

@ |bp == 0|.
\initials{LDF 2004.11.03.}

@<Define rules@>=

  if (bp == static_cast<Bool_Point*>(0))
    {
      
      bp = bpe;
      
      @=$$@> = entry->object = static_cast<void*>(bp);
 

    } /* |if (bp == static_cast<Bool_Point*>(0))|  */

@q ******** (8) |bp != static_cast<Bool_Point*>(0)|.@> 

@ |bp != static_cast<Bool_Point*>(0)|.
\initials{LDF 2004.11.03.}

@<Define rules@>=

   else  /* |bp != static_cast<Bool_Point*>(0)|  */
     {
        bp->b  = bpe->b,
        bp->pt = bpe->pt;

        delete bpe;

        @=$$@> = entry->object;

     }   /* |else| (|bp != static_cast<Bool_Point*>(0)|)  */

   } /* |else| (|entry != static_cast<Id_Map_Entry_Node>(0)|)  */

};

@q ***** (5) bool_point_assignment --> bool_point_variable @>
@q ***** (5) := boolean_expression.                        @>
                                                            
@*3 \§bool-point assignment> $\longrightarrow$ 
\§bool-point variable> \.{ASSIGN} \§boolean expression>.

\LOG
\initials{LDF 2004.11.05.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

 
@<Define rules@>=
 
@=bool_point_assignment: bool_point_variable ASSIGN @>@/
@=boolean_expression@>@/
{


  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

  bool* b = static_cast<bool*>(@=$3@>); 
  
@q ****** (6) Error handling for the case that |entry == 0|.@>

@ Error handling for the case that |entry == 0 |.
\initials{LDF 2004.09.01.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      delete b;

      @=$$@> = static_cast<void*>(0);

    } /* |if (entry == 0)|  */


@q ****** (6) |entry != 0|.@>   

@ |entry != 0|.
\initials{LDF 2004.09.01.}

@<Define rules@>=

  else /* |entry != 0|  */
    {

 
        Bool_Point* bp = static_cast<Bool_Point*>(entry->object);



@q ******* (7) |bp == 0|.@> 

@ |bp == 0|.
\initials{LDF 2004.11.03.}

@<Define rules@>=

  if (bp == static_cast<Bool_Point*>(0))
    {
      
        bp = new Bool_Point;


    } /* |if (bp == 0)|  */

@q ******* (7) @> 

     bp->b = *b;
      
     delete b;

     @=$$@> = entry->object = static_cast<void*>(bp); 

   } /* |else| (|entry != 0|)  */

@q ****** (6).@> 

};

@q ***** (5) bool_point_assignment --> bool_point_variable @>
@q ***** (5) := point_expression.                          @>
                                                            
@*3 \§bool-point assignment> $\longrightarrow$ 
\§bool-point variable> \.{ASSIGN} \§point expression>.

\LOG
\initials{LDF 2004.11.05.}
Added this rule.

\initials{LDF 2004.12.01.}
Now using |bool| instead of |int| for the type of the object 
referenced by |boolean_variables|, |boolean_primaries|,
|secondaries|, |tertiaries|, and |expressions|.
\ENDLOG

@q ***** (5) Definition.@> 

 
@<Define rules@>=
 
@=bool_point_assignment: bool_point_variable ASSIGN @>@/
@=point_expression@>@/
{

  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

  Point* p = static_cast<Point*>(@=$3@>); 
  
@q ****** (6) Error handling for the case that |entry == 0|.@>

@ Error handling for the case that |entry == 0 |.
\initials{LDF 2004.09.01.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {


      delete p;

      @=$$@> = static_cast<void*>(0);

    } /* |if (entry == 0)|  */


@q ****** (6) |entry != 0|.@>   

@ |entry != 0|.
\initials{LDF 2004.09.01.}

@<Define rules@>=

  else /* |entry != 0|  */
    {

        Bool_Point* bp = static_cast<Bool_Point*>(entry->object);



@q ******* (7) |bp == 0|.@> 

@ |bp == 0|.
\initials{LDF 2004.11.03.}

@<Define rules@>=

  if (bp == static_cast<Bool_Point*>(0))
    {
      
       bp = new Bool_Point;


    } /* |if (bp == 0)|  */

@q ******* (7) @> 

     bp->pt = *p;
      
     delete p;

     @=$$@> = entry->object = static_cast<void*>(bp); 

   } /* |else| (|entry != 0|)  */

@q ****** (6).@> 

};

@q **** (4) path_assignment.  @>
@*3 \§path assignment>. 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> path_assignment@>@/

@q ***** (5) path_assignment --> path_variable := path_expression.@>   

@*4 \§path assignment> $\longrightarrow$ \§path variable> 
\.{ASSIGN} \§path expression>. 

\LOG
\initials{LDF 2004.05.06.}
Added this rule.

\initials{LDF 2004.05.13.}  
Replaced the dummy code in this rule with 
code that works. 

\initials{LDF 2004.07.28.}
Removed code from this rule.  
Now calling |Scan_Parse::assign_simple|.

\initials{LDF 2004.12.10.}
Changed \§path expression> to \§path-like expression>. 

\initials{LDF 2005.10.24.}
Changed \§path-like expression> back to \§path expression>.
Removed all debugging code. 
\ENDLOG 

@<Define rules@>= 
 
@=path_assignment: path_variable ASSIGN path_expression@>
{
  
  Path* p = static_cast<Path*>(@=$3@>);

  Int_Void_Ptr ivp = assign_simple<Path>(static_cast<Scanner_Node>(parameter),
                                         "Path",
                                         @=$1@>,
                                         p);

  @=$$@> = ivp.v;

};

@q ***** (5) path_assignment --> path_variable := circle_expression.@>   

@*4 \§path assignment> $\longrightarrow$ \§path variable> 
\.{ASSIGN} \§circle expression>. 

\LOG
\initials{LDF 2005.10.24.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
 
@=path_assignment: path_variable ASSIGN circle_expression@>
{
  
  Path* p = static_cast<Path*>(@=$3@>);

  Int_Void_Ptr ivp = assign_simple<Path>(static_cast<Scanner_Node>(parameter),
                                         "Path",
                                         @=$1@>,
                                         p);

  @=$$@> = ivp.v;

};

@q ***** (5) path_assignment --> path_variable := ellipse_expression.@>   

@*4 \§path assignment> $\longrightarrow$ \§path variable> 
\.{:=} \§ellipse expression>. 

\LOG
\initials{LDF 2005.10.24.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
 
@=path_assignment: path_variable ASSIGN ellipse_expression@>
{
  
  Path* p = static_cast<Path*>(@=$3@>);

  Int_Void_Ptr ivp = assign_simple<Path>(static_cast<Scanner_Node>(parameter),
                                         "Path",
                                         @=$1@>,
                                         p);

  @=$$@> = ivp.v;

};


@q ***** (5) path_assignment --> path_variable := rectangle_expression.@>   

@*4 \§path assignment> $\longrightarrow$ \§path variable> 
\.{:=} \§rectangle expression>. 

\LOG
\initials{LDF 2005.10.24.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
 
@=path_assignment: path_variable ASSIGN rectangle_expression@>
{
  
  Path* p = static_cast<Path*>(@=$3@>);

  Int_Void_Ptr ivp = assign_simple<Path>(static_cast<Scanner_Node>(parameter),
                                         "Path",
                                         @=$1@>,
                                         p);

  @=$$@> = ivp.v;

};

@q ***** (5) path_assignment --> path_variable := triangle_expression.@>   

@*4 \§path assignment> $\longrightarrow$ \§path variable> 
\.{:=} \§triangle expression>. 

\LOG
\initials{LDF 2005.10.24.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
 
@=path_assignment: path_variable ASSIGN triangle_expression@>
{
  
  Path* p = static_cast<Path*>(@=$3@>);

  Int_Void_Ptr ivp = assign_simple<Path>(static_cast<Scanner_Node>(parameter),
                                         "Path",
                                         @=$1@>,
                                         p);

  @=$$@> = ivp.v;

};


@q ***** (5) path_assignment --> path_variable := polygon_expression.@>   

@*4 \§path assignment> $\longrightarrow$ \§path variable> 
\.{:=} \§polygon expression>. 

\LOG
\initials{LDF 2005.10.24.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
 
@=path_assignment: path_variable ASSIGN polygon_expression@>
{
  
  Path* p = static_cast<Path*>(@=$3@>);

  Int_Void_Ptr ivp = assign_simple<Path>(static_cast<Scanner_Node>(parameter),
                                         "Path",
                                         @=$1@>,
                                         p);

  @=$$@> = ivp.v;

};


@q ***** (5) path_assignment --> path_variable := reg_polygon_expression.@>   

@*4 \§path assignment> $\longrightarrow$ \§path variable> 
\.{:=} \§regular polygon expression>. 

\LOG
\initials{LDF 2005.10.24.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
 
@=path_assignment: path_variable ASSIGN reg_polygon_expression@>
{
  
  Path* p = static_cast<Path*>(@=$3@>);

  Int_Void_Ptr ivp = assign_simple<Path>(static_cast<Scanner_Node>(parameter),
                                         "Path",
                                         @=$1@>,
                                         p);

  @=$$@> = ivp.v;

};

@q ***** (5) path_assignment --> path_variable := path_assignment.@>

@*3 \§path assignment> $\longrightarrow$ \§path variable>
\.{:=} \§path assignment>.

This rule makes it possible to chain |path_assignments|.
\initials{LDF 2004.08.08.}

\LOG
\initials{LDF 2004.08.08.}  
Tried to add this rule, but I couldn't get it to work.

\initials{LDF 2004.10.27.}
Got this rule to work.
\ENDLOG 

@<Define rules@>=
@=path_assignment: path_variable ASSIGN path_assignment@>@/
{

    Int_Void_Ptr ivp = assign_chained<Path>(static_cast<Scanner_Node>(parameter),
                                            @=$1@>,
                                            static_cast<Path*>(@=$3@>));

    @=$$@> = ivp.v;

};

@q **** (4) ellipse_assignment.  @>
@*2 \§ellipse assignment>. 

\LOG
\initials{LDF 2004.06.29.}  
Added this section.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> ellipse_assignment@>@/

@q ***** (5) ellipse_assignment --> ellipse_variable @>
@q ***** (5) := ellipse_expression.                  @>   

@*3 \§ellipse assignment> $\longrightarrow$ \§ellipse variable> 
\.{ASSIGN} \§ellipse expression>. 

\LOG
\initials{LDF 2004.06.29.}
Added this rule.

\initials{LDF 2004.06.29.}
Changed |ellipse_expression| to |ellipse_like_expression|.

\initials{LDF 2004.07.28.}
Removed code from this rule.  
Now calling |Scan_Parse::assign_simple|.

\initials{LDF 2005.10.24.}
Changed |ellipse_like_expression| back to |ellipse_expression|.
Removed all debugging code.
\ENDLOG

@<Define rules@>= 
 
@=ellipse_assignment: ellipse_variable ASSIGN ellipse_expression@>
{

  Ellipse* f = static_cast<Ellipse*>(@=$3@>);

  Int_Void_Ptr ivp = assign_simple<Ellipse>(static_cast<Scanner_Node>(parameter),
                                            "Ellipse",
                                            @=$1@>,
                                            f);

  @=$$@> = ivp.v;

};

@q ***** (5) ellipse_assignment --> ellipse_variable @>
@q ***** (5) := circle_expression.                  @>   

@*3 \§ellipse assignment> $\longrightarrow$ \§ellipse variable> 
\.{ASSIGN} \§circle expression>. 

\LOG
\initials{LDF 2005.10.24.}
Added this rule.
\ENDLOG

@<Define rules@>= 
 
@=ellipse_assignment: ellipse_variable ASSIGN circle_expression@>
{

  Ellipse* f = static_cast<Ellipse*>(@=$3@>);

  Int_Void_Ptr ivp = assign_simple<Ellipse>(static_cast<Scanner_Node>(parameter),
                                            "Ellipse",
                                            @=$1@>,
                                            f);

  @=$$@> = ivp.v;

};

@q **** (4) circle_assignment.@>
@*3 \§circle assignment>. 
\initials{LDF 2004.06.17.}  

\LOG
\initials{LDF 2004.06.17.}  
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> circle_assignment@>@/

@q ***** (5) circle_assignment --> circle_variable := circle_expression.@>   
@*4 \§circle assignment> $\longrightarrow$ \§circle variable> 
\.{:=} \§circle expression>. 
\initials{LDF 2004.06.17.}

\LOG
\initials{LDF 2004.06.17.}
Added this rule.

\initials{LDF 2004.07.28.}
Removed code from this rule.  
Now calling |Scan_Parse::assign_simple|.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>= 
 
@=circle_assignment: circle_variable ASSIGN circle_expression@>
{

  Circle* f = static_cast<Circle*>(@=$3@>);

  Int_Void_Ptr ivp = assign_simple<Circle>(static_cast<Scanner_Node>(parameter),
                                   "Circle",
                                   @=$1@>,
                                   f);

  @=$$@> = ivp.v;

};

@q **** (4) parabola_assignment.@>
@*3 \§parabola assignment>. 
\initials{LDF 2005.11.07.}

\LOG
\initials{LDF 2005.11.07.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> parabola_assignment@>@/

@q ***** (5) parabola_assignment --> parabola_variable := parabola_expression.@>   
@*4 \§parabola assignment> $\longrightarrow$ \§parabola variable> 
\.{:=} \§parabola expression>. 
\initials{LDF 2005.11.07.}

\LOG
\initials{LDF 2005.11.07.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>= 
 
@=parabola_assignment: parabola_variable ASSIGN parabola_expression@>
{

  Parabola* f = static_cast<Parabola*>(@=$3@>);

  Int_Void_Ptr ivp = Scan_Parse::assign_simple<Parabola>(static_cast<Scanner_Node>(parameter),
                                                      "Parabola",
                                                      @=$1@>,
                                                      f);

  @=$$@> = ivp.v;

};

@q **** (4) hyperbola_assignment.@>
@*3 \§hyperbola assignment>. 
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> hyperbola_assignment@>@/

@q ***** (5) hyperbola_assignment --> hyperbola_variable := hyperbola_expression.@>   
@*4 \§hyperbola assignment> $\longrightarrow$ \§hyperbola variable> 
\.{:=} \§hyperbola expression>. 
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>= 
 
@=hyperbola_assignment: hyperbola_variable ASSIGN hyperbola_expression@>
{

  Hyperbola* f = static_cast<Hyperbola*>(@=$3@>);

  Int_Void_Ptr ivp = Scan_Parse::assign_simple<Hyperbola>(static_cast<Scanner_Node>(parameter),
                                                      "Hyperbola",
                                                      @=$1@>,
                                                      f);

  @=$$@> = ivp.v;

};

@q **** (4) conic_section_lattice_assignment.@>
@*3 \§conic section lattice assignment>. 
\initials{LDF 2007.07.29.}

\LOG
\initials{LDF 2007.07.29.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> conic_section_lattice_assignment@>@/

@q ***** (5) conic_section_lattice_assignment --> conic_section_lattice_variable @>
@q ***** (5) ASSIGN conic_section_lattice_expression.                            @>   
@*4 \§conic section lattice assignment> $\longrightarrow$ 
\§conic section lattice variable> \.{ASSIGN} \§conic section lattice expression>. 
\initials{LDF 2007.07.29.}

\LOG
\initials{LDF 2007.07.29.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>= 
 
@=conic_section_lattice_assignment: conic_section_lattice_variable ASSIGN @>@/
@=conic_section_lattice_expression@>@/
{

  Conic_Section_Lattice* f = static_cast<Conic_Section_Lattice*>(@=$3@>);

  Int_Void_Ptr ivp = Scan_Parse::assign_simple<Conic_Section_Lattice>(
                                    static_cast<Scanner_Node>(parameter),
                                    "Conic_Section_Lattice",
                                    @=$1@>,
                                    f);

  @=$$@> = ivp.v;

};


@q **** (4) arc_assignment.@>
@*3 \§arc assignment>. 
\initials{LDF 2007.10.13.}

\LOG
\initials{LDF 2007.10.13.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> arc_assignment@>@/

@q ***** (5) arc_assignment --> arc_variable := arc_expression.@>   
@*4 \§arc assignment> $\longrightarrow$ \§arc variable> 
\.{:=} \§arc expression>. 
\initials{LDF 2007.10.13.}

\LOG
\initials{LDF 2007.10.13.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>= 
 
@=arc_assignment: arc_variable ASSIGN arc_expression@>
{

  Arc* a = static_cast<Arc*>(@=$3@>);

  Int_Void_Ptr ivp = 
     Scan_Parse::assign_simple<Arc>(static_cast<Scanner_Node>(parameter),
                                                      "Arc",
                                                      @=$1@>,
                                                      a);

  @=$$@> = ivp.v;

};



@q **** (4) helix_assignment.@>
@*3 \§helix assignment>. 
\initials{LDF 2005.05.20.}

\LOG
\initials{LDF 2005.05.20.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> helix_assignment@>@/

@q ***** (5) helix_assignment --> helix_variable := helix_expression.@>   
@*4 \§helix assignment> $\longrightarrow$ \§helix variable> 
\.{:=} \§helix expression>. 
\initials{LDF 2005.05.20.}

\LOG
\initials{LDF 2005.05.20.}
Added this rule.

\initials{LDF 2005.11.07.}
Removed debugging code.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>= 
 
@=helix_assignment: helix_variable ASSIGN helix_expression@>
{

  Helix* f = static_cast<Helix*>(@=$3@>);

  Int_Void_Ptr ivp = Scan_Parse::assign_simple<Helix>(static_cast<Scanner_Node>(parameter),
                                                      "Helix",
                                                      @=$1@>,
                                                      f);

  @=$$@> = ivp.v;

};

@q **** (4) polygon_assignment.  @>
@*3 \§polygon assignment>. 
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.02.11.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> polygon_assignment@>@/

@q **** (4) polygon_assignment --> polygon_variable @>
@q **** (4) ASSIGN polygon_expression.              @>   

@*3 \§polygon assignment> $\longrightarrow$ \§polygon variable> 
\.{ASSIGN} \§polygon expression>. 
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.02.11.}
Added this rule.

\initials{LDF 2005.03.01.}
Changed \§polygon expression> to \§polygon-like expression>.

\initials{LDF 2005.10.24.}
Changed |polygon_like_expression| back to |polygon_expression|.
Removed all debugging code.
\ENDLOG

@<Define rules@>= 
 
@=polygon_assignment: polygon_variable ASSIGN polygon_expression@>
{


  Polygon* f = static_cast<Polygon*>(@=$3@>);

  Int_Void_Ptr ivp = assign_simple<Polygon>(static_cast<Scanner_Node>(parameter),
                                            "Polygon",
                                            @=$1@>,
                                            f);      

  @=$$@> = ivp.v;

};

@q **** (4) polygon_assignment --> polygon_variable @>
@q **** (4) ASSIGN rectangle_expression.            @>   

@*3 \§polygon assignment> $\longrightarrow$ \§polygon variable> 
\.{ASSIGN} \§rectangle expression>. 
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.10.24.}
Added this rule.
\ENDLOG

@<Define rules@>= 
 
@=polygon_assignment: polygon_variable ASSIGN rectangle_expression@>
{


  Polygon* f = static_cast<Polygon*>(@=$3@>);

  Int_Void_Ptr ivp = assign_simple<Polygon>(static_cast<Scanner_Node>(parameter),
                                            "Polygon",
                                            @=$1@>,
                                            f);      

  @=$$@> = ivp.v;

};

@q **** (4) polygon_assignment --> polygon_variable @>
@q **** (4) ASSIGN triangle_expression.         @>   

@*3 \§polygon assignment> $\longrightarrow$ \§polygon variable> 
\.{ASSIGN} \§triangle expression>. 
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.10.24.}
Added this rule.
\ENDLOG

@<Define rules@>= 
 
@=polygon_assignment: polygon_variable ASSIGN triangle_expression@>
{


  Polygon* f = static_cast<Polygon*>(@=$3@>);

  Int_Void_Ptr ivp = assign_simple<Polygon>(static_cast<Scanner_Node>(parameter),
                                            "Polygon",
                                            @=$1@>,
                                            f);      

  @=$$@> = ivp.v;

};

@q **** (4) polygon_assignment --> polygon_variable @>
@q **** (4) ASSIGN reg_polygon_expression.          @>   

@*3 \§polygon assignment> $\longrightarrow$ \§polygon variable> 
\.{ASSIGN} \§regular polygon expression>. 
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.10.24.}
Added this rule.
\ENDLOG

@<Define rules@>= 
 
@=polygon_assignment: polygon_variable ASSIGN reg_polygon_expression@>
{


  Polygon* f = static_cast<Polygon*>(@=$3@>);

  Int_Void_Ptr ivp = assign_simple<Polygon>(static_cast<Scanner_Node>(parameter),
                                            "Polygon",
                                            @=$1@>,
                                            f);      

  @=$$@> = ivp.v;

};

@q **** (4) reg_polygon_assignment.  @>
@*3 \§regular polygon assignment>. 

\LOG
\initials{LDF 2004.07.06.}  
Added this section.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> reg_polygon_assignment@>@/

@q ***** (5) reg_polygon_assignment --> reg_polygon_variable @>
@q ***** (5) := reg_polygon_expression.  @>   

@*4 \§regular polygon assignment> 
$\longrightarrow$ \§regular polygon variable> 
\.{:=} \§regular polygon expression>. 

\LOG
\initials{LDF 2004.07.06.}
Added this rule.

\initials{LDF 2004.07.28.}
Removed code from this rule.  
Now calling |Scan_Parse::assign_simple|.
\ENDLOG

@<Define rules@>= 
 
@=reg_polygon_assignment: reg_polygon_variable ASSIGN reg_polygon_expression@>
{

  Reg_Polygon* f = static_cast<Reg_Polygon*>(@=$3@>);

  Int_Void_Ptr ivp = assign_simple<Reg_Polygon>(static_cast<Scanner_Node>(parameter),
                                   "Reg_Polygon",
                                   @=$1@>,
                                   f);

  @=$$@> = ivp.v;

};



@q **** (4) rectangle_assignment.  @>
@*2 \§rectangle assignment>. 

\LOG
\initials{LDF 2004.06.30.}  
Added this section.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> rectangle_assignment@>@/

@q ***** (5) rectangle_assignment --> rectangle_variable @>
@q ***** (5) := rectangle_expression.                    @>   

@*3 \§rectangle assignment> $\longrightarrow$ \§rectangle variable> 
\.{:=} \§rectangle expression>. 

\LOG
\initials{LDF 2004.06.30.}
Added this rule.

\initials{LDF 2004.07.28.}
Removed code from this rule.  
Now calling |Scan_Parse::assign_simple|.

\initials{LDF 2005.10.24.}
Changed |rectangle_like_expression| to |rectangle_expression|.
Removed all debugging code.
\ENDLOG

@<Define rules@>= 
 
@=rectangle_assignment: rectangle_variable ASSIGN rectangle_expression@>
{

   Rectangle* f = static_cast<Rectangle*>(@=$3@>);

   Int_Void_Ptr ivp = assign_simple<Rectangle>(static_cast<Scanner_Node>(parameter),
                                               "Rectangle",
                                               @=$1@>,
                                               f);

  @=$$@> = ivp.v;

};

@q **** (4) triangle_assignment.  @>
@*3 \§triangle assignment>. 
\initials{2009.01.06.}

\LOG
\initials{2009.01.06.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> triangle_assignment@>@/

@q **** (4) triangle_assignment --> triangle_variable @>
@q **** (4) ASSIGN triangle_expression.               @>   

@*3 \§triangle assignment> $\longrightarrow$ \§triangle variable> 
\.{ASSIGN} \§triangle expression>. 
\initials{2009.01.06.}

\LOG
\initials{2009.01.06.}
Added this rule.
\ENDLOG

@<Define rules@>= 
 
@=triangle_assignment: triangle_variable ASSIGN triangle_expression@>
{


  Triangle* t = static_cast<Triangle*>(@=$3@>);

  Int_Void_Ptr ivp = assign_simple<Triangle>(static_cast<Scanner_Node>(parameter),
                                             "Triangle",
                                             @=$1@>,
                                             t);      

  @=$$@> = ivp.v;

};

@q **** (4) cuboid_assignment.  @>
@*2 \§cuboid assignment>. 

\LOG
\initials{LDF 2004.08.18.}  
Added this section.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> cuboid_assignment@>@/

@q ***** (5) cuboid_assignment --> cuboid_variable @>
@q ***** (5) := cuboid_expression.  @>   

@*3 \§cuboid assignment> 
$\longrightarrow$ \§cuboid variable> 
\.{:=} \§cuboid expression>. 

\LOG
\initials{LDF 2004.08.18.}
Added this rule.
\ENDLOG

@<Define rules@>= 
 
@=cuboid_assignment: cuboid_variable ASSIGN cuboid_expression@>
{

  Cuboid* c = static_cast<Cuboid*>(@=$3@>);

  Int_Void_Ptr ivp = assign_simple<Cuboid>(static_cast<Scanner_Node>(parameter),
                                   "Cuboid",
                                   @=$1@>,
                                   c);

  @=$$@> = ivp.v;

};

@q **** (4) polyhedron_assignment.  @>
@*2 \§polyhedron assignment>. 

\LOG
\initials{LDF 2004.08.31.}  
Added this section.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> polyhedron_assignment@>@/

@q ***** (5) polyhedron_assignment --> polyhedron_variable @>
@q ***** (5) := polyhedron_expression.  @>   

@*3 \§polyhedron assignment> 
$\longrightarrow$ \§polyhedron variable> 
\.{:=} \§polyhedron expression>. 

\LOG
\initials{LDF 2004.08.31.}
Added this rule.
\ENDLOG

@<Define rules@>=
 
@=polyhedron_assignment: polyhedron_variable ASSIGN polyhedron_expression@>
{

  
  Polyhedron* c = static_cast<Polyhedron*>(@=$3@>);

  Int_Void_Ptr ivp = assign_simple<Polyhedron>(static_cast<Scanner_Node>(parameter),
                                   "Polyhedron",
                                   @=$1@>,
                                   c);

  @=$$@> = ivp.v;

};

@q **** (4) sphere_assignment.@>
@*2 \§sphere assignment>. 

\LOG
\initials{LDF 2005.10.30.}
Added this section.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> sphere_assignment@>@/

@q ***** (5) sphere_assignment --> sphere_variable @>
@q ***** (5) ASSIGN sphere_expression.  @>   

@*3 \§sphere assignment> 
$\longrightarrow$ \§sphere variable> 
\.{ASSIGN} \§sphere expression>. 

\LOG
\initials{LDF 2005.10.30.}
Added this rule.
\ENDLOG

@<Define rules@>=
 
@=sphere_assignment: sphere_variable ASSIGN sphere_expression@>
{

  Sphere* c = static_cast<Sphere*>(@=$3@>);

  Int_Void_Ptr ivp = assign_simple<Sphere>(static_cast<Scanner_Node>(parameter),
                                           "Sphere",
                                           @=$1@>,
                                           c);

  @=$$@> = ivp.v;

};

@q **** (4) sphere_development_assignment.@>
@*2 \§sphere development assignment>. 

\LOG
\initials{LDF 2009.11.09.}
Added this section.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> sphere_development_assignment@>@/

@q ***** (5) sphere_development_assignment --> sphere_development_variable @>
@q ***** (5) ASSIGN sphere_development_expression.  @>   

@*3 \§sphere development assignment> 
$\longrightarrow$ \§sphere development variable> 
\.{ASSIGN} \§sphere development expression>. 

\LOG
\initials{LDF 2009.11.09.}
Added this rule.
\ENDLOG

@<Define rules@>=
 
@=sphere_development_assignment: sphere_development_variable ASSIGN sphere_development_expression@>
{

  Sphere_Development* c = static_cast<Sphere_Development*>(@=$3@>);

  Int_Void_Ptr ivp = assign_simple<Sphere_Development>(static_cast<Scanner_Node>(parameter),
                                           "Sphere_Development",
                                           @=$1@>,
                                           c);

  @=$$@> = ivp.v;

};

@q ***** (5) sphere_development_assignment --> sphere_development_variable @>
@q ***** (5) ASSIGN numeric_expression.  @>   

@*3 \§sphere development assignment> 
$\longrightarrow$ \§sphere development variable> 
\.{ASSIGN} \§numeric expression>. 

\LOG
\initials{LDF 2009.11.10.}
Added this rule.
\ENDLOG

@<Define rules@>=
 
@=sphere_development_assignment: sphere_development_variable ASSIGN numeric_expression@>
{

    Sphere_Development* s = create_new<Sphere_Development>(0);

    s->set_radius(@=$3@>);
    
    Int_Void_Ptr ivp = assign_simple<Sphere_Development>(static_cast<Scanner_Node>(parameter),
                                                         "Sphere_Development", 
                                                         @=$1@>, s, true);

    @=$$@> = ivp.v;

};

@q **** (4) plane_assignment.@>
@*2 \§plane assignment>. 

\LOG
\initials{LDF 2005.10.30.}
Added this section.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> plane_assignment@>@/

@q ***** (5) plane_assignment --> plane_variable ASSIGN plane_expression.@>   

@*4 \§plane assignment> $\longrightarrow$ \§plane variable> 
\.{ASSIGN} \§plane expression>. 

\LOG
\initials{LDF 2005.10.30.}
Added this rule.
\ENDLOG

@<Define rules@>=
 
@=plane_assignment: plane_variable ASSIGN plane_expression@>
{

  Int_Void_Ptr ivp = assign_simple<Plane>(static_cast<Scanner_Node>(parameter),
                                           "Plane",
                                           @=$1@>,
                                           static_cast<Plane*>(@=$3@>));
  @=$$@> = ivp.v;

};

@q ***** (5) plane_assignment --> plane_variable ASSIGN path_expression.@>   

@*3 \§plane assignment> $\longrightarrow$ \§plane variable> 
\.{ASSIGN} \§path expression>. 

\LOG
\initials{LDF 2005.10.30.}
Added this rule.
\ENDLOG

@<Define rules@>=
 
@=plane_assignment: plane_variable ASSIGN path_expression@>
{

    @=$$@> = Scan_Parse::plane_assignment_func<Path>(@=$1@>,
                                                     static_cast<Path*>(@=$3@>),
                                                     parameter);
};


@q **** (4) origami_figure_assignment.  @>
@*2 \§origami figure assignment>. 
\initials{LDF 2005.02.03.}

\LOG
\initials{LDF 2005.02.03.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> origami_figure_assignment@>@/

@q ***** (5) origami_figure_assignment --> @>
@q ***** (5) origami_figure_variable :=    @>
@q ***** (5) origami_figure_expression.    @>   

@*3 \§origami figure assignment> 
$\longrightarrow$ \§origami figure variable> 
\.{:=} \§origami figure expression>. 
\initials{LDF 2005.02.03.}

\LOG
\initials{LDF 2005.02.03.}
Added this rule.
\ENDLOG

@<Define rules@>= 
 
@=origami_figure_assignment: origami_figure_variable @>
@=ASSIGN origami_figure_expression@>
{

  Origami_Figure* c = static_cast<Origami_Figure*>(@=$3@>);

  Int_Void_Ptr ivp = assign_simple<Origami_Figure>(static_cast<Scanner_Node>(parameter),
                                   "Origami_Figure",
                                   @=$1@>,
                                   c);

  @=$$@> = ivp.v;

};


@q ** (2) Vector types.@> 
@*1 Vector types.
\initials{LDF 2005.01.07.}

\LOG
\initials{LDF 2005.01.07.}
Added this section.
\ENDLOG

@q *** (3) Non-|Shape| types.@> 
@*1 Non-{\bf Shape} types.
\initials{LDF 2005.01.07.}

\LOG
\initials{LDF 2005.01.07.}
Added this section.
\ENDLOG

@q **** (4) boolean_vector_assignment.  @>
@*2 \§boolean vector assignment>. 
\initials{LDF 2005.01.07.}

\LOG
\initials{LDF 2005.01.07.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> boolean_vector_assignment@>

@q **** (4) boolean_vector_assignment -->  @>  
@q **** (4) boolean_vector_variable ASSIGN @>  
@q **** (4) boolean_vector_expression.     @> 

@*3 \§boolean vector assignment>
$\longrightarrow$ \§boolean vector variable> 
\.{ASSIGN} \§boolean vector expression>.      
\initials{LDF 2005.01.07.}

\LOG
\initials{LDF 2005.01.07.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=

@=boolean_vector_assignment: boolean_vector_variable @>  
@=ASSIGN boolean_vector_expression@>@/
{

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ****** (6) Error handling for the case that |entry == 0|.@>

@ Error handling for the case that |entry == 0 |.
\initials{LDF 2005.01.07.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == 0)|  */

@q ****** (6) |entry != 0|.@>   

@ |entry != 0|.
\initials{LDF 2005.01.07.}

@<Define rules@>=

  else /* |entry != 0|  */
    {


   typedef Pointer_Vector<bool> PV;
 
   PV* pv = static_cast<PV*>(@=$3@>);

   PV* entry_pv = static_cast<PV*>(entry->object);

   if (entry_pv)
     entry_pv->clear();

   
      int status = vector_type_assign<bool, bool>(static_cast<Scanner_Node>(parameter),
                                              entry,
                                              pv);

@q ******* (7) Error handling:                           @> 
@q ******* (7) |Scan_Parse::vector_type_assign| failed.@> 

@ Error handling:  |Scan_Parse::vector_type_assign| 
failed. 
\initials{LDF 2005.01.07.}

@<Define rules@>=
                            

    if (status != 0)
         {

            delete pv;

            @=$$@> = static_cast<void*>(0);

         } /* |if (status != 0)|  */

@q ******* (7) |Scan_Parse::vector_type_assign| succeeded.@> 

@ |Scan_Parse::vector_type_assign| succeeded. 
\initials{LDF 2005.01.07.}

@<Define rules@>=

   else /* |status == 0|  */
      {
         delete pv;

         @=$$@> = static_cast<void*>(entry->object); 
 
      }  /* |else| (|status == 0|)  */


   }   /* |else| (|entry != 0|)  */


@q ****** (6).@> 

}; 

@q **** (4) complex vector assignment.  @>
@*2 complex vector assignment. 
\initials{LDF 2007.12.02.}

@q ***** (5) assignment -->  @>  
@q ***** (5) complex_vector_variable ASSIGN @>  
@q ***** (5) complex_vector_expression.     @> 

@*3 \§assignment>
$\longrightarrow$ \§complex vector variable> 
\.{ASSIGN} \§complex vector expression>.      
\initials{LDF 2007.12.02.}

\LOG
\initials{LDF 2007.12.02.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=

@=assignment: complex_vector_variable ASSIGN complex_vector_expression@>@/
{


   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ****** (6) Error handling for the case that |entry == 0|.@>

@ Error handling for the case that |entry == 0 |.
\initials{LDF 2007.12.02.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == 0)|  */

@q ****** (6) |entry != 0|.@>   

@ |entry != 0|.
\initials{LDF 2007.12.02.}

@<Define rules@>=

  else /* |entry != 0|  */
    {

   typedef Pointer_Vector<Complex> PV;
 
   PV* pv = static_cast<PV*>(@=$3@>);

   PV* entry_pv = static_cast<PV*>(entry->object);

   if (entry_pv)
     entry_pv->clear();

   int status 
       = Scan_Parse::vector_type_assign<Complex, Complex>(
            static_cast<Scanner_Node>(parameter),
            entry,
            pv);

@q ******* (7) Error handling:                           @> 
@q ******* (7) |Scan_Parse::vector_type_assign| failed.@> 

@ Error handling:  |Scan_Parse::vector_type_assign| 
failed. 
\initials{LDF 2007.12.02.}

@<Define rules@>=
                            

    if (status != 0)
         {

            delete pv;

            @=$$@> = static_cast<void*>(0);

         } /* |if (status != 0)|  */

@q ******* (7) |Scan_Parse::vector_type_assign| succeeded.@> 

@ |Scan_Parse::vector_type_assign| succeeded. 
\initials{LDF 2007.12.02.}

@<Define rules@>=

   else /* |status == 0|  */
      {
         delete pv;

         @=$$@> = static_cast<void*>(entry->object); 
 
      }  /* |else| (|status == 0|)  */


   }   /* |else| (|entry != 0|)  */


@q ****** (6).@> 

}; 

@q **** (4) matrix vector assignment.  @>
@*2 matrix vector assignment. 
\initials{LDF 2007.11.28.}

@q ***** (5) assignment -->  @>  
@q ***** (5) matrix_vector_variable ASSIGN @>  
@q ***** (5) matrix_vector_expression.     @> 

@*3 \§assignment>
$\longrightarrow$ \§matrix vector variable> 
\.{ASSIGN} \§matrix vector expression>.      
\initials{LDF 2007.11.28.}

\LOG
\initials{LDF 2007.11.28.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=

@=assignment: matrix_vector_variable ASSIGN matrix_vector_expression@>@/
{


   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ****** (6) Error handling for the case that |entry == 0|.@>

@ Error handling for the case that |entry == 0 |.
\initials{LDF 2007.10.13.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == 0)|  */

@q ****** (6) |entry != 0|.@>   

@ |entry != 0|.
\initials{LDF 2007.10.13.}

@<Define rules@>=

  else /* |entry != 0|  */
    {

   typedef Pointer_Vector<Matrix> PV;
 
   PV* pv = static_cast<PV*>(@=$3@>);

   PV* entry_pv = static_cast<PV*>(entry->object);

   if (entry_pv)
     entry_pv->clear();

   int status 
       = Scan_Parse::vector_type_assign<Matrix, Matrix>(
            static_cast<Scanner_Node>(parameter),
            entry,
            pv);

@q ******* (7) Error handling:                           @> 
@q ******* (7) |Scan_Parse::vector_type_assign| failed.@> 

@ Error handling:  |Scan_Parse::vector_type_assign| 
failed. 
\initials{LDF 2007.10.13.}

@<Define rules@>=
                            

    if (status != 0)
         {

            delete pv;

            @=$$@> = static_cast<void*>(0);

         } /* |if (status != 0)|  */

@q ******* (7) |Scan_Parse::vector_type_assign| succeeded.@> 

@ |Scan_Parse::vector_type_assign| succeeded. 
\initials{LDF 2007.10.13.}

@<Define rules@>=

   else /* |status == 0|  */
      {
         delete pv;

         @=$$@> = static_cast<void*>(entry->object); 
 
      }  /* |else| (|status == 0|)  */


   }   /* |else| (|entry != 0|)  */


@q ****** (6).@> 

}; 

@q **** (4) string_vector_assignment.  @>
@*2 \§string vector assignment>. 
\initials{LDF 2005.01.09.}

\LOG
\initials{LDF 2005.01.09.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> string_vector_assignment@>

@q **** (4) string_vector_assignment -->  @>  
@q **** (4) string_vector_variable ASSIGN @>  
@q **** (4) string_vector_expression.     @> 

@*3 \§string vector assignment>
$\longrightarrow$ \§string vector variable> 
\.{ASSIGN} \§string vector expression>.      
\initials{LDF 2005.01.09.}

\LOG
\initials{LDF 2005.01.09.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=

@=string_vector_assignment: string_vector_variable @>  
@=ASSIGN string_vector_expression@>@/
{

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ****** (6) Error handling for the case that |entry == 0|.@>

@ Error handling for the case that |entry == 0 |.
\initials{LDF 2005.01.09.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == 0)|  */

@q ****** (6) |entry != 0|.@>   

@ |entry != 0|.
\initials{LDF 2005.01.09.}

@<Define rules@>=

  else /* |entry != 0|  */
    {

   typedef Pointer_Vector<string> PV;
 
   PV* pv = static_cast<PV*>(@=$3@>);

   PV* entry_pv = static_cast<PV*>(entry->object);

   if (entry_pv)
     entry_pv->clear();

   
      int status = vector_type_assign<string, string>(static_cast<Scanner_Node>(parameter),
                                                  entry,
                                                  pv);

@q ******* (7) Error handling:                           @> 
@q ******* (7) |Scan_Parse::vector_type_assign| failed.@> 

@ Error handling:  |Scan_Parse::vector_type_assign| 
failed. 
\initials{LDF 2005.01.09.}

@<Define rules@>=
                            

    if (status != 0)
         {

            delete pv;

            @=$$@> = static_cast<void*>(0);

         } /* |if (status != 0)|  */

@q ******* (7) |Scan_Parse::vector_type_assign| succeeded.@> 

@ |Scan_Parse::vector_type_assign| succeeded. 
\initials{LDF 2005.01.09.}

@<Define rules@>=

   else /* |status == 0|  */
      {
         delete pv;

         @=$$@> = static_cast<void*>(entry->object); 
 
      }  /* |else| (|status == 0|)  */


   }   /* |else| (|entry != 0|)  */


@q ****** (6).@> 

}; 

@q **** (4) pen_vector_assignment.  @>
@*2 \§pen vector assignment>. 
\initials{LDF 2005.01.13.}

\LOG
\initials{LDF 2005.01.13.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> pen_vector_assignment@>

@q **** (4) pen_vector_assignment -->  @>  
@q **** (4) pen_vector_variable ASSIGN @>  
@q **** (4) pen_vector_expression.     @> 

@*3 \§pen vector assignment>
$\longrightarrow$ \§pen vector variable> 
\.{ASSIGN} \§pen vector expression>.      
\initials{LDF 2005.01.13.}

\LOG
\initials{LDF 2005.01.13.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=

@=pen_vector_assignment: pen_vector_variable @>  
@=ASSIGN pen_vector_expression@>@/
{


   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ****** (6) Error handling for the case that |entry == 0|.@>

@ Error handling for the case that |entry == 0 |.
\initials{LDF 2005.01.13.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == static_cast<Id_Map_Entry_Node>(0))|  */

@q ****** (6) |entry != static_cast<Id_Map_Entry_Node>(0)|.@>   

@ |entry != static_cast<Id_Map_Entry_Node>(0)|.
\initials{LDF 2005.01.13.}

@<Define rules@>=

  else /* |entry != static_cast<Id_Map_Entry_Node>(0)|  */
    {

   typedef Pointer_Vector<Pen> PV;
 
   PV* pv = static_cast<PV*>(@=$3@>);

   PV* entry_pv = static_cast<PV*>(entry->object);

   if (entry_pv)
     entry_pv->clear();

   
      int status = vector_type_assign<Pen, Pen>(static_cast<Scanner_Node>(parameter),
                                                entry,
                                                pv);

@q ******* (7) Error handling:                           @> 
@q ******* (7) |Scan_Parse::vector_type_assign| failed.@> 

@ Error handling:  |Scan_Parse::vector_type_assign| 
failed. 
\initials{LDF 2005.01.13.}

@<Define rules@>=
                            

    if (status != 0)
         {

            delete pv;

            @=$$@> = static_cast<void*>(0);

         } /* |if (status != 0)|  */

@q ******* (7) |Scan_Parse::vector_type_assign| succeeded.@> 

@ |Scan_Parse::vector_type_assign| succeeded. 
\initials{LDF 2005.01.13.}

@<Define rules@>=

   else /* |status == 0|  */
      {
         delete pv;

         @=$$@> = static_cast<void*>(entry->object); 
 
      }  /* |else| (|status == 0|)  */


   }   /* |else| (|entry != static_cast<Id_Map_Entry_Node>(0)|)  */


@q ****** (6).@> 

}; 

@q **** (4) dash_pattern_vector_assignment.  @>
@*2 \§dash pattern vector assignment>. 
\initials{LDF 2005.01.13.}

\LOG
\initials{LDF 2005.01.13.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> dash_pattern_vector_assignment@>

@q **** (4) dash_pattern_vector_assignment -->  @>  
@q **** (4) dash_pattern_vector_variable ASSIGN @>  
@q **** (4) dash_pattern_vector_expression.     @> 

@*3 \§dash pattern vector assignment>
$\longrightarrow$ \§dash pattern vector variable> 
\.{ASSIGN} \§dash pattern vector expression>.      
\initials{LDF 2005.01.13.}

\LOG
\initials{LDF 2005.01.13.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=

@=dash_pattern_vector_assignment: dash_pattern_vector_variable @>  
@=ASSIGN dash_pattern_vector_expression@>@/
{

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ****** (6) Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0)|.@>

@ Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0) |.
\initials{LDF 2005.01.13.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == static_cast<Id_Map_Entry_Node>(0))|  */

@q ****** (6) |entry != static_cast<Id_Map_Entry_Node>(0)|.@>   

@ |entry != static_cast<Id_Map_Entry_Node>(0)|.
\initials{LDF 2005.01.13.}

@<Define rules@>=

  else /* |entry != static_cast<Id_Map_Entry_Node>(0)|  */
    {


   typedef Pointer_Vector<Dash_Pattern> PV;
 
   PV* pv = static_cast<PV*>(@=$3@>);

   PV* entry_pv = static_cast<PV*>(entry->object);

   if (entry_pv)
     entry_pv->clear();

   
   int status = vector_type_assign<Dash_Pattern, Dash_Pattern>(
                                             static_cast<Scanner_Node>(parameter),
                                             entry,
                                             pv);

@q ******* (7) Error handling:                           @> 
@q ******* (7) |Scan_Parse::vector_type_assign| failed.@> 

@ Error handling:  |Scan_Parse::vector_type_assign| 
failed. 
\initials{LDF 2005.01.13.}

@<Define rules@>=
                            

    if (status != 0)
         {

            delete pv;

            @=$$@> = static_cast<void*>(0);

         } /* |if (status != 0)|  */

@q ******* (7) |Scan_Parse::vector_type_assign| succeeded.@> 

@ |Scan_Parse::vector_type_assign| succeeded. 
\initials{LDF 2005.01.13.}

@<Define rules@>=

   else /* |status == 0|  */
      {
         delete pv;

         @=$$@> = static_cast<void*>(entry->object); 
 
      }  /* |else| (|status == 0|)  */


   }   /* |else| (|entry != static_cast<Id_Map_Entry_Node>(0)|)  */


@q ****** (6).@> 

}; 


@q **** (4) color_vector_assignment.  @>
@*2 \§color vector assignment>. 

\LOG
\initials{LDF 2004.11.05.}
Commented-out this type declaration.

\initials{LDF 2005.01.07.}
Commented this type declaration back in.
\ENDLOG


@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> color_vector_assignment@>

@q **** (4) color_vector_assignment -->  @>  
@q **** (4) color_vector_variable ASSIGN @>  
@q **** (4) color_vector_expression.     @> 

@*3 \§color vector assignment>
$\longrightarrow$ \§color vector variable> 
\.{ASSIGN} \§color vector expression>.      
\initials{LDF 2005.01.07.}

\LOG
\initials{LDF 2005.01.07.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=

@=color_vector_assignment: color_vector_variable @>  
@=ASSIGN color_vector_expression@>@/
{

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ****** (6) Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0)|.@>

@ Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0) |.
\initials{LDF 2005.01.07.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == static_cast<Id_Map_Entry_Node>(0))|  */

@q ****** (6) |entry != static_cast<Id_Map_Entry_Node>(0)|.@>   

@ |entry != static_cast<Id_Map_Entry_Node>(0)|.
\initials{LDF 2005.01.07.}

@<Define rules@>=

  else /* |entry != static_cast<Id_Map_Entry_Node>(0)|  */
    {

   typedef Pointer_Vector<Color> PV;
 
   PV* pv = static_cast<PV*>(@=$3@>);

   PV* entry_pv = static_cast<PV*>(entry->object);

   if (entry_pv)
     entry_pv->clear();

   
   int status = vector_type_assign<Color, Color>(static_cast<Scanner_Node>(parameter),
                                                 entry,
                                                 pv);

@q ******* (7) Error handling:                           @> 
@q ******* (7) |Scan_Parse::vector_type_assign| failed.@> 

@ Error handling:  |Scan_Parse::vector_type_assign| 
failed. 
\initials{LDF 2005.01.07.}

@<Define rules@>=
                            

    if (status != 0)
         {

            delete pv;

            @=$$@> = static_cast<void*>(0);

         } /* |if (status != 0)|  */

@q ******* (7) |Scan_Parse::vector_type_assign| succeeded.@> 

@ |Scan_Parse::vector_type_assign| succeeded. 
\initials{LDF 2005.01.07.}

@<Define rules@>=

   else /* |status == 0|  */
      {
         delete pv;

         @=$$@> = static_cast<void*>(entry->object); 
 
      }  /* |else| (|status == 0|)  */


   }   /* |else| (|entry != static_cast<Id_Map_Entry_Node>(0)|)  */


@q ****** (6).@> 

}; 

@q **** (4) transform_vector_assignment.  @>
@*2 \§transform vector assignment>. 
\initials{LDF 2005.01.13.}

\LOG
\initials{LDF 2005.01.13.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> transform_vector_assignment@>

@q **** (4) transform_vector_assignment -->  @>  
@q **** (4) transform_vector_variable ASSIGN @>  
@q **** (4) transform_vector_expression.     @> 

@*3 \§transform vector assignment>
$\longrightarrow$ \§transform vector variable> 
\.{ASSIGN} \§transform vector expression>.      
\initials{LDF 2005.01.13.}

\LOG
\initials{LDF 2005.01.13.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=

@=transform_vector_assignment: transform_vector_variable @>  
@=ASSIGN transform_vector_expression@>@/
{

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ****** (6) Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0)|.@>

@ Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0) |.
\initials{LDF 2005.01.13.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == static_cast<Id_Map_Entry_Node>(0))|  */

@q ****** (6) |entry != static_cast<Id_Map_Entry_Node>(0)|.@>   

@ |entry != static_cast<Id_Map_Entry_Node>(0)|.
\initials{LDF 2005.01.13.}

@<Define rules@>=

  else /* |entry != static_cast<Id_Map_Entry_Node>(0)|  */
    {

   typedef Pointer_Vector<Transform> PV;
 
   PV* pv = static_cast<PV*>(@=$3@>);

   PV* entry_pv = static_cast<PV*>(entry->object);

   if (entry_pv)
     entry_pv->clear();

   
   int status = vector_type_assign<Transform, Transform>(static_cast<Scanner_Node>(parameter),
                                                entry,
                                                pv);

@q ******* (7) Error handling:                           @> 
@q ******* (7) |Scan_Parse::vector_type_assign| failed.@> 

@ Error handling:  |Scan_Parse::vector_type_assign| 
failed. 
\initials{LDF 2005.01.13.}

@<Define rules@>=
                            

    if (status != 0)
         {

            delete pv;

            @=$$@> = static_cast<void*>(0);

         } /* |if (status != 0)|  */

@q ******* (7) |Scan_Parse::vector_type_assign| succeeded.@> 

@ |Scan_Parse::vector_type_assign| succeeded. 
\initials{LDF 2005.01.13.}

@<Define rules@>=

   else /* |status == 0|  */
      {
         delete pv;

         @=$$@> = static_cast<void*>(entry->object); 
 
      }  /* |else| (|status == 0|)  */


   }   /* |else| (|entry != static_cast<Id_Map_Entry_Node>(0)|)  */


@q ****** (6).@> 

}; 

@q **** (4) focus_vector_assignment.  @>
@*2 \§focus vector assignment>. 
\initials{LDF 2005.01.18.}

\LOG
\initials{LDF 2005.01.18.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> focus_vector_assignment@>

@q **** (4) focus_vector_assignment -->  @>  
@q **** (4) focus_vector_variable ASSIGN @>  
@q **** (4) focus_vector_expression.     @> 

@*3 \§focus vector assignment>
$\longrightarrow$ \§focus vector variable> 
\.{ASSIGN} \§focus vector expression>.      
\initials{LDF 2005.01.18.}

\LOG
\initials{LDF 2005.01.18.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=

@=focus_vector_assignment: focus_vector_variable @>  
@=ASSIGN focus_vector_expression@>@/
{

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ****** (6) Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0)|.@>

@ Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0) |.
\initials{LDF 2005.01.18.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == static_cast<Id_Map_Entry_Node>(0))|  */

@q ****** (6) |entry != static_cast<Id_Map_Entry_Node>(0)|.@>   

@ |entry != static_cast<Id_Map_Entry_Node>(0)|.
\initials{LDF 2005.01.18.}

@<Define rules@>=

  else /* |entry != static_cast<Id_Map_Entry_Node>(0)|  */
    {

   typedef Pointer_Vector<Focus> PV;
 
   PV* pv = static_cast<PV*>(@=$3@>);

   PV* entry_pv = static_cast<PV*>(entry->object);

   if (entry_pv)
     entry_pv->clear();

   
   int status = vector_type_assign<Focus, Focus>(static_cast<Scanner_Node>(parameter),
                                                entry,
                                                pv);

@q ******* (7) Error handling:                           @> 
@q ******* (7) |Scan_Parse::vector_type_assign| failed.@> 

@ Error handling:  |Scan_Parse::vector_type_assign| 
failed. 
\initials{LDF 2005.01.18.}

@<Define rules@>=
                            

    if (status != 0)
         {

            delete pv;

            @=$$@> = static_cast<void*>(0);

         } /* |if (status != 0)|  */

@q ******* (7) |Scan_Parse::vector_type_assign| succeeded.@> 

@ |Scan_Parse::vector_type_assign| succeeded. 
\initials{LDF 2005.01.18.}

@<Define rules@>=

   else /* |status == 0|  */
      {
         delete pv;

         @=$$@> = static_cast<void*>(entry->object); 
 
      }  /* |else| (|status == 0|)  */


   }   /* |else| (|entry != static_cast<Id_Map_Entry_Node>(0)|)  */


@q ****** (6).@> 

}; 



@q *** (3) |Shape| types.@> 
@*1 {\bf Shape} types.
\initials{LDF 2005.01.07.}

\LOG
\initials{LDF 2005.01.07.}
Added this section.
\ENDLOG

@q **** (4) |point_vector_assignment|.  @>
@*2 \§point vector assignment>. 

\LOG
\initials{LDF 2004.09.01.}
Added this section.

\initials{LDF 2004.11.05.}
Commented-out this type declaration.

\initials{LDF 2004.11.10.}
Commented this type declaration back in.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> point_vector_assignment@>

@q ***** (5) point_vector_assignment -->  @>  
@q ***** (5) point_vector_variable ASSIGN @>  
@q ***** (5) point_vector_expression.     @> 

@*3 \§point vector assignment>
$\longrightarrow$ \§point vector variable> 
\.{ASSIGN} \§point vector expression>.      

\LOG
\initials{LDF 2004.11.10.}
Added this rule.

\initials{LDF 2004.11.11.}
Changed the code in this rule to account for changes
I've made in |Scan_Parse::vector_type_assign|, which is 
defined in \filename{scanprse.web}.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=

@=point_vector_assignment: point_vector_variable @>  
@=ASSIGN point_vector_expression@>@/
{

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ****** (6) Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0)|.@>

@ Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0) |.
\initials{LDF 2004.11.06.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == static_cast<Id_Map_Entry_Node>(0))|  */

@q ****** (6) |entry != static_cast<Id_Map_Entry_Node>(0)|.@>   

@ |entry != static_cast<Id_Map_Entry_Node>(0)|.
\initials{LDF 2004.09.01.}

@<Define rules@>=

  else /* |entry != static_cast<Id_Map_Entry_Node>(0)|  */
    {

   typedef Pointer_Vector<Point> PV;
 
   PV* pv = static_cast<PV*>(@=$3@>);

   PV* entry_pv = static_cast<PV*>(entry->object);

   if (entry_pv)
     entry_pv->clear();

   
   int status = vector_type_assign<Point, Point>(static_cast<Scanner_Node>(parameter),
                                                entry,
                                                pv);

@q ******* (7) Error handling:                           @> 
@q ******* (7) |Scan_Parse::vector_type_assign| failed.@> 

@ Error handling:  |Scan_Parse::vector_type_assign| 
failed. 
\initials{LDF 2004.11.06.}

@<Define rules@>=
                            

    if (status != 0)
         {

            delete pv;

            @=$$@> = static_cast<void*>(0);

         } /* |if (status != 0)|  */

@q ******* (7) |Scan_Parse::vector_type_assign| succeeded.@> 

@ |Scan_Parse::vector_type_assign| succeeded. 
\initials{LDF 2004.11.06.}

@<Define rules@>=

   else /* |status == 0|  */
      {
         delete pv;

         @=$$@> = static_cast<void*>(entry->object); 
 
      }  /* |else| (|status == 0|)  */


   }   /* |else| (|entry != static_cast<Id_Map_Entry_Node>(0)|)  */


@q ****** (6).@> 

}; 

@q ***** (5) point_vector_assignment --> point_vector_variable @>  
@q ***** (5) ASSIGN bool_point_vector_expression.              @> 

@*3 \§point vector assignment>
$\longrightarrow$ \§point vector variable> 
\.{ASSIGN} \§bool-point vector expression>.      

\LOG
\initials{LDF 2004.11.10.}
Added this rule.

\initials{LDF 2004.11.11.}
Changed the code in this rule to account for changes
I've made in |Scan_Parse::vector_type_assign|, which is 
defined in \filename{scanprse.web}.
\ENDLOG


@q ***** (5) Definition.@> 

@<Define rules@>=

@=point_vector_assignment: point_vector_variable @>  
@=ASSIGN bool_point_vector_expression@>@/
{

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ****** (6) Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0)|.@>

@ Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0) |.
\initials{LDF 2004.11.10.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == static_cast<Id_Map_Entry_Node>(0))|  */

@q ****** (6) |entry != static_cast<Id_Map_Entry_Node>(0)|.@>   

@ |entry != static_cast<Id_Map_Entry_Node>(0)|.
\initials{LDF 2004.11.10.}


@<Define rules@>=

  else /* |entry != static_cast<Id_Map_Entry_Node>(0)|  */
    {

   typedef Pointer_Vector<Point> PV;
   typedef Pointer_Vector<Bool_Point> BPV;
 
   BPV* bpv = static_cast<BPV*>(@=$3@>);

   PV* entry_pv = static_cast<PV*>(entry->object);

   if (entry_pv)
     entry_pv->clear();

   
   int status = vector_type_assign<Point, Bool_Point>(static_cast<Scanner_Node>(parameter),
                                                  entry,                   
                                                  bpv);

@q ******* (7) Error handling:                           @> 
@q ******* (7) |Scan_Parse::vector_type_assign| failed.@> 

@ Error handling:  |Scan_Parse::vector_type_assign| 
failed. 
\initials{LDF 2004.11.10.}

@<Define rules@>=
                            

    if (status != 0)
         {

            delete bpv;

            @=$$@> = static_cast<void*>(0);

         } /* |if (status != 0)|  */

@q ******* (7) |Scan_Parse::vector_type_assign| succeeded.@> 

@ |Scan_Parse::vector_type_assign| succeeded. 
\initials{LDF 2004.11.10.}

@<Define rules@>=

   else /* |status == 0|  */
      {
         delete bpv;

         @=$$@> = static_cast<void*>(entry->object); 
 
      }  /* |else| (|status == 0|)  */


   }   /* |else| (|entry != static_cast<Id_Map_Entry_Node>(0)|)  */


@q ****** (6).@> 

}; 

@q **** (4) |bool_point_vector_assignment|.  @>
@*2 \§bool-point vector assignment>. 

\LOG
\initials{LDF 2004.09.01.}
Added this section.

\initials{LDF 2004.11.05.}
Commented-out this type declaration.

\initials{LDF 2004.11.05.}
Commented this type declaration back in.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> bool_point_vector_assignment@>

@q ***** (5) bool_point_vector_assignment -->  @>  
@q ***** (5) bool_point_vector_variable ASSIGN @>  
@q ***** (5) bool_point_vector_expression.     @> 

@*3 \§bool-point vector assignment>
$\longrightarrow$ \§bool-point vector variable> 
\.{ASSIGN} \§bool-point vector expression>.      

\LOG
\initials{LDF 2004.11.05.}
Added this rule.

\initials{LDF 2004.11.06.}
Finished writing this rule.

\initials{LDF 2004.11.11.}
Changed the code in this rule to account for changes
I've made in |Scan_Parse::vector_type_assign|, which is 
defined in \filename{scanprse.web}.
\ENDLOG


@q ***** (5) Definition.@> 

@<Define rules@>=

@=bool_point_vector_assignment: bool_point_vector_variable @>  
@=ASSIGN bool_point_vector_expression@>@/
{

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ****** (6) Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0)|.@>

@ Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0) |.
\initials{LDF 2004.11.06.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == static_cast<Id_Map_Entry_Node>(0))|  */

@q ****** (6) |entry != static_cast<Id_Map_Entry_Node>(0)|.@>   

@ |entry != static_cast<Id_Map_Entry_Node>(0)|.
\initials{LDF 2004.09.01.}

\LOG
initials{LDF 2004.11.06.}
@:BUG FIX@> BUG FIX:  If |entry->object != 0|, 
now clearing the |Pointer_Vector<Bool_Point>| it points to 
before looping over the |bool_point_vector_expression|
and calling |Scan_Parse::vector_type_plus_assign|.

\initials{LDF 2004.11.08.}
Now calling |Scan_Parse::vector_type_assign| instead of 
|Scan_Parse::vector_type_plus_assign|.  This function loops over 
the |bool_point_vector_expression|, so we don't have to do so here.

\initials{LDF 2005.11.28.}
@:BUG FIX@> BUG FIX:  Now calling |Scan_Parse::clear_vector_func|.
\ENDLOG 

@<Define rules@>=

  else /* |entry != static_cast<Id_Map_Entry_Node>(0)|  */
    {

        Scan_Parse::clear_vector_func(static_cast<Scanner_Node>(parameter),
                                      entry);

        typedef Pointer_Vector<Bool_Point> BPV;
 
        BPV* bpv = static_cast<BPV*>(@=$3@>);


   
        int status = vector_type_assign<Bool_Point, Bool_Point>(
                          static_cast<Scanner_Node>(parameter),
                          entry,
                          bpv);

@q ******* (7) Error handling:                                @> 
@q ******* (7) |Scan_Parse::vector_type_assign| failed.@> 

@ Error handling:  |Scan_Parse::vector_type_assign| 
failed. 
\initials{LDF 2004.11.06.}

@<Define rules@>=
                            

    if (status != 0)
         {

            delete bpv;
            bpv = 0;

            @=$$@> = static_cast<void*>(0);

         } /* |if (status != 0)|  */

@q ******* (7) |Scan_Parse::vector_type_assign| succeeded.@> 

@ |Scan_Parse::vector_type_assign| succeeded. 
\initials{LDF 2004.11.06.}

@<Define rules@>=

   else /* |status == 0|  */
      {
         delete bpv;
         bpv = 0;

         @=$$@> = static_cast<void*>(entry->object); 
 
      }  /* |else| (|status == 0|)  */

   }   /* |else| (|entry != static_cast<Id_Map_Entry_Node>(0)|)  */


@q ****** (6).@> 

}; 

@q ***** (5) bool_point_vector_assignment -->  @>  
@q ***** (5) bool_point_vector_variable ASSIGN @>  
@q ***** (5) point_vector_expression.     @> 

@*3 \§bool-point vector assignment>
$\longrightarrow$ \§bool-point vector variable> 
\.{ASSIGN} \§point vector expression>.      

\LOG
\initials{LDF 2004.11.10.}
Added this rule.

\initials{LDF 2004.11.11.}
Changed the code in this rule to account for changes
I've made in |Scan_Parse::vector_type_assign|, which is 
defined in \filename{scanprse.web}.
\ENDLOG


@q ***** (5) Definition.@> 

@<Define rules@>=

@=bool_point_vector_assignment: bool_point_vector_variable @>  
@=ASSIGN point_vector_expression@>@/
{

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ****** (6) Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0)|.@>

@ Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0) |.
\initials{LDF 2004.11.10.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == static_cast<Id_Map_Entry_Node>(0))|  */

@q ****** (6) |entry != static_cast<Id_Map_Entry_Node>(0)|.@>   

@ |entry != static_cast<Id_Map_Entry_Node>(0)|.
\initials{LDF 2004.11.10.}


@<Define rules@>=

  else /* |entry != static_cast<Id_Map_Entry_Node>(0)|  */
    {


   typedef Pointer_Vector<Point> PV;
   typedef Pointer_Vector<Bool_Point> BPV;
 
   PV* pv = static_cast<PV*>(@=$3@>);

   BPV* entry_bpv = static_cast<BPV*>(entry->object);

   if (entry_bpv)
     entry_bpv->clear();
   
   int status = vector_type_assign<Bool_Point, Point>(static_cast<Scanner_Node>(parameter),
                                                  entry,
                                                  pv);

@q ******* (7) Error handling:                           @> 
@q ******* (7) |Scan_Parse::vector_type_assign| failed.@> 

@ Error handling:  |Scan_Parse::vector_type_assign| 
failed. 
\initials{LDF 2004.11.10.}

@<Define rules@>=
                            

    if (status != 0)
         {
            delete pv;

            @=$$@> = static_cast<void*>(0);

         } /* |if (status != 0)|  */

@q ******* (7) |Scan_Parse::vector_type_assign| succeeded.@> 

@ |Scan_Parse::vector_type_assign| succeeded. 
\initials{LDF 2004.11.10.}

@<Define rules@>=

   else /* |status == 0|  */
      {
         delete pv;

         @=$$@> = static_cast<void*>(entry->object); 
 
      }  /* |else| (|status == 0|)  */


   }   /* |else| (|entry != static_cast<Id_Map_Entry_Node>(0)|)  */


@q ****** (6).@> 

}; 

@q **** (4) |path_vector_assignment|.  @>
@*2 \§path vector assignment>. 

\LOG
\initials{LDF 2004.12.10.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> path_vector_assignment@>

@q ***** (5) path_vector_assignment -->  @>  
@q ***** (5) path_vector_variable ASSIGN @>  
@q ***** (5) path_vector_expression.     @> 

@*3 \§path vector assignment>
$\longrightarrow$ \§path vector variable> 
\.{ASSIGN} \§path vector expression>.      

\LOG
\initials{LDF 2004.12.10.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=

@=path_vector_assignment: path_vector_variable @>  
@=ASSIGN path_vector_expression@>@/
{


   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ****** (6) Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0)|.@>

@ Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0) |.
\initials{LDF 2004.12.10.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == static_cast<Id_Map_Entry_Node>(0))|  */

@q ****** (6) |entry != static_cast<Id_Map_Entry_Node>(0)|.@>   

@ |entry != static_cast<Id_Map_Entry_Node>(0)|.
\initials{LDF 2004.12.10.}

@<Define rules@>=

  else /* |entry != static_cast<Id_Map_Entry_Node>(0)|  */
    {

   typedef Pointer_Vector<Path> PV;
 
   PV* pv = static_cast<PV*>(@=$3@>);

   PV* entry_pv = static_cast<PV*>(entry->object);

   if (entry_pv)
     entry_pv->clear();

   int status = vector_type_assign<Path, Path>(static_cast<Scanner_Node>(parameter),
                                           entry,
                                           pv);

@q ******* (7) Error handling:                           @> 
@q ******* (7) |Scan_Parse::vector_type_assign| failed.@> 

@ Error handling:  |Scan_Parse::vector_type_assign| 
failed. 
\initials{LDF 2004.12.10.}

@<Define rules@>=
                            

    if (status != 0)
         {

            delete pv;

            @=$$@> = static_cast<void*>(0);

         } /* |if (status != 0)|  */

@q ******* (7) |Scan_Parse::vector_type_assign| succeeded.@> 

@ |Scan_Parse::vector_type_assign| succeeded. 
\initials{LDF 2004.12.10.}

@<Define rules@>=

   else /* |status == 0|  */
      {
         delete pv;

         @=$$@> = static_cast<void*>(entry->object); 
 
      }  /* |else| (|status == 0|)  */


   }   /* |else| (|entry != static_cast<Id_Map_Entry_Node>(0)|)  */


@q ****** (6).@> 

}; 

@q **** (4) |ellipse_vector_assignment|.  @>
@*2 \§ellipse vector assignment>. 

\LOG
\initials{LDF 2004.12.14.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> ellipse_vector_assignment@>

@q ***** (5) ellipse_vector_assignment -->  @>  
@q ***** (5) ellipse_vector_variable ASSIGN @>  
@q ***** (5) ellipse_vector_expression.     @> 

@*3 \§ellipse vector assignment>
$\longrightarrow$ \§ellipse vector variable> 
\.{ASSIGN} \§ellipse vector expression>.      

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=

@=ellipse_vector_assignment: ellipse_vector_variable @>  
@=ASSIGN ellipse_vector_expression@>@/
{

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ****** (6) Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0)|.@>

@ Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0) |.
\initials{LDF 2004.12.14.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == static_cast<Id_Map_Entry_Node>(0))|  */

@q ****** (6) |entry != static_cast<Id_Map_Entry_Node>(0)|.@>   

@ |entry != static_cast<Id_Map_Entry_Node>(0)|.
\initials{LDF 2004.12.14.}

@<Define rules@>=

  else /* |entry != static_cast<Id_Map_Entry_Node>(0)|  */
    {

   typedef Pointer_Vector<Ellipse> PV;
 
   PV* pv = static_cast<PV*>(@=$3@>);

   PV* entry_pv = static_cast<PV*>(entry->object);

   if (entry_pv)
     entry_pv->clear();

   int status = vector_type_assign<Ellipse, Ellipse>(static_cast<Scanner_Node>(parameter),
                                           entry,
                                           pv);

@q ******* (7) Error handling:                           @> 
@q ******* (7) |Scan_Parse::vector_type_assign| failed.@> 

@ Error handling:  |Scan_Parse::vector_type_assign| 
failed. 
\initials{LDF 2004.12.14.}

@<Define rules@>=
                            

    if (status != 0)
         {

            delete pv;

            @=$$@> = static_cast<void*>(0);

         } /* |if (status != 0)|  */

@q ******* (7) |Scan_Parse::vector_type_assign| succeeded.@> 

@ |Scan_Parse::vector_type_assign| succeeded. 
\initials{LDF 2004.12.14.}

@<Define rules@>=

   else /* |status == 0|  */
      {
         delete pv;

         @=$$@> = static_cast<void*>(entry->object); 
 
      }  /* |else| (|status == 0|)  */


   }   /* |else| (|entry != static_cast<Id_Map_Entry_Node>(0)|)  */


@q ****** (6).@> 

}; 

@q **** (4) |circle_vector_assignment|.  @>
@*2 \§circle vector assignment>. 

\LOG
\initials{LDF 2004.12.14.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> circle_vector_assignment@>

@q ***** (5) circle_vector_assignment -->  @>  
@q ***** (5) circle_vector_variable ASSIGN @>  
@q ***** (5) circle_vector_expression.     @> 

@*3 \§circle vector assignment>
$\longrightarrow$ \§circle vector variable> 
\.{ASSIGN} \§circle vector expression>.      

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=

@=circle_vector_assignment: circle_vector_variable @>  
@=ASSIGN circle_vector_expression@>@/
{

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ****** (6) Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0)|.@>

@ Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0) |.
\initials{LDF 2004.12.14.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == static_cast<Id_Map_Entry_Node>(0))|  */

@q ****** (6) |entry != static_cast<Id_Map_Entry_Node>(0)|.@>   

@ |entry != static_cast<Id_Map_Entry_Node>(0)|.
\initials{LDF 2004.12.14.}

@<Define rules@>=

  else /* |entry != static_cast<Id_Map_Entry_Node>(0)|  */
    {

   typedef Pointer_Vector<Circle> PV;
 
   PV* pv = static_cast<PV*>(@=$3@>);

   PV* entry_pv = static_cast<PV*>(entry->object);

   if (entry_pv)
     entry_pv->clear();

   int status = vector_type_assign<Circle, Circle>(static_cast<Scanner_Node>(parameter),
                                           entry,
                                           pv);

@q ******* (7) Error handling:                           @> 
@q ******* (7) |Scan_Parse::vector_type_assign| failed.@> 

@ Error handling:  |Scan_Parse::vector_type_assign| 
failed. 
\initials{LDF 2004.12.14.}

@<Define rules@>=
                            

    if (status != 0)
         {

            delete pv;

            @=$$@> = static_cast<void*>(0);

         } /* |if (status != 0)|  */

@q ******* (7) |Scan_Parse::vector_type_assign| succeeded.@> 

@ |Scan_Parse::vector_type_assign| succeeded. 
\initials{LDF 2004.12.14.}

@<Define rules@>=

   else /* |status == 0|  */
      {
         delete pv;

         @=$$@> = static_cast<void*>(entry->object); 
 
      }  /* |else| (|status == 0|)  */


   }   /* |else| (|entry != static_cast<Id_Map_Entry_Node>(0)|)  */


@q ****** (6).@> 

}; 


@q **** (4) |parabola_vector_assignment|.  @>
@*2 \§parabola vector assignment>. 
\initials{LDF 2005.11.07.}

\LOG
\initials{LDF 2005.11.07.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> parabola_vector_assignment@>

@q ***** (5) parabola_vector_assignment -->  @>  
@q ***** (5) parabola_vector_variable ASSIGN @>  
@q ***** (5) parabola_vector_expression.     @> 

@*3 \§parabola vector assignment>
$\longrightarrow$ \§parabola vector variable> 
\.{ASSIGN} \§parabola vector expression>.      
\initials{LDF 2005.11.07.}

\LOG
\initials{LDF 2005.11.07.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=

@=parabola_vector_assignment: parabola_vector_variable @>  
@=ASSIGN parabola_vector_expression@>@/
{


   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ****** (6) Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0)|.@>

@ Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0) |.
\initials{LDF 2005.11.07.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == static_cast<Id_Map_Entry_Node>(0))|  */

@q ****** (6) |entry != static_cast<Id_Map_Entry_Node>(0)|.@>   

@ |entry != static_cast<Id_Map_Entry_Node>(0)|.
\initials{LDF 2005.11.07.}

@<Define rules@>=

  else /* |entry != static_cast<Id_Map_Entry_Node>(0)|  */
    {

   typedef Pointer_Vector<Parabola> PV;
 
   PV* pv = static_cast<PV*>(@=$3@>);

   PV* entry_pv = static_cast<PV*>(entry->object);

   if (entry_pv)
     entry_pv->clear();

   int status 
       = Scan_Parse::vector_type_assign<Parabola, Parabola>(
            static_cast<Scanner_Node>(parameter),
            entry,
            pv);

@q ******* (7) Error handling:                           @> 
@q ******* (7) |Scan_Parse::vector_type_assign| failed.@> 

@ Error handling:  |Scan_Parse::vector_type_assign| 
failed. 
\initials{LDF 2005.11.07.}

@<Define rules@>=
                            

    if (status != 0)
         {

            delete pv;

            @=$$@> = static_cast<void*>(0);

         } /* |if (status != 0)|  */

@q ******* (7) |Scan_Parse::vector_type_assign| succeeded.@> 

@ |Scan_Parse::vector_type_assign| succeeded. 
\initials{LDF 2005.11.07.}

@<Define rules@>=

   else /* |status == 0|  */
      {
         delete pv;

         @=$$@> = static_cast<void*>(entry->object); 
 
      }  /* |else| (|status == 0|)  */


   }   /* |else| (|entry != static_cast<Id_Map_Entry_Node>(0)|)  */


@q ****** (6).@> 

}; 

@q **** (4) |hyperbola_vector_assignment|.  @>
@*2 \§hyperbola vector assignment>. 
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> hyperbola_vector_assignment@>

@q ***** (5) hyperbola_vector_assignment -->  @>  
@q ***** (5) hyperbola_vector_variable ASSIGN @>  
@q ***** (5) hyperbola_vector_expression.     @> 

@*3 \§hyperbola vector assignment>
$\longrightarrow$ \§hyperbola vector variable> 
\.{ASSIGN} \§hyperbola vector expression>.      
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=

@=hyperbola_vector_assignment: hyperbola_vector_variable @>  
@=ASSIGN hyperbola_vector_expression@>@/
{


   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ****** (6) Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0)|.@>

@ Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0) |.
\initials{LDF 2005.11.14.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == static_cast<Id_Map_Entry_Node>(0))|  */

@q ****** (6) |entry != static_cast<Id_Map_Entry_Node>(0)|.@>   

@ |entry != static_cast<Id_Map_Entry_Node>(0)|.
\initials{LDF 2005.11.14.}

@<Define rules@>=

  else /* |entry != static_cast<Id_Map_Entry_Node>(0)|  */
    {

   typedef Pointer_Vector<Hyperbola> PV;
 
   PV* pv = static_cast<PV*>(@=$3@>);

   PV* entry_pv = static_cast<PV*>(entry->object);

   if (entry_pv)
     entry_pv->clear();

   int status 
       = Scan_Parse::vector_type_assign<Hyperbola, Hyperbola>(
            static_cast<Scanner_Node>(parameter),
            entry,
            pv);

@q ******* (7) Error handling:                           @> 
@q ******* (7) |Scan_Parse::vector_type_assign| failed.@> 

@ Error handling:  |Scan_Parse::vector_type_assign| 
failed. 
\initials{LDF 2005.11.14.}

@<Define rules@>=
                            

    if (status != 0)
         {

            delete pv;

            @=$$@> = static_cast<void*>(0);

         } /* |if (status != 0)|  */

@q ******* (7) |Scan_Parse::vector_type_assign| succeeded.@> 

@ |Scan_Parse::vector_type_assign| succeeded. 
\initials{LDF 2005.11.14.}

@<Define rules@>=

   else /* |status == 0|  */
      {
         delete pv;

         @=$$@> = static_cast<void*>(entry->object); 
 
      }  /* |else| (|status == 0|)  */


   }   /* |else| (|entry != static_cast<Id_Map_Entry_Node>(0)|)  */


@q ****** (6).@> 

}; 

@q **** (4) |arc_vector_assignment|.  @>
@*2 \§arc vector assignment>. 
\initials{LDF 2007.10.13.}

\LOG
\initials{LDF 2007.10.13.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> arc_vector_assignment@>

@q ***** (5) arc_vector_assignment -->  @>  
@q ***** (5) arc_vector_variable ASSIGN @>  
@q ***** (5) arc_vector_expression.     @> 

@*3 \§arc vector assignment>
$\longrightarrow$ \§arc vector variable> 
\.{ASSIGN} \§arc vector expression>.      
\initials{LDF 2007.10.13.}

\LOG
\initials{LDF 2007.10.13.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=

@=arc_vector_assignment: arc_vector_variable @>  
@=ASSIGN arc_vector_expression@>@/
{


   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ****** (6) Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0)|.@>

@ Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0) |.
\initials{LDF 2007.10.13.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == static_cast<Id_Map_Entry_Node>(0))|  */

@q ****** (6) |entry != static_cast<Id_Map_Entry_Node>(0)|.@>   

@ |entry != static_cast<Id_Map_Entry_Node>(0)|.
\initials{LDF 2007.10.13.}

@<Define rules@>=

  else /* |entry != static_cast<Id_Map_Entry_Node>(0)|  */
    {

   typedef Pointer_Vector<Arc> PV;
 
   PV* pv = static_cast<PV*>(@=$3@>);

   PV* entry_pv = static_cast<PV*>(entry->object);

   if (entry_pv)
     entry_pv->clear();

   int status 
       = Scan_Parse::vector_type_assign<Arc, Arc>(
            static_cast<Scanner_Node>(parameter),
            entry,
            pv);

@q ******* (7) Error handling:                           @> 
@q ******* (7) |Scan_Parse::vector_type_assign| failed.@> 

@ Error handling:  |Scan_Parse::vector_type_assign| 
failed. 
\initials{LDF 2007.10.13.}

@<Define rules@>=
                            

    if (status != 0)
         {

            delete pv;

            @=$$@> = static_cast<void*>(0);

         } /* |if (status != 0)|  */

@q ******* (7) |Scan_Parse::vector_type_assign| succeeded.@> 

@ |Scan_Parse::vector_type_assign| succeeded. 
\initials{LDF 2007.10.13.}

@<Define rules@>=

   else /* |status == 0|  */
      {
         delete pv;

         @=$$@> = static_cast<void*>(entry->object); 
 
      }  /* |else| (|status == 0|)  */


   }   /* |else| (|entry != static_cast<Id_Map_Entry_Node>(0)|)  */


@q ****** (6).@> 

}; 

@q **** (4) |conic_section_lattice_vector_assignment|.  @>
@*2 \§conic section lattice vector assignment>. 
\initials{LDF 2007.07.29.}

\LOG
\initials{LDF 2007.07.29.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> conic_section_lattice_vector_assignment@>

@q ***** (5) conic_section_lattice_vector_assignment -->  @>  
@q ***** (5) conic_section_lattice_vector_variable ASSIGN @>  
@q ***** (5) conic_section_lattice_vector_expression.     @> 

@*3 \§conic section lattice vector assignment>
$\longrightarrow$ \§conic section lattice vector variable> 
\.{ASSIGN} \§conic section lattice vector expression>.      
\initials{LDF 2007.07.29.}

\LOG
\initials{LDF 2005.11.14.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=

@=conic_section_lattice_vector_assignment: conic_section_lattice_vector_variable @>  
@=ASSIGN conic_section_lattice_vector_expression@>@/
{


   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ****** (6) Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0)|.@>

@ Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0) |.
\initials{LDF 2005.11.14.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == static_cast<Id_Map_Entry_Node>(0))|  */

@q ****** (6) |entry != static_cast<Id_Map_Entry_Node>(0)|.@>   

@ |entry != static_cast<Id_Map_Entry_Node>(0)|.
\initials{LDF 2005.11.14.}

@<Define rules@>=

  else /* |entry != static_cast<Id_Map_Entry_Node>(0)|  */
    {

   typedef Pointer_Vector<Conic_Section_Lattice> PV;
 
   PV* pv = static_cast<PV*>(@=$3@>);

   PV* entry_pv = static_cast<PV*>(entry->object);

   if (entry_pv)
     entry_pv->clear();

   int status 
       = Scan_Parse::vector_type_assign<Conic_Section_Lattice, 
                                        Conic_Section_Lattice>(
            static_cast<Scanner_Node>(parameter),
            entry,
            pv);

@q ******* (7) Error handling:                           @> 
@q ******* (7) |Scan_Parse::vector_type_assign| failed.@> 

@ Error handling:  |Scan_Parse::vector_type_assign| 
failed. 
\initials{LDF 2005.11.14.}

@<Define rules@>=
                            

    if (status != 0)
         {

            delete pv;

            @=$$@> = static_cast<void*>(0);

         } /* |if (status != 0)|  */

@q ******* (7) |Scan_Parse::vector_type_assign| succeeded.@> 

@ |Scan_Parse::vector_type_assign| succeeded. 
\initials{LDF 2005.11.14.}

@<Define rules@>=

   else /* |status == 0|  */
      {
         delete pv;

         @=$$@> = static_cast<void*>(entry->object); 
 
      }  /* |else| (|status == 0|)  */


   }   /* |else| (|entry != static_cast<Id_Map_Entry_Node>(0)|)  */


@q ****** (6).@> 

}; 



@q **** (4) |helix_vector_assignment|.  @>
@*2 \§helix vector assignment>. 
\initials{LDF 2005.05.20.}

\LOG
\initials{LDF 2005.05.20.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> helix_vector_assignment@>

@q ***** (5) helix_vector_assignment -->  @>  
@q ***** (5) helix_vector_variable ASSIGN @>  
@q ***** (5) helix_vector_expression.     @> 

@*3 \§helix vector assignment>
$\longrightarrow$ \§helix vector variable> 
\.{ASSIGN} \§helix vector expression>.      
\initials{LDF 2005.05.20.}

\LOG
\initials{LDF 2005.05.20.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=

@=helix_vector_assignment: helix_vector_variable @>  
@=ASSIGN helix_vector_expression@>@/
{

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ****** (6) Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0)|.@>

@ Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0) |.
\initials{LDF 2005.05.20.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == static_cast<Id_Map_Entry_Node>(0))|  */

@q ****** (6) |entry != static_cast<Id_Map_Entry_Node>(0)|.@>   

@ |entry != static_cast<Id_Map_Entry_Node>(0)|.
\initials{LDF 2005.05.20.}

@<Define rules@>=

  else /* |entry != static_cast<Id_Map_Entry_Node>(0)|  */
    {

   typedef Pointer_Vector<Helix> PV;
 
   PV* pv = static_cast<PV*>(@=$3@>);

   PV* entry_pv = static_cast<PV*>(entry->object);

   if (entry_pv)
     entry_pv->clear();

   int status = vector_type_assign<Helix, Helix>(static_cast<Scanner_Node>(parameter),
                                                 entry,
                                                 pv);

@q ******* (7) Error handling:                           @> 
@q ******* (7) |Scan_Parse::vector_type_assign| failed.@> 

@ Error handling:  |Scan_Parse::vector_type_assign| 
failed. 
\initials{LDF 2005.05.20.}

@<Define rules@>=
                            

    if (status != 0)
         {

            delete pv;

            @=$$@> = static_cast<void*>(0);

         } /* |if (status != 0)|  */

@q ******* (7) |Scan_Parse::vector_type_assign| succeeded.@> 

@ |Scan_Parse::vector_type_assign| succeeded. 
\initials{LDF 2005.05.20.}

@<Define rules@>=

   else /* |status == 0|  */
      {
         delete pv;

         @=$$@> = static_cast<void*>(entry->object); 
 
      }  /* |else| (|status == 0|)  */


   }   /* |else| (|entry != static_cast<Id_Map_Entry_Node>(0)|)  */


@q ****** (6).@> 

}; 

@q **** (4) |triangle_vector_assignment|.  @>
@*2 \§triangle vector assignment>. 
\initials{LDF 2005.01.25.}

\LOG
\initials{LDF 2005.01.25.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> triangle_vector_assignment@>

@q ***** (5) triangle_vector_assignment -->  @>  
@q ***** (5) triangle_vector_variable ASSIGN @>  
@q ***** (5) triangle_vector_expression.     @> 

@*3 \§triangle vector assignment>
$\longrightarrow$ \§triangle vector variable> 
\.{ASSIGN} \§triangle vector expression>.      
\initials{LDF 2005.01.25.}

\LOG
\initials{LDF 2005.01.25.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=

@=triangle_vector_assignment: triangle_vector_variable @>  
@=ASSIGN triangle_vector_expression@>@/
{

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ****** (6) Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0)|.@>

@ Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0) |.
\initials{LDF 2005.01.25.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == static_cast<Id_Map_Entry_Node>(0))|  */

@q ****** (6) |entry != static_cast<Id_Map_Entry_Node>(0)|.@>   

@ |entry != static_cast<Id_Map_Entry_Node>(0)|.
\initials{LDF 2005.01.25.}

@<Define rules@>=

  else /* |entry != static_cast<Id_Map_Entry_Node>(0)|  */
    {

   typedef Pointer_Vector<Triangle> PV;
 
   PV* pv = static_cast<PV*>(@=$3@>);

   PV* entry_pv = static_cast<PV*>(entry->object);

   if (entry_pv)
     entry_pv->clear();

   int status = vector_type_assign<Triangle, Triangle>(static_cast<Scanner_Node>(parameter),
                                                       entry,
                                                       pv);

@q ******* (7) Error handling:                           @> 
@q ******* (7) |Scan_Parse::vector_type_assign| failed.@> 

@ Error handling:  |Scan_Parse::vector_type_assign| 
failed. 
\initials{LDF 2005.01.25.}

@<Define rules@>=
                            

    if (status != 0)
         {

            delete pv;

            @=$$@> = static_cast<void*>(0);

         } /* |if (status != 0)|  */

@q ******* (7) |Scan_Parse::vector_type_assign| succeeded.@> 

@ |Scan_Parse::vector_type_assign| succeeded. 
\initials{LDF 2005.01.25.}

@<Define rules@>=

   else /* |status == 0|  */
      {
         delete pv;

         @=$$@> = static_cast<void*>(entry->object); 
 
      }  /* |else| (|status == 0|)  */


   }   /* |else| (|entry != static_cast<Id_Map_Entry_Node>(0)|)  */


@q ****** (6).@> 

}; 

@q **** (4) |nurb_vector_assignment|.  @>
@*2 \§nurb vector assignment>. 
\initials{LDF 2005.01.26.}

\LOG
\initials{LDF 2005.01.26.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> nurb_vector_assignment@>

@q ***** (5) nurb_vector_assignment -->  @>  
@q ***** (5) nurb_vector_variable ASSIGN @>  
@q ***** (5) nurb_vector_expression.     @> 

@*3 \§nurb vector assignment>
$\longrightarrow$ \§nurb vector variable> 
\.{ASSIGN} \§nurb vector expression>.      
\initials{LDF 2005.01.26.}

\LOG
\initials{LDF 2005.01.26.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=

@=nurb_vector_assignment: nurb_vector_variable @>  
@=ASSIGN nurb_vector_expression@>@/
{

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ****** (6) Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0)|.@>

@ Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0) |.
\initials{LDF 2005.01.26.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == static_cast<Id_Map_Entry_Node>(0))|  */

@q ****** (6) |entry != static_cast<Id_Map_Entry_Node>(0)|.@>   

@ |entry != static_cast<Id_Map_Entry_Node>(0)|.
\initials{LDF 2005.01.26.}

@<Define rules@>=

  else /* |entry != static_cast<Id_Map_Entry_Node>(0)|  */
    {

   typedef Pointer_Vector<Nurb> PV;
 
   PV* pv = static_cast<PV*>(@=$3@>);

   PV* entry_pv = static_cast<PV*>(entry->object);

   if (entry_pv)
     entry_pv->clear();

   int status = vector_type_assign<Nurb, Nurb>(static_cast<Scanner_Node>(parameter),
                                               entry,
                                               pv);

@q ******* (7) Error handling:                           @> 
@q ******* (7) |Scan_Parse::vector_type_assign| failed.@> 

@ Error handling:  |Scan_Parse::vector_type_assign| 
failed. 
\initials{LDF 2005.01.26.}

@<Define rules@>=
                            

    if (status != 0)
         {
            delete pv;

            @=$$@> = static_cast<void*>(0);

         } /* |if (status != 0)|  */

@q ******* (7) |Scan_Parse::vector_type_assign| succeeded.@> 

@ |Scan_Parse::vector_type_assign| succeeded. 
\initials{LDF 2005.01.26.}

@<Define rules@>=

   else /* |status == 0|  */
      {
         delete pv;

         @=$$@> = static_cast<void*>(entry->object); 
 
      }  /* |else| (|status == 0|)  */


   }   /* |else| (|entry != static_cast<Id_Map_Entry_Node>(0)|)  */


@q ****** (6).@> 

}; 

@q **** (4) |polygon_vector_assignment|.  @>
@*2 \§polygon vector assignment>. 
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.02.11.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> polygon_vector_assignment@>

@q ***** (5) polygon_vector_assignment -->  @>  
@q ***** (5) polygon_vector_variable ASSIGN @>  
@q ***** (5) polygon_vector_expression.     @> 

@*3 \§polygon vector assignment>
$\longrightarrow$ \§polygon vector variable> 
\.{ASSIGN} \§polygon vector expression>.      
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.02.11.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=

@=polygon_vector_assignment: polygon_vector_variable @>  
@=ASSIGN polygon_vector_expression@>@/
{

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

   typedef Pointer_Vector<Polygon> PV;
 
   PV* pv = static_cast<PV*>(@=$3@>);

@q ****** (6).@> 
@ 

\LOG
\initials{LDF 2005.03.01.}
Now calling |Scan_Parse::clear_vector_func| on |entry|.

\initials{LDF 2005.03.01.}
Removed the call to |Scan_Parse::clear_vector_func| on |entry|
to |Scan_Parse::vector_type_assign| in \filename{scanprse.web}.

\initials{LDF 2005.03.01.}
Now calling |Scan_Parse::vector_type_assign| in a |try| block
and catching |bad_alloc|.
\ENDLOG 

@<Define rules@>=

          int status = vector_type_assign<Polygon, Polygon>(
                          static_cast<Scanner_Node>(parameter),
                                                        entry,
                                                        pv);

@q ******* (7) Error handling:                            @> 
@q ******* (7) |Scan_Parse::vector_type_assign| failed, @> 
@q ******* (7) returning a non-zero value.                @> 

@ Error handling:  |Scan_Parse::vector_type_assign| 
failed, returning a non-zero value.
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.02.28.}
Now deleting |entry_pv| and setting it to 0 if 
|vector_type_assign<Polygon, Polygon>| failed.

\initials{LDF 2005.03.01.}
No longer declaring |Pointer_Vector<Polygon>* entry_pv|, 
and no longer deleting it and setting it to 0 
if |vector_type_assign<Polygon, Polygon>| failed.
\ENDLOG 

@<Define rules@>=

    if (status != 0)
         {

            delete pv;

            @=$$@> = static_cast<void*>(0);

         } /* |if (status != 0)|  */

@q ******* (7) |Scan_Parse::vector_type_assign| succeeded.@> 

@ |Scan_Parse::vector_type_assign| succeeded. 
\initials{LDF 2005.02.11.}

@<Define rules@>=

   else /* |status == 0|  */
      {
         delete pv;

         @=$$@> = static_cast<void*>(entry->object); 
 
      }  /* |else| (|status == 0|)  */


@q ****** (6).@> 

}; 

@q **** (4) |rectangle_vector_assignment|.  @>
@*2 \§rectangle vector assignment>. 

\LOG
\initials{LDF 2004.12.14.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> rectangle_vector_assignment@>

@q ***** (5) rectangle_vector_assignment -->  @>  
@q ***** (5) rectangle_vector_variable ASSIGN @>  
@q ***** (5) rectangle_vector_expression.     @> 

@*3 \§rectangle vector assignment>
$\longrightarrow$ \§rectangle vector variable> 
\.{ASSIGN} \§rectangle vector expression>.      

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=

@=rectangle_vector_assignment: rectangle_vector_variable @>  
@=ASSIGN rectangle_vector_expression@>@/
{


   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ****** (6) Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0)|.@>

@ Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0) |.
\initials{LDF 2004.12.14.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == static_cast<Id_Map_Entry_Node>(0))|  */

@q ****** (6) |entry != static_cast<Id_Map_Entry_Node>(0)|.@>   

@ |entry != static_cast<Id_Map_Entry_Node>(0)|.
\initials{LDF 2004.12.14.}

@<Define rules@>=

  else /* |entry != static_cast<Id_Map_Entry_Node>(0)|  */
    {

   typedef Pointer_Vector<Rectangle> PV;
 
   PV* pv = static_cast<PV*>(@=$3@>);

   PV* entry_pv = static_cast<PV*>(entry->object);

   if (entry_pv)
     entry_pv->clear();

   int status = vector_type_assign<Rectangle, Rectangle>(static_cast<Scanner_Node>(parameter),
                                                         entry,
                                                         pv);

@q ******* (7) Error handling:                           @> 
@q ******* (7) |Scan_Parse::vector_type_assign| failed.@> 

@ Error handling:  |Scan_Parse::vector_type_assign| 
failed. 
\initials{LDF 2004.12.14.}

@<Define rules@>=
                            

    if (status != 0)
         {
            delete pv;

            @=$$@> = static_cast<void*>(0);

         } /* |if (status != 0)|  */

@q ******* (7) |Scan_Parse::vector_type_assign| succeeded.@> 

@ |Scan_Parse::vector_type_assign| succeeded. 
\initials{LDF 2004.12.14.}

@<Define rules@>=

   else /* |status == 0|  */
      {
         delete pv;

         @=$$@> = static_cast<void*>(entry->object); 
 
      }  /* |else| (|status == 0|)  */


   }   /* |else| (|entry != static_cast<Id_Map_Entry_Node>(0)|)  */


@q ****** (6).@> 

}; 




@q **** (4) |reg_polygon_vector_assignment|.  @>
@*2 \§regular polygon vector assignment>. 

\LOG
\initials{LDF 2004.12.14.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> reg_polygon_vector_assignment@>

@q ***** (5) reg_polygon_vector_assignment -->  @>  
@q ***** (5) reg_polygon_vector_variable ASSIGN @>  
@q ***** (5) reg_polygon_vector_expression.     @> 

@*3 \§regular polygon vector assignment>
$\longrightarrow$ \§regular polygon vector variable> 
\.{ASSIGN} \§regular polygon vector expression>.      

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=

@=reg_polygon_vector_assignment: reg_polygon_vector_variable @>  
@=ASSIGN reg_polygon_vector_expression@>@/
{

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ****** (6) Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0)|.@>

@ Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0) |.
\initials{LDF 2004.12.14.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == static_cast<Id_Map_Entry_Node>(0))|  */

@q ****** (6) |entry != static_cast<Id_Map_Entry_Node>(0)|.@>   

@ |entry != static_cast<Id_Map_Entry_Node>(0)|.
\initials{LDF 2004.12.14.}

@<Define rules@>=

  else /* |entry != static_cast<Id_Map_Entry_Node>(0)|  */
    {

   typedef Pointer_Vector<Reg_Polygon> PV;
 
   PV* pv = static_cast<PV*>(@=$3@>);

   PV* entry_pv = static_cast<PV*>(entry->object);

   if (entry_pv)
     entry_pv->clear();

   int status = vector_type_assign<Reg_Polygon, Reg_Polygon>(
                    static_cast<Scanner_Node>(parameter),
                    entry,
                    pv);

@q ******* (7) Error handling:                           @> 
@q ******* (7) |Scan_Parse::vector_type_assign| failed.@> 

@ Error handling:  |Scan_Parse::vector_type_assign| 
failed. 
\initials{LDF 2004.12.14.}

@<Define rules@>=
                            

    if (status != 0)
         {

            delete pv;

            @=$$@> = static_cast<void*>(0);

         } /* |if (status != 0)|  */

@q ******* (7) |Scan_Parse::vector_type_assign| succeeded.@> 

@ |Scan_Parse::vector_type_assign| succeeded. 
\initials{LDF 2004.12.14.}

@<Define rules@>=

   else /* |status == 0|  */
      {
         delete pv;

         @=$$@> = static_cast<void*>(entry->object); 
 
      }  /* |else| (|status == 0|)  */


   }   /* |else| (|entry != static_cast<Id_Map_Entry_Node>(0)|)  */


@q ****** (6).@> 

}; 

@q **** (4) |cuboid_vector_assignment|.  @>
@*2 \§cuboid vector assignment>. 

\LOG
\initials{LDF 2004.12.14.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> cuboid_vector_assignment@>

@q ***** (5) cuboid_vector_assignment -->  @>  
@q ***** (5) cuboid_vector_variable ASSIGN @>  
@q ***** (5) cuboid_vector_expression.     @> 

@*3 \§cuboid vector assignment>
$\longrightarrow$ \§cuboid vector variable> 
\.{ASSIGN} \§cuboid vector expression>.      

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=

@=cuboid_vector_assignment: cuboid_vector_variable @>  
@=ASSIGN cuboid_vector_expression@>@/
{

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ****** (6) Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0)|.@>

@ Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0) |.
\initials{LDF 2004.12.14.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == static_cast<Id_Map_Entry_Node>(0))|  */

@q ****** (6) |entry != static_cast<Id_Map_Entry_Node>(0)|.@>   

@ |entry != static_cast<Id_Map_Entry_Node>(0)|.
\initials{LDF 2004.12.14.}

@<Define rules@>=

  else /* |entry != static_cast<Id_Map_Entry_Node>(0)|  */
    {

   typedef Pointer_Vector<Cuboid> PV;
 
   PV* pv = static_cast<PV*>(@=$3@>);

   PV* entry_pv = static_cast<PV*>(entry->object);

   if (entry_pv)
     entry_pv->clear();

   int status = vector_type_assign<Cuboid, Cuboid>(
                   static_cast<Scanner_Node>(parameter),
                   entry,
                   pv);

@q ******* (7) Error handling:                           @> 
@q ******* (7) |Scan_Parse::vector_type_assign| failed.@> 

@ Error handling:  |Scan_Parse::vector_type_assign| 
failed. 
\initials{LDF 2004.12.14.}

@<Define rules@>=
                            

    if (status != 0)
         {

            delete pv;

            @=$$@> = static_cast<void*>(0);

         } /* |if (status != 0)|  */

@q ******* (7) |Scan_Parse::vector_type_assign| succeeded.@> 

@ |Scan_Parse::vector_type_assign| succeeded. 
\initials{LDF 2004.12.14.}

@<Define rules@>=

   else /* |status == 0|  */
      {
         delete pv;

         @=$$@> = static_cast<void*>(entry->object); 
 
      }  /* |else| (|status == 0|)  */


   }   /* |else| (|entry != static_cast<Id_Map_Entry_Node>(0)|)  */


@q ****** (6).@> 

}; 

@q **** (4) |polyhedron_vector_assignment|.  @>
@*2 \§polyhedron vector assignment>. 

\LOG
\initials{LDF 2005.01.14.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> polyhedron_vector_assignment@>

@q ***** (5) polyhedron_vector_assignment -->  @>  
@q ***** (5) polyhedron_vector_variable ASSIGN @>  
@q ***** (5) polyhedron_vector_expression.     @> 

@*3 \§polyhedron vector assignment>
$\longrightarrow$ \§polyhedron vector variable> 
\.{ASSIGN} \§polyhedron vector expression>.      

\LOG
\initials{LDF 2005.01.14.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=

@=polyhedron_vector_assignment: polyhedron_vector_variable @>  
@=ASSIGN polyhedron_vector_expression@>@/
{

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ****** (6) Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0)|.@>

@ Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0) |.
\initials{LDF 2005.01.14.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == static_cast<Id_Map_Entry_Node>(0))|  */

@q ****** (6) |entry != static_cast<Id_Map_Entry_Node>(0)|.@>   

@ |entry != static_cast<Id_Map_Entry_Node>(0)|.
\initials{LDF 2005.01.14.}

@<Define rules@>=

  else /* |entry != static_cast<Id_Map_Entry_Node>(0)|  */
    {

   typedef Pointer_Vector<Polyhedron> PV;
 
   PV* pv = static_cast<PV*>(@=$3@>);

   PV* entry_pv = static_cast<PV*>(entry->object);

   if (entry_pv)
     entry_pv->clear();

   int status = vector_type_assign<Polyhedron, Polyhedron>(
                   static_cast<Scanner_Node>(parameter),
                   entry,
                   pv);

@q ******* (7) Error handling:                           @> 
@q ******* (7) |Scan_Parse::vector_type_assign| failed.@> 

@ Error handling:  |Scan_Parse::vector_type_assign| 
failed. 
\initials{LDF 2005.01.14.}

@<Define rules@>=
                            

    if (status != 0)
         {

            delete pv;

            @=$$@> = static_cast<void*>(0);

         } /* |if (status != 0)|  */

@q ******* (7) |Scan_Parse::vector_type_assign| succeeded.@> 

@ |Scan_Parse::vector_type_assign| succeeded. 
\initials{LDF 2005.01.14.}

@<Define rules@>=

   else /* |status == 0|  */
      {
         delete pv;

         @=$$@> = static_cast<void*>(entry->object); 
 
      }  /* |else| (|status == 0|)  */


   }   /* |else| (|entry != static_cast<Id_Map_Entry_Node>(0)|)  */


@q ****** (6).@> 

}; 

@q **** (4) |sphere_vector_assignment|.  @>
@*2 \§sphere vector assignment>. 

\LOG
\initials{LDF 2005.10.30.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> sphere_vector_assignment@>

@q ***** (5) sphere_vector_assignment -->  @>  
@q ***** (5) sphere_vector_variable ASSIGN @>  
@q ***** (5) sphere_vector_expression.     @> 

@*3 \§sphere vector assignment>
$\longrightarrow$ \§sphere vector variable> 
\.{ASSIGN} \§sphere vector expression>.      

\LOG
\initials{LDF 2005.10.30.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=

@=sphere_vector_assignment: sphere_vector_variable @>  
@=ASSIGN sphere_vector_expression@>@/
{

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ****** (6) Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0)|.@>

@ Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0) |.
\initials{LDF 2005.10.30.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == static_cast<Id_Map_Entry_Node>(0))|  */

@q ****** (6) |entry != static_cast<Id_Map_Entry_Node>(0)|.@>   

@ |entry != static_cast<Id_Map_Entry_Node>(0)|.
\initials{LDF 2005.10.30.}

@<Define rules@>=

  else /* |entry != static_cast<Id_Map_Entry_Node>(0)|  */
    {

   typedef Pointer_Vector<Sphere> PV;
 
   PV* pv = static_cast<PV*>(@=$3@>);

   PV* entry_pv = static_cast<PV*>(entry->object);

   if (entry_pv)
     entry_pv->clear();

   int status = vector_type_assign<Sphere, Sphere>(
                   static_cast<Scanner_Node>(parameter),
                   entry,
                   pv);

@q ******* (7) Error handling:                           @> 
@q ******* (7) |Scan_Parse::vector_type_assign| failed.@> 

@ Error handling:  |Scan_Parse::vector_type_assign| 
failed. 
\initials{LDF 2005.10.30.}

@<Define rules@>=
                            

    if (status != 0)
         {
            delete pv;

            @=$$@> = static_cast<void*>(0);

         } /* |if (status != 0)|  */

@q ******* (7) |Scan_Parse::vector_type_assign| succeeded.@> 

@ |Scan_Parse::vector_type_assign| succeeded. 
\initials{LDF 2005.10.30.}

@<Define rules@>=

   else /* |status == 0|  */
      {
         delete pv;

         @=$$@> = static_cast<void*>(entry->object); 
 
      }  /* |else| (|status == 0|)  */


   }   /* |else| (|entry != static_cast<Id_Map_Entry_Node>(0)|)  */


@q ****** (6).@> 

}; 

@q **** (4) |sphere_development_vector_assignment|.  @>
@*2 \§sphere development vector assignment>. 

\LOG
\initials{LDF 2009.11.09.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> sphere_development_vector_assignment@>

@q ***** (5) sphere_development_vector_assignment -->  @>  
@q ***** (5) sphere_development_vector_variable ASSIGN @>  
@q ***** (5) sphere_development_vector_expression.     @> 

@*3 \§sphere development vector assignment>
$\longrightarrow$ \§sphere development vector variable> 
\.{ASSIGN} \§sphere development vector expression>.      

\LOG
\initials{LDF 2009.11.09.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=

@=sphere_development_vector_assignment: sphere_development_vector_variable @>  
@=ASSIGN sphere_development_vector_expression@>@/
{

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ****** (6) Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0)|.@>

@ Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0) |.
\initials{LDF 2005.10.30.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == static_cast<Id_Map_Entry_Node>(0))|  */

@q ****** (6) |entry != static_cast<Id_Map_Entry_Node>(0)|.@>   

@ |entry != static_cast<Id_Map_Entry_Node>(0)|.
\initials{LDF 2005.10.30.}

@<Define rules@>=

  else /* |entry != static_cast<Id_Map_Entry_Node>(0)|  */
    {

   typedef Pointer_Vector<Sphere_Development> PV;
 
   PV* pv = static_cast<PV*>(@=$3@>);

   PV* entry_pv = static_cast<PV*>(entry->object);

   if (entry_pv)
     entry_pv->clear();

   int status = vector_type_assign<Sphere_Development, Sphere_Development>(
                   static_cast<Scanner_Node>(parameter),
                   entry,
                   pv);

@q ******* (7) Error handling:                           @> 
@q ******* (7) |Scan_Parse::vector_type_assign| failed.@> 

@ Error handling:  |Scan_Parse::vector_type_assign| 
failed. 
\initials{LDF 2005.10.30.}

@<Define rules@>=
                            

    if (status != 0)
         {
            delete pv;

            @=$$@> = static_cast<void*>(0);

         } /* |if (status != 0)|  */

@q ******* (7) |Scan_Parse::vector_type_assign| succeeded.@> 

@ |Scan_Parse::vector_type_assign| succeeded. 
\initials{LDF 2005.10.30.}

@<Define rules@>=

   else /* |status == 0|  */
      {
         delete pv;

         @=$$@> = static_cast<void*>(entry->object); 
 
      }  /* |else| (|status == 0|)  */


   }   /* |else| (|entry != static_cast<Id_Map_Entry_Node>(0)|)  */


@q ****** (6).@> 

}; 

@q **** (4) |plane_vector_assignment|.  @>
@*2 \§plane vector assignment>. 

\LOG
\initials{LDF 2005.10.30.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> plane_vector_assignment@>

@q ***** (5) plane_vector_assignment -->  @>  
@q ***** (5) plane_vector_variable ASSIGN @>  
@q ***** (5) plane_vector_expression.     @> 

@*3 \§plane vector assignment>
$\longrightarrow$ \§plane vector variable> 
\.{ASSIGN} \§plane vector expression>.      

\LOG
\initials{LDF 2005.10.30.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=

@=plane_vector_assignment: plane_vector_variable @>  
@=ASSIGN plane_vector_expression@>@/
{

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ****** (6) Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0)|.@>

@ Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0) |.
\initials{LDF 2005.10.30.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == static_cast<Id_Map_Entry_Node>(0))|  */

@q ****** (6) |entry != static_cast<Id_Map_Entry_Node>(0)|.@>   

@ |entry != static_cast<Id_Map_Entry_Node>(0)|.
\initials{LDF 2005.10.30.}

@<Define rules@>=

  else /* |entry != static_cast<Id_Map_Entry_Node>(0)|  */
    {

   typedef Pointer_Vector<Plane> PV;
 
   PV* pv = static_cast<PV*>(@=$3@>);

   PV* entry_pv = static_cast<PV*>(entry->object);

   if (entry_pv)
     entry_pv->clear();

   int status = vector_type_assign<Plane, Plane>(
                   static_cast<Scanner_Node>(parameter),
                   entry,
                   pv);

@q ******* (7) Error handling:                           @> 
@q ******* (7) |Scan_Parse::vector_type_assign| failed.@> 

@ Error handling:  |Scan_Parse::vector_type_assign| 
failed. 
\initials{LDF 2005.10.30.}

@<Define rules@>=
                            

    if (status != 0)
         {

            delete pv;

            @=$$@> = static_cast<void*>(0);

         } /* |if (status != 0)|  */

@q ******* (7) |Scan_Parse::vector_type_assign| succeeded.@> 

@ |Scan_Parse::vector_type_assign| succeeded. 
\initials{LDF 2005.10.30.}

@<Define rules@>=

   else /* |status == 0|  */
      {
         delete pv;

         @=$$@> = static_cast<void*>(entry->object); 
 
      }  /* |else| (|status == 0|)  */


   }   /* |else| (|entry != static_cast<Id_Map_Entry_Node>(0)|)  */


@q ****** (6).@> 

}; 


@q **** (4) |origami_figure_vector_assignment|.  @>
@*2 \§origami figure vector assignment>. 

\LOG
\initials{LDF 2005.02.03.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> origami_figure_vector_assignment@>

@q ***** (5) origami_figure_vector_assignment -->  @>  
@q ***** (5) origami_figure_vector_variable ASSIGN @>  
@q ***** (5) origami_figure_vector_expression.     @> 

@*3 \§origami figure vector assignment>
$\longrightarrow$ \§origami figure vector variable> 
\.{ASSIGN} \§origami figure vector expression>.      

\LOG
\initials{LDF 2005.02.03.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=

@=origami_figure_vector_assignment: origami_figure_vector_variable @>  
@=ASSIGN origami_figure_vector_expression@>@/
{

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ****** (6) Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0)|.@>

@ Error handling for the case that |entry == static_cast<Id_Map_Entry_Node>(0) |.
\initials{LDF 2005.02.03.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == static_cast<Id_Map_Entry_Node>(0))|  */

@q ****** (6) |entry != static_cast<Id_Map_Entry_Node>(0)|.@>   

@ |entry != static_cast<Id_Map_Entry_Node>(0)|.
\initials{LDF 2005.02.03.}

@<Define rules@>=

  else /* |entry != static_cast<Id_Map_Entry_Node>(0)|  */
    {

   typedef Pointer_Vector<Origami_Figure> PV;
 
   PV* pv = static_cast<PV*>(@=$3@>);

   PV* entry_pv = static_cast<PV*>(entry->object);

   if (entry_pv)
     entry_pv->clear();

   int status = vector_type_assign<Origami_Figure, Origami_Figure>(
                   static_cast<Scanner_Node>(parameter),
                                           entry,
                                           pv);

@q ******* (7) Error handling:                           @> 
@q ******* (7) |Scan_Parse::vector_type_assign| failed.@> 

@ Error handling:  |Scan_Parse::vector_type_assign| 
failed. 
\initials{LDF 2005.02.03.}

@<Define rules@>=
                            

    if (status != 0)
         {

            delete pv;

            @=$$@> = static_cast<void*>(0);

         } /* |if (status != 0)|  */

@q ******* (7) |Scan_Parse::vector_type_assign| succeeded.@> 

@ |Scan_Parse::vector_type_assign| succeeded. 
\initials{LDF 2005.02.03.}

@<Define rules@>=

   else /* |status == 0|  */
      {
         delete pv;

         @=$$@> = static_cast<void*>(entry->object); 
 
      }  /* |else| (|status == 0|)  */


   }   /* |else| (|entry != static_cast<Id_Map_Entry_Node>(0)|)  */


@q ****** (6).@> 

}; 


@q * (1) |assignment_command|.@>
@** \§assignment command>. 
\initials{LDF Undated.}

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> assignment_command@>

@q ** (2) assignment_command --> intersection_assignment_command.  @>
@* \§assignment command> $\longrightarrow$ 
\§intersection assignment command>.


\LOG
\initials{LDF 2004.09.05.}
Added this rule.

\initials{LDF 2004.11.05.}
Commented-out this rule.
\ENDLOG

@<Garbage@>= 

@q <Define rules@>
 
#if 0 
@=assignment_command: intersection_assignment_command@>
{

  @=$$@> = @=$1@>;
   
};

#endif 

@q ** (2) assignment_command --> transformation_assignment_command.@>
@* \§assignment command> $\longrightarrow$ 
\§transformation assignment command>.


\LOG
\initials{LDF 2004.10.01.}
Added this rule.

\initials{LDF 2004.10.01.}
Now deleting |transformation_assignment_command|.
\ENDLOG
 
@<Define rules@>= 
 
@=assignment_command: transformation_assignment_command@>
{

  delete static_cast<Transform*>(@=$1@>); 

  @=$$@> = static_cast<void*>(0);
   
};

@q * @>

@q   Emacs-Lisp code for use in indirect buffers when using the          @>
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
@q run-cweave-on-file:"3DLDFprg.web"  @>
@q End: @>
