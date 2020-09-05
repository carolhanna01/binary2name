@q ppygexpr.w @> 
@q Created by Laurence Finston Fr Feb 11 13:25:13 CET 2005 @>
       
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



@q * (0) polygon expressions.  @>
@** \§polygon expressions>.
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.02.11.}
Created this file.
\ENDLOG 

@q * (1) polygon primary.  @>
@* \§polygon primary>.
\initials{LDF 2005.02.11.}
  
\LOG
\initials{LDF 2005.02.11.}  
Added this type declaration.
\ENDLOG 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> polygon_primary@>@/

@q ** (2) polygon_primary --> polygon_variable.@>
@*1 \§polygon primary> $\longrightarrow$ \§polygon variable>.  
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.02.11.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=polygon_primary: polygon_variable@>@/
{

  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

  if (entry == static_cast<Id_Map_Entry_Node>(0) || entry->object == static_cast<void*>(0))
    {

      @=$$@> = static_cast<void*>(0);

    } /* |if (entry == 0 || entry->object == 0)|  */

  else /* |entry != 0 && entry->object != 0|  */

    @=$$@> = static_cast<void*>(create_new<Polygon>(
                                static_cast<Polygon*>(
                                   entry->object))); 
};

@q ** (2) polygon_primary --> LEFT_PARENTHESIS polygon_expression @>
@q ** (2) RIGHT_PARENTHESIS.                                      @> 

@*1 \§polygon primary> $\longrightarrow$ \.{LEFT\_PARENTHESIS} 
\§polygon expression> \.{RIGHT\_PARENTHESIS}.
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.02.11.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=polygon_primary: LEFT_PARENTHESIS polygon_expression RIGHT_PARENTHESIS@>@/
{
   @=$$@> = @=$2@>;

};

@q ** (2) polygon_primary --> GET_PENTAGON conic_section_lattice_expression.@>

@*1 \§polygon primary> $\longrightarrow$ \.{GET\_PENTAGON} 
\§conic section lattice expression>.
\initials{LDF 2007.08.19.}

\LOG
\initials{LDF 2007.08.19.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=polygon_primary: GET_PENTAGON conic_section_lattice_expression@>@/
{

   if (@=$2@> == static_cast<void*>(0))
      @=$$@> = static_cast<void*>(0);
   else
   {
      Conic_Section_Lattice* c = static_cast<Conic_Section_Lattice*>(@=$2@>);
      @=$$@> = c->get_pentagon(static_cast<Scanner_Node>(parameter));
      delete c;
      c = 0;
   }
};

@q ** (2) polygon_primary --> GET_HEXAGON conic_section_lattice_expression.@>

@*1 \§polygon primary> $\longrightarrow$ \.{GET\_HEXAGON} 
\§conic section lattice expression>.
\initials{LDF 2007.08.17.}

\LOG
\initials{LDF 2007.08.17.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=polygon_primary: GET_HEXAGON conic_section_lattice_expression@>@/
{

   if (@=$2@> == static_cast<void*>(0))
      @=$$@> = static_cast<void*>(0);
   else
   {
      Conic_Section_Lattice* c = static_cast<Conic_Section_Lattice*>(@=$2@>);
      @=$$@> = c->get_hexagon(static_cast<Scanner_Node>(parameter));
      delete c;
      c = 0;
   }
};

@q ** (2) polygon_primary --> GET_POLYGON conic_section_lattice_expression.@>

@*1 \§polygon primary> $\longrightarrow$ \.{GET\_POLYGON} 
\§conic section lattice expression>.
\initials{LDF 2007.08.19.}

\LOG
\initials{LDF 2007.08.19.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=polygon_primary: GET_POLYGON conic_section_lattice_expression@>@/
{

   if (@=$2@> == static_cast<void*>(0))
      @=$$@> = static_cast<void*>(0);
   else
   {
      Conic_Section_Lattice* c = static_cast<Conic_Section_Lattice*>(@=$2@>);
      @=$$@> = c->get_polygon(static_cast<Scanner_Node>(parameter));
      delete c;
      c = 0;
   }
};

@q ** (2) polygon_primary --> GET_RHOMBUS WITH_SIDE_LENGTH @>
@q ** (2) numeric_expression.                              @>

@*1 \§polygon primary> $\longrightarrow$ \.{GET\_RHOMBUS} 
\.{WITH\_SIDE\_LENGTH} \§numeric expression>.
\initials{LDF 2007.10.26.}

\LOG
\initials{LDF 2007.10.26.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=polygon_primary: GET_RHOMBUS WITH_SIDE_LENGTH numeric_expression@>@/
{

    @=$$@> = static_cast<void*>(
                Rhombic_Triacontahedron::get_rhombus(@=$3@>,
                   static_cast<Scanner_Node>(parameter))); 
};




@q ** (2) polygon_primary --> LAST @>
@q ** (2) polygon_vector_expression.@>

@*1 \§polygon primary> $\longrightarrow$ 
\.{LAST} \§polygon vector expression>.
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.02.11.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=polygon_primary: LAST polygon_vector_expression@>@/
{ 

   Polygon* r;


         r = create_new<Polygon>(0);

   Pointer_Vector<Polygon>* pv 
      = static_cast<Pointer_Vector<Polygon>*>(@=$2@>);

@q ******* (7) Error handling:  |pv == 0|.@> 

@ Error handling:  |pv == 0|.
\initials{LDF 2005.02.11.}

@<Define rules@>=

   if (pv == static_cast<Pointer_Vector<Polygon>*>(0))
      {

          delete r;

          @=$$@> = static_cast<void*>(0);

      }  /* |if (pv == 0)|  */

@q ******* (7) Error handling:  |pv->ctr == 0|.@> 

@ Error handling:  |pv->ctr == 0|.
\initials{LDF 2005.02.11.}

@<Define rules@>=

   else if (pv->ctr == 0)
      {
          delete r;

          @=$$@> = static_cast<void*>(0);

      }  /* |else if (pv->ctr == 0)|  */

@q ******* (7) |pv != 0 && pv->ctr > 0|.@> 

@ |pv != 0 && pv->ctr > 0|.  Set |@=$$@>| to |*(pv->v[pv->ctr - 1])|.
\initials{LDF 2005.02.11.}

@<Define rules@>=

   else 
      {
         *r = *(pv->v[pv->ctr - 1]);
         @=$$@> = static_cast<void*>(r); 
      }
@q ******* (7) @> 

};

@q ** (2) polygon_primary --> DISENTANGLE point_vector_primary.@>

@*1 \§polygon primary> $\longrightarrow$ \.{DISENTANGLE} \§point vector primary>.
\initials{LDF 2006.01.20.}

\LOG
\initials{LDF 2006.01.20.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=polygon_primary: DISENTANGLE point_vector_primary@>@/
{ 

    @=$$@> = Polygon::disentangle(@=$2@>, parameter);                                
};


@q * (1) polygon secondary.@>
@* \§polygon secondary>.
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.02.11.}  
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> polygon_secondary@>
  
@q ** (2) polygon secondary --> polygon_primary.@>
@*1 \§polygon secondary> $\longrightarrow$ \§polygon primary>.
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.02.11.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=polygon_secondary: polygon_primary@>@/
{

  @=$$@> = @=$1@>;

};

@q ** (2) polygon secondary --> polygon_secondary transformer.@>
@*1 \§polygon secondary> $\longrightarrow$ 
\§polygon secondary> \§transformer>.
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.02.11.}
Added this rule.

\initials{LDF 2006.01.20.}
@:BUG FIX@> BUG FIX:  Added code to this rule.  Previously, |@=$$@>| was 
simply set to |@=$1@>|, and the |Transform*| wasn't deleted.
\ENDLOG

@<Define rules@>=
@=polygon_secondary: polygon_secondary transformer@>@/
{

   Polygon* p = static_cast<Polygon*>(@=$1@>);
   Transform* t = static_cast<Transform*>(@=$2@>);

   *p *= *t;

   delete t; 

   @=$$@> = static_cast<void*>(p); 

};

@q ***** (5) polygon_secondary --> polygon_secondary PROJECTED focus_primary.@>

@*4 \§polygon secondary> $\longrightarrow$ 
\§polygon secondary> \.{PROJECTED} \§focus primary>.
\initials{LDF 2005.04.15.}

\LOG
\initials{LDF 2005.04.15.}
Added this rule.

\initials{LDF 2005.04.18.}
Changed |PROJECT| to |PROJECTED|.

\initials{LDF 2005.04.19.}
Changed this rule from 
\§polygon primary> $\longrightarrow$ 
\§polygon primary> \.{PROJECTED} \.{WITH\_FOCUS} \§focus expression>.
to 
\§polygon secondary> $\longrightarrow$ 
\§polygon-like secondary> \.{PROJECTED} \§focus primary>.

\initials{LDF 2005.04.19.}
Changed this rule from 
\§polygon secondary> $\longrightarrow$ 
\§polygon-like secondary> \.{PROJECTED} \§focus primary>
to 
\§polygon secondary> $\longrightarrow$ 
\§polygon secondary> \.{PROJECTED} \§focus primary>.
This was necessary because of reduce/reduce conflicts.

\initials{LDF 2005.04.19.}
Added \§with z optional>.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=polygon_secondary: polygon_secondary PROJECTED focus_primary with_z_optional@>@/
{ 
   @=$$@> = polygon_secondary_rule_func_0(@=$1@>, 
                                          @=$3@>,
                                          @=$4@>,
                                          static_cast<Scanner_Node>(parameter));

};


@q * (1)  polygon tertiary.@>
@* \§polygon tertiary>.
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.02.11.}  
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> polygon_tertiary@>

@q ** (2) polygon tertiary --> polygon_secondary.  @>
@*1 \§polygon tertiary> $\longrightarrow$ \§polygon secondary>.
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.02.11.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=polygon_tertiary: polygon_secondary@>@/
{

   @=$$@> = @=$1@>;

};

@q ** (2) polygon_tertiary --> cuboid_tertiary INTERSECTION plane_secondary. @>

@*1 \§polygon tertiary> $\longrightarrow$ \§cuboid tertiary> \.{INTERSECTION}
\§plane secondary>.
\initials{LDF 2005.12.11.}

\LOG
\initials{LDF 2005.12.11.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=polygon_tertiary: cuboid_tertiary INTERSECTION plane_secondary@>@/
{ 
   Cuboid* c = static_cast<Cuboid*>(@=$1@>);
   Plane*  p = static_cast<Plane*>(@=$3@>);

   if (!(c && p))
      @=$$@> = static_cast<void*>(0);
   else
     @=$$@> = static_cast<void*>(c->intersection(*p, 
                                                 false,  /* Don't suppress warnings.  */
                                                 static_cast<Scanner_Node>(parameter)));

   delete c;
   delete p;
   c = 0;
   p = 0; 

};

@q ** (2) polygon_tertiary --> plane_tertiary INTERSECTION cuboid_secondary. @>

@*1 \§polygon tertiary> $\longrightarrow$ \§plane tertiary> \.{INTERSECTION}
\§cuboid secondary>.
\initials{LDF 2005.12.11.}

\LOG
\initials{LDF 2005.12.11.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=polygon_tertiary: plane_tertiary INTERSECTION cuboid_secondary@>@/
{
   Cuboid* c = static_cast<Cuboid*>(@=$3@>);
   Plane*  p = static_cast<Plane*>(@=$1@>);

   if (!(c && p))
      @=$$@> = static_cast<void*>(0);
   else
     @=$$@> = static_cast<void*>(c->intersection(*p, 
                                                 false,  /* Don't suppress warnings.  */
                                                 static_cast<Scanner_Node>(parameter)));

   delete c;
   delete p;
   c = 0;
   p = 0; 

};

@q ** (2) polygon_tertiary --> polyhedron_tertiary INTERSECTION plane_secondary.@>

@*1 \§polygon tertiary> $\longrightarrow$ \§polyhedron tertiary> \.{INTERSECTION}
\§plane secondary>.
\initials{LDF 2006.01.20.}

\LOG
\initials{LDF 2006.01.20.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=polygon_tertiary: polyhedron_tertiary INTERSECTION plane_secondary@>@/
{ 
   Polyhedron* p = static_cast<Polyhedron*>(@=$1@>);
   Plane*  q = static_cast<Plane*>(@=$3@>);

   if (!(p && q))
      @=$$@> = static_cast<void*>(0);
   else
     @=$$@> = static_cast<void*>(p->intersection(*q, 
                                                 false,  /* Don't suppress warnings.  */
                                                 static_cast<Scanner_Node>(parameter)));

   delete p;
   delete q;
   p = 0;
   q = 0; 

};

@q ** (2) polygon_tertiary --> plane_tertiary INTERSECTION polyhedron_secondary. @>

@*1 \§polygon tertiary> $\longrightarrow$ \§plane tertiary> \.{INTERSECTION}
\§polyhedron secondary>.
\initials{LDF 2006.01.20.}

\LOG
\initials{LDF 2006.01.20.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=polygon_tertiary: plane_tertiary INTERSECTION polyhedron_secondary@>@/
{
   Polyhedron* p = static_cast<Polyhedron*>(@=$3@>);
   Plane*  q = static_cast<Plane*>(@=$1@>);

   if (!(p && q))
      @=$$@> = static_cast<void*>(0);
   else
     @=$$@> = static_cast<void*>(p->intersection(*q, 
                                                 false,  /* Don't suppress warnings.  */
                                                 static_cast<Scanner_Node>(parameter)));

   delete p;
   delete q;
   p = 0;
   q = 0; 

};





@q * (1) polygon expression.  @>
@* \§polygon expression>.
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.02.11.}  
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> polygon_expression@>

@q ** (2) polygon expression --> polygon_tertiary.  @>
@*1 \§polygon expression> $\longrightarrow$ \§polygon tertiary>.
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.02.11.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=polygon_expression: polygon_tertiary@>@/
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

