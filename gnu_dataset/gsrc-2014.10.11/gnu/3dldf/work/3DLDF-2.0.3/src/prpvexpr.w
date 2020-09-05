@q prpvexpr.w @> 
@q Created by Laurence Finston Tue Dec 14 20:39:36 CET 2004 @>
     
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

@q Please send bug reports to bug-3dldf@@gnu.org @>
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




@q * (0) |reg_polygon_vector| expressions.@>
@** \§regular polygon vector> expressions.
\initials{LDF 2004.12.14.}

\LOG
\initials{LDF 2004.12.14.}
Created this file and wrote quite a few rules.  
\ENDLOG 


@q * (1) |reg_polygon_vector| primary.  @>
@* \§regular polygon vector primary>.
\initials{LDF 2004.12.14.}
  
\LOG
\initials{LDF 2004.12.14.}
Added this type declaration.
\ENDLOG


@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> reg_polygon_vector_primary@>@/

@q ** (2) reg_polygon_vector_primary --> reg_polygon_vector_variable.@>
@*1 \§regular polygon vector primary> $\longrightarrow$ 
\§regular polygon vector variable>.  
\initials{LDF 2004.12.14.}

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=reg_polygon_vector_primary: reg_polygon_vector_variable@>@/ 
{
  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

@q **** (4) Error handling:  |entry == 0 || entry->object == 0|.@> 

@ Error handling:  |entry == 0 || entry->object == 0|.
\initials{LDF 2004.12.14.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0) || entry->object == static_cast<void*>(0))
    {

      @=$$@> = static_cast<void*>(0);

    } /* |if (entry == 0 || entry->object == 0)|  */

@q **** (4) |!(entry == 0 || entry->object == 0)|.@> 

@ |!(entry == 0 || entry->object == 0)|.
\initials{LDF 2004.12.14.}

@<Define rules@>=
  else /* |!(entry == 0 || entry->object == 0)|  */

     {

        typedef Pointer_Vector<Reg_Polygon> PV;

        PV* pv = new PV;

        *pv = *static_cast<PV*>(entry->object);

        @=$$@> = static_cast<void*>(pv);                    

     }  /* |else| (|!(entry == 0 || entry->object == 0)|)  */

};


@q ** (2) reg_polygon_vector_primary --> LEFT_PARENTHESIS  @>
@q ** (2) reg_polygon_vector_expression  RIGHT_PARENTHESIS.@>

@*1 \§regular polygon vector primary> $\longrightarrow$ \.{LEFT\_PARENTHESIS}
\§regular polygon vector expression> \.{RIGHT\_PARENTHESIS}.
\initials{LDF 2004.12.14.}

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=reg_polygon_vector_primary: LEFT_PARENTHESIS@>@/ 
@=reg_polygon_vector_expression RIGHT_PARENTHESIS@>@/ 
{
   
  @=$$@> = @=$1@>;

};

@q ** (2) reg_polygon_vector_primary --> GET_NET DODECAHEDRON.@>

@*1 \§regular polygon vector primary> $\longrightarrow$ \.{GET\_NET}
\.{DODECAHEDRON}.
\initials{LDF 2007.10.11.}

\LOG
\initials{LDF 2007.10.11.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=reg_polygon_vector_primary: GET_NET DODECAHEDRON@>@/
@=with_diameter_optional with_half_optional@>@/ 
{
   
    @=$$@> = polyhedron_get_net_func(
                DODECAHEDRON, @=$3@>, @=$4@>, parameter);

};


@q ** (2) reg_polygon_vector_primary --> GET_NET ICOSAHEDRON.@>

@*1 \§regular polygon vector primary> $\longrightarrow$ \.{GET\_NET}
\.{ICOSAHEDRON}.
\initials{2008.12.30.}

\LOG
\initials{2008.12.30.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=reg_polygon_vector_primary: GET_NET ICOSAHEDRON@>@/
@=with_diameter_optional with_half_optional@>@/ 
{
   
    @=$$@> = polyhedron_get_net_func(
                ICOSAHEDRON, @=$3@>, @=$4@>, parameter);

};


@q ** (2) reg_polygon_vector_primary --> GET_NET GREAT_RHOMBICOSIDODECAHEDRON.@>

@*1 \§regular polygon vector primary> $\longrightarrow$ \.{GET\_NET}
\.{GREAT\_RHOMBICOSIDODECAHEDRON}.
\initials{LDF 2005.12.20.}

\LOG
\initials{LDF 2005.12.20.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=reg_polygon_vector_primary: GET_NET GREAT_RHOMBICOSIDODECAHEDRON@>@/
@=with_diameter_optional with_half_optional@>@/ 
{
   
    @=$$@> = polyhedron_get_net_func(
                GREAT_RHOMBICOSIDODECAHEDRON, @=$3@>, @=$4@>, parameter);

};

@q ** (2) with_half_optional.@>
@*1 \§with half optional>.
\initials{LDF 2005.12.20.}

\LOG
\initials{LDF 2005.12.20.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <int_value> with_half_optional@>@/

@q *** (3) with_half_optional --> /* Empty  */@>

@*2 \§with half optional> $\longrightarrow$ \.{EMPTY}.
\initials{LDF 2005.12.20.}

\LOG
\initials{LDF 2005.12.20.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=with_half_optional: /* Empty  */@>@/
{
   
    @=$$@> = 0;

};

@q *** (3) with_half_optional --> WITH_HALF@>

@*2 \§with half optional> $\longrightarrow$ \.{WITH\_HALF}.
\initials{LDF 2005.12.20.}

\LOG
\initials{LDF 2005.12.20.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=with_half_optional: WITH_HALF@>@/
{
   
    @=$$@> = 1;

};




@q * (1) reg_polygon_vector secondary.@>

@* \§regular polygon vector secondary>.
\initials{LDF 2004.12.14.}

\LOG
\initials{LDF 2004.12.14.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> reg_polygon_vector_secondary@>
  
@q ** (2) reg_polygon_vector secondary --> reg_polygon_vector_primary.@>
@*1 \§regular polygon vector secondary> $\longrightarrow$ 
\§regular polygon vector primary>.
\initials{LDF 2004.12.14.}

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=reg_polygon_vector_secondary: reg_polygon_vector_primary@>@/ 
{

  @=$$@> = @=$1@>;

};

@q *** (3) reg_polygon_vector_secondary --> reg_polygon_secondary REFLECTED_OFF @>
@q *** (3) path_expression WITH_DIRECTION point_expression.  @>

@*2 \§regular polygon vector secondary> $\longrightarrow$ \§regular polygon secondary> 
\.{REFLECTED\_OFF} \§path expression> \.{WITH\_DIRECTION} \§point expression>.

\LOG
\initials{LDF 2004.12.14.}
Added this rule.

\initials{LDF 2005.10.24.}
Changed |path_like_expression| to |path_expression|.
Removed debugging code.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=reg_polygon_vector_secondary: reg_polygon_secondary REFLECTED_OFF @>
@=path_expression WITH_DIRECTION point_expression@>@/ 
{

    Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

    typedef Pointer_Vector<Reg_Polygon> PV;

    PV* pv = new PV;

    *pv += create_new<Reg_Polygon>(0, scanner_node);
    *pv += create_new<Reg_Polygon>(0, scanner_node);
    *pv += create_new<Reg_Polygon>(0, scanner_node);
    *pv += create_new<Reg_Polygon>(0, scanner_node);

@q ******* (7) Call |Scan_Parse::reflect_off_func<Reg_Polygon>()|.@> 

@ Call |Scan_Parse::reflect_off_func<Reg_Polygon>()|.
\initials{LDF 2004.12.14.}

@<Define rules@>=

   int status = reflect_off_func<Reg_Polygon>(scanner_node,
                                              static_cast<Reg_Polygon*>(@=$1@>), 
                                              static_cast<Reg_Polygon*>(@=$3@>), 
                                              static_cast<Point*>(@=$5@>), 
                                              pv);          

@q ******** (8) Error handling:  |status != 0|.         @> 
@q ******** (8) |Scan_Parse::reflect_off_func()| failed.@> 

@ Error handling:  |status != 0|.  
|Scan_Parse::reflect_off_func()| failed.
\initials{LDF 2004.12.14.}

@<Define rules@>=                        
  
  if (status != 0)
     {
       
       @=$$@> = static_cast<void*>(0);

     } /* |if (status != 0)|  */
      
@q ******** (8) |Scan_Parse::reflect_off_func()| succeeded.@> 

@ |Scan_Parse::reflect_off_func()| succeeded.
\initials{LDF 2004.12.14.}

@<Define rules@>=                        


  else /* |status == 0|  */
     {

          @=$$@> = static_cast<void*>(pv);

     }  /* |else| (|status == 0|)  */

@q ******* (7).@> 

};


@q * (1) reg_polygon_vector tertiary.  @>

@* \§regular polygon vector tertiary>.
\initials{LDF 2004.12.14.}

\LOG
\initials{LDF 2004.12.14.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> reg_polygon_vector_tertiary@>

@q ***** (5) reg_polygon_vector tertiary --> reg_polygon_vector_secondary.@>
@ \§regular polygon vector tertiary> $\longrightarrow$ 
\§regular polygon vector secondary>.
\initials{LDF 2004.12.14.}

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=reg_polygon_vector_tertiary: reg_polygon_vector_secondary@>@/ 
{

  @=$$@> = @=$1@>;

};


@q * (1) reg_polygon_vector expression.@>
@* \§regular polygon vector expression>.

\LOG
\initials{LDF 2004.12.14.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> reg_polygon_vector_expression@>

@q ** (2) reg_polygon_vector expression --> reg_polygon_vector_tertiary.  @>
@*1 \§regular polygon vector expression> $\longrightarrow$ 
\§regular polygon vector tertiary>.

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=reg_polygon_vector_expression: reg_polygon_vector_tertiary@>@/ 
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


