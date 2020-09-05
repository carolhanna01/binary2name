@q pplnexpr.w @> 
@q Created by Laurence Finston Sun Oct 30 14:25:12 CET 2005 @>
       
@q * (0) Copyright and License.@>

@q This file is part of GNU 3DLDF, a package for three-dimensional drawing. @>
@q Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, @>
@q 2011, 2012, 2013 The Free Software Foundation  @>

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



@q * (0) plane expressions.  @>
@** plane expressions.
\initials{LDF 2005.10.30.}

\LOG
\initials{LDF 2005.10.30.}
Created this file.
\ENDLOG 

@q * (1) plane_primary.  @>
@* \§plane primary>.
\initials{LDF 2005.10.30.}  

\LOG
\initials{LDF 2005.10.30.}
Added this type declaration.
\ENDLOG 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> plane_primary@>@/

@q ** (2) plane_primary --> plane_variable.@>
@*1 \§plane primary> $\longrightarrow$ \§plane variable>.  

\LOG
\initials{LDF 2005.10.30.}
Added this rule.
\ENDLOG 

@q *** (3) Definition.@> 

@<Define rules@>=
@=plane_primary: plane_variable@>@/
{

  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

  if (entry == static_cast<Id_Map_Entry_Node>(0) || entry->object == static_cast<void*>(0))
    {

      @=$$@> = static_cast<void*>(0);

    } /* |if (entry == 0 || entry->object == 0)|  */

  else /* |entry != 0 && entry->object != 0|  */

    @=$$@> = static_cast<void*>(create_new<Plane>(
                                  static_cast<Plane*>(
                                     entry->object))); 

};

@q *** (3) plane_primary --> LEFT_PARENTHESIS point_expression COMMA @>
@q *** (3) point_expression COMMA point_expression RIGHT_PARENTHESIS.@>

@*2 \§plane primary> $\longrightarrow$ etc.
%
\§plane primary> $\longrightarrow$ \.{\LP} 
\§point expression>  \.{COMMA} \§point expression>  \.{COMMA}
\§point expression>  \.{\RP}.
\initials{LDF 2007.10.06.}

\LOG
\initials{LDF 2007.10.06.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=plane_primary: LEFT_PARENTHESIS point_expression COMMA@>@/
@=point_expression COMMA point_expression RIGHT_PARENTHESIS@>@/
{

   Point* p0 = static_cast<Point*>(@=$2@>);
   Point* p1 = static_cast<Point*>(@=$4@>);
   Point* p2 = static_cast<Point*>(@=$6@>);
  
   if (p0 && p1 && p2)
   {
      Path q;
      q += *p0;
      q += *p1;
      q += *p2;
  
      Plane v = q.get_plane();

      if (v == INVALID_PLANE)
         @=$$@> = static_cast<void*>(0);
      else
      {
          @=$$@> = static_cast<void*>(new Plane(v));  
      }
 
   }  /* Outer |if|  */

   else
   {
      @=$$@> = static_cast<void*>(0);
   }

   delete p0;
   delete p1;
   delete p2;

};

@q ** (2) Get |Plane| from planar |path| expressions.@> 
@*1 Get |Plane| from planar {\bf path} expressions.
\initials{LDF 2005.10.30.}

\LOG
\initials{LDF 2005.10.30.}
Added this section.

\initials{LDF 2007.10.02.}
Changed all of these rules so that they use \.{GET\_PLANE} instead 
of ``{\LP} \.{PLANE\_DECLARATOR} {\RP}''.
\ENDLOG

@q *** (3) plane_primary --> GET_PLANE path_expression.@>
@*2 \§plane primary> $\longrightarrow$ \.{GET\_PLANE} \§path expression>.
\initials{LDF 2005.10.30.}

\LOG
\initials{LDF 2005.10.30.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=plane_primary: GET_PLANE path_expression@>@/
{

   @=$$@> = Scan_Parse::plane_cast_func<Path>(
                                          static_cast<Path*>(@=$2@>), 
                                          parameter);

};

@q *** (3) plane_primary --> GET_PLANE ellipse_expression.@>
@*2 \§plane primary> $\longrightarrow$ \.{GET\_PLANE} \§ellipse expression>.
\initials{LDF 2005.10.30.}

\LOG
\initials{LDF 2005.10.30.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=plane_primary: GET_PLANE ellipse_expression@>@/
{

   @=$$@> = Scan_Parse::plane_cast_func<Ellipse>(
                                          static_cast<Ellipse*>(@=$2@>), 
                                          parameter);

};

@q *** (3) plane_primary --> GET_PLANE circle_expression.@>
@*2 \§plane primary> $\longrightarrow$ \.{GET\_PLANE} \§circle expression>.
\initials{LDF 2005.10.30.}

\LOG
\initials{LDF 2005.10.30.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=plane_primary: GET_PLANE circle_expression@>@/
{

   @=$$@> = Scan_Parse::plane_cast_func<Circle>(
                                          static_cast<Circle*>(@=$2@>), 
                                          parameter);

};

@q *** (3) plane_primary --> GET_PLANE polygon_expression.@>
@*2 \§plane primary> $\longrightarrow$ \.{GET\_PLANE} \§polygon expression>.
\initials{LDF 2005.10.30.}

\LOG
\initials{LDF 2005.10.30.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=plane_primary: GET_PLANE polygon_expression@>@/
{

   @=$$@> = Scan_Parse::plane_cast_func<Polygon>(
                                          static_cast<Polygon*>(@=$2@>), 
                                          parameter);

};

@q *** (3) plane_primary --> GET_PLANE reg_polygon_expression.@>
@*2 \§plane primary> $\longrightarrow$ \.{GET\_PLANE} 
\§regular polygon expression>.
\initials{LDF 2005.10.30.}

\LOG
\initials{LDF 2005.10.30.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=plane_primary: GET_PLANE reg_polygon_expression@>@/
{

   @=$$@> = Scan_Parse::plane_cast_func<Reg_Polygon>(
                                          static_cast<Reg_Polygon*>(@=$2@>), 
                                          parameter);

};

@q *** (3) plane_primary --> GET_PLANE rectangle_expression.@>
@*2 \§plane primary> $\longrightarrow$ 
\.{GET\_PLANE}
\§rectangle expression>.
\initials{LDF 2005.10.30.}

\LOG
\initials{LDF 2005.10.30.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=plane_primary: GET_PLANE rectangle_expression@>@/
{

   @=$$@> = Scan_Parse::plane_cast_func<Rectangle>(
                                          static_cast<Rectangle*>(@=$2@>), 
                                          parameter);

};

@q *** (3) plane_primary --> GET_PLANE triangle_expression.@>
@*2 \§plane primary> $\longrightarrow$ 
\.{GET\_PLANE}
\§triangle expression>.
\initials{LDF 2005.10.30.}

\LOG
\initials{LDF 2005.10.30.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=plane_primary: GET_PLANE triangle_expression@>@/
{

   @=$$@> = Scan_Parse::plane_cast_func<Triangle>(
                                          static_cast<Triangle*>(@=$2@>), 
                                          parameter);

};

@q ** (2) plane_primary --> LEFT_PARENTHESIS plane_expression @> 
@q ** (2) RIGHT_PARENTHESIS                                   @>

@*1 \§plane primary> $\longrightarrow$ \.{\LP} 
\§plane expression> \.{\RP}.

\LOG
\initials{LDF 2005.10.30.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=plane_primary: LEFT_PARENTHESIS plane_expression RIGHT_PARENTHESIS@>@/
{

  @=$$@> = @=$2@>;

};


@q ***** (5) plane_primary --> LAST @>
@q ***** (5) plane_vector_expression.@>

@*4 \§plane primary> $\longrightarrow$ 
\.{LAST} \§plane vector expression>.
\initials{LDF 2005.10.30.}

\LOG
\initials{LDF 2005.10.30.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=plane_primary: LAST plane_vector_expression@>@/
{ 

   Plane* c = create_new<Plane>(0);

   Pointer_Vector<Plane>* pv 
      = static_cast<Pointer_Vector<Plane>*>(@=$2@>);

@q ******* (7) Error handling:  |pv == 0|.@> 

@ Error handling:  |pv == 0|.
\initials{LDF 2005.10.30.}

@<Define rules@>=

   if (pv == static_cast<Pointer_Vector<Plane>*>(0))
      {
#if 0 
          cerr_strm << thread_name 
                    << "ERROR!  In `yyparse()', rule "
                    << endl 
                    << "`plane_primary "
                    << "--> LAST plane_vector_expression':"
                    << endl << "Invalid `plane_vector_expression'.  "
                    << "Setting `plane_primary' to 0"
                    << endl << "and will try to continue.";

          log_message(cerr_strm);
          cerr_message(cerr_strm, error_stop_value);
          cerr_strm.str("");
#endif 
          delete c;
          c = 0;

          @=$$@> = static_cast<void*>(0);

      }  /* |if (pv == 0)|  */

@q ******* (7) Error handling:  |pv->ctr == 0|.@> 

@ Error handling:  |pv->ctr == 0|.
\initials{LDF 2005.10.30.}

@<Define rules@>=

   else if (pv->ctr == 0)
      {
#if 0 
          cerr_strm << thread_name 
                    << "ERROR!  In `yyparse()', rule "
                    << endl 
                    << "`plane_primary "
                    << "--> LAST plane_vector_expression':"
                    << endl << "`plane_vector_expression' is empty.  "
                    << "Setting `plane_primary' to 0"
                    << endl << "and will try to continue.";

          log_message(cerr_strm);
          cerr_message(cerr_strm, error_stop_value);
          cerr_strm.str("");
#endif 
          delete c;
          c = 0;

          @=$$@> = static_cast<void*>(0);

      }  /* |else if (pv->ctr == 0)|  */

@q ******* (7) |pv != 0 && pv->ctr > 0|.@> 

@ |pv != 0 && pv->ctr > 0|.  Set |@=$$@>| to |*(pv->v[pv->ctr - 1])|.
\initials{LDF 2005.10.30.}

@<Define rules@>=

   else 
      {
         *c = *(pv->v[pv->ctr - 1]);
         @=$$@> = static_cast<void*>(c); 
      }
@q ******* (7) @> 

};

@q * (1) plane_secondary.  @>
@* \§plane secondary>.
\initials{LDF 2005.10.30.}

\LOG
\initials{LDF 2005.10.30.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> plane_secondary@>
  
@q ** (2) plane secondary --> plane_primary.@>
@*1 \§plane secondary> $\longrightarrow$ \§plane primary>.

\LOG
\initials{LDF 2005.10.30.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=plane_secondary: plane_primary@>@/
{
#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @;
  if (DEBUG)
    {
      cerr << "\n*** Parser: plane_secondary --> plane_primary "
           << endl;
    }
#endif /* |DEBUG_COMPILE|  */@;

  @=$$@> = @=$1@>;

};

@q * (1) plane tertiary.@>
@* \§plane tertiary>.
\initials{LDF 2005.10.30.}

\LOG
\initials{LDF 2005.10.30.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> plane_tertiary@>

@q ** (2) plane tertiary --> plane_secondary.  @>
@*1 \§plane tertiary> $\longrightarrow$ \§plane secondary>.
\initials{LDF 2005.10.30.}

\LOG
\initials{LDF 2005.10.30.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=plane_tertiary: plane_secondary@>@/
{

  @=$$@> = @=$1@>;

};



@q * (1) plane expression.@>
@* \§plane expression>.
\initials{LDF 2005.10.30.}

\LOG
\initials{LDF 2005.10.30.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> plane_expression@>

@q ** (2) plane expression --> plane_tertiary.  @>
@*1 \§plane expression> $\longrightarrow$ \§plane tertiary>.

\LOG
\initials{LDF 2005.10.30.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=plane_expression: plane_tertiary@>@/
{

  @=$$@> = @=$1@>;

};


@q * Emacs-Lisp code for use in indirect buffers when using the          @>
@q   GNU Emacs editor.  The local variable list is not evaluated when an @>
@q   indirect buffer is visited, so it's necessary to evaluate the       @>
@q   following s-expression in order to use the facilities normally      @>
@q   accessed via the local variables list.                              @>
@q   \initials{LDF 2004.02.12}.                                          @>
@q   (progn (cweb-mode) (outline-minor-mode t) (setq fill-column 70))    @>


@q Local Variables:                   @>
@q mode:CWEB                          @>
@q eval:(outline-minor-mode t)        @>
@q abbrev-file-name:"~/.abbrev_defs"  @>
@q eval:(read-abbrev-file)            @>
@q fill-column:70                     @>
@q run-cweave-on-file:"3DLDFprg.web"  @>
@q End:                               @>

