@q pcirexpr.w @> 
@q Created by Laurence Finston Thu May  6 15:21:19 MEST 2004  @>
       
@q * (1) Copyright and License.@>

@q This file is part of GNU 3DLDF, a package for three-dimensional drawing.  @>
@q Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, @>
@q 2011, 2012, 2013 The Free Software Foundation  @>

@q GNU 3DLDF is free software; you can redistribute it and/or modify @>
@q it under the terms of the GNU General Public License as published by @>
@q the Free Software Foundation; either version 3 of the License, or @>
@q (at your option) any later version.  @>

@q GNU 3DLDF is distributed in the hope that it will be useful, @>
@q but WITHOUT ANY WARRANTY; without even the implied warranty of @>
@q MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the @>
@q GNU General Public License for more details.  @>

@q You should have received a copy of the GNU General Public License @>
@q along with GNU 3DLDF; if not, write to the Free Software @>
@q Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA@>

@q GNU 3DLDF is a GNU package.  @>
@q It is part of the GNU Project of the  @>
@q Free Software Foundation @>
@q and is published under the GNU General Public License. @>
@q See the website http://www.gnu.org @>
@q for more information.   @>
@q GNU 3DLDF is available for downloading from @>
@q http://www.gnu.org/software/3dldf/LDF.html.@>

@q ("@@" stands for a single at-sign in the following paragraph.) @>

@q Please send bug reports to Laurence.Finston@@gmx.de@>
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

@q Laurence.Finston@@gmx.de (@@ stands for a single "at" sign.)@>



@q * (0) circle expressions.  @>
@** circle expressions.

\LOG
\initials{LDF 2004.06.17.}
Created this file.
\ENDLOG 


@q * (1) circle primary.  @>
@* \§circle primary>.
  
\LOG
\initials{LDF 2004.06.17.}  
Added this type declaration.
\ENDLOG 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> circle_primary@>@/

@q ** (2) circle_primary --> circle_variable.@>
@*1 \§circle primary> $\longrightarrow$ \§circle variable>.  

\LOG
\initials{LDF 2004.06.17.}
Added this rule.

\initials{LDF 2004.11.22.}
No longer issuing an error message if |entry->object == 0|.
This condition occurs legitimately when one tries to show
an ``unknown |circle|''.
\ENDLOG 

@q *** (3) Definition.@> 

@<Define rules@>=
@=circle_primary: circle_variable@>@/
{

  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

  if (entry == static_cast<Id_Map_Entry_Node>(0) || entry->object == static_cast<void*>(0))
    {

      @=$$@> = static_cast<void*>(0);

    } /* |if (entry == 0 || entry->object == 0)|  */

  else /* |entry != 0 && entry->object != 0|  */

    @=$$@> = static_cast<void*>(create_new<Circle>(
                                  static_cast<Circle*>(
                                     entry->object))); 

};

@q ** (2) circle_primary --> circle_argument.@>
@*1 \§circle primary> $\longrightarrow$ \§circle argument>.  


@q ** (2) circle_primary --> ( circle_expression )  @>
@*1 \§circle primary> $\longrightarrow$ `\.{\LP}' 
\§circle expression> `\.{\RP}'.

\LOG
\initials{LDF 2004.06.17.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=circle_primary: LEFT_PARENTHESIS circle_expression RIGHT_PARENTHESIS@>@/
{

  @=$$@> = @=$2@>;

};


@q ** (2) circle_primary --> GET_CIRCLE numeric_primary sphere_primary@>
@*1 \§circle primary> $\longrightarrow$ \.{GET\_CIRCLE}
\§numeric primary> \§sphere primary>. 

\LOG
\initials{LDF 2004.10.13.}
Added this rule.

\initials{LDF 2009.09.09.}
Changed |@=$2@>| from \.{INTEGER} to \§numeric primary>.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=circle_primary: GET_CIRCLE numeric_primary sphere_primary@>@/
{


  Sphere* s = static_cast<Sphere*>(@=$3@>);

  Circle* c;

  const Circle* t = s->get_circle_ptr(@=$2@>); 


@q **** (4) Error handling:  |Sphere::get_circle_ptr()| returned 0.@>   

@ Error handling:  |Sphere::get_circle_ptr()| returned 0.
\initials{LDF 2004.10.13.}

@<Define rules@>=

  if (t == static_cast<const Circle*>(0))
   {

      delete s;

      @=$$@> = static_cast<void*>(0);

   } /* |if (t == 0)|  */


@q **** (4) Try to allocate memory on free store for |circle_primary|.@>   

@ Try to allocate memory on free store for \§circle primary>.
\initials{LDF 2004.10.13.}

@<Define rules@>=

  else /* |t != 0|  */
     {

         c = create_new<Circle>(t, static_cast<Scanner_Node>(parameter));


@q **** (4) Delete |s|, set |circle_primary| to                 @>   
@q **** (4) |static_cast<void*>(c)|, and exit rule successfully.@>   

@ Delete |s|, set |circle_primary| to                 
|static_cast<void*>(c)|, and exit rule successfully.   
\initials{LDF 2004.10.13.}

@<Define rules@>=

  delete s;

  @=$$@> = static_cast<void*>(c);

  }  /* |else| (|t != 0|)  */

};

@q ** (2) circle_primary --> GET_CIRCLE INTEGER cone_primary@>
@*1 \§circle primary> $\longrightarrow$ \.{GET\_CIRCLE}
INTEGER \§cone primary>. 
\initials{LDF 2006.11.09.}

\LOG
\initials{LDF 2006.11.09.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=circle_primary: GET_CIRCLE INTEGER cone_primary@>@/
{

   @=$$@> = quad_surf_get_element_func(parameter, 
                                       GET_CIRCLE, 
                                       Shape::CONE_TYPE, 
                                       @=$3@>,
                                       @=$2@>);


};

@q ** (2) circle_primary --> GET_CIRCLE conic_section_lattice_expression@>
@*1 \§circle primary> $\longrightarrow$ \.{GET\_CIRCLE}
\§conic section lattice expression>.
\initials{LDF 2007.09.26.}

\LOG
\initials{LDF 2007.09.26.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=circle_primary: GET_CIRCLE conic_section_lattice_expression@>@/ 
{

   Conic_Section_Lattice* c = static_cast<Conic_Section_Lattice*>(@=$2@>);

   @=$$@> = (c) ?  c->get_circle(static_cast<Scanner_Node>(parameter))
                :  @=$$@> = static_cast<void*>(0);

};

@q ** (2) circle_primary --> IN_CIRCLE reg_polygon_primary@>
@*1 \§circle primary> $\longrightarrow$ \.{IN\_CIRCLE}
\§regular polygon primary>. 

\LOG
\initials{LDF 2004.11.16.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=circle_primary: IN_CIRCLE reg_polygon_primary@>@/
{

   Reg_Polygon* r = static_cast<Reg_Polygon*>(@=$2@>);

   Circle* c;

   c = create_new<Circle>(0);

   *c = r->in_circle();
   
   @=$$@> = static_cast<void*>(c); 

};

@q ** (2) circle_primary --> OUT_CIRCLE reg_polygon_primary@>
@*1 \§circle primary> $\longrightarrow$ \.{OUT\_CIRCLE}
\§regular polygon primary>. 

\LOG
\initials{LDF 2004.11.16.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=circle_primary: OUT_CIRCLE reg_polygon_primary@>@/
{

   Reg_Polygon* r = static_cast<Reg_Polygon*>(@=$2@>);

   Circle* c;


         c = create_new<Circle>(0);

   *c = r->out_circle();
   
   @=$$@> = static_cast<void*>(c); 

};

@q ***** (5) circle_primary --> LAST @>
@q ***** (5) circle_vector_expression.@>

@*4 \§circle primary> $\longrightarrow$ 
\.{LAST} \§circle vector expression>.
\initials{LDF 2005.01.14.}

\LOG
\initials{LDF 2005.01.14.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=circle_primary: LAST circle_vector_expression@>@/
{ 
   Circle* c;


         c = create_new<Circle>(0);

   Pointer_Vector<Circle>* pv 
      = static_cast<Pointer_Vector<Circle>*>(@=$2@>);

@q ******* (7) Error handling:  |pv == 0|.@> 

@ Error handling:  |pv == 0|.
\initials{LDF 2005.01.14.}

@<Define rules@>=

   if (pv == static_cast<Pointer_Vector<Circle>*>(0))
      {

          delete c;

          @=$$@> = static_cast<void*>(0);

      }  /* |if (pv == 0)|  */

@q ******* (7) Error handling:  |pv->ctr == 0|.@> 

@ Error handling:  |pv->ctr == 0|.
\initials{LDF 2005.01.14.}

@<Define rules@>=

   else if (pv->ctr == 0)
      {

          delete c;

          @=$$@> = static_cast<void*>(0);

      }  /* |else if (pv->ctr == 0)|  */

@q ******* (7) |pv != 0 && pv->ctr > 0|.@> 

@ |pv != 0 && pv->ctr > 0|.  Set |@=$$@>| to |*(pv->v[pv->ctr - 1])|.
\initials{LDF 2005.01.14.}

@<Define rules@>=

   else 
      {
         *c = *(pv->v[pv->ctr - 1]);
         @=$$@> = static_cast<void*>(c); 
      }
@q ******* (7) @> 

};




@q * (1) circle secondary.  @>
@* \§circle secondary>.
\initials{LDF 2004.06.17.}  

\LOG
\initials{LDF 2004.06.17.}  
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> circle_secondary@>
  
@q ** (2) circle secondary --> circle_primary.@>
@*1 \§circle secondary> $\longrightarrow$ \§circle primary>.

\LOG
\initials{LDF 2004.06.17.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=circle_secondary: circle_primary@>@/
{
#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @;
  if (DEBUG)
    {
      cerr << "\n*** Parser: circle_secondary --> circle_primary "
           << endl;
    }
#endif /* |DEBUG_COMPILE|  */@;

  @=$$@> = @=$1@>;

};

@q ** (2) circle secondary --> circle_secondary transformer.  @>
@*1 \§circle secondary> $\longrightarrow$ \§circle secondary> 
\§transformer>.
\initials{LDF 2004.06.17.}


\LOG
\initials{LDF 2004.06.17.}
Added this rule.

\initials{LDF 2004.10.05.}
Made the debugging output thread-safe.

\initials{LDF 2005.12.16.}
@:BUG FIX@> BUG FIX: Now deleting |Transform* t|.
\ENDLOG

@<Define rules@>=
@=circle_secondary: circle_secondary transformer@>@/
{

   Circle* p = static_cast<Circle*>(@=$1@>);
   Transform* t = static_cast<Transform*>(@=$2@>);

   *p *= *t;

   delete t;

   @=$$@> = static_cast<void*>(p); 

};

@q ** (2) circle secondary --> circle_secondary REFLECTED_IN @>
@q ** (2) path_expression.@> 

@*1 \§circle secondary> $\longrightarrow$ 
\§circle secondary> 
\.{REFLECTED\_IN} \§path expression>.

\LOG
\initials{LDF 2004.10.05.}
Added this rule.

\initials{LDF 2004.10.09.}
Changed \§optional in> to \.{IN}.

\initials{LDF 2004.12.03.}
Changed |REFLECTED IN| to |REFLECTED_IN|.

\initials{LDF 2005.10.24.}
Changed |path_like_expression| to |path_expression|.
Removed debugging code.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=circle_secondary: circle_secondary REFLECTED_IN @>
@=path_expression@>@/ 
{

     Circle* c = reflect_in_func<Circle>(static_cast<Scanner_Node>(parameter),
                                         static_cast<Circle*>(@=$1@>),
                                         static_cast<Path*>(@=$3@>));

     @=$$@> = static_cast<void*>(c);

};

@q * (1) circle tertiary.@>
@* \§circle tertiary>.


\LOG
\initials{LDF 2004.06.17.} 
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> circle_tertiary@>

@q ** (2) circle tertiary --> circle_secondary.  @>
@*1 \§circle tertiary> $\longrightarrow$ \§circle secondary>.


\LOG
\initials{LDF 2004.06.17.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=circle_tertiary: circle_secondary@>@/
{
  @=$$@> = @=$1@>;
};


@q **** (4) circle_tertiary: sphere_tertiary @> 
@q **** (4) INTERSECTION plane_secondary.    @> 

@*3 \§circle tertiary> $\longrightarrow$ 
\§sphere tertiary>
\.{INTERSECTION} \§plane secondary>.
\initials{LDF 2005.10.28.}

\LOG
\initials{LDF 2005.10.28.}
Added this rule.

\initials{LDF 2005.10.30.}
Changed |rectangle_secondary| to |plane_secondary|.

\initials{LDF 2005.10.31.}
Changed the call to |Scan_Parse::sphere_plane_intersection_func()| 
to one to |Scan_Parse::ellipsoid_like_plane_intersection_func()|.
\ENDLOG
 
@q ***** (5) Definition.@> 

@<Define rules@>= 
@=circle_tertiary: sphere_tertiary INTERSECTION plane_secondary@>@/
{

   @=$$@> = Scan_Parse::ellipsoid_like_plane_intersection_func<Sphere>(
                                    static_cast<Sphere*>(@=$1@>),
                                    static_cast<Plane*>(@=$3@>),
                                    parameter);         
};

@q * (1) circle expression.  @>
@* \§circle expression>.

\LOG
\initials{LDF 2004.06.17.}  
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> circle_expression@>

@q ** (2) circle expression --> circle_tertiary.  @>
@*1 \§circle expression> $\longrightarrow$ \§circle tertiary>.

\LOG
\initials{LDF 2004.06.17.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=circle_expression: circle_tertiary@>@/
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

