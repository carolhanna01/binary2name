@q pellexpr.w @> 
@q Created by Laurence Finston Tue Jun 29 17:06:20 CEST 2004  @>
       
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



@q * (0) ellipse expressions.  @>
@** Ellipse expressions.
\initials{LDF 2004.06.29.}

\LOG
\initials{LDF 2004.06.29.}
Created this file.
\ENDLOG 

@q ** (2) Ellipse primary.  @>
@*1 \§ellipse primary>.
\initials{LDF 2004.06.29.}    

\LOG
\initials{LDF 2004.06.29.}  
Added this type declaration.
\ENDLOG 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> ellipse_primary@>@/

@q *** (3) ellipse_primary --> ellipse_variable.@>
@*2 \§ellipse primary> $\longrightarrow$ \§ellipse variable>.  

\LOG
\initials{LDF 2004.06.29.}
Added this rule.

\initials{LDF 2004.11.23.}
No longer issuing an error message if |entry->object == 0|.
This condition occurs legitimately when one tries to show
an ``unknown |ellipse|''.
\ENDLOG 

@<Define rules@>=
@=ellipse_primary: ellipse_variable@>@/
{

  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

  if (entry == static_cast<Id_Map_Entry_Node>(0) || entry->object == static_cast<void*>(0))
    {

      @=$$@> = static_cast<void*>(0);

    } /* |if (entry == 0 || entry->object == 0)|  */

  else /* |entry != 0 && entry->object != 0|  */

    @=$$@> = static_cast<void*>(create_new<Ellipse>(
                                  static_cast<Ellipse*>(
                                     entry->object))); 
};

@q *** ellipse_primary --> ellipse_argument.@>
@ \§ellipse primary> $\longrightarrow$ \§ellipse argument>.  


@q *** ellipse_primary --> ( ellipse_expression )  @>
@ \§ellipse primary> $\longrightarrow$ `\.{\LP}' 
\§ellipse expression> `\.{\RP}'.

\LOG
\initials{LDF 2004.06.29.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=ellipse_primary: LEFT_PARENTHESIS ellipse_expression RIGHT_PARENTHESIS@>@/
{
#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
      cerr << "\n*** Parser: ellipse_primary --> `(' ellipse_expression `)' "
           << endl;
    }
#endif /* |DEBUG_COMPILE|  */@;

  @=$$@> = @=$2@>;

};


@q ** (2) ellipse_primary --> GET_ELLIPSE INTEGER cone_primary@>
@*1 \§ellipse primary> $\longrightarrow$ \.{GET\_ELLIPSE}
INTEGER \§cone primary>. 
\initials{LDF 2006.11.09.}

\LOG
\initials{LDF 2006.11.09.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=ellipse_primary: GET_ELLIPSE INTEGER cone_primary@>@/
{

   @=$$@> = quad_surf_get_element_func(parameter, 
                                       GET_ELLIPSE, 
                                       Shape::CONE_TYPE, 
                                       @=$3@>,
                                       @=$2@>);

};


@q ** (2) ellipse_primary --> GET_ELLIPSE numeric_primary ellipsoid_primary@>
@*1 \§ellipse primary> $\longrightarrow$ \.{GET\_ELLIPSE}
\§numeric primary> \§ellipsoid primary>. 

\LOG
\initials{LDF 2004.10.13.}
Added this rule.

\initials{LDF 2005.11.02.}
Commented-in this rule.  
Removed debugging code.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=ellipse_primary: GET_ELLIPSE numeric_primary ellipsoid_primary@>@/ 
{

  Ellipsoid* s = static_cast<Ellipsoid*>(@=$3@>);

  Ellipse* e;

  const Ellipse* t 
     = s->get_ellipse_ptr(static_cast<unsigned short>( floor(fabs(@=$2@>) + .5))); 


  if (t == static_cast<const Ellipse*>(0))
   {
      delete s;

      @=$$@> = static_cast<void*>(0);

   } /* |if (t == 0)|  */


  else /* |t != 0|  */
     {
            e = create_new<Ellipse>(t);
            delete s;
            @=$$@> = static_cast<void*>(e);

  }  /* |else| (|t != 0|)  */
};

@q ** (2) ellipse_primary --> GET_ELLIPSE point_vector_primary@>
@*1 \§ellipse primary> $\longrightarrow$ \.{GET\_ELLIPSE}
\§point vector primary>.
\initials{LDF 2005.11.24.}

\LOG
\initials{LDF 2005.11.24.}
Added this rule.

\initials{LDF 2007.06.27.}
Added \§numeric primary> symbol.

\initials{LDF 2007.07.03.}
Replaced |Ellipse::create_new_ellipse| with |Conic_Section::generate|.

\initials{LDF 2007.07.03.}
Now passing |Shape::ELLIPSE_TYPE| to |Scan_Parse::get_conic_section_func|.

\initials{LDF 2007.07.20.}
Added \§with test optional>.  It's  tested and the appropriate |bool| value
is passed to |Scan_Parse::get_conic_section_func|.

\initials{LDF 2007.07.20.}
Added \§with rectify optional>.  It's tested and the appropriate |bool| value
is passed to |Scan_Parse::get_conic_section_func|.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=ellipse_primary: GET_ELLIPSE point_vector_primary numeric_primary@>@/ 
@=with_test_optional with_rectify_optional@>@/
{

   bool test_value    = (@=$4@> == WITH_NO_TEST)    ? false : true;
   bool rectify_value = (@=$4@> == WITH_NO_RECTIFY) ? false : true;

   @=$$@> = get_conic_section_func<Ellipse>(static_cast<Pointer_Vector<Point>*>(@=$2@>), 
                                    static_cast<int>(@=$3@>),
                                    Shape::ELLIPSE_TYPE,
                                    static_cast<Ellipse*>(0),
                                    parameter,
                                    test_value,
                                    rectify_value);


};

@q ** (2) ellipse_primary --> GET_ELLIPSE conic_section_lattice_expression@>
@*1 \§ellipse primary> $\longrightarrow$ \.{GET\_ELLIPSE}
\§conic section lattice expression>.
\initials{LDF 2007.09.26.}

\LOG
\initials{LDF 2007.09.26.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=ellipse_primary: GET_ELLIPSE conic_section_lattice_expression@>@/ 
{

   Conic_Section_Lattice* c = static_cast<Conic_Section_Lattice*>(@=$2@>);

   @=$$@> = (c) ?  c->get_ellipse(static_cast<Scanner_Node>(parameter))
                :  @=$$@> = static_cast<void*>(0);

};

@q ** (2) ellipse_primary --> IN_ELLIPSE rectangle_primary@>
@*1 \§ellipse primary> $\longrightarrow$ \.{IN\_ELLIPSE}
\§ellipse primary>. 

\LOG
\initials{LDF 2004.11.20.}
Added this rule.

\initials{LDF 2005.12.16.}
@:BUG FIX@> BUG FIX: Now deleting |Rectangle* r|.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=ellipse_primary: IN_ELLIPSE rectangle_primary@>@/
{

   Rectangle* r = static_cast<Rectangle*>(@=$2@>);

   Ellipse* e;

   e = create_new<Ellipse>(0);

   *e = r->in_ellipse();
   
   delete r;

   @=$$@> = static_cast<void*>(e); 

};

@q ** (2) ellipse_primary --> OUT_ELLIPSE rectangle_primary@>
@*1 \§ellipse primary> $\longrightarrow$ \.{OUT\_ELLIPSE}
\§ellipse primary>. 

\LOG
\initials{LDF 2004.11.20.}
Added this rule.

\initials{LDF 2005.12.16.}
@:BUG FIX@> BUG FIX: Now deleting |Rectangle* r|.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=ellipse_primary: OUT_ELLIPSE rectangle_primary@>@/
{

   Rectangle* r = static_cast<Rectangle*>(@=$2@>);

   Ellipse* e;

         e = create_new<Ellipse>(0);

   *e = r->out_ellipse();
   
   delete r;

   @=$$@> = static_cast<void*>(e); 

};


@q ***** (5) ellipse_primary --> LAST @>
@q ***** (5) ellipse_vector_expression.@>

@*4 \§ellipse primary> $\longrightarrow$ 
\.{LAST} \§ellipse vector expression>.
\initials{LDF 2005.01.14.}

\LOG
\initials{LDF 2005.01.14.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=ellipse_primary: LAST ellipse_vector_expression@>@/
{ 
   Ellipse* p;

         p = create_new<Ellipse>(0);

   Pointer_Vector<Ellipse>* pv 
      = static_cast<Pointer_Vector<Ellipse>*>(@=$2@>);

@q ******* (7) Error handling:  |pv == 0|.@> 

@ Error handling:  |pv == 0|.
\initials{LDF 2005.01.14.}

@<Define rules@>=

   if (pv == static_cast<Pointer_Vector<Ellipse>*>(0))
      {

          delete p;

          @=$$@> = static_cast<void*>(0);

      }  /* |if (pv == 0)|  */

@q ******* (7) Error handling:  |pv->ctr == 0|.@> 

@ Error handling:  |pv->ctr == 0|.
\initials{LDF 2005.01.14.}

@<Define rules@>=

   else if (pv->ctr == 0)
      {

          delete p;

          @=$$@> = static_cast<void*>(0);

      }  /* |else if (pv->ctr == 0)|  */

@q ******* (7) |pv != 0 && pv->ctr > 0|.@> 

@ |pv != 0 && pv->ctr > 0|.  Set |@=$$@>| to |*(pv->v[pv->ctr - 1])|.
\initials{LDF 2005.01.14.}

@<Define rules@>=

   else 
      {
         *p = *(pv->v[pv->ctr - 1]);
         @=$$@> = static_cast<void*>(p); 
      }
@q ******* (7) @> 

};



@q ** (2) ellipse secondary.  @>
@*1 \§ellipse secondary>.


\LOG
\initials{LDF 2004.06.29.}  
Added this section.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> ellipse_secondary@>
  
@q *** ellipse secondary --> ellipse_primary.  @>
@ \§ellipse secondary> $\longrightarrow$ \§ellipse primary>.

\LOG
\initials{LDF 2004.06.29.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=ellipse_secondary: ellipse_primary@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3) ellipse secondary --> ellipse_secondary transformer.  @>
@*2 \§ellipse secondary> $\longrightarrow$ \§ellipse secondary> 
\§transformer>.

\LOG
\initials{LDF 2004.06.29.}
Added this rule.

\initials{LDF 2005.12.16.}
@:BUG FIX@> BUG FIX:  Now deleting |Transform* t|.
\ENDLOG

@<Define rules@>=
@=ellipse_secondary: ellipse_secondary transformer@>@/
{

  Ellipse* p = static_cast<Ellipse*>(@=$1@>);
  Transform* t = static_cast<Transform*>(@=$2@>);

  *p *= *t;

  delete t;

  @=$$@> = static_cast<void*>(p); 

};

@q *** (3) ellipse secondary --> ellipse_secondary REFLECTED_IN @>
@q *** (3) path_expression.@> 

@*2 \§ellipse secondary> $\longrightarrow$ 
\§ellipse secondary> 
\.{REFLECTED\_IN} \§path expression>.

\LOG
\initials{LDF 2004.10.05.}
Added this rule.

\initials{LDF 2004.12.03.}
Changed |REFLECTED IN| to |REFLECTED_IN|.

\initials{LDF 2005.10.24.}
Changed |path_like_expression| to |path_expression|.
Removed debugging code.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=ellipse_secondary: ellipse_secondary REFLECTED_IN @>
@=path_expression@>@/ 
{

     Ellipse* p = reflect_in_func<Ellipse>(static_cast<Scanner_Node>(parameter),
                                           static_cast<Ellipse*>(@=$1@>),
                                           static_cast<Path*>(@=$3@>));


     @=$$@> = static_cast<void*>(p);

};

@q ** (2) ellipse tertiary.  @>
@*1 \§ellipse tertiary>.

\LOG
\initials{LDF 2004.06.29.}  
Added this section.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> ellipse_tertiary@>

@q *** ellipse tertiary --> ellipse_secondary.  @>
@ \§ellipse tertiary> $\longrightarrow$ \§ellipse secondary>.


\LOG
\initials{LDF 2004.06.29.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=ellipse_tertiary: ellipse_secondary@>@/
{

  @=$$@> = @=$1@>;

};


@q **** (4) ellipse_tertiary: ellipsoid_tertiary @> 
@q **** (4) INTERSECTION plane_secondary.        @> 

@*3 \§ellipse tertiary> $\longrightarrow$ 
\§ellipsoid tertiary>
\.{INTERSECTION} \§plane secondary>.
\initials{LDF 2005.10.31.}

\LOG
\initials{LDF 2005.10.31.}
Added this rule.

\initials{LDF 2005.12.15.}
Changed \§path vector tertiary> to \§ellipse tertiary>.
I was using \§path vector tertiary> for testing.
\ENDLOG
 
@q ***** (5) Definition.@> 

@<Define rules@>=
@=ellipse_tertiary: ellipsoid_tertiary INTERSECTION plane_secondary@>@/
{

   @=$$@> = Scan_Parse::ellipsoid_like_plane_intersection_func<Ellipsoid>(
                     static_cast<Ellipsoid*>(@=$1@>),
                     static_cast<Plane*>(@=$3@>),
                     parameter);
};

@q ** (2) ellipse expression.  @>
@*1 \§ellipse expression>.
\initials{LDF 2004.06.29.}  

\LOG
\initials{LDF 2004.06.29.}  
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> ellipse_expression@>

@q *** (3) ellipse expression --> ellipse_tertiary.  @>
@*2 \§ellipse expression> $\longrightarrow$ \§ellipse tertiary>.
\initials{LDF 2004.06.29.}

\LOG
\initials{LDF 2004.06.29.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=ellipse_expression: ellipse_tertiary@>@/
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

