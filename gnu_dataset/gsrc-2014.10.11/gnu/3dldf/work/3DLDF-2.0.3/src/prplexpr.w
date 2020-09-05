@q prplexpr.w @> 
@q Created by Laurence Finston Tue Jul  6 14:19:33 CEST 2004 @>
       
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



@q * (0) reg_polygon expressions.  @>
@** {\bf reg\_polygon} expressions.

\LOG
\initials{LDF 2004.07.06.}
Created this file.

\initials{LDF 2004.07.06.}
Made debugging output thread-safe in all rules.
\ENDLOG 


@q * (1) reg_polygon primary.  @>
@* \§regular polygon primary>.
  
\LOG
\initials{LDF 2004.07.06.}  
Added this section.
\ENDLOG 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> reg_polygon_primary@>@/

@q ** (2) reg_polygon_primary --> reg_polygon_variable.@>
@*1 \§regular polygon primary> $\longrightarrow$ \§regular polygon variable>.  

\LOG
\initials{LDF 2004.07.06.}
Added this rule.

\initials{LDF 2004.11.23.}
No longer issuing an error message if |entry->object == 0|.
This condition occurs legitimately when one tries to show
an ``unknown |reg_polygon|''.
\ENDLOG 

@<Define rules@>=
@=reg_polygon_primary: reg_polygon_variable@>@/
{

  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

  if (entry == static_cast<Id_Map_Entry_Node>(0) || entry->object == static_cast<void*>(0))
    {

      @=$$@> = static_cast<void*>(0);

    } /* |if (entry == 0 || entry->object == 0)|  */

  else /* |entry != 0 && entry->object != 0|  */

    @=$$@> = static_cast<void*>(create_new<Reg_Polygon>(
                                  static_cast<Reg_Polygon*>(
                                     entry->object))); 
};

@q ** (2)reg_polygon_primary --> reg_polygon_argument.@>
@*1 \§regular polygon primary> $\longrightarrow$ 
\§regular polygon argument>.  


@q ** (2) reg_polygon_primary --> ( reg_polygon_expression )  @>
@*1 \§regular polygon primary> $\longrightarrow$ `\.{\LP}' 
\§regular polygon expression> `\.{\RP}'.

\LOG
\initials{LDF 2004.07.06.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=reg_polygon_primary: LEFT_PARENTHESIS reg_polygon_expression RIGHT_PARENTHESIS@>@/
{

  @=$$@> = @=$2@>;

};


@q ** (2) reg_polygon_primary --> GET_REG_POLYGON numeric_expression @>
@q ** (2) cuboid_primary.                                        @>
@*1 \§regular polygon primary> $\longrightarrow$ \.{GET\_REG\_POLYGON}
\§numeric expression> \§cuboid primary>. 


@:!!@> !!  For some reason, a parse error occurs if \§numeric variables> aren't
enclosed in parentheses.  It doesn't happen with literal numbers, i.e.,  
\§numeric tokens>.  I don't know why this happens or how to fix it.
\initials{LDF 2004.10.21.}

\LOG
\initials{LDF 2004.10.13.}
Added this rule.

\initials{LDF 2004.10.21.}
Changed \.{INTEGER} to \§numeric expression>.  Now passing  
|static_cast<unsigned short>(floor(fabs(@=$2@>) + .5))| 
as the argument to |Cuboid::get_reg_polygon_ptr()|. 
\ENDLOG

\TODO
@q { @>
@:TO DO}{{\bf TO DO}@>
@q } @> 
@q !! TO DO:  @>
\initials{LDF 2004.10.21.}
Add error handling for the cases 
that \§numeric expression> is negative and/or 
non-integral.
\ENDTODO 

@q *** (3) Definition.@> 

@<Define rules@>=

@=reg_polygon_primary: GET_REG_POLYGON numeric_expression cuboid_primary@>@/
{

  Cuboid* s = static_cast<Cuboid*>(@=$3@>);

  Reg_Polygon* r;

  unsigned short u = static_cast<unsigned short>(floor(fabs(@=$2@>) + .5));


  const Reg_Polygon* t = s->get_reg_polygon_ptr(u); 


  if (t == static_cast<const Reg_Polygon*>(0))
   {

      delete s;

      @=$$@> = static_cast<void*>(0);

   } /* |if (t == 0)|  */


  else /* |t != 0|  */
     {

            r = create_new<Reg_Polygon>(t, static_cast<Scanner_Node>(parameter));

            delete s;

            @=$$@> = static_cast<void*>(r);

  }  /* |else| (|t != 0|)  */

};

@q ** (2) reg_polygon_primary --> GET_REG_POLYGON numeric_primary @>
@q ** (2) polyhedron_primary.                                        @>
@*1 \§regular polygon primary> $\longrightarrow$ \.{GET\_REG\_POLYGON}
\§numeric primary> \§polyhedron primary>. 
\initials{LDF 2005.12.14.}

\LOG
\initials{LDF 2005.12.14.}
Added this rule.
\ENDLOG

\TODO
@q { @>
@:TO DO}{{\bf TO DO}@>
@q } @> 
@q !! TO DO:  @>
\initials{LDF 2004.10.21.}
Add error handling for the cases 
that \§numeric expression> is negative and/or 
non-integral.
\ENDTODO 

@q *** (3) Definition.@> 

@<Define rules@>=

@=reg_polygon_primary: GET_REG_POLYGON numeric_primary polyhedron_primary@>@/
{

     Polyhedron* p = static_cast<Polyhedron*>(@=$3@>);

     Reg_Polygon* rp;

     unsigned short u = static_cast<unsigned short>(floor(fabs(@=$2@>) + .5));

     const Reg_Polygon* t = p->get_reg_polygon_ptr(u); 

     if (t == static_cast<const Reg_Polygon*>(0))
      {

         delete p;

         @=$$@> = static_cast<void*>(0);

      } /* |if (t == 0)|  */



     else /* |t != 0|  */
        {

     rp = create_new<Reg_Polygon>(t, static_cast<Scanner_Node>(parameter));

     delete p;

     @=$$@> = static_cast<void*>(rp);

     }  /* |else| (|t != 0|)  */

};



@q ***** (5) reg_polygon_primary --> LAST @>
@q ***** (5) reg_polygon_vector_expression.@>

@*4 \§regular polygon primary> $\longrightarrow$ 
\.{LAST} \§regular polygon vector expression>.
\initials{LDF 2005.01.14.}

\LOG
\initials{LDF 2005.01.14.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=reg_polygon_primary: LAST reg_polygon_vector_expression@>@/
{ 

   Reg_Polygon* r;


         r = create_new<Reg_Polygon>(0);

   Pointer_Vector<Reg_Polygon>* pv 
      = static_cast<Pointer_Vector<Reg_Polygon>*>(@=$2@>);

@q ******* (7) Error handling:  |pv == 0|.@> 

@ Error handling:  |pv == 0|.
\initials{LDF 2005.01.14.}

@<Define rules@>=

   if (pv == static_cast<Pointer_Vector<Reg_Polygon>*>(0))
      {

          delete r;

          @=$$@> = static_cast<void*>(0);

      }  /* |if (pv == 0)|  */

@q ******* (7) Error handling:  |pv->ctr == 0|.@> 

@ Error handling:  |pv->ctr == 0|.
\initials{LDF 2005.01.14.}

@<Define rules@>=

   else if (pv->ctr == 0)
      {
          delete r;

          @=$$@> = static_cast<void*>(0);

      }  /* |else if (pv->ctr == 0)|  */

@q ******* (7) |pv != 0 && pv->ctr > 0|.@> 

@ |pv != 0 && pv->ctr > 0|.  Set |@=$$@>| to |*(pv->v[pv->ctr - 1])|.
\initials{LDF 2005.01.14.}

@<Define rules@>=

   else 
      {
         *r = *(pv->v[pv->ctr - 1]);
         @=$$@> = static_cast<void*>(r); 
      }
@q ******* (7) @> 

};




@q * (1) reg_polygon secondary.  @>
@* \§regular polygon secondary>.
\initials{LDF 2004.07.06.}  

\LOG
\initials{LDF 2004.07.06.}  
Added this section.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> reg_polygon_secondary@>
  
@q *** reg_polygon secondary --> reg_polygon_primary.  @>
@ \§regular polygon secondary> $\longrightarrow$ \§regular polygon primary>.

\LOG
\initials{LDF 2004.07.06.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=reg_polygon_secondary: reg_polygon_primary@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3) reg_polygon secondary --> reg_polygon_secondary transformer.  @>
@*2 \§regular polygon secondary> $\longrightarrow$ 
\§regular polygon secondary> \§transformer>.


\LOG
\initials{LDF 2004.07.06.}
Added this rule.

\initials{LDF 2006.01.20.}
@:BUG FIX@> BUG FIX:  Now deleting |Transform* t|.
\ENDLOG

@<Define rules@>=
@=reg_polygon_secondary: reg_polygon_secondary transformer@>@/
{

  Reg_Polygon* p = static_cast<Reg_Polygon*>(@=$1@>);
  Transform* t = static_cast<Transform*>(@=$2@>);

  *p *= *t;

  delete t;

  @=$$@> = static_cast<void*>(p); 

};

@q *** (3) reg_polygon secondary --> reg_polygon_secondary REFLECTED_IN @>
@q *** (3) path_expression.                                             @> 

@*2 \§regular polygon secondary> $\longrightarrow$ 
\§regular polygon secondary> 
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
@=reg_polygon_secondary: reg_polygon_secondary REFLECTED_IN @>
@=path_expression@>@/ 
{

    Reg_Polygon* p = reflect_in_func<Reg_Polygon>(static_cast<Scanner_Node>(parameter),
                                                  static_cast<Reg_Polygon*>(@=$1@>),
                                                  static_cast<Path*>(@=$3@>));


    @=$$@> = static_cast<void*>(p);

};





@q ** (2) reg_polygon tertiary.  @>
@*1 \§regular polygon tertiary>.


\LOG
\initials{LDF 2004.07.06.}  
Added this section.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> reg_polygon_tertiary@>

@q *** reg_polygon tertiary --> reg_polygon_secondary.  @>
@ \§regular polygon tertiary> $\longrightarrow$ \§regular polygon secondary>.


\LOG
\initials{LDF 2004.07.06.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=reg_polygon_tertiary: reg_polygon_secondary@>@/
{

  @=$$@> = @=$1@>;

};


@q ** (2) reg_polygon expression.  @>
@*1 \§regular polygon expression>.
\initials{LDF 2004.07.06.}  

\LOG
\initials{LDF 2004.07.06.}  
Added this section.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> reg_polygon_expression@>

@q *** reg_polygon expression --> reg_polygon_tertiary.  @>
@ \§regular polygon expression> $\longrightarrow$ \§regular polygon tertiary>.

\LOG
\initials{LDF 2004.07.06.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=reg_polygon_expression: reg_polygon_tertiary@>@/
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

