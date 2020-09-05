@q phypexpr.w @> 
@q Created by Laurence Finston Mon Nov  7 19:41:56 CET 2005 @>
       
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



@q * (0) hyperbola expressions.  @>
@** hyperbola expressions.
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Created this file.
\ENDLOG 

@q * (1) hyperbola_primary.  @>
@* \§hyperbola primary>.
\initials{LDF 2005.11.14.}  

\LOG
\initials{LDF 2005.11.14.}
Added this type declaration.
\ENDLOG 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> hyperbola_primary@>@/

@q ** (2) hyperbola_primary --> hyperbola_variable.@>
@*1 \§hyperbola primary> $\longrightarrow$ \§hyperbola variable>.  

\LOG
\initials{LDF 2005.11.14.}
Added this rule.
\ENDLOG 

@q *** (3) Definition.@> 

@<Define rules@>=
@=hyperbola_primary: hyperbola_variable@>@/
{
  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

  if (entry == static_cast<Id_Map_Entry_Node>(0) || entry->object == static_cast<void*>(0))
    {

      @=$$@> = static_cast<void*>(0);

    } /* |if (entry == 0 || entry->object == 0)|  */

  else /* |entry != 0 && entry->object != 0|  */

    @=$$@> = static_cast<void*>(create_new<Hyperbola>(
                                  static_cast<Hyperbola*>(
                                     entry->object))); 

};

@q ** (2) hyperbola_primary --> LEFT_PARENTHESIS hyperbola_expression @> 
@q ** (2) RIGHT_PARENTHESIS                                   @>

@*1 \§hyperbola primary> $\longrightarrow$ \.{\LP} 
\§hyperbola expression> \.{\RP}.

\LOG
\initials{LDF 2005.11.14.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=hyperbola_primary: LEFT_PARENTHESIS hyperbola_expression RIGHT_PARENTHESIS@>@/
{

  @=$$@> = @=$2@>;

};


@q ***** (5) hyperbola_primary --> LAST @>
@q ***** (5) hyperbola_vector_expression.@>

@*4 \§hyperbola primary> $\longrightarrow$ 
\.{LAST} \§hyperbola vector expression>.
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=hyperbola_primary: LAST hyperbola_vector_expression@>@/
{ 

   Hyperbola* c;

         c = create_new<Hyperbola>(0);

   Pointer_Vector<Hyperbola>* pv 
      = static_cast<Pointer_Vector<Hyperbola>*>(@=$2@>);

@q ******* (7) Error handling:  |pv == 0|.@> 

@ Error handling:  |pv == 0|.
\initials{LDF 2005.11.14.}

@<Define rules@>=

   if (pv == static_cast<Pointer_Vector<Hyperbola>*>(0))
      {

          delete c;

          @=$$@> = static_cast<void*>(0);

      }  /* |if (pv == 0)|  */

@q ******* (7) Error handling:  |pv->ctr == 0|.@> 

@ Error handling:  |pv->ctr == 0|.
\initials{LDF 2005.11.14.}

@<Define rules@>=

   else if (pv->ctr == 0)
      {
          delete c;

          @=$$@> = static_cast<void*>(0);

      }  /* |else if (pv->ctr == 0)|  */

@q ******* (7) |pv != 0 && pv->ctr > 0|.@> 

@ |pv != 0 && pv->ctr > 0|.  Set |@=$$@>| to |*(pv->v[pv->ctr - 1])|.
\initials{LDF 2005.11.14.}

@<Define rules@>=

   else 
      {
         *c = *(pv->v[pv->ctr - 1]);
         @=$$@> = static_cast<void*>(c); 
      }
@q ******* (7) @> 

};

@q ** (2) hyperbola_primary --> GET_HYPERBOLA point_vector_primary@>
@*1 \§hyperbola primary> $\longrightarrow$ \.{GET\_HYPERBOLA}
\§point vector primary>.
\initials{LDF 2007.07.09.}

\LOG
\initials{LDF 2007.07.09.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=hyperbola_primary: GET_HYPERBOLA point_vector_primary numeric_primary@>@/ 
{

   @=$$@> = get_conic_section_func<Hyperbola>(
                                     static_cast<Pointer_Vector<Point>*>(@=$2@>), 
                                     static_cast<int>(@=$3@>),
                                     Shape::HYPERBOLA_TYPE,
                                     static_cast<Hyperbola*>(0),
                                     parameter);

};

@q ** (2) hyperbola_primary --> GET_HYPERBOLA conic_section_lattice_expression@>
@*1 \§hyperbola primary> $\longrightarrow$ \.{GET\_HYPERBOLA}
\§conic section lattice expression>.
\initials{LDF 2007.09.26.}

\LOG
\initials{LDF 2007.09.26.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=hyperbola_primary: GET_HYPERBOLA conic_section_lattice_expression@>@/ 
{

   Conic_Section_Lattice* c = static_cast<Conic_Section_Lattice*>(@=$2@>);

   @=$$@> = (c) ?  c->get_hyperbola(static_cast<Scanner_Node>(parameter))
                :  @=$$@> = static_cast<void*>(0);

};

@q * (1) hyperbola_secondary.  @>
@* \§hyperbola secondary>.
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> hyperbola_secondary@>
  
@q ** (2) hyperbola secondary --> hyperbola_primary.@>
@*1 \§hyperbola secondary> $\longrightarrow$ \§hyperbola primary>.

\LOG
\initials{LDF 2005.11.14.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=hyperbola_secondary: hyperbola_primary@>@/
{
#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @;
  if (DEBUG)
    {
      cerr << "\n*** Parser: hyperbola_secondary --> hyperbola_primary "
           << endl;
    }
#endif /* |DEBUG_COMPILE|  */@;

  @=$$@> = @=$1@>;

};

@q ** (2) hyperbola secondary --> hyperbola_secondary transformer.  @>
@*1 \§hyperbola secondary> $\longrightarrow$ \§hyperbola secondary> 
\§transformer>.
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Added this rule.

\initials{LDF 2005.12.20.}
@:BUG FIX@> BUG FIX:  Now deleting |Transform* t|.
\ENDLOG

@<Define rules@>=
@=hyperbola_secondary: hyperbola_secondary transformer@>@/
{

  Hyperbola* p = static_cast<Hyperbola*>(@=$1@>);
  Transform* t = static_cast<Transform*>(@=$2@>);

  *p *= *t;

  @=$$@> = static_cast<void*>(p); 

  delete t;

};

@q * (1) hyperbola tertiary.@>
@* \§hyperbola tertiary>.
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> hyperbola_tertiary@>

@q ** (2) hyperbola tertiary --> hyperbola_secondary.  @>
@*1 \§hyperbola tertiary> $\longrightarrow$ \§hyperbola secondary>.
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=hyperbola_tertiary: hyperbola_secondary@>@/
{

  @=$$@> = @=$1@>;

};



@q * (1) hyperbola expression.@>
@* \§hyperbola expression>.
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> hyperbola_expression@>

@q ** (2) hyperbola expression --> hyperbola_tertiary.  @>
@*1 \§hyperbola expression> $\longrightarrow$ \§hyperbola tertiary>.

\LOG
\initials{LDF 2005.11.14.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=hyperbola_expression: hyperbola_tertiary@>@/
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

