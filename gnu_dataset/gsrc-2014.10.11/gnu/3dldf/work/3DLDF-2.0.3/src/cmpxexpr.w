@q cmpxexpr.w @> 
@q Created by Laurence Finston (LDF) Sun Dec  2 18:10:05 CET 2007 @>

@q * (0) Copyright and License.@>

@q This file is part of GNU 3DLDF, a package for three-dimensional drawing. @>
@q Copyright (C) 2007, 2008, 2009, 2010, 2011, 2012, 2013 The Free Software Foundation  @>

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


@q * (0) complex expressions.  @>
@** complex expressions.
\initials{LDF 2007.12.02.}

\LOG
\initials{LDF 2007.12.02.}
Created this file.
\ENDLOG 

@q * (1) complex_primary.  @>
@* \§complex primary>.
\initials{LDF 2007.12.02.}  

\LOG
\initials{LDF 2007.12.02.}
Added this type declaration.
\ENDLOG 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> complex_primary@>@/

@q ** (2) complex_primary --> complex_variable.@>
@*1 \§complex primary> $\longrightarrow$ \§complex variable>.  

\LOG
\initials{LDF 2007.12.02.}
Added this rule.
\ENDLOG 

@q *** (3) Definition.@> 

@<Define rules@>=
@=complex_primary: complex_variable@>@/
{
  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

  if (entry == static_cast<Id_Map_Entry_Node>(0) || entry->object == static_cast<void*>(0))
    {

      @=$$@> = static_cast<void*>(0);

    } /* |if (   entry == static_cast<Id_Map_Entry_Node>(0) 
              || entry->object == static_cast<void*>(0))|  */

  else /* |   entry != static_cast<Id_Map_Entry_Node>(0) 
           && entry->object != static_cast<void*>(0)|  */

    @=$$@> = static_cast<void*>(create_new<Complex>(
                                  static_cast<Complex*>(
                                     entry->object))); 

};

@q ** (2) complex_primary --> LEFT_PARENTHESIS complex_expression @> 
@q ** (2) RIGHT_PARENTHESIS                                       @>

@*1 \§complex primary> $\longrightarrow$ \.{\LP} 
\§complex expression> \.{\RP}.

\LOG
\initials{LDF 2007.12.02.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=complex_primary: LEFT_PARENTHESIS complex_expression RIGHT_PARENTHESIS@>@/
{

  @=$$@> = @=$2@>;

};


@q ***** (5) complex_primary --> LAST @>
@q ***** (5) complex_vector_expression.@>

@*4 \§complex primary> $\longrightarrow$ 
\.{LAST} \§complex vector expression>.
\initials{LDF 2007.12.02.}

\LOG
\initials{LDF 2007.12.02.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=complex_primary: LAST complex_vector_expression@>@/
{ 

   Complex* c;

         c = create_new<Complex>(0);

   Pointer_Vector<Complex>* pv 
      = static_cast<Pointer_Vector<Complex>*>(@=$2@>);

@q ******* (7) Error handling:  |pv == static_cast<Pointer_Vector<Complex>*>(0)|.@> 

@ Error handling:  |pv == |pv == static_cast<Pointer_Vector<Complex>*>(0)|.
\initials{LDF 2007.12.02.}

@<Define rules@>=

   if (pv == static_cast<Pointer_Vector<Complex>*>(0))
      {

          delete c;

          @=$$@> = static_cast<void*>(0);

      }  /* |if (pv == static_cast<Pointer_Vector<Complex>*>(0))|  */

@q ******* (7) Error handling:  |pv->ctr == 0|.@> 

@ Error handling:  |pv->ctr == 0|.
\initials{LDF 2007.12.02.}

@<Define rules@>=

   else if (pv->ctr == 0)
      {
          delete c;

          @=$$@> = static_cast<void*>(0);

      }  /* |else if (pv->ctr == 0)|  */

@q ******* (7) |pv != static_cast<Pointer_Vector<Complex>*>(0) && pv->ctr > 0|.@> 

@ |pv != static_cast<Pointer_Vector<Complex>*>(0) && pv->ctr > 0|.  
Set |@=$$@>| to |*(pv->v[pv->ctr - 1])|.
\initials{LDF 2007.12.02.}

@<Define rules@>=

   else 
      {
         *c = *(pv->v[pv->ctr - 1]);
         @=$$@> = static_cast<void*>(c); 
      }
@q ******* (7) @> 

};

@q ** (2) complex_primary --> PLUS complex_primary.@> 

@*1 \§complex primary> $\longrightarrow$ \.{PLUS} 
\§complex primary>.

\LOG
\initials{LDF 2007.12.11.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=complex_primary: PLUS complex_primary@>@/
{

  @=$$@> = complex_primary_rule_func_0(@=$2@>, PLUS, parameter);

};

@q ** (2) complex_primary --> MINUS complex_primary.@> 


@*1 \§complex primary> $\longrightarrow$ \.{MINUS} 
\§complex primary>.

\LOG
\initials{LDF 2007.12.11.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=complex_primary: MINUS complex_primary@>@/
{

  @=$$@> = complex_primary_rule_func_0(@=$2@>, MINUS, parameter);

};

@q * (1) complex_secondary.  @>
@* \§complex secondary>.
\initials{LDF 2007.12.02.}

\LOG
\initials{LDF 2007.12.02.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> complex_secondary@>
  
@q ** (2) complex secondary --> complex_primary.@>
@*1 \§complex secondary> $\longrightarrow$ \§complex primary>.

\LOG
\initials{LDF 2007.12.02.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=complex_secondary: complex_primary@>@/
{

  @=$$@> = @=$1@>;

};

@q ***** (5) complex_secondary --> complex_secondary @>
@q ***** (5) times_or_over complex_primary.          @>

@*4 \§complex secondary> $\longrightarrow$ \§complex secondary> 
\§times or over> \§complex primary>. 
\initials{LDF 2007.12.11.}

\LOG
\initials{LDF 2007.12.11.}
Added this rule.
\ENDLOG

@<Define rules@>= 
@=complex_secondary: complex_secondary times_or_over complex_primary@>
{

   @=$$@> = complex_arith_op_rule_func(@=$1@>, 
                                       @=$3@>, 
                                       @=$2@>, 
                                       parameter);

};

@q * (1) complex tertiary.@>
@* \§complex tertiary>.
\initials{LDF 2007.12.02.}

\LOG
\initials{LDF 2007.12.02.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> complex_tertiary@>

@q ** (2) complex tertiary --> complex_secondary.  @>
@*1 \§complex tertiary> $\longrightarrow$ \§complex secondary>.
\initials{LDF 2007.12.02.}

\LOG
\initials{LDF 2007.12.02.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=complex_tertiary: complex_secondary@>@/
{

  @=$$@> = @=$1@>;

};

@q ***** (5) complex_tertiary --> complex_tertiary @>
@q ***** (5) PLUS complex_secondary.               @>

@ \§complex tertiary> $\longrightarrow$ \§complex tertiary> 
\.{PLUS} \§complex secondary>.
\initials{LDF 2007.12.11.}

\LOG
\initials{LDF 2007.12.11.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=complex_tertiary: complex_tertiary PLUS complex_secondary@>
{
   @=$$@> = complex_arith_op_rule_func(@=$1@>, @=$3@>, PLUS, parameter);
};

@q ***** (5) complex_tertiary --> complex_tertiary @>
@q ***** (5) MINUS complex_secondary.               @>

@ \§complex tertiary> $\longrightarrow$ \§complex tertiary> 
\.{MINUS} \§complex secondary>.
\initials{LDF 2007.12.11.}

\LOG
\initials{LDF 2007.12.11.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=complex_tertiary: complex_tertiary MINUS complex_secondary@>
{
   @=$$@> = complex_arith_op_rule_func(@=$1@>, @=$3@>, MINUS, parameter);
};

@q * (1) complex expression.@>
@* \§complex expression>.
\initials{LDF 2007.12.02.}

\LOG
\initials{LDF 2007.12.02.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> complex_expression@>

@q ** (2) complex expression --> complex_tertiary.  @>
@*1 \§complex expression> $\longrightarrow$ \§complex tertiary>.

\LOG
\initials{LDF 2007.12.02.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=complex_expression: complex_tertiary@>@/
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

