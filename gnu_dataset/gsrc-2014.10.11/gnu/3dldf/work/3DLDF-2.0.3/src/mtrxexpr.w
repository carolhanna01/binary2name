@q mtrxexpr.w @> 
@q Created by Laurence Finston (LDF) Wed Nov 28 16:30:38 CET 2007  @>


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



@q * (0) matrix expressions.  @>
@** matrix expressions.
\initials{LDF 2007.11.28.}

\LOG
\initials{LDF 2007.11.28.}
Created this file.
\ENDLOG 

@q * (1) matrix_primary.  @>
@* \§matrix primary>.
\initials{LDF 2007.11.28.}  

\LOG
\initials{LDF 2007.11.28.}
Added this type declaration.
\ENDLOG 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> matrix_primary@>@/

@q ** (2) matrix_primary --> matrix_variable.@>
@*1 \§matrix primary> $\longrightarrow$ \§matrix variable>.  

\LOG
\initials{LDF 2007.11.28.}
Added this rule.
\ENDLOG 

@q *** (3) Definition.@> 

@<Define rules@>=
@=matrix_primary: matrix_variable@>@/
{
  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

  if (entry == static_cast<Id_Map_Entry_Node>(0) || entry->object == static_cast<void*>(0))
    {

      @=$$@> = static_cast<void*>(0);

    } /* |if (   entry == static_cast<Id_Map_Entry_Node>(0) 
              || entry->object == static_cast<void*>(0))|  */

  else 
  {
      @=$$@> = static_cast<void*>(create_new<Matrix>(
                                  static_cast<Matrix*>(
                                     entry->object))); 
  }
 

};

@q ** (2) matrix_primary --> LEFT_PARENTHESIS matrix_expression @> 
@q ** (2) RIGHT_PARENTHESIS                                     @>

@*1 \§matrix primary> $\longrightarrow$ \.{\LP} 
\§matrix expression> \.{\RP}.

\LOG
\initials{LDF 2007.11.28.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=matrix_primary: LEFT_PARENTHESIS matrix_expression RIGHT_PARENTHESIS@>@/
{

  @=$$@> = @=$2@>;

};

@q ***** (5) matrix_primary --> LAST @>
@q ***** (5) matrix_vector_expression.@>

@*4 \§matrix primary> $\longrightarrow$ 
\.{LAST} \§matrix vector expression>.
\initials{LDF 2007.11.28.}

\LOG
\initials{LDF 2007.11.28.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=matrix_primary: LAST matrix_vector_expression@>@/
{ 

   Matrix* c;

         c = create_new<Matrix>(0);

   Pointer_Vector<Matrix>* pv 
      = static_cast<Pointer_Vector<Matrix>*>(@=$2@>);

@q ******* (7) Error handling:  |pv == static_cast<Pointer_Vector<Matrix>*>(0)|.@> 

@ Error handling:  |pv == static_cast<Pointer_Vector<Matrix>*>(0)|.
\initials{LDF 2007.11.28.}

@<Define rules@>=

   if (pv == static_cast<Pointer_Vector<Matrix>*>(0))
      {

          delete c;

          @=$$@> = static_cast<void*>(0);

      }  /* |if (pv == static_cast<Pointer_Vector<Matrix>*>(0))|  */

@q ******* (7) Error handling:  |pv->ctr == 0|.@> 

@ Error handling:  |pv->ctr == 0|.
\initials{LDF 2007.11.28.}

@<Define rules@>=

   else if (pv->ctr == 0)
      {
          delete c;

          @=$$@> = static_cast<void*>(0);

      }  /* |else if (pv->ctr == 0)|  */

@q ******* (7) |pv != static_cast<Pointer_Vector<Matrix>*>(0) && pv->ctr > 0|.@> 

@ |pv != static_cast<Pointer_Vector<Matrix>*>(0) && pv->ctr > 0|.  
Set |@=$$@>| to |*(pv->v[pv->ctr - 1])|.
\initials{LDF 2007.11.28.}

@<Define rules@>=

   else 
      {
         *c = *(pv->v[pv->ctr - 1]);
         @=$$@> = static_cast<void*>(c); 
      }
@q ******* (7) @> 

};

@q * (1) matrix_secondary.  @>
@* \§matrix secondary>.
\initials{LDF 2007.11.28.}

\LOG
\initials{LDF 2007.11.28.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> matrix_secondary@>
  
@q ** (2) matrix secondary --> matrix_primary.@>
@*1 \§matrix secondary> $\longrightarrow$ \§matrix primary>.

\LOG
\initials{LDF 2007.11.28.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=matrix_secondary: matrix_primary@>@/
{

  @=$$@> = @=$1@>;

};

@q ***** (5) matrix_secondary --> matrix_secondary @>
@q ***** (5) TIMES matrix_primary.                 @>

@*4 \§matrix secondary> $\longrightarrow$ \§matrix secondary> 
\§TIMES> \§matrix primary>. 
\initials{LDF 2007.12.20.}

\LOG
\initials{LDF 2007.12.20.}
Added this rule.
\ENDLOG

@<Define rules@>= 
@=matrix_secondary: matrix_secondary TIMES matrix_primary@>
{
    @=$$@> = matrix_secondary_rule_func_0(@=$1@>, @=$3@>, parameter);
};

@q * (1) matrix tertiary.@>
@* \§matrix tertiary>.
\initials{LDF 2007.11.28.}

\LOG
\initials{LDF 2007.11.28.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> matrix_tertiary@>

@q ** (2) matrix tertiary --> matrix_secondary.  @>
@*1 \§matrix tertiary> $\longrightarrow$ \§matrix secondary>.
\initials{LDF 2007.11.28.}

\LOG
\initials{LDF 2007.11.28.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=matrix_tertiary: matrix_secondary@>@/
{

  @=$$@> = @=$1@>;

};

@q * (1) matrix expression.@>
@* \§matrix expression>.
\initials{LDF 2007.11.28.}

\LOG
\initials{LDF 2007.11.28.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> matrix_expression@>

@q ** (2) matrix expression --> matrix_tertiary.  @>
@*1 \§matrix expression> $\longrightarrow$ \§matrix tertiary>.

\LOG
\initials{LDF 2007.11.28.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=matrix_expression: matrix_tertiary@>@/
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

