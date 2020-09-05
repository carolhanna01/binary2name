@q pnmvexpr.w @> 
@q Created by Laurence Finston Fri Jan  7 11:08:25 CET 2005 @>
     
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



@q * (0) |numeric_vector| expressions.@>
@** {\bf numeric\_vector} expressions.
\initials{LDF 2005.01.07.}

\LOG
\initials{LDF 2005.01.07.}
Created this file and wrote quite a few rules.  
\ENDLOG 


@q * (1) |numeric_vector| primary.  @>
@* \§numeric vector primary>.
\initials{LDF 2005.01.07.}
  
\LOG
\initials{LDF 2005.01.07.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> numeric_vector_primary@>@/

@q ** (2) numeric_vector_primary --> numeric_vector_variable.@>
@*1 \§numeric vector primary> $\longrightarrow$ 
\§numeric vector variable>.  
\initials{LDF 2005.01.07.}

\LOG
\initials{LDF 2005.01.07.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=numeric_vector_primary: numeric_vector_variable@>@/ 
{

  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

@q **** (4) Error handling:  |entry == 0 || entry->object == 0|.@> 

@ Error handling:  |entry == 0 || entry->object == 0|.
\initials{LDF 2005.01.07.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0) || entry->object == static_cast<void*>(0))
    {

      @=$$@> = static_cast<void*>(0);

    } /* |if (entry == 0 || entry->object == 0)|  */

@q **** (4) |!(entry == 0 || entry->object == 0)|.@> 

@ |!(entry == 0 || entry->object == 0)|.
\initials{LDF 2005.01.07.}

@<Define rules@>=
  else /* |!(entry == 0 || entry->object == 0)|  */

     {

        typedef Pointer_Vector<real> PV;

        PV* pv = new PV;

        *pv = *static_cast<PV*>(entry->object);

        @=$$@> = static_cast<void*>(pv);                    

     }  /* |else| (|!(entry == 0 || entry->object == 0)|)  */

};


@q ** (2) numeric_vector_primary --> LEFT_PARENTHESIS  @>
@q ** (2) numeric_vector_expression  RIGHT_PARENTHESIS.@>

@*1 \§numeric vector primary> $\longrightarrow$ \.{LEFT\_PARENTHESIS}
\§numeric vector expression> \.{RIGHT\_PARENTHESIS}.
\initials{LDF 2005.01.07.}

\LOG
\initials{LDF 2005.01.07.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=numeric_vector_primary: LEFT_PARENTHESIS@>@/ 
@=numeric_vector_expression RIGHT_PARENTHESIS@>@/ 
{
   
  @=$$@> = @=$1@>;

};

@q ** (2) numeric_vector_primary --> MEASURE_TEXT string_expression.@>

@*1 \§numeric vector primary> $\longrightarrow$ 
\.{MEASURE\_TEXT} \§string expression>.
\initials{LDF 2005.08.30.}

\LOG
\initials{LDF 2005.08.30.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=numeric_vector_primary: MEASURE_TEXT string_expression@>@/ 
{
   
    Pointer_Vector<real>* pv = new Pointer_Vector<real>;
 
    int status = Scan_Parse::measure_text_func(
                                static_cast<Scanner_Node>(parameter), 
                                @=$2@>, 
                                pv);

@q ****** (6)@> 

    if (status == 0)
       @=$$@> = static_cast<void*>(pv);

@q ****** (6)@> 

    else /* |status != 0|  */
       {

           @=$$@> = static_cast<void*>(0);

       }  /* |else|  (|status != 0|)  */
};


@q * (1) numeric_vector secondary.@>

@* \§numeric vector secondary>.
\initials{LDF 2005.01.07.}

\LOG
\initials{LDF 2005.01.07.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> numeric_vector_secondary@>
  
@q ** (2) numeric_vector secondary --> numeric_vector_primary.@>
@*1 \§numeric vector secondary> $\longrightarrow$ 
\§numeric vector primary>.
\initials{LDF 2005.01.07.}

\LOG
\initials{LDF 2005.01.07.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=numeric_vector_secondary: numeric_vector_primary@>@/ 
{

  @=$$@> = @=$1@>;

};


@q * (1) numeric_vector tertiary.  @>

@* \§numeric vector tertiary>.
\initials{LDF 2005.01.07.}

\LOG
\initials{LDF 2005.01.07.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> numeric_vector_tertiary@>

@q ***** (5) numeric_vector tertiary --> numeric_vector_secondary.@>
@ \§numeric vector tertiary> $\longrightarrow$ 
\§numeric vector secondary>.
\initials{LDF 2005.01.07.}

\LOG
\initials{LDF 2005.01.07.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=numeric_vector_tertiary: numeric_vector_secondary@>@/ 
{

  @=$$@> = @=$1@>;

};

@q * (1) numeric_vector expression.@>
@* \§numeric vector expression>.

\LOG
\initials{LDF 2005.01.07.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> numeric_vector_expression@>

@q ** (2) numeric_vector expression --> numeric_vector_tertiary.  @>
@*1 \§numeric vector expression> $\longrightarrow$ 
\§numeric vector tertiary>.

\LOG
\initials{LDF 2005.01.07.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=numeric_vector_expression: numeric_vector_tertiary@>@/ 
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


