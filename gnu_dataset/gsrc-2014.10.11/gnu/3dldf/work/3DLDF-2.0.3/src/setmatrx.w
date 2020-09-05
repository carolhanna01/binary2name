@q setmatrx.w @> 
@q Created by Laurence Finston (LDF) Wed Nov 28 20:16:10 CET 2007 @>       


@q * (0) Copyright and License.@>

@q This file is part of GNU 3DLDF, a package for three-dimensional drawing. @>
@q Copyright (C) 2007, 2008, 2009, 2010, @>
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


@q *** (3) command --> SET matrix_variable set_matrix_option_list.@> 

@*2 \§command> $\longrightarrow$ \.{SET} \§matrix variable> 
\§set matrix option list>.
\initials{LDF 2007.11.28.}

\LOG
\initials{LDF 2007.11.28.}
Added this rule.
\ENDLOG 

@q *** (3) Definition.@>

@<Define rules@>= 
@=command: SET matrix_variable LEFT_PARENTHESIS numeric_expression COMMA @>@/
@=numeric_expression RIGHT_PARENTHESIS set_matrix_option_list@>@/
{

   Scan_Parse::set_matrix_rule_func_0(@=$2@>, 
                                      @=$4@>,
                                      @=$6@>, 
                                      parameter);

   @=$$@> = static_cast<void*>(0);

};

@q * (1) set_matrix_option_list.@>   
@* \§set matrix option list>.
\initials{LDF 2007.11.28.}

\LOG
\initials{LDF 2007.11.28.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> set_matrix_option_list@>

@q ** (2) set_matrix_option_list --> EMPTY.@>   
@* \§set matrix option list> $\longrightarrow$ \.{EMPTY}.
\initials{LDF 2007.11.28.}

\LOG
\initials{LDF 2007.11.28.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_matrix_option_list: /* Empty  */@>@/        
{
   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

   if (scanner_node->matrix_options != static_cast<Matrix_Options*>(0))
      scanner_node->matrix_options->clear();
   else
      scanner_node->matrix_options = new Matrix_Options;

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_matrix_option_list --> set_matrix_option_list @>   
@q ** (2) set_matrix_option.                                @>   

@* \§set matrix option list> $\longrightarrow$ 
\§set matrix option list> \§set matrix option>. 
\initials{LDF 2007.11.28.}

\LOG
\initials{LDF 2007.11.28.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_matrix_option_list: set_matrix_option_list set_matrix_option@>@/        
{

   @=$$@> = static_cast<void*>(0);

};

@q * (1) set_matrix_option.@>   
@* \§set matrix option>.
\initials{LDF 2007.11.28.}

\LOG
\initials{LDF 2007.11.28.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> set_matrix_option@>

@q ** (2) set_matrix_option_list --> WITH_TYPE DOUBLE_TYPE_MATRIX.@>   
@* \§set matrix option list> $\longrightarrow$ \.{WITH\_TYPE}
\.{DOUBLE\_TYPE\_MATRIX}.
\initials{LDF 2007.12.18.}

\LOG
\initials{LDF 2007.12.18.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_matrix_option: WITH_TYPE DOUBLE_TYPE_MATRIX@>@/
{

   Matrix_Options* options = static_cast<Scanner_Node>(parameter)->matrix_options;
   
   options->type = Matrix::MATRIX_DOUBLE_TYPE;

   @=$$@> = static_cast<void*>(0);

};
@q ** (2) set_matrix_option_list --> WITH_TYPE FLOAT_TYPE_MATRIX.@>   
@* \§set matrix option list> $\longrightarrow$ \.{WITH\_TYPE}
\.{FLOAT\_TYPE\_MATRIX}.
\initials{LDF 2007.11.28.}

\LOG
\initials{LDF 2007.11.28.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_matrix_option: WITH_TYPE FLOAT_TYPE_MATRIX@>@/
{

   Matrix_Options* options = static_cast<Scanner_Node>(parameter)->matrix_options;
   
   options->type = Matrix::MATRIX_FLOAT_TYPE;

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_matrix_option_list --> WITH_TYPE COMPLEX_TYPE_MATRIX.@>   
@* \§set matrix option list> $\longrightarrow$ \.{WITH\_TYPE}
\.{COMPLEX\_TYPE\_MATRIX}.
\initials{LDF 2007.12.18.}

\LOG
\initials{LDF 2007.12.18.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_matrix_option: WITH_TYPE COMPLEX_TYPE_MATRIX@>@/
{

   Matrix_Options* options = static_cast<Scanner_Node>(parameter)->matrix_options;
   
   options->type = Matrix::MATRIX_COMPLEX_TYPE;

   @=$$@> = static_cast<void*>(0);

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

