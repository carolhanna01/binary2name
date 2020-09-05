@q mtrxassn.w @> 
@q Created by Laurence Finston (LDF) Thu Nov 29 20:03:21 CET 2007 @>


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



@q **** (4) matrix assignments.@>
@*3 matrix assignments. 
\initials{LDF 2007.11.28.}

@q ***** (5) assignment --> matrix_variable := matrix_expression.@>   
@*4 \§assignment> $\longrightarrow$ \§matrix variable> 
\.{:=} \§matrix expression>. 
\initials{LDF 2007.11.28.}

\LOG
\initials{LDF 2007.11.28.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>= 
 
@=assignment: matrix_variable ASSIGN matrix_expression@>
{

  Matrix* m = static_cast<Matrix*>(@=$3@>);

  Int_Void_Ptr ivp = 
     Scan_Parse::assign_simple<Matrix>(static_cast<Scanner_Node>(parameter),
                                       "Matrix",
                                       @=$1@>,
                                       m);

  @=$$@> = ivp.v;

};

@q ***** (5) matrix element assignment.@>
@*3 matrix element assignment. 
\initials{LDF 2007.11.28.}

@q ***** (5) assignment --> matrix_variable LEFT_PARENTHESIS @>
@q ***** (5) numeric_expression COMMA numeric_expression     @>
@q ***** (5) RIGHT_PARENTHESIS ASSIGN numeric_expression.    @>   

@*4 \§assignment> $\longrightarrow$ \§matrix variable> 
\.{LEFT\_PARENTHESIS} \§numeric expression> \.{COMMA} \§numeric expression> 
\.{RIGHT\_PARENTHESIS} \.{ASSIGN} \§numeric expression>. 
\initials{LDF 2007.11.28.}

\LOG
\initials{LDF 2007.11.28.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>= 
 
@=assignment: matrix_variable LEFT_PARENTHESIS numeric_expression@>@/ 
@=COMMA numeric_expression RIGHT_PARENTHESIS ASSIGN numeric_expression@>@/   
{

  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

  if (entry && entry->object)
  {
     int status 
        = static_cast<Matrix*>(entry->object)->set_element(@=$3@>, 
                                                           @=$5@>, 
                                                           static_cast<void*>(&@=$8@>), 
                                                           Matrix::MATRIX_REAL_TYPE,
                                                           parameter); 
  }   

  @=$$@> = static_cast<void*>(0);

};

@q ***** (5) assignment --> matrix_variable LEFT_PARENTHESIS @>
@q ***** (5) numeric_expression COMMA numeric_expression     @>
@q ***** (5) RIGHT_PARENTHESIS ASSIGN complex_expression.    @>   

@*4 \§assignment> $\longrightarrow$ \§matrix variable> 
\.{LEFT\_PARENTHESIS} \§numeric expression> \.{COMMA} \§numeric expression> 
\.{RIGHT\_PARENTHESIS} \.{ASSIGN} \§complex expression>. 
\initials{LDF 2007.12.18.}

\LOG
\initials{LDF 2007.12.18.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>= 
 
@=assignment: matrix_variable LEFT_PARENTHESIS numeric_expression@>@/ 
@=COMMA numeric_expression RIGHT_PARENTHESIS ASSIGN complex_expression@>@/   
{

  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

#if (HAVE_LIBGSL & HAVE_GSL_GSL_COMPLEX_H)
  if (entry && entry->object)
  {

     Complex* c = static_cast<Complex*>(@=$8@>);

     int status 
        = static_cast<Matrix*>(entry->object)->set_element(@=$3@>, 
                                                           @=$5@>, 
                                                           &c->complex,
                                                           Matrix::MATRIX_COMPLEX_TYPE,
                                                           parameter); 
     delete c;
  }   
#endif 

  @=$$@> = static_cast<void*>(0);

};

@q ***** (5) Set matrix to identity matrix.@>
@*3 Set matrix to identity matrix.
\initials{LDF 2007.11.29.}

@q ***** (5) assignment --> matrix_variable ASSIGN IDENTITY_MATRIX.@>


@*4 \§assignment> $\longrightarrow$ \§matrix variable> 
\.{ASSIGN} \.{IDENTITY\_MATRIX}.
\initials{LDF 2007.11.29.}

\LOG
\initials{LDF 2007.11.29.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>= 
 
@=assignment: matrix_variable ASSIGN IDENTITY_MATRIX@>@/   
{

  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

  if (entry && entry->object)
  {
     int status 
        = static_cast<Matrix*>(entry->object)->set_identity(parameter); 
  }   

  @=$$@> = static_cast<void*>(0);

};

@q ***** (5) Set matrix to zero matrix.@>
@*3 Set matrix to zero matrix.
\initials{LDF 2007.11.29.}

@q ***** (5) assignment --> matrix_variable ASSIGN ZERO_MATRIX.@>

@*4 \§assignment> $\longrightarrow$ \§matrix variable> 
\.{ASSIGN} \.{ZERO\_MATRIX}.
\initials{LDF 2007.11.29.}

\LOG
\initials{LDF 2007.11.29.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>= 
 
@=assignment: matrix_variable ASSIGN ZERO_MATRIX@>@/   
{

  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

  if (entry && entry->object)
  {
     int status 
        = static_cast<Matrix*>(entry->object)->set_zero(parameter); 
  }   

  @=$$@> = static_cast<void*>(0);

};

@q ***** (5) Set all elements of |matrix| to 1.@>
@*3 Set all elements of {\bf matrix} to 1.
\initials{LDF 2007.11.29.}

@q ***** (5) assignment --> matrix_variable ASSIGN ONE_MATRIX.@>

@*4 \§assignment> $\longrightarrow$ \§matrix variable> 
\.{ASSIGN} \.{ONE\_MATRIX}.
\initials{LDF 2007.11.29.}

\LOG
\initials{LDF 2007.11.29.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>= 
 
@=assignment: matrix_variable ASSIGN ONE_MATRIX@>@/   
{

  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

  if (entry && entry->object)
  {
     int status 
        = static_cast<Matrix*>(entry->object)->set_one(parameter); 
  }   

  @=$$@> = static_cast<void*>(0);

};


@q ***** (5) Set all elements of |matrix| to a single value.@>
@*3 Set all elements of {\bf matrix} to a single value.
\initials{LDF 2007.11.29.}

@q ***** (5) assignment --> matrix_variable ASSIGN numeric_expression.@>

@*4 \§assignment> $\longrightarrow$ \§matrix variable> 
\.{ASSIGN} \§numeric expression>.
\initials{LDF 2007.11.29.}

\LOG
\initials{LDF 2007.11.29.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>= 
 
@=assignment: matrix_variable ASSIGN numeric_expression@>@/   
{

  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

  if (entry && entry->object)
  {
     int status 
        = static_cast<Matrix*>(entry->object)->set_all(@=$3@>, parameter); 
  }   

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

