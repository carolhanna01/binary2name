@q poptions.w @> 
@q Created by Laurence Finston (LDF) Fri Jul 20 15:16:48 CEST 2007  @>

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




@q * (0) Options.  @>
@** Options.
\initials{LDF 2007.07.20.}

\LOG
\initials{LDF 2007.07.20.}
Created this file.
\ENDLOG 

@q ** (2) with_tolerance_optional.@> 
@*1 \§with tolerance optional>.
\initials{LDF 2007.07.15.}

\LOG
\initials{LDF 2007.07.15.}
Added this type declaration.

\initials{LDF 2007.07.27.}
Moved this section from \filename{pnumexpr.w} to this file 
(\filename{poptions.w}).
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <real_value> with_tolerance_optional@>@/

@q *** (3) with_tolerance_optional --> EMPTY.@> 
@*2 \§with tolerance optional> $\longrightarrow$ \.{EMPTY}.
\initials{LDF 2007.07.15.}

\LOG
\initials{LDF 2007.07.15.}
Added this rule.

\initials{LDF 2007.07.27.}
Now setting the value of the rule to -1 instead of |INVALID_REAL|.
\ENDLOG

@<Define rules@>= 
@=with_tolerance_optional: /* EMPTY */@>
{
    @=$$@> = -1;

};

@q *** (3) with_tolerance_optional --> WITH_TOLERANCE numeric_expression.@> 
@*2 \§with tolerance optional> $\longrightarrow$ \.{WITH\_TOLERANCE }
\§numeric expression>.
\initials{LDF 2007.07.15.}

\LOG
\initials{LDF 2007.07.15.}
Added this rule.
\ENDLOG

@<Define rules@>= 
@=with_tolerance_optional: WITH_TOLERANCE numeric_expression@>
{
    @=$$@> = @=$2@>;

};
  
@q ** (2) with_increment_optional.@> 
@*1 \§with increment optional>.
\initials{LDF 2007.08.21.}

\LOG
\initials{LDF 2007.08.21.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <real_value> with_increment_optional@>@/

@q *** (3) with_increment_optional --> EMPTY.@> 
@*2 \§with increment optional> $\longrightarrow$ \.{EMPTY}.
\initials{LDF 2007.08.21.}

\LOG
\initials{LDF 2007.08.21.}
Added this rule.
\ENDLOG

@<Define rules@>= 
@=with_increment_optional: /* EMPTY */@>
{
    @=$$@> = -1;

};

@q *** (3) with_increment_optional --> WITH_INCREMENT numeric_expression.@> 
@*2 \§with increment optional> $\longrightarrow$ \.{WITH\_INCREMENT }
\§numeric expression>.
\initials{LDF 2007.08.21.}

\LOG
\initials{LDF 2007.08.21.}
Added this rule.
\ENDLOG

@<Define rules@>= 
@=with_increment_optional: WITH_INCREMENT numeric_expression@>
{
    @=$$@> = @=$2@>;

};
  
@q ** (2) with_test_optional.@>  
@*1 \§with test optional>.
\initials{LDF 2007.07.20.}

\LOG
\initials{LDF 2007.07.20.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <int_value> with_test_optional@>


@q *** (3) with_test_optional:  EMPTY.@>  

@*2 \§with test optional> $\longrightarrow$ \.{EMPTY}.
\initials{LDF 2007.07.20.}

\LOG
\initials{LDF 2007.07.20.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=

@=with_test_optional: /* EMPTY */@>@/
{
   @=$$@> = NULL_VALUE;
};

@q *** (3) with_test_optional: WITH_TEST.@>  

@*2 \§with test optional> $\longrightarrow$ \.{WITH\_TEST}.
\initials{LDF 2007.07.20.}

\LOG
\initials{LDF 2007.07.20.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=

@=with_test_optional: WITH_TEST@>@/
{
   @=$$@> = WITH_TEST;
};


@q *** (3) with_test_optional: WITH_NO_TEST.@>  

@*2 \§with test optional> $\longrightarrow$ \.{WITH\_NO\_TEST}.
\initials{LDF 2007.07.20.}

\LOG
\initials{LDF 2007.07.20.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=

@=with_test_optional: WITH_NO_TEST@>@/
{
   @=$$@> = WITH_NO_TEST;
};

@q ** (2) with_cull_optional.@>  
@*1 \§with cull optional>.
\initials{LDF 2007.07.27.}

\LOG
\initials{LDF 2007.07.27.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <int_value> with_cull_optional@>


@q *** (3) with_cull_optional:  EMPTY.@>  

@*2 \§with cull optional> $\longrightarrow$ \.{EMPTY}.
\initials{LDF 2007.07.27.}

\LOG
\initials{LDF 2007.07.27.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=

@=with_cull_optional: /* EMPTY */@>@/
{
   @=$$@> = NULL_VALUE;
};

@q *** (3) with_cull_optional: WITH_CULL.@>  

@*2 \§with cull optional> $\longrightarrow$ \.{WITH\_CULL}.
\initials{LDF 2007.07.27.}

\LOG
\initials{LDF 2007.07.27.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=

@=with_cull_optional: WITH_CULL@>@/
{
   @=$$@> = WITH_CULL;
};


@q *** (3) with_cull_optional: WITH_NO_CULL.@>  

@*2 \§with cull optional> $\longrightarrow$ \.{WITH\_NO\_CULL}.
\initials{LDF 2007.07.27.}

\LOG
\initials{LDF 2007.07.27.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=

@=with_cull_optional: WITH_NO_CULL@>@/
{
   @=$$@> = WITH_NO_CULL;
};

@q ** (2) with_rectify_optional.@>  
@*1 \§with rectify optional>.
\initials{LDF 2007.07.20.}

\LOG
\initials{LDF 2007.07.20.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <int_value> with_rectify_optional@>


@q *** (3) with_rectify_optional:  EMPTY.@>  

@*2 \§with rectify optional> $\longrightarrow$ \.{EMPTY}.
\initials{LDF 2007.07.20.}

\LOG
\initials{LDF 2007.07.20.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=

@=with_rectify_optional: /* EMPTY */@>@/
{
   @=$$@> = NULL_VALUE;
};

@q *** (3) with_rectify_optional: WITH_RECTIFY.@>  

@*2 \§with rectify optional> $\longrightarrow$ \.{WITH\_RECTIFY}.
\initials{LDF 2007.07.20.}

\LOG
\initials{LDF 2007.07.20.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=

@=with_rectify_optional: WITH_RECTIFY@>@/
{
   @=$$@> = WITH_RECTIFY;
};


@q *** (3) with_rectify_optional: WITH_NO_RECTIFY.@>  

@*2 \§with rectify optional> $\longrightarrow$ \.{WITH\_NO\_RECTIFY}.
\initials{LDF 2007.07.20.}

\LOG
\initials{LDF 2007.07.20.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=

@=with_rectify_optional: WITH_NO_RECTIFY@>@/
{
   @=$$@> = WITH_NO_RECTIFY;
};


@q * (0) @>

@q   Emacs-Lisp code for use in indirect buffers when using the          @>
@q   GNU Emacs editor.  The local variable list is not evaluated when an @>
@q   indirect buffer is visited, so it's necessary to evaluate the       @>
@q   following s-expression in order to use the facilities normally      @>
@q   accessed via the local variables list.                              @>
@q   \initials{LDF 2004.02.12}.                                          @>
@q   (progn (cweb-mode) (outline-minor-mode t) (setq fill-column 70))    @>




@q Local Variables: @>
@q mode:CWEB  @>
@q eval:(outline-minor-mode t)  @>
@q abbrev-file-name:"~/.abbrev_defs" @>
@q eval:(read-abbrev-file)  @>
@q fill-column:70 @>
@q run-cweave-on-file:"3DLDFprg.web"  @>
@q End: @>
