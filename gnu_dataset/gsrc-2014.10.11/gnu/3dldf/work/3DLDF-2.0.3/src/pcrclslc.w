@q pcrclslc.w @> 
@q Created by Laurence Finston Mi Mär 30 11:27:18 CEST 2005 @>
       
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



@q * (0) circle_slice expressions.@>
@** {\bf circle\_slice} expressions.
\initials{LDF 2005.03.30.}

\LOG
\initials{LDF 2005.03.30.}
Created this file.
\ENDLOG 

@q ** (2) circle_slice primary.  @>
@*1 \§circle slice primary>.
\initials{LDF 2005.03.30.}  

\LOG
\initials{LDF 2005.03.30.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> circle_slice_primary@>@/

@q *** (3) circle_slice_primary --> circle_slice_variable.@>
@*2 \§circle slice primary> $\longrightarrow$ \§circle slice variable>.  
\initials{LDF 2005.03.30.}

\LOG
\initials{LDF 2005.03.30.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=circle_slice_primary: circle_slice_variable@>@/
{

  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

  if (entry == static_cast<Id_Map_Entry_Node>(0) || entry->object == static_cast<void*>(0))
    {

      @=$$@> = static_cast<void*>(0);

    } /* |if (entry == 0 || entry->object == 0)|  */

  else /* |entry != 0 && entry->object != 0|  */

    @=$$@> = static_cast<void*>(create_new<Circle_Slice>(
                                  static_cast<Circle_Slice*>(
                                     entry->object))); 
};

@q *** (3) circle_slice_primary --> ( circle_slice_expression )  @>
@*2 \§circle slice primary> $\longrightarrow$ `\.{\LP}' 
\§circle slice expression> `\.{\RP}'.
\initials{LDF 2005.03.30.}

\LOG
\initials{LDF 2005.03.30.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=circle_slice_primary: LEFT_PARENTHESIS circle_slice_expression@>@/
@=RIGHT_PARENTHESIS@>@/
{

   @=$$@> = @=$2@>;

};


@q ***** (5) circle_slice_primary --> LAST @>
@q ***** (5) circle_slice_vector_expression.@>

@*4 \§circle slice primary> $\longrightarrow$ 
\.{LAST} \§circle slice vector expression>.
\initials{LDF 2005.03.30.}

\TODO
@q { @>
@:TO DO}{{\bf TO DO}@>
@q } @> 
@q !! TO DO:  @>
\initials{LDF 2005.03.30.}
Add this rule.
\ENDTODO 


@q ** (2) circle_slice secondary.  @>
@*1 \§circle slice secondary>.
\initials{LDF 2005.03.30.}

\LOG
\initials{LDF 2005.03.30.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> circle_slice_secondary@>
  
@q *** (3) circle_slice secondary --> circle_slice_primary.  @>
@*2 \§circle slice secondary> $\longrightarrow$ 
\§circle slice primary>.
\initials{LDF 2005.03.30.}

\LOG
\initials{LDF 2005.03.30.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=circle_slice_secondary: circle_slice_primary@>@/
{

   @=$$@> = @=$1@>;

};

@q ** (2) circle_slice tertiary.  @>
@*1 \§circle slice tertiary>.
\initials{LDF 2005.03.30.}

\LOG
\initials{LDF 2005.03.30.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> circle_slice_tertiary@>

@q *** (3) circle_slice tertiary --> circle_slice_secondary.@>
@*2 \§circle slice tertiary> $\longrightarrow$ 
\§circle slice secondary>.
\initials{LDF 2005.03.30.}

\LOG
\initials{LDF 2005.03.30.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=circle_slice_tertiary: circle_slice_secondary@>@/
{

  @=$$@> = @=$1@>;

};


@q ** (2) circle_slice expression.@>
@*1 \§circle slice expression>.
\initials{LDF 2005.03.30.}

\LOG
\initials{LDF 2005.03.30.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> circle_slice_expression@>

@q *** (3) circle_slice expression --> circle_slice_tertiary.  @>
@*2 \§circle slice expression> $\longrightarrow$ \§circle slice tertiary>.
\initials{LDF 2005.03.30.}

\LOG
\initials{LDF 2005.03.30.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=circle_slice_expression: circle_slice_tertiary@>@/
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

