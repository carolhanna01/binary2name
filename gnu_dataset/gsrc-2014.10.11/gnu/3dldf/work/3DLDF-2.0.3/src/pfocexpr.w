@q pfocexpr.w @> 
@q Created by Laurence Finston Mon Jun  7 22:32:27 CEST 2004  @>
     
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



@q * (0) focus expressions.  @>
@** {\bf focus} expressions.

\LOG
\initials{LDF 2004.06.07.}  
Created this file.
\ENDLOG 


@q * (1) focus primary.  @>
@* \§focus primary>.
  
@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> focus_primary@>@/

@q ** (2) focus_primary --> focus_variable.@>
@*1 \§focus primary> $\longrightarrow$ \§focus variable>.  


\LOG
\initials{LDF 2004.11.22.}
No longer issuing an error message if |entry->object == 0|.
This condition occurs legitimately when one tries to show
an ``unknown |focus|''.
\ENDLOG 

@<Define rules@>=
@=focus_primary: focus_variable@>@/
{

  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

  if (entry == static_cast<Id_Map_Entry_Node>(0) || entry->object == static_cast<void*>(0))
    {

      @=$$@> = static_cast<void*>(0);

    } /* |if (entry == 0 || entry->object == 0)|  */

  else /* |entry != 0 && entry->object != 0|  */@;

  {
    Focus* s = create_new<Focus>(static_cast<Focus*>(entry->object));
  
  
    @=$$@> = static_cast<void*>(s);
 
  }   /* |else| (|entry != 0 && entry->object != 0|)  */@;

};

@q *** (3) focus_primary --> ( focus_expression )  @>
@*2 \§focus primary> $\longrightarrow$ \.{\LP} \§focus expression> \.{\RP}.
\initials{LDF Undated.}

@<Define rules@>=
@=focus_primary: LEFT_PARENTHESIS focus_expression RIGHT_PARENTHESIS@>@/
{

  @=$$@> = @=$2@>;

};

@q *** (3) focus_primary --> LAST focus_vector_expression.@>

@*2 \§focus primary> $\longrightarrow$ 
\.{LAST} \§focus vector expression>.
\initials{LDF 2005.01.18.}

\LOG
\initials{LDF 2005.01.18.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=focus_primary: LAST focus_vector_expression@>@/
{ 
   Focus* p;

         p = create_new<Focus>(0);

   Pointer_Vector<Focus>* pv 
      = static_cast<Pointer_Vector<Focus>*>(@=$2@>);

@q ******* (7) Error handling:  |pv == 0|.@> 

@ Error handling:  |pv == 0|.
\initials{LDF 2005.01.18.}

@<Define rules@>=

   if (pv == static_cast<Pointer_Vector<Focus>*>(0))
      {

          delete p;

          @=$$@> = static_cast<void*>(0);

      }  /* |if (pv == 0)|  */

@q ******* (7) Error handling:  |pv->ctr == 0|.@> 

@ Error handling:  |pv->ctr == 0|.
\initials{LDF 2005.01.18.}

@<Define rules@>=

   else if (pv->ctr == 0)
      {

          delete p;

          @=$$@> = static_cast<void*>(0);

      }  /* |else if (pv->ctr == 0)|  */

@q ******* (7) |pv != 0 && pv->ctr > 0|.@> 

@ |pv != 0 && pv->ctr > 0|.  Set |@=$$@>| to |*(pv->v[pv->ctr - 1])|.
\initials{LDF 2005.01.18.}

@<Define rules@>=

   else 
      {
         *p = *(pv->v[pv->ctr - 1]);
         @=$$@> = static_cast<void*>(p); 
      }
@q ******* (7) @> 

};


@q ** (2) focus secondary.  @>
@*1 \§focus secondary>.
\initials{LDF Undated.}

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> focus_secondary@>
  
@q *** (3) focus secondary --> focus_primary.@>
@*2 \§focus secondary> $\longrightarrow$ \§focus primary>.

@<Define rules@>=
@=focus_secondary: focus_primary@>@/
{

  @=$$@> = @=$1@>;

};

@q ** (2) focus tertiary.  @>
@*1 \§focus tertiary>.
\initials{LDF Undated.}

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> focus_tertiary@>

@q *** (3) focus tertiary --> focus_secondary.@>
@*2 \§focus tertiary> $\longrightarrow$ \§focus secondary>.
\initials{LDF Undated.}

@<Define rules@>=
@=focus_tertiary: focus_secondary@>@/
{

  @=$$@> = @=$1@>;

};


@q ** (2) focus expression.  @>
@*1 \§focus expression>.
\initials{LDF Undated.}

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> focus_expression@>

@q *** (3) focus expression --> focus_tertiary.@>
@*2 \§focus expression> $\longrightarrow$ \§focus tertiary>.
\initials{LDF Undated.}

@<Define rules@>=
@=focus_expression: focus_tertiary@>@/
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

