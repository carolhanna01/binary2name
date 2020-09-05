@q ppenexpr.w @> 
@q Created by Laurence Finston Fri May 21 18:28:59 MEST 2004  @>
     
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



@q * (0) pen expressions.  @>
@** {\bf pen} expressions.

\LOG
\initials{LDF 2004.05.21.}  
Created this file.
\ENDLOG 


@q * (1) pen primary.  @>
@* \§pen primary>.
  
@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> pen_primary@>@/

@q ** (2) pen_primary --> pen_variable.@>
@*1 \§pen primary> $\longrightarrow$ \§pen variable>.  


\LOG
\initials{LDF 2004.06.07.}  
Now calling |create_new<Pen>()| rather than
|new Pen|.

\initials{LDF 2004.11.22.}
No longer issuing an error message if |entry->object == 0|.
This condition occurs legitimately when one tries to show an 
``unknown |pen|''.
\ENDLOG 

@q *** (3) Definition.@> 

@<Define rules@>=
@=pen_primary: pen_variable@>@/
{

  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

  if (entry == static_cast<Id_Map_Entry_Node>(0) || entry->object == static_cast<void*>(0))
    {

      @=$$@> = static_cast<void*>(0);

    } /* |if (entry == 0 || entry->object == 0)|  */

  else /* |entry != 0 && entry->object != 0|  */@;

  {
    Pen* s
      = create_new<Pen>(static_cast<Pen*>(entry->object));
  
  
    @=$$@> = static_cast<void*>(s);
  }

};

@q ** (2) pen_primary --> pen_argument.@>
@*1 \§pen primary> $\longrightarrow$ \§pen argument>.  



@q ** (2) pen_primary --> PENCIRCLE  @>
@*1 \§pen primary> $\longrightarrow$ `\.{PENCIRCLE}'.

@<Define rules@>=
@=pen_primary: PENCIRCLE@>@/
{

  Pen* p = create_new<Pen>(0);

  p->set_type(Pen::CIRCLE_PEN);
  p->set_name("pencircle");

  @=$$@> = static_cast<void*>(p); 

};


@q *** (3) pen_primary --> PENRAZOR  @>
@*2 \§pen primary> $\longrightarrow$ \.{PENRAZOR}.


@<Define rules@>=
@=pen_primary: PENRAZOR@>@/
{

  Pen* p = create_new<Pen>(0);

  p->set_type(Pen::RAZOR_PEN);
  p->set_name("penrazor");

  @=$$@> = static_cast<void*>(p); 

};

@q *** (3) pen_primary --> PENSPECK  @>
@*2 \§pen primary> $\longrightarrow$ `\.{PENSPECK}'.


@<Define rules@>=
@=pen_primary: PENSPECK@>@/
{

  Pen* p = create_new<Pen>(0);

  p->set_type(Pen::SPECK_PEN);
  p->set_name("penspeck");

  @=$$@> = static_cast<void*>(p); 

};

@q *** (3) pen_primary --> PENSQUARE  @>
@*2 \§pen primary> $\longrightarrow$ \.{PENSQUARE}.

@<Define rules@>=
@=pen_primary: PENSQUARE@>@/
{

  Pen* p = create_new<Pen>(0);

  p->set_type(Pen::SQUARE_PEN);
  p->set_name("pensquare");

  @=$$@> = static_cast<void*>(p); 

};



@q *** (3) pen_primary --> ( pen_expression )  @>
@*2 \§pen primary> $\longrightarrow$ `\.{\LP}' 
\§pen expression> `\.{\RP}'.

@<Define rules@>=
@=pen_primary: LEFT_PARENTHESIS pen_expression RIGHT_PARENTHESIS@>@/
{

  @=$$@> = @=$2@>;

};

@q ***** (5) pen_primary --> LAST pen_vector_expression.@>

@*4 \§pen primary> $\longrightarrow$ 
\.{LAST} \§pen vector expression>.
\initials{LDF 2005.01.13.}

\LOG
\initials{LDF 2005.01.13.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=pen_primary: LAST pen_vector_expression@>@/
{ 
   Pen* p;

         p = create_new<Pen>(0);

   Pointer_Vector<Pen>* pv 
      = static_cast<Pointer_Vector<Pen>*>(@=$2@>);

@q ******* (7) Error handling:  |pv == 0|.@> 

@ Error handling:  |pv == 0|.
\initials{LDF 2005.01.13.}

@<Define rules@>=

   if (pv == static_cast<Pointer_Vector<Pen>*>(0))
      {
          delete p;

          @=$$@> = static_cast<void*>(0);

      }  /* |if (pv == 0)|  */

@q ******* (7) Error handling:  |pv->ctr == 0|.@> 

@ Error handling:  |pv->ctr == 0|.
\initials{LDF 2005.01.13.}

@<Define rules@>=

   else if (pv->ctr == 0)
      {

          delete p;

          @=$$@> = static_cast<void*>(0);

      }  /* |else if (pv->ctr == 0)|  */

@q ******* (7) |pv != 0 && pv->ctr > 0|.@> 

@ |pv != 0 && pv->ctr > 0|.  Set |@=$$@>| to |*(pv->v[pv->ctr - 1])|.
\initials{LDF 2005.01.13.}

@<Define rules@>=

   else 
      {
         *p = *(pv->v[pv->ctr - 1]);
         @=$$@> = static_cast<void*>(p); 
      }
@q ******* (7) @> 

};


@q ** (2) pen secondary.  @>
@*1 \§pen secondary>.

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> pen_secondary@>
  
@q *** pen secondary --> pen_primary.@>
@ \§pen secondary> $\longrightarrow$ \§pen primary>.

@<Define rules@>=
@=pen_secondary: pen_primary@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3) pen secondary --> pen_secondary transformer.@>
@*2 \§pen secondary> $\longrightarrow$ \§pen secondary> \§transformer>.


!! START HERE.  LDF 2004.09.07.  
3D transformations don't work on |pens|.  They won't work for 
|dash_patterns|, either.  Must define new type of |transformer| 
for them.

\LOG
@:BUG FIX@> BUG FIX: |pen_secondary| on the right-hand side of the rule
was formerly |pen_primary|.  This caused input with |pen_secondaries| 
with multiple transformers to fail.

\initials{LDF 2006.01.20.}
@:BUG FIX@> BUG FIX:  Now deleting |Transform* t|.
\ENDLOG 


@<Define rules@>=
@=pen_secondary: pen_secondary transformer@>@/
{

  Pen* p = static_cast<Pen*>(@=$1@>);
  Transform* t = static_cast<Transform*>(@=$2@>); 

  *p *= *t;
    
  delete t;

  @=$$@> = static_cast<void*>(p); 

};


@q ** (2) pen tertiary.  @>
@*1 \§pen tertiary>.

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> pen_tertiary@>

@q *** pen tertiary --> pen_secondary.@>
@ \§pen tertiary> $\longrightarrow$ \§pen secondary>.

@<Define rules@>=
@=pen_tertiary: pen_secondary@>@/
{

  @=$$@> = @=$1@>;

};


@q ** (2) pen expression.  @>
@*1 \§pen expression>.

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> pen_expression@>

@q *** (3) pen expression --> pen_tertiary.@>
@*2 \§pen expression> $\longrightarrow$ \§pen tertiary>.

@<Define rules@>=
@=pen_expression: pen_tertiary@>@/
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

