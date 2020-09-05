@q pcolexpr.w @> 
@q Created by Laurence Finston Tue May 25 12:54:45 MEST 2004  @>
     
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



@q * (0) color expressions.  @>
@** \§color expressions>.

\LOG
\initials{LDF 2004.05.25.}  
Created this file.
\ENDLOG 


@q ** (2) color primary.  @>
@*1 \§color primary>.
\initials{LDF Undated.}

  
@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> color_primary@>@/

@q *** (3) color_primary --> color_variable.@>
@*2 \§color primary> $\longrightarrow$ \§color variable>.  
\initials{LDF Undated.} 

\LOG
\initials{LDF 2004.11.22.}
No longer issuing an error message if |entry->object == 0|.
This condition occurs legitimately when one tries to show 
an ``unknown |color|''.
\ENDLOG

@q **** (4) Definition.@>   

@<Define rules@>=
@=color_primary: color_variable@>@/
{

  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

  if (entry == static_cast<Id_Map_Entry_Node>(0) || entry->object == static_cast<void*>(0))
    {

      @=$$@> = static_cast<void*>(0);

    } /* |if (entry == 0 || entry->object == 0)|  */

  else /* |entry != 0 && entry->object != 0|  */@;

  {
    Color* s = new Color;
  
    *s = *static_cast<Color*>(entry->object); 
  
    @=$$@> = static_cast<void*>(s);
  }

};

@q *** (3) color_primary --> color_argument.@>
@*2 \§color primary> $\longrightarrow$ \§color argument>.  
\initials{LDF Undated.} 

@q *** (3) color_primary --> ( color_expression )  @>
@*2 \§color primary> $\longrightarrow$ `\.{\LP}' 
\§color expression> `\.{\RP}'.
\initials{LDF Undated.} 

@q **** (4) Definition.@>   

@<Define rules@>=
@=color_primary: LEFT_PARENTHESIS color_expression RIGHT_PARENTHESIS@>@/
{

     @=$$@> = @=$2@>;

};

@q ***** (5) color_primary --> LAST color_vector_expression.@>

@*4 \§color primary> $\longrightarrow$ 
\.{LAST} \§color vector expression>.
\initials{LDF 2005.01.07.}

\LOG
\initials{LDF 2005.01.07.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=color_primary: LAST color_vector_expression@>@/
{ 

   Color* c;

   c = create_new<Color>(0);

   Pointer_Vector<Color>* pv 
      = static_cast<Pointer_Vector<Color>*>(@=$2@>);

@q ******* (7) Error handling:  |pv == 0|.@> 

@ Error handling:  |pv == 0|.
\initials{LDF 2005.01.07.}

@<Define rules@>=

   if (pv == static_cast<Pointer_Vector<Color>*>(0))
      {

          delete c;

          @=$$@> = static_cast<void*>(0);

      }  /* |if (pv == 0)|  */

@q ******* (7) Error handling:  |pv->ctr == 0|.@> 

@ Error handling:  |pv->ctr == 0|.
\initials{LDF 2005.01.07.}

@<Define rules@>=

   else if (pv->ctr == 0)
      {
          delete c;

          @=$$@> = static_cast<void*>(0);

      }  /* |else if (pv->ctr == 0)|  */

@q ******* (7) |pv != 0 && pv->ctr > 0|.@> 

@ |pv != 0 && pv->ctr > 0|.  Set |@=$$@>| to |*(pv->v[pv->ctr - 1])|.
\initials{LDF 2005.01.07.}

@<Define rules@>=

   else 
      {
         *c = *(pv->v[pv->ctr - 1]);
         @=$$@> = static_cast<void*>(c); 
      }
@q ******* (7) @> 

};


@q ** (2) color secondary.  @>
@*1 \§color secondary>.
\initials{LDF Undated.} 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> color_secondary@>
  
@q *** (3) color secondary --> color_primary.@>
@*2 \§color secondary> $\longrightarrow$ \§color primary>.
\initials{LDF Undated.} 

@<Define rules@>=
@=color_secondary: color_primary@>@/
{

    @=$$@> = @=$1@>;

};

@q *** (3) color secondary --> color_secondary times_or_over @>
@q *** (3) numeric_primary.                                  @> 
@*2 \§color secondary> $\longrightarrow$ \§color secondary>
\§times or over> \§numeric primary>.
\initials{LDF 2004.12.17.}

@<Define rules@>=
@=color_secondary: color_secondary times_or_over numeric_primary@>@/
{
   Color* c = static_cast<Color*>(@=$1@>);

@q **** (4) Error handling:  |c == 0|.@>   

@ Error handling:  |c == 0|.
\initials{LDF 2004.12.17.}

@<Define rules@>=

   if (c == static_cast<Color*>(0))
      {
          @=$$@> = static_cast<void*>(0);

      }  /* |if (c == 0)|  */

@q **** (4) @>   

   else /* |c != 0|  */
      {
         real r = @=$3@>;

@q ***** (5) Multiplication.@>   

          if (@=$2@> == TIMES)
             {
                *c *= r;
 
                @=$$@> = static_cast<void*>(c); 
             }  

@q ***** (5) Division.@>   

   else if (@=$2@> == OVER)

      {

@q ****** (6) Error handling:  Division by 0.@>   
@ Error handling:  Division by 0.
\initials{LDF 2004.12.17.}

@<Define rules@>=

         if (r == ZERO_REAL)
            {

#if 0
                cerr_strm << thread_name 
                          << "ERROR!  In `yyparse()', rule "
                          << "`color_secondary --> "
                          << endl 
                          << "color_secondary times_or_over "
                          << "numeric_primary':"
                          << endl << "`numeric_primary' == 0.  "
                          << "Can't divide."
                          << endl << "Setting `color_secondary' on "
                          << "the left-hand side to null, exiting rule, "
                          << "and will try to continue.";
      
                log_message(cerr_strm);
                cerr_message(cerr_strm, error_stop_value);
                cerr_strm.str("");
#endif 
                delete c;
                c = 0;

                @=$$@> = static_cast<void*>(0);


            }  /* |if (r == 0)|  */

@q ****** (6) |r != 0|.@>   
@ |r != 0|.
\initials{LDF 2004.12.17.}

@<Define rules@>=

      else  /* |r != 0|  */
         {

             *c *= 1/r;

             @=$$@> = static_cast<void*>(c); 
 
         }   /* |else| (|r != 0|)  */

@q ***** (5).@> 

      } /* |else if (@=$2@> == OVER)|  */


@q ***** (5) Error handling:  Invalid value for |times_or_over|.@>   

@ Error handling:  Invalid value for |times_or_over|.
\initials{LDF 2004.12.17.}

@<Define rules@>=

   else  /* Invalid |times_or_over|.  */
      {
#if 0 
          cerr_strm << thread_name 
                    << "ERROR!  In `yyparse()', rule "
                    << "`color_secondary --> "
                    << endl 
                    << "color_secondary times_or_over "
                    << "numeric_primary':"
                    << endl << "Invalid value for `times_or_over'."
                    << endl << "Setting `color_secondary' on "
                    << "the left-hand side "
                    << endl 
                    << "to `color_secondary' on "
                    << "the right-hand side unchanged and continuing.";
      
          log_message(cerr_strm);
          cerr_message(cerr_strm, error_stop_value);
          cerr_strm.str("");
#endif 
          @=$$@> = static_cast<void*>(c); 

      }  /* |else| (Invalid value for |times_or_over|).  */

@q ***** (5).@> 

   }   /* |else| (|c != 0|)  */       

};

@q *** (3) color secondary --> numeric_secondary TIMES @>
@q *** (3) color_primary.                              @> 

@*2 \§color secondary> $\longrightarrow$ \§numeric secondary>
\.{TIMES} \§color primary>.
\initials{LDF 2004.12.18.}

\LOG
\initials{LDF 2004.12.18.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=color_secondary: numeric_secondary TIMES color_primary@>@/
{

   Color* c = static_cast<Color*>(@=$3@>);

@q **** (4) Error handling:  |c == 0|.@>   

@ Error handling:  |c == 0|.
\initials{LDF 2004.12.18.}

@<Define rules@>=

   if (c == static_cast<Color*>(0))
      {

          @=$$@> = static_cast<void*>(0);

      }  /* |if (c == 0)|  */

@q **** (4) @>   

   else /* |c != 0|  */
      {
         real r = @=$1@>;

         *c *= r;
 
         @=$$@> = static_cast<void*>(c); 

      }   /* |else| (|c != 0|)  */       

};




@q *** (3) color_secondary --> numeric_secondary color_variable.@> 

@*2 \§color secondary> $\longrightarrow$ \§numeric secondary> 
\§color variable>.
\initials{LDF 2004.12.17.}

Multiplication without \.{TIMES}.
\initials{LDF 2004.12.17.}

\LOG
\initials{LDF 2004.12.17.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@>   

@<Define rules@>=
@=color_secondary: numeric_secondary color_variable@>@/
{

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$2@>);

@q ***** (5) Error handling:  |entry == 0 || entry->object == 0|.@> 

@ Error handling:  |entry == 0 || entry->object == 0|.
\initials{LDF 2004.12.17.}

@<Define rules@>=

   if (entry == static_cast<Id_Map_Entry_Node>(0) || entry->object == static_cast<void*>(0))
      {
          @=$$@> = static_cast<void*>(0);

      }  /* |if (entry == 0 || entry->object == 0)|  */


@q ***** (5) |entry != 0 && entry->object != 0|.@> 
@ |entry != 0 && entry->object != 0|.
\initials{LDF 2004.12.17.}

@<Define rules@>=

   else  /* |entry != && entry->object != 0|  */
      {
         Color* c;

         c = create_new<Color>(static_cast<Color*>(entry->object));

         *c *= @=$1@>;

         @=$$@> = static_cast<void*>(c); 

      }  /* |else| (|entry != && entry->object != 0|)  */

@q ***** (5).@>    

};




@q ** (2) color tertiary.@>
@*1 \§color tertiary>.
\initials{LDF Undated.} 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> color_tertiary@>

@q *** (3) color tertiary --> color_secondary.@>
@*2 \§color tertiary> $\longrightarrow$ \§color secondary>.

@<Define rules@>=
@=color_tertiary: color_secondary@>@/
{

    @=$$@> = @=$1@>;

};

@q *** (3) color tertiary --> color_tertiary PLUS @>
@q *** (3) color_secondary.                       @>

@*2 \§color tertiary> $\longrightarrow$ \§color tertiary>
\.{PLUS} \§color secondary>.
\initials{LDF 2005.01.06.}

\LOG
\initials{LDF 2004.12.17.}
Added this rule.

\initials{LDF 2005.01.06.}
Replaced ``\§plus or minus>'' with ``\.{PLUS}'' and 
added the rule ``\§color tertiary> $\longrightarrow$ 
\§color tertiary> \.{MINUS} \§color secondary>'' below.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=color_tertiary: color_tertiary PLUS color_secondary@>@/
{

   Color* c = static_cast<Color*>(@=$1@>);    

   Color* d = static_cast<Color*>(@=$3@>);    

@q **** (4) @>   

   if (c == static_cast<Color*>(0) || d == static_cast<Color*>(0))
      {

          delete c;
          delete d;

          c = static_cast<Color*>(0);
          d = static_cast<Color*>(0);

          @=$$@> = static_cast<void*>(0);

      }  /* |if (c == 0 || d == 0)|  */

@q **** (4) |c != 0 && d != 0|.@>   

@ |c != 0 && d != 0|
\initials{LDF 2004.12.17.}

@<Define rules@>=

      else /* |c != 0 && d != 0|  */
         {
             *c += *d;

             @=$$@> = static_cast<void*>(c); 

         }  /* |else| (|c != 0 && d != 0|)  */

@q **** (4) @>   

               delete d;
               d = 0;
};

@q *** (3) color tertiary --> color_tertiary MINUS @>
@q *** (3) color_secondary.                        @>

@*2 \§color tertiary> $\longrightarrow$ \§color tertiary>
\.{MINUS} \§color secondary>.
\initials{LDF 2005.01.06.}

\LOG
\initials{LDF 2005.01.06.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=color_tertiary: color_tertiary MINUS color_secondary@>@/
{

   Color* c = static_cast<Color*>(@=$1@>);    

   Color* d = static_cast<Color*>(@=$3@>);    

@q **** (4) @>   

   if (c == static_cast<Color*>(0) || d == static_cast<Color*>(0))
      {
          delete c;
          delete d;

          c = static_cast<Color*>(0);
          d = static_cast<Color*>(0);

          @=$$@> = static_cast<void*>(0);

      }  /* |if (c == 0 || d == 0)|  */

@q **** (4) |c != 0 && d != 0|.@>   

@ |c != 0 && d != 0|
\initials{LDF 2004.12.17.}

@<Define rules@>=

      else /* |c != 0 && d != 0|  */
         {
             *c -= *d;

             @=$$@> = static_cast<void*>(c); 

         }  /* |else| (|c != 0 && d != 0|)  */

@q **** (4) @>   

               delete d;
               d = static_cast<Color*>(0);
};

@q ** (2) color expression.  @>
@*1 \§color expression>.
\initials{LDF Undated.} 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> color_expression@>

@q *** (3) color expression --> color_tertiary.  @>
@*2 \§color expression> $\longrightarrow$ \§color tertiary>.

@<Define rules@>=
@=color_expression: color_tertiary@>@/
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

