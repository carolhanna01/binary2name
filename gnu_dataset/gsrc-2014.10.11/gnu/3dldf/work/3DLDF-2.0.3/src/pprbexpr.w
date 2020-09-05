@q pprbexpr.w @> 
@q Created by Laurence Finston Mon Nov  7 19:41:56 CET 2005 @>
       
@q * (0) Copyright and License.@>

@q This file is part of GNU 3DLDF, a package for three-dimensional drawing. @>
@q Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, @>
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



@q * (0) parabola expressions.  @>
@** parabola expressions.
\initials{LDF 2005.11.07.}

\LOG
\initials{LDF 2005.11.07.}
Created this file.
\ENDLOG 

@q * (1) parabola_primary.  @>
@* \§parabola primary>.
\initials{LDF 2005.11.07.}  

\LOG
\initials{LDF 2005.11.07.}
Added this type declaration.
\ENDLOG 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> parabola_primary@>@/

@q ** (2) parabola_primary --> parabola_variable.@>
@*1 \§parabola primary> $\longrightarrow$ \§parabola variable>.  

\LOG
\initials{LDF 2005.11.07.}
Added this rule.
\ENDLOG 

@q *** (3) Definition.@> 

@<Define rules@>=
@=parabola_primary: parabola_variable@>@/
{
  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @;
  if (DEBUG)
    {
      cerr_strm << thread_name 
                << "*** Parser: `parabola_primary --> parabola_variable'.";

      log_message(cerr_strm);
      cerr_message(cerr_strm);
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */@;

  entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

  if (entry == static_cast<Id_Map_Entry_Node>(0) || entry->object == static_cast<void*>(0))
    {

      @=$$@> = static_cast<void*>(0);

    } /* |if (entry == 0 || entry->object == 0)|  */

  else /* |entry != 0 && entry->object != 0|  */

    @=$$@> = static_cast<void*>(create_new<Parabola>(
                                  static_cast<Parabola*>(
                                     entry->object))); 

};

@q ** (2) parabola_primary --> LEFT_PARENTHESIS parabola_expression @> 
@q ** (2) RIGHT_PARENTHESIS                                   @>

@*1 \§parabola primary> $\longrightarrow$ \.{\LP} 
\§parabola expression> \.{\RP}.

\LOG
\initials{LDF 2005.11.07.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=parabola_primary: LEFT_PARENTHESIS parabola_expression RIGHT_PARENTHESIS@>@/
{

   @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @;
  if (DEBUG)
    {
      cerr_strm << thread_name 
                << "*** Parser:  'parabola_primary --> "
                << "LEFT_PARENTHESIS parabola_expression RIGHT_PARENTHESIS'.";

      log_message(cerr_strm);
      cerr_message(cerr_strm);
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */@;

  @=$$@> = @=$2@>;

};


@q ***** (5) parabola_primary --> LAST @>
@q ***** (5) parabola_vector_expression.@>

@*4 \§parabola primary> $\longrightarrow$ 
\.{LAST} \§parabola vector expression>.
\initials{LDF 2005.11.07.}

\LOG
\initials{LDF 2005.11.07.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=parabola_primary: LAST parabola_vector_expression@>@/
{ 
   @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
   DEBUG = false; /* |true| */ @; 
   if (DEBUG) 
     {
         cerr_strm << thread_name << "*** Parser:  `parabola_primary "
                   << "--> LAST parabola_vector_expression'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str("");
     }
#endif /* |DEBUG_COMPILE|  */

@q ******* (7) @> 

   Parabola* c;

   try 
      {
         c = create_new<Parabola>(0);
      }

   catch (bad_alloc)
      {
         cerr_strm << thread_name << "ERROR!  In `yyparse()', rule "
                   << "`parabola_primary "
                   << "--> LAST parabola_vector_expression':"
                   << endl << "`create_new<Parabola>()' failed.  "
                   << "Rethrowing `bad_alloc'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm, error_stop_value);
         cerr_strm.str("");

         throw;

      }  /* |catch (bad_alloc)|  */

@q ******* (7) @> 

   Pointer_Vector<Parabola>* pv 
      = static_cast<Pointer_Vector<Parabola>*>(@=$2@>);

@q ******* (7) Error handling:  |pv == 0|.@> 

@ Error handling:  |pv == 0|.
\initials{LDF 2005.11.07.}

@<Define rules@>=

   if (pv == static_cast<Pointer_Vector<Parabola>*>(0))
      {
          cerr_strm << thread_name 
                    << "ERROR!  In `yyparse()', rule "
                    << endl 
                    << "`parabola_primary "
                    << "--> LAST parabola_vector_expression':"
                    << endl << "Invalid `parabola_vector_expression'.  "
                    << "Setting `parabola_primary' to 0"
                    << endl << "and will try to continue.";

          log_message(cerr_strm);
          cerr_message(cerr_strm, error_stop_value);
          cerr_strm.str("");

          delete c;

          @=$$@> = static_cast<void*>(0);

      }  /* |if (pv == 0)|  */

@q ******* (7) Error handling:  |pv->ctr == 0|.@> 

@ Error handling:  |pv->ctr == 0|.
\initials{LDF 2005.11.07.}

@<Define rules@>=

   else if (pv->ctr == 0)
      {
          cerr_strm << thread_name 
                    << "ERROR!  In `yyparse()', rule "
                    << endl 
                    << "`parabola_primary "
                    << "--> LAST parabola_vector_expression':"
                    << endl << "`parabola_vector_expression' is empty.  "
                    << "Setting `parabola_primary' to 0"
                    << endl << "and will try to continue.";

          log_message(cerr_strm);
          cerr_message(cerr_strm, error_stop_value);
          cerr_strm.str("");

          delete c;

          @=$$@> = static_cast<void*>(0);

      }  /* |else if (pv->ctr == 0)|  */

@q ******* (7) |pv != 0 && pv->ctr > 0|.@> 

@ |pv != 0 && pv->ctr > 0|.  Set |@=$$@>| to |*(pv->v[pv->ctr - 1])|.
\initials{LDF 2005.11.07.}

@<Define rules@>=

   else 
      {
         *c = *(pv->v[pv->ctr - 1]);
         @=$$@> = static_cast<void*>(c); 
      }
@q ******* (7) @> 

};

@q ** (2) parabola_primary --> GET_PARABOLA point_vector_primary@>
@*1 \§parabola primary> $\longrightarrow$ \.{GET\_PARABOLA}
\§point vector primary>.
\initials{LDF 2007.07.03.}

\LOG
\initials{LDF 2007.07.03.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=parabola_primary: GET_PARABOLA point_vector_primary numeric_primary@>@/ 
{

   @=$$@> = get_conic_section_func<Parabola>(
                                     static_cast<Pointer_Vector<Point>*>(@=$2@>), 
                                     static_cast<int>(@=$3@>),
                                     Shape::PARABOLA_TYPE,
                                     static_cast<Parabola*>(0),
                                     parameter);

};

@q ** (2) parabola_primary --> GET_PARABOLA conic_section_lattice_expression@>
@*1 \§parabola primary> $\longrightarrow$ \.{GET\_PARABOLA}
\§conic section lattice expression>.
\initials{LDF 2007.09.26.}

\LOG
\initials{LDF 2007.09.26.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=parabola_primary: GET_PARABOLA conic_section_lattice_expression@>@/ 
{

   Conic_Section_Lattice* c = static_cast<Conic_Section_Lattice*>(@=$2@>);

   @=$$@> = (c) ?  c->get_parabola(static_cast<Scanner_Node>(parameter))
                :  @=$$@> = static_cast<void*>(0);

};

@q * (1) parabola_secondary.  @>
@* \§parabola secondary>.
\initials{LDF 2005.11.07.}

\LOG
\initials{LDF 2005.11.07.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> parabola_secondary@>
  
@q ** (2) parabola secondary --> parabola_primary.@>
@*1 \§parabola secondary> $\longrightarrow$ \§parabola primary>.

\LOG
\initials{LDF 2005.11.07.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=parabola_secondary: parabola_primary@>@/
{
#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @;
  if (DEBUG)
    {
      cerr << "\n*** Parser: parabola_secondary --> parabola_primary "
           << endl;
    }
#endif /* |DEBUG_COMPILE|  */@;

  @=$$@> = @=$1@>;

};

@q ** (2) parabola secondary --> parabola_secondary transformer.  @>
@*1 \§parabola secondary> $\longrightarrow$ \§parabola secondary> 
\§transformer>.


\LOG
\initials{LDF 2005.11.07.}
Added this rule.

\initials{LDF 2006.01.20.}
@:BUG FIX@> BUG FIX:  Now deleting |Transform* t|.
\ENDLOG

@<Define rules@>=
@=parabola_secondary: parabola_secondary transformer@>@/
{

  Parabola* p = static_cast<Parabola*>(@=$1@>);
  Transform* t = static_cast<Transform*>(@=$2@>);

  *p *= *t;

  delete t;
 
  @=$$@> = static_cast<void*>(p); 

};



@q * (1) parabola tertiary.@>
@* \§parabola tertiary>.
\initials{LDF 2005.11.07.}

\LOG
\initials{LDF 2005.11.07.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> parabola_tertiary@>

@q ** (2) parabola tertiary --> parabola_secondary.  @>
@*1 \§parabola tertiary> $\longrightarrow$ \§parabola secondary>.
\initials{LDF 2005.11.07.}

\LOG
\initials{LDF 2005.11.07.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=parabola_tertiary: parabola_secondary@>@/
{

   @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @;
  if (DEBUG)
    {
        cerr_strm << thread_name 
                  << "*** Parser: `parabola_tertiary --> parabola_secondary'.";

        log_message(cerr_strm);
        cerr_message(cerr_strm);
        cerr_strm.str();

    }
#endif /* |DEBUG_COMPILE|  */@;

  @=$$@> = @=$1@>;

};



@q * (1) parabola expression.@>
@* \§parabola expression>.
\initials{LDF 2005.11.07.}

\LOG
\initials{LDF 2005.11.07.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> parabola_expression@>

@q ** (2) parabola expression --> parabola_tertiary.  @>
@*1 \§parabola expression> $\longrightarrow$ \§parabola tertiary>.

\LOG
\initials{LDF 2005.11.07.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=parabola_expression: parabola_tertiary@>@/
{

   @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @;
  if (DEBUG)
    {
        cerr_strm << thread_name 
                  << "*** Parser: `parabola_expression --> parabola_tertiary'.";

        log_message(cerr_strm);
        cerr_message(cerr_strm);
        cerr_strm.str();
    }
#endif /* |DEBUG_COMPILE|  */@;

  @=$$@> = @=$1@>;

};


@q * Emacs-Lisp code for use in indirect buffers when using the          @>
@q   GNU Emacs editor.  The local variable list is not evaluated when an @>
@q   indirect buffer is visited, so it's necessary to evaluate the       @>
@q   following s-expression in order to use the facilities normally      @>
@q   accessed via the local variables list.                              @>
@q   \initials{LDF 2004.02.12}.                                          @>
@q   (progn (cweb-mode) (outline-minor-mode t) (setq fill-column 70))    @>


@q Local Variables:                   @>
@q mode:CWEB                          @>
@q eval:(outline-minor-mode t)        @>
@q abbrev-file-name:"~/.abbrev_defs"  @>
@q eval:(read-abbrev-file)            @>
@q fill-column:70                     @>
@q run-cweave-on-file:"3DLDFprg.web"  @>
@q End:                               @>

