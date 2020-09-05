@q pstrexpr.w @> 
@q Created by Laurence Finston Wed May 19 15:22:54 MEST 2004  @>
     
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



@q * (0) string expressions.  @>
@** string expressions.
\initials{LDF 2004.05.19.}  

\LOG
\initials{LDF 2004.05.19.}  
Created this file.
\ENDLOG 

\TODO
@q { @>
@:TO DO}{{\bf TO DO}@>
@q } @> 
@q !! TO DO:  @>
\initials{LDF 2004.05.19.}
Check over all rules for strings.
\ENDTODO 

@q ** (2) string primary.@>
@*1 \§string primary>.
\initials{LDF Undated.}  

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> string_primary@>@/

@q *** (3) string_primary --> string_variable.@>
@*2 \§string primary> $\longrightarrow$ \§string variable>.  
\initials{LDF Undated.}

\LOG
\initials{LDF 2004.11.22.}
No longer issuing an error message if |entry->object == 0|.
This condition occurs legitimately when one tries to show an 
``unknown |string|''.
\ENDLOG 

@<Define rules@>=
@=string_primary: string_variable@>@/
{

  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
   DEBUG = false; /* |true| */ @; 
   if (DEBUG)
      {
         cerr_strm << thread_name 
                   << "*** Parser: `string_primary --> string_variable'.";


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

   else /* |entry != 0 && entry->object != 0|  */@;
      {
         string* s = new string;
  
         *s = *static_cast<string*>(entry->object); 
  
         @=$$@> = static_cast<void*>(s);

      }  /* |else| (|entry != 0 && entry->object != 0|)  */@;

};

@q *** (3) string_primary --> STRING.@>
@*2 \§string primary> $\longrightarrow$ \.{STRING}.
\initials{LDF Undated.}

\LOG
\initials{LDF 2005.11.03.}
Removed code from this rule.  Replaced it with a call to 
|Scan_Parse::string_primary_rule_func()|.
\ENDLOG 

@<Define rules@>=
@=string_primary: STRING@>@/
{

   if (@=$1@>)
      @=$$@> = Scan_Parse::string_primary_rule_func(@=$1@>, parameter);
   else
      @=$$@> = static_cast<void*>(0);

};


@q *** (3) string_primary --> LEFT_PARENTHESIS string_expression @>
@q *** (3) RIGHT_PARENTHESIS.                                    @>   

@*2 \§string primary> $\longrightarrow$ \.{\LP} 
\§string expression> \.{\RP}.
\initials{LDF Undated.}

@<Define rules@>=
@=string_primary: LEFT_PARENTHESIS string_expression RIGHT_PARENTHESIS@>@/
{
   @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
   DEBUG = false; /* |true| */ @; 
   if (DEBUG)
      {
          cerr_strm << thread_name 
                    << "*** Parser:  `string_primary --> "
                    << "LEFT_PARENTHESIS string_expression "
                    << "RIGHT_PARENTHESIS'.";

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str();
      }
#endif /* |DEBUG_COMPILE|  */@;

   @=$$@> = @=$2@>;

};

@q ***** (5) string_primary --> LAST string_vector_expression.@>

@*4 \§string primary> $\longrightarrow$ 
\.{LAST} \§string vector expression>.
\initials{LDF 2005.01.09.}

\LOG
\initials{LDF 2005.01.09.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=string_primary: LAST string_vector_expression@>@/
{ 
   @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
   DEBUG = false; /* |true| */ @; 
   if (DEBUG) 
     {
         cerr_strm << thread_name << "*** Parser:  `string_primary "
                   << "--> LAST string_vector_expression'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str("");
     }
#endif /* |DEBUG_COMPILE|  */

@q ******* (7) @> 

   string* s;

   try 
      {
         s = new string;
      }
   catch (bad_alloc)
      {
         cerr_strm << thread_name << "ERROR!  In `yyparse()', rule "
                   << "`string_primary "
                   << "--> LAST string_vector_expression':"
                   << endl << "`new string' failed.  "
                   << "Rethrowing `bad_alloc'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm, error_stop_value);
         cerr_strm.str("");

         throw;

      }  /* |catch (bad_alloc)|  */

@q ******* (7) @> 

   Pointer_Vector<string>* pv 
      = static_cast<Pointer_Vector<string>*>(@=$2@>);

@q ******* (7) Error handling:  |pv == 0|.@> 

@ Error handling:  |pv == 0|.
\initials{LDF 2005.01.09.}

@<Define rules@>=

   if (pv == static_cast<Pointer_Vector<string>*>(0))
      {
          cerr_strm << thread_name 
                    << "ERROR!  In `yyparse()', rule "
                    << endl 
                    << "`string_primary "
                    << "--> LAST string_vector_expression':"
                    << endl << "Invalid `string_vector_expression'.  "
                    << "Setting `string_primary' to 0"
                    << endl << "and will try to continue.";

          log_message(cerr_strm);
          cerr_message(cerr_strm, error_stop_value);
          cerr_strm.str("");

          delete s;

          @=$$@> = static_cast<void*>(0);

      }  /* |if (pv == 0)|  */

@q ******* (7) Error handling:  |pv->ctr == 0|.@> 

@ Error handling:  |pv->ctr == 0|.
\initials{LDF 2005.01.09.}

@<Define rules@>=

   else if (pv->ctr == 0)
      {
          cerr_strm << thread_name 
                    << "ERROR!  In `yyparse()', rule "
                    << endl 
                    << "`string_primary "
                    << "--> LAST string_vector_expression':"
                    << endl << "`string_vector_expression' is empty.  "
                    << "Setting `string_primary' to 0"
                    << endl << "and will try to continue.";

          log_message(cerr_strm);
          cerr_message(cerr_strm, error_stop_value);
          cerr_strm.str("");

          delete s;

          @=$$@> = static_cast<void*>(0);

      }  /* |else if (pv->ctr == 0)|  */

@q ******* (7) |pv != 0 && pv->ctr > 0|.@> 

@ |pv != 0 && pv->ctr > 0|.  Set |@=$$@>| to |*(pv->v[pv->ctr - 1])|.
\initials{LDF 2005.01.09.}

@<Define rules@>=

   else 
      {
         *s = *(pv->v[pv->ctr - 1]);
         @=$$@> = static_cast<void*>(s); 
      }
@q ******* (7) @> 

};


@q ***** (5) string_primary --> BINARY numeric_expression.@>

@*4 \§string primary> $\longrightarrow$ 
\.{BINARY} \§numeric expression>.
\initials{LDF 2005.09.09.}

\LOG
\initials{LDF 2005.09.09.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=string_primary: BINARY numeric_expression@>@/
{ 
   @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
   DEBUG = false; /* |true| */ @; 
   if (DEBUG) 
     {
         cerr_strm << thread_name << "*** Parser:  `string_primary "
                   << "--> BINARY numeric_expression'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str("");
     }
#endif /* |DEBUG_COMPILE|  */

   string* s = binary_number_string_func(@=$2@>, scanner_node);

   @=$$@> = static_cast<void*>(s); 

};



@q ***** (5) string_primary --> OCTAL numeric_expression.@>

@*4 \§string primary> $\longrightarrow$ 
\.{OCTAL} \§numeric expression>.
\initials{LDF 2005.09.08.}

\LOG
\initials{LDF 2005.09.08.}
Added this rule.

\initials{LDF 2005.09.09.}
Now using an |unsigned long| instead of an |int| to store the 
value of |numeric_expression|.
Added error handling.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=string_primary: OCTAL numeric_expression@>@/
{ 
   @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
   DEBUG = false; /* |true| */ @; 
   if (DEBUG) 
     {
         cerr_strm << thread_name << "*** Parser:  `string_primary "
                   << "--> OCTAL numeric_expression'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str("");
     }
#endif /* |DEBUG_COMPILE|  */

@q ******* (7) @> 

   string* s;
   s = new string;

   real r = floor(fabs(@=$2@>) + .5);

@q ******* (7) @>

   if (r > ULONG_MAX)
      {
           cerr_strm << thread_name 
                     << "WARNING!  In 'yyparse()', rule "
                     << "`string_primary --> OCTAL numeric_expression':"
                     << endl 
                     << "`numeric_expression' is too large, can't "
                     << "convert to an `unsigned long':  " 
                     << @=$2@> 
                     << endl 
                     << "Setting `string_primary' to \"INVALID INTEGER\" "
                     << "and continuing.";

           log_message(cerr_strm);
           cerr_message(cerr_strm, warning_stop_value);
           cerr_strm.str("");

           *s = "INVALID INTEGER";       

       } /* |if (r > ULONG_MAX)|  */


@q ******* (7) @>

   else  /* |r <= ULONG_MAX|  */
      {

         stringstream t;
   
         unsigned long u = static_cast<unsigned long>(r); 

         if (@=$2@> < 0) 
            t << "-";

         t << oct << u;

         *s = t.str();
   
#if DEBUG_COMPILE
         if (DEBUG)
           {
               cerr_strm << thread_name 
                         << "In 'yyparse()', rule `string_primary "
                         << "--> OCTAL numeric_expression':"
                         << endl << "`*s' == " << *s << "." << endl;

               log_message(cerr_strm);
               cerr_message(cerr_strm);
               cerr_strm.str("");
           }   
#endif /* |DEBUG_COMPILE|  */@; 

      }  /* |else| (|r <= ULONG_MAX|)  */


@q ******* (7) @> 

    
   @=$$@> = static_cast<void*>(s); 

};

@q ***** (5) string_primary --> DECIMAL numeric_expression.@>

@*4 \§string primary> $\longrightarrow$ 
\.{DECIMAL} \§numeric expression>.
\initials{LDF 2005.09.08.}

\LOG
\initials{LDF 2005.09.08.}
Added this rule.

\initials{LDF 2005.11.03.}
Added |precision_optional|.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=string_primary: DECIMAL numeric_expression precision_optional@>@/
{ 

@q ******* (7) @> 

   string* s;
   s = new string;

   stringstream t;
   
   real r = @=$2@>;

   t << setprecision(@=$3@>) << r;

   *s = t.str();
   
   @=$$@> = static_cast<void*>(s); 

};

@q ****** (6) precision_optional.@> 
@*5 \§precision optional>.
\initials{LDF 2005.11.03.}

\LOG
\initials{LDF 2005.11.03.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <int_value> precision_optional@>@/


@q ******* (7) precision_optional --> /* Empty  */@> 
@*6 \§precision optional> $\longrightarrow$  \.{EMPTY}.
\initials{LDF 2005.11.03.}

\LOG
\initials{LDF 2005.11.03.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=precision_optional: /* Empty  */@>@/
{
    
    @=$$@> = Scan_Parse::DEFAULT_PRECISION;
};

@q ******* (7) precision_optional --> WITH_PRECISION numeric_expression.@> 
@*6 \§precision optional> $\longrightarrow$  \.{WITH\_PRECISION} 
\§numeric expression>.
\initials{LDF 2005.11.03.}

\LOG
\initials{LDF 2005.11.03.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=precision_optional: WITH_PRECISION numeric_expression@>@/
{
    @=$$@> = static_cast<int>(floor(fabs(@=$2@> + .5)));
};



@q ***** (5) string_primary --> HEXADECIMAL numeric_expression.@>

@*4 \§string primary> $\longrightarrow$ 
\.{HEXADECIMAL} \§numeric expression>.
\initials{LDF 2005.09.08.}

\LOG
\initials{LDF 2005.09.08.}
Added this rule.

\initials{LDF 2005.09.09.}
Now using an |unsigned long| instead of an |int| to store the 
value of |numeric_expression|.
Added error handling.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=string_primary: HEXADECIMAL numeric_expression@>@/
{ 
   @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
   DEBUG = false; /* |true| */ @; 
   if (DEBUG) 
     {
         cerr_strm << thread_name << "*** Parser:  `string_primary "
                   << "--> HEXADECIMAL numeric_expression'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str("");
     }
#endif /* |DEBUG_COMPILE|  */

@q ******* (7) @> 

   string* s;
   s = new string;

   real r = floor(fabs(@=$2@>) + .5);

@q ******* (7) @>

   if (r > ULONG_MAX)
      {
           cerr_strm << thread_name 
                     << "WARNING!  In 'yyparse()', rule "
                     << "`string_primary --> HEXADECIMAL numeric_expression':"
                     << endl 
                     << "`numeric_expression' is too large, can't "
                     << "convert to an `unsigned long':  " 
                     << @=$2@> 
                     << endl 
                     << "Setting `string_primary' to \"INVALID INTEGER\" "
                     << "and continuing.";

           log_message(cerr_strm);
           cerr_message(cerr_strm, warning_stop_value);
           cerr_strm.str("");

           *s = "INVALID INTEGER";       

       } /* |if (r > ULONG_MAX)|  */


@q ******* (7) @>

   else  /* |r <= ULONG_MAX|  */
      {

         stringstream t;
   
         unsigned long u = static_cast<unsigned long>(r); 

         if (@=$2@> < 0) 
            t << "-";

         t << hex << u;

         *s = t.str();
   
#if DEBUG_COMPILE
         if (DEBUG)
           {
               cerr_strm << thread_name 
                         << "In 'yyparse()', rule `string_primary "
                         << "--> HEXADECIMAL numeric_expression':"
                         << endl << "`*s' == " << *s << "." << endl;

               log_message(cerr_strm);
               cerr_message(cerr_strm);
               cerr_strm.str("");
           }   
#endif /* |DEBUG_COMPILE|  */@; 

      }  /* |else| (|r <= ULONG_MAX|)  */


@q ******* (7) @> 

    
   @=$$@> = static_cast<void*>(s); 

};

@q ** (2) string secondary.  @>
@*1 \§string secondary>.
\initials{LDF Undated.}

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> string_secondary@>
  
@q *** (3) string secondary --> string_primary.@>
@*2 \§string secondary> $\longrightarrow$ \§string primary>.
\initials{LDF Undated.}

@<Define rules@>=
@=string_secondary: string_primary@>@/
{
   @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
   DEBUG = false; /* |true| */ @; 
   if (DEBUG)
     {
         cerr_strm << thread_name 
                   << "*** Parser:  `string_secondary --> string_primary'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str("");
     }
#endif /* |DEBUG_COMPILE|  */@;

  @=$$@> = @=$1@>;

};


@q ** (2) string tertiary.  @>
@*1 \§string tertiary>.
\initials{LDF Undated.}

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> string_tertiary@>

@q *** (3) string tertiary --> string_secondary.@>
@*2 \§string tertiary> $\longrightarrow$ \§string secondary>.
\initials{LDF Undated.}

@<Define rules@>=
@=string_tertiary: string_secondary@>@/
{
   @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
   DEBUG = false; /* |true| */ @; 
   if (DEBUG)
     {
         cerr_strm << thread_name 
                   << "*** Parser:  `string_tertiary --> string_secondary'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str("");
     }
#endif /* |DEBUG_COMPILE|  */@;

  @=$$@> = @=$1@>;

};


@q ** (3) string expression.  @>
@*2 \§string expression>.
\initials{LDF Undated.}

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> string_expression@>

@q *** (3) string expression --> string_tertiary.  @>
@*2 \§string expression> $\longrightarrow$ \§string tertiary>.
\initials{LDF Undated.}

@<Define rules@>=
@=string_expression: string_tertiary@>@/
{
   @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
   DEBUG = false; /* |true| */ @; 
   if (DEBUG)
     {
         cerr_strm << thread_name 
                   << "*** Parser:  `string_expression --> string_tertiary'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str("");
     }
#endif /* |DEBUG_COMPILE|  */@;

  @=$$@> = @=$1@>;

};

@q *** (3) string expression --> string_expression AMPERSAND @>
@q *** (3) string_tertiary.                                  @> 

@*2 \§string expression> $\longrightarrow$ \§string expression> 
\.{AMPERSAND} \§string tertiary>.
\initials{LDF 2004.05.21.}  

\LOG
\initials{LDF 2004.05.21.}  
Added this rule.
\ENDLOG 

@q **** (4) Definition.@>   

@<Define rules@>=
@=string_expression: string_expression AMPERSAND string_tertiary@>@/
{

  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
   DEBUG = false; /* |true| */ @; 
   if (DEBUG)
      {
          cerr_strm << thread_name 
                    << "*** Parser:  `string_expression --> "
                    << "string_expression "
                    << "AMPERSAND string_tertiary.";

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str("");    
      }
#endif /* |DEBUG_COMPILE|  */@;

   string* s = static_cast<string*>(@=$1@>); 
   string* t = static_cast<string*>(@=$3@>); 

@q ***** (5) Error handling:  |(s == 0 || t == 0)|.@>

@ Error handling:  |(s == 0 || t == 0)|.
\initials{LDF Undated.}

@<Define rules@>=
   if (s == static_cast<string*>(0) || t == static_cast<string*>(0))
      {

          cerr_strm << thread_name << "ERROR!  In `yyparse()', rule "
                    << "`string_expression --> string_expression "
                    << "AMPERSAND string_tertiary':"
                    << endl 
                    << "Invalid `string_expression' or `string_tertiary' "
                    << "on the right-hand side."
                    << "Setting `string_expression' on the left-hand side "
                    << "to the empty string "
                    << "and will try to continue.";

         log_message(cerr_strm);
         cerr_message(cerr_strm, error_stop_value);
         cerr_strm.str("");

         s = new string;
         *s = "";
      

    }  /* |if (s == 0 || t == 0)|  */

@q ***** (5) |s != 0 && t != 0|.@>

@ |s != 0 && t != 0|.
\initials{LDF Undated.}

@<Define rules@>=

  else  /* |s != 0 && t != 0|  */
    {
      *s += *t;

      delete t;

      @=$$@> = static_cast<void*>(s); 

    } /* |else| (Success!)  */@;

@q ***** (5).@> 

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

