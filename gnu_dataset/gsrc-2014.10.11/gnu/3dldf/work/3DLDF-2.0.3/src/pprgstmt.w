@q pprgstmt.w @>
@q Created by Laurence Finston Tue Sep  7 12:24:12 CEST 2004 @>
     
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



@q ** (2) Common declarations for rules.@>
@*1 Common declarations for rules.
It would be nice to declare the variables in this section as local variables
within |yyparse()|, but I don't know of any way to do that.
\initials{LDF 2004.04.16.}


I also wanted |out_stream_metapost| to be a plain |ostream| rather than an
|ostream*|, but I couldn't get this to work.  I believe it will only be needed
for debugging purposes, because real output should be performed by
|beginfig()|, |endfig()|, |Picture::output()|, and other functions.
\initials{LDF 2004.04.16.}

@q *** (3) LOG.@>

\LOG
\initials{LDF 2004.04.16.}  
Added this section.

\initials{LDF 2004.04.22.}
Added |int status| and |using namespace Scan_Parse|.

\initials{LDF 2004.06.30.}
Added |string rule_name_string|.

\initials{LDF 2004.07.03.}
Added |Thread_Info_Type* thread_info|.

\initials{LDF 2004.07.05.}
Added |stringstream cerr_strm|.

\initials{LDF 2004.07.19.}
@:??@> ?? For some reason, |Thread_Info_Type::get_thread_info()| doesn't seem to
work here.  I've now put a pointer to |Thread_Info_Type| onto |Scanner_Type|
instead.  I think this is better anyway, but I would like to know why the other
way didn't work.  It seems as if |yyparse()| is not in the scope of the
thread-specific data, but this shouldn't be the case.

\initials{LDF 2004.07.21.}
Now declaring and initializing |string thread_name| for use in error and 
help messages and debugging output. 

\initials{LDF 2004.07.21.}
Now declaring and initializing |bool stop_value|.

\initials{LDF 2004.09.09.}
@:BUG FIX@> BUG FIX: 
Now testing |HAVE_PTHREAD_H|, |scanner_node->run_state.multithread_input|, 
and |scanner_node->run_state.multithread_output| when setting |thread_name|.

\initials{LDF 2004.09.12.}
Added |stringstream location_strm|.

\initials{LDF 2004.09.13.}
@:KLUDGE@> KLUDGE: 
Now setting |stop_value| to |false| if 
|scanner_node->in->type == Io_Struct::STDIN_TYPE|.  The problem is that the 
``\<RETURNS>'' typed by the user are read by |sub_yylex()|, which can cause
old input characters that haven't been processed yet to be overwritten.

\initials{LDF 2004.09.13.}
If the user is typing input in line-by-line, then we don't really need to stop.
However, I'd prefer to find a better solution.  Perhaps the input characters
should be read into a buffer.  

\initials{LDF 2004.09.22.}
Removed |string rule_name_string|.

\initials{LDF 2004.10.09.}
Added |bool warning_stop_value|.
\ENDLOG 

@q **** (4) End of LOG.@>

@q *** (3) Code.@> 

@<Common declarations for rules@>=

   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

   ostream* out_stream_metapost
      = scanner_node->out[Run_State::METAPOST]->stream_ptr;


   bool error_stop_value = (      scanner_node->run_state.error_stop_mode
                         == Run_State::STOPPING
                      && scanner_node->in->type != Io_Struct::STDIN_TYPE)
                      
                      ? true : false;

   bool warning_stop_value = (      scanner_node->run_state.warning_stop_mode
                         == Run_State::STOPPING
                      && scanner_node->in->type != Io_Struct::STDIN_TYPE)
                      
                      ? true : false;



   Id_Map_Entry_Node entry;

   int status;

   using namespace Scan_Parse;

   stringstream cerr_strm;

   stringstream location_strm;

@q *** (3).@> 
@
\LOG
@:BUG FIX@> BUG FIX:  Now checking 
that |scanner_node != 0 && scanner_node->thread_info != static_cast<Thread_Info_Type*>(0)| before 
setting |thread_name = scanner_node->thread_info->name|.
\ENDLOG 

@<Common declarations for rules@>=

   string thread_name;

#ifdef HAVE_PTHREAD_H  
   if (   scanner_node != static_cast<Scanner_Node>(0) 
       && scanner_node->thread_info != static_cast<Thread_Info_Type*>(0)
       && (   scanner_node->run_state.multithread_input 
           || scanner_node->run_state.multithread_output))      

      thread_name = scanner_node->thread_info->name;

#else /* |HAVE_PTHREAD_H| is undefined.  */@; 

   if (0)  /* Dummy conditional.  */
      ;

#endif /* |HAVE_PTHREAD_H| is undefined.  */@; 

   else  /* Threads not available or not using them.  */

      thread_name = "";

@q *** (3).@> 
@
@<Common declarations for rules@>=

#if DEBUG_COMPILE
   bool DEBUG = false;
#endif /* |DEBUG_COMPILE|  */@; 


@q ** (2) program.  Start symbol.  @>
@q ** (2) program: statement_list END  @>
@*1 \§program> \§statement list> \.{END}.
\initials{LDF Undated.}

\syntax{program: statement_list END}

\LOG
\initials{LDF 2004.07.09.}
Added |Thread_Info_Type* t| and |pthread_t tid| for debugging.

\initials{LDF 2004.07.16.}
No longer closing the input and output streams.  
This wasn't thread-safe.  They are now closed in the destructors.

\initials{LDF 2004.07.21.}
Now using |Scan_Parse::log_message()|.

\initials{LDF 2004.09.13.}
Added code for handling unmatched |begingroup| commands in the input.  The
excess |Id_Map_Nodes| are deleted.
\ENDLOG 

@f program non_terminal_symbol

@q *** (3) Definition.  @>

@<Define rules@>=

@=program: statement_list END@>
{

 @<Common declarations for rules@>@;

#if DEBUG_COMPILE
 DEBUG = false; /* |true| */ @; 
 if (DEBUG)
   {
     cerr_strm << thread_name << "*** Parser: "
               << "`program --> statement_list END'.";
      
     log_message(cerr_strm);
     cerr_message(cerr_strm); 
     cerr_strm.str("");
   }
#endif /* |DEBUG_COMPILE|  */

@q **** (4) Delete all of the |Input_Struct| pointers on |scanner_node| @>
@q **** (4) if we're using single-threaded input and inclusion.@>

@ Delete all of the |Input_Struct| pointers on |scanner_node|
if we're using single-threaded input and inclusion.
\initials{LDF 2004.09.30.}

\LOG
\initials{LDF 2004.09.30.}
Added this section.
\ENDLOG

@<Define rules@>=


  if (   scanner_node->in->up != static_cast<Input_Struct*>(0)
      && !(   scanner_node->run_state.multithread_input
           || scanner_node->run_state.multithread_include))
      
    {
      Input_Struct* up = scanner_node->in->up;

      while (up != static_cast<Input_Struct*>(0))
        {
          delete scanner_node->in;
          scanner_node->in = up;
          up = scanner_node->in->up;
        }




      delete scanner_node->in;
      scanner_node->in = 0;
    }

@q **** (4) Remove excess |Id_Map_Nodes|.  @>
@ Remove excess |Id_Map_Nodes|.  They will only exist if there were 
unmatched |begingroup| commands in the input.
\initials{LDF 2004.09.13.}

\LOG
\initials{LDF 2004.09.13.}
Added this section.
\ENDLOG 

@<Define rules@>=

  if (scanner_node->group_ctr > 0)
    {

      cerr_strm << thread_name << "WARNING! In `yyparse()',"
                << endl << " rule `program --> statement_list END':"
                << endl 
                << scanner_node->group_ctr 
                << " unmatched `begingroup' commands."
                << endl << "Deleting excess `Id_Map_Nodes' and "
                << "continuing.";
      
      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value); 
      cerr_strm.str("");
      
    } /* |if (scanner_node->group_ctr > 0)|  */


 Id_Map_Node id_map_node = 0; 

 while (scanner_node->group_ctr > 0)
   {
     id_map_node = scanner_node->id_map_node;
     scanner_node->id_map_node = id_map_node->up;
     id_map_node->up = 0;
     delete id_map_node;
     id_map_node = 0;
     scanner_node->group_ctr--;

   } /* |while (scanner_node->group_ctr > 0)|  */

@q **** (4) Exit |yyparse()| with return value 0.@>

@ Exit |yyparse()| with return value 0.
\initials{LDF 2004.09.13.}

@<Define rules@>=

#if DEBUG_COMPILE
   if (DEBUG)
     {
       cerr_strm << thread_name << "In `yyparse()',"
                << endl << " rule `program --> statement_list END':"
                << endl << "`yyparse()' returning 0.";

       log_message(cerr_strm);
       cerr_message(cerr_strm);
       cerr_strm.str("");
     }
#endif /* |DEBUG_COMPILE|  */@; 

@q #define DEBUG_COMPILE 0 @>

  return 0;

};

@q ** (2) Statements.   @>
@*1 Statements.

@q *** (3) statement_list.   @>
@ \§statement\_list>.  


@f statement_list non_terminal_symbol

@q **** (4) statement_list --> /* Empty  */.@>   
@*3 \§statement\_list> $\longrightarrow$ \.{EMPTY}. 
\initials{LDF 2004.10.25.}

\syntax{statement_list: /* EMPTY */}

@<Define rules@>=
@=statement_list: /* EMPTY */@>@/
;

@q **** (4) statement_list --> statement_list statement.@>   
@*3 \§statement\_list> $\longrightarrow$ \§statement\_list>
\§statement>.
\initials{LDF 2004.10.25.}

@<Define rules@>=
@=statement_list: statement_list  statement@>
{



#if DEBUG_COMPILE 
  @<Common declarations for rules@>@;
#endif

#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @;   
  if (DEBUG)
    {
      cerr_strm << thread_name 
                << "*** Parser: statement_list --> statement_list "
                << "statement.";

      log_message(cerr_strm); 
      cerr_message(cerr_strm); 
      cerr_strm.str(""); 
      
    }
#endif /* |DEBUG_COMPILE|  */

};

@q *** (3) statement.@>
@*2 \§statement>.  

|SEMI_COLON| appears at the end of each of the token lists in all of the rules
for |statement|.  This makes it possible to use {\it left recursion\/}%
%
\fnote{``Any kind of sequence can be defined using either left recursion or
right recursion, but you should always use left recursion, because it
can parse a sequence of any number of elements with bounded stack
space.''
Donnelly, Charles, and Richard Stallman. 
{\it Bison.  The YACC-compatible Parser Generator}, p.~28.}
%
in the
non-empty rule for |statement_list|.
This differs from Metafont's grammar, where the semi-colon is part of the 
$\langle {\rm statement\ list}\rangle$ rule, which uses right recursion.
%
\fnote{Knuth, {\it The METAFONTbook}, p.~217.}
%
$$\langle {\rm statement\ list}\rangle \longrightarrow \langle{\rm empty}
\rangle\ \vert\ \langle{\rm statement}\rangle {\ \tt ;\ }
\langle {\rm statement\ list} \rangle$$

@f statement non_terminal_symbol
@<Define rules@>= 

@q **** (4) statement --> SEMI_COLON.  @>
@ \§statement> $\longrightarrow$ \.{SEMI\_COLON}.
This rule makes extra semi-colons valid input.
\initials{LDF 2004.04.23.}

\LOG
\initials{LDF 2004.04.23.}  
Added this rule.   
\ENDLOG 

@<Define rules@>=
@=statement: SEMI_COLON@>
{

#if DEBUG_COMPILE 
  @<Common declarations for rules@>@;
#endif

#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
  if (DEBUG) 
    {
      cerr_strm << thread_name 
                << "*** Parser: statement --> SEMI_COLON.";

      log_message(cerr_strm); 
      cerr_message(cerr_strm); 
      cerr_strm.str(""); 
    }
#endif /* |DEBUG_COMPILE|  */

};


@q **** (4) statement --> END_INPUT.@>
@*3  \§statement> $\longrightarrow$ \.{END\_INPUT}.

\LOG
\initials{LDF 2004.10.01.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@
@<Define rules@>=
  
@=statement: END_INPUT@>@/

{

  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
      cerr_strm << thread_name 
                << "*** Parser: `statement --> END_INPUT'.";

      log_message(cerr_strm); 
      cerr_message(cerr_strm); 
      cerr_strm.str(""); 
    }
#endif /* |DEBUG_COMPILE|  */@;


  Int_Void_Ptr_Bool end_ivp(END, 0, false);


@q ****** (6) Using multithreaded input and/or multithreaded inclusion.@>   

@ Using multithreaded input and/or multithreaded inclusion.
\initials{LDF 2004.10.01.}

@<Define rules@>=

  if (   scanner_node->run_state.multithread_input 
      || scanner_node->run_state.multithread_include)

    {

#if DEBUG_COMPILE
      if (DEBUG)
        {
          cerr_strm << thread_name 
                    << "WARNING! In `yyparse()', rule `statement --> "
                    << "END_INPUT':"
                    << endl 
                    << "`end_input' shouldn't be used when "
                    << "multithreaded input and/or inclusion"
                    << endl
                    << "is enabled. Will treat `end_input' as "
                    << "though it were `end' and continue.";
                
          log_message(cerr_strm); 
          cerr_message(cerr_strm, error_stop_value); 
          cerr_strm.str("");           
        }
#endif /* |DEBUG_COMPILE|  */@; 
 
      scanner_node->rescan_stack.push(end_ivp);

    } /* |if (   scanner_node->run_state.multithread_input 
              || scanner_node->run_state.multithread_include)|  */
    

@q ****** (6) Using single-threaded input and inclusion.@>   

@ Using single-threaded input and inclusion.
\initials{LDF 2004.10.01.}

@<Define rules@>=

  
  else /* |!(   scanner_node->run_state.multithread_input 
             || scanner_node->run_state.multithread_include)|  */
    {

      
@q ******* (7) |scanner_node->in->up != 0|.       @> 
@q ******* (7) Will call |scanner_node->pop_in()|.@> 

@ |scanner_node->in->up != 0|.  Will call |scanner_node->pop_in()|. 
\initials{LDF 2004.10.01.}

@<Define rules@>=
   
  if (scanner_node->in->up)
    {

#if DEBUG_COMPILE
      if (DEBUG)
        {
          cerr_strm << thread_name 
                    << "In `yyparse()', rule `statement --> "
                    << "END_INPUT':"
                    << endl 
                    << "Using single-threaded input and inclusion "
                    << "and `scanner_node->in->up' != 0."
                    << endl 
                    << "Calling `scanner_node->pop_in()'.";
          
          log_message(cerr_strm); 
          cerr_message(cerr_strm); 
          cerr_strm.str("");           
        }
#endif /* |DEBUG_COMPILE|  */@; 


      status = scanner_node->pop_in();


@q ******** (8) Error handling:  |scanner_node->pop_in()| failed.@> 

@ Error handling:  |scanner_node->pop_in()| failed.
\initials{LDF 2004.10.01.}

@<Define rules@>=

  if (status != 0)
    {

      cerr_strm << thread_name 
                << "ERROR! In `yyparse()', rule `statement --> "
                << "END_INPUT':"
                << endl 
                << "`scanner_node->pop_in()' failed. "
                << "Will treat `end_input' as though it were `end' "
                << "and try to continue.";

      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");
      
      scanner_node->rescan_stack.push(end_ivp);

    }  /* |if (status != 0)|  */


@q ******** (8) |scanner_node->pop_in()| succeeded.@> 
@ |scanner_node->pop_in()| succeeded.
\initials{LDF 2004.10.01.}

@<Define rules@>=

   else /* |status == 0|  */
      {
#if DEBUG_COMPILE
          if (DEBUG) 
             {

                 cerr_strm << thread_name 
                           << "In `yyparse()', rule `statement --> "
                           << "END_INPUT':"
                           << endl 
                           << "`scanner_node->pop_in()' succeeded.";


                 log_message(cerr_strm);
                 cerr_message(cerr_strm);
                 cerr_strm.str("");
      
             }      
#endif /* |DEBUG_COMPILE|  */@; 

    scanner_node->set_first_call_yylex(true);

  } /* |else| (|status == 0|)  */

@q ******** (8).@> 


    } /* |if (scanner_node->in->up)|  */
  
  

@q ******* (7) |scanner_node->in->up == 0|.       @> 
@q ******* (7) Will push |END| onto |scanner_node->rescan_stack|.@> 

@ |scanner_node->in->up == 0|. 
Will push |END| onto |scanner_node->rescan_stack|. 
\initials{LDF 2004.10.01.}

@<Define rules@>=

  else  /* |scanner_node->in->up == 0|  */
    {

#if DEBUG_COMPILE
      if (DEBUG)
        {
          cerr_strm << thread_name 
                    << "In `yyparse()', rule `statement --> "
                    << "END_INPUT':"
                    << endl 
                    << "`scanner_node->in->up' == 0. "
                    << "Will treat `end_input' as though it were `end'.";

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str("");


        }
#endif /* |DEBUG_COMPILE|  */@; 
 
      
      scanner_node->rescan_stack.push(end_ivp);

    }  /* |else| (|scanner_node->in->up == 0|)  */



@q ******* (7) End of single-threaded input and inclusion case.@> 

@ End of single-threaded input and inclusion case.
\initials{LDF 2004.10.01.}

@<Define rules@>=

    }    /* |else| (|!(   scanner_node->run_state.multithread_input 
                       || scanner_node->run_state.multithread_include)|)  */

@q ****** (6) Exit rule.@>   

@ Exit rule.
\initials{LDF 2004.10.01.}

@<Define rules@>=

};

@q **** (4) statement --> equation SEMI_COLON.  @>

@ \§statement> $\longrightarrow$ \§equation>  \.{SEMI\_COLON}.
\initials{LDF Undated.}

\LOG
\initials{LDF 2005.12.09.}
Now including this rule in |@<Garbage@>|.
\ENDLOG


@<Garbage@>=
@=statement:  equation SEMI_COLON@>
;

@q **** (4) statement --> declaration SEMI_COLON.@>
@*3 \§statement> $\longrightarrow$ \§declaration> \.{SEMI\_COLON}.
\initials{LDF 2004.10.01.}

\LOG
\initials{LDF 2004.10.01.}
Made debugging output thread-safe.
\ENDLOG 

@<Define rules@>=

@=statement:  declaration SEMI_COLON@>
{

#if DEBUG_COMPILE 
  @<Common declarations for rules@>@;
#endif

#if DEBUG_COMPILE
  DEBUG = false; /* |true|  */
  if (DEBUG)
    {
      cerr_strm << thread_name 
                << "*** Parser: `statement --> declaration SEMI_COLON'.";

      log_message(cerr_strm);
      cerr_message(cerr_strm);
      cerr_strm.str("");

    }
#endif /* |DEBUG_COMPILE|  */

};

@q **** (4) statement --> assignment SEMI_COLON.  @>
@*3 \§statement> $\longrightarrow$ \§assignment> \.{SEMI\_COLON}.
\initials{LDF 2004.10.01.}

\LOG
\initials{LDF 2004.10.01.}
Made debugging output thread-safe.
\ENDLOG 


@<Define rules@>=


@=statement:  assignment SEMI_COLON@>
{

#if DEBUG_COMPILE 
  @<Common declarations for rules@>@;
#endif

#if DEBUG_COMPILE
  DEBUG = false; /* |true|  */
  if (DEBUG)
    {
      cerr_strm << thread_name 
                << "*** Parser: `statement --> assignment SEMI_COLON'.";
      
      log_message(cerr_strm);
      cerr_message(cerr_strm);
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */

};

@q **** (4) statement --> operation_assignment SEMI_COLON.@>
@*3 \§statement> $\longrightarrow$ \§operation assignment> 
\.{SEMI\_COLON}.
\initials{LDF 2004.11.05.}

\LOG
\initials{LDF 2004.11.05.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=statement: operation_assignment SEMI_COLON@>
{

#if DEBUG_COMPILE 
  @<Common declarations for rules@>@;
#endif

#if DEBUG_COMPILE
  DEBUG = false; /* |true|  */
  if (DEBUG)
    {
      cerr_strm << thread_name 
                << "*** Parser: `statement --> "
                << "operation_assignment SEMI_COLON'.";
      
      log_message(cerr_strm);
      cerr_message(cerr_strm);
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */

};

@q **** (4) statement --> macro_definition.@>
@*3 \§statement> $\longrightarrow$ \§macro definition>.
\initials{LDF 2004.12.29.}

\LOG
\initials{LDF 2004.12.29.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=statement: macro_definition@>
{

#if DEBUG_COMPILE 
  @<Common declarations for rules@>@;
#endif

#if DEBUG_COMPILE
  DEBUG = false; /* |true|  */
  if (DEBUG)
    {
      cerr_strm << thread_name 
                << "*** Parser: `statement --> "
                << "macro_definition'.";
      
      log_message(cerr_strm);
      cerr_message(cerr_strm);
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */

};



@q **** (4) statement --> command SEMI_COLON.  @>
@ \§statement> $\longrightarrow$ \§command>  \.{SEMI\_COLON}.


\LOG
\initials{LDF 2004.06.02.}  
Added this rule.

\initials{LDF 2004.10.01.}
Made debugging output thread-safe.
\ENDLOG 

@<Define rules@>=

@=statement: command SEMI_COLON@>
{

#if DEBUG_COMPILE 
  @<Common declarations for rules@>@;
#endif

#if DEBUG_COMPILE
  DEBUG = false; /* |true|  */
  if (DEBUG) 
    {
      cerr_strm << thread_name 
                << "*** Parser: `statement --> command SEMI_COLON'.";
      
      log_message(cerr_strm);
      cerr_message(cerr_strm);
      cerr_strm.str("");

    }
#endif /* |DEBUG_COMPILE|  */

};

@q **** (4) statement --> conditional@>
@ \§statement> $\longrightarrow$ \§conditional>.

\LOG
\initials{LDF 2004.09.10.}  
Added this rule.
\ENDLOG 

@<Define rules@>=

@=statement: conditional@>@/
{

#if DEBUG_COMPILE 
  @<Common declarations for rules@>@;
#endif

#if DEBUG_COMPILE
  DEBUG = false; /* |true|  */
  if (DEBUG) 
    {
      cerr_strm << thread_name 
                << "*** Parser: `statement --> conditional'.";
      
      log_message(cerr_strm);
      cerr_message(cerr_strm);
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */

};

@q **** (4) statement --> loop@>
@ \§statement> $\longrightarrow$ \§loop>.

\LOG
\initials{LDF 2004.09.12.}  
Added this rule.
\ENDLOG 

@<Define rules@>=

@=statement: loop@>@/
{

#if DEBUG_COMPILE 
  @<Common declarations for rules@>@;
#endif

#if DEBUG_COMPILE
  DEBUG = false; /* |true|  */
  if (DEBUG) 
    {
      cerr_strm << thread_name 
                << "*** Parser: `statement --> loop'.";
      
      log_message(cerr_strm);
      cerr_message(cerr_strm);
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */

};

@q **** (4) statement --> macro_call@>
@ \§statement> $\longrightarrow$ \§macro call>.

\LOG
\initials{LDF 2004.12.22.}
Added this rule.

\initials{LDF 2004.12.29.}
Changed |defun_call| to |macro_call|.
\ENDLOG 

@<Define rules@>=

@=statement: macro_call@>@/
{

#if DEBUG_COMPILE 
  @<Common declarations for rules@>@;
#endif

#if DEBUG_COMPILE
  DEBUG = false; /* |true|  */
  if (DEBUG) 
    {
      cerr_strm << thread_name 
                << "*** Parser: `statement --> macro_call'.";
      
      log_message(cerr_strm);
      cerr_message(cerr_strm);
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */

};

@q **** (4) statement --> let_statement SEMI_COLON.@>

@*3 \§statement> $\longrightarrow$ \§let statement>  \.{SEMI\_COLON}.
\initials{LDF 2005.01.25.}

\LOG
\initials{LDF 2005.01.25.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=statement:  let_statement SEMI_COLON@>
{

#if DEBUG_COMPILE 
  @<Common declarations for rules@>@;
#endif

#if DEBUG_COMPILE
  DEBUG = false; /* |true|  */
  if (DEBUG)
     {
         cerr_strm << thread_name 
                   << "*** Parser:  `statement --> let_statement SEMI_COLON'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str("");
     }
#endif /* |DEBUG_COMPILE|  */

};

@q ** (2) let_statement --> LET symbolic_token equate_or_assign.@> 

@*1 \§let statement> $\longrightarrow$ \.{LET} \§equate or assign>. 
\initials{LDF 2005.01.25.}

\LOG
\initials{LDF 2005.01.25.}
Added this rule.

\initials{LDF 2005.01.26.}
Changed the second \§symbolic token> to \§string expression>.

\initials{LDF 2005.01.26.}
Changing the way this rule works.  Previously, it simply 
added a synonym.  I want it to copy the |Id_Map_Entry_Node| 
referenced by the \§string expression> to one referenced by the
\§symbolic token>.  If the \§symbolic token> already references 
an |Id_Map_Entry_Node|, the latter must first be destroyed.

\initials{LDF 2005.01.26.}
Now calling |Scan_Parse::let_func()|.

\initials{LDF 2005.01.26.}
Removed \§string expression> from the rule.  The |string| is now collected
in |Scan_Parse::let_func()|.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>= 
@=let_statement: LET symbolic_token equate_or_assign@>
{

   @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
   DEBUG = false; /* |true|  */
   if (DEBUG) 
      {
          cerr_strm << thread_name 
                    << "*** Parser:  `let_statement --> "
                    << "symbolic_token equate_or_assign'.";

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */

   string s = @=$2@>;

   try
      {
         status = let_func(scanner_node, s);
      }   

@q ***** (5) Error handling:  |Scan_Parse::let_func()| failed, @>   
@q ***** (5) throwing |bad_alloc|.                             @>   
@ Error handling:  |Scan_Parse::let_func()| failed, 
throwing |bad_alloc|.                                
\initials{LDF 2005.01.26.}

\LOG
\initials{LDF 2005.01.26.}
Added this section.
\ENDLOG

@<Define rules@>=

           catch (bad_alloc)
              {
                  cerr_strm << thread_name << "In `yyparse()', rule "
                            << "`let_statement --> "
                            << "symbolic_token equate_or_assign':"
                            << endl << "`Scan_Parse::let_func()' failed, throwing "
                            << "`bad_alloc'.  Rethrowing.";

                  log_message(cerr_strm);
                  cerr_message(cerr_strm, error_stop_value);
                  cerr_strm.str("");

                  throw;

              }  /* |catch (bad_alloc)|  */



@q ***** (5) Error handling:  |Scan_Parse::let_func()| failed, @>   
@q ***** (5) returning a non-zero value                        @>   

@ Error handling:  |Scan_Parse::let_func()| failed, 
returning a non-zero value.
\initials{LDF 2005.01.26.}

\LOG
\initials{LDF 2005.01.26.}
Added this section.
\ENDLOG

@<Define rules@>=

             if (status != 0)
                {
                    cerr_strm << thread_name << "In `yyparse()', rule "
                              << "`let_statement --> "
                              << "symbolic_token equate_or_assign':"
                              << endl << "`Scan_Parse::let_func()' failed, returning "
                              << status << "."  << endl << "Will try to continue.";

                    log_message(cerr_strm);
                    cerr_message(cerr_strm, error_stop_value);
                    cerr_strm.str("");

                }  /* |if (status != 0)|  */
@q ***** (5).@> 

#if DEBUG_COMPILE

            else if (DEBUG) /* |status == 0|  */
               {
                   cerr_strm << thread_name << "In `yyparse()', rule "
                             << "`let_statement --> "
                             << "symbolic_token equate_or_assign':"
                             << endl << "`Scan_Parse::let_func()' succeeded.";

                   log_message(cerr_strm);
                   cerr_message(cerr_strm, error_stop_value);
                   cerr_strm.str(""); 

               }  /* |else if (DEBUG) | (|status == 0|)  */

#endif /* |DEBUG_COMPILE|  */@; 
 
 
@q **** (4) @>   

};

  
@q * Emacs-Lisp code for use in indirect buffers when using the          @>
@q   GNU Emacs editor.  The local variable list is not evaluated when an @>
@q   indirect buffer is visited, so it's necessary to evaluate the       @>
@q   following s-expression in order to use the facilities normally      @>
@q   accessed via the local variables list.                              @>
@q   \initials{LDF 2004.02.12}.                                          @>
@q   (progn (cweb-mode) (outline-minor-mode t) (setq fill-column 80))    @>



@q Local Variables: @>
@q mode:CWEB  @>
@q eval:(outline-minor-mode t)  @>
@q abbrev-file-name:"~/.abbrev_defs" @>
@q eval:(read-abbrev-file)  @>
@q fill-column:80 @>
@q run-cweave-on-file:"3DLDFprg.web"  @>
@q End: @>
