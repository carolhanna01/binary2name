@q pmacros.w @> 
@q Created by Laurence Finston Tue Thu Dec 16 18:25:42 CET 2004 @>
     
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



@q * (0) Macros.@>
@** Macros.
\initials{LDF 2004.12.16.}

\LOG
\initials{LDF 2004.12.16.}
Created this file.
\ENDLOG 

@q * (1) macro_definition.@>
@* \§macro definition>. 

\LOG
\initials{LDF 2004.12.29.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> macro_definition@>


@q ** (2) macro_definition --> DEF variable UNDECLARED untyped_parameter_list @>
@q ** (2) typed_parameter_list  ASSIGN.                                       @> 

@*1 \§macro definition> $\longrightarrow$ \.{DEF} \§symbolic token>
\§untyped parameter list> \§typed parameter list> 
\§equate or assign> \§macro variable>.  
\initials{LDF 2004.12.30.}

\LOG
\initials{LDF 2004.12.30.}
Added this rule.
\ENDLOG 

@q ***** (5) Definition.@> 

@<Define rules@>= 
 
@=macro_definition: DEF variable UNDECLARED untyped_parameter_list@>@/
@=typed_parameter_list equate_or_assign@>@/
{

   @<Common declarations for rules@>@; 

 #if DEBUG_COMPILE
    DEBUG = false;  /* |true|  */
    if (DEBUG)
       {

           cerr_strm << thread_name << "*** Parser: `macro_definition --> "
                     << endl 
                     << "DEF variable UNDECLARED untyped_parameter_list "
                     << endl 
                     << "typed_parameter_list equate_or_assign'.";

           log_message(cerr_strm);
           cerr_message(cerr_strm);
           cerr_strm.str("");
       }
#endif /* |DEBUG_COMPILE|  */@; 

@q ****** (6).@> 

   Definition_Info_Node d;

   try 
      {
         d = new Definition_Info_Type;
      }

   catch (bad_alloc)
      {
         cerr_strm << thread_name << "ERROR!  In `yyparse()', rule "
                   << "`macro_definition --> "
                   << endl 
                   << "DEF variable UNDECLARED untyped_parameter_list "
                   << endl 
                   << "typed_parameter_list equate_or_assign':"
                   << endl << "`new Definition_Info_Type' failed.  "
                   << "Clearing `scanner_node->macro_untyped_parameter_vector' "
                   << endl 
                   << "and `scanner_node->macro_typed_parameter_vector' "
                   << "and rethrowing `bad_alloc'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm, error_stop_value);
         cerr_strm.str("");

         scanner_node->macro_untyped_parameter_vector.clear();
         scanner_node->macro_typed_parameter_vector.clear();

         throw;
 
      }  /* |catch (bad_alloc)|  */


@q ****** (6).@> 

   entry = scanner_node->add_entry(@=$2@>,
                                   MACRO,
                                   Id_Map_Entry_Type::REGULAR,
                                   false,
                                   false,
                                   Id_Map_Entry_Type::KNOWN,
                                   static_cast<void*>(d)); 

@q ****** (6).@> 

   if (entry == static_cast<Id_Map_Entry_Node>(0))
      {
         cerr_strm << thread_name << "ERROR!  In `yyparse()', rule "
                   << "`macro_definition --> "
                   << endl 
                   << "DEF variable UNDECLARED untyped_parameter_list "
                   << endl 
                   << "typed_parameter_list equate_or_assign':"
                   << endl << "`Scanner_Type::add_entry() failed.  "
                   << "Clearing `scanner_node->macro_untyped_parameter_vector' "
                   << endl 
                   << "and `scanner_node->macro_typed_parameter_vector', "
                   << "setting `macro_definition' to 0, and will "
                   << "try to continue.";

         log_message(cerr_strm);
         cerr_message(cerr_strm, error_stop_value);
         cerr_strm.str("");
         
         scanner_node->macro_untyped_parameter_vector.clear();
         scanner_node->macro_typed_parameter_vector.clear();

         @=$$@> = static_cast<void*>(0);         

      }  /* |if (entry == 0)|  */

@q ****** (6).@> 

   else /* |entry != 0|  */
      {
     

@q ******* (7) Call |Scan_Parse::macro_definition_func()|.@> 

@ Call |Scan_Parse::macro_definition_func()|.
\initials{LDF 2004.12.30.}

@<Define rules@>=

         try  
            {
               status = macro_definition_func(scanner_node,
                                              entry); 
            }


@q ******* (7) Error handling:  |Scan_Parse::macro_definition_func()| failed, @>
@q ******* (7) throwing |bad_alloc|.                                          @>  

@ Error handling:  |Scan_Parse::macro_definition_func()| failed, 
throwing |bad_alloc|.                                            
\initials{LDF 2004.12.30.}

@<Define rules@>=

         catch (bad_alloc)
            {
                cerr_strm << thread_name << "ERROR!  In `yyparse()', rule "
                          << "`macro_definition --> "
                          << "DEF variable UNDECLARED equate_or_assign':"
                          << endl << "`Scan_Parse::macro_definition_func()' failed, "
                          << "throwing `bad_alloc'."
                          << endl  
                          << "Clearing `scanner_node->macro_untyped_parameter_vector' "
                          << "and `scanner_node->macro_typed_parameter_vector'"
                          << endl << "and rethrowing `bad_alloc'.";

                log_message(cerr_strm);
                cerr_message(cerr_strm, error_stop_value);
                cerr_strm.str("");

                scanner_node->macro_untyped_parameter_vector.clear();
                scanner_node->macro_typed_parameter_vector.clear();

                throw;

            }  /* |catch (bad_alloc)|  */

@q ******* (7) Error handling:  |Scan_Parse::macro_definition_func()| failed, @>
@q ******* (7) returning a non-zero value.                                    @>  

@ Error handling:  |Scan_Parse::macro_definition_func()| failed, 
returning a non-zero value.                                      
\initials{LDF 2004.12.30.}

@<Define rules@>=

         if (status != 0)
            {     

                cerr_strm << thread_name << "ERROR!  In `yyparse()', rule "
                          << "`macro_definition --> "
                          << "DEF variable UNDECLARED equate_or_assign':"
                          << endl << "`Scan_Parse::macro_definition_func()' failed, "
                          << "returning " << status << "." 
                          << endl 
                          << "Clearing `scanner_node->macro_untyped_parameter_vector' "
                          << "and `scanner_node->macro_typed_parameter_vector'"
                          << endl 
                          << "and will try to continue.";

                log_message(cerr_strm);
                cerr_message(cerr_strm, error_stop_value);
                cerr_strm.str("");

                scanner_node->macro_untyped_parameter_vector.clear();
                scanner_node->macro_typed_parameter_vector.clear();


                @=$$@> = static_cast<void*>(0);
     
            }  /* |if (status != 0)|  */

         else /* |status == 0|  */
            {
               scanner_node->macro_untyped_parameter_vector.clear();
               scanner_node->macro_typed_parameter_vector.clear();

               @=$$@> = static_cast<void*>(0);

            }  /* |else| (|status == 0|)  */

      }  /* |else| (|entry != 0|)  */

@q ****** (6).@> 

};

@q ** (2) macro_definition --> DEF macro_variable untyped_parameter_list @>
@q ** (2) typed_parameter_list equate_or_assign.                         @> 

@*1 \§macro definition> $\longrightarrow$ \.{DEF} \§macro variable>
\§untyped parameter list> \§typed parameter list>
\§equate or assign>. 

\LOG
\initials{LDF 2005.01.03.}
Rewrote this rule.
\ENDLOG 

@q ***** (5) Definition.@> 

@<Define rules@>= 
 
@=macro_definition: DEF macro_variable untyped_parameter_list@>@/
@=typed_parameter_list equate_or_assign@>@/
{

   @<Common declarations for rules@>@; 

 #if DEBUG_COMPILE
    DEBUG = false;  /* |true|  */
    if (DEBUG)
       {

           cerr_strm << thread_name << "*** Parser: `macro_definition --> "
                     << endl 
                     << "DEF macro_variable untyped_parameter_list "
                     << endl 
                     << "typed_parameter_list equate_or_assign'.";

           log_message(cerr_strm);
           cerr_message(cerr_strm);
           cerr_strm.str("");
       }
#endif /* |DEBUG_COMPILE|  */@; 


@q ****** (6).@> 

   entry = static_cast<Id_Map_Entry_Node>(@=$2@>); 

   if (entry == static_cast<Id_Map_Entry_Node>(0))
      {
         cerr_strm << thread_name << "ERROR!  In `yyparse()', rule "
                   << "`macro_definition --> "
                   << endl 
                   << "DEF macro_variable untyped_parameter_list "
                   << endl 
                   << "typed_parameter_list equate_or_assign':"
                   << endl << "`Scanner_Type::add_entry() failed.  "
                   << "Clearing `scanner_node->macro_untyped_parameter_vector' "
                   << endl 
                   << "and `scanner_node->macro_typed_parameter_vector', "
                   << "setting `macro_definition' to 0, and will "
                   << "try to continue.";

         log_message(cerr_strm);
         cerr_message(cerr_strm, error_stop_value);
         cerr_strm.str("");
         
         scanner_node->macro_untyped_parameter_vector.clear();
         scanner_node->macro_typed_parameter_vector.clear();

         @=$$@> = static_cast<void*>(0);         

      }  /* |if (entry == 0)|  */

@q ****** (6).@> 

   else /* |entry != 0|  */
      {
     

@q ******* (7) @> 

         if (entry->object == static_cast<void*>(0))
            {
               Definition_Info_Node d;

               try 
                  {
                     d = new Definition_Info_Type;
                  }

               catch (bad_alloc)
                  {
                     cerr_strm << thread_name << "ERROR!  In `yyparse()', rule "
                               << "`macro_definition --> "
                               << endl 
                               << "DEF macro_variable untyped_parameter_list "
                               << endl 
                               << "typed_parameter_list equate_or_assign':"
                               << endl << "`new Definition_Info_Type' failed.  "
                               << "Clearing "
                               << "`scanner_node->macro_untyped_parameter_vector' "
                               << endl 
                               << "and `scanner_node->macro_typed_parameter_vector' "
                               << "and rethrowing `bad_alloc'.";

                     log_message(cerr_strm);
                     cerr_message(cerr_strm, error_stop_value);
                     cerr_strm.str("");

                     scanner_node->macro_untyped_parameter_vector.clear();
                     scanner_node->macro_typed_parameter_vector.clear();

                     throw;
 
                 }  /* |catch (bad_alloc)|  */

                entry->object = static_cast<void*>(d); 

            }  /* |if (entry->object == 0)|  */


@q ******* (7) Call |Scan_Parse::macro_definition_func()|.@> 

@ Call |Scan_Parse::macro_definition_func()|.
\initials{LDF 2005.01.03.}

@<Define rules@>=

         try  
            {
               status = macro_definition_func(scanner_node,
                                              entry); 
            }


@q ******* (7) Error handling:  |Scan_Parse::macro_definition_func()| failed, @>
@q ******* (7) throwing |bad_alloc|.                                          @>  

@ Error handling:  |Scan_Parse::macro_definition_func()| failed, 
throwing |bad_alloc|.                                            
\initials{LDF 2005.01.03.}

@<Define rules@>=

         catch (bad_alloc)
            {
                cerr_strm << thread_name << "ERROR!  In `yyparse()', rule "
                          << "`macro_definition --> "
                          << "DEF macro_variable equate_or_assign':"
                          << endl << "`Scan_Parse::macro_definition_func()' failed, "
                          << "throwing `bad_alloc'."
                          << endl  
                          << "Clearing `scanner_node->macro_untyped_parameter_vector' "
                          << "and `scanner_node->macro_typed_parameter_vector'"
                          << endl << "and rethrowing `bad_alloc'.";

                log_message(cerr_strm);
                cerr_message(cerr_strm, error_stop_value);
                cerr_strm.str("");

                scanner_node->macro_untyped_parameter_vector.clear();
                scanner_node->macro_typed_parameter_vector.clear();

                throw;

            }  /* |catch (bad_alloc)|  */

@q ******* (7) Error handling:  |Scan_Parse::macro_definition_func()| failed, @>
@q ******* (7) returning a non-zero value.                                    @>  

@ Error handling:  |Scan_Parse::macro_definition_func()| failed, 
returning a non-zero value.                                      
\initials{LDF 2005.01.03.}

@<Define rules@>=

         if (status != 0)
            {     

                cerr_strm << thread_name << "ERROR!  In `yyparse()', rule "
                          << "`macro_definition --> "
                          << "DEF macro_variable equate_or_assign':"
                          << endl << "`Scan_Parse::macro_definition_func()' failed, "
                          << "returning " << status << "." 
                          << endl 
                          << "Clearing `scanner_node->macro_untyped_parameter_vector' "
                          << "and `scanner_node->macro_typed_parameter_vector'"
                          << endl 
                          << "and will try to continue.";

                log_message(cerr_strm);
                cerr_message(cerr_strm, error_stop_value);
                cerr_strm.str("");

                scanner_node->macro_untyped_parameter_vector.clear();
                scanner_node->macro_typed_parameter_vector.clear();


                @=$$@> = static_cast<void*>(0);
     
            }  /* |if (status != 0)|  */

         else /* |status == 0|  */
            {
               scanner_node->macro_untyped_parameter_vector.clear();
               scanner_node->macro_typed_parameter_vector.clear();

               @=$$@> = static_cast<void*>(0);

            }  /* |else| (|status == 0|)  */

      }  /* |else| (|entry != 0|)  */

@q ****** (6).@> 

};


@q ** (2) macro_definition --> DEF macro_vector_variable untyped_parameter_list @>
@q ** (2) typed_parameter_list PLUS_ASSIGN. @> 

@*1 \§macro definition> $\longrightarrow$ \.{DEF} \§macro vector variable>
\§untyped parameter list> \§typed parameter list>
\.{PLUS\_ASSIGN} \§macro variable>.  

\LOG
\initials{LDF 2005.01.03.}
Added this rule.
\ENDLOG 

@q ***** (5) Definition.@> 

@<Define rules@>= 
 
@=macro_definition: DEF macro_vector_variable untyped_parameter_list@>@/
@=typed_parameter_list PLUS_ASSIGN@>@/
{


   macro_vector_definition_func(static_cast<Scanner_Node>(parameter),
                                static_cast<Id_Map_Entry_Node>(@=$2@>)); 

   @=$$@> = static_cast<void*>(0);

};


@q * (1) typed_parameter_list.@> 
@* \§typed parameter list>.
\initials{LDF 2004.12.29.}

\LOG
\initials{LDF 2004.12.29.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> typed_parameter_list@>

@q ** (2) typed_parameter_list --> EMPTY.@> 
@*1 \§typed parameter list> $\longrightarrow$ \.{EMPTY}.
\initials{LDF 2004.12.29.}

\LOG
\initials{LDF 2004.12.29.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=typed_parameter_list: /* Empty  */@>@/ 
{

   @=$$@> = static_cast<void*>(0);
};

@q ** (2) typed_parameter_list --> LEFT_BRACE typed_parameter_sublist RIGHT_BRACE.@> 
@*1 \§typed parameter list> $\longrightarrow$ \.{LEFT\_BRACE}
\§typed parameter sublist> \.{RIGHT\_BRACE}.
\initials{LDF 2004.12.29.}

\LOG
\initials{LDF 2004.12.29.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=typed_parameter_list: LEFT_BRACE typed_parameter_sublist RIGHT_BRACE@>@/ 
{
   @=$$@> = @=$1@>;
};

@q * (1) typed_parameter_sublist.@> 
@* \§typed parameter sublist>.
\initials{LDF 2004.12.29.}

\LOG
\initials{LDF 2004.12.29.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> typed_parameter_sublist@>

@q ** (2) typed_parameter_sublist --> any_declarator symbolic_token.@> 
@*1 \§typed parameter sublist> $\longrightarrow$ \§any declarator> 
\§symbolic token>.
\initials{LDF 2004.12.29.}

\LOG
\initials{LDF 2004.12.29.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=typed_parameter_sublist: any_declarator symbolic_token@>@/ 
{
   static_cast<Scanner_Node>(
        parameter)->macro_typed_parameter_vector.push_back(
            make_pair(@=$1@>, @=$2@>));   
   
   @=$$@> = static_cast<void*>(0);

};

@q ** (2) typed_parameter_sublist --> typed_parameter_sublist COMMA @>
@q ** (2) any_declarator symbolic_token.@>  

@*1 \§typed parameter sublist> $\longrightarrow$ \§typed parameter sublist> 
\.{COMMA} \§any declarator> \§symbolic token>.
\initials{LDF 2004.12.29.}

\LOG
\initials{LDF 2004.12.29.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=typed_parameter_sublist: typed_parameter_sublist COMMA any_declarator symbolic_token@>@/ 
{
   static_cast<Scanner_Node>(
      parameter)->macro_typed_parameter_vector.push_back(
         make_pair(@=$3@>, @=$4@>));   

   @=$$@> = static_cast<void*>(0);

};


@q * (1) untyped_parameter_list.@> 
@* \§untyped parameter list>.
\initials{LDF 2004.12.29.}

\LOG
\initials{LDF 2004.12.29.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> untyped_parameter_list@>

@q ** (2) untyped_parameter_list --> EMPTY.@> 
@*1 \§untyped parameter list> $\longrightarrow$ \.{EMPTY}.
\initials{LDF 2004.12.29.}

\LOG
\initials{LDF 2004.12.29.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=untyped_parameter_list: /* Empty  */@>@/ 
{
   @=$$@> = static_cast<void*>(0);

};

@q ** (2) untyped_parameter_list --> LEFT_PARENTHESIS @> 
@q ** (2) untyped_parameter_sublist RIGHT_PARENTHESIS.@> 
@*1 \§untyped parameter list> $\longrightarrow$ \.{LEFT\_PARENTHESIS}
\§untyped parameter sublist> \.{RIGHT\_PARENTHESIS}.
\initials{LDF 2005.01.02.}

\LOG
\initials{LDF 2005.01.02.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=untyped_parameter_list: LEFT_PARENTHESIS untyped_parameter_sublist RIGHT_PARENTHESIS@>@/ 
{
   @=$$@> = static_cast<void*>(0);

};

@q * (1) untyped_parameter_sublist.@> 
@* \§untyped parameter sublist>.
\initials{LDF 2005.01.02.}

\LOG
\initials{LDF 2005.01.02.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> untyped_parameter_sublist@>

@q ** (2) untyped_parameter_sublist --> any_declarator symbolic_token.@> 
@*1 \§untyped parameter sublist> $\longrightarrow$ 
\§symbolic token>.
\initials{LDF 2005.01.02.}

\LOG
\initials{LDF 2005.01.02.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=untyped_parameter_sublist: symbolic_token@>@/ 
{
   static_cast<Scanner_Node>(
      parameter)->macro_untyped_parameter_vector.push_back(
         @=$1@>);

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) untyped_parameter_sublist --> untyped_parameter_sublist COMMA @>
@q ** (2) symbolic_token.@>  

@*1 \§untyped parameter sublist> $\longrightarrow$ \§untyped parameter sublist> 
\.{COMMA} \§symbolic token>.
\initials{LDF 2005.01.02.}

\LOG
\initials{LDF 2005.01.02.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=untyped_parameter_sublist: untyped_parameter_sublist COMMA symbolic_token@>@/ 
{

   static_cast<Scanner_Node>(
      parameter)->macro_untyped_parameter_vector.push_back(
         @=$3@>);

   @=$$@> = static_cast<void*>(0);

};


@q * (1) macro_call.@> 
@* \§macro call>.
\initials{LDF 2004.12.22.}

\LOG
\initials{LDF 2004.12.22.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> macro_call@>

@q ** (2) macro_call --> macro_variable untyped_parameter_list@> 

@*1 \§macro call> $\longrightarrow$  \§macro variable> 
\§untyped parameter list>.
\initials{LDF 2005.01.02.}

\LOG
\initials{LDF 2004.12.22.}
Added this rule.

\initials{LDF 2004.12.29.}
Changed |defun_call| and |defun_variable| to 
|macro_call| and |macro_variable|.
\ENDLOG


@q *** (3) Definition.@> 

@<Define rules@>=
@=macro_call: macro_variable untyped_parameter_list@>@/
{

   macro_call_func(static_cast<Scanner_Node>(parameter),
                   static_cast<Id_Map_Entry_Node>(@=$1@>));
   @=$$@> = static_cast<void*>(0);

};

@q ** (2) macro_call --> macro_variable SEMI_COLON@> 

@*1 \§macro call> $\longrightarrow$  \§macro variable> 
\.{SEMI\_COLON}.
\initials{LDF 2005.01.03.}

\LOG
\initials{LDF 2005.01.03.}
Added this rule.
\ENDLOG


@q *** (3) Definition.@> 

@<Define rules@>=
@=macro_call: macro_variable SEMI_COLON@>@/
{

   macro_call_func(static_cast<Scanner_Node>(parameter),
                   static_cast<Id_Map_Entry_Node>(@=$1@>));

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) macro_call --> macro_variable LEFT_BRACE@> 

@*1 \§macro call> $\longrightarrow$  \§macro variable> 
\.{LEFT\_BRACE}.
\initials{LDF 2005.01.03.}

\LOG
\initials{LDF 2005.01.03.}
Added this rule.
\ENDLOG


@q *** (3) Definition.@> 

@<Define rules@>=
@=macro_call: macro_variable LEFT_BRACE@>@/
{
    macro_call_func(static_cast<Scanner_Node>(parameter),
                    static_cast<Id_Map_Entry_Node>(@=$1@>),
                    true);
   @=$$@> = static_cast<void*>(0);

};


@q * (0) @>


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


