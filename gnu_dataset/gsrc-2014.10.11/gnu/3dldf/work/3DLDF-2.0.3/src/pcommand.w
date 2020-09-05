@q pcommand.w @> 
@q Created by Laurence Finston Wed Jun  2 19:02:39 CEST 2004  @>
     
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



@q * (0) Commands.  @>
@** Commands.  

\LOG
\initials{LDF 2004.06.02.}  
Created this file.
\ENDLOG 

\TODO
@q { @>
@:TO DO}{{\bf TO DO}@>
@q } @> 
@q !! TO DO:  @>
\initials{LDF 2004.09.22.}
Fix the outline headings and the levels of the starred sections.
\ENDTODO 

@q * (1) command.  @>
@* \§command>.

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> command@>

@q ** (2) command --> label_command.  @>
@*1 \§command> $\longrightarrow$ \§label command>.

\LOG
\initials{LDF 2004.06.30.}  Added this rule.
\ENDLOG 

@<Define rules@>= 
  
@=command: label_command@>
{

  @=$$@> = @=$1@>;

};

@q ** (2) command --> message_command.  @>
@*1 \§command> $\longrightarrow$ \§message command>.

\LOG
\initials{LDF 2004.09.22.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
  
@=command: message_command@>
{

  @=$$@> = @=$1@>;

};

@q *** (3) command --> CLIP_TO path_expression.@> 
@*2 \§command> $\longrightarrow$ \.{CLIP\_TO}
\§path-like expression>.
\initials{LDF 2005.08.16.}

\LOG
\initials{LDF 2005.08.16.}

\initials{LDF 2005.10.24.}
Changed |path_like_expression| to |path_expression|.
Removed debugging code.
\ENDLOG

@<Define rules@>=
@=command: CLIP_TO path_expression@>@/
{

   clip_to_func(@=$2@>, parameter);

   @=$$@> = static_cast<void*>(0);

};


@q ** (2) command --> group_command.  @>
@*1 \§command> $\longrightarrow$ \§group command>.

\LOG
\initials{LDF 2004.09.12.}  
Added this rule.
\ENDLOG 

@<Define rules@>= 
  
@=command: group_command@>
{

  @=$$@> = @=$1@>;

};


@q * (1) message_command.@> 
@* \§message command>.
\initials{LDF 2004.09.22.}

\LOG
\initials{LDF 2004.09.22.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> message_command@>


@q ** (2) message_or_errmessage.@> 
@*1 \§message or errmessage>.
\initials{LDF 2004.09.22.}

\LOG
\initials{LDF 2004.09.22.}
Added this type declaration.
\ENDLOG 

@<Type declarations for non-terminal symbols@>=
@=%type <int_value> message_or_errmessage@>

@q ** (2) message_or_errmessage --> MESSAGE.@> 
@*1 \§message or errmessage> $\longrightarrow$ \.{MESSAGE}.
\initials{LDF 2004.09.22.}

\LOG
\initials{LDF 2004.09.22.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=message_or_errmessage: MESSAGE@>@/
{

  @=$$@> = MESSAGE;

};


@q ** (2) message_or_errmessage --> ERRMESSAGE.@> 
@*1 \§message or errmessage> $\longrightarrow$ \.{ERRMESSAGE}.
\initials{LDF 2004.09.22.}

\LOG
\initials{LDF 2004.09.22.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=message_or_errmessage: ERRMESSAGE@>@/
{

  @=$$@> = ERRMESSAGE;

};

@q * (1) message_command --> message_or_errmessage string_expression.@>

@* \§message command> $\longrightarrow$ \§message or errmessage> \.{string expression}.
\initials{LDF 2004.09.22.}

\LOG
\initials{LDF 2005.11.03.}
Changed |STRING| to |string_expression|.  
Removed code.  Now calling |Scan_Parse::message_command_func()| instead.
\ENDLOG 

@<Define rules@>=
@=message_command: message_or_errmessage string_expression@>@/
{

    Scan_Parse::message_command_func(@=$1@>, @=$2@>, parameter);
    @=$$@> = static_cast<void*>(0);
};


@q ** (2) command --> PAUSE.@> 
@*1 \§command> $\longrightarrow$ \.{PAUSE}.
\initials{LDF 2004.09.22.}

\LOG
\initials{LDF 2004.09.22.}
Added this rule.

\initials{LDF 2005.11.09.}
Removed debugging code.  Added code for pausing.  
I must have removed it at some time in the past.
\ENDLOG

@<Define rules@>=
@=command: PAUSE@>@/
{

    cerr_mutex.lock(); 
    cerr << "Type <RETURN> to continue: ";
    getchar(); 
    cerr_mutex.unlock();  

    @=$$@> = static_cast<void*>(0);
};

@q ** (2) command --> SCANTOKENS string_expression.@> 
@*1 \§command> $\longrightarrow$ \.{SCANTOKENS}.
\§string expression>.
\initials{LDF 2004.12.01.}

\LOG
\initials{LDF 2004.12.01.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=command: SCANTOKENS string_expression@>@/
{

   string* str_ptr = static_cast<string*>(@=$2@>); 

   if (str_ptr == static_cast<string*>(0))
     {
     
         @=$$@> = static_cast<void*>(0);

    }  /* |if (str_ptr == 0)|  */


   else /* |str_ptr != 0|  */
      {

          stringstream* str_strm_ptr;

          Input_Struct* curr_input_struct;


                str_strm_ptr = new stringstream;
                curr_input_struct = new Input_Struct;

          *str_strm_ptr << *str_ptr << ";";

          curr_input_struct->up = static_cast<Scanner_Node>(parameter)->in;
          curr_input_struct->stream_ptr 
            = static_cast<istream*>(str_strm_ptr);

          curr_input_struct->type = Io_Struct::SCANTOKENS_STRING_TYPE;
           
          static_cast<Scanner_Node>(parameter)->in = curr_input_struct;

          @=$$@> = static_cast<void*>(0);

      }   /* |else| (|str_ptr != 0|)  */
};

@q ** (2) command --> VERBATIM_METAPOST.@> 
@*1 \§command> $\longrightarrow$ \.{VERBATIM\_METAPOST}.
\§string expression>.
\initials{LDF 2004.12.13.}

\LOG
\initials{LDF 2004.12.13.}
Added this rule.
\ENDLOG

@q *** (3).@> 

@<Define rules@>=
@=command: VERBATIM_METAPOST string_expression@>@/
{

   string* s = static_cast<string*>(@=$2@>); 

@q **** (4) Error handling:  |s == 0|.@>   

@ Error handling:  |s == 0|.
\initials{LDF 2004.12.13.}

@<Define rules@>=

   if (s == static_cast<string*>(0))
      {
          @=$$@> = static_cast<void*>(0);

      } /* |s == 0|  */


@q **** (4) |s != 0|.@>   

@ |s != 0|.
\initials{LDF 2004.12.13.}

@<Define rules@>=

   else /* |s != 0|  */
      {
         int status = verbatim_metapost_func(static_cast<Scanner_Node>(parameter), s);

@q ***** (5) Error handling:  |verbatim_metapost_func()| failed.@>   

@ Error handling:  |verbatim_metapost_func()| failed.
\initials{LDF 2004.12.13.}

@<Define rules@>=
 
         if (status != 0)
            {

#if 0 
                cerr_strm << thread_name 
                          << "ERROR!  In `yyparse()', rule "
                          << "`verbatim_metapost_command "
                          << "--> VERBATIM_METAPOST string_expression':"
                          << endl << "`verbatim_metapost_func()' failed.  "
                          << "Will try to continue.";

                log_message(cerr_strm); 
                cerr_message(cerr_strm, error_stop_value); 
                cerr_strm.str("");
#endif 

            } /* |if (status != 0)|  */

@q ***** (5) @>   

          delete s;

          @=$$@> = static_cast<void*>(0);

@q ***** (5).@> 

   }   /* |else| (|s != 0|)  */  


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
@q fill-column:70 @>
@q run-cweave-on-file:"3DLDFprg.web"  @>
@q End: @>
