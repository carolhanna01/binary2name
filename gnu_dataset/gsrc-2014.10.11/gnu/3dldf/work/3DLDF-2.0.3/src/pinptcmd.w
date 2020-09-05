@q pinptcmd.w @> 
@q Created by Laurence Finston Tue Sep 14 16:50:08 CEST 2004 @>
     
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

@q Please send bug reports to bug-3dldf@@gnu.org @>
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



@q * (0) Input Command.@>
@** Input Command.

\LOG
\initials{LDF 2004.09.14.}
Created this file. 
\ENDLOG 


@q ** (2) command --> INPUT STRING. @>
@*1  \§command> \.{INPUT} \.{STRING}.

\LOG
\initials{LDF 2004.09.14.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 
@
@<Define rules@>=
  
@=command: INPUT STRING@>@/

{

  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
      cerr_strm << thread_name 
                << "*** Parser: `input_command --> INPUT STRING'.";

      log_message(cerr_strm); 
      cerr_message(cerr_strm); 
      cerr_strm.str(""); 
    }
#endif /* |DEBUG_COMPILE|  */@;


  
  string* s = static_cast<string*>(@=$2@>);

@q **** (4) Error handling:  |s == 0 || s->size() == 0|.@> 

@ Error handling:  |s == 0 || s->size() == 0|.
\initials{LDF 2004.09.14.}

@<Define rules@>=

  if (s == static_cast<string*>(0) || s->size() == 0)
    {

      cerr_strm << thread_name 
                << "ERROR! In `yyparse()', rule `input_command --> INPUT STRING':"
                << endl << "Invalid `STRING'. "
                << endl << "Not inputting file and will try to continue.";

      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");

    } /* |if (s == 0 || s->size() == 0)|  */

@q **** (4) |STRING| is valid.@> 

@ |STRING| is valid.
\initials{LDF 2004.09.14.}

@<Define rules@>=

  else /* |!(s == 0 || s->size() == 0)|  */
    {


#if DEBUG_COMPILE
      if (DEBUG)
        {
          cerr_strm << thread_name 
                    << "In `yyparse()', rule `input_command --> INPUT STRING':"
                    << endl << "*s == " << *s;

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str("");

        }
#endif /* |DEBUG_COMPILE|  */@; 
      


@q ***** (5) Call |Scan_Parse::input_command()|.@> 

@ Call |Scan_Parse::input_command()|.
\initials{LDF 2004.09.14.}

@<Define rules@>=

  status = input_command(scanner_node, s);

@q ****** (6) Error handling:  |Scan_Parse::input_command()| failed.@> 

@ Error handling:  |Scan_Parse::input_command()| failed.
\initials{LDF 2004.09.14.}

@<Define rules@>=

  if (status != 0)
    {

      cerr_strm << thread_name 
                 << "ERROR! In `yyparse()', rule `input_command --> INPUT STRING':"
                 << endl << "`Scan_Parse::input_command()' failed."
                 << endl << "Not inputting file \"" << *s << "\" and will try to continue.";

      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");

    } /* |if (status != 0)|  */


@q ****** (6) |Scan_Parse::input_command()| succeeded.@> 

@ Error handling:  |Scan_Parse::input_command()| succeeded.
\initials{LDF 2004.09.14.}

@<Define rules@>=
#if DEBUG_COMPILE
  else if (DEBUG) /* |status == 0|  */
    {

      cerr_strm << thread_name 
                << "In `yyparse()', rule `input_command --> INPUT STRING':"
                << endl << "`Scan_Parse::input_command()' succeeded."
                 << endl << "Inputting file \"" << *s << "\".";


      log_message(cerr_strm);
      cerr_message(cerr_strm);
      cerr_strm.str("");
       
    }   /* |else if (DEBUG)| (|status == 0|)  */
#endif /* |DEBUG_COMPILE|  */@; 

@q ***** (5) @> 
@
@<Define rules@>=

}  /* |else| (|!(s == 0 || s->size() == 0)|)  */
 
@q **** (4) Delete |s|, set |$$| to 0, and exit rule.@> 

@ Delete |s|, set |@=$$@>| to 0, and exit rule.
|s| is the |string*| represented by |STRING|, 
cast from |void*|.
\initials{LDF 2004.09.14.}


\LOG
\initials{LDF 2004.09.15.}
Now deleting |s|, if it's non-null.
\ENDLOG 

@<Define rules@>=

  if (s != static_cast<string*>(0))
    delete s;

  @=$$@> = static_cast<void*>(0); 

}
;






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
