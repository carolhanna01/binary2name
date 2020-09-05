@q pgrpcmnd.w @> 
@q Created by Laurence Finston Sun Sep 12 17:33:25 CEST 2004 @>
     
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



@q * (0) Group commands.  @>
@** Group commands.  

\LOG
\initials{LDF 2004.09.12.}  
Created this file.
\ENDLOG 


@q * (1) group_command.@>
@*  {\bf group\_command}.@>

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> group_command@>

@q ** (2) group_command --> begin_group_command.@>
@*1 \§group command> $\longrightarrow$ \§begin-group command>.

@
@<Define rules@>=
@=group_command: begin_group_command@>@/
{

#if DEBUG_COMPILE 
  @<Common declarations for rules@>@;
#endif
  
#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
      cerr_strm << "*** Parser: `group_command --> "
                << "begin_group_command'.";

      log_message(cerr_strm);
      cerr_message(cerr_strm);
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */@;

};

@q ** (2) group_command --> save_command.@>
@*1 \§group command> $\longrightarrow$ \§end-group command>.

\LOG
\initials{LDF 2004.09.13.}
Added this rule.  I'm not sure whether I'll need it, though. 
\ENDLOG 

@
@<Define rules@>=
@=group_command: save_command@>@/
{

#if DEBUG_COMPILE 
  @<Common declarations for rules@>@;
#endif
  
#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
      cerr_strm << "*** Parser: `group_command --> "
                << "save_command'.";

      log_message(cerr_strm);
      cerr_message(cerr_strm);
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */@;

};



@q * (1) begin_group_command.@>
@* \§begin group command>.

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> begin_group_command@>


@q ** (2) begin_group_command: BEGIN_GROUP@>
@*1 \§begin group command> $\longrightarrow$ \.{BEGIN\_GROUP}.
\initials{LDF Undated.}

\LOG
\initials{LDF 2007.10.09.}
@:BUG FIX@> BUG FIX:  Now setting |id_map_node->scanner_node = scanner_node|.
\ENDLOG 

@<Define rules@>=
@=begin_group_command: BEGIN_GROUP@>@/
{

#if DEBUG_COMPILE 
   @<Common declarations for rules@>@;
#else
   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter); 
#endif
  
#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
      cerr_strm << thread_name << "*** Parser: `begin_group_command --> "
                << "BEGIN_GROUP'.";


      cerr_strm << endl << "`scanner_node->token_string' == "
                << scanner_node->token_string;

      log_message(cerr_strm);
      cerr_message(cerr_strm);
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */@;

@q ** (2).@>

#if DEBUG_COMPILE
   if (DEBUG)
     {
       cerr_strm << thread_name << "In `yyparse()', rule "
                 << endl << "`begin_group_command --> "
                 << "BEGIN_GROUP':"
                 << endl << "Incrementing `scanner_node->group_ctr'. "
                 << "New value: " << (scanner_node->group_ctr + 1) << ".";

      log_message(cerr_strm);
      cerr_message(cerr_strm);
      cerr_strm.str("");

     }
#endif /* |DEBUG_COMPILE|  */@; 
 
  scanner_node->group_ctr++;

  Id_Map_Node id_map_node = new Id_Map_Type;
  id_map_node->scanner_node = scanner_node;

  id_map_node->up = scanner_node->id_map_node;

  scanner_node->id_map_node = id_map_node;

@q ** (2) Set |$$| to 0 and exit rule.@>

@ Set |$$| to 0 and exit rule.
\initials{LDF 2004.09.13.}

@<Define rules@>=

  @=$$@> = static_cast<void*>(0); 


};

@q * (1) command: \.{END\_GROUP}.@>
@* \§command> $\longrightarrow$ \.{END\_GROUP}.
\initials{LDF 2007.10.09.}

\LOG
\initials{LDF 2007.10.09.}
Changed this rule from ``\§end group command> $\longrightarrow$ 
\.{END\_GROUP}'' to 
``\§command> $\longrightarrow$ \.{END\_GROUP}''.
Removed code from it and now calling |Scan_Parse::end_group_func|.
\ENDLOG 

@<Define rules@>=
@=command: END_GROUP@>@/
{

  Scan_Parse::end_group_func(parameter);
  
  @=$$@> = static_cast<void*>(0);

};


@q * (1) save_command.@>
@*  \§save command>.

\LOG
\initials{LDF 2004.09.13.}
Added this rule.
\ENDLOG 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> save_command@>

@
@<Define rules@>=
@=save_command: SAVE@>@/
{

  @=$$@> = static_cast<void*>(0);

};

@q * @>

@q   Emacs-Lisp code for use in indirect buffers when using the          @>
@q   GNU Emacs editor.  The local variable list is not evaluated when an @>
@q   indirect buffer is visited, so it's necessary to evaluate the       @>
@q   following s-expression in order to use the facilities normally      @>
@q   accessed via the local variables list.                              @>
@q   \initials{LDF 2004.02.12}.                                          @>
@q   (progn (cweb-mode) (outline-minor-mode t) (setq fill-column 80))    @>


  
@q * Local variables for Emacs.@>
@q Local Variables: @>
@q mode:CWEB @>
@q eval:(display-time) @>
@q run-cweave-on-file:"3DLDFprg.web" @>
@q eval:(read-abbrev-file) @>
@q indent-tabs-mode:nil @>
@q eval:(outline-minor-mode) @>
@q End: @>
