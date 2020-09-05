@q ppicexpr.w @> 
@q Created by Laurence Finston Fri May 21 21:19:28 MEST 2004  @>
     
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



@q * (0) picture expressions.  @>
@** picture expressions.

\LOG
\initials{LDF 2004.05.21.}  Created this file.
\ENDLOG 


@q * (1) picture primary.  @>
@* \§picture primary>.
  
@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> picture_primary@>@/

@q ** (2) picture_primary --> picture_variable.@>
@*1 \§picture primary> $\longrightarrow$ \§picture variable>.  


\LOG
\initials{LDF 2004.06.04.}  
No longer creating a new |Picture| for 
|@=$$@>|.  |Pictures| are persistent, i.e., they shouldn't be copied and 
deleted in rules that use |picture_primaries|, |picture_secondaries|, 
|picture_tertiaries|, or |picture_expressions|.

\initials{LDF 2004.06.30.}
Changed value of |@=$$@>| from |Picture*|, cast to |void*|, 
to |Id_Map_Entry_Node|, cast to |void*|.
\ENDLOG 

@<Define rules@>=
@=picture_primary: picture_variable@>@/
{
#if DEBUG_COMPILE
   bool DEBUG = false; /* |true| */ @; 
   stringstream cerr_strm;
   if (DEBUG)
      {
          cerr_strm << "*** Parser:  In rule `picture_primary: picture_variable':"
                    << endl;
          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str("");
     }
#endif /* |DEBUG_COMPILE|  */@;

  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      @=$$@> = static_cast<void*>(0);

    } /* |if (entry == 0 || entry->object == 0)|  */

@ |entry->object| can be a |void| pointer if the |picture| was declared, 
but hasn't been assigned to.  It shouldn't be necessary to assign to a 
|picture| before using it.
\initials{LDF 2004.06.03.}

\LOG
\initials{LDF 2004.06.03.}  
Added this conditional.  
\ENDLOG 

@<Define rules@>=
  else if (entry->object == static_cast<void*>(0))
    {
      entry->known_state = Id_Map_Entry_Type::KNOWN;
 
      Picture* p = new Picture;
      p->clear();

      entry->object = static_cast<void*>(p);
      
      @=$$@> = static_cast<void*>(entry);

    }

@
@<Define rules@>=

  else /* |entry != 0 && entry->object != 0|  */@;

  {
    @=$$@> = static_cast<void*>(entry);

  }  /* |else| (|entry != 0 && entry->object != 0|)  */@;

};

@q *** (3) picture_primary --> picture_argument..@>
@ \§picture primary> $\longrightarrow$ \§picture argument>.  


@q *** picture_primary --> ( picture_expression )  @>
@ \§picture primary> $\longrightarrow$ `\.{\LP}' \§picture expression> `\.{\RP}'.


@<Define rules@>=
@=picture_primary: LEFT_PARENTHESIS picture_expression RIGHT_PARENTHESIS@>@/
{
#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
      cerr << "*** Parser: picture_primary --> `(' picture_expression `)'."
           << endl;
    }
#endif /* |DEBUG_COMPILE|  */@;

  @=$$@> = @=$2@>;

};

@q ** (2) picture secondary.  @>
@ \§picture secondary>.

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> picture_secondary@>
  
@q *** picture secondary --> picture_primary.@>
@ \§picture secondary> $\longrightarrow$ \§picture primary>.

@<Define rules@>=
@=picture_secondary: picture_primary@>@/
{
#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
      cerr << "\n*** Parser: picture_secondary --> picture_primary "
           << endl;
    }
#endif /* |DEBUG_COMPILE|  */@;

  @=$$@> = @=$1@>;

};

@q *** (3) picture secondary --> picture_primary transformer.@>
@*2 \§picture secondary> $\longrightarrow$ \§picture primary> \§transformer>.

\LOG
\initials{LDF 2004.06.30.}
|picture_primary| is now an |Id_Map_Entry_Node|, cast to |void*|.  
Changed value of |@=$$@>| from |Picture*|, cast to |void*|, 
to |Id_Map_Entry_Node|, cast to |void*|.
Made changes to account for these facts.

\initials{LDF 2004.12.02.}
Commented-out this rule.  It doesn't work properly, 
since temporaries are not created for |pictures|.

\initials{LDF 2004.12.02.}
Commented this rule back in.
Now issuing a warning to the effect that |transformation_commands| 
should be used instead of |transformers| for transforming |pictures|,
setting |picture_secondary| to |picture_primary| unchanged, 
and continuing.  

\initials{LDF 2006.01.20.}
@:BUG FIX@> BUG FIX:  Now deleting |Transform* t|.
\ENDLOG 

@<Define rules@>=
@=picture_secondary: picture_primary transformer@>@/
{
   @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
    DEBUG = false; /* |true| */ @; 
   if (DEBUG)
      {
          cerr_strm << thread_name 
                    << "*** Parser: `picture_secondary --> "
                    << "picture_primary transformer'.";

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str("");
     }
#endif /* |DEBUG_COMPILE|  */@;


   cerr_strm << thread_name << "WARNING!  In `yyparse()', rule "
             << endl << "`picture_secondary --> "
             << "picture_primary transformer':"
             << endl << "It's not possible to transform "
             << "`pictures' using `transformers'.  "
             << "Use a `transformation_command' instead."
             << endl << "Setting `picture_secondary' to "
             << "`picture_primary' unchanged and continuing.";

   log_message(cerr_strm);
   cerr_message(cerr_strm, warning_stop_value);
   cerr_strm.str("");

   Transform* t = static_cast<Transform*>(@=$2@>);

   delete t;

   @=$$@> = @=$1@>;

};


@q ** (2) picture tertiary.  @>
@ \§picture tertiary>.

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> picture_tertiary@>

@q *** picture tertiary --> picture_secondary.@>
@ \§picture tertiary> $\longrightarrow$ \§picture secondary>.


@<Define rules@>=
@=picture_tertiary: picture_secondary@>@/
{



  @=$$@> = @=$1@>;

};

@q *** (3) picture tertiary --> picture_tertiary PLUS picture_secondary.  @>
@ \§picture tertiary> $\longrightarrow$ \§picture tertiary> 
`\.{PLUS}' \§picture secondary>.
\initials{LDF 2004.05.21.}  


Pictures cannot yet be subtracted in 3DLDF.
\initials{LDF 2004.05.21.}

\LOG
\initials{LDF 2004.05.21.}  
Added this rule.


\initials{LDF 2004.06.04.}  
No longer deleting |q| at the end of this rule. 
|Pictures| are persistent, i.e., they shouldn't be copied and 
deleted in rules that use |picture_primaries|, |picture_secondaries|, 
|picture_tertiaries|, or |picture_expressions|.

\initials{LDF 2004.06.30.}
|picture_tertiary| and |picture_secondary| are now both |Id_Map_Entry_Nodes|,
cast to |void*|.  
Changed value of |@=$$@>| from |Picture*|, cast to |void*|, 
to |Id_Map_Entry_Node|, cast to |void*|.
Made changes to account for these facts.
\ENDLOG 

@<Define rules@>=
@=picture_tertiary: picture_tertiary PLUS picture_secondary@>@/
{

  Id_Map_Entry_Node entry_0 = static_cast<Id_Map_Entry_Node>(@=$1@>);
  Id_Map_Entry_Node entry_1 = static_cast<Id_Map_Entry_Node>(@=$3@>);

  Picture* p = static_cast<Picture*>(entry_0->object); 
  Picture* q = static_cast<Picture*>(entry_1->object); 

  *p += *q;

  @=$$@> = static_cast<void*>(entry_0);
  
}
;

@q ** (2) picture expression.  @>
@*1 \§picture expression>.

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> picture_expression@>

@q *** picture expression --> picture_tertiary.  @>
@ \§picture expression> $\longrightarrow$ \§picture tertiary>.

@<Define rules@>=
@=picture_expression: picture_tertiary@>@/
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

