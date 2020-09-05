@q ptokdecl.w @> 
@q Created by Laurence Finston Thu Jan 29 19:09:21 MET 2004  @>
     
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



@q * (0) symbolic_token.  @>
@** \§symbolic token>.
\initials{LDF Undated.}

@<Type declarations for non-terminal symbols@>=
@=%type <string_value> symbolic_token@>@/

@q * (1) symbolic_token --> spark.  @>
@* \§symbolic token> $\longrightarrow$ \§spark>.
\initials{LDF Undated.}

@<Define rules@>=
@=symbolic_token: spark@>
{

  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  if (DEBUG)
    {
      cerr << "\n*** Parser: symbolic_token (spark)." << endl;
    }
#endif /* |DEBUG_COMPILE|  */
};

@=symbolic_token: tag@>
{
  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  if (DEBUG)
    {
      cerr << "\n*** Parser: symbolic_token (tag)." << endl;
    }
#endif /* |DEBUG_COMPILE|  */
}
;

@q ** spark.  @>
@*1 \§spark>.
@q { @>
@:spark}{{\bf spark}@>
@q } @>

\TODO
@q { @>
@:TO DO}{{\bf TO DO}@>
@q } @> 
!! TO DO:  Add more rules for the other sparks.
\initials{LDF 2004.04.22.}
\ENDTODO 


@q *** spark --> BOOLEAN_DECLARATOR.  @>
@*2 \§spark> $\longrightarrow$ \.{BOOLEAN\_DECLARATOR}.

\LOG
\initials{LDF 2004.05.17.}  Added this rule.
\ENDLOG 

@<Define rules@>=
@=spark: BOOLEAN_DECLARATOR@>
{
  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  if (DEBUG)
    {
      cerr_strm << "*** Parser: `spark --> BOOLEAN_DECLARATOR'.";
      
      log_message(cerr_strm);
      cerr_message(cerr_strm);
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */
}
;

@q *** spark --> BOOL_POINT_DECLARATOR.  @>
@*2 \§spark> $\longrightarrow$ \.{BOOL\_POINT\_DECLARATOR}.

\LOG
\initials{LDF 2004.09.01.}  Added this rule.
\ENDLOG 

@<Define rules@>=
@=spark: BOOL_POINT_DECLARATOR@>
{
  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  if (DEBUG)
    {
      cerr_strm << thread_name << "*** Parser: `spark --> "
                << "BOOL_POINT_DECLARATOR'.";
      
      log_message(cerr_strm);
      cerr_message(cerr_strm);
      cerr_strm.str("");
    
    }
#endif /* |DEBUG_COMPILE|  */
}
;



@q *** STRING_DECLARATOR.  @>
@ |STRING_DECLARATOR|.

\LOG
\initials{LDF 2004.05.19.}  Added this rule.
\ENDLOG 

@<Define rules@>=
@=spark: STRING_DECLARATOR@>
{
  
  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  if (DEBUG)
    {
      cerr << "\n*** Parser: spark (STRING_DECLARATOR)." << endl;
    }
#endif /* |DEBUG_COMPILE|  */
}
;

@q *** NUMERIC_DECLARATOR.  @>
@ |NUMERIC_DECLARATOR|.

@<Define rules@>=
@=spark: NUMERIC_DECLARATOR@>
{

  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  if (DEBUG)
    {
      cerr << "\n*** Parser: spark (NUMERIC_DECLARATOR)." << endl;
    }
#endif /* |DEBUG_COMPILE|  */
}
;

@q *** TRANSFORM_DECLARATOR.  @>
@ |TRANSFORM_DECLARATOR|.

@<Define rules@>=
@=spark: TRANSFORM_DECLARATOR@>
{

  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  if (DEBUG)
    {
      cerr << "\n*** Parser: spark (TRANSFORM_DECLARATOR)." << endl;
    }
#endif /* |DEBUG_COMPILE|  */
}
;

@q *** POINT_DECLARATOR.  @>
@ |POINT_DECLARATOR|.

@<Define rules@>=
@=spark: POINT_DECLARATOR@>
{
  
  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  if (DEBUG)
    {
      cerr << "\n*** Parser: spark (POINT_DECLARATOR)." << endl;
    }
#endif /* |DEBUG_COMPILE|  */
}
;

@q *** PATH_DECLARATOR.  @>
@ |PATH_DECLARATOR|.

@<Define rules@>=
@=spark: PATH_DECLARATOR@>
{
  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  if (DEBUG)
    {
      cerr << "\n*** Parser: spark (PATH_DECLARATOR)." << endl;
    }
#endif /* |DEBUG_COMPILE|  */
}
;

@q *** TRIANGLE_DECLARATOR.  @>
@ |TRIANGLE_DECLARATOR|.

@<Define rules@>=
@=spark: TRIANGLE_DECLARATOR@>
{
  @<Common declarations for rules@>@; 
  
#if DEBUG_COMPILE
  if (DEBUG)
    {
      cerr << "\n*** Parser: spark (TRIANGLE_DECLARATOR)." << endl;
    }
#endif /* |DEBUG_COMPILE|  */
}
;

@q *** REG_POLYGON_DECLARATOR.  @>
@ |REG_POLYGON_DECLARATOR|.

@<Define rules@>=
@=spark: REG_POLYGON_DECLARATOR@>
{
  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  if (DEBUG)
    {
      cerr << "\n*** Parser: spark (REG_POLYGON_DECLARATOR)." << endl;
    }
#endif /* |DEBUG_COMPILE|  */
}
;

@q *** RECTANGLE_DECLARATOR.  @>
@ |RECTANGLE_DECLARATOR|.

@<Define rules@>=
@=spark: RECTANGLE_DECLARATOR@>
{

  @<Common declarations for rules@>@; 


#if DEBUG_COMPILE
  if (DEBUG)
    {
      cerr << "\n*** Parser: spark (RECTANGLE_DECLARATOR)." << endl;
    }
#endif /* |DEBUG_COMPILE|  */
}
;

@q *** SQUARE_DECLARATOR.  @>
@ |SQUARE_DECLARATOR|.

@<Define rules@>=
@=spark: SQUARE_DECLARATOR@>
{

  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  if (DEBUG)
    {
      cerr << "\n*** Parser: spark (SQUARE_DECLARATOR)." << endl;
    }
#endif /* |DEBUG_COMPILE|  */
}
;

@q *** ELLIPSE_DECLARATOR.  @>
@ |ELLIPSE_DECLARATOR|.

@<Define rules@>=
@=spark: ELLIPSE_DECLARATOR@>
{

  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  if (DEBUG)
    {
      cerr << "\n*** Parser: spark (ELLIPSE_DECLARATOR)." << endl;
    }
#endif /* |DEBUG_COMPILE|  */

};

@q *** (3) CIRCLE_DECLARATOR.  @>
@*2 |CIRCLE_DECLARATOR|.

@<Define rules@>=
@=spark: CIRCLE_DECLARATOR@>
{
  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
   if (DEBUG)
      {
          cerr_strm << "In `yyparse()', rule 'spark --> CIRCLE_DECLARATOR'.";

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */

};

@q *** (3) spark --> HELIX_DECLARATOR.  @>
@*2 \§spark> $\longrightarrow$ \.{HELIX\_DECLARATOR}.
\initials{LDF 2005.05.18.}

\LOG
\initials{LDF 2005.05.18.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=spark: HELIX_DECLARATOR@>
{
  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
   if (DEBUG)
      {
          cerr_strm << "In `yyparse()', rule 'spark --> HELIX_DECLARATOR'.";

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */

};

@q *** (3) spark --> HELIX_VECTOR_DECLARATOR.@>
@*2 \§spark> $\longrightarrow$ \.{HELIX\_VECTOR\_DECLARATOR}.
\initials{LDF 2005.05.18.}

\LOG
\initials{LDF 2005.05.18.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=spark: HELIX_VECTOR_DECLARATOR@>
{
  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
   if (DEBUG)
      {
          cerr_strm << "In `yyparse()', "
                    << "rule 'spark --> HELIX_VECTOR_DECLARATOR'.";

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */

};





@q *** (3) CUBOID_DECLARATOR.  @>
@*2 |CUBOID_DECLARATOR|.

@<Define rules@>=
@=spark: CUBOID_DECLARATOR@>
{
  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  if (DEBUG)
    {
      cerr << "\n*** Parser: spark (CUBOID_DECLARATOR)." << endl;
    }
#endif /* |DEBUG_COMPILE|  */

};

@q *** POLYHEDRON_DECLARATOR.  @>
@ |POLYHEDRON_DECLARATOR|.

@<Define rules@>=
@=spark: POLYHEDRON_DECLARATOR@>
{
  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  if (DEBUG)
    {
      cerr << "\n*** Parser: spark (POLYHEDRON_DECLARATOR)." << endl;
    }
#endif /* |DEBUG_COMPILE|  */

};

@q ** (2) tag.@>
@*1 \§tag>.
\initials{LDF Undated.}

@<Type declarations for non-terminal symbols@>=
@=%type <string_value> tag@>@/


@q *** (3) SYMBOL_0.  @>
@*2 \.{SYMBOL\_0}.
\initials{LDF Undated.}

\LOG
\initials{LDF 2004.12.28.}
Made debugging output thread-safe.
\ENDLOG 

@<Define rules@>=
@=tag: SYMBOL_0@>@/
{

  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
   if (DEBUG)
      {
         cerr_strm << thread_name << "*** Parser:  `tag --> SYMBOL_0'.";
    
         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */

};

@q *** SYMBOL_1.  @>

@=tag:  SYMBOL_1@>
{
  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  if (DEBUG)
    {
      cerr << "\n*** Parser: tag (SYMBOL_1)." << endl;
    }
#endif /* |DEBUG_COMPILE|  */
};

@q *** SYMBOL_2.  @>

@=tag:  SYMBOL_2@>
{

  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  if (DEBUG)
    {
      cerr << "\n*** Parser: tag (SYMBOL_2)." << endl;
    }
#endif /* |DEBUG_COMPILE|  */
}
;
@q *** SYMBOL_3.  @>

@=tag: SYMBOL_3@>
{
  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  if (DEBUG)
    {
      cerr << "\n*** Parser: tag (SYMBOL_3)." << endl;
    }
#endif /* |DEBUG_COMPILE|  */
};

@q *** SYMBOL_4.  @>

@=tag:  SYMBOL_4@>
{

  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  if (DEBUG)
    {
      cerr << "\n*** Parser: tag (SYMBOL_4)." << endl;
    }
#endif /* |DEBUG_COMPILE|  */
};

@q *** SYMBOL_5.  @>

@=tag:  SYMBOL_5@>
{

@<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  if (DEBUG)
    {
      cerr << "\n*** Parser: tag (SYMBOL_5)." << endl;
    }
#endif /* |DEBUG_COMPILE|  */
};

@q *** SYMBOL_6.  @>

@=tag:  SYMBOL_6@>
{

  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  if (DEBUG)
    {
      cerr << "\n*** Parser: tag (SYMBOL_6)." << endl;
    }
#endif /* |DEBUG_COMPILE|  */
};

@q *** SYMBOL_7.  @>

@=tag:  SYMBOL_7@>
{

  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  if (DEBUG)
    {
      cerr << "\n*** Parser: tag (SYMBOL_7)." << endl;
    }
#endif /* |DEBUG_COMPILE|  */
};


@q ** declared_suffix.  @>
@ {\bf declared\_suffix}.
@q { @>
@:declared_suffix}{{\bf declared\_suffix}@>
@q } @>

@<Type declarations for non-terminal symbols@>=
@=%type <string_value> declared_suffix@>@/

@
@<Define rules@>=

@q *** Empty.  @>

@=declared_suffix: /* empty */@>@/
{

  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
    if (DEBUG)
      {
          cerr << "\n*** Parser: declared_suffix (empty)." << endl;
      }
#endif /* |DEBUG_COMPILE|  */
    
    strcpy(@=$$@>, "");

};

@q *** declared_suffix tag.  @>

@=declared_suffix: declared_suffix tag@>
{

  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  if (DEBUG)
    {
      cerr << "\n*** Parser: declared_suffix (declared_suffix tag)." << endl;
    }
#endif /* |DEBUG_COMPILE|  */

  strcat(@=$$@>, @=$2@>);


#if DEBUG_COMPILE
  if (DEBUG)
    {
      cerr << "$2 == " << @=$2@> << endl;
      cerr << "$$ == " << @=$$@> << endl;
    }
#endif /* |DEBUG_COMPILE|  */ 
};

@ In this case, we use the `\.{-}' character to stand for the ``collective'' subscript 
(Knuth, {\it The METAFONTbook}, p.56) on.    
It's convenient to use `\.{-}' because this character can't normally occur in a
tag, since it's a spark. 

\LOG
\initials{LDF 2004.12.28.}
Now using the character octal 243 (British Pound) as the placeholder
for subscripts rather than \.{-}.
\ENDLOG


@<Define rules@>=

@q *** (3) declared_suffix LEFT_BRACKET RIGHT_BRACKET.  @>

@=declared_suffix: declared_suffix LEFT_BRACKET RIGHT_BRACKET@>
{

  @<Common declarations for rules@>@;

#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
      cerr << "\n*** Parser: declared_suffix "
           << "(declared_suffix LEFT_BRACKET RIGHT_BRACKET)." 
           << endl;
    }
#endif /* |DEBUG_COMPILE|  */

  strcat(@=$$@>, "£");

#if DEBUG_COMPILE
  DEBUG = false; /* |true|  */
  if (DEBUG)
    {
      cerr << "$2 == " << @=$2@> << endl;
      cerr << "$3 == " << @=$3@> << endl;
      cerr << "$$ == " << @=$$@> << endl;
    }
#endif /* |DEBUG_COMPILE|  */ 
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
