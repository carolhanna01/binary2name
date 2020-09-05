@q pfuncfin.w @> 

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



@q * (1) Additional C++ code. @>
@* Additional \CPLUSPLUS/ code.

@q ** (2) |yylex| declaration.@>
@*1 {\it yylex\/} declaration.
\initials{LDF 2004.04.06.}.

This function is a layer between the low-level 
scanning performed by |sub_yylex| and |yyparse|.  
It's defined in \filename{scan.web}.
\initials{LDF 2004.04.22.}

\LOG
Added this function.
\initials{LDF 2004.04.06.}.
\ENDLOG 

@q *** Declaration. @>

@<Declare functions@>=
int
yylex(YYSTYPE* value,
      yyscan_t parameter);  

#if 0 
      YYLTYPE* location,
#endif 

@q ** yyerror() @>
@ {\it yyerror\/}().
@<Declare functions@>=
int
yyerror(void *v, char const* message);@/

@
@<Define functions@>=
int
yyerror(void *v, char const* message)
{
  cerr << "ERROR! In yyparse(): " << message << endl;
  return 0;
}

@q ** Set |yydebug|.  @>
@ Set {\bf yydebug}.

\LOG
\initials{LDF 2004.05.05.}  Added this function.
\ENDLOG 

@<Declare functions@>=
void
set_yydebug(int i = 1);

@
@<Define functions@>=
void
set_yydebug(int i)
{
  yydebug = i;
  return;
}


@q ** Unset |yydebug|.  @>
@ Unset {\bf yydebug}.

\LOG
\initials{LDF 2004.05.05.}  Added this function.
\ENDLOG 

@<Declare functions@>=
inline
void
unset_yydebug(void)
{
  return set_yydebug(0);
}

@q ** (2) Reset |DEBUG_COMPILE|.@> 
@* Reset {\tt DEBUG\_COMPILE}.
\initials{LDF 2005.10.28.}

\LOG
\initials{LDF 2005.10.28.}
Added this section.
\ENDLOG

@<Reset |DEBUG_COMPILE|@>=

@q #undef DEBUG_COMPILE @>

@q #if DEBUG_COMPILE_SAVE @>
@q #define DEBUG_COMPILE 1 @>
@q #else @>
@q #define DEBUG_COMPILE 0 @>
@q #endif @>

@q #undef DEBUG_COMPILE_SAVE @>

@q ** (2) Garbage. @>
@ Garbage.  Just in case it's not used anywhere else.
\initials{LDF 2004.05.02.}

@<Garbage@>=


@q * (1) Putting the parser together.@>
@* Putting the parser together.

@q ** (2) This is what's compiled.@>

@ This is what's compiled.
\initials{LDF Undated.}

\LOG
\initials{LDF 2005.10.20.}
Now setting |DEBUG_COMPILE| back to |DEBUG_COMPILE_SAVE|
and undefining |DEBUG_COMPILE_SAVE|.

\initials{LDF 2005.10.22.}
@:BUG FIX@> BUG FIX:  Fixed the way I reset  
|DEBUG_COMPILE| to |DEBUG_COMPILE_SAVE|.  
The way it was before didn't work, 
because the bodies of macros are not expanded in 
definitions, but only when the macros are expanded.
\ENDLOG 

@c
@=%{@>
@<Include files@>@;
@<Using declarations@>@;
typedef void* yyscan_t;
@q Declare location type @>
@<Preprocessor macros not only for the parser@>@;
@<Preprocessor macros for the parser only@>@;
@<Declare |name_map| and |type_name_map|@>@;
@=%}@>
@<Bison declarations@>@;
@<Union declaration for |YYSTYPE|@>@;
@<Token and precedence declarations@>@;
@<Type declarations for non-terminal symbols@>@;

@=%{@>
@<Constants in |namespace Scan_Parse|@>@;
@<Declare functions@>@;
@=%}@>

@=%%@> /* Introduces ``Rules'' section.  \initials{LDF 2004.02.11}.  */
@<Define rules@>@;
@=%%@> /* Introduces ``Additional C++ code'' section.  
          \initials{LDF 2004.04.07}.  */
@q <Define |LDF_LOCATION_TYPE| functions@>
@<Define functions@>@;

@<Reset |DEBUG_COMPILE|@>@;


#if 0 
@<Garbage@>@;
#endif 

@q ** (2) This is what's written to `parser_1.h'.@>
@ This is what's written to \filename{parser_1.h}.

It's necessary to write the declaration of |LDF_LOCATION_TYPE| to
\filename{parser.h}, because Bison does not write it to 
\filename{parser.h++}.
\initials{LDF 2004.04.08.}   

\LOG
\initials{LDF 2005.10.28.}
@:BUG FIX@> BUG FIX: Now including |@<Reset |DEBUG_COMPILE|@>|.
Before, the files that loaded \filename{parser.h} had the value
set for parser input files.
\ENDLOG 

@(parser_1.h++@>=

#undef YYLTYPE
typedef void* yyscan_t;
@q <Declare location type @>
@<Preprocessor macros not only for the parser@>@;
@<|extern| declarations in |namespace Scan_Parse|@>@;
@<|extern| declaration of |name_map| and |type_name_map|@>@;
@<Declare functions@>@;
@<Reset |DEBUG_COMPILE|@>@;

  
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
