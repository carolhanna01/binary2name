@q pvariabl.w @> 
@q Created by Laurence Finston Mon May  3 19:30:10 MEST 2004 @>
            
@q * Copyright and License.@>

@q This file is part of GNU 3DLDF, a package for three-dimensional drawing.  @>
@q Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, @>
@q 2011, 2012, 2013 The Free Software Foundation  @>

@q GNU 3DLDF is free software; you can redistribute it and/or modify @>
@q it under the terms of the GNU General Public License as published by @>
@q the Free Software Foundation; either version 3 of the License, or @>
@q (at your option) any later version.  @>

@q GNU 3DLDF is distributed in the hope that it will be useful, @>
@q but WITHOUT ANY WARRANTY; without even the implied warranty of @>
@q MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the @>
@q GNU General Public License for more details.  @>

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




@q * (0) Variables.  @>
@** Variables.

@q * (1) Variables and suffixes.  @>
@* Variables and suffixes.


@q ** (2) variable.  @>
@*1 \§variable>.
\initials{LDF Undated.}

@<Type declarations for non-terminal symbols@>=
@=%type <string_value> variable@>@/

@q *** (3) variable --> tag suffix.@>

@*2 \§variable> $\longrightarrow$ \§tag> \§suffix>.

In this rule, the look-ahead token is pushed onto |scanner_node->rescan_stack| 
followed by a token representing the type of the |variable| found.  
Actually, |Int_Void_Ptr_Bool| |structs| are pushed onto the stack, and the 
|value| of the latter token is included in the |struct|.  This |value| is
the |Id_Map_Entry_Node| referenced by |variable|.  
\initials{LDF 2004.05.03.}

|yylex()| returns the tokens passed to it via |scanner_node->rescan_stack| 
without calling |sub_yylex()| for the next token, making it possible 
to insert a token in the input stream indicating what type of variable we've
found.  This is necessary, because simply using |variable| with no further 
qualification would cause reduce/reduce conflicts.
\initials{LDF 2004.05.03.}

\LOG
\initials{LDF 2004.05.03.}  
Worked on this rule.  

\initials{LDF 2004.07.21.}
Removed most of the code from this rule to |Scan_Parse::variable_func()|, 
defined in \filename{scan.web}.
\ENDLOG 


@q ***** (5) Definition.  @>

@
@<Define rules@>=
@=variable: tag suffix@>@/
{

  String_Bool sb = variable_func(static_cast<Scanner_Node>(parameter),
                                 @=$1@>,
                                 @=$2@>,
                                 yychar,
                                 YYEMPTY);
  
  if (sb.second)
    yyclearin;

  strcpy(@=$$@>, sb.first.c_str());

};

@q ** (2) suffix.  @>
@*1 \§suffix>. 
\initials{LDF Undated.} 

@<Type declarations for non-terminal symbols@>=
@=%type <string_value> suffix@>@/


@q *** (3) suffix --> (EMPTY).  @>
@*2 \§suffix> $\longrightarrow$ (EMPTY). 

@<Define rules@>= 
@=suffix: /* empty */@>@/
{
  strcpy(@=$$@>,"");
  
};

@q **** (4) suffix --> subscript.  @>
@*3 \§suffix> $\longrightarrow$ \§subscript>. 

@<Define rules@>= 

@=suffix: suffix subscript@>
{

  strcat(@=$$@>, "£");

  static_cast<Scanner_Node>(parameter)->token_real_vector.push_back(@=$2@>);


};


@q **** (4) suffix --> tag.  @>
@*3 \§suffix> $\longrightarrow$ \§tag>. 

\LOG
\initials{LDF 2004.12.28.}
@:BUG FIX@> BUG FIX:  Now including this rule in 
|@<Define rules@>|.  The command for doing so was missing;  
I don't know for how long.
\ENDLOG 

@<Define rules@>= 

@=suffix: suffix tag@>
{

  strcat(@=$$@>, @=$2@>);

};


@q *** (3) subscript.  @>
@*2 \§subscript>.  

@<Type declarations for non-terminal symbols@>=
@=%type <real_value> subscript@>@/


@q **** (4) subscript --> numeric_token.  @>

@*3 \§subscript> $\longrightarrow$  \§numeric token>.

@<Define rules@>= 

@=subscript: numeric_token@>@/
{

   @=$$@> = @=$1@>;

};

@q **** (4) subscript --> [ numeric_expression ].  @>

@*3 \§subscript> $\longrightarrow$  `$\lbrack$' \§numeric expression>
`$\rbrack$'.

@<Define rules@>=

@=subscript: LEFT_BRACKET numeric_expression RIGHT_BRACKET@>@/
{
   @=$$@> = @=$2@>;

};

@q ** (2) Variable types.  @>
@*1 Variable types.
\initials{LDF Undated.}

@q *** (3) Non-vector types.@> 
@*2 Non-vector types.
\initials{LDF 2004.12.23.}

\LOG
\initials{LDF 2004.12.23.}
Added this section.
\ENDLOG

@q *** (3) predicate_variable.@>
@*2 \§predicate variable>.
\initials{LDF 2007.11.28.}

\LOG
\initials{LDF 2007.11.28.}
Added this type declaration.

\initials{LDF 2007.11.28.}
Commented-out this type declaration.
\ENDLOG

@q <Type declarations for non-terminal symbols@>

@<Garbage@>=
@=%type <pointer_value> predicate_variable@>@/

@q **** (4) predicate_variable --> variable PREDICATE.@>
@*3 \§predicate variable> $\longrightarrow$ \§variable> \.{PREDICATE}.
\initials{LDF 2007.11.28.}

\LOG
\initials{LDF 2007.11.28.}
Added this rule.

\initials{LDF 2007.11.28.}
Commented-out this rule.
\ENDLOG

@
@<Garbage@>=
@=predicate_variable: variable PREDICATE@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               PREDICATE,
                               parameter);
};

@q *** (3) boolean_variable.  @>
@*2 \§boolean variable>.

\LOG
\initials{LDF 2004.05.17.}  
Added this section.
\ENDLOG 

@q **** (4) boolean_variable --> variable BOOLEAN.@>
@*3 \§boolean variable> $\longrightarrow$ \§variable> \.{BOOLEAN}.

\LOG
\initials{LDF 2004.05.17.}  
Added this rule.

\initials{LDF 2004.08.19} 
Now using |Scan_Parse::variable_type()| instead of
including |@<Common code for variable types@>|.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> boolean_variable@>@/

@
@<Define rules@>= 
@=boolean_variable: variable BOOLEAN@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               BOOLEAN,
                               parameter);

};

@q *** (3) bool_point_variable.  @>
@*2 \§bool-point variable>.

\LOG
\initials{LDF 2004.09.01.}  
Added this section.
\ENDLOG 

@q **** (4) bool_point_variable --> variable BOOL_POINT.@>
@*3 \§bool-point variable> $\longrightarrow$ 
\§variable> \.{BOOL\_POINT}.

\LOG
\initials{LDF 2004.09.01.}  
Added this rule.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> bool_point_variable@>@/

@
@<Define rules@>= 
@=bool_point_variable: variable BOOL_POINT@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               BOOL_POINT,
                               parameter);
};


@q *** (3) string_variable.  @>
@*2 \§string variable>.

\LOG
\initials{LDF 2004.05.19.}  
Added this section.
\ENDLOG 

@q ***  string_variable --> variable STRING.@>
@ \§string variable> $\longrightarrow$ \§variable> \.{STRING}.

\LOG
\initials{LDF 2004.05.19.}  
Added this rule.

\initials{LDF 2004.08.19} 
Now using |Scan_Parse::variable_type()| instead of
including |@<Common code for variable types@>|.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 


@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> string_variable@>@/

@
@<Define rules@>= 
@=string_variable: variable STRING@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               STRING,
                               parameter);

};

@q ***  (3) pen_variable.  @>
@*2 \§pen variable>.

\LOG
\initials{LDF 2004.05.21.}  
Added this section.

\initials{LDF 2004.08.20}
Now using |Scan_Parse::variable_type()| instead of
including |@<Common code for variable types@>|.
\ENDLOG 

@q *** (3) pen_variable --> variable PEN.  pen_variable 1.@>
@*2 \§pen variable> $\longrightarrow$ \§variable> \.{PEN}.

\LOG
\initials{LDF 2004.05.21.}  
Added this rule.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 


@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> pen_variable@>@/

@
@<Define rules@>= 
@=pen_variable: variable PEN@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               PEN,
                               parameter);

};

@q *** (3) dash_pattern_variable.  @>
@ *2\§dash pattern variable>.

\LOG
\initials{LDF 2004.06.07.}  
Added this section.
\ENDLOG 

@q ***  dash_pattern_variable --> variable DASH_PATTERN.  @>
@ \§dash pattern variable> $\longrightarrow$ \§variable> \.{DASH PATTERN}.

\LOG
\initials{LDF 2004.06.07.}  
Added this rule.

\initials{LDF 2004.08.20}
Now using |Scan_Parse::variable_type()| instead of
including |@<Common code for variable types@>|.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 


@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> dash_pattern_variable@>@/

@
@<Define rules@>= 
@=dash_pattern_variable: variable DASH_PATTERN@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               DASH_PATTERN,
                               parameter);
};

@q *** (3) color_variable.@>
@*2 \§color variable>.

\LOG
\initials{LDF 2004.05.25.}  
Added this section.

\initials{LDF 2004.08.20}
Now using |Scan_Parse::variable_type()| instead of
including |@<Common code for variable types@>|.
\ENDLOG 

@q ****  (4) color_variable --> variable COLOR.@>
@*3 \§color variable> $\longrightarrow$ \§variable> \.{COLOR}.

\LOG
\initials{LDF 2004.05.25.}  
Added this rule.

\initials{LDF 2004.08.20}
Now using |Scan_Parse::variable_type()| instead of
including |@<Common code for variable types@>|.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 


@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> color_variable@>@/

@
@<Define rules@>= 
@=color_variable: variable COLOR@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               COLOR,
                               parameter);

};


@q *** (3) Vector types.@> 
@*2 Vector types.
\initials{LDF 2004.12.23.}

\LOG
\initials{LDF 2004.12.23.}
Added this section.
\ENDLOG


@q **** (4) boolean_vector_variable.  @>
@*3 \§boolean vector variable>.
\initials{LDF 2004.12.23.}

\LOG
\initials{LDF 2004.12.23.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> boolean_vector_variable@>@/

@q *****  (5) boolean_vector_variable --> variable BOOLEAN_VECTOR.@>
@*4 \§boolean vector variable> $\longrightarrow$ \§variable> \.{BOOLEAN\_VECTOR}.

\LOG
\initials{LDF 2004.12.23.}
Added this rule.

\initials{LDF 2005.06.08.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 

@<Define rules@>= 
@=boolean_vector_variable: variable BOOLEAN_VECTOR@>
{

   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               BOOLEAN_VECTOR,
                               parameter);
};

@q **** (4) string_vector_variable.  @>
@*3 \§string vector variable>.
\initials{LDF 2004.12.23.}

\LOG
\initials{LDF 2004.12.23.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> string_vector_variable@>@/

@q *****  (5) string_vector_variable --> variable STRING_VECTOR.@>
@*4 \§string vector variable> $\longrightarrow$ \§variable> \.{STRING\_VECTOR}.

\LOG
\initials{LDF 2004.12.23.}
Added this rule.

\initials{LDF 2005.06.08.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 

@<Define rules@>= 
@=string_vector_variable: variable STRING_VECTOR@>
{

   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               STRING_VECTOR,
                               parameter);
};

@q **** (4) numeric_vector_variable.  @>
@*3 \§numeric vector variable>.
\initials{LDF 2004.12.23.}

\LOG
\initials{LDF 2004.12.23.}
Added this type declaration.

\initials{LDF 2005.06.08.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> numeric_vector_variable@>@/

@q *****  (5) numeric_vector_variable --> variable NUMERIC_VECTOR.@>
@*4 \§numeric vector variable> $\longrightarrow$ \§variable> \.{NUMERIC\_VECTOR}.

\LOG
\initials{LDF 2004.12.23.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=numeric_vector_variable: variable NUMERIC_VECTOR@>
{

   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               NUMERIC_VECTOR,
                               parameter);

};

@q **** (4) ulong_long_vector_variable.  @>
@*3 \§ulong long vector variable>.
\initials{LDF 2005.12.05.}

\LOG
\initials{LDF 2005.12.05.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> ulong_long_vector_variable@>@/

@q *****  (5) ulong_long_vector_variable --> variable ULONG_LONG_VECTOR.@>
@*4 \§ulong long vector variable> $\longrightarrow$ \§variable> \.{ULONG\_LONG\_VECTOR}.
\initials{LDF 2005.12.05.}

\LOG
\initials{LDF 2005.12.05.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=ulong_long_vector_variable: variable ULONG_LONG_VECTOR@>
{

   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               ULONG_LONG_VECTOR,
                               parameter);

};

@q *** (3) complex_vector_variable.  @>
@*2 \§complex vector variable>.
\initials{LDF 2007.12.02.}

\LOG
\initials{LDF 2007.12.02.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> complex_vector_variable@>@/

@q **** (4) complex_vector_variable --> variable COMPLEX_VECTOR.@>
@*3 \§complex vector variable> $\longrightarrow$ \§variable> 
\.{COMPLEX\_VECTOR}.
\initials{LDF 2007.12.02.}

\LOG
\initials{LDF 2007.12.02.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=complex_vector_variable: variable COMPLEX_VECTOR@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               COMPLEX_VECTOR,
                               parameter);
};

@q *** (3) matrix_vector_variable.  @>
@*2 \§matrix vector variable>.
\initials{LDF 2007.11.28.}

\LOG
\initials{LDF 2007.11.28.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> matrix_vector_variable@>@/

@q **** (4) matrix_vector_variable --> variable MATRIX_VECTOR.@>
@*3 \§matrix vector variable> $\longrightarrow$ \§variable> 
\.{MATRIX\_VECTOR}.
\initials{LDF 2007.11.28.}

\LOG
\initials{LDF 2007.11.28.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=matrix_vector_variable: variable MATRIX_VECTOR@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               MATRIX_VECTOR,
                               parameter);
};




@q **** (4) pen_vector_variable.  @>
@*3 \§pen vector variable>.
\initials{LDF 2004.12.23.}

\LOG
\initials{LDF 2004.12.23.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> pen_vector_variable@>@/

@q *****  (5) pen_vector_variable --> variable PEN_VECTOR.@>
@*4 \§pen vector variable> $\longrightarrow$ \§variable> \.{PEN\_VECTOR}.

\LOG
\initials{LDF 2004.12.23.}
Added this rule.

\initials{LDF 2005.06.08.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 

@<Define rules@>= 
@=pen_vector_variable: variable PEN_VECTOR@>
{

   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               PEN_VECTOR,
                               parameter);
};

@q **** (4) dash_pattern_vector_variable.  @>
@*3 \§dash pattern vector variable>.
\initials{LDF 2004.12.23.}

\LOG
\initials{LDF 2004.12.23.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> dash_pattern_vector_variable@>@/

@q *****  (5) dash_pattern_vector_variable --> variable DASH_PATTERN_VECTOR.@>
@*4 \§dash pattern vector variable> $\longrightarrow$ \§variable> 
\.{DASH\_PATTERN\_VECTOR}.

\LOG
\initials{LDF 2004.12.23.}
Added this rule.

\initials{LDF 2005.06.08.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 

@<Define rules@>= 
@=dash_pattern_vector_variable: variable DASH_PATTERN_VECTOR@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               DASH_PATTERN_VECTOR,
                               parameter);
};

@q **** (4) picture_vector_variable.  @>
@*3 \§picture vector variable>.
\initials{LDF 2004.12.23.}

\LOG
\initials{LDF 2004.12.23.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> picture_vector_variable@>@/

@q *****  (5) picture_vector_variable --> variable PICTURE_VECTOR.@>
@*4 \§picture vector variable> $\longrightarrow$ \§variable> \.{PICTURE\_VECTOR}.

\LOG
\initials{LDF 2004.12.23.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=picture_vector_variable: variable PICTURE_VECTOR@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               PICTURE_VECTOR,
                               parameter);


};

@q **** (4) transform_vector_variable.  @>
@*3 \§transform vector variable>.
\initials{LDF 2004.12.23.}

\LOG
\initials{LDF 2004.12.23.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> transform_vector_variable@>@/

@q *****  (5) transform_vector_variable --> variable TRANSFORM_VECTOR.@>
@*4 \§transform vector variable> $\longrightarrow$ \§variable> 
\.{TRANSFORM\_VECTOR}.

\LOG
\initials{LDF 2004.12.23.}
Added this rule.

\initials{LDF 2005.06.08.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 

@<Define rules@>= 
@=transform_vector_variable: variable TRANSFORM_VECTOR@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               TRANSFORM_VECTOR,
                               parameter);
};

@q **** (4) focus_vector_variable.  @>
@*3 \§focus vector variable>.
\initials{LDF 2004.12.23.}

\LOG
\initials{LDF 2004.12.23.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> focus_vector_variable@>@/

@q *****  (5) focus_vector_variable --> variable FOCUS_VECTOR.@>
@*4 \§focus vector variable> $\longrightarrow$ \§variable> \.{FOCUS\_VECTOR}.

\LOG
\initials{LDF 2004.12.23.}
Added this rule.

\initials{LDF 2005.06.08.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 

@<Define rules@>= 
@=focus_vector_variable: variable FOCUS_VECTOR@>
{

   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               FOCUS_VECTOR,
                               parameter);
};

@q **** (4) macro_vector_variable.  @>
@*3 \§macro vector variable>.
\initials{LDF 2004.12.28.}

\LOG
\initials{LDF 2004.12.28.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> macro_vector_variable@>@/

@q *****  (5) macro_vector_variable --> variable MACRO_VECTOR.@>
@*4 \§macro vector variable> $\longrightarrow$ \§variable> \.{MACRO\_VECTOR}.

\LOG
\initials{LDF 2004.12.28.}
Added this rule.

\initials{LDF 2005.06.08.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 

@<Define rules@>= 
@=macro_vector_variable: variable MACRO_VECTOR@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               MACRO_VECTOR,
                               parameter);
};



@q *** (3) color_vector_variable.  @>
@*2 \§color vector variable>.

\LOG
\initials{LDF 2004.08.23.}  
Added this section.
\ENDLOG 

@q ***  color_vector_variable --> variable COLOR_VECTOR.@>
@ \§color vector variable> $\longrightarrow$ \§variable> \.{COLOR\_VECTOR}.

\LOG
\initials{LDF 2004.08.23.}  
Added this rule.


\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 


@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> color_vector_variable@>@/

@
@<Define rules@>= 
@=color_vector_variable: variable COLOR_VECTOR@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               COLOR_VECTOR,
                               parameter);
};

@q *** (3) point_vector_variable.  @>
@*2 \§point vector variable>.

\LOG
\initials{LDF 2004.08.31.}  
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> point_vector_variable@>@/

@q **** (4) point_vector_variable --> variable POINT_VECTOR.@>
@*3 \§point vector variable> $\longrightarrow$ \§variable> 
\.{POINT\_VECTOR}.
\initials{LDF 2004.08.31.}  

\LOG
\initials{LDF 2004.08.31.}  
Added this rule.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 

@<Define rules@>= 
@=point_vector_variable: variable POINT_VECTOR@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               POINT_VECTOR,
                               parameter);
};

@q *** (3) bool_point_vector_variable.  @>
@*2 \§bool-point vector variable>.

\LOG
\initials{LDF 2004.08.31.}  
Added this section.
\ENDLOG 

@q ***  bool_point_vector_variable --> variable BOOL_POINT_VECTOR.@>
@ \§bool-point vector variable> $\longrightarrow$ \§variable> 
\.{BOOL\_POINT\_VECTOR}.

\LOG
\initials{LDF 2004.09.01.}  
Added this rule.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 


@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> bool_point_vector_variable@>@/

@
@<Define rules@>= 
@=bool_point_vector_variable: variable BOOL_POINT_VECTOR@>
{

   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               BOOL_POINT_VECTOR,
                               parameter);

};

@q *** (3) path_vector_variable.  @>
@*2 \§path vector variable>.

\LOG
\initials{LDF 2004.12.10.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> path_vector_variable@>@/

@q **** (4) path_vector_variable --> variable PATH_VECTOR.@>
@*3 \§path vector variable> $\longrightarrow$ \§variable> 
\.{PATH\_VECTOR}.
\initials{LDF 2004.12.10.}

\LOG
\initials{LDF 2004.12.10.}
Added this rule.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 

@<Define rules@>= 
@=path_vector_variable: variable PATH_VECTOR@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               PATH_VECTOR,
                               parameter);

};

@q **** (4) triangle_vector_variable.  @>
@*3 \§triangle vector variable>.
\initials{LDF 2004.12.23.}

\LOG
\initials{LDF 2004.12.23.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> triangle_vector_variable@>@/

@q *****  (5) triangle_vector_variable --> variable TRIANGLE_VECTOR.@>
@*4 \§triangle vector variable> $\longrightarrow$ \§variable> \.{TRIANGLE\_VECTOR}.

\LOG
\initials{LDF 2004.12.23.}
Added this rule.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 

@<Define rules@>= 
@=triangle_vector_variable: variable TRIANGLE_VECTOR@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               TRIANGLE_VECTOR,
                               parameter);

};

@q **** (4) nurb_vector_variable.  @>
@*3 \§nurb vector variable>.
\initials{LDF 2005.01.26.}

\LOG
\initials{LDF 2005.01.26.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> nurb_vector_variable@>@/

@q *****  (5) nurb_vector_variable --> variable NURB_VECTOR.@>
@*4 \§nurb vector variable> $\longrightarrow$ \§variable> \.{NURB\_VECTOR}.

\LOG
\initials{LDF 2005.01.26.}
Added this rule.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 

@<Define rules@>= 
@=nurb_vector_variable: variable NURB_VECTOR@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               NURB_VECTOR,
                               parameter);

};



@q *** (3) ellipse_vector_variable.  @>
@*2 \§ellipse vector variable>.

\LOG
\initials{LDF 2004.12.14.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> ellipse_vector_variable@>@/

@q **** (4) ellipse_vector_variable --> variable ELLIPSE_VECTOR.@>
@*3 \§ellipse vector variable> $\longrightarrow$ \§variable> 
\.{ELLIPSE\_VECTOR}.
\initials{LDF 2004.12.14.}

\LOG
\initials{LDF 2004.12.14.}
Added this rule.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 

@<Define rules@>= 
@=ellipse_vector_variable: variable ELLIPSE_VECTOR@>
{

   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               ELLIPSE_VECTOR,
                               parameter);

};

@q *** (3) circle_vector_variable.  @>
@*2 \§circle vector variable>.

\LOG
\initials{LDF 2004.12.14.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> circle_vector_variable@>@/

@q **** (4) circle_vector_variable --> variable CIRCLE_VECTOR.@>
@*3 \§circle vector variable> $\longrightarrow$ \§variable> 
\.{CIRCLE\_VECTOR}.
\initials{LDF 2004.12.14.}

\LOG
\initials{LDF 2004.12.14.}
Added this rule.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 

@<Define rules@>= 
@=circle_vector_variable: variable CIRCLE_VECTOR@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               CIRCLE_VECTOR,
                               parameter);

};


@q *** (3) parabola_vector_variable.  @>
@*2 \§parabola vector variable>.
\initials{LDF 2005.11.07.}

\LOG
\initials{LDF 2005.11.07.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> parabola_vector_variable@>@/

@q **** (4) parabola_vector_variable --> variable PARABOLA_VECTOR.@>
@*3 \§parabola vector variable> $\longrightarrow$ \§variable> 
\.{PARABOLA\_VECTOR}.
\initials{LDF 2005.11.07.}

\LOG
\initials{LDF 2005.11.07.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=parabola_vector_variable: variable PARABOLA_VECTOR@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               PARABOLA_VECTOR,
                               parameter);
};

@q *** (3) hyperbola_vector_variable.  @>
@*2 \§hyperbola vector variable>.
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> hyperbola_vector_variable@>@/

@q **** (4) hyperbola_vector_variable --> variable HYPERBOLA_VECTOR.@>
@*3 \§hyperbola vector variable> $\longrightarrow$ \§variable> 
\.{HYPERBOLA\_VECTOR}.
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=hyperbola_vector_variable: variable HYPERBOLA_VECTOR@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               HYPERBOLA_VECTOR,
                               parameter);
};

@q *** (3) arc_vector_variable.  @>
@*2 \§arc vector variable>.
\initials{LDF 2007.10.13.}

\LOG
\initials{LDF 2007.10.13.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> arc_vector_variable@>@/

@q **** (4) arc_vector_variable --> variable ARC_VECTOR.@>
@*3 \§arc vector variable> $\longrightarrow$ \§variable> 
\.{ARC\_VECTOR}.
\initials{LDF 2007.10.13.}

\LOG
\initials{LDF 2007.10.13.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=arc_vector_variable: variable ARC_VECTOR@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               ARC_VECTOR,
                               parameter);
};


@q *** (3) conic_section_lattice_vector_variable.  @>
@*2 \§conic section lattice vector variable>.
\initials{LDF 2007.07.29.}

\LOG
\initials{LDF 2007.07.29.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> conic_section_lattice_vector_variable@>@/

@q **** (4) conic_section_lattice_vector_variable --> variable CONIC_SECTION_LATTICE_VECTOR.@>
@*3 \§conic section lattice vector variable> $\longrightarrow$ \§variable> 
\.{CONIC\_SECTION\_LATTICE\_VECTOR}.
\initials{LDF 2007.07.29.}

\LOG
\initials{LDF 2007.07.29.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=conic_section_lattice_vector_variable: variable CONIC_SECTION_LATTICE_VECTOR@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               CONIC_SECTION_LATTICE_VECTOR,
                               parameter);
};

@q *** (3) helix_vector_variable.  @>
@*2 \§helix vector variable>.
\initials{LDF 2005.05.19.}

\LOG
\initials{LDF 2005.05.19.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> helix_vector_variable@>@/

@q **** (4) helix_vector_variable --> variable HELIX_VECTOR.@>
@*3 \§helix vector variable> $\longrightarrow$ \§variable> 
\.{HELIX\_VECTOR}.
\initials{LDF 2005.05.19.}

\LOG
\initials{LDF 2005.05.19.}
Added this rule.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 

@<Define rules@>= 
@=helix_vector_variable: variable HELIX_VECTOR@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               HELIX_VECTOR,
                               parameter);

};


@q *** (3) rectangle_vector_variable.  @>
@*2 \§rectangle vector variable>.

\LOG
\initials{LDF 2004.12.14.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> rectangle_vector_variable@>@/

@q **** (4) rectangle_vector_variable --> variable RECTANGLE_VECTOR.@>
@*3 \§rectangle vector variable> $\longrightarrow$ \§variable> 
\.{RECTANGLE\_VECTOR}.
\initials{LDF 2004.12.14.}

\LOG
\initials{LDF 2004.12.14.}
Added this rule.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 

@<Define rules@>= 
@=rectangle_vector_variable: variable RECTANGLE_VECTOR@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               RECTANGLE_VECTOR,
                               parameter);
};


@q *** (3) polygon_vector_variable.  @>
@*2 \§polygon vector variable>.
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.02.11.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> polygon_vector_variable@>@/

@q **** (4) polygon_vector_variable --> variable POLYGON_VECTOR.@>
@*3 \§polygon vector variable> $\longrightarrow$ \§variable> 
\.{POLYGON\_VECTOR}.
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.02.11.}
Added this rule.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 

@<Define rules@>= 
@=polygon_vector_variable: variable POLYGON_VECTOR@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               POLYGON_VECTOR,
                               parameter);
};


@q *** (3) reg_polygon_vector_variable.  @>
@*2 \§regular polygon vector variable>.

\LOG
\initials{LDF 2004.12.14.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> reg_polygon_vector_variable@>@/

@q **** (4) reg_polygon_vector_variable --> variable REG_POLYGON_VECTOR.@>
@*3 \§regular polygon vector variable> $\longrightarrow$ \§variable> 
\.{REG\_POLYGON\_VECTOR}.
\initials{LDF 2004.12.14.}

\LOG
\initials{LDF 2004.12.14.}
Added this rule.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 

@<Define rules@>= 
@=reg_polygon_vector_variable: variable REG_POLYGON_VECTOR@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               REG_POLYGON_VECTOR,
                               parameter);
};

@q *** (3) cone_vector_variable.  @>
@*2 \§cone vector variable>.
\initials{LDF 2005.05.24.}

\LOG
\initials{LDF 2005.05.24.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> cone_vector_variable@>@/

@q **** (4) cone_vector_variable --> variable CONE_VECTOR.@>
@*3 \§cone vector variable> $\longrightarrow$ \§variable> 
\.{CONE\_VECTOR}.
\initials{LDF 2005.05.24.}

\LOG
\initials{LDF 2005.05.24.}
Added this rule.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 

@<Define rules@>= 
@=cone_vector_variable: variable CONE_VECTOR@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               CONE_VECTOR,
                               parameter);
};
  
@q *** (3) cylinder_vector_variable.  @>
@*2 \§cylinder vector variable>.
\initials{LDF 2005.05.25.}

\LOG
\initials{LDF 2005.05.25.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> cylinder_vector_variable@>@/

@q **** (4) cylinder_vector_variable --> variable CYLINDER_VECTOR.@>
@*3 \§cylinder vector variable> $\longrightarrow$ \§variable> 
\.{CYLINDER\_VECTOR}.
\initials{LDF 2005.05.25.}

\LOG
\initials{LDF 2005.05.25.}
Added this rule.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 

@<Define rules@>= 
@=cylinder_vector_variable: variable CYLINDER_VECTOR@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               CYLINDER_VECTOR,
                               parameter);

};

@q *** (3) cuboid_vector_variable.  @>
@*2 \§cuboid vector variable>.

\LOG
\initials{LDF 2004.12.14.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> cuboid_vector_variable@>@/

@q **** (4) cuboid_vector_variable --> variable CUBOID_VECTOR.@>
@*3 \§cuboid vector variable> $\longrightarrow$ \§variable> 
\.{CUBOID\_VECTOR}.
\initials{LDF 2004.12.14.}

\LOG
\initials{LDF 2004.12.14.}
Added this rule.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 

@<Define rules@>= 
@=cuboid_vector_variable: variable CUBOID_VECTOR@>
{

   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               CUBOID_VECTOR,
                               parameter);
};

@q **** (4) polyhedron_vector_variable.  @>
@*3 \§polyhedron vector variable>.
\initials{LDF 2004.12.23.}

\LOG
\initials{LDF 2004.12.23.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> polyhedron_vector_variable@>@/

@q *****  (5) polyhedron_vector_variable --> variable POLYHEDRON_VECTOR.@>
@*4 \§polyhedron vector variable> $\longrightarrow$ \§variable> \.{POLYHEDRON\_VECTOR}.

\LOG
\initials{LDF 2004.12.23.}
Added this rule.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 

@<Define rules@>= 
@=polyhedron_vector_variable: variable POLYHEDRON_VECTOR@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               POLYHEDRON_VECTOR,
                               parameter);

};

@q *** (3) ellipse_slice_vector_variable.  @>
@*2 \§ellipse slice vector variable>.
\initials{LDF 2005.03.29.}

\LOG
\initials{LDF 2005.03.29.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> ellipse_slice_vector_variable@>@/

@q **** (4) ellipse_slice_vector_variable --> variable ELLIPSE_SLICE_VECTOR.@>
@*3 \§ellipse slice vector variable> $\longrightarrow$ \§variable> 
\.{ELLIPSE\_SLICE\_VECTOR}.
\initials{LDF 2005.03.29.}

\LOG
\initials{LDF 2005.03.29.}
Added this rule.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 

@<Define rules@>= 
@=ellipse_slice_vector_variable: variable ELLIPSE_SLICE_VECTOR@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               ELLIPSE_SLICE_VECTOR,
                               parameter);
};

@q *** (3) circle_slice_vector_variable.  @>
@*2 \§circle slice vector variable>.
\initials{LDF 2005.03.29.}

\LOG
\initials{LDF 2005.03.29.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> circle_slice_vector_variable@>@/

@q **** (4) circle_slice_vector_variable --> variable CIRCLE_SLICE_VECTOR.@>
@*3 \§circle slice vector variable> $\longrightarrow$ \§variable> 
\.{CIRCLE\_SLICE\_VECTOR}.
\initials{LDF 2005.03.29.}

\LOG
\initials{LDF 2005.03.29.}
Added this rule.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 

@<Define rules@>= 
@=circle_slice_vector_variable: variable CIRCLE_SLICE_VECTOR@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               CIRCLE_SLICE_VECTOR,
                               parameter);
};

@q *** (3) polyhedron_slice_vector_variable.  @>
@*2 \§polyhedron slice vector variable>.
\initials{LDF 2005.03.29.}

\LOG
\initials{LDF 2005.03.29.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> polyhedron_slice_vector_variable@>@/

@q **** (4) polyhedron_slice_vector_variable --> variable POLYHEDRON_SLICE_VECTOR.@>
@*3 \§polyhedron slice vector variable> $\longrightarrow$ \§variable> 
\.{POLYHEDRON\_SLICE\_VECTOR}.
\initials{LDF 2005.03.29.}

\LOG
\initials{LDF 2005.03.29.}
Added this rule.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 

@<Define rules@>= 
@=polyhedron_slice_vector_variable: variable POLYHEDRON_SLICE_VECTOR@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               POLYHEDRON_SLICE_VECTOR,
                               parameter);
};

@q *** (3) ellipsoid_vector_variable.  @>
@*2 \§ellipsoid vector variable>.
\initials{LDF 2005.05.26.}

\LOG
\initials{LDF 2005.05.26.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> ellipsoid_vector_variable@>@/

@q **** (4) ellipsoid_vector_variable --> variable ELLIPSOID_VECTOR.@>
@*3 \§ellipsoid vector variable> $\longrightarrow$ \§variable> 
\.{ELLIPSOID\_VECTOR}.
\initials{LDF 2005.05.26.}

\LOG
\initials{LDF 2005.05.26.}
Added this rule.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 

@<Define rules@>= 
@=ellipsoid_vector_variable: variable ELLIPSOID_VECTOR@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               ELLIPSOID_VECTOR,
                               parameter);
};

@q *** (3) sphere_vector_variable.  @>
@*2 \§sphere vector variable>.
\initials{LDF 2005.06.06.}

\LOG
\initials{LDF 2005.06.06.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> sphere_vector_variable@>@/

@q **** (4) sphere_vector_variable --> variable SPHERE_VECTOR.@>
@*3 \§sphere vector variable> $\longrightarrow$ \§variable> 
\.{SPHERE\_VECTOR}.
\initials{LDF 2005.06.06.}

\LOG
\initials{LDF 2005.06.06.}
Added this rule.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 

@<Define rules@>= 
@=sphere_vector_variable: variable SPHERE_VECTOR@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               SPHERE_VECTOR,
                               parameter);
};


@q *** (3) sphere_development_vector_variable.  @>
@*2 \§sphere development vector variable>.
\initials{LDF 2009.11.09.}

\LOG
\initials{LDF 2009.11.09.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> sphere_development_vector_variable@>@/

@q **** (4) sphere_development_vector_variable --> variable SPHERE_DEVELOPMENT_VECTOR.@>
@*3 \§sphere development vector variable> $\longrightarrow$ \§variable> 
\.{SPHERE_DEVELOPMENT\_VECTOR}.
\initials{LDF 2009.11.09.}

\LOG
\initials{LDF 2009.11.09.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=sphere_development_vector_variable: variable SPHERE_DEVELOPMENT_VECTOR@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               SPHERE_DEVELOPMENT_VECTOR,
                               parameter);
};

@q *** (3) paraboloid_vector_variable.  @>
@*2 \§paraboloid vector variable>.
\initials{LDF 2006.01.23.}

\LOG
\initials{LDF 2006.01.23.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> paraboloid_vector_variable@>@/

@q **** (4) paraboloid_vector_variable --> variable PARABOLOID_VECTOR.@>
@*3 \§paraboloid vector variable> $\longrightarrow$ \§variable> 
\.{PARABOLOID\_VECTOR}.
\initials{LDF 2006.01.23.}

\LOG
\initials{LDF 2006.01.23.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=paraboloid_vector_variable: variable PARABOLOID_VECTOR@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               PARABOLOID_VECTOR,
                               parameter);
};



@q *** (3) glyph_vector_variable.  @>
@*2 \§glyph vector variable>.
\initials{LDF 2005.06.08.}

\LOG
\initials{LDF 2005.06.08.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> glyph_vector_variable@>@/

@q **** (4) glyph_vector_variable --> variable GLYPH_VECTOR.@>
@*3 \§glyph vector variable> $\longrightarrow$ \§variable> 
\.{GLYPH\_VECTOR}.
\initials{LDF 2005.06.08.}

\LOG
\initials{LDF 2005.06.08.}
Added this rule.

\initials{LDF 2005.06.09.}
Now calling |Scan_Parse::variable_decl_func()|
instead of 
|Scan_Parse::glyph_vector_variable_rule_func_0()|.
\ENDLOG 

@<Define rules@>= 
@=glyph_vector_variable: variable GLYPH_VECTOR@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               GLYPH_VECTOR,
                               parameter);

};

@q *** (3) plane_vector_variable.  @>
@*2 \§plane vector variable>.
\initials{LDF 2005.10.30.}

\LOG
\initials{LDF 2005.10.30.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> plane_vector_variable@>@/

@q **** (4) plane_vector_variable --> variable PLANE_VECTOR.@>
@*3 \§plane vector variable> $\longrightarrow$ \§variable> 
\.{PLANE\_VECTOR}.
\initials{LDF 2005.10.30.}

\LOG
\initials{LDF 2005.10.30.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=plane_vector_variable: variable PLANE_VECTOR@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               PLANE_VECTOR,
                               parameter);

};





@q **** (4) origami_figure_vector_variable.  @>
@*3 \§origami figure vector variable>.
\initials{LDF 2005.02.03.}

\LOG
\initials{LDF 2005.02.03.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> origami_figure_vector_variable@>@/

@q *****  (5) origami_figure_vector_variable --> variable ORIGAMI_FIGURE_VECTOR.@>
@*4 \§origami figure vector variable> $\longrightarrow$
\§variable> \.{ORIGAMI\_FIGURE\_VECTOR}.

\LOG
\initials{LDF 2005.02.03.}
Added this rule.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 

@<Define rules@>= 
@=origami_figure_vector_variable: variable ORIGAMI_FIGURE_VECTOR@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               ORIGAMI_FIGURE_VECTOR,
                               parameter);
};

@q *** (3)  vector_type_variable.  @>
@*2 \§vector type variable>.

\LOG
\initials{LDF 2004.08.27.}  
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> vector_type_variable@>@/

@q **** (4) vector_type_variable --> boolean_vector_variable.@>

@*3 \§vector type variable>  $\longrightarrow$ 
\§boolean vector variable>.
\initials{LDF 2004.12.23.}

\LOG
\initials{LDF 2004.12.23.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: boolean_vector_variable@>
{

  @=$$@> = @=$1@>;

};

@q **** (4) vector_type_variable --> string_vector_variable.@>

@*3 \§vector type variable>  $\longrightarrow$ 
\§string vector variable>.
\initials{LDF 2004.12.23.}

\LOG
\initials{LDF 2004.12.23.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: string_vector_variable@>
{

  @=$$@> = @=$1@>;

};

@q **** (4) vector_type_variable --> numeric_vector_variable.@>

@*3 \§vector type variable>  $\longrightarrow$ 
\§numeric vector variable>.
\initials{LDF 2004.12.23.}

\LOG
\initials{LDF 2004.12.23.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: numeric_vector_variable@>
{


  @=$$@> = @=$1@>;

};

@q **** (4) vector_type_variable --> ulong_long_vector_variable.@>

@*3 \§vector type variable>  $\longrightarrow$ 
\§ulong long vector variable>.
\initials{LDF 2005.12.05.}

\LOG
\initials{LDF 2005.12.05.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: ulong_long_variable@>
{
  @=$$@> = @=$1@>;
};

@q **** (4) vector_type_variable --> complex_vector_variable.@>

@*3 \§vector type variable>  $\longrightarrow$ 
\§complex vector variable>.
\initials{LDF 2007.12.02.}

\LOG
\initials{LDF 2007.12.02.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: complex_vector_variable@>
{
  @=$$@> = @=$1@>;
};




@q **** (4) vector_type_variable --> matrix_vector_variable.@>

@*3 \§vector type variable>  $\longrightarrow$ 
\§matrix vector variable>.
\initials{LDF 2007.11.28.}

\LOG
\initials{LDF 2007.11.28.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: matrix_variable@>
{
  @=$$@> = @=$1@>;
};



@q **** (4) vector_type_variable --> pen_vector_variable.@>

@*3 \§vector type variable>  $\longrightarrow$ 
\§pen vector variable>.
\initials{LDF 2004.12.23.}

\LOG
\initials{LDF 2004.12.23.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: pen_vector_variable@>
{

  @=$$@> = @=$1@>;

};

@q **** (4) vector_type_variable --> dash_pattern_vector_variable.@>

@*3 \§vector type variable>  $\longrightarrow$ 
\§dash pattern vector variable>.
\initials{LDF 2004.12.23.}

\LOG
\initials{LDF 2004.12.23.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: dash_pattern_vector_variable@>
{

  @=$$@> = @=$1@>;

};

@q **** (4) vector_type_variable --> color_vector_variable.@>
@*3 \§vector type variable>  $\longrightarrow$ 
\§color vector variable>.

\LOG
\initials{LDF 2004.08.27.}  
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: color_vector_variable@>
{

  @=$$@> = @=$1@>;

};

@q **** (4) vector_type_variable --> picture_vector_variable.@>

@*3 \§vector type variable>  $\longrightarrow$ 
\§picture vector variable>.
\initials{LDF 2004.12.23.}

\LOG
\initials{LDF 2004.12.23.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: picture_vector_variable@>
{

  @=$$@> = @=$1@>;

};

@q **** (4) vector_type_variable --> transform_vector_variable.@>

@*3 \§vector type variable>  $\longrightarrow$ 
\§transform vector variable>.
\initials{LDF 2004.12.23.}

\LOG
\initials{LDF 2004.12.23.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: transform_vector_variable@>
{

  @=$$@> = @=$1@>;

};

@q **** (4) vector_type_variable --> focus_vector_variable.@>

@*3 \§vector type variable>  $\longrightarrow$ 
\§focus vector variable>.
\initials{LDF 2004.12.23.}

\LOG
\initials{LDF 2004.12.23.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: focus_vector_variable@>
{

  @=$$@> = @=$1@>;

};

@q **** (4) vector_type_variable --> macro_vector_variable.@>

@*3 \§vector type variable>  $\longrightarrow$ 
\§macro vector variable>.
\initials{LDF 2004.12.28.}

\LOG
\initials{LDF 2004.12.28.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: macro_vector_variable@>
{

  @=$$@> = @=$1@>;

};



@q **** (4) vector_type_variable --> triangle_vector_variable.@>

@*3 \§vector type variable>  $\longrightarrow$ 
\§triangle vector variable>.
\initials{LDF 2004.12.23.}

\LOG
\initials{LDF 2004.12.23.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: triangle_vector_variable@>
{

  @=$$@> = @=$1@>;

};


@q **** (4) vector_type_variable --> nurb_vector_variable.@>

@*3 \§vector type variable>  $\longrightarrow$ 
\§nurb vector variable>.
\initials{LDF 2005.01.26.}

\LOG
\initials{LDF 2005.01.26.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: nurb_vector_variable@>
{

  @=$$@> = @=$1@>;

};



@q **** (4) vector_type_variable --> point_vector_variable.@>
@*3 \§vector type variable>  $\longrightarrow$ 
\§point vector variable>.

\LOG
\initials{LDF 2004.08.31.}  Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: point_vector_variable@>
{

  @=$$@> = @=$1@>;

};


@q **** (4)  vector_type_variable --> bool_point_vector_variable.@>
@*3 \§vector type variable>  $\longrightarrow$ 
\§bool-point vector variable>.

\LOG
\initials{LDF 2004.09.01.}  
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: bool_point_vector_variable@>
{

  @=$$@> = @=$1@>;

};

@q **** (4)  vector_type_variable --> path_vector_variable.@>
@*3 \§vector type variable>  $\longrightarrow$ 
\§path vector variable>.

\LOG
\initials{LDF 2004.12.10.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: path_vector_variable@>
{


  @=$$@> = @=$1@>;

};

@q **** (4)  vector_type_variable --> ellipse_vector_variable.@>
@*3 \§vector type variable>  $\longrightarrow$ 
\§ellipse vector variable>.

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: ellipse_vector_variable@>
{

  @=$$@> = @=$1@>;

};

@q **** (4)  vector_type_variable --> circle_vector_variable.@>
@*3 \§vector type variable>  $\longrightarrow$ 
\§circle vector variable>.

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: circle_vector_variable@>
{

  @=$$@> = @=$1@>;

};

@q **** (4)  vector_type_variable --> parabola_vector_variable.@>
@*3 \§vector type variable>  $\longrightarrow$ 
\§parabola vector variable>.
\initials{LDF 2005.11.07.}

\LOG
\initials{LDF 2005.11.07.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: parabola_vector_variable@>
{

  @=$$@> = @=$1@>;

};

@q **** (4)  vector_type_variable --> hyperbola_vector_variable.@>
@*3 \§vector type variable>  $\longrightarrow$ 
\§hyperbola vector variable>.
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: hyperbola_vector_variable@>
{

  @=$$@> = @=$1@>;

};

@q **** (4)  vector_type_variable --> arc_vector_variable.@>
@*3 \§vector type variable>  $\longrightarrow$ 
\§arc vector variable>.
\initials{LDF 2007.10.13.}

\LOG
\initials{LDF 2007.10.13.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: arc_vector_variable@>
{

  @=$$@> = @=$1@>;

};

@q **** (4)  vector_type_variable --> conic_section_lattice_vector_variable.@>
@*3 \§vector type variable>  $\longrightarrow$ 
\§conic section lattice vector variable>.
\initials{LDF 2007.07.29.}

\LOG
\initials{LDF 2007.07.29.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: conic_section_lattice_vector_variable@>
{

  @=$$@> = @=$1@>;

};




@q **** (4)  vector_type_variable --> helix_vector_variable.@>
@*3 \§vector type variable>  $\longrightarrow$ 
\§helix vector variable>.
\initials{LDF 2005.05.19.}

\LOG
\initials{LDF 2005.05.19.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: helix_vector_variable@>
{

  @=$$@> = @=$1@>;

};


@q **** (4)  vector_type_variable --> rectangle_vector_variable.@>
@*3 \§vector type variable>  $\longrightarrow$ 
\§rectangle vector variable>.

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: rectangle_vector_variable@>
{

  @=$$@> = @=$1@>;

};


@q **** (4)  vector_type_variable --> polygon_vector_variable.@>
@*3 \§vector type variable>  $\longrightarrow$ 
\§polygon vector variable>.
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.02.11.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: polygon_vector_variable@>
{

   @=$$@> = @=$1@>;

};


@q **** (4)  vector_type_variable --> reg_polygon_vector_variable.@>
@*3 \§vector type variable>  $\longrightarrow$ 
\§regular polygon vector variable>.

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: reg_polygon_vector_variable@>
{

  @=$$@> = @=$1@>;

};

@q **** (4)  vector_type_variable --> cone_vector_variable.@>
@*3 \§vector type variable>  $\longrightarrow$ 
\§cone vector variable>.
\initials{LDF 2005.05.24.}

\LOG
\initials{LDF 2005.05.24.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: cone_vector_variable@>
{


  @=$$@> = @=$1@>;

};

@q **** (4)  vector_type_variable --> cylinder_vector_variable.@>
@*3 \§vector type variable>  $\longrightarrow$ 
\§cylinder vector variable>.
\initials{LDF 2005.05.25.}

\LOG
\initials{LDF 2005.05.25.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: cylinder_vector_variable@>
{

  @=$$@> = @=$1@>;

};



@q **** (4)  vector_type_variable --> cuboid_vector_variable.@>
@*3 \§vector type variable>  $\longrightarrow$ 
\§cuboid vector variable>.

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: cuboid_vector_variable@>
{

  @=$$@> = @=$1@>;

};

@q **** (4) vector_type_variable --> polyhedron_vector_variable.@>

@*3 \§vector type variable>  $\longrightarrow$ 
\§polyhedron vector variable>.
\initials{LDF 2004.12.23.}

\LOG
\initials{LDF 2004.12.23.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: polyhedron_vector_variable@>
{


  @=$$@> = @=$1@>;

};

@q **** (4)  vector_type_variable --> ellipse_slice_vector_variable.@>
@*3 \§vector type variable>  $\longrightarrow$ 
\§ellipse slice vector variable>.
\initials{LDF 2005.03.29.}

\LOG
\initials{LDF 2005.03.29.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: ellipse_slice_vector_variable@>
{

  @=$$@> = @=$1@>;

};

@q **** (4)  vector_type_variable --> circle_slice_vector_variable.@>
@*3 \§vector type variable>  $\longrightarrow$ 
\§circle slice vector variable>.
\initials{LDF 2005.03.29.}

\LOG
\initials{LDF 2005.03.29.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: circle_slice_vector_variable@>
{

  @=$$@> = @=$1@>;

};

@q **** (4)  vector_type_variable --> polyhedron_slice_vector_variable.@>
@*3 \§vector type variable>  $\longrightarrow$ 
\§polyhedron slice vector variable>.
\initials{LDF 2005.03.29.}

\LOG
\initials{LDF 2005.03.29.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: polyhedron_slice_vector_variable@>
{

  @=$$@> = @=$1@>;

};


@q **** (4) vector_type_variable --> ellipsoid_vector_variable.@>

@*3 \§vector type variable>  $\longrightarrow$ 
\§ellipsoid vector variable>.
\initials{LDF 2005.05.26.}

\LOG
\initials{LDF 2005.05.26.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: ellipsoid_vector_variable@>
{

  @=$$@> = @=$1@>;

};

@q **** (4) vector_type_variable --> sphere_vector_variable.@>

@*3 \§vector type variable>  $\longrightarrow$ 
\§sphere vector variable>.
\initials{LDF 2005.06.06.}

\LOG
\initials{LDF 2005.06.06.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: sphere_vector_variable@>
{

  @=$$@> = @=$1@>;

};

@q **** (4) vector_type_variable --> sphere_development_vector_variable.@>

@*3 \§vector type variable>  $\longrightarrow$ 
\§sphere development vector variable>.
\initials{LDF 2009.11.09.}

\LOG
\initials{LDF 2009.11.09.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: sphere_development_vector_variable@>
{

  @=$$@> = @=$1@>;

};

@q **** (4) vector_type_variable --> paraboloid_vector_variable.@>

@*3 \§vector type variable>  $\longrightarrow$ 
\§paraboloid vector variable>.
\initials{LDF 2006.01.23.}

\LOG
\initials{LDF 2006.01.23.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: paraboloid_vector_variable@>
{

  @=$$@> = @=$1@>;

};


@q **** (4) vector_type_variable --> glyph_vector_variable.@>

@*3 \§vector type variable>  $\longrightarrow$ 
\§glyph vector variable>.
\initials{LDF 2005.06.08.}

\LOG
\initials{LDF 2005.06.08.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: glyph_vector_variable@>
{

  @=$$@> = @=$1@>;

};

@q **** (4) vector_type_variable --> plane_vector_variable.@>

@*3 \§vector type variable>  $\longrightarrow$ 
\§plane vector variable>.
\initials{LDF 2005.10.30.}

\LOG
\initials{LDF 2005.10.30.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: plane_vector_variable@>
{

  @=$$@> = @=$1@>;

};



@q **** (4) vector_type_variable --> origami_figure_vector_variable.@>

@*3 \§vector type variable>  $\longrightarrow$ 
\§origami figure vector variable>.
\initials{LDF 2005.02.03.}

\LOG
\initials{LDF 2005.02.03.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=vector_type_variable: origami_figure_vector_variable@>
{

  @=$$@> = @=$1@>;

};

@q *** (3) numeric_variable.  @>
@*2 \§numeric variable>.
\initials{LDF Undated.} 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> numeric_variable@>@/

@q **** (4) numeric_variable --> variable NUMERIC.@>
@*3 \§numeric variable> $\longrightarrow$ \§variable> \.{NUMERIC}.
\initials{LDF Undated.} 

\LOG
\initials{LDF 2004.05.03.}
Changed |variable| to |variable NUMERIC|.  The way
it was before caused reduce/reduce conflicts when I tried to use |variable| in
the rule for |point_primary|.

\initials{LDF 2004.08.20}
Now using |Scan_Parse::variable_type()| instead of
including |@<Common code for variable types@>|.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 

@
@<Define rules@>= 
@=numeric_variable: variable NUMERIC@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               NUMERIC,
                               parameter);

};

@q *** (3) numeric_variable --> variable UNDECLARED.@>
@*2 \§numeric variable> $\longrightarrow$ \§variable> \.{UNDECLARED}.

\LOG
\initials{LDF 2004.05.04.}  
Debugged this rule.

\initials{LDF 2004.05.09.}  
@:BUG FIX@> BUG FIX:  Previously, |array_flag| was set to
|true| for all variables.

\initials{LDF 2004.08.20}
Now using |Scan_Parse::variable_type()| instead of
including |@<Common code for variable types@>|.

\initials{LDF 2004.08.28.}
Now passing |scanner_node| as an argument in the call to 
|Scanner_Type::add_entry()|.

\initials{LDF 2004.08.29.}
Removed the |Scanner_Node| argument in the call to 
|Scanner_Type::add_entry()|.
\ENDLOG 

@
@<Define rules@>=
@=numeric_variable: variable UNDECLARED@>
{

/* !! START HERE.  LDF 2004.08.20.  This rule requires special handling.  */@; 

  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
  if (DEBUG) 
    {
      cerr_strm << thread_name 
                << "*** Parser: numeric_variable --> variable UNDECLARED.";
      
      log_message(cerr_strm); 
      cerr_message(cerr_strm);
      cerr_strm.str(""); 
    }
#endif /* |DEBUG_COMPILE|  */


  bool is_array;

  is_array = (scanner_node->token_real_vector.size() > 0) ? true : false;

#if DEBUG_COMPILE
  if (DEBUG)
    {
      cerr_strm << thread_name 
                << "Parser rule `numeric_variable --> variable UNDECLARED':"
                << endl
                << "$1 == " << @=$1@> << "." << endl
                << "scanner_node->token_real_vector.size() == "
                << scanner_node->token_real_vector.size() << endl;

      for (vector<real>::const_iterator iter
              = scanner_node->token_real_vector.begin();
           iter != scanner_node->token_real_vector.end();
           ++iter)
        {
          cerr_strm << "*iter == " << *iter << endl;
        }
      
      cerr_strm << "is_array == " << is_array << ".";

      log_message(cerr_strm);
      cerr_message(cerr_strm); 
      cerr_strm.str(""); 

    } /* |if (DEBUG)|  */

#endif /* |DEBUG_COMPILE|  */@; 

  entry = scanner_node->add_entry(@=$1@>, NUMERIC,
                                  Id_Map_Entry_Type::REGULAR, 
                                  false, is_array);



  Int_Void_Ptr ivp = variable_type(scanner_node,
                                   entry,
                                   @=$1@>,
                                   NUMERIC);
  if (ivp.i != 0)
    {
      cerr_strm << thread_name 
                << "ERROR! In parser rule "
                << "`numeric_variable --> variable NUMERIC':"
                << endl << "`Scan_Parse::variable_type()' failed."
                << "Setting value of rule to `static_cast<void*>(0)' and "
                << "will try to continue.";

      log_message(cerr_strm); 
      cerr_message(cerr_strm, error_stop_value); 
      cerr_strm.str(""); 
 
      @=$$@> = static_cast<void*>(0); 

   } /* |if (ivp.i != 0)| 
         (|Scan_Parse::variable_type()| failed.)  */


   else /* (|ivp.i == 0|---|Scan_Parse::variable_type()| 
            succeeded.)  */
     {

#if DEBUG_COMPILE
       if (DEBUG)
         {
           cerr_strm << thread_name 
                     << "In parser rule "
                     << "`numeric_variable --> variable NUMERIC':"
                     << endl 
                     << "`Scan_Parse::variable_type()' succeeded.";

           log_message(cerr_strm); 
           cerr_message(cerr_strm); 
           cerr_strm.str(""); 
         } 
#endif /* |DEBUG_COMPILE|  */@;          
                          
       @=$$@> = ivp.v;

     }  /* |else| (|ivp.i == 0|---|Scan_Parse::variable_type()| 
                   succeeded.)  */

@ The following conditional is just for insurance. 
|scanner_node->token_real_vector| is cleared in |resolve_array_name()|,
which is called in |@<Common code for variable types@>|.  However, it doesn't do
any harm to make sure that it's cleared here, and it might prevent problems
that may result from future changes.
\initials{LDF 2004.05.09.}

@<Define rules@>=
  if (scanner_node->token_real_vector.size() > 0)
    scanner_node->token_real_vector.clear();  

};

@q *** (3) ulong long_variable.@>
@*2 \§ulong long variable>.
\initials{LDF 2005.12.05.}

\LOG
\initials{LDF 2005.12.05.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> ulong_long_variable@>@/

@q **** (4) ulong_long_variable --> variable ULONG_LONG.@>
@*3 \§ulong long variable> $\longrightarrow$ \§variable> \.{ULONG\_LONG}.
\initials{LDF 2005.12.05.}

\LOG
\initials{LDF 2005.12.05.}
Added this rule.
\ENDLOG

@
@<Define rules@>= 
@=ulong_long_variable: variable ULONG_LONG@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               ULONG_LONG,
                               parameter);
};

@q *** (3) complex_variable.@>
@*2 \§complex variable>.
\initials{LDF 2007.12.02.}

\LOG
\initials{LDF 2007.12.02.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> complex_variable@>@/

@q **** (4) complex_variable --> variable COMPLEX.@>
@*3 \§complex variable> $\longrightarrow$ \§variable> \.{COMPLEX}.
\initials{LDF 2007.12.02.}

\LOG
\initials{LDF 2007.12.02.}
Added this rule.
\ENDLOG

@
@<Define rules@>= 
@=complex_variable: variable COMPLEX@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               COMPLEX,
                               parameter);
};

@q *** (3) matrix_variable.@>
@*2 \§matrix variable>.
\initials{LDF 2007.11.28.}

\LOG
\initials{LDF 2007.11.28.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> matrix_variable@>@/

@q **** (4) matrix_variable --> variable MATRIX.@>
@*3 \§matrix variable> $\longrightarrow$ \§variable> \.{ULONG\_LONG}.
\initials{LDF 2007.11.28.}

\LOG
\initials{LDF 2007.11.28.}
Added this rule.
\ENDLOG

@
@<Define rules@>= 
@=matrix_variable: variable MATRIX@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               MATRIX,
                               parameter);
};

@q *** (3) transform variable.  @>
@*2 \§transform variable>.

\LOG
\initials{LDF 2004.08.20}
Now using |Scan_Parse::variable_type()| instead of
including |@<Common code for variable types@>|.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> transform_variable@>@/

@
@<Define rules@>= 
@=transform_variable: variable TRANSFORM@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               TRANSFORM,
                               parameter);
};


@q *** (3) picture_variable.@>
@*2 \§picture variable>.
\initials{LDF 2004.05.21.}  

\LOG
\initials{LDF 2004.05.21.}  
Added this type declaration.

\initials{LDF 2004.08.20}
Now using |Scan_Parse::variable_type()| instead of
including |@<Common code for variable types@>|.
\ENDLOG 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> picture_variable@>@/

@q **** (4) picture_variable --> variable PICTURE.@>
@*3 \§picture variable> $\longrightarrow$ \§variable> \.{PICTURE}.
\initials{LDF 2004.05.21.}  

\LOG
\initials{LDF 2004.05.21.}  
Added this rule.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG

@<Define rules@>= 
@=picture_variable: variable PICTURE@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               PICTURE,
                               parameter);
};


@q **** (4)  picture_variable --> LAST picture_vector_variable.@>
@*3 \§picture variable> $\longrightarrow$ \.{LAST} 
\§picture vector variable>. 
\initials{LDF 2005.01.17.}

\LOG
\initials{LDF 2005.01.17.}
Added this rule.
\ENDLOG 

@q ***** (5) Definition.@> 

@<Define rules@>= 
@=picture_variable: LAST picture_vector_variable@>
{
  
    @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
  if (DEBUG) 
    {
      cerr_strm << thread_name 
                << "*** Parser: `picture_variable --> "
                << "LAST picture_vector_variable'.";
      log_message(cerr_strm); 
      cerr_message(cerr_strm); 
      cerr_strm.str(""); 
    }
#endif /* |DEBUG_COMPILE|  */

   entry = static_cast<Id_Map_Entry_Node>(@=$2@>);

@q ****** (6) Error handling:  |entry == 0 || entry->object == 0|.@> 

@ Error handling:  |entry == 0 || entry->object == 0|.
\initials{LDF 2005.01.17.}

@<Define rules@>=

   if (entry == static_cast<Id_Map_Entry_Node>(0) || entry->object == static_cast<void*>(0))
      {
          cerr_strm << thread_name 
                    << "ERROR!  In `yyparse()', rule "
                    << endl 
                    << "`picture_variable "
                    << "--> LAST picture_vector_variable':"
                    << endl << "Invalid `picture_vector_variable'.  "
                    << "Setting `picture_variable' to 0"
                    << endl << "and will try to continue.";

          log_message(cerr_strm);
          cerr_message(cerr_strm, error_stop_value);
          cerr_strm.str("");


          @=$$@> = static_cast<void*>(0);

      }  /* |if (entry == 0 || entry->object == 0)|  */

@q ****** (6).@> 

   else /* |entry != 0 && entry->object != 0|  */
      {

          Pointer_Vector<Picture>* pv 
              = static_cast<Pointer_Vector<Picture>*>(entry->object);

@q ******* (7) @> 

          if (pv->ctr <= 0)
             {
                 cerr_strm << thread_name 
                           << "ERROR!  In `yyparse()', rule "
                           << endl 
                           << "`picture_variable "
                           << "--> LAST picture_vector_variable':"
                           << endl << "`picture_vector_variable' is empty.  "
                           << "Setting `picture_variable' to 0"
                           << endl << "and will try to continue.";

                 log_message(cerr_strm);
                 cerr_message(cerr_strm, error_stop_value);
                 cerr_strm.str("");

                 @=$$@> = static_cast<void*>(0);                


             }  /* |if (pv->ctr <= 0)|  */

@q ******* (7) @> 

        else /* |pv->ctr > 0|  */
           {


              stringstream name_strm;

              name_strm << entry->name << "[" << pv->ctr-1 << "]";
  
              Id_Map_Entry_Node array_entry 
                 = scanner_node->get_array_entry(name_strm.str(),
                                                 entry->subordinate_array,
                                                 false);
             
@q ******** (8) @> 

              if (array_entry == static_cast<Id_Map_Entry_Node>(0))
                 {
                     cerr_strm << thread_name 
                               << "ERROR!  In `yyparse()', rule "
                               << endl 
                               << "`picture_variable "
                               << "--> LAST picture_vector_variable':"
                               << endl << "`" << name_strm.str() << "' "
                               << "not found."
                               << endl 
                               << "Setting `picture_variable' to 0"
                               << endl << "and will try to continue.";

                     log_message(cerr_strm);
                     cerr_message(cerr_strm, error_stop_value);
                     cerr_strm.str("");

                     @=$$@> = static_cast<void*>(0);                                 


                 }  /* |if (array_entry == 0)|  */

@q ******** (8) @> 

              else  /* |array_entry != 0|  */
                 @=$$@> = static_cast<void*>(array_entry); 

@q ******** (8) @> 

           }  /* |else| (|pv->ctr > 0|)  */


      }  /* |else| (|entry != 0 && entry->object != 0|)  */
};


@q ***  (3) point variable.@>
@*2 \§point variable>.
\initials{LDF Undated.}

\LOG
\initials{LDF 2004.08.20}
Now using |Scan_Parse::variable_type()| instead of
including |@<Common code for variable types@>|.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> point_variable@>@/

@
@<Define rules@>= 
@=point_variable: variable POINT@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               POINT,
                               parameter);
};

@q *** (3) focus_variable.@>
@*2 \§focus variable>.
\initials{LDF 2004.06.07.}  

\LOG
\initials{LDF 2004.06.07.}  
Added this type declaration.
\ENDLOG 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> focus_variable@>@/

@q **** (4)  focus_variable --> variable FOCUS.  @>
@*3 \§focus variable> $\longrightarrow$ \§variable> \.{FOCUS}.
\initials{LDF 2004.06.07.}  

\LOG
\initials{LDF 2004.06.07.}  
Added this rule.

\initials{LDF 2004.08.20}
Now using |Scan_Parse::variable_type()| instead of
including |@<Common code for variable types@>|.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 

@<Define rules@>= 
@=focus_variable: variable FOCUS@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               FOCUS,
                               parameter);
};

@q *** (3) macro_variable.@>
@*2 \§macro variable>.
\initials{LDF 2004.12.28.}

\LOG
\initials{LDF 2004.12.28.}
Added this type declaration.
\ENDLOG 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> macro_variable@>@/

@q **** (4)  macro_variable --> variable MACRO.  @>
@*3 \§macro variable> $\longrightarrow$ \§variable> \.{MACRO}.
\initials{LDF 2004.12.28.}

\LOG
\initials{LDF 2004.12.28.}
Added this rule.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 

@<Define rules@>= 
@=macro_variable: variable MACRO@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               MACRO,
                               parameter);

};

@q **** (4)  macro_variable --> LAST macro_vector_variable.@>
@*3 \§macro variable> $\longrightarrow$ \.{LAST} 
\§macro vector variable>. 
\initials{LDF 2005.01.08.}

\LOG
\initials{LDF 2005.01.08.}
Added this rule.
\ENDLOG 

@q ***** (5) Definition.@> 

@<Define rules@>= 
@=macro_variable: LAST macro_vector_variable@>
{
  
    @<Common declarations for rules@>@;

#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
  if (DEBUG) 
    {
      cerr_strm << thread_name 
                << "*** Parser: `macro_variable --> "
                << "LAST macro_vector_variable'.";
      log_message(cerr_strm); 
      cerr_message(cerr_strm); 
      cerr_strm.str(""); 
    }
#endif /* |DEBUG_COMPILE|  */

   entry = static_cast<Id_Map_Entry_Node>(@=$2@>);

@q ****** (6) Error handling:  |entry == 0 || entry->object == 0|.@> 

@ Error handling:  |entry == 0 || entry->object == 0|.
\initials{LDF 2005.01.08.}

@<Define rules@>=

   if (entry == static_cast<Id_Map_Entry_Node>(0) || entry->object == static_cast<void*>(0))
      {
          cerr_strm << thread_name 
                    << "ERROR!  In `yyparse()', rule "
                    << endl 
                    << "`macro_variable "
                    << "--> LAST macro_vector_variable':"
                    << endl << "Invalid `macro_vector_variable'.  "
                    << "Setting `macro_variable' to 0"
                    << endl << "and will try to continue.";

          log_message(cerr_strm);
          cerr_message(cerr_strm, error_stop_value);
          cerr_strm.str("");


          @=$$@> = static_cast<void*>(0);

      }  /* |if (entry == 0 || entry->object == 0)|  */

@q ****** (6).@> 

   else /* |entry != 0 && entry->object != 0|  */
      {

          Pointer_Vector<Definition_Info_Type>* pv 
              = static_cast<Pointer_Vector<Definition_Info_Type>*>(entry->object);

@q ******* (7) @> 

          if (pv->ctr <= 0)
             {
                 cerr_strm << thread_name 
                           << "ERROR!  In `yyparse()', rule "
                           << endl 
                           << "`macro_variable "
                           << "--> LAST macro_vector_variable':"
                           << endl << "`macro_vector_variable' is empty.  "
                           << "Setting `macro_variable' to 0"
                           << endl << "and will try to continue.";

                 log_message(cerr_strm);
                 cerr_message(cerr_strm, error_stop_value);
                 cerr_strm.str("");

                 @=$$@> = static_cast<void*>(0);                


             }  /* |if (pv->ctr <= 0)|  */

@q ******* (7) @> 

        else /* |pv->ctr > 0|  */
           {


              stringstream name_strm;

              name_strm << entry->name << "[" << pv->ctr-1 << "]";
  
              Id_Map_Entry_Node array_entry 
                 = scanner_node->get_array_entry(name_strm.str(),
                                                 entry->subordinate_array,
                                                 false);
             
@q ******** (8) @> 

              if (array_entry == static_cast<Id_Map_Entry_Node>(0))
                 {
                     cerr_strm << thread_name 
                               << "ERROR!  In `yyparse()', rule "
                               << endl 
                               << "`macro_variable "
                               << "--> LAST macro_vector_variable':"
                               << endl << "`" << name_strm.str() << "' "
                               << "not found."
                               << endl 
                               << "Setting `macro_variable' to 0"
                               << endl << "and will try to continue.";

                     log_message(cerr_strm);
                     cerr_message(cerr_strm, error_stop_value);
                     cerr_strm.str("");

                     @=$$@> = static_cast<void*>(0);                                 


                 }  /* |if (array_entry == 0)|  */

@q ******** (8) @> 

              else  /* |array_entry != 0|  */
                 @=$$@> = static_cast<void*>(array_entry); 

@q ******** (8) @> 

           }  /* |else| (|pv->ctr > 0|)  */


      }  /* |else| (|entry != 0 && entry->object != 0|)  */


 
};

@q *** (3) path variable.  @>
@*2 \§path variable>.

\LOG
\initials{LDF 2004.08.20}
Now using |Scan_Parse::variable_type()| instead of
including |@<Common code for variable types@>|.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> path_variable@>@/

@
@<Define rules@>= 
@=path_variable: variable PATH@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               PATH,
                               parameter);

};

@q *** (3) ellipse variable --> variable ELLIPSE.  @>
@ \§ellipse variable> $\longrightarrow$ \§variable> 
`\.{ellipse}'.

@
\LOG
\initials{LDF 2004.06.29.}
Added this rule.

\initials{LDF 2004.08.20}
Now using |Scan_Parse::variable_type()| instead of
including |@<Common code for variable types@>|.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> ellipse_variable@>@/

@
@<Define rules@>= 
@=ellipse_variable: variable ELLIPSE@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               ELLIPSE,
                               parameter);
};

@q *** (3) circle variable --> variable CIRCLE.  @>
@*2 \§circle variable> $\longrightarrow$ \§variable> 
`\.{circle}'.

\LOG
\initials{LDF 2004.06.17.}
Added this rule.

\initials{LDF 2004.08.20}
Now using |Scan_Parse::variable_type()| instead of
including |@<Common code for variable types@>|.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> circle_variable@>@/

@
@<Define rules@>= 
@=circle_variable: variable CIRCLE@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               CIRCLE,
                               parameter);
};


@q *** (3) parabola variable --> variable PARABOLA.@>
@*2 \§parabola variable> $\longrightarrow$ \§variable> 
\.{PARABOLA}.
\initials{LDF 2005.11.07.}

\LOG
\initials{LDF 2005.11.07.}
Added this rule.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> parabola_variable@>@/

@
@<Define rules@>= 
@=parabola_variable: variable PARABOLA@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               PARABOLA,
                               parameter);
};

@q *** (3) hyperbola variable --> variable HYPERBOLA.@>
@*2 \§hyperbola variable> $\longrightarrow$ \§variable> 
\.{HYPERBOLA}.
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Added this rule.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> hyperbola_variable@>@/

@
@<Define rules@>= 
@=hyperbola_variable: variable HYPERBOLA@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               HYPERBOLA,
                               parameter);
};

@q *** (3) conic_section_lattice variable --> variable CONIC_SECTION_LATTICE.@>
@*2 \§conic section lattice variable> $\longrightarrow$ \§variable> 
\.{CONIC\_SECTION\_LATTICE}.
\initials{LDF 2007.07.29.}

\LOG
\initials{LDF 2007.07.29.}
Added this rule.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> conic_section_lattice_variable@>@/

@
@<Define rules@>= 
@=conic_section_lattice_variable: variable CONIC_SECTION_LATTICE@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               CONIC_SECTION_LATTICE,
                               parameter);
};

@q *** (3) arc variable --> variable ARC.@>
@*2 \§arc variable> $\longrightarrow$ \§variable> 
\.{ARC}.
\initials{LDF 2007.10.13.}

\LOG
\initials{LDF 2007.10.13.}
Added this rule.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> arc_variable@>@/

@
@<Define rules@>= 
@=arc_variable: variable ARC@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               ARC,
                               parameter);
};



@q *** (3) helix variable --> variable HELIX.@>
@*2 \§helix variable> $\longrightarrow$ \§variable> 
\.{HELIX}.
\initials{LDF 2005.05.19.}

\LOG
\initials{LDF 2005.05.19.}
Added this rule.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> helix_variable@>@/

@
@<Define rules@>= 
@=helix_variable: variable HELIX@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               HELIX,
                               parameter);
};

@q ***  (3) rectangle variable --> variable RECTANGLE.  @>
@*2 \§rectangle variable> $\longrightarrow$ \§variable> 
`\.{rectangle}'.

@
\LOG
\initials{LDF 2004.06.30.}
Added this rule.

\initials{LDF 2004.08.20}
Now using |Scan_Parse::variable_type()| instead of
including |@<Common code for variable types@>|.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> rectangle_variable@>@/

@
@<Define rules@>= 
@=rectangle_variable: variable RECTANGLE@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               RECTANGLE,
                               parameter);
};

@q *** (3)  polygon variable --> variable POLYGON.  @>
@*2 \§polygon variable> $\longrightarrow$ \§variable> 
\.{POLYGON}.
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.02.11.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> polygon_variable@>@/

@
\LOG
\initials{LDF 2005.02.11.}
Added this rule.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 

@<Define rules@>= 
@=polygon_variable: variable POLYGON@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               POLYGON,
                               parameter);
};



@q *** (3)  reg_polygon variable --> variable REG_POLYGON.  @>
@*1 \§regular polygon variable> $\longrightarrow$ \§variable> 
\.{REG\_POLYGON}.

@
\LOG
\initials{LDF 2004.07.06.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> reg_polygon_variable@>@/

@
\LOG
\initials{LDF 2004.07.06.}
Added this rule.

\initials{LDF 2004.08.20}
Now using |Scan_Parse::variable_type()| instead of
including |@<Common code for variable types@>|.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 

@<Define rules@>= 
@=reg_polygon_variable: variable REG_POLYGON@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               REG_POLYGON,
                               parameter);

};

@q *** (3)  triangle variable --> variable TRIANGLE.@>
@*1 \§triangle variable> $\longrightarrow$ \§variable> 
\.{TRIANGLE}.
\initials{LDF 2005.01.20.}

@
\LOG
\initials{LDF 2005.01.20.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> triangle_variable@>@/

@
\LOG
\initials{LDF 2005.01.20.}
Added this rule.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 

@<Define rules@>= 
@=triangle_variable: variable TRIANGLE@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               TRIANGLE,
                               parameter);
};


@q *** (3)  nurb variable --> variable NURB.@>
@*1 \§nurb variable> $\longrightarrow$ \§variable> 
\.{NURB}.
\initials{LDF 2005.01.26.}

@
\LOG
\initials{LDF 2005.01.26.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> nurb_variable@>@/

@
\LOG
\initials{LDF 2005.01.26.}
Added this rule.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 

@<Define rules@>= 
@=nurb_variable: variable NURB@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               NURB,
                               parameter);
};


@q *** (3) cone variable --> variable CONE.  @>
@*2 \§cone variable> $\longrightarrow$ \§variable> 
\.{CONE}.
\initials{LDF 2005.05.24.}

\LOG
\initials{LDF 2005.05.24.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> cone_variable@>@/

@
\LOG
\initials{LDF 2005.05.24.}
Added this rule.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 

@<Define rules@>= 
@=cone_variable: variable CONE@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               CONE,
                               parameter);
};

@q *** (3) cylinder variable --> variable CYLINDER.  @>
@*2 \§cylinder variable> $\longrightarrow$ \§variable> 
\.{CYLINDER}.
\initials{LDF 2005.05.25.}

\LOG
\initials{LDF 2005.05.25.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> cylinder_variable@>@/

@
\LOG
\initials{LDF 2005.05.25.}
Added this rule.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 

@<Define rules@>= 
@=cylinder_variable: variable CYLINDER@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               CYLINDER,
                               parameter);
};


@q *** (3) cuboid variable --> variable CUBOID.  @>
@*2 \§cuboid variable> $\longrightarrow$ \§variable> 
\.{CUBOID}.

@
\LOG
\initials{LDF 2004.08.17.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> cuboid_variable@>@/

@
\LOG
\initials{LDF 2004.08.17.}
Added this rule.

\initials{LDF 2004.08.20}
Now using |Scan_Parse::variable_type()| instead of
including |@<Common code for variable types@>|.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG 

@<Define rules@>= 
@=cuboid_variable: variable CUBOID@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               CUBOID,
                               parameter);
};



@q ***  polyhedron variable --> variable POLYHEDRON.  @>
@ \§polyhedron variable> $\longrightarrow$ \§variable> 
`\.{POLYHEDRON}'.

@
\LOG
\initials{LDF 2004.08.31.}
Added this rule.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> polyhedron_variable@>@/

@
@<Define rules@>= 
@=polyhedron_variable: variable POLYHEDRON@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               POLYHEDRON,
                               parameter);
};

@q *** (3) ellipse_slice variable.@>
@*2 \§ellipse slice variable>.
\initials{LDF 2005.03.29.}

\LOG
\initials{LDF 2005.03.29.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> ellipse_slice_variable@>@/


@q ****  (4) ellipse_slice variable --> variable ELLIPSE_SLICE.@>
@*3 \§ellipse slice variable> $\longrightarrow$ \§variable> 
\.{ELLIPSE\_SLICE}.
\initials{LDF 2005.03.29.}

\LOG
\initials{LDF 2005.03.29.}
Added this rule.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG

@<Define rules@>= 
@=ellipse_slice_variable: variable ELLIPSE_SLICE@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               ELLIPSE_SLICE,
                               parameter);
};


@q *** (3) circle_slice variable.@>
@*2 \§circle slice variable>.
\initials{LDF 2005.03.29.}

\LOG
\initials{LDF 2005.03.29.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> circle_slice_variable@>@/


@q ****  (4) circle_slice variable --> variable CIRCLE_SLICE.@>
@*3 \§circle slice variable> $\longrightarrow$ \§variable> 
\.{CIRCLE\_SLICE}.
\initials{LDF 2005.03.29.}

\LOG
\initials{LDF 2005.03.29.}
Added this rule.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG

@<Define rules@>= 
@=circle_slice_variable: variable CIRCLE_SLICE@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               CIRCLE_SLICE,
                               parameter);
};

@q ***  (3) polyhedron_slice variable.@>
@*2 \§polyhedron slice variable>.
\initials{LDF 2005.03.29.}

\LOG
\initials{LDF 2005.03.29.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> polyhedron_slice_variable@>@/


@q ****  (4) polyhedron_slice variable --> variable POLYHEDRON_SLICE.@>
@*3 \§polyhedron slice variable> $\longrightarrow$ \§variable> 
\.{POLYHEDRON\_SLICE}.
\initials{LDF 2005.03.29.}

\LOG
\initials{LDF 2005.03.29.}
Added this rule.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG

@<Define rules@>= 
@=polyhedron_slice_variable: variable POLYHEDRON_SLICE@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               POLYHEDRON_SLICE,
                               parameter);
};

@q *** (3) ellipsoid variable --> variable ELLIPSOID.  @>
@*2 \§ellipsoid variable> $\longrightarrow$ \§variable> 
\.{ELLIPSOID}.
\initials{LDF 2005.05.26.}

\LOG
\initials{LDF 2005.05.26.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> ellipsoid_variable@>@/

@
\LOG
\initials{LDF 2005.05.26.}
Added this rule.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG

@<Define rules@>= 
@=ellipsoid_variable: variable ELLIPSOID@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               ELLIPSOID,
                               parameter);
};

@q *** (3) sphere variable --> variable SPHERE.  @>
@*2 \§sphere variable> $\longrightarrow$ \§variable> 
\.{SPHERE}.
\initials{LDF 2005.06.06.}

\LOG
\initials{LDF 2005.06.06.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> sphere_variable@>@/

@
\LOG
\initials{LDF 2005.06.06.}
Added this rule.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG

@<Define rules@>= 
@=sphere_variable: variable SPHERE@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               SPHERE,
                               parameter);
};

@q *** (3) sphere_development variable --> variable SPHERE_DEVELOPMENT.  @>
@*2 \§sphere development variable> $\longrightarrow$ \§variable> 
\.{SPHERE_DEVELOPMENT}.
\initials{LDF 2009.11.09.}

\LOG
\initials{LDF 2009.11.09.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> sphere_development_variable@>@/

@
\LOG
\initials{LDF 2009.11.09.}
Added this rule.
\ENDLOG

@<Define rules@>= 
@=sphere_development_variable: variable SPHERE_DEVELOPMENT@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               SPHERE_DEVELOPMENT,
                               parameter);
};

@q *** (3) paraboloid variable --> variable PARABOLOID.  @>
@*2 \§paraboloid variable> $\longrightarrow$ \§variable> 
\.{PARABOLOID}.
\initials{LDF 2006.01.23.}

\LOG
\initials{LDF 2006.01.23.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> paraboloid_variable@>@/

@
\LOG
\initials{LDF 2006.01.23.}
Added this rule.
\ENDLOG

@<Define rules@>= 
@=paraboloid_variable: variable PARABOLOID@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               PARABOLOID,
                               parameter);
};


@q *** (3) glyph variable --> variable GLYPH.  @>
@*2 \§glyph variable> $\longrightarrow$ \§variable> 
\.{GLYPH}.
\initials{LDF 2005.06.08.}

\LOG
\initials{LDF 2005.06.08.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> glyph_variable@>@/

@
\LOG
\initials{LDF 2005.06.08.}
Added this rule.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG

@<Define rules@>= 
@=glyph_variable: variable GLYPH@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               GLYPH,
                               parameter);
};

@q *** (3) plane variable --> variable PLANE.  @>
@*2 \§plane variable> $\longrightarrow$ \§variable> 
\.{PLANE}.
\initials{LDF 2005.10.30.}

\LOG
\initials{LDF 2005.10.30.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> plane_variable@>@/

@
\LOG
\initials{LDF 2005.10.30.}
Added this rule.
\ENDLOG

@<Define rules@>= 
@=plane_variable: variable PLANE@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               PLANE,
                               parameter);
};

@q *** (3) origami_figure variable --> variable ORIGAMI_FIGURE.  @>
@*2 \§origami figure variable> $\longrightarrow$ \§variable> 
\.{ORIGAMI\_FIGURE}.
\initials{LDF 2005.02.03.}

\LOG
\initials{LDF 2005.02.03.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> origami_figure_variable@>@/

@
\LOG
\initials{LDF 2005.02.03.}
Added this rule.

\initials{LDF 2005.06.09.}
Removed code from this rule.  Now calling 
|Scan_Parse::variable_decl_func()|.
\ENDLOG

@<Define rules@>= 
@=origami_figure_variable: variable ORIGAMI_FIGURE@>
{
   @=$$@> = variable_decl_func(@=$1@>, 
                               @=$2@>, 
                               ORIGAMI_FIGURE,
                               parameter);
};




@q *** (3) any_variable.  @>
@*2 \§any variable>.

\LOG
\initials{LDF 2004.05.17.}  
Added this type declaration.
\ENDLOG 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> any_variable@>

@q **** (4) Non-vector types.@>   
@*3 Non-vector types.
\initials{LDF 2004.12.23.}

\LOG
\initials{LDF 2004.12.23.}
Added this section.
\ENDLOG

@q *** (3) any_variable --> boolean_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§boolean variable>.

\LOG
\initials{LDF 2004.05.17.}  
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: boolean_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3) any_variable --> bool_point_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§bool-point variable>.

\LOG
\initials{LDF 2004.09.01.}  
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: bool_point_variable@>@/
{


  @=$$@> = @=$1@>;

};



@q *** (3) any_variable --> string_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§string variable>.

\LOG
\initials{LDF 2004.05.17.}  Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: string_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3)  any_variable --> numeric_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§numeric variable>.
\initials{LDF 2004.05.17.}  

\LOG
\initials{LDF 2004.05.17.}  
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: numeric_variable@>@/
{
  @=$$@> = @=$1@>;

};

@q *** (3) any_variable --> ulong_long_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§ulong long variable>.
\initials{LDF 2005.12.05.}

\LOG
\initials{LDF 2005.12.05.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: ulong_long_variable@>@/
{
  @=$$@> = @=$1@>;

};

@q *** (3) any_variable --> complex_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§complex variable>.
\initials{LDF 2007.12.02.}

\LOG
\initials{LDF 2007.12.02.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: complex_variable@>@/
{
  @=$$@> = @=$1@>;

};



@q *** (3) any_variable --> matrix_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§matrix variable>.
\initials{LDF 2007.11.28.}

\LOG
\initials{LDF 2007.11.28.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: matrix_variable@>@/
{
  @=$$@> = @=$1@>;

};

@q *** (3)  any_variable --> pen_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§pen variable>.

\LOG
\initials{LDF 2004.05.21.}  
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: pen_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3)  any_variable --> dash_pattern_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§dash pattern variable>.

\LOG
\initials{LDF 2004.06.07.}  Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: dash_pattern_variable@>@/
{

  @=$$@> = @=$1@>;

};


@q ***  any_variable --> color_variable.  @>
@ \§any variable> $\longrightarrow$ \§color variable>.

\LOG
\initials{LDF 2004.05.25.}  Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: color_variable@>@/
{

  @=$$@> = @=$1@>;

};





@q *** (3) any_variable --> transform_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§transform variable>.

\LOG
\initials{LDF 2004.05.17.}  Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: transform_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q ***  any_variable --> picture_variable.  @>
@ \§any variable> $\longrightarrow$ \§picture variable>.

\LOG
\initials{LDF 2004.05.21.}  Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: picture_variable@>@/
{

  @=$$@> = @=$1@>;

};



@q ***  any_variable --> point_variable.  @>
@ \§any variable> $\longrightarrow$ \§point variable>.

\LOG
\initials{LDF 2004.05.17.}  Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: point_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3) any_variable --> focus_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§focus variable>.
\initials{LDF 2004.06.07.}  

\LOG
\initials{LDF 2004.06.07.}  
Added this rule.

\initials{LDF 2004.12.28.}
Made the debugging output thread-safe.
\ENDLOG 

@<Define rules@>=
@=any_variable: focus_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3) any_variable --> macro_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§macro variable>.
\initials{LDF 2004.12.28.}

\LOG
\initials{LDF 2004.12.28.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: macro_variable@>@/
{

  @=$$@> = @=$1@>;

};


@q ***  any_variable --> path_variable.  @>
@ \§any variable> $\longrightarrow$ \§path variable>.

\LOG
\initials{LDF 2004.05.17.}  Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: path_variable@>@/
{

  @=$$@> = @=$1@>;

};


@q *** (3) any_variable --> ellipse_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§ellipse variable>.
\initials{LDF 2004.06.29.}  

\LOG
\initials{LDF 2004.06.29.}  
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: ellipse_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3) any_variable --> circle_variable.@>
@*2 \§any variable> $\longrightarrow$ \§circle variable>.
\initials{LDF 2004.06.17.}  

\LOG
\initials{LDF 2004.06.17.}  
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: circle_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3) any_variable --> parabola_variable.@>
@*2 \§any variable> $\longrightarrow$ \§parabola variable>.
\initials{LDF 2005.11.07.}

\LOG
\initials{LDF 2005.11.07.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: parabola_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3) any_variable --> hyperbola_variable.@>
@*2 \§any variable> $\longrightarrow$ \§hyperbola variable>.
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: hyperbola_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3) any_variable --> conic_section_lattice_variable.@>
@*2 \§any variable> $\longrightarrow$ \§conic section lattice variable>.
\initials{LDF 2007.07.29.}

\LOG
\initials{LDF 2007.07.29.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: conic_section_lattice_variable@>@/
{

  @=$$@> = @=$1@>;

};


@q *** (3) any_variable --> helix_variable.@>
@*2 \§any variable> $\longrightarrow$ \§helix variable>.
\initials{LDF 2005.05.19.}

\LOG
\initials{LDF 2005.05.19.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: helix_variable@>@/
{

  @=$$@> = @=$1@>;

};



@q *** (3) any_variable --> rectangle_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§rectangle variable>.

\LOG
\initials{LDF 2004.06.30.}  Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: rectangle_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3)  any_variable --> polygon_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§polygon variable>.
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.02.11.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: polygon_variable@>@/
{
   @=$$@> = @=$1@>;

};



@q *** (3)  any_variable --> reg_polygon_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§regular polygon variable>.
\initials{LDF 2004.07.06.}  

\LOG
\initials{LDF 2004.07.06.}  
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: reg_polygon_variable@>@/
{

   @=$$@> = @=$1@>;

};

@q *** (3)  any_variable --> triangle_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§triangle variable>.
\initials{LDF 2005.01.20.}

\LOG
\initials{LDF 2005.01.20.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: triangle_variable@>@/
{

   @=$$@> = @=$1@>;

};

@q *** (3)  any_variable --> nurb_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§nurb variable>.
\initials{LDF 2005.01.26.}

\LOG
\initials{LDF 2005.01.26.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: nurb_variable@>@/
{

   @=$$@> = @=$1@>;

};


@q *** (3) any_variable --> cone_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§cone variable>.
\initials{LDF 2005.05.24.}

\LOG
\initials{LDF 2005.05.24.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: cone_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3) any_variable --> cylinder_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§cylinder variable>.
\initials{LDF 2005.05.25.}

\LOG
\initials{LDF 2005.05.25.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: cylinder_variable@>@/
{

   @=$$@> = @=$1@>;

};


@q *** (3) any_variable --> cuboid_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§cuboid variable>.
\initials{LDF 2004.08.17.}  

\LOG
\initials{LDF 2004.08.17.}  
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: cuboid_variable@>@/
{

  @=$$@> = @=$1@>;

};


@q ***  any_variable --> polyhedron_variable.  @>
@ \§any variable> $\longrightarrow$ \§polyhedron variable>.

\LOG
\initials{LDF 2004.08.31.}  Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: polyhedron_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3) any_variable --> ellipse_slice_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§ellipse slice variable>.
\initials{LDF 2005.03.29.}

\LOG
\initials{LDF 2005.03.29.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: ellipse_slice_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3) any_variable --> circle_slice_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§circle slice variable>.
\initials{LDF 2005.03.29.}

\LOG
\initials{LDF 2005.03.29.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: circle_slice_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3) any_variable --> polyhedron_slice_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§polyhedron slice variable>.
\initials{LDF 2005.03.29.}

\LOG
\initials{LDF 2005.03.29.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: polyhedron_slice_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3) any_variable --> ellipsoid_variable.  @>
@*2 \§any variable> $\longrightarrow$ 
\§ellipsoid variable>.
\initials{LDF 2005.05.26.}

\LOG
\initials{LDF 2005.05.26.}
Added this rule.
\ENDLOG 

@<Define rules@>=

@=any_variable: ellipsoid_variable@>@/
{

   @=$$@> = @=$1@>;

};

@q *** (3) any_variable --> sphere_variable.  @>
@*2 \§any variable> $\longrightarrow$ 
\§sphere variable>.
\initials{LDF 2005.06.06.}

\LOG
\initials{LDF 2005.06.06.}
Added this rule.
\ENDLOG 

@<Define rules@>=

@=any_variable: sphere_variable@>@/
{

   @=$$@> = @=$1@>;

};

@q *** (3) any_variable --> sphere_development_variable.  @>
@*2 \§any variable> $\longrightarrow$ 
\§sphere_development variable>.
\initials{LDF 2009.11.09.}

\LOG
\initials{LDF 2009.11.09.}
Added this rule.
\ENDLOG 

@<Define rules@>=

@=any_variable: sphere_development_variable@>@/
{

   @=$$@> = @=$1@>;

};

@q *** (3) any_variable --> paraboloid_variable.  @>
@*2 \§any variable> $\longrightarrow$ 
\§paraboloid variable>.
\initials{LDF 2006.01.23.}

\LOG
\initials{LDF 2006.01.23.}
Added this rule.
\ENDLOG 

@<Define rules@>=

@=any_variable: paraboloid_variable@>@/
{

   @=$$@> = @=$1@>;

};


@q *** (3) any_variable --> glyph_variable.  @>
@*2 \§any variable> $\longrightarrow$ 
\§glyph variable>.
\initials{LDF 2005.06.08.}

\LOG
\initials{LDF 2005.06.08.}
Added this rule.
\ENDLOG 

@<Define rules@>=

@=any_variable: glyph_variable@>@/
{

   @=$$@> = @=$1@>;

};

@q *** (3) any_variable --> plane_variable.  @>
@*2 \§any variable> $\longrightarrow$ 
\§plane variable>.
\initials{LDF 2005.10.30.}

\LOG
\initials{LDF 2005.10.30.}
Added this rule.
\ENDLOG 

@<Define rules@>=

@=any_variable: plane_variable@>@/
{

   @=$$@> = @=$1@>;

};





@q *** (3) any_variable --> origami_figure_variable.  @>
@*2 \§any variable> $\longrightarrow$ 
\§origami figure variable>.
\initials{LDF 2005.02.04.}

\LOG
\initials{LDF 2005.02.04.}
Added this rule.
\ENDLOG 

@<Define rules@>=

@=any_variable: origami_figure_variable@>@/
{

   @=$$@> = @=$1@>;

};

@q **** (4) Vector-types.@>   
@*3 Vector-types.
\initials{LDF 2004.12.23.}

\LOG
\initials{LDF 2004.12.23.}
Added this section.
\ENDLOG

@q *** (3)  any_variable --> boolean_vector_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§boolean vector variable>.

\LOG
\initials{LDF 2004.12.23.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: boolean_vector_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3)  any_variable --> string_vector_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§string vector variable>.

\LOG
\initials{LDF 2004.12.23.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: string_vector_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3)  any_variable --> numeric_vector_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§numeric vector variable>.

\LOG
\initials{LDF 2004.12.23.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: numeric_vector_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3)  any_variable --> ulong_long_vector_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§ulong long vector variable>.

\LOG
\initials{LDF 2005.12.05.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: ulong_long_vector_variable@>@/
{
  @=$$@> = @=$1@>;
};


@q *** (3)  any_variable --> complex_vector_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§complex vector variable>.
\initials{LDF 2007.12.02.}

\LOG
\initials{LDF 2007.12.02.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: complex_vector_variable@>@/
{
  @=$$@> = @=$1@>;
};



@q *** (3)  any_variable --> matrix_vector_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§matrix vector variable>.

\LOG
\initials{LDF 2007.11.28.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: matrix_vector_variable@>@/
{
  @=$$@> = @=$1@>;
};



@q *** (3)  any_variable --> pen_vector_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§pen vector variable>.

\LOG
\initials{LDF 2004.12.23.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: pen_vector_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3)  any_variable --> dash_pattern_vector_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§dash pattern vector variable>.

\LOG
\initials{LDF 2004.12.23.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: dash_pattern_vector_variable@>@/
{

  @=$$@> = @=$1@>;

};


@q *** (3)  any_variable --> color_vector_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§color vector variable>.

\LOG
\initials{LDF 2004.08.23.}  Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: color_vector_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3)  any_variable --> picture_vector_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§picture vector variable>.

\LOG
\initials{LDF 2004.12.23.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: picture_vector_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3)  any_variable --> transform_vector_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§transform vector variable>.

\LOG
\initials{LDF 2004.12.23.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: transform_vector_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3)  any_variable --> focus_vector_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§focus vector variable>.

\LOG
\initials{LDF 2004.12.23.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: focus_vector_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3)  any_variable --> macro_vector_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§macro vector variable>.
\initials{LDF 2004.12.28.}

\LOG
\initials{LDF 2004.12.28.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: macro_vector_variable@>@/
{

  @=$$@> = @=$1@>;

};


@q *** (3) any_variable --> point_vector_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§point vector variable>.

\LOG
\initials{LDF 2004.08.31.}  Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: point_vector_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3)  any_variable --> bool_point_vector_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§bool-point vector variable>.

\LOG
\initials{LDF 2004.09.01.}  
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: bool_point_vector_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3) any_variable --> path_vector_variable.@>
@*2 \§any variable> $\longrightarrow$ \§path vector variable>.

\LOG
\initials{LDF 2004.12.10.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: path_vector_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3)  any_variable --> triangle_vector_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§triangle vector variable>.

\LOG
\initials{LDF 2004.12.23.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: triangle_vector_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3)  any_variable --> nurb_vector_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§nurb vector variable>.
\initials{LDF 2005.01.26.}

\LOG
\initials{LDF 2005.01.26.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: nurb_vector_variable@>@/
{

  @=$$@> = @=$1@>;

};


@q *** (3) any_variable --> ellipse_vector_variable.@>
@*2 \§any variable> $\longrightarrow$ \§ellipse vector variable>.

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: ellipse_vector_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3) any_variable --> circle_vector_variable.@>
@*2 \§any variable> $\longrightarrow$ \§circle vector variable>.
\initials{LDF 2004.12.14.}

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: circle_vector_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3) any_variable --> parabola_vector_variable.@>
@*2 \§any variable> $\longrightarrow$ \§parabola vector variable>.
\initials{LDF 2005.11.07.}

\LOG
\initials{LDF 2005.11.07.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: parabola_vector_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3) any_variable --> hyperbola_vector_variable.@>
@*2 \§any variable> $\longrightarrow$ \§hyperbola vector variable>.
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: hyperbola_vector_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3) any_variable --> arc_vector_variable.@>
@*2 \§any variable> $\longrightarrow$ \§arc vector variable>.
\initials{LDF 2007.10.13.}

\LOG
\initials{LDF 2007.10.13.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: arc_vector_variable@>@/
{

  @=$$@> = @=$1@>;

};



@q *** (3) any_variable --> conic_section_lattice_vector_variable.@>
@*2 \§any variable> $\longrightarrow$ \§conic section lattice vector variable>.
\initials{LDF 2007.07.29.}

\LOG
\initials{LDF 2007.07.29.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: conic_section_lattice_vector_variable@>@/
{

  @=$$@> = @=$1@>;

};



@q *** (3) any_variable --> helix_vector_variable.@>
@*2 \§any variable> $\longrightarrow$ \§helix vector variable>.
\initials{LDF 2005.05.19.}

\LOG
\initials{LDF 2005.05.19.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: helix_vector_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3) any_variable --> rectangle_vector_variable.@>
@*2 \§any variable> $\longrightarrow$ \§rectangle vector variable>.

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: rectangle_vector_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3) any_variable --> polygon_vector_variable.@>
@*2 \§any variable> $\longrightarrow$ \§polygon vector variable>.
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.02.11.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: polygon_vector_variable@>@/
{

  @=$$@> = @=$1@>;

};



@q *** (3) any_variable --> reg_polygon_vector_variable.@>
@*2 \§any variable> $\longrightarrow$ \§regular polygon vector variable>.

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: reg_polygon_vector_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3)  any_variable --> cone_vector_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§cone vector variable>.
\initials{LDF 2005.05.24.}

\LOG
\initials{LDF 2005.05.24.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: cone_vector_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3)  any_variable --> cylinder_vector_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§cylinder vector variable>.
\initials{LDF 2005.05.25.}

\LOG
\initials{LDF 2005.05.25.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: cylinder_vector_variable@>@/
{

  @=$$@> = @=$1@>;

};


@q *** (3)  any_variable --> cuboid_vector_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§cuboid vector variable>.

\LOG
\initials{LDF 2004.12.23.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: cuboid_vector_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3)  any_variable --> polyhedron_vector_variable.  @>
@*2 \§any variable> $\longrightarrow$ \§polyhedron vector variable>.

\LOG
\initials{LDF 2004.12.23.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: polyhedron_vector_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3) any_variable --> ellipse_slice_vector_variable.@>
@*2 \§any variable> $\longrightarrow$ \§ellipse slice vector variable>.
\initials{LDF 2005.03.29.}

\LOG
\initials{LDF 2005.03.29.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: ellipse_slice_vector_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3) any_variable --> circle_slice_vector_variable.@>
@*2 \§any variable> $\longrightarrow$ \§circle slice vector variable>.
\initials{LDF 2005.03.29.}

\LOG
\initials{LDF 2005.03.29.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: circle_slice_vector_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3) any_variable --> polyhedron_slice_vector_variable.@>
@*2 \§any variable> $\longrightarrow$ \§polyhedron slice vector variable>.
\initials{LDF 2005.03.29.}

\LOG
\initials{LDF 2005.03.29.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: polyhedron_slice_vector_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3)  any_variable --> ellipsoid_vector_variable.  @>
@*2 \§any variable> $\longrightarrow$ 
\§ellipsoid vector variable>.
\initials{LDF 2005.05.26.}

\LOG
\initials{LDF 2005.05.26.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: ellipsoid_vector_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3)  any_variable --> sphere_vector_variable.  @>
@*2 \§any variable> $\longrightarrow$ 
\§sphere vector variable>.
\initials{LDF 2005.06.06.}

\LOG
\initials{LDF 2005.06.06.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: sphere_vector_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3)  any_variable --> sphere_development_vector_variable.  @>
@*2 \§any variable> $\longrightarrow$ 
\§sphere_development vector variable>.
\initials{LDF 2009.11.09.}

\LOG
\initials{LDF 2009.11.09.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: sphere_development_vector_variable@>@/
{

  @=$$@> = @=$1@>;

};


@q *** (3)  any_variable --> paraboloid_vector_variable.  @>
@*2 \§any variable> $\longrightarrow$ 
\§paraboloid vector variable>.
\initials{LDF 2006.01.23.}

\LOG
\initials{LDF 2006.01.23.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: paraboloid_vector_variable@>@/
{

  @=$$@> = @=$1@>;

};


@q *** (3)  any_variable --> glyph_vector_variable.  @>
@*2 \§any variable> $\longrightarrow$ 
\§glyph vector variable>.
\initials{LDF 2005.06.08.}

\LOG
\initials{LDF 2005.06.08.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: glyph_vector_variable@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3)  any_variable --> plane_vector_variable.  @>
@*2 \§any variable> $\longrightarrow$ 
\§plane vector variable>.
\initials{LDF 2005.10.30.}

\LOG
\initials{LDF 2005.10.30.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: plane_vector_variable@>@/
{

  @=$$@> = @=$1@>;

};


@q *** (3)  any_variable --> origami_figure_vector_variable.  @>
@*2 \§any variable> $\longrightarrow$ 
\§origami figure vector variable>.
\initials{LDF 2005.02.04.}

\LOG
\initials{LDF 2005.02.04.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=any_variable: origami_figure_vector_variable@>@/
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
