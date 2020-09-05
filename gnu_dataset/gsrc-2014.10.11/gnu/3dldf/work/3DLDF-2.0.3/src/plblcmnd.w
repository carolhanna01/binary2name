@q plblcmnd.w @> 
@q Created by Laurence Finston Do Okt 28 23:16:05 CEST 2004 @>
     
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

@q ("@@" stands for a single at-sign in the following paragraph.) @>

@q Please send bug reports to Laurence.Finston@@gmx.de @>
@q The mailing list help-3dldf@@gnu.org is available for people to @>
@q ask other users for help.  @>
@q The mailing list info-3dldf@@gnu.org is for sending @>
@q announcements to users. To subscribe to these mailing lists, send an @>
@q email with "subscribe <email-address>" as the subject.  @>

@q The author can be contacted at: @>

@q Laurence D. Finston                 @> 
@q c/o Free Software Foundation, Inc.  @>
@q 51 Franklin St, Fifth Floor         @> 
@q Boston, MA  02110-1301              @>
@q USA                                 @>

@q Laurence.Finston@@gmx.de (@@ stands for a single "at" sign.)@>



@q *** (3) label_command.@>
@*2 \§label command>.

\LOG
\initials{LDF 2004.06.30.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> label_command@>


@q **** (4) label_command --> label_or_dotlabel label_suffix @>
@q **** (4) LEFT_PARENTHESIS string_expression               @>
@q **** (4) COMMA point_expression RIGHT_PARENTHESIS         @>
@q **** (4) transformer_optional                        @>   
@q **** (4) with_text_color_optional with_dot_color_optional @>   
@q **** (4) on_picture_optional                              @>

@*3 \§label command> $\longrightarrow$ \§label or dotlabel>
\§label suffix>
\.{LEFT\_PARENTHESIS} \§string expression>
\.{COMMA} \§point expression> \.{RIGHT\_PARENTHESIS}
\§with transformer optional>
\§with text color optional> \§with dot color optional>
\§on picture optional>.

\LOG
\initials{LDF 2004.06.30.}  
Added this rule.

\initials{LDF 2004.06.30.}
Now using |label_or_dotlabel| instead of |LABEL| as the first symbol 
on the right-hand side.  Also, now calling |Point::label()|.  
Previously, this rule didn't actually do anything.

\initials{LDF 2004.08.16.}
Removed code from this rule to |Scan_Parse::label_command()|, 
which is defined in \filename{scanprsf.web}.

\initials{LDF 2005.01.31.}
Added \§with text color optional> and \§with dot color optional>.
Passing the pointers to |Color| that these non-terminal symbols
reference, or 0, on to |Scan_Parse::label_point_command()|.
\ENDLOG 

@q ***** (5) Definition.@>

@
@<Define rules@>= 

@=label_command: label_or_dotlabel label_suffix @>@/
@=LEFT_PARENTHESIS string_expression @>@/
@=COMMA point_expression RIGHT_PARENTHESIS @>@/
@=transformer_optional@>@/
@=with_text_color_optional with_dot_color_optional@>@/ 
@=on_picture_optional@>@/
{


    label_point_command(static_cast<Scanner_Node>(parameter),
                        @=$1@>, 
                        @=$2@>, 
                        static_cast<string*>(@=$4@>),
                        static_cast<Point*>(@=$6@>),
                        static_cast<Transform*>(@=$8@>),
                        static_cast<Color*>(@=$9@>),
                        static_cast<Color*>(@=$10@>),
                        static_cast<Id_Map_Entry_Node>(@=$11@>)); 

};

@q **** (4) label_command --> label_or_dotlabel label_suffix @>
@q **** (4) LEFT_PARENTHESIS numeric_expression              @>
@q **** (4) COMMA point_expression RIGHT_PARENTHESIS         @>
@q **** (4) with_text_color_optional with_dot_color_optional @>
@q **** (4) on_picture_optional                              @>

@*3 \§label command> $\longrightarrow$ \§label or dotlabel>
\§label suffix>
\.{LEFT\_PARENTHESIS} \§string expression>
\.{COMMA} \§point expression> \.{RIGHT\_PARENTHESIS}
\§with text color optional> \§with dot color optional>
\§on picture optional>.

\LOG
\initials{LDF 2004.10.09.}
Added this rule.

\initials{LDF 2004.10.21.}
Changed \.{INTEGER} to \§numeric expression>.

\initials{LDF 2005.01.31.}
Added \§with text color optional> and \§with dot color optional>.
\ENDLOG 

@q ***** (5) Definition.@>

@
@<Define rules@>= 

@=label_command: label_or_dotlabel label_suffix @>@/
@=LEFT_PARENTHESIS numeric_expression @>@/
@=COMMA point_expression RIGHT_PARENTHESIS @>@/
@=with_text_color_optional with_dot_color_optional@>@/
@=on_picture_optional@>@/
{

  stringstream label_strm;

  label_strm << @=$4@>;

  string* s;
  
      s = new string;

  *s = label_strm.str();

  label_point_command(static_cast<Scanner_Node>(parameter),
                      @=$1@>, 
                      @=$2@>, 
                      s,
                      static_cast<Point*>(@=$6@>),
                      0,
                      static_cast<Color*>(@=$8@>),
                      static_cast<Color*>(@=$9@>),
                      static_cast<Id_Map_Entry_Node>(@=$10@>)); 
};


@q **** (4) label_command --> label_or_dotlabel label_suffix     @>
@q **** (4) LEFT_PARENTHESIS string_expression                   @>
@q **** (4) COMMA bool_point_expression RIGHT_PARENTHESIS        @>
@q **** (4) with_text_color_optional with_dot_color_optional     @>
@q **** (4) on_picture_optional                                  @>

@*3 \§label command> $\longrightarrow$ \§label or dotlabel>
\§label suffix>
\.{LEFT\_PARENTHESIS} \§string expression>
\.{COMMA} \§bool-point expression> \.{RIGHT\_PARENTHESIS}
\§with text color optional> and \§with dot color optional>
\§on picture optional>.

\LOG
\initials{LDF 2004.11.06.}
Added this rule.

\initials{LDF 2005.01.31.}
Added \§with text color optional> 
and \§with dot color optional>.

\initials{LDF 2005.12.01.}
Removed code from this rule.  Now calling |Scan_Parse::label_bool_point_func()|.
\ENDLOG 

@q ***** (5) Definition.@>

@
@<Define rules@>= 

@=label_command: label_or_dotlabel label_suffix@>@/
@=LEFT_PARENTHESIS string_expression@>@/
@=COMMA bool_point_expression RIGHT_PARENTHESIS@>@/
@=with_text_color_optional with_dot_color_optional@>@/
@=on_picture_optional@>@/
{

   Scan_Parse::label_bool_point_func(parameter,
                                     @=$1@>,
                                     @=$2@>,
                                     @=$4@>,
                                     0,
                                     @=$6@>,
                                     @=$8@>,
                                     @=$9@>,
                                     @=$10@>);
};


@q **** (4) label_command --> label_or_dotlabel label_suffix @>
@q **** (4) LEFT_PARENTHESIS numeric_expression              @>
@q **** (4) COMMA bool_point_expression RIGHT_PARENTHESIS    @>
@q **** (4) with_text_color_optional with_dot_color_optional @>   
@q **** (4) on_picture_optional                              @>

@*3 \§label command> $\longrightarrow$ \§label or dotlabel>
\§label suffix>
\.{LEFT\_PARENTHESIS} \§numeric expression>
\.{COMMA} \§bool-point expression> \.{RIGHT\_PARENTHESIS}
\§with text color optional> and \§with dot color optional>
\§on picture optional>.
\initials{LDF 2004.11.06.}

\LOG
\initials{LDF 2004.11.06.}
Added this rule.  

\initials{LDF 2005.01.31.}
Added \§with text color optional> 
and \§with dot color optional>.

\initials{LDF 2005.12.01.}
Removed code from this rule.  Now calling |Scan_Parse::label_bool_point_func()|.
\ENDLOG 

@q ***** (5) Definition.@>

@
@<Define rules@>= 

@=label_command: label_or_dotlabel label_suffix@>@/
@=LEFT_PARENTHESIS numeric_expression@>@/
@=COMMA bool_point_expression RIGHT_PARENTHESIS@>@/
@=with_text_color_optional with_dot_color_optional@>@/
@=on_picture_optional@>@/
{


   Scan_Parse::label_bool_point_func(parameter,
                                     @=$1@>,
                                     @=$2@>,
                                     0,
                                     @=$4@>,
                                     @=$6@>,
                                     @=$8@>,
                                     @=$9@>,
                                     @=$10@>);
};

@q **** (4) label_command --> conditional_label_or_dotlabel label_suffix  @>
@q **** (4) LEFT_PARENTHESIS string_expression                            @>          
@q **** (4) COMMA bool_point_expression RIGHT_PARENTHESIS                 @>
@q **** (4) with_text_color_optional with_dot_color_optional              @> 
@q **** (4) on_picture_optional                                           @>          

@*3 \§label command> $\longrightarrow$ \§label or dotlabel>
\§label suffix>
\.{LEFT\_PARENTHESIS} \§string expression>
\.{COMMA} \§bool-point expression> \.{RIGHT\_PARENTHESIS}
\§with text color optional> and \§with dot color optional>
\§on picture optional>.

\LOG
\initials{LDF 2004.11.06.}
Added this rule.

\initials{LDF 2005.01.31.}
Added \§with text color optional> 
and \§with dot color optional>.

\initials{LDF 2005.12.01.}
Removed code from this rule.  Now calling |Scan_Parse::label_bool_point_func()|.
\ENDLOG 

@q ***** (5) Definition.@>

@
@<Define rules@>= 

@=label_command: conditional_label_or_dotlabel label_suffix@>@/
@=LEFT_PARENTHESIS string_expression@>@/
@=COMMA bool_point_expression RIGHT_PARENTHESIS@>@/
@=with_text_color_optional with_dot_color_optional@>@/
@=on_picture_optional@>@/
{

   Scan_Parse::label_bool_point_func(parameter,
                                     @=$1@>,
                                     @=$2@>,
                                     @=$4@>,
                                     0,
                                     @=$6@>,
                                     @=$8@>,
                                     @=$9@>,
                                     @=$10@>);
};


@q **** (4) label_command --> conditional_label_or_dotlabel  @>
@q **** (4) label_suffix LEFT_PARENTHESIS numeric_expression @>
@q **** (4) COMMA bool_point_expression RIGHT_PARENTHESIS    @>
@q **** (4) with_text_color_optional with_dot_color_optional @>   
@q **** (4) on_picture_optional                              @>

@*3 \§label command> $\longrightarrow$ \§conditional label or dotlabel>
\§label suffix>
\.{LEFT\_PARENTHESIS} \§numeric expression>
\.{COMMA} \§bool-point expression> \.{RIGHT\_PARENTHESIS}
\§with text color optional> and \§with dot color optional>
\§on picture optional>.

\LOG
\initials{LDF 2004.11.06.}
Added this rule.

\initials{LDF 2005.01.31.}
Added \§with text color optional> 
and \§with dot color optional>.

\initials{LDF 2005.12.01.}
Removed code from this rule.  Now calling |Scan_Parse::label_bool_point_func()|.
\ENDLOG 

@q ***** (5) Definition.@>

@
@<Define rules@>= 

@=label_command: conditional_label_or_dotlabel label_suffix@>@/
@=LEFT_PARENTHESIS numeric_expression@>@/
@=COMMA bool_point_expression RIGHT_PARENTHESIS@>@/
@=with_text_color_optional with_dot_color_optional@>@/
@=on_picture_optional@>@/
{
   Scan_Parse::label_bool_point_func(parameter,
                                     @=$1@>,
                                     @=$2@>,
                                     0,
                                     @=$4@>,
                                     @=$6@>,
                                     @=$8@>,
                                     @=$9@>,
                                     @=$10@>);
};




@q **** (4) label_command --> label_or_dotlabel label_suffix    @>
@q **** (4) LEFT_PARENTHESIS string_expression                  @>
@q **** (4) COMMA numeric_expression COMMA path_expression @>
@q **** (4) RIGHT_PARENTHESIS                                   @>
@q **** (4) with_text_color_optional with_dot_color_optional    @>
@q **** (4) on_picture_optional@>   

@*3 \§label command> $\longrightarrow$ \§label or dotlabel>
\§label suffix>
\.{LEFT\_PARENTHESIS} \§string expression> \.{COMMA} 
\§numeric expression>
\.{COMMA} \§path expression> \.{RIGHT\_PARENTHESIS}
\§with text color optional> \§with dot color optional>
\§on picture optional>.

\LOG
\initials{LDF 2004.10.08.}
Added this rule.

\initials{LDF 2004.11.26.}
Changed \.{INTEGER} to \§numeric expression>.

\initials{LDF 2005.01.31.}
Added \§with text color optional> and \§with dot color optional>.
Passing the pointers to |Color| that these non-terminal symbols 
reference on to |Scan_Parse:::label_path_command()|.

\initials{LDF 2005.10.24.}
Changed |path_like_expression| to |path_expression|.
Removed debugging code.
\ENDLOG 

@q ***** (5) Definition.@>


@<Define rules@>= 

@=label_command: label_or_dotlabel label_suffix@>@/
@=LEFT_PARENTHESIS string_expression COMMA numeric_expression@>@/
@=COMMA path_expression RIGHT_PARENTHESIS@>@/
@=with_text_color_optional with_dot_color_optional@>@/ 
@=on_picture_optional@>@/
{
   

    label_path_command(static_cast<Scanner_Node>(parameter),
                       @=$1@>, 
                       @=$2@>, 
                       static_cast<string*>(@=$4@>),
                       static_cast<Color*>(@=$10@>), 
                       static_cast<Color*>(@=$11@>), 
                       static_cast<int>(floor(fabs(@=$6@>) + .5)),
                       static_cast<Path*>(@=$8@>),
                       static_cast<Id_Map_Entry_Node>(@=$12@>)); 
};


@q **** (4) label_command --> label_or_dotlabel label_suffix @>
@q **** (4) LEFT_PARENTHESIS numeric_expression              @>
@q **** (4) COMMA path_expression RIGHT_PARENTHESIS          @>
@q **** (4) with_text_color_optional with_dot_color_optional @>
@q **** (4) on_picture_optional                              @>

@*3 \§label command> $\longrightarrow$ \§label or dotlabel>
\§label suffix>
\.{LEFT\_PARENTHESIS} \.{COMMA} \§numeric expression>
\.{COMMA} \§path expression> \.{RIGHT\_PARENTHESIS}
\§with text color optional> \§with dot color optional>
\§on picture optional>.

\LOG
\initials{LDF 2004.10.09.}
Added this rule.

\initials{LDF 2005.01.31.}
Now passing two nulls, cast to |Color*|, to 
|Scan_Parse::label_path_command()| for the |text_color| and 
|dot_color| arguments.  I plan to add \§with text color optional>
and \§with dot color optional> to this rule.

\initials{LDF 2005.01.31.}
Changed \.{INTEGER} to \§numeric expression>.

\initials{LDF 2005.01.31.}
Added \§with text color optional> \§with dot color optional>
Passing the pointers to |Color| that these non-terminal symbols 
reference on to |Scan_Parse:::label_path_command()|.

\initials{LDF 2005.10.24.}
Changed |path_like_expression| to |path_expression|.
Removed debugging code.
\ENDLOG 


@q ***** (5) Definition.@>

@<Define rules@>= 

@=label_command: label_or_dotlabel label_suffix @>@/
@=LEFT_PARENTHESIS numeric_expression@>@/
@=COMMA path_expression RIGHT_PARENTHESIS @>@/
@=with_text_color_optional with_dot_color_optional@>@/
@=on_picture_optional@>@/
{

   string* s = new string;
  
   *s = "";

   label_path_command(static_cast<Scanner_Node>(parameter),
                      @=$1@>, 
                      @=$2@>, 
                      s,
                      static_cast<Color*>(@=$8@>), 
                      static_cast<Color*>(@=$9@>), 
                      static_cast<int>(floor(fabs(@=$4@>) + .5)),
                      static_cast<Path*>(@=$6@>),
                      static_cast<Id_Map_Entry_Node>(@=$10@>)); 

};


@q **** (4) label_command --> label_or_dotlabel label_suffix @>
@q **** (4) LEFT_PARENTHESIS numeric_expression              @>
@q **** (4) COMMA ellipse_expression RIGHT_PARENTHESIS          @>
@q **** (4) with_text_color_optional with_dot_color_optional @>
@q **** (4) on_picture_optional                              @>

@*3 \§label command> $\longrightarrow$ \§label or dotlabel>
\§label suffix>
\.{LEFT\_PARENTHESIS} \.{COMMA} \§numeric expression>
\.{COMMA} \§ellipse expression> \.{RIGHT\_PARENTHESIS}
\§with text color optional> \§with dot color optional>
\§on picture optional>.

\LOG
\initials{LDF 2005.11.24.}
Added this rule.
\ENDLOG 

@q ***** (5) Definition.@>

@<Define rules@>= 

@=label_command: label_or_dotlabel label_suffix @>@/
@=LEFT_PARENTHESIS numeric_expression@>@/
@=COMMA ellipse_expression RIGHT_PARENTHESIS @>@/
@=with_text_color_optional with_dot_color_optional@>@/
@=on_picture_optional@>@/
{

   string* s = new string;
  
   *s = "";

   label_path_command(static_cast<Scanner_Node>(parameter),
                      @=$1@>, 
                      @=$2@>, 
                      s,
                      static_cast<Color*>(@=$8@>), 
                      static_cast<Color*>(@=$9@>), 
                      static_cast<int>(floor(fabs(@=$4@>) + .5)),
                      static_cast<Ellipse*>(@=$6@>),
                      static_cast<Id_Map_Entry_Node>(@=$10@>)); 

};


@q **** (4) label_command --> label_or_dotlabel label_suffix @>
@q **** (4) LEFT_PARENTHESIS numeric_expression              @>
@q **** (4) COMMA circle_expression RIGHT_PARENTHESIS          @>
@q **** (4) with_text_color_optional with_dot_color_optional @>
@q **** (4) on_picture_optional                              @>

@*3 \§label command> $\longrightarrow$ \§label or dotlabel>
\§label suffix>
\.{LEFT\_PARENTHESIS} \.{COMMA} \§numeric expression>
\.{COMMA} \§circle expression> \.{RIGHT\_PARENTHESIS}
\§with text color optional> \§with dot color optional>
\§on picture optional>.

\LOG
\initials{LDF 2005.11.24.}
Added this rule.
\ENDLOG 

@q ***** (5) Definition.@>

@<Define rules@>= 

@=label_command: label_or_dotlabel label_suffix @>@/
@=LEFT_PARENTHESIS numeric_expression@>@/
@=COMMA circle_expression RIGHT_PARENTHESIS @>@/
@=with_text_color_optional with_dot_color_optional@>@/
@=on_picture_optional@>@/
{

   string* s = new string;
  
   *s = "";

   label_path_command(static_cast<Scanner_Node>(parameter),
                      @=$1@>, 
                      @=$2@>, 
                      s,
                      static_cast<Color*>(@=$8@>), 
                      static_cast<Color*>(@=$9@>), 
                      static_cast<int>(floor(fabs(@=$4@>) + .5)),
                      static_cast<Circle*>(@=$6@>),
                      static_cast<Id_Map_Entry_Node>(@=$10@>)); 

};




@q **** (4) label_command --> label_or_dotlabel label_suffix @>
@q **** (4) LEFT_PARENTHESIS numeric_expression              @>
@q **** (4) COMMA parabola_expression RIGHT_PARENTHESIS          @>
@q **** (4) with_text_color_optional with_dot_color_optional @>
@q **** (4) on_picture_optional                              @>

@*3 \§label command> $\longrightarrow$ \§label or dotlabel>
\§label suffix>
\.{LEFT\_PARENTHESIS} \.{COMMA} \§numeric expression>
\.{COMMA} \§parabola expression> \.{RIGHT\_PARENTHESIS}
\§with text color optional> \§with dot color optional>
\§on picture optional>.
\initials{LDF 2005.11.09.}


\LOG
\initials{LDF 2005.11.09.}
Added this rule.
\ENDLOG 


@q ***** (5) Definition.@>

@<Define rules@>= 

@=label_command: label_or_dotlabel label_suffix @>@/
@=LEFT_PARENTHESIS numeric_expression@>@/
@=COMMA parabola_expression RIGHT_PARENTHESIS @>@/
@=with_text_color_optional with_dot_color_optional@>@/
@=on_picture_optional@>@/
{

   string* s = new string;
  
   *s = "";

   label_path_command(static_cast<Scanner_Node>(parameter),
                      @=$1@>, 
                      @=$2@>, 
                      s,
                      static_cast<Color*>(@=$8@>), 
                      static_cast<Color*>(@=$9@>), 
                      static_cast<int>(floor(fabs(@=$4@>) + .5)),
                      static_cast<Parabola*>(@=$6@>),
                      static_cast<Id_Map_Entry_Node>(@=$10@>)); 

};


@q **** (4) label_command --> label_or_dotlabel label_suffix @>
@q **** (4) LEFT_PARENTHESIS numeric_expression              @>
@q **** (4) COMMA hyperbola_expression RIGHT_PARENTHESIS          @>
@q **** (4) with_text_color_optional with_dot_color_optional @>
@q **** (4) on_picture_optional                              @>

@*3 \§label command> $\longrightarrow$ \§label or dotlabel>
\§label suffix>
\.{LEFT\_PARENTHESIS} \.{COMMA} \§numeric expression>
\.{COMMA} \§hyperbola expression> \.{RIGHT\_PARENTHESIS}
\§with text color optional> \§with dot color optional>
\§on picture optional>.
\initials{LDF 2005.11.15.}

\LOG
\initials{LDF 2005.11.15.}
Added this rule.
\ENDLOG 

@q ***** (5) Definition.@>

@<Define rules@>= 

@=label_command: label_or_dotlabel label_suffix @>@/
@=LEFT_PARENTHESIS numeric_expression@>@/
@=COMMA hyperbola_expression RIGHT_PARENTHESIS @>@/
@=with_text_color_optional with_dot_color_optional@>@/
@=on_picture_optional@>@/
{

   string* s = new string;
  
   *s = "";

   label_path_command(static_cast<Scanner_Node>(parameter),
                      @=$1@>, 
                      @=$2@>, 
                      s,
                      static_cast<Color*>(@=$8@>), 
                      static_cast<Color*>(@=$9@>), 
                      static_cast<int>(floor(fabs(@=$4@>) + .5)),
                      static_cast<Hyperbola*>(@=$6@>),
                      static_cast<Id_Map_Entry_Node>(@=$10@>)); 

};


@q **** (4) label_command --> label_or_dotlabel label_suffix @>
@q **** (4) LEFT_PARENTHESIS numeric_expression              @>
@q **** (4) COMMA polygon_expression RIGHT_PARENTHESIS          @>
@q **** (4) with_text_color_optional with_dot_color_optional @>
@q **** (4) on_picture_optional                              @>

@*3 \§label command> $\longrightarrow$ \§label or dotlabel>
\§label suffix>
\.{LEFT\_PARENTHESIS} \.{COMMA} \§numeric expression>
\.{COMMA} \§polygon expression> \.{RIGHT\_PARENTHESIS}
\§with text color optional> \§with dot color optional>
\§on picture optional>.

\LOG
\initials{LDF 2005.12.13.}
Added this rule.
\ENDLOG 

@q ***** (5) Definition.@>

@<Define rules@>= 

@=label_command: label_or_dotlabel label_suffix @>@/
@=LEFT_PARENTHESIS numeric_expression@>@/
@=COMMA polygon_expression RIGHT_PARENTHESIS @>@/
@=with_text_color_optional with_dot_color_optional@>@/
@=on_picture_optional@>@/
{

   string* s = new string;
  
   *s = "";

   label_path_command(static_cast<Scanner_Node>(parameter),
                      @=$1@>, 
                      @=$2@>, 
                      s,
                      static_cast<Color*>(@=$8@>), 
                      static_cast<Color*>(@=$9@>), 
                      static_cast<int>(floor(fabs(@=$4@>) + .5)),
                      static_cast<Polygon*>(@=$6@>),
                      static_cast<Id_Map_Entry_Node>(@=$10@>)); 

};

@q **** (4) label_command --> label_or_dotlabel label_suffix @>
@q **** (4) LEFT_PARENTHESIS numeric_expression              @>
@q **** (4) COMMA reg_polygon_expression RIGHT_PARENTHESIS          @>
@q **** (4) with_text_color_optional with_dot_color_optional @>
@q **** (4) on_picture_optional                              @>

@*3 \§label command> $\longrightarrow$ \§label or dotlabel>
\§label suffix>
\.{LEFT\_PARENTHESIS} \.{COMMA} \§numeric expression>
\.{COMMA} \§regular polygon expression> \.{RIGHT\_PARENTHESIS}
\§with text color optional> \§with dot color optional>
\§on picture optional>.

\LOG
\initials{LDF 2005.12.14.}
Added this rule.
\ENDLOG 

@q ***** (5) Definition.@>

@<Define rules@>= 

@=label_command: label_or_dotlabel label_suffix @>@/
@=LEFT_PARENTHESIS numeric_expression@>@/
@=COMMA reg_polygon_expression RIGHT_PARENTHESIS @>@/
@=with_text_color_optional with_dot_color_optional@>@/
@=on_picture_optional@>@/
{

   string* s = new string;
  
   *s = "";

   label_path_command(static_cast<Scanner_Node>(parameter),
                      @=$1@>, 
                      @=$2@>, 
                      s,
                      static_cast<Color*>(@=$8@>), 
                      static_cast<Color*>(@=$9@>), 
                      static_cast<int>(floor(fabs(@=$4@>) + .5)),
                      static_cast<Reg_Polygon*>(@=$6@>),
                      static_cast<Id_Map_Entry_Node>(@=$10@>)); 

};

@q **** (4) label_command --> label_or_dotlabel label_suffix @>
@q **** (4) LEFT_PARENTHESIS numeric_expression              @>
@q **** (4) COMMA rectangle_expression RIGHT_PARENTHESIS          @>
@q **** (4) with_text_color_optional with_dot_color_optional @>
@q **** (4) on_picture_optional                              @>

@*3 \§label command> $\longrightarrow$ \§label or dotlabel>
\§label suffix>
\.{LEFT\_PARENTHESIS} \.{COMMA} \§numeric expression>
\.{COMMA} \§rectangle expression> \.{RIGHT\_PARENTHESIS}
\§with text color optional> \§with dot color optional>
\§on picture optional>.
\initials{LDF 2005.12.15.}

\LOG
\initials{LDF 2005.12.15.}
Added this rule.
\ENDLOG 

@q ***** (5) Definition.@>

@<Define rules@>= 

@=label_command: label_or_dotlabel label_suffix @>@/
@=LEFT_PARENTHESIS numeric_expression@>@/
@=COMMA rectangle_expression RIGHT_PARENTHESIS @>@/
@=with_text_color_optional with_dot_color_optional@>@/
@=on_picture_optional@>@/
{

   string* s = new string;
  
   *s = "";

   label_path_command(static_cast<Scanner_Node>(parameter),
                      @=$1@>, 
                      @=$2@>, 
                      s,
                      static_cast<Color*>(@=$8@>), 
                      static_cast<Color*>(@=$9@>), 
                      static_cast<int>(floor(fabs(@=$4@>) + .5)),
                      static_cast<Rectangle*>(@=$6@>),
                      static_cast<Id_Map_Entry_Node>(@=$10@>)); 

};


@q **** (4) label_command --> label_or_dotlabel label_suffix @>
@q **** (4) LEFT_PARENTHESIS numeric_expression              @>
@q **** (4) COMMA triangle_expression RIGHT_PARENTHESIS          @>
@q **** (4) with_text_color_optional with_dot_color_optional @>
@q **** (4) on_picture_optional                              @>

@*3 \§label command> $\longrightarrow$ \§label or dotlabel>
\§label suffix>
\.{LEFT\_PARENTHESIS} \.{COMMA} \§numeric expression>
\.{COMMA} \§triangle expression> \.{RIGHT\_PARENTHESIS}
\§with text color optional> \§with dot color optional>
\§on picture optional>.
\initials{LDF 2005.12.15.}

\LOG
\initials{LDF 2005.12.15.}
Added this rule.
\ENDLOG 

@q ***** (5) Definition.@>

@<Define rules@>= 

@=label_command: label_or_dotlabel label_suffix @>@/
@=LEFT_PARENTHESIS numeric_expression@>@/
@=COMMA triangle_expression RIGHT_PARENTHESIS @>@/
@=with_text_color_optional with_dot_color_optional@>@/
@=on_picture_optional@>@/
{

   string* s = new string;
  
   *s = "";

   label_path_command(static_cast<Scanner_Node>(parameter),
                      @=$1@>, 
                      @=$2@>, 
                      s,
                      static_cast<Color*>(@=$8@>), 
                      static_cast<Color*>(@=$9@>), 
                      static_cast<int>(floor(fabs(@=$4@>) + .5)),
                      static_cast<Triangle*>(@=$6@>),
                      static_cast<Id_Map_Entry_Node>(@=$10@>)); 

};





@q **** (4) label_command --> SUPPRESS_LABELS picture_variable.@>

@*3 \§label command> $\longrightarrow$ \.{SUPPRESS\_LABELS}
\§picture variable>.

\LOG
\initials{LDF 2004.10.22.}
Added this rule.  It doesn't work, though.

\initials{LDF 2005.01.31.}
This rule works now.  I don't know what the problem was before, 
nor what I did to make it work.  Perhaps I was mistaken about it not 
working before.
\ENDLOG 

@q ***** (5) Definition.@>

@
@<Define rules@>= 

@=label_command: SUPPRESS_LABELS picture_variable@>@/
{

  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$2@>);

  Picture* p = static_cast<Picture*>(entry->object); 

  p->suppress_labels();

  @=$$@> = static_cast<void*>(0);

};


@q **** (4) label_command --> UNSUPPRESS_LABELS picture_variable.@>

@*3 \§label command> $\longrightarrow$ \.{UNSUPPRESS\_LABELS}
\§picture variable>.

\LOG
\initials{LDF 2004.10.22.}
Added this rule.  I haven't tested it, though.
\ENDLOG 


@q ***** (5) Definition.@>

@
@<Define rules@>= 

@=label_command: UNSUPPRESS_LABELS picture_variable@>@/
{


/* !! TODO:  LDF 2004.10.22.  Test this rule.  */@; 


  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$2@>);

  Picture* p = static_cast<Picture*>(entry->object); 

  p->unsuppress_labels();

  @=$$@> = static_cast<void*>(0);

};

@q **** (4) label_command --> KILL_LABELS picture_variable.@>

@*3 \§label command> $\longrightarrow$ \.{KILL\_LABELS}
\§picture variable>.

\LOG
\initials{LDF 2004.10.22.}
Added this rule.
\ENDLOG 


@q ***** (5) Definition.@>

@
@<Define rules@>= 

@=label_command: KILL_LABELS picture_variable@>@/
{

  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$2@>);

  Picture* p = static_cast<Picture*>(entry->object); 

  p->kill_labels();

  @=$$@> = static_cast<void*>(0);

};

@q **** (4) label_or_dotlabel.@>
@*3 \§label or dotlabel>.

\LOG
\initials{LDF 2004.06.30.}  Added this section.
\ENDLOG 

@<Type declarations for non-terminal symbols@>=
@=%type <int_value> label_or_dotlabel@>

@q **** (4) label_or_dotlabel --> LABEL.@>
@*3 \§label or dotlabel> $\longrightarrow$ \.{LABEL}.

\LOG
\initials{LDF 2004.06.30.}  Added this rule.
\ENDLOG 


@
@<Define rules@>= 

@=label_or_dotlabel: LABEL@>@/
{

  @=$$@> = LABEL;

};

@q **** (4) label_or_dotlabel --> DOTLABEL.@>
@*3 \§label or dotlabel> $\longrightarrow$ \.{DOTLABEL}.

\LOG
\initials{LDF 2004.06.30.}  Added this rule.
\ENDLOG 


@
@<Define rules@>= 

@=label_or_dotlabel: DOTLABEL@>@/
{

  @=$$@> = DOTLABEL;

};

@q **** (4) conditional_label_or_dotlabel.@>
@*3 \§conditional label or dotlabel>.

\LOG
\initials{LDF 2004.11.06.}
Added this type declaration.
\ENDLOG


@<Type declarations for non-terminal symbols@>=
@=%type <int_value> conditional_label_or_dotlabel@>

@q **** (4) conditional_label_or_dotlabel --> LABEL_IF.@>
@*3 \§conditional label or dotlabel> $\longrightarrow$ 
\.{LABEL\_IF}.

\LOG
\initials{LDF 2004.11.06.}
Added this rule.
\ENDLOG 

@
@<Define rules@>= 

@=conditional_label_or_dotlabel: LABEL_IF@>@/
{

  @=$$@> = LABEL_IF;

};

@q **** (4) conditional_label_or_dotlabel --> LABEL_UNLESS.@>
@*3 \§conditional label or dotlabel> $\longrightarrow$ 
\.{LABEL\_UNLESS}.

\LOG
\initials{LDF 2004.11.06.}
Added this rule.
\ENDLOG 

@
@<Define rules@>= 

@=conditional_label_or_dotlabel: LABEL_UNLESS@>@/
{

  @=$$@> = LABEL_UNLESS;

};

@q **** (4) conditional_label_or_dotlabel --> DOTLABEL_IF.@>
@*3 \§conditional label or dotlabel> $\longrightarrow$ 
\.{DOTLABEL\_IF}.

\LOG
\initials{LDF 2004.11.06.}
Added this rule.
\ENDLOG 

@
@<Define rules@>= 

@=conditional_label_or_dotlabel: DOTLABEL_IF@>@/
{

  @=$$@> = DOTLABEL_IF;

};

@q **** (4) conditional_label_or_dotlabel --> DOTLABEL_UNLESS.@>
@*3 \§conditional label or dotlabel> $\longrightarrow$ 
\.{DOTLABEL\_UNLESS}.

\LOG
\initials{LDF 2004.11.06.}
Added this rule.
\ENDLOG 

@
@<Define rules@>= 

@=conditional_label_or_dotlabel: DOTLABEL_UNLESS@>@/
{

  @=$$@> = DOTLABEL_UNLESS;

};



@q **** (4) label_suffix.@>
@*3 \§label suffix>.

\LOG
\initials{LDF 2004.06.30.}  
Added this section.
\ENDLOG 

@<Type declarations for non-terminal symbols@>=
@=%type <int_value> label_suffix@>


@q ***** (5) label_suffix --> EMPTY.@>
@*4 \§label suffix> $\longrightarrow$ \.{EMPTY}.

\LOG
\initials{LDF 2004.06.30.}  
Added this rule.
\ENDLOG 

@q ****** (6) Definition.@>

@
@<Define rules@>= 

@=label_suffix: /* EMPTY */@>@/
{

  @=$$@> = NULL_VALUE;

  
};

@q ***** (5) label_suffix --> RT.@>
@*4 \§label suffix> $\longrightarrow$ \.{RT}.

\LOG
\initials{LDF 2004.06.30.}  
Added this rule.
\ENDLOG 

@q ****** (6) Definition.@>

@
@<Define rules@>= 

@=label_suffix: RT@>@/
{

  @=$$@> = RT;

};

@q ***** (5) label_suffix --> LFT.@>
@*4 \§label suffix> $\longrightarrow$ \.{LFT}.

\LOG
\initials{LDF 2004.06.30.}  
Added this rule.
\ENDLOG 

@q ****** (6) Definition.@>

@
@<Define rules@>= 

@=label_suffix: LFT@>@/
{

  @=$$@> = LFT;

};


@q ***** (5) label_suffix --> TOP.@>
@*4 \§label suffix> $\longrightarrow$ \.{TOP}.

\LOG
\initials{LDF 2004.06.30.}  
Added this rule.
\ENDLOG 

@q ****** (6) Definition.@>

@
@<Define rules@>= 

@=label_suffix: TOP@>@/
{

  @=$$@> = TOP;

};


@q ***** (5) label_suffix --> BOT.@>
@*4 \§label suffix> $\longrightarrow$ \.{BOT}.

\LOG
\initials{LDF 2004.06.30.}  
Added this rule.
\ENDLOG 

@q ****** (6) Definition.@>

@
@<Define rules@>= 

@=label_suffix: BOT@>@/
{


  @=$$@> = BOT;

};

@q ***** (5) label_suffix --> URT.@>
@*4 \§label suffix> $\longrightarrow$ \.{URT}.

\LOG
\initials{LDF 2004.06.30.}  
Added this rule.
\ENDLOG 

@q ****** (6) Definition.@>

@
@<Define rules@>= 

@=label_suffix: URT@>@/
{
  @=$$@> = URT;

};

@q ***** (5) label_suffix --> ULFT.@>
@*4 \§label suffix> $\longrightarrow$ \.{ULFT}.

\LOG
\initials{LDF 2004.06.30.}  
Added this rule.
\ENDLOG 

@q ****** (6) Definition.@>

@
@<Define rules@>= 

@=label_suffix: ULFT@>@/
{

  @=$$@> = ULFT;

};


@q ***** (5) label_suffix --> LRT.@>
@*4 \§label suffix> $\longrightarrow$ '\.{LRT}'.

\LOG
\initials{LDF 2004.06.30.}  
Added this rule.
\ENDLOG 

@q ****** (6) Definition.@>

@
@<Define rules@>= 

@=label_suffix: LRT@>@/
{

  @=$$@> = LRT;

};


@q ***** (5) label_suffix --> LLFT.@>
@*4 \§label suffix> $\longrightarrow$ '\.{LLFT}'.

\LOG
\initials{LDF 2004.06.30.}  
Added this rule.
\ENDLOG 

@q ****** (6) Definition.@>

@
@<Define rules@>= 

@=label_suffix: LLFT@>@/
{

  @=$$@> = LLFT;

};



@q **** (4) on_picture_optional.  @>

@*3 \§on picture optional>. 

\LOG
\initials{LDF 2004.06.30.}  Added this section.

\initials{LDF 2004.08.17.}
Removed code from this rule to the function |on_picture_optional_0()|, which is
defined in \filename{scanprse.web}.
\ENDLOG 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> on_picture_optional@>

@q ***** (5) on_picture_optional --> EMPTY@>

@*4 \§on picture optional> $\longrightarrow$ \.{EMPTY}.

\LOG
\initials{LDF 2004.06.30.}  Added this rule.
\ENDLOG 

@<Define rules@>= 
@=on_picture_optional: /* Empty */@>
{


  void* v = on_picture_optional_0(static_cast<Scanner_Node>(parameter));

  if (v == 0) /* (|on_picture_optional_0()| failed.)  */
    {
      
      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == 0)| (|on_picture_optional_0()| failed.)  */



  else  /* (|on_picture_optional_0()| succeeded.)  */
    {

      @=$$@> = v;

} /* |else| (|on_picture_optional_0()| succeeded.)  */

};


@*4 \§on picture optional> $\longrightarrow$ \.{picture expression}.

\LOG
\initials{LDF 2004.06.30.}  Added this rule.


\initials{LDF 2004.08.16.}
@:BUG FIX@> BUG FIX:  Now setting |@=$$@> = @=$1@>|.
Also made debugging output thread-safe.
\ENDLOG 

@<Define rules@>= 
@=on_picture_optional: picture_expression@>
{

  @=$$@> = @=$1@>;

};


@q * (1) with_text_color_optional.@> 
@* \§with text color optional>.
\initials{LDF 2005.01.29.}

\LOG
\initials{LDF 2005.01.29.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> with_text_color_optional@>


@q ** (2) with_text_color_optional --> EMPTY.@> 
@*1 \§with text color optional> $\longrightarrow$ \.{EMPTY}.
\initials{LDF 2005.01.29.}

\LOG
\initials{LDF 2005.01.29.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=with_text_color_optional: /* Empty  */@>
{

  @=$$@> = static_cast<void*>(0);

};

@q ** (2) with_text_color_optional -->              @> 
@q ** (2) WITH_TEXT_COLOR color_expression.@> 

@*1 \§with text color optional> $\longrightarrow$ 
\.{WITH\_TEXT\_COLOR} \§color expression>.
\initials{LDF 2005.01.29.}

\LOG
\initials{LDF 2005.01.29.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=with_text_color_optional: WITH_TEXT_COLOR color_expression@>
{

  @=$$@> = static_cast<void*>(@=$2@>);

};

@q * (1) with_dot_color_optional.@> 
@* \§with dot color optional>.
\initials{LDF 2005.01.29.}

\LOG
\initials{LDF 2005.01.29.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> with_dot_color_optional@>


@q ** (2) with_dot_color_optional --> EMPTY.@> 
@*1 \§with dot color optional> $\longrightarrow$ \.{EMPTY}.
\initials{LDF 2005.01.29.}

\LOG
\initials{LDF 2005.01.29.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=with_dot_color_optional: /* Empty  */@>
{

  @=$$@> = static_cast<void*>(0);

};

@q ** (2) with_dot_color_optional -->     @> 
@q ** (2) WITH_DOT_COLOR color_expression.@> 

@*1 \§with dot color optional> $\longrightarrow$ 
\.{WITH\_DOT\_COLOR} \§color expression>.
\initials{LDF 2005.01.29.}

\LOG
\initials{LDF 2005.01.29.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=with_dot_color_optional: WITH_DOT_COLOR color_expression@>
{

  @=$$@> = static_cast<void*>(@=$2@>);

};

@q * (1) transformer_optional.@>
@* \§transformer optional>.
\initials{LDF 2005.08.14.}

\LOG
\initials{LDF 2005.08.14.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> transformer_optional@>


@q ** (2) transformer_optional --> /* EMPTY  */ @> 

@*1 \§transformer optional> $\longrightarrow$ 
\.{EMPTY}.
\initials{LDF 2005.08.14.}

\LOG
\initials{LDF 2005.08.14.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=transformer_optional: /* Empty  */@>
{

  @=$$@> = static_cast<void*>(0);

};

@q ** (2) transformer_optional --> transformer@> 

@*1 \§transformer optional> $\longrightarrow$ \§transformer>.
\initials{LDF 2005.08.14.}

\LOG
\initials{LDF 2005.08.14.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=transformer_optional: transformer@>
{

  @=$$@> = @=$1@>;

};

@q * (1) command: label_or_dotlabel_lattice label_suffix  @>
@q * (1) conic_section_lattice_expression                 @>
@q * (1) transformer_optional with_text_color_optional    @>
@q * (1) with_dot_color_optional on_picture_optional      @>

@* \§command> $\longrightarrow$ \§label or dotlabel>
\§label suffix> \§conic section lattice expression>
\§transformer optional> \§with text color optional>
\§with dot color optional> \§on picture optional>.

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG 

@q ***** (5) Definition.@>

@
@<Define rules@>= 

@=command: label_or_dotlabel_lattice label_suffix@>@/
@=conic_section_lattice_expression @>@/
@=transformer_optional with_text_color_optional @>@/ 
@=with_dot_color_optional on_picture_optional@>@/
{

    Conic_Section_Lattice* c = static_cast<Conic_Section_Lattice*>(@=$3@>); 

    if (c != static_cast<Conic_Section_Lattice*>(0))
    {       
        c->label_lattice(parameter, 
                         @=$1@>, 
                         @=$2@>, 
                         0,
                         @=$4@>, 
                         @=$5@>, 
                         @=$6@>, 
                         @=$7@>);

        delete c;
        c = 0;
    }
};

@q * (1) command: label_or_dotlabel_lattice label_suffix  @>
@q * (1) conic_section_lattice_expression COMMA           @>
@q * (1) numeric_expression transformer_optional          @>
@q * (1) with_text_color_optional with_dot_color_optional @>
@q * (1) on_picture_optional.                             @> 

@* \§command> $\longrightarrow$ \§label or dotlabel>
\§label suffix> 
\§conic section lattice expression>
\.{COMMA} \§numeric expression> 
\§transformer optional> \§with text color optional>
\§with dot color optional> \§on picture optional>.

\LOG
\initials{LDF 2007.08.16.}
Added this rule.

\initials{LDF 2007.08.19.}
Changed \§point expression> to \§numeric expression> on the
right-hand side.
\ENDLOG 

@q ***** (5) Definition.@>

@
@<Define rules@>= 

@=command: label_or_dotlabel_lattice label_suffix@>@/
@=conic_section_lattice_expression COMMA@>@/
@=numeric_expression transformer_optional@>@/ 
@=with_text_color_optional with_dot_color_optional on_picture_optional@>@/
{

    Conic_Section_Lattice* c = static_cast<Conic_Section_Lattice*>(@=$3@>); 

    if (c != static_cast<Conic_Section_Lattice*>(0))
    {       
        c->label_lattice(parameter, 
                         @=$1@>, 
                         @=$2@>, 
                         @=$5@>, 
                         @=$6@>, 
                         @=$7@>, 
                         @=$8@>, 
                         @=$9@>);

        delete c;
        c = 0;
    }
};

@q * (1) command: label_or_dotlabel_lattice label_suffix         @>
@q * (1) LEFT_PARENTHESIS conic_section_lattice_expression COMMA @>
@q * (1) numeric_expression RIGHT_PARENTHESIS transformer_optional @>
@q * (1) with_text_color_optional with_dot_color_optional        @>
@q * (1) on_picture_optional.@> 

@* \§command> $\longrightarrow$ \§label or dotlabel>
\§label suffix> 
\.{LEFT\_PARENTHESIS}
\§conic section lattice expression>
\.{COMMA} \§numeric expression> \.{RIGHT\_PARENTHESIS}
\§transformer optional> \§with text color optional>
\§with dot color optional> \§on picture optional>.

\LOG
\initials{LDF 2007.08.16.}
Added this rule.

\initials{LDF 2007.08.19.}
Changed \§point expression> to \§numeric expression> on the
right-hand side.
\ENDLOG 

@q ***** (5) Definition.@>

@
@<Define rules@>= 

@=command: label_or_dotlabel_lattice label_suffix@>@/
@=LEFT_PARENTHESIS conic_section_lattice_expression COMMA@>@/
@=numeric_expression RIGHT_PARENTHESIS transformer_optional@>@/ 
@=with_text_color_optional with_dot_color_optional on_picture_optional@>@/
{

    Conic_Section_Lattice* c = static_cast<Conic_Section_Lattice*>(@=$4@>); 

    if (c != static_cast<Conic_Section_Lattice*>(0))
    {       
        c->label_lattice(parameter, 
                         @=$1@>, 
                         @=$2@>, 
                         @=$6@>, 
                         @=$8@>, 
                         @=$9@>, 
                         @=$10@>, 
                         @=$11@>);

        delete c;
        c = 0;
    }
};


@q ** (2) label_or_dotlabel_lattice.@>
@*1 \§label or dotlabel lattice>.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <int_value> label_or_dotlabel_lattice@>

@q *** (3) label_or_dotlabel_lattice --> LABEL_LATTICE.@>
@*2 \§label or dotlabel lattice> $\longrightarrow$ \.{LABEL\_LATTICE}.
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG

@<Define rules@>= 

@=label_or_dotlabel_lattice: LABEL_LATTICE@>@/
{

  @=$$@> = LABEL_LATTICE;

};

@q *** (4) label_or_dotlabel_lattice --> DOTLABEL_LATTICE.@>
@*2 \§label or dotlabel lattice> $\longrightarrow$ \.{DOTLABEL\_LATTICE}.

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG 

@<Define rules@>= 

@=label_or_dotlabel_lattice: DOTLABEL_LATTICE@>@/
{

  @=$$@> = DOTLABEL_LATTICE;

};

@q * (1).@> 


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
