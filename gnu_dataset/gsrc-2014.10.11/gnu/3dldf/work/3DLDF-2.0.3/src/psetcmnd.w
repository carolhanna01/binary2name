@q psetcmnd.w @> 
@q Created by Laurence Finston Do Okt 28 22:52:44 CEST 2004 @>
     
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



@q * (1) Set Commands.  @>
@* Set Commands.  

\LOG
\initials{LDF 2004.10.28.}
Created this file.  It contains rules removed from 
\filename{pcommand.w}.
\ENDLOG

@q ** (2) colors.@>  
@*1 {\bf colors}.
\initials{LDF 2004.12.19.}

\LOG
\initials{LDF 2004.12.19.}
Added this section.
\ENDLOG

@q *** (3) command --> SET color_variable LEFT_PARENTHESIS  @>
@q *** (3) numeric_expression COMMA numeric_expression COMMA    @> 
@q *** (3) numeric_expression RIGHT_PARENTHESIS.                @> 


@*2 \§command> $\longrightarrow$ \.{SET} \§color variable> 
\.{LEFT\_PARENTHESIS} \§numeric expression> \.{COMMA} 
\§numeric expression> \.{COMMA} \§numeric expression> 
\.{RIGHT\_PARENTHESIS}.                

\LOG
\initials{LDF 2004.06.30.}
Added this rule.

\initials{LDF 2004.08.14.}
Removed code from this rule and put it into 
|Scan_Parse::set_color()|, which is defined in 
\filename{scanprse.web}.
\ENDLOG 

@q **** (4) Definition.@> 

@<Define rules@>=

@=command: SET color_variable LEFT_PARENTHESIS @>@/
@=numeric_expression COMMA numeric_expression COMMA @>@/
@=numeric_expression RIGHT_PARENTHESIS@>@/
{

  Int_Void_Ptr ivp
    = set_color(static_cast<Scanner_Node>(parameter),
                static_cast<Id_Map_Entry_Node>(@=$2@>),
                @=$4@>,
                @=$6@>,
                @=$8@>);

@q ***** (5) Error handling:  |Scan_Parse::set_color()| failed.@>

@ Error handling:  |Scan_Parse::set_color()| failed.
\initials{LDF 2004.10.28.}

@<Define rules@>=

  if (ivp.i != 0) /*   */
    {
      
      @=$$@> = static_cast<void*>(0); 
      
    } /* |if (ivp.i != 0)| (|set_color()| failed.)  */

@q ***** (5) |Scan_Parse::set_color()| succeeded.@>

@ |Scan_Parse::set_color()| succeeded.
\initials{LDF 2004.10.28.}

@<Define rules@>=
   
  else /* (|ivp.i == 0|,  |set_color()| succeeded.)  */
    {

        @=$$@> = ivp.v;

    } /* |else| (|ivp.i == 0|, |set_color()| succeeded.)  */

@q ***** (5).@> 

};

@q *** (3) command --> SET color_part   @> 
@q *** (3) numeric_secondary color_variable.@> 

@*2 \§command> $\longrightarrow$ \.{SET} \§color part> 
\§numeric secondary> \§color variable>.
\initials{LDF 2004.12.20.}

\LOG
\initials{LDF 2004.12.20.}
Added this rule.  When I tried using |numeric_expression| or
|numeric_tertiary|, this rule wasn't reduced and the parser 
signalled an error.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: SET color_part numeric_secondary color_variable@>@/
{

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$4@>); 

@q ***** (5) Error handling:  |entry == 0 |.@> 
@ Error handling:  |entry == 0 |.
\initials{LDF 2004.12.20.}

@<Define rules@>=

   if (entry == static_cast<Id_Map_Entry_Node>(0))
      {
   

      } /* |if (entry == 0)|  */


@q ***** (5) |entry != 0|.@> 

@ |entry != 0|.
\initials{LDF 2004.12.20.}

@<Define rules@>=
 
   else /* |entry != 0|  */

      {


          Color* c;

@q ****** (6) |entry->object == 0|.@> 

@ |entry->object == 0|.  Try to allocate memory for a new |Color|
for |entry->object|.
Rethrow |bad_alloc| upon error.
\initials{LDF 2004.12.20.}

@<Define rules@>=
   
         if (entry->object == static_cast<void*>(0))
            {

                c = create_new<Color>(0);

                entry->object = static_cast<void*>(c); 

            }  /* |if (entry->object == 0)|  */


@q ****** (6) |entry->object != 0|.@> 

@ |entry->object != 0|.
\initials{LDF 2004.12.20.}

@<Define rules@>=         
 
         else  /* |entry->object != 0|  */
             c = static_cast<Color*>(entry->object);
 

@q ****** (6).@> 

          if (@=$2@> == RED_PART)
             c->set_red_part(@=$3@>);

          else if (@=$2@> == GREEN_PART)
             c->set_green_part(@=$3@>);

          else if (@=$2@> == BLUE_PART)
             c->set_blue_part(@=$3@>);

      
@q ****** (6) Error handling:  Invalid |color_part|.@> 
@ Error handling:  Invalid |color_part|.
\initials{LDF 2004.12.20.}

@<Define rules@>=


          else /* Invalid |color_part|.  */
             {

             
             }  /* |else| (Invalid |color_part|).  */

@q ****** (6) @> 

      }   /* |else| (|entry != 0|)  */

@q ***** (5) @> 
   
   @=$$@> = static_cast<void*>(0);

};


@q ** (2) focuses.@>
@*1 {\bf focuses}.  
\initials{LDF 2004.12.19.}

\LOG
\initials{LDF 2004.12.19.}
Added this section.
\ENDLOG


@q *** (3) command --> SET focus_variable WITH_POSITION @>
@q *** (3) point_expression WITH_DIRECTION point_expression @> 
@q *** (3) WITH_DISTANCE numeric_expression                 @>
@q *** (3) with_angle_optional with_axis_optional.          @>


@*2 \§command> $\longrightarrow$ \.{SET} \§focus variable>
\.{WITH\_POSITION}
\§point expression> \.{WITH\_DIRECTION} \§point expression>  
\.{WITH\_DISTANCE} \§numeric expression>                 
\§with angle optional> \§with axis optional>.          

\LOG
\initials{LDF 2004.06.08.}  
Added this rule.

\initials{LDF 2004.06.24.} 
Now trying to lock and unlock |entry->mutex|.  
Setting |@=$$@>| to |entry->object|, so that 
I'll be able to put |set_commands| on the right-hand 
side of assignments. 

\initials{LDF 2004.08.14.}
Removed code from this rule and put it into 
|Scan_Parse::set_focus()|, which is defined in 
\filename{scanprse.web}.

\initials{LDF 2004.08.14.}
Now passing |X_AXIS|, |Y_AXIS|, and |Z_AXIS| to
|set_focus()|.  This makes it possible to leave
the definition of this function in \filename{scanprse.web}.  
|X_AXIS|, |Y_AXIS|, and |Z_AXIS| are
terminal symbols declared in \filename{pbsndecl.w} 
and therefore unknown when 
\filename{scanprse.web} is compiled.
\ENDLOG 

@q *** (3) Definition.@>


@<Define rules@>= 

@=command: SET focus_variable WITH_POSITION point_expression@>@/
@= WITH_DIRECTION point_expression WITH_DISTANCE numeric_expression@>@/
@= with_angle_optional with_axis_optional@>@/
{

  Int_Void_Ptr ivp = set_focus(static_cast<Scanner_Node>(parameter),
                               static_cast<Id_Map_Entry_Node>(@=$2@>),
                               static_cast<Point*>(@=$4@>),
                               static_cast<Point*>(@=$6@>),
                               @=$8@>,
                               @=$9@>,
                               @=$10@>,
                               X_AXIS,
                               Y_AXIS,
                               Z_AXIS);
@q **** (4) Error handling:  |Scan_Parse::set_focus()| failed.@> 

@ Error handling:  |Scan_Parse::set_focus()| failed.
\initials{LDF 2004.10.28.}

@<Define rules@>=

  if (ivp.i != 0) 
    {
      
      @=$$@> = static_cast<void*>(0); 
      
    } /* |if (ivp.i != 0)| (|set_focus()| failed.)  */
   

@q **** (4) |Scan_Parse::set_focus()| succeeded.@> 

@ |Scan_Parse::set_focus()| succeeded.
\initials{LDF 2004.10.28.}

@<Define rules@>=

  else /* |ivp.i == 0|  */
    {

        @=$$@> = ivp.v;

    } /* |else| (|ivp.i == 0|.)  */

};

@q **** (4) with_angle_optional   @>

@*3 \§with angle optional>. 

\LOG
\initials{LDF 2004.06.08.}  
Added this type declaration.
\ENDLOG 

@<Type declarations for non-terminal symbols@>=
@=%type <real_value> with_angle_optional@>

@q ***** (5) with_angle_optional --> `Empty'.  @>

@*4  \§with angle optional> $\longrightarrow$ `\.{Empty}'.  

\LOG
\initials{LDF 2004.06.08.}  
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=with_angle_optional: /* Empty.  */@>@/
{

  @=$$@> = 0;

};

@q ***** (5) with_angle_optional --> WITH_ANGLE numeric_expression.  @>

@*4  \§with angle optional> $\longrightarrow$ `\.{WITH\_ANGLE}'
\§numeric expression>.

\LOG
\initials{LDF 2004.06.08.}  
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=with_angle_optional: WITH_ANGLE numeric_expression@>@/
{

  @=$$@> = @=$2@>;

};

@q **** (4) with_axis_optional   @>

@*3 \§with axis optional>. 
\LOG
\initials{LDF 2004.06.08.}  
Added this section.
\ENDLOG 

@<Type declarations for non-terminal symbols@>=
@=%type <int_value> with_axis_optional@>


@q ***** (5) with_axis_optional --> EMPTY.  @>

@*4  \§with axis optional> $\longrightarrow$ \.{EMPTY}.  

\LOG
\initials{LDF 2004.06.08.}  
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=with_axis_optional: /* Empty.  */@>@/
{

  @=$$@> = Z_AXIS;

};

@q ***** (5) with_axis_optional --> WITH_AXIS axis_specifier.  @>

@*4 \§with axis optional> $\longrightarrow$ `\.{WITH\_AXIS}'
\§axis specifier>.

\LOG
\initials{LDF 2004.06.08.}  
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=with_axis_optional: WITH_AXIS axis_specifier@>@/
{

  @=$$@> = @=$2@>;

};

@q **** (4) axis_specifier   @>

@*3 \§axis specifier>. 

\LOG
\initials{LDF 2004.06.08.}  
Added this section.
\ENDLOG 

@<Type declarations for non-terminal symbols@>=
@=%type <int_value> axis_specifier@>


@q ***** (5) axis_specifier --> X_AXIS.@>

@*4  \§with axis optional> $\longrightarrow$ `\.{X\_AXIS}'.  

\LOG
\initials{LDF 2004.06.08.}  
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=axis_specifier: X_AXIS@>@/
{

  @=$$@> = X_AXIS;

};


@q ***** (5) axis_specifier --> Y_AXIS.  @>

@*4  \§with axis optional> $\longrightarrow$ `\.{Y\_AXIS}'.  

\LOG
\initials{LDF 2004.06.08.}  
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=axis_specifier: Y_AXIS@>@/
{

  @=$$@> = Y_AXIS;

};

@q ***** (5) axis_specifier --> Z_AXIS.@>

@*4  \§with axis optional> $\longrightarrow$ `\.{Z\_AXIS}'.  

\LOG
\initials{LDF 2004.06.08.}  
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=axis_specifier: Z_AXIS@>@/
{

  @=$$@> = Z_AXIS;

};

@q ** (2) ellipses.@>  
@*1 {\bf ellipses}.
\initials{LDF 2004.12.19.}

\LOG
\initials{LDF 2004.12.19.}
Added this section.
\ENDLOG

@q *** (3) command --> SET ellipse_variable      @>  
@q *** (3) with_center_optional with_axis_h_optional      @>
@q *** (3) with_axis_v_optional with_point_count_optional @>
@q *** (3) with_normal_optional.                          @> 

@*2 \§command> $\longrightarrow$ \.{SET} 
\§ellipse variable> \§with center optional>  
\§with axis h optional> \§with axis v optional> 
\§with point count optional> \§with normal optional>.


\LOG
\initials{LDF 2004.11.01.}
Added this rule.
\ENDLOG 

@q *** (3) Definition.@>


@<Define rules@>= 
@=command: SET ellipse_variable@>@/        
@= with_center_optional  with_axis_h_optional@>@/             
@= with_axis_v_optional with_point_count_optional@>@/         
@= with_normal_optional@>@/                           
{

  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$2@>); 

@q **** (4) Error handling:  |entry == 0|.@>   

@ Error handling:  |entry == 0|.
\initials{LDF 2004.11.01.}

@<Define rules@>=

   if (entry == static_cast<Id_Map_Entry_Node>(0))
     {
        
        @=$$@> = static_cast<void*>(0);

     }  /* |if (entry == 0)|  */

@q **** (4) |entry != 0|.@>   
@ |entry != 0|.
\initials{LDF 2004.11.01.}

@<Define rules@>=

    else /* |entry != 0|  */
       {

          Ellipse* e = static_cast<Ellipse*>(entry->object); 

@q ***** (5) |e == 0|.@>   
@ |e == 0|.
\initials{LDF 2004.11.01.}

@<Define rules@>=

          if (e == static_cast<Ellipse*>(0))
             {

                    e = create_new<Ellipse>(0);

             }  /* |if (e == 0)|  */


@q ***** (5).@>               
@
\LOG
\initials{LDF 2004.11.02.}
@:BUG FIX@> BUG FIX:  Now passing |Point origin| as the |center|
argument to |Ellipse::set()| and shifting |e| later, 
if |*center != origin|.
\ENDLOG 

@<Define rules@>=

         Point origin(0, 0, 0);
         Point* center              = static_cast<Point*>(@=$3@>); 
         real axis_h                = @=$4@>;
         real axis_v                = @=$5@>; 
         unsigned short point_count = @=$6@>;
         Point* normal    = static_cast<Point*>(@=$7@>); 

         e->set(origin, axis_h, axis_v, 0, 0, 0, point_count); 

         Point y_axis_pt(0, 1, 0);

         normal->unit_vector(true);

@q ***** (5) |*normal != y_axis_pt|.  Rotate |*e|.@>  

@ |normal != y_axis_pt|.  Rotate |*e|.
\initials{LDF 2004.11.02.}
            
@<Define rules@>=

         if (*normal != y_axis_pt)
           {
             Transform t;
             t.align_with_axis(origin, *normal, 'y');
             t.inverse(true);
             *e *= t;
           } /* |if (*normal != y_axis_pt)|  */


@q ***** (5) |*center != origin|.  Shift |*e|.@>  

@ |*center != origin|.  Shift |*e|.
\initials{LDF 2004.11.02.}

\LOG
\initials{LDF 2004.11.02.}
Added this section.
\ENDLOG
            
@<Define rules@>=

   if (*center != origin)
    e->shift(*center);

@q ***** (5) Delete |center| and |normal|, set |entry->object| and   @> 
@q ***** (5) |command| to |static_cast<void*>(e)|, and exit rule.@> 

@ Delete |center| and |normal|, set |entry->object| and    
|command| to |static_cast<void*>(e)|, and exit rule.
\initials{LDF 2004.11.02.}

@<Define rules@>=

         delete center;
         delete normal;

         @=$$@> = entry->object = static_cast<void*>(e); 

       } /* |else| (|entry != 0|)  */


@q **** (4) @>   

};

@q ** (2) circles.@>  
@*1 {\bf circles}.
\initials{LDF 2004.12.19.}

\LOG
\initials{LDF 2004.12.19.}
Added this section.
\ENDLOG

@q *** (3) command --> SET circle_variable            @>  
@q *** (3) with_center_optional with_diameter_optional    @>
@q *** (3) with_point_count_optional with_normal_optional.@> 

@*2 \§command> $\longrightarrow$ \.{SET} 
\§circle variable> \§with center optional>  
\§with diameter optional> \§with point count optional> 
\§with normal optional>.


\LOG
\initials{LDF 2004.11.01.}
Added this rule.
\ENDLOG 

@q *** (3) Definition.@>


@<Define rules@>= 
@=command: SET circle_variable@>@/        
@= with_center_optional with_diameter_optional@>@/             
@= with_point_count_optional@>@/         
@= with_normal_optional@>@/                           
{

  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$2@>); 

@q **** (4) Error handling:  |entry == 0|.@>   

@ Error handling:  |entry == 0|.
\initials{LDF 2004.11.01.}

@<Define rules@>=

   if (entry == static_cast<Id_Map_Entry_Node>(0))
     {
        @=$$@> = static_cast<void*>(0);

     }  /* |if (entry == 0)|  */

@q **** (4) |entry != 0|.@>   
@ |entry != 0|.
\initials{LDF 2004.11.01.}

@<Define rules@>=

    else /* |entry != 0|  */
       {

          Circle* c = static_cast<Circle*>(entry->object); 

@q ***** (5) |c == 0|.@>   
@ |c == 0|.
\initials{LDF 2004.11.01.}

@<Define rules@>=

          if (c == static_cast<Circle*>(0))
             {

                    c = create_new<Circle>(0);

             }  /* |if (c == 0)|  */


@q ***** (5).@>               
@
\LOG
\initials{LDF 2004.11.02.}
@:BUG FIX@> BUG FIX:  Now passing |Point origin| as the |center|
argument to |Circle::set()| and shifting |c| later, 
if |*center != origin|.
\ENDLOG 
@<Define rules@>=

         Point origin(0, 0, 0);
         Point* center              = static_cast<Point*>(@=$3@>); 
         real diameter              = @=$4@>;
         unsigned short point_count = @=$5@>;
         Point* normal    = static_cast<Point*>(@=$6@>); 

         c->set(origin, diameter, 0, 0, 0, point_count); 

         Point y_axis_pt(0, 1, 0);

         normal->unit_vector(true);

@q ***** (5) |*normal != y_axis_pt|.  Rotate |*c|.@>  

@ |normal != y_axis_pt|.  Rotate |*c|.
\initials{LDF 2004.11.02.}
            
@<Define rules@>=

         if (*normal != y_axis_pt)
           {
             Transform t;
             t.align_with_axis(origin, *normal, 'y');
             t.inverse(true);
             *c *= t;
           } /* |if (*normal != y_axis_pt)|  */


@q ***** (5) |*center != origin|.  Shift |*c|.@>  

@ |*center != origin|.  Shift |*c|.
\initials{LDF 2004.11.02.}

\LOG
\initials{LDF 2004.11.02.}
Added this section.
\ENDLOG
            
@<Define rules@>=

   if (*center != origin)
    c->shift(*center);


@q ***** (5) Delete |center| and |normal|, set |entry->object| and   @> 
@q ***** (5) |command| to |static_cast<void*>(c)|, and exit rule.@> 

@ Delete |center| and |normal|, set |entry->object| and    
|command| to |static_cast<void*>(c)|, and exit rule.
\initials{LDF 2004.11.02.}

@<Define rules@>=

         delete center;
         delete normal;

         @=$$@> = entry->object = static_cast<void*>(c); 

       } /* |else| (|entry != 0|)  */


@q **** (4) @>   

};

@q ** (2) parabolae.@>  
@*1 {\bf parabolae}.
\initials{LDF 2005.11.07.}

\LOG
\initials{LDF 2005.11.07.}
Added this section.
\ENDLOG

@q *** (3) command --> SET parabola_variable @>  
@q *** (3) set_parabola_option_list.             @> 

@*2 \§command> $\longrightarrow$ \.{SET} 
\§parabola variable> \§set parabola option list>.
\initials{LDF 2005.11.07.}

\LOG
\initials{LDF 2005.11.07.}
Added this rule.
\ENDLOG 

@q *** (3) Definition.@>

@<Define rules@>= 
@=command: SET parabola_variable set_parabola_option_list@>@/
{

   Scan_Parse::set_parabola_rule_func_0(@=$2@>, 
                                        parameter);

   static_cast<Scanner_Node>(parameter)->parabola_set_option_struct = 0;

   @=$$@> = static_cast<void*>(0);

};


@q * (1) set_parabola_option_list.@>   
@* \§set parabola option list>.
\initials{LDF 2005.11.07.}

\LOG
\initials{LDF 2005.11.07.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> set_parabola_option_list@>

@q ** (2) set_parabola_option_list --> EMPTY.@>   
@* \§set parabola option list> $\longrightarrow$ \.{EMPTY}.
\initials{LDF 2005.11.07.}

\LOG
\initials{LDF 2005.11.07.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_parabola_option_list: /* Empty  */@>@/        
{
   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);


   if (   scanner_node->parabola_set_option_struct 
       != static_cast<Parabola_Set_Option_Struct*>(0))
      scanner_node->parabola_set_option_struct->clear();

   else
      scanner_node->parabola_set_option_struct = new Parabola_Set_Option_Struct;

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_parabola_option_list --> set_parabola_option_list @>   
@q ** (2) set_parabola_option.                                  @>   

@* \§set parabola option list> $\longrightarrow$ 
\§set parabola option list> \§set parabola option>. 
\initials{LDF 2005.11.07.}

\LOG
\initials{LDF 2005.11.07.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_parabola_option_list: set_parabola_option_list set_parabola_option@>@/        
{

   @=$$@> = static_cast<void*>(0);

};

@q * (1) set_parabola_option.@>   
@* \§set parabola option>.
\initials{LDF 2005.11.07.}

\LOG
\initials{LDF 2005.11.07.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> set_parabola_option@>

@q ** (2) set_parabola_option_list --> WITH_FOCUS point_expression.@>   
@* \§set parabola option list> $\longrightarrow$ \.{WITH\_FOCUS}
\§point expresssion>.
\initials{LDF 2005.11.07.}

\LOG
\initials{LDF 2005.11.07.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_parabola_option: WITH_FOCUS point_expression@>@/        
{

   static_cast<Scanner_Node>(parameter)->parabola_set_option_struct->focus 
      = static_cast<Point*>(@=$2@>); 

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_parabola_option_list --> WITH_VERTEX point_expression.@>   
@* \§set parabola option list> $\longrightarrow$ \.{WITH\_VERTEX}
\§point expresssion>.
\initials{LDF 2005.11.08.}

\LOG
\initials{LDF 2005.11.08.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_parabola_option: WITH_VERTEX point_expression@>@/
{

   static_cast<Scanner_Node>(parameter)->parabola_set_option_struct->vertex 
      = static_cast<Point*>(@=$2@>); 

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_parabola_option_list --> WITH_PARAMETER numeric_expression.@>   
@* \§set parabola option list> $\longrightarrow$ \.{WITH\_PARAMETER}
\§numeric expresssion>.
\initials{LDF 2005.11.08.}

\LOG
\initials{LDF 2005.11.08.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_parabola_option: WITH_PARAMETER numeric_expression@>@/
{

   static_cast<Scanner_Node>(parameter)->parabola_set_option_struct->parameter 
      = @=$2@>; 

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_parabola_option_list -->           @>   
@q ** (2) WITH_HALF_PARAMETER numeric_expression.@>   
@* \§set parabola option list> $\longrightarrow$ 
\.{WITH\_HALF\_PARAMETER} \§numeric expresssion>.
\initials{LDF 2005.11.08.}

\LOG
\initials{LDF 2005.11.08.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_parabola_option: WITH_HALF_PARAMETER numeric_expression@>@/
{

   static_cast<Scanner_Node>(parameter)->parabola_set_option_struct->parameter 
      = @=$2@> / 2; 

   @=$$@> = static_cast<void*>(0);

};


@q ** (2) set_parabola_option_list -->           @>   
@q ** (2) WITH_DIRECTRIX path_expression.@>   
@* \§set parabola option list> $\longrightarrow$ 
\.{WITH\_DIRECTRIX} \§path expresssion>.
\initials{LDF 2005.11.08.}

\LOG
\initials{LDF 2005.11.08.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_parabola_option: WITH_DIRECTRIX path_expression@>@/
{

   static_cast<Scanner_Node>(parameter)->parabola_set_option_struct->directrix 
      = static_cast<Path*>(@=$2@>); 

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_parabola_option_list --> WITH_MAX_EXTENT numeric_expression.@>   
@* \§set parabola option list> $\longrightarrow$ \.{WITH\_MAX\_EXTENT}
\§numeric expresssion>.
\initials{LDF 2005.11.09.}

\LOG
\initials{LDF 2005.11.09.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_parabola_option: WITH_MAX_EXTENT numeric_expression@>@/
{

   static_cast<Scanner_Node>(parameter)->parabola_set_option_struct->max_extent 
      = @=$2@>; 

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_parabola_option_list --> WITH_INCREMENT numeric_expression.@>   
@* \§set parabola option list> $\longrightarrow$ \.{WITH\_INCREMENT}
\§numeric expresssion>.
\initials{LDF 2005.11.09.}

\LOG
\initials{LDF 2005.11.09.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_parabola_option: WITH_INCREMENT numeric_expression@>@/
{

   static_cast<Scanner_Node>(parameter)->parabola_set_option_struct->increment_value 
      = @=$2@>; 

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) hyperbolae.@>  
@*1 {\bf hyperbolae}.
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Added this section.
\ENDLOG

@q *** (3) command --> SET hyperbola_variable @>  
@q *** (3) set_hyperbola_option_list.             @> 

@*2 \§command> $\longrightarrow$ \.{SET} 
\§hyperbola variable> \§set hyperbola option list>.
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Added this rule.
\ENDLOG 

@q *** (3) Definition.@>

@<Define rules@>= 
@=command: SET hyperbola_variable set_hyperbola_option_list@>@/
{

   Scan_Parse::set_hyperbola_rule_func_0(@=$2@>, 
                                        parameter);

   static_cast<Scanner_Node>(parameter)->hyperbola_set_option_struct = 0;

   @=$$@> = static_cast<void*>(0);

};


@q * (1) set_hyperbola_option_list.@>   
@* \§set hyperbola option list>.
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> set_hyperbola_option_list@>

@q ** (2) set_hyperbola_option_list --> EMPTY.@>   
@* \§set hyperbola option list> $\longrightarrow$ \.{EMPTY}.
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_hyperbola_option_list: /* Empty  */@>@/        
{
   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);


   if (scanner_node->hyperbola_set_option_struct != static_cast<Hyperbola_Set_Option_Struct*>(0))
      scanner_node->hyperbola_set_option_struct->clear();

   else
      scanner_node->hyperbola_set_option_struct = new Hyperbola_Set_Option_Struct;

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_hyperbola_option_list --> set_hyperbola_option_list @>   
@q ** (2) set_hyperbola_option.                                  @>   

@* \§set hyperbola option list> $\longrightarrow$ 
\§set hyperbola option list> \§set hyperbola option>. 
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_hyperbola_option_list: set_hyperbola_option_list set_hyperbola_option@>@/        
{

   @=$$@> = static_cast<void*>(0);

};

@q * (1) set_hyperbola_option.@>   
@* \§set hyperbola option>.
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> set_hyperbola_option@>

@q ** (2) set_hyperbola_option_list --> WITH_CENTER point_expression.@>   
@* \§set hyperbola option list> $\longrightarrow$ \.{WITH\_CENTER}
\§point expresssion>.
\initials{LDF 2005.11.15.}

\LOG
\initials{LDF 2005.11.15.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_hyperbola_option: WITH_CENTER point_expression@>@/        
{

   static_cast<Scanner_Node>(parameter)->hyperbola_set_option_struct->center
      = static_cast<Point*>(@=$2@>); 

   @=$$@> = static_cast<void*>(0);

};




@q ** (2) set_hyperbola_option_list --> WITH_FOCUS point_expression.@>   
@* \§set hyperbola option list> $\longrightarrow$ \.{WITH\_FOCUS}
\§point expresssion>.
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_hyperbola_option: WITH_FOCUS point_expression@>@/        
{

   static_cast<Scanner_Node>(parameter)->hyperbola_set_option_struct->focus_0
      = static_cast<Point*>(@=$2@>); 

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_hyperbola_option_list --> WITH_FOCUS_0 point_expression.@>   
@* \§set hyperbola option list> $\longrightarrow$ \.{WITH\_FOCUS\_0}
\§point expresssion>.
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_hyperbola_option: WITH_FOCUS_0 point_expression@>@/        
{

   static_cast<Scanner_Node>(parameter)->hyperbola_set_option_struct->focus_0
      = static_cast<Point*>(@=$2@>); 

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_hyperbola_option_list --> WITH_FOCUS_1 point_expression.@>   
@* \§set hyperbola option list> $\longrightarrow$ \.{WITH\_FOCUS\_1}
\§point expresssion>.
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_hyperbola_option: WITH_FOCUS_1 point_expression@>@/        
{

   static_cast<Scanner_Node>(parameter)->hyperbola_set_option_struct->focus_1
      = static_cast<Point*>(@=$2@>); 

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_hyperbola_option_list --> WITH_VERTEX point_expression.@>   
@* \§set hyperbola option list> $\longrightarrow$ \.{WITH\_VERTEX}
\§point expresssion>.
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_hyperbola_option: WITH_VERTEX point_expression@>@/
{

   static_cast<Scanner_Node>(parameter)->hyperbola_set_option_struct->vertex_0 
      = static_cast<Point*>(@=$2@>); 

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_hyperbola_option_list --> WITH_VERTEX_0 point_expression.@>   
@* \§set hyperbola option list> $\longrightarrow$ \.{WITH\_VERTEX\_0}
\§point expresssion>.
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_hyperbola_option: WITH_VERTEX_0 point_expression@>@/
{

   static_cast<Scanner_Node>(parameter)->hyperbola_set_option_struct->vertex_0 
      = static_cast<Point*>(@=$2@>); 

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_hyperbola_option_list --> WITH_VERTEX_1 point_expression.@>   
@* \§set hyperbola option list> $\longrightarrow$ \.{WITH\_VERTEX\_1}
\§point expresssion>.
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_hyperbola_option: WITH_VERTEX_1 point_expression@>@/
{

   static_cast<Scanner_Node>(parameter)->hyperbola_set_option_struct->vertex_1 
      = static_cast<Point*>(@=$2@>); 

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_hyperbola_option_list --> WITH_PARAMETER numeric_expression.@>   
@* \§set hyperbola option list> $\longrightarrow$ \.{WITH\_PARAMETER}
\§numeric expresssion>.
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_hyperbola_option: WITH_PARAMETER numeric_expression@>@/
{

   static_cast<Scanner_Node>(parameter)->hyperbola_set_option_struct->parameter 
      = @=$2@>; 

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_hyperbola_option_list -->           @>   
@q ** (2) WITH_HALF_PARAMETER numeric_expression.@>   
@* \§set hyperbola option list> $\longrightarrow$ 
\.{WITH\_HALF\_PARAMETER} \§numeric expresssion>.
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_hyperbola_option: WITH_HALF_PARAMETER numeric_expression@>@/
{

   static_cast<Scanner_Node>(parameter)->hyperbola_set_option_struct->parameter 
      = @=$2@> / 2; 

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_hyperbola_option_list -->           @>   
@q ** (2) WITH_DIRECTRIX path_expression.@>   
@* \§set hyperbola option list> $\longrightarrow$ 
\.{WITH\_DIRECTRIX} \§path expresssion>.
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_hyperbola_option: WITH_DIRECTRIX path_expression@>@/
{

   static_cast<Scanner_Node>(parameter)->hyperbola_set_option_struct->directrix 
      = static_cast<Path*>(@=$2@>); 

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_hyperbola_option_list --> WITH_MAX_EXTENT numeric_expression.@>   
@* \§set hyperbola option list> $\longrightarrow$ \.{WITH\_MAX\_EXTENT}
\§numeric expresssion>.
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_hyperbola_option: WITH_MAX_EXTENT numeric_expression@>@/
{

   static_cast<Scanner_Node>(parameter)->hyperbola_set_option_struct->max_extent 
      = @=$2@>; 

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_hyperbola_option_list --> WITH_INCREMENT numeric_expression.@>   
@* \§set hyperbola option list> $\longrightarrow$ \.{WITH\_INCREMENT}
\§numeric expresssion>.
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_hyperbola_option: WITH_INCREMENT numeric_expression@>@/
{

   static_cast<Scanner_Node>(parameter)->hyperbola_set_option_struct->increment_value 
      = @=$2@>; 

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_hyperbola_option_list -->           @>   
@q ** (2) WITH_MAJOR_AXIS_LENGTH numeric_expression.@>   
@* \§set hyperbola option list> $\longrightarrow$ 
\.{WITH\_MAJOR\_AXIS\_LENGTH} \§numeric expression>.
\initials{LDF 2005.11.15.}

\LOG
\initials{LDF 2005.11.15.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_hyperbola_option: WITH_MAJOR_AXIS_LENGTH numeric_expression@>@/
{

   static_cast<Scanner_Node>(
      parameter)->hyperbola_set_option_struct->major_axis_length 
      = @=$2@>; 

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_hyperbola_option_list -->           @>   
@q ** (2) WITH_HALF_MAJOR_AXIS_LENGTH numeric_expression.@>   
@* \§set hyperbola option list> $\longrightarrow$ 
\.{WITH\_HALF\_MAJOR\_AXIS\_LENGTH} \§numeric expresssion>.
\initials{LDF 2005.11.15.}

\LOG
\initials{LDF 2005.11.15.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_hyperbola_option: WITH_HALF_MAJOR_AXIS_LENGTH numeric_expression@>@/
{

   static_cast<Scanner_Node>(
       parameter)->hyperbola_set_option_struct->major_axis_length 
      = @=$2@> / 2; 

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_hyperbola_option_list -->           @>   
@q ** (2) WITH_MINOR_AXIS_LENGTH numeric_expression.@>   
@* \§set hyperbola option list> $\longrightarrow$ 
\.{WITH\_MINOR\_AXIS\_LENGTH} \§numeric expression>.
\initials{LDF 2005.11.15.}

\LOG
\initials{LDF 2005.11.15.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_hyperbola_option: WITH_MINOR_AXIS_LENGTH numeric_expression@>@/
{

   static_cast<Scanner_Node>(
      parameter)->hyperbola_set_option_struct->minor_axis_length 
      = @=$2@>; 

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_hyperbola_option_list -->           @>   
@q ** (2) WITH_HALF_MINOR_AXIS_LENGTH numeric_expression.@>   
@* \§set hyperbola option list> $\longrightarrow$ 
\.{WITH\_HALF\_MINOR\_AXIS\_LENGTH} \§numeric expresssion>.
\initials{LDF 2005.11.15.}

\LOG
\initials{LDF 2005.11.15.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_hyperbola_option: WITH_HALF_MINOR_AXIS_LENGTH numeric_expression@>@/
{

   static_cast<Scanner_Node>(
      parameter)->hyperbola_set_option_struct->minor_axis_length 
      = @=$2@> / 2; 

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) helices.@>  
@*1 {\bf helices}.
\initials{LDF 2005.05.19.}

\LOG
\initials{LDF 2005.05.19.}
Added this section.
\ENDLOG

@q *** (3) command --> SET helix_variable @>  
@q *** (3) set_helix_option_list.             @> 

@*2 \§command> $\longrightarrow$ \.{SET} 
\§helix variable> \§set helix option list>.
\initials{LDF 2005.05.19.}

\LOG
\initials{LDF 2005.05.19.}
Added this rule.

\initials{LDF 2005.05.20.}
Now passing |scanner_node->helix_set_option_struct| to
|Scan_Parse::set_helix_rule_func_0()|.

\initials{LDF 2005.05.21.}
No longer passing |scanner_node->helix_set_option_struct| to
|Scan_Parse::set_helix_rule_func_0()|.  It's unnecessary, since it's already accessible via
|void* parameter|.

\initials{LDF 2005.05.21.}
@:BUG FIX@> BUG FIX:  Now setting |scanner_node->helix_set_option_struct| to 0 
before exiting rule.
\ENDLOG 



@q *** (3) Definition.@>

@<Define rules@>= 
@=command: SET helix_variable set_helix_option_list@>@/
{

   set_helix_rule_func_0(@=$2@>, 
                         parameter);


   static_cast<Scanner_Node>(parameter)->helix_set_option_struct = 0;

   @=$$@> = static_cast<void*>(0);

};


@q * (1) set_helix_option_list.@>   
@* \§set helix option list>.
\initials{LDF 2005.05.19.}

\LOG
\initials{LDF 2005.05.19.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> set_helix_option_list@>

@q ** (2) set_helix_option_list --> EMPTY.@>   
@* \§set helix option list> $\longrightarrow$ \.{EMPTY}.
\initials{LDF 2005.05.19.}

\LOG
\initials{LDF 2005.05.19.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_helix_option_list: /* Empty  */@>@/        
{

   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter); 

   if (scanner_node->helix_set_option_struct != static_cast<Helix_Set_Option_Struct*>(0))
      scanner_node->helix_set_option_struct->clear();

   else
      scanner_node->helix_set_option_struct = new Helix_Set_Option_Struct;

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_helix_option_list --> set_helix_option_list @>   
@q ** (2) set_helix_option.                               @>   

@* \§set helix option list> $\longrightarrow$ 
\§set helix option list> \§set helix option>. 
\initials{LDF 2005.05.19.}

\LOG
\initials{LDF 2005.05.19.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_helix_option_list: set_helix_option_list set_helix_option@>@/        
{

   @=$$@> = static_cast<void*>(0);

};

@q * (1) set_helix_option.@>   
@* \§set helix option>.
\initials{LDF 2005.05.19.}

\LOG
\initials{LDF 2005.05.19.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> set_helix_option@>

@q ** (2) set_helix_option_list --> WITH_CENTER point_expression.@>   
@* \§set helix option list> $\longrightarrow$ \.{WITH\_CENTER}
\§point expresssion>.
\initials{LDF 2005.05.19.}

\LOG
\initials{LDF 2005.05.19.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_helix_option: WITH_CENTER point_expression@>@/        
{

   static_cast<Scanner_Node>(parameter)->helix_set_option_struct->center 
      = static_cast<Point*>(@=$2@>); 

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_helix_option_list --> WITH_DIRECTION point_expression.@>   
@* \§set helix option list> $\longrightarrow$ \.{WITH\_DIRECTION}
\§point expresssion>.
\initials{LDF 2005.05.21.}

\LOG
\initials{LDF 2005.05.21.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_helix_option: WITH_DIRECTION point_expression@>@/        
{

   static_cast<Scanner_Node>(parameter)->helix_set_option_struct->direction 
       = static_cast<Point*>(@=$2@>); 

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_helix_option_list --> WITH_DIRECTION_VECTOR point_vector_expression.@>   
@* \§set helix option list> $\longrightarrow$ \.{WITH\_DIRECTION\_VECTOR}
\§point vector expresssion>.
\initials{LDF 2005.05.21.}

\LOG
\initials{LDF 2005.05.21.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_helix_option: WITH_DIRECTION_VECTOR point_vector_expression@>@/        
{

   static_cast<Scanner_Node>(parameter)->helix_set_option_struct->direction_vector 
      = static_cast<Pointer_Vector<Point>*>(@=$2@>); 

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_helix_option_list --> WITH_START_DIAMETER numeric_expression.@>

@* \§set helix option list> $\longrightarrow$ \.{WITH\_START\_DIAMETER}
\§numeric expresssion>.
\initials{LDF 2005.05.21.}

\LOG
\initials{LDF 2005.05.21.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_helix_option: WITH_START_DIAMETER numeric_expression@>@/        
{
   static_cast<Scanner_Node>(parameter)->helix_set_option_struct->start_diameter 
      = @=$2@>; 

   @=$$@> = static_cast<void*>(0);

};

@q START HERE!!  Commenting out unnecessary debugging code.  LDF 2005.10.26.@>

@q ** (2) set_helix_option_list --> WITH_POINTS_PER_CYCLE numeric_expression.@>

@* \§set helix option list> $\longrightarrow$ \.{WITH\_POINTS\_PER\_CYCLE}
\§numeric expresssion>.
\initials{LDF 2005.05.21.}

\LOG
\initials{LDF 2005.05.21.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_helix_option: WITH_POINTS_PER_CYCLE numeric_expression@>@/        
{
   
   static_cast<Scanner_Node>(parameter)->helix_set_option_struct->points_per_cycle 
      = static_cast<unsigned short>(floor(fabs(@=$2@>) + .5)); 


   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_helix_option_list --> WITH_CYCLES numeric_expression.@>

@* \§set helix option list> $\longrightarrow$ \.{WITH\_CYCLES}
\§numeric expresssion>.
\initials{LDF 2005.05.21.}

\LOG
\initials{LDF 2005.05.21.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_helix_option: WITH_CYCLES numeric_expression@>@/        
{
   static_cast<Scanner_Node>(parameter)->helix_set_option_struct->cycles = fabs(@=$2@>);

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_helix_option_list --> WITH_ANGLE numeric_expression.@>

@* \§set helix option list> $\longrightarrow$ \.{WITH\_ANGLE}
\§numeric expresssion>.
\initials{LDF 2005.05.21.}

\LOG
\initials{LDF 2005.05.21.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_helix_option: WITH_ANGLE numeric_expression@>@/        
{
   static_cast<Scanner_Node>(parameter)->helix_set_option_struct->angle 
      = fmod(fabs(@=$2@>), 360);

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_helix_option_list --> WITH_TYPE helix_type_specifier.@>

@* \§set helix option list> $\longrightarrow$ \.{WITH\_TYPE}
\§helix type specifier>.
\initials{LDF 2005.05.21.}

\LOG
\initials{LDF 2005.05.21.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_helix_option: WITH_TYPE helix_type_specifier@>@/        
{
   
   static_cast<Scanner_Node>(parameter)->helix_set_option_struct->type = @=$2@>;

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) helix_type_specifier.@>   
@*1 \§helix type specifier>.
\initials{LDF 2005.05.21.}

\LOG
\initials{LDF 2005.05.21.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <int_value> helix_type_specifier@>

@q *** (3) helix_type_specifier --> NULL_TYPE.@>

@*2 \§helix type specifier> $\longrightarrow$ \.{NULL\_TYPE}.
\initials{LDF 2005.05.21.}

\LOG
\initials{LDF 2005.05.21.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=helix_type_specifier: NULL_TYPE@>@/        
{
 
   @=$$@> = Helix::HELIX_NULL_TYPE@>;
  
};

@q *** (3) helix_type_specifier --> ARCHIMEDEAN.@>

@*2 \§helix type specifier> $\longrightarrow$ \.{ARCHIMEDEAN}.
\initials{LDF 2005.05.21.}

\LOG
\initials{LDF 2005.05.21.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=helix_type_specifier: ARCHIMEDEAN@>@/        
{
   
   @=$$@> = Helix::HELIX_ARCHIMEDEAN_TYPE@>;

};

@q *** (3) helix_type_specifier --> LOGARITHMIC.@>

@*2 \§helix type specifier> $\longrightarrow$ \.{LOGARITHMIC}.
\initials{LDF 2005.05.21.}

\LOG
\initials{LDF 2005.05.21.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=helix_type_specifier: LOGARITHMIC@>@/        
{
   
   @=$$@> = Helix::HELIX_LOGARITHMIC_TYPE@>;

};


@q *** (3) helix_type_specifier --> PARABOLIC.@>

@*2 \§helix type specifier> $\longrightarrow$ \.{PARABOLIC}.
\initials{LDF 2005.05.21.}

\LOG
\initials{LDF 2005.05.21.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=helix_type_specifier: PARABOLIC@>@/        
{
   
   @=$$@> = Helix::HELIX_PARABOLIC_TYPE@>;


};

@q *** (3) helix_type_specifier --> HYPERBOLIC.@>

@*2 \§helix type specifier> $\longrightarrow$ \.{HYPERBOLIC}.
\initials{LDF 2005.05.21.}

\LOG
\initials{LDF 2005.05.21.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=helix_type_specifier: HYPERBOLIC@>@/        
{
   
   @=$$@> = Helix::HELIX_HYPERBOLIC_TYPE@>;

};


@q ** (2) triangles.@>  
@*1 {\bf triangles}.
\initials{LDF 2005.01.20.}

\LOG
\initials{LDF 2005.01.20.}
Added this section.
\ENDLOG


@q *** (3) command --> SET triangle_variable WITH_POINTS@>        
@q *** (3) LEFT_PARENTHESIS point_expression COMMA point_expression   @>
@q *** (3) COMMA point_expression RIGHT_PARENTHESIS                     @>

@*2 \§command> $\longrightarrow$ \.{SET} 
\§triangle variable> \§triangle constructor type>
\.{LEFT\_PARENTHESIS} \§point expression> \.{COMMA} 
\§point expression> \.{COMMA} \§point expression> 
\.{RIGHT\_PARENTHESIS}.
\initials{LDF 2005.01.23.}

\LOG
\initials{LDF 2005.01.23.}
Started working on this rule.
\ENDLOG 

@q **** (4) Definition.@>


@<Define rules@>= 
@=command: SET triangle_variable WITH_POINTS@>@/        
@=LEFT_PARENTHESIS point_expression COMMA point_expression@>@/
@=COMMA point_expression RIGHT_PARENTHESIS@>@/        
{

  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$2@>); 

@q ***** (5) Error handling:  |entry == 0|.@>   

@ Error handling:  |entry == 0|.
\initials{LDF 2005.01.23.}

@<Define rules@>=

   if (entry == static_cast<Id_Map_Entry_Node>(0))
     {
        
        @=$$@> = static_cast<void*>(0);

     }  /* |if (entry == 0)|  */

@q ***** (5) |entry != 0|.@>   
@ |entry != 0|.
\initials{LDF 2005.01.23.}

@<Define rules@>=

    else /* |entry != 0|  */
       {

          Triangle* t = static_cast<Triangle*>(entry->object); 

@q ****** (6) |t == 0|.@>   
@ |t == 0|.
\initials{LDF 2005.01.23.}

@<Define rules@>=

          if (t == static_cast<Triangle*>(0))
             {

                    t = create_new<Triangle>(0);

 
               entry->object = static_cast<void*>(t); 

            }  /* |if (t == 0)|  */

@q ****** (6).@> 

         t->set(static_cast<Point*>(@=$5@>), 
                static_cast<Point*>(@=$7@>), 
                static_cast<Point*>(@=$9@>), 
                static_cast<Scanner_Node>(parameter));
         
@q ****** (6).@> 

   }  /* |else| (|entry != 0|)  */

@q ***** (5).@> 

};




@q ** (2) reg_polygons.@>  

@*1 {\bf reg\_polygons}.

\LOG
\initials{LDF 2004.12.19.}
Added this section.
\ENDLOG

@q *** (3) command --> SET reg_polygon_variable            @>  
@q *** (3) WITH_SIDES numeric_expression with_center_optional  @>
@q *** (3) with_diameter_optional with_normal_optional.        @> 

@*2 \§command> $\longrightarrow$ \.{SET} 
\§regular polygon variable> \.{WITH\_SIDES} \§numeric expression>
\§with center optional>  \§with diameter optional> 
\§with normal optional>.


\LOG
\initials{LDF 2004.11.01.}
Added this rule.
\ENDLOG 

@q *** (3) Definition.@>


@<Define rules@>= 
@=command: SET reg_polygon_variable@>@/        
@= WITH_SIDES numeric_expression with_center_optional@>@/             
@= with_diameter_optional with_normal_optional@>@/                           
{


   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$2@>); 

@q **** (4) Error handling:  |entry == 0|.@>   

@ Error handling:  |entry == 0|.
\initials{LDF 2004.11.01.}

@<Define rules@>=

   if (entry == static_cast<Id_Map_Entry_Node>(0))
     {
        @=$$@> = static_cast<void*>(0);

     }  /* |if (entry == 0)|  */

@q **** (4) |entry != 0|.@>   
@ |entry != 0|.
\initials{LDF 2004.11.01.}

@<Define rules@>=

    else /* |entry != 0|  */
       {

          Reg_Polygon* r = static_cast<Reg_Polygon*>(entry->object); 

@q ***** (5) |r == 0|.@>   
@ |r == 0|.
\initials{LDF 2004.11.01.}

@<Define rules@>=

          if (r == static_cast<Reg_Polygon*>(0))
             {


                    r = create_new<Reg_Polygon>(0);

             }  /* |if (r == 0)|  */


@q ***** (5).@>               
@
\LOG
\initials{LDF 2004.11.02.}
@:BUG FIX@> BUG FIX:  Now passing |Point origin| as the |center|
argument to |Reg_Polygon::set()| and shifting |r| later, 
if |*center != origin|.
\ENDLOG 
@<Define rules@>=

         Point origin(0, 0, 0);
         real sides    = @=$4@>;
         Point* center = static_cast<Point*>(@=$5@>); 
         real diameter = @=$6@>;
         Point* normal = static_cast<Point*>(@=$7@>); 

@q ****** (6) Error handling:  |sides < 0|.@> 
@ Error handling:  |sides < 0|.
\initials{LDF 2004.11.02.}

@<Define rules@>=


         if (sides < 0)
           {

               sides *= -1;

           }  /* |if (sides < 0)|  */


@q ****** (6) Error handling:  |sides == 0|.@> 
@ Error handling:  |sides == 0|.
\initials{LDF 2004.11.02.}

@<Define rules@>=

         else if (sides == 0)
            {
                 sides = 5;

            } /* |else if (sides == 0)|  */

@q ****** (6).@> 

         sides = floor(sides + .5);

         unsigned short u = static_cast<unsigned short>(sides); 

         r->set(origin, u, diameter); 

         Point y_axis_pt(0, 1, 0);

         normal->unit_vector(true);

@q ***** (5) |*normal != y_axis_pt|.  Rotate |*r|.@>  

@ |normal != y_axis_pt|.  Rotate |*r|.
\initials{LDF 2004.11.02.}
            
@<Define rules@>=

         if (*normal != y_axis_pt)
           {
             Transform t;
             t.align_with_axis(origin, *normal, 'y');
             t.inverse(true);
             *r *= t;
           } /* |if (*normal != y_axis_pt)|  */


@q ***** (5) |*center != origin|.  Shift |*r|.@>  

@ |*center != origin|.  Shift |*r|.
\initials{LDF 2004.11.02.}

\LOG
\initials{LDF 2004.11.02.}
Added this section.
\ENDLOG
            
@<Define rules@>=

   if (*center != origin)
    r->shift(*center);


@q ***** (5) Delete |center| and |normal|, set |entry->object| and   @> 
@q ***** (5) |command| to |static_cast<void*>(r)|, and exit rule.@> 

@ Delete |center| and |normal|, set |entry->object| and    
|command| to |static_cast<void*>(r)|, and exit rule.
\initials{LDF 2004.11.02.}

@<Define rules@>=

         delete center;
         delete normal;

         @=$$@> = entry->object = static_cast<void*>(r); 

       } /* |else| (|entry != 0|)  */


@q **** (4) @>   

};



@q * (1) with_center_optional.@> 
@* \§with center optional>.
\initials{LDF 2004.11.02.}

\LOG
\initials{LDF 2004.11.02.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> with_center_optional@>

@q ** (2) with_center_optional --> EMPTY.@> 
@*1 \§with center optional> $\longrightarrow$ \.{EMPTY}.
\initials{LDF 2004.11.02.}

\LOG
\initials{LDF 2004.11.02.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=with_center_optional: /* Empty.  */@>@/
{

      Point* p;

           p = create_new<Point>(0);

     p->set(0, 0, 0);

     @=$$@> = static_cast<void*>(p); 
 
};

@q ** (2) with_center_optional --> \.{WITH\_CENTER} point_expression.@> 
@*1 \§with center optional> $\longrightarrow$ \§point expression>.
\initials{LDF 2004.11.02.}

\LOG
\initials{LDF 2004.11.02.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=with_center_optional: WITH_CENTER point_expression@>@/
{

   @=$$@> = @=$2@>;
 
};

@q * (1) with_diameter_optional.@> 
@* \§with diameter optional>.
\initials{LDF 2004.11.02.}

\LOG
\initials{LDF 2004.11.02.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <real_value> with_diameter_optional@>

@q ** (2) with_diameter_optional --> EMPTY.@> 
@*1 \§with diameter optional> $\longrightarrow$ \.{EMPTY}.
\initials{LDF 2004.11.02.}

\LOG
\initials{LDF 2004.11.02.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=with_diameter_optional: /* Empty.  */@>@/
{

     @=$$@> = 1.0;
 
};

@q ** (2) with_diameter_optional --> WITH_DIAMETER numeric_expression.@> 
@*1 \§with diameter optional> $\longrightarrow$ 
\.{WITH\_DIAMETER}
\§numeric expression>.
\initials{LDF 2004.11.02.}

\LOG
\initials{LDF 2004.11.02.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=with_diameter_optional: WITH_DIAMETER numeric_expression@>@/
{

   @=$$@> = @=$2@>;
 
};


@q * (1) with_axis_h_optional.@> 
@* \§with axis h optional>.
\initials{LDF 2004.11.02.}

\LOG
\initials{LDF 2004.11.02.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <real_value> with_axis_h_optional@>

@q ** (2) with_axis_h_optional --> EMPTY.@> 
@*1 \§with axis h optional> $\longrightarrow$ \.{EMPTY}.
\initials{LDF 2004.11.02.}

\LOG
\initials{LDF 2004.11.02.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=with_axis_h_optional: /* Empty.  */@>@/
{

     @=$$@> = 1.0;
 
};

@q ** (2) with_axis_h_optional --> WITH_AXIS_H numeric_expression.@> 
@*1 \§with axis h optional> $\longrightarrow$ 
\.{WITH\_AXIS\_H}
\§numeric expression>.
\initials{LDF 2004.11.02.}

\LOG
\initials{LDF 2004.11.02.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=with_axis_h_optional: WITH_AXIS_H numeric_expression@>@/
{

   @=$$@> = @=$2@>;
 
};


@q * (1) with_axis_v_optional.@> 
@* \§with axis v optional>.
\initials{LDF 2004.11.02.}

\LOG
\initials{LDF 2004.11.02.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <real_value> with_axis_v_optional@>

@q ** (2) with_axis_v_optional --> EMPTY.@> 
@*1 \§with axis v optional> $\longrightarrow$ \.{EMPTY}.
\initials{LDF 2004.11.02.}

\LOG
\initials{LDF 2004.11.02.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=with_axis_v_optional: /* Empty.  */@>@/
{

     @=$$@> = 1.0;
 
};

@q ** (2) with_axis_v_optional --> WITH_AXIS_V numeric_expression.@> 
@*1 \§with axis v optional> $\longrightarrow$ 
\.{WITH\_AXIS\_V} \§numeric expression>.
\initials{LDF 2004.11.02.}

\LOG
\initials{LDF 2004.11.02.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=with_axis_v_optional: WITH_AXIS_V numeric_expression@>@/
{

   @=$$@> = @=$2@>;
 
};

@q * (1) with_point_count_optional.@> 
@* \§with point count optional>.
\initials{LDF 2004.11.02.}

\LOG
\initials{LDF 2004.11.02.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <int_value> with_point_count_optional@>

@q ** (2) with_point_count_optional --> EMPTY.@> 
@*1 \§with point count optional> $\longrightarrow$ \.{EMPTY}.
\initials{LDF 2004.11.02.}

\LOG
\initials{LDF 2004.11.02.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=with_point_count_optional: /* Empty.  */@>@/
{

     @=$$@> = 32;
 
};

@q ** (2) with_point_count_optional --> WITH_POINT_COUNT numeric_expression.@> 
@*1 \§with point count optional> $\longrightarrow$ 
\.{WITH\_POINT\_COUNT} \§numeric expression>.

\initials{LDF 2004.11.02.}

\LOG
\initials{LDF 2004.11.02.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=with_point_count_optional: WITH_POINT_COUNT numeric_expression@>@/
{

   real r = @=$2@>;

@q **** (4) Error handling:  |r < 0|.@>   

@ Error handling:  |r < 0|.
\initials{LDF 2004.11.02.}

@<Define rules@>=

   if (r == ZERO_REAL)
      {


        @=$$@> = 32;

      } /* |if (r < 0)|  */


@q **** (4) |r != 0|.@>   

@ |r != 0|.
\initials{LDF 2004.11.02.}

@<Define rules@>=

   else /* |r != 0|  */
     {


@q ***** (5) Error handling:  |r < 0|.@> 

@ Error handling:  |r < 0|.
\initials{LDF 2004.11.02.}

@<Define rules@>=

        if (r < 0)
           {

              r = -r;

           }  /* |if (r < 0)|  */


         r = floor(r + .5); 
        
         unsigned short u = static_cast<unsigned short>(r);

@q ***** (5) Error handling:  |u % 4 != 0|.@> 
@ Error handling:  |u % 4 != 0|.
\initials{LDF 2004.11.02.}

@<Define rules@>=

         unsigned short v = u % 4;

         if (v != 0)
           {

               u += (4 - v);


           } /* |if (u % 4 != 0)|  */


@q ***** (5).@> 

        @=$$@> = u;


    } /* |else| (|r != 0|)  */
 
};

@q * (1) with_normal_optional.@> 
@* \§with normal optional>.
\initials{LDF 2004.11.02.}

\LOG
\initials{LDF 2004.11.02.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> with_normal_optional@>

@q ** (2) with_normal_optional --> EMPTY.@> 
@*1 \§with normal optional> $\longrightarrow$ \.{EMPTY}.
\initials{LDF 2004.11.02.}

\LOG
\initials{LDF 2004.11.02.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=with_normal_optional: /* Empty.  */@>@/
{

      Point* p;

           p = create_new<Point>(0);
 

     p->set(0, 1, 0);

     @=$$@> = static_cast<void*>(p); 
 
};

@q ** (2) with_normal_optional --> WITH_NORMAL point_expression.@> 
@*1 \§with normal optional> $\longrightarrow$ 
\.{WITH\_NORMAL} \§point expression>.
\initials{LDF 2004.11.02.}

\LOG
\initials{LDF 2004.11.02.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=with_normal_optional: WITH_NORMAL point_expression@>@/
{

   @=$$@> = @=$2@>;
 
};

@q ** (2) Cones.@>  
@*1 Cones.
\initials{LDF 2005.05.25.}

\LOG
\initials{LDF 2005.05.25.}
Added this section.
\ENDLOG

@q *** (3) command --> SET cone_variable @>  
@q *** (3) set_cone_option_list.             @> 

@*2 \§command> $\longrightarrow$ \.{SET} 
\§cone variable> \§set cone option list>.
\initials{LDF 2005.05.25.}

\LOG
\initials{LDF 2005.05.25.}
Added this rule.

\initials{LDF 2005.05.25.}
Commented-in the call to |Scan_Parse::set_cone_rule_func_0()|.
\ENDLOG 

@q *** (3) Definition.@>

@<Define rules@>= 
@=command: SET cone_variable set_cone_option_list@>@/
{

   set_cone_rule_func_0(@=$2@>, 
                        parameter);

   static_cast<Scanner_Node>(parameter)->cone_set_option_struct = 0;

   @=$$@> = static_cast<void*>(0);

};


@q * (1) set_cone_option_list.@>   
@* \§set cone option list>.
\initials{LDF 2005.05.25.}

\LOG
\initials{LDF 2005.05.25.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> set_cone_option_list@>

@q ** (2) set_cone_option_list --> EMPTY.@>   
@* \§set cone option list> $\longrightarrow$ \.{EMPTY}.
\initials{LDF 2005.05.25.}

\LOG
\initials{LDF 2005.05.25.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_cone_option_list: /* Empty  */@>@/        
{

   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

   if (scanner_node->cone_set_option_struct != static_cast<Cone_Set_Option_Struct*>(0))
      scanner_node->cone_set_option_struct->clear();

   else
      scanner_node->cone_set_option_struct = new Cone_Set_Option_Struct;

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_cone_option_list --> set_cone_option_list @>   
@q ** (2) set_cone_option.                               @>   

@* \§set cone option list> $\longrightarrow$ 
\§set cone option list> \§set cone option>. 
\initials{LDF 2005.05.25.}

\LOG
\initials{LDF 2005.05.25.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_cone_option_list: set_cone_option_list set_cone_option@>@/        
{

   @=$$@> = static_cast<void*>(0);

};

@q * (1) set_cone_option.@>   
@* \§set cone option>.
\initials{LDF 2005.05.25.}

\LOG
\initials{LDF 2005.05.25.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> set_cone_option@>

@q ** (2) set_cone_option_list --> WITH_CENTER point_expression.@>   
@* \§set cone option list> $\longrightarrow$ \.{WITH\_CENTER}
\§point expresssion>.
\initials{LDF 2005.05.25.}

\LOG
\initials{LDF 2005.05.25.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_cone_option: WITH_CENTER point_expression@>@/        
{

   static_cast<Scanner_Node>(parameter)->cone_set_option_struct->center 
      = static_cast<Point*>(@=$2@>); 

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_cone_option_list --> WITH_DIRECTION point_expression.@>   
@* \§set cone option list> $\longrightarrow$ \.{WITH\_DIRECTION}
\§point expresssion>.
\initials{LDF 2005.05.21.}

\LOG
\initials{LDF 2005.05.21.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_cone_option: WITH_DIRECTION point_expression@>@/        
{

   static_cast<Scanner_Node>(parameter)->cone_set_option_struct->direction 
      = static_cast<Point*>(@=$2@>); 

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_cone_option --> WITH_RADIUS numeric_expression.@>   
@* \§set cone option> $\longrightarrow$ \.{WITH\_RADIUS} \§numeric expresssion>.
\initials{LDF 2006.11.08.}

\LOG
\initials{LDF 2006.11.08.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_cone_option: WITH_RADIUS numeric_expression@>@/        
{

   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);
 
    if (scanner_node->cone_set_option_struct->radius == static_cast<real*>(0))
       scanner_node->cone_set_option_struct->radius = new real(@=$2@>); 
    else 
       *scanner_node->cone_set_option_struct->radius = @=$2@>; 


   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_cone_option_list --> WITH_AXIS_X numeric_expression.@>   
@* \§set cone option list> $\longrightarrow$ \.{WITH\_AXIS\_X}
\§numeric expresssion>.
\initials{LDF 2006.11.08.}

\LOG
\initials{LDF 2006.11.08.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_cone_option: WITH_AXIS_X numeric_expression@>@/        
{

   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);
 
    if (scanner_node->cone_set_option_struct->axis_x == static_cast<real*>(0))
       scanner_node->cone_set_option_struct->axis_x = new real(@=$2@>); 
    else 
       *scanner_node->cone_set_option_struct->axis_x = @=$2@>; 

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_cone_option_list --> WITH_AXIS_Y numeric_expression.@>   
@* \§set cone option list> $\longrightarrow$ \.{WITH\_AXIS\_Y}
\§numeric expresssion>.
\initials{LDF 2006.11.08.}

\LOG
\initials{LDF 2006.11.08.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_cone_option: WITH_AXIS_Y numeric_expression@>@/        
{

   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);
 
    if (scanner_node->cone_set_option_struct->axis_y == static_cast<real*>(0))
       scanner_node->cone_set_option_struct->axis_y = new real(@=$2@>); 
    else 
       *scanner_node->cone_set_option_struct->axis_y = @=$2@>; 

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_cone_option_list --> WITH_AXIS_Z numeric_expression.@>   
@* \§set cone option list> $\longrightarrow$ \.{WITH\_AXIS\_Z}
\§numeric expresssion>.
\initials{LDF 2006.11.08.}

\LOG
\initials{LDF 2006.11.08.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_cone_option: WITH_AXIS_Z numeric_expression@>@/        
{

   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);
 
    if (scanner_node->cone_set_option_struct->axis_z == static_cast<real*>(0))
       scanner_node->cone_set_option_struct->axis_z = new real(@=$2@>); 
    else 
       *scanner_node->cone_set_option_struct->axis_z = @=$2@>; 
    
   @=$$@> = static_cast<void*>(0);

};

@q *** (3) set_cone_option_list --> WITH_POINT_COUNT numeric_expression.@>   
@*2 \§set cone option list> $\longrightarrow$ \.{WITH\_POINT\_COUNT}
\§numeric expresssion>.
\initials{LDF 2006.11.08.}

\LOG
\initials{LDF 2006.11.08.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_cone_option: WITH_POINT_COUNT numeric_expression@>@/        
{

   static_cast<Scanner_Node>(parameter)->cone_set_option_struct->base_point_count
      = static_cast<unsigned int>(floor(fabs(@=$2@>) + .5)); 

   @=$$@> = static_cast<void*>(0);

};

@q *** (3) set_cone_option_list --> WITH_DIVISIONS numeric_expression.@>   
@*2 \§set cone option list> $\longrightarrow$ \.{WITH\_DIVISIONS}
\§numeric expresssion>.
\initials{LDF 2006.11.08.}

\LOG
\initials{LDF 2006.11.08.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_cone_option: WITH_DIVISIONS numeric_expression@>@/        
{

   static_cast<Scanner_Node>(parameter)->cone_set_option_struct->divisions
      = static_cast<unsigned int>(floor(fabs(@=$2@>) + .5)); 

   @=$$@> = static_cast<void*>(0);

};


@q ** (2) set_cone_option_list --> WITH_TYPE cone_type.@>   
@* \§set cone option list> $\longrightarrow$ \.{WITH\_TYPE}
\§cone type>.
\initials{LDF 2006.11.08.}

\LOG
\initials{LDF 2006.11.08.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_cone_option: WITH_TYPE cone_type@>@/        
{

   static_cast<Scanner_Node>(parameter)->cone_set_option_struct->type 
      = @=$2@>;

   @=$$@> = static_cast<void*>(0);

};


@q * (1) cone_type.@> 
@* \§cone type>.
\initials{LDF 2006.11.08.}

\LOG
\initials{LDF 2006.11.08.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <int_value> cone_type@>


@q ** (2) cone_type: ELLIPTICAL.@> 
@*1 \§cone type> $\longrightarrow$ \.{ELLIPTICAL}.
\initials{LDF 2006.11.08.}

\LOG
\initials{LDF 2006.11.08.}
Added this rule.
\ENDLOG

@<Define rules@>= 
@=cone_type: ELLIPTICAL@>@/
{

   @=$$@> = Shape::ELLIPTICAL_CONE_TYPE;

};

@q ** (2) cone_type: CIRCULAR.@> 
@*1 \§cone type> $\longrightarrow$ \.{CIRCULAR}.
\initials{LDF 2006.11.08.}

\LOG
\initials{LDF 2006.11.08.}
Added this rule.
\ENDLOG

@<Define rules@>= 
@=cone_type: CIRCULAR@>@/
{

   @=$$@> = Shape::CIRCULAR_CONE_TYPE;

};


@q ** (2) cone_type: PARABOLIC.@> 
@*1 \§cone type> $\longrightarrow$ \.{PARABOLIC}.
\initials{LDF 2006.11.08.}

\LOG
\initials{LDF 2006.11.08.}
Added this rule.
\ENDLOG

@<Define rules@>= 
@=cone_type: PARABOLIC@>@/
{

   @=$$@> = Shape::PARABOLIC_CONE_TYPE;

};

@q ** (2) cone_type: HYPERBOLIC.@> 
@*1 \§cone type> $\longrightarrow$ \.{HYPERBOLIC}.
\initials{LDF 2006.11.08.}

\LOG
\initials{LDF 2006.11.08.}
Added this rule.
\ENDLOG

@<Define rules@>= 
@=cone_type: HYPERBOLIC@>@/
{

   @=$$@> = Shape::HYPERBOLIC_CONE_TYPE;

};


@q ** (2) set_cone_option_list --> WITH_TYPE nap_type.@>   
@* \§set cone option list> $\longrightarrow$ \.{WITH\_TYPE}
\§nap type>.
\initials{LDF 2006.11.09.}

\LOG
\initials{LDF 2006.11.09.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_cone_option: WITH_TYPE nap_type@>@/        
{

   static_cast<Scanner_Node>(parameter)->cone_set_option_struct->nap_type 
      = @=$2@>;

   @=$$@> = static_cast<void*>(0);

};

@q * (1) nap_type.@> 
@* \§nap type>.
\initials{LDF 2006.11.08.}

\LOG
\initials{LDF 2006.11.09.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <int_value> nap_type@>


@q ** (2) nap_type: SINGLE_NAPPED.@> 
@*1 \§nap type> $\longrightarrow$ \.{SINGLE\_NAPPED}.
\initials{LDF 2006.11.09.}

\LOG
\initials{LDF 2006.11.09.}
Added this rule.
\ENDLOG

@<Define rules@>= 
@=nap_type: SINGLE_NAPPED@>@/
{

   @=$$@> = Cone::SINGLE_NAPPED_TYPE;

};

@q ** (2) nap_type: DOUBLE_NAPPED.@> 
@*1 \§nap type> $\longrightarrow$ \.{DOUBLE\_NAPPED}.
\initials{LDF 2006.11.09.}

\LOG
\initials{LDF 2006.11.09.}
Added this rule.
\ENDLOG

@<Define rules@>= 
@=nap_type: DOUBLE_NAPPED@>@/
{

   @=$$@> = Cone::DOUBLE_NAPPED_TYPE;

};


@q ** (2) cylinders.@>  
@*1 {\bf cylinders}.
\initials{LDF 2005.05.25.}

\LOG
\initials{LDF 2005.05.25.}
Added this section.
\ENDLOG

@q *** (3) command --> SET cylinder_variable @>  
@q *** (3) set_cylinder_option_list.             @> 

@*2 \§command> $\longrightarrow$ \.{SET} 
\§cylinder variable> \§set cylinder option list>.
\initials{LDF 2005.05.25.}

\LOG
\initials{LDF 2005.05.25.}
Added this rule.

\initials{LDF 2005.05.25.}
Commented-in the call to |Scan_Parse::set_cylinder_rule_func_0()|.
\ENDLOG 

@q *** (3) Definition.@>

@<Define rules@>= 
@=command: SET cylinder_variable set_cylinder_option_list@>@/
{

   set_cylinder_rule_func_0(@=$2@>, 
                            parameter);

   static_cast<Scanner_Node>(parameter)->cylinder_set_option_struct = 0;

   @=$$@> = static_cast<void*>(0);

};


@q * (1) set_cylinder_option_list.@>   
@* \§set cylinder option list>.
\initials{LDF 2005.05.25.}

\LOG
\initials{LDF 2005.05.25.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> set_cylinder_option_list@>

@q ** (2) set_cylinder_option_list --> EMPTY.@>   
@* \§set cylinder option list> $\longrightarrow$ \.{EMPTY}.
\initials{LDF 2005.05.25.}

\LOG
\initials{LDF 2005.05.25.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_cylinder_option_list: /* Empty  */@>@/        
{

   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

   if (scanner_node->cylinder_set_option_struct != static_cast<Cylinder_Set_Option_Struct*>(0))
      scanner_node->cylinder_set_option_struct->clear();

   else
      scanner_node->cylinder_set_option_struct = new Cylinder_Set_Option_Struct;


   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_cylinder_option_list --> set_cylinder_option_list @>   
@q ** (2) set_cylinder_option.                                  @>   

@* \§set cylinder option list> $\longrightarrow$ 
\§set cylinder option list> \§set cylinder option>. 
\initials{LDF 2005.05.25.}

\LOG
\initials{LDF 2005.05.25.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_cylinder_option_list: set_cylinder_option_list set_cylinder_option@>@/        
{

   @=$$@> = static_cast<void*>(0);

};

@q * (1) set_cylinder_option.@>   
@* \§set cylinder option>.
\initials{LDF 2005.05.25.}

\LOG
\initials{LDF 2005.05.25.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> set_cylinder_option@>

@q ** (2) set_cylinder_option_list --> WITH_CENTER point_expression.@>   
@* \§set cylinder option list> $\longrightarrow$ \.{WITH\_CENTER}
\§point expresssion>.
\initials{LDF 2005.05.25.}

\LOG
\initials{LDF 2005.05.25.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_cylinder_option: WITH_CENTER point_expression@>@/        
{

   static_cast<Scanner_Node>(parameter)->cylinder_set_option_struct->center 
      = static_cast<Point*>(@=$2@>); 

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_cylinder_option_list --> WITH_DIRECTION point_expression.@>   
@* \§set cylinder option list> $\longrightarrow$ \.{WITH\_DIRECTION}
\§point expresssion>.
\initials{LDF 2005.05.21.}

\LOG
\initials{LDF 2005.05.21.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_cylinder_option: WITH_DIRECTION point_expression@>@/        
{


   static_cast<Scanner_Node>(parameter)->cylinder_set_option_struct->direction 
      = static_cast<Point*>(@=$2@>); 

   @=$$@> = static_cast<void*>(0);


};

@q ** (2) set_cylinder_option_list --> WITH_RADIUS numeric_expression.@>   
@* \§set cylinder option list> $\longrightarrow$ \.{WITH\_RADIUS}
\§numeric expresssion>.
\initials{LDF 2006.11.06.}

\LOG
\initials{LDF 2006.11.06.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_cylinder_option: WITH_RADIUS numeric_expression@>@/        
{

   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);
 
    if (scanner_node->cylinder_set_option_struct->radius == static_cast<real*>(0))
       scanner_node->cylinder_set_option_struct->radius = new real(@=$2@>); 
    else 
       *scanner_node->cylinder_set_option_struct->radius = @=$2@>; 


   @=$$@> = static_cast<void*>(0);

};


@q ** (2) set_cylinder_option_list --> WITH_AXIS_X numeric_expression.@>   
@* \§set cylinder option list> $\longrightarrow$ \.{WITH\_AXIS\_X}
\§numeric expresssion>.
\initials{LDF 2006.11.06.}

\LOG
\initials{LDF 2006.11.06.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_cylinder_option: WITH_AXIS_X numeric_expression@>@/        
{

   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);
 
    if (scanner_node->cylinder_set_option_struct->axis_x == static_cast<real*>(0))
       scanner_node->cylinder_set_option_struct->axis_x = new real(@=$2@>); 
    else 
       *scanner_node->cylinder_set_option_struct->axis_x = @=$2@>; 

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_cylinder_option_list --> WITH_AXIS_Y numeric_expression.@>   
@* \§set cylinder option list> $\longrightarrow$ \.{WITH\_AXIS\_Y}
\§numeric expresssion>.
\initials{LDF 2006.11.06.}

\LOG
\initials{LDF 2006.11.06.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_cylinder_option: WITH_AXIS_Y numeric_expression@>@/        
{

   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);
 
    if (scanner_node->cylinder_set_option_struct->axis_y == static_cast<real*>(0))
       scanner_node->cylinder_set_option_struct->axis_y = new real(@=$2@>); 
    else 
       *scanner_node->cylinder_set_option_struct->axis_y = @=$2@>; 

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_cylinder_option_list --> WITH_AXIS_Z numeric_expression.@>   
@* \§set cylinder option list> $\longrightarrow$ \.{WITH\_AXIS\_Z}
\§numeric expresssion>.
\initials{LDF 2006.11.06.}

\LOG
\initials{LDF 2006.11.06.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_cylinder_option: WITH_AXIS_Z numeric_expression@>@/        
{

   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);
 
    if (scanner_node->cylinder_set_option_struct->axis_z == static_cast<real*>(0))
       scanner_node->cylinder_set_option_struct->axis_z = new real(@=$2@>); 
    else 
       *scanner_node->cylinder_set_option_struct->axis_z = @=$2@>; 
    
   @=$$@> = static_cast<void*>(0);

};

@q *** (3) set_cylinder_option_list --> WITH_POINT_COUNT numeric_expression.@>   
@*2 \§set cylinder option list> $\longrightarrow$ \.{WITH\_POINT\_COUNT}
\§numeric expresssion>.
\initials{LDF 2006.11.06.}

\LOG
\initials{LDF 2006.11.06.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_cylinder_option: WITH_POINT_COUNT numeric_expression@>@/        
{

   static_cast<Scanner_Node>(parameter)->cylinder_set_option_struct->base_point_count
      = static_cast<unsigned int>(floor(fabs(@=$2@>) + .5)); 

   @=$$@> = static_cast<void*>(0);

};

@q *** (3) set_cylinder_option_list --> WITH_DIVISIONS numeric_expression.@>   
@*2 \§set cylinder option list> $\longrightarrow$ \.{WITH\_DIVISIONS}
\§numeric expresssion>.
\initials{LDF 2006.11.08.}

\LOG
\initials{LDF 2006.11.08.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_cylinder_option: WITH_DIVISIONS numeric_expression@>@/        
{

   static_cast<Scanner_Node>(parameter)->cylinder_set_option_struct->divisions
      = static_cast<unsigned int>(floor(fabs(@=$2@>) + .5)); 

   @=$$@> = static_cast<void*>(0);

};


@q ** (2) set_cylinder_option_list --> WITH_TYPE .@>   
@* \§set cylinder option list> $\longrightarrow$ \.{WITH\_TYPE}
\§cylinder type>.
\initials{LDF 2006.11.06.}

\LOG
\initials{LDF 2006.11.06.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_cylinder_option: WITH_TYPE cylinder_type@>@/        
{

   static_cast<Scanner_Node>(parameter)->cylinder_set_option_struct->type 
      = @=$2@>;

   @=$$@> = static_cast<void*>(0);

};


@q * (1) cylinder_type.@> 
@* \§cylinder type>.
\initials{LDF 2006.11.06.}

\LOG
\initials{LDF 2006.11.06.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <int_value> cylinder_type@>


@q ** (2) cylinder_type: ELLIPTICAL.@> 
@*1 \§cylinder type> $\longrightarrow$ \.{ELLIPTICAL}.
\initials{LDF 2006.11.06.}

\LOG
\initials{LDF 2006.11.06.}
Added this rule.
\ENDLOG

@<Define rules@>= 
@=cylinder_type: ELLIPTICAL@>@/
{

   @=$$@> = Shape::ELLIPTICAL_CYLINDER_TYPE;

};

@q ** (2) cylinder_type: CIRCULAR.@> 
@*1 \§cylinder type> $\longrightarrow$ \.{CIRCULAR}.
\initials{LDF 2006.11.06.}

\LOG
\initials{LDF 2006.11.06.}
Added this rule.
\ENDLOG

@<Define rules@>= 
@=cylinder_type: CIRCULAR@>@/
{

   @=$$@> = Shape::CIRCULAR_CYLINDER_TYPE;

};


@q ** (2) cylinder_type: PARABOLIC.@> 
@*1 \§cylinder type> $\longrightarrow$ \.{PARABOLIC}.
\initials{LDF 2006.11.06.}

\LOG
\initials{LDF 2006.11.06.}
Added this rule.
\ENDLOG

@<Define rules@>= 
@=cylinder_type: PARABOLIC@>@/
{

   @=$$@> = Shape::PARABOLIC_CYLINDER_TYPE;

};

@q ** (2) cylinder_type: HYPERBOLIC.@> 
@*1 \§cylinder type> $\longrightarrow$ \.{HYPERBOLIC}.
\initials{LDF 2006.11.06.}

\LOG
\initials{LDF 2006.11.06.}
Added this rule.
\ENDLOG

@<Define rules@>= 
@=cylinder_type: HYPERBOLIC@>@/
{

   @=$$@> = Shape::HYPERBOLIC_CYLINDER_TYPE;

};




@q ** (2) ellipsoids.@>  
@*1 {\bf ellipsoids}.
\initials{LDF 2005.05.26.}

\LOG
\initials{LDF 2005.05.26.}
Added this section.
\ENDLOG

@q *** (3) command --> SET ellipsoid_variable @>  
@q *** (3) set_ellipsoid_option_list.             @> 

@*2 \§command> $\longrightarrow$ \.{SET} 
\§ellipsoid variable> \§set ellipsoid option list>.
\initials{LDF 2005.05.26.}

\LOG
\initials{LDF 2005.05.26.}
Added this rule.
\ENDLOG 

@q *** (3) Definition.@>

@<Define rules@>= 
@=command: SET ellipsoid_variable set_ellipsoid_option_list@>@/
{

   Scan_Parse::set_ellipsoid_rule_func_0(@=$2@>, 
                                         parameter);

   static_cast<Scanner_Node>(parameter)->ellipsoid_set_option_struct = 0;

   @=$$@> = static_cast<void*>(0);

};


@q * (1) set_ellipsoid_option_list.@>   
@* \§set ellipsoid option list>.
\initials{LDF 2005.05.26.}

\LOG
\initials{LDF 2005.05.26.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> set_ellipsoid_option_list@>

@q ** (2) set_ellipsoid_option_list --> EMPTY.@>   
@* \§set ellipsoid option list> $\longrightarrow$ \.{EMPTY}.
\initials{LDF 2005.05.26.}

\LOG
\initials{LDF 2005.05.26.}
Added this rule.

\initials{LDF 2005.10.31.}
Commented-in the code in this rule.  Removed debugging code.
\ENDLOG

@<Define rules@>=
@=set_ellipsoid_option_list: /* Empty  */@>@/        
{

   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

   if (scanner_node->ellipsoid_set_option_struct != static_cast<Ellipsoid_Set_Option_Struct*>(0))
      scanner_node->ellipsoid_set_option_struct->clear();

   else
      scanner_node->ellipsoid_set_option_struct = new Ellipsoid_Set_Option_Struct;

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_ellipsoid_option_list --> set_ellipsoid_option_list @>   
@q ** (2) set_ellipsoid_option.                               @>   

@* \§set ellipsoid option list> $\longrightarrow$ 
\§set ellipsoid option list> \§set ellipsoid option>. 
\initials{LDF 2005.05.26.}

\LOG
\initials{LDF 2005.05.26.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_ellipsoid_option_list: set_ellipsoid_option_list set_ellipsoid_option@>@/        
{
   @=$$@> = static_cast<void*>(0);

};

@q * (1) set_ellipsoid_option.@>   
@* \§set ellipsoid option>.
\initials{LDF 2005.05.26.}

\LOG
\initials{LDF 2005.05.26.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> set_ellipsoid_option@>

@q ** (2) set_ellipsoid_option_list --> WITH_CENTER point_expression.@>   
@* \§set ellipsoid option list> $\longrightarrow$ \.{WITH\_CENTER}
\§point expresssion>.
\initials{LDF 2005.05.26.}

\LOG
\initials{LDF 2005.05.26.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_ellipsoid_option: WITH_CENTER point_expression@>@/        
{

   static_cast<Scanner_Node>(parameter)->ellipsoid_set_option_struct->center 
      = static_cast<Point*>(@=$2@>); 

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) Axes.@> 
@*1 Axes.
\initials{LDF 2005.10.31.}

\LOG
\initials{LDF 2005.10.31.}
Added this section.
\ENDLOG

@q *** (3) set_ellipsoid_option_list --> WITH_AXIS_X point_expression.@>   
@*2 \§set ellipsoid option list> $\longrightarrow$ \.{WITH\_AXIS\_X}
\§numeric expresssion>.
\initials{LDF 2005.10.31.}

\LOG
\initials{LDF 2005.10.31.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_ellipsoid_option: WITH_AXIS_X numeric_expression@>@/        
{

   static_cast<Scanner_Node>(parameter)->ellipsoid_set_option_struct->axis_x 
      = @=$2@>; 

   @=$$@> = static_cast<void*>(0);

};


@q *** (3) set_ellipsoid_option_list --> WITH_AXIS_Y numeric_expression.@>   
@*2 \§set ellipsoid option list> $\longrightarrow$ \.{WITH\_AXIS\_Y}
\§numeric expresssion>.
\initials{LDF 2005.10.31.}

\LOG
\initials{LDF 2005.10.31.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_ellipsoid_option: WITH_AXIS_Y numeric_expression@>@/        
{

   static_cast<Scanner_Node>(parameter)->ellipsoid_set_option_struct->axis_y 
      = @=$2@>; 

   @=$$@> = static_cast<void*>(0);

};

@q *** (3) set_ellipsoid_option_list --> WITH_AXIS_Z numeric_expression.@>   
@*2 \§set ellipsoid option list> $\longrightarrow$ \.{WITH\_AXIS\_Z}
\§numeric expresssion>.
\initials{LDF 2005.10.31.}

\LOG
\initials{LDF 2005.10.31.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_ellipsoid_option: WITH_AXIS_Z numeric_expression@>@/        
{

   static_cast<Scanner_Node>(parameter)->ellipsoid_set_option_struct->axis_z 
      = @=$2@>; 

   @=$$@> = static_cast<void*>(0);

};


@q ** (2) Divisions.@> 
@*1 Divisions.
\initials{LDF 2005.10.31.}

\LOG
\initials{LDF 2005.10.31.}
Added this section.
\ENDLOG

@q *** (3) set_ellipsoid_option_list --> WITH_DIVISIONS_X numeric_expression.@>   
@*2 \§set ellipsoid option list> $\longrightarrow$ \.{WITH\_DIVISIONS\_X}
\§numeric expresssion>.
\initials{LDF 2005.10.31.}

\LOG
\initials{LDF 2005.10.31.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_ellipsoid_option: WITH_DIVISIONS_X numeric_expression@>@/        
{

   static_cast<Scanner_Node>(parameter)->ellipsoid_set_option_struct->divisions_x 
      = static_cast<unsigned short>(floor(fabs(@=$2@>) + .5)); 

   @=$$@> = static_cast<void*>(0);

};

@q *** (3) set_ellipsoid_option_list --> WITH_DIVISIONS_Y numeric_expression.@>   
@*2 \§set ellipsoid option list> $\longrightarrow$ \.{WITH\_DIVISIONS\_Y}
\§numeric expresssion>.
\initials{LDF 2005.10.31.}

\LOG
\initials{LDF 2005.10.31.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_ellipsoid_option: WITH_DIVISIONS_Y numeric_expression@>@/        
{

   static_cast<Scanner_Node>(parameter)->ellipsoid_set_option_struct->divisions_y 
      = static_cast<unsigned short>(floor(fabs(@=$2@>) + .5)); 

   @=$$@> = static_cast<void*>(0);

};

@q *** (3) set_ellipsoid_option_list --> WITH_DIVISIONS_Z numeric_expression.@>   
@*2 \§set ellipsoid option list> $\longrightarrow$ \.{WITH\_DIVISIONS\_Z}
\§numeric expresssion>.
\initials{LDF 2005.10.31.}

\LOG
\initials{LDF 2005.10.31.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_ellipsoid_option: WITH_DIVISIONS_Z numeric_expression@>@/        
{

   static_cast<Scanner_Node>(parameter)->ellipsoid_set_option_struct->divisions_z 
      = static_cast<unsigned short>(floor(fabs(@=$2@>) + .5)); 

   @=$$@> = static_cast<void*>(0);

};

@q *** (3) set_ellipsoid_option_list --> WITH_POINT_COUNT numeric_expression.@>   
@*2 \§set ellipsoid option list> $\longrightarrow$ \.{WITH\_POINT\_COUNT}
\§numeric expresssion>.
\initials{LDF 2005.10.31.}

\LOG
\initials{LDF 2005.10.31.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_ellipsoid_option: WITH_POINT_COUNT numeric_expression@>@/        
{

   static_cast<Scanner_Node>(parameter)->ellipsoid_set_option_struct->ellipse_point_count
      = static_cast<unsigned int>(floor(fabs(@=$2@>) + .5)); 

   @=$$@> = static_cast<void*>(0);

};
  
@q ** (2) spheres.@>  
@*1 {\bf spheres}.
\initials{LDF 2005.06.06.}

\LOG
\initials{LDF 2005.06.06.}
Added this section.
\ENDLOG

@q *** (3) command --> SET sphere_variable @>  
@q *** (3) set_sphere_option_list.             @> 

@*2 \§command> $\longrightarrow$ \.{SET} 
\§sphere variable> \§set sphere option list>.
\initials{LDF 2005.06.06.}

\LOG
\initials{LDF 2005.06.06.}
Added this rule.
\ENDLOG 

@q *** (3) Definition.@>

@<Define rules@>= 
@=command: SET sphere_variable set_sphere_option_list@>@/
{

   Scan_Parse::set_sphere_rule_func_0(@=$2@>, 
                                      parameter);

   static_cast<Scanner_Node>(parameter)->sphere_set_option_struct = 0;

   @=$$@> = static_cast<void*>(0);

};


@q ** (2) set_sphere_option_list.@>   
@*1 \§set sphere option list>.
\initials{LDF 2005.06.06.}

\LOG
\initials{LDF 2005.06.06.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> set_sphere_option_list@>

@q *** (3) set_sphere_option_list --> EMPTY.@>   
@*2 \§set sphere option list> $\longrightarrow$ \.{EMPTY}.
\initials{LDF 2005.06.06.}

\LOG
\initials{LDF 2005.06.06.}
Added this rule.

\initials{LDF 2005.10.19.}
Now compiling the code that checks whether |scanner_node->sphere_set_option_struct|
exists or not and proceeds accordingly.
\ENDLOG

@<Define rules@>=
@=set_sphere_option_list: /* Empty  */@>@/        
{
    Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

    if (scanner_node->sphere_set_option_struct != static_cast<Sphere_Set_Option_Struct*>(0))
       scanner_node->sphere_set_option_struct->clear();

    else
       scanner_node->sphere_set_option_struct = new Sphere_Set_Option_Struct;

    @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_sphere_option_list --> set_sphere_option_list @>   
@q ** (2) set_sphere_option.                                @>   

@* \§set sphere option list> $\longrightarrow$ 
\§set sphere option list> \§set sphere option>. 
\initials{LDF 2005.06.06.}

\LOG
\initials{LDF 2005.06.06.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_sphere_option_list: set_sphere_option_list set_sphere_option@>@/        
{

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_sphere_option.@>   
@*1 \§set sphere option>.
\initials{LDF 2005.06.06.}

\LOG
\initials{LDF 2005.06.06.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> set_sphere_option@>

@q *** (3) set_sphere_option_list --> WITH_CENTER point_expression.@>   
@*2 \§set sphere option list> $\longrightarrow$ \.{WITH\_CENTER}
\§point expresssion>.
\initials{LDF 2005.06.06.}

\LOG
\initials{LDF 2005.06.06.}
Added this rule.

\initials{LDF 2005.10.19.}
Now compiling the code that sets 
|scanner_node->sphere_set_option_struct->center| to 
|static_cast<Point*>(@=$2@>)|.
\ENDLOG

@<Define rules@>=
@=set_sphere_option: WITH_CENTER point_expression@>@/        
{
   static_cast<Scanner_Node>(parameter)->sphere_set_option_struct->center 
      = static_cast<Point*>(@=$2@>); 

   @=$$@> = static_cast<void*>(0);

};


@q *** (3) set_sphere_option_list --> WITH_DIVISIONS numeric_expression.@>   
@*2 \§set sphere option list> $\longrightarrow$ \.{WITH\_DIVISIONS}
\§numeric expresssion>.
\initials{LDF 2005.10.19.}

\LOG
\initials{LDF 2005.10.19.}
Added this rule.

\initials{LDF 2005.10.20.}
Now rounding |@=$2@>| before setting 
|scanner_node->sphere_set_option_struct->divisions| to it.

\initials{LDF 2005.10.25.}
Now setting |scanner_node->sphere_set_option_struct->divisions_vertical|
and
|scanner_node->sphere_set_option_struct->divisions_horizontal|,
instead of |scanner_node->sphere_set_option_struct->divisions|.
I've removed |unsigned int Sphere_Set_Option_Struct::divisions|.
\ENDLOG

@<Define rules@>=
@=set_sphere_option: WITH_DIVISIONS numeric_expression@>@/        
{
    Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

    scanner_node->sphere_set_option_struct->divisions_vertical
       = scanner_node->sphere_set_option_struct->divisions_horizontal
       = static_cast<unsigned int>(floor(fabs(@=$2@>) + .5)); 

    @=$$@> = static_cast<void*>(0);

};

@q *** (3) set_sphere_option_list --> WITH_DIVISIONS_VERTICAL numeric_expression.@>   
@*2 \§set sphere option list> $\longrightarrow$ \.{WITH\_DIVISIONS\_VERTICAL}
\§numeric expresssion>.
\initials{LDF 2005.10.19.}

\LOG
\initials{LDF 2005.10.19.}
Added this rule.

\initials{LDF 2005.10.20.}
Now rounding |@=$2@>| before setting 
|scanner_node->sphere_set_option_struct->divisions_vertical| to it.
\ENDLOG

@<Define rules@>=
@=set_sphere_option: WITH_DIVISIONS_VERTICAL numeric_expression@>@/        
{

    static_cast<Scanner_Node>(parameter)->sphere_set_option_struct->divisions_vertical
       = static_cast<unsigned int>(floor(fabs(@=$2@>) + .5)); 

   @=$$@> = static_cast<void*>(0);

};

@q *** (3) set_sphere_option_list --> WITH_DIVISIONS_HORIZONTAL numeric_expression.@>   
@*2 \§set sphere option list> $\longrightarrow$ \.{WITH\_DIVISIONS\_HORIZONTAL}
\§numeric expresssion>.
\initials{LDF 2005.10.19.}

\LOG
\initials{LDF 2005.10.19.}
Added this rule.

\initials{LDF 2005.10.20.}
Now rounding |@=$2@>| before setting 
|scanner_node->sphere_set_option_struct->divisions_vertical| to it.
\ENDLOG

@<Define rules@>=
@=set_sphere_option: WITH_DIVISIONS_HORIZONTAL numeric_expression@>@/        
{

    static_cast<Scanner_Node>(parameter)->sphere_set_option_struct->divisions_horizontal
       = static_cast<unsigned int>(floor(fabs(@=$2@>) + .5)); 

    @=$$@> = static_cast<void*>(0);

};

@q *** (3) set_sphere_option_list --> WITH_RADIUS numeric_expression.@>   
@*2 \§set sphere option list> $\longrightarrow$ \.{WITH\_RADIUS}
\§numeric expresssion>.
\initials{LDF 2005.10.28.}

\LOG
\initials{LDF 2005.10.28.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_sphere_option: WITH_RADIUS numeric_expression@>@/        
{

   static_cast<Scanner_Node>(parameter)->sphere_set_option_struct->radius = @=$2@>; 

   @=$$@> = static_cast<void*>(0);

};

@q *** (3) set_sphere_option_list --> WITH_DIAMETER numeric_expression.@>   
@*2 \§set sphere option list> $\longrightarrow$ \.{WITH\_DIAMETER}
\§numeric expresssion>.
\initials{LDF 2005.10.20.}

\LOG
\initials{LDF 2005.10.20.}
Added this rule.

\initials{LDF 2005.10.28.}
Now setting |scanner_node->sphere_set_option_struct->radius|
instead of 
|scanner_node->sphere_set_option_struct->diameter|.
\ENDLOG

@<Define rules@>=
@=set_sphere_option: WITH_DIAMETER numeric_expression@>@/        
{

   static_cast<Scanner_Node>(parameter)->sphere_set_option_struct->radius = .5 * @=$2@>; 

   @=$$@> = static_cast<void*>(0);

};


@q *** (3) set_sphere_option --> WITH_POINT_COUNT numeric_expression.@>   
@*2 \§set sphere option> $\longrightarrow$ \.{WITH\_POINT\_COUNT}
\§numeric expresssion>.
\initials{LDF 2005.10.20.}

\LOG
\initials{LDF 2005.10.20.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_sphere_option: WITH_POINT_COUNT numeric_expression@>@/        
{


   static_cast<Scanner_Node>(parameter)->sphere_set_option_struct->circle_point_count
      = static_cast<unsigned int>(floor(fabs(@=$2@>) + .5)); 

   @=$$@> = static_cast<void*>(0);

};


@q *** (3) set_sphere_option_list --> WITH_TYPE sphere_type@>   
@*2 \§set sphere option list> $\longrightarrow$ \§sphere type>.
\initials{LDF 2005.10.25.}

\LOG
\initials{LDF 2005.10.25.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_sphere_option: WITH_TYPE sphere_type@>@/        
{
   static_cast<Scanner_Node>(parameter)->sphere_set_option_struct->sphere_type 
      = @=$2@>;

   @=$$@> = static_cast<void*>(0);

};

@q *** (3) sphere_type.@>  
@*4 \§sphere type>.
\initials{LDF 2005.10.25.}

\LOG
\initials{LDF 2005.10.25.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <int_value> sphere_type@>  

@q **** (4) sphere_type --> SPHERE_GLOBE_TYPE.@> 
@*5 \§sphere type> $\longrightarrow$ \.{SPHERE\_GLOBE\_TYPE}.
\initials{LDF 2005.10.25.}

\LOG
\initials{LDF 2005.10.25.}
Added this rule.
\ENDLOG
 
@<Define rules@>=
@=sphere_type: SPHERE_GLOBE_TYPE@>@/        
{

   @=$$@> = Sphere::GLOBE_TYPE;

};


@q **** (4) sphere_type --> SPHERE_PANEL_TYPE.@> 
@*5 \§sphere type> $\longrightarrow$ \.{SPHERE\_PANEL\_TYPE}.
\initials{LDF 2005.10.25.}

\LOG
\initials{LDF 2005.10.25.}
Added this rule.
\ENDLOG
 
@<Define rules@>=
@=sphere_type: SPHERE_PANEL_TYPE@>@/        
{

   @=$$@> = Sphere::PANEL_TYPE;

};

@q ** (2) sphere_developments.@>  
@*1 {\bf sphere\_developments}.
\initials{LDF 2009.11.16.}

\LOG
\initials{LDF 2009.11.16.}
Added this section.
\ENDLOG

@q *** (3) command --> SET sphere_development_variable @>  
@q *** (3) set_sphere_development_option_list.             @> 

@*2 \§command> $\longrightarrow$ \.{SET} 
\§sphere development variable> \§set sphere_development option list>.
\initials{LDF 2009.11.16.}

\LOG
\initials{LDF 2009.11.16.}
Added this rule.
\ENDLOG 

@q *** (3) Definition.@>

@<Define rules@>= 
@=command: SET sphere_development_variable set_sphere_development_option_list@>@/
{

   Scan_Parse::set_sphere_development_rule_func_0(@=$2@>, 
                                                  parameter);

   static_cast<Scanner_Node>(parameter)->sphere_development_set_option_struct = 0;

   @=$$@> = static_cast<void*>(0);

};


@q ** (2) set_sphere_development_option_list.@>   
@*1 \§set sphere_development option list>.
\initials{LDF 2009.11.16.}

\LOG
\initials{LDF 2009.11.16.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> set_sphere_development_option_list@>

@q *** (3) set_sphere_development_option_list --> EMPTY.@>   
@*2 \§set sphere_development option list> $\longrightarrow$ \.{EMPTY}.
\initials{LDF 2009.11.16.}

\LOG
\initials{LDF 2009.11.16.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_sphere_development_option_list: /* Empty  */@>@/        
{
    Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

    if (   scanner_node->sphere_development_set_option_struct 
        != static_cast<Sphere_Development_Set_Option_Struct*>(0))
       scanner_node->sphere_development_set_option_struct->clear();

    else
       scanner_node->sphere_development_set_option_struct 
          = new Sphere_Development_Set_Option_Struct;

    @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_sphere_development_option_list -->  @>   
@q ** (2) set_sphere_development_option_list set_sphere_development_option. @>   

@* \§set sphere_development option list> $\longrightarrow$ 
\§set sphere_development option list> \§set sphere_development option>. 
\initials{LDF 2009.11.16.}

\LOG
\initials{LDF 2009.11.16.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_sphere_development_option_list: set_sphere_development_option_list @>        
@=set_sphere_development_option@>@/
{

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) set_sphere_development_option.@>   
@*1 \§set sphere_development option>.
\initials{LDF 2009.11.16.}

\LOG
\initials{LDF 2009.11.16.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> set_sphere_development_option@>



@q *** (3) set_sphere_development_option_list --> WITH_RADIUS numeric_expression.@>   
@*2 \§set sphere_development option list> $\longrightarrow$ \.{WITH\_RADIUS}
\§numeric expresssion>.
\initials{LDF 2009.11.16.}

\LOG
\initials{LDF 2009.11.16.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_sphere_development_option: WITH_RADIUS numeric_expression@>@/        
{

   static_cast<Scanner_Node>(parameter)->sphere_development_set_option_struct->radius 
      = @=$2@>; 

   @=$$@> = static_cast<void*>(0);

};

@q *** (3) set_sphere_development_option_list --> WITH_DIAMETER numeric_expression.@>   
@*2 \§set sphere_development option list> $\longrightarrow$ \.{WITH\_DIAMETER}
\§numeric expresssion>.
\initials{LDF 2009.11.16.}

\LOG
\initials{LDF 2009.11.16.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_sphere_development_option: WITH_DIAMETER numeric_expression@>@/        
{

   static_cast<Scanner_Node>(parameter)->sphere_development_set_option_struct->radius 
      = .5 * @=$2@>; 

   @=$$@> = static_cast<void*>(0);

};

@q *** (3) set_sphere_development_option_list --> WITH_DIVISIONS_LATITUDE numeric_expression.@>   
@*2 \§set sphere_development option list> $\longrightarrow$ \.{WITH\_DIVISIONS\_LATITUDE}
\§numeric expresssion>.
\initials{LDF 2009.11.16.}

\LOG
\initials{LDF 2009.11.16.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_sphere_development_option: WITH_DIVISIONS_LATITUDE numeric_expression@>@/        
{
    Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

    scanner_node->sphere_development_set_option_struct->divisions_lat
       = static_cast<unsigned int>(floor(fabs(@=$2@>) + .5)); 

    @=$$@> = static_cast<void*>(0);

};

@q *** (3) set_sphere_development_option_list --> WITH_DIVISIONS_LONGITUDE numeric_expression.@>
@*2 \§set sphere_development option list> $\longrightarrow$ \.{WITH\_DIVISIONS\_LONGITUDE}
\§numeric expresssion>.
\initials{LDF 2009.11.18.}

\LOG
\initials{LDF 2009.11.18.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_sphere_development_option: WITH_DIVISIONS_LONGITUDE numeric_expression@>@/        
{
    Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

    scanner_node->sphere_development_set_option_struct->divisions_long
       = static_cast<unsigned int>(floor(fabs(@=$2@>) + .5)); 

    @=$$@> = static_cast<void*>(0);

};

@q *** (3) set_sphere_development_option --> WITH_POINT_COUNT numeric_expression.@>   
@*2 \§set sphere_development option> $\longrightarrow$ \.{WITH\_POINT\_COUNT}
\§numeric expresssion>.
\initials{LDF 2009.11.16.}

\LOG
\initials{LDF 2009.11.16.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_sphere_development_option: WITH_POINT_COUNT numeric_expression@>@/        
{


   static_cast<Scanner_Node>(parameter)->sphere_development_set_option_struct->path_point_count
      = static_cast<unsigned int>(floor(fabs(@=$2@>) + .5)); 

   @=$$@> = static_cast<void*>(0);

};

@q *** (3) set_sphere_development_option --> WITH_SEGMENT_COUNT numeric_expression.@>   
@*2 \§set sphere_development option> $\longrightarrow$ \.{WITH\_SEGMENT\_COUNT}
\§numeric expresssion>.
\initials{LDF 2009.11.18.}

\LOG
\initials{LDF 2009.11.18.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_sphere_development_option: WITH_SEGMENT_COUNT numeric_expression@>@/        
{

   static_cast<Scanner_Node>(parameter)->sphere_development_set_option_struct->segments
      = static_cast<unsigned int>(floor(fabs(@=$2@>) + .5)); 

   @=$$@> = static_cast<void*>(0);

};


@q *** (3) set_sphere_development_option --> WITH_TRANSFORM transform_expression.@>   
@*2 \§set sphere_development option> $\longrightarrow$ \.{WITH\_TRANSFORM}
\§transform expresssion>.
\initials{LDF 2009.11.18.}

\LOG
\initials{LDF 2009.11.18.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_sphere_development_option: WITH_TRANSFORM transform_expression@>@/        
{

   Transform* t = static_cast<Transform*>(@=$2@>);

   if (t != static_cast<Transform*>(0))
   {
       static_cast<Scanner_Node>(
          parameter)->sphere_development_set_option_struct->transform = *t;

       delete t;
       t = 0;
    
   }

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) paraboloids.@>  
@*1 {\bf paraboloids}.
\initials{LDF 2006.01.23.}

\LOG
\initials{LDF 2006.01.23.}
Added this section.
\ENDLOG

@q *** (3) command --> SET paraboloid_variable @>  
@q *** (3) set_paraboloid_option_list.             @> 

@*2 \§command> $\longrightarrow$ \.{SET} 
\§paraboloid variable> \§set paraboloid option list>.
\initials{LDF 2006.01.23.}

\LOG
\initials{LDF 2006.01.23.}
Added this rule.

\initials{LDF 2006.01.25.}
Working on this rule.
\ENDLOG 

@q *** (3) Definition.@>

@<Define rules@>= 
@=command: SET paraboloid_variable set_paraboloid_option_list@>@/
{
   Scan_Parse::set_paraboloid_rule_func_0(@=$2@>, 
                                          parameter);
   static_cast<Scanner_Node>(parameter)->paraboloid_set_option_struct = 0;

   @=$$@> = static_cast<void*>(0);

};

@q *** (3) set_paraboloid_option_list.@>   
@*2 \§set paraboloid option list>.
\initials{LDF 2006.01.23.}

\LOG
\initials{LDF 2006.01.23.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> set_paraboloid_option_list@>

@q **** (4) set_paraboloid_option_list --> EMPTY.@>   
@*3 \§set paraboloid option list> $\longrightarrow$ \.{EMPTY}.
\initials{LDF 2006.01.23.}

\LOG
\initials{LDF 2006.01.23.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_paraboloid_option_list: /* Empty  */@>@/        
{

    Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

    if (   scanner_node->paraboloid_set_option_struct 
        != static_cast<Paraboloid_Set_Option_Struct*>(0))
       scanner_node->paraboloid_set_option_struct->clear();

    else
       scanner_node->paraboloid_set_option_struct = new Paraboloid_Set_Option_Struct;

    @=$$@> = static_cast<void*>(0);

};

@q **** (4) set_paraboloid_option_list --> set_paraboloid_option_list @>   
@q **** (4) set_paraboloid_option.                                    @>   

@*3 \§set paraboloid option list> $\longrightarrow$ 
\§set paraboloid option list> \§set paraboloid option>. 
\initials{LDF 2006.01.23.}

\LOG
\initials{LDF 2006.01.23.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_paraboloid_option_list: set_paraboloid_option_list set_paraboloid_option@>@/        
{

   @=$$@> = static_cast<void*>(0);

};

@q *** (3) set_paraboloid_option.@>   
@*2 \§set paraboloid option>.
\initials{LDF 2006.01.23.}

\LOG
\initials{LDF 2006.01.23.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> set_paraboloid_option@>

@q **** (4) set_paraboloid_option_list --> WITH_TYPE paraboloid_type/@>   
@*3 \§set paraboloid option list> $\longrightarrow$ \.{WITH\_TYPE} \§paraboloid type>.
\initials{LDF 2006.01.25.}

\LOG
\initials{LDF 2006.01.25.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_paraboloid_option: WITH_TYPE paraboloid_type@>@/        
{

    static_cast<Scanner_Node>(parameter)->paraboloid_set_option_struct->type = @=$2@>;

};

@q *** (3) paraboloid_type.@> 
@*2 \§paraboloid type>.
\initials{LDF 2006.01.25.}

\LOG
\initials{LDF 2006.01.25.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <int_value> paraboloid_type@>


@q **** (4) paraboloid_type --> ELLIPTICAL.@> 
@*3 \§paraboloid type> $\longrightarrow$ \.{ELLIPTICAL}.
\initials{LDF 2006.01.25.}

@<Define rules@>=
@=paraboloid_type: ELLIPTICAL@>@/        
{

   @=$$@> = Paraboloid::ELLIPTICAL_TYPE;

};

@q **** (4) paraboloid_type --> HYPERBOLIC.@> 
@*3 \§paraboloid type> $\longrightarrow$ \.{HYPERBOLIC}.
\initials{LDF 2006.01.25.}

@<Define rules@>=
@=paraboloid_type: HYPERBOLIC@>@/        
{

   @=$$@> = Paraboloid::HYPERBOLIC_TYPE;

};

@q **** (4) set_paraboloid_option_list --> WITH_MAX_EXTENT numeric_expression/@>   
@*3 \§set paraboloid option list> $\longrightarrow$ \.{WITH\_MAX\_EXTENT} 
\§numeric expression>.
\initials{LDF 2006.01.25.}

\LOG
\initials{LDF 2006.01.25.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=set_paraboloid_option: WITH_MAX_EXTENT numeric_expression@>@/        
{

    static_cast<Scanner_Node>(parameter)->paraboloid_set_option_struct->max_extent = @=$2@>;

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
