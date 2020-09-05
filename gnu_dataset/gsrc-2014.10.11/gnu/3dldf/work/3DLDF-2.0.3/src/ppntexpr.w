@q ppntexpr.w @> 
@q Created by Laurence Finston Thu Apr 29 21:32:10 MEST 2004  @>
    
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



@q * (0) point expressions.  @>
@** \§point expressions>.

\LOG
\initials{LDF 2004.04.29.}  Created this file.  
It's included in that file by means of CWEB's ``\.{@@i}'' command.
\ENDLOG 

@q * (1) point primary.  @>
@* \§point primary>.
\initials{LDF Undated.}  

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> point_primary@>@/

@q ** (2) point_primary --> point_variable.@>
@*1 \§point primary> $\longrightarrow$ \§point variable>.  

\LOG
\initials{LDF 2004.05.03.}  
Added this rule.

\initials{LDF 2004.05.04.}  
Fixed the way |@=$$@>| is set.  |@=$1@>| 
is an |Id_Map_Entry_Node| cast to |void*|, so its |object| must be 
accessed in order to set |@=$$@>|, which is a |Point*| cast to |void*|.  

\initials{LDF 2004.05.04.}  
Added error handling.

\initials{LDF 2004.05.05.}  
@:BUG FIX@> BUG FIX:  Now creating a new |Point| on
the free store, assigning to it from |static_cast<Point*>(entry->object)|,
and pointing |@=$$@>| at it.  Formerly, I simply pointed |@=$$@>| at
|entry->object|.  Now I can always call |delete| on the pointers 
to temporary |Points| in the rules where they're used.

\initials{LDF 2004.10.27.}
@:BUG FIX@> BUG FIX:  Now allocating memory for a new |Point|, 
if the |object| of the |Id_Map_Entry_Node| referenced by |point_variable| is 0.

\initials{LDF 2004.11.22.}
@:BUG FIX@> BUG FIX:  Undid the last bug fix:  Now setting 
|point_primary| to 0 if |entry->object == 0|, and not allocating 
memory for a new |Point|.

\initials{LDF 2004.11.22.}
No longer issuing an error message if |entry->object == 0|.
This condition occurs legitimately when one tries to show an 
``unknown |point|''.
\ENDLOG 

@q *** (3) Definition.@> 

@<Define rules@>=
@=point_primary: point_variable@>@/ 
{
  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
      cerr_strm << thread_name 
                << "*** Parser: point_primary --> point_variable.";

      log_message(cerr_strm);
      cerr_message(cerr_strm);
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */@;

  entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

@q **** (4) |entry == 0 || entry->object == 0|.@>   

@ |entry == 0 || entry->object == 0|.
\initials{LDF 2004.11.22.}

@<Define rules@>=

   if (entry == static_cast<Id_Map_Entry_Node>(0) || entry->object == static_cast<void*>(0))
     {

        @=$$@> = static_cast<void*>(0);

     } /* |if (entry == 0 || entry->object == 0)|  */

@q **** (4) |entry != 0 && entry->object != 0|.@>   

@ |entry != 0 && entry->object != 0|.
\initials{LDF 2004.11.22.}

@<Define rules@>=

   else /* |entry != 0 && entry->object != 0|  */
      {
          

@q ***** (5) Try to allocate memory for a new |Point| for |point_primary|.@>        

@ Try to allocate memory for a new |Point| for |point_primary|.@>        
\initials{LDF 2004.10.27.}

@<Define rules@>=

         Point* q;

         try 
           {
             q = create_new<Point>(static_cast<Point*>(entry->object));
           }

         catch (bad_alloc)
           {
         
                 cerr_strm << "ERROR! In yyparse(), rule " 
                           << "`point_primary --> point_variable':"
                           << endl 
                           << "`create_new<Point>()' failed. "
                           << "Rethrowing `bad_alloc'.";

                    log_message(cerr_strm);
                    cerr_message(cerr_strm, error_stop_value);
                    cerr_strm.str("");

                    throw;

           } /* |catch (bad_alloc)|  */
                  
        @=$$@> = static_cast<void*>(q);           

   }  /* |else| (|entry != 0 && entry->object != 0|)  */


@q **** (4) @>   

};

@q ** (2) point_argument.@>
@*1 \§point argument>.  

\TODO
@q { @>
@:TO DO}{{\bf TO DO}@>
@q } @> 
@q !! TO DO:  @>
Program this case.  \initials{LDF 2004.04.29.}
\ENDTODO 

@q ** (2) point_primary --> numeric_list  @>
@*1 \§point primary> $\longrightarrow$ \§numeric list>.

\LOG
\initials{LDF 2004.05.12.}  
Added this rule.  It replaces the rule
\§point primary> $\longrightarrow$ \§numeric two to three>.

\initials{LDF 2004.09.24.}
Changed |w| from |Real_Pointer_Vector*| to |Pointer_Vector<real>*|.

\initials{LDF 2007.11.04.}
Removed code from this action.  Now calling 
|Scan_Parse::point_primary_rule_func_0|, which contains the old code.
\ENDLOG   

@<Define rules@>=
@=point_primary: numeric_list@>@/ 
{

   @=$$@> = point_primary_rule_func_0(@=$1@>, parameter);

};
 
@q ** (2) point_primary --> LEFT_PARENTHESIS point_expression @>
@q ** (2) RIGHT_PARENTHESIS.                                  @>
@*1 \§point primary> $\longrightarrow$ \.{\LP} 
\§point expression> \.{\RP}.
\initials{LDF Undated.}

\LOG
\initials{LDF 2005.02.15.}
@:BUG FIX@> BUG FIX:  Now setting |@=$$@>| to |@=$2@>| instead of 
|@=$1@>|.
Made debugging output thread-safe.

\initials{LDF 2007.06.19.}
Moved |@<Common declarations for rules@>| into the block of conditionally 
compiled and executed code with the debugging output.
\ENDLOG 

@<Define rules@>=
@=point_primary: LEFT_PARENTHESIS point_expression RIGHT_PARENTHESIS@>@/ 
{

  @=$$@> = @=$2@>;

};

@q ** (2) point_primary: MEDIATE LEFT_PARENTHESIS point_expression @>
@q ** (2) COMMA point_expression COMMA numeric_expression          @>
@q ** (2) RIGHT_PARENTHESIS.                                       @>

@ \§point primary> $\longrightarrow$ \.{MEDIATE} 
\.{LEFT\_PARENTHESIS} \§point expression> \.{COMMA} 
\§point expression> \.{COMMA} \§numeric expression> 
\.{RIGHT\_PARENTHESIS}.                                       

\LOG
\initials{LDF 2004.10.20.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=point_primary: MEDIATE LEFT_PARENTHESIS point_expression @>
@=COMMA point_expression COMMA numeric_expression RIGHT_PARENTHESIS@>
{

  @<Common declarations for rules@>@; 

  #if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
    if (DEBUG) 
      {
        cerr_strm << thread_name << "*** Parser: "
                  << "`point_primary: MEDIATE LEFT_PARENTHESIS"
                  << endl 
                  << "point_expression COMMA point_expression COMMA "
                  << "numeric_expression RIGHT_PARENTHESIS'.";
        
        log_message(cerr_strm);
        cerr_message(cerr_strm);
        cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */

   Point* p = static_cast<Point*>(@=$3@>);
   Point* q = static_cast<Point*>(@=$5@>); 

   real r = @=$7@>;

   *p = p->mediate(*q, r);

@q **** (4) Error handling:  |Point::mediate| returned |INVALID_POINT|.@>   

@ Error handling:  |Point::mediate| returned |INVALID_POINT|.
\initials{LDF 2004.10.20.}

@<Define rules@>=

    if (*p == INVALID_POINT)
      {

           cerr_strm << thread_name << "ERROR! In `yyparse()', rule "
                     << "`point_primary: MEDIATE LEFT_PARENTHESIS"
                     << endl 
                     << "point_expression COMMA point_expression COMMA "
                     << "numeric_expression RIGHT_PARENTHESIS':"
                     << endl  
                     << "`Point::mediate()' returned `INVALID_POINT'."
                     << endl << "Deleting the `point_expressions', "
                     << "setting `point_primary' to `static_cast<void*>(0)', "
                     << endl    
                     << "and exiting rule.";

           log_message(cerr_strm);
           cerr_message(cerr_strm, error_stop_value);
           cerr_strm.str("");
        
           delete p;
 
           delete q;
   
           @=$$@> = static_cast<void*>(0); 

      } /* |if (*p == INVALID_POINT)|  */


@q **** (4) |Point::mediate| succeeded.@>   

@ |Point::mediate| succeeded.  Delete |q| and set |point_primary| 
to |static_cast<void*>(p)|.
\initials{LDF 2004.10.20.}

@<Define rules@>=

    else /* |*p != INVALID_POINT|  */
       {

          delete q;

          @=$$@> = static_cast<void*>(p); 

       }  /* |else| (|*p != INVALID_POINT|)  */
   
@q **** (4) @>   

};


@q ** (2) point_primary: MEDIATE LEFT_PARENTHESIS point_expression @>
@q ** (2) COMMA point_expression RIGHT_PARENTHESIS.                @>


@ \§point primary> $\longrightarrow$ \.{MEDIATE} 
\.{LEFT\_PARENTHESIS} \§point expression> \.{COMMA} 
\§point expression> \.{RIGHT\_PARENTHESIS}.                                       

\LOG
\initials{LDF 2004.10.20.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=point_primary: MEDIATE LEFT_PARENTHESIS point_expression @>
@=COMMA point_expression RIGHT_PARENTHESIS@>
{

  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
    if (DEBUG) 
      {
        cerr_strm << thread_name << "*** Parser: "
                  << "`point_primary: MEDIATE LEFT_PARENTHESIS"
                  << endl 
                  << "point_expression COMMA point_expression "
                  << "RIGHT_PARENTHESIS'.";
        
        log_message(cerr_strm);
        cerr_message(cerr_strm);
        cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */

   Point* p = static_cast<Point*>(@=$3@>);
   Point* q = static_cast<Point*>(@=$5@>); 

   *p = p->mediate(*q, .5);

@q **** (4) Error handling:  |Point::mediate| returned |INVALID_POINT|.@>   

@ Error handling:  |Point::mediate| returned |INVALID_POINT|.
\initials{LDF 2004.10.20.}

@<Define rules@>=

    if (*p == INVALID_POINT)
      {

           cerr_strm << thread_name << "ERROR! In `yyparse()', rule "
                     << "`point_primary: MEDIATE LEFT_PARENTHESIS"
                     << endl 
                     << "point_expression COMMA point_expression COMMA "
                     << "numeric_expression RIGHT_PARENTHESIS':"
                     << endl  
                     << "`Point::mediate()' returned `INVALID_POINT'."
                     << endl << "Deleting the `point_expressions', "
                     << "setting `point_primary' to `static_cast<void*>(0)', "
                     << endl    
                     << "and exiting rule.";

           log_message(cerr_strm);
           cerr_message(cerr_strm, error_stop_value);
           cerr_strm.str("");
        
           delete p;
 
           delete q;
   
           @=$$@> = static_cast<void*>(0); 

      } /* |if (*p == INVALID_POINT)|  */


@q **** (4) |Point::mediate| succeeded.@>   

@ |Point::mediate| succeeded.  Delete |q| and set |point_primary| 
to |static_cast<void*>(p)|.
\initials{LDF 2004.10.20.}

@<Define rules@>=

    else /* |*p != INVALID_POINT|  */
       {

          delete q;

          @=$$@> = static_cast<void*>(p); 

       }  /* |else| (|*p != INVALID_POINT|)  */
   
@q **** (4) @>   

};

@q ** (2) point_primary --> GET_NORMAL path_primary@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_NORMAL} 
\§path primary>. 
\initials{LDF 2004.10.06.}

\LOG
\initials{LDF 2004.10.06.}
Added this rule.

\initials{LDF 2004.10.07.}
Removed \§optional of>.  
Changed |path_like_expression| to |path_like_primary|.

\initials{LDF 2005.10.24.}
Changed |path_like_primary| to |path_primary|.
Removed debugging code.

\initials{LDF 2005.12.09.}
Removed code from this rule.  Now calling |Scan_Parse::normal_func|.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=point_primary: GET_NORMAL path_primary@>@/
{
   @=$$@> = normal_func<Path>(static_cast<Path*>(@=$2@>), 
                              static_cast<Scanner_Node>(parameter));

};


@q ** (2) point_primary --> GET_PERPENDICULAR_BASE point_secondary COMMA plane_primary@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_PERPENDICULAR\_BASE} 
\filename{point secondary} \.{COMMA} \§plane primary>. 
\initials{LDF 2007.11.08.}

\LOG
\initials{LDF 2007.11.08.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=point_primary: GET_PERPENDICULAR_BASE point_secondary COMMA plane_primary@>@/
{
   Point* p = static_cast<Point*>(@=$2@>);
   Plane* q = static_cast<Plane*>(@=$4@>);

   if (p == static_cast<Point*>(0) || q == static_cast<Plane*>(0))
      @=$$@> = static_cast<void*>(0);
   else
   {

       Point* base = create_new<Point>(0);

       int status = p->get_perpendicular_base(*q, 
                                              base, 
                                              static_cast<Scanner_Node>(parameter));

       @=$$@> = (status == 0) ? static_cast<void*>(base) : 0;

   }  /* |else|  */

   delete p;
   delete q;


};

@q ** (2) point_primary --> GET_PERPENDICULAR_BASE point_secondary COMMA reg_polygon_primary@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_PERPENDICULAR\_BASE} 
\filename{point secondary} \.{COMMA} \§regular polygon primary>. 
\initials{LDF 2007.11.08.}

\LOG
\initials{LDF 2007.11.08.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=point_primary: GET_PERPENDICULAR_BASE point_secondary COMMA reg_polygon_primary@>@/
{
   Point* p       = static_cast<Point*>(@=$2@>);
   Reg_Polygon* r = static_cast<Reg_Polygon*>(@=$4@>);

   if (p == static_cast<Point*>(0) || r == static_cast<Reg_Polygon*>(0))
      @=$$@> = static_cast<void*>(0);
   else
   {

       Plane q = r->get_plane();

       Point* base = create_new<Point>(0);

       int status = p->get_perpendicular_base(q, 
                                              base, 
                                              static_cast<Scanner_Node>(parameter));

       @=$$@> = (status == 0) ? static_cast<void*>(base) : 0;


   }  /* |else|  */

   delete p;
   delete r;

};

@q ** (2) point_primary --> GET_POSITION focus_expression@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_POSITION} 
\§focus expression>. 
\initials{LDF 2007.09.02.}

\LOG
\initials{LDF 2007.09.02.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=point_primary: GET_POSITION focus_expression@>@/
{
   if (@=$2@>)
   {
      Focus* f = static_cast<Focus*>(@=$2@>);
      @=$$@> = static_cast<void*>(create_new<Point>(
                                     f->get_position())); 
      delete f;
   }
   else
      @=$$@> = static_cast<void*>(0);

};

@q ** (2) point_primary --> GET_DIRECTION focus_expression@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_DIRECTION} 
\§focus expression>. 
\initials{LDF 2007.09.02.}

\LOG
\initials{LDF 2007.09.02.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=point_primary: GET_DIRECTION focus_expression@>@/
{
   if (@=$2@>)
   {
      Focus* f = static_cast<Focus*>(@=$2@>);
      @=$$@> = static_cast<void*>(create_new<Point>(
                                     f->get_direction())); 
      delete f;
   }

   else
      @=$$@> = static_cast<void*>(0);

};

@q ** (2) point_primary --> GET_UP focus_expression@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_UP} 
\§focus expression>. 
\initials{LDF 2007.09.02.}

\LOG
\initials{LDF 2007.09.02.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=point_primary: GET_UP focus_expression@>@/
{
   if (@=$2@>)
   {
      Focus* f = static_cast<Focus*>(@=$2@>);
      @=$$@> = static_cast<void*>(create_new<Point>(
                                     f->get_up())); 
      delete f;
   }
   else
      @=$$@> = static_cast<void*>(0);

};

@q ** (2) point_primary --> GET_NORMAL ellipse_primary@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_NORMAL} 
\§ellipse primary>. 
\initials{LDF 2005.12.09.}

\LOG
\initials{LDF 2005.12.09.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=point_primary: GET_NORMAL ellipse_primary@>@/
{
   @=$$@> = normal_func<Ellipse>(static_cast<Ellipse*>(@=$2@>), 
                                 static_cast<Scanner_Node>(parameter));

};

@q ** (2) point_primary --> GET_NORMAL circle_primary@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_NORMAL} 
\§circle primary>. 
\initials{LDF 2005.12.09.}

\LOG
\initials{LDF 2005.12.09.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=point_primary: GET_NORMAL circle_primary@>@/
{
   @=$$@> = normal_func<Circle>(static_cast<Circle*>(@=$2@>), 
                                static_cast<Scanner_Node>(parameter));

};

@q ** (2) point_primary --> GET_NORMAL parabola_primary@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_NORMAL} 
\§parabola primary>. 
\initials{LDF 2005.12.09.}

\LOG
\initials{LDF 2005.12.09.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=point_primary: GET_NORMAL parabola_primary@>@/
{
   @=$$@> = normal_func<Parabola>(static_cast<Parabola*>(@=$2@>), 
                                  static_cast<Scanner_Node>(parameter));

};

@q ** (2) point_primary --> GET_NORMAL hyperbola_primary@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_NORMAL} 
\§hyperbola primary>. 
\initials{LDF 2005.12.09.}

\LOG
\initials{LDF 2005.12.09.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=point_primary: GET_NORMAL hyperbola_primary@>@/
{
   @=$$@> = normal_func<Hyperbola>(static_cast<Hyperbola*>(@=$2@>), 
                                   static_cast<Scanner_Node>(parameter));

};

@q ** (2) point_primary --> GET_NORMAL polygon_primary@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_NORMAL} 
\§polygon primary>. 
\initials{LDF 2005.12.09.}

\LOG
\initials{LDF 2005.12.09.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=point_primary: GET_NORMAL polygon_primary@>@/
{
   @=$$@> = normal_func<Polygon>(static_cast<Polygon*>(@=$2@>), 
                                 static_cast<Scanner_Node>(parameter));

};

@q ** (2) point_primary --> GET_NORMAL reg_polygon_primary@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_NORMAL} 
\§regular polygon primary>. 
\initials{LDF 2005.12.09.}

\LOG
\initials{LDF 2005.12.09.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=point_primary: GET_NORMAL reg_polygon_primary@>@/
{
   @=$$@> = normal_func<Reg_Polygon>(static_cast<Reg_Polygon*>(@=$2@>), 
                                 static_cast<Scanner_Node>(parameter));

};

@q ** (2) point_primary --> GET_NORMAL rectangle_primary@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_NORMAL} 
\§rectangle primary>. 
\initials{LDF 2005.12.09.}

\LOG
\initials{LDF 2005.12.09.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=point_primary: GET_NORMAL rectangle_primary@>@/
{
   @=$$@> = normal_func<Rectangle>(static_cast<Rectangle*>(@=$2@>), 
                                   static_cast<Scanner_Node>(parameter));

};

@q ** (2) point_primary --> GET_NORMAL triangle_primary@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_NORMAL} 
\§triangle primary>. 
\initials{LDF 2005.12.09.}

\LOG
\initials{LDF 2005.12.09.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=point_primary: GET_NORMAL triangle_primary@>@/
{
   @=$$@> = normal_func<Triangle>(static_cast<Triangle*>(@=$2@>), 
                                  static_cast<Scanner_Node>(parameter));

};

@q ** (2) point_primary --> GET_NORMAL focus_primary@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_NORMAL} 
\§focus primary>. 
\initials{LDF 2007.09.19.}

\LOG
\initials{LDF 2007.09.19.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=point_primary: GET_NORMAL focus_primary@>@/
{

   Focus* f = static_cast<Focus*>(@=$2@>);

   if (f)
   {
       Point p = f->get_normal();
       if (p == INVALID_POINT)
          @=$$@> = static_cast<void*>(0);
       else
          @=$$@> = static_cast<void*>(create_new<Point>(p));

       delete f;
   }
   else
      @=$$@> = static_cast<void*>(0);

};

@q ** (2) point_primary --> UNIT_VECTOR point_primary@>
@*1 \§point primary> $\longrightarrow$ \.{UNIT\_VECTOR} 
\§point primary>. 

\LOG
\initials{LDF 2004.10.06.}
Added this rule.

\initials{LDF 2004.10.07.}
Removed \§optional of>.  
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=point_primary: UNIT_VECTOR point_primary@>@/
{

  @<Common declarations for rules@>@; 

  #if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
    if (DEBUG) 
      {
        cerr_strm << thread_name << "*** Parser: "
                  << "`point_primary --> UNIT_VECTOR point_primary'.";
        

        log_message(cerr_strm);
        cerr_message(cerr_strm);
        cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */


  Point* p = static_cast<Point*>(@=$2@>);

  p->unit_vector(true);

  @=$$@> = static_cast<void*>(p); 

};


@q ** (2) point_primary --> GET_POINT numeric_secondary path_primary@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_POINT} 
\§numeric secondary> \§path primary>. 


@:!!@> !!  For some reason, a parse error occurs if \§numeric variables> aren't
enclosed in parentheses.  It doesn't happen with literal numbers, i.e.,  
\§numeric tokens>.  I don't know why this happens or how to fix it.
\initials{LDF 2004.10.21.}

\LOG
\initials{LDF 2004.10.20.}
Added this rule.

\initials{LDF 2004.10.21.}
Changed \.{INTEGER} to \§numeric expression>.  Now passing  
|static_cast<unsigned short>(floor(fabs(@=$2@>) + .5))| 
as the argument to |Path::get_point|. 

\initials{LDF 2004.12.10.}
Changed \§numeric expression> to \§numeric secondary>.

\initials{LDF 2005.11.09.}
Removed code.  Replaced it with a call to |Scan_Parse::get_point_func|.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=point_primary: GET_POINT numeric_secondary path_primary@>@/
{

    @=$$@> = Scan_Parse::get_point_func<Path>(@=$2@>, 
                                              static_cast<Path*>(@=$3@>), 
                                              parameter);
  
};

@q ** (2) point_primary --> GET_POINT numeric_secondary ellipse_primary@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_POINT} 
\§numeric secondary> \§ellipse primary>. 
\initials{LDF 2005.11.02.}

\LOG
\initials{LDF 2005.11.02.}
Added this rule.

\initials{LDF 2005.11.09.}
Removed code.  Replaced it with a call to |Scan_Parse::get_point_func|.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=point_primary: GET_POINT numeric_secondary ellipse_primary@>@/
{

    @=$$@> = Scan_Parse::get_point_func<Ellipse>(@=$2@>, 
                                                 static_cast<Ellipse*>(@=$3@>), 
                                                 parameter);
};

@q ** (2) point_primary --> GET_POINT numeric_secondary circle_primary@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_POINT} 
\§numeric secondary> \§circle primary>. 
\initials{LDF 2005.11.09.}

\LOG
\initials{LDF 2005.11.09.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=point_primary: GET_POINT numeric_secondary circle_primary@>@/
{

    @=$$@> = Scan_Parse::get_point_func<Circle>(@=$2@>, 
                                                 static_cast<Circle*>(@=$3@>), 
                                                 parameter);
};

@q ** (2) point_primary --> GET_POINT numeric_secondary parabola_primary@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_POINT} 
\§numeric secondary> \§parabola primary>. 
\initials{LDF 2005.11.09.}

\LOG
\initials{LDF 2005.11.09.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=point_primary: GET_POINT numeric_secondary parabola_primary@>@/
{

    @=$$@> = Scan_Parse::get_point_func<Parabola>(@=$2@>, 
                                                 static_cast<Parabola*>(@=$3@>), 
                                                 parameter);
};

@q ** (2) point_primary --> GET_POINT numeric_secondary hyperbola_primary@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_POINT} 
\§numeric secondary> \§hyperbola primary>. 
\initials{LDF 2005.11.28.}

\LOG
\initials{LDF 2005.11.28.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=point_primary: GET_POINT numeric_secondary hyperbola_primary@>@/
{

    @=$$@> = Scan_Parse::get_point_func<Hyperbola>(@=$2@>, 
                                                   static_cast<Hyperbola*>(@=$3@>), 
                                                   parameter);
};


@q ** (2) point_primary --> GET_APPROX_CENTER conic_section_lattice_expression@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_APPROX\_CENTER} 
\§conic section lattice expression>. 
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=point_primary: GET_APPROX_CENTER conic_section_lattice_expression@>@/
{
 
   Conic_Section_Lattice* c = static_cast<Conic_Section_Lattice*>(@=$2@>);

   if (c == static_cast<Conic_Section_Lattice*>(0))
      @=$$@> = static_cast<void*>(0);
   else
   {
       Point* p = create_new<Point>(c->get_approx_center());
       @=$$@> = static_cast<void*>(p); 
   }  

};

@q ** (2) point_primary --> GET_TRUE_CENTER conic_section_lattice_expression@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_TRUE\_CENTER} 
\§conic section lattice expression>. 
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=point_primary: GET_TRUE_CENTER conic_section_lattice_expression@>@/
{
 
   Conic_Section_Lattice* c = static_cast<Conic_Section_Lattice*>(@=$2@>);

   if (c == static_cast<Conic_Section_Lattice*>(0))
      @=$$@> = static_cast<void*>(0);
   else
   {
       Point* p = create_new<Point>(c->get_true_center());
       @=$$@> = static_cast<void*>(p); 
   }  

};

@q ** (2) point_primary --> GET_POINT_SIX conic_section_lattice_expression @>
@*1 \§point primary> $\longrightarrow$ \.{GET\_POINT\_SIX} 
\§conic section lattice expression>. 
\initials{LDF 2007.08.16.}

\LOG
\initials{LDF 2007.08.16.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=point_primary: GET_POINT_SIX conic_section_lattice_expression@>@/
{
 
   Conic_Section_Lattice* c = static_cast<Conic_Section_Lattice*>(@=$2@>);

   if (c == static_cast<Conic_Section_Lattice*>(0))
      @=$$@> = static_cast<void*>(0);
   else
   {
       Point* p = create_new<Point>(c->get_point_6());
       @=$$@> = static_cast<void*>(p); 
   }  

};

@q ** (2) point_primary --> GET_POINT conic_section_lattice_expression @>
@q ** (2) with_conic_section_lattice_option_list          @> 

@*1 \§point primary> $\longrightarrow$ \.{GET\_POINT} 
\§conic section lattice expression>
\§with conic section lattice option list>.
\initials{LDF 2007.08.03.}

\LOG
\initials{LDF 2007.08.03.}
Added this rule.

\initials{LDF 2007.08.05.}
Revised this rule.

\initials{LDF 2007.08.08.}
Changed \§conic section lattice variable> to \§conic section lattice primary>.

\initials{LDF 2007.08.19.}
Changed \§conic section lattice primary> to 
\§conic section lattice expression>.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=point_primary:  GET_POINT conic_section_lattice_expression @>@/
@=with_conic_section_lattice_option_list@>@/
{
  
   Conic_Section_Lattice* c 
      = static_cast<Conic_Section_Lattice*>(@=$2@>); 


   if (c)
   {
      @=$$@> = Scan_Parse::get_point_func<Conic_Section_Lattice>(
                              0,
                              c,
                              parameter);
      delete c;
      c = 0;
   }
   else
   {
       Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

       delete scanner_node->conic_section_lattice_options;  
       scanner_node->conic_section_lattice_options = 0;
       @=$$@> = static_cast<void*>(0);    
   }

};

@q ** (2) point_primary --> GET_LATTICE_POINT numeric_expression @>
@q ** (2) COMMA conic_section_lattice_expression                 @> 
@q ** (2) with_conic_section_lattice_option_list       @> 

@*1 \§point primary> $\longrightarrow$ \.{GET\_LATTICE\_POINT} 
\§numeric expression> \.{COMMA} \§conic section lattice expression>
\§with conic section lattice option list>.
\initials{LDF 2007.08.03.}

\LOG
\initials{LDF 2007.08.03.}
Added this rule.

\initials{LDF 2007.08.05.}
Revised this rule.

\initials{LDF 2007.08.08.}
Changed |numeric_tertiary| to |numeric_expression|
and |conic_section_lattice_variable| to |conic_section_lattice_expression|.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=point_primary: GET_LATTICE_POINT numeric_expression @>
@=COMMA conic_section_lattice_primary  @>@/
@=with_conic_section_lattice_option_list@>@/
{
    Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

    Conic_Section_Lattice* c 
       = static_cast<Conic_Section_Lattice*>(@=$4@>);

    if (c)
    {
       if (scanner_node && scanner_node->conic_section_lattice_options)
       {
          scanner_node->conic_section_lattice_options->lattice_point_flag 
             = true;
          scanner_node->conic_section_lattice_options->increment_vector.push_back(@=$2@>);
       }

       @=$$@> = Scan_Parse::get_point_func<Conic_Section_Lattice>(
                               0,
                               c,
                               parameter);

       delete c;
       c = 0;
    }
    else
    {
        if (scanner_node)
        {
           delete scanner_node->conic_section_lattice_options;  
           scanner_node->conic_section_lattice_options = 0;
        }

        @=$$@> = static_cast<void*>(0);    

    }  /* |else|  */

};

@q ** (2) point_primary --> GET_PASCAL_POINT LEFT_PARENTHESIS           @>
@q ** (2) numeric_expression COMMA numeric_expression RIGHT_PARENTHESIS @> 
@q ** (2) conic_section_lattice_expression.                             @>
@q ** (2) with_conic_section_lattice_option_list                        @>

@*1 \§point primary> $\longrightarrow$ \.{GET\_PASCAL\_POINT} 
\.{LEFT\_PARENTHESIS} \§numeric expression> \.{COMMA} 
\§numeric expression> \.{RIGHT\_PARENTHESIS} 
\§conic section lattice expression>
\§with conic section lattice option list>.
\initials{LDF 2007.08.19.}

\LOG
\initials{LDF 2007.08.19.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=point_primary:  GET_PASCAL_POINT LEFT_PARENTHESIS numeric_expression @>
@=COMMA numeric_expression RIGHT_PARENTHESIS conic_section_lattice_expression@>@/
@=with_conic_section_lattice_option_list@>@/
{

   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);
  
   Conic_Section_Lattice* c 
      = static_cast<Conic_Section_Lattice*>(@=$7@>); 

   if (c)
   {
      @=$$@> = c->get_pascal_point(@=$3@>, 
                                   @=$5@>, 
                                   scanner_node);

      delete c;
      c = 0;
   }
   else
      @=$$@> = static_cast<void*>(0);    
};

@q *** (3) with_conic_section_lattice_option_list and @> 
@q *** (3) with_conic_section_lattice_option@> 
@*2 \§with conic section lattice option list>
and
\§with conic section lattice option>.
\initials{LDF 2007.08.03.}

\LOG
\initials{LDF 2007.08.03.}
Added these type declarations.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> with_conic_section_lattice_option_list@>
@=%type <pointer_value> with_conic_section_lattice_option@>

@q *** (3) with_conic_section_lattice_option_list@> 
@*2 \§with conic section lattice option list>.
\initials{LDF 2007.08.03.}

@q **** (4) with_conic_section_lattice_option_list: /* Empty  */@> 
@*3 \§with conic section lattice option list> $\longrightarrow$ \.{EMPTY}.
\initials{LDF 2007.08.03.}

\LOG
\initials{LDF 2007.08.03.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=with_conic_section_lattice_option_list: /* Empty  */ @>@/
{
   
    static_cast<Scanner_Node>(parameter)->conic_section_lattice_options 
       = new Conic_Section_Lattice_Options;

};

@q **** (4) with_conic_section_lattice_option_list: @> 
@q **** (4) with_conic_section_lattice_option_list  @> 
@q **** (4) with_conic_section_lattice_option       @> 

@*3 \§with conic section lattice option list> $\longrightarrow$ 
\§with conic section lattice option list> 
\§with conic section lattice option>.
\initials{LDF 2007.08.03.}

\LOG
\initials{LDF 2007.08.03.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=with_conic_section_lattice_option_list: @>@/
@=with_conic_section_lattice_option_list @>@/
@=with_conic_section_lattice_option@>@/
{
   
    @=$$@> = static_cast<void*>(0);

};

@q **** (4) with_conic_section_lattice_option: WITH_CULL @> 

@*3 \§with conic section lattice option> $\longrightarrow$ 
\.{WITH\_CULL}.
\initials{LDF 2007.08.03.}

\LOG
\initials{LDF 2007.08.03.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=with_conic_section_lattice_option: WITH_CULL@>@/
{
    
   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

   if (   scanner_node != static_cast<Scanner_Node>(0) 
       &&    scanner_node->conic_section_lattice_options 
          != static_cast<Conic_Section_Lattice_Options*>(0))
      scanner_node->conic_section_lattice_options->do_cull = true;      

   @=$$@> = static_cast<void*>(0);
};

@q **** (4) with_conic_section_lattice_option: WITH_NO_CULL @> 

@*3 \§with conic section lattice option> $\longrightarrow$ 
\.{WITH\_NO\_CULL}.
\initials{LDF 2007.08.03.}

\LOG
\initials{LDF 2007.08.03.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=with_conic_section_lattice_option: WITH_NO_CULL@>@/
{
    
   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

   if (   scanner_node != static_cast<Scanner_Node>(0) 
       &&    scanner_node->conic_section_lattice_options 
          != static_cast<Conic_Section_Lattice_Options*>(0))
          scanner_node->conic_section_lattice_options->do_cull = false;      

   @=$$@> = static_cast<void*>(0);
};

@q **** (4) with_conic_section_lattice_option: @> 
@q **** (4) WITH_TOLERANCE numeric_expression            @> 

@*3 \§with conic section lattice option> $\longrightarrow$ 
\.{WITH\_TOLERANCE}.
\initials{LDF 2007.08.05.}

\LOG
\initials{LDF 2007.08.05.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=with_conic_section_lattice_option: WITH_TOLERANCE @>@/
@=numeric_expression@>@/
{
    
   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

   if (   scanner_node != static_cast<Scanner_Node>(0) 
       &&    scanner_node->conic_section_lattice_options 
          != static_cast<Conic_Section_Lattice_Options*>(0))
       scanner_node->conic_section_lattice_options->tolerance = @=$2@>;      

   @=$$@> = static_cast<void*>(0);
};




@q **** (4) with_conic_section_lattice_option: @> 
@q **** (4) WITH_CYCLE_VALUE numeric_expression          @> 

@*3 \§with conic section lattice option> $\longrightarrow$ 
\.{WITH\_CYCLE\_VALUE} \§numeric expression>.
\initials{LDF 2007.08.03.}

\LOG
\initials{LDF 2007.08.03.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=with_conic_section_lattice_option: @>@/
@=WITH_CYCLE_VALUE numeric_expression@>@/
{
    
   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

   real r = @=$1@>; 

   if (scanner_node && scanner_node->conic_section_lattice_options)
   {
       scanner_node->conic_section_lattice_options->cycle_value 
          = static_cast<signed short>(static_cast<int>(floor(fabs(@=$2@>) + .5))  
                 % 5);

       if (@=$1@> < 0) 
          scanner_node->conic_section_lattice_options->cycle_value *= -1;

   }

   @=$$@> = static_cast<void*>(0);
};

@q **** (4) with_conic_section_lattice_option: @> 
@q **** (4) WITH_INCREMENT numeric_expression            @> 

@*3 \§with conic section lattice option> $\longrightarrow$ 
\.{WITH\_INCREMENT} \§numeric expression>.
\initials{LDF 2007.08.03.}

\LOG
\initials{LDF 2007.08.03.}
Added this rule.

\initials{LDF 2007.08.05.}
Revised this rule.
\ENDLOG

@<Define rules@>=
@=with_conic_section_lattice_option: @>@/
@=WITH_INCREMENT numeric_expression@>@/
{
    
   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

   if (scanner_node && scanner_node->conic_section_lattice_options)
   {
        scanner_node->
           conic_section_lattice_options->
              increment_vector.push_back(@=$2@>);
   }

   @=$$@> = static_cast<void*>(0);
};


@q **** (4) with_conic_section_lattice_option: @> 
@q **** (4) WITH_INCREMENTS numeric_list                 @> 

@*3 \§with conic section lattice option> $\longrightarrow$ 
\.{WITH\_INCREMENTS} \§numeric list>.
\initials{LDF 2007.08.05.}

\LOG
\initials{LDF 2007.08.05.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=with_conic_section_lattice_option: @>@/
@=WITH_INCREMENTS numeric_list@>@/
{
    
   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

   Pointer_Vector<real>* pv 
      = static_cast<Pointer_Vector<real>*>(@=$2@>);        


   if (scanner_node && scanner_node->conic_section_lattice_options)
   {
       
        for (vector<real*>::iterator iter = pv->v.begin();
             iter != pv->v.end();
             ++iter)
        {
            scanner_node->
               conic_section_lattice_options->
                  increment_vector.push_back(**iter);
        }


   }  /* |if|  */

   delete pv;
   @=$$@> = static_cast<void*>(0);
};

@q **** (4) with_conic_section_lattice_option: @> 
@q **** (4) WITH_INCREMENTS numeric_vector_expression    @> 

@*3 \§with conic section lattice option> $\longrightarrow$ 
\.{WITH\_INCREMENTS} \§numeric vector expression>.
\initials{LDF 2007.08.05.}

\LOG
\initials{LDF 2007.08.05.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=with_conic_section_lattice_option: @>@/
@=WITH_INCREMENTS numeric_vector_expression@>@/
{
    
   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

   Pointer_Vector<real>* pv 
      = static_cast<Pointer_Vector<real>*>(@=$2@>);        

   if (scanner_node && scanner_node->conic_section_lattice_options)
   {
       
        for (vector<real*>::iterator iter = pv->v.begin();
             iter != pv->v.end();
             ++iter)
        {
            scanner_node->
               conic_section_lattice_options->
                  increment_vector.push_back(**iter);
        }


   }  /* |if|  */

   delete pv;
   @=$$@> = static_cast<void*>(0);
};

@q **** (4) with_conic_section_lattice_option: WITH_RECTIFY @> 

@*3 \§with conic section lattice option> $\longrightarrow$ 
\.{WITH\_RECTIFY}.
\initials{LDF 2007.08.03.}

\LOG
\initials{LDF 2007.08.03.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=with_conic_section_lattice_option: WITH_RECTIFY@>@/
{
    
   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

   if (   scanner_node != static_cast<Scanner_Node>(0) 
       &&    scanner_node->conic_section_lattice_options 
          != static_cast<Conic_Section_Lattice_Options*>(0))
      scanner_node->conic_section_lattice_options->do_rectify = true;      

   @=$$@> = static_cast<void*>(0);
};

@q **** (4) with_conic_section_lattice_option: WITH_NO_RECTIFY @> 

@*3 \§with conic section lattice option> $\longrightarrow$ 
\.{WITH\_NO\_RECTIFY}.
\initials{LDF 2007.08.03.}

\LOG
\initials{LDF 2007.08.03.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=with_conic_section_lattice_option: WITH_NO_RECTIFY@>@/
{
    
   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

   if (   scanner_node != static_cast<Scanner_Node>(0) 
       &&    scanner_node->conic_section_lattice_options 
          != static_cast<Conic_Section_Lattice_Options*>(0))
      scanner_node->conic_section_lattice_options->do_rectify = false;      

   @=$$@> = static_cast<void*>(0);
};

@q **** (4) with_conic_section_lattice_option: WITH_SHIFT.@> 

@*3 \§with conic section lattice option> $\longrightarrow$ 
\.{WITH\_SHIFT}.
\initials{LDF 2007.08.07.}

\LOG
\initials{LDF 2007.08.07.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=with_conic_section_lattice_option: WITH_SHIFT@>@/
{
    
   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

   if (scanner_node && scanner_node->conic_section_lattice_options)
        scanner_node->conic_section_lattice_options->do_shift = true;

   @=$$@> = static_cast<void*>(0);
};

@q **** (4) with_conic_section_lattice_option: WITH_NO_SHIFT.@> 

@*3 \§with conic section lattice option> $\longrightarrow$ 
\.{WITH\_NO\_SHIFT}.
\initials{LDF 2007.08.07.}

\LOG
\initials{LDF 2007.08.07.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=with_conic_section_lattice_option: WITH_NO_SHIFT@>@/
{
    
   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

   if (scanner_node && scanner_node->conic_section_lattice_options)
        scanner_node->conic_section_lattice_options->do_shift = false;

   @=$$@> = static_cast<void*>(0);
};

@q **** (4) with_conic_section_lattice_option: WITH_TEST @> 

@*3 \§with conic section lattice option> $\longrightarrow$ 
\.{WITH\_TEST}.
\initials{LDF 2007.08.03.}

\LOG
\initials{LDF 2007.08.03.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=with_conic_section_lattice_option: WITH_TEST@>@/
{
    
   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

   if (   scanner_node != static_cast<Scanner_Node>(0) 
       &&    scanner_node->conic_section_lattice_options 
          != static_cast<Conic_Section_Lattice_Options*>(0))
      scanner_node->conic_section_lattice_options->do_test = true;      

   @=$$@> = static_cast<void*>(0);
};

@q **** (4) with_conic_section_lattice_option: WITH_NO_TEST @> 

@*3 \§with conic section lattice option> $\longrightarrow$ 
\.{WITH\_NO\_TEST}.
\initials{LDF 2007.08.03.}

\LOG
\initials{LDF 2007.08.03.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=with_conic_section_lattice_option: WITH_NO_TEST@>@/
{
    
   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

   if (   scanner_node != static_cast<Scanner_Node>(0) 
       &&    scanner_node->conic_section_lattice_options 
          != static_cast<Conic_Section_Lattice_Options*>(0))
      scanner_node->conic_section_lattice_options->do_test = false;      

   @=$$@> = static_cast<void*>(0);
};

@q **** (4) with_conic_section_lattice_option: @> 
@q **** (4) WITH_TRANSFORM                               @> 

@*3 \§with conic section lattice option> $\longrightarrow$ 
\.{WITH\_TRANSFORM}.
\initials{LDF 2007.08.03.}

\LOG
\initials{LDF 2007.08.03.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=with_conic_section_lattice_option: WITH_TRANSFORM@>@/
{
    
   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

   if (   scanner_node != static_cast<Scanner_Node>(0) 
       &&    scanner_node->conic_section_lattice_options 
          != static_cast<Conic_Section_Lattice_Options*>(0))
      scanner_node->conic_section_lattice_options->do_transform = true;      

   @=$$@> = static_cast<void*>(0);
};


@q **** (4) with_conic_section_lattice_option: @> 
@q **** (4) WITH_TRANSFORM transform_primary             @> 

@*3 \§with conic section lattice option> $\longrightarrow$ 
\.{WITH\_TRANSFORM} \§transform primary>.
\initials{LDF 2007.08.03.}

\LOG
\initials{LDF 2007.08.03.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=with_conic_section_lattice_option: WITH_TRANSFORM transform_primary@>@/
{
    
   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

   if (   scanner_node != static_cast<Scanner_Node>(0) 
       &&    scanner_node->conic_section_lattice_options 
          != static_cast<Conic_Section_Lattice_Options*>(0))
   {
      scanner_node->conic_section_lattice_options->do_transform = true;      
      scanner_node->conic_section_lattice_options->transform 
         = static_cast<Transform*>(@=$2@>);      
   }

   @=$$@> = static_cast<void*>(0);
};

@q **** (4) with_conic_section_lattice_option: @> 
@q **** (4) WITH_NO_TRANSFORM                            @> 

@*3 \§with conic section lattice option> $\longrightarrow$ 
\.{WITH\_NO\_TRANSFORM}.
\initials{LDF 2007.08.03.}

\LOG
\initials{LDF 2007.08.03.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=with_conic_section_lattice_option: WITH_NO_TRANSFORM@>@/
{
    
   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

   if (   scanner_node != static_cast<Scanner_Node>(0) 
       &&    scanner_node->conic_section_lattice_options 
          != static_cast<Conic_Section_Lattice_Options*>(0))
   {
      scanner_node->conic_section_lattice_options->do_transform = false;      
   }

   @=$$@> = static_cast<void*>(0);
};

@q ** (2) point_primary --> GET_POINT numeric_secondary polygon_primary@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_POINT} 
\§numeric secondary> \§polygon primary>. 
\initials{LDF 2005.11.09.}

\LOG
\initials{LDF 2005.11.09.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=point_primary: GET_POINT numeric_secondary polygon_primary@>@/
{

    @=$$@> = Scan_Parse::get_point_func<Polygon>(@=$2@>, 
                                                 static_cast<Polygon*>(@=$3@>), 
                                                 parameter);
};

@q ** (2) point_primary --> GET_POINT numeric_secondary reg_polygon_primary@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_POINT} 
\§numeric secondary> \§regular polygon primary>. 
\initials{LDF 2005.11.09.}

\LOG
\initials{LDF 2005.11.09.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=point_primary: GET_POINT numeric_secondary reg_polygon_primary@>@/
{

    @=$$@> = Scan_Parse::get_point_func<Reg_Polygon>(@=$2@>, 
                                                 static_cast<Reg_Polygon*>(@=$3@>), 
                                                 parameter);
};

@q ** (2) point_primary --> GET_POINT numeric_secondary triangle_primary@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_POINT} 
\§numeric secondary> \§triangle primary>. 
\initials{LDF 2005.11.09.}

\LOG
\initials{LDF 2005.11.09.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=point_primary: GET_POINT numeric_secondary triangle_primary@>@/
{

    @=$$@> = Scan_Parse::get_point_func<Triangle>(@=$2@>, 
                                                 static_cast<Triangle*>(@=$3@>), 
                                                 parameter);
};

@q ** (2) point_primary --> GET_POINT numeric_secondary rectangle_primary@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_POINT} 
\§numeric secondary> \§rectangle primary>. 
\initials{LDF 2005.11.09.}

\LOG
\initials{LDF 2005.11.09.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=point_primary: GET_POINT numeric_secondary rectangle_primary@>@/
{

    @=$$@> = Scan_Parse::get_point_func<Rectangle>(@=$2@>, 
                                                 static_cast<Rectangle*>(@=$3@>), 
                                                 parameter);
};

@q ** (2) point_primary --> GET_LAST_POINT path_primary@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_LAST\_POINT} 
\§path primary>. 
\initials{LDF 2005.11.09.}

\LOG
\initials{LDF 2005.11.09.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=point_primary: GET_LAST_POINT path_primary@>@/
{

    @=$$@> = Scan_Parse::get_point_func<Path>(MAX_REAL,
                                              static_cast<Path*>(@=$2@>), 
                                              parameter);
  
};

@q ** (2) point_primary --> GET_LAST_POINT ellipse_primary@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_LAST\_POINT} 
\§ellipse primary>. 
\initials{LDF 2005.11.09.}

\LOG
\initials{LDF 2005.11.09.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=point_primary: GET_LAST_POINT ellipse_primary@>@/
{

    @=$$@> = Scan_Parse::get_point_func<Ellipse>(MAX_REAL, 
                                                 static_cast<Ellipse*>(@=$2@>), 
                                                 parameter);
};

@q ** (2) point_primary --> GET_LAST_POINT circle_primary@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_LAST\_POINT} 
\§circle primary>. 
\initials{LDF 2005.11.09.}

\LOG
\initials{LDF 2005.11.09.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=point_primary: GET_LAST_POINT circle_primary@>@/
{

    @=$$@> = Scan_Parse::get_point_func<Circle>(MAX_REAL, 
                                                 static_cast<Circle*>(@=$2@>), 
                                                 parameter);
};

@q ** (2) point_primary --> GET_LAST_POINT parabola_primary@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_LAST\_POINT} 
\§parabola primary>. 
\initials{LDF 2005.11.09.}

\LOG
\initials{LDF 2005.11.09.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=point_primary: GET_LAST_POINT parabola_primary@>@/
{

    @=$$@> = Scan_Parse::get_point_func<Parabola>(MAX_REAL, 
                                                 static_cast<Parabola*>(@=$2@>), 
                                                 parameter);
};

@q ** (2) point_primary --> GET_LAST_POINT polygon_primary@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_LAST\_POINT} 
\§polygon primary>. 
\initials{LDF 2005.11.09.}

\LOG
\initials{LDF 2005.11.09.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=point_primary: GET_LAST_POINT polygon_primary@>@/
{

    @=$$@> = Scan_Parse::get_point_func<Polygon>(MAX_REAL, 
                                                 static_cast<Polygon*>(@=$2@>), 
                                                 parameter);
};

@q ** (2) point_primary --> GET_LAST_POINT reg_polygon_primary@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_LAST\_POINT} 
\§regular polygon primary>. 
\initials{LDF 2005.11.09.}

\LOG
\initials{LDF 2005.11.09.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=point_primary: GET_LAST_POINT reg_polygon_primary@>@/
{

    @=$$@> = Scan_Parse::get_point_func<Reg_Polygon>(MAX_REAL, 
                                                 static_cast<Reg_Polygon*>(@=$2@>), 
                                                 parameter);
};

@q ** (2) point_primary --> GET_LAST_POINT triangle_primary@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_LAST\_POINT} 
\§triangle primary>. 
\initials{LDF 2005.11.09.}

\LOG
\initials{LDF 2005.11.09.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=point_primary: GET_LAST_POINT triangle_primary@>@/
{

    @=$$@> = Scan_Parse::get_point_func<Triangle>(MAX_REAL, 
                                                 static_cast<Triangle*>(@=$2@>), 
                                                 parameter);
};

@q ** (2) point_primary --> GET_LAST_POINT rectangle_primary@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_LAST\_POINT} 
\§rectangle primary>. 
\initials{LDF 2005.11.09.}

\LOG
\initials{LDF 2005.11.09.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=point_primary: GET_LAST_POINT rectangle_primary@>@/
{

    @=$$@> = Scan_Parse::get_point_func<Rectangle>(MAX_REAL, 
                                                 static_cast<Rectangle*>(@=$2@>), 
                                                 parameter);
};




@q ** (2) point_primary --> GET_CENTER ellipse_primary@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_CENTER} 
\§ellipse primary>. 

\LOG
\initials{LDF 2004.10.09.}
Added this rule.

\initials{LDF 2005.10.24.}
Changed |ellipse_like_primary| to |ellipse_primary|.
Removed debugging code.

\initials{LDF 2005.10.25.}
Removed code from the action of this rule.  Replaced it with a call 
to |Scan_Parse::get_center_func<Ellipse>|.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=point_primary: GET_CENTER ellipse_primary@>@/
{

   @=$$@> = Scan_Parse::get_center_func<Ellipse>(static_cast<Ellipse*>(@=$2@>));


};



@q ** (2) point_primary --> GET_CENTER circle_primary@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_CENTER} 
\§circle primary>. 

\LOG
\initials{LDF 2005.10.25.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=point_primary: GET_CENTER circle_primary@>@/
{

   @=$$@> = Scan_Parse::get_center_func<Circle>(static_cast<Circle*>(@=$2@>));

};

@q ** (2) point_primary --> GET_FOCUS numeric_primary ellipse_primary@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_FOCUS} 
\§numeric primary> \§ellipse primary>. 
\initials{LDF 2005.12.16.}

\LOG
\initials{LDF 2005.12.16.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=point_primary: GET_FOCUS numeric_primary ellipse_primary@>@/
{

   @=$$@> = Scan_Parse::get_focus_func<Ellipse>(static_cast<Ellipse*>(@=$3@>),
                           static_cast<unsigned short>(floor(fabs(@=$2@>) + .5)),
                           parameter);

};

@q ** (2) point_primary --> GET_FOCUS parabola_primary@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_FOCUS} 
\§parabola primary>. 
\initials{LDF 2005.11.09.}

\LOG
\initials{LDF 2005.11.09.}
Added this rule.

\initials{LDF 2005.11.14.}
Renamed |Scan_Parse::get_focus0_func| |get_focus_0_func|.

\initials{LDF 2005.11.17.}
Replaced |Scan_Parse::get_focus_0_func| and |Scan_Parse::get_focus_1_func| 
with |Scan_Parse::get_focus_func|.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=point_primary: GET_FOCUS parabola_primary@>@/
{

   @=$$@> = Scan_Parse::get_focus_func<Parabola>(static_cast<Parabola*>(@=$2@>),
                                                 0,
                                                 parameter);

};

@q ** (2) point_primary --> GET_VERTEX numeric_primary ellipse_primary@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_FOCUS} 
\§numeric primary> \§ellipse primary>. 
\initials{LDF 2005.12.16.}

\LOG
\initials{LDF 2005.12.16.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=point_primary: GET_VERTEX numeric_primary ellipse_primary@>@/
{

   @=$$@> = Scan_Parse::get_vertex_func<Ellipse>(static_cast<Ellipse*>(@=$3@>),
                           static_cast<unsigned short>(floor(fabs(@=$2@>) + .5)) ,
                           parameter);

};

@q ** (2) point_primary --> GET_VERTEX parabola_primary@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_FOCUS} 
\§parabola primary>. 
\initials{LDF 2005.11.09.}

\LOG
\initials{LDF 2005.11.09.}
Added this rule.

\initials{LDF 2005.11.14.}
Renamed |Scan_Parse::get_vertex_func| |get_vertex_0_func|.

\initials{LDF 2005.11.17.}
Renamed |Scan_Parse::get_vertex_0_func| |get_vertex_func|.

\initials{LDF 2005.11.17.}
Replaced |Scan_Parse::get_vertex_0_func| and |Scan_Parse::get_vertex_1_func| 
with |Scan_Parse::get_vertex_func|.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=point_primary: GET_VERTEX parabola_primary@>@/
{

   @=$$@> = Scan_Parse::get_vertex_func<Parabola>(static_cast<Parabola*>(@=$2@>),
                                                  0,
                                                  parameter);

};

@q ** (2) point_primary --> GET_FOCUS numeric_primary hyperbola_primary@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_FOCUS} 
\§numeric primary> \§hyperbola primary>. 
\initials{LDF 2005.11.15.}

\LOG
\initials{LDF 2005.11.15.}
Added this rule.

\initials{LDF 2005.11.17.}
Replaced |Scan_Parse::get_focus_0_func| and |Scan_Parse::get_focus_1_func| 
with |Scan_Parse::get_focus_func|.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=point_primary: GET_FOCUS numeric_primary hyperbola_primary@>@/
{

    @=$$@> = Scan_Parse::get_focus_func<Hyperbola>(static_cast<Hyperbola*>(@=$3@>),
                             static_cast<unsigned short>(floor(fabs(@=$2@>) + .5)),
                                                   parameter);
};

@q ** (2) point_primary --> GET_CENTER polygon_primary@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_CENTER} 
\§polygon primary>. 

\LOG
\initials{LDF 2004.10.09.}
Added this rule.

\initials{LDF 2005.10.24.}
Changed |polygon_like_primary| to |polygon_primary|.
Removed debugging code.

\initials{LDF 2005.10.25.}
Removed code from the action of this rule.  Replaced it with a call 
to |Scan_Parse::get_center_func<Polygon>|.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=point_primary: GET_CENTER polygon_primary@>@/
{

   @=$$@> = Scan_Parse::get_center_func<Polygon>(static_cast<Polygon*>(@=$2@>));

};

@q ** (2) point_primary --> GET_CENTER reg_polygon_primary@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_CENTER} 
\§regular polygon primary>. 

\LOG
\initials{LDF 2005.10.25.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=point_primary: GET_CENTER reg_polygon_primary@>@/
{

   @=$$@> = Scan_Parse::get_center_func<Reg_Polygon>(static_cast<Reg_Polygon*>(@=$2@>));


};

@q ** (2) point_primary --> GET_CENTER rectangle_primary@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_CENTER} 
\§rectangle primary>. 

\LOG
\initials{LDF 2005.10.25.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=point_primary: GET_CENTER rectangle_primary@>@/
{

   @=$$@> = Scan_Parse::get_center_func<Rectangle>(static_cast<Rectangle*>(@=$2@>));


};

@q *** (3) point_primary --> GET_CENTER triangle_primary@>
@*2 \§point primary> $\longrightarrow$ \.{GET\_CENTER} 
\§triangle primary>. 

\LOG
\initials{LDF 2005.10.25.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=point_primary: GET_CENTER triangle_primary@>@/
{

   @=$$@> = Scan_Parse::get_center_func<Triangle>(static_cast<Triangle*>(@=$2@>));


};

@q *** (3) point_primary --> GET_CENTER cuboid_primary@>
@*2 \§point primary> $\longrightarrow$ \.{GET\_CENTER} 
\§cuboid primary>. 
\initials{LDF 2004.12.15.}

\LOG
\initials{LDF 2004.12.15.}
Added this rule.

\initials{LDF 2005.10.22.}
Replaced |solid_like_primary| with |cuboid_primary|.

\initials{LDF 2005.10.25.}
Removed code from the action of this rule.  Replaced it with a call 
to |Scan_Parse::get_center_func<Cuboid>|.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=

@=point_primary: GET_CENTER cuboid_primary@>@/
{

   @=$$@> = Scan_Parse::get_center_func<Cuboid>(static_cast<Cuboid*>(@=$2@>));
};

@q *** (3) point_primary --> GET_CENTER polyhedron_primary@>
@*2 \§point primary> $\longrightarrow$ \.{GET\_CENTER} 
\§polyhedron primary>. 
\initials{LDF 2005.10.25.}

\LOG
\initials{LDF 2005.10.25.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=

@=point_primary: GET_CENTER polyhedron_primary@>@/
{

   @=$$@> = Scan_Parse::get_center_func<Polyhedron>(static_cast<Polyhedron*>(@=$2@>));

};

@q *** (3) point_primary --> GET_CENTER ellipsoid_primary@>
@*2 \§point primary> $\longrightarrow$ \.{GET\_CENTER} 
\§ellipsoid primary>. 
\initials{LDF 2006.11.06.}

\LOG
\initials{LDF 2006.11.06.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=

@=point_primary: GET_CENTER ellipsoid_primary@>@/
{

   @=$$@> = Scan_Parse::get_center_func<Ellipsoid>(static_cast<Ellipsoid*>(@=$2@>));


};


@q *** (3) point_primary --> GET_CENTER sphere_primary@>
@*2 \§point primary> $\longrightarrow$ \.{GET\_CENTER} 
\§sphere primary>. 

\LOG
\initials{LDF 2005.10.25.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=

@=point_primary: GET_CENTER sphere_primary@>@/
{

   @=$$@> = Scan_Parse::get_center_func<Sphere>(static_cast<Sphere*>(@=$2@>));


};


@q *** (3) point_primary --> GET_APEX cone_primary@>
@*2 \§point primary> $\longrightarrow$ \.{GET\_APEX} 
\§cone primary>. 
\initials{LDF 2006.11.09.}

\LOG
\initials{LDF 2006.11.09.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=

@=point_primary: GET_APEX cone_primary@>@/
{

   @=$$@> = quad_surf_get_element_func(parameter, 
                                       GET_APEX, 
                                       Shape::CONE_TYPE, 
                                       @=$2@>);


};

@q ** (2) point_primary --> GET_BASE_CENTER cone_primary@>
@*1 \§point primary> $\longrightarrow$ \.{GET\_BASE\_CENTER} 
\§cone primary>. 
\initials{LDF 2006.11.11.}

\LOG
\initials{LDF 2006.11.11.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=

@=point_primary: GET_BASE_CENTER cone_primary@>@/
{

   @=$$@> = static_cast<Cone*>(@=$2@>)->get_base_center(parameter);

};

@q *** (3) point_primary --> GET_CAP_CENTER cone_primary@>
@*2 \§point primary> $\longrightarrow$ \.{GET\_CAP\_CENTER} 
\§cone primary>. 
\initials{LDF 2006.11.11.}

\LOG
\initials{LDF 2006.11.11.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=

@=point_primary: GET_CAP_CENTER cone_primary@>@/
{

   @=$$@> = static_cast<Cone*>(@=$2@>)->get_cap_center(parameter);

};


@q *** (2) point_primary --> GET_CENTER cylinder_primary@>
@*2 \§point primary> $\longrightarrow$ \.{GET\_CENTER} 
\§cylinder primary>. 
\initials{LDF 2006.11.06.}

\LOG
\initials{LDF 2006.11.06.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=point_primary: GET_CENTER cylinder_primary@>@/
{

   @=$$@> = Scan_Parse::get_center_func<Cylinder>(static_cast<Cylinder*>(@=$2@>));


};


@q *** (3) point_primary --> GET_BASE_CENTER cylinder_primary@>
@*2 \§point primary> $\longrightarrow$ \.{GET\_BASE\_CENTER} 
\§cylinder primary>. 
\initials{LDF 2006.11.06.}

\LOG
\initials{LDF 2006.11.06.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=

@=point_primary: GET_BASE_CENTER cylinder_primary@>@/
{

   @=$$@> = static_cast<Cylinder*>(@=$2@>)->get_base_center(parameter);

};

@q *** (2) point_primary --> GET_CAP_CENTER cylinder_primary@>
@*2 \§point primary> $\longrightarrow$ \.{GET\_CAP\_CENTER} 
\§cylinder primary>. 
\initials{LDF 2006.11.06.}

\LOG
\initials{LDF 2006.11.06.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=point_primary: GET_CAP_CENTER cylinder_primary@>@/
{

   @=$$@> = static_cast<Cylinder*>(@=$2@>)->get_cap_center(parameter);


};

@q ** (2) point_primary --> LAST point_vector_expression.@>

@*1 \§point primary> $\longrightarrow$ 
\.{LAST} \§point vector expression>.
\initials{LDF 2005.01.08.}

\LOG
\initials{LDF 2005.01.08.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=point_primary: LAST point_vector_expression@>@/
{ 
   @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
   DEBUG = false; /* |true| */ @; 
   if (DEBUG) 
     {
         cerr_strm << thread_name << "*** Parser:  `point_primary "
                   << "--> LAST point_vector_expression'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str("");
     }
#endif /* |DEBUG_COMPILE|  */

@q ******* (7) @> 

   Point* p;

   try 
      {
         p = create_new<Point>(0);
      }
@q ******* (7) @> 
@

\LOG
\initials{LDF 2005.01.09.}
@:BUG FIX@> BUG FIX:  Added |throw|.  I'd forgotten it.
\ENDLOG 

@<Define rules@>=


   catch (bad_alloc)
      {
         cerr_strm << thread_name << "ERROR!  In `yyparse()', rule "
                   << "`point_primary "
                   << "--> LAST point_vector_expression':"
                   << endl << "`create_new<Point>()' failed.  "
                   << "Rethrowing `bad_alloc'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm, error_stop_value);
         cerr_strm.str("");

         throw;

      }  /* |catch (bad_alloc)|  */

@q ******* (7) @> 

   Pointer_Vector<Point>* pv 
      = static_cast<Pointer_Vector<Point>*>(@=$2@>);

@q ******* (7) Error handling:  |pv == 0|.@> 

@ Error handling:  |pv == 0|.
\initials{LDF 2005.01.08.}

@<Define rules@>=

   if (pv == static_cast<Pointer_Vector<Point>*>(0))
      {
          cerr_strm << thread_name 
                    << "ERROR!  In `yyparse()', rule "
                    << endl 
                    << "`point_primary "
                    << "--> LAST point_vector_expression':"
                    << endl << "Invalid `point_vector_expression'.  "
                    << "Setting `point_primary' to 0"
                    << endl << "and will try to continue.";

          log_message(cerr_strm);
          cerr_message(cerr_strm, error_stop_value);
          cerr_strm.str("");

          delete p;

          @=$$@> = static_cast<void*>(0);

      }  /* |if (pv == 0)|  */

@q ******* (7) Error handling:  |pv->ctr == 0|.@> 

@ Error handling:  |pv->ctr == 0|.
\initials{LDF 2005.01.08.}

@<Define rules@>=

   else if (pv->ctr == 0)
      {
          cerr_strm << thread_name 
                    << "ERROR!  In `yyparse()', rule "
                    << endl 
                    << "`point_primary "
                    << "--> LAST point_vector_expression':"
                    << endl << "`point_vector_expression' is empty.  "
                    << "Setting `point_primary' to 0"
                    << endl << "and will try to continue.";

          log_message(cerr_strm);
          cerr_message(cerr_strm, error_stop_value);
          cerr_strm.str("");

          delete p;

          @=$$@> = static_cast<void*>(0);

      }  /* |else if (pv->ctr == 0)|  */

@q ******* (7) |pv != 0 && pv->ctr > 0|.@> 

@ |pv != 0 && pv->ctr > 0|.  Set |@=$$@>| to |*(pv->v[pv->ctr - 1])|.
\initials{LDF 2005.01.08.}

@<Define rules@>=

   else 
      {
         *p = *(pv->v[pv->ctr - 1]);
         @=$$@> = static_cast<void*>(p); 
      }
@q ******* (7) @> 

};

@q ** (2) point_primary --> GET_ANGLE_POINT numeric_primary ellipse_expression.@>

@*1 \§point primary> $\longrightarrow$ 
\.{GET\_ANGLE\_POINT} \§numeric primary> \§ellipse expression>.
\initials{LDF 2010.08.26.}

@:??@> ?? I don't know why |Point q| is needed, but if I try to assign to |*p| from 
|Ellipse::angle_point|, it causes a ``Segmentation fault'' error.  \.{gdb} shows 
the error occurring in code that doesn't seem to be related.
\initials{LDF 2010.08.27.}

\LOG
\initials{LDF 2010.08.26.}
Added this rule.
\ENDLOG


@q ****** (6) Definition.@> 

@<Define rules@>=
@=point_primary: GET_ANGLE_POINT numeric_primary ellipse_expression@>@/
{ 
@q ******* (7) @>


   Ellipse* e = static_cast<Ellipse*>(@=$3@>); 

   if (e == static_cast<Ellipse*>(0))
      @=$$@> = static_cast<void*>(0); 
   else
   {
   
      Point* p = create_new<Point>(0);
      Point q = e->angle_point(@=$2@>);

      *p = q;

      delete e; 
      e = 0; 
      @=$$@> = static_cast<void*>(p); 
   }

@q ******* (7) @> 

};

@q ** (2) point secondary.  @>
@*1 \§point secondary>.
\initials{LDF Undated.}

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> point_secondary@>
  
@q *** (3) point secondary --> point_primary.@>
@*2 \§point secondary> $\longrightarrow$ \§point primary>.
\initials{LDF Undated.}

\LOG
\initials{LDF 2004.12.21.}
Made debugging output thread-safe.  
\ENDLOG 

@<Define rules@>=
@=point_secondary: point_primary@>@/ 
{
   @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
   DEBUG = false; /* |true| */ @; 
   if (DEBUG)
      {
          cerr_strm << thread_name 
                    << "*** Parser:  `point_secondary --> point_primary'.";

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */@;
  
  @=$$@> = @=$1@>;

};

@q *** (3) point secondary --> point_secondary times_or_over numeric_primary.@>

@*2 \§point secondary> $\longrightarrow$ \§point secondary> 
\§times or over> \§numeric primary>.

\LOG
\initials{LDF 2004.05.05.}  
Added this rule.

\initials{LDF 2004.10.05.}
Made debugging and error output thread-safe.  
Added {\TeX} text and edited the formatting.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=point_secondary: point_secondary times_or_over numeric_primary@>@/ 
{

  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
      cerr_strm << thread_name 
                << "*** Parser: `point_secondary --> "
                << "point_secondary times_or_over "
                << "numeric_primary.";

      log_message(cerr_strm);
      cerr_message(cerr_strm);
      cerr_strm.str("");

    }
#endif /* |DEBUG_COMPILE|  */@;

  Point* p = static_cast<Point*>(@=$1@>); 
  
@q ******* (7) Error handling:  |p == 0|.@> 

@ Error handling:  |p == 0|.
\initials{LDF 2004.10.05.}

@<Define rules@>=

  if (p == static_cast<Point*>(0))
    {
       cerr_strm << thread_name << "ERROR! In `yyparse()', "
                 << "rule `point_secondary --> "
                 << endl << "point_secondary times_or_over "
                 << "numeric_primary':"
                 << endl 
                 << "`point_secondary' on the right-hand side is invalid."
                 << "Setting `point_primary' on the left-hand side to "
                 << "`INVALID_POINT' and will try to continue.";

       
       log_message(cerr_strm);
       cerr_message(cerr_strm, error_stop_value);
       cerr_strm.str("");

       p = create_new<Point>(&INVALID_POINT);

     }  /* |if (p == 0)|  */@;

@q ******* (7) |p != 0|.@> 

@ |p != 0|.
\initials{LDF 2004.10.05.}

@<Define rules@>=


  else /* |p != 0|  */@;
    {

@q ******** (8) |times_or_over == TIMES|.  Perform multiplication.@> 

@ |times_or_over == TIMES|.  Perform multiplication.
\initials{LDF 2004.10.05.}

@<Define rules@>=

      if (@=$2@> == TIMES)
        *p *= @=$3@>;


@q ******** (8) |times_or_over == OVER|.@> 

     else if (@=$2@> == OVER)
        {

@q ********* (9) Error handling:  |numeric_primary == 0|.  Can't divide.@> 

@ Error handling:  |numeric_primary == 0|.  Can't divide.
\initials{LDF 2004.10.05.}

@<Define rules@>=

            if (@=$3@> == ZERO_REAL)
              {
                cerr_strm << thread_name 
                          << "ERROR! In `yyparse()', "
                          << "rule `point_secondary --> "
                          << "point_secondary times_or_over "
                          << "numeric_primary':" << endl 
                          << "`numeric_primary' == 0. Can't divide by 0."
                          << "Setting the `point_secondary' on the "
                          << "left-hand side "
                          << "to `INVALID_POINT' and will try to continue.";
                     
                log_message(cerr_strm);
                cerr_message(cerr_strm, error_stop_value);
                cerr_strm.str("");
  
                *p = INVALID_POINT;
                      
        }  /* |if (@=$3@> == 0)|  */

@q ********* (9) Perform division.@>   

@ Perform division.
\initials{LDF 2004.10.05.}

@<Define rules@>=

      else
          *p /= @=$3@>;

@q ********* (9).@> 

          } /* |else if (@=$2@> == OVER)|  */


@q ******** (8).@>   

    }  /* |else| (|p != 0|)  */@;

@q ******* (7) Set value of rule to |static_cast<void*>(p)| and exit.@> 

@ Set value of rule to |static_cast<void*>(p)| and exit.
\initials{LDF 2004.10.05.}

@<Define rules@>=

  @=$$@> = static_cast<void*>(p);

};

@q *** (3) point secondary --> point_secondary CROSS_PRODUCT point_primary.@>

@*2 \§point secondary> $\longrightarrow$ \§point secondary> 
\.{CROSS\_PRODUCT} \§point primary>.

\LOG
\initials{LDF 2004.10.05.}  
Added this rule.
\ENDLOG

@q ****** (6) Definition.@>  

@<Define rules@>=
@=point_secondary: point_primary CROSS_PRODUCT point_primary@>@/ 
{

  @<Common declarations for rules@>@; 
  
#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
      cerr_strm << thread_name 
                << "*** Parser: `point_secondary --> "
                << "point_secondary CROSS_PRODUCT point_primary'.";

      log_message(cerr_strm);
      cerr_message(cerr_strm);
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */@;

  Point* p = static_cast<Point*>(@=$1@>); 
  Point* q = static_cast<Point*>(@=$3@>);

  Point r;
  
  try 
     {
        r = p->cross_product(*q);
     }

@q ******* (7) Error handling:  |Point::cross_product| threw an exception.@> 
@ Error handling:  |Point::cross_product| threw an exception.
\initials{LDF 2004.10.05.}

@<Define rules@>=

  catch (...)
     {
      
      cerr_strm << thread_name 
                << "ERROR! In `yyparse()', rule `point_secondary --> "
                << endl 
                << "point_secondary CROSS_PRODUCT point_primary':"
                << endl << "`Point::cross_product()' failed and threw "
                << "an exception.  Rethrowing.";

      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");

      throw;

     } /* |catch|  */

@q ******* (7) |Point::cross_product| succeeded.@> 
@ |Point::cross_product| succeeded.
Set value of rule to |r|, delete |q|, and exit rule.
\initials{LDF 2004.10.05.}

@<Define rules@>=

  *p = r;

  delete q;

  @=$$@> = static_cast<void*>(p);

};

@q *** (3) point secondary --> numeric_secondary TIMES point_primary.  @>

@*2 \§point secondary> $\longrightarrow$ \§numeric secondary> 
\.{TIMES} \§point primary>.

Adding this rule caused a jump from 57 to 111 shift/reduce conflicts.
\initials{LDF 2004.05.05.}


\LOG
\initials{LDF 2004.05.05.}  
Added this rule.
\ENDLOG

@<Define rules@>=

@=point_secondary: numeric_secondary TIMES point_primary@>@/ 
{

   @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
   DEBUG = false; /* |true| */ @; 
   if (DEBUG)
      {
         cerr_strm << thread_name 
                   << "*** Parser:  `point_secondary --> numeric_secondary TIMES "
                   << "point_primary'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */@;

   Point* p = static_cast<Point*>(@=$3@>); 

   if (p == static_cast<Point*>(0))
      {
         cerr_strm << thread_name << "ERROR! In yyparse(), rule "
                   << "`point_secondary --> numeric_secondary TIMES "
                   << "point_primary':"
                   << endl 
                   << "`point_primary' is invalid."
                   << "Setting `point_secondary' to `INVALID_POINT' "
                   << "and will try to continue.";
       

         log_message(cerr_strm);
         cerr_message(cerr_strm, error_stop_value);
         cerr_strm.str("");
       
         p = create_new<Point>(&INVALID_POINT);

     }  /* |if (p == 0)|  */@;


  else /* |p != 0|  */

    *p *= @=$1@>;

  @=$$@> = static_cast<void*>(p);

};

@q *** (3) point secondary --> numeric_secondary point_primary.  @>

@*2 \§point secondary> $\longrightarrow$ \§numeric secondary> 
\§point primary>.

Multiplication without \.{TIMES}.
\initials{LDF 2004.12.18.}

\LOG
\initials{LDF 2004.12.18.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=point_secondary: numeric_secondary point_primary@>@/ 
{

   @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
   DEBUG = false; /* |true| */ @; 
   if (DEBUG)
      {
         cerr_strm << thread_name 
                   << "*** Parser:  `point_secondary --> numeric_secondary "
                   << "point_primary'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */@;

   Point* p = static_cast<Point*>(@=$2@>); 

   if (p == static_cast<Point*>(0))
      {
         cerr_strm << thread_name << "ERROR! In yyparse(), rule "
                   << "`point_secondary --> numeric_secondary "
                   << "point_primary':"
                   << endl 
                   << "`point_primary' is invalid."
                   << "Will try to set `point_secondary' to `INVALID_POINT' "
                   << "and try to continue.";
       

         log_message(cerr_strm);
         cerr_message(cerr_strm, error_stop_value);
         cerr_strm.str("");
       
         try 
            {
                p = create_new<Point>(&INVALID_POINT);
            }

         catch (bad_alloc)
            {
               cerr_strm << thread_name << "ERROR! In yyparse(), rule "
                         << "`point_secondary --> numeric_secondary "
                         << "point_primary':"
                         << endl 
                         << "`create_new<Point>()' failed.  "
                         << "Rethrowing `bad_alloc'.";

               log_message(cerr_strm);
               cerr_message(cerr_strm, error_stop_value);
               cerr_strm.str("");
 
               throw;

            }  /* |catch (bad_alloc)|  */

     }  /* |if (p == 0)|  */@;


  else /* |p != 0|  */

    *p *= @=$1@>;

  @=$$@> = static_cast<void*>(p);

};


@q *** (3) point secondary --> point_secondary transformer.  @>

@*2 \§point secondary> $\longrightarrow$ \§point secondary> \§transformer>. 

\LOG
\initials{LDF 2004.09.09.}  
Added this rule.

\initials{LDF 2004.10.05.}
Edited this rule.
Now setting the value of rule to 0 if the |point_secondary| on the right-hand
side is null or the |Point| it points to is equal to |INVALID_POINT|.  In the
latter case, the pointer is deleted.
@:BUG FIX@> BUG FIX:  Now deleting |transformer|.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=point_secondary: point_secondary transformer@>@/ 
{

  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
      cerr_strm << thread_name 
                << "*** Parser: `point_secondary --> point_secondary "
                << "transformer'.";

      log_message(cerr_strm);
      cerr_message(cerr_strm);
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */@;

  Point* p = static_cast<Point*>(@=$1@>); 
  
  Transform* t = static_cast<Transform*>(@=$2@>); 

@q ******* (7) Error handling:  |p == 0|.@> 

@ Error handling:  |p == 0|.
\initials{LDF 2004.09.09.}

\LOG
\initials{LDF 2004.10.05.}
@:BUG FIX@> BUG FIX:  Now deleting |t|.
\ENDLOG 

@<Define rules@>=

  if (p == static_cast<Point*>(0))
    {
      cerr_strm << thread_name << "ERROR! In `yyparse()',"
                << endl    
                << "rule `point_secondary --> point_secondary transformer':"
                << endl 
                << "`point_secondary' on right-hand side is null."
                << endl 
                << "Setting `point_secondary' on left-hand side to 0 "
                << "and will try to continue.";

      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");
      
      delete t;

      @=$$@> = static_cast<void*>(0);

    }  /* |if (p == 0)|  */@;

@q ******* (7) Error handling:  |p == 0|.@> 

@ Error handling:  |p == 0|.
\initials{LDF 2004.09.09.}

@<Define rules@>=

  else if (*p == INVALID_POINT)
    {
      cerr_strm << thread_name << "ERROR! In `yyparse()',"
                << endl    
                << "rule `point_secondary --> point_secondary transformer':"
                << endl 
                << "`point_secondary' on right-hand side is invalid."
                << endl 
                << "Deleting it, setting `point_secondary' on "
                << "left-hand side to 0, "
                << "and will try to continue.";

      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");
      
      delete p;
      delete t;

      @=$$@> = static_cast<void*>(0);

    }  /* |if (p == 0 || *p == INVALID_POINT)|  */@;

@q ******* (7) |p != 0|.@> 

@ |p != 0|.
\initials{LDF 2004.09.09.}

@<Define rules@>=

  else /* |p != 0|  */
    {


@q ******** (8) Error handling:  |t == 0|.@> 

@ Error handling:  |t == 0|.
\initials{LDF 2004.09.09.}

@<Define rules@>=

  if (t == static_cast<Transform*>(0))
    {

      cerr_strm << thread_name << "ERROR! In `yyparse()',"
                << endl    
                << "rule `point_secondary --> point_secondary transformer':"
                << endl 
                << "`transformer' on right-hand side is invalid."
                << endl 
                << "Not transforming `point_secondary', "
                << "but will try to continue.";

      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");
      

      @=$$@> = static_cast<void*>(p); 

    } /* |if (t == 0)|  */

@q ******** (8) |t != 0|.  Transform |*p|, delete |t|            @>
@q ******** (8) and set value of rule to |static_cast<void*>(p)|.@>

@ |t != 0|.  Transform |*p|, delete |t| and set value of rule to 
|static_cast<void*>(p)|.
\initials{LDF 2004.10.05.}

\LOG
\initials{LDF 2004.10.05.}
@:BUG FIX@> BUG FIX:  Now deleting |t|.
\ENDLOG 

@<Define rules@>=

  else /* |t != 0| */

    {
      *p *= *t;

 
      delete t;

      @=$$@> = static_cast<void*>(p);       


    }  /* |else| (|t != 0|)  */


@q ******** (8)@> 

    }  /* |else| (|p != 0|)  */


@q ******* (7) Exit rule.@> 

};

@q *** (3) point_secondary --> point_secondary PROJECTED @>
@q *** (3) focus_primary with_z_optional.                @>  

@*2 \§point secondary> $\longrightarrow$ 
\§point secondary> \.{PROJECTED} \§focus primary>
\§with z optional>.
\initials{LDF 2005.04.15.}

\LOG
\initials{LDF 2005.04.15.}
Added this rule.

\initials{LDF 2005.04.18.}
Changed |PROJECT| to |PROJECTED|.

\initials{LDF 2005.04.19.}
Changed this rule from 
\§point primary> $\longrightarrow$ 
\§point primary> \.{PROJECTED} \.{WITH\_FOCUS} \§focus expression>
to
\§point secondary> $\longrightarrow$ 
\§point secondary> \.{PROJECTED} \§focus primary>.

\initials{LDF 2005.04.19.}
Added \§with z optional> to this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=point_secondary: point_secondary PROJECTED focus_primary with_z_optional@>@/
{ 
   @=$$@> = point_secondary_rule_func_0(@=$1@>, 
                                        @=$3@>, 
                                        @=$4@>, 
                                        static_cast<Scanner_Node>(parameter));

};


@q ****** (6) with_z_optional.@> 
@*5 \§with z optional>.
\initials{LDF 2005.04.19.}

\LOG
\initials{LDF 2005.04.19.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <int_value> with_z_optional@>@/

@q ******* (7) with_z_optional --> EMPTY.@> 
@*6 \§with z optional> $\longrightarrow$ \.{EMPTY}.
\initials{LDF 2005.04.19.}

\LOG
\initials{LDF 2005.04.19.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=with_z_optional: /* Empty  */@>@/
{
   @=$$@> = 0;
}; 

@q ******* (7) with_z_optional --> WITH_Z.@> 
@*6 \§with z optional> $\longrightarrow$ \.{WITH\_Z}.
\initials{LDF 2005.04.19.}

\LOG
\initials{LDF 2005.04.19.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=with_z_optional: WITH_Z@>@/
{
   @=$$@> = 1;
}; 

@q *** (3) point secondary --> point_secondary REFLECTED_IN @>
@q *** (3) path_expression.@> 

@*2 \§point secondary> $\longrightarrow$ \§point secondary> 
\.{REFLECTED\_IN} \§path expression>.

\LOG
\initials{LDF 2004.10.03.}
Added this rule.

\initials{LDF 2004.10.04.}
@:BUG FIX@> BUG FIX:  Now setting the value of the rule 
to |Point* p|, which will either be the reflection of the
|Point| represented by the |point_secondary| on the left-hand side, 
or 0, if |Scan_Parse::reflect_in| fails.
Previously, I had forgotten to set it.

\initials{LDF 2004.12.03.}
Changed |REFLECTED IN| to |REFLECTED_IN|.

\initials{LDF 2005.10.24.}
Changed |path_like_expression| to |path_expression|.
Removed debugging code.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=point_secondary: point_secondary REFLECTED_IN @>
@=path_expression@>@/ 
{

    Point* p = reflect_in_func<Point>(static_cast<Scanner_Node>(parameter),
                                    static_cast<Point*>(@=$1@>),
                                    static_cast<Path*>(@=$3@>));

    @=$$@> = static_cast<void*>(p);

};

@q ** (2) point tertiary.@>

@*1 \§point tertiary>.
\initials{LDF Undated.}

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> point_tertiary@>

@q *** (3) point tertiary --> point_secondary.@>

@*2 \§point tertiary> $\longrightarrow$ \§point secondary>.
\initials{LDF Undated.}

\LOG
\initials{LDF 2004.12.21.}
Made debugging output thread-safe.
\ENDLOG 

@<Define rules@>=
@=point_tertiary: point_secondary@>@/ 
{
   @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
   DEBUG = false; /* |true| */ @; 
   if (DEBUG)
     {
         cerr_strm << thread_name 
                   << "*** Parser:  `point_tertiary --> point_secondary'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str("");          
     }
#endif /* |DEBUG_COMPILE|  */@;

  @=$$@> = @=$1@>;

};

@q *** (3) point tertiary --> point_tertiary PLUS point_secondary.@>

@*2 \§point tertiary> $\longrightarrow$ \§point tertiary> \.{PLUS}
\§point secondary>.
\initials{LDF 2004.05.06.}  

\LOG
\initials{LDF 2004.05.06.}  
Added this rule.
\ENDLOG 
  
@<Define rules@>=
@=point_tertiary: point_tertiary PLUS point_secondary@>@/ 
{

  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
   DEBUG = false; /* |true| */ @; 
   if (DEBUG)
      {
          cerr_strm << thread_name << "*** Parser:  "
                    << "`point_tertiary --> point_tertiary "
                    << " PLUS point_secondary'.";

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */@;

  Point* p = static_cast<Point*>(@=$1@>); 
  Point* q = static_cast<Point*>(@=$3@>); 


@q ****** (6) Error handling for |p == 0|.  @>
@ Error handling for |p == 0|.

@<Define rules@>=

  if (p == static_cast<Point*>(0))
    {
       cerr_strm << thread_name << "ERROR!  In yyparse(), rule "
                 << "`point_tertiary -->"
                 << endl 
                 << "point_tertiary "
                 << " PLUS point_secondary':"
                 << endl 
                 << "`point_tertiary' on right-hand side is invalid."
                 << "Setting `point_tertiary' on left-hand side to `INVALID_POINT' "
                 << "and will try to continue.";

        log_message(cerr_strm);
        cerr_message(cerr_strm, error_stop_value);
        cerr_strm.str("");
       
        @=$$@> = static_cast<void*>(create_new<Point>(&INVALID_POINT));

        if (q != static_cast<Point*>(0))
           delete q;

     }  /* |if (p == 0)|  */@;

@q ****** (6) Error handling for |q == 0|.  @>
@ Error handling for |q == 0|.

@<Define rules@>=

  else if (q == static_cast<Point*>(0))
    {
       cerr_strm << thread_name << "ERROR! In yyparse(), rule "
                 << "`point_tertiary --> "
                 << endl << "point_tertiary "
                 << " PLUS point_secondary' (point_tertiary 2):"
                 << endl 
                 << "`point_secondary' on right-hand side is invalid."
                 << "Setting `point_tertiary' on left-hand side to `INVALID_POINT' "
                 << "and will try to continue.";
       
       log_message(cerr_strm);
       cerr_message(cerr_strm, error_stop_value);
       cerr_strm.str("");       

       @=$$@> = static_cast<void*>(create_new<Point>(&INVALID_POINT)); 

       if (p != static_cast<Point*>(0))
         delete p;

     }  /* |if (q == 0)|  */@;


@q ****** (6) Success!  @>
@ Success!

@<Define rules@>=

  else
    {
        *p += *q;
 
         @=$$@> = static_cast<void*>(p); 
       
         delete q;
    }
};

@q *** (3) point tertiary --> point_tertiary MINUS point_secondary.@>

@*2 \§point tertiary> $\longrightarrow$ \§point tertiary> \.{MINUS}
\§point secondary>.
\initials{LDF 2004.05.06.}  

\LOG
\initials{LDF 2004.05.06.}  
Added this rule.
\ENDLOG 
  
@<Define rules@>=
@=point_tertiary: point_tertiary MINUS point_secondary@>@/ 
{

  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
   DEBUG = false; /* |true| */ @; 
   if (DEBUG)
      {
          cerr_strm << thread_name << "*** Parser:  "
                    << "`point_tertiary --> point_tertiary "
                    << " MINUS point_secondary'.";

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */@;

  Point* p = static_cast<Point*>(@=$1@>); 
  Point* q = static_cast<Point*>(@=$3@>); 


@q ****** (6) Error handling for |p == 0|.  @>
@ Error handling for |p == 0|.

@<Define rules@>=

  if (p == static_cast<Point*>(0))
    {
       cerr_strm << thread_name << "ERROR!  In yyparse(), rule "
                 << "`point_tertiary -->"
                 << endl 
                 << "point_tertiary "
                 << " MINUS point_secondary':"
                 << endl 
                 << "`point_tertiary' on right-hand side is invalid."
                 << "Setting `point_tertiary' on left-hand side to `INVALID_POINT' "
                 << "and will try to continue.";

        log_message(cerr_strm);
        cerr_message(cerr_strm, error_stop_value);
        cerr_strm.str("");
       
        @=$$@> = static_cast<void*>(create_new<Point>(&INVALID_POINT));

        if (q != static_cast<Point*>(0))
           delete q;

     }  /* |if (p == 0)|  */@;

@q ****** (6) Error handling for |q == 0|.  @>
@ Error handling for |q == 0|.

@<Define rules@>=

  else if (q == static_cast<Point*>(0))
    {
       cerr_strm << thread_name << "ERROR! In yyparse(), rule "
                 << "`point_tertiary --> "
                 << endl << "point_tertiary "
                 << " MINUS point_secondary' (point_tertiary 2):"
                 << endl 
                 << "`point_secondary' on right-hand side is invalid."
                 << "Setting `point_tertiary' on left-hand side to `INVALID_POINT' "
                 << "and will try to continue.";
       
       log_message(cerr_strm);
       cerr_message(cerr_strm, error_stop_value);
       cerr_strm.str("");       

       @=$$@> = static_cast<void*>(create_new<Point>(&INVALID_POINT)); 

       if (p != static_cast<Point*>(0))
         delete p;

     }  /* |if (q == 0)|  */@;


@q ****** (6) Success!  @>
@ Success!

@<Define rules@>=

  else
    {
        *p -= *q;
 
         @=$$@> = static_cast<void*>(p); 
       
         delete q;
    }
};

@q ** (2) point expression.  @>
@*1 \§point expression>.

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> point_expression@>

@q *** (3) point expression --> point_tertiary.  @>
@*2 \§point expression> $\longrightarrow$ \§point tertiary>.

@<Define rules@>=
@=point_expression: point_tertiary@>@/ 
{

#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
        cerr_mutex.lock(); 
        cerr << "*** Parser: point_expression (point_tertiary)."
             << endl;
        cerr_mutex.unlock(); 
    }
#endif /* |DEBUG_COMPILE|  */@;

  @=$$@> = @=$1@>;

#if DEBUG_COMPILE
#if 0 
   if (DEBUG)
     {
       Point* q = static_cast<Point*>(@=$$@>);
       q->show("q:");
     }
#endif 
#endif /* |DEBUG_COMPILE|  */@; 

};

@q * (1) point_expression_list.@> 
@* \§point expression list>.
\initials{LDF 2007.07.15.}

\LOG
\initials{LDF 2007.07.15.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> point_expression_list@>@/

@q ** (2) point_expression_list --> point_expression COMMA point_expression.@> 
@*1 \§point expression list> $\longrightarrow$ \§point expression> 
\.{COMMA} \§point expression>.
\initials{LDF 2007.07.15.}

\LOG
\initials{LDF 2007.07.15.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=point_expression_list: point_expression COMMA point_expression@>@/
{

@q **** (4)@> 

#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
        cerr_mutex.lock(); 
        cerr << "** Parser: `point_expression_list: point_expression "
             << "COMMA point_expression'."
             << endl;
        cerr_mutex.unlock(); 
    }
#endif /* |DEBUG_COMPILE|  */@;

@q **** (4)@> 

   Point* p0 = static_cast<Point*>(@=$1@>);
   Point* p1 = static_cast<Point*>(@=$3@>);

@q **** (4)@> 

   Pointer_Vector<Point>* pv = new Pointer_Vector<Point>;

   *pv += p0;
   *pv += p1;

   @=$$@> = static_cast<void*>(pv); 

};

@q ** (2) point_expression_list --> point_expression_list COMMA point_expression.@> 
@*1 \§point expression list> $\longrightarrow$ \§point expression list> 
\.{COMMA} \§point expression>.
\initials{LDF 2007.07.15.}

\LOG
\initials{LDF 2007.07.15.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=point_expression_list: point_expression_list COMMA point_expression@>@/
{

@q **** (4)@> 

#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
        cerr_mutex.lock(); 
        cerr << "** Parser: `point_expression_list: point_expression_list "
             << "COMMA point_expression'."
             << endl;
        cerr_mutex.unlock(); 
    }
#endif /* |DEBUG_COMPILE|  */@;

@q **** (4)@> 

   Pointer_Vector<Point>* pv = static_cast<Pointer_Vector<Point>*>(@=$1@>); 

   Point* p0 = static_cast<Point*>(@=$3@>);

   *pv += p0;

   @=$$@> = static_cast<void*>(pv); 

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
