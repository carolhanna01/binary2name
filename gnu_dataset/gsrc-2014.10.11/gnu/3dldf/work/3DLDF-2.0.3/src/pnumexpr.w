@q pnumexpr.w @> 
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


 
@q * (0) Numeric expressions.  @>
@** Numeric expressions.
\initials{LDF 2004.10.01.}

\LOG
\initials{LDF 2004.04.29.}  
Created this file.  It contains code formerly contained in
\filename{parser.w} and is included in that file by means of CWEB's
``\.{@@i}'' command. 
\ENDLOG 

@q **** (4) numeric_atom.  @>
@*3 \§numeric atom>.
\initials{LDF 2004.10.01.}

@<Type declarations for non-terminal symbols@>=
@=%type <real_value> numeric_atom@>@/

@q ***** (5) numeric_atom --> numeric_variable.@>
@*4 \§numeric atom> $\longrightarrow$ \§numeric variable>. 
\initials{LDF 2004.10.01.}

\LOG
\initials{LDF 2004.05.03.}  
Changed |variable| to |numeric_variable|.
The way it was before caused reduce/reduce conflicts when I tried to 
use |variable| in the rule for |point_primary|.


\initials{LDF 2004.05.03.}
@:BUG FIX@> BUG FIX: 
Changed this rule to account for the fact that
|@=$1@>| is now an |Id_Map_Entry_Node| rather that a |real*| (after
being cast from |void*|).

\initials{LDF 2004.05.03.}
Added error handling code for the case that |entry == 0|.

\initials{LDF 2004.10.13.}
@:BUG FIX@> BUG FIX:  Now testing whether |entry->object == 0| as well
as whether |entry == 0|.

\initials{LDF 2004.11.21.}
Commented-out the error message formerly issued in the case that
|entry == 0 || entry->object == 0|.  This condition occurs legitimately 
when one tries to show an ``unknown |numeric|''.
\ENDLOG 

@q ****** (6) Definition.  @>

@<Define rules@>= 

@=numeric_atom: numeric_variable@>
{


  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

  if (entry == static_cast<Id_Map_Entry_Node>(0) || entry->object == static_cast<void*>(0))
    {

      @=$$@> = ZERO_REAL;

    }  /* |if (entry == 0 || entry->object == 0)| */


  else /* |entry != 0| */
 
      @=$$@> = *static_cast<real*>(entry->object);
 
};


@q ***** (5) numeric_argument.  @>
@*4 \§numeric argument>.
\initials{LDF 2004.10.01.}

\TODO
@q { @>
@:TO DO}{{\bf TO DO}@>
@q } @> 
@q !! TO DO:  @>
Add this case.  \initials{LDF 2004.04.28.}
\ENDTODO 


@q ***** (5) numeric_token_atom.  @>
@*4 \§numeric token atom>.
\initials{LDF 2004.10.01.}

@<Define rules@>= 
@=numeric_atom: numeric_token_atom@>
{

  @=$$@> = @=$1@>;

};

@q ***** (5) NORMAL_DEVIATE.  @>
@*4 \.{NORMAL\_DEVIATE}.
\initials{LDF 2004.10.01.}

\TODO
@q { @>
@:TO DO}{{\bf TO DO}@>
@q } @> 
@q !! TO DO:  @>
\initials{LDF 2004.04.28.}
Add this case.  
\ENDTODO 

@q ***** (5) numeric_single.  @>

@*4 \§numeric single>.
\initials{LDF 2004.10.01.}

\LOG
\initials{LDF 2004.04.30.}
Changed this rule.  It now uses |numeric_single| rather than
`\.{\LP}' \§numeric expression> `\.{\RP}' explicitly.
\ENDLOG 


@<Define rules@>= 
@=numeric_atom: numeric_single@>
{

  @=$$@> = @=$1@>;

};




@q **** (4) Numeric token atom.  @>
@*3 \§numeric token atom>.

@<Type declarations for non-terminal symbols@>=
@=%type <real_value> numeric_token_atom@>@/

@q ***** (5) numeric_token> / numeric_token.  @>

@*4 \§numeric token> / \§numeric token>.  @>

\TODO
@q { @>
@:TO DO}{{\bf TO DO}@>
@q } @> 
\initials{LDF 2004.04.24.}
!! TO DO:  Add error handling code for division by 0.  
Print out context. 
\ENDTODO  

@<Define rules@>= 
@=numeric_token_atom: numeric_token OVER numeric_token@>
{
  @<Common declarations for rules@>@;

#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
    if (DEBUG) 
      {
        cerr_strm << thread_name 
                  << "*** Parser: `numeric_token_atom --> "
                  << "numeric_token OVER numeric_token'.";

        log_message(cerr_strm);
        cerr_message(cerr_strm);
        cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */

  if (@=$3@> == ZERO_REAL)
    {
#if 0 
@q       unsigned int first_line   = @@=@@1@@>.first_line;   @>
@q       unsigned int first_column = @@=@@1@@>.first_column; @>
@q       unsigned int last_line    = @@=@@3@@>.last_line;    @>
@q       unsigned int last_column  = @@=@@3@@>.last_column;  @>
#endif 

      cerr_strm << "ERROR! ";

#if 0 
      if (scanner_node->in->type == Io_Struct::FILE_TYPE)
        cerr_strm << "In input file `" << scanner_node->in->filename << "' "
                  << first_line << "." << first_column << "--" << last_line
                  << "." << last_column << "." << endl;
#endif 

      cerr_strm << "Division by 0.  Setting <numeric token atom> to 0."
                << "Will try to continue.";


      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");

      @=$$@> = ZERO_REAL;


    } /* |if (@=$3@> == 0)|  */
  
  else
    @=$$@> = @=$1@> / @=$3@>;

#if DEBUG_COMPILE
  if (DEBUG)
    {
      cerr_strm << thread_name 
                << "In `yyparse()', rule "
                << "`numeric_token_atom --> "
                << "numeric_token OVER numeric_token':"
                << endl 
                << "`$$' == " << @=$$@> << ".";

      log_message(cerr_strm);
      cerr_message(cerr_strm);
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */ 

};

@q ***** (5) numeric_token not followed by "/ numeric_token".@>
@*4 \§numeric token> not followed by ``$/$ \§<numeric token>''. 
\initials{LDF 2004.10.01.}

@<Define rules@>= 
@=numeric_token_atom: numeric_token@>
{
  @=$$@> = @=$1@>;

};


@q **** (4) Numeric primary.  @>
@*3 \§numeric primary>.
\initials{LDF 2004.10.01.}

@<Type declarations for non-terminal symbols@>=
@=%type <real_value> numeric_primary@>@/

@q ***** (5) numeric_primary --> numeric_atom.  @>
@*4 \§numeric primary> $\longrightarrow$ \§numeric atom>.

@<Define rules@>= 
@=numeric_primary: numeric_atom@>
{
  @=$$@> = @=$1@>;

};

@q ***** (5) numeric_atom [ numeric_expression, numeric_expression ].  @>

@*4 \§numeric atom> \.{LEFT\_BRACKET} 
\§numeric expression> \.{COMMA} \.{RIGHT\_BRACKET}.
\initials{LDF 2004.04.27.}  

\LOG
\initials{LDF 2004.04.27.}  
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=numeric_primary: numeric_atom LEFT_BRACKET numeric_expression COMMA@>@/
@=numeric_expression RIGHT_BRACKET@>
{

  @=$$@> = @=$3@> + @=$1@> * (@=$5@> - @=$3@>);

};


@*4 \§numeric primary> $\longrightarrow$ \.{MAGNITUDE} \§numeric primary>.
\initials{LDF 2007.06.20.}

\LOG
\initials{LDF 2007.06.20.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=numeric_primary: MAGNITUDE numeric_primary@>
{

  @=$$@> = fabs(@=$2@>);

};


@q ***** (5) numeric_primary LENGTH numeric_primary.  @>

@*4 \§numeric primary> $\longrightarrow$ \.{LENGTH} 
\§numeric primary>.

This rule is trivial.  The length of a \§numeric primary> is simply it's 
value.  This doesn't seem to be documented in {\it The METAFONTbook}.
\initials{LDF 2004.04.27.}

\LOG
\initials{LDF 2004.04.27.}  
Added this rule.

\initials{LDF 2004.10.06.}
No longer including 
|@<Common code for |LENGTH| AND |MAGNITUDE|@>| 
in the action for this rule.
\ENDLOG 

@<Define rules@>= 
@=numeric_primary: LENGTH numeric_primary@>
{

  @=$$@> = @=$2@>;

};

@q ***** (5) numeric_primary --> LENGTH point_primary.@>

@*4 \§numeric primary> $\longrightarrow$ \.{LENGTH} 
\§point primary>.

\LOG
\initials{LDF 2004.05.05.}  
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=numeric_primary: LENGTH point_primary@>
{

  Point* p = static_cast<Point*>(@=$2@>);

  if (p == static_cast<Point*>(0))
    {

      @=$$@> = ZERO_REAL;
      
    } /* |if (p == 0)|  */

  else /* |p != 0|  */
    {
      @=$$@> = p->magnitude();
      delete p;

    } /* |else| (|p != 0|)  */

};




@q ***** (5) numeric_primary --> MAGNITUDE point_primary.@>

@*4 \§numeric primary> $\longrightarrow$ \.{MAGNITUDE} 
\§point primary>.

\LOG
\initials{LDF 2004.05.05.}  
Added this rule.

\initials{LDF 2004.10.06.}
Added \§optional of> to this rule.

\initials{LDF 2004.10.07.}
Removed \§optional of> from this rule.
\ENDLOG 

@<Define rules@>= 
@=numeric_primary: MAGNITUDE point_primary@>
{

  Point* p = static_cast<Point*>(@=$2@>);

  if (p == static_cast<Point*>(0))
    {
      @=$$@> = ZERO_REAL;
      
    } /* |if (p == 0)|  */

  else /* |p != 0|  */
    {
      @=$$@> = p->magnitude();
      delete p;

    } /* |else| (|p != 0|)  */
};

@q ***** (5) numeric_primary --> LENGTH path_primary.  @>

@*4 \§numeric primary> $\longrightarrow$ \.{LENGTH} \§path primary>.

@<Define rules@>=
@=numeric_primary: LENGTH path_primary@>@/
{
   Path* p = static_cast<Path*>(@=$2@>);

   if (p)
     @=$$@> = p->size();
   else
     @=$$@> = ZERO_REAL;

   delete p;

};

@q ***** (5) numeric_primary --> SIZE path_primary.  @>

@*4 \§numeric primary> $\longrightarrow$ \.{SIZE} \§path primary>.

@<Define rules@>=
@=numeric_primary: SIZE path_primary@>@/
{
   Path* p = static_cast<Path*>(@=$2@>);

   if (p)
     @=$$@> = p->size();
   else
     @=$$@> = ZERO_REAL;

   delete p;

};

@q ***** (5) numeric_primary --> LENGTH ellipse_primary.  @>

@*4 \§numeric primary> $\longrightarrow$ \.{LENGTH} \§ellipse primary>.

@<Define rules@>=
@=numeric_primary: LENGTH ellipse_primary@>@/
{
   Ellipse* e = static_cast<Ellipse*>(@=$2@>);

   if (e)
     @=$$@> = e->size();
   else
     @=$$@> = ZERO_REAL;

   delete e;

};

@q ***** (5) numeric_primary --> SIZE ellipse_primary.  @>

@*4 \§numeric primary> $\longrightarrow$ \.{SIZE} \§ellipse primary>.

@<Define rules@>=
@=numeric_primary: SIZE ellipse_primary@>@/
{
   Ellipse* e = static_cast<Ellipse*>(@=$2@>);

   if (e)
     @=$$@> = e->size();
   else
     @=$$@> = ZERO_REAL;

   delete e;

};

@q ***** (5) numeric_primary --> LENGTH circle_primary.  @>

@*4 \§numeric primary> $\longrightarrow$ \.{LENGTH} \§circle primary>.
\initials{LDF 2007.11.08.}

\LOG
\initials{LDF 2007.11.08.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=numeric_primary: LENGTH circle_primary@>@/
{
   Circle* c = static_cast<Circle*>(@=$2@>);

   if (c)
     @=$$@> = c->size();
   else
     @=$$@> = ZERO_REAL;

   delete c;

};

@q ***** (5) numeric_primary --> SIZE circle_primary.  @>

@*4 \§numeric primary> $\longrightarrow$ \.{SIZE} \§circle primary>.
\initials{LDF 2007.11.08.}

\LOG
\initials{LDF 2007.11.08.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=numeric_primary: SIZE circle_primary@>@/
{
   Circle* c = static_cast<Circle*>(@=$2@>);

   if (c)
     @=$$@> = c->size();
   else
     @=$$@> = ZERO_REAL;

   delete c;

};





@q ***** (5) numeric_primary --> LENGTH parabola_primary.  @>

@*4 \§numeric primary> $\longrightarrow$ \.{LENGTH} \§parabola primary>.
\initials{LDF 2005.11.09.}

\LOG
\initials{LDF 2005.11.09.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=numeric_primary: LENGTH parabola_primary@>@/
{
   Parabola* p = static_cast<Parabola*>(@=$2@>);

   if (p)
     @=$$@> = p->size();
   else
     @=$$@> = ZERO_REAL;

   delete p;

};

@q ***** (5) numeric_primary --> SIZE parabola_primary.  @>

@*4 \§numeric primary> $\longrightarrow$ \.{SIZE} \§parabola primary>.
\initials{LDF 2005.11.09.}

\LOG
\initials{LDF 2005.11.09.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=numeric_primary: SIZE parabola_primary@>@/
{
   Parabola* p = static_cast<Parabola*>(@=$2@>);

   if (p)
     @=$$@> = p->size();
   else
     @=$$@> = ZERO_REAL;

   delete p;

};

@q ***** (5) numeric_primary --> SIZE hyperbola_primary.  @>

@*4 \§numeric primary> $\longrightarrow$ \.{SIZE} \§hyperbola primary>.
\initials{LDF 2007.07.03.}

\LOG
\initials{LDF 2007.07.03.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=numeric_primary: SIZE hyperbola_primary@>@/
{
   Hyperbola* p = static_cast<Hyperbola*>(@=$2@>);

   if (p)
     @=$$@> = p->size();
   else
     @=$$@> = ZERO_REAL;

   delete p;

};


@q ***** (5) numeric_primary --> LENGTH polygon_primary.  @>

@*4 \§numeric primary> $\longrightarrow$ \.{LENGTH} \§polygon primary>.
\initials{LDF 2005.12.11.}

\LOG
\initials{LDF 2005.12.11.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=numeric_primary: LENGTH polygon_primary@>@/
{
   Polygon* p = static_cast<Polygon*>(@=$2@>);

   if (p)
     @=$$@> = p->size();
   else
     @=$$@> = ZERO_REAL;

   delete p;

};

@q ***** (5) numeric_primary --> SIZE polygon_primary.  @>

@*4 \§numeric primary> $\longrightarrow$ \.{SIZE} \§polygon primary>.
\initials{LDF 2005.12.11.}

\LOG
\initials{LDF 2005.12.11.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=numeric_primary: SIZE polygon_primary@>@/
{
   Polygon* p = static_cast<Polygon*>(@=$2@>);

   if (p)
     @=$$@> = p->size();
   else
     @=$$@> = ZERO_REAL;

   delete p;

};

@q ***** (5) numeric_primary --> LENGTH reg_polygon_primary.  @>

@*4 \§numeric primary> $\longrightarrow$ \.{LENGTH} \§regular polygon primary>.
\initials{2009.01.15.}

\LOG
\initials{2009.01.15.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=numeric_primary: LENGTH reg_polygon_primary@>@/
{
   Reg_Polygon* p = static_cast<Reg_Polygon*>(@=$2@>);

   if (p)
     @=$$@> = p->size();
   else
     @=$$@> = ZERO_REAL;

   delete p;

};

@q ***** (5) numeric_primary --> SIZE reg_polygon_primary.  @>

@*4 \§numeric primary> $\longrightarrow$ \.{SIZE} \§regular polygon primary>.
\initials{2009.01.15.}

\LOG
\initials{2009.01.15.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=numeric_primary: SIZE reg_polygon_primary@>@/
{
   Reg_Polygon* p = static_cast<Reg_Polygon*>(@=$2@>);

   if (p)
     @=$$@> = p->size();
   else
     @=$$@> = ZERO_REAL;

   delete p;

};

@q ***** (5) numeric_primary --> SIZE ellipsoid_primary.  @>

@*4 \§numeric primary> $\longrightarrow$ \.{SIZE} \§ellipsoid primary>.

@<Define rules@>=
@=numeric_primary: SIZE ellipsoid_primary@>@/
{
   Ellipsoid* e = static_cast<Ellipsoid*>(@=$2@>);

   if (e)
     @=$$@> = e->size();
   else
     @=$$@> = ZERO_REAL;

   delete e;


};

@q ***** (5) numeric_primary --> SIZE sphere_primary.  @>

@*4 \§numeric primary> $\longrightarrow$ \.{SIZE} \§sphere primary>.
\initials{LDF 2006.01.23.}

\LOG
\initials{LDF 2006.01.23.}
Added this rule.  It currently contains a dummy action.

\initials{LDF 2009.09.09.}
Now calling |Scan_Parse::sphere_size_func|.
\ENDLOG

@<Define rules@>=
@=numeric_primary: SIZE sphere_primary@>@/
{

   @=$$@> = sphere_size_func(@=$2@>, parameter);

};

@q ***** (5) numeric_primary --> SIZE paraboloid_primary.  @>

@*4 \§numeric primary> $\longrightarrow$ \.{SIZE} \§paraboloid primary>.
\initials{LDF 2006.01.23.}

\LOG
\initials{LDF 2006.01.23.}
Added this rule.  It currently contains a dummy action.
\ENDLOG



@<Define rules@>=
@=numeric_primary: SIZE paraboloid_primary@>@/
{
   Paraboloid* p = static_cast<Paraboloid*>(@=$2@>);

   delete p;

   @=$$@> = ZERO_REAL;

};



@q ***** (5) numeric_primary --> SIZE polyhedron_primary.  @>

@*4 \§numeric primary> $\longrightarrow$ \.{SIZE} \§polyhedron primary>.
\initials{LDF 2005.12.20.}

\LOG
\initials{LDF 2005.12.20.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=numeric_primary: SIZE polyhedron_primary@>@/
{
   Polyhedron* p = static_cast<Polyhedron*>(@=$2@>);

   if (p)
     @=$$@> = p->size();
   else
     @=$$@> = ZERO_REAL;

   delete p;

};






@q ***** (5) numeric_primary --> LENGTH string_primary.  @>

@*4 \§numeric primary> $\longrightarrow$ \.{LENGTH} \§string primary>.

\TODO
@q { @>
@:TO DO}{{\bf TO DO}@>
@q } @> 
@q !! TO DO:  @>
Add this rule.  \initials{LDF 2004.04.28.}
\ENDTODO 
  
@q ***** (5) numeric_primary --> ASCII string_primary.  @>

@*4 \§numeric primary> $\longrightarrow$ \.{ASCII} \§string primary>.

\TODO
@q { @>
@:TO DO}{{\bf TO DO}@>
@q } @> 
@q !! TO DO:  @>
\initials{LDF 2004.04.28.}
Add this rule.  
\ENDTODO 

@q ***** (5) numeric_primary --> OCT string_primary.  @>

@*4 \§numeric primary> $\longrightarrow$ \.{OCT} \§string primary>.

\TODO
@q { @>
@:TO DO}{{\bf TO DO}@>
@q } @> 
@q !! TO DO:  @>
\initials{LDF 2004.04.28.}
Add this rule.  
\ENDTODO 

@q ***** (5) numeric_primary --> HEX string_primary.  @>

@*4 \§numeric primary> $\longrightarrow$ \.{HEX} \§string primary>.

\TODO
@q { @>
@:TO DO}{{\bf TO DO}@>
@q } @> 
@q !! TO DO:  @>
\initials{LDF 2004.04.28.}
Add this rule.  
\ENDTODO 

@q ***** (5) numeric_primary --> SIZE boolean_vector_primary.@>

@*4 \§numeric primary> $\longrightarrow$ \.{SIZE} \§numeric vector primary>.
\initials{LDF 2005.01.07.}

\LOG
\initials{LDF 2005.01.07.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=numeric_primary: SIZE boolean_vector_primary@>@/
{

   Pointer_Vector<bool>* bv 
         = static_cast<Pointer_Vector<bool>*>(@=$2@>); 

   @=$$@> = bv->ctr;

   delete bv;

};



@q ***** (5) numeric_primary --> SIZE numeric_vector_primary.@>

@*4 \§numeric primary> $\longrightarrow$ \.{SIZE} \§numeric vector primary>.
\initials{LDF 2005.01.07.}

\LOG
\initials{LDF 2005.01.07.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=numeric_primary: SIZE numeric_vector_primary@>@/
{

   Pointer_Vector<real>* nv 
         = static_cast<Pointer_Vector<real>*>(@=$2@>); 

   @=$$@> = nv->ctr;

   delete nv;

};

@q ***** (5) numeric_primary --> SIZE ulong_long_vector_primary.@>

@*4 \§numeric primary> $\longrightarrow$ \.{SIZE} \§ulong long vector primary>.
\initials{LDF 2005.12.07.}

\LOG
\initials{LDF 2005.12.07.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=numeric_primary: SIZE ulong_long_vector_primary@>@/
{

   Pointer_Vector<ulong_long>* nv 
         = static_cast<Pointer_Vector<ulong_long>*>(@=$2@>); 

   @=$$@> = nv->ctr;

   delete nv;

};



@q ***** (5) numeric_primary --> SIZE string_vector_primary.@>

@*4 \§numeric primary> $\longrightarrow$ \.{SIZE} \§string vector primary>.
\initials{LDF 2005.01.09.}

\LOG
\initials{LDF 2005.01.09.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=numeric_primary: SIZE string_vector_primary@>@/
{

   Pointer_Vector<string>* sv 
         = static_cast<Pointer_Vector<string>*>(@=$2@>); 

   @=$$@> = sv->ctr;

   delete sv;

};


@q ***** (5) numeric_primary --> SIZE pen_vector_primary.@>

@*4 \§numeric primary> $\longrightarrow$ \.{SIZE} \§pen vector primary>.
\initials{LDF 2005.01.13.}

\LOG
\initials{LDF 2005.01.13.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=numeric_primary: SIZE pen_vector_primary@>@/
{

   Pointer_Vector<Pen>* pv 
         = static_cast<Pointer_Vector<Pen>*>(@=$2@>); 

   @=$$@> = pv->ctr;

   delete pv;

};

@q ***** (5) numeric_primary --> SIZE dash_pattern_vector_primary.@>

@*4 \§numeric primary> $\longrightarrow$ \.{SIZE} 
\§dash pattern vector primary>.
\initials{LDF 2005.01.13.}

\LOG
\initials{LDF 2005.01.13.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=numeric_primary: SIZE dash_pattern_vector_primary@>@/
{

   Pointer_Vector<Dash_Pattern>* pv 
         = static_cast<Pointer_Vector<Dash_Pattern>*>(@=$2@>); 

   @=$$@> = pv->ctr;

   delete pv;

};



@q ***** (5) numeric_primary --> SIZE color_vector_primary.@>

@*4 \§numeric primary> $\longrightarrow$ \.{SIZE} \§color vector primary>.
\initials{LDF 2004.12.10.}

\LOG
\initials{LDF 2004.12.10.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=numeric_primary: SIZE color_vector_primary@>@/
{

   Pointer_Vector<Color>* cv 
         = static_cast<Pointer_Vector<Color>*>(@=$2@>); 

   @=$$@> = cv->ctr;

   delete cv;

};

@q ***** (5) numeric_primary --> SIZE transform_vector_primary.@>

@*4 \§numeric primary> $\longrightarrow$ \.{SIZE} 
\§transform vector primary>.
\initials{LDF 2005.01.13.}

\LOG
\initials{LDF 2005.01.13.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=numeric_primary: SIZE transform_vector_primary@>@/
{

   Pointer_Vector<Transform>* pv 
         = static_cast<Pointer_Vector<Transform>*>(@=$2@>); 

   @=$$@> = pv->ctr;

   delete pv;

};

@q ***** (5) numeric_primary --> SIZE focus_vector_primary.@>

@*4 \§numeric primary> $\longrightarrow$ \.{SIZE} 
\§focus vector primary>.
\initials{LDF 2005.01.18.}

\LOG
\initials{LDF 2005.01.18.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=numeric_primary: SIZE focus_vector_primary@>@/
{

   Pointer_Vector<Focus>* pv 
         = static_cast<Pointer_Vector<Focus>*>(@=$2@>); 

   @=$$@> = pv->ctr;

   delete pv;

};


@q ***** (5) numeric_primary --> SIZE point_vector_primary.@>

@*4 \§numeric primary> $\longrightarrow$ \.{SIZE} \§point vector primary>.
\initials{LDF 2004.11.10.}

\LOG
\initials{LDF 2004.11.10.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=numeric_primary: SIZE point_vector_primary@>@/
{

   Pointer_Vector<Point>* pv 
         = static_cast<Pointer_Vector<Point>*>(@=$2@>); 

   @=$$@> = pv->ctr;

   delete pv;

};


@q ***** (5) numeric_primary --> SIZE bool_point_vector_primary.  @>

@*4 \§numeric primary> $\longrightarrow$ \.{SIZE} \§bool-point vector primary>.
\initials{LDF 2004.11.09.}

\LOG
\initials{LDF 2004.11.09.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=numeric_primary: SIZE bool_point_vector_primary@>@/
{

   Pointer_Vector<Bool_Point>* bpv 
         = static_cast<Pointer_Vector<Bool_Point>*>(@=$2@>); 

   @=$$@> = bpv->ctr;

   delete bpv;

};

@q ***** (5) numeric_primary --> SIZE path_vector_primary.@>

@*4 \§numeric primary> $\longrightarrow$ \.{SIZE} \§path vector primary>.
\initials{LDF 2004.12.12.}

\LOG
\initials{LDF 2004.12.12.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=numeric_primary: SIZE path_vector_primary@>@/
{

   Pointer_Vector<Path>* pv 
         = static_cast<Pointer_Vector<Path>*>(@=$2@>); 

   @=$$@> = pv->ctr;

   delete pv;

};

@q ***** (5) numeric_primary --> SIZE ellipse_vector_primary.@>

@*4 \§numeric primary> $\longrightarrow$ \.{SIZE} \§ellipse vector primary>.
\initials{LDF 2004.12.14.}

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=numeric_primary: SIZE ellipse_vector_primary@>@/
{

   Pointer_Vector<Ellipse>* pv 
         = static_cast<Pointer_Vector<Ellipse>*>(@=$2@>); 

   @=$$@> = pv->ctr;

   delete pv;

};

@q ***** (5) numeric_primary --> SIZE circle_vector_primary.@>

@*4 \§numeric primary> $\longrightarrow$ \.{SIZE} \§circle vector primary>.
\initials{LDF 2004.12.14.}

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=numeric_primary: SIZE circle_vector_primary@>@/
{

   Pointer_Vector<Circle>* pv 
         = static_cast<Pointer_Vector<Circle>*>(@=$2@>); 

   @=$$@> = pv->ctr;

   delete pv;

};

@q ***** (5) numeric_primary --> SIZE parabola_vector_primary.@>

@*4 \§numeric primary> $\longrightarrow$ \.{SIZE} \§parabola vector primary>.
\initials{LDF 2005.12.01.}

\LOG
\initials{LDF 2005.12.01.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=numeric_primary: SIZE parabola_vector_primary@>@/
{

   Pointer_Vector<Parabola>* pv 
         = static_cast<Pointer_Vector<Parabola>*>(@=$2@>); 

   @=$$@> = pv->ctr;

   delete pv;

};

@q ***** (5) numeric_primary --> SIZE hyperbola_vector_primary.@>

@*4 \§numeric primary> $\longrightarrow$ \.{SIZE} \§hyperbola vector primary>.
\initials{LDF 2005.12.01.}

\LOG
\initials{LDF 2005.12.01.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=numeric_primary: SIZE hyperbola_vector_primary@>@/
{

   Pointer_Vector<Hyperbola>* pv 
         = static_cast<Pointer_Vector<Hyperbola>*>(@=$2@>); 

   @=$$@> = pv->ctr;

   delete pv;

};

@q ***** (5) numeric_primary --> SIZE conic_section_lattice_vector_primary.@>

@*4 \§numeric primary> $\longrightarrow$ \.{SIZE} 
\§conic section lattice vector primary>.
\initials{LDF 2007.07.29.}

\LOG
\initials{LDF 2007.07.29.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=numeric_primary: SIZE conic_section_lattice_vector_primary@>@/
{

   Pointer_Vector<Conic_Section_Lattice>* pv 
         = static_cast<Pointer_Vector<Conic_Section_Lattice>*>(@=$2@>); 

   @=$$@> = pv->ctr;

   delete pv;

};




@q ***** (5) numeric_primary --> SIZE nurb_vector_primary.@>

@*4 \§numeric primary> $\longrightarrow$ \.{SIZE} 
\§nurb vector primary>.
\initials{LDF 2005.01.26.}

\LOG
\initials{LDF 2005.01.26.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=numeric_primary: SIZE nurb_vector_primary@>@/
{

   Pointer_Vector<Nurb>* pv 
         = static_cast<Pointer_Vector<Nurb>*>(@=$2@>); 

   @=$$@> = pv->ctr;

   delete pv;

};

@q ***** (5) numeric_primary --> SIZE polygon_vector_primary.@>

@*4 \§numeric primary> $\longrightarrow$ \.{SIZE} \§polygon vector primary>.
\initials{LDF 2005.02.25.}

\LOG
\initials{LDF 2005.02.25.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=numeric_primary: SIZE polygon_vector_primary@>@/
{

   Pointer_Vector<Polygon>* pv 
         = static_cast<Pointer_Vector<Polygon>*>(@=$2@>); 

   @=$$@> = pv->ctr;

   delete pv;

};

@q ***** (5) numeric_primary --> SIZE triangle_vector_primary.@>

@*4 \§numeric primary> $\longrightarrow$ \.{SIZE} 
\§triangle vector primary>.
\initials{LDF 2005.01.25.}

\LOG
\initials{LDF 2005.01.25.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=numeric_primary: SIZE triangle_vector_primary@>@/
{

   Pointer_Vector<Triangle>* pv 
         = static_cast<Pointer_Vector<Triangle>*>(@=$2@>); 

   @=$$@> = pv->ctr;

   delete pv;

};

@q ***** (5) numeric_primary --> SIZE rectangle_vector_primary.@>

@*4 \§numeric primary> $\longrightarrow$ \.{SIZE} \§rectangle vector primary>.
\initials{LDF 2005.01.14.}

\LOG
\initials{LDF 2005.01.14.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=numeric_primary: SIZE rectangle_vector_primary@>@/
{

   Pointer_Vector<Rectangle>* pv 
         = static_cast<Pointer_Vector<Rectangle>*>(@=$2@>); 

   @=$$@> = pv->ctr;

   delete pv;

};


@q ***** (5) numeric_primary --> SIZE reg_polygon_vector_primary.@>

@*4 \§numeric primary> $\longrightarrow$ \.{SIZE} \§regular polygon vector primary>.
\initials{LDF 2004.12.14.}

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=numeric_primary: SIZE reg_polygon_vector_primary@>@/
{

   Pointer_Vector<Reg_Polygon>* pv 
         = static_cast<Pointer_Vector<Reg_Polygon>*>(@=$2@>); 

   @=$$@> = pv->ctr;

   delete pv;

};

@q ***** (5) numeric_primary --> SIZE ellipsoid_vector_primary.@>

@*4 \§numeric primary> $\longrightarrow$ \.{SIZE} \§ellipsoid vector primary>.
\initials{LDF 2005.12.01.}

\LOG
\initials{LDF 2005.12.01.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=numeric_primary: SIZE ellipsoid_vector_primary@>@/
{

   Pointer_Vector<Ellipsoid>* pv 
         = static_cast<Pointer_Vector<Ellipsoid>*>(@=$2@>); 

   @=$$@> = pv->ctr;

   delete pv;

};

@q ***** (5) numeric_primary --> SIZE sphere_vector_primary.@>

@*4 \§numeric primary> $\longrightarrow$ \.{SIZE} \§sphere vector primary>.
\initials{LDF 2005.12.01.}

\LOG
\initials{LDF 2005.12.01.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=numeric_primary: SIZE sphere_vector_primary@>@/
{

   Pointer_Vector<Sphere>* pv 
         = static_cast<Pointer_Vector<Sphere>*>(@=$2@>); 

   @=$$@> = pv->ctr;

   delete pv;

};

@q ***** (5) numeric_primary --> SIZE cuboid_vector_primary.@>

@*4 \§numeric primary> $\longrightarrow$ \.{SIZE} \§cuboid vector primary>.
\initials{LDF 2004.12.14.}

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=numeric_primary: SIZE cuboid_vector_primary@>@/
{

   Pointer_Vector<Cuboid>* pv 
         = static_cast<Pointer_Vector<Cuboid>*>(@=$2@>); 

   @=$$@> = pv->ctr;

   delete pv;

};

@q ***** (5) numeric_primary --> SIZE polyhedron_vector_primary.@>

@*4 \§numeric primary> $\longrightarrow$ \.{SIZE} 
\§polyhedron vector primary>.
\initials{LDF 2005.01.14.}

\LOG
\initials{LDF 2005.01.14.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=numeric_primary: SIZE polyhedron_vector_primary@>@/
{

   Pointer_Vector<Polyhedron>* pv 
         = static_cast<Pointer_Vector<Polyhedron>*>(@=$2@>); 

   @=$$@> = pv->ctr;

   delete pv;

};

@q ***** (5) numeric_primary --> SIZE picture_vector_variable.@>

@*4 \§numeric primary> $\longrightarrow$ \.{SIZE} 
\§picture vector primary>.
\initials{LDF 2005.01.17.}

\LOG
\initials{LDF 2005.01.17.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=numeric_primary: SIZE picture_vector_variable@>@/
{


   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$2@>);

@q ******* (7) Error handling:  |entry == 0|.@> 

@ Error handling:  |entry == 0|.
\initials{LDF 2005.01.17.}

@<Define rules@>=

   if (entry == static_cast<Id_Map_Entry_Node>(0))
      {

         @=$$@> = INVALID_REAL;

      }  /* |if (entry == 0)|  */

@q ******* (7) |entry != 0|.@>     

@ |entry != 0|.
\initials{LDF 2005.01.17.}

@<Define rules@>=

   else  /* |entry != 0|  */
      {
          if (entry->object == static_cast<void*>(0))
             @=$$@> = ZERO_REAL;

          else  /* |entry->object != 0|  */
             {
                 Pointer_Vector<Picture>* pv 
                    = static_cast<Pointer_Vector<Picture>*>(
                                                        entry->object); 
                 @=$$@> = pv->ctr;

             }  /* |else| (|entry->object != 0|)  */

      }  /* |else| (|entry != 0|)  */

@q ******* (7) @> 

};

@q ***** (5) numeric_primary --> SIZE macro_vector_variable.@>

@*4 \§numeric primary> $\longrightarrow$ \.{SIZE} \§macro vector primary>.
\initials{LDF 2005.01.06.}

\LOG
\initials{LDF 2005.01.06.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=numeric_primary: SIZE macro_vector_variable@>@/
{


   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$2@>);

@q ******* (7) Error handling:  |entry == 0|.@> 

@ Error handling:  |entry == 0|.
\initials{LDF 2005.01.06.}

@<Define rules@>=

   if (entry == static_cast<Id_Map_Entry_Node>(0))
      {
         @=$$@> = INVALID_REAL;

      }  /* |if (entry == 0)|  */

@q ******* (7) |entry != 0|.@>     

@ |entry != 0|.
\initials{LDF 2005.01.06.}

@<Define rules@>=

   else  /* |entry != 0|  */
      {
          if (entry->object == static_cast<void*>(0))
             @=$$@> = ZERO_REAL;

          else  /* |entry->object != 0|  */
             {
                 Pointer_Vector<Definition_Info_Type>* pv 
                    = static_cast<Pointer_Vector<Definition_Info_Type>*>(
                                                               entry->object); 
                 @=$$@> = pv->ctr;

             }  /* |else| (|entry->object != 0|)  */

      }  /* |else| (|entry != 0|)  */

@q ******* (7) @> 

};


@q ***** (5) numeric_primary --> point_part point_primary.@>

@*4 \§numeric primary> $\longrightarrow$ \§point part> \§point primary>.

\TODO
@q { @>
@:TO DO}{{\bf TO DO}@>
@q } @> 
@q !! TO DO:  @>
\initials{LDF 2004.05.05.}  
This rule currently only extracts coordinates from |Point::world_coordinates|.
I want to make it possible to get coordinates from the other coordinate
vectors, either by modifying this rule, or by adding one or more others.
\ENDTODO 

\LOG
\initials{LDF 2004.05.05.}  
Added this rule.

\initials{LDF 2007.11.04.}
Removed code from this rule.  Now calling 
|Scan_Parse::numeric_primary_rule_func_1|, which contains the old code.
\ENDLOG 

@q ****** (6) Definition.  @>

@<Define rules@>=
@=numeric_primary: point_part point_primary@>@/
{

    @=$$@> = numeric_primary_rule_func_1(@=$1@>, @=$2@>, parameter);

};


@q ***** (5) numeric_primary --> point_part transform_primary.@>

@*4 \§numeric primary> $\longrightarrow$ 
\§transform part> \§transform primary>.
\initials{LDF 2004.10.06.}

@<Define rules@>=
@=numeric_primary: point_part transform_primary@>@/
{

  Transform* t = static_cast<Transform*>(@=$2@>);

  int part = @=$1@>;

  if (part == XPART)
    @=$$@> = t->get_element(3, 0);
  
  else if (part == YPART)
    @=$$@> = t->get_element(3, 1);

  else if (part == ZPART)
    @=$$@> = t->get_element(3, 2);

  else if (part == WPART)
    @=$$@> = t->get_element(3, 3);

  
  else
    {
#if 0 
      cerr_strm << thread_name << "ERROR! In `yyparse()', rule "
                << "`numeric_primary --> "
                << endl 
                << "point_part transform_primary': "
                << "Invalid `point_part': " << name_map[part]
                << "." << endl << "Setting `numeric_primary' "
                << "to `INVALID_REAL' "
                << "and will try to continue.";

      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");
#endif 
      @=$$@> = INVALID_REAL;

    } /* |else|  */


  delete t;


};

@q ***** (5) numeric_primary --> transform_part transform_primary.@>

@*4 \§numeric primary> $\longrightarrow$ 
\§transform part> \§transform primary>.
\initials{LDF 2004.10.06.}

@<Define rules@>=
@=numeric_primary: transform_part transform_primary@>@/
{

  Transform* t = static_cast<Transform*>(@=$2@>);

  int part = @=$1@>;

  if (part == XXPART)
    @=$$@> = t->get_element(0, 0);

  else if (part == XYPART)
    @=$$@> = t->get_element(1, 0);

  else if (part == XZPART)
    @=$$@> = t->get_element(2, 0);


  else if (part == YXPART)
    @=$$@> = t->get_element(0, 0);

  else if (part == YYPART)
    @=$$@> = t->get_element(1, 0);

  else if (part == YZPART)
    @=$$@> = t->get_element(2, 0);


  else if (part == ZXPART)
    @=$$@> = t->get_element(0, 0);

  else if (part == ZYPART)
    @=$$@> = t->get_element(1, 0);

  else if (part == ZZPART)
    @=$$@> = t->get_element(2, 0);


  else if (part == WXPART)
    @=$$@> = t->get_element(0, 0);

  else if (part == WYPART)
    @=$$@> = t->get_element(1, 0);

  else if (part == WZPART)
    @=$$@> = t->get_element(2, 0);


  else
    {
#if 0 
      cerr_strm << thread_name << "ERROR! In `yyparse()', rule "
                << "`numeric_primary --> "
                << endl 
                << "transform_part transform_primary': "
                << "Invalid `transform_part': " << name_map[part]
                << "." << endl << "Setting `numeric_primary' "
                << "to `INVALID_REAL' "
                << "and will try to continue.";

      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");
#endif

      @=$$@> = INVALID_REAL;

    } /* |else|  */



  delete t;

};

@q ***** (5) numeric_primary --> color_part color_primary.@>

@*4 \§numeric primary> $\longrightarrow$ 
\§color part> \§color primary>.
\initials{LDF 2004.12.16.}

\LOG
\initials{LDF 2004.12.16.}
Added this rule.

\initials{LDF 2004.12.20.}
Now setting \§numeric primary> to 0 if \§color part> is
\.{YELLOW\_PART}, 
\.{CYAN\_PART}, 
\.{MAGENTA\_PART}, 
\.{BLACK\_PART}, 
\.{WHITE\_PART}, 
\.{RED\_ORANGE\_PART}, or
\.{BLUE\_VIOLET\_PART}.  No warning is issued.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=numeric_primary: color_part color_primary@>@/
{

  Color* c = static_cast<Color*>(@=$2@>);

  int part = @=$1@>;

  if (part == RED_PART)
    @=$$@> = c->get_red_part();

  else if (part == GREEN_PART)
    @=$$@> = c->get_green_part();

  else if (part == BLUE_PART)
    @=$$@> = c->get_blue_part();

  else if (part == YELLOW_PART)
    @=$$@> = 0;

  else if (part == CYAN_PART)
    @=$$@> = ZERO_REAL;

  else if (part == MAGENTA_PART)
    @=$$@> = ZERO_REAL;

  else if (part == BLACK_PART)
    @=$$@> = ZERO_REAL;

  else if (part == WHITE_PART)
    @=$$@> = ZERO_REAL;

  else if (part == RED_ORANGE_PART)
    @=$$@> = ZERO_REAL;

  else if (part == BLUE_VIOLET_PART)
    @=$$@> = ZERO_REAL;

  else
    {

#if 0
      cerr_strm << thread_name << "ERROR! In `yyparse()', rule "
                << "`numeric_primary --> "
                << endl 
                << "color_part color_primary': "
                << "Invalid `color_part': " << name_map[part]
                << "." << endl << "Setting `numeric_primary' "
                << "to `INVALID_REAL' "
                << "and will try to continue.";

      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");
#endif

      @=$$@> = INVALID_REAL;

    } /* |else|  */


  delete c;

};


@q **** (4) point_part.@>
@*3 \§point part>.

\LOG
\initials{LDF 2004.05.05.}  
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <int_value> point_part@>@/


@q ***** (5) point_part --> XPART.  @>
@ \§point part> $\longrightarrow$ \.{XPART}.

\LOG
\initials{LDF 2004.05.05.}  Added this rule.
\ENDLOG 

@<Define rules@>=
@=point_part: XPART@>@/
{

  @=$$@> = XPART;

};

@q ***** (5) point_part --> YPART.  @>
@ \§point part> $\longrightarrow$ \.{YPART}.

\LOG
\initials{LDF 2004.05.05.}  
Added this rule.
\ENDLOG 

@<Define rules@>=
@=point_part: YPART@>@/
{

  @=$$@> = YPART;

};

@q ***** (5) point_part --> ZPART.  @>
@ \§point part> $\longrightarrow$ \.{ZPART}.

\LOG
\initials{LDF 2004.05.05.}  
Added this rule.
\ENDLOG 

@<Define rules@>=
@=point_part: ZPART@>@/
{

  @=$$@> = ZPART;

};

@q ***** (5) point_part --> WPART.  @>
@ \§point part> $\longrightarrow$ \.{WPART}.

\LOG
\initials{LDF 2004.05.05.}  
Added this rule.
\ENDLOG 

@<Define rules@>=
@=point_part: WPART@>@/
{

  @=$$@> = WPART;

};

@q **** (4) transform_part.@> 
@*3 \§transform part>.
\initials{LDF 2004.10.07.}

\LOG
\initials{LDF 2004.10.07.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <int_value> transform_part@>@/
 

@q ***** (5) transform_part --> XXPART.@> 
@*4 \§transform part> $\longrightarrow$ \.{XXPART}.
\initials{LDF 2004.10.07.}

\LOG
\initials{LDF 2004.10.07.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=transform_part: XXPART@>@/
{
  @=$$@> = XXPART;

};

@q ***** (5) transform_part --> XYPART.@> 
@*4 \§transform part> $\longrightarrow$ \.{XYPART}.
\initials{LDF 2004.10.07.}

\LOG
\initials{LDF 2004.10.07.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=transform_part: XYPART@>@/
{

  @=$$@> = XYPART;

};


@q ***** (5) transform_part --> XZPART.@> 
@*4 \§transform part> $\longrightarrow$ \.{XZPART}.
\initials{LDF 2004.10.07.}

\LOG
\initials{LDF 2004.10.07.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=transform_part: XZPART@>@/
{

  @=$$@> = XZPART;

};


@q ***** (5) transform_part --> YXPART.@> 
@*4 \§transform part> $\longrightarrow$ \.{YXPART}.
\initials{LDF 2004.10.07.}

\LOG
\initials{LDF 2004.10.07.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=transform_part: YXPART@>@/
{

  @=$$@> = YXPART;

};

@q ***** (5) transform_part --> YYPART.@> 
@*4 \§transform part> $\longrightarrow$ \.{YYPART}.
\initials{LDF 2004.10.07.}

\LOG
\initials{LDF 2004.10.07.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=transform_part: YYPART@>@/
{

  @=$$@> = YYPART;

};



@q ***** (5) transform_part --> YZPART.@> 
@*4 \§transform part> $\longrightarrow$ \.{YZPART}.
\initials{LDF 2004.10.07.}

\LOG
\initials{LDF 2004.10.07.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=transform_part: YZPART@>@/
{

  @=$$@> = YZPART;

};


@q ***** (5) transform_part --> ZXPART.@> 
@*4 \§transform part> $\longrightarrow$ \.{ZXPART}.
\initials{LDF 2004.10.07.}

\LOG
\initials{LDF 2004.10.07.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=transform_part: ZXPART@>@/
{

  @=$$@> = ZXPART;

};


@q ***** (5) transform_part --> ZYPART.@> 
@*4 \§transform part> $\longrightarrow$ \.{ZYPART}.
\initials{LDF 2004.10.07.}

\LOG
\initials{LDF 2004.10.07.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=transform_part: ZYPART@>@/
{

  @=$$@> = ZYPART;

};

@q ***** (5) transform_part --> ZZPART.@> 
@*4 \§transform part> $\longrightarrow$ \.{ZZPART}.
\initials{LDF 2004.10.07.}

\LOG
\initials{LDF 2004.10.07.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=transform_part: ZZPART@>@/
{

  @=$$@> = ZZPART;

};


@q ***** (5) transform_part --> WXPART.@> 
@*4 \§transform part> $\longrightarrow$ \.{WXPART}.
\initials{LDF 2004.10.07.}

\LOG
\initials{LDF 2004.10.07.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=transform_part: WXPART@>@/
{

  @=$$@> = WXPART;

};


@q ***** (5) transform_part --> WYPART.@> 
@*4 \§transform part> $\longrightarrow$ \.{WYPART}.
\initials{LDF 2004.10.07.}

\LOG
\initials{LDF 2004.10.07.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=transform_part: WYPART@>@/
{

  @=$$@> = WYPART;

};

@q ***** (5) transform_part --> WZPART.@> 
@*4 \§transform part> $\longrightarrow$ \.{WZPART}.
\initials{LDF 2004.10.07.}

\LOG
\initials{LDF 2004.10.07.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=transform_part: WZPART@>@/
{

  @=$$@> = WZPART;

};

@q **** (4) color_part.@>
@*3 \§color part>.

\LOG
\initials{LDF 2004.12.16.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <int_value> color_part@>@/

@q ***** (5) color_part --> RED_PART.  @>
@ \§color part> $\longrightarrow$ \.{RED\_PART}.

\LOG
\initials{LDF 2004.12.16.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=color_part: RED_PART@>@/
{

  @=$$@> = RED_PART;

};

@q ***** (5) color_part --> GREEN_PART.  @>
@ \§color part> $\longrightarrow$ \.{GREEN\_PART}.

\LOG
\initials{LDF 2004.12.16.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=color_part: GREEN_PART@>@/
{

  @=$$@> = GREEN_PART;

};

@q ***** (5) color_part --> BLUE_PART.  @>
@ \§color part> $\longrightarrow$ \.{BLUE\_PART}.

\LOG
\initials{LDF 2004.12.16.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=color_part: BLUE_PART@>@/
{

  @=$$@> = BLUE_PART;

};


@q ***** (5) color_part --> YELLOW_PART.  @>
@ \§color part> $\longrightarrow$ \.{YELLOW\_PART}.

\LOG
\initials{LDF 2004.12.16.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=color_part: YELLOW_PART@>@/
{

  @=$$@> = YELLOW_PART;

};

@q ***** (5) color_part --> CYAN_PART.  @>
@ \§color part> $\longrightarrow$ \.{CYAN\_PART}.

\LOG
\initials{LDF 2004.12.16.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=color_part: CYAN_PART@>@/
{

  @=$$@> = CYAN_PART;

};

@q ***** (5) color_part --> MAGENTA_PART.  @>
@ \§color part> $\longrightarrow$ \.{MAGENTA\_PART}.

\LOG
\initials{LDF 2004.12.16.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=color_part: MAGENTA_PART@>@/
{

  @=$$@> = MAGENTA_PART;

};

@q ***** (5) color_part --> BLACK_PART.  @>
@ \§color part> $\longrightarrow$ \.{BLACK\_PART}.

\LOG
\initials{LDF 2004.12.16.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=color_part: BLACK_PART@>@/
{

  @=$$@> = BLACK_PART;

};

@q ***** (5) color_part --> WHITE_PART.  @>
@ \§color part> $\longrightarrow$ \.{WHITE\_PART}.

\LOG
\initials{LDF 2004.12.16.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=color_part: WHITE_PART@>@/
{

  @=$$@> = WHITE_PART;

};

@q ***** (5) color_part --> RED_ORANGE_PART.  @>
@ \§color part> $\longrightarrow$ \.{RED\_ORANGE\_PART}.

\LOG
\initials{LDF 2004.12.16.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=color_part: RED_ORANGE_PART@>@/
{

  @=$$@> = RED_ORANGE_PART;

};

@q ***** (5) color_part --> BLUE_VIOLET_PART.  @>
@ \§color part> $\longrightarrow$ \.{BLUE\_VIOLET\_PART}.

\LOG
\initials{LDF 2004.12.16.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=color_part: BLUE_VIOLET_PART@>@/
{

  @=$$@> = BLUE_VIOLET_PART;

};

@q ***** (5) TURNING_NUMBER path_primary.  @>

@*4 \.{TURNING\_NUMBER} \§path primary>.

\TODO
@q { @>
@:TO DO}{{\bf TO DO}@>
@q } @> 
@q !! TO DO:  @>
\initials{LDF 2004.04.28.}
Add this rule.  
\ENDTODO 

@q ***** (5) TOTALWEIGHT picture_primary.  @>

@*4 \.{TOTALWEIGHT} \§picture primary>.

\TODO
@q { @>
@:TO DO}{{\bf TO DO}@>
@q } @> 
@q !! TO DO:  @>
\initials{LDF 2004.04.28.}
Add this rule.  
\ENDTODO 

@q ***** (5) numeric_primary --> numeric_operator numeric_primary.  @>

@*4 \§numeric primary> $\longrightarrow$ 
\§numeric operator> \§numeric primary>.

\LOG
\initials{LDF 2004.04.28.}  
Added code for all cases.  Some contain dummy code,
however. 

\initials{LDF 2007.02.06.}
Now calling |Scan_Parse::numeric_primary_rule_func_0|.
\ENDLOG 

\TODO
@q { @>
@:TO DO}{{\bf TO DO}@>
@q } @> 
@q !! TO DO:  @>
\initials{LDF Undated.}
Program the |UNIFORMDEVIATE| case. 
\ENDTODO 

@<Define rules@>=
@=numeric_primary: numeric_operator numeric_primary@>
{ 
#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @; 
    if (DEBUG) 
      cerr << "\n*** Parser: numeric_primary "
           << "--> numeric_operator numeric_primary." << endl;
#endif /* |DEBUG_COMPILE|  */

       real r;

       int i = numeric_primary_rule_func_0(@=$1@>,  
                                           @=$2@>, 
                                           &r,                               
                                           static_cast<Scanner_Node>(
                                              parameter));   

       @=$$@> = (i == 0) ? r : INVALID_REAL;

} /* End of rule |@=numeric_operator numeric_primary@>|.  */
;


@q ***** (5) numeric_primary --> LAST numeric_vector_expression.@>

@*4 \§numeric primary> $\longrightarrow$ 
\.{LAST} \§numeric vector expression>.
\initials{LDF 2005.01.07.}

\LOG
\initials{LDF 2005.01.07.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=numeric_primary: LAST numeric_vector_expression@>@/
{ 

   Pointer_Vector<real>* pv 
      = static_cast<Pointer_Vector<real>*>(@=$2@>);

@q ******* (7) Error handling:  |pv == 0|.@> 

@ Error handling:  |pv == 0|.
\initials{LDF 2005.01.07.}

@<Define rules@>=

   if (pv == static_cast<Pointer_Vector<real>*>(0))
      {
          @=$$@> = INVALID_REAL;

      }  /* |if (pv == 0)|  */

@q ******* (7) Error handling:  |pv->ctr == 0|.@> 

@ Error handling:  |pv->ctr == 0|.
\initials{LDF 2005.01.07.}

@<Define rules@>=

   else if (pv->ctr == 0)
      {

          @=$$@> = INVALID_REAL;

      }  /* |else if (pv->ctr == 0)|  */

@q ******* (7) |pv != 0 && pv->ctr > 0|.@> 

@ |pv != 0 && pv->ctr > 0|.  Set |@=$$@>| to |*(pv->v[pv->ctr - 1])|.
\initials{LDF 2005.01.07.}

@<Define rules@>=

   else 
      @=$$@> = *(pv->v[pv->ctr - 1]);

@q ******* (7) @> 

};

@q ***** (5) numeric_primary --> GET_DISTANCE focus_expression.  @>

@ \§numeric primary> $\longrightarrow$ \.{GET\_DISTANCE}
\§focus expression>.
\initials{LDF 2007.09.24.}

\LOG
\initials{LDF 2007.09.24.}
Added this rule.
\ENDLOG 

@<Define rules@>= 

@=numeric_primary: GET_DISTANCE focus_expression@>@/
{

  Focus* f = static_cast<Focus*>(@=$2@>); 

  if (f)
  {
     @=$$@> = f->get_distance();
     delete f;
  }
  else
     @=$$@> = INVALID_REAL;

};

@q ***** (5) numeric_primary --> GET_DIAMETER circle_expression.  @>

@ \§numeric primary> $\longrightarrow$ \.{GET\_DIAMETER}
\§circle expression>.
\initials{LDF 2007.11.04.}

\LOG
\initials{LDF 2007.11.04.}
Added this rule.
\ENDLOG 

@<Define rules@>= 

@=numeric_primary: GET_DIAMETER circle_expression@>@/
{

  Circle* c = static_cast<Circle*>(@=$2@>); 

  if (c)
  {
     @=$$@> = c->get_diameter();
     delete c;
  }
  else
     @=$$@> = INVALID_REAL;

};



@q ***** (5) numeric_primary --> GET_DIAMETER circle_expression.  @>

@ \§numeric primary> $\longrightarrow$ \.{GET\_DIAMETER}
\§circle expression>.
\initials{LDF 2009.09.15.}

\LOG
\initials{LDF 2009.09.15.}
Added this rule.
\ENDLOG 

@<Define rules@>= 

@=numeric_primary: GET_RADIUS circle_expression@>@/
{

  Circle* c = static_cast<Circle*>(@=$2@>); 

  if (c)
  {
     @=$$@> = c->get_radius();
     delete c;
  }
  else
     @=$$@> = INVALID_REAL;

};


@q ***** (5) numeric_primary --> CIRCUMFERENCE circle_expression.  @>

@ \§numeric primary> $\longrightarrow$ \.{CIRCUMFERENCE}
\§circle expression>.
\initials{LDF 2009.09.16.}

\LOG
\initials{LDF 2009.09.16.}
Added this rule.
\ENDLOG 

@<Define rules@>= 

@=numeric_primary: CIRCUMFERENCE circle_expression@>@/
{

  Circle* c = static_cast<Circle*>(@=$2@>); 

  if (c)
  {
     @=$$@> = c->get_circumference();
     delete c;
  }
  else
     @=$$@> = INVALID_REAL;

};

@q ***** (5) numeric_primary --> DIRECTIONTIME picture_primary.  @>

@*4 \§numeric primary> $\longrightarrow$ 
\.{DIRECTIONTIME} \§picture primary>.
\initials{LDF Undated.}

\TODO
@q { @>
@:TO DO}{{\bf TO DO}@>
@q } @> 
@q !! TO DO:  @>
\initials{LDF 2004.04.28.}
Add this rule.  
\ENDTODO 

@q **** (4) numeric_operator.  @>
@ \§numeric operator>.

@<Type declarations for non-terminal symbols@>=
@=%type <int_value> numeric_operator@>@/


@q ***** (5) numeric_operator --> SQRT.@>
@*4 \§numeric operator> $\longrightarrow$ \.{SQRT}.
\initials{LDF 2004.04.28.}  

\LOG
\initials{LDF 2004.04.28.}  
Added this rule.
\ENDLOG 

@<Define rules@>= 

@=numeric_operator: SQRT@>@;  
{
#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @; 
    if (DEBUG) 
      cerr << "\n*** Parser: numeric_operator "
           << "(SQRT)." << endl;
#endif /* |DEBUG_COMPILE|  */

  @=$$@> = SQRT;

};

@q ***** (5) Trigonometric functions.@>
@*4 Trigonometric functions.
\initials{LDF 2007.02.06.}

\LOG
\initials{LDF 2007.02.06.}
Added this section.
\ENDLOG

@q ****** (6) numeric_operator --> SIND.@>

@*5 \§numeric operator> $\longrightarrow$ \.{SIND}.
\initials{LDF 2004.04.28.}  

\LOG
\initials{LDF 2004.04.28.}  
Added this rule.
\ENDLOG 

@<Define rules@>= 

@=numeric_operator: SIND@>@;  
{
    @=$$@> = SIND;
};

@q ****** (6) numeric_operator --> COSD.  @>

@*5 \§numeric operator> $\longrightarrow$ \.{COSD}.
\initials{LDF 2004.04.28.}  

\LOG
\initials{LDF 2004.04.28.}  
Added this rule.
\ENDLOG 

@<Define rules@>= 

@=numeric_operator: COSD@>@;  
{
  @=$$@> = COSD;
};

@q ****** (6) numeric_operator --> TAND.@>

@*5 \§numeric operator> $\longrightarrow$ \.{TAND}.
\initials{LDF 2007.02.06.}

\LOG
\initials{LDF 2007.02.06.}
Added this rule.
\ENDLOG 

@<Define rules@>= 

@=numeric_operator: TAND@>@;  
{

  @=$$@> = TAND;

};

@q ****** (6) numeric_operator --> ARCSIND.@>

@*5 \§numeric operator> $\longrightarrow$ \.{ARCSIND}.
\initials{LDF 2007.02.06.}

\LOG
\initials{LDF 2007.02.06.}
Added this rule.
\ENDLOG 

@<Define rules@>= 

@=numeric_operator: ARCSIND@>@;  
{

  @=$$@> = ARCSIND;

};


@q ****** (6) numeric_operator --> ARCCOSD.@>

@*5 \§numeric operator> $\longrightarrow$ \.{ARCCOSD}.
\initials{LDF 2007.02.06.}

\LOG
\initials{LDF 2007.02.06.}
Added this rule.
\ENDLOG 

@<Define rules@>= 

@=numeric_operator: ARCCOSD@>@;  
{

  @=$$@> = ARCCOSD;

};

@q ****** (6) numeric_operator --> ARCTAND.@>

@*5 \§numeric operator> $\longrightarrow$ \.{ARCTAND}.
\initials{LDF 2007.02.06.}

\LOG
\initials{LDF 2007.02.06.}
Added this rule.
\ENDLOG 

@<Define rules@>= 

@=numeric_operator: ARCTAND@>@;  
{

  @=$$@> = ARCTAND;

};

@q ***** (5) numeric_operator --> MLOG.  @>
@q           numeric_operator 4. @>                       

@*4 \§numeric operator> $\longrightarrow$ \.{MLOG}.

\LOG
\initials{LDF 2004.04.28.}  
Added this rule.
\ENDLOG 

@<Define rules@>= 

@=numeric_operator: MLOG@>@;  
{

  @=$$@> = MLOG;

};

@q ***** (5) numeric_operator --> MEXP.  @>

@*4 \§numeric operator> $\longrightarrow$ \.{MEXP}.
\initials{LDF 2004.04.28.}  

\LOG
\initials{LDF 2004.04.28.}  
Added this rule.
\ENDLOG 

@<Define rules@>= 

@=numeric_operator: MEXP@>@;  
{
#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @; 
    if (DEBUG) 
      cerr << "\n*** Parser: numeric_operator "
           << "(MEXP)." << endl;
#endif /* |DEBUG_COMPILE|  */

  @=$$@> = MEXP;

}
;

@q ***** (5) numeric_operator --> FLOOR.  @>
@q           numeric_operator 6. @>                       

@ \§numeric operator> $\longrightarrow$ \.{FLOOR}.
\§numeric operator> 6.

\LOG
\initials{LDF 2004.04.28.}  Added this rule.
\ENDLOG 

@<Define rules@>= 

@=numeric_operator: FLOOR@>@;  
{
#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @; 
    if (DEBUG) 
      cerr << "\n*** Parser: numeric_operator "
           << "(FLOOR)." << endl;
#endif /* |DEBUG_COMPILE|  */

  @=$$@> = FLOOR;

};

@q ***** (5) numeric_operator --> UNIFORMDEVIATE.  @>

@ \§numeric operator> $\longrightarrow$ \.{UNIFORMDEVIATE}.

\LOG
\initials{LDF 2004.04.28.}  
Added this rule.
\ENDLOG 

@<Define rules@>= 

@=numeric_operator: UNIFORMDEVIATE@>@;  
{

  @=$$@> = UNIFORMDEVIATE;

};

@q **** (4) Numeric secondary.  @>
@*3 Numeric secondary.

@<Type declarations for non-terminal symbols@>=
@=%type <real_value> numeric_secondary@>@/

@q ***** (5) numeric_secondary --> numeric_primary.@>

@*4 \§numeric secondary> $\longrightarrow$ \§numeric primary>.  

@<Define rules@>= 
@=numeric_secondary: numeric_primary@>
{

  @=$$@> = @=$1@>;
  
};


@q ***** (5) numeric_secondary --> numeric_secondary @>
@q ***** (5) TIMES numeric_variable.                 @>

@*4 \§numeric secondary> $\longrightarrow$ \§numeric secondary> 
\.{TIMES} \§numeric variable>. 

\LOG
\initials{LDF 2004.12.07.}
Added this rule.
\ENDLOG


@q ****** (6) Definition.@> 

@<Define rules@>= 
@=numeric_secondary: numeric_secondary TIMES numeric_variable@>
{

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$3@>);

@q ******* (7) Error handling:  |entry == 0 || entry->object == 0|.@>    

@ Error handling:  |entry == 0 || entry->object == 0|.
\initials{LDF 2004.12.07.}

@<Define rules@>=

   if (entry == static_cast<Id_Map_Entry_Node>(0) || entry->object == static_cast<void*>(0))
      {

         @=$$@> = INVALID_REAL;
            
      } /* |if (entry == 0 || entry->object == 0)|  */



@q ******* (7) |entry != 0 && entry->object != 0|.@> 

@ |entry != 0 && entry->object != 0|.
\initials{LDF 2004.12.07.}

@<Define rules@>=

   else  /* |entry != 0 && entry->object != 0|  */
      {
         real* r = static_cast<real*>(entry->object);
 
         @=$$@> = @=$1@> * *r;  

      }  /* |else| (|entry != 0 && entry->object != 0|)  */

};

@q ***** (5) numeric_secondary --> numeric_secondary @>
@q ***** (5) numeric_variable.                       @>

@*4 \§numeric secondary> $\longrightarrow$ \§numeric secondary> 
\§numeric variable>. 
\initials{LDF 2004.12.10.}

Multiplication without \.{TIMES}.
\initials{LDF 2004.12.10.}

\LOG
\initials{LDF 2004.12.10.}
Added this rule.
\ENDLOG


@q ****** (6) Definition.@> 

@<Define rules@>= 
@=numeric_secondary: numeric_secondary numeric_variable@>
{

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$2@>);

@q ******* (7) Error handling:  |entry == 0 || entry->object == 0|.@>    

@ Error handling:  |entry == 0 || entry->object == 0|.
\initials{LDF 2004.12.07.}

@<Define rules@>=

   if (entry == static_cast<Id_Map_Entry_Node>(0) || entry->object == static_cast<void*>(0))
      {

         @=$$@> = INVALID_REAL;
            
      } /* |if (entry == 0 || entry->object == 0)|  */



@q ******* (7) |entry != 0 && entry->object != 0|.@> 

@ |entry != 0 && entry->object != 0|.
\initials{LDF 2004.12.07.}

@<Define rules@>=

   else  /* |entry != 0 && entry->object != 0|  */
      {
         real* r = static_cast<real*>(entry->object);
 
         @=$$@> = @=$1@> * *r;  

      }  /* |else| (|entry != 0 && entry->object != 0|)  */

};

@q ***** (5) numeric_secondary --> numeric_token    @>
@q ***** (5) OVER numeric_variable.                 @>

@*4 \§numeric secondary> $\longrightarrow$ \§numeric token> 
\.{OVER} \§numeric variable>. 

\LOG
\initials{LDF 2004.12.07.}
Added this rule.
\ENDLOG


@q ****** (6) Definition.@> 

@<Define rules@>= 
@=numeric_secondary: numeric_token OVER numeric_variable@>
{

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$3@>);

@q ******* (7) Error handling:  |entry == 0 || entry->object == 0|.@>    

@ Error handling:  |entry == 0 || entry->object == 0|.
\initials{LDF 2004.12.07.}

@<Define rules@>=

   if (entry == static_cast<Id_Map_Entry_Node>(0) || entry->object == static_cast<void*>(0))
      {
         @=$$@> = INVALID_REAL;
            
      } /* |if (entry == 0 || entry->object == 0)|  */


@q ******* (7) |entry != 0 && entry->object != 0|.@> 

@ |entry != 0 && entry->object != 0|.
\initials{LDF 2004.12.07.}

@<Define rules@>=

   else  /* |entry != 0 && entry->object != 0|  */
      {
         real* r = static_cast<real*>(entry->object);
 


@q ******** (8) Error handling:  |*r == 0|.@> 

@ Error handling:  |*r == 0|.
\initials{LDF 2004.12.07.}

@<Define rules@>=

      if (*r == ZERO_REAL)
         {
             @=$$@> = INVALID_REAL;

         } /* |if (*r == 0)|  */


@q ******** (8) @> 

      else /* |*r != 0|  */
         {

            @=$$@> = @=$1@> / *r;  

         } /* |else| (|*r != 0|)  */


      }   /* |else| (|entry != 0 && entry->object != 0|)  */

};

@q ***** (5) numeric_secondary --> numeric_secondary @>
@q ***** (5) times_or_over numeric_primary.          @>

@*4 \§numeric secondary> $\longrightarrow$ \§numeric secondary> 
\§times or over> 
\§numeric primary>. 

@<Define rules@>= 
@=numeric_secondary: numeric_secondary times_or_over numeric_primary@>
{

  @<Common declarations for rules@>@; 

  if (@=$2@> == TIMES)
    @=$$@> = @=$1@> * @=$3@>;
  else if (@=$2@> == OVER)
    {
      if (@=$3@> == ZERO_REAL)
        {

#if 0 
@q           unsigned int first_line   = @@=@@1.first_line;@@>@@;   @>
@q           unsigned int first_column = @@=@@1.first_column;@@>@@; @>
@q           unsigned int last_line    = @@=@@3.last_line;@@>@@;    @>
@q           unsigned int last_column  = @@=@@3.last_column;@@>@@;  @>
#endif 

          cerr_strm << "ERROR! ";


#if 0 
          if (scanner_node->in->type == Io_Struct::FILE_TYPE)
            cerr_strm << "In input file `" << scanner_node->in->filename << "' "
                      << first_line << "." << first_column << "--" << last_line
                      << "." << last_column << ".\n";
#endif 

          cerr_strm << "Division by 0.  Setting <numeric secondary> to 0.\n"
                    << "Will try to continue.\n";

          @=$$@> = ZERO_REAL;
            
          log_message(cerr_strm);
          cerr_message(cerr_strm, error_stop_value);
          cerr_strm.str("");

        } /* |if (@=$3@> == 0)|  */

      else
        @=$$@> = @=$1@> / @=$3@>;
    }
  else
    {
      int i = @=$2@>;

      cerr_strm << thread_name 
                << "ERROR! In `yyparse()': Invalid value for `times_or_over': "
                << i << ".\nSetting <numeric_secondary> to 0. "
                << "Will try to continue.\n";



      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");


    }  /* |else|  */  
  
#if DEBUG_COMPILE
  if (DEBUG)
     {
        cerr_strm << thread_name << "`$$' == " 
                  << @=$$@> << ".";
        
        log_message(cerr_strm);
        cerr_message(cerr_strm, error_stop_value);
        cerr_strm.str("");

     }
#endif /* |DEBUG_COMPILE|  */ 

};


@q ***** (5) numeric_secondary --> point_secondary @>
@q ***** (5) DOT_PRODUCT point_primary.            @>

@*4 \§numeric secondary> $\longrightarrow$ \§point secondary> 
\.{DOT\_PRODUCT} \§point primary>. 
\initials{LDF 2004.10.05.}

\LOG
\initials{LDF 2004.10.05.}  
Added this rule.
\ENDLOG

@q ****** (6) Definition.@>  

@<Define rules@>= 
@=numeric_secondary: point_secondary DOT_PRODUCT point_primary@>
{

  Point* p = static_cast<Point*>(@=$1@>);
  
  Point* q = static_cast<Point*>(@=$3@>);  

  real r;


        r = p->dot_product(*q);  

@q ******* (7) |Point::dot_product()| succeeded.@> 
@ |Point::dot_product()| succeeded.
Set value of rule to |r|, delete |p| and |q|, and exit rule.
\initials{LDF 2004.10.05.}

@<Define rules@>=

  delete p;
  delete q;

  @=$$@> = r;

};

@q ***** (5) numeric_secondary --> point_secondary ANGLE point_primary.  @>

@*4 \§numeric secondary> $\longrightarrow$ \§point secondary> 
\.{ANGLE} \§point>.
\initials{LDF 2004.05.05.}  

This rule doesn't correspond to a rule in Metafont.  It replaces the rule
``\§numeric primary> $\longrightarrow$ \.{ANGLE} \§point primary>''.  
The latter wouldn't make sense in 3DLDF, because 
we can't simply use the x-axis as our reference for angles.
The function |Point::angle()| requires two operands, so this rule is at the
secondary rather than the primary level.
\initials{LDF 2004.05.05.}

\LOG
\initials{LDF 2004.05.05.}  
Added this rule.

\initials{LDF 2004.10.01.}
Revised this rule.  Removed the |goto| statements and made the
debugging and error output thread-safe.
\ENDLOG 

@q ****** (6) Definition.@> 

@<Define rules@>= 
@=numeric_secondary: point_secondary ANGLE point_primary@>

{
    Point* p = static_cast<Point*>(@=$1@>); 
    Point* q = static_cast<Point*>(@=$3@>); 


@q ****** (6)  Error handling:  |p == 0 || q == 0|.  @>

@ Error handling.  |p == 0 || q == 0|.
\initials{LDF 2004.10.01.}

@<Define rules@>=

    if (p == static_cast<Point*>(0) || q == static_cast<Point*>(0))
      {

        @=$$@> = INVALID_REAL;

      }  /* |if (p == 0 || q == 0)|  */@;


@q ******* (7)  Error handling.  |*p == *q|.  @>

@ Error handling.  |*p == *q|.
\initials{LDF 2004.10.01.}

@<Define rules@>=

    else if (*p == *q)
      {

        @=$$@> = INVALID_REAL;

      }  /* |if (*p == *q)|  */@;

@q ******* (7) Success! @>

@ Success!
\initials{LDF 2004.10.01.}

@<Define rules@>=
  
  else /* |p| and |q| point to valid and non-congruent |Points|.  */
                            
    @=$$@> = p->angle(*q);


@q ******* (7) Delete |p| and |q| and exit rule.@>

@ Delete |p| and |q| and exit rule.
\initials{LDF 2004.10.01.}

@<Define rules@>=

  delete p;
  
  delete q;


};

@q ***** (5) numeric_secondary --> point_secondary        @>
@q ***** (5) LOCATION ellipse_primary.                    @>

@*4 \§numeric secondary> $\longrightarrow$ \§point secondary> 
\.{LOCATION} \§ellipse primary>. 
\initials{LDF 2004.10.23.}

\LOG
\initials{LDF 2004.10.23.}  
Added this rule.

\initials{LDF 2005.10.24.}
Changed |ellipse_like_primary| to |ellipse_primary|.
Removed debugging code.
\ENDLOG

@q ****** (6) Definition.@>  

@<Define rules@>= 
@=numeric_secondary: point_secondary LOCATION ellipse_primary@>
{

   Point* p   = static_cast<Point*>(@=$1@>);
   Ellipse* e = static_cast<Ellipse*>(@=$3@>);  

   @=$$@> = e->location(*p);

   delete p;
   delete e;   

};

@q ***** (5) numeric_secondary --> point_secondary @>
@q ***** (5) LOCATION circle_primary.              @>

@*4 \§numeric secondary> $\longrightarrow$ \§point secondary> 
\.{LOCATION} \§circle primary>. 
\initials{LDF 2004.10.23.}

\LOG
\initials{LDF 2005.10.24.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@>  

@<Define rules@>= 
@=numeric_secondary: point_secondary LOCATION circle_primary@>
{

   Point* p   = static_cast<Point*>(@=$1@>);
   Circle* e = static_cast<Circle*>(@=$3@>);  

   @=$$@> = e->location(*p);

   delete p;
   delete e;   

};

@q ***** (5) numeric_secondary --> point_secondary        @>
@q ***** (5) LOCATION parabola_primary.                    @>

@*4 \§numeric secondary> $\longrightarrow$ \§point secondary> 
\.{LOCATION} \§parabola primary>. 
\initials{LDF 2005.11.21.}

\LOG
\initials{LDF 2005.11.21.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@>  

@<Define rules@>= 
@=numeric_secondary: point_secondary LOCATION parabola_primary@>
{

   Point* p   = static_cast<Point*>(@=$1@>);
   Parabola* e = static_cast<Parabola*>(@=$3@>);  
  
   if (!(p && e))
      @=$$@> = INVALID_REAL;
   else
      @=$$@> = e->location(*p);

   delete p;
   delete e;   

};

@q ***** (5) numeric_secondary --> point_secondary        @>
@q ***** (5) LOCATION hyperbola_primary.                    @>

@*4 \§numeric secondary> $\longrightarrow$ \§point secondary> 
\.{LOCATION} \§hyperbola primary>. 
\initials{LDF 2005.11.28.}

\LOG
\initials{LDF 2005.11.28.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@>  

@<Define rules@>= 
@=numeric_secondary: point_secondary LOCATION hyperbola_primary@>
{

   Point* p     = static_cast<Point*>(@=$1@>);
   Hyperbola* e = static_cast<Hyperbola*>(@=$3@>);  
  
   if (!(p && e))
      @=$$@> = INVALID_REAL;
   else
      @=$$@> = e->location(*p);

   delete p;
   delete e;   

};

@q ***** (5) numeric_secondary --> point_secondary  @>
@q ***** (5) LOCATION ellipsoid_primary.            @>

@*4 \§numeric secondary> $\longrightarrow$ \§point secondary> 
\.{LOCATION} \§ellipsoid primary>. 
\initials{LDF 2005.12.07.}

\LOG
\initials{LDF 2005.12.07.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@>  

@<Define rules@>= 
@=numeric_secondary: point_secondary LOCATION ellipsoid_primary@>
{

   Point* p     = static_cast<Point*>(@=$1@>);
   Ellipsoid* e = static_cast<Ellipsoid*>(@=$3@>);  
  
   if (!(p && e))
      @=$$@> = INVALID_REAL;
   else
      @=$$@> = e->location(*p, static_cast<Scanner_Node>(parameter));

   delete p;
   delete e;   

};

@q ***** (5) numeric_secondary --> point_secondary  @>
@q ***** (5) LOCATION sphere_primary.               @>

@*4 \§numeric secondary> $\longrightarrow$ \§point secondary> 
\.{LOCATION} \§sphere primary>. 
\initials{LDF 2005.12.07.}

\LOG
\initials{LDF 2005.12.07.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@>  

@<Define rules@>= 
@=numeric_secondary: point_secondary LOCATION sphere_primary@>
{

   Point* p     = static_cast<Point*>(@=$1@>);
   Sphere* s = static_cast<Sphere*>(@=$3@>);  
  
   if (!(p && s))
      @=$$@> = INVALID_REAL;
   else
      @=$$@> = s->location(*p);

   delete p;
   delete s;   

};

@q ***** (5) numeric_secondary --> point_secondary  @>
@q ***** (5) LOCATION cuboid_primary.               @>

@*4 \§numeric secondary> $\longrightarrow$ \§point secondary> 
\.{LOCATION} \§cuboid primary>. 
\initials{LDF 2005.12.09.}

\LOG
\initials{LDF 2005.12.09.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@>  

@<Define rules@>= 
@=numeric_secondary: point_secondary LOCATION cuboid_primary@>
{

   Point*  p = static_cast<Point*>(@=$1@>);
   Cuboid* c = static_cast<Cuboid*>(@=$3@>);  
  
   if (!(p && c))
      @=$$@> = INVALID_REAL;
   else
      @=$$@> = c->location(*p);

   delete p;
   delete c;   

};


@q ***** (5) numeric_secondary --> point_secondary  @>
@q ***** (5) LOCATION cone_primary.            @>

@*4 \§numeric secondary> $\longrightarrow$ \§point secondary> 
\.{LOCATION} \§cone primary>. 
\initials{LDF 2006.11.08.}

\LOG
\initials{LDF 2006.11.08.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@>  

@<Define rules@>= 
@=numeric_secondary: point_secondary LOCATION cone_primary@>
{

   Point* p     = static_cast<Point*>(@=$1@>);
   Cone* c  = static_cast<Cone*>(@=$3@>);  
  
#if 0 
   if (!(p && c))
      @=$$@> = INVALID_REAL;
   else
      @=$$@> = c->location(*p, static_cast<Scanner_Node>(parameter));

   delete p;
   delete c;   
#else
   @=$$@> = INVALID_REAL;
#endif 

};


@q ***** (5) numeric_secondary --> point_secondary  @>
@q ***** (5) LOCATION cylinder_primary.            @>

@*4 \§numeric secondary> $\longrightarrow$ \§point secondary> 
\.{LOCATION} \§cylinder primary>. 
\initials{LDF 2006.11.08.}

\LOG
\initials{LDF 2006.11.08.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@>  

@<Define rules@>= 
@=numeric_secondary: point_secondary LOCATION cylinder_primary@>
{

   Point* p     = static_cast<Point*>(@=$1@>);
   Cylinder* c  = static_cast<Cylinder*>(@=$3@>);  
  
   if (!(p && c))
      @=$$@> = INVALID_REAL;
   else
      @=$$@> = c->location(*p, static_cast<Scanner_Node>(parameter));

   delete p;
   delete c;   

};


@q ***** (5) numeric_secondary --> point_secondary @>
@q ***** (5) DISTANCE_TO_PLANE path_primary.       @>

@*4 \§numeric secondary> $\longrightarrow$ \§point secondary> 
\.{DISTANCE\_TO\_PLANE} \§path primary>. 
\initials{LDF 2004.10.23.}

\LOG
\initials{LDF 2004.10.23.}  
Added this rule.

\initials{LDF 2005.10.24.}
Changed |path_like_primary| to |path_primary|.
Removed debugging code.
\ENDLOG

@q ****** (6) Definition.@>  

@<Define rules@>= 
@=numeric_secondary: point_secondary DISTANCE_TO_PLANE path_primary@>@/
{

   Point* p   = static_cast<Point*>(@=$1@>);
   Path*  q = static_cast<Path*>(@=$3@>);  

@q ******* (7) Error handling:  |*q| is non-planar.@>    

@ Error handling:  |*q| is non-planar.
\initials{LDF 2004.10.23.}  

@<Define rules@>=

   if (!q->is_planar())
      {
  
          @=$$@> = INVALID_REAL;
  
      } /* |if (!q->is_planar())|  */

@q ******* (7) |*q| is planar.@> 

@ |*q| is planar.
\initials{LDF 2004.10.23.}

@<Define rules@>=


   else /* |q->is_planar() == true|  */
      {
         Plane pplane = q->get_plane();

         Real_Short rs = pplane.get_distance(*p, static_cast<Scanner_Node>(parameter));

         @=$$@> = rs.first * rs.second;

      } /* |else| (|q->is_planar() == true|)  */

@q ******* (7) Delete |p| and |q| and exit rule.@> 

@ Delete |p| and |q| and exit rule.
\initials{LDF 2004.10.23.}

@<Define rules@>=

   delete p;
   delete q;   

};

@q ***** (5) numeric_secondary --> point_secondary @>
@q ***** (5) DISTANCE_ALONG_LINE path_primary.     @>

@*4 \§numeric secondary> $\longrightarrow$ \§point secondary> 
\.{DISTANCE\_ALONG\_LINE} \§path primary>. 
\initials{LDF 2004.10.25.}

\LOG
\initials{LDF 2004.10.25.}  
Added this rule.

\initials{LDF 2005.10.24.}
Changed |path_like_primary| to |path_primary|.
Removed debugging code.
\ENDLOG

@q ****** (6) Definition.@>  

@<Define rules@>= 
@=numeric_secondary: point_secondary DISTANCE_ALONG_LINE path_primary@>
{

   Point* p   = static_cast<Point*>(@=$1@>);
   Path*  q   = static_cast<Path*>(@=$3@>);  

   if (q && p && q->is_linear())
     {

         Bool_Real br = p->is_on_segment(q->get_point(0), q->get_last_point());

         @=$$@> = br.second;


     }  /* |if (q && p && q->is_linear())|  */


   else /* |!(q && p && q->is_linear())|  */
      {

          @=$$@> = INVALID_REAL;

      } /*  |else| (|!(q && p && q->is_linear())|)  */

@q ******* (7) Delete |p| and |q| and exit rule.@> 

@ Delete |p| and |q| and exit rule.
\initials{LDF 2004.10.25.}

@<Define rules@>=

   delete p;
   delete q;   

};



@q **** (4) times_or_over.  @>
@ \§times or over>. 

@<Type declarations for non-terminal symbols@>=
@=%type <int_value> times_or_over@>@/

@q ***** (5) TIMES.  @>

@ \.{TIMES}.
@<Define rules@>= 
@=times_or_over: TIMES@>
{
#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @; 
    if (DEBUG) 
      cerr << "\n*** Parser: times_or_over (TIMES)." << endl;
#endif /* |DEBUG_COMPILE|  */
  
  @=$$@> = TIMES;


}
;

@q ***** (5) OVER.  @>

@ \.{OVER}.
@<Define rules@>= 
@=times_or_over: OVER@>
{
#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @; 
    if (DEBUG) 
      cerr << "\n*** Parser: times_or_over (OVER)." << endl;
#endif /* |DEBUG_COMPILE|  */

  @=$$@> = OVER;

}
;

@q **** (4) Numeric tertiary.  @>
@ \§numeric tertiary>.

@<Type declarations for non-terminal symbols@>=
@=%type <real_value> numeric_tertiary@>@/

@q ***** (5) numeric_secondary@>

@ \§numeric secondary>.
@<Define rules@>= 
@=numeric_tertiary: numeric_secondary@>
{
#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @; 
    if (DEBUG) 
      cerr << "\n*** Parser: numeric_tertiary (numeric_secondary)." << endl;
#endif /* |DEBUG_COMPILE|  */

  @=$$@> = @=$1@>;
  
#if DEBUG_COMPILE
  if (DEBUG)
    cerr << "$$ == " << @=$$@> << endl;
#endif /* |DEBUG_COMPILE|  */ 

};

@q ***** (5) numeric_tertiary --> numeric_tertiary @>
@q ***** (5) PLUS numeric_secondary.               @>

@ \§numeric tertiary> $\longrightarrow$ \§numeric tertiary> 
\.{PLUS} \§numeric secondary>.

\LOG
\initials{LDF 2004.04.27.}  
Added this rule.

\initials{LDF 2005.01.06.}
Changed ``\§plus or minus>'' to \.{PLUS}.
Made debugging output thread-safe.
\ENDLOG 

@<Define rules@>= 
@=numeric_tertiary: numeric_tertiary PLUS numeric_secondary@>
{

   @=$$@> = @=$1@> + @=$3@>;

};

@q ***** (5) numeric_tertiary --> numeric_tertiary @>
@q ***** (5) MINUS numeric_secondary.               @>

@ \§numeric tertiary> $\longrightarrow$ \§numeric tertiary> 
\.{MINUS} \§numeric secondary>.

\LOG
\initials{LDF 2005.01.06.}
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=numeric_tertiary: numeric_tertiary MINUS numeric_secondary@>
{

   @=$$@> = @=$1@> - @=$3@>;

};




@q ***** (5) numeric_tertiary pythagorean_plus_or_minus numeric_secondary@>

@ \§numeric tertiary> \§Pythagorean plus or minus> \§numeric secondary>.

\LOG
\initials{LDF 2004.04.27.}  Added this rule.

\initials{LDF 2004.04.28.}  Added error handling and got Pythagorean 
subtraction to work.  
  
\ENDLOG 
@q ****** (6) Definition.  @>
@<Define rules@>= 
@=numeric_tertiary: numeric_tertiary pythagorean_plus_or_minus numeric_secondary@>
{

    @<Common declarations for rules@>@; 

  real r0;
  real r1;

  if (@=$1@> > MAX_REAL_SQRT)
    {

      cerr << "ERROR! In yyparse().\n"
           << "Rule `numeric_tertiary: numeric_tertiary "
           << "pythagorean_plus_or_minus" << endl 
           << "numeric_secondary'." << endl
           << "($1 == " << @=$1@> << ") > MAX_REAL_SQRT" 
           << "Can't square it."
           << endl << "Setting `numeric_tertiary' to 0 "
           << "and will try to continue.\n";

      @=$$@> = ZERO_REAL;
      goto end_numeric_tertiary_2;
    }

  else if (@=$2@> > MAX_REAL_SQRT)
    {
      cerr << "ERROR! In yyparse().\n"
           << "Rule `numeric_tertiary: numeric_tertiary "
           << "pythagorean_plus_or_minus" << endl 
           << "numeric_secondary'." << endl
           << "($2 == " << @=$2@> << ") > MAX_REAL_SQRT" 
           << "Can't square it."
           << endl << "Setting `numeric_tertiary' to 0 "
           << "and will try to continue.\n";

      @=$$@> = ZERO_REAL;
      goto end_numeric_tertiary_2;
    }

  r0 = @=$1@> * @=$1@>;
  r1 = @=$3@> * @=$3@>;

@q ****** (6) Pythagorean plus.  @>
@ Pythagorean plus.
\initials{LDF 2004.05.02.}
@<Define rules@>= 
  if (@=$2@> ==  1) 
    {
      if (MAX_REAL - r0 < r1)
        {
          cerr << "ERROR! In yyparse().\n"
               << "Rule `numeric_tertiary: numeric_tertiary "
               << "pythagorean_plus_or_minus" << endl 
               << "numeric_secondary'." << endl
               << r0 << " + " << r1 << " > MAX_REAL" 
               << "Can't perform Pythagorean addition."
               << endl << "Setting `numeric_tertiary' to 0 "
               << "and will try to continue.\n";

          @=$$@> = ZERO_REAL;

          if (scanner_node->run_state.error_stop_mode == Run_State::STOPPING)
            {
              cerr << "Type <RETURN> to continue.\n";

              getchar();  /* Don't delete this!  */@;
            } 

          goto end_numeric_tertiary_2;
        }
      else  
        @=$$@> = sqrt(r0 + r1);
    }
@q ****** (6) Pythagorean minus.  @>
@ Pythagorean minus.
\initials{LDF 2004.05.02.}

\LOG
\initials{LDF 2004.05.02.}  Added error handling for the case that $r_0 < r_1$.  
\ENDLOG 
@<Define rules@>= 
  else if (@=$2@> ==  -1)  
    {

      if (r0 < r1)
        {
          cerr << "ERROR! In yyparse().\n"
               << "Rule `numeric_tertiary: numeric_tertiary "
               << "pythagorean_plus_or_minus" << endl 
               << "numeric_secondary'." << endl
               << r0 << " < " << r1 << ". "
               << "Can't perform Pythagorean subtraction."
               << endl << "Setting `numeric_tertiary' to 0 "
               << "and will try to continue.\n";

          @=$$@> = ZERO_REAL;

          if (scanner_node->run_state.error_stop_mode == Run_State::STOPPING)
            {
              cerr << "Type <RETURN> to continue.\n";
              getchar();  /* Don't delete this!  */@;
            } 

          goto end_numeric_tertiary_2;
        }

      @=$$@> = sqrt(r0 - r1);

    } /* |else if (@=$2@> ==  -1)|   */

@q ****** (6) Error handling.  @>
@ Error handling.
\initials{LDF 2004.05.02.}
@<Define rules@>=
  else
    {
      cerr << "ERROR! In yyparse().\n"
           << "Rule `numeric_tertiary: numeric_tertiary "
           << "pythagorean_plus_or_minus" << endl 
           << "numeric_secondary'." << endl
           << "$2 has invalid value: " << @=$2@>
           << endl << "Setting `numeric_tertiary' to 0 "
           << "and will try to continue.\n";

      @=$$@> = ZERO_REAL;

      goto end_numeric_tertiary_2;
    }

@ End.
\initials{LDF 2004.05.02.}
@<Define rules@>=
#if DEBUG_COMPILE
  if (DEBUG)
    {
      cerr << "$$ == " << @=$$@> << endl;
    }
#endif /* |DEBUG_COMPILE|  */ 
 end_numeric_tertiary_2:
  ; /* Do nothing.  */
}
;

@q **** (4) pythagorean_plus_or_minus.  @>
@ \§Pythagorean plus or minus>.

@<Type declarations for non-terminal symbols@>=
@=%type <int_value> pythagorean_plus_or_minus@>@/

@q ***** (5) PYTHAGOREAN_PLUS.  @>

@
@<Define rules@>= 
@=pythagorean_plus_or_minus: PYTHAGOREAN_PLUS@>
{
#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @; 
    if (DEBUG) 
      cerr << "\n*** Parser: pythagorean_plus_or_minus (PYTHAGOREAN_PLUS)." << endl;
#endif /* |DEBUG_COMPILE|  */
  @=$$@> = 1;
}  
;

@q ***** (5) PYTHAGOREAN_MINUS.  @>

@
@<Define rules@>= 
@=pythagorean_plus_or_minus: PYTHAGOREAN_MINUS@>
{
#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @; 
    if (DEBUG) 
      cerr << "\n*** Parser: pythagorean_plus_or_minus (PYTHAGOREAN_MINUS)." << endl;
#endif /* |DEBUG_COMPILE|  */
  @=$$@> = -1;

};

@q **** (4) numeric_expression.  @>
@ \§numeric expression>.

@<Type declarations for non-terminal symbols@>=
@=%type <real_value> numeric_expression@>@/

@
@<Define rules@>= 
@=numeric_expression: numeric_tertiary@>
{
#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @; 
    if (DEBUG) 
      cerr << "\n*** Parser: numeric_expression (numeric_tertiary)." << endl;
#endif /* |DEBUG_COMPILE|  */

  @=$$@> = @=$1@>;
  
#if DEBUG_COMPILE
  if (DEBUG)
    cerr << "$$ == " << @=$$@> << endl;
#endif /* |DEBUG_COMPILE|  */ 

}
;

@q **** (4) numeric_token.  @>
@ \§numeric token>.  

@<Type declarations for non-terminal symbols@>=
@=%type <real_value> numeric_token@>@/

@
@<Define rules@>= 

@q ***** (5) INTEGER.  @>

@=numeric_token: INTEGER@>
{
#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @; 
  if (DEBUG) 
    cerr << "\n*** Parser: numeric_token (INTEGER)."
         << endl;
#endif /* |DEBUG_COMPILE|  */

  @=$$@> = static_cast<real>(@=$1@>);

#if DEBUG_COMPILE
  if (DEBUG)
    cerr << "$$ == " << @=$$@> << endl;
#endif /* |DEBUG_COMPILE|  */ 
}
;

@q ***** (5) REAL.  @>

@=numeric_token: REAL@>
{
#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @; 
  if (DEBUG) 
    cerr << "\n*** Parser: numeric_token (REAL)."
         << endl;
#endif /* |DEBUG_COMPILE|  */

  @=$$@> = @=$1@>;


#if DEBUG_COMPILE
  if (DEBUG)
    cerr << "$$ == " << @=$$@> << endl;
#endif /* |DEBUG_COMPILE|  */ 

};

@q *** (3) numeric_single.  @>
@*2 \§numeric single>.
`\.{\LP}' \§numeric expression> `\.{\RP}'.

\LOG
\initials{LDF 2004.04.30.}  
Added this rule.
\ENDLOG 

@<Type declarations for non-terminal symbols@>=
@=%type <real_value> numeric_single@>@;

@
@<Define rules@>= 
@=numeric_single: LEFT_PARENTHESIS numeric_expression RIGHT_PARENTHESIS@>
{
#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @; 
    if (DEBUG) 
      cerr << "\n*** Parser: numeric_single "
           << "(LEFT_PARENTHESIS numeric_expression RIGHT_PARENTHESIS)."
           << endl;
#endif /* |DEBUG_COMPILE|  */

  @=$$@> = @=$2@>;

#if DEBUG_COMPILE
  if (DEBUG)
    cerr << "$$ == " << @=$$@> << endl;
#endif /* |DEBUG_COMPILE|  */ 

};

@q *** (3) numeric_list  @>
@*2 \§numeric list>.
\initials{LDF 2004.05.12.}  

It's not possible to define a rule for \§numeric list> without parentheses
because this causes reduce/reduce conflicts.
\initials{LDF 2004.05.12.}

\LOG
\initials{LDF 2004.05.12.}  
Added this type declaration.
\ENDLOG 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> numeric_list@>
  
@q **** (4) @>   

@*5 \§numeric list> $\longrightarrow$ 
\.{LEFT\_PARENTHESIS} \§numeric sublist> 
\.{RIGHT\_PARENTHESIS}.
\initials{LDF 2005.01.26.}

\LOG
\initials{LDF 2005.01.26.}
Added this rule.

\initials{LDF 2004.09.24.}
Changed |w| from |Real_Pointer_Vector*| to |Pointer_Vector<real>*|.

\initials{LDF 2007.09.19.}
@:BUG FIX@> BUG FIX:  Now using |Pointer_Vector::operator+=| to put
pointers to |real| onto the |Pointer_Vector<real>| pointed to by |w|.
The way it was before caused |w->ctr| to be 0.

\initials{LDF 2007.11.04.}
Removed code from this rule.  Now calling 
|Scan_Parse::numeric_list_rule_func_0|, which contains the old code.
\ENDLOG

@<Define rules@>=
@=numeric_list: LEFT_PARENTHESIS numeric_sublist RIGHT_PARENTHESIS@>@/ 
{

   @=$$@> = numeric_list_rule_func_0(@=$2@>, parameter);

};

@q *** (3) numeric_sublist. @>

@*2 \§numeric sublist>.
\§numeric sublist>.

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> numeric_sublist@>

@q **** (4) numeric_sublist --> numeric_expression COMMA numeric_expression.@>

@ \§numeric subvector> $\longrightarrow$ \§numeric expression> 
`\.{,}' \§numeric expression>. 

\LOG
\initials{LDF 2004.05.12.}  
Added this rule.

\initials{LDF 2007.11.25.}
Changed |scanner_node->token_real_vector| to |scanner_node->numeric_list_real_vector|.
\ENDLOG

@<Define rules@>=
@=numeric_sublist: numeric_expression COMMA numeric_expression@>@/ 
{
    Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

     scanner_node->numeric_list_real_vector.push_back(@=$1@>);
     scanner_node->numeric_list_real_vector.push_back(@=$3@>);

#if 0
   cerr << "`numeric_sublist: numeric_expression COMMA numeric_expression':"
        << endl 
        << "static_cast<Scanner_Node>(parameter)->numeric_list_real_vector.size() == " 
        << static_cast<Scanner_Node>(parameter)->numeric_list_real_vector.size() << endl;
#endif 

     @=$$@> = static_cast<void*>(0);

};

@q *** (3) numeric_sublist --> numeric_sublist COMMA numeric_expression. @>
@*2 \§numeric subvector> $\longrightarrow$ 
\§numeric subvector> \§numeric subvector> `\.{,}' \§numeric expression>. 

\LOG
\initials{LDF 2004.05.12.}  
Added this rule.

\initials{LDF 2007.11.25.}
Changed |scanner_node->token_real_vector| to |scanner_node->numeric_list_real_vector|.
\ENDLOG

@<Define rules@>=
@=numeric_sublist: numeric_sublist COMMA numeric_expression@>@/ 
{

   static_cast<Scanner_Node>(parameter)->numeric_list_real_vector.push_back(@=$3@>);

#if 0
   cerr << "`numeric_sublist: numeric_sublist COMMA numeric_expression':"
        << endl 
        << "'static_cast<Scanner_Node>(parameter)->numeric_list_real_vector.size() == " 
        << static_cast<Scanner_Node>(parameter)->numeric_list_real_vector.size() << endl;
#endif 

   @=$$@> = static_cast<void*>(0);

};


@q ***** (5) numeric_operator --> PLUS.  @>
@*4 \§numeric operator> $\longrightarrow$ 
\.{PLUS}.
\initials{LDF Undated.}

\LOG
\initials{LDF 2007.02.06.}
Now setting the semantic value of the rule to |PLUS|.
\ENDLOG 

@<Define rules@>= 

@=numeric_operator: PLUS %prec UNARY_PLUS_OR_MINUS@>@;
{
    @=$$@> = PLUS;
};

@q ***** (5) numeric_operator --> MINUS.  @>
@*4 \§numeric operator> $\longrightarrow$ \.{MINUS}.
\initials{LDF Undated.}

\LOG
\initials{LDF 2007.02.06.}
Now setting the semantic value of the rule to |MINUS|.
\ENDLOG 

@<Define rules@>= 

@=numeric_operator: MINUS %prec UNARY_PLUS_OR_MINUS@>@;
{

  @=$$@> = MINUS;

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
@q abbrev-mode:t                      @>
@q eval:(outline-minor-mode t)        @>
@q abbrev-file-name:"~/.abbrev_defs"  @>
@q eval:(read-abbrev-file)            @>
@q fill-column:80                     @>
@q run-cweave-on-file:"3DLDFprg.web"  @>
@q End:                               @>
