@q ppgvexpr.w @> 
@q Created by Laurence Finston Fr Feb 11 13:29:23 CET 2005 @>
       
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



@q * (0) |polygon_vector| expressions.@>
@** \§polygon vector> expressions.
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.02.11.}
Created this file and wrote quite a few rules.  
\ENDLOG 


@q * (1) |polygon_vector| primary.@>
@* \§polygon vector primary>.
\initials{LDF 2005.02.11.}
  
\LOG
\initials{LDF 2005.02.11.}
Added this type declaration.
\ENDLOG


@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> polygon_vector_primary@>@/

@q ** (2) polygon_vector_primary --> polygon_vector_variable.@>
@*1 \§polygon vector primary> $\longrightarrow$ 
\§polygon vector variable>.  
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.02.11.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=polygon_vector_primary: polygon_vector_variable@>@/ 
{
  @<Common declarations for rules@>@;

#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
      cerr_strm << thread_name 
                << "*** Parser: `polygon_vector_primary --> "
                << "polygon_vector_variable'.";

      log_message(cerr_strm);
      cerr_message(cerr_strm);
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */@;

  entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

@q **** (4) Error handling:  |entry == 0 || entry->object == 0|.@> 

@ Error handling:  |entry == 0 || entry->object == 0|.
\initials{LDF 2005.02.11.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0) || entry->object == static_cast<void*>(0))
    {

      cerr_strm << thread_name 
                << "ERROR! In `yyparse()', rule " 
                << endl 
                << "Rule `polygon_vector_primary --> "
                << "polygon_vector_variable':"
                << endl << "`entry' == 0 or `entry->object' == 0."
                << endl 
                << "Setting `polygon_vector_primary' "
                << "to 0 and will try to continue.";

      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");

      @=$$@> = static_cast<void*>(0);

    } /* |if (entry == 0 || entry->object == 0)|  */

@q **** (4) |!(entry == 0 || entry->object == 0)|.@> 

@ |!(entry == 0 || entry->object == 0)|.
\initials{LDF 2005.02.11.}

@<Define rules@>=
  else /* |!(entry == 0 || entry->object == 0)|  */

     {

        typedef Pointer_Vector<Polygon> PV;

        PV* pv = new PV;

        *pv = *static_cast<PV*>(entry->object);

        @=$$@> = static_cast<void*>(pv);                    

     }  /* |else| (|!(entry == 0 || entry->object == 0)|)  */

};


@q ** (2) polygon_vector_primary --> LEFT_PARENTHESIS  @>
@q ** (2) polygon_vector_expression  RIGHT_PARENTHESIS.@>

@*1 \§polygon vector primary> $\longrightarrow$ \.{LEFT\_PARENTHESIS}
\§polygon vector expression> \.{RIGHT\_PARENTHESIS}.
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.02.11.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=polygon_vector_primary: LEFT_PARENTHESIS@>@/ 
@=polygon_vector_expression RIGHT_PARENTHESIS@>@/ 
{
   
   @<Common declarations for rules@>@;

#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
       cerr_strm << thread_name 
                << "*** Parser:  `polygon_vector_primary "
                << "--> LEFT_PARENTHESIS polygon_vector_expression "
                << "RIGHT_PARENTHESIS'.";

       log_message(cerr_strm);
       cerr_message(cerr_strm);
       cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */@;

  @=$$@> = @=$1@>;

};

@*1 \§polygon vector primary> $\longrightarrow$ \.{GET\_NET}
\.{RHOMBIC\_TRIACONTAHEDRON} \§with net option list>.
\initials{LDF 2007.10.22.}

\LOG
\initials{LDF 2007.10.22.}
Added this rule.

\initials{LDF 2007.10.24.}
Changed \§path vector primary> to \§polygon vector primary>.
\ENDLOG

@<Define rules@>=

@=polygon_vector_primary: GET_NET RHOMBIC_TRIACONTAHEDRON@>@/
@=with_net_option_list@>@/ 
{
   
    @=$$@> = polyhedron_get_net_func(
                RHOMBIC_TRIACONTAHEDRON, 0, 0, parameter);

};

@q ** (2) with_net_option_list.@>
@*1 \§with net option list>.
\initials{LDF 2007.10.22.}

@q *** (3) with_net_option_list --> /* Empty  */@>
@*2 \§with net option list> $\longrightarrow$ \.{EMPTY}.
\initials{LDF 2007.10.22.}

\LOG
\initials{LDF 2007.10.22.}
Added this rule.

\initials{LDF 2007.10.24.}
Added real code to this rule.
\ENDLOG

@<Define rules@>=

@=with_net_option_list: /* Empty  */@>@/
{
   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

   if (scanner_node->polyhedron_options)
      scanner_node->polyhedron_options->clear();
   else
      scanner_node->polyhedron_options = new Polyhedron_Options;

};

@q *** (3) with_net_option_list --> with_net_option_list @>
@q *** (3) WITH_RHOMBUS_SIDE_LENGTH numeric_expression.  @>
@*2 \§with net option list> $\longrightarrow$ 
\§with net option list> \.{WITH\_RHOMBUS\_SIDE\_LENGTH} 
\§numeric expression>.
\initials{LDF 2007.10.24.}

\LOG
\initials{LDF 2007.10.24.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=with_net_option_list: with_net_option_list WITH_RHOMBUS_SIDE_LENGTH @>
@=numeric_expression@>@/
{
   static_cast<Scanner_Node>(
      parameter)->polyhedron_options->rhombus_0_side_length = @=$3@>;

};

@q * (1) polygon_vector secondary.@>

@* \§polygon vector secondary>.
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.02.11.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> polygon_vector_secondary@>
  
@q ** (2) polygon_vector secondary --> polygon_vector_primary.@>
@*1 \§polygon vector secondary> $\longrightarrow$ 
\§polygon vector primary>.
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.02.11.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=polygon_vector_secondary: polygon_vector_primary@>@/ 
{

  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
       cerr_strm << thread_name 
                 << "*** Parser:  `polygon_vector_secondary "
                 << "--> polygon_vector_primary'.";

       log_message(cerr_strm);
       cerr_message(cerr_strm);
       cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */@;
  
  @=$$@> = @=$1@>;

};

@q ** (2) polygon_vector_secondary --> polygon_secondary    @> 
@q ** (2) DECOMPOSE LEFT_PARENTHESIS point_expression COMMA @>  
@q ** (2) point_expression RIGHT_PARENTHESIS                @> 

@*1 \§polygon vector secondary> $\longrightarrow$ 
\§polygon secondary> \.{DECOMPOSE} 
\.{LEFT\_PARENTHESIS} \§point expression> \.{COMMA} 
\§point expression> \.{RIGHT\_PARENTHESIS}.
\initials{LDF 2005.02.21.}

\LOG
\initials{LDF 2005.02.21.}
Added this rule.

\initials{LDF 2005.02.21.}
Now always deleting the \§rectangle expression> and the 
\§point expressions>.

\initials{LDF 2005.02.22.}
Changed the \§rectangle expression> to a \§polygon-like expression>.

\initials{LDF 2005.02.23.}
Changed this rule from
``\§polygon vector primary> $\longrightarrow$ 
\§polygon-like expression> \.{DECOMPOSE} 
\.{LEFT\_PARENTHESIS} \§point expression> \.{COMMA} 
\§point expression> \.{RIGHT\_PARENTHESIS}''
to 
``\§polygon vector secondary> $\longrightarrow$ 
\§polygon-like secondary> \.{DECOMPOSE} 
\.{LEFT\_PARENTHESIS} \§point expression> \.{COMMA} 
\§point expression> \.{RIGHT\_PARENTHESIS}'',
i.e.,
changed ``\§polygon vector primary>'' to 
``\§polygon vector secondary>''
and 
``\§polygon-like expression>''
to ``\§polygon-like secondary>''.

\initials{LDF 2005.10.24.}
Changed |polygon_like_secondary| to |polygon_secondary|.
Removed debugging code.

\initials{LDF 2007.06.19.}
Removed code from this rule and put it into 
the section |@<Decompose |Polygon| using two |Points|@>|, which 
is now included in this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=polygon_vector_secondary: polygon_secondary DECOMPOSE@>@/
@=LEFT_PARENTHESIS point_expression COMMA point_expression@>@/ 
@=RIGHT_PARENTHESIS@>@/ 
{

    @<Decompose |Polygon| using two |Points|@>@;

};

@q ** (2) polygon_vector_secondary --> reg_polygon_secondary @> 
@q ** (2) DECOMPOSE LEFT_PARENTHESIS point_expression COMMA  @>  
@q ** (2) point_expression RIGHT_PARENTHESIS                 @> 

@*1 \§polygon vector secondary> $\longrightarrow$ 
\§regular polygon secondary> \.{DECOMPOSE} 
\.{LEFT\_PARENTHESIS} \§point expression> \.{COMMA} 
\§point expression> \.{RIGHT\_PARENTHESIS}.
\initials{LDF 2007.06.19.}

\LOG
\initials{LDF 2007.06.19.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=polygon_vector_secondary: reg_polygon_secondary DECOMPOSE@>@/
@=LEFT_PARENTHESIS point_expression COMMA point_expression@>@/ 
@=RIGHT_PARENTHESIS@>@/ 
{
    @<Decompose |Polygon| using two |Points|@>@;
};

@q ** (2) Decompose |Polygon| using two |Points|.@> 
@ Decompose {\bf Polygon} using two {\bf Points}.
\initials{LDF 2007.06.19.}

This section is used in the rules
\§polygon vector secondary> $\longrightarrow$ 
\§polygon secondary> \.{DECOMPOSE} 
\.{LEFT\_PARENTHESIS} \§point expression> \.{COMMA} 
\§point expression> \.{RIGHT\_PARENTHESIS}
and 
\§polygon vector secondary> $\longrightarrow$ 
\§regular polygon secondary> \.{DECOMPOSE} 
\.{LEFT\_PARENTHESIS} \§point expression> \.{COMMA} 
\§point expression> \.{RIGHT\_PARENTHESIS}.
\initials{LDF 2007.06.19.}

\LOG
\initials{LDF 2007.06.19.}
Added this section.
\ENDLOG

@<Decompose |Polygon| using two |Points|@>=

    Pointer_Vector<Polygon>* pv = new Pointer_Vector<Polygon>;

    int status = decompose_func(static_cast<Scanner_Node>(parameter),
                                @=$1@>,
                                POLYGON,
                                @=$4@>,
                                POINT,
                                @=$6@>,
                                POINT,
                                static_cast<void*>(pv),
                                POLYGON_VECTOR);


   if (status != 0)
      {
         
          @=$$@> = static_cast<void*>(0);

      }  /* |if (status != 0)|  */

 
   else /* |status != 0|  */
      {
 
         @=$$@> = static_cast<void*>(pv);

      }  /* |else| (|status != 0|)  */




@q ** (2) polygon_vector_secondary --> polygon_secondary @> 
@q ** (2) DECOMPOSE path_primary                         @> 

@*1 \§polygon vector secondary> $\longrightarrow$ 
\§polygon secondary> \.{DECOMPOSE} \§path primary>.
\initials{LDF 2007.06.19.}

\LOG
\initials{LDF 2007.06.19.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=polygon_vector_secondary: polygon_secondary DECOMPOSE@>@/
@=path_primary@>@/ 
{

   @<Decompose |Polygon| using |Path|@>@;

};

@q ** (2) polygon_vector_secondary --> reg_polygon_secondary @> 
@q ** (2) DECOMPOSE path_primary                             @> 

@*1 \§polygon vector secondary> $\longrightarrow$ 
\§regular polygon secondary> \.{DECOMPOSE} \§path primary>.
\initials{LDF 2007.06.19.}

\LOG
\initials{LDF 2007.06.19.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=polygon_vector_secondary: reg_polygon_secondary DECOMPOSE@>@/
@=path_primary@>@/ 
{

   @<Decompose |Polygon| using |Path|@>@;

};

@q ** (2) polygon_vector_secondary --> polygon_secondary @> 
@q ** (2) OVER path_primary                              @> 

@*1 \§polygon vector secondary> $\longrightarrow$ 
\§polygon secondary> \.{OVER} \§path primary>.
\initials{LDF 2007.06.19.}

\LOG
\initials{LDF 2007.06.19.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=polygon_vector_secondary: polygon_secondary OVER@>@/
@=path_primary@>@/ 
{

   @<Decompose |Polygon| using |Path|@>@;

};


@q ** (2) polygon_vector_secondary --> reg_polygon_secondary @> 
@q ** (2) OVER path_primary                                  @> 

@*1 \§polygon vector secondary> $\longrightarrow$ 
\§regular polygon secondary> \.{OVER} \§path primary>.
\initials{LDF 2007.06.19.}

\LOG
\initials{LDF 2007.06.19.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=polygon_vector_secondary: reg_polygon_secondary OVER@>@/
@=path_primary@>@/ 
{

   @<Decompose |Polygon| using |Path|@>@;

};


@q ** (2) Decompose |Polygon| using |Path|.@> 
@*1 Decompose {\bf Polygon} using {\bf Path}.
\initials{LDF 2007.06.19.}

This section is used in the rules 
\§polygon vector secondary> $\longrightarrow$ 
\§polygon secondary> \.{DECOMPOSE} \§path primary>.
and \§polygon vector secondary> $\longrightarrow$ 
\§polygon secondary> \.{OVER} \§path primary>, 
which are synonymous.
\initials{LDF 2007.06.19.}

\LOG
\initials{LDF 2007.06.19.}
Added this section.
\ENDLOG

@<Decompose |Polygon| using |Path|@>=

    Pointer_Vector<Polygon>* pv = new Pointer_Vector<Polygon>;
 
    int status = decompose_func(static_cast<Scanner_Node>(parameter),
                                @=$1@>,
                                POLYGON,
                                @=$3@>,
                                PATH,
                                0,
                                NULL_VALUE,
                                static_cast<void*>(pv),
                                POLYGON_VECTOR);

   if (status != 0)
      {
          @=$$@> = static_cast<void*>(0);
        
      }  /* |if (status != 0)|  */

   else /* |status == 0|  */
      {
         @=$$@> = static_cast<void*>(pv);
 
      }  /* |else| (|status == 0|)  */

@q ** (2) polygon_vector_secondary --> polygon_secondary @> 
@q ** (2) OVER polygon_primary.                          @>  

@*1 \§polygon vector secondary> $\longrightarrow$ 
\§polygon secondary> \.{OVER} \§polygon primary>.
\initials{LDF 2005.02.21.}

\LOG
\initials{LDF 2005.02.21.}
Added this rule.

\initials{LDF 2005.02.21.}
Now always deleting the \§polygon-like expressions>.

\initials{LDF 2005.02.23.}
Changed this rule from 
``\§polygon vector primary> $\longrightarrow$ 
\§polygon-like expression> \.{OVER} 
\§polygon-like expression>'' 
to
``\§polygon vector secondary> $\longrightarrow$ 
\§polygon-like secondary> \.{OVER} 
\§polygon-like primary>''.
  
\initials{LDF 2005.02.24.}
Removed code from this rule.  Now calling |Scan_Parse::decompose_func()|.

\initials{LDF 2005.10.24.}
Changed |polygon_like_secondary| and |polygon_like_primary| to 
|polygon_secondary| and |polygon_primary|, respectively.
Removed debugging code.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=polygon_vector_secondary: polygon_secondary OVER@>@/
@=polygon_primary@>@/ 
{
    Pointer_Vector<Polygon>* pv = new Pointer_Vector<Polygon>;

 
    int status = decompose_func(static_cast<Scanner_Node>(parameter),
                                @=$1@>,
                                POLYGON,
                                @=$3@>,
                                POLYGON,
                                0,
                                NULL_VALUE,
                                static_cast<void*>(pv),
                                POLYGON_VECTOR);

   if (status != 0)
      {
          @=$$@> = static_cast<void*>(0);
        
      }  /* |if (status != 0)|  */

   else /* |status == 0|  */
      {
         @=$$@> = static_cast<void*>(pv);
 
      }  /* |else| (|status == 0|)  */

@q **** (4) @>      

};

@q ** (2) polygon_vector_secondary --> polygon_secondary        @> 
@q ** (2) DECOMPOSE numeric_expression with_rectangles_optional @>  

@*1 \§polygon vector secondary> $\longrightarrow$ 
\§polygon secondary> \.{DECOMPOSE} 
\§numeric expression> \§with rectangles optional>.
\initials{LDF 2005.04.13.}

\LOG
\initials{LDF 2005.04.13.}
Added this rule.  It's for self-decomposition.

\initials{LDF 2005.04.14.}
Added the non-terminal symbol \§with rectangles optional> 
to this rule.

\initials{LDF 2005.10.24.}
Changed |polygon_like_secondary| to |polygon_secondary|.
Removed debugging code.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=polygon_vector_secondary: polygon_secondary DECOMPOSE@>@/
@=numeric_expression with_rectangles_optional@>@/ 
{

   Polygon* r = static_cast<Polygon*>(@=$1@>); 
   unsigned short u = static_cast<unsigned short>(floor(fabs(@=$3@>) + .5)); 
   
   bool use_rectangles = @=$4@>;

   Pointer_Vector<Shape>* pvs = r->decompose(u, use_rectangles,
                                             static_cast<Scanner_Node>(parameter));
@
\LOG
\initials{LDF 2005.04.14.}
Now converting calling |Pointer_Vector<Polygon, Shape>::convert| 
in order to create a |Pointer_Vector<Polygon>*| from the 
|Pointer_Vector<Shape>*| returned by |Polygon::decompose|.
\ENDLOG

@<Define rules@>=

   Pointer_Vector<Polygon>* pvp 
      = Pointer_Vector<Polygon, Shape>::convert(pvs);

   @=$$@> = static_cast<void*>(pvp); 

};


@q *** (3) with_rectangles_optional.@> 

@*1 \§with rectangles optional>.
\initials{LDF 2005.04.14.}

\LOG
\initials{LDF 2005.04.14.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <int_value> with_rectangles_optional@>@/

@q **** (4) with_rectangles_optional --> EMPTY.@>   
@*3 \§with rectangles optional> $\longrightarrow$ \.{EMPTY}.
\initials{LDF 2005.04.14.}

\LOG
\initials{LDF 2005.04.14.}
Added this rule.
\ENDLOG

@q **** (5) Definition.@> 

@<Define rules@>=

@=with_rectangles_optional: /* Empty  */@>@/  
{
   @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
   DEBUG = false;  /* |true| */ @; 
   if (DEBUG)
      {
          cerr_strm << thread_name 
                    << "*** Parser:  `with_rectangles_optional --> "
                    << "EMPTY'.";

          log_message(cerr_strm); 
          cerr_message(cerr_strm); 
          cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */@;

@q ****** (6) @>   

   @=$$@> = 0;


};

@q **** (4) with_rectangles_optional --> WITH_RECTANGLES.@>   
@*3 \§with rectangles optional> $\longrightarrow$ \.{WITH\_RECTANGLES}.
\initials{LDF 2005.04.14.}

\LOG
\initials{LDF 2005.04.14.}
Added this rule.
\ENDLOG

@q **** (5) Definition.@> 

@<Define rules@>=

@=with_rectangles_optional: WITH_RECTANGLES@>@/
{
   @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
   DEBUG = false;  /* |true| */ @; 
   if (DEBUG)
      {
          cerr_strm << thread_name 
                    << "*** Parser:  `with_rectangles_optional --> "
                    << "WITH_RECTANGLES'.";

          log_message(cerr_strm); 
          cerr_message(cerr_strm); 
          cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */@;

@q ****** (6) @>   

   @=$$@> = 1;

};



@q * (1) polygon_vector tertiary.  @>

@* \§polygon vector tertiary>.
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.02.11.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> polygon_vector_tertiary@>

@q ***** (5) polygon_vector tertiary --> polygon_vector_secondary.@>
@ \§polygon vector tertiary> $\longrightarrow$ 
\§polygon vector secondary>.
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.02.11.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=polygon_vector_tertiary: polygon_vector_secondary@>@/ 
{

   @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
       cerr_strm << thread_name 
                 << "*** Parser: `polygon_vector_tertiary "
                 << "--> polygon_vector_secondary'.";

      log_message(cerr_strm);
      cerr_message(cerr_strm);
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */@;

  @=$$@> = @=$1@>;

};


@q * (1) polygon_vector expression.@>
@* \§polygon vector expression>.

\LOG
\initials{LDF 2005.02.11.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> polygon_vector_expression@>

@q ** (2) polygon_vector expression --> polygon_vector_tertiary.  @>
@*1 \§polygon vector expression> $\longrightarrow$ 
\§polygon vector tertiary>.

\LOG
\initials{LDF 2005.02.11.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=polygon_vector_expression: polygon_vector_tertiary@>@/ 
{
   @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
       cerr_strm << thread_name 
                 << "*** Parser: `polygon_vector_expression "
                 << "--> polygon_vector_tertiary'.";

       log_message(cerr_strm);
       cerr_message(cerr_strm);
       cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */@;

  @=$$@> = @=$1@>;

};


@q * Emacs-Lisp code for use in indirect buffers when using the          @>
@q   GNU Emacs editor.  The local variable list is not evaluated when an @>
@q   indirect buffer is visited, so it's necessary to evaluate the       @>
@q   following s-expression in order to use the facilities normally      @>
@q   accessed via the local variables list.                              @>
@q   \initials{LDF 2004.02.12}.                                          @>
@q   (progn (cweb-mode) (outline-minor-mode t) (setq fill-column 70))    @>


@q Local Variables:                   @>
@q mode:CWEB                          @>
@q eval:(outline-minor-mode t)        @>
@q abbrev-file-name:"~/.abbrev_defs"  @>
@q eval:(read-abbrev-file)            @>
@q fill-column:70                     @>
@q run-cweave-on-file:"3DLDFprg.web"  @>
@q End:                               @>
