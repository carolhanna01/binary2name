@q pphvexpr.w @> 
@q Created by Laurence Finston Fri Dec 10 16:27:40 CET 2004 @>
     
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



@q * (0) |path_vector| expressions.@>
@** {\bf path\_vector} expressions.
\initials{LDF 2004.12.10.}

\LOG
\initials{LDF 2004.12.10.}
Created this file and wrote quite a few rules.  
\ENDLOG 


@q * (1) |path_vector| primary.  @>
@* \§path vector primary>.
\initials{LDF 2004.12.10.}
  
\LOG
\initials{LDF 2004.12.10.}
Added this type declaration.
\ENDLOG


@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> path_vector_primary@>@/

@q ** (2) path_vector_primary --> path_vector_variable.@>
@*1 \§path vector primary> $\longrightarrow$ 
\§path vector variable>.  
\initials{LDF 2004.12.10.}

\LOG
\initials{LDF 2004.12.10.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=path_vector_primary: path_vector_variable@>@/ 
{
  @<Common declarations for rules@>@;

#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
      cerr_strm << thread_name 
                << "*** Parser: `path_vector_primary --> "
                << "path_vector_variable'.";

      log_message(cerr_strm);
      cerr_message(cerr_strm);
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */@;

  entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

@q **** (4) Error handling:  |entry == 0 || entry->object == 0|.@> 

@ Error handling:  |entry == 0 || entry->object == 0|.
\initials{LDF 2004.12.10.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0) || entry->object == static_cast<void*>(0))
    {

      cerr_strm << thread_name 
                << "ERROR! In `yyparse()', rule " 
                << endl 
                << "Rule `path_vector_primary --> "
                << "path_vector_variable':"
                << endl << "`entry' == 0 or `entry->object' == 0."
                << endl 
                << "Setting `path_vector_primary' "
                << "to 0 and will try to continue.";

      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");

      @=$$@> = static_cast<void*>(0);

    } /* |if (entry == 0 || entry->object == 0)|  */

@q **** (4) |!(entry == 0 || entry->object == 0)|.@> 

@ |!(entry == 0 || entry->object == 0)|.
\initials{LDF 2004.12.10.}

@<Define rules@>=
  else /* |!(entry == 0 || entry->object == 0)|  */

     {

        typedef Pointer_Vector<Path> PV;

        PV* pv = new PV;

        *pv = *static_cast<PV*>(entry->object);

        @=$$@> = static_cast<void*>(pv);                    

     }  /* |else| (|!(entry == 0 || entry->object == 0)|)  */

};


@q ** (2) path_vector_primary --> LEFT_PARENTHESIS  @>
@q ** (2) path_vector_expression  RIGHT_PARENTHESIS.@>

@*1 \§path vector primary> $\longrightarrow$ \.{LEFT\_PARENTHESIS}
\§path vector expression> \.{RIGHT\_PARENTHESIS}.
\initials{LDF 2004.12.10.}

\LOG
\initials{LDF 2004.12.10.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=path_vector_primary: LEFT_PARENTHESIS@>@/ 
@=path_vector_expression RIGHT_PARENTHESIS@>@/ 
{
   
   @<Common declarations for rules@>@;

#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
       cerr_strm << thread_name 
                << "*** Parser:  `path_vector_primary "
                << "--> LEFT_PARENTHESIS path_vector_expression "
                << "RIGHT_PARENTHESIS'.";

       log_message(cerr_strm);
       cerr_message(cerr_strm);
       cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */@;

  @=$$@> = @=$1@>;

};

@q ** (2) path_vector_primary --> DEVELOP numeric_expression COMMA @>
@q        numeric_expression COMMA numeric_expression              @>

@*2 \§path vector primary> $\longrightarrow$ 
\.{DEVELOP} $\ldots$.

\§path vector primary> $\longrightarrow$ \.{DEVELOP}
 \§numeric expression> \.{COMMA} 
\§numeric expression> \.{RIGHT\_PARENTHESIS}.
\initials{LDF 2009.11.03.}

\LOG
\initials{LDF 2009.11.03.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=path_vector_primary: DEVELOP numeric_expression COMMA @>
@=numeric_expression COMMA numeric_expression@>
{ 

@q develop(real rad = 1, @>
@q         unsigned int segments = 8, @>
@q         real increment = 5.0; @>
@q         Transform* t = 0, @>
@q         Scanner_Node scanner_node = 0); @>

     @=$$@> = Sphere::develop(@=$2@>, @=$4@>, @=$6@>, 0,  
                              static_cast<Scanner_Node>(parameter));

};


@q * (1) path_vector secondary.@>

@* \§path vector secondary>.
\initials{LDF 2004.12.10.}

\LOG
\initials{LDF 2004.12.10.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> path_vector_secondary@>
  
@q ** (2) path_vector secondary --> path_vector_primary.@>
@*1 \§path vector secondary> $\longrightarrow$ 
\§path vector primary>.
\initials{LDF 2004.12.10.}

\LOG
\initials{LDF 2004.12.10.}
Added this rule.

\initials{LDF 2007.06.19.}
Moved the |@<Common declarations for rules@>| section inside the conditional 
debugging code.
\ENDLOG

@<Define rules@>=
@=path_vector_secondary: path_vector_primary@>@/ 
{
#if DEBUG_COMPILE
  @<Common declarations for rules@>@; 
   DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {


       cerr_strm << thread_name 
                 << "*** Parser:  `path_vector_secondary "
                 << "--> path_vector_primary'.";

       log_message(cerr_strm);
       cerr_message(cerr_strm);
       cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */@;
  
  @=$$@> = @=$1@>;

};

@q *** (3) path_vector_secondary --> path_secondary REFLECTED_OFF @>
@q *** (3) path_expression WITH_DIRECTION point_expression.       @>

@*2 \§path vector secondary> $\longrightarrow$ \§path secondary> 
\.{REFLECTED\_OFF} \§path expression>
\.{WITH\_DIRECTION} \§point expression>.

\LOG
\initials{LDF 2004.12.10.}
Added this rule.  It is not yet functional, because I haven't written 
|Path::reflect_off()| yet.

\initials{LDF 2004.12.10.}
Added ``\.{WITH\_DIRECTION} \§point expression>''.

\initials{LDF 2004.12.10.}
Working on |Path::reflect_off()|.

\initials{LDF 2004.12.10.}
Changed \§path secondary> on the left-hand side to 
\§path vector secondary>.  Moved this rule from 
\filename{ppthexpr.w} to this file (\filename{pphvexpr.w}).

\initials{LDF 2005.10.24.}
Changed |path_like_expression| to |path_like_expression|.
Removed debugging code.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=path_vector_secondary: path_secondary REFLECTED_OFF @>
@=path_expression WITH_DIRECTION point_expression@>@/ 
{

    Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

    typedef Pointer_Vector<Path> PV;

    PV* pv = new PV;



    *pv += create_new<Path>(0, scanner_node);
    *pv += create_new<Path>(0, scanner_node);
    *pv += create_new<Path>(0, scanner_node);
    *pv += create_new<Path>(0, scanner_node);


@q ******* (7) Call |Scan_Parse::reflect_off_func<Path>()|.@> 

@ Call |Scan_Parse::reflect_off_func<Path>()|.
\initials{LDF 2004.12.10.}

@<Define rules@>=

    int status = reflect_off_func<Path>(scanner_node,
                                        static_cast<Path*>(@=$1@>), 
                                        static_cast<Path*>(@=$3@>), 
                                        static_cast<Point*>(@=$5@>), 
                                        pv);          

@q ******** (8) Error handling:  |status != 0|.         @> 
@q ******** (8) |Scan_Parse::reflect_off_func()| failed.@> 

@ Error handling:  |status != 0|.  
|Scan_Parse::reflect_off_func()| failed.
\initials{LDF 2004.12.10.}

@<Define rules@>=                        
  
  if (status != 0)
     {
       @=$$@> = static_cast<void*>(0);

     } /* |if (status != 0)|  */
      
@q ******** (8) |Scan_Parse::reflect_off_func()| succeeded.@> 

@ |Scan_Parse::reflect_off_func()| succeeded.
\initials{LDF 2004.10.05.}


\LOG
\initials{LDF 2004.12.13.}
@:BUG FIX@> BUG FIX:  
Now setting |@=$$@>| to |static_cast<void*>(pv)| rather than
|static_cast<void*>(pv->v[0])|.
\ENDLOG 

@<Define rules@>=                        


  else /* |status == 0|  */
     {

          @=$$@> = static_cast<void*>(pv);

     }  /* |else| (|status == 0|)  */

@q ******* (7).@> 

};


@q ** (2) path_vector_secondary --> path_secondary @> 
@q ** (2) DECOMPOSE point_primary                  @>  

@*1 \§path vector secondary> $\longrightarrow$ 
\§path secondary> \.{DECOMPOSE} 
\§point primary>.
\initials{LDF 2007.06.19.}

\LOG
\initials{LDF 2007.06.19.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=path_vector_secondary: path_secondary DECOMPOSE@>@/
@=point_primary@>@/ 
{

       @<Decompose |Path| using a |Point|@>@;

};

@q ** (2) path_vector_secondary --> path_secondary @> 
@q ** (2) OVER point_primary                       @>  

@*1 \§path vector secondary> $\longrightarrow$ 
\§path secondary> \.{OVER} 
\§point primary>.
\initials{LDF 2007.06.19.}

\LOG
\initials{LDF 2007.06.19.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=path_vector_secondary: path_secondary OVER@>@/
@=point_primary@>@/ 
{

       @<Decompose |Path| using a |Point|@>@;

};


@q ** (2) Decompose |Path| using a |Point|.@> 
@ Decompose {\bf Path} using a {\bf Point}.
\initials{LDF 2007.06.19.}

This section is used in the rules
\§path vector secondary> $\longrightarrow$ 
\§path secondary> \.{DECOMPOSE} 
\§point primary>
and 
\§path vector secondary> $\longrightarrow$ 
\§path secondary> \.{OVER} 
\§point primary>

\LOG
\initials{LDF 2007.06.19.}
Added this section.
\ENDLOG

@<Decompose |Path| using a |Point|@>=

    Pointer_Vector<Path>* pv = new Pointer_Vector<Path>;

    int status = decompose_func(static_cast<Scanner_Node>(parameter),
                                @=$1@>,
                                PATH,
                                @=$3@>,
                                POINT,
                                0,
                                NULL_VALUE,
                                static_cast<void*>(pv),
                                PATH_VECTOR);

   if (status != 0)
   {
      
       @=$$@> = static_cast<void*>(0);

   }  /* |if (status != 0)|  */

 
   else /* |status != 0|  */
   {
 
      @=$$@> = static_cast<void*>(pv);

   }  /* |else| (|status != 0|)  */


@q * (1) path_vector tertiary.  @>

@* \§path vector tertiary>.
\initials{LDF 2004.12.10.}

\LOG
\initials{LDF 2004.12.10.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> path_vector_tertiary@>

@q ***** (5) path_vector tertiary --> path_vector_secondary.@>
@ \§path vector tertiary> $\longrightarrow$ 
\§path vector secondary>.
\initials{LDF 2004.12.10.}

\LOG
\initials{LDF 2004.12.10.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=path_vector_tertiary: path_vector_secondary@>@/ 
{

   @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
       cerr_strm << thread_name 
                 << "*** Parser: `path_vector_tertiary "
                 << "--> path_vector_secondary'.";

      log_message(cerr_strm);
      cerr_message(cerr_strm);
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */@;

  @=$$@> = @=$1@>;

};

@q * (1) path_vector expression.@>
@* \§path vector expression>.

\LOG
\initials{LDF 2004.12.10.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> path_vector_expression@>

@q ** (2) path_vector expression --> path_vector_tertiary.  @>
@*1 \§path vector expression> $\longrightarrow$ 
\§path vector tertiary>.

\LOG
\initials{LDF 2004.12.10.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=path_vector_expression: path_vector_tertiary@>@/ 
{
   @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
       cerr_strm << thread_name 
                 << "*** Parser: `path_vector_expression "
                 << "--> path_vector_tertiary'.";

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


