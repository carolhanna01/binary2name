@q ptgvexpr.w @> 
@q Created by Laurence Finston Thu Jan 20 12:41:06 CET 2005 @>
     
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



@q * (0) |triangle_vector| expressions.@>
@** \§triangle vector> expressions.
\initials{LDF 2005.01.20.}

\LOG
\initials{LDF 2005.01.20.}
Created this file and wrote quite a few rules.  
\ENDLOG 


@q * (1) |triangle_vector| primary.  @>
@* \§triangle vector primary>.
\initials{LDF 2005.01.20.}
  
\LOG
\initials{LDF 2005.01.20.}
Added this type declaration.
\ENDLOG


@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> triangle_vector_primary@>@/

@q ** (2) triangle_vector_primary --> triangle_vector_variable.@>
@*1 \§triangle vector primary> $\longrightarrow$ 
\§triangle vector variable>.  
\initials{LDF 2005.01.20.}

\LOG
\initials{LDF 2005.01.20.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=triangle_vector_primary: triangle_vector_variable@>@/ 
{
  @<Common declarations for rules@>@;

#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
      cerr_strm << thread_name 
                << "*** Parser: `triangle_vector_primary --> "
                << "triangle_vector_variable'.";

      log_message(cerr_strm);
      cerr_message(cerr_strm);
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */@;

  entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

@q **** (4) Error handling:  |entry == 0 || entry->object == 0|.@> 

@ Error handling:  |entry == 0 || entry->object == 0|.
\initials{LDF 2005.01.20.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0) || entry->object == static_cast<void*>(0))
    {

      cerr_strm << thread_name 
                << "ERROR! In `yyparse()', rule " 
                << endl 
                << "Rule `triangle_vector_primary --> "
                << "triangle_vector_variable':"
                << endl << "`entry' == 0 or `entry->object' == 0."
                << endl 
                << "Setting `triangle_vector_primary' "
                << "to 0 and will try to continue.";

      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");

      @=$$@> = static_cast<void*>(0);

    } /* |if (entry == 0 || entry->object == 0)|  */

@q **** (4) |!(entry == 0 || entry->object == 0)|.@> 

@ |!(entry == 0 || entry->object == 0)|.
\initials{LDF 2005.01.20.}

@<Define rules@>=
  else /* |!(entry == 0 || entry->object == 0)|  */

     {

        typedef Pointer_Vector<Triangle> PV;

        PV* pv = new PV;

        *pv = *static_cast<PV*>(entry->object);

        @=$$@> = static_cast<void*>(pv);                    

     }  /* |else| (|!(entry == 0 || entry->object == 0)|)  */

};


@q ** (2) triangle_vector_primary --> LEFT_PARENTHESIS  @>
@q ** (2) triangle_vector_expression  RIGHT_PARENTHESIS.@>

@*1 \§triangle vector primary> $\longrightarrow$ \.{LEFT\_PARENTHESIS}
\§triangle vector expression> \.{RIGHT\_PARENTHESIS}.
\initials{LDF 2005.01.20.}

\LOG
\initials{LDF 2005.01.20.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=triangle_vector_primary: LEFT_PARENTHESIS@>@/ 
@=triangle_vector_expression RIGHT_PARENTHESIS@>@/ 
{
   
   @<Common declarations for rules@>@;

#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
       cerr_strm << thread_name 
                << "*** Parser:  `triangle_vector_primary "
                << "--> LEFT_PARENTHESIS triangle_vector_expression "
                << "RIGHT_PARENTHESIS'.";

       log_message(cerr_strm);
       cerr_message(cerr_strm);
       cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */@;

  @=$$@> = @=$1@>;

};

@q * (1) triangle_vector secondary.@>

@* \§triangle vector secondary>.
\initials{LDF 2005.01.20.}

\LOG
\initials{LDF 2005.01.20.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> triangle_vector_secondary@>
  
@q ** (2) triangle_vector secondary --> triangle_vector_primary.@>
@*1 \§triangle vector secondary> $\longrightarrow$ 
\§triangle vector primary>.
\initials{LDF 2005.01.20.}

\LOG
\initials{LDF 2005.01.20.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=triangle_vector_secondary: triangle_vector_primary@>@/ 
{

  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
       cerr_strm << thread_name 
                 << "*** Parser:  `triangle_vector_secondary "
                 << "--> triangle_vector_primary'.";

       log_message(cerr_strm);
       cerr_message(cerr_strm);
       cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */@;
  
  @=$$@> = @=$1@>;

};

@q *** (3) triangle_vector_secondary --> triangle_secondary REFLECTED_OFF @>
@q *** (3) path_expression WITH_DIRECTION point_expression.               @>

@*2 \§triangle vector secondary> $\longrightarrow$ \§triangle secondary> 
\.{REFLECTED\_OFF} \§path expression>
\.{WITH\_DIRECTION} \§point expression>.

\LOG
\initials{LDF 2005.01.20.}
Added this rule.

\initials{LDF 2005.10.24.}
Changed |path_like_expression| to |path_expression|.
Removed debugging code.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=triangle_vector_secondary: triangle_secondary REFLECTED_OFF @>
@=path_expression WITH_DIRECTION point_expression@>@/ 
{

    Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

    typedef Pointer_Vector<Triangle> PV;

    PV* pv = new PV;


    *pv += create_new<Triangle>(0, scanner_node);
    *pv += create_new<Triangle>(0, scanner_node);
    *pv += create_new<Triangle>(0, scanner_node);
    *pv += create_new<Triangle>(0, scanner_node);


@q ******* (7) Call |Scan_Parse::reflect_off_func<Triangle>()|.@> 

@ Call |Scan_Parse::reflect_off_func<Triangle>()|.
\initials{LDF 2005.01.20.}

@<Define rules@>=

    int status = reflect_off_func<Triangle>(scanner_node,
                                            static_cast<Triangle*>(@=$1@>), 
                                            static_cast<Triangle*>(@=$3@>), 
                                            static_cast<Point*>(@=$5@>), 
                                            pv);          

@q ******** (8) Error handling:  |status != 0|.         @> 
@q ******** (8) |Scan_Parse::reflect_off_func()| failed.@> 

@ Error handling:  |status != 0|.  
|Scan_Parse::reflect_off_func()| failed.
\initials{LDF 2005.01.20.}

@<Define rules@>=                        
  
  if (status != 0)
     {
         
         @=$$@> = static_cast<void*>(0);

     } /* |if (status != 0)|  */
      
@q ******** (8) |Scan_Parse::reflect_off_func()| succeeded.@> 

@ |Scan_Parse::reflect_off_func()| succeeded.
\initials{LDF 2005.01.20.}

@<Define rules@>=                        


  else /* |status == 0|  */
     {
       
          @=$$@> = static_cast<void*>(pv);

     }  /* |else| (|status == 0|)  */

@q ******* (7).@> 

};

@q * (1) triangle_vector tertiary.  @>

@* \§triangle vector tertiary>.
\initials{LDF 2005.01.20.}

\LOG
\initials{LDF 2005.01.20.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> triangle_vector_tertiary@>

@q ***** (5) triangle_vector tertiary --> triangle_vector_secondary.@>
@ \§triangle vector tertiary> $\longrightarrow$ 
\§triangle vector secondary>.
\initials{LDF 2005.01.20.}

\LOG
\initials{LDF 2005.01.20.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=triangle_vector_tertiary: triangle_vector_secondary@>@/ 
{

   @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
       cerr_strm << thread_name 
                 << "*** Parser: `triangle_vector_tertiary "
                 << "--> triangle_vector_secondary'.";

      log_message(cerr_strm);
      cerr_message(cerr_strm);
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */@;

  @=$$@> = @=$1@>;

};


@q * (1) triangle_vector expression.@>
@* \§triangle vector expression>.

\LOG
\initials{LDF 2005.01.20.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> triangle_vector_expression@>

@q ** (2) triangle_vector expression --> triangle_vector_tertiary.  @>
@*1 \§triangle vector expression> $\longrightarrow$ 
\§triangle vector tertiary>.

\LOG
\initials{LDF 2005.01.20.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=triangle_vector_expression: triangle_vector_tertiary@>@/ 
{
   @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
       cerr_strm << thread_name 
                 << "*** Parser: `triangle_vector_expression "
                 << "--> triangle_vector_tertiary'.";

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


