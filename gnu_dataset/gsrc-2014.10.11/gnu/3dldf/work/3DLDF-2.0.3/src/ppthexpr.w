@q ppthexpr.w @> 
@q Created by Laurence Finston Thu May  6 15:21:19 MEST 2004  @>
       
@q * (1) Copyright and License.@>

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



@q * (0) path expressions.@>
@** path expressions.

\LOG
\initials{LDF 2004.05.13.}  
Added ``real'' code to this file.  It formerly only
contained dummy code.  
\ENDLOG 

@q ** (2) path primary.  @>
@*1 \§path primary>.
  
@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> path_primary@>@/

@q *** (3) path_primary --> path_variable.@>
@*2 \§path primary> $\longrightarrow$ \§path variable>.  

\LOG
\initials{LDF 2004.10.27.}
@:BUG FIX@> BUG FIX:  Now allocating memory for a new |Path|, 
if the |object| of the |Id_Map_Entry_Node| referenced 
by |path_variable| is 0.

\initials{LDF 2004.10.30.}
@:BUG FIX@> BUG FIX:  Now setting the value of 
|path_primary| to 0 if the |object| of the |Id_Map_Entry_Node| 
referenced by |path_variable| is 0.  If it's non-zero, now allocating 
memory for a new |Path| and assigning to it from the |Path| stored 
in the |Id_Map_Entry_Node|. 
\ENDLOG 

@q **** (4) Definition.@>   

@<Define rules@>=
@=path_primary: path_variable@>@/
{

  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @;
  if (DEBUG)
    {
      cerr_strm << thread_name 
                << "*** Parser: `path_primary --> path_variable'.";


      log_message(cerr_strm);
      cerr_message(cerr_strm);
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */@;


  entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

@q ***** (5) Error handling:  |entry == 0|.@> 

@ Error handling:  |entry == 0|.
\initials{LDF 2004.10.27.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

       cerr_strm << "ERROR! In yyparse(), rule " 
                 << "`path_primary --> path_variable':"
                 << endl 
                 << "`entry' == 0. "
                 << "Setting `path_primary' to 0 "
                 << "and will try to continue.";

       log_message(cerr_strm);
       cerr_message(cerr_strm, error_stop_value);
       cerr_strm.str("");

       @=$$@> = static_cast<void*>(0);
 
    } /* |if (entry == 0)|  */


@q ***** (5) |entry != 0|.@> 

@ |entry != 0|.
\initials{LDF 2004.10.27.}

@<Define rules@>=


  else /* |entry != 0|  */
    {

        Path* p = static_cast<Path*>(entry->object); 
   
@q ****** (6) |p == 0|.  Set |$$| to 0.@> 

@ |p == 0|.  Set |@=$$@>| to 0.
\initials{LDF 2004.10.30.}

\LOG
\initials{LDF 2004.11.22.}
No longer issuing a warning if |p == 0|.
This condition occurs legitimately when one tries to show
an ``unknown |path|''.
\ENDLOG 

@<Define rules@>=

       if (p == static_cast<Path*>(0))
          {

             @=$$@> = static_cast<void*>(0);

          } /* |if (p == 0)|  */



@q ****** (6) Try to allocate memory for a new |Path|.@>        

@ Try to allocate memory for a new |Path|.        
\initials{LDF 2004.10.30.}

\LOG
@:BUG FIX@> BUG FIX:  
Put the following code inside an |else| clause.
\ENDLOG 

@<Define rules@>=

   else /* |p != 0|  */
     {
        Path* q;
         
              try 
                {
                   q = create_new<Path>(p);
                }             
              
              catch (bad_alloc)
                {
  
                    cerr_strm << "ERROR! In yyparse(), rule " 
                              << "`path_primary --> path_variable':"
                              << endl 
                              << "`create_new<Path>()' failed. "
                              << "Rethrowing `bad_alloc'.";

                    log_message(cerr_strm);
                    cerr_message(cerr_strm, error_stop_value);
                    cerr_strm.str("");

                    throw;
                
                
                }  /* |catch (bad_alloc)|  */


        @=$$@> = static_cast<void*>(q); 


@q ******* (7).@> 

      }  /* |else| (|p != 0|)  */


@q ****** (6).@> 

   }  /* |else| (|entry != 0|)  */

@q ***** (5).@> 

};

@q *** (3) path_primary --> ( path_expression )  @>
@*2 \§path primary> $\longrightarrow$ \.{\LP} 
\§path expression> \.{\RP}.
\initials{LDF Undated.}

@<Define rules@>=
@=path_primary: LEFT_PARENTHESIS path_expression RIGHT_PARENTHESIS@>@/
{

  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
        cerr_strm << thread_name 
                << "*** Parser:  `path_primary --> LEFT_PARENTHESIS "
                << "path_expression RIGHT_PARENTHESIS'.";

        log_message(cerr_strm);
        cerr_message(cerr_strm);
        cerr_strm.str("");

    }
#endif /* |DEBUG_COMPILE|  */@;

  @=$$@> = @=$2@>;

};

@q *** (3) path_primary --> REVERSE path_primary  @>
@*2 \§path primary> $\longrightarrow$ \.{REVERSE} \§path primary>.

\LOG
\initials{LDF 2004.05.13.}  
Added this rule.

\initials{LDF 2004.11.26.}
Changed |Path::reverse()|, so that cyclical |Paths| can be 
reversed. 

\initials{LDF 2005.11.09.}
Removed debugging code.
\ENDLOG 

@<Define rules@>=
@=path_primary: REVERSE path_primary@>@/
{
  Path* p = static_cast<Path*>(@=$2@>); 

  p->reverse(1, static_cast<Scanner_Node>(parameter));

  @=$$@> = static_cast<void*>(p);  

};


@q *** (3) path_primary --> SUBPATH numeric_list OF path_primary  @>
@*2 \§path primary> $\longrightarrow$ \.{SUBPATH} \§numeric list>
\.{OF} \§path primary>.
\initials{LDF 2004.05.13.}  

\LOG
\initials{LDF 2004.05.13.}  
Added this rule.

\initials{LDF 2004.05.17.}  
Added code to the error handling portions of this
rule. 

\initials{LDF 2004.09.24.}
Changed |w| from |Real_Pointer_Vector*| to |Pointer_Vector<real>*|.
\ENDLOG 

\TODO
@q { @>
@:TO DO}{{\bf TO DO}@>
@q } @> 
@q !! TO DO:  @>
\initials{LDF 2004.05.13.} 
Make sure this works the way Metafont does, i.e., the numbering 
of the points on the path is the same. 
\ENDTODO 

@q **** (4) Definition.  @>

@<Define rules@>=
@=path_primary: SUBPATH numeric_list OF path_primary@>@/
{

  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
      cerr << "\n*** Parser: path_primary --> SUBPATH numeric_list "
           << "OF path_primary'." << endl;
    }
#endif /* |DEBUG_COMPILE|  */@;

  Path* p = create_new<Path>(0);
  Path* q = static_cast<Path*>(@=$4@>); 
  
  Pointer_Vector<real>* w = static_cast<Pointer_Vector<real>*>(@=$2@>); 
  
@q **** (4) Error handling for the case that |(w->v.size() < 2)|.  @>
@ Error handling for the case that |(w->v.size() < 2)|.

@<Define rules@>=
  if (w->v.size() < 2)
    {

      cerr << "ERROR! In yyparse():\n"
           << "Rule `path_primary --> SUBPATH numeric_list OF path_primary', "
           << "(path_primary 5):"
           << endl 
           << "Invalid indices for specifying `subpath':  `w->v.size() < 2'."
           << "Setting the `path_primary' on the left-hand side to the "
           << "`path_primary' on the right-hand side "
           << endl << "and proceeding with fingers crossed."
           << endl; 


      if (scanner_node->run_state.error_stop_mode == Run_State::STOPPING)
        {
          cerr << "Type <RETURN> to continue.\n";  

          getchar();  /* Don't delete this!  */@;  
        }  

    } /* |if (w->v.size() < 2)|  */@;

@q **** (4) Error handling for the case that |(w->v.size() > 2)|.  @>
@ Error handling for the case that |(w->v.size() > 2)|.

@<Define rules@>=
  else if (w->v.size() > 2)
    {
      cerr_strm << thread_name << "WARNING! In yyparse():\n"
                << "Rule `path_primary --> SUBPATH numeric_list OF "
                << "path_primary':"
                << endl 
                << "Too many indices for specifying `subpath': "
                << "`w->v.size() > 2'."
                << endl
                << "Using the first two, `" << w->v[0] 
                << "' and `" 
                << w->v[1] << "', and `continuing.";

      log_message(cerr_strm);
      cerr_message(cerr_strm, warning_stop_value);
      cerr_strm.str("");

    }  /* |else if (w->v.size() > 2)|  */@;

@q **** (4) Success!  @>
@ Success!

@<Define rules@>=
  else 
    {
      *p = q->subpath(static_cast<size_t>(*(w->v[0])), 
                         static_cast<size_t>(*(w->v[1])));

#if DEBUG_COMPILE
   if (DEBUG)
     {
       p->show("p:");
       q->show("q:");
     }
#endif /* |DEBUG_COMPILE|  */@; 

    }

@q **** (4) Finish and exit rule.  @>
@ Finish and exit rule.
@<Define rules@>=
  
  @=$$@> = static_cast<void*>(p);  

  delete q;

  for (vector<real*>::iterator iter = w->v.begin();
       iter != w->v.end();
       ++iter)
    {
      delete *iter;
    }
  
  w->v.clear();
  delete w;

};


@q *** (3) path_primary --> SEGMENT ellipse_primary                  @>
@q *** (3) WITH_START numeric_expression WITH_END numeric_expression @>
@q *** (3) with_closed_optional with_angle_optional.                 @>   

@*1 \§path primary> $\longrightarrow$ \.{SEGMENT}
\§ellipse primary> 
\.{WITH\_START} \§numeric expression> 
\.{WITH\_END} \§numeric expression>
\§with closed optional> \§with angle optional>.
\initials{LDF 2004.11.26.}

\LOG
\initials{LDF 2004.11.26.}
Added this rule.

\initials{LDF 2005.10.24.}
Changed |ellipse_like_primary| to |ellipse_primary|.
Removed debugging code.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=

@=path_primary: SEGMENT ellipse_primary@>@/
@=WITH_START numeric_expression WITH_END numeric_expression@>@/
@=with_closed_optional with_angle_optional@>@/
{

   Ellipse* e = static_cast<Ellipse*>(@=$2@>);

   Path* p = create_new<Path>(0);

   bool b = (@=$7@>) ? true : false;

   real angle = @=$8@>;

   unsigned int start 
      = static_cast<unsigned int>(floor(fabs(@=$4@>) + .5)); 

   unsigned int end 
      = static_cast<unsigned int>(floor(fabs(@=$6@>) + .5)); 

   *p = e->segment(start, end,  angle, b, static_cast<Scanner_Node>(parameter));

   delete e;

   @=$$@> = static_cast<void*>(p); 

};


@q *** (3) path_primary --> HALF ellipse_primary    @>
@q *** (3) with_closed_optional with_angle_optional @>
@q *** (3) with_start_optional.                     @>

@*1 \§path primary> $\longrightarrow$ \.{HALF}
\§ellipse primary> \§with closed optional> 
\§with angle optional> \§with start optional>.
\initials{LDF 2004.11.25.}

\LOG
\initials{LDF 2004.11.25.}
Added this rule.

\initials{LDF 2004.11.26.}
Changed \§circle primary> to \§ellipse-like primary> and 
added \§with start optional>.

\initials{LDF 2005.10.24.}
Changed |ellipse_like_primary| to |ellipse_primary|.
Removed debugging code.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=

@=path_primary: HALF ellipse_primary with_closed_optional@>@/ 
@=with_angle_optional with_start_optional@>@/
{

   Ellipse* e = static_cast<Ellipse*>(@=$2@>);

   Path* p = create_new<Path>(0);

   bool b = (@=$3@>) ? true : false;

   real angle = @=$4@>;

   unsigned int start 
      = static_cast<unsigned int>(floor(fabs(@=$5@>) + .5)); 

   *p = e->half(start, angle, b, static_cast<Scanner_Node>(parameter));

   delete e;

   @=$$@> = static_cast<void*>(p); 

};

@q *** (3) path_primary --> QUARTER ellipse_primary @>
@q *** (3) with_closed_optional with_angle_optional @>
@q *** (3) with_start_optional.                     @>

@*1 \§path primary> $\longrightarrow$ \.{QUARTER}
\§ellipse primary> \§with closed optional> 
\§with angle optional> \§with start optional>.
\initials{LDF 2004.11.26.}

\LOG
\initials{LDF 2004.11.26.}
Added this rule.

\initials{LDF 2005.10.24.}
Changed |ellipse_like_primary| to |ellipse_primary|.
Removed debugging code.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=

@=path_primary: QUARTER ellipse_primary with_closed_optional@>@/ 
@=with_angle_optional with_start_optional@>@/
{

   Ellipse* e = static_cast<Ellipse*>(@=$2@>);

   Path* p = create_new<Path>(0);
      
   bool b = (@=$3@>) ? true : false;

   real angle = @=$4@>;

   unsigned int start 
      = static_cast<unsigned int>(floor(fabs(@=$5@>) + .5)); 

   *p = e->quarter(start, angle, b, static_cast<Scanner_Node>(parameter));

   delete e;

   @=$$@> = static_cast<void*>(p); 

};

@q *** (3) path_primary --> HALF circle_primary    @>
@q *** (3) with_closed_optional with_angle_optional @>
@q *** (3) with_start_optional.                     @>

@*1 \§path primary> $\longrightarrow$ \.{HALF}
\§circle primary> \§with closed optional> 
\§with angle optional> \§with start optional>.
\initials{LDF 2009.09.15.}

\LOG
\initials{LDF 2009.09.15.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=

@=path_primary: HALF circle_primary with_closed_optional@>@/ 
@=with_angle_optional with_start_optional@>@/
{

   Circle* c = static_cast<Circle*>(@=$2@>);

   Path* p = create_new<Path>(0);

   bool b = (@=$3@>) ? true : false;

   real angle = @=$4@>;

   unsigned int start 
      = static_cast<unsigned int>(floor(fabs(@=$5@>) + .5)); 

   *p = c->half(start, angle, b, static_cast<Scanner_Node>(parameter));

   delete c;

   @=$$@> = static_cast<void*>(p); 

};

@q *** (3) path_primary --> QUARTER circle_primary @>
@q *** (3) with_closed_optional with_angle_optional @>
@q *** (3) with_start_optional.                     @>

@*1 \§path primary> $\longrightarrow$ \.{QUARTER}
\§circle primary> \§with closed optional> 
\§with angle optional> \§with start optional>.
\initials{LDF 2009.09.15.}

\LOG
\initials{LDF 2009.09.15.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=

@=path_primary: QUARTER circle_primary with_closed_optional@>@/ 
@=with_angle_optional with_start_optional@>@/
{

   Circle* c = static_cast<Circle*>(@=$2@>);

   Path* p = create_new<Path>(0);
      
   bool b = (@=$3@>) ? true : false;

   real angle = @=$4@>;

   unsigned int start 
      = static_cast<unsigned int>(floor(fabs(@=$5@>) + .5)); 

   *p = c->quarter(start, angle, b, static_cast<Scanner_Node>(parameter));

   delete c;

   @=$$@> = static_cast<void*>(p); 

};

@q *** (3) with_start_optional.@> 
@*2 \§with start optional>.
\initials{LDF 2004.11.25.}

\LOG
\initials{LDF 2004.11.25.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <real_value> with_start_optional@>@/

@q **** (4) with_start_optional --> EMPTY.@> 
@*3 \§with start optional> $\longrightarrow$ \.{EMPTY}.
\initials{LDF 2004.11.25.}

\LOG
\initials{LDF 2004.11.25.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=with_start_optional: /* Empty  */@>@/
{

   @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
    if (DEBUG) 
      {
        cerr_strm << thread_name << "*** Parser: "
                  << "`with_start_optional: EMPTY'.";
        
        log_message(cerr_strm);
        cerr_message(cerr_strm);
        cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */


   @=$$@> = ZERO_REAL;

};

@q **** (4) with_start_optional --> WITH_START numeric_expression.@> 
@*3 \§with start optional> $\longrightarrow$ 
\.{WITH\_START} \§numeric expression>.
\initials{LDF 2004.11.25.}

\LOG
\initials{LDF 2004.11.25.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=with_start_optional: WITH_START numeric_expression@>@/
{
   @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
    if (DEBUG) 
      {
        cerr_strm << thread_name << "*** Parser: "
                  << "`with_start_optional: WITH_START numeric_expression'.";
        
        log_message(cerr_strm);
        cerr_message(cerr_strm);
        cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */

   @=$$@> = @=$2@>;

};

@q *** (3) with_closed_optional.@> 
@*2 \§with closed optional>.
\initials{LDF 2004.11.25.}

\LOG
\initials{LDF 2004.11.25.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <int_value> with_closed_optional@>@/

@q **** (4) with_closed_optional --> EMPTY.@> 
@*3 \§with closed optional> $\longrightarrow$ \.{EMPTY}.
\initials{LDF 2004.11.25.}

\LOG
\initials{LDF 2004.11.25.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=with_closed_optional: /* Empty  */@>@/
{

   @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
    if (DEBUG) 
      {
        cerr_strm << thread_name << "*** Parser: "
                  << "`with_closed_optional: EMPTY'.";
        
        log_message(cerr_strm);
        cerr_message(cerr_strm);
        cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */


   @=$$@> = 0;

};

@q **** (4) with_closed_optional --> WITH_CLOSED.@> 
@*3 \§with closed optional> $\longrightarrow$ 
\.{WITH\_CLOSED}.
\initials{LDF 2004.11.25.}

\LOG
\initials{LDF 2004.11.25.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=with_closed_optional: WITH_CLOSED@>@/
{
   @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
    if (DEBUG) 
      {
        cerr_strm << thread_name << "*** Parser: "
                  << "`with_closed_optional: WITH_CLOSED'.";
        
        log_message(cerr_strm);
        cerr_message(cerr_strm);
        cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */

   @=$$@> = WITH_CLOSED;

};

@q *** (3) path_primary --> GET_PATH numeric_expression @>
@q *** (3) cuboid_primary.                              @>   
@*1 \§path primary> $\longrightarrow$ \.{GET\_PATH}
\§numeric expression> \§cuboid primary>. 
\initials{LDF 2004.10.13.}

\LOG
\initials{LDF 2004.10.13.}
Added this rule.

\initials{LDF 2004.11.14.}
Changed \.{INTEGER} to \§numeric expression>.

\initials{LDF 2005.10.22.}
Replaced |solid_like_primary| with |cuboid_primary|.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=

@=path_primary: GET_PATH numeric_expression cuboid_primary@>@/
{

  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
    if (DEBUG) 
      {
        cerr_strm << thread_name << "*** Parser: "
                  << "`path_primary --> "
                  << "GET_PATH numeric_expression cuboid_primary'.";
        
        log_message(cerr_strm);
        cerr_message(cerr_strm);
        cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */

  Cuboid* s = static_cast<Cuboid*>(@=$3@>);

  Path* p;

  unsigned short u;

  u = static_cast<unsigned short>(floor(fabs(@=$2@>) + .5)); 

@q **** (4) Warning:  |numeric_expression != u|.@> 

@ Warning:  |numeric_expression != u|.
\initials{LDF 2004.11.14.}

@<Define rules@>=

  if (@=$2@> != u)
     {
         cerr_strm << thread_name << "WARNING! In `yyparse()', rule "
                   << "`path_primary --> GET_PATH "
                   << "numeric_expression cuboid_primary':"
                   << endl 
                   << "`numeric_expression' has invalid value:  "
                   << @=$2@> << "." << endl << "using " << u 
                   << " instead.";

         log_message(cerr_strm);
         cerr_message(cerr_strm, warning_stop_value);
         cerr_strm.str("");
                
     }  /* |if (@=$2@> != u)|  */

@q **** (4).@> 

  const Path* t = s->get_path_ptr(u); 


@q **** (4) Error handling:  |Cuboid::get_path_ptr()| returned 0.@>   

@ Error handling:  |Cuboid::get_path_ptr()| returned 0.
\initials{LDF 2004.10.13.}

@<Define rules@>=

  if (t == static_cast<const Path*>(0))
   {
      cerr_strm << thread_name << "ERROR! In `yyparse()', rule "
                << "`path_primary --> GET_PATH "
                << "numeric_expression cuboid_primary':"
                << endl 
                << "`Cuboid::get_path_ptr()' failed, "
                << "returning 0."
                << endl << "Deleting `cuboid_primary' and "
                << "setting `path_primary' to 0.";

      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");

      delete s;

      @=$$@> = static_cast<void*>(0);

   } /* |if (t == 0)|  */


@q **** (4) Try to allocate memory on free store for |path_primary|.@>   

@ Try to allocate memory on free store for \§path primary>.
\initials{LDF 2004.10.13.}

@<Define rules@>=

  else /* |t != 0|  */
     {
        try
          {
            p = create_new<Path>(t, scanner_node);
          }


@q ***** (5) Error handling:  |create_new<Path>()| failed.@> 

@ Error handling:  |create_new<Path>()| failed.
\initials{LDF 2004.10.13.}

@<Define rules@>=

        catch (bad_alloc)
           {             
      
               cerr_strm << thread_name << "ERROR! In `yyparse()', rule "
                         << "`path_primary --> GET_PATH "
                         << "numeric_expression cuboid_primary':"
                         << endl 
                         << "Failed to allocate memory for "
                         << "`path_primary'."
                         << endl << "Deleting `cuboid_primary' and "
                         << "rethrowing `bad_alloc'.";

               log_message(cerr_strm);
               cerr_message(cerr_strm, error_stop_value);
               cerr_strm.str("");
        
               delete s;

               throw;

            } /*        |catch (bad_alloc)|  */



@q **** (4) Delete |s|, set |path_primary| to              @>   
@q **** (4) |static_cast<void*>(p)|, and exit rule successfully.@>   

@ Delete |s|, set |path_primary| to                 
|static_cast<void*>(p)|, and exit rule successfully.   
\initials{LDF 2004.10.13.}

@<Define rules@>=

  delete s;

  @=$$@> = static_cast<void*>(p);

  }  /* |else| (|t != 0|)  */

};

@q *** (3) path_primary --> GET_BASE cone_primary.@>
@*1 \§path primary> $\longrightarrow$ \.{GET\_BASE}
\§cone primary>. 
\initials{LDF 2006.11.09.}

\LOG
\initials{LDF 2006.11.09.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=

@=path_primary: GET_BASE cone_primary@>@/
{
   @=$$@> = quad_surf_get_element_func(parameter, 
                                       GET_BASE, 
                                       Shape::CONE_TYPE, 
                                       @=$2@>);
};

@q *** (3) path_primary --> GET_CAP cone_primary.@>
@*1 \§path primary> $\longrightarrow$ \.{GET\_CAP}
\§cone primary>. 
\initials{LDF 2006.11.09.}

\LOG
\initials{LDF 2006.11.09.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=

@=path_primary: GET_CAP cone_primary@>@/
{
   @=$$@> = quad_surf_get_element_func(parameter, 
                                       GET_CAP, 
                                       Shape::CONE_TYPE, 
                                       @=$2@>);
};


@q *** (3) path_primary --> GET_PATH INTEGER cone_primary.@>
@*1 \§path primary> $\longrightarrow$ \.{INTEGER} \.{GET\_PATH}
\§cone primary>. 
\initials{LDF 2006.11.09.}

\LOG
\initials{LDF 2006.11.09.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=

@=path_primary: GET_PATH INTEGER cone_primary@>@/
{
   @=$$@> = quad_surf_get_element_func(parameter, 
                                       GET_PATH, 
                                       Shape::CONE_TYPE, 
                                       @=$3@>,
                                       @=$2@>);
};



@q ***** (5) path_primary --> LAST @>
@q ***** (5) path_vector_expression.@>

@*4 \§path primary> $\longrightarrow$ 
\.{LAST} \§path vector expression>.
\initials{LDF 2005.01.14.}

\LOG
\initials{LDF 2005.01.14.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=path_primary: LAST path_vector_expression@>@/
{ 
   @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
   DEBUG = false; /* |true| */ @; 
   if (DEBUG) 
     {
         cerr_strm << thread_name << "*** Parser:  `path_primary "
                   << "--> LAST path_vector_expression'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str("");
     }
#endif /* |DEBUG_COMPILE|  */

@q ******* (7) @> 

   Path* p;

   try 
      {
         p = create_new<Path>(0);
      }

   catch (bad_alloc)
      {
         cerr_strm << thread_name << "ERROR!  In `yyparse()', rule "
                   << "`path_primary "
                   << "--> LAST path_vector_expression':"
                   << endl << "`create_new<Path>()' failed.  "
                   << "Rethrowing `bad_alloc'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm, error_stop_value);
         cerr_strm.str("");

         throw;

      }  /* |catch (bad_alloc)|  */

@q ******* (7) @> 

   Pointer_Vector<Path>* pv 
      = static_cast<Pointer_Vector<Path>*>(@=$2@>);

@q ******* (7) Error handling:  |pv == 0|.@> 

@ Error handling:  |pv == 0|.
\initials{LDF 2005.01.14.}

@<Define rules@>=

   if (pv == static_cast<Pointer_Vector<Path>*>(0))
      {
          cerr_strm << thread_name 
                    << "ERROR!  In `yyparse()', rule "
                    << endl 
                    << "`path_primary "
                    << "--> LAST path_vector_expression':"
                    << endl << "Invalid `path_vector_expression'.  "
                    << "Setting `path_primary' to 0"
                    << endl << "and will try to continue.";

          log_message(cerr_strm);
          cerr_message(cerr_strm, error_stop_value);
          cerr_strm.str("");

          delete p;

          @=$$@> = static_cast<void*>(0);

      }  /* |if (pv == 0)|  */

@q ******* (7) Error handling:  |pv->ctr == 0|.@> 

@ Error handling:  |pv->ctr == 0|.
\initials{LDF 2005.01.14.}

@<Define rules@>=

   else if (pv->ctr == 0)
      {
          cerr_strm << thread_name 
                    << "ERROR!  In `yyparse()', rule "
                    << endl 
                    << "`path_primary "
                    << "--> LAST path_vector_expression':"
                    << endl << "`path_vector_expression' is empty.  "
                    << "Setting `path_primary' to 0"
                    << endl << "and will try to continue.";

          log_message(cerr_strm);
          cerr_message(cerr_strm, error_stop_value);
          cerr_strm.str("");

          delete p;

          @=$$@> = static_cast<void*>(0);

      }  /* |else if (pv->ctr == 0)|  */

@q ******* (7) |pv != 0 && pv->ctr > 0|.@> 

@ |pv != 0 && pv->ctr > 0|.  Set |@=$$@>| to |*(pv->v[pv->ctr - 1])|.
\initials{LDF 2005.01.14.}

@<Define rules@>=

   else 
      {
         *p = *(pv->v[pv->ctr - 1]);
         @=$$@> = static_cast<void*>(p); 
      }
@q ******* (7) @> 

};




@q ***** (5) path_primary --> GENERATE nurb_expression @>
@q ***** (5) WITH_POWER numeric_expression             @> 
@q ***** (5) WITH_TIME_PARAMETER numeric_expression.   @> 

@*4 \§path primary> $\longrightarrow$ 
\.{GENERATE} \§nurb expression> \.{WITH\_POWER} 
\§numeric expression> \.{WITH\_TIME\_PARAMETER} 
\§numeric expression>.
\initials{LDF 2005.01.27.}

\LOG
\initials{LDF 2005.01.27.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=

@=path_primary: GENERATE nurb_expression@>@/
@=WITH_POWER numeric_expression@>@/
@=WITH_TIME_PARAMETER numeric_expression@>@/ 
{
   @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
   DEBUG = false;  /* |true| */ @; 
   if (DEBUG)
      {
          cerr_strm << thread_name 
                    << "*** Parser:  `path_primary "
                    << "--> nurb_expression WITH_POWER numeric_expression "
                    << "WITH_TIME_PARAMETER numeric_expression'.";

          log_message(cerr_strm); 
          cerr_message(cerr_strm); 
          cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */@;

   Nurb* n = static_cast<Nurb*>(@=$2@>); 

@q ****** (6).@> 

   if (n == static_cast<Nurb*>(0))
      {
          cerr_strm << thread_name 
                    << "ERROR! In `yyparse()', rule "
                    << "`path_primary --> "
                    << endl 
                    << "nurb_expression WITH_POWER numeric_expression "
                    << "WITH_TIME_PARAMETER numeric_expression':"
                    << endl << "`nurb_expression' is 0.  Setting "
                    << "`path_primary' to 0 and will try to continue.";

          log_message(cerr_strm); 
          cerr_message(cerr_strm, error_stop_value); 
          cerr_strm.str("");         

          @=$$@> = static_cast<void*>(0);       

      }  /* |if (n == 0)|  */
   
@q ****** (6).@> 

   else  /* |n != 0|  */
      {

          Path* p;

          try 
             {
                 p = create_new<Path>(0);
             }

@q ******* (7) @> 

          catch (bad_alloc)
             {
                 cerr_strm << thread_name 
                           << "ERROR! In `yyparse()', rule "
                           << "`path_primary --> "
                           << endl 
                           << "nurb_expression WITH_POWER numeric_expression "
                           << "WITH_TIME_PARAMETER numeric_expression':"
                           << endl << "`create_new<Path>() failed.  "
                           << "Deleting `nurb_expression' and rethrowing "
                           << "`bad_alloc'.";

                 log_message(cerr_strm); 
                 cerr_message(cerr_strm, error_stop_value); 
                 cerr_strm.str("");         

                 delete n;

                 throw;
                
             }  /* |catch (bad_alloc)|  */

@q ******* (7) @> 
@
\LOG
\initials{LDF 2005.01.28.}
No longer passing |@=$2@>| to |generate()|.  
It simply duplicated |n|.
\ENDLOG 

@<Define rules@>=
          status = n->generate(scanner_node,
                               @=$4@>,
                               @=$6@>,
                               p);


@q ******* (7) @>           

         if (status != 0)
            {
                 cerr_strm << thread_name 
                           << "ERROR! In `yyparse()', rule "
                           << "`path_primary --> "
                           << endl 
                           << "nurb_expression WITH_POWER numeric_expression "
                           << "WITH_TIME_PARAMETER numeric_expression':"
                           << endl << "`Nurb::generate() failed, returning "
                           << status << ".  Setting `path_primary' to 0 "
                           << "and will try to continue.";

                 log_message(cerr_strm); 
                 cerr_message(cerr_strm, error_stop_value); 
                 cerr_strm.str("");         

                 @=$$@> = static_cast<void*>(0);
     
            }  /* |if (status != 0)|  */

@q ******* (7) @> 


        else  /* |status == 0|  */
           {

#if DEBUG_COMPILE
              if (DEBUG)
                 {
                     cerr_strm << thread_name 
                               << "In `yyparse()', rule "
                               << "`path_primary --> "
                               << endl 
                               << "nurb_expression WITH_POWER numeric_expression "
                               << "WITH_TIME_PARAMETER numeric_expression':"
                               << endl << "`Nurb::generate()' succeeded.";

                     log_message(cerr_strm); 
                     cerr_message(cerr_strm); 
                     cerr_strm.str("");         
                 }           
#endif /* |DEBUG_COMPILE|  */@; 
 
               @=$$@> = static_cast<void*>(p); 

           }  /* |else| (|status == 0|)  */
           

      }  /* |else| (|n != 0|)  */
 
@q ****** (6).@> 

};


@q *** (3) path_primary --> GET_BRANCH numeric_secondary hyperbola_primary.@>
@*1 \§path primary> $\longrightarrow$ \.{GET\_BRANCH} 
\§numeric secondary> \§hyperbola primary>.
\initials{LDF 2007.07.09.}

\LOG
\initials{LDF 2007.07.09.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=path_primary: GET_BRANCH numeric_secondary hyperbola_primary@>@/
{
    @=$$@> = Scan_Parse::get_hyperbola_branch_func(@=$2@>, @=$3@>, parameter);
};  

@q *** (3) path_primary --> GET_PASCAL_LINE numeric_expression @>
@q *** (3) COMMA conic_section_lattice_expression.             @>   
@q *** (3) with_conic_section_lattice_option_list.             @>   

@*1 \§path primary> $\longrightarrow$ \.{GET\_PASCAL\_LINE}
\§numeric expression> \.{COMMA} \§conic section lattice expression>
\§with conic section lattice option list>.
\initials{LDF 2007.08.19.}

\LOG
\initials{LDF 2007.08.19.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=

@=path_primary: GET_PASCAL_LINE numeric_expression COMMA@>@/
@=conic_section_lattice_expression with_conic_section_lattice_option_list@>@/
{
   if (@=$4@> == static_cast<void*>(0))
      @=$$@> = static_cast<void*>(0);
   else
   {
      Conic_Section_Lattice* c = static_cast<Conic_Section_Lattice*>(@=$4@>);
      @=$$@> = c->get_pascal_line(@=$2@>, static_cast<Scanner_Node>(parameter));
      delete c;
   }
};

@q ** (2) path secondary.  @>
@*1 \§path secondary>.

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> path_secondary@>
  
@q *** path secondary --> path_primary.@>
@ \§path secondary> $\longrightarrow$ \§path primary>.

@<Define rules@>=
@=path_secondary: path_primary@>@/
{
#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
      cerr << "\n*** Parser: `path_secondary --> path_primary'."
           << endl;
    }
#endif /* |DEBUG_COMPILE|  */@;

  @=$$@> = @=$1@>;

};

@q *** (3) path secondary --> path_secondary transformer.  @>
@*2 \§path secondary> $\longrightarrow$ \§path secondary> 
\§transformer>.

\LOG
\initials{LDF 2004.06.07.}  
Added this rule.

\initials{LDF 2004.10.05.}
Made debugging output thread-safe.

\initials{LDF 2006.01.20.}
@:BUG FIX@> BUG FIX:  Now deleting |Transform* t|.
\ENDLOG 

@<Define rules@>=
@=path_secondary: path_secondary transformer@>@/
{
  
  Path* p = static_cast<Path*>(@=$1@>);
  Transform* t = static_cast<Transform*>(@=$2@>);

  *p *= *t;

  delete t;

  @=$$@> = static_cast<void*>(p); 

};

@q *** (3) path secondary --> path_secondary REFLECTED_IN @>
@q *** (3) path_expression.                               @> 

@*2 \§path secondary> $\longrightarrow$ \§path secondary> 
\.{REFLECTED\_IN} \§path expression>.

\LOG
\initials{LDF 2004.10.05.}
Added this rule.

\initials{LDF 2004.12.03.}
Changed |REFLECTED IN| to |REFLECTED_IN|.

\initials{LDF 2005.10.24.}
Changed |path_like_expression| to |path_expression|.
Removed debugging code.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=path_secondary: path_secondary REFLECTED_IN @>
@=path_expression@>@/ 
{


    Path* p = reflect_in_func<Path>(static_cast<Scanner_Node>(parameter),
                                    static_cast<Path*>(@=$1@>),
                                    static_cast<Path*>(@=$3@>));

    @=$$@> = static_cast<void*>(p);

};

@q ***** (5) path_secondary --> path_secondary PROJECTED @>
@q ***** (5) focus_primary with_z_optional.@>  

@*4 \§path secondary> $\longrightarrow$ 
\§path secondary> \.{PROJECTED} \§focus primary>.
\initials{LDF 2005.04.15.}

\LOG
\initials{LDF 2005.04.15.}
Added this rule.

\initials{LDF 2005.04.18.}
Changed |PROJECT| to |PROJECTED|.

\initials{LDF 2005.04.19.}
Changed this rule from 
\§path primary> $\longrightarrow$ 
\§path primary> \.{PROJECTED} \.{WITH\_FOCUS} \§focus expression>.
to 
\§path secondary> $\longrightarrow$ 
\§path secondary> \.{PROJECTED} \§focus primary>.

\initials{LDF 2005.04.19.}
Added \§with z optional>.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=path_secondary: path_secondary PROJECTED focus_primary with_z_optional@>@/
{ 
   @=$$@> = path_secondary_rule_func_0(@=$1@>, 
                                       @=$3@>, 
                                       @=$4@>,
                                       static_cast<Scanner_Node>(parameter));

};

@q ** (2) path tertiary.  @>
@*1 \§path tertiary>.

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> path_tertiary@>

@q *** path tertiary --> path_secondary.@>
@ \§path tertiary> $\longrightarrow$ \§path secondary>.

@<Define rules@>=
@=path_tertiary: path_secondary@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3) path tertiary --> path_tertiary path_join path_expression.@>
@*2 \§path tertiary> $\longrightarrow$ \§path secondary> \§path join>
\§path expression>.
\initials{LDF 2005.11.09.}

\LOG
\initials{LDF 2005.11.09.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=path_tertiary: path_secondary path_join path_expression@>@/
{

    Path* p = static_cast<Path*>(@=$1@>);
    Path* q = static_cast<Path*>(@=$3@>);
    string s = @=$2@>;

    p->append(*q, s, true);

    delete q;
    q = 0;

    @=$$@> = static_cast<void*>(p); 
   

};

@q *** (3) path tertiary --> plane_tertiary INTERSECTION plane_secondary.@>
@*2 \§path tertiary> $\longrightarrow$ \§plane tertiary> \.{INTERSECTION}
\§plane secondary>.
\initials{LDF 2007.10.06.}

\LOG
\initials{LDF 2007.10.06.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=path_tertiary: plane_tertiary INTERSECTION plane_secondary@>@/
{

    @=$$@> = plane_intersection_func(@=$1@>, @=$3@>, parameter);
   
};



@q **** (4) path_join.@>
@*3 \§path join>.

@<Type declarations for non-terminal symbols@>=
@=%type <string_value> path_join@>

@q ***** (5) path_join --> basic_path_join.@>
@*4 \§path join> $\longrightarrow$ \§basic path join>.

@<Define rules@>=
@=path_join: basic_path_join@>@/
{

  strcpy(@=$$@>, @=$1@>);
  
};


@q *** (3) basic_path_join.@>
@*2 \§basic path join>.

@<Type declarations for non-terminal symbols@>=
@=%type <string_value> basic_path_join@>

@q **** (4) basic_path_join --> PERIOD_PAIR.@>
@*3 \§basic path join> $\longrightarrow$ \.{PERIOD\_PAIR}.

@<Define rules@>=
@=basic_path_join: PERIOD_PAIR@>@/
{

  strcpy(@=$$@>, "..");

};

@q **** (4) basic_path_join --> PERIOD_TRIPLE.@>
@*3 \§basic path join> $\longrightarrow$ \.{PERIOD\_TRIPLE}.

@<Define rules@>=
@=basic_path_join: PERIOD_TRIPLE@>@/
{

  strcpy(@=$$@>, "...");

};

@q **** (4) basic_path_join --> HYPHEN_PAIR.@>
@*3 \§basic path join> $\longrightarrow$ \.{HYPHEN\_PAIR}.

@<Define rules@>=
@=basic_path_join: HYPHEN_PAIR@>@/
{

  strcpy(@=$$@>, "--");

};

@q **** (4) basic_path_join --> HYPHEN_TRIPLE.@>
@*3 \§basic path join> $\longrightarrow$ \.{HYPHEN\_TRIPLE}.

@<Define rules@>=
@=basic_path_join: HYPHEN_TRIPLE@>@/
{

  strcpy(@=$$@>, "---");

};

@q **** (4) basic_path_join --> AMPERSAND.@>
@*3 \§basic path join> $\longrightarrow$ \.{AMPERSAND}.

@<Define rules@>=
@=basic_path_join: AMPERSAND@>@/
{

  strcpy(@=$$@>, "&");

};


@q ** (2) path expression.  @>
@*1 \§path expression>.

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> path_expression@>


@q *** (3) path expression --> path_tertiary.  @>
@*2 \§path expression> $\longrightarrow$ \§path tertiary>.

@<Define rules@>=
@=path_expression: path_tertiary@>@/
{

  @=$$@> = @=$1@>;
  
};

@q **** (4) path_expression --> path_element_list.@>   
@*3 \§path expression> $\longrightarrow$ \§path element list>.

\LOG
\initials{LDF 2004.05.13.}  
Added this rule.
\ENDLOG 

@<Define rules@>= 
 
@=path_expression: path_element_list@>
{

  @=$$@> = @=$1@>;

};

@q **** (4) path_expression --> path_expression path_join @>   
@q **** (4)  path_element_list.                           @>   

@*3 \§path expression> $\longrightarrow$ 
\§path expression> \§path join>  \§path element list>.

\LOG
\initials{LDF 2004.11.01.}  
Added this rule.
\ENDLOG 

@<Define rules@>= 
 
@=path_expression: path_expression path_join path_element_list@>
{

  Path* p = static_cast<Path*>(@=$1@>);
  Path* q = static_cast<Path*>(@=$3@>);

  p->append(*q, @=$2@>, true);  


  @=$$@> = static_cast<void*>(p); 

};

@q **** (4) path_expression --> path_expression path_join @>   
@q **** (4) CYCLE.                                        @>   

@*3 \§path expression> $\longrightarrow$ 
\§path expression> \§path join>  \.{CYCLE}.

\LOG
\initials{LDF 2004.11.01.}  
Added this rule.

\initials{LDF 2004.11.01.}
Changed the way |Path *p| is handled. 
Now calling |p->adjust_connectors()|.
\ENDLOG 

@<Define rules@>= 
 
@=path_expression: path_expression path_join CYCLE@>@/
{

  Path* p = static_cast<Path*>(@=$1@>);

  p->set_cycle(false);
  p->adjust_connectors();

  *p += @=$2@>;

  p->set_cycle(true);

  @=$$@> = static_cast<void*>(p); 

};



@q **** (4) path_element_list  @>   
@*3 \§path element list>.

\LOG
\initials{LDF 2004.05.13.}  
Added this type declaration.

\initials{LDF 2004.10.30.}
Changed the name of |point_connector_list| to 
|path_element_list|.  
Changed everywhere else without comment.
\ENDLOG 
 
@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> path_element_list@>

@q ***** (5) path_element_list --> point_expression.@>   
@*4 \§path element list> $\longrightarrow$ \§point expression>.

\LOG
\initials{LDF 2004.05.13.}  
Added this rule.

\initials{LDF 2005.10.27.}
@:BUG FIX@> BUG FIX:  Now checking whether |point_expression| is non-null
before using it.  
Removed debugging code.
\ENDLOG 

@<Define rules@>= 
@=path_element_list: point_expression@>
{

   Point* p = static_cast<Point*>(@=$1@>);  
   Path* q = create_new<Path>(0);

   if (p)
      {
          *q += *p; 
          delete p;
          p = 0;
      } 

    @=$$@> = static_cast<void*>(q); 

};


@q ***** (5) path_element_list --> path_element_list @>   
@q ***** (5) path_join point_expression.             @>   

@*4 \§path element list> $\longrightarrow$ 
\§path element list> \§path join> \§point expression>.

\LOG
\initials{LDF 2004.05.13.}  
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=path_element_list: path_element_list path_join point_expression@>
{

  Point* p = static_cast<Point*>(@=$3@>); 
  Path* q = static_cast<Path*>(@=$1@>);

  *q += @=$2@>;

  *q += *p;

  @=$$@> = static_cast<void*>(q);

  delete p;

};

@q ***** (5) path_element_list --> path_element_list @>   
@q ***** (5) path_join path_tertiary.                @>   

@*4 \§path element list> $\longrightarrow$ 
\§path element list> \§path join> \§path tertiary>.

\LOG
\initials{LDF 2004.05.13.}  
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=path_element_list: path_element_list path_join path_tertiary@>
{

  Path* p = static_cast<Path*>(@=$1@>);
  Path* q = static_cast<Path*>(@=$3@>); 

  p->append(*q, @=$2@>, true);

  @=$$@> = static_cast<void*>(p);

  delete q;

};


@q **** (4) path_element_list --> path_element_list path_join CYCLE.  @>   
@ \§path element list> $\longrightarrow$ 
\§path element list> \§path join> \.{CYCLE}.

\LOG
\initials{LDF 2004.05.13.}  
Added this rule.
\ENDLOG 

@<Define rules@>= 
@=path_element_list: path_element_list path_join CYCLE@>
{

  Path* q = static_cast<Path*>(@=$1@>);

  *q += @=$2@>;

  q->set_cycle();

  @=$$@> = static_cast<void*>(q);

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

