@q popassgn.w @> 
@q Created by Laurence Finston Sa Dez  4 18:48:15 CET 2004 @>

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




@q * (0) Operation Assignments.  @>
@** Operation Assignments.  

Operation assignments cannot be used in 
assignment chains.  
\initials{LDF 2004.12.04.}

Multiplication by a |transform| with assignment is 
not an \§operation assignment>, but rather than a
\§transformation assignment command>,   
which can be chained.
Rules for \§transformation assignment commands> 
are defined in \filename{ptrfcmnd.w}.
\initials{LDF 2004.12.04.} 

@q * (1) operation_assignment.  @>
@* \§operation assignment>.
\initials{LDF 2004.12.04.}  

\LOG
\initials{LDF 2004.12.04.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> operation_assignment@>


@q ** (2) Non-vector types.@>  
@*1 Non-vector types.
\initials{LDF 2004.12.04.}

\LOG
\initials{LDF 2004.12.04.}
Added this section.
\ENDLOG

@q *** (3) |numerics|.@>  

@*2 {\bf numerics}.
\initials{LDF 2004.12.04.}

\LOG
\initials{LDF 2004.12.04.}
Added this section.
\ENDLOG


@q **** (4) operation_assignment --> numeric_variable @> 
@q **** (4) PLUS_ASSIGN numeric_expression.           @>

@*3 \§operation assignment> $\longrightarrow$ 
\§numeric variable>\.{PLUS\_ASSIGN} \§numeric expression>.
\initials{LDF 2004.12.04.}

\LOG
\initials{LDF 2004.12.04.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=

@=operation_assignment: numeric_variable PLUS_ASSIGN numeric_expression@>@/
{

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

@q ****** (6) Error handling:  |entry == 0|.@>   

@ Error handling:  |entry == 0|.
\initials{LDF 2004.12.04.}

@<Define rules@>=

   if (entry == static_cast<Id_Map_Entry_Node>(0))
      {
 
      } /* |if (entry == 0)|  */


@q ***** (5) |entry != 0|.@>   

@ |entry != 0|.
\initials{LDF 2004.12.04.}

@<Define rules@>=

    else /* |entry != 0|  */
      {

@q ****** (6).  Error handling:  |entry->object == 0|.@> 

@ Error handling:  |entry->object == 0|.
\initials{LDF 2004.12.04.}
  
@<Define rules@>=

         if (entry->object == static_cast<void*>(0))
           {

           } /* |if (entry->object == 0)|  */

@q ****** (6).  |entry->object != 0|.@> 

@ |entry->object != 0|.
\initials{LDF 2004.12.04.}
  
@<Define rules@>=

         else /* |entry->object != 0|  */
            {
               real* r = static_cast<real*>(entry->object);              

               *r += @=$3@>;

            }   /* |else| (|entry->object != 0|)  */                

@q ****** (6).@> 

      }  /* |else| (|entry != 0|)  */
   
@q ***** (5)  Set |$$| to 0 and exit rule.@> 

@ Set |@=$$@>| to 0 and exit rule.
\initials{LDF 2004.12.04.}

@<Define rules@>=

  @=$$@> = static_cast<void*>(0);

};

@q **** (4) operation_assignment --> numeric_variable @> 
@q **** (4) MINUS_ASSIGN numeric_expression.           @>

@*3 \§operation assignment> $\longrightarrow$ 
\§numeric variable>\.{MINUS\_ASSIGN} \§numeric expression>.
\initials{LDF 2004.12.04.}

\LOG
\initials{LDF 2004.12.04.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=
@=operation_assignment: numeric_variable MINUS_ASSIGN numeric_expression@>@/
{

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

@q ****** (6) Error handling:  |entry == 0|.@>   

@ Error handling:  |entry == 0|.
\initials{LDF 2004.12.04.}

@<Define rules@>=

   if (entry == static_cast<Id_Map_Entry_Node>(0))
      {
 
      } /* |if (entry == 0)|  */


@q ***** (5) |entry != 0|.@>   

@ |entry != 0|.
\initials{LDF 2004.12.04.}

@<Define rules@>=

    else /* |entry != 0|  */
      {

@q ****** (6).  Error handling:  |entry->object == 0|.@> 

@ Error handling:  |entry->object == 0|.
\initials{LDF 2004.12.04.}
  
@<Define rules@>=

         if (entry->object == static_cast<void*>(0))
           {


           } /* |if (entry->object == 0)|  */

@q ****** (6).  |entry->object != 0|.@> 

@ |entry->object != 0|.
\initials{LDF 2004.12.04.}
  
@<Define rules@>=

         else /* |entry->object != 0|  */
            {
               real* r = static_cast<real*>(entry->object);              

               *r -= @=$3@>;

            }   /* |else| (|entry->object != 0|)  */                

@q ****** (6).@> 

      }  /* |else| (|entry != 0|)  */
   
@q ***** (5)  Set |$$| to 0 and exit rule.@> 

@ Set |@=$$@>| to 0 and exit rule.
\initials{LDF 2004.12.04.}

@<Define rules@>=

  @=$$@> = static_cast<void*>(0);

};

@q **** (4) operation_assignment --> numeric_variable @> 
@q **** (4) TIMES_ASSIGN numeric_expression.           @>

@*3 \§operation assignment> $\longrightarrow$ 
\§numeric variable>\.{TIMES\_ASSIGN} \§numeric expression>.
\initials{LDF 2004.12.04.}

\LOG
\initials{LDF 2004.12.04.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=
@=operation_assignment: numeric_variable TIMES_ASSIGN numeric_expression@>@/
{

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

@q ****** (6) Error handling:  |entry == 0|.@>   

@ Error handling:  |entry == 0|.
\initials{LDF 2004.12.04.}

@<Define rules@>=

   if (entry == static_cast<Id_Map_Entry_Node>(0))
      {
 
      } /* |if (entry == 0)|  */


@q ***** (5) |entry != 0|.@>   

@ |entry != 0|.
\initials{LDF 2004.12.04.}

@<Define rules@>=

    else /* |entry != 0|  */
      {

@q ****** (6).  Error handling:  |entry->object == 0|.@> 

@ Error handling:  |entry->object == 0|.
\initials{LDF 2004.12.04.}
  
@<Define rules@>=

         if (entry->object == static_cast<void*>(0))
           {


           } /* |if (entry->object == 0)|  */

@q ****** (6).  |entry->object != 0|.@> 

@ |entry->object != 0|.
\initials{LDF 2004.12.04.}
  
@<Define rules@>=

         else /* |entry->object != 0|  */
            {
               real* r = static_cast<real*>(entry->object);              

               *r *= @=$3@>;

            }   /* |else| (|entry->object != 0|)  */                

@q ****** (6).@> 

      }  /* |else| (|entry != 0|)  */
   
@q ***** (5)  Set |$$| to 0 and exit rule.@> 

@ Set |@=$$@>| to 0 and exit rule.
\initials{LDF 2004.12.04.}

@<Define rules@>=

  @=$$@> = static_cast<void*>(0);

};

@q **** (4) operation_assignment --> numeric_variable @> 
@q **** (4) DIVIDE_ASSIGN numeric_expression.           @>

@*3 \§operation assignment> $\longrightarrow$ 
\§numeric variable>\.{DIVIDE\_ASSIGN} \§numeric expression>.
\initials{LDF 2004.12.04.}

\LOG
\initials{LDF 2004.12.04.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=
@=operation_assignment: numeric_variable DIVIDE_ASSIGN numeric_expression@>@/
{
   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

@q ****** (6) Error handling:  |numeric_expression == 0|.@>   

@ Error handling:  |numeric_expression == 0|.
\initials{LDF 2004.12.04.}

@<Define rules@>=

   if (@=$3@> == ZERO_REAL)
   ;   /* Do nothing  */

@q ****** (6) Error handling:  |entry == 0|.@>   

@ Error handling:  |entry == 0|.
\initials{LDF 2004.12.04.}

@<Define rules@>=

   else if (entry == static_cast<Id_Map_Entry_Node>(0))
   ;  /* Do nothing  */ 

@q ***** (5) |$3 != 0 && entry != 0|.@>   

@ |@=$3@> != 0 && entry != 0|.
\initials{LDF 2004.12.04.}

@<Define rules@>=

    else /* |@=$3@> != 0 && entry != 0|  */
      {

@q ****** (6).  Error handling:  |entry->object == 0|.@> 

@ Error handling:  |entry->object == 0|.
\initials{LDF 2004.12.04.}
  
@<Define rules@>=

         if (entry->object == static_cast<void*>(0))
           {

           } /* |if (entry->object == 0)|  */

@q ****** (6).  |entry->object != 0|.@> 

@ |entry->object != 0|.
\initials{LDF 2004.12.04.}
  
@<Define rules@>=

         else /* |entry->object != 0|  */
            {
               real* r = static_cast<real*>(entry->object);              

               *r /= @=$3@>;

            }   /* |else| (|entry->object != 0|)  */                

@q ****** (6).@> 

      }  /* |else| (|@=$3@> != 0 && entry != 0|)  */
   
@q ***** (5)  Set |$$| to 0 and exit rule.@> 

@ Set |@=$$@>| to 0 and exit rule.
\initials{LDF 2004.12.04.}

@<Define rules@>=

  @=$$@> = static_cast<void*>(0);

};

@q *** (3) |colors|.@>  

@*2 {\bf colors}.
\initials{LDF 2004.12.18.}

\LOG
\initials{LDF 2004.12.18.}
Added this section.
\ENDLOG

@q **** (4) operation_assignment --> color_variable @> 
@q **** (4) PLUS_ASSIGN color_expression.           @>

@*3 \§operation assignment> $\longrightarrow$ 
\§color variable>\.{PLUS\_ASSIGN} \§color expression>.
\initials{LDF 2004.12.18.}

\LOG
\initials{LDF 2004.12.18.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=
@=operation_assignment: color_variable PLUS_ASSIGN color_expression@>@/
{

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

   Color* c = static_cast<Color*>(@=$3@>);

@q ****** (6) Error handling:  |c == 0|.@>   

@ Error handling:  |c == 0|.
\initials{LDF 2004.12.18.}

@<Define rules@>=


   if (c == static_cast<Color*>(0))
   ;

@q ****** (6) Error handling:  |entry == 0|.@>   

@ Error handling:  |entry == 0|.
\initials{LDF 2004.12.18.}

@<Define rules@>=

   else if (entry == static_cast<Id_Map_Entry_Node>(0))
   ;


@q ***** (5) |c != 0 && entry != 0|.@>   

@ |c != 0 && entry != 0|.
\initials{LDF 2004.12.18.}

@<Define rules@>=

    else /* |c != 0 && entry != 0|  */
      {

@q ****** (6).  Error handling:  |entry->object == 0|.@> 

@ Error handling:  |entry->object == 0|.
\initials{LDF 2004.12.18.}
  
@<Define rules@>=

         if (entry->object == static_cast<void*>(0))
           {

              delete c;

           } /* |if (entry->object == 0)|  */

@q ****** (6).  |entry->object != 0|.@> 

@ |entry->object != 0|.
\initials{LDF 2004.12.18.}
  
@<Define rules@>=

         else /* |entry->object != 0|  */
            {
               Color* q = static_cast<Color*>(entry->object);              

               *q += *c;

               delete c;

            }   /* |else| (|entry->object != 0|)  */                

@q ****** (6).@> 

      }  /* |else| (|c != 0 && entry != 0|)  */
   
@q ***** (5)  Set |$$| to 0 and exit rule.@> 

@ Set |@=$$@>| to 0 and exit rule.
\initials{LDF 2004.12.18.}

@<Define rules@>=

  @=$$@> = static_cast<void*>(0);

};


@q **** (4) operation_assignment --> color_variable @> 
@q **** (4) MINUS_ASSIGN color_expression.           @>

@*3 \§operation assignment> $\longrightarrow$ 
\§color variable>\.{MINUS\_ASSIGN} \§color  expression>.
\initials{LDF 2004.12.18.}

\LOG
\initials{LDF 2004.12.18.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=
@=operation_assignment: color_variable MINUS_ASSIGN color_expression@>@/
{


   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

   Color* c = static_cast<Color*>(@=$3@>);

@q ****** (6) Error handling:  |c == 0|.@>   

@ Error handling:  |c == 0|.
\initials{LDF 2004.12.18.}

@<Define rules@>=


   if (c == static_cast<Color*>(0))
      ;  /* Do nothing  */ 

@q ****** (6) Error handling:  |entry == 0|.@>   

@ Error handling:  |entry == 0|.
\initials{LDF 2004.12.18.}

@<Define rules@>=

   else if (entry == static_cast<Id_Map_Entry_Node>(0))
   ; /* Do nothing  */


@q ***** (5) |c != 0 && entry != 0|.@>   

@ |c != 0 && entry != 0|.
\initials{LDF 2004.12.18.}

@<Define rules@>=

    else /* |c != 0 && entry != 0|  */
      {

@q ****** (6).  Error handling:  |entry->object == 0|.@> 

@ Error handling:  |entry->object == 0|.
\initials{LDF 2004.12.18.}
  
@<Define rules@>=

         if (entry->object == static_cast<void*>(0))
           {


              delete c;

           } /* |if (entry->object == 0)|  */

@q ****** (6).  |entry->object != 0|.@> 

@ |entry->object != 0|.
\initials{LDF 2004.12.18.}
  
@<Define rules@>=

         else /* |entry->object != 0|  */
            {
               Color* q = static_cast<Color*>(entry->object);              

               *q -= *c;

               delete c;

            }   /* |else| (|entry->object != 0|)  */                

@q ****** (6).@> 

      }  /* |else| (|c != 0 && entry != 0|)  */
   
@q ***** (5)  Set |$$| to 0 and exit rule.@> 

@ Set |@=$$@>| to 0 and exit rule.
\initials{LDF 2004.12.18.}

@<Define rules@>=

  @=$$@> = static_cast<void*>(0);

};


@q **** (4) operation_assignment --> color_variable @> 
@q **** (4) TIMES_ASSIGN numeric_expression.        @>

@*3 \§operation assignment> $\longrightarrow$ 
\§color variable>\.{TIMES\_ASSIGN} \§numeric expression>.
\initials{LDF 2004.12.18.}

\LOG
\initials{LDF 2004.12.18.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=
@=operation_assignment: color_variable TIMES_ASSIGN numeric_expression@>@/
{

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

   real r = @=$3@>;

@q ****** (6) Error handling:  |entry == 0|.@>   

@ Error handling:  |entry == 0|.
\initials{LDF 2004.12.18.}

@<Define rules@>=

   if (entry == static_cast<Id_Map_Entry_Node>(0))
      {
 
      } /* |else if (entry == 0)|  */


@q ***** (5) |entry != 0|.@>   

@ |entry != 0|.
\initials{LDF 2004.12.18.}

@<Define rules@>=

    else /* |entry != 0|  */
      {

@q ****** (6).  Error handling:  |entry->object == 0|.@> 

@ Error handling:  |entry->object == 0|.
\initials{LDF 2004.12.18.}
  
@<Define rules@>=

         if (entry->object == static_cast<void*>(0))
         ; /* Do nothing  */ 

@q ****** (6).  |entry->object != 0|.@> 

@ |entry->object != 0|.
\initials{LDF 2004.12.18.}
  
@<Define rules@>=

         else /* |entry->object != 0|  */
            {
               Color* q = static_cast<Color*>(entry->object);              

               *q *= r;


            }   /* |else| (|entry->object != 0|)  */                

@q ****** (6).@> 

      }  /* |else| (|c != 0 && entry != 0|)  */
   
@q ***** (5)  Set |$$| to 0 and exit rule.@> 

@ Set |@=$$@>| to 0 and exit rule.
\initials{LDF 2004.12.18.}

@<Define rules@>=

  @=$$@> = static_cast<void*>(0);

};


@q **** (4) operation_assignment --> color_variable @> 
@q **** (4) DIVIDE_ASSIGN numeric_expression.        @>

@*3 \§operation assignment> $\longrightarrow$ 
\§color variable>\.{DIVIDE\_ASSIGN} \§numeric expression>.
\initials{LDF 2004.12.18.}

\LOG
\initials{LDF 2004.12.18.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=
@=operation_assignment: color_variable DIVIDE_ASSIGN numeric_expression@>@/
{
  @<Common declarations for rules@>@; 

 #if DEBUG_COMPILE
   DEBUG = false;  /* |true|  */
   if (DEBUG)
     {

          cerr_strm << thread_name << "*** Parser: `operation_assignment --> "
                    << "color_variable DIVIDE_ASSIGN numeric_expression'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str("");
     }
#endif /* |DEBUG_COMPILE|  */@; 

   entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

   real r = @=$3@>;


@q ****** (6) Error handling:  |r == 0|.@>   

@ Error handling:  |r == 0|.
\initials{LDF 2004.12.18.}

@<Define rules@>=


   if (r == ZERO_REAL)
      {
         cerr_strm << thread_name << "ERROR! In `yyparse()', rule "
                   << "`operation_assignment --> "
                   << endl 
                   << "color_variable DIVIDE_ASSIGN numeric_expression':"
                   << endl << "`numeric_expression' == 0.  " 
                   << "Can't divide."
                   << endl << "Leaving value of `color_variable', "
                   << "if any, unchanged and will try to continue.";
         
         log_message(cerr_strm);
         cerr_message(cerr_strm, error_stop_value);
         cerr_strm.str(""); 

      } /* |if (c == 0)|  */   



@q ****** (6) Error handling:  |entry == 0|.@>   

@ Error handling:  |entry == 0|.
\initials{LDF 2004.12.18.}

@<Define rules@>=

   else if (entry == static_cast<Id_Map_Entry_Node>(0))
      {
         cerr_strm << thread_name << "ERROR! In `yyparse()', rule "
                   << "`operation_assignment --> "
                   << endl 
                   << "color_variable DIVIDE_ASSIGN numeric_expression':"
                   << endl << "`color_variable' is invalid, i.e., " 
                   << "`entry == 0'.  "
                   << "Can't multiply.  Will try to continue.";
         
         log_message(cerr_strm);
         cerr_message(cerr_strm, error_stop_value);
         cerr_strm.str("");
 
      } /* |else if (entry == 0)|  */


@q ***** (5) |entry != 0|.@>   

@ |entry != 0|.
\initials{LDF 2004.12.18.}

@<Define rules@>=

    else /* |entry != 0|  */
      {

@q ****** (6).  Error handling:  |entry->object == 0|.@> 

@ Error handling:  |entry->object == 0|.
\initials{LDF 2004.12.18.}
  
@<Define rules@>=

         if (entry->object == static_cast<void*>(0))
           {

              cerr_strm << thread_name << "ERROR! In `yyparse()', rule "
                        << "`operation_assignment --> "
                        << endl 
                        << "color_variable DIVIDE_ASSIGN numeric_expression':"
                        << endl << "`color_variable' is unknown.  "
                        << "Can't subtract from it."
                        << endl 
                        << "Will try to continue.";

              log_message(cerr_strm);
              cerr_message(cerr_strm, error_stop_value);
              cerr_strm.str("");

           } /* |if (entry->object == 0)|  */

@q ****** (6).  |entry->object != 0|.@> 

@ |entry->object != 0|.
\initials{LDF 2004.12.18.}
  
@<Define rules@>=

         else /* |entry->object != 0|  */
            {
               Color* q = static_cast<Color*>(entry->object);              

               *q /= r;


            }   /* |else| (|entry->object != 0|)  */                

@q ****** (6).@> 

      }  /* |else| (|c != 0 && entry != 0|)  */
   
@q ***** (5)  Set |$$| to 0 and exit rule.@> 

@ Set |@=$$@>| to 0 and exit rule.
\initials{LDF 2004.12.18.}

@<Define rules@>=

  @=$$@> = static_cast<void*>(0);

};

@q *** (3) |strings|.@>  

@*2 {\bf strings}.
\initials{LDF 2005.01.21.}

\LOG
\initials{LDF 2005.01.21.}
Added this section.
\ENDLOG


@q **** (4) operation_assignment --> string_variable PLUS_ASSIGN @>
@q **** (4) numeric_expression.                                  @>

@*3 \§operation assignment> $\longrightarrow$ \§string variable> 
\.{PLUS\_ASSIGN} \§numeric expression>.
\initials{LDF 2005.01.21.}

\LOG
\initials{LDF 2005.01.21.}
Added this rule.
\ENDLOG


@q **** (4) Definition.@> 

@<Define rules@>=
@=operation_assignment: string_variable PLUS_ASSIGN numeric_expression@>@/
{
  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
   DEBUG = false;  /* |true|  */
   if (DEBUG)
     {

          cerr_strm << thread_name << "*** Parser: `operation_assignment --> "
                    << "string_variable PLUS_ASSIGN numeric_expression'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str("");
     }
#endif /* |DEBUG_COMPILE|  */@; 

  entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

@q ***** (5) Error handling:  |entry == 0|.@>   

@ Error handling:  |entry == 0|.
\initials{LDF 2005.01.21.}

@<Define rules@>=

   if (entry == static_cast<Id_Map_Entry_Node>(0))
      {
         cerr_strm << thread_name << "ERROR! In `yyparse()', rule "
                   << "`operation_assignment --> "
                   << endl 
                   << "string_variable PLUS_ASSIGN numeric_expression':"
                   << endl << "`string_variable' is invalid, i.e., " 
                   << "`entry == 0'.  "
                   << "Not assigning, but will try to continue.";
         
         log_message(cerr_strm);
         cerr_message(cerr_strm, error_stop_value);
         cerr_strm.str("");
 

      } /* |if (entry == 0)|  */


@q ***** (5) |entry != 0|.@>   


    else /* |entry != 0|  */
      {


         string* s = static_cast<string*>(entry->object);

@q ****** (6).  |s == 0|.  Try to allocate memory for a new |string|.@> 

@ |s == 0|.  Try to allocate memory for a new |string|.
\initials{LDF 2005.01.21.}
  
@<Define rules@>=

         if (s == static_cast<string*>(0))
           {

              try 
                {
                   s = new string;     
                }

              catch (bad_alloc)
                {
                   
                   cerr_strm << thread_name << "ERROR! In `yyparse()', rule "
                             << "`operation_assignment --> "
                             << endl 
                             << "string_variable PLUS_ASSIGN numeric_expression':"
                             << endl << "Failed to create new `string'. " 
                             << "Rethrowing `bad_alloc'.";

                   log_message(cerr_strm);
                   cerr_message(cerr_strm, error_stop_value);
                   cerr_strm.str("");
 
                   throw;

                } /* |catch (bad_alloc)|  */
              
               entry->object = static_cast<void*>(s); 

           } /* |if (s == 0)|  */


@q ****** (6).@>           
 
         stringstream t;

         t << @=$3@>;

         *s += t.str();

      }  /* |else| (|entry != 0|)  */
   
@q ***** (5)  Set |$$| to 0 and exit rule.@> 

@ Set |@=$$@>| to 0 and exit rule.
\initials{LDF 2005.01.21.}

@<Define rules@>=

  @=$$@> = static_cast<void*>(0);

};


@q *** (3) |points|.@>  

@*2 {\bf points}.
\initials{LDF 2004.12.04.}

\LOG
\initials{LDF 2004.12.04.}
Added this section.
\ENDLOG

@q **** (4) operation_assignment --> point_variable @> 
@q **** (4) PLUS_ASSIGN point_expression.           @>

@*3 \§operation assignment> $\longrightarrow$ 
\§point variable>\.{PLUS\_ASSIGN} \§point expression>.
\initials{LDF 2004.12.04.}

\LOG
\initials{LDF 2004.12.04.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=
@=operation_assignment: point_variable PLUS_ASSIGN point_expression@>@/
{
  @<Common declarations for rules@>@; 

 #if DEBUG_COMPILE
   DEBUG = false;  /* |true|  */
   if (DEBUG)
     {

          cerr_strm << thread_name << "*** Parser: `operation_assignment --> "
                    << "point_variable PLUS_ASSIGN point_expression'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str("");
     }
#endif /* |DEBUG_COMPILE|  */@; 

   entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

   Point* p = static_cast<Point*>(@=$3@>);

@q ****** (6) Error handling:  |p == 0|.@>   

@ Error handling:  |p == 0|.
\initials{LDF 2004.12.04.}

@<Define rules@>=


   if (p == static_cast<Point*>(0))
      {
         cerr_strm << thread_name << "ERROR! In `yyparse()', rule "
                   << "`operation_assignment --> "
                   << endl 
                   << "point_variable PLUS_ASSIGN point_expression':"
                   << endl << "`point_expression' == 0.  " 
                   << "Leaving value of `point_variable', if any, "
                   << "unchanged and will try to continue.";
         
         log_message(cerr_strm);
         cerr_message(cerr_strm, error_stop_value);
         cerr_strm.str(""); 

      } /* |if (p == 0)|  */   

@q ****** (6) Error handling:  |entry == 0|.@>   

@ Error handling:  |entry == 0|.
\initials{LDF 2004.12.04.}

@<Define rules@>=

   else if (entry == static_cast<Id_Map_Entry_Node>(0))
      {
         cerr_strm << thread_name << "ERROR! In `yyparse()', rule "
                   << "`operation_assignment --> "
                   << endl 
                   << "point_variable PLUS_ASSIGN point_expression':"
                   << endl << "`point_variable' is invalid, i.e., " 
                   << "`entry == 0'.  "
                   << "Deleting `point_expression' and will try to continue.";
         
         log_message(cerr_strm);
         cerr_message(cerr_strm, error_stop_value);
         cerr_strm.str("");
 
      } /* |else if (entry == 0)|  */


@q ***** (5) |p != 0 && entry != 0|.@>   

@ |p != 0 && entry != 0|.
\initials{LDF 2004.12.04.}

@<Define rules@>=

    else /* |p != 0 && entry != 0|  */
      {

@q ****** (6).  Error handling:  |entry->object == 0|.@> 

@ Error handling:  |entry->object == 0|.
\initials{LDF 2004.12.04.}
  
@<Define rules@>=

         if (entry->object == static_cast<void*>(0))
           {

              cerr_strm << thread_name << "ERROR! In `yyparse()', rule "
                        << "`operation_assignment --> "
                        << endl 
                        << "point_variable PLUS_ASSIGN point_expression':"
                        << endl << "`point_variable' is unknown.  "
                        << "Can't add to it."
                        << endl 
                        << "Deleting `point_expression' and will "
                        << "try to continue.";

              log_message(cerr_strm);
              cerr_message(cerr_strm, error_stop_value);
              cerr_strm.str("");

              delete p;

           } /* |if (entry->object == 0)|  */

@q ****** (6).  |entry->object != 0|.@> 

@ |entry->object != 0|.
\initials{LDF 2004.12.04.}
  
@<Define rules@>=

         else /* |entry->object != 0|  */
            {
               Point* q = static_cast<Point*>(entry->object);              

               *q += *p;

               delete p;

            }   /* |else| (|entry->object != 0|)  */                

@q ****** (6).@> 

      }  /* |else| (|p != 0 && entry != 0|)  */
   
@q ***** (5)  Set |$$| to 0 and exit rule.@> 

@ Set |@=$$@>| to 0 and exit rule.
\initials{LDF 2004.12.04.}

@<Define rules@>=

  @=$$@> = static_cast<void*>(0);

};

@q **** (4) operation_assignment --> point_variable @> 
@q **** (4) MINUS_ASSIGN point_expression.           @>

@*3 \§operation assignment> $\longrightarrow$ 
\§point variable>\.{MINUS\_ASSIGN} \§point expression>.
\initials{LDF 2004.12.06.}

\LOG
\initials{LDF 2004.12.06.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=
@=operation_assignment: point_variable MINUS_ASSIGN point_expression@>@/
{
  @<Common declarations for rules@>@; 

 #if DEBUG_COMPILE
   DEBUG = false;  /* |true|  */
   if (DEBUG)
     {

          cerr_strm << thread_name << "*** Parser: `operation_assignment --> "
                    << "point_variable MINUS_ASSIGN point_expression'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str("");
     }
#endif /* |DEBUG_COMPILE|  */@; 

   entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

   Point* p = static_cast<Point*>(@=$3@>);

@q ****** (6) Error handling:  |p == 0|.@>   

@ Error handling:  |p == 0|.
\initials{LDF 2004.12.06.}

@<Define rules@>=


   if (p == 0)
      {
         cerr_strm << thread_name << "ERROR! In `yyparse()', rule "
                   << "`operation_assignment --> "
                   << endl 
                   << "point_variable MINUS_ASSIGN point_expression':"
                   << endl << "`point_expression' == 0.  " 
                   << "Leaving value of `point_variable', if any, "
                   << "unchanged and will try to continue.";
         
         log_message(cerr_strm);
         cerr_message(cerr_strm, error_stop_value);
         cerr_strm.str(""); 

      } /* |if (p == 0)|  */   

@q ****** (6) Error handling:  |entry == 0|.@>   

@ Error handling:  |entry == 0|.
\initials{LDF 2004.12.06.}

@<Define rules@>=

   else if (entry == static_cast<Id_Map_Entry_Node>(0))
      {
         cerr_strm << thread_name << "ERROR! In `yyparse()', rule "
                   << "`operation_assignment --> "
                   << endl 
                   << "point_variable MINUS_ASSIGN point_expression':"
                   << endl << "`point_variable' is invalid, i.e., " 
                   << "`entry == 0'.  "
                   << "Deleting `point_expression' and will try to continue.";
         
         log_message(cerr_strm);
         cerr_message(cerr_strm, error_stop_value);
         cerr_strm.str("");
 
      } /* |else if (entry == 0)|  */


@q ***** (5) |p != 0 && entry != 0|.@>   

@ |p != 0 && entry != 0|.
\initials{LDF 2004.12.06.}

@<Define rules@>=

    else /* |p != 0 && entry != 0|  */
      {

@q ****** (6).  Error handling:  |entry->object == 0|.@> 

@ Error handling:  |entry->object == 0|.
\initials{LDF 2004.12.06.}
  
@<Define rules@>=

         if (entry->object == static_cast<void*>(0))
           {

              cerr_strm << thread_name << "ERROR! In `yyparse()', rule "
                        << "`operation_assignment --> "
                        << endl 
                        << "point_variable MINUS_ASSIGN point_expression':"
                        << endl << "`point_variable' is unknown.  "
                        << "Can't subtract from it."
                        << endl 
                        << "Deleting `point_expression' and will "
                        << "try to continue.";

              log_message(cerr_strm);
              cerr_message(cerr_strm, error_stop_value);
              cerr_strm.str("");

              delete p;

           } /* |if (entry->object == 0)|  */

@q ****** (6).  |entry->object != 0|.@> 

@ |entry->object != 0|.
\initials{LDF 2004.12.06.}
  
@<Define rules@>=

         else /* |entry->object != 0|  */
            {
               Point* q = static_cast<Point*>(entry->object);              

               *q -= *p;

               delete p;

            }   /* |else| (|entry->object != 0|)  */                

@q ****** (6).@> 

      }  /* |else| (|p != 0 && entry != 0|)  */
   
@q ***** (5)  Set |$$| to 0 and exit rule.@> 

@ Set |@=$$@>| to 0 and exit rule.
\initials{LDF 2004.12.06.}

@<Define rules@>=

  @=$$@> = static_cast<void*>(0);

};

@q **** (4) operation_assignment --> point_variable @> 
@q **** (4) TIMES_ASSIGN numeric_expression.        @>

@*3 \§operation assignment> $\longrightarrow$ 
\§point variable>\.{TIMES\_ASSIGN} \§numeric expression>.
\initials{LDF 2004.12.18.}

\LOG
\initials{LDF 2004.12.18.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=
@=operation_assignment: point_variable TIMES_ASSIGN numeric_expression@>@/
{
  @<Common declarations for rules@>@; 

 #if DEBUG_COMPILE
   DEBUG = false;  /* |true|  */
   if (DEBUG)
     {

          cerr_strm << thread_name << "*** Parser: `operation_assignment --> "
                    << "point_variable TIMES_ASSIGN numeric_expression'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str("");
     }
#endif /* |DEBUG_COMPILE|  */@; 

   entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

   real r = @=$3@>;

@q ****** (6) Error handling:  |entry == 0|.@>   

@ Error handling:  |entry == 0|.
\initials{LDF 2004.12.18.}

@<Define rules@>=

   if (entry == static_cast<Id_Map_Entry_Node>(0))
      {
         cerr_strm << thread_name << "ERROR! In `yyparse()', rule "
                   << "`operation_assignment --> "
                   << endl 
                   << "point_variable TIMES_ASSIGN numeric_expression':"
                   << endl << "`point_variable' is invalid, i.e., " 
                   << "`entry == 0'.  "
                   << "Can't multiply.  Will try to continue.";
         
         log_message(cerr_strm);
         cerr_message(cerr_strm, error_stop_value);
         cerr_strm.str("");
 
      } /* |else if (entry == 0)|  */


@q ***** (5) |entry != 0|.@>   

@ |entry != 0|.
\initials{LDF 2004.12.18.}

@<Define rules@>=

    else /* |entry != 0|  */
      {

@q ****** (6).  Error handling:  |entry->object == 0|.@> 

@ Error handling:  |entry->object == 0|.
\initials{LDF 2004.12.18.}
  
@<Define rules@>=

         if (entry->object == static_cast<void*>(0))
           {

              cerr_strm << thread_name << "ERROR! In `yyparse()', rule "
                        << "`operation_assignment --> "
                        << endl 
                        << "point_variable TIMES_ASSIGN numeric_expression':"
                        << endl << "`point_variable' is unknown.  "
                        << "Can't subtract from it."
                        << endl 
                        << "Will try to continue.";

              log_message(cerr_strm);
              cerr_message(cerr_strm, error_stop_value);
              cerr_strm.str("");

           } /* |if (entry->object == 0)|  */

@q ****** (6).  |entry->object != 0|.@> 

@ |entry->object != 0|.
\initials{LDF 2004.12.18.}
  
@<Define rules@>=

         else /* |entry->object != 0|  */
            {
               Point* q = static_cast<Point*>(entry->object);              

               *q *= r;


            }   /* |else| (|entry->object != 0|)  */                

@q ****** (6).@> 

      }  /* |else| (|p != 0 && entry != 0|)  */
   
@q ***** (5)  Set |$$| to 0 and exit rule.@> 

@ Set |@=$$@>| to 0 and exit rule.
\initials{LDF 2004.12.18.}

@<Define rules@>=

  @=$$@> = static_cast<void*>(0);

};

@q **** (4) operation_assignment --> point_variable @> 
@q **** (4) DIVIDE_ASSIGN numeric_expression.        @>

@*3 \§operation assignment> $\longrightarrow$ 
\§point variable>\.{DIVIDE\_ASSIGN} \§numeric expression>.
\initials{LDF 2004.12.18.}

\LOG
\initials{LDF 2004.12.18.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=
@=operation_assignment: point_variable DIVIDE_ASSIGN numeric_expression@>@/
{
  @<Common declarations for rules@>@; 

 #if DEBUG_COMPILE
   DEBUG = false;  /* |true|  */
   if (DEBUG)
     {

          cerr_strm << thread_name << "*** Parser: `operation_assignment --> "
                    << "point_variable DIVIDE_ASSIGN numeric_expression'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str("");
     }
#endif /* |DEBUG_COMPILE|  */@; 

   entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

   real r = @=$3@>;


@q ****** (6) Error handling:  |r == 0|.@>   

@ Error handling:  |r == 0|.
\initials{LDF 2004.12.18.}

@<Define rules@>=


   if (r == ZERO_REAL)
      {
         cerr_strm << thread_name << "ERROR! In `yyparse()', rule "
                   << "`operation_assignment --> "
                   << endl 
                   << "point_variable DIVIDE_ASSIGN numeric_expression':"
                   << endl << "`numeric_expression' == 0.  " 
                   << "Can't divide."
                   << endl << "Leaving value of `point_variable', "
                   << "if any, unchanged and will try to continue.";
         
         log_message(cerr_strm);
         cerr_message(cerr_strm, error_stop_value);
         cerr_strm.str(""); 

      } /* |if (p == 0)|  */   



@q ****** (6) Error handling:  |entry == 0|.@>   

@ Error handling:  |entry == 0|.
\initials{LDF 2004.12.18.}

@<Define rules@>=

   else if (entry == static_cast<Id_Map_Entry_Node>(0))
      {
         cerr_strm << thread_name << "ERROR! In `yyparse()', rule "
                   << "`operation_assignment --> "
                   << endl 
                   << "point_variable DIVIDE_ASSIGN numeric_expression':"
                   << endl << "`point_variable' is invalid, i.e., " 
                   << "`entry == 0'.  "
                   << "Can't multiply.  Will try to continue.";
         
         log_message(cerr_strm);
         cerr_message(cerr_strm, error_stop_value);
         cerr_strm.str("");
 
      } /* |else if (entry == 0)|  */


@q ***** (5) |entry != 0|.@>   

@ |entry != 0|.
\initials{LDF 2004.12.18.}

@<Define rules@>=

    else /* |entry != 0|  */
      {

@q ****** (6).  Error handling:  |entry->object == 0|.@> 

@ Error handling:  |entry->object == 0|.
\initials{LDF 2004.12.18.}
  
@<Define rules@>=

         if (entry->object == static_cast<void*>(0))
           {

              cerr_strm << thread_name << "ERROR! In `yyparse()', rule "
                        << "`operation_assignment --> "
                        << endl 
                        << "point_variable DIVIDE_ASSIGN numeric_expression':"
                        << endl << "`point_variable' is unknown.  "
                        << "Can't subtract from it."
                        << endl 
                        << "Will try to continue.";

              log_message(cerr_strm);
              cerr_message(cerr_strm, error_stop_value);
              cerr_strm.str("");

           } /* |if (entry->object == 0)|  */

@q ****** (6).  |entry->object != 0|.@> 

@ |entry->object != 0|.
\initials{LDF 2004.12.18.}
  
@<Define rules@>=

         else /* |entry->object != 0|  */
            {
               Point* q = static_cast<Point*>(entry->object);              

               *q /= r;


            }   /* |else| (|entry->object != 0|)  */                

@q ****** (6).@> 

      }  /* |else| (|p != 0 && entry != 0|)  */
   
@q ***** (5)  Set |$$| to 0 and exit rule.@> 

@ Set |@=$$@>| to 0 and exit rule.
\initials{LDF 2004.12.18.}

@<Define rules@>=

  @=$$@> = static_cast<void*>(0);

};

@q *** (3) |paths|.@>  

@*2 {\bf paths}.
\initials{LDF 2004.12.04.}

\LOG
\initials{LDF 2004.12.04.}
Added this section.
\ENDLOG


@q **** (4) operation_assignment --> path_variable PLUS_ASSIGN @>
@q **** (4) point_expression.                                       @>

@*3 \§operation assignment> $\longrightarrow$ \§path variable> 
\.{PLUS\_ASSIGN} \§point expression>.
\initials{LDF 2004.10.27.}

\LOG
\initials{LDF 2004.10.27.}
Added this rule.

\initials{LDF 2004.11.05.}
Changed the symbol on the left-hand side of this rule from 
|path_assignment| to |operation_assignment|.

\initials{LDF 2004.12.03.}
Moved this rule from \filename{passign.w} to this file 
(\filename{popassgn.w}). 

\initials{LDF 2005.02.11.}
Changed \§path variable> to \§path-like variable>.

\initials{LDF 2005.04.30.}
Changed \§path-like variable> back to \§path variable>.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=operation_assignment: path_variable PLUS_ASSIGN point_expression@>@/
{
  @<Common declarations for rules@>@; 

 #if DEBUG_COMPILE
   DEBUG = false;  /* |true|  */
   if (DEBUG)
     {

          cerr_strm << thread_name << "*** Parser: `operation_assignment --> "
                    << "path_variable PLUS_ASSIGN point_expression'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str("");
     }
#endif /* |DEBUG_COMPILE|  */@; 

  Point* p = static_cast<Point*>(@=$3@>); 
  
  entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

@q ***** (5) Error handling:  |entry == 0|.@>   
@ Error handling:  |entry == 0|.
\initials{LDF 2004.10.27.}
@<Define rules@>=

   if (entry == static_cast<Id_Map_Entry_Node>(0))
      {
         cerr_strm << thread_name << "ERROR! In `yyparse()', rule "
                   << "`operation_assignment --> "
                   << endl 
                   << "path_variable PLUS_ASSIGN point_expression':"
                   << endl << "`path_variable' is invalid, i.e., " 
                   << "`entry == 0'."
                   << endl << "Deleting `point_expression'.";
         
         log_message(cerr_strm);
         cerr_message(cerr_strm, error_stop_value);
         cerr_strm.str("");
 
         delete p;

      } /* |if (entry == 0)|  */


@q ***** (5) Error handling:  |entry| has invalid type.@>   
@ Error handling:  |entry| has invalid type.
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.02.11.}
Added this section.
\ENDLOG

@<Define rules@>=

   else if (!(   entry->type == PATH
              || entry->type == POLYGON
              || entry->type == TRIANGLE
              || entry->type == RECTANGLE
              || entry->type == REG_POLYGON
              || entry->type == ELLIPSE
              || entry->type == CIRCLE))


      {
          cerr_strm << thread_name << "ERROR! In `yyparse()', rule "
                    << "`operation_assignment --> "
                    << endl 
                    << "path_variable PLUS_ASSIGN point_expression':"
                    << endl << "`entry' has an invalid `type':  " 
                    << name_map[entry->type] << "."
                    << endl << "Deleting `point_expression'.";
         
          log_message(cerr_strm);
          cerr_message(cerr_strm, error_stop_value);
          cerr_strm.str("");
 
          delete p;

      }  /* |else|  (|entry->type| is invalid.)  */

@q ***** (5) |entry != 0| and |entry->type| is valid.@>   

@ |entry != 0| and |entry->type| is valid.
\initials{LDF Undated.}

@<Define rules@>=

    else /* |entry != 0| and |entry->type| is valid.  */
      {

         Path* q = static_cast<Path*>(entry->object);

@q ****** (6).  |q == 0|.  Try to allocate memory for a object.@> 

@ |q == 0|.  Try to allocate memory for a new object.
\initials{LDF 2004.10.27.}

\LOG
\initials{LDF 2005.02.11.}
Now creating a new |Path|, if |entry->type == PATH|,
a new |Polygon|, if |entry->type == POLYGON|, etc.
If |entry->type != PATH|, the pointer to the new object 
is cast to |Path*|.
\ENDLOG 
  
@<Define rules@>=

         if (q == static_cast<Path*>(0))
           {

              try 
                {
                   if (entry->type == PATH)
                      q = create_new<Path>(0);     
                   else if (entry->type == POLYGON)
                      q = static_cast<Path*>(create_new<Polygon>(0)); 
                   else if (entry->type == TRIANGLE)
                      q = static_cast<Path*>(create_new<Triangle>(0)); 
                   else if (entry->type == RECTANGLE)
                      q = static_cast<Path*>(create_new<Rectangle>(0)); 
                   else if (entry->type == REG_POLYGON)
                      q = static_cast<Path*>(create_new<Reg_Polygon>(0)); 
                   else if (entry->type == ELLIPSE)
                      q = static_cast<Path*>(create_new<Ellipse>(0)); 
                   else if (entry->type == CIRCLE)
                      q = static_cast<Path*>(create_new<Circle>(0)); 
                      
                }  /* |try|  */

              catch (bad_alloc)
                {
                   
                   cerr_strm << thread_name << "ERROR! In `yyparse()', rule "
                             << "`operation_assignment --> "
                             << endl 
                             << "path_variable PLUS_ASSIGN point_expression':"
                             << endl << "Failed to create new object. " 
                             << "Deleting `point_expression' and rethrowing "
                             << "`bad_alloc'.";

                   log_message(cerr_strm);
                   cerr_message(cerr_strm, error_stop_value);
                   cerr_strm.str("");
 
                   delete p;
 
                   throw;

                } /* |catch (bad_alloc)|  */


           } /* |if (q == 0)|  */


@q ****** (6).@>           


         *q += *p;

         entry->object = static_cast<void*>(q);


         delete p;

      }  /* |else| (|entry != 0|)  */
   
@q ***** (5)  Set |$$| to 0 and exit rule.@> 

@ Set |@=$$@>| to 0 and exit rule.
\initials{LDF 2004.11.05.}

\LOG
\initials{LDF 2004.11.05.}
Now setting |@=$$@>| to 0.  Formerly, it was set to |entry->object|
upon success and 0 upon failure.
\ENDLOG 

@<Define rules@>=

  @=$$@> = static_cast<void*>(0);

};

@q **** (4) operation_assignment --> path_variable PLUS_ASSIGN path_join.@>

@*3 \§operation assignment> $\longrightarrow$ \§path variable>
\.{PLUS\_ASSIGN} \§path join>.
\initials{LDF 2004.10.27.}

\LOG
\initials{LDF 2004.10.27.}
Added this rule.

\initials{LDF 2004.11.01.}
Changed |path_variable| to |path_like_variable|.  This change made it
impossible to allocate memory for |entry->object| if it's null,
because it's not known what type it should be.  It would be possible
to find out by examining |entry->type| and proceding accordingly, 
but I don't think it would be worthwhile.

\initials{LDF 2004.11.05.}
Changed the symbol on the left-hand side of this rule from 
|path_assignment| to |operation_assignment|.

\initials{LDF 2004.12.03.}
Moved this rule from \filename{passign.w} to this file 
(\filename{popassgn.w}). 

\initials{LDF 2005.10.24.}
Changed |path_like_variable| to |path_variable|.
Removed debugging code.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=operation_assignment: path_variable PLUS_ASSIGN path_join@>@/
{

  string s(@=$3@>); 

  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

@q ***** (5) Error handling:  |entry == 0 || entry->object == 0|.@>   
@ Error handling:  |entry == 0 || entry->object == 0|.
\initials{LDF 2004.10.27.}
@<Define rules@>=

   if (entry == static_cast<Id_Map_Entry_Node>(0) || entry->object == static_cast<void*>(0))
      {

          /* Do nothing.  */

      } /* |if (entry == 0 || entry->object == 0)|  */

@q ****** (6).  |entry != 0 && entry->object != 0|.@> 

@ |entry != 0 && entry->object != 0|.
\initials{LDF 2004.10.27.}
  
@<Define rules@>=

         else /* |entry != 0 && entry->object != 0|  */
           { 

              Path* q = static_cast<Path*>(entry->object);           

              *q += s;
 
              entry->object = static_cast<void*>(q);
  
           }  /* |else| (|entry != 0 && entry->object != 0|)  */

@q ***** (5) Set |$$| to 0 and exit rule.@>   

@ Set |$$| to 0 and exit rule.
\initials{LDF 2004.11.05.}

\LOG
\initials{LDF 2004.11.05.}
Now setting |@=$$@>| to 0.  Formerly, it was set to |entry->object|
upon success and 0 upon failure. 
\ENDLOG 

@<Define rules@>=

  @=$$@> = static_cast<void*>(0);

};

@q **** (4) operation_assignment --> path_variable PLUS_ASSIGN CYCLE.@>

@*3 \§operation assignment> $\longrightarrow$ \§path variable> 
\.{PLUS\_ASSIGN} \.{CYCLE}.
\initials{LDF 2004.11.01.}

\LOG
\initials{LDF 2004.11.01.}
Added this rule.

\initials{LDF 2004.11.05.}
Changed the symbol on the left-hand side of this rule from 
|path_assignment| to |operation_assignment|.

\initials{LDF 2004.12.03.}
Moved this rule from \filename{passign.w} to this file 
(\filename{popassgn.w}). 

\initials{LDF 2005.02.11.}
Changed \§path variable> to \§path-like variable>.

\initials{LDF 2005.04.30.}
Changed \§path-like variable> back to \§path variable>.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=operation_assignment: path_variable PLUS_ASSIGN CYCLE@>@/
{

  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

@q ***** (5) Error handling:  |entry == 0|.@>   
@ Error handling:  |entry == 0|.
\initials{LDF 2004.11.01.}
@<Define rules@>=

   if (entry == static_cast<Id_Map_Entry_Node>(0))
      {

  
      } /* |if (entry == 0)|  */


@q ***** (5) |entry != 0|.@>   

    else /* |entry != 0|  */
      {

         Path* p = static_cast<Path*>(entry->object);

@q ****** (6).  |p == 0|.  Try to allocate memory for a new |Path|.@> 

@ |p == 0|.  Try to allocate memory for a new |Path|.
\initials{LDF 2004.11.01.}

\LOG
\initials{LDF 2005.02.21.}
Added code for allocating memory for an object of the type 
indicated by |entry->type|.
\ENDLOG 
  
@<Define rules@>=

         if (p == static_cast<Path*>(0))
           {

@q ******* (7) @> 

     
                   if (entry->type == PATH)
                      p = create_new<Path>(0);     
                   else if (entry->type == POLYGON)
                      p = create_new<Polygon>(0);      
                   else if (entry->type == TRIANGLE)
                      p = create_new<Triangle>(0);      
                   else if (entry->type == REG_POLYGON)
                      p = create_new<Reg_Polygon>(0);      
                   else if (entry->type == RECTANGLE)
                      p = create_new<Rectangle>(0);      
                   else if (entry->type == ELLIPSE)
                      p = create_new<Ellipse>(0);      
                   else if (entry->type == CIRCLE)
                      p = create_new<Circle>(0);      
                   else 
                      {

                      }  /* |else| (|entry->type| is invalid).  */


@q ******* (7) @> 

              entry->object = static_cast<void*>(p); 


           } /* |if (p == 0)|  */


@q ****** (6).@>           

         if (p != static_cast<Path*>(0))
            p->set_cycle(true);

      }  /* |else| (|entry != 0|)  */
   
@q ***** (5) Set |$$| to 0 and exit rule.@>   

@ Set |$$| to 0 and exit rule.
\initials{LDF 2004.11.05.}

\LOG
\initials{LDF 2004.11.05.}
Now setting |@=$$@>| to 0.  Formerly, it was set to |entry->object|
upon success and 0 upon failure. 
\ENDLOG 

@<Define rules@>=

  @=$$@> = static_cast<void*>(0);

};

@q **** (4) operation_assignment --> path_variable MINUS_ASSIGN CYCLE.@>

@*3 \§operation assignment> $\longrightarrow$ \§path variable>
\.{MINUS\_ASSIGN} \.{CYCLE}.
\initials{LDF 2004.11.01.}


\LOG
\initials{LDF 2004.11.01.}
Added this rule.

\initials{LDF 2004.11.05.}
Changed the symbol on the left-hand side of this rule from 
|path_assignment| to |operation_assignment|.

\initials{LDF 2004.12.03.}
Moved this rule from \filename{passign.w} to this file 
(\filename{popassgn.w}). 

\initials{LDF 2005.02.21.}
Changed \§path variable> to \§path-like variable>.

\initials{LDF 2005.10.24.}
Changed |path_like_variable| to |path_variable|.
Removed debugging code.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=operation_assignment: path_variable MINUS_ASSIGN CYCLE@>@/ 
{

    Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

   if (entry == static_cast<Id_Map_Entry_Node>(0))
      {
         /* Do nothing.  */          
      }

@q ***** (5) |entry != 0|.@>   
@
\LOG
\initials{LDF 2005.02.21.}
Removed code for allocating memory for a new |Path|, 
if |entry->object == 0|.  There's no need to make an unknown 
|Path| non-cyclical.
\ENDLOG

@<Define rules@>=

    else /* |entry != 0|  */
      {

         Path* p = static_cast<Path*>(entry->object);

         if (p != static_cast<Path*>(0))
            {
                p->set_cycle(false);

            }   /* |if (p != 0)|  */

@q ****** (6).@> 

      }  /* |else| (|entry != 0|)  */
   
@q ***** (5) Set |$$| to 0 and exit rule.@>   

@ Set |$$| to 0 and exit rule.
\initials{LDF 2004.11.05.}

\LOG
\initials{LDF 2004.11.05.}
Now setting |@=$$@>| to 0.  Formerly, it was set to |entry->object|
upon success and 0 upon failure. 
\ENDLOG 

@<Define rules@>=

  @=$$@> = static_cast<void*>(0);

};

@q *** (3) |nurbs|.@>  

@*2 {\bf nurbs}.
\initials{LDF 2005.01.26.}

\LOG
\initials{LDF 2005.01.26.}
Added this section.
\ENDLOG


@q **** (4) operation_assignment --> nurb_variable PLUS_ASSIGN point_expression.@>

@*3 \§operation assignment> $\longrightarrow$ \§nurb variable> 
\.{PLUS\_ASSIGN} \§point expression>.
\initials{LDF 2005.01.26.}

\LOG
\initials{LDF 2005.01.26.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=
@=operation_assignment: nurb_variable PLUS_ASSIGN point_expression@>@/
{
  @<Common declarations for rules@>@; 

 #if DEBUG_COMPILE
   DEBUG = false;  /* |true|  */
   if (DEBUG)
     {

          cerr_strm << thread_name << "*** Parser:  "
                    << "`operation_assignment --> "
                    << "nurb_variable PLUS_ASSIGN point_expression'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str("");
     }
#endif /* |DEBUG_COMPILE|  */@; 

  Point* p = static_cast<Point*>(@=$3@>); 
  
  entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

@q ****** (6) Error handling:  |entry == 0|.@>   
@ Error handling:  |entry == 0|.
\initials{LDF 2005.01.26.}
@<Define rules@>=

   if (entry == static_cast<Id_Map_Entry_Node>(0))
      {
         cerr_strm << thread_name << "ERROR! In `yyparse()', rule "
                   << "`operation_assignment --> "
                   << endl 
                   << "nurb_variable PLUS_ASSIGN point_expression':"
                   << endl << "`nurb_variable' is invalid, i.e., " 
                   << "`entry == 0'."
                   << endl << "Deleting `point_expression'.";
         
         log_message(cerr_strm);
         cerr_message(cerr_strm, error_stop_value);
         cerr_strm.str("");
 
         delete p;

      } /* |if (entry == 0)|  */


@q ****** (6) |entry != 0|.@>   


    else /* |entry != 0|  */
      {


         Nurb* q = static_cast<Nurb*>(entry->object);

@q ******* (7).  |q == 0|.  Try to allocate memory for a new |Nurb|.@> 

@ |q == 0|.  Try to allocate memory for a new |Nurb|.
\initials{LDF 2005.01.26.}
  
@<Define rules@>=

         if (q == static_cast<Nurb*>(0))
           {

              try 
                {
                   q = new Nurb;     
                }

              catch (bad_alloc)
                {
                   
                   cerr_strm << thread_name << "ERROR! In `yyparse()', rule "
                             << "`operation_assignment --> "
                             << endl 
                             << "nurb_variable PLUS_ASSIGN point_expression':"
                             << endl << "Failed to create new `nurb'. " 
                             << "Deleting `point_expression' and rethrowing "
                             << "`bad_alloc'.";

                   log_message(cerr_strm);
                   cerr_message(cerr_strm, error_stop_value);
                   cerr_strm.str("");
 
                   delete p;
 
                   throw;

                } /* |catch (bad_alloc)|  */


           } /* |if (q == 0)|  */


@q ******* (7).@>           


         *q += p;

         entry->object = static_cast<void*>(q);


      }  /* |else| (|entry != 0|)  */
   
@q ****** (6)  Set |$$| to 0 and exit rule.@> 

@ Set |@=$$@>| to 0 and exit rule.
\initials{LDF 2005.01.26.}

@<Define rules@>=

  @=$$@> = static_cast<void*>(0);

};

@q **** (4) operation_assignment --> nurb_variable PLUS_ASSIGN @>
@q **** (4) knot_or_weight numeric_expression.                 @>

@*3 \§operation assignment> $\longrightarrow$ \§nurb variable> 
\.{PLUS\_ASSIGN} \§knot or weight> \§numeric expression>.
\initials{LDF 2005.01.26.}

\LOG
\initials{LDF 2005.01.26.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=
@=operation_assignment: nurb_variable PLUS_ASSIGN knot_or_weight@>@/
@=numeric_expression@>@/
{
  @<Common declarations for rules@>@; 

 #if DEBUG_COMPILE
   DEBUG = false;  /* |true|  */
   if (DEBUG)
     {

          cerr_strm << thread_name << "*** Parser:  "
                    << "`operation_assignment --> "
                    << "nurb_variable PLUS_ASSIGN knot_or_weight "
                    << "numeric_expression'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str("");
     }
#endif /* |DEBUG_COMPILE|  */@; 

  real r = @=$4@>; 
  
  entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

@q ****** (6) Error handling:  |entry == 0|.@>   
@ Error handling:  |entry == 0|.
\initials{LDF 2005.01.26.}
@<Define rules@>=

   if (entry == static_cast<Id_Map_Entry_Node>(0))
      {
         cerr_strm << thread_name << "ERROR! In `yyparse()', rule "
                   << "`operation_assignment --> "
                   << endl 
                   << "nurb_variable PLUS_ASSIGN knot_or_weight numeric_expression':"
                   << endl << "`nurb_variable' is invalid, i.e., " 
                   << "`entry == 0'."
                   << endl << "Deleting `numeric_expression'.";
         
         log_message(cerr_strm);
         cerr_message(cerr_strm, error_stop_value);
         cerr_strm.str("");
 
      } /* |if (entry == 0)|  */


@q ****** (6) |entry != 0|.@>   


    else /* |entry != 0|  */
      {


         Nurb* q = static_cast<Nurb*>(entry->object);

@q ******* (7).  |q == 0|.  Try to allocate memory for a new |Nurb|.@> 

@ |q == 0|.  Try to allocate memory for a new |Nurb|.
\initials{LDF 2005.01.26.}
  
@<Define rules@>=

         if (q == static_cast<Nurb*>(0))
           {

              try 
                {
                   q = new Nurb;     
                }

              catch (bad_alloc)
                {
                   
                   cerr_strm << thread_name << "ERROR! In `yyparse()', rule "
                             << "`operation_assignment --> "
                             << endl 
                             << "nurb_variable PLUS_ASSIGN knot_or_weight "
                             << "numeric_expression':"
                             << endl << "Failed to create new `nurb'. " 
                             << "Deleting `numeric_expression' and rethrowing "
                             << "`bad_alloc'.";

                   log_message(cerr_strm);
                   cerr_message(cerr_strm, error_stop_value);
                   cerr_strm.str("");
 
                   throw;

                } /* |catch (bad_alloc)|  */


           } /* |if (q == 0)|  */


@q ******* (7).@>           

         q->append_knot_or_weight(r, @=$3@>, scanner_node);

         entry->object = static_cast<void*>(q);


      }  /* |else| (|entry != 0|)  */
   
@q ****** (6)  Set |$$| to 0 and exit rule.@> 

@ Set |@=$$@>| to 0 and exit rule.
\initials{LDF 2005.01.26.}

@<Define rules@>=

  @=$$@> = static_cast<void*>(0);

};


@q **** (4) knot_or_weight.@>   
@*3 \§knot or weight>.
\initials{LDF 2005.01.27.}

\LOG
\initials{LDF 2005.01.27.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <int_value> knot_or_weight@>

@q ***** (5) knot_or_weight --> KNOT.@> 

@*4 \§knot or weight> $\longrightarrow$ \.{KNOT}.
\initials{LDF 2005.01.27.}

\LOG
\initials{LDF 2005.01.27.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=knot_or_weight: KNOT@>@/
{

   @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
   DEBUG = false;  /* |true|  */
   if (DEBUG)
     {

          cerr_strm << thread_name << "*** Parser:  `knot_or_weight --> "
                    << "KNOT'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str("");
     }
#endif /* |DEBUG_COMPILE|  */@; 

   @=$$@> = 0;
   
};

@*4 \§knot or weight> $\longrightarrow$ \.{WEIGHT}.
\initials{LDF 2005.01.27.}

\LOG
\initials{LDF 2005.01.27.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=knot_or_weight: WEIGHT@>@/
{

   @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
   DEBUG = false;  /* |true|  */
   if (DEBUG)
     {

          cerr_strm << thread_name << "*** Parser:  `knot_or_weight --> "
                    << "WEIGHT'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str("");
     }
#endif /* |DEBUG_COMPILE|  */@; 

   @=$$@> = 1;
   
};

@q *** (3) |sphere_developments|.@>  

@*2 {\bf sphere_developments}.
\initials{LDF 2009.11.16.}

\LOG
\initials{LDF 2009.11.16.}
Added this section.
\ENDLOG

@q **** (4) operation_assignment --> sphere_development_variable PLUS_ASSIGN @>
@q **** (4) LEFT_PARENTHESIS numeric_expression COMMA numeric_expression     @>
@q          RIGHT_PARENTHESIS                                                @>

@*3 \§operation assignment> $\longrightarrow$ \§sphere development variable> 
\.{PLUS\_ASSIGN} \.{LEFT\_PARENTHESIS} \§numeric expression> \.{COMMA} 
\.{RIGHT\_PARENTHESIS}
\initials{LDF 2009.11.16.}

\LOG
\initials{LDF 2009.11.16.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=operation_assignment: sphere_development_variable PLUS_ASSIGN @>
@=LEFT_PARENTHESIS numeric_expression COMMA numeric_expression  @>   
@=RIGHT_PARENTHESIS                                             @>
{
  @<Common declarations for rules@>@; 

 #if DEBUG_COMPILE
   DEBUG = false;  /* |true|  */
   if (DEBUG)
     {

          cerr_strm << thread_name << "*** Parser: `operation_assignment --> "
                    << "sphere_development_variable PLUS_ASSIGN"
                    << endl 
                    << "LEFT_PARENTHESIS numeric_expression COMMA numeric_expression "
                    << "RIGHT_PARENTHESIS':"
                    << endl;

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str("");
     }
#endif /* |DEBUG_COMPILE|  */@; 

  real latitude  = @=$4@>;
  real longitude = @=$6@>;
  
  entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

@q ***** (5) Error handling:  |entry == 0|.@>   
@ Error handling:  |entry == 0|.
\initials{LDF 2009.11.16.}
@<Define rules@>=

   if (entry == static_cast<Id_Map_Entry_Node>(0))
      {
         cerr_strm << thread_name << "ERROR! In `yyparse()', rule "
                   << "`operation_assignment --> "
                   << "sphere_development_variable PLUS_ASSIGN"
                   << endl 
                   << "LEFT_PARENTHESIS numeric_expression COMMA numeric_expression "
                   << "RIGHT_PARENTHESIS':"
                   << endl << "`sphere_development_variable' is invalid, i.e., " 
                   << "`entry == 0'."
                   << endl;
         
         log_message(cerr_strm);
         cerr_message(cerr_strm, error_stop_value);
         cerr_strm.str("");
 
      } /* |if (entry == 0)|  */


@q ***** (5) Error handling:  |entry| has invalid type.@>   
@ Error handling:  |entry| has invalid type.
\initials{LDF 2009.11.16.}

\LOG
\initials{LDF 2009.11.16.}
Added this section.
\ENDLOG

@<Define rules@>=

   else if (entry->type != SPHERE_DEVELOPMENT)
   {
       cerr_strm << thread_name << "ERROR! In `yyparse()', rule "
                 << "`operation_assignment --> "
                 << "sphere_development_variable PLUS_ASSIGN"
                 << endl 
                 << "LEFT_PARENTHESIS numeric_expression COMMA numeric_expression "
                 << "RIGHT_PARENTHESIS':"
                 << endl << "`entry' has an invalid `type':  " 
                 << name_map[entry->type] << "."
                 << endl;
      
       log_message(cerr_strm);
       cerr_message(cerr_strm, error_stop_value);
       cerr_strm.str("");

   }  /* |else|  (|entry->type| is invalid.)  */

@q ***** (5) |entry != 0| and |entry->type| is valid.@>   

@ |entry != 0| and |entry->type| is valid.
\initials{LDF 2009.11.16..}

@<Define rules@>=

    else /* |entry != 0| and |entry->type| is valid.  */
    {

       Sphere_Development* q = static_cast<Sphere_Development*>(entry->object);

@q ****** (6).  |q == 0|.  Try to allocate memory for a object.@> 

@ |q == 0|.  Try to allocate memory for a new object.
\initials{LDF 2009.11.16.}

@<Define rules@>=

         if (q == static_cast<Sphere_Development*>(0))
         {

            try 
            {
               q = create_new<Sphere_Development>(0);     
                  
            }  /* |try|  */

            catch (bad_alloc)
            {
               
               cerr_strm << thread_name << "ERROR! In `yyparse()', rule "
                         << "`operation_assignment --> "
                         << "sphere_development_variable PLUS_ASSIGN"
                         << endl 
                         << "LEFT_PARENTHESIS numeric_expression COMMA numeric_expression "
                         << "RIGHT_PARENTHESIS':"
                         << endl << "Failed to create new object. " 
                         << "Rethrowing `bad_alloc'.";

               log_message(cerr_strm);
               cerr_message(cerr_strm, error_stop_value);
               cerr_strm.str("");
 
               throw;

            } /* |catch (bad_alloc)|  */


         } /* |if (q == 0)|  */


@q ****** (6).@>           

         q->map_point(latitude, longitude);

         entry->object = static_cast<void*>(q);

      }  /* |else| (|entry != 0|)  */
   
@q ***** (5)  Set |$$| to 0 and exit rule.@> 

@ Set |@=$$@>| to 0 and exit rule.
\initials{LDF 2009.11.16.}

@<Define rules@>=

  @=$$@> = static_cast<void*>(0);

};




@q *** (3) parabolae.@> 
@*2 {\bf parabolae}.
\initials{LDF 2005.11.18.}

\LOG
\initials{LDF 2005.11.18.}
Added this section.
\ENDLOG

@q **** (4) operation_assignment --> parabola_variable PLUS_ASSIGN CYCLE.@>

@*3 \§operation assignment> $\longrightarrow$ \§parabola variable> 
\.{PLUS\_ASSIGN} \.{CYCLE}.
\initials{LDF 2005.11.18.}


\LOG
\initials{LDF 2005.11.18.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=operation_assignment: parabola_variable PLUS_ASSIGN CYCLE@>@/
{

  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

@q ***** (5) Error handling:  |entry == 0|.@>   
@ Error handling:  |entry == 0|.
\initials{LDF 2005.11.18.}
@<Define rules@>=

   if (entry == static_cast<Id_Map_Entry_Node>(0))
      {

  
      } /* |if (entry == 0)|  */


@q ***** (5) |entry != 0|.@>   

    else /* |entry != 0|  */
      {

         Parabola* p = static_cast<Parabola*>(entry->object);

         if (p == static_cast<Parabola*>(0))
           {

                p = create_new<Parabola>(0);     

                entry->object = static_cast<void*>(p); 


           } /* |if (p == 0)|  */


         if (p != static_cast<Parabola*>(0))
            p->set_cycle(true);

      }  /* |else| (|entry != 0|)  */
   
@q ***** (5) Set |$$| to 0 and exit rule.@>   

@ Set |$$| to 0 and exit rule.
\initials{LDF 2005.11.18.}

@<Define rules@>=

  @=$$@> = static_cast<void*>(0);

};

@q **** (4) operation_assignment --> parabola_variable MINUS_ASSIGN CYCLE.@>

@*3 \§operation assignment> $\longrightarrow$ \§parabola variable> 
\.{MINUS\_ASSIGN} \.{CYCLE}.
\initials{LDF 2005.11.18.}


\LOG
\initials{LDF 2005.11.18.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=operation_assignment: parabola_variable MINUS_ASSIGN CYCLE@>@/
{

  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

@q ***** (5) Error handling:  |entry == 0|.@>   
@ Error handling:  |entry == 0|.
\initials{LDF 2005.11.18.}
@<Define rules@>=

   if (entry == static_cast<Id_Map_Entry_Node>(0))
      {

  
      } /* |if (entry == 0)|  */


@q ***** (5) |entry != 0|.@>   

    else /* |entry != 0|  */
      {

         Parabola* p = static_cast<Parabola*>(entry->object);

         if (p == static_cast<Parabola*>(0))
           {

                p = create_new<Parabola>(0);     

                entry->object = static_cast<void*>(p); 


           } /* |if (p == 0)|  */


         if (p != static_cast<Parabola*>(0))
            p->set_cycle(false);

      }  /* |else| (|entry != 0|)  */
   
@q ***** (5) Set |$$| to 0 and exit rule.@>   

@ Set |$$| to 0 and exit rule.
\initials{LDF 2005.11.18.}

@<Define rules@>=

  @=$$@> = static_cast<void*>(0);

};


@q *** (3) |rectangles|.@>  

@*2 {\bf rectangles}.
\initials{LDF 2005.01.25.}

\LOG
\initials{LDF 2005.01.25.}
Added this section.
\ENDLOG

@q **** (4) operation_assignment --> rectangle_variable @>
@q **** (4) PLUS_ASSIGN point_expression.               @>

@*3 \§operation assignment> $\longrightarrow$ \§rectangle variable> 
\.{PLUS\_ASSIGN} \§point expression>.
\initials{LDF 2005.01.25.}

\LOG
\initials{LDF 2005.01.25.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=operation_assignment: rectangle_variable PLUS_ASSIGN point_expression@>@/
{
  @<Common declarations for rules@>@; 

 #if DEBUG_COMPILE
   DEBUG = false;  /* |true|  */
   if (DEBUG)
     {

          cerr_strm << thread_name << "*** Parser: `operation_assignment --> "
                    << "rectangle_variable PLUS_ASSIGN point_expression'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str("");
     }
#endif /* |DEBUG_COMPILE|  */@; 

  Point* p = static_cast<Point*>(@=$3@>); 
  
  entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

@q ***** (5) Error handling:  |entry == 0|.@>   

@ Error handling:  |entry == 0|.
\initials{LDF 2005.01.25.}

@<Define rules@>=

   if (entry == static_cast<Id_Map_Entry_Node>(0))
      {
         cerr_strm << thread_name << "ERROR! In `yyparse()', rule "
                   << "`operation_assignment --> "
                   << endl 
                   << "rectangle_variable PLUS_ASSIGN point_expression':"
                   << endl << "`rectangle_variable' is invalid, i.e., " 
                   << "`entry == 0'."
                   << endl << "Deleting `point_expression'.";
         
         log_message(cerr_strm);
         cerr_message(cerr_strm, error_stop_value);
         cerr_strm.str("");
 
         delete p;

      } /* |if (entry == 0)|  */


@q ***** (5) |entry != 0|.@>   


    else /* |entry != 0|  */
      {


         Rectangle* q = static_cast<Rectangle*>(entry->object);

@q ****** (6).  |q == 0|.  Try to allocate memory for a new |Rectangle|.@> 

@ |q == 0|.  Try to allocate memory for a new |Rectangle|.
\initials{LDF 2005.01.25.}
  
@<Define rules@>=

         if (q == static_cast<Rectangle*>(0))
           {

              try 
                {
                   q = create_new<Rectangle>(0);     
                }

              catch (bad_alloc)
                {
                   
                   cerr_strm << thread_name << "ERROR! In `yyparse()', rule "
                             << "`operation_assignment --> "
                             << endl 
                             << "rectangle_variable PLUS_ASSIGN point_expression':"
                             << endl << "Failed to create new `rectangle'. " 
                             << "Deleting `point_expression' and rethrowing "
                             << "`bad_alloc'.";

                   log_message(cerr_strm);
                   cerr_message(cerr_strm, error_stop_value);
                   cerr_strm.str("");
 
                   delete p;
 
                   throw;

                } /* |catch (bad_alloc)|  */


           } /* |if (q == 0)|  */


@q ****** (6).@>           


         *q += *p;

         entry->object = static_cast<void*>(q);


         delete p;

      }  /* |else| (|entry != 0|)  */
   
@q ***** (5)  Set |$$| to 0 and exit rule.@> 

@ Set |@=$$@>| to 0 and exit rule.
\initials{LDF 2005.01.25.}

\LOG
\initials{LDF 2004.11.05.}
Now setting |@=$$@>| to 0.  Formerly, it was set to |entry->object|
upon success and 0 upon failure.
\ENDLOG 

@<Define rules@>=

  @=$$@> = static_cast<void*>(0);

};


@q *** (3) |pictures|.@>  

@*2 {\bf pictures}.
\initials{LDF 2005.05.12.}

\LOG
\initials{LDF 2005.05.12.}
Added this section.
\ENDLOG

@q **** (4) operation_assignment --> picture_variable   @>
@q **** (4) PLUS_ASSIGN picture_expression.             @>

@*3 \§operation assignment> $\longrightarrow$ \§picture variable> 
\.{PLUS\_ASSIGN} \§picture expression>.
\initials{LDF 2005.05.12.}

\LOG
\initials{LDF 2005.05.12.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=operation_assignment: picture_variable PLUS_ASSIGN picture_expression@>@/
{
   
   operation_assignment_picture_rule_func_0(
                                         @=$1@>,
                                         @=$3@>,
                                         static_cast<Scanner_Node>(parameter));

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) Vector-types.@>  

@*1 Vector-types.
\initials{LDF 2004.12.04.}

\LOG
\initials{LDF 2004.12.04.}
Added this section.
\ENDLOG

@q *** (3) |boolean_vectors|.@>  

@*2 {\bf boolean\_vectors}.
\initials{LDF 2005.01.07.}

\LOG
\initials{LDF 2005.01.07.}
Added this section.
\ENDLOG

@q *** (3) operation_assignment --> boolean_vector_variable  @>
@q *** (3) PLUS_ASSIGN boolean_expression.                   @>

@*3 \§operation assignment> $\longrightarrow$ \§boolean vector variable>
\.{PLUS\_ASSIGN} \§boolean expression>.

\LOG
\initials{LDF 2005.01.07.}
Added this rule.
\ENDLOG 

@q **** (4) Definition.@>   

@<Define rules@>=
 
@=operation_assignment: boolean_vector_variable PLUS_ASSIGN boolean_expression@>
{

   @<Common declarations for rules@>@;
  
#if DEBUG_COMPILE
   DEBUG = false;  /* |true|  */
   
   if (DEBUG)
      {
        cerr_strm << thread_name 
                  << "*** Parser: `operation_assignment --> "
                  << "boolean_vector_variable PLUS_ASSIGN boolean_expression'.";
        
        log_message(cerr_strm);
        cerr_message(cerr_strm);
        cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */@;

@q ***** (5) Call |Scan_Parse::vector_type_plus_assign()|.@>   
@ Call |Scan_Parse::vector_type_plus_assign()|.
\initials{LDF 2005.01.07.}

@<Define rules@>=

  status
    = vector_type_plus_assign<bool>(scanner_node,
                                     static_cast<Id_Map_Entry_Node>(@=$1@>), 
                                     BOOLEAN_VECTOR,
                                     BOOLEAN,
                                     static_cast<bool*>(@=$3@>));



@q ***** (5) |Scan_Parse::vector_type_plus_assign()| failed.@> 
@ |Scan_Parse::vector_type_plus_assign()| failed.
\initials{LDF 2005.01.07.}

@<Define rules@>=

  if (status != 0)
    {
      cerr_strm << thread_name 
                << "ERROR! In parser rule `operation_assignment --> "
                << endl 
                << "boolean_vector_variable PLUS_ASSIGN boolean_expression':"
                << endl << "`Scan_Parse::vector_type_plus_assign()' "
                << "failed.  Didn't add `boolean' to `boolean_vector'.";

      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");

    }  /* |if (status != 0)|  */
 
                             
@q ***** (5) |Scan_Parse::vector_type_plus_assign()| succeeded.@> 
@ |Scan_Parse::vector_type_plus_assign()| succeeded.
\initials{LDF 2005.01.07.}

@<Define rules@>=
                                           

  else /* |status == 0|  */
    {

#if DEBUG_COMPILE
      if (DEBUG)
        {
          cerr_strm << thread_name 
                    << "In parser rule `operation_assignment --> "
                    << endl 
                    << "boolean_vector_variable PLUS_ASSIGN boolean_expression':"
                    << endl << "`Scan_Parse::vector_type_plus_assign()' "
                    << "succeeded.";

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str("");
        }
#endif /* |DEBUG_COMPILE|  */@; 
 

    } /* |else| (|status == 0|)  */ 

@q ***** (5) Set |$$| to 0 and exit rule.@>   

@ Set |@=$$@>| to 0 and exit rule.
\initials{LDF 2005.01.07.}

@<Define rules@>=

  @=$$@> = static_cast<void*>(0);

};




@q *** (3) |numeric_vectors|.@>  

@*2 {\bf numeric\_vectors}.
\initials{LDF 2005.01.06.}

\LOG
\initials{LDF 2005.01.06.}
Added this section.
\ENDLOG

@q *** (3) operation_assignment --> numeric_vector_variable  @>
@q *** (3) PLUS_ASSIGN numeric_expression.                   @>

@*3 \§operation assignment> $\longrightarrow$ \§numeric vector variable>
\.{PLUS\_ASSIGN} \§numeric expression>.
\initials{LDF 2005.01.06.}

\LOG
\initials{LDF 2005.01.06.}
Added this rule.
\ENDLOG 

@q **** (4) Definition.@>   

@<Define rules@>=
 
@=operation_assignment: numeric_vector_variable PLUS_ASSIGN @>
@=numeric_expression@>@/ 
{

   @<Common declarations for rules@>@;
  
#if DEBUG_COMPILE
   DEBUG = false;  /* |true|  */
   
   if (DEBUG)
      {
        cerr_strm << thread_name 
                  << "*** Parser: `operation_assignment --> "
                  << "numeric_vector_variable PLUS_ASSIGN numeric_expression'.";
        
        log_message(cerr_strm);
        cerr_message(cerr_strm);
        cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */@;

@q ***** (5) Call |Scan_Parse::vector_type_plus_assign()|.@>   
@ Call |Scan_Parse::vector_type_plus_assign()|.
\initials{LDF 2005.01.06.}

@<Define rules@>=

  
   real* r;

   try 
      {
          r = new real;
      }

@q ***** (5).@> 

   catch (bad_alloc)
      {
          cerr_strm << thread_name << "ERROR!  In `yyparse()', rule "
                    << "`operation_assignment -->"
                    << endl 
                    << "numeric_vector_variable PLUS_ASSIGN numeric_expression':"
                    << endl 
                    << "`new real' failed.  Rethrowing `bad_alloc'.";

          log_message(cerr_strm);
          cerr_message(cerr_strm, error_stop_value);
          cerr_strm.str("");

      }  /* |catch (bad_alloc)|  */


@q ***** (5).@> 

  *r = @=$3@>;

  status
    = vector_type_plus_assign<real>(scanner_node,
                                     static_cast<Id_Map_Entry_Node>(@=$1@>), 
                                     NUMERIC_VECTOR,
                                     NUMERIC,
                                     r);



@q ***** (5) |Scan_Parse::vector_type_plus_assign()| failed.@> 
@ |Scan_Parse::vector_type_plus_assign()| failed.
\initials{LDF 2005.01.06.}

@<Define rules@>=

  if (status != 0)
    {
      cerr_strm << thread_name 
                << "ERROR! In parser rule `operation_assignment --> "
                << endl 
                << "numeric_vector_variable PLUS_ASSIGN numeric_expression':"
                << endl << "`Scan_Parse::vector_type_plus_assign()' "
                << "failed.  Didn't add `numeric' to `numeric_vector'.";

      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");

    }  /* |if (status != 0)|  */
 
                             
@q ***** (5) |Scan_Parse::vector_type_plus_assign()| succeeded.@> 
@ |Scan_Parse::vector_type_plus_assign()| succeeded.
\initials{LDF 2005.01.06.}

@<Define rules@>=
                                           

  else /* |status == 0|  */
    {

#if DEBUG_COMPILE
      if (DEBUG)
        {
          cerr_strm << thread_name 
                    << "In parser rule `operation_assignment --> "
                    << endl 
                    << "numeric_vector_variable PLUS_ASSIGN numeric_expression':"
                    << endl << "`Scan_Parse::vector_type_plus_assign()' "
                    << "succeeded.";

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str("");
        }
#endif /* |DEBUG_COMPILE|  */@; 
 

    } /* |else| (|status == 0|)  */ 

@q ***** (5) Set |$$| to 0 and exit rule.@>   

@ Set |@=$$@>| to 0 and exit rule.
\initials{LDF 2005.01.06.}

@<Define rules@>=

  @=$$@> = static_cast<void*>(0);

};

@q *** (3) |string_vectors|.@>  

@*2 {\bf string\_vectors}.
\initials{LDF 2005.01.09.}

\LOG
\initials{LDF 2005.01.09.}
Added this section.
\ENDLOG

@q *** (3) operation_assignment --> string_vector_variable  @>
@q *** (3) PLUS_ASSIGN string_expression.                   @>

@*3 \§operation assignment> $\longrightarrow$ \§string vector variable>
\.{PLUS\_ASSIGN} \§string expression>.
\initials{LDF 2005.01.09.}

\LOG
\initials{LDF 2005.01.09.}
Added this rule.
\ENDLOG 

@q **** (4) Definition.@>   

@<Define rules@>=
 
@=operation_assignment: string_vector_variable PLUS_ASSIGN string_expression@>
{

   @<Common declarations for rules@>@;
  
#if DEBUG_COMPILE
   DEBUG = false;  /* |true|  */
   
   if (DEBUG)
      {
        cerr_strm << thread_name 
                  << "*** Parser: `operation_assignment --> "
                  << "string_vector_variable PLUS_ASSIGN string_expression'.";
        
        log_message(cerr_strm);
        cerr_message(cerr_strm);
        cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */@;

@q ***** (5) Call |Scan_Parse::vector_type_plus_assign()|.@>   
@ Call |Scan_Parse::vector_type_plus_assign()|.
\initials{LDF 2005.01.09.}

@<Define rules@>=

  status
    = vector_type_plus_assign<string>(scanner_node,
                                      static_cast<Id_Map_Entry_Node>(@=$1@>), 
                                      STRING_VECTOR,
                                      STRING,
                                      static_cast<string*>(@=$3@>));



@q ***** (5) |Scan_Parse::vector_type_plus_assign()| failed.@> 
@ |Scan_Parse::vector_type_plus_assign()| failed.
\initials{LDF 2005.01.09.}

@<Define rules@>=

  if (status != 0)
    {
      cerr_strm << thread_name 
                << "ERROR! In parser rule `operation_assignment --> "
                << endl 
                << "string_vector_variable PLUS_ASSIGN string_expression':"
                << endl << "`Scan_Parse::vector_type_plus_assign()' "
                << "failed.  Didn't add `string' to `string_vector'.";

      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");

    }  /* |if (status != 0)|  */
 
                             
@q ***** (5) |Scan_Parse::vector_type_plus_assign()| succeeded.@> 
@ |Scan_Parse::vector_type_plus_assign()| succeeded.
\initials{LDF 2005.01.09.}

@<Define rules@>=
                                           

  else /* |status == 0|  */
    {

#if DEBUG_COMPILE
      if (DEBUG)
        {
          cerr_strm << thread_name 
                    << "In parser rule `operation_assignment --> "
                    << endl 
                    << "string_vector_variable PLUS_ASSIGN string_expression':"
                    << endl << "`Scan_Parse::vector_type_plus_assign()' "
                    << "succeeded.";

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str("");
        }
#endif /* |DEBUG_COMPILE|  */@; 
 

    } /* |else| (|status == 0|)  */ 

@q ***** (5) Set |$$| to 0 and exit rule.@>   

@ Set |@=$$@>| to 0 and exit rule.
\initials{LDF 2004.11.05.}

\LOG
\initials{LDF 2004.11.05.}
Now setting |@=$$@>| to 0.  Formerly, it was set to |@=$3@>|. 
\ENDLOG 

@<Define rules@>=

  @=$$@> = static_cast<void*>(0);

};

@q *** (3) |pen_vectors|.@>  

@*2 {\bf pen\_vectors}.
\initials{LDF 2005.01.11.}

\LOG
\initials{LDF 2005.01.11.}
Added this section.
\ENDLOG

@q *** (3) operation_assignment --> pen_vector_variable  @>
@q *** (3) PLUS_ASSIGN pen_expression.                   @>

@*3 \§operation assignment> $\longrightarrow$ \§pen vector variable>
\.{PLUS\_ASSIGN} \§pen expression>.

\LOG
\initials{LDF 2005.01.11.}
Added this rule.
\ENDLOG 

@q **** (4) Definition.@>   

@<Define rules@>=
 
@=operation_assignment: pen_vector_variable PLUS_ASSIGN pen_expression@>
{

   @<Common declarations for rules@>@;
  
#if DEBUG_COMPILE
   DEBUG = false;  /* |true|  */
   
   if (DEBUG)
      {
        cerr_strm << thread_name 
                  << "*** Parser: `operation_assignment --> "
                  << "pen_vector_variable PLUS_ASSIGN pen_expression'.";
        
        log_message(cerr_strm);
        cerr_message(cerr_strm);
        cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */@;

@q ***** (5) Call |Scan_Parse::vector_type_plus_assign()|.@>   
@ Call |Scan_Parse::vector_type_plus_assign()|.
\initials{LDF 2005.01.11.}

@<Define rules@>=

  status
    = vector_type_plus_assign<Pen>(scanner_node,
                                   static_cast<Id_Map_Entry_Node>(@=$1@>), 
                                   PEN_VECTOR,
                                   PEN,
                                   static_cast<Pen*>(@=$3@>));



@q ***** (5) |Scan_Parse::vector_type_plus_assign()| failed.@> 
@ |Scan_Parse::vector_type_plus_assign()| failed.
\initials{LDF 2005.01.11.}

@<Define rules@>=

  if (status != 0)
    {
      cerr_strm << thread_name 
                << "ERROR! In parser rule `operation_assignment --> "
                << endl 
                << "pen_vector_variable PLUS_ASSIGN pen_expression':"
                << endl << "`Scan_Parse::vector_type_plus_assign()' "
                << "failed.  Didn't add `pen' to `pen_vector'.";

      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");

    }  /* |if (status != 0)|  */
 
                             
@q ***** (5) |Scan_Parse::vector_type_plus_assign()| succeeded.@> 
@ |Scan_Parse::vector_type_plus_assign()| succeeded.
\initials{LDF 2005.01.11.}

@<Define rules@>=
                                           

  else /* |status == 0|  */
    {

#if DEBUG_COMPILE
      if (DEBUG)
        {
          cerr_strm << thread_name 
                    << "In parser rule `operation_assignment --> "
                    << endl 
                    << "pen_vector_variable PLUS_ASSIGN pen_expression':"
                    << endl << "`Scan_Parse::vector_type_plus_assign()' "
                    << "succeeded.";

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str("");
        }
#endif /* |DEBUG_COMPILE|  */@; 
 

    } /* |else| (|status == 0|)  */ 

@q ***** (5) Set |$$| to 0 and exit rule.@>   

@ Set |@=$$@>| to 0 and exit rule.
\initials{LDF 2005.01.11.}

@<Define rules@>=

  @=$$@> = static_cast<void*>(0);

};

@q *** (3) |dash_pattern_vectors|.@>  

@*2 \§dash pattern vectors>.
\initials{LDF 2005.01.13.}

\LOG
\initials{LDF 2005.01.13.}
Added this section.
\ENDLOG

@q *** (3) operation_assignment --> dash_pattern_vector_variable  @>
@q *** (3) PLUS_ASSIGN dash_pattern_expression.                   @>

@*3 \§operation assignment> $\longrightarrow$ \§dash pattern vector variable>
\.{PLUS\_ASSIGN} \§dash pattern expression>.

\LOG
\initials{LDF 2005.01.13.}
Added this rule.
\ENDLOG 

@q **** (4) Definition.@>   

@<Define rules@>=
 
@=operation_assignment: dash_pattern_vector_variable @>
@=PLUS_ASSIGN dash_pattern_expression@>@/
{

   @<Common declarations for rules@>@;
  
#if DEBUG_COMPILE
   DEBUG = false;  /* |true|  */
   
   if (DEBUG)
      {
        cerr_strm << thread_name 
                  << "*** Parser: `operation_assignment --> "
                  << "dash_pattern_vector_variable PLUS_ASSIGN "
                  << "dash_pattern_expression'.";
        
        log_message(cerr_strm);
        cerr_message(cerr_strm);
        cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */@;

@q ***** (5) Call |Scan_Parse::vector_type_plus_assign()|.@>   
@ Call |Scan_Parse::vector_type_plus_assign()|.
\initials{LDF 2005.01.13.}

@<Define rules@>=

  status
    = vector_type_plus_assign<Dash_Pattern>(scanner_node,
                                   static_cast<Id_Map_Entry_Node>(@=$1@>), 
                                   DASH_PATTERN_VECTOR,
                                   DASH_PATTERN,
                                   static_cast<Dash_Pattern*>(@=$3@>));



@q ***** (5) |Scan_Parse::vector_type_plus_assign()| failed.@> 
@ |Scan_Parse::vector_type_plus_assign()| failed.
\initials{LDF 2005.01.13.}

@<Define rules@>=

  if (status != 0)
    {
      cerr_strm << thread_name 
                << "ERROR! In parser rule `operation_assignment --> "
                << endl 
                << "dash_pattern_vector_variable PLUS_ASSIGN dash_pattern_expression':"
                << endl << "`Scan_Parse::vector_type_plus_assign()' "
                << "failed.  "
                << "Didn't add `dash_pattern' to `dash_pattern_vector'.";

      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");

    }  /* |if (status != 0)|  */
 
                             
@q ***** (5) |Scan_Parse::vector_type_plus_assign()| succeeded.@> 
@ |Scan_Parse::vector_type_plus_assign()| succeeded.
\initials{LDF 2005.01.13.}

@<Define rules@>=
                                           

  else /* |status == 0|  */
    {

#if DEBUG_COMPILE
      if (DEBUG)
        {
          cerr_strm << thread_name 
                    << "In parser rule `operation_assignment --> "
                    << endl 
                    << "dash_pattern_vector_variable PLUS_ASSIGN "
                    << "dash_pattern_expression':"
                    << endl << "`Scan_Parse::vector_type_plus_assign()' "
                    << "succeeded.";

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str("");
        }
#endif /* |DEBUG_COMPILE|  */@; 
 

    } /* |else| (|status == 0|)  */ 

@q ***** (5) Set |$$| to 0 and exit rule.@>   

@ Set |@=$$@>| to 0 and exit rule.
\initials{LDF 2005.01.13.}

@<Define rules@>=

  @=$$@> = static_cast<void*>(0);

};

@q *** (3) |color_vectors|.@>  

@*2 {\bf color\_vectors}.
\initials{LDF 2004.12.04.}

\LOG
\initials{LDF 2004.12.04.}
Added this section.
\ENDLOG

@q *** (3) operation_assignment --> color_vector_variable  @>
@q *** (3) PLUS_ASSIGN color_expression.                   @>

@*3 \§operation assignment> $\longrightarrow$ \§color vector variable>
\.{PLUS\_ASSIGN} \§color expression>.

\LOG
\initials{LDF 2004.08.23.}
Added this rule.

\initials{LDF 2004.08.26.}
Got this rule to work.

\initials{LDF 2004.09.01.}
Removed most of the code from this rule to 
|Scan_Parse::vector_type_plus_assign()|, which is now called in this
rule.  

\initials{LDF 2004.11.05.}
Changed the symbol on the left-hand side of this rule from 
|color_vector_assignment| to |operation_assignment|.

\initials{LDF 2004.12.04.}
Moved this rule from \filename{passign.w} to this file 
(\filename{popassgn.w}). 
\ENDLOG 

@q **** (4) Definition.@>   

@<Define rules@>=
 
@=operation_assignment: color_vector_variable PLUS_ASSIGN color_expression@>
{

   @<Common declarations for rules@>@;
  
#if DEBUG_COMPILE
   DEBUG = false;  /* |true|  */
   
   if (DEBUG)
      {
        cerr_strm << thread_name 
                  << "*** Parser: `operation_assignment --> "
                  << "color_vector_variable PLUS_ASSIGN color_expression'.";
        
        log_message(cerr_strm);
        cerr_message(cerr_strm);
        cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */@;

@q ***** (5) Call |Scan_Parse::vector_type_plus_assign()|.@>   
@ Call |Scan_Parse::vector_type_plus_assign()|.
\initials{LDF 2004.08.31.}

@<Define rules@>=

  status
    = vector_type_plus_assign<Color>(scanner_node,
                                     static_cast<Id_Map_Entry_Node>(@=$1@>), 
                                     COLOR_VECTOR,
                                     COLOR,
                                     static_cast<Color*>(@=$3@>));



@q ***** (5) |Scan_Parse::vector_type_plus_assign()| failed.@> 
@ |Scan_Parse::vector_type_plus_assign()| failed.
\initials{LDF 2004.08.31.}

@<Define rules@>=

  if (status != 0)
    {
      cerr_strm << thread_name 
                << "ERROR! In parser rule `operation_assignment --> "
                << endl 
                << "color_vector_variable PLUS_ASSIGN color_expression':"
                << endl << "`Scan_Parse::vector_type_plus_assign()' "
                << "failed.  Didn't add `color' to `color_vector'.";

      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");

    }  /* |if (status != 0)|  */
 
                             
@q ***** (5) |Scan_Parse::vector_type_plus_assign()| succeeded.@> 
@ |Scan_Parse::vector_type_plus_assign()| succeeded.
\initials{LDF 2004.08.31.}

@<Define rules@>=
                                           

  else /* |status == 0|  */
    {

#if DEBUG_COMPILE
      if (DEBUG)
        {
          cerr_strm << thread_name 
                    << "In parser rule `operation_assignment --> "
                    << endl 
                    << "color_vector_variable PLUS_ASSIGN color_expression':"
                    << endl << "`Scan_Parse::vector_type_plus_assign()' "
                    << "succeeded.";

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str("");
        }
#endif /* |DEBUG_COMPILE|  */@; 
 

    } /* |else| (|status == 0|)  */ 

@q ***** (5) Set |$$| to 0 and exit rule.@>   

@ Set |@=$$@>| to 0 and exit rule.
\initials{LDF 2004.11.05.}

\LOG
\initials{LDF 2004.11.05.}
Now setting |@=$$@>| to 0.  Formerly, it was set to |@=$3@>|. 
\ENDLOG 

@<Define rules@>=

  @=$$@> = static_cast<void*>(0);

};

@q *** (3) |transform_vectors|.@>  

@*2 \§transform vectors>.
\initials{LDF 2005.01.13.}

\LOG
\initials{LDF 2005.01.13.}
Added this section.
\ENDLOG

@q *** (3) operation_assignment --> transform_vector_variable  @>
@q *** (3) PLUS_ASSIGN transform_expression.                   @>

@*3 \§operation assignment> $\longrightarrow$ \§transform vector variable>
\.{PLUS\_ASSIGN} \§transform expression>.

\LOG
\initials{LDF 2005.01.13.}
Added this rule.
\ENDLOG 

@q **** (4) Definition.@>   

@<Define rules@>=
 
@=operation_assignment: transform_vector_variable @>
@=PLUS_ASSIGN transform_expression@>@/
{

   @<Common declarations for rules@>@;
  
#if DEBUG_COMPILE
   DEBUG = false;  /* |true|  */
   
   if (DEBUG)
      {
        cerr_strm << thread_name 
                  << "*** Parser: `operation_assignment --> "
                  << "transform_vector_variable PLUS_ASSIGN "
                  << "transform_expression'.";
        
        log_message(cerr_strm);
        cerr_message(cerr_strm);
        cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */@;

@q ***** (5) Call |Scan_Parse::vector_type_plus_assign()|.@>   
@ Call |Scan_Parse::vector_type_plus_assign()|.
\initials{LDF 2005.01.13.}

@<Define rules@>=

  status
    = vector_type_plus_assign<Transform>(scanner_node,
                                   static_cast<Id_Map_Entry_Node>(@=$1@>), 
                                   TRANSFORM_VECTOR,
                                   TRANSFORM,
                                   static_cast<Transform*>(@=$3@>));



@q ***** (5) |Scan_Parse::vector_type_plus_assign()| failed.@> 
@ |Scan_Parse::vector_type_plus_assign()| failed.
\initials{LDF 2005.01.13.}

@<Define rules@>=

  if (status != 0)
    {
      cerr_strm << thread_name 
                << "ERROR! In parser rule `operation_assignment --> "
                << endl 
                << "transform_vector_variable PLUS_ASSIGN transform_expression':"
                << endl << "`Scan_Parse::vector_type_plus_assign()' "
                << "failed.  "
                << "Didn't add `transform' to `transform_vector'.";

      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");

    }  /* |if (status != 0)|  */
 
                             
@q ***** (5) |Scan_Parse::vector_type_plus_assign()| succeeded.@> 
@ |Scan_Parse::vector_type_plus_assign()| succeeded.
\initials{LDF 2005.01.13.}

@<Define rules@>=
                                           

  else /* |status == 0|  */
    {

#if DEBUG_COMPILE
      if (DEBUG)
        {
          cerr_strm << thread_name 
                    << "In parser rule `operation_assignment --> "
                    << endl 
                    << "transform_vector_variable PLUS_ASSIGN "
                    << "transform_expression':"
                    << endl << "`Scan_Parse::vector_type_plus_assign()' "
                    << "succeeded.";

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str("");
        }
#endif /* |DEBUG_COMPILE|  */@; 
 

    } /* |else| (|status == 0|)  */ 

@q ***** (5) Set |$$| to 0 and exit rule.@>   

@ Set |@=$$@>| to 0 and exit rule.
\initials{LDF 2005.01.13.}

@<Define rules@>=

  @=$$@> = static_cast<void*>(0);

};

@q *** (3) |focus_vectors|.@> 
@* \§focus vectors>.
\initials{LDF 2005.01.17.}

\LOG
\initials{LDF 2005.01.17.}
Added this section.
\ENDLOG

@q **** (4) @>   
@

\LOG
\initials{LDF 2005.01.18.}
Added this rule.
\ENDLOG 

@q ***** (5) Definition.@> 

@<Define rules@>=

@=operation_assignment: focus_vector_variable@>@/
@=PLUS_ASSIGN WITH_POSITION point_expression@>@/
@=WITH_DIRECTION point_expression WITH_DISTANCE@>@/
@=numeric_expression with_angle_optional with_axis_optional@>@/
{

   @<Common declarations for rules@>@;
  
#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
       cerr_strm << thread_name << "*** Parser:  `operation_assignment -->  "
                 << "focus_vector_variable PLUS_ASSIGN "
                 << "WITH_POSITION point_expression"
                 << endl 
                 << "WITH_DIRECTION point_expression "
                 << "WITH_DISTANCE numeric_expression"
                 << endl 
                 << "with_angle_optional with_axis_optional'.";


       log_message(cerr_strm);       
       cerr_message(cerr_strm); 
       cerr_strm.str(""); 
    }
#endif /* |DEBUG_COMPILE|  */@;
   
   entry            = static_cast<Id_Map_Entry_Node>(@=$1@>);
   Point* position  = static_cast<Point*>(@=$4@>); 
   Point* direction = static_cast<Point*>(@=$6@>); 
   real distance    = @=$8@>;
   real angle       = @=$9@>;
   real axis        = @=$10@>;  

@q ****** (6) Error handling:  |entry == 0|.@> 
@ Error handling:  |entry == 0|.
\initials{LDF 2005.01.18.}

@<Define rules@>=

   if (entry == static_cast<Id_Map_Entry_Node>(0))
      {
          cerr_strm << thread_name << "ERROR!  In `yyparse()', rule "
                    << "`operation_assignment -->  "
                    << "focus_vector_variable PLUS_ASSIGN "
                    << "WITH_POSITION point_expression"
                    << endl 
                    << "WITH_DIRECTION point_expression "
                    << "WITH_DISTANCE numeric_expression"
                    << endl 
                    << "with_angle_optional with_axis_optional':"
                    << endl << "`focus_vector_variable' is invalid.  "
                    << "Not assigning, deleting the `point_expressions', "
                    << "and will try to continue.";

          log_message(cerr_strm);
          cerr_message(cerr_strm, error_stop_value);
          cerr_strm.str("");
         
          delete position;
          delete direction; 

          @=$$@> = static_cast<void*>(0);
          
      }  /* |if (entry == 0)|  */
 
@q ****** (6) |entry != 0|.@> 
@ |entry != 0|.
\initials{LDF 2005.01.18.}

@<Define rules@>=

   else /* |entry != 0|  */
      {

         Focus* f;


         try 
            {
               f = new Focus;
            }

@q ******* (7) @> 

         catch (bad_alloc) 
            {
                cerr_strm << thread_name << "ERROR!  "
                          << "In `yyparse()', rule "
                          << "`operation_assignment -->  "
                          << "focus_vector_variable PLUS_ASSIGN "
                          << "WITH_POSITION point_expression"
                          << endl 
                          << "WITH_DIRECTION point_expression "
                          << "WITH_DISTANCE numeric_expression"
                          << endl 
                          << "with_angle_optional with_axis_optional':"
                          << endl 
                          << "`new Focus' failed.  "
                          << "Deleting the `point_expressions' and "
                          << "rethrowing `bad_alloc'.";

                log_message(cerr_strm);
                cerr_message(cerr_strm, error_stop_value);
                cerr_strm.str("");
         
                delete position;
                delete direction; 

                throw;
                      
            }  /* |catch (bad_alloc)|  */

@q ******* (7) @> 

         char axis_char;

         if (axis == Z_AXIS)
            axis_char = 'z';
         else if (axis == X_AXIS)
            axis_char = 'x';
         else if (axis == Y_AXIS)
            axis_char = 'y';

@q ******** (8) Error handling:  |axis| has invalid value.@>  
@  Error handling:  |axis| has invalid value.
\initials{LDF 2005.01.18.}

@<Define rules@>=

  else /* |axis| has invalid value.  */
     {
         cerr_strm << thread_name << "ERROR!  "
                   << "In `yyparse()', rule "
                   << "`operation_assignment -->  "
                   << "focus_vector_variable PLUS_ASSIGN "
                   << "WITH_POSITION point_expression"
                   << endl 
                   << "WITH_DIRECTION point_expression "
                   << "WITH_DISTANCE numeric_expression"
                   << endl 
                   << "with_angle_optional with_axis_optional':"
                   << endl 
                   << "`with_axis_optional' has invalid value.  "
                   << "Setting `axis_char' to 'z'.";


         log_message(cerr_strm); 
         cerr_message(cerr_strm, error_stop_value); 
         cerr_strm.str(""); 
       
         axis_char = 'z';

     }  /* |else|  (|axis| has invalid value.)  */

@q ******** (8) @> 

         f->set(*position, *direction, distance, angle, axis_char);

         status = vector_type_plus_assign(scanner_node,
                                          entry,
                                          FOCUS_VECTOR,
                                          FOCUS,
                                          f,
                                          false);

@q ******* (7) @> 

         if (status != 0)
            {

                cerr_strm << thread_name << "ERROR!  "
                          << "In `yyparse()', rule "
                          << "`operation_assignment -->  "
                          << "focus_vector_variable PLUS_ASSIGN "
                          << "WITH_POSITION point_expression"
                          << endl 
                          << "WITH_DIRECTION point_expression "
                          << "WITH_DISTANCE numeric_expression"
                          << endl 
                          << "with_angle_optional with_axis_optional':"
                          << endl 
                          << "`Scan_Parse::vector_type_plus_assign()' failed.  "
                          << "Deleting the `point_expressions' and "
                          << "will try to continue.";

                log_message(cerr_strm);
                cerr_message(cerr_strm, error_stop_value);
                cerr_strm.str("");
         
                delete position;
                delete direction; 

               @=$$@> = static_cast<void*>(0);
 
            }  /* |if (status != 0)|  */

@q ******* (7) @>

        else /* |status != 0|  */
           {
                delete position;
                delete direction; 
 
                @=$$@> = static_cast<void*>(0);

           }  /* |else| (|status != 0|)  */


@q ******* (7) @>

      }  /* |else| (|entry != 0|)  */

@q ****** (6).@> 

};

@q *** (3) |Shape| types.@> 
@*2 {\bf Shape} types.
\initials{LDF 2005.01.17.}

@q **** (4) |point_vectors|.@>  

@*3 \§point vectors>.
\initials{LDF 2004.12.04.}

\LOG
\initials{LDF 2004.12.04.}
Added this section.
\ENDLOG

@q *** (3) operation_assignment --> point_vector_variable  @>
@q *** (3) PLUS_ASSIGN point_expression.                   @>

@*2 \§operation assignment> $\longrightarrow$ \§point vector variable>
\.{PLUS\_ASSIGN} \§point expression>.

\LOG
\initials{LDF 2004.09.01.}
Added this rule.

\initials{LDF 2004.11.05.}
Changed the symbol on the left-hand side of this rule from 
|point_vector_assignment| to |operation_assignment|.

\initials{LDF 2004.12.04.}
Moved this rule from \filename{passign.w} to this file 
(\filename{popassgn.w}). 
\ENDLOG 

@q **** (4) Definition.@> 

@<Define rules@>=
 
@=operation_assignment: point_vector_variable PLUS_ASSIGN point_expression@>
{

@q ***** (5).@>   

   @<Common declarations for rules@>@;
  
#if DEBUG_COMPILE
   DEBUG = false;  /* |true|  */
   
   if (DEBUG)
      {
        cerr_strm << thread_name 
                  << "*** Parser: `operation_assignment --> "
                  << "point_vector_variable PLUS_ASSIGN point_expression'.";
        
        log_message(cerr_strm);
        cerr_message(cerr_strm);
        cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */@;

@q ***** (5) Call |Scan_Parse::vector_type_plus_assign()|.@>   
@ Call |Scan_Parse::vector_type_plus_assign()|.
\initials{LDF 2004.08.31.}

@<Define rules@>=

  status
    = vector_type_plus_assign<Point>(scanner_node,
                                     static_cast<Id_Map_Entry_Node>(@=$1@>), 
                                     POINT_VECTOR,
                                     POINT,
                                     static_cast<Point*>(@=$3@>));



@q ****** (6) |Scan_Parse::vector_type_plus_assign()| failed.@> 
@ |Scan_Parse::vector_type_plus_assign()| failed.
\initials{LDF 2004.08.31.}

@<Define rules@>=

  if (status != 0)
    {
      cerr_strm << thread_name 
                << "ERROR! In parser rule `operation_assignment --> "
                << endl 
                << "point_vector_variable PLUS_ASSIGN point_expression':"
                << endl << "`Scan_Parse::vector_type_plus_assign()' "
                << "failed.  Didn't add `point' to `point_vector'.";

      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");

    }  /* |if (status != 0)|  */
 
                             
@q ****** (6) |Scan_Parse::vector_type_plus_assign()| succeeded.@> 
@ |Scan_Parse::vector_type_plus_assign()| succeeded.
\initials{LDF 2004.08.31.}

@<Define rules@>=
                                           

  else /* |status == 0|  */
    {

#if DEBUG_COMPILE
      if (DEBUG)
        {
          cerr_strm << thread_name 
                    << "In parser rule `operation_assignment --> "
                    << endl 
                    << "point_vector_variable PLUS_ASSIGN point_expression':"
                    << endl << "`Scan_Parse::vector_type_plus_assign()' "
                    << "succeeded.";

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str("");
        }
#endif /* |DEBUG_COMPILE|  */@; 
 

    } /* |else| (|status == 0|)  */ 

@q ***** (5) Set |$$| to 0 and exit rule.@>   

@ Set |@=$$@>| to 0 and exit rule.
\initials{LDF 2004.11.05.}

\LOG
\initials{LDF 2004.11.05.}
Now setting |@=$$@>| to 0.  Formerly, it was set to |@=$3@>|. 
\ENDLOG 

@<Define rules@>=

  @=$$@> = static_cast<void*>(0);

};

@q *** (3) operation_assignment --> point_vector_variable @>  
@q *** (3) PLUS_ASSIGN point_vector_expression.           @> 

@*2 \§point vector assignment>
$\longrightarrow$ \§point vector variable> 
\.{PLUS\_ASSIGN} \§point vector expression>.      

\LOG
\initials{LDF 2004.11.10.}
Added this rule.

\initials{LDF 2004.11.11.}
Changed the code in this rule to account for changes
I've made in |Scan_Parse::vector_type_assign()|, which is 
defined in \filename{scanprse.web}.

\initials{LDF 2004.12.03.}
Moved this rule from \filename{passign.w} to this file 
(\filename{popassgn.w}). 
\ENDLOG


@q **** (4) Definition.@> 

@<Define rules@>=

@=operation_assignment: point_vector_variable @>  
@=PLUS_ASSIGN point_vector_expression@>@/
{

   @<Common declarations for rules@>@;
  
#if DEBUG_COMPILE
   DEBUG = false;  /* |true|  */
   
   if (DEBUG)
      {
        cerr_strm << thread_name 
                  << "*** Parser: "
                  << "`operation_assignment -->"
                  << endl << "point_vector_variable "
                  << "PLUS_ASSIGN point_vector_expression'.";
        
        log_message(cerr_strm);
        cerr_message(cerr_strm);
        cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */@;

   typedef Pointer_Vector<Point> PV;

   PV* pv = static_cast<PV*>(@=$3@>); 

   entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ***** (5) Error handling for the case that |entry == 0|.@>

@ Error handling for the case that |entry == 0 |.
\initials{LDF 2004.11.10.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      cerr_strm << thread_name 
                << "ERROR! In parser rule "
                << "`operation_assignment --> "
                << endl 
                << "point_vector_variable PLUS_ASSIGN "
                << "point_vector_expression':"
                << endl 
                << "`point_vector_variable' is invalid. "
                << "Can't assign to it."
                << endl << "Setting `operation_assignment' to 0, "
                << "deleting `point_vector_expression', "
                << "and will try to continue.";
                
      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");

      delete pv;

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == 0)|  */

@q ***** (5) |entry != 0|.@>   

@ |entry != 0|.
\initials{LDF 2004.11.10.}


@<Define rules@>=

  else /* |entry != 0|  */
    {

#if DEBUG_COMPILE
      if (DEBUG)
        {
          cerr_strm << thread_name 
                    << "In parser rule `operation_assignment --> "
                    << endl 
                    << "point_vector_variable PLUS_ASSIGN "
                    << "point_vector_expression':"
                    << endl 
                    << "`point_vector_variable' is valid.";
                
          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str("");
        }
#endif /* |DEBUG_COMPILE|  */@; 

  
      status = vector_type_assign<Point, Point>(scanner_node,
                                                entry,
                                                pv);          

@q ****** (6) Error handling:                                @> 
@q ****** (6) |Scan_Parse::vector_type_assign()| failed.@> 

@ Error handling:  |Scan_Parse::vector_type_assign()| 
failed. 
\initials{LDF 2004.11.10.}

@<Define rules@>=
                            

    if (status != 0)
         {
            cerr_strm << thread_name 
                      << "ERROR! In parser rule "
                      << "`operation_assignment --> "
                      << endl 
                      << "point_vector_variable PLUS_ASSIGN "
                      << "point_vector_expression':"
                      << endl 
                      << "`Scan_Parse::vector_type_assign()' failed."
                      << endl << "Deleting `point_vector_expression' "
                      << "and setting "
                      << "`operation_assignment' to 0.";

            log_message(cerr_strm);
            cerr_message(cerr_strm, error_stop_value);
            cerr_strm.str("");

            delete pv;

            @=$$@> = static_cast<void*>(0);

         } /* |if (status != 0)|  */

@q ****** (6) |Scan_Parse::vector_type_assign()| succeeded.@> 

@ |Scan_Parse::vector_type_assign()| succeeded. 
\initials{LDF 2004.11.10.}

@<Define rules@>=

   else /* |status == 0|  */
      {
         delete pv;

         @=$$@> = static_cast<void*>(0);
 
      }  /* |else| (|status == 0|)  */


   }   /* |else| (|entry != 0|)  */


@q ***** (5).@> 

}; 


@q *** (3) |bool_point_vectors|.@>  
@*2 {\bf bool\_point\_vectors}.
\initials{LDF 2004.12.04.}

\LOG
\initials{LDF 2004.12.04.}
Added this section.
\ENDLOG

@q *** (3) operation_assignment --> bool_point_vector_variable @>
@q *** (3)  := bool_point_variable.                            @>
@*2 \§operation assignment> $\longrightarrow$ 
\§bool-point vector variable>
\.{+=} \§bool-point expression>.

\LOG
\initials{LDF 2004.09.01.}
Added this rule.

\initials{LDF 2004.09.05.}
@:BUG FIX@> BUG FIX:  Now allocating memory for a new |Bool_Point|.

\initials{LDF 2004.09.05.}
Changed |@=$3@>| from |bool_point_variable| to 
|bool_point_expression|.  This makes it unnecessary to allocate 
memory for a new |Bool_Point|. 

\initials{LDF 2004.11.05.}
Changed the symbol on the left-hand side of this rule from 
|bool_point_vector_assignment| to |operation_assignment|.

\initials{LDF 2004.12.03.}
Moved this rule from \filename{passign.w} to this file 
(\filename{popassgn.w}). 
\ENDLOG 

@q **** (4) Definition.@> 

@<Define rules@>=
 
@=operation_assignment: bool_point_vector_variable @>
@=PLUS_ASSIGN bool_point_expression                @>
{

@q ***** (5).@>   

   @<Common declarations for rules@>@;
  
#if DEBUG_COMPILE
   DEBUG = false;  /* |true|  */
   
   if (DEBUG)
      {
        cerr_strm << thread_name 
                  << "*** Parser: `operation_assignment --> "
                  << endl 
                  << "bool_point_vector_variable PLUS_ASSIGN "
                  << "bool_point_expression'.";
        
        log_message(cerr_strm);
        cerr_message(cerr_strm);
        cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */@;

 
  Bool_Point* bp = static_cast<Bool_Point*>(@=$3@>);

@q ***** (5) Call |Scan_Parse::vector_type_plus_assign()|.@>   
@ Call |Scan_Parse::vector_type_plus_assign()|.
\initials{LDF 2004.09.01.}

@<Define rules@>=

   status 
      = vector_type_plus_assign<Bool_Point>(scanner_node,
                             static_cast<Id_Map_Entry_Node>(@=$1@>), 
                             BOOL_POINT_VECTOR,
                             BOOL_POINT,
                             bp);   


@q ****** (6) |Scan_Parse::vector_type_plus_assign()| failed.@> 
@ |Scan_Parse::vector_type_plus_assign()| failed.
\initials{LDF 2004.09.01.}

@<Define rules@>=

  if (status != 0)
    {
      cerr_strm << thread_name 
                << "ERROR! In parser rule `operation_assignment --> "
                << endl 
                << "bool_point_vector_variable PLUS_ASSIGN "
                << "bool_point_variable':"
                << endl << "`Scan_Parse::vector_type_plus_assign()' "
                << "failed. Didn't add `bool_point' "
                << "to `bool_point_vector'.";

      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");

    }  /* |if (status != 0)|  */
 
                             
@q ****** (6) |Scan_Parse::vector_type_plus_assign()| succeeded.@> 
@ |Scan_Parse::vector_type_plus_assign()| succeeded.
\initials{LDF 2004.09.01.}

@<Define rules@>=
                                           

  else /* |status == 0|  */
    {

#if DEBUG_COMPILE
      if (DEBUG)
        {
          cerr_strm << thread_name 
                    << "In parser rule `operation_assignment --> "
                    << endl 
                    << "bool_point_vector_variable "
                    << "PLUS_ASSIGN bool_point_variable':"
                    << endl << "`Scan_Parse::vector_type_plus_assign()' "
                    << "succeeded.";

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str("");
        }
#endif /* |DEBUG_COMPILE|  */@; 
 

    } /* |else| (|status == 0|)  */ 

@q ***** (5) Set |$$| to 0 and exit rule.@>   

@ Set |@=$$@>| to 0 and exit rule.
\initials{LDF 2004.11.05.}

\LOG
\initials{LDF 2004.11.05.}
Now setting |@=$$@>| to 0.  Formerly, it was set to |@=$3@>|. 
\ENDLOG 

@<Define rules@>=

  @=$$@> = static_cast<void*>(0);

};


@q *** (3) |path_vectors|.@>  

@*2 \§path vectors>.
\initials{LDF 2004.12.12.}

\LOG
\initials{LDF 2004.12.12.}
Added this section.
\ENDLOG

@q *** (3) operation_assignment --> path_vector_variable  @>
@q *** (3) PLUS_ASSIGN path_expression.                   @>

@*2 \§operation assignment> $\longrightarrow$ \§path vector variable>
\.{PLUS\_ASSIGN} \§path expression>.

\LOG
\initials{LDF 2004.12.12.}
Added this rule.
\ENDLOG 

@q **** (4) Definition.@> 

@<Define rules@>=
 
@=operation_assignment: path_vector_variable PLUS_ASSIGN path_expression@>
{

@q ***** (5).@>   

   @<Common declarations for rules@>@;
  
#if DEBUG_COMPILE
   DEBUG = false;  /* |true|  */
   
   if (DEBUG)
      {
        cerr_strm << thread_name 
                  << "*** Parser: `operation_assignment --> "
                  << "path_vector_variable PLUS_ASSIGN path_expression'.";
        
        log_message(cerr_strm);
        cerr_message(cerr_strm);
        cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */@;

@q ***** (5) Call |Scan_Parse::vector_type_plus_assign()|.@>   
@ Call |Scan_Parse::vector_type_plus_assign()|.
\initials{LDF 2004.12.12.}

@<Define rules@>=

  status
    = vector_type_plus_assign<Path>(scanner_node,
                                     static_cast<Id_Map_Entry_Node>(@=$1@>), 
                                     PATH_VECTOR,
                                     PATH,
                                     static_cast<Path*>(@=$3@>));



@q ****** (6) |Scan_Parse::vector_type_plus_assign()| failed.@> 
@ |Scan_Parse::vector_type_plus_assign()| failed.
\initials{LDF 2004.12.12.}

@<Define rules@>=

  if (status != 0)
    {
      cerr_strm << thread_name 
                << "ERROR! In parser rule `operation_assignment --> "
                << endl 
                << "path_vector_variable PLUS_ASSIGN path_expression':"
                << endl << "`Scan_Parse::vector_type_plus_assign()' "
                << "failed.  Didn't add `path' to `path_vector'.";

      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");

    }  /* |if (status != 0)|  */
 
                             
@q ****** (6) |Scan_Parse::vector_type_plus_assign()| succeeded.@> 
@ |Scan_Parse::vector_type_plus_assign()| succeeded.
\initials{LDF 2004.12.12.}

@<Define rules@>=
                                           

  else /* |status == 0|  */
    {

#if DEBUG_COMPILE
      if (DEBUG)
        {
          cerr_strm << thread_name 
                    << "In parser rule `operation_assignment --> "
                    << endl 
                    << "path_vector_variable PLUS_ASSIGN path_expression':"
                    << endl << "`Scan_Parse::vector_type_plus_assign()' "
                    << "succeeded.";

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str("");
        }
#endif /* |DEBUG_COMPILE|  */@; 
 

    } /* |else| (|status == 0|)  */ 

@q ***** (5) Set |$$| to 0 and exit rule.@>   

@ Set |@=$$@>| to 0 and exit rule.
\initials{LDF 2004.12.12.}

@<Define rules@>=

  @=$$@> = static_cast<void*>(0);

};

@q *** (3) operation_assignment --> path_vector_variable @>  
@q *** (3) PLUS_ASSIGN path_vector_expression.           @> 

@*2 \§path vector assignment>
$\longrightarrow$ \§path vector variable> 
\.{PLUS\_ASSIGN} \§path vector expression>.      

\LOG
\initials{LDF 2004.12.12.}
Added this rule.
\ENDLOG


@q **** (4) Definition.@> 

@<Define rules@>=

@=operation_assignment: path_vector_variable @>  
@=PLUS_ASSIGN path_vector_expression@>@/
{

   @<Common declarations for rules@>@;
  
#if DEBUG_COMPILE
   DEBUG = false;  /* |true|  */
   
   if (DEBUG)
      {
        cerr_strm << thread_name 
                  << "*** Parser: "
                  << "`operation_assignment -->"
                  << endl << "path_vector_variable "
                  << "PLUS_ASSIGN path_vector_expression'.";
        
        log_message(cerr_strm);
        cerr_message(cerr_strm);
        cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */@;

   typedef Pointer_Vector<Path> PV;

   PV* pv = static_cast<PV*>(@=$3@>); 

   entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ***** (5) Error handling for the case that |entry == 0|.@>

@ Error handling for the case that |entry == 0 |.
\initials{LDF 2004.12.12.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      cerr_strm << thread_name 
                << "ERROR! In parser rule "
                << "`operation_assignment --> "
                << endl 
                << "path_vector_variable PLUS_ASSIGN "
                << "path_vector_expression':"
                << endl 
                << "`path_vector_variable' is invalid. "
                << "Can't assign to it."
                << endl << "Setting `operation_assignment' to 0, "
                << "deleting `path_vector_expression', "
                << "and will try to continue.";
                
      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");

      delete pv;

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == 0)|  */

@q ***** (5) |entry != 0|.@>   

@ |entry != 0|.
\initials{LDF 2004.12.12.}

@<Define rules@>=

  else /* |entry != 0|  */
    {

#if DEBUG_COMPILE
      if (DEBUG)
        {
          cerr_strm << thread_name 
                    << "In parser rule `operation_assignment --> "
                    << endl 
                    << "path_vector_variable PLUS_ASSIGN "
                    << "path_vector_expression':"
                    << endl 
                    << "`path_vector_variable' is valid.";
                
          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str("");
        }
#endif /* |DEBUG_COMPILE|  */@; 

  
      status = vector_type_assign<Path, Path>(scanner_node,
                                                entry,
                                                pv);          

@q ****** (6) Error handling:                                @> 
@q ****** (6) |Scan_Parse::vector_type_assign()| failed.@> 

@ Error handling:  |Scan_Parse::vector_type_assign()| 
failed. 
\initials{LDF 2004.12.12.}

@<Define rules@>=
                            

    if (status != 0)
         {
            cerr_strm << thread_name 
                      << "ERROR! In parser rule "
                      << "`operation_assignment --> "
                      << endl 
                      << "path_vector_variable PLUS_ASSIGN "
                      << "path_vector_expression':"
                      << endl 
                      << "`Scan_Parse::vector_type_assign()' failed."
                      << endl << "Deleting `path_vector_expression' "
                      << "and setting "
                      << "`operation_assignment' to 0.";

            log_message(cerr_strm);
            cerr_message(cerr_strm, error_stop_value);
            cerr_strm.str("");

            delete pv;

            @=$$@> = static_cast<void*>(0);

         } /* |if (status != 0)|  */

@q ****** (6) |Scan_Parse::vector_type_assign()| succeeded.@> 

@ |Scan_Parse::vector_type_assign()| succeeded. 
\initials{LDF 2004.12.12.}

@<Define rules@>=

   else /* |status == 0|  */
      {
         delete pv;

         @=$$@> = static_cast<void*>(0);
 
      }  /* |else| (|status == 0|)  */


   }   /* |else| (|entry != 0|)  */


@q ***** (5).@> 

}; 


@q *** (3) |ellipse_vectors|.@>  

@*2 \§ellipse vectors>.
\initials{LDF 2004.12.14.}

\LOG
\initials{LDF 2004.12.14.}
Added this section.
\ENDLOG

@q *** (3) operation_assignment --> ellipse_vector_variable  @>
@q *** (3) PLUS_ASSIGN ellipse_expression.                   @>

@*2 \§operation assignment> $\longrightarrow$ \§ellipse vector variable>
\.{PLUS\_ASSIGN} \§ellipse expression>.

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG 

@q **** (4) Definition.@> 

@<Define rules@>=
 
@=operation_assignment: ellipse_vector_variable PLUS_ASSIGN ellipse_expression@>
{

@q ***** (5) Call |Scan_Parse::vector_type_plus_assign()|.@>   
@ Call |Scan_Parse::vector_type_plus_assign()|.
\initials{LDF 2004.12.14.}

@<Define rules@>=

   vector_type_plus_assign<Ellipse>(static_cast<Scanner_Node>(parameter),
                                    static_cast<Id_Map_Entry_Node>(@=$1@>), 
                                    ELLIPSE_VECTOR,
                                    ELLIPSE,
                                    static_cast<Ellipse*>(@=$3@>));



    @=$$@> = static_cast<void*>(0);

};

@q *** (3) operation_assignment --> ellipse_vector_variable @>  
@q *** (3) PLUS_ASSIGN ellipse_vector_expression.           @> 

@*2 \§ellipse vector assignment>
$\longrightarrow$ \§ellipse vector variable> 
\.{PLUS\_ASSIGN} \§ellipse vector expression>.      

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG


@q **** (4) Definition.@> 

@<Define rules@>=

@=operation_assignment: ellipse_vector_variable @>  
@=PLUS_ASSIGN ellipse_vector_expression@>@/
{

   typedef Pointer_Vector<Ellipse> PV;

   PV* pv = static_cast<PV*>(@=$3@>); 

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ***** (5) Error handling for the case that |entry == 0|.@>

@ Error handling for the case that |entry == 0 |.
\initials{LDF 2004.12.14.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      delete pv;

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == 0)|  */

@q ***** (5) |entry != 0|.@>   

@ |entry != 0|.
\initials{LDF 2004.12.14.}

@<Define rules@>=

  else /* |entry != 0|  */
    {

        vector_type_assign<Ellipse, Ellipse>(static_cast<Scanner_Node>(parameter),
                                                          entry,
                                                          pv);          
        delete pv;

        @=$$@> = static_cast<void*>(0);
   }

}; 

@q *** (3) |circle_vectors|.@>  

@*2 \§circle vectors>.
\initials{LDF 2004.12.14.}

\LOG
\initials{LDF 2004.12.14.}
Added this section.
\ENDLOG

@q *** (3) operation_assignment --> circle_vector_variable  @>
@q *** (3) PLUS_ASSIGN circle_expression.                   @>

@*2 \§operation assignment> $\longrightarrow$ \§circle vector variable>
\.{PLUS\_ASSIGN} \§circle expression>.

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG 

@q **** (4) Definition.@> 

@<Define rules@>=
 
@=operation_assignment: circle_vector_variable PLUS_ASSIGN circle_expression@>
{

@q ***** (5).@>   

   @<Common declarations for rules@>@;
  
#if DEBUG_COMPILE
   DEBUG = false;  /* |true|  */
   
   if (DEBUG)
      {
        cerr_strm << thread_name 
                  << "*** Parser: `operation_assignment --> "
                  << "circle_vector_variable PLUS_ASSIGN circle_expression'.";
        
        log_message(cerr_strm);
        cerr_message(cerr_strm);
        cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */@;

@q ***** (5) Call |Scan_Parse::vector_type_plus_assign()|.@>   
@ Call |Scan_Parse::vector_type_plus_assign()|.
\initials{LDF 2004.12.14.}

@<Define rules@>=

  status
     = vector_type_plus_assign<Circle>(scanner_node,
                                        static_cast<Id_Map_Entry_Node>(@=$1@>), 
                                        CIRCLE_VECTOR,
                                        CIRCLE,
                                        static_cast<Circle*>(@=$3@>));



@q ****** (6) |Scan_Parse::vector_type_plus_assign()| failed.@> 
@ |Scan_Parse::vector_type_plus_assign()| failed.
\initials{LDF 2004.12.14.}

@<Define rules@>=

  if (status != 0)
    {
      cerr_strm << thread_name 
                << "ERROR! In parser rule `operation_assignment --> "
                << endl 
                << "circle_vector_variable PLUS_ASSIGN circle_expression':"
                << endl << "`Scan_Parse::vector_type_plus_assign()' "
                << "failed.  Didn't add `circle' to `circle_vector'.";

      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");

    }  /* |if (status != 0)|  */
 
                             
@q ****** (6) |Scan_Parse::vector_type_plus_assign()| succeeded.@> 
@ |Scan_Parse::vector_type_plus_assign()| succeeded.
\initials{LDF 2004.12.14.}

@<Define rules@>=
                                           

  else /* |status == 0|  */
    {

#if DEBUG_COMPILE
      if (DEBUG)
        {
          cerr_strm << thread_name 
                    << "In parser rule `operation_assignment --> "
                    << endl 
                    << "circle_vector_variable PLUS_ASSIGN circle_expression':"
                    << endl << "`Scan_Parse::vector_type_plus_assign()' "
                    << "succeeded.";

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str("");
        }
#endif /* |DEBUG_COMPILE|  */@; 
 

    } /* |else| (|status == 0|)  */ 

@q ***** (5) Set |$$| to 0 and exit rule.@>   

@ Set |@=$$@>| to 0 and exit rule.
\initials{LDF 2004.12.14.}

@<Define rules@>=

  @=$$@> = static_cast<void*>(0);

};

@q *** (3) operation_assignment --> circle_vector_variable @>  
@q *** (3) PLUS_ASSIGN circle_vector_expression.           @> 

@*2 \§circle vector assignment>
$\longrightarrow$ \§circle vector variable> 
\.{PLUS\_ASSIGN} \§circle vector expression>.      

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG


@q **** (4) Definition.@> 

@<Define rules@>=

@=operation_assignment: circle_vector_variable @>  
@=PLUS_ASSIGN circle_vector_expression@>@/
{

   @<Common declarations for rules@>@;
  
#if DEBUG_COMPILE
   DEBUG = false;  /* |true|  */
   
   if (DEBUG)
      {
        cerr_strm << thread_name 
                  << "*** Parser: "
                  << "`operation_assignment -->"
                  << endl << "circle_vector_variable "
                  << "PLUS_ASSIGN circle_vector_expression'.";
        
        log_message(cerr_strm);
        cerr_message(cerr_strm);
        cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */@;

   typedef Pointer_Vector<Circle> PV;

   PV* pv = static_cast<PV*>(@=$3@>); 

   entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ***** (5) Error handling for the case that |entry == 0|.@>

@ Error handling for the case that |entry == 0 |.
\initials{LDF 2004.12.14.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      cerr_strm << thread_name 
                << "ERROR! In parser rule "
                << "`operation_assignment --> "
                << endl 
                << "circle_vector_variable PLUS_ASSIGN "
                << "circle_vector_expression':"
                << endl 
                << "`circle_vector_variable' is invalid. "
                << "Can't assign to it."
                << endl << "Setting `operation_assignment' to 0, "
                << "deleting `circle_vector_expression', "
                << "and will try to continue.";
                
      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");

      delete pv;

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == 0)|  */

@q ***** (5) |entry != 0|.@>   

@ |entry != 0|.
\initials{LDF 2004.12.14.}

@<Define rules@>=

  else /* |entry != 0|  */
    {

#if DEBUG_COMPILE
      if (DEBUG)
        {
          cerr_strm << thread_name 
                    << "In parser rule `operation_assignment --> "
                    << endl 
                    << "circle_vector_variable PLUS_ASSIGN "
                    << "circle_vector_expression':"
                    << endl 
                    << "`circle_vector_variable' is valid.";
                
          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str("");
        }
#endif /* |DEBUG_COMPILE|  */@; 

  
      status = vector_type_assign<Circle, Circle>(scanner_node,
                                                entry,
                                                pv);          

@q ****** (6) Error handling:                                @> 
@q ****** (6) |Scan_Parse::vector_type_assign()| failed.@> 

@ Error handling:  |Scan_Parse::vector_type_assign()| 
failed. 
\initials{LDF 2004.12.14.}

@<Define rules@>=
                            

    if (status != 0)
         {
            cerr_strm << thread_name 
                      << "ERROR! In parser rule "
                      << "`operation_assignment --> "
                      << endl 
                      << "circle_vector_variable PLUS_ASSIGN "
                      << "circle_vector_expression':"
                      << endl 
                      << "`Scan_Parse::vector_type_assign()' failed."
                      << endl << "Deleting `circle_vector_expression' "
                      << "and setting "
                      << "`operation_assignment' to 0.";

            log_message(cerr_strm);
            cerr_message(cerr_strm, error_stop_value);
            cerr_strm.str("");

            delete pv;

            @=$$@> = static_cast<void*>(0);

         } /* |if (status != 0)|  */

@q ****** (6) |Scan_Parse::vector_type_assign()| succeeded.@> 

@ |Scan_Parse::vector_type_assign()| succeeded. 
\initials{LDF 2004.12.14.}

@<Define rules@>=

   else /* |status == 0|  */
      {
         delete pv;

         @=$$@> = static_cast<void*>(0);
 
      }  /* |else| (|status == 0|)  */


   }   /* |else| (|entry != 0|)  */


@q ***** (5).@> 

}; 

@q *** (3) |parabola_vectors|.@>  

@*2 \§parabola vectors>.
\initials{LDF 2005.12.01.}

\LOG
\initials{LDF 2005.12.01.}
Added this section.
\ENDLOG

@q *** (3) operation_assignment --> parabola_vector_variable  @>
@q *** (3) PLUS_ASSIGN parabola_expression.                   @>

@*2 \§operation assignment> $\longrightarrow$ \§parabola vector variable>
\.{PLUS\_ASSIGN} \§parabola expression>.

\LOG
\initials{LDF 2005.12.01.}
Added this rule.
\ENDLOG 

@q **** (4) Definition.@> 

@<Define rules@>=
 
@=operation_assignment: parabola_vector_variable PLUS_ASSIGN parabola_expression@>
{

@q ***** (5) Call |Scan_Parse::vector_type_plus_assign()|.@>   
@ Call |Scan_Parse::vector_type_plus_assign()|.
\initials{LDF 2005.12.01.}

@<Define rules@>=

   vector_type_plus_assign<Parabola>(static_cast<Scanner_Node>(parameter),
                                    static_cast<Id_Map_Entry_Node>(@=$1@>), 
                                    PARABOLA_VECTOR,
                                    PARABOLA,
                                    static_cast<Parabola*>(@=$3@>));



    @=$$@> = static_cast<void*>(0);

};

@q *** (3) operation_assignment --> parabola_vector_variable @>  
@q *** (3) PLUS_ASSIGN parabola_vector_expression.           @> 

@*2 \§parabola vector assignment>
$\longrightarrow$ \§parabola vector variable> 
\.{PLUS\_ASSIGN} \§parabola vector expression>.      

\LOG
\initials{LDF 2005.12.01.}
Added this rule.
\ENDLOG


@q **** (4) Definition.@> 

@<Define rules@>=

@=operation_assignment: parabola_vector_variable @>  
@=PLUS_ASSIGN parabola_vector_expression@>@/
{

   typedef Pointer_Vector<Parabola> PV;

   PV* pv = static_cast<PV*>(@=$3@>); 

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ***** (5) Error handling for the case that |entry == 0|.@>

@ Error handling for the case that |entry == 0 |.
\initials{LDF 2005.12.01.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      delete pv;

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == 0)|  */

@q ***** (5) |entry != 0|.@>   

@ |entry != 0|.
\initials{LDF 2005.12.01.}

@<Define rules@>=

  else /* |entry != 0|  */
    {

        vector_type_assign<Parabola, Parabola>(static_cast<Scanner_Node>(parameter),
                                                          entry,
                                                          pv);          
        delete pv;

        @=$$@> = static_cast<void*>(0);
   }

}; 

@q *** (3) |hyperbola_vectors|.@>  

@*2 \§hyperbola vectors>.
\initials{LDF 2005.12.01.}

\LOG
\initials{LDF 2005.12.01.}
Added this section.
\ENDLOG

@q *** (3) operation_assignment --> hyperbola_vector_variable  @>
@q *** (3) PLUS_ASSIGN hyperbola_expression.                   @>

@*2 \§operation assignment> $\longrightarrow$ \§hyperbola vector variable>
\.{PLUS\_ASSIGN} \§hyperbola expression>.

\LOG
\initials{LDF 2005.12.01.}
Added this rule.
\ENDLOG 

@q **** (4) Definition.@> 

@<Define rules@>=
 
@=operation_assignment: hyperbola_vector_variable PLUS_ASSIGN hyperbola_expression@>
{

@q ***** (5) Call |Scan_Parse::vector_type_plus_assign()|.@>   
@ Call |Scan_Parse::vector_type_plus_assign()|.
\initials{LDF 2005.12.01.}

@<Define rules@>=

   vector_type_plus_assign<Hyperbola>(static_cast<Scanner_Node>(parameter),
                                    static_cast<Id_Map_Entry_Node>(@=$1@>), 
                                    HYPERBOLA_VECTOR,
                                    HYPERBOLA,
                                    static_cast<Hyperbola*>(@=$3@>));



    @=$$@> = static_cast<void*>(0);

};

@q *** (3) operation_assignment --> hyperbola_vector_variable @>  
@q *** (3) PLUS_ASSIGN hyperbola_vector_expression.           @> 

@*2 \§hyperbola vector assignment>
$\longrightarrow$ \§hyperbola vector variable> 
\.{PLUS\_ASSIGN} \§hyperbola vector expression>.      

\LOG
\initials{LDF 2005.12.01.}
Added this rule.
\ENDLOG


@q **** (4) Definition.@> 

@<Define rules@>=

@=operation_assignment: hyperbola_vector_variable @>  
@=PLUS_ASSIGN hyperbola_vector_expression@>@/
{

   typedef Pointer_Vector<Hyperbola> PV;

   PV* pv = static_cast<PV*>(@=$3@>); 

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ***** (5) Error handling for the case that |entry == 0|.@>

@ Error handling for the case that |entry == 0 |.
\initials{LDF 2005.12.01.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      delete pv;

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == 0)|  */

@q ***** (5) |entry != 0|.@>   

@ |entry != 0|.
\initials{LDF 2005.12.01.}

@<Define rules@>=

  else /* |entry != 0|  */
    {

        vector_type_assign<Hyperbola, Hyperbola>(static_cast<Scanner_Node>(parameter),
                                                          entry,
                                                          pv);          
        delete pv;

        @=$$@> = static_cast<void*>(0);
   }

}; 

@q *** (3) |conic_section_lattice_vectors|.@>  

@*2 \§conic section lattice vectors>.
\initials{LDF 2007.07.29.}

\LOG
\initials{LDF 2007.07.29.}
Added this section.
\ENDLOG

@q *** (3) operation_assignment --> conic_section_lattice_vector_variable  @>
@q *** (3) PLUS_ASSIGN conic_section_lattice_expression.                   @>

@*2 \§operation assignment> $\longrightarrow$ \§conic section lattice vector variable>
\.{PLUS\_ASSIGN} \§conic section lattice expression>.

\LOG
\initials{LDF 2007.07.29.}
Added this rule.
\ENDLOG 

@q **** (4) Definition.@> 

@<Define rules@>=
 
@=operation_assignment: conic_section_lattice_vector_variable @>@/
@=PLUS_ASSIGN conic_section_lattice_expression@>@/
{

@q ***** (5) Call |Scan_Parse::vector_type_plus_assign()|.@>   
@ Call |Scan_Parse::vector_type_plus_assign()|.
\initials{LDF 2007.07.29.}

@<Define rules@>=

   vector_type_plus_assign<Conic_Section_Lattice>(static_cast<Scanner_Node>(parameter),
                                    static_cast<Id_Map_Entry_Node>(@=$1@>), 
                                    CONIC_SECTION_LATTICE_VECTOR,
                                    CONIC_SECTION_LATTICE,
                                    static_cast<Conic_Section_Lattice*>(@=$3@>));



    @=$$@> = static_cast<void*>(0);

};

@q *** (3) operation_assignment --> conic_section_lattice_vector_variable @>  
@q *** (3) PLUS_ASSIGN conic_section_lattice_vector_expression.           @> 

@*2 \§conic section lattice vector assignment>
$\longrightarrow$ \§conic section lattice vector variable> 
\.{PLUS\_ASSIGN} \§conic section lattice vector expression>.      

\LOG
\initials{LDF 2007.07.29.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=

@=operation_assignment: conic_section_lattice_vector_variable @>  
@=PLUS_ASSIGN conic_section_lattice_vector_expression@>@/
{

   typedef Pointer_Vector<Conic_Section_Lattice> PV;

   PV* pv = static_cast<PV*>(@=$3@>); 

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ***** (5) Error handling for the case that |entry == 0|.@>

@ Error handling for the case that |entry == 0 |.
\initials{LDF 2007.07.29.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      delete pv;

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == 0)|  */

@q ***** (5) |entry != 0|.@>   

@ |entry != 0|.
\initials{LDF 2007.07.29.}

@<Define rules@>=

  else /* |entry != 0|  */
    {

        vector_type_assign<Conic_Section_Lattice, Conic_Section_Lattice>(
                                 static_cast<Scanner_Node>(parameter),
                                 entry,
                                 pv);          
        delete pv;

        @=$$@> = static_cast<void*>(0);
   }

}; 

@q *** (3) |polygon_vectors|.@>  

@*2 \§polygon vectors>.
\initials{LDF 2005.03.01.}

\LOG
\initials{LDF 2005.03.01.}
Added this section.
\ENDLOG

@q *** (3) operation_assignment --> polygon_vector_variable  @>
@q *** (3) PLUS_ASSIGN polygon_expression.                   @>

@*2 \§operation assignment> $\longrightarrow$ \§polygon vector variable>
\.{PLUS\_ASSIGN} \§polygon expression>.
\initials{LDF 2005.03.01.}

\LOG
\initials{LDF 2005.03.01.}
Added this rule.

\initials{LDF 2005.03.01.}
Changed |polygon_expression| to |polygon_like_expression|.

\initials{LDF 2005.10.24.}
Changed |polygon_like_expression| back to |polygon_expression|.
Removed debugging code.
\ENDLOG 

@q **** (4) Definition.@> 

@<Define rules@>=
 

@=operation_assignment: polygon_vector_variable PLUS_ASSIGN polygon_expression@>
{


    vector_type_plus_assign<Polygon>(static_cast<Scanner_Node>(parameter),
                                     static_cast<Id_Map_Entry_Node>(@=$1@>), 
                                     POLYGON_VECTOR,
                                     POLYGON,
                                     static_cast<Polygon*>(@=$3@>));



    @=$$@> = static_cast<void*>(0);

};

@q *** (3) operation_assignment --> polygon_vector_variable @>  
@q *** (3) PLUS_ASSIGN polygon_vector_expression.           @> 

@*2 \§polygon vector assignment>
$\longrightarrow$ \§polygon vector variable> 
\.{PLUS\_ASSIGN} \§polygon vector expression>.      
\initials{LDF 2005.03.01.}

\LOG
\initials{LDF 2005.03.01.}
Added this rule.
\ENDLOG


@q **** (4) Definition.@> 

@<Define rules@>=

@=operation_assignment: polygon_vector_variable @>  
@=PLUS_ASSIGN polygon_vector_expression@>@/
{

   @<Common declarations for rules@>@;
  
#if DEBUG_COMPILE
   DEBUG = false;  /* |true|  */
   
   if (DEBUG)
      {
        cerr_strm << thread_name 
                  << "*** Parser: "
                  << "`operation_assignment -->"
                  << endl << "polygon_vector_variable "
                  << "PLUS_ASSIGN polygon_vector_expression'.";
        
        log_message(cerr_strm);
        cerr_message(cerr_strm);
        cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */@;

   typedef Pointer_Vector<Polygon> PV;

   PV* pv = static_cast<PV*>(@=$3@>); 

   entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ***** (5) Error handling for the case that |entry == 0|.@>

@ Error handling for the case that |entry == 0 |.
\initials{LDF 2005.03.01.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      cerr_strm << thread_name 
                << "ERROR! In parser rule "
                << "`operation_assignment --> "
                << endl 
                << "polygon_vector_variable PLUS_ASSIGN "
                << "polygon_vector_expression':"
                << endl 
                << "`polygon_vector_variable' is invalid. "
                << "Can't assign to it."
                << endl << "Setting `operation_assignment' to 0, "
                << "deleting `polygon_vector_expression', "
                << "and will try to continue.";
                
      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");

      delete pv;

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == 0)|  */

@q ***** (5) |entry != 0|.@>   

@ |entry != 0|.
\initials{LDF 2005.03.01.}

@<Define rules@>=

  else /* |entry != 0|  */
    {

#if DEBUG_COMPILE
      if (DEBUG)
        {
          cerr_strm << thread_name 
                    << "In parser rule `operation_assignment --> "
                    << endl 
                    << "polygon_vector_variable PLUS_ASSIGN "
                    << "polygon_vector_expression':"
                    << endl 
                    << "`polygon_vector_variable' is valid.";
                
          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str("");
        }
#endif /* |DEBUG_COMPILE|  */@; 

  
      status = vector_type_assign<Polygon, Polygon>(scanner_node,
                                                entry,
                                                pv);          

@q ****** (6) Error handling:                                @> 
@q ****** (6) |Scan_Parse::vector_type_assign()| failed.@> 

@ Error handling:  |Scan_Parse::vector_type_assign()| 
failed. 
\initials{LDF 2005.03.01.}

@<Define rules@>=
                            

    if (status != 0)
         {
            cerr_strm << thread_name 
                      << "ERROR! In parser rule "
                      << "`operation_assignment --> "
                      << endl 
                      << "polygon_vector_variable PLUS_ASSIGN "
                      << "polygon_vector_expression':"
                      << endl 
                      << "`Scan_Parse::vector_type_assign()' failed."
                      << endl << "Deleting `polygon_vector_expression' "
                      << "and setting "
                      << "`operation_assignment' to 0.";

            log_message(cerr_strm);
            cerr_message(cerr_strm, error_stop_value);
            cerr_strm.str("");

            delete pv;

            @=$$@> = static_cast<void*>(0);

         } /* |if (status != 0)|  */

@q ****** (6) |Scan_Parse::vector_type_assign()| succeeded.@> 

@ |Scan_Parse::vector_type_assign()| succeeded. 
\initials{LDF 2005.03.01.}

@<Define rules@>=

   else /* |status == 0|  */
      {
         delete pv;

         @=$$@> = static_cast<void*>(0);
 
      }  /* |else| (|status == 0|)  */


   }   /* |else| (|entry != 0|)  */


@q ***** (5).@> 

}; 


@q *** (3) |triangle_vectors|.@>  

@*2 \§triangle vectors>.
\initials{LDF 2005.01.25.}

\LOG
\initials{LDF 2005.01.25.}
Added this section.
\ENDLOG

@q *** (3) operation_assignment --> triangle_vector_variable  @>
@q *** (3) PLUS_ASSIGN triangle_expression.                   @>

@*2 \§operation assignment> $\longrightarrow$ \§triangle vector variable>
\.{PLUS\_ASSIGN} \§triangle expression>.

\LOG
\initials{LDF 2005.01.25.}
Added this rule.
\ENDLOG 

@q **** (4) Definition.@> 

@<Define rules@>=
 
@=operation_assignment: triangle_vector_variable PLUS_ASSIGN triangle_expression@>
{

@q ***** (5).@>   

   @<Common declarations for rules@>@;
  
#if DEBUG_COMPILE
   DEBUG = false;  /* |true|  */
   
   if (DEBUG)
      {
        cerr_strm << thread_name 
                  << "*** Parser:  `operation_assignment --> "
                  << "triangle_vector_variable PLUS_ASSIGN triangle_expression'.";
        
        log_message(cerr_strm);
        cerr_message(cerr_strm);
        cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */@;

@q ***** (5) Call |Scan_Parse::vector_type_plus_assign()|.@>   
@ Call |Scan_Parse::vector_type_plus_assign()|.
\initials{LDF 2005.01.25.}

@<Define rules@>=

  status
     = vector_type_plus_assign<Triangle>(scanner_node,
                                         static_cast<Id_Map_Entry_Node>(@=$1@>), 
                                         TRIANGLE_VECTOR,
                                         TRIANGLE,
                                         static_cast<Triangle*>(@=$3@>));



@q ****** (6) |Scan_Parse::vector_type_plus_assign()| failed.@> 
@ |Scan_Parse::vector_type_plus_assign()| failed.
\initials{LDF 2005.01.25.}

@<Define rules@>=

  if (status != 0)
    {
      cerr_strm << thread_name 
                << "ERROR! In parser rule `operation_assignment --> "
                << endl 
                << "triangle_vector_variable PLUS_ASSIGN triangle_expression':"
                << endl << "`Scan_Parse::vector_type_plus_assign()' "
                << "failed.  Didn't add `triangle' to `triangle_vector'.";

      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");

    }  /* |if (status != 0)|  */
 
                             
@q ****** (6) |Scan_Parse::vector_type_plus_assign()| succeeded.@> 
@ |Scan_Parse::vector_type_plus_assign()| succeeded.
\initials{LDF 2005.01.25.}

@<Define rules@>=
                                           

  else /* |status == 0|  */
    {

#if DEBUG_COMPILE
      if (DEBUG)
        {
          cerr_strm << thread_name 
                    << "In parser rule `operation_assignment --> "
                    << endl 
                    << "triangle_vector_variable PLUS_ASSIGN triangle_expression':"
                    << endl << "`Scan_Parse::vector_type_plus_assign()' "
                    << "succeeded.";

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str("");
        }
#endif /* |DEBUG_COMPILE|  */@; 
 

    } /* |else| (|status == 0|)  */ 

@q ***** (5) Set |$$| to 0 and exit rule.@>   

@ Set |@=$$@>| to 0 and exit rule.
\initials{LDF 2005.01.25.}

@<Define rules@>=

  @=$$@> = static_cast<void*>(0);

};

@q *** (3) operation_assignment --> triangle_vector_variable @>  
@q *** (3) PLUS_ASSIGN triangle_vector_expression.           @> 

@*2 \§triangle vector assignment>
$\longrightarrow$ \§triangle vector variable> 
\.{PLUS\_ASSIGN} \§triangle vector expression>.      

\LOG
\initials{LDF 2005.01.25.}
Added this rule.
\ENDLOG


@q **** (4) Definition.@> 

@<Define rules@>=

@=operation_assignment: triangle_vector_variable @>  
@=PLUS_ASSIGN triangle_vector_expression@>@/
{

   @<Common declarations for rules@>@;
  
#if DEBUG_COMPILE
   DEBUG = false;  /* |true|  */
   
   if (DEBUG)
      {
        cerr_strm << thread_name 
                  << "*** Parser: "
                  << "`operation_assignment -->"
                  << endl << "triangle_vector_variable "
                  << "PLUS_ASSIGN triangle_vector_expression'.";
        
        log_message(cerr_strm);
        cerr_message(cerr_strm);
        cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */@;

   typedef Pointer_Vector<Triangle> PV;

   PV* pv = static_cast<PV*>(@=$3@>); 

   entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ***** (5) Error handling for the case that |entry == 0|.@>

@ Error handling for the case that |entry == 0 |.
\initials{LDF 2005.01.25.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      cerr_strm << thread_name 
                << "ERROR! In parser rule "
                << "`operation_assignment --> "
                << endl 
                << "triangle_vector_variable PLUS_ASSIGN "
                << "triangle_vector_expression':"
                << endl 
                << "`triangle_vector_variable' is invalid. "
                << "Can't assign to it."
                << endl << "Setting `operation_assignment' to 0, "
                << "deleting `triangle_vector_expression', "
                << "and will try to continue.";
                
      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");

      delete pv;

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == 0)|  */

@q ***** (5) |entry != 0|.@>   

@ |entry != 0|.
\initials{LDF 2005.01.25.}

@<Define rules@>=

  else /* |entry != 0|  */
    {

#if DEBUG_COMPILE
      if (DEBUG)
        {
          cerr_strm << thread_name 
                    << "In parser rule `operation_assignment --> "
                    << endl 
                    << "triangle_vector_variable PLUS_ASSIGN "
                    << "triangle_vector_expression':"
                    << endl 
                    << "`triangle_vector_variable' is valid.";
                
          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str("");
        }
#endif /* |DEBUG_COMPILE|  */@; 

  
      status = vector_type_assign<Triangle, Triangle>(scanner_node,
                                                      entry,
                                                      pv);          

@q ****** (6) Error handling:                                @> 
@q ****** (6) |Scan_Parse::vector_type_assign()| failed.@> 

@ Error handling:  |Scan_Parse::vector_type_assign()| 
failed. 
\initials{LDF 2005.01.25.}

@<Define rules@>=
                            

    if (status != 0)
         {
            cerr_strm << thread_name 
                      << "ERROR! In parser rule "
                      << "`operation_assignment --> "
                      << endl 
                      << "triangle_vector_variable PLUS_ASSIGN "
                      << "triangle_vector_expression':"
                      << endl 
                      << "`Scan_Parse::vector_type_assign()' failed."
                      << endl << "Deleting `triangle_vector_expression' "
                      << "and setting "
                      << "`operation_assignment' to 0.";

            log_message(cerr_strm);
            cerr_message(cerr_strm, error_stop_value);
            cerr_strm.str("");

            delete pv;

            @=$$@> = static_cast<void*>(0);

         } /* |if (status != 0)|  */

@q ****** (6) |Scan_Parse::vector_type_assign()| succeeded.@> 

@ |Scan_Parse::vector_type_assign()| succeeded. 
\initials{LDF 2005.01.25.}

@<Define rules@>=

   else /* |status == 0|  */
      {
         delete pv;

         @=$$@> = static_cast<void*>(0);
 
      }  /* |else| (|status == 0|)  */


   }   /* |else| (|entry != 0|)  */


@q ***** (5).@> 

}; 

@q *** (3) |rectangle_vectors|.@>  

@*2 \§rectangle vectors>.
\initials{LDF 2004.12.14.}

\LOG
\initials{LDF 2004.12.14.}
Added this section.
\ENDLOG

@q *** (3) operation_assignment --> rectangle_vector_variable  @>
@q *** (3) PLUS_ASSIGN rectangle_expression.                   @>

@*2 \§operation assignment> $\longrightarrow$ \§rectangle vector variable>
\.{PLUS\_ASSIGN} \§rectangle expression>.

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG 

@q **** (4) Definition.@> 

@<Define rules@>=
 
@=operation_assignment: rectangle_vector_variable PLUS_ASSIGN rectangle_expression@>
{

@q ***** (5).@>   

   @<Common declarations for rules@>@;
  
#if DEBUG_COMPILE
   DEBUG = false;  /* |true|  */
   
   if (DEBUG)
      {
        cerr_strm << thread_name 
                  << "*** Parser: `operation_assignment --> "
                  << "rectangle_vector_variable PLUS_ASSIGN rectangle_expression'.";
        
        log_message(cerr_strm);
        cerr_message(cerr_strm);
        cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */@;

@q ***** (5) Call |Scan_Parse::vector_type_plus_assign()|.@>   
@ Call |Scan_Parse::vector_type_plus_assign()|.
\initials{LDF 2004.12.14.}

@<Define rules@>=

  status
     = vector_type_plus_assign<Rectangle>(scanner_node,
                                        static_cast<Id_Map_Entry_Node>(@=$1@>), 
                                        RECTANGLE_VECTOR,
                                        RECTANGLE,
                                        static_cast<Rectangle*>(@=$3@>));



@q ****** (6) |Scan_Parse::vector_type_plus_assign()| failed.@> 
@ |Scan_Parse::vector_type_plus_assign()| failed.
\initials{LDF 2004.12.14.}

@<Define rules@>=

  if (status != 0)
    {
      cerr_strm << thread_name 
                << "ERROR! In parser rule `operation_assignment --> "
                << endl 
                << "rectangle_vector_variable PLUS_ASSIGN rectangle_expression':"
                << endl << "`Scan_Parse::vector_type_plus_assign()' "
                << "failed.  Didn't add `rectangle' to `rectangle_vector'.";

      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");

    }  /* |if (status != 0)|  */
 
                             
@q ****** (6) |Scan_Parse::vector_type_plus_assign()| succeeded.@> 
@ |Scan_Parse::vector_type_plus_assign()| succeeded.
\initials{LDF 2004.12.14.}

@<Define rules@>=
                                           

  else /* |status == 0|  */
    {

#if DEBUG_COMPILE
      if (DEBUG)
        {
          cerr_strm << thread_name 
                    << "In parser rule `operation_assignment --> "
                    << endl 
                    << "rectangle_vector_variable PLUS_ASSIGN rectangle_expression':"
                    << endl << "`Scan_Parse::vector_type_plus_assign()' "
                    << "succeeded.";

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str("");
        }
#endif /* |DEBUG_COMPILE|  */@; 
 

    } /* |else| (|status == 0|)  */ 

@q ***** (5) Set |$$| to 0 and exit rule.@>   

@ Set |@=$$@>| to 0 and exit rule.
\initials{LDF 2004.12.14.}

@<Define rules@>=

  @=$$@> = static_cast<void*>(0);

};

@q *** (3) operation_assignment --> rectangle_vector_variable @>  
@q *** (3) PLUS_ASSIGN rectangle_vector_expression.           @> 

@*2 \§rectangle vector assignment>
$\longrightarrow$ \§rectangle vector variable> 
\.{PLUS\_ASSIGN} \§rectangle vector expression>.      

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG


@q **** (4) Definition.@> 

@<Define rules@>=

@=operation_assignment: rectangle_vector_variable @>  
@=PLUS_ASSIGN rectangle_vector_expression@>@/
{

   @<Common declarations for rules@>@;
  
#if DEBUG_COMPILE
   DEBUG = false;  /* |true|  */
   
   if (DEBUG)
      {
        cerr_strm << thread_name 
                  << "*** Parser: "
                  << "`operation_assignment -->"
                  << endl << "rectangle_vector_variable "
                  << "PLUS_ASSIGN rectangle_vector_expression'.";
        
        log_message(cerr_strm);
        cerr_message(cerr_strm);
        cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */@;

   typedef Pointer_Vector<Rectangle> PV;

   PV* pv = static_cast<PV*>(@=$3@>); 

   entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ***** (5) Error handling for the case that |entry == 0|.@>

@ Error handling for the case that |entry == 0 |.
\initials{LDF 2004.12.14.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      cerr_strm << thread_name 
                << "ERROR! In parser rule "
                << "`operation_assignment --> "
                << endl 
                << "rectangle_vector_variable PLUS_ASSIGN "
                << "rectangle_vector_expression':"
                << endl 
                << "`rectangle_vector_variable' is invalid. "
                << "Can't assign to it."
                << endl << "Setting `operation_assignment' to 0, "
                << "deleting `rectangle_vector_expression', "
                << "and will try to continue.";
                
      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");

      delete pv;

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == 0)|  */

@q ***** (5) |entry != 0|.@>   

@ |entry != 0|.
\initials{LDF 2004.12.14.}

@<Define rules@>=

  else /* |entry != 0|  */
    {

#if DEBUG_COMPILE
      if (DEBUG)
        {
          cerr_strm << thread_name 
                    << "In parser rule `operation_assignment --> "
                    << endl 
                    << "rectangle_vector_variable PLUS_ASSIGN "
                    << "rectangle_vector_expression':"
                    << endl 
                    << "`rectangle_vector_variable' is valid.";
                
          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str("");
        }
#endif /* |DEBUG_COMPILE|  */@; 

  
      status = vector_type_assign<Rectangle, Rectangle>(scanner_node,
                                                entry,
                                                pv);          

@q ****** (6) Error handling:                                @> 
@q ****** (6) |Scan_Parse::vector_type_assign()| failed.@> 

@ Error handling:  |Scan_Parse::vector_type_assign()| 
failed. 
\initials{LDF 2004.12.14.}

@<Define rules@>=
                            

    if (status != 0)
         {
            cerr_strm << thread_name 
                      << "ERROR! In parser rule "
                      << "`operation_assignment --> "
                      << endl 
                      << "rectangle_vector_variable PLUS_ASSIGN "
                      << "rectangle_vector_expression':"
                      << endl 
                      << "`Scan_Parse::vector_type_assign()' failed."
                      << endl << "Deleting `rectangle_vector_expression' "
                      << "and setting "
                      << "`operation_assignment' to 0.";

            log_message(cerr_strm);
            cerr_message(cerr_strm, error_stop_value);
            cerr_strm.str("");

            delete pv;

            @=$$@> = static_cast<void*>(0);

         } /* |if (status != 0)|  */

@q ****** (6) |Scan_Parse::vector_type_assign()| succeeded.@> 

@ |Scan_Parse::vector_type_assign()| succeeded. 
\initials{LDF 2004.12.14.}

@<Define rules@>=

   else /* |status == 0|  */
      {
         delete pv;

         @=$$@> = static_cast<void*>(0);
 
      }  /* |else| (|status == 0|)  */


   }   /* |else| (|entry != 0|)  */


@q ***** (5).@> 

}; 

@q *** (3) |reg_polygon_vectors|.@>  

@*2 \§regular polygon vectors>.
\initials{LDF 2004.12.14.}

\LOG
\initials{LDF 2004.12.14.}
Added this section.
\ENDLOG

@q *** (3) operation_assignment --> reg_polygon_vector_variable  @>
@q *** (3) PLUS_ASSIGN reg_polygon_expression.                   @>

@*2 \§operation assignment> $\longrightarrow$ \§regular polygon vector variable>
\.{PLUS\_ASSIGN} \§regular polygon expression>.

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG 

@q **** (4) Definition.@> 

@<Define rules@>=
 
@=operation_assignment: reg_polygon_vector_variable PLUS_ASSIGN reg_polygon_expression@>
{

@q ***** (5).@>   

   @<Common declarations for rules@>@;
  
#if DEBUG_COMPILE
   DEBUG = false;  /* |true|  */
   
   if (DEBUG)
      {
        cerr_strm << thread_name 
                  << "*** Parser: `operation_assignment --> "
                  << "reg_polygon_vector_variable PLUS_ASSIGN reg_polygon_expression'.";
        
        log_message(cerr_strm);
        cerr_message(cerr_strm);
        cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */@;

@q ***** (5) Call |Scan_Parse::vector_type_plus_assign()|.@>   
@ Call |Scan_Parse::vector_type_plus_assign()|.
\initials{LDF 2004.12.14.}

@<Define rules@>=

  status
     = vector_type_plus_assign<Reg_Polygon>(scanner_node,
                                        static_cast<Id_Map_Entry_Node>(@=$1@>), 
                                        REG_POLYGON_VECTOR,
                                        REG_POLYGON,
                                        static_cast<Reg_Polygon*>(@=$3@>));



@q ****** (6) |Scan_Parse::vector_type_plus_assign()| failed.@> 
@ |Scan_Parse::vector_type_plus_assign()| failed.
\initials{LDF 2004.12.14.}

@<Define rules@>=

  if (status != 0)
    {
      cerr_strm << thread_name 
                << "ERROR! In parser rule `operation_assignment --> "
                << endl 
                << "reg_polygon_vector_variable PLUS_ASSIGN reg_polygon_expression':"
                << endl << "`Scan_Parse::vector_type_plus_assign()' "
                << "failed.  Didn't add `reg_polygon' to `reg_polygon_vector'.";

      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");

    }  /* |if (status != 0)|  */
 
                             
@q ****** (6) |Scan_Parse::vector_type_plus_assign()| succeeded.@> 
@ |Scan_Parse::vector_type_plus_assign()| succeeded.
\initials{LDF 2004.12.14.}

@<Define rules@>=
                                           

  else /* |status == 0|  */
    {

#if DEBUG_COMPILE
      if (DEBUG)
        {
          cerr_strm << thread_name 
                    << "In parser rule `operation_assignment --> "
                    << endl 
                    << "reg_polygon_vector_variable PLUS_ASSIGN reg_polygon_expression':"
                    << endl << "`Scan_Parse::vector_type_plus_assign()' "
                    << "succeeded.";

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str("");
        }
#endif /* |DEBUG_COMPILE|  */@; 
 

    } /* |else| (|status == 0|)  */ 

@q ***** (5) Set |$$| to 0 and exit rule.@>   

@ Set |@=$$@>| to 0 and exit rule.
\initials{LDF 2004.12.14.}

@<Define rules@>=

  @=$$@> = static_cast<void*>(0);

};

@q *** (3) operation_assignment --> reg_polygon_vector_variable @>  
@q *** (3) PLUS_ASSIGN reg_polygon_vector_expression.           @> 

@*2 \§regular polygon vector assignment>
$\longrightarrow$ \§regular polygon vector variable> 
\.{PLUS\_ASSIGN} \§regular polygon vector expression>.      

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG


@q **** (4) Definition.@> 

@<Define rules@>=

@=operation_assignment: reg_polygon_vector_variable @>  
@=PLUS_ASSIGN reg_polygon_vector_expression@>@/
{

   @<Common declarations for rules@>@;
  
#if DEBUG_COMPILE
   DEBUG = false;  /* |true|  */
   
   if (DEBUG)
      {
        cerr_strm << thread_name 
                  << "*** Parser: "
                  << "`operation_assignment -->"
                  << endl << "reg_polygon_vector_variable "
                  << "PLUS_ASSIGN reg_polygon_vector_expression'.";
        
        log_message(cerr_strm);
        cerr_message(cerr_strm);
        cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */@;

   typedef Pointer_Vector<Reg_Polygon> PV;

   PV* pv = static_cast<PV*>(@=$3@>); 

   entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ***** (5) Error handling for the case that |entry == 0|.@>

@ Error handling for the case that |entry == 0 |.
\initials{LDF 2004.12.14.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      cerr_strm << thread_name 
                << "ERROR! In parser rule "
                << "`operation_assignment --> "
                << endl 
                << "reg_polygon_vector_variable PLUS_ASSIGN "
                << "reg_polygon_vector_expression':"
                << endl 
                << "`reg_polygon_vector_variable' is invalid. "
                << "Can't assign to it."
                << endl << "Setting `operation_assignment' to 0, "
                << "deleting `reg_polygon_vector_expression', "
                << "and will try to continue.";
                
      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");

      delete pv;

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == 0)|  */

@q ***** (5) |entry != 0|.@>   

@ |entry != 0|.
\initials{LDF 2004.12.14.}

@<Define rules@>=

  else /* |entry != 0|  */
    {

#if DEBUG_COMPILE
      if (DEBUG)
        {
          cerr_strm << thread_name 
                    << "In parser rule `operation_assignment --> "
                    << endl 
                    << "reg_polygon_vector_variable PLUS_ASSIGN "
                    << "reg_polygon_vector_expression':"
                    << endl 
                    << "`reg_polygon_vector_variable' is valid.";
                
          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str("");
        }
#endif /* |DEBUG_COMPILE|  */@; 

  
      status = vector_type_assign<Reg_Polygon, Reg_Polygon>(scanner_node,
                                                entry,
                                                pv);          

@q ****** (6) Error handling:                                @> 
@q ****** (6) |Scan_Parse::vector_type_assign()| failed.@> 

@ Error handling:  |Scan_Parse::vector_type_assign()| 
failed. 
\initials{LDF 2004.12.14.}

@<Define rules@>=
                            

    if (status != 0)
         {
            cerr_strm << thread_name 
                      << "ERROR! In parser rule "
                      << "`operation_assignment --> "
                      << endl 
                      << "reg_polygon_vector_variable PLUS_ASSIGN "
                      << "reg_polygon_vector_expression':"
                      << endl 
                      << "`Scan_Parse::vector_type_assign()' failed."
                      << endl << "Deleting `reg_polygon_vector_expression' "
                      << "and setting "
                      << "`operation_assignment' to 0.";

            log_message(cerr_strm);
            cerr_message(cerr_strm, error_stop_value);
            cerr_strm.str("");

            delete pv;

            @=$$@> = static_cast<void*>(0);

         } /* |if (status != 0)|  */

@q ****** (6) |Scan_Parse::vector_type_assign()| succeeded.@> 

@ |Scan_Parse::vector_type_assign()| succeeded. 
\initials{LDF 2004.12.14.}

@<Define rules@>=

   else /* |status == 0|  */
      {
         delete pv;

         @=$$@> = static_cast<void*>(0);
 
      }  /* |else| (|status == 0|)  */


   }   /* |else| (|entry != 0|)  */


@q ***** (5).@> 

}; 

@q *** (3) |ellipsoid_vectors|.@>  

@*2 \§ellipsoid vectors>.
\initials{LDF 2005.12.01.}

\LOG
\initials{LDF 2005.12.01.}
Added this section.
\ENDLOG

@q *** (3) operation_assignment --> ellipsoid_vector_variable  @>
@q *** (3) PLUS_ASSIGN ellipsoid_expression.                   @>

@*2 \§operation assignment> $\longrightarrow$ \§ellipsoid vector variable>
\.{PLUS\_ASSIGN} \§ellipsoid expression>.

\LOG
\initials{LDF 2005.12.01.}
Added this rule.
\ENDLOG 

@q **** (4) Definition.@> 

@<Define rules@>=
 
@=operation_assignment: ellipsoid_vector_variable PLUS_ASSIGN ellipsoid_expression@>
{

@q ***** (5) Call |Scan_Parse::vector_type_plus_assign()|.@>   
@ Call |Scan_Parse::vector_type_plus_assign()|.
\initials{LDF 2005.12.01.}

@<Define rules@>=

   vector_type_plus_assign<Ellipsoid>(static_cast<Scanner_Node>(parameter),
                                    static_cast<Id_Map_Entry_Node>(@=$1@>), 
                                    ELLIPSOID_VECTOR,
                                    ELLIPSOID,
                                    static_cast<Ellipsoid*>(@=$3@>));



    @=$$@> = static_cast<void*>(0);

};

@q *** (3) operation_assignment --> ellipsoid_vector_variable @>  
@q *** (3) PLUS_ASSIGN ellipsoid_vector_expression.           @> 

@*2 \§ellipsoid vector assignment>
$\longrightarrow$ \§ellipsoid vector variable> 
\.{PLUS\_ASSIGN} \§ellipsoid vector expression>.      

\LOG
\initials{LDF 2005.12.01.}
Added this rule.
\ENDLOG


@q **** (4) Definition.@> 

@<Define rules@>=

@=operation_assignment: ellipsoid_vector_variable @>  
@=PLUS_ASSIGN ellipsoid_vector_expression@>@/
{

   typedef Pointer_Vector<Ellipsoid> PV;

   PV* pv = static_cast<PV*>(@=$3@>); 

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ***** (5) Error handling for the case that |entry == 0|.@>

@ Error handling for the case that |entry == 0 |.
\initials{LDF 2005.12.01.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      delete pv;

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == 0)|  */

@q ***** (5) |entry != 0|.@>   

@ |entry != 0|.
\initials{LDF 2005.12.01.}

@<Define rules@>=

  else /* |entry != 0|  */
    {

        vector_type_assign<Ellipsoid, Ellipsoid>(static_cast<Scanner_Node>(parameter),
                                                          entry,
                                                          pv);          
        delete pv;

        @=$$@> = static_cast<void*>(0);
   }

}; 

@q *** (3) |sphere_vectors|.@>  

@*2 \§sphere vectors>.
\initials{LDF 2005.12.01.}

\LOG
\initials{LDF 2005.12.01.}
Added this section.
\ENDLOG

@q *** (3) operation_assignment --> sphere_vector_variable  @>
@q *** (3) PLUS_ASSIGN sphere_expression.                   @>

@*2 \§operation assignment> $\longrightarrow$ \§sphere vector variable>
\.{PLUS\_ASSIGN} \§sphere expression>.

\LOG
\initials{LDF 2005.12.01.}
Added this rule.
\ENDLOG 

@q **** (4) Definition.@> 

@<Define rules@>=
 
@=operation_assignment: sphere_vector_variable PLUS_ASSIGN sphere_expression@>
{

@q ***** (5) Call |Scan_Parse::vector_type_plus_assign()|.@>   
@ Call |Scan_Parse::vector_type_plus_assign()|.
\initials{LDF 2005.12.01.}

@<Define rules@>=

   vector_type_plus_assign<Sphere>(static_cast<Scanner_Node>(parameter),
                                    static_cast<Id_Map_Entry_Node>(@=$1@>), 
                                    SPHERE_VECTOR,
                                    SPHERE,
                                    static_cast<Sphere*>(@=$3@>));



    @=$$@> = static_cast<void*>(0);

};

@q *** (3) operation_assignment --> sphere_vector_variable @>  
@q *** (3) PLUS_ASSIGN sphere_vector_expression.           @> 

@*2 \§sphere vector assignment>
$\longrightarrow$ \§sphere vector variable> 
\.{PLUS\_ASSIGN} \§sphere vector expression>.      

\LOG
\initials{LDF 2005.12.01.}
Added this rule.
\ENDLOG


@q **** (4) Definition.@> 

@<Define rules@>=

@=operation_assignment: sphere_vector_variable @>  
@=PLUS_ASSIGN sphere_vector_expression@>@/
{

   typedef Pointer_Vector<Sphere> PV;

   PV* pv = static_cast<PV*>(@=$3@>); 

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ***** (5) Error handling for the case that |entry == 0|.@>

@ Error handling for the case that |entry == 0 |.
\initials{LDF 2005.12.01.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      delete pv;

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == 0)|  */

@q ***** (5) |entry != 0|.@>   

@ |entry != 0|.
\initials{LDF 2005.12.01.}

@<Define rules@>=

  else /* |entry != 0|  */
    {

        vector_type_assign<Sphere, Sphere>(static_cast<Scanner_Node>(parameter),
                                                          entry,
                                                          pv);          
        delete pv;

        @=$$@> = static_cast<void*>(0);
   }

}; 




@q *** (3) |cuboid_vectors|.@>  

@*2 \§cuboid vectors>.
\initials{LDF 2004.12.14.}

\LOG
\initials{LDF 2004.12.14.}
Added this section.
\ENDLOG

@q *** (3) operation_assignment --> cuboid_vector_variable  @>
@q *** (3) PLUS_ASSIGN cuboid_expression.                   @>

@*2 \§operation assignment> $\longrightarrow$ \§cuboid vector variable>
\.{PLUS\_ASSIGN} \§cuboid expression>.

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG 

@q **** (4) Definition.@> 

@<Define rules@>=
 
@=operation_assignment: cuboid_vector_variable PLUS_ASSIGN cuboid_expression@>
{

@q ***** (5).@>   

   @<Common declarations for rules@>@;
  
#if DEBUG_COMPILE
   DEBUG = false;  /* |true|  */
   
   if (DEBUG)
      {
        cerr_strm << thread_name 
                  << "*** Parser: `operation_assignment --> "
                  << "cuboid_vector_variable PLUS_ASSIGN cuboid_expression'.";
        
        log_message(cerr_strm);
        cerr_message(cerr_strm);
        cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */@;

@q ***** (5) Call |Scan_Parse::vector_type_plus_assign()|.@>   
@ Call |Scan_Parse::vector_type_plus_assign()|.
\initials{LDF 2004.12.14.}

@<Define rules@>=

  status
     = vector_type_plus_assign<Cuboid>(scanner_node,
                                        static_cast<Id_Map_Entry_Node>(@=$1@>), 
                                        CUBOID_VECTOR,
                                        CUBOID,
                                        static_cast<Cuboid*>(@=$3@>));



@q ****** (6) |Scan_Parse::vector_type_plus_assign()| failed.@> 
@ |Scan_Parse::vector_type_plus_assign()| failed.
\initials{LDF 2004.12.14.}

@<Define rules@>=

  if (status != 0)
    {
      cerr_strm << thread_name 
                << "ERROR! In parser rule `operation_assignment --> "
                << endl 
                << "cuboid_vector_variable PLUS_ASSIGN cuboid_expression':"
                << endl << "`Scan_Parse::vector_type_plus_assign()' "
                << "failed.  Didn't add `cuboid' to `cuboid_vector'.";

      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");

    }  /* |if (status != 0)|  */
 
                             
@q ****** (6) |Scan_Parse::vector_type_plus_assign()| succeeded.@> 
@ |Scan_Parse::vector_type_plus_assign()| succeeded.
\initials{LDF 2004.12.14.}

@<Define rules@>=
                                           

  else /* |status == 0|  */
    {

#if DEBUG_COMPILE
      if (DEBUG)
        {
          cerr_strm << thread_name 
                    << "In parser rule `operation_assignment --> "
                    << endl 
                    << "cuboid_vector_variable PLUS_ASSIGN cuboid_expression':"
                    << endl << "`Scan_Parse::vector_type_plus_assign()' "
                    << "succeeded.";

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str("");
        }
#endif /* |DEBUG_COMPILE|  */@; 
 

    } /* |else| (|status == 0|)  */ 

@q ***** (5) Set |$$| to 0 and exit rule.@>   

@ Set |@=$$@>| to 0 and exit rule.
\initials{LDF 2004.12.14.}

@<Define rules@>=

  @=$$@> = static_cast<void*>(0);

};

@q *** (3) operation_assignment --> cuboid_vector_variable @>  
@q *** (3) PLUS_ASSIGN cuboid_vector_expression.           @> 

@*2 \§cuboid vector assignment>
$\longrightarrow$ \§cuboid vector variable> 
\.{PLUS\_ASSIGN} \§cuboid vector expression>.      

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG


@q **** (4) Definition.@> 

@<Define rules@>=

@=operation_assignment: cuboid_vector_variable @>  
@=PLUS_ASSIGN cuboid_vector_expression@>@/
{

   @<Common declarations for rules@>@;
  
#if DEBUG_COMPILE
   DEBUG = false;  /* |true|  */
   
   if (DEBUG)
      {
        cerr_strm << thread_name 
                  << "*** Parser: "
                  << "`operation_assignment -->"
                  << endl << "cuboid_vector_variable "
                  << "PLUS_ASSIGN cuboid_vector_expression'.";
        
        log_message(cerr_strm);
        cerr_message(cerr_strm);
        cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */@;

   typedef Pointer_Vector<Cuboid> PV;

   PV* pv = static_cast<PV*>(@=$3@>); 

   entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

@q ***** (5) Error handling for the case that |entry == 0|.@>

@ Error handling for the case that |entry == 0 |.
\initials{LDF 2004.12.14.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      cerr_strm << thread_name 
                << "ERROR! In parser rule "
                << "`operation_assignment --> "
                << endl 
                << "cuboid_vector_variable PLUS_ASSIGN "
                << "cuboid_vector_expression':"
                << endl 
                << "`cuboid_vector_variable' is invalid. "
                << "Can't assign to it."
                << endl << "Setting `operation_assignment' to 0, "
                << "deleting `cuboid_vector_expression', "
                << "and will try to continue.";
                
      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");

      delete pv;

      @=$$@> = static_cast<void*>(0); 

    } /* |if (entry == 0)|  */

@q ***** (5) |entry != 0|.@>   

@ |entry != 0|.
\initials{LDF 2004.12.14.}

@<Define rules@>=

  else /* |entry != 0|  */
    {

#if DEBUG_COMPILE
      if (DEBUG)
        {
          cerr_strm << thread_name 
                    << "In parser rule `operation_assignment --> "
                    << endl 
                    << "cuboid_vector_variable PLUS_ASSIGN "
                    << "cuboid_vector_expression':"
                    << endl 
                    << "`cuboid_vector_variable' is valid.";
                
          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str("");
        }
#endif /* |DEBUG_COMPILE|  */@; 

  
      status = vector_type_assign<Cuboid, Cuboid>(scanner_node,
                                                entry,
                                                pv);          

@q ****** (6) Error handling:                                @> 
@q ****** (6) |Scan_Parse::vector_type_assign()| failed.@> 

@ Error handling:  |Scan_Parse::vector_type_assign()| 
failed. 
\initials{LDF 2004.12.14.}

@<Define rules@>=
                            

    if (status != 0)
         {
            cerr_strm << thread_name 
                      << "ERROR! In parser rule "
                      << "`operation_assignment --> "
                      << endl 
                      << "cuboid_vector_variable PLUS_ASSIGN "
                      << "cuboid_vector_expression':"
                      << endl 
                      << "`Scan_Parse::vector_type_assign()' failed."
                      << endl << "Deleting `cuboid_vector_expression' "
                      << "and setting "
                      << "`operation_assignment' to 0.";

            log_message(cerr_strm);
            cerr_message(cerr_strm, error_stop_value);
            cerr_strm.str("");

            delete pv;

            @=$$@> = static_cast<void*>(0);

         } /* |if (status != 0)|  */

@q ****** (6) |Scan_Parse::vector_type_assign()| succeeded.@> 

@ |Scan_Parse::vector_type_assign()| succeeded. 
\initials{LDF 2004.12.14.}

@<Define rules@>=

   else /* |status == 0|  */
      {
         delete pv;

         @=$$@> = static_cast<void*>(0);
 
      }  /* |else| (|status == 0|)  */


   }   /* |else| (|entry != 0|)  */


@q ***** (5).@> 

}; 

@q *** (3) |polyhedron_vectors|.@>  

@*2 \§polyhedron vectors>.
\initials{LDF 2005.01.14.}

\LOG
\initials{LDF 2005.01.14.}
Added this section.
\ENDLOG

@q *** (3) operation_assignment --> polyhedron_vector_variable  @>
@q *** (3) PLUS_ASSIGN polyhedron_expression.                   @>

@*2 \§operation assignment> $\longrightarrow$ \§polyhedron vector variable>
\.{PLUS\_ASSIGN} \§polyhedron expression>.

\LOG
\initials{LDF 2005.01.14.}
Added this rule.
\ENDLOG 

@q **** (4) Definition.@> 

@<Define rules@>=
 
@=operation_assignment: polyhedron_vector_variable PLUS_ASSIGN @>
@=polyhedron_expression@>@/
{

@q ***** (5).@>   

   @<Common declarations for rules@>@;
  
#if DEBUG_COMPILE
   DEBUG = false;  /* |true|  */
   
   if (DEBUG)
      {
        cerr_strm << thread_name 
                  << "*** Parser: `operation_assignment --> "
                  << "polyhedron_vector_variable "
                  << "PLUS_ASSIGN polyhedron_expression'.";
        
        log_message(cerr_strm);
        cerr_message(cerr_strm);
        cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */@;

@q ***** (5) Call |Scan_Parse::vector_type_plus_assign()|.@>   
@ Call |Scan_Parse::vector_type_plus_assign()|.
\initials{LDF 2005.01.14.}

@<Define rules@>=

  status
     = vector_type_plus_assign<Polyhedron>(scanner_node,
                                        static_cast<Id_Map_Entry_Node>(@=$1@>), 
                                        POLYHEDRON_VECTOR,
                                        POLYHEDRON,
                                        static_cast<Polyhedron*>(@=$3@>));



@q ****** (6) |Scan_Parse::vector_type_plus_assign()| failed.@> 
@ |Scan_Parse::vector_type_plus_assign()| failed.
\initials{LDF 2005.01.14.}

@<Define rules@>=

  if (status != 0)
    {
      cerr_strm << thread_name 
                << "ERROR! In parser rule `operation_assignment --> "
                << endl 
                << "polyhedron_vector_variable PLUS_ASSIGN polyhedron_expression':"
                << endl << "`Scan_Parse::vector_type_plus_assign()' "
                << "failed.  Didn't add `polyhedron' to `polyhedron_vector'.";

      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");

    }  /* |if (status != 0)|  */
 
                             
@q ****** (6) |Scan_Parse::vector_type_plus_assign()| succeeded.@> 
@ |Scan_Parse::vector_type_plus_assign()| succeeded.
\initials{LDF 2005.01.14.}

@<Define rules@>=
                                           

  else /* |status == 0|  */
    {

#if DEBUG_COMPILE
      if (DEBUG)
        {
          cerr_strm << thread_name 
                    << "In parser rule `operation_assignment --> "
                    << endl 
                    << "polyhedron_vector_variable "
                    << "PLUS_ASSIGN polyhedron_expression':"
                    << endl << "`Scan_Parse::vector_type_plus_assign()' "
                    << "succeeded.";

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str("");
        }
#endif /* |DEBUG_COMPILE|  */@; 
 

    } /* |else| (|status == 0|)  */ 

@q ***** (5) Set |$$| to 0 and exit rule.@>   

@ Set |@=$$@>| to 0 and exit rule.
\initials{LDF 2005.01.14.}

@<Define rules@>=

  @=$$@> = static_cast<void*>(0);

};

@q * (0) @>

@q   Emacs-Lisp code for use in indirect buffers when using the          @>
@q   GNU Emacs editor.  The local variable list is not evaluated when an @>
@q   indirect buffer is visited, so it's necessary to evaluate the       @>
@q   following s-expression in order to use the facilities normally      @>
@q   accessed via the local variables list.                              @>
@q   \initials{LDF 2004.02.12}.                                          @>
@q   (progn (cweb-mode) (outline-minor-mode t) (setq fill-column 70))    @>




@q Local Variables: @>
@q mode:CWEB  @>
@q eval:(outline-minor-mode t)  @>
@q abbrev-file-name:"~/.abbrev_defs" @>
@q eval:(read-abbrev-file)  @>
@q fill-column:70 @>
@q run-cweave-on-file:"3DLDFprg.web"  @>
@q End: @>
