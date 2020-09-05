@q ploops.w @> 
@q Created by Laurence Finston Sun Sep 12 14:09:17 CEST 2004 @>
     
@q * (0) Copyright and License.@>

@q This file is part of GNU 3DLDF, a package for three-dimensional drawing. @>
@q Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, @>
@q 2011, 2012, 2013 The Free Software Foundation  @>

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




@q * (0) Loops.  @>
@** Loops.  

\LOG
\initials{LDF 2004.09.12.}  
Created this file.
\ENDLOG 

@q ** (2) loop.  @>
@*1 \§loop>.

@<Type declarations for non-terminal symbols@>=
@=%type <int_value> loop@>

@q *** (3) loop --> for_loop.  @>
@*2 \§loop> $\longrightarrow$ \§for-loop>.

\LOG
\initials{LDF 2004.09.12.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>= 
  
@=loop: for_loop@>
{

   @=$$@> = @=$1@>;

};

@q *** (3) loop --> end_loop.  @>
@*2 \§loop> $\longrightarrow$ \§end loop>.

\LOG
\initials{LDF 2004.09.13.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=loop: end_loop@>
{

   @=$$@> = @=$1@>;

};

@q ** (2) |equate_or_assign|.  @>
@*1 \§equate-or-assign>.

@<Type declarations for non-terminal symbols@>=
@=%type <int_value> equate_or_assign@>

@q *** (3) equate_or_assign --> EQUATE.  @>
@*2 \§equate-or-assign> $\longrightarrow$ \.{EQUATE}.

@<Define rules@>= 
  
@=equate_or_assign: EQUATE@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3) equate_or_assign --> ASSIGN.  @>
@*2 \§equate-or-assign> $\longrightarrow$ \.{ASSIGN}.

@<Define rules@>= 
  
@=equate_or_assign: ASSIGN@>@/
{

  @=$$@> = @=$1@>;

};

@q ** (2) for_loop.  @>
@*1 \§for-loop>.
\initials{LDF Undated.}

@<Type declarations for non-terminal symbols@>=
@=%type <int_value> for_loop@>

@q *** (3) for_loop --> FOR symbolic_token equate_or_assign @>
@q *** (3) numeric_expression STEP numeric_expression       @>
@q *** (3) UNTIL numeric_expression COLON                   @>

@ \§for-loop> $\longrightarrow$ 
\.{FOR} \§symbolic token> \§equate or assign> 
\§numeric expression> \.{STEP} \§numeric expression> 
\.{UNTIL} \§numeric expression> \.{COLON}.
\initials{LDF 2004.09.12.}

\LOG
\initials{LDF 2004.09.12.}
Added this rule.

\initials{LDF 2004.09.14.}
Revised this rule.  It now handles nested loops and negative |step|
values correctly. 

\initials{LDF 2004.09.22.}
Now setting |curr_loop_info_node->type| to |Loop_Info_Type::FOR_TYPE|.

\initials{LDF 2004.10.28.}
Removed code from this rule to |Scan_Parse::for_loop_func()|, 
which is defined in \filename{scanprsf.web}.
\ENDLOG

@q **** (4) Definition.@>

@<Define rules@>=
  
@=for_loop: FOR symbolic_token equate_or_assign numeric_expression @>@/
@= STEP numeric_expression UNTIL numeric_expression COLON@>@/
{

@q ***** (5) Call |Scan_Parse::for_loop_func()|.@>

@ Call |Scan_Parse::for_loop_func()|.
\initials{LDF 2004.10.28.}

@<Define rules@>=

         for_loop_func(static_cast<Scanner_Node>(parameter), 
                                    Loop_Info_Type::FOR_TYPE,
                                    @=$2@>,
                                    @=$4@>,
                                    @=$6@>,
                                    @=$8@>);



     @=$$@> = 0;

};


@q *** (3) for_loop --> FOR symbolic_token equate_or_assign @>
@q *** (3) numeric_expression upto_or_downto                @>
@q *** (3) numeric_expression COLON.                        @> 

@ \§for-loop> $\longrightarrow$ 
\.{FOR} \§symbolic token> \§equate or assign> 
\§numeric expression> \§upto or downto> \§numeric expression> 
\.{COLON}.
\initials{LDF 2004.11.26.}

\LOG
\initials{LDF 2004.11.26.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@>

@<Define rules@>=

@=for_loop: FOR symbolic_token equate_or_assign numeric_expression @>@/
@= upto_or_downto numeric_expression COLON@>@/
{

@q ***** (5).@>

   real start_value = @=$4@>;
   real limit_value = @=$6@>;
   real step_value; 


   if (@=$5@> == UPTO)
      step_value = 1;

   else if (@=$5@> == DOWNTO)
      step_value = -1;

   else /* |!(@=$5@> == UPTO || @=$5@> == DOWNTO)|  */
      {

         if (limit_value >= start_value)
            step_value = 1;
         else
            step_value = -1;

      }  /* |else| (|!(@=$5@> == UPTO || @=$5@> == DOWNTO)|)  */   


@q ***** (5) Call |Scan_Parse::for_loop_func()|.@>

@ Call |Scan_Parse::for_loop_func()|.
\initials{LDF 2004.10.28.}

@<Define rules@>=

         for_loop_func(static_cast<Scanner_Node>(parameter),
                                Loop_Info_Type::FOR_TYPE,
                                @=$2@>,
                                start_value,
                                step_value,
                                limit_value);

  @=$$@> = 0;

};

@q *** (3) upto_or_downto.@> 
@*2 \§upto or downto>.
\initials{LDF 2004.11.26.}

\LOG
\initials{LDF 2004.11.26.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <int_value> upto_or_downto@>

@q **** (4) upto_or_downto --> UPTO.@> 
@*3 \§upto or downto> $\longrightarrow$ \.{UPTO}.@> 
\initials{LDF 2004.11.26.}

\LOG
\initials{LDF 2004.11.26.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=upto_or_downto: UPTO@>@/
{
   @=$$@> = UPTO;

};

@q **** (4) upto_or_downto --> DOWNTO.@> 
@*3 \§upto or downto> $\longrightarrow$ \.{DOWNTO}.@> 
\initials{LDF 2004.11.26.}

\LOG
\initials{LDF 2004.11.26.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=upto_or_downto: DOWNTO@>@/
{

   @=$$@> = DOWNTO;

};




@q *** (3) for_loop --> FOREVER COLON.@>

@*2 \§for-loop> $\longrightarrow$ 
\.{FOREVER} \.{COLON}.

\LOG
\initials{LDF 2004.09.22.}
Added this rule.

\initials{LDF 2004.10.28.}
Added |int prev_token|.
\ENDLOG

@q **** (4) Definition.@>

@<Define rules@>=
  
@=for_loop: FOREVER COLON@>@/
{

@q ***** (5) Call |Scan_Parse::for_loop_func()|.@>

@ Call |Scan_Parse::for_loop_func()|.
\initials{LDF 2004.10.28.}

@<Define rules@>=

         for_loop_func(static_cast<Scanner_Node>(parameter),
                                Loop_Info_Type::FOREVER_TYPE);


  @=$$@> = 0;

};




@q *** (3) for_loop --> FOR_SUFFIXES symbolic_token equate_or_assign.@> 

@ \§for-loop> $\longrightarrow$ 
\.{FOR\_SUFFIXES} \§symbolic token> \§equate or assign>.
\initials{LDF 2004.09.22.}

\LOG
\initials{LDF 2004.09.22.}
Added this rule.

\initials{LDF 2005.01.18.}
Started rewriting this rule.
\ENDLOG

@q **** (4) Definition.@>

@<Define rules@>=
  
@=for_loop: FOR_SUFFIXES symbolic_token equate_or_assign@>@/
{

   string s = @=$2@>;


          for_suffixes_loop_func(static_cast<Scanner_Node>(parameter), s);

          @=$$@> = 0;

};


@q *** (3) for_loop --> FOR_SUFFIXES_INTERNAL_START SEMI_COLON.@> 

@*2 \§for-loop> $\longrightarrow$ 
\.{FOR\_SUFFIXES\_INTERNAL\_START} \.{SEMI\_COLON}.
\initials{LDF 2005.01.19.}

\LOG
initials{LDF 2005.01.19.}
Started working on this rule.

\initials{LDF 2005.01.19.}
Now calling |Scan_Parse::for_suffixes_internal_func()| 
in a |try| block and catching |bad_alloc|.
\ENDLOG

@q **** (4) Definition.@>

@<Define rules@>=
  
@=for_loop: FOR_SUFFIXES_INTERNAL_START SEMI_COLON@>@/
{

   for_suffixes_internal_func(static_cast<Scanner_Node>(parameter), 0);

   @=$$@> = 0;

};


@q *** (3) for_loop --> FOR_SUFFIXES_INTERNAL SEMI_COLON.@> 

@*2 \§for-loop> $\longrightarrow$ 
\.{FOR\_SUFFIXES\_INTERNAL} \.{SEMI\_COLON}.
\initials{LDF 2005.01.18.}

\LOG
\initials{LDF 2005.01.18.}
Started working on this rule.

\initials{LDF 2005.01.19.}
Now calling |Scan_Parse::for_suffixes_internal_func()|, which
is defined in \filename{scanprsf.web}.
\ENDLOG

@q **** (4) Definition.@>

@<Define rules@>=
  
@=for_loop: FOR_SUFFIXES_INTERNAL SEMI_COLON@>@/
{


   for_suffixes_internal_func(static_cast<Scanner_Node>(parameter), 1);

   @=$$@> = 0;

};

@q ** (2) end_loop.  @>
@*1 \§end loop>.
\initials{LDF Undated.}

@<Type declarations for non-terminal symbols@>=
@=%type <int_value> end_loop@>

@q *** (3) end_loop --> END_FOR SEMI_COLON.@>
@*2 \§end-loop> $\longrightarrow$ \.{END\_FOR} \.{SEMI\_COLON}.
\initials{LDF 2004.09.22.}

\LOG
\initials{LDF 2004.09.13.}
Added this rule.

\initials{LDF 2004.09.14.}
Revised this rule.  It now handles nested loops 
and negative |step| values correctly. 

\initials{LDF 2004.09.22.}
Added error handling for the case that |Scanner_Type::pop_in()| fails.
Added and edited debugging output.

\initials{LDF 2004.09.22.}
Now testing what type of loop is being ended and proceeding accordingly.

\initials{LDF 2004.10.28.}
@:BUG FIX@> BUG FIX:  Now using 
|*static_cast<real*>(scanner_node->loop_info_node->entry->object)| 
instead of |scanner_node->loop_info_node->start_value|, 
which I've removed.  This makes it possible to change the value of 
the variable declared in the loop header within the loop.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
  
@=end_loop: END_FOR SEMI_COLON@>@/
{

  string rule_name_string 
    = "In `yyparse()', rule `end_loop --> END_FOR SEMI_COLON':";

  Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

@q ***** (5) End |for| loop.@>   
@ End |for| loop.
\initials{LDF 2004.09.22.}

@<Define rules@>=

  if (scanner_node->loop_info_node->type == Loop_Info_Type::FOR_TYPE)
    {

@q ****** (6) Increment |*static_cast<real*>(                @>   
@q ****** (6)  scanner_node->loop_info_node->entry->object)| @>   
@q ****** (6) by |scanner_node->loop_info_node->step_value|. @>   

@ Increment 
|*static_cast<real*>(scanner_node->loop_info_node->entry->object)| 
by |scanner_node->loop_info_node->step_value|
\initials{LDF 2004.10.28.}

@<Define rules@>=

  *static_cast<real*>(scanner_node->loop_info_node->entry->object) 
    += scanner_node->loop_info_node->step_value;

  real start_value = *static_cast<real*>(
                         scanner_node->loop_info_node->entry->object); 

  real limit_value = scanner_node->loop_info_node->limit_value;

  real step_value  = scanner_node->loop_info_node->step_value;


@q ******* (7) We've surpassed the limit.  Ending the loop.@>   

@ We've surpassed the limit.  Ending the loop.
Pop |scanner_node->in| and |scanner_node->loop_info_node|.
\initials{LDF 2004.09.13.}

@<Define rules@>=


  if (   (step_value > 0 && start_value > limit_value)
      || (step_value < 0 && start_value < limit_value))
    {

      @<End loop@>@;

    } /*  |if (   (step_value > 0 && start_value > limit_value)
               || (step_value < 0 && start_value < limit_value))|  */
  

@q ******* (7) We haven't reached the limit yet.  Continuing the loop.@>   

@ We haven't reached the limit yet.  Continuing the loop.
\initials{LDF 2004.09.13.}

@<Define rules@>=

  else /*  |!(   (step_value > 0 && start_value > limit_value)
              || (step_value < 0 && start_value < limit_value))|  */
    {

      scanner_node->in->stream_ptr->seekg(0);

    } /* |else| (|!(   (step_value > 0 && start_value > limit_value)
                    || (step_value < 0 && start_value < limit_value))|)  */


}  /* |if (scanner_node->loop_info_node->type == Loop_Info_Type::FOR_TYPE)|  */

@q ***** (5) End |for_suffixes| loop.@>   

@ End |for_suffixes| loop.
\initials{LDF 2004.09.22.}

@<Define rules@>=


  else if (   scanner_node->loop_info_node->type 
           == Loop_Info_Type::FOR_SUFFIXES_TYPE)
    {


@q ****** (6) |scanner_node->loop_info_node->suffix_list.size() == 0|.@> 

@ |scanner_node->loop_info_node->suffix_list.size() == 0|.
\initials{LDF 2005.01.19.}

\LOG
\initials{LDF 2005.01.19.}
Added this section.
\ENDLOG

@<Define rules@>=

   if (scanner_node->loop_info_node->suffix_list.size() == 0)
      {    

          Loop_Info_Node temp_loop_info_node = scanner_node->loop_info_node; 

          scanner_node->loop_info_node = scanner_node->loop_info_node->up;
      
          if (temp_loop_info_node != static_cast<Loop_Info_Node>(0))

            delete temp_loop_info_node;

          temp_loop_info_node = 0;

      }  /* |if (scanner_node->loop_info_node->suffix_list.size() == 0)|  */

@q ****** (6) |scanner_node->loop_info_node->suffix_list.size() != 0|.@> 
@ |scanner_node->loop_info_node->suffix_list.size() != 0|.
\initials{LDF 2005.01.19.}

\LOG
\initials{LDF 2005.01.19.}
Added this section.

\initials{LDF 2005.01.20.}
Now pushing |SEMI_COLON| and |FOR_SUFFIXES_INTERNAL|
onto |scanner_node->rescan_stack| instead of 
calling 
|scanner_node->in->stream_ptr->seekg(scanner_node->loop_info_node->skip_value)|.
\ENDLOG

@<Define rules@>=

   else  /* |scanner_node->loop_info_node->suffix_list.size() != 0|  */
      {

          Int_Void_Ptr_Bool ivpb(SEMI_COLON, 0, false);
          scanner_node->rescan_stack.push(ivpb);

          ivpb.set(FOR_SUFFIXES_INTERNAL, 0, false);
          scanner_node->rescan_stack.push(ivpb);

      }  /* |else| (|scanner_node->loop_info_node->suffix_list.size() != 0|)  */

@q ****** (6).@> 

    } /* |else if (   scanner_node->loop_info_node->type
                   == Loop_Info_Type::FOR_SUFFIXES_TYPE)|  */

@q ***** (5) Continue |forever| loop.@>   

@ Continue |forever| loop.
\initials{LDF 2004.09.22.}

@<Define rules@>=

  else if (scanner_node->loop_info_node->type == Loop_Info_Type::FOREVER_TYPE)
    {

      scanner_node->in->stream_ptr->seekg(0);

    } /* |else if (   scanner_node->loop_info_node->type
                   == Loop_Info_Type::FOREVER_TYPE)|  */



@q ***** (5) Error handling:  Invalid loop type.@>   

@ Error handling:  Invalid loop type.
\initials{LDF 2004.09.22.}

@<Define rules@>=
  
  else 
    {
      return 1;

    } /* |else|  */


@q **** (4) Set |$$| and exit rule successfully.@>   

@ Set |@=$$@>| and exit rule successfully.
\initials{LDF 2004.09.22.}

@<Define rules@>=


  @=$$@> = 0;

};


@q *** (3) end_loop --> exit_if_or_exit_unless boolean_expression SEMI_COLON.@>
@*2 \§exit-loop> $\longrightarrow$ 
\§exit-if or exit-unless> \§boolean expression> \.{SEMI\_COLON}.
\initials{LDF 2004.09.22.}

\LOG
\initials{LDF 2004.09.22.}
Added this rule.

\initials{LDF 2004.10.27.}
@:BUG FIX@> BUG FIX:  Changed the way |boolean_expression| is handled
to account for the fact that its type is now |pointer_value| rather
than |int_value|.  If I change its type back, which I'm considering, I
will have to change this code back, too.
\ENDLOG

@<Define rules@>=
  
@=end_loop: exit_if_or_exit_unless boolean_expression SEMI_COLON@>@/
{

  bool* b = static_cast<bool*>(@=$2@>);
  Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);
 

  if (@=$1@> == EXIT_UNLESS)
    *b = !(*b);

@q ***** (5) |*b == true|.  End the loop.@>   

@ |*b == true|.  End the loop.
Pop |scanner_node->in| and |scanner_node->loop_info_node|.
\initials{LDF 2004.09.22.}

@<Define rules@>=

  if (*b)
    {


      int status = scanner_node->pop_in();


@q ****** (6) Error handling:  |Scanner_Type::pop_in()| failed.@> 

@ Error handling:  |Scanner_Type::pop_in()| failed.
\initials{LDF 2004.09.22.}

@<Define rules@>=

  if (status != 0)
    {

      return 1;

    } /* |if (status != 0)|  */
                


@q ****** (6) |Scanner_Type::pop_in()| succeeded.@> 

@ |Scanner_Type::pop_in()| succeeded.
\initials{LDF 2004.09.22.}

\LOG
\initials{LDF 2004.09.22.}
Added this section.
\ENDLOG

@<Define rules@>=


  Loop_Info_Node temp_loop_info_node = scanner_node->loop_info_node;


  scanner_node->loop_info_node = scanner_node->loop_info_node->up;
      
      if (temp_loop_info_node != static_cast<Loop_Info_Node>(0))

        delete temp_loop_info_node;

      temp_loop_info_node = 0;
      

      Int_Void_Ptr_Bool ivp(END_GROUP, 0, false);
      scanner_node->rescan_stack.push(ivp);



    } /* |if (*b)| */

  
  
@q ***** (5) |*b == false|.  Continue looping.@>   

@ |*b == false|.  Continue looping.
\initials{LDF 2004.09.22.}

@<Define rules@>=

  delete b;

  @=$$@> = 0;

};



@q *** (3) End loop.@> 
@ End loop.
\initials{LDF 2004.09.22.}

\LOG
\initials{LDF 2004.09.22.}
Added this section.
\ENDLOG 

@<End loop@>=
{ /* Beginning of ``End loop'' group.  */
  
  int status = static_cast<Scanner_Node>(parameter)->pop_in();

@q **** (4) Error handling:  |Scanner_Type::pop_in()| failed.@> 

@ Error handling:  |Scanner_Type::pop_in()| failed.
\initials{LDF 2004.09.22.}

\LOG
\initials{LDF 2004.09.22.}
Added this section.
\ENDLOG

@<End loop@>=

  if (status != 0)
    {

      return 1;

    } /* |if (status != 0)|  */
                


@q **** (4) |Scanner_Type::pop_in()| succeeded.@> 

@ |Scanner_Type::pop_in()| succeeded.
\initials{LDF 2004.09.22.}

\LOG
\initials{LDF 2004.09.22.}
Added this section.
\ENDLOG

@<End loop@>=


      Loop_Info_Node temp_loop_info_node = scanner_node->loop_info_node;


      scanner_node->loop_info_node = scanner_node->loop_info_node->up;
      
      if (temp_loop_info_node != static_cast<Loop_Info_Node>(0))

        delete temp_loop_info_node;

      temp_loop_info_node = 0;


      Int_Void_Ptr_Bool ivp(END_GROUP, 0, false);
      scanner_node->rescan_stack.push(ivp);

} /* End of ``End loop'' group.  */




@q *** (3) exit_if_or_exit_unless.@>
@*2 \§exit-if or exit-unless>.
\initials{LDF 2004.09.22.}

\LOG
\initials{LDF 2004.09.22.}
Added this type declaration.
\ENDLOG 

@<Type declarations for non-terminal symbols@>=
@=%type <int_value> exit_if_or_exit_unless@>


@q **** (4) exit_if_or_exit_unless --> EXIT_IF.@>
@*3 \§exit-if or exit-unless> $\longrightarrow$ \.{EXIT\_IF}.
\initials{LDF 2004.09.22.}

\LOG
\initials{LDF 2004.09.22.}
Added this rule.
\ENDLOG

@<Define rules@>=
  
@=exit_if_or_exit_unless: EXIT_IF@>@/
{

  @=$$@> = EXIT_IF;

};

@q **** (4) exit_if_or_exit_unless --> EXIT_UNLESS.@>
@*2 \§exit-if or exit-unless> $\longrightarrow$ \.{EXIT\_UNLESS}.
\initials{LDF 2004.09.22.}

\LOG
\initials{LDF 2004.09.22.}
Added this rule.
\ENDLOG

@<Define rules@>=
  
@=exit_if_or_exit_unless: EXIT_UNLESS@>@/
{

  @=$$@> = EXIT_UNLESS;

};

@q * @>

@q   Emacs-Lisp code for use in indirect buffers when using the          @>
@q   GNU Emacs editor.  The local variable list is not evaluated when an @>
@q   indirect buffer is visited, so it's necessary to evaluate the       @>
@q   following s-expression in order to use the facilities normally      @>
@q   accessed via the local variables list.                              @>
@q   \initials{LDF 2004.02.12}.                                          @>
@q   (progn (cweb-mode) (outline-minor-mode t) (setq fill-column 80))    @>


  

@q * (0) Local variables for Emacs.@>

@q Local Variables: @>
@q mode:CWEB @>
@q eval:(display-time) @>
@q run-cweave-on-file:"3DLDFprg.web" @>
@q eval:(read-abbrev-file) @>
@q indent-tabs-mode:nil @>
@q eval:(outline-minor-mode) @>
@q End: @>
