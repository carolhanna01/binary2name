@q pvcmncmd.w @> 
@q Created by Laurence Finston Fri Jul 27 17:18:40 CEST 2007 @>
     
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



@q * (0) Vector Maninpulation Commands.  @>
@** Vector Maninpulation Commands.  

\LOG
\initials{LDF 2007.07.27.}
Created this file.  It contains code 
formerly in \filename{pcommand.w}.
\ENDLOG

@q ** (2) Commands for all vector-type variables.@> 
@*1 Commands for all vector-type variables.
\initials{LDF 2007.07.27.}

\LOG
\initials{LDF 2007.07.27.}
Added this section.
\ENDLOG

@q *** (3) command --> POP_BACK vector_type_variable.@> 
@*2 \§command> $\longrightarrow$ \.{POP\_BACK}
\§vector-type variable>.
\initials{LDF 2005.01.05.}

\LOG
\initials{LDF 2005.01.05.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: POP_BACK vector_type_variable@>@/
{

   int status = pop_back_func(static_cast<Scanner_Node>(parameter),
                              static_cast<Id_Map_Entry_Node>(@=$2@>)); 
   
@q ***** (5)@> 

   if (status != 0)
      {

#if 0
          cerr_strm << thread_name << "ERROR!  In `yyparse()', rule "
                    << "`command "
                    << "--> POP_BACK vector_type_variable':"
                    << endl << "`Scan_Parse::pop_back_func()' failed, "
                    << "returning " << status << "."
                    << endl << "Will try to continue.";

          log_message(cerr_strm);
          cerr_message(cerr_strm, error_stop_value);
          cerr_strm.str("");
#endif 
      }  /* |if (status != 0)|  */

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) Commands for point-vector variables.@> 
@*1 Commands for {\bf point-vector} variables.
\initials{LDF 2007.07.27.}

\LOG
\initials{LDF 2007.07.27.}
Added this section.
\ENDLOG

@q *** (3) command --> CULL point_vector_variable.@> 
@*2 \§command> $\longrightarrow$ \.{CULL}
\§point-vector variable>.
\initials{LDF 2007.07.22.}

\LOG
\initials{LDF 2007.07.22.}
Added this rule.

\initials{LDF 2007.07.23.}
Now passing |Sorting::SORTING_EQUAL| to |Scan_Parse::cull_func|.

\initials{LDF 2007.07.27.}
Added \§with tolerance optional> to right-hand side.  Passing its (|real|) 
value to |Scan_Parse::cull_func|.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: CULL point_vector_variable with_tolerance_optional@>@/
{

   int status =  Scan_Parse::cull_func<Point>(@=$2@>, 
                                Sorting::SORTING_EQUAL,               
                                static_cast<Scanner_Node>(
                                   parameter),
                                @=$3@>);

   @=$$@> = static_cast<void*>(0);

};


@q *** (3) command --> RECTIFY point_vector_variable.@> 
@*2 \§command> $\longrightarrow$ \.{RECTIFY}
\§point-vector variable>.
\initials{LDF 2007.07.27.}

\LOG
\initials{LDF 2007.07.27.}
Added this rule.
\ENDLOG

\TODO
@q { @>
@:TO DO}{{\bf TO DO}@>
@q } @> 
@q !! TO DO:  @>
\initials{LDF 2007.08.06.}
Change the way the options are handled.
\ENDTODO 

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: RECTIFY point_vector_variable with_test_optional @>@/ 
@=with_cull_optional with_tolerance_optional@>@/
{
   bool do_test = (@=$3@> != WITH_NO_TEST) ? true : false;
   bool do_cull = (@=$4@> != WITH_NO_CULL) ? true : false;

/* START HERE!!  LDF 2007.08.06.  Change options.  Use list.  */ 

   int status =  Scan_Parse::rectify_func<Point>(@=$2@>, 
                                static_cast<Scanner_Node>(
                                   parameter),
                                do_test,     
                                do_cull,
                                true,
                                @=$5@>,
                                static_cast<Transform*>(0),
                                static_cast<Point*>(0));

   @=$$@> = static_cast<void*>(0);

};


@q ** (2) Commands for picture-vector variables.@> 
@*1 Commands for {\bf picture-vector} variables.
\initials{LDF 2007.07.27.}

\LOG
\initials{LDF 2007.07.27.}
Added this section.
\ENDLOG


@q *** (3) command --> picture_vector_variable @>
@q *** (3) PYTHAGOREAN_PLUS.                   @> 

@*2 \§command> $\longrightarrow$ 
\§picture vector variable> \.{PYTHAGOREAN\_PLUS}.
\initials{LDF 2005.01.15.}

\LOG
\initials{LDF 2005.01.15.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=command: picture_vector_variable PYTHAGOREAN_PLUS@>@/
{

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

   Pointer_Vector<Picture>* pv;

@q ***** (5)@> 

   if (entry == static_cast<Id_Map_Entry_Node>(0))
      {
#if 0 
          cerr_strm << thread_name << "ERROR!  In `yyparse()', rule "
                    << "`command -->"
                    << endl 
                    << "picture_vector_variable PYTHAGOREAN_PLUS':"
                    << endl << "`picture_vector_variable' is invalid.  "
                    << "Not incrementing counter.  Will try to continue.";

          log_message(cerr_strm);
          cerr_message(cerr_strm, error_stop_value);
          cerr_strm.str("");
#endif 

      }  /* |if (entry == 0)|  */
    
@q ***** (5)@> 
 
   else /* |entry != 0|  */
      {

@q ****** (6)@> 

          if (entry->object == static_cast<void*>(0))
             {
              
                         pv  = new Pointer_Vector<Picture>;
               

@q ******* (7)@> 

                  entry->object = static_cast<void*>(pv);  

             }  /* |else if (entry->object == 0)|  */

@q ****** (6)@> 

          pv = static_cast<Pointer_Vector<Picture>*>(entry->object);

          stringstream name_strm;          
          stringstream root_strm;          

          name_strm << entry->name << "[" << pv->ctr << "]";
          root_strm << entry->name << "£";
          Id_Map_Entry_Node root = static_cast<Scanner_Node>(parameter)->lookup(root_strm.str());

@q ******* (7)@> 

          if (root == static_cast<Id_Map_Entry_Node>(0))
             {
#if 0 
                 cerr_strm << thread_name 
                           << "ERROR!  In `yyparse()', rule "
                           << "`command -->"
                           << endl 
                           << "picture_vector_variable PYTHAGOREAN_PLUS':"
                           << endl
                           << "root' == 0, i.e., no `Id_Map_Entry_Node' exists "
                           << "for `" << root_strm.str() << "'."
                           << endl << "Will try to continue.";

                 log_message(cerr_strm);
                 cerr_message(cerr_strm, error_stop_value);
                 cerr_strm.str("");
#endif 

             }  /* |if (root == 0)|  */

@q ******* (7)@> 

         else /* |root != 0|  */
            {

                Id_Map_Entry_Node temp_entry 
                    = static_cast<Scanner_Node>(parameter)->get_array_entry(name_strm.str(), 
                                                    root);
@q ******** (8)@> 

                if (temp_entry == static_cast<Id_Map_Entry_Node>(0))
                   {
#if 0 
                       cerr_strm << thread_name 
                           << "ERROR!  In `yyparse()', rule "
                           << "`command -->"
                           << endl 
                           << "picture_vector_variable PYTHAGOREAN_PLUS':"
                           << endl
                           << "`Scanner_Type::get_array_entry()' failed, "
                           << "returning 0.  "
                           << "Will try to continue.";

                       log_message(cerr_strm);
                       cerr_message(cerr_strm, error_stop_value);
                       cerr_strm.str("");
#endif 

                   }  /* |if (temp_entry == 0)|  */


@q ******** (8)@> 

               else  /* |temp_entry != 0|  */
                  {
                      Picture* p;

@q ********* (9).@>  
                      if (temp_entry->object == static_cast<void*>(0))
                         {
                              Picture* p;
                        

                              p = new Picture;
                              p->clear();

                              temp_entry->object = static_cast<void*>(p);

@q ********** (10).@> 
            
                         }  /* |if (temp_entry->object == 0)|  */
                         

@q ********* (9).@>  

                       p = static_cast<Picture*>(temp_entry->object);

                       pv->v.push_back(p);

                       ++pv->ctr;

                  }  /* |else| (|temp_entry != 0|)  */





@q ******** (8)@> 

   }   /* |else| (|root != 0|)  */

@q ******* (7)@> 

@q ****** (6)@> 

      }  /* |else| (|entry != 0|)  */

@q ***** (5)@> 

   @=$$@> = static_cast<void*>(0);

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
@q fill-column:70 @>
@q run-cweave-on-file:"3DLDFprg.web"  @>
@q End: @>
