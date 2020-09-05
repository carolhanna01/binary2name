@q precexpr.w @> 
@q Created by Laurence Finston Wed Jun 30 20:26:42 CEST 2004  @>
       
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

@q ("@@" stands for a single at-sign in the following paragraph.) @>

@q Please send bug reports to bug-3dldf@@gnu.org @>
@q The mailing list help-3dldf@@gnu.org is available for people to @>
@q ask other users for help.  @>
@q The mailing list info-3dldf@@gnu.org is for sending @>
@q announcements to users. To subscribe to these mailing lists, send an @>
@q email with "subscribe <email-address>" as the subject.  @>

@q The author can be contacted at: @>

@q Laurence D. Finston                 @> 
@q c/o Free Software Foundation, Inc.  @>
@q 51 Franklin St, Fifth Floor         @> 
@q Boston, MA  02110-1301              @>
@q USA                                 @>

@q Laurence.Finston@@gmx.de (@@ stands for a single "at" sign.)@>



@q * rectangle expressions.  @>
@** rectangle expressions.

\LOG
\initials{LDF 2004.06.30.}
Created this file.
\ENDLOG 


@q ** (2) rectangle primary.  @>
@*1 \§rectangle primary>.
  
\LOG
\initials{LDF 2004.06.30.}  
Added this section.
\ENDLOG 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> rectangle_primary@>@/

@q *** (3) rectangle_primary --> rectangle_variable.@>
@*2 \§rectangle primary> $\longrightarrow$ \§rectangle variable>.  

\LOG
\initials{LDF 2004.06.30.}
Added this rule.

\initials{LDF 2004.11.23.}
No longer issuing an error message if |entry->object == 0|.
This condition occurs legitimately when one tries to show
an ``unknown |rectangle|''.

\initials{LDF 2007.09.30.}
Removed code from this rule.
\ENDLOG 

@<Define rules@>=
@=rectangle_primary: rectangle_variable@>@/
{

  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

  if (entry == static_cast<Id_Map_Entry_Node>(0) || entry->object == static_cast<void*>(0))
    {

      @=$$@> = static_cast<void*>(0);

    } /* |if (entry == 0 || entry->object == 0)|  */

  else /* |entry != 0 && entry->object != 0|  */

    @=$$@> = static_cast<void*>(create_new<Rectangle>(
                                  static_cast<Rectangle*>(
                                     entry->object))); 
};

@q *** (3) rectangle_primary --> rectangle_argument.@>
@*2 \§rectangle primary> $\longrightarrow$ \§rectangle argument>.  


@q *** (3) rectangle_primary --> ( rectangle_expression )  @>
@*2 \§rectangle primary> $\longrightarrow$ \.{\LP} 
\§rectangle expression> \.{\RP}.

\LOG
\initials{LDF 2004.06.30.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=rectangle_primary: LEFT_PARENTHESIS rectangle_expression RIGHT_PARENTHESIS@>@/
{

  @=$$@> = @=$2@>;

};

@q ** (2) rectangle_primary --> GET_RECTANGLE numeric_secondary @>
@q ** (2) cuboid_primary                                        @>
@*1 \§rectangle primary> $\longrightarrow$ \.{GET\_RECTANGLE}
\§numeric secondary> \§cuboid primary>. 

\LOG
\initials{LDF 2004.10.13.}
Added this rule.

\initials{LDF 2005.06.27.}
Changed \.{INTEGER} to \§numeric expression>.  Added |real q| and 
|unsigned short u|.

\initials{LDF 2005.06.27.}
Changed \§numeric expression> to \§numeric secondary> and 
\§solid-like primary> to \§cuboid primary>.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=rectangle_primary: GET_RECTANGLE numeric_secondary cuboid_primary@>@/
{

  @<Common declarations for rules@>@; 

  #if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
    if (DEBUG) 
      {
        cerr_strm << thread_name << "*** Parser:  "
                  << "'rectangle_primary --> "
                  << "GET_RECTANGLE numeric_secondary cuboid_primary'.";
        
        log_message(cerr_strm);
        cerr_message(cerr_strm);
        cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */

  Cuboid* s = static_cast<Cuboid*>(@=$3@>);

  Rectangle* r;

  real q = @=$2@>;

  unsigned short u = static_cast<unsigned short>(floor(fabs(q) + .5)); 

  const Rectangle* t = s->get_rectangle_ptr(u); 


@q **** (4) Error handling:  |Solid::get_rectangle_ptr()| returned 0.@>   

@ Error handling:  |Solid::get_rectangle_ptr()| returned 0.
\initials{LDF 2004.10.13.}

@<Define rules@>=

  if (t == static_cast<const Rectangle*>(0))
   {
      cerr_strm << thread_name << "ERROR! In `yyparse()', rule "
                << "`rectangle_primary --> GET_RECTANGLE "
                << "INTEGER cuboid_primary':"
                << endl 
                << "`Solid::get_rectangle_ptr()' failed, "
                << "returning 0."
                << endl << "Deleting `cuboid_primary' and "
                << "setting `rectangle_primary' to 0.";

      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");

      delete s;

      @=$$@> = static_cast<void*>(0);

   } /* |if (t == 0)|  */


@q **** (4) Try to allocate memory on free store for |rectangle_primary|.@>   

@ Try to allocate memory on free store for \§rectangle primary>.
\initials{LDF 2004.10.13.}

@<Define rules@>=

  else /* |t != 0|  */
     {
        try
          {
            r = create_new<Rectangle>(t, scanner_node);
          }


@q ***** (5) Error handling:  |create_new<Rectangle>()| failed.@> 

@ Error handling:  |create_new<Rectangle>()| failed.
\initials{LDF 2004.10.13.}

@<Define rules@>=

        catch (bad_alloc)
           {             
      
               cerr_strm << thread_name << "ERROR! In `yyparse()', rule "
                         << "`rectangle_primary --> GET_RECTANGLE "
                         << "INTEGER cuboid_primary':"
                         << endl 
                         << "Failed to allocate memory for "
                         << "`rectangle_primary'."
                         << endl << "Deleting `cuboid_primary' and "
                         << "rethrowing `bad_alloc'.";

               log_message(cerr_strm);
               cerr_message(cerr_strm, error_stop_value);
               cerr_strm.str("");
        
               delete s;

               throw;

            } /*        |catch (bad_alloc)|  */



@q **** (4) Delete |s|, set |rectangle_primary| to              @>   
@q **** (4) |static_cast<void*>(r)|, and exit rule successfully.@>   

@ Delete |s|, set |rectangle_primary| to                 
|static_cast<void*>(r)|, and exit rule successfully.   
\initials{LDF 2004.10.13.}

@<Define rules@>=

  delete s;

  @=$$@> = static_cast<void*>(r);

  }  /* |else| (|t != 0|)  */

};






@q ** (2) rectangle_primary --> IN_RECTANGLE ellipse_primary@>
@*1 \§rectangle primary> $\longrightarrow$ \.{IN\_RECTANGLE}
\§rectangle primary>. 

\LOG
\initials{LDF 2004.11.20.}
Added this rule.

\initials{LDF 2007.09.30.}
Removed error handling code from the action for this rule.  
Edited the remaining code.  
@:BUG FIX@> BUG FIX:  
Now deleting |Ellipse* e|.  
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=rectangle_primary: IN_RECTANGLE ellipse_primary@>@/
{

   Ellipse* e = static_cast<Ellipse*>(@=$2@>);

   if (e)
   {
      Rectangle* r = create_new<Rectangle>(e->in_rectangle());
      delete e;
      @=$$@> = static_cast<void*>(r); 
   }
   else 
      @=$$@> = static_cast<void*>(0);

};

@q ** (2) rectangle_primary --> OUT_RECTANGLE ellipse_primary@>
@*1 \§rectangle primary> $\longrightarrow$ \.{OUT\_RECTANGLE}
\§rectangle primary>. 
\initials{LDF 2004.11.20.}

\LOG
\initials{LDF 2004.11.20.}
Added this rule.

\initials{LDF 2007.09.30.}
Removed error handling code from the action for this rule.  
Edited the remaining code.  
@:BUG FIX@> BUG FIX:  
Now deleting |Ellipse* e|.  
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=rectangle_primary: OUT_RECTANGLE ellipse_primary@>@/
{

   Ellipse* e = static_cast<Ellipse*>(@=$2@>);
   
   if (e)
   {
      Rectangle* r = create_new<Rectangle>(e->out_rectangle());
      delete e;
      @=$$@> = static_cast<void*>(r); 
   }
   else
      @=$$@> = static_cast<void*>(0);

};


@q ** (2) rectangle_primary --> OUT_RECTANGLE circle_primary@>
@*1 \§rectangle primary> $\longrightarrow$ \.{OUT\_RECTANGLE}
\§rectangle primary>. 
\initials{LDF 2007.09.30.}

\LOG
\initials{LDF 2007.09.30.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=rectangle_primary: OUT_RECTANGLE circle_primary@>@/
{

   Circle* c = static_cast<Circle*>(@=$2@>);

   Rectangle* r = create_new<Rectangle>(c->out_rectangle());

   delete c;
   
   @=$$@> = static_cast<void*>(r); 

};


@q ***** (5) rectangle_primary --> LAST @>
@q ***** (5) rectangle_vector_expression.@>

@*4 \§rectangle primary> $\longrightarrow$ 
\.{LAST} \§rectangle vector expression>.
\initials{LDF 2005.01.14.}

\LOG
\initials{LDF 2005.01.14.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=rectangle_primary: LAST rectangle_vector_expression@>@/
{ 
   @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
   DEBUG = false; /* |true| */ @; 
   if (DEBUG) 
     {
         cerr_strm << thread_name << "*** Parser:  `rectangle_primary "
                   << "--> LAST rectangle_vector_expression'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str("");
     }
#endif /* |DEBUG_COMPILE|  */

@q ******* (7) @> 

   Rectangle* r;

   try 
      {
         r = create_new<Rectangle>(0);
      }

   catch (bad_alloc)
      {
         cerr_strm << thread_name << "ERROR!  In `yyparse()', rule "
                   << "`rectangle_primary "
                   << "--> LAST rectangle_vector_expression':"
                   << endl << "`create_new<Rectangle>()' failed.  "
                   << "Rethrowing `bad_alloc'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm, error_stop_value);
         cerr_strm.str("");

         throw;

      }  /* |catch (bad_alloc)|  */

@q ******* (7) @> 

   Pointer_Vector<Rectangle>* pv 
      = static_cast<Pointer_Vector<Rectangle>*>(@=$2@>);

@q ******* (7) Error handling:  |pv == 0|.@> 

@ Error handling:  |pv == 0|.
\initials{LDF 2005.01.14.}

@<Define rules@>=

   if (pv == static_cast<Pointer_Vector<Rectangle>*>(0))
      {
          cerr_strm << thread_name 
                    << "ERROR!  In `yyparse()', rule "
                    << endl 
                    << "`rectangle_primary "
                    << "--> LAST rectangle_vector_expression':"
                    << endl << "Invalid `rectangle_vector_expression'.  "
                    << "Setting `rectangle_primary' to 0"
                    << endl << "and will try to continue.";

          log_message(cerr_strm);
          cerr_message(cerr_strm, error_stop_value);
          cerr_strm.str("");

          delete r;

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
                    << "`rectangle_primary "
                    << "--> LAST rectangle_vector_expression':"
                    << endl << "`rectangle_vector_expression' is empty.  "
                    << "Setting `rectangle_primary' to 0"
                    << endl << "and will try to continue.";

          log_message(cerr_strm);
          cerr_message(cerr_strm, error_stop_value);
          cerr_strm.str("");

          delete r;

          @=$$@> = static_cast<void*>(0);

      }  /* |else if (pv->ctr == 0)|  */

@q ******* (7) |pv != 0 && pv->ctr > 0|.@> 

@ |pv != 0 && pv->ctr > 0|.  Set |@=$$@>| to |*(pv->v[pv->ctr - 1])|.
\initials{LDF 2005.01.14.}

@<Define rules@>=

   else 
      {
         *r = *(pv->v[pv->ctr - 1]);
         @=$$@> = static_cast<void*>(r); 
      }
@q ******* (7) @> 

};


@q *** (3) rectangle_primary --> LEFT_PARENTHESIS point_expression COMMA   @>
@q *** (3) point_expression COMMA point_expression COMMA  point_expression @>
@q *** (3) RIGHT_PARENTHESIS.                                              @>

@*2 \§rectangle primary> $\longrightarrow$ etc.
%
\§rectangle primary> $\longrightarrow$ \.{\LP} 
\§point expression>  \.{COMMA} \§point expression>  \.{COMMA}
\§point expression>  \.{COMMA} \§point expression> \.{\RP}.
\initials{LDF 2007.09.30.}

\LOG
\initials{LDF 2007.09.30.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=rectangle_primary: LEFT_PARENTHESIS point_expression COMMA@>@/
@=point_expression COMMA point_expression COMMA point_expression@>@/
@=RIGHT_PARENTHESIS@>@/
{

   Point* p0 = static_cast<Point*>(@=$2@>);
   Point* p1 = static_cast<Point*>(@=$4@>);
   Point* p2 = static_cast<Point*>(@=$6@>);
   Point* p3 = static_cast<Point*>(@=$8@>);
  
   if (p0 && p1 && p2 && p3)
   {
      Rectangle r(*p0, *p1, *p2, *p3);

      if (r.size() != 4)
         @=$$@> = static_cast<void*>(0);
      else
         @=$$@> = static_cast<void*>(create_new<Rectangle>(r)); 

   }  /* Outer |if|  */

   else
   {
      @=$$@> = static_cast<void*>(0);
   }

   delete p0;
   delete p1;
   delete p2;
   delete p3;

};



@q ** (2) rectangle secondary.  @>
@*1 \§rectangle secondary>.


\LOG
\initials{LDF 2004.06.30.}  
Added this section.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> rectangle_secondary@>
  
@q *** rectangle secondary --> rectangle_primary.  @>
@ \§rectangle secondary> $\longrightarrow$ \§rectangle primary>.

\LOG
\initials{LDF 2004.06.30.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=rectangle_secondary: rectangle_primary@>@/
{
#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
      cerr << "\n*** Parser: rectangle_secondary --> rectangle_primary "
           << endl;
    }
#endif /* |DEBUG_COMPILE|  */@;

  @=$$@> = @=$1@>;

};

@q *** (3) rectangle secondary --> rectangle_secondary transformer.@>
@*2 \§rectangle secondary> $\longrightarrow$ 
\§rectangle secondary> \§transformer>.

\LOG
\initials{LDF 2004.06.30.}
Added this rule.

\initials{LDF 2006.01.20.}
@:BUG FIX@> BUG FIX:  Now deleting |Transform* t|.
\ENDLOG

@<Define rules@>=
@=rectangle_secondary: rectangle_secondary transformer@>@/
{

  Rectangle* p = static_cast<Rectangle*>(@=$1@>);
  Transform* t = static_cast<Transform*>(@=$2@>);

  *p *= *t;

  delete t;

  @=$$@> = static_cast<void*>(p); 

};

@q *** (3) rectangle secondary --> rectangle_secondary REFLECTED_IN @>
@q *** (3) path_expression.@> 

@*2 \§rectangle secondary> $\longrightarrow$ 
\§rectangle secondary> 
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
@=rectangle_secondary: rectangle_secondary REFLECTED_IN @>
@=path_expression@>@/ 
{

    Rectangle* p = reflect_in_func<Rectangle>(static_cast<Scanner_Node>(parameter),
                                            static_cast<Rectangle*>(@=$1@>),
                                            static_cast<Path*>(@=$3@>));

  
    @=$$@> = static_cast<void*>(p);

};



@q ** (2)  rectangle tertiary.  @>
@*1 \§rectangle tertiary>.


\LOG
\initials{LDF 2004.06.30.}  
Added this section.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> rectangle_tertiary@>

@q *** rectangle tertiary --> rectangle_secondary.  @>
@ \§rectangle tertiary> $\longrightarrow$ \§rectangle secondary>.


\LOG
\initials{LDF 2004.06.30.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=rectangle_tertiary: rectangle_secondary@>@/
{
#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
      cerr << "\n*** Parser: rectangle_tertiary --> rectangle_secondary "
           << endl;
    }
#endif /* |DEBUG_COMPILE|  */@;

  @=$$@> = @=$1@>;

}
;



@q ** rectangle expression.  @>
@ \§rectangle expression>.

\LOG
\initials{LDF 2004.06.30.}  
Added this section.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> rectangle_expression@>

@q *** rectangle expression --> rectangle_tertiary.  @>
@ \§rectangle expression> $\longrightarrow$ \§rectangle tertiary>.

\LOG
\initials{LDF 2004.06.30.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=rectangle_expression: rectangle_tertiary@>@/
{
#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
      cerr << "\n*** Parser: rectangle_expression --> rectangle_tertiary "
           << endl;
    }
#endif /* |DEBUG_COMPILE|  */@;

  @=$$@> = @=$1@>;
}
;


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

