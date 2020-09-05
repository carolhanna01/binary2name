@q ptrnexpr.w @> 
@q Created by Laurence Finston Thu May  6 16:13:44 MEST 2004  @>
     
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



@q * (0) transform expressions.  @>
@** transform expressions.

\TODO
@q { @>
@:TO DO}{{\bf TO DO}@>
@q } @> 
@q !! TO DO:  @>
\initials{LDF 2004.05.13.}
Add rules for \§transformer> $\longrightarrow$ \§rotated around> using
\§path expression> once I've defined rules for `\§path expressions>.

\initials{LDF 2004.05.13.}
Start adding rules for reflection once I've defined rules for |plane|.  I'll
need to define |Path::reflect_in()|, etc., too.
\ENDTODO 

@q ** (2) transform primary.  @>
@*1 \§transform primary>.
  
@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> transform_primary@>@/

@q *** (3) transform_primary --> transform_variable.@>
@*2 \§transform primary> $\longrightarrow$ \§transform variable>.  

\LOG
\initials{LDF 2004.09.28.}
Made debugging output thread-safe.  Formatting changes. 

\initials{LDF 2004.11.22.}
No longer issuing an error message if |entry->object == 0|.
This condition occurs legitimately when one tries to show
an ``unknown |transform|''.
\ENDLOG 

@<Define rules@>=
@=transform_primary: transform_variable@>@/
{

  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>); 

  if (entry == static_cast<Id_Map_Entry_Node>(0) || entry->object == static_cast<void*>(0))
    {

      @=$$@> = static_cast<void*>(0);

    } /* |if (entry == 0 || entry->object == 0)|  */

  else /* |entry != 0 && entry->object != 0|  */

    @=$$@>
      = static_cast<void*>(
            create_new<Transform>(
                 static_cast<Transform*>(entry->object))); 

};

@q *** (3) transform_primary --> transform_argument.@>
@*2 \§transform primary> $\longrightarrow$ \§transform argument>.  

@q *** (3) transform_primary --> ( transform_expression )  @>
@*2 \§transform primary> $\longrightarrow$ `\.{\LP}' 
\§transform expression> `\.{\RP}'.

\LOG
\initials{LDF 2004.09.28.}
Made debugging output thread-safe.  Formatting changes. 
\ENDLOG 


@<Define rules@>=
@=transform_primary: LEFT_PARENTHESIS transform_expression RIGHT_PARENTHESIS@>@/
{

  @=$$@> = @=$2@>;

};

@q ***** (5) transform_primary --> LAST @>
@q ***** (5) transform_vector_expression.@>

@*4 \§transform primary> $\longrightarrow$ 
\.{LAST} \§transform vector expression>.
\initials{LDF 2005.01.13.}

\LOG
\initials{LDF 2005.01.13.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=transform_primary: LAST transform_vector_expression@>@/
{ 

   Transform* p;

   p = create_new<Transform>(0);

   Pointer_Vector<Transform>* pv 
      = static_cast<Pointer_Vector<Transform>*>(@=$2@>);

@q ******* (7) Error handling:  |pv == 0|.@> 

@ Error handling:  |pv == 0|.
\initials{LDF 2005.01.13.}

@<Define rules@>=

   if (pv == static_cast<Pointer_Vector<Transform>*>(0))
      {
          delete p;

          @=$$@> = static_cast<void*>(0);

      }  /* |if (pv == 0)|  */

@q ******* (7) Error handling:  |pv->ctr == 0|.@> 

@ Error handling:  |pv->ctr == 0|.
\initials{LDF 2005.01.13.}

@<Define rules@>=

   else if (pv->ctr == 0)
      {

          delete p;

          @=$$@> = static_cast<void*>(0);

      }  /* |else if (pv->ctr == 0)|  */

@q ******* (7) |pv != 0 && pv->ctr > 0|.@> 

@ |pv != 0 && pv->ctr > 0|.  Set |@=$$@>| to |*(pv->v[pv->ctr - 1])|.
\initials{LDF 2005.01.13.}

@<Define rules@>=

   else 
      {
         *p = *(pv->v[pv->ctr - 1]);
         @=$$@> = static_cast<void*>(p); 
      }
@q ******* (7) @> 

};

@q ***** (5) transform_primary --> GET_TRANSFORM focus_expression.@>

@*4 \§transform primary> $\longrightarrow$ 
\.{GET\_TRANSFORM} \§focus expression>.
\initials{LDF 2005.04.16.}

\LOG
\initials{LDF 2005.04.16.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=transform_primary: GET_TRANSFORM focus_expression@>@/
{ 
    
    @=$$@> = transform_primary_rule_func_0(@=$2@>, 
                                           static_cast<Scanner_Node>(
                                              parameter));

};

@q ***** (5) transform_primary --> GET_PERSPECTIVE_TRANSFORM focus_expression.@>

@*4 \§transform primary> $\longrightarrow$ 
\.{GET\_PERSPECTIVE\_TRANSFORM} \§focus expression>.
\initials{LDF 2007.09.02.}

\LOG
\initials{LDF 2007.09.02.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=transform_primary: GET_PERSPECTIVE_TRANSFORM focus_expression@>@/
{ 

    if (@=$2@>)
    {
       Focus* f = static_cast<Focus*>(@=$2@>); 
       @=$$@> = static_cast<void*>(create_new<Transform>(f->get_persp())); 
       delete f;
    }
    else
       @=$$@> = static_cast<void*>(0);

};



@q ***** (5) transform_primary --> GET_TRANSFORM @>
@q ***** (5) conic_section_lattice_expression.   @>

@*4 \§transform primary> $\longrightarrow$ 
\.{GET\_TRANSFORM} \§conic section lattice expression>.
\initials{LDF 2007.08.21.}

\LOG
\initials{LDF 2007.08.21.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=transform_primary: GET_TRANSFORM conic_section_lattice_expression@>@/
{ 
    
    if (@=$2@>)
    {
       Conic_Section_Lattice* c 
          = static_cast<Conic_Section_Lattice*>(@=$2@>); 

       @=$$@> = c->get_transform();

       delete c;
    }
    else
       @=$$@> = static_cast<void*>(0);
};


@q ***** (5) transform_primary --> INVERSE transform_expression.@>

@*4 \§transform primary> $\longrightarrow$ 
\.{INVERSE} \§transform expression>.
\initials{LDF 2005.04.16.}

\LOG
\initials{LDF 2005.04.16.}
Added this rule.

\initials{LDF 2007.10.14.}
Now passing |INVERSE| to |Scan_Parse::transform_primary_rule_func_1|.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=transform_primary: INVERSE transform_expression@>@/
{ 
    @=$$@> = transform_primary_rule_func_1(static_cast<Transform*>(@=$2@>),
                                           INVERSE,
                                           static_cast<Scanner_Node>(
                                              parameter));
};


@q ***** (5) transform_primary --> TRANSPOSED transform_expression.@>

@*4 \§transform primary> $\longrightarrow$ 
\.{TRANSPOSED} \§transform expression>.
\initials{LDF 2007.10.14.}

\LOG
\initials{LDF 2007.10.14.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=transform_primary: TRANSPOSED transform_expression@>@/
{ 
   
    @=$$@> = transform_primary_rule_func_1(static_cast<Transform*>(@=$2@>),
                                           TRANSPOSED,
                                           static_cast<Scanner_Node>(
                                              parameter));

};

@q ***** (5) transform_primary --> STANDARDIZE parabola_primary.@>

@*4 \§transform primary> $\longrightarrow$ 
\.{STANDARDIZE} \§parabola primary>.
\initials{LDF 2005.11.18.}

\LOG
\initials{LDF 2005.11.18.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=transform_primary: STANDARDIZE parabola_primary@>@/
{ 

   @=$$@> = Scan_Parse::standardize_func<Parabola>(
               static_cast<Parabola*>(@=$2@>),
               static_cast<Scanner_Node>(parameter)); 

};


@q ***** (5) transform_primary --> ALIGN path_primary WITH_AXIS axis_specifier.@>

@*4 \§transform primary> $\longrightarrow$ 
\.{ALIGN} \§path primary> \.{WITH\_AXIS} \§axis specifier>.
\initials{LDF 2006.10.01.}

\LOG
\initials{LDF 2006.10.01.}
Started working on this rule.

\initials{LDF 2007.10.02.}
Finished this rule.  Now calling |Scan_Parse::align_path_func| and
setting the value of the rule to the former's return value.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=transform_primary: ALIGN path_primary WITH_AXIS axis_specifier@>@/
{ 
   @=$$@> = align_path_func(@=$2@>, @=$4@>, parameter);
};

@q ** (2) transform secondary.  @>
@*1 \§transform secondary>.
\initials{LDF Undated.}

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> transform_secondary@>
  
@q *** (3) transform secondary --> transform_primary.@>
@*2 \§transform secondary> $\longrightarrow$ \§transform primary>.
\initials{LDF Undated.}

\LOG
\initials{LDF 2004.09.28.}
Made debugging output thread-safe.  Formatting changes. 
\ENDLOG 

@<Define rules@>=
@=transform_secondary: transform_primary@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3) transform secondary --> transform_secondary transformer.@>
@*2 \§transform secondary> $\longrightarrow$ \§transform secondary>
\§transformer>.


\LOG
\initials{LDF 2004.09.28.}
Made debugging output thread-safe.  Formatting changes.
\ENDLOG 

@<Define rules@>=
@=transform_secondary: transform_secondary transformer@>@/
{
  
  
  Transform* t = static_cast<Transform*>(@=$1@>);
  Transform* u = static_cast<Transform*>(@=$2@>);

  *t *= *u;

  @=$$@> = static_cast<void*>(t);

  delete (u);

};


@q ** (2) transform tertiary.  @>
@*1 \§transform tertiary>.

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> transform_tertiary@>

@q *** (3) transform tertiary --> transform_secondary.@>
@*2 \§transform tertiary> $\longrightarrow$ \§transform secondary>.

\LOG
\initials{LDF 2004.09.28.}
Made debugging output thread-safe.  Formatting changes.
\ENDLOG 

@<Define rules@>=
@=transform_tertiary: transform_secondary@>@/
{
  
  @=$$@> = @=$1@>;

};


@q ** (2) transform expression.  @>
@*1 \§transform expression>.

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> transform_expression@>

@q *** (3) transform expression --> transform_tertiary.  @>
@*2 \§transform expression> $\longrightarrow$ \§transform tertiary>.

\LOG
\initials{LDF 2004.09.28.}
Made debugging output thread-safe.  Formatting changes.
\ENDLOG 

@<Define rules@>=
@=transform_expression: transform_tertiary@>@/
{

  @=$$@> = @=$1@>;

};


@q ** (2) transformer.  @>
@*1 \§transformer>.

\TODO
@q { @>
@:TO DO}{{\bf TO DO}@>
@q } @> 
@q !! TO DO:  @>
Number the rules when I've written them all.
\initials{LDF 2004.05.13.}
\ENDTODO 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> transformer@>

@q *** (3) transformer --> basic_transformer numeric_primary.  @>
@*2 \§transformer> $\longrightarrow$ \§basic transformer> 
\§numeric primary>.

\LOG
\initials{LDF 2004.09.28.}
Added this rule.

\initials{LDF 2004.09.29.}
Rewrote this rule.  It now puts the |numeric_primary| onto a
|Pointer_Vector<real>| and passes the latter to 
|Scanner_Type::handle_numeric_list()|.

\initials{LDF 2004.11.09.}
Now pushing three copies of |numeric_primary| onto the
|Pointer_Vector<real>| if |basic_transformer == SCALED|.  This way,
scaling takes place in all three dimensions.  For scaling in the 
x-dimension only, |xscaled| should be used.  A |numeric_list| such as 
|(13, 1)| or |(13, 1, 1)|, i.e., with 1 as the value for scaling in
the y-dimension or the y and z-dimensions, can also be used.
Please note that |(13)| is a |numeric_primary| and {\it not\/} a
|numeric_list|. 
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=transformer: basic_transformer numeric_primary@>@/
{
  
@q ***** (5) @> @ Memory must be allocated for |Pointer_Vector<real>* p| and |real* r|
here, because |Scanner_Type::handle_numeric_list()| deletes the
|Pointer_Vector<real>*| passed to it via its |void*| argument. 
The |Pointer_Vector<real>| destructor, in turn, deletes the elements
of the |vector<real*>|. 
\initials{LDF 2004.09.29.}
                              
@<Define rules@>=

  int transform_token = @=$1@>;

  Pointer_Vector<real>* p;
  
  real* r;

        p = new Pointer_Vector<real>;  

        r = new real;
        *r = @=$2@>;
        *p += r;

        if (transform_token == SCALED)
           {
              r = new real;
              *r = @=$2@>;
              *p += r;
        
              r = new real;
              *r = @=$2@>;
              *p += r;

           } /* |if (transform_token == SCALED)|  */

@q ****** (6).@> 
@
@<Define rules@>=

  Bool_Void_Pointer bvp
    = static_cast<Scanner_Node>(parameter)->handle_numeric_list(transform_token,
                                               static_cast<void*>(p));

@q ***** (5) Error handling:  |Scanner_Type::handle_numeric_list()| failed.@>

@ Error handling:  |Scanner_Type::handle_numeric_list()| failed.
\initials{LDF 2004.09.28.}

@<Define rules@>=

  if (!bvp.first)
    {

      Transform* t = static_cast<Transform*>(bvp.second);

      t->reset();
      
      @=$$@> = static_cast<void*>(t); 
        

    } /* |if (!bvp.first)|  */


@q ***** (5) |Scanner_Type::handle_numeric_list()| succeeded.@>

@ |Scanner_Type::handle_numeric_list()| succeeded.
\initials{LDF 2004.09.28.}

@<Define rules@>=


  else /* |bvp.first == true|  */
    {

      @=$$@> = bvp.second;


    }  /* |else| (|bvp.first == true|)  */

};

@q *** (3) transformer --> basic_transformer numeric_list  @>
@*2 \§transformer> $\longrightarrow$ \§basic transformer> 
\§numeric list>.

\LOG
\initials{LDF 2004.09.28.}
Added this rule.

\initials{LDF 2004.09.28.}
Rewrote this rule.

\initials{LDF 2004.09.29.}
Removed |Pointer_Vector<real>* r|.
It was no longer needed, since |Scanner_Type::handle_numeric_list()|
now takes a |void*| argument.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=transformer: basic_transformer numeric_list@>
{

  int transform_token = @=$1@>;

  Bool_Void_Pointer bvp
    = static_cast<Scanner_Node>(parameter)->handle_numeric_list(transform_token, @=$2@>);


@q ***** (5) Error handling:  |Scanner_Type::handle_numeric_list()| failed.@>

@ Error handling:  |Scanner_Type::handle_numeric_list()| failed.
\initials{LDF 2004.09.28.}

@<Define rules@>=

  if (!bvp.first)
    {

      Transform* t = static_cast<Transform*>(bvp.second);

      t->reset();
      
      @=$$@> = static_cast<void*>(t); 
        

    } /* |if (!bvp.first)|  */


@q ***** (5) |Scanner_Type::handle_numeric_list()| succeeded.@>

@ |Scanner_Type::handle_numeric_list()| succeeded.
\initials{LDF 2004.09.28.}

@<Define rules@>=


  else /* |bvp.first == true|  */
    {

      @=$$@> = bvp.second;


    }  /* |else| (|bvp.first == true|)  */

};

@q *** (3) transformer --> basic_transformer point_variable.@>
@*2 \§transformer> $\longrightarrow$ \§basic transformer> 
\§point variable>.
\initials{LDF Undated.}

@:!!@> !! Please note:  \§point variable> can't be changed to \§point primary>,
\§point secondary>, \§point tertiary> or \§point expression> because
this causes over 200 shift/reduce conflicts.
\initials{LDF 2007.10.19.}

\LOG
\initials{LDF 2004.09.28.}
Added this rule.

\initials{LDF 2004.09.29.}
Rewrote this rule.  It now puts the x, y, and z-coordinates of the 
|Point| stored in the |point_variable| onto a
|Pointer_Vector<real>| and passes the latter to 
|Scanner_Type::handle_numeric_list()|.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=transformer: basic_transformer point_variable@>
{

@q ***** (5).@> 

@ Memory must be allocated for |Pointer_Vector<real>* p| and |real* r[3]|
here, because |Scanner_Type::handle_numeric_list()| deletes the
|Pointer_Vector<real>*| passed to it via its |void*| argument. 
The |Pointer_Vector<real>| destructor, in turn, deletes the elements
of the |vector<real*>|. 
\initials{LDF 2004.09.29.}
                              
@<Define rules@>=

  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$2@>); 

@q ***** (5) Error handling:  |entry == 0 || entry->object == 0|.@> 
@ Error handling:  |entry == 0 || entry->object == 0|.
\initials{LDF 2004.09.29.}

@<Define rules@>=

   if (entry == static_cast<Id_Map_Entry_Node>(0) || entry->object == static_cast<void*>(0))
     {


         Transform* t = new Transform;

         @=$$@> = static_cast<void*>(t); 


     }  /* |if (entry == 0 || entry->object == 0)|  */


@q ***** (5) |entry| and |entry->object| are non-null.@> 

@ |entry| and |entry->object| are non-null.
\initials{LDF 2004.09.29.}

@<Define rules@>=

   else /* |!(entry == 0 || entry->object == 0)|  */
     {

       int transform_token = @=$1@>;
       Pointer_Vector<real>* p = new Pointer_Vector<real>;  

       Point* q = static_cast<Point*>(entry->object);

       real* r[3];
  
       r[0] = new real;
       r[1] = new real;
       r[2] = new real;


       *(r[0]) = q->get_x();
       *(r[1]) = q->get_y();
       *(r[2]) = q->get_z();

       *p += r[0];
       *p += r[1];
       *p += r[2];

       Bool_Void_Pointer bvp
         = static_cast<Scanner_Node>(parameter)->handle_numeric_list(transform_token,
                                             static_cast<void*>(p));

@q ***** (5) Error handling:  |Scanner_Type::handle_numeric_list()| failed.@>

@ Error handling:  |Scanner_Type::handle_numeric_list()| failed.
\initials{LDF 2004.09.28.}

@<Define rules@>=

  if (!bvp.first)
    {

      Transform* t = static_cast<Transform*>(bvp.second);

      t->reset();
      
      @=$$@> = static_cast<void*>(t); 
        

    } /* |if (!bvp.first)|  */


@q ***** (5) |Scanner_Type::handle_numeric_list()| succeeded.@>

@ |Scanner_Type::handle_numeric_list()| succeeded.
\initials{LDF 2004.09.28.}

@<Define rules@>=


  else /* |bvp.first == true|  */
    {

      @=$$@> = bvp.second;

    }  /* |else| (|bvp.first == true|)  */

 }  /* |else| (|!(entry == 0 || entry->object == 0)|)  */

};

@q *** (3) transformer --> basic_transformer BY point_expression.@>
@*2 \§transformer> $\longrightarrow$ \§basic transformer> 
\.{BY} point expression>.
\initials{LDF 2007.10.19.}

\LOG
\initials{LDF 2007.10.19.}
Added this rule.
\ENDLOG

@q **** (4) Definition.@> 

@<Define rules@>=
@=transformer: basic_transformer BY point_expression@>
{

  Point* q = static_cast<Point*>(@=$3@>); 
  
@q ***** (5) Error handling:  |q == 0 || *q == INVALID_POINT|.@> 
@ Error handling:  |q == 0 || *q == INVALID_POINT|.
\initials{LDF 2007.10.19.}

@<Define rules@>=

   if (q == static_cast<Point*>(0) || *q == INVALID_POINT)
     {

         Transform* t = new Transform;

         @=$$@> = static_cast<void*>(t); 


     }  /* |if (q == 0 || *q == INVALID_POINT)|  */


@q ***** (5) |q| is valid.@> 

@ |q| is valid.
\initials{LDF 2007.10.19.}

@<Define rules@>=

   else /* |q != 0 && *q != INVALID_POINT|  */
   {

       int transform_token = @=$1@>;
       Pointer_Vector<real>* p = new Pointer_Vector<real>;  

       real* r[3];
  
       r[0] = new real;
       r[1] = new real;
       r[2] = new real;


       *(r[0]) = q->get_x();
       *(r[1]) = q->get_y();
       *(r[2]) = q->get_z();

       *p += r[0];
       *p += r[1];
       *p += r[2];

       Bool_Void_Pointer bvp
         = static_cast<Scanner_Node>(parameter)->handle_numeric_list(transform_token,
                                             static_cast<void*>(p));

@q ****** (6) Error handling:  |Scanner_Type::handle_numeric_list()| failed.@>

@ Error handling:  |Scanner_Type::handle_numeric_list()| failed.
\initials{LDF 2007.10.19.}

@<Define rules@>=

  if (!bvp.first)
    {

      Transform* t = static_cast<Transform*>(bvp.second);

      t->reset();
      
      @=$$@> = static_cast<void*>(t); 
        

    } /* |if (!bvp.first)|  */


@q ****** (6) |Scanner_Type::handle_numeric_list()| succeeded.@>

@ |Scanner_Type::handle_numeric_list()| succeeded.
\initials{LDF 2007.10.19.}

@<Define rules@>=


     else /* |bvp.first == true|  */
    {

      @=$$@> = bvp.second;

    }  /* |else| (|bvp.first == true|)  */

@q ****** (6)@>


  }  /* |else| (|q != 0 && *q != INVALID_POINT|)  */

@q ***** (5)@>

   delete q;

};




@q *** (3) basic_transformer.  @>
@*2 \§basic transformer>.
\initials{LDF 2004.09.28.}

@<Type declarations for non-terminal symbols@>=
@=%type <int_value> basic_transformer@>@/

@q **** (4) basic_transformer --> ROTATED.@>   
@*3 \§basic transformer> $\longrightarrow$ \.{ROTATED.}.
\initials{LDF 2004.09.28.}

\LOG
\initials{LDF 2004.09.28.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=basic_transformer: ROTATED@>@/

{

  @=$$@> = ROTATED;

};


@q **** (4) basic_transformer --> SCALED.@>   
@*3 \§basic transformer> $\longrightarrow$ \.{SCALED.}.
\initials{LDF 2004.09.28.}

\LOG
\initials{LDF 2004.09.28.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=basic_transformer: SCALED@>@/

{

  @=$$@> = SCALED;


};


@q **** (4) basic_transformer --> XSCALED.@>   
@*3 \§basic transformer> $\longrightarrow$ \.{XSCALED.}.
\initials{LDF 2004.09.28.}

\LOG
\initials{LDF 2004.09.28.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=basic_transformer: XSCALED@>@/

{

  @=$$@> = XSCALED;


};

@q **** (4) basic_transformer --> YSCALED.@>   
@*3 \§basic transformer> $\longrightarrow$ \.{YSCALED.}.
\initials{LDF 2004.09.28.}

\LOG
\initials{LDF 2004.09.28.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=basic_transformer: YSCALED@>@/

{

  @=$$@> = YSCALED;


};


@q **** (4) basic_transformer --> ZSCALED.@>   
@*3 \§basic transformer> $\longrightarrow$ \.{ZSCALED.}.
\initials{LDF 2004.09.28.}

\LOG
\initials{LDF 2004.09.28.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=basic_transformer: ZSCALED@>@/

{

  @=$$@> = ZSCALED;


};




@q **** (4) basic_transformer --> SHEARED.@>   
@*3 \§basic transformer> $\longrightarrow$ \.{SHEARED}.
\initials{LDF 2004.09.28.}

\LOG
\initials{LDF 2004.09.28.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=basic_transformer: SHEARED@>@/

{

  @=$$@> = SHEARED;


};

@q **** (4) basic_transformer --> SHIFTED.@>   
@*3 \§basic transformer> $\longrightarrow$ \.{SHIFTED}.
\initials{LDF 2004.09.28.}

\LOG
\initials{LDF 2004.09.28.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=basic_transformer: SHIFTED@>@/

{

  @=$$@> = SHIFTED;


};

@q *** (3) Rotation around.  @>
@*2 Rotation around.
  
\LOG
\initials{LDF 2004.05.13.}  
Added this section.
\ENDLOG 

@q **** (4) transformer --> ROTATED_AROUND ( point_expression,  @>
@q **** (4) point_expression ) numeric_expression.@>
@*3 \§transformer> $\longrightarrow$ \.{ROTATED\_AROUND} 
`\.{(}' \§point expression> `\.{,}' \§point expression> `\.{)}'.

\LOG
\initials{LDF 2010.07.09.}
Added this rule.  Rotation is by $180^{\circ}$.
\ENDLOG 

@q ***** (5) Definition.@> 

@<Define rules@>=
@=transformer: ROTATED_AROUND LEFT_PARENTHESIS point_expression COMMA @>@/
@=point_expression RIGHT_PARENTHESIS @>@/
{

 
  Transform* t = rotate_around_func(static_cast<Scanner_Node>(parameter),
                                    static_cast<Point*>(@=$3@>),
                                    static_cast<Point*>(@=$5@>),
                                    180);

@q ****** (6) Error handling:  |Scan_Parse::rotate_around_func()| failed.@>

@ Error handling:  |Scan_Parse::rotate_around_func()| failed.
\initials{LDF 2004.09.29.}

@<Define rules@>=

  if (t == static_cast<Transform*>(0))
     {

      t = new Transform;

      t->reset();

      @=$$@> = static_cast<void*>(t); 

     } /* |if (t == 0)|  */


@q ****** (6) |Scan_Parse::rotate_around_func()| succeeded.@>

@ |Scan_Parse::rotate_around_func()| succeeded.
\initials{LDF 2004.09.29.}

@<Define rules@>=

  else  /* |t != 0|  */
    {

       @=$$@> = static_cast<Transform*>(t); 

    }  /* |else| (|t != 0|)  */


};

@q **** (4) transformer --> ROTATED_AROUND ( point_expression,  @>
@q **** (4) point_expression ) numeric_expression.@>
@*3 \§transformer> $\longrightarrow$ \.{ROTATED\_AROUND} 
`\.{(}' \§point expression> `\.{,}' \§point expression> `\.{)}'
\§numeric expression>.

\LOG
\initials{LDF 2004.05.13.}  
Added this rule.

\initials{LDF 2004.09.28.}
Made debugging output thread-safe.  Formatting changes.

\initials{LDF 2004.09.29.}
Changed this rule to make it possible for it to take care of
the case that a |numeric_expression| for the amount and
direction of rotation is specified, and for the case that 
none is specified.  If the latter, rotation is by $180^\circ$.

\initials{LDF 2004.09.29.}
Removed code from this rule to the function 
|Scan_Parse::rotate_around_func()| in \filename{scanprsf.web}.
\ENDLOG 

@q ***** (5) Definition.@> 

@<Define rules@>=
@=transformer: ROTATED_AROUND LEFT_PARENTHESIS point_expression COMMA @>@/
@=point_expression RIGHT_PARENTHESIS numeric_expression @>@/
{

 
  Transform* t = rotate_around_func(static_cast<Scanner_Node>(parameter),
                                    static_cast<Point*>(@=$3@>),
                                    static_cast<Point*>(@=$5@>),
                                    @=$7@>);

@q ****** (6) Error handling:  |Scan_Parse::rotate_around_func()| failed.@>

@ Error handling:  |Scan_Parse::rotate_around_func()| failed.
\initials{LDF 2004.09.29.}

@<Define rules@>=

  if (t == static_cast<Transform*>(0))
     {

      t = new Transform;

      t->reset();

      @=$$@> = static_cast<void*>(t); 

     } /* |if (t == 0)|  */


@q ****** (6) |Scan_Parse::rotate_around_func()| succeeded.@>

@ |Scan_Parse::rotate_around_func()| succeeded.
\initials{LDF 2004.09.29.}

@<Define rules@>=

  else  /* |t != 0|  */
    {

       @=$$@> = static_cast<Transform*>(t); 

    }  /* |else| (|t != 0|)  */


};

@q *** (3) Transformed.  @>
@*2 Transformed.

@q **** (4) transformer --> TRANSFORMED transform_primary.  @>
@*3 \§transformer> $\longrightarrow$ \.{TRANSFORMED} \§transform primary>.

\LOG
\initials{LDF 2004.05.13.}  
Added this rule.

\initials{LDF 2004.09.28.}
Made debugging output thread-safe.  Formatting changes.
\ENDLOG 

@<Define rules@>=
@=transformer: TRANSFORMED transform_primary@>@/
{

  @=$$@> = @=$2@>;

};


@q **** (4) TIMES (synonym for "TRANSFORMED").  @>
@*2 \.{TIMES}.

@q **** (4) transformer --> TIMES transform_primary.  @>
@*3 \§transformer> $\longrightarrow$ \.{TIMES} \§transform primary>.

\LOG
\initials{LDF 2007.10.14.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=transformer: TIMES transform_primary@>@/
{

  @=$$@> = @=$2@>;

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

