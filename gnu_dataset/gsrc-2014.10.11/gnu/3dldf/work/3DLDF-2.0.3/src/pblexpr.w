@q pblexpr.w @> 
@q Created by Laurence Finston Mon May 17 14:00:33 MEST 2004  @>
     
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



@q * (0) boolean expressions.  @>
@** boolean expressions.
\initials{LDF 2004.05.17.}  

\LOG
\initials{LDF 2004.05.17.}  
Created this file and wrote quite a few rules.  
Some are still missing, though.  

\initials{LDF 2004.12.01.}
Now using |bool| instead of |int| for the type of the object 
referenced by |boolean_variables|, |boolean_primaries|, 
|secondaries|, |tertiaries|, and |expressions|.
Made the necessary changes in all of the affected rules 
without comment.
\ENDLOG 

@q * (1) boolean_primary.  @>
@* \§boolean primary>.

\LOG
\initials{LDF 2004.05.17.}  
Added this section.

\initials{LDF 2004.10.26.}
Changed type of |boolean_primary| from |int_value| 
to |pointer_value|.
\ENDLOG 
  
@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> boolean_primary@>@/

@q ** (2) boolean_primary --> boolean_variable.@>
@*1 \§boolean primary> $\longrightarrow$ \§boolean variable>.  

\LOG
\initials{LDF 2004.05.17.}  
Added this rule.
\ENDLOG 

@<Define rules@>=
@=boolean_primary: boolean_variable@>@/

{

  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);


@q *** (3) Error handling: |entry == 0 || entry->object == 0|.@>

@ Error handling: |entry == 0 || entry->object == 0|.
\initials{LDF 2004.10.02.}

\LOG
\initials{LDF 2004.11.21.}
Commented-out the error message.  This condition occurs legitimately 
when one tries to show an ``unknown |boolean|''.

\initials{LDF 2004.11.21.}
@:BUG FIX@> BUG FIX:  Now setting |@=$$@>| to 0, not to 
|@=$$@> = static_cast<void*>(i)|, for |int* i| and |*i == 0|.
Previously, memory had already been allocated for |i| at this point.  
It is now in the following |else| clause.
\ENDLOG 

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0) || entry->object == static_cast<void*>(0))
    {

        @=$$@> = static_cast<void*>(0);

    } /* |if (entry == 0 || entry->object == 0)|  */


@q *** (3) |!(entry == 0 || entry->object == 0)|.@>

@ |!(entry == 0 || entry->object == 0)|.
\initials{LDF 2004.10.02.}

\LOG
\initials{LDF 2004.11.21.}
Now trying to allocate memory for a new |int| in this clause.  Previously, I did
it before the |if| clause.
\ENDLOG 

@<Define rules@>=


  else /* |entry != 0 && entry->object != 0|  */
    {


       bool* b;
   

            b = new bool;

      *b = *static_cast<bool*>(entry->object); 
      
      @=$$@> = static_cast<void*>(b);

    }  /* |else| (|entry != 0 && entry->object != 0|)  */

@q *** (3).@> 

};



@q ** (2) boolean_primary --> boolean_argument.@>
@*1 \§boolean primary> $\longrightarrow$ \§boolean argument>.  

@q ** (2) boolean_primary --> TRUE.@>
@*1 \§boolean primary> $\longrightarrow$ \.{TRUE}.

\LOG
\initials{LDF 2004.05.17.}  
Added this rule.
\ENDLOG 

@q *** (3) Definition.@> 

@<Define rules@>=
@=boolean_primary: TRUE@>@/

{

    bool* b;
   

         b = new bool;

      *b = 1;
      
      @=$$@> = static_cast<void*>(b);

@q **** (4) @>   

};



@q ** (2) boolean_primary --> FALSE.@>
@*1 \§boolean primary> $\longrightarrow$ \.{FALSE}.

\LOG
\initials{LDF 2004.05.17.}  
Added this rule.
\ENDLOG 

@<Define rules@>=
@=boolean_primary: FALSE@>@/
{

    bool* i;
   

         i = new bool;

      *i = 0;
      
      @=$$@> = static_cast<void*>(i);
};

@q ** (2) boolean_primary --> ( boolean_expression )  @>
@*1 \§boolean primary> $\longrightarrow$ `\.{\LP}' 
\§boolean expression> `\.{\RP}'.

\LOG
\initials{LDF 2004.05.17.}  
Added this rule.
\ENDLOG 

@<Define rules@>=
@=boolean_primary: LEFT_PARENTHESIS boolean_expression RIGHT_PARENTHESIS@>@/
{

  @=$$@> = @=$2@>;

};

@q ** (2) Rules with |predicate_variables|.@>
@*1 Rules with {\bf predicate\_variables}.
\initials{LDF 2007.11.28.}

There are many possible tests that can be performed on objects of various types.
It may therefore be desirable to reduce the number of rules needed for the 
predicates.  |predicate_variables| can be used for this purpose.  However, 
they have a significant disadvantage, since the names that refer to them 
cannot be followed directly by other variable names in the input file.  The 
reason is that the beginning of the second variable name is interpreted as a ``tag'' 
continuing the name of the |predicate_variable|, since blank space is ignored in this
context.  The second variable name must therefore be separated somehow from the 
name of the |predicate_variable|.  Enclosing it in parentheses works without 
having to account for this in the parser rules.  Another possibility would be to 
use a comma or some other symbol to separate the two items.  However, this would need to 
be accounted for in the parser rules.
\initials{LDF 2007.11.28.}

I am leaving this rule here as an example, but commenting it out.
I will not be using this approach for the present. 
\initials{LDF 2007.11.28.}


\LOG
\initials{LDF 2007.11.28.}
Added this section.
\ENDLOG

@q *** (3) boolean_primary --> predicate_variable path_expression.@> 
@*2 \§boolean primary> $\longrightarrow$ \§predicate variable> \§path expression>.
\initials{LDF 2007.11.27.}

\LOG
\initials{LDF 2007.11.27.}
Added this rule.  It works as of this date, but is currently commented-out.
\ENDLOG

@q **** (4)@>

@<Garbage@>=
@=boolean_primary: predicate_variable path_expression@>@/
{
   
    Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

    bool* b = new bool;
    *b = false;    

    Path* q = static_cast<Path*>(@=$2@>); 

    if (entry && entry->object && q)
    {
        Predicate* p = static_cast<Predicate*>(entry->object);

        *b = (*p)(@=$2@>, Predicate::PRED_PATH_TYPE, parameter);

        delete q;
    }
       
    @=$$@> = static_cast<void*>(b); 

};

@q *** (3)@>

@q ** (2) IS_KNOWN and IS_UNKNOWN.  @>
@*1 \.{IS\_KNOWN} and \.{IS\_UNKNOWN}.

In {\MF/}, the operators corresponding to the ones used in the 
following two rules are called ``{\bf known}'' and ``{\bf unknown}''.  
In 3DLDF, ``{\bf known}'' and ``{\bf unknown}'' are synonyms for 
``{\bf is\_known}'' and ``{\bf is\_unknown}'', set using 
|Scanner_Type::add_synonym()|
in |Scanner_Type::create()| in the file \filename{sctpcrt.web}.  
\initials{LDF 2004.05.17.}

Another difference is that in {\MF/}, the operand to |IS_KNOWN| and
|IS_UNKNOWN| is \§primary>.  This won't work in 3DLDF yet, because unknown
primaries that aren't variables are not yet possible. 
\initials{LDF 2004.05.17.}

\TODO
@q { @>
@:TO DO}{{\bf TO DO}@>
@q } @> 
@q !! TO DO:  @>
\initials{LDF 2004.05.17.}
Make these rules work for \§primaries> rather than \§any variables>.
\ENDTODO 

@q *** (3) boolean_primary --> IS_KNOWN any_variable. @>
@*2 \§boolean primary> $\longrightarrow$ `\.{IS\_KNOWN}' 
\§any variable>. 

\LOG
\initials{LDF 2004.05.17.}  
Added this rule.
\ENDLOG 

@<Define rules@>=
@=boolean_primary: IS_KNOWN any_variable@>@/
{

  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$2@>);


    bool* i;
      

    i = new bool;

@q **** (4) Error handling:  |new bool| failed.  Rethrow |bad_alloc|.@>   

@ Error handling:  |new bool| failed.  Rethrow |bad_alloc|.
\initials{LDF 2004.10.26.}

@<Define rules@>=

@q **** (4) |new bool| succeeded.@> 

@ |new bool| succeeded.
\initials{LDF 2004.10.26.}

@<Define rules@>=


@q **** (4).@> 

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      *i = 0;

      @=$$@> = static_cast<void*>(i);
      
    }  /* |if (entry == 0)|  */@;


@q **** (4).@> 
  
  else if (entry->known_state == Id_Map_Entry_Type::KNOWN)
     {
        *i = 1;
        @=$$@> = static_cast<void*>(i);
     }
    

@q **** (4).@> 

  else
     {
       *i = 0;
       @=$$@> = static_cast<void*>(i);
     }

@q **** (4).@> 

};

@q **** (4) boolean_primary --> IS_UNKNOWN any_variable.@>
@*3 \§boolean primary> $\longrightarrow$ `\.{IS\_UNKNOWN}' 
\§any variable>. 


\LOG
\initials{LDF 2004.05.17.}  
Added this rule.
\ENDLOG 

@<Define rules@>=
@=boolean_primary: IS_UNKNOWN any_variable@>@/
{

    bool* i;
   
    i = new bool;


@q **** (4) |new bool| succeeded.@> 

@ |new bool| succeeded.
\initials{LDF 2004.10.26.}

@<Define rules@>=


  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$2@>);

@q **** (4).@> 

  if (entry == static_cast<Id_Map_Entry_Node>(0))
    {

      *i = 1;
  
      @=$$@> = static_cast<void*>(i);
      
      
    }  /* |if (entry == 0)|  */@;

  
  else if (entry->known_state == Id_Map_Entry_Type::KNOWN)
     {
        *i = 0;
        @=$$@> = static_cast<void*>(i);
     }

  else
    {
       *i = 1;
       @=$$@> = static_cast<void*>(i);
    }
    
};

@q ** (2) Validity.@>
@*1 Validity.
\initials{LDF 2005.11.21.}

\LOG
\initials{LDF 2005.11.21.}
Added this section.
\ENDLOG

@q *** (3) boolean_primary --> IS_VALID numeric_expression.@>
@*2 \§boolean primary> $\longrightarrow$ \.{IS\_VALID} 
\§numeric expression>. 

\LOG
\initials{LDF 2005.11.21.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=boolean_primary: IS_VALID numeric_expression@>@/
{
   bool* b = new bool;

   *b = (@=$2@> == INVALID_REAL) ? false : true;

   @=$$@> = static_cast<void*>(b);

};

@q *** (3) boolean_primary --> IS_INVALID numeric_expression.@>
@*2 \§boolean primary> $\longrightarrow$ \.{IS\_INVALID} 
\§numeric expression>. 

\LOG
\initials{LDF 2005.11.21.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=boolean_primary: IS_INVALID numeric_expression@>@/
{
   bool* b = new bool;

   *b = (@=$2@> == INVALID_REAL) ? true : false;

   @=$$@> = static_cast<void*>(b);

};

@q *** (3) boolean_primary --> IS_VALID point_expression.@>
@*2 \§boolean primary> $\longrightarrow$ \.{IS\_VALID} 
\§point expression>. 

\LOG
\initials{LDF 2005.11.21.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=boolean_primary: IS_VALID point_expression@>@/
{
   bool* b  = new bool;
   Point* p = static_cast<Point*>(@=$2@>); 

   *b = (   p == static_cast<Point*>(0) 
         || *p  == INVALID_POINT) ? false : true;

   if (p)
   {
      delete p;   
      p = static_cast<Point*>(0);
   }

   @=$$@> = static_cast<void*>(b);

};

@q *** (3) boolean_primary --> IS_INVALID point_expression.@>
@*2 \§boolean primary> $\longrightarrow$ \.{IS\_INVALID} 
\§point expression>. 

\LOG
\initials{LDF 2005.11.21.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=boolean_primary: IS_INVALID point_expression@>@/
{

   bool* b  = new bool;
   Point* p = static_cast<Point*>(@=$2@>); 

   *b = (p == static_cast<Point*>(0) || *p  == INVALID_POINT) ? true : false;

   if (p) 
   {
      delete p;   
      p = 0;
   }

   @=$$@> = static_cast<void*>(b);

};

@q *** (3) boolean_primary --> IS_VALID bool_point_vector_expression.@>
@*2 \§boolean primary> $\longrightarrow$ \.{IS\_VALID} 
\§bool-point vector expression>. 

\LOG
\initials{LDF 2005.11.21.}
Added this rule.

\initials{LDF 2007.07.09.}
@:BUG FIX@> BUG FIX:  Fixed memory leak.  
Now deleting |Pointer_Vector<Bool_Point>* bpv|.
\ENDLOG 

@<Define rules@>=
@=boolean_primary: IS_VALID bool_point_vector_expression@>@/
{
    bool* b  = new bool; 

    Pointer_Vector<Bool_Point>* bpv 
       = static_cast<Pointer_Vector<Bool_Point>*>(@=$2@>); 

    *b = (   bpv == static_cast<Pointer_Vector<Bool_Point>*>(0) 
          || bpv->v.size() <= 0) ? false : true;

    delete bpv;
    bpv = 0;

    @=$$@> = static_cast<void*>(b);

};

@q *** (3) boolean_primary --> IS_INVALID bool_point_vector_expression.@>
@*2 \§boolean primary> $\longrightarrow$ \.{IS\_INVALID} 
\§bool-point  expression>. 

\LOG
\initials{LDF 2005.11.21.}
Added this rule.

\initials{LDF 2007.07.09.}
@:BUG FIX@> BUG FIX:  Fixed memory leak.  
Now deleting |Pointer_Vector<Bool_Point>* bpv|.
\ENDLOG 

@<Define rules@>=
@=boolean_primary: IS_INVALID bool_point_vector_expression@>@/
{
    bool* b  = new bool; 

    Pointer_Vector<Bool_Point>* bpv = static_cast<Pointer_Vector<Bool_Point>*>(@=$2@>); 

    *b = (   bpv == static_cast<Pointer_Vector<Bool_Point>*>(0) 
          || bpv->v.size() <= 0) ? true : false;

    delete bpv;
    bpv = 0;

    @=$$@> = static_cast<void*>(b);

};





@q ** (2) boolean_primary --> NOT boolean_primary.@>
@*1 \§boolean primary> $\longrightarrow$ 
\.{NOT} \§boolean primary>. 


\LOG
\initials{LDF 2004.10.02.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=boolean_primary: NOT boolean_primary@>@/
{

    bool* i = static_cast<bool*>(@=$2@>);    

    *i = (*i) ? 0 : 1;
  
    @=$$@> = static_cast<void*>(i);

};

@q ** (2) boolean_primary --> type_predicate any_variable.@>
@*1 \§boolean primary> $\longrightarrow$ \§type predicate> 
\§any variable>. 

In {\MF/}, it's possible to test for the type of \§primaries>.  
This is not possible in {\TDLDF} because 1. \§primary> isn't 
a Bison symbol, and 2. type information is only stored in variables, 
not in expressions (including primaries).  For example, the value 
of a \§boolean expression> is an integer and that of a 
\§point expression> is a pointer to |void| that points to a |Point|.
Since the value types differ, and are not all class types in a single 
class hierarchy, it will not be possible to implement 
{\MF/}-like behavior.  If they were, it would be possible to cast 
the objects to the base type (or equivalently cast pointers to the objects 
to pointers to the base type) and call the appropriate virtual 
functions on the objects.
\initials{LDF 2004.10.02.}

\LOG
\initials{LDF 2004.10.02.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=boolean_primary: type_predicate any_variable@>@/
{

    bool* i;
   
    i = new bool;

@q **** (4) |new bool| succeeded.@> 

@ |new bool| succeeded.
\initials{LDF 2004.10.26.}

@<Define rules@>=

   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$2@>);

@q **** (4) Warning:  |entry == 0|.  Set value of rule to |false|.@> 

@ Warning:  |entry == 0|.  Set value of rule to |false|.
\initials{LDF 2004.10.02.}

@<Define rules@>=

   if (entry == static_cast<Id_Map_Entry_Node>(0))
      {

        *i = false;

        @=$$@> = static_cast<void*>(i);


      }  /* |if (entry == 0)|  */


@q **** (4) |entry != 0|.@> 

@ |entry != 0|.
\initials{LDF 2004.10.02.}

@<Define rules@>=


   else  /* |entry != 0|  */
      {

        *i = (@=$1@> == entry->type);

	@=$$@> = static_cast<void*>(i);

      }  /* |else| (|entry != 0|)  */


@q **** (4).@> 

};


@q ** (2) boolean_primary --> IS_BIG_ENDIAN.@>
@*1 \§boolean primary> $\longrightarrow$ \§type predicate> 
\§IS\_BIG\_ENDIAN>. 
\initials{LDF 2004.11.15.}

\LOG
\initials{LDF 2004.11.15.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=boolean_primary: IS_BIG_ENDIAN@>@/
{

    bool* i;

    i = new bool;

@q **** (4) |new bool| succeeded.@> 

@ |new bool| succeeded.
\initials{LDF 2004.11.15.}

@<Define rules@>=


   *i = (System::is_big_endian()) ? 1 : 0;

   @=$$@> = static_cast<void*>(i); 

};


@q ** (2) boolean_primary --> IS_LITTLE_ENDIAN.@>
@*1 \§boolean primary> $\longrightarrow$ \§type predicate> 
\§IS\_LITTLE\_ENDIAN>. 
\initials{LDF 2004.11.15.}

\LOG
\initials{LDF 2004.11.15.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=boolean_primary: IS_LITTLE_ENDIAN@>@/
{

    bool* i;

    i = new bool;

@q **** (4) |new bool| succeeded.@> 

@ |new bool| succeeded.
\initials{LDF 2004.11.15.}

@<Define rules@>=


   *i = (System::is_little_endian()) ? 1 : 0;

   @=$$@> = static_cast<void*>(i); 

};


@q * (1) type_predicate.@>
@* \§type predicate>.

\LOG
\initials{LDF 2004.10.02.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <int_value> type_predicate@>


@q ** (2) type_predicate --> IS_BOOLEAN.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_BOOLEAN}.

\LOG
\initials{LDF 2004.10.02.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_BOOLEAN@>@/
{

  @=$$@> = BOOLEAN;

};

@q ** (2) type_predicate --> IS_BOOLEAN_VECTOR.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_BOOLEAN\_VECTOR}.

\LOG
\initials{LDF 2004.12.20.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_BOOLEAN_VECTOR@>@/
{

  @=$$@> = BOOLEAN_VECTOR;

};

@q ** (2) type_predicate --> IS_MATRIX.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_MATRIX}.
\initials{LDF 2007.11.29.}

\LOG
\initials{LDF 2007.11.29.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_MATRIX@>@/
{

  @=$$@> = MATRIX;

};

@q ** (2) type_predicate --> IS_MATRIX_VECTOR.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_MATRIX\_VECTOR}.
\initials{LDF 2007.11.29.}

\LOG
\initials{LDF 2007.11.29.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_MATRIX_VECTOR@>@/
{

  @=$$@> = MATRIX_VECTOR;

};


@q ** (2) type_predicate --> IS_BOOL_POINT.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_BOOL\_POINT}.

\LOG
\initials{LDF 2004.10.02.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_BOOL_POINT@>@/
{

  @=$$@> = BOOL_POINT;

};

@q ** (2) type_predicate --> IS_BOOL_POINT_VECTOR.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_BOOL\_POINT\_VECTOR}.

\LOG
\initials{LDF 2004.10.02.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_BOOL_POINT_VECTOR@>@/
{

  @=$$@> = BOOL_POINT_VECTOR;

};

@q ** (2) type_predicate --> IS_STRING.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_STRING}.

\LOG
\initials{LDF 2004.10.02.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_STRING@>@/
{

  @=$$@> = STRING;

};

@q ** (2) type_predicate --> IS_STRING_VECTOR.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_STRING\_VECTOR}.

\LOG
\initials{LDF 2004.12.20.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_STRING_VECTOR@>@/
{

  @=$$@> = STRING_VECTOR;

};



@q ** (2) type_predicate --> IS_NUMERIC.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_NUMERIC}.

\LOG
\initials{LDF 2004.10.02.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_NUMERIC@>@/
{

  @=$$@> = NUMERIC;

};

@q ** (2) type_predicate --> IS_NUMERIC_VECTOR.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_NUMERIC\_VECTOR}.

\LOG
\initials{LDF 2004.12.20.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_NUMERIC_VECTOR@>@/
{

  @=$$@> = NUMERIC_VECTOR;

};

@q ** (2) type_predicate --> IS_PEN.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_PEN}.

\LOG
\initials{LDF 2004.10.02.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_PEN@>@/
{

  @=$$@> = PEN;

};

@q ** (2) type_predicate --> IS_PEN_VECTOR.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_PEN\_VECTOR}.

\LOG
\initials{LDF 2004.12.20.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_PEN_VECTOR@>@/
{

  @=$$@> = PEN_VECTOR;

};

@q ** (2) type_predicate --> IS_DASH_PATTERN.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_DASH\_PATTERN}.

\LOG
\initials{LDF 2004.10.02.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_DASH_PATTERN@>@/
{

  @=$$@> = DASH_PATTERN;

};

@q ** (2) type_predicate --> IS_DASH_PATTERN_VECTOR.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_DASH\_PATTERN\_VECTOR}.

\LOG
\initials{LDF 2004.12.20.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_DASH_PATTERN_VECTOR@>@/
{

  @=$$@> = DASH_PATTERN_VECTOR;

};

@q ** (2) type_predicate --> IS_COLOR.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_COLOR}.

\LOG
\initials{LDF 2004.10.02.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_COLOR@>@/
{

  @=$$@> = COLOR;

};

@q ** (2) type_predicate --> IS_COLOR_VECTOR.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_COLOR\_VECTOR}.

\LOG
\initials{LDF 2004.10.02.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_COLOR_VECTOR@>@/
{

  @=$$@> = COLOR_VECTOR;

};

@q ** (2) type_predicate --> IS_PICTURE.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_PICTURE}.

\LOG
\initials{LDF 2004.10.02.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_PICTURE@>@/
{

  @=$$@> = PICTURE;

};

@q ** (2) type_predicate --> IS_PICTURE_VECTOR.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_PICTURE\_VECTOR}.

\LOG
\initials{LDF 2004.12.20.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_PICTURE_VECTOR@>@/
{

  @=$$@> = PICTURE_VECTOR;

};



@q ** (2) type_predicate --> IS_TRANSFORM.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_TRANSFORM}.
\initials{LDF 2004.10.02.}

\LOG
\initials{LDF 2004.10.02.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_TRANSFORM@>@/
{

  @=$$@> = TRANSFORM;

};

@q ** (2) type_predicate --> IS_TRANSFORM_VECTOR.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_TRANSFORM\_VECTOR}.
\initials{LDF 2004.12.20.}

\LOG
\initials{LDF 2004.12.20.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_TRANSFORM_VECTOR@>@/
{

  @=$$@> = TRANSFORM_VECTOR;

};


@q ** (2) type_predicate --> IS_ORIGAMI_FIGURE.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_ORIGAMI\_FIGURE}.
\initials{LDF 2005.02.04.}

\LOG
\initials{LDF 2005.02.04.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_ORIGAMI_FIGURE@>@/
{

   @=$$@> = ORIGAMI_FIGURE;

};

@q ** (2) type_predicate --> IS_ORIGAMI_FIGURE_VECTOR.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_ORIGAMI\_FIGURE\_VECTOR}.
\initials{LDF 2005.02.04.}

\LOG
\initials{LDF 2005.02.04.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_ORIGAMI_FIGURE_VECTOR@>@/
{

   @=$$@> = ORIGAMI_FIGURE_VECTOR;

};



@q ** (2) type_predicate --> IS_POINT.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_POINT}.

\LOG
\initials{LDF 2004.10.02.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_POINT@>@/
{

  @=$$@> = POINT;

};

@q ** (2) type_predicate --> IS_POINT_VECTOR.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_POINT\_VECTOR}.

\LOG
\initials{LDF 2004.10.02.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_POINT_VECTOR@>@/
{

  @=$$@> = POINT_VECTOR;

};

@q ** (2) type_predicate --> IS_FOCUS.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_FOCUS}.

\LOG
\initials{LDF 2004.10.02.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_FOCUS@>@/
{

  @=$$@> = FOCUS;

};

@q ** (2) type_predicate --> IS_FOCUS_VECTOR.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_FOCUS\_VECTOR}.

\LOG
\initials{LDF 2004.12.20.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_FOCUS_VECTOR@>@/
{

  @=$$@> = FOCUS_VECTOR;

};



@q ** (2) type_predicate --> IS_PATH.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_PATH}.

\LOG
\initials{LDF 2004.10.02.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_PATH@>@/
{

  @=$$@> = PATH;

};

@q ** (2) type_predicate --> IS_PATH_VECTOR.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_PATH\_VECTOR}.

\LOG
\initials{LDF 2004.12.20.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_PATH_VECTOR@>@/
{

  @=$$@> = PATH_VECTOR;

};



@q ** (2) type_predicate --> IS_TRIANGLE.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_TRIANGLE}.

\LOG
\initials{LDF 2004.10.02.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_TRIANGLE@>@/
{

  @=$$@> = TRIANGLE;

};

@q ** (2) type_predicate --> IS_TRIANGLE_VECTOR.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_TRIANGLE\_VECTOR}.

\LOG
\initials{LDF 2004.12.20.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_TRIANGLE_VECTOR@>@/
{

  @=$$@> = TRIANGLE_VECTOR;

};


@q ** (2) type_predicate --> IS_POLYGON.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_POLYGON}.
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.02.11.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_POLYGON@>@/
{

  @=$$@> = POLYGON;

};

@q ** (2) type_predicate --> IS_POLYGON_VECTOR.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_POLYGON\_VECTOR}.
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.02.11.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_POLYGON_VECTOR@>@/
{

  @=$$@> = POLYGON_VECTOR;

};



@q ** (2) type_predicate --> IS_REG_POLYGON.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_REG\_POLYGON}.

\LOG
\initials{LDF 2004.10.02.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_REG_POLYGON@>@/
{

  @=$$@> = REG_POLYGON;

};

@q ** (2) type_predicate --> IS_REG_POLYGON_VECTOR.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_REG\_POLYGON\_VECTOR}.

\LOG
\initials{LDF 2004.12.20.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_REG_POLYGON_VECTOR@>@/
{

  @=$$@> = REG_POLYGON_VECTOR;

};

@q ** (2) type_predicate --> IS_RECTANGLE.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_RECTANGLE}.

\LOG
\initials{LDF 2004.10.02.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_RECTANGLE@>@/
{

  @=$$@> = RECTANGLE;

};

@q ** (2) type_predicate --> IS_RECTANGLE_VECTOR.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_RECTANGLE\_VECTOR}.

\LOG
\initials{LDF 2004.12.20.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_RECTANGLE_VECTOR@>@/
{

  @=$$@> = RECTANGLE_VECTOR;

};

@q ** (2) type_predicate --> IS_ELLIPSE.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_ELLIPSE}.

\LOG
\initials{LDF 2004.10.02.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_ELLIPSE@>@/
{

  @=$$@> = ELLIPSE;

};


@q ** (2) type_predicate --> IS_ELLIPSE_VECTOR.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_ELLIPSE\_VECTOR}.

\LOG
\initials{LDF 2004.12.20.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_ELLIPSE_VECTOR@>@/
{

  @=$$@> = ELLIPSE_VECTOR;

};


@q ** (2) type_predicate --> IS_CIRCLE.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_CIRCLE}.

\LOG
\initials{LDF 2004.10.02.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_CIRCLE@>@/
{

  @=$$@> = CIRCLE;

};

@q ** (2) type_predicate --> IS_CIRCLE_VECTOR.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_CIRCLE\_VECTOR}.

\LOG
\initials{LDF 2004.12.20.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_CIRCLE_VECTOR@>@/
{

  @=$$@> = CIRCLE_VECTOR;

};

@q ** (2) type_predicate --> IS_PARABOLA.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_PARABOLA}.
\initials{LDF 2005.12.01.}

\LOG
\initials{LDF 2005.12.01.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_PARABOLA@>@/
{

  @=$$@> = PARABOLA;

};


@q ** (2) type_predicate --> IS_PARABOLA_VECTOR.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_PARABOLA\_VECTOR}.
\initials{LDF 2005.12.01.}

\LOG
\initials{LDF 2005.12.01.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_PARABOLA_VECTOR@>@/
{

  @=$$@> = PARABOLA_VECTOR;

};

@q ** (2) type_predicate --> IS_HYPERBOLA.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_HYPERBOLA}.
\initials{LDF 2005.12.01.}

\LOG
\initials{LDF 2005.12.01.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_HYPERBOLA@>@/
{

  @=$$@> = HYPERBOLA;

};

@q ** (2) type_predicate --> IS_HYPERBOLA_VECTOR.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_HYPERBOLA\_VECTOR}.
\initials{LDF 2005.12.01.}

\LOG
\initials{LDF 2005.12.01.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_HYPERBOLA_VECTOR@>@/
{

  @=$$@> = HYPERBOLA_VECTOR;

};


@q ** (2) type_predicate --> IS_CONIC_SECTION_LATTICE.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_CONIC\_SECTION\_LATTICE}.
\initials{LDF 2007.07.29.}

\LOG
\initials{LDF 2007.07.29.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_CONIC_SECTION_LATTICE@>@/
{

  @=$$@> = CONIC_SECTION_LATTICE;

};

@q ** (2) type_predicate --> IS_CONIC_SECTION_LATTICE_VECTOR.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_CONIC\_SECTION\_LATTICE\_VECTOR}.
\initials{LDF 2007.07.29.}

\LOG
\initials{LDF 2007.07.29.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_CONIC_SECTION_LATTICE_VECTOR@>@/
{

  @=$$@> = CONIC_SECTION_LATTICE_VECTOR;

};


@q ** (2) type_predicate --> IS_ELLIPSOID.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_ELLIPSOID}.
\initials{LDF 2005.12.01.}

\LOG
\initials{LDF 2005.12.01.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_ELLIPSOID@>@/
{

  @=$$@> = ELLIPSOID;

};


@q ** (2) type_predicate --> IS_ELLIPSOID_VECTOR.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_ELLIPSOID\_VECTOR}.
\initials{LDF 2005.12.01.}

\LOG
\initials{LDF 2005.12.01.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_ELLIPSOID_VECTOR@>@/
{

  @=$$@> = ELLIPSOID_VECTOR;

};


@q ** (2) type_predicate --> IS_SPHERE.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_SPHERE}.
\initials{LDF 2005.12.01.}

\LOG
\initials{LDF 2005.12.01.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_SPHERE@>@/
{

  @=$$@> = SPHERE;

};


@q ** (2) type_predicate --> IS_SPHERE_VECTOR.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_SPHERE\_VECTOR}.
\initials{LDF 2005.12.01.}

\LOG
\initials{LDF 2005.12.01.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_SPHERE_VECTOR@>@/
{

  @=$$@> = SPHERE_VECTOR;

};

@q ** (2) type_predicate --> IS_CUBOID.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_CUBOID}.

\LOG
\initials{LDF 2004.10.02.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_CUBOID@>@/
{

  @=$$@> = CUBOID;

};

@q ** (2) type_predicate --> IS_CUBOID_VECTOR.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_CUBOID\_VECTOR}.

\LOG
\initials{LDF 2004.12.20.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_CUBOID_VECTOR@>@/
{

  @=$$@> = CUBOID_VECTOR;

};

@q ** (2) type_predicate --> IS_POLYHEDRON.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_POLYHEDRON}.

\LOG
\initials{LDF 2004.10.02.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_POLYHEDRON@>@/
{

  @=$$@> = POLYHEDRON;

};

@q ** (2) type_predicate --> IS_POLYHEDRON_VECTOR.@>
@*1 \§type predicate> $\longrightarrow$ \.{IS\_POLYHEDRON\_VECTOR}.

\LOG
\initials{LDF 2004.12.20.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=type_predicate: IS_POLYHEDRON_VECTOR@>@/
{

  @=$$@> = POLYHEDRON_VECTOR;

};

@q ** (2) boolean_primary --> IS_CYCLE path_expression.@>
@*1 \§boolean primary> $\longrightarrow$ \.{IS\_CYCLE}
\§path expression>. 

\LOG
\initials{LDF 2004.10.02.}
Added this rule.

\initials{LDF 2005.10.24.}
Changed |path_like_expression| to |path_expression|.
Now calling |Scan_Parse::is_cycle_func()|.  Removed all 
debugging code.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=boolean_primary: IS_CYCLE path_expression@>@/
{

   @=$$@> = Scan_Parse::is_cycle_func(@=$2@>, parameter);

};

@q ** (2) boolean_primary --> IS_CYCLE circle_expression.@>
@*1 \§boolean primary> $\longrightarrow$ \.{IS\_CYCLE}
\§circle expression>. 


\LOG
\initials{LDF 2005.10.24.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=boolean_primary: IS_CYCLE circle_expression@>@/
{

   @=$$@> = is_cycle_func(@=$2@>, parameter);

};

@q ** (2) boolean_primary --> IS_CYCLE ellipse_expression.@>
@*1 \§boolean primary> $\longrightarrow$ \.{IS\_CYCLE}
\§ellipse expression>. 


\LOG
\initials{LDF 2005.10.24.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=boolean_primary: IS_CYCLE ellipse_expression@>@/
{

   @=$$@> = is_cycle_func(@=$2@>, parameter);

};

@q ** (2) boolean_primary --> IS_CYCLE rectangle_expression.@>
@*1 \§boolean primary> $\longrightarrow$ \.{IS\_CYCLE}
\§rectangle expression>. 


\LOG
\initials{LDF 2005.10.24.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=boolean_primary: IS_CYCLE rectangle_expression@>@/
{

   @=$$@> = is_cycle_func(@=$2@>, parameter);

};

@q ** (2) boolean_primary --> IS_CYCLE triangle_expression.@>
@*1 \§boolean primary> $\longrightarrow$ \.{IS\_CYCLE}
\§triangle expression>. 


\LOG
\initials{LDF 2005.10.24.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=boolean_primary: IS_CYCLE triangle_expression@>@/
{

   @=$$@> = is_cycle_func(@=$2@>, parameter);

};

@q ** (2) boolean_primary --> IS_CYCLE polygon_expression.@>
@*1 \§boolean primary> $\longrightarrow$ \.{IS\_CYCLE}
\§polygon expression>. 


\LOG
\initials{LDF 2005.10.24.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=boolean_primary: IS_CYCLE polygon_expression@>@/
{

   @=$$@> = is_cycle_func(@=$2@>, parameter);

};

@q ** (2) boolean_primary --> IS_CYCLE reg_polygon_expression.@>
@*1 \§boolean primary> $\longrightarrow$ \.{IS\_CYCLE}
\§regular polygon expression>. 


\LOG
\initials{LDF 2005.10.24.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=boolean_primary: IS_CYCLE reg_polygon_expression@>@/
{

   @=$$@> = is_cycle_func(@=$2@>, parameter);

};

@q ** (2) boolean_primary --> IS_EVEN numeric_primary.@>
@*1 \§boolean primary> $\longrightarrow$ 
\.{IS\_EVEN} \§numeric primary>. 
\initials{LDF 2004.10.02.}

\LOG
\initials{LDF 2004.10.02.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=boolean_primary: IS_EVEN numeric_primary@>@/
{

    bool* i;
   

    i = new bool;

@q **** (4) |new bool| succeeded.@> 

@ |new bool| succeeded.
\initials{LDF 2004.10.26.}

\LOG
\initials{LDF 2005.01.10.}
@:BUG FIX@> BUG FIX: 
Fixed the expression used for setting |*i|, 
which was incorrect.
\ENDLOG 

@<Define rules@>=

  *i = !(static_cast<int>(floor(@=$2@> + .5)) % 2); 

  @=$$@> = static_cast<void*>(i);

};

@q ** (2) boolean_primary --> IS_ODD numeric_primary.@>
@*1 \§boolean primary> $\longrightarrow$ 
\.{IS\_ODD} \§numeric primary>. 
\initials{LDF 2004.10.02.}


\LOG
\initials{LDF 2004.10.02.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=boolean_primary: IS_ODD numeric_primary@>@/
{

    bool* i;
   
    i = new bool;

@q **** (4) |new bool| succeeded.@> 

@ |new bool| succeeded.
\initials{LDF 2004.10.26.}

\LOG
@:BUG FIX@> BUG FIX:  
\initials{LDF 2005.01.11.}  
Fixed the way |*i| is set.
\ENDLOG 

@<Define rules@>=

  *i = static_cast<bool>(static_cast<int>(floor(@=$2@> + .5)) % 2); 

  @=$$@> = static_cast<void*>(i);
    
};

@q * (1) Predicates for matrices.@> 
@* Predicates for matrices.
\initials{LDF 2007.12.20.}

\LOG
\initials{LDF 2007.12.20.}
Added this section.
\ENDLOG

@q ** (2) boolean_primary --> matrix_expression IS_MULTIPLIABLE matrix_expression.@>
@*1 \§boolean primary> $\longrightarrow$ \§matrix expression>. 
\.{IS\_MULTIPLIABLE} \§matrix expression>.

\LOG
\initials{LDF 2007.12.20.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=boolean_primary: matrix_expression IS_MULTIPLIABLE matrix_expression@>@/
{

    Matrix* m0 = static_cast<Matrix*>(@=$1@>);
    Matrix* m1 = static_cast<Matrix*>(@=$3@>);

    bool* i = new bool;

    *i = (m0->is_multipliable(*m1));

    delete m0;
    delete m1;

    @=$$@> = static_cast<void*>(i);

};

@q * (1) |Shape| characteristic predicates.@>
@* |Shape| characteristic predicates.

\LOG
\initials{LDF 2004.10.14.}
Added this section.
\ENDLOG

@q ** (2) boolean_primary --> IS_LINEAR path_expression.@>
@*1 \§boolean primary> 
$\longrightarrow$ \.{IS\_LINEAR} \§path expression>.

\LOG
\initials{LDF 2004.10.25.}
Added this rule.

\initials{LDF 2005.10.24.}
Changed |path_like_expression| to |path_expression|.
Removed debugging code.

\initials{LDF 2005.10.26.}
@:BUG FIX@> BUG FIX:  Now allocating memory for |bool* i|.
\ENDLOG

@<Define rules@>=
@=boolean_primary: IS_LINEAR path_expression@>@/
{

    Path* p = static_cast<Path*>(@=$2@>);

    bool* i = new bool;

   *i = (p && p->is_linear()) ? 1 : 0;

   delete p;

   @=$$@> = static_cast<void*>(i);

};


@q ** (2) boolean_primary --> IS_PLANAR path_expression.@>
@*1 \§boolean primary> 
$\longrightarrow$ \.{IS\_PLANAR} \§path expression>.

\LOG
\initials{LDF 2004.10.25.}
Added this rule.

\initials{LDF 2005.10.24.}
Changed |path_like_expression| to |path_expression|.
Removed debugging code.

\initials{LDF 2005.10.26.}
@:BUG FIX@> BUG FIX:  Now allocating memory for |bool* i|.
\ENDLOG

@<Define rules@>=
@=boolean_primary: IS_PLANAR path_expression@>@/
{

    Path* p = static_cast<Path*>(@=$2@>);

    bool* i = new bool;
   
    *i = (p && p->is_planar()) ? 1 : 0;

    delete p;

    @=$$@> = static_cast<void*>(i);

};

@q ** (2) boolean_primary --> path_expression IS_COPLANAR @>
@q ** (2) path_expression.                                @>
@*1 \§boolean primary> 
$\longrightarrow$ \§path expression> \.{IS\_COPLANAR} 
\§path expression>.

\LOG
\initials{LDF 2004.10.25.}
Added this rule.

\initials{LDF 2005.10.24.}
Changed |path_like_expression| to |path_expression|.
Removed debugging code.

\initials{LDF 2005.10.26.}
@:BUG FIX@> BUG FIX:  Now allocating memory for |bool* i|.
\ENDLOG

@<Define rules@>=
@=boolean_primary: path_expression IS_COPLANAR path_expression@>@/
{

    bool* i = new bool;

    Path* p = static_cast<Path*>(@=$1@>);
    Path* q = static_cast<Path*>(@=$3@>);
   
    *i = (p && q && p->is_coplanar(*q)) ? 1 : 0;

    delete p;
    delete q;

    @=$$@> = static_cast<void*>(i);

};

@q ** (2) boolean_primary --> IS_TRIANGULAR path_expression.@>
@*1 \§boolean primary> $\longrightarrow$ \.{IS\_TRIANGULAR} 
\§path expression>.
\initials{LDF 2005.01.23.}

\LOG
\initials{LDF 2005.01.23.}
Added this rule.

\initials{LDF 2005.10.24.}
Changed |path_like_expression| to |path_expression|.
Removed debugging code.

\initials{LDF 2005.10.26.}
@:BUG FIX@> BUG FIX:  Now allocating memory for |bool* i|.
\ENDLOG

@<Define rules@>=
@=boolean_primary: IS_TRIANGULAR path_expression@>@/
{

    Path* p = static_cast<Path*>(@=$2@>);

    bool* i = new bool;

    *i = (p && p->is_triangular()) ? true : false;

    delete p;
 
    @=$$@> = static_cast<void*>(i);

};

@q ** (2) boolean_primary --> IS_QUADRILATERAL path_expression.@>
@*1 \§boolean primary> 
$\longrightarrow$ \.{IS\_QUADRILATERAL} \§path expression>.
\initials{LDF 2005.01.25.}

\LOG
\initials{LDF 2005.01.25.}
Added this rule.

\initials{LDF 2005.10.24.}
Changed |path_like_expression| to |path_expression|.
Removed debugging code.

\initials{LDF 2005.10.26.}
@:BUG FIX@> BUG FIX:  Now allocating memory for |bool* i|.
\ENDLOG

@<Define rules@>=
@=boolean_primary: IS_QUADRILATERAL path_expression@>@/
{

    Path* p = static_cast<Path*>(@=$2@>);

    bool* i = new bool;

    *i = (p && p->is_quadrilateral(static_cast<Scanner_Node>(parameter))) 
            ? true : false;

    delete p;

    @=$$@> = static_cast<void*>(i);
};


@q ** (2) boolean_primary --> IS_RECTANGULAR path_expression.@>
@*1 \§boolean primary> 
$\longrightarrow$ \.{IS\_RECTANGULAR} \§path expression>.

\LOG
\initials{LDF 2004.10.14.}
Added this rule.

\initials{LDF 2005.10.24.}
Changed |path_like_expression| to |path_expression|.
Removed debugging code.

\initials{LDF 2005.10.26.}
@:BUG FIX@> BUG FIX:  Now allocating memory for |bool* i|.
\ENDLOG

@<Define rules@>=
@=boolean_primary: IS_RECTANGULAR path_expression@>@/
{

    Path* p = static_cast<Path*>(@=$2@>);

    bool* i = new bool;

    *i = (p && p->is_rectangular()) ? true : false;

    delete p; 
 
    @=$$@> = static_cast<void*>(i);

};


@q ** (2) boolean_primary --> IS_SQUARE path_expression.@>
@*1 \§boolean primary> 
$\longrightarrow$ \.{IS\_SQUARE} \§path expression>.

\LOG
\initials{LDF 2004.10.14.}
Added this rule.

\initials{LDF 2005.10.24.}
Changed |path_like_expression| to |path_expression|.
Removed debugging code.

\initials{LDF 2005.10.26.}
@:BUG FIX@> BUG FIX:  Now allocating memory for |bool* i|.
\ENDLOG

@<Define rules@>=
@=boolean_primary: IS_SQUARE path_expression@>@/
{

    Path* p = static_cast<Path*>(@=$2@>);

    bool* i = new bool;
   
    *i = (p && p->is_square()) ? 1 : 0;

    delete p;

    @=$$@> = static_cast<void*>(i);

};

@q ** (2) boolean_primary --> IS_POLYGONAL path_expression.@>
@*1 \§boolean primary> 
$\longrightarrow$ \.{IS\_POLYGONAL} \§path expression>.
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.02.11.}
Added this rule.

\initials{LDF 2005.10.24.}
Changed |path_like_expression| to |path_expression|.
Removed debugging code.

\initials{LDF 2005.10.26.}
@:BUG FIX@> BUG FIX:  Now allocating memory for |bool* i|.
\ENDLOG

@<Define rules@>=
@=boolean_primary: IS_POLYGONAL path_expression@>@/
{

    Path* p = static_cast<Path*>(@=$2@>);

    bool* i = new bool;

    *i = (p && p->is_polygonal(static_cast<Scanner_Node>(parameter))) ? true : false;

    delete p;
  
    @=$$@> = static_cast<void*>(i);

};



@q ** (2) boolean_primary --> IS_CONVEX_POLYGONAL path_expression.@>
@*1 \§boolean primary> 
$\longrightarrow$ \.{IS\_CONVEX\_POLYGONAL} \§path expression>.
\initials{LDF 2005.02.08.}

\LOG
\initials{LDF 2005.02.08.}
Added this rule.

\initials{LDF 2005.10.24.}
Changed |path_like_expression| to |path_expression|.
Removed debugging code.

\initials{LDF 2005.10.26.}
@:BUG FIX@> BUG FIX:  Now allocating memory for |bool* i|.
\ENDLOG

@<Define rules@>=
@=boolean_primary: IS_CONVEX_POLYGONAL path_expression@>@/
{

    Path* p = static_cast<Path*>(@=$2@>);

    bool* i = new bool;

    *i = (p && p->is_convex_polygonal(static_cast<Scanner_Node>(parameter))) 
         ? true : false;

    delete p;

    @=$$@> = static_cast<void*>(i);

};


@q ** (2) boolean_primary --> IS_ELLIPTICAL path_expression.@>
@*1 \§boolean primary> 
$\longrightarrow$ \.{IS\_ELLIPTICAL} \§path expression>.

\LOG
\initials{LDF 2004.10.16.}
Added this rule.

\initials{LDF 2005.10.24.}
Changed |path_like_expression| to |path_expression|.
Removed debugging code.

\initials{LDF 2005.10.26.}
@:BUG FIX@> BUG FIX:  Now allocating memory for |bool* i|.
\ENDLOG

@<Define rules@>=
@=boolean_primary: IS_ELLIPTICAL path_expression@>@/
{

    Path* p = static_cast<Path*>(@=$2@>);

    bool* i = new bool;

    *i = (p && p->is_elliptical()) ? 1 : 0;

    delete p;

    @=$$@> = static_cast<void*>(i);

};

@q ** (2) boolean_primary --> IS_ELLIPTICAL ellipse_expression.@>
@*1 \§boolean primary> 
$\longrightarrow$ \.{IS\_ELLIPTICAL} \§ellipse expression>.
\initials{LDF 2005.12.15.}

\LOG
\initials{LDF 2005.12.15.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=boolean_primary: IS_ELLIPTICAL ellipse_expression@>@/
{

    Ellipse* p = static_cast<Ellipse*>(@=$2@>);

    bool* i = new bool;

    *i = (p && p->is_elliptical()) ? 1 : 0;

    delete p;

    @=$$@> = static_cast<void*>(i);

};

@q ** (2) boolean_primary --> IS_ELLIPTICAL circle_expression.@>
@*1 \§boolean primary> 
$\longrightarrow$ \.{IS\_ELLIPTICAL} \§circle expression>.
\initials{LDF 2005.12.15.}

\LOG
\initials{LDF 2005.12.15.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=boolean_primary: IS_ELLIPTICAL circle_expression@>@/
{

    Circle* p = static_cast<Circle*>(@=$2@>);

    bool* i = new bool;

    *i = (p && p->is_elliptical()) ? 1 : 0;

    delete p;

    @=$$@> = static_cast<void*>(i);

};




@q ** (2) boolean_primary --> IS_CIRCULAR path_expression.@>
@*1 \§boolean primary> 
$\longrightarrow$ \.{IS\_CIRCULAR} \§path expression>.

\LOG
\initials{LDF 2004.10.18.}
Added this rule.

\initials{LDF 2005.10.24.}
Changed |path_like_expression| to |path_expression|.
Removed debugging code.

\initials{LDF 2005.10.26.}
@:BUG FIX@> BUG FIX:  Now allocating memory for |bool* i|.
\ENDLOG

@<Define rules@>=
@=boolean_primary: IS_CIRCULAR path_expression@>@/
{

    Path* p = static_cast<Path*>(@=$2@>);

    bool* i = new bool;
   
    *i = (p && p->is_circular()) ? 1 : 0;

    delete p;
 
    @=$$@> = static_cast<void*>(i);
};

@q ** (2) boolean_primary --> IS_SPHERICAL sphere_expression.@>
@*1 \§boolean primary> 
$\longrightarrow$ \.{IS\_SPHERICAL} \§sphere expression>.

\LOG
\initials{LDF 2005.10.26.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=boolean_primary: IS_SPHERICAL sphere_expression@>@/
{
   @=$$@> = is_whatever_func<Sphere>(static_cast<Sphere*>(@=$2@>), 
                                     parameter); 

};

@q * (1) |Point| locations.@> 


@q ** (2) boolean_primary --> point_expression IS_ON_SEGMENT @>
@q ** (2) path_expression.                                   @>
@*1 \§boolean primary> 
$\longrightarrow$ \§point expression> \.{IS\_ON\_SEGMENT} 
\§path expression>.

\LOG
\initials{LDF 2004.10.22.}
Added this rule.

\initials{LDF 2005.10.24.}
Changed |path_like_expression| to |path_expression|.
Removed debugging code.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=boolean_primary: point_expression IS_ON_SEGMENT path_expression@>@/
{

    bool* b = new bool;

    Point* p = static_cast<Point*>(@=$1@>);    
    
    Path* q = static_cast<Path*>(@=$3@>);

    if (p && q && q->is_linear())
       {
            Point r0 = q->get_point(0);
            Point r1 = q->get_last_point();

            Bool_Real br = p->is_on_segment(r0, r1);

            *b = br.first;

       }

     else /* |!(p && q && q->is_linear()q->is_linear())|  */
        {
            *b = false;

        }  /* |else| (|!(p && q && q->is_linear()q->is_linear())|)  */

      delete p;
      delete q;

      @=$$@> = static_cast<void*>(b);


@q **** (4) @>   

};

@q ** (2) boolean_primary --> point_expression IS_ON_LINE @>
@q ** (2) path_expression.                           @>
@*1 \§boolean primary> $\longrightarrow$ \§point expression> 
\.{IS\_ON\_LINE} \§path expression>.

\LOG
\initials{LDF 2004.10.22.}
Added this rule.

\initials{LDF 2005.10.24.}
Changed |path_like_expression| to |path_expression|.
Removed debugging code.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=boolean_primary: point_expression IS_ON_LINE path_expression@>@/
{

    bool* b = new bool;
   
    Point* p = static_cast<Point*>(@=$1@>); 
   
    Path* q = static_cast<Path*>(@=$3@>);

    if (p && q && q->is_linear())
       {
            Point r0 = q->get_point(0);
            Point r1 = q->get_last_point();

            Bool_Real br = p->is_on_line(r0, r1);

            *b = br.first;
          
       }  /* |if (p && q && q->is_linear())|  */


     else /* |!(p && q && q->is_linear())|  */
        { 
            *b = false;

        }  /* |else | (|!(p && q && q->is_linear())|)  */


     delete p;
     delete q;

     @=$$@> = static_cast<void*>(b);

};

@q ** (2) boolean_primary --> point_expression IS_ON_PLANE plane_expression.@>
@*1 \§boolean primary> $\longrightarrow$ \§point expression> 
\.{IS\_ON\_PLANE} \§plane expression> \§with tolerance optional>.

\LOG
\initials{LDF 2007.11.08.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=boolean_primary: point_expression IS_ON_PLANE plane_expression with_tolerance_optional@>@/
{

    bool* b = new bool;
   
    Point* p = static_cast<Point*>(@=$1@>); 
   
    Plane* q = static_cast<Plane*>(@=$3@>);

    if (p && q)
       {
            *b = p->is_on_plane(*q, @=$4@>);

          
       }  /* |if (p && q)|  */


     else /* |!(p && q)|  */
        { 
            *b = false;

        }  /* |else | (|!(p && q)|)  */


     delete p;
     delete q;

     @=$$@> = static_cast<void*>(b);

};

@q ** (2) boolean_primary --> point_expression IS_IN_TRIANGLE        @>
@q ** (2) LEFT_PARENTHESIS point_expression COMMA                    @>
@q ** (2) point_expression COMMA point_expression RIGHT_PARENTHESIS. @>

@*1 \§boolean primary> $\longrightarrow$ \§point expression> 
\.{IS\_IN\_TRIANGLE} \.{LEFT\_PARENTHESIS} \§point expression> \.{COMMA}
\§point expression> \.{COMMA} \§point expression> \.{RIGHT\_PARENTHESIS}.

\LOG
\initials{LDF 2004.10.22.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=boolean_primary: point_expression IS_IN_TRIANGLE @>@/
@=LEFT_PARENTHESIS point_expression COMMA @>@/                   
@=point_expression COMMA point_expression RIGHT_PARENTHESIS@>@/


{

   Point* p = static_cast<Point*>(@=$1@>); 

   Point* q0 = static_cast<Point*>(@=$4@>); 
   Point* q1 = static_cast<Point*>(@=$6@>); 
   Point* q2 = static_cast<Point*>(@=$8@>); 

   bool* b;
   
   b = new bool;

@q **** (4) |new bool| succeeded.@> 

@ |new bool| succeeded.
\initials{LDF 2004.10.26.}

@<Define rules@>=

   *b = p->is_in_triangle(*q0, *q1, *q2);

   @=$$@> = static_cast<void*>(b);

   delete p;
   delete q0;
   delete q1;
   delete q2;

};

@q ** (2) boolean_primary --> point_expression IS_ON_ELLIPSE @>
@q ** (2) ellipse_expression.                                   @>
@*1 \§boolean primary> 
$\longrightarrow$ \§point expression> \.{IS\_ON\_ELLIPSE} 
\§ellipse expression>.

\LOG
\initials{LDF 2005.11.02.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=boolean_primary: point_expression IS_ON_ELLIPSE ellipse_expression@>@/
{



    Point* p = static_cast<Point*>(@=$1@>);    
    
    Ellipse* e = static_cast<Ellipse*>(@=$3@>);

    if (!(p && e))
       {
           delete p;
           delete e;
           @=$$@> = static_cast<void*>(0);
       }

    else 
       {
           bool* b = new bool;

           *b = !(e->location(*p)); 

           delete p;
           delete e;

           @=$$@> = static_cast<void*>(b);
       }



@q **** (4) @>   

};

@q ** (2) boolean_primary --> bool_point_expression IS_ON_ELLIPSE @>
@q ** (2) ellipse_expression.                                   @>
@*1 \§boolean primary> 
$\longrightarrow$ \§bool-point expression> \.{IS\_ON\_ELLIPSE} 
\§ellipse expression>.

\LOG
\initials{LDF 2005.11.02.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=boolean_primary: bool_point_expression IS_ON_ELLIPSE ellipse_expression@>@/
{

    Bool_Point* bp = static_cast<Bool_Point*>(@=$1@>);    

    Ellipse* e = static_cast<Ellipse*>(@=$3@>);

    if (!(bp && e))
       {
           delete bp;
           delete e;
           @=$$@> = static_cast<void*>(0);
       }

    else 
       {
           bool* b = new bool;
           *b = !(e->location(bp->pt)); 

           delete bp;
           delete e;

           @=$$@> = static_cast<void*>(b);
       }

@q **** (4) @>   

};

@q ** (2) boolean_primary --> ARE_ON_CONIC_SECTION        @>
@q ** (2) point_expression_list with_tolerance_optional.  @>
@*1 \§boolean primary> $\longrightarrow$ 
\.{ARE\_ON\_CONIC\_SECTION} \§point expression list> 
\§with tolerance optional>.
\initials{LDF 2007.07.09.}

\LOG
\initials{LDF 2007.07.09.}
Added this rule.

\initials{LDF 2007.07.15.}
Now using \§point expression list> instead of \§point expression> \.{COMMA}
$\ldots$.

\initials{LDF 2007.07.15.}
Added \§with tolerance optional> symbol to right-hand side.

\initials{LDF 2007.07.27.}
Now calling |Pointer_Vector::cull| on the |Pointer_Vector<Point>| pointed to 
by |pv|.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=boolean_primary: ARE_ON_CONIC_SECTION point_expression_list@>@/
@=with_tolerance_optional@>@/
{
   bool* b = new bool;
   
   Point* p[5];

   Pointer_Vector<Point>* pv = static_cast<Pointer_Vector<Point>*>(@=$2@>);  

   pv->cull(Sorting::SORTING_EQUAL,
            @=$3@>,
            static_cast<Scanner_Node>(parameter),
            static_cast<Point*>(0));

  
   *b = Conic_Section::are_on_conic_section(pv,
                                            parameter,
                                            @=$3@>);

   delete pv;
   pv = 0;

   @=$$@> = static_cast<void*>(b);  

@q **** (4) @>   

};


@q ** (2) boolean_primary --> ARE_ON_CONIC_SECTION          @>
@q ** (2) point_vector_expression with_tolerance_optional.  @>
@*1 \§boolean primary> $\longrightarrow$ 
\.{ARE\_ON\_CONIC\_SECTION} \§point vector expression>
\§with tolerance optional>.
\initials{LDF 2007.07.09.}

\LOG
\initials{LDF 2007.07.09.}
Added this rule.

\initials{LDF 2007.07.15.}
Added \§with tolerance optional> symbol to right-hand side.

\initials{LDF 2007.07.27.}
Now calling |Pointer_Vector::cull| on the |Pointer_Vector<Point>| pointed to 
by |pv|.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=boolean_primary: ARE_ON_CONIC_SECTION point_vector_expression@>@/
@=with_tolerance_optional@>@/
{

   bool* b = new bool;
   
   Pointer_Vector<Point>* pv = static_cast<Pointer_Vector<Point>*>(@=$2@>);

   pv->cull(Sorting::SORTING_EQUAL,
            @=$3@>,
            static_cast<Scanner_Node>(parameter),
            static_cast<Point*>(0));


   *b = Conic_Section::are_on_conic_section(pv, parameter, @=$3@>);

   delete pv;
   pv = 0;

   @=$$@> = static_cast<void*>(b);  


@q **** (4) @>   

};

@q ** (2) boolean_primary --> point_primary IS_ON_SPHERE @>
@q ** (2) sphere_secondary.                              @>

@*1 \§boolean primary> 
$\longrightarrow$ \§point primary> \.{IS\_ON\_SPHERE} 
\§sphere secondary>.

\LOG
\initials{LDF 2005.10.26.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=boolean_primary: point_primary IS_ON_SPHERE sphere_secondary predicate_clause@>@/
{
   @=$$@> = Scan_Parse::is_on_sphere_func(@=$1@>, @=$3@>, parameter);
   
};


@q ** (2) boolean_primary --> ARE_DISTINCT point_list with_tolerance_optional@>

@*1 \§boolean primary> $\longrightarrow$ \.{ARE\_DISTINCT} 
\§point expression list> \§with tolerance optional>.
\initials{LDF 2007.07.22.}

\LOG
\initials{LDF 2007.07.22.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=boolean_primary: ARE_DISTINCT point_expression_list with_tolerance_optional@>@/
{

   Pointer_Vector<Point>* pv = static_cast<Pointer_Vector<Point>*>(@=$2@>); 
   
   bool* b = new bool;

   *b = (pv == static_cast<Pointer_Vector<Point>*>(0)) ? 
            false : pv->are_distinct(@=$3@>, 10, parameter);

   delete pv;

   @=$$@> = static_cast<void*>(b);

};



@q ** (2) boolean_primary --> ARE_DISTINCT point_vector_primary with_tolerance_optional@>

@*1 \§boolean primary> $\longrightarrow$ \.{ARE\_DISTINCT} 
\§point vector primary> \§with tolerance optional>.
\initials{LDF 2007.07.27.}

\LOG
\initials{LDF 2007.07.27.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=boolean_primary: ARE_DISTINCT point_vector_primary with_tolerance_optional@>@/
{

   Pointer_Vector<Point>* pv = static_cast<Pointer_Vector<Point>*>(@=$2@>); 
   
   bool* b = new bool;

   *b = (pv == static_cast<Pointer_Vector<Point>*>(0)) ? 
            false : pv->are_distinct(@=$3@>, 10, parameter);

   delete pv;

   @=$$@> = static_cast<void*>(b);

};



@q ** (2) predicate_clause.@>   
@*1 \§predicate clause>.
\initials{LDF 2005.10.26.}

\LOG
\initials{LDF 2005.10.26.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> predicate_clause@>@/

@q *** (3) predicate_clause --> /* Empty */.@>   
@*2 \§predicate clause> $\longrightarrow$ \.{Empty}.
\initials{LDF 2005.10.26.}

\LOG
\initials{LDF 2005.10.26.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=predicate_clause: /* Empty */@>@/
{
   @=$$@> = static_cast<void*>(0);
}   


@q *** (3) predicate_clause --> WITH_TOLERANCE numeric_expression.@>   
@*2 \§predicate clause> $\longrightarrow$ 
\.{WITH\_TOLERANCE} \§numeric expression>.
\initials{LDF 2005.10.26.}

\LOG
\initials{LDF 2005.10.26.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=predicate_clause: WITH_TOLERANCE numeric_expression@>@/
{

   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

   scanner_node->tolerance = new real;
   *(scanner_node->tolerance) = @=$2@>; 

   @=$$@> = static_cast<void*>(0);
}   

@q ** (2) boolean_primary --> path_expression IS_PARALLEL @>
@q ** (2) path_expression.                                 @>

@*1 \§boolean primary> $\longrightarrow$ \§path expression> 
\.{IS\_PARALLEL} \§path expression>.

\LOG
\initials{LDF 2004.10.23.}
Added this rule.

\initials{LDF 2005.10.24.}
Changed |path_like_expression| to |path_expression|.
Removed debugging code.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=boolean_primary: path_expression IS_PARALLEL path_expression@>@/
{

   Path* p = static_cast<Path*>(@=$1@>);
   Path* q = static_cast<Path*>(@=$3@>);  

   bool* b = new bool;
   

   if (p && q && p->is_linear() && q->is_linear())
      {
         Line l = p->get_line();
         Line m = q->get_line();

         Bool_Real_Point brp = l.get_distance(m, static_cast<Scanner_Node>(parameter));

         *b = !(brp.b); 

      }  /* |if (p && q && p->is_linear() && q->is_linear())|  */

   else /* |!(p && q && p->is_linear() && q->is_linear())|  */
      {
         *b = false;

      }  /* |else| (|!(p && q && p->is_linear() && q->is_linear())|)  */

    delete p;
    delete q;
    @=$$@> = static_cast<void*>(b);

};

@q ** (2) boolean_primary --> path_expression IS_COLINEAR @>
@q ** (2) path_expression.                                @>

@*1 \§boolean primary> $\longrightarrow$ \§path expression> 
\.{IS\_COLINEAR} \§path expression>.

\LOG
\initials{LDF 2004.10.26.}
Added this rule.

\initials{LDF 2005.10.24.}
Changed |path_like_expression| to |path_expression|.
Removed debugging code.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=boolean_primary: path_expression IS_COLINEAR path_expression@>@/
{


   Path* p = static_cast<Path*>(@=$1@>);
   Path* q = static_cast<Path*>(@=$3@>);  

   bool* b = new bool;
   
   if (p && q && p->is_linear() && q->is_linear())
      {  

         Point P0 = p->get_point(0);
         Point Q0 = q->get_point(0);
         Point Q1 = q->get_last_point();

         Point a = p->get_last_point() - P0; 
         a.unit_vector(true);

         Point r = Q1 - Q0;
         r.unit_vector(true);
        
         *b = ((a == r || a == -r) && P0.is_on_line(Q0, Q1).first);

      }  /* |if (p && q && p->is_linear() && q->is_linear())|  */

   else /* |!(p && q && p->is_linear() && q->is_linear())|  */
      {
         *b = false;         

      }  /* |else| (|!(p && q && p->is_linear() && q->is_linear())|)  */

    delete p;
    delete q;
    @=$$@> = static_cast<void*>(b);

};

@q ***** (5) boolean_primary --> LAST boolean_vector_expression.@>

@*4 \§boolean primary> $\longrightarrow$ 
\.{LAST} \§boolean vector expression>.
\initials{LDF 2005.01.07.}

\LOG
\initials{LDF 2005.01.07.}
Added this rule.

\initials{LDF 2007.07.09.}
@:BUG FIX@> BUG FIX:  Fixed memory leak.  
Now deleting |Pointer_Vector<bool>* pv|.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=boolean_primary: LAST boolean_vector_expression@>@/
{ 
@q ******* (7) @> 

   bool* b;

   b = new bool;

@q ******* (7) @> 

   Pointer_Vector<bool>* pv 
      = static_cast<Pointer_Vector<bool>*>(@=$2@>);

@q ******* (7) Error handling:  |pv == 0|.@> 

@ Error handling:  |pv == 0|.
\initials{LDF 2005.01.07.}

@<Define rules@>=

   if (pv == static_cast<Pointer_Vector<bool>*>(0))
      {

          *b = false;

          @=$$@> = static_cast<void*>(b);

      }  /* |if (pv == 0)|  */

@q ******* (7) Error handling:  |pv->ctr == 0|.@> 

@ Error handling:  |pv->ctr == 0|.
\initials{LDF 2005.01.07.}

@<Define rules@>=

   else if (pv->ctr == 0)
      {

          *b = false;

          @=$$@> = static_cast<void*>(b);

      }  /* |else if (pv->ctr == 0)|  */

@q ******* (7) |pv != 0 && pv->ctr > 0|.@> 

@ |pv != 0 && pv->ctr > 0|.  Set |@=$$@>| to |*(pv->v[pv->ctr - 1])|.
\initials{LDF 2005.01.07.}

@<Define rules@>=

   else 
      {
         *b = *(pv->v[pv->ctr - 1]);
         @=$$@> = static_cast<void*>(b); 
      }
@q ******* (7) @> 

   delete pv;
   
};


@q * (1) boolean_secondary.  @>
@* \§boolean secondary>.

\LOG
\initials{LDF 2004.05.17.}  
Added this section.

\initials{LDF 2004.10.26.}
Changed type of |boolean_secondary| from |int_value| 
to |pointer_value|.
\ENDLOG 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> boolean_secondary@>
  
@q ** (2) boolean secondary --> boolean_primary.@>
@*1 \§boolean secondary> $\longrightarrow$ \§boolean primary>.

\LOG
\initials{LDF 2004.05.17.}  
Added this rule.
\ENDLOG 

@<Define rules@>=
@=boolean_secondary: boolean_primary@>@/
{

  @=$$@> = @=$1@>;

};

@q ** (2) boolean secondary --> boolean_secondary AND boolean_primary.@>
@*1 \§boolean secondary> $\longrightarrow$ \§boolean secondary> \.{AND} 
\§boolean primary>.

\LOG
\initials{LDF 2004.05.17.}
Added this rule.

\initials{LDF 2004.07.25.}
Changed the way |@=$$@>| is set.  The way it was set formerly required
an implicit conversion from |bool| to |int|.
\ENDLOG 

@<Define rules@>=
@=boolean_secondary: boolean_secondary AND boolean_primary@>@/
{

  bool* i = static_cast<bool*>(@=$1@>);
  bool* j = static_cast<bool*>(@=$3@>);


  *i = (*i && *j) ? 1 : 0;

  @=$$@> = static_cast<void*>(i); 

  delete j;

};

@q * (1) boolean_tertiary.@>
@* \§boolean tertiary>.

\LOG
\initials{LDF 2004.05.17.}  
Added this type declaration.

\initials{LDF 2004.10.26.}
Changed type of |boolean_tertiary| from |int_value| 
to |pointer_value|.
\ENDLOG 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> boolean_tertiary@>

@q ** (2) boolean tertiary --> boolean_secondary.@>
@*1 \§boolean tertiary> $\longrightarrow$ \§boolean secondary>.

\LOG
\initials{LDF 2004.05.17.}  
Added this rule.
\ENDLOG 

@<Define rules@>=
@=boolean_tertiary: boolean_secondary@>@/
{

  @=$$@> = @=$1@>;

};

@q *** (3) boolean tertiary --> boolean_tertiary OR boolean_secondary.@>
@*2 \§boolean tertiary> $\longrightarrow$ \§boolean tertiary> \.{OR}
\§boolean secondary>.

\LOG
\initials{LDF 2004.05.17.}  
Added this rule.
\ENDLOG 

@<Define rules@>=
@=boolean_tertiary: boolean_tertiary OR boolean_secondary@>@/
{

  bool* i = static_cast<bool*>(@=$1@>);
  bool* j = static_cast<bool*>(@=$3@>);

  
  *i = (*i || *j) ? 1 : 0;

  @=$$@> = static_cast<void*>(i);

  delete j; 

};

@q * (1) boolean expression.@>
@* \§boolean expression>.

\LOG
\initials{LDF 2004.05.17.}  
Added this type declaration.

\initials{LDF 2004.10.26.}
Changed type of |boolean_expression| from |int_value| 
to |pointer_value|.
\ENDLOG 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> boolean_expression@>

@q ** (2) boolean expression --> boolean_tertiary.@>
@*1 \§boolean expression> $\longrightarrow$ \§boolean tertiary>.

\LOG
\initials{LDF 2004.05.17.}  
Added this rule.
\ENDLOG 

@<Define rules@>=
@=boolean_expression: boolean_tertiary@>@/
{

  @=$$@> = @=$1@>;

};

@q ** (2) boolean expression --> numeric_expression relation numeric_tertiary.@>
@*1 \§boolean expression> $\longrightarrow$ \§numeric expression> 
\§relation> \§numeric tertiary>.

\LOG
\initials{LDF 2004.05.17.}  
Added this rule.
\ENDLOG 

@<Define rules@>=
@=boolean_expression: numeric_expression relation numeric_tertiary@>@/
{
    bool* i;
   
    i = new bool;

@q **** (4) |new bool| succeeded.@> 

@ |new bool| succeeded.
\initials{LDF 2004.10.26.}

@<Define rules@>=

  if (@=$2@> == LESS)
    *i = (@=$1@> < @=$3@>) ? 1 : 0;

  else if (@=$2@> == LESS_OR_EQUAL)
    *i = (@=$1@> <= @=$3@>) ? 1 : 0;


  else if (@=$2@> == GREATER)
    *i = (@=$1@> > @=$3@>) ? 1 : 0;


  else if (@=$2@> == GREATER_OR_EQUAL)
    *i = (@=$1@> >= @=$3@>) ? 1 : 0;


  else if (@=$2@> == EQUAL)
    *i = (@=$1@> == @=$3@>) ? 1 : 0;

  else if (@=$2@> == NOT_EQUAL)
    *i = (@=$1@> != @=$3@>) ? 1 : 0;

  else
    {

      *i = 0;

    } /* |else|  (Invalid value for |relation|).  */@;

   @=$$@> = static_cast<void*>(i);

};

@q ** (2) boolean expression --> point_expression relation point_tertiary.@>
@*1 \§boolean expression> $\longrightarrow$ \§point expression> 
\§relation> \§point tertiary>.

\LOG
\initials{LDF 2004.05.17.}  
Added this rule.
\ENDLOG 

@<Define rules@>=
@=boolean_expression: point_expression relation point_tertiary@>@/
{

    bool* i;


    Point* p = static_cast<Point*>(@=$1@>);

    Point* q = static_cast<Point*>(@=$3@>);

   
    i = new bool;

@q **** (4) |new bool| succeeded.@> 

@ |new bool| succeeded.
\initials{LDF 2004.10.26.}

@<Define rules@>=

  real p_mag;
  real q_mag;
 
@q *** (3) Error handling:  |(p == 0 || q == 0)|.@>

@ Error handling:  |(p == 0 || q == 0)|.

@<Define rules@>=

  if (p == static_cast<Point*>(0) || q == static_cast<Point*>(0))
    {

      *i = 0;
       
      @=$$@> = static_cast<void*>(i);

    } /* |if (p == 0 || q == 0)|  */


@q *** (3) Error handling:  |(*p == INVALID_POINT || *q == INVALID_POINT)|.@>

@ Error handling:  |(*p == INVALID_POINT || *q == INVALID_POINT)|.

@<Define rules@>=
  
  else if (*p == INVALID_POINT || *q == INVALID_POINT)
    {

     @=$$@> = static_cast<void*>(i);

    } /* |if (*p == INVALID_POINT || *q == INVALID_POINT)|  */


@q **** (4) |Points| are valid.@>
@ |Points| are valid.
\initials{LDF 2004.10.26.}

@<Define rules@>=

  else  
     {
         p_mag = p->magnitude(); 
         q_mag = q->magnitude(); 

         if (@=$2@> == EQUAL)
           *i = (*p == *q) ? 1 : 0;

         else if (@=$2@> == NOT_EQUAL)
           *i = (*p != *q) ? 1 : 0;

         else if (@=$2@> == LESS)
           *i = (p_mag < q_mag) ? 1 : 0;


         else if (@=$2@> == LESS_OR_EQUAL)
            *i = (p_mag <= q_mag) ? 1 : 0;

         else if (@=$2@> == GREATER)
           *i = (p_mag > q_mag) ? 1 : 0;

         else if (@=$2@> == GREATER_OR_EQUAL)
           *i = (p_mag >= q_mag) ? 1 : 0;
  
         else
           {

             *i = 0;
         
           } /* |else|  (Invalid value for |relation|).  */

       @=$$@> = static_cast<void*>(i);

    }    /* |else| (|Points| are valid.)  */
     

@q **** (4) @>              
                  
  delete p;
  delete q;
  
};

@q *** (3) boolean expression --> boolean_expression relation boolean_tertiary.@>
@ \§boolean expression> $\longrightarrow$ \§boolean expression> 
\§relation> \§boolean tertiary>.
\initials{LDF 2004.05.17.}  

\LOG
\initials{LDF 2004.05.17.}  
Added this rule.
\ENDLOG 

@<Define rules@>=
@=boolean_expression: boolean_expression relation boolean_tertiary@>@/
{

   bool* i = static_cast<bool*>(@=$1@>); 

   bool* j = static_cast<bool*>(@=$3@>); 


  if (@=$2@> == EQUAL)
    *i = (*i == *j) ? 1 : 0;

  else if (@=$2@> == NOT_EQUAL)
    *i = (*i != *j) ? 1 : 0;

  else if (@=$2@> == LESS)
    *i = (*i < *j) ? 1 : 0;

  else if (@=$2@> == LESS_OR_EQUAL)
     *i = (*i <= *j) ? 1 : 0;

  else if (@=$2@> == GREATER)
    *i = (*i > *j) ? 1 : 0;

  else if (@=$2@> == GREATER_OR_EQUAL)
    *i = (*i >= *j) ? 1 : 0;
  
  else
    {

      *i = 0;

    } /* |else|  (Invalid value for |relation|).  */

  @=$$@> = static_cast<void*>(i);
  
  delete j; 

};


@q ** (2) boolean expression --> transform_expression EQUAL transform_tertiary.@>
@*1 \§boolean expression> $\longrightarrow$ \§transform expression> 
\.{EQUAL} \§transform tertiary>.
\initials{LDF 2007.10.14.}

\LOG
\initials{LDF 2007.10.14.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=boolean_expression: transform_expression EQUAL transform_tertiary @>
@=with_tolerance_optional@>@/
{
   bool* b = new bool;

   Transform* t = static_cast<Transform*>(@=$1@>);
   Transform* u = static_cast<Transform*>(@=$3@>);

   if (!(t && u))
       *b = false;
   else
      *b = t->is_equal(*u, @=$4@>);

   delete t;
   delete u;
   @=$$@> = static_cast<void*>(b);   

};

@q ** (2) boolean expression --> transform_expression NOT_EQUAL transform_tertiary.@>
@*1 \§boolean expression> $\longrightarrow$ \§transform expression> 
\.{NOT\_EQUAL} \§transform tertiary>.
\initials{LDF 2007.10.14.}

\LOG
\initials{LDF 2007.10.14.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=boolean_expression: transform_expression NOT_EQUAL transform_tertiary @>
@=with_tolerance_optional@>@/
{
   bool* b = new bool;

   Transform* t = static_cast<Transform*>(@=$1@>);
   Transform* u = static_cast<Transform*>(@=$3@>);

   if (!(t && u))
       *b = false;
   else
      *b = !t->is_equal(*u, @=$4@>);

   delete t;
   delete u;
   @=$$@> = static_cast<void*>(b);   

};


@q ** (2) boolean expression --> triangle_expression EQUAL triangle_tertiary.  @>
@*1 \§boolean expression> $\longrightarrow$ \§triangle expression> 
\.{EQUAL} \§triangle tertiary>.
\initials{LDF 2010.08.11.}

\LOG
\initials{LDF 2010.08.11.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=boolean_expression: triangle_expression EQUAL triangle_tertiary @>
@=with_tolerance_optional@>@/
{
   bool* b = new bool;

   Triangle* t = static_cast<Triangle*>(@=$1@>);
   Triangle* u = static_cast<Triangle*>(@=$3@>);

   if (!(t && u))
       *b = false;
   else
   {
      real tolerance = (@=$4@> >= 0) ? @=$4@> : .001;
      *b = t->is_equal(*u, tolerance);
   }

   delete t;
   delete u;
   @=$$@> = static_cast<void*>(b);   

};

@q ** (2) boolean expression --> triangle_expression NOT_EQUAL triangle_tertiary.@>
@*1 \§boolean expression> $\longrightarrow$ \§triangle expression> 
\.{NOT\_EQUAL} \§triangle tertiary>.
\initials{LDF 2010.08.11.}

\LOG
\initials{LDF 2010.08.11.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=boolean_expression: triangle_expression NOT_EQUAL triangle_tertiary @>
@=with_tolerance_optional@>@/
{
   bool* b = new bool;

   Triangle* t = static_cast<Triangle*>(@=$1@>);
   Triangle* u = static_cast<Triangle*>(@=$3@>);

   if (!(t && u))
       *b = false;
   else
   {
      real tolerance = (@=$4@> >= 0) ? @=$4@> : .001;
      *b = !t->is_equal(*u, tolerance);
   }

   delete t;
   delete u;
   @=$$@> = static_cast<void*>(b);   

};




@q ** (2) boolean expression --> string_expression relation string_tertiary.@>
@*1 \§boolean expression> $\longrightarrow$ \§string expression> 
\§relation> \§string tertiary>.


\TODO
@q { @>
@:TO DO}{{\bf TO DO}@>
@q } @> 
@q !! TO DO:  @>
\initials{LDF 2004.10.02.}
Write this rule.
\ENDTODO

@q * (1) relation.@>
@* \§relation>.

\LOG
\initials{LDF 2004.05.17.}  
Added this type declaration.
\ENDLOG 

@<Type declarations for non-terminal symbols@>=
@=%type <int_value> relation@>

@q ** (2) relation --> LESS.  @>
@*1 \§relation> $\longrightarrow$ \.{LESS}.

\LOG
\initials{LDF 2004.05.17.}  
Added this rule.
\ENDLOG 

@<Define rules@>=
@=relation: LESS@>@/
{

   @=$$@> = LESS;

};

@q ** (2) relation --> LESS_OR_EQUAL.@>
@*1 \§relation> $\longrightarrow$ \.{LESS\_OR\_EQUAL}.

\LOG
\initials{LDF 2004.05.17.}  
Added this rule.
\ENDLOG 

@<Define rules@>=
@=relation: LESS_OR_EQUAL@>@/
{

  @=$$@> = LESS_OR_EQUAL;

};


@q ** (2) relation --> GREATER.@>
@*1 \§relation> $\longrightarrow$ \.{GREATER}.

\LOG
\initials{LDF 2004.05.17.}  
Added this rule.
\ENDLOG 

@<Define rules@>=
@=relation: GREATER@>@/
{

   @=$$@> = GREATER;

};

@q ** (2) relation --> GREATER_OR_EQUAL.@>
@*1 \§relation> $\longrightarrow$ \.{GREATER\_OR\_EQUAL}.

\LOG
\initials{LDF 2004.05.17.}  
Added this rule.
\ENDLOG 

@<Define rules@>=
@=relation: GREATER_OR_EQUAL@>@/
{

   @=$$@> = GREATER_OR_EQUAL;

};


@q ** (2) relation --> EQUAL.  @>
@*1 \§relation> $\longrightarrow$ \.{EQUAL}.

\LOG
\initials{LDF 2004.05.17.}  
Added this rule.
\ENDLOG 

@<Define rules@>=
@=relation: EQUAL@>@/
{

  @=$$@> = EQUAL;

};

@q ** (2) relation --> NOT_EQUAL.@>
@*1 \§relation> $\longrightarrow$ \.{NOT\_EQUAL}.

\LOG
\initials{LDF 2004.05.17.}  
Added this rule.
\ENDLOG 

@<Define rules@>=
@=relation: NOT_EQUAL@>@/
{

  @=$$@> = NOT_EQUAL;

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
@q eval:(outline-minor-mode t)        @>
@q abbrev-file-name:"~/.abbrev_defs"  @>
@q eval:(read-abbrev-file)            @>
@q fill-column:80                     @>
@q run-cweave-on-file:"3DLDFprg.web"  @>
@q End:                               @>


