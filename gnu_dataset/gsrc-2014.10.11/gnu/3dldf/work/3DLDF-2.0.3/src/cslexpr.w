@q cslexpr.w @> 
@q Created by Laurence Finston Sun Jul 29 16:12:27 CEST 2007 @>
       
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



@q * (0) conic_section_lattice expressions.  @>
@** \§conic section lattice expressions>.
\initials{LDF 2007.07.29.}

\LOG
\initials{LDF 2007.07.29.}
Created this file.
\ENDLOG 

@q * (1) conic_section_lattice primary.  @>
@* \§conic section lattice primary>.
\initials{LDF 2007.07.29.}
  
\LOG
\initials{LDF 2007.07.29.}
Added this type declaration.
\ENDLOG 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> conic_section_lattice_primary@>@/

@q ** (2) conic_section_lattice_primary --> conic_section_lattice_variable.@>
@*1 \§conic section lattice primary> $\longrightarrow$ 
\§conic section lattice variable>.  
\initials{LDF 2007.07.29.}

\syntax{conic_section_lattice_primary: conic_section_lattice_variable}

\LOG
\initials{LDF 2007.07.29.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=conic_section_lattice_primary: conic_section_lattice_variable@>@/
{

  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

  if (entry == static_cast<Id_Map_Entry_Node>(0) || entry->object == static_cast<void*>(0))
    {

      @=$$@> = static_cast<void*>(0);

    } /* |if (   entry == static_cast<Id_Map_Entry_Node>(0) 
              || entry->object == static_cast<void*>(0))|  */

  else /* |   entry != static_cast<Id_Map_Entry_Node>(0) 
           && entry->object != static_cast<void*>(0)|  */

    @=$$@> = static_cast<void*>(create_new<Conic_Section_Lattice>(
                                  static_cast<Conic_Section_Lattice*>(
                                     entry->object))); 
};

@q ** (2) conic_section_lattice_primary --> LEFT_PARENTHESIS @>
@q ** (2) conic_section_lattice_expression RIGHT_PARENTHESIS.@> 

@*1 \§conic section lattice primary> $\longrightarrow$ 
\.{LEFT\_PARENTHESIS} \§conic section lattice expression> 
\.{RIGHT\_PARENTHESIS}.
\initials{LDF 2007.07.29.}

\LOG
\initials{LDF 2007.07.29.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=conic_section_lattice_primary: LEFT_PARENTHESIS conic_section_lattice_expression @>@/
@=RIGHT_PARENTHESIS@>@/
{

   @=$$@> = @=$2@>;

};

@q ** (2) conic_section_lattice_primary --> GET_CONIC_SECTION_LATTICE @>
@q ** (2) point_vector_expression get_conic_section_lattice_option_list.@> 

@*1 \§conic section lattice primary>  $\longrightarrow$ 
\.{GET\_CONIC\_SECTION\_LATTICE} \§point vector expression>
\§get conic section lattice option list>.
\initials{LDF 2007.07.29.}

The semantic value of \§get conic section lattice option list> is 0.
The rules that reduce to this symbol set values on 
|scanner_node->conic_section_lattice_options|.  |scanner_node| is passed 
to |Scan_Parse::get_conic_section_lattice_func|, which can then access
any values that have been set.
\initials{LDF 2007.08.05.}

\LOG
\initials{LDF 2007.07.29.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=conic_section_lattice_primary: GET_CONIC_SECTION_LATTICE @>@/
@=point_vector_expression get_conic_section_lattice_option_list@>@/
{
    @=$$@> 
      = Scan_Parse::get_conic_section_lattice_func(
           static_cast<Pointer_Vector<Point>*>(@=$2@>),
           static_cast<Scanner_Node>(parameter));

};


@q *** (3) get_conic_section_lattice_option_list.@>   
@*2 \§get conic section lattice option list>.
\initials{LDF 2007.07.29.}

\LOG
\initials{LDF 2007.07.29.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> get_conic_section_lattice_option_list@>

@q **** (4) get_conic_section_lattice_option_list --> EMPTY.@>   
@*3 \§get conic section lattice option list> $\longrightarrow$ \.{EMPTY}.
\initials{LDF 2007.07.29.}

\LOG
\initials{LDF 2007.07.29.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=get_conic_section_lattice_option_list: /* Empty  */@>@/        
{

   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

   if (   scanner_node->conic_section_lattice_options 
       == static_cast<Conic_Section_Lattice_Options*>(0))
   {
      scanner_node->conic_section_lattice_options = new Conic_Section_Lattice_Options;
   }

   @=$$@> = static_cast<void*>(0);

};

@q **** (4) get_conic_section_lattice_option_list --> @>
@q **** (4) get_conic_section_lattice_option_list     @>   
@q **** (4) get_conic_section_lattice_option.         @>   

@*3 \§get conic section lattice option list> $\longrightarrow$ 
\§get conic section lattice option list> \§get conic section lattice option>. 
\initials{LDF 2007.07.29.}

\LOG
\initials{LDF 2007.07.29.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=get_conic_section_lattice_option_list: @>@/
@=get_conic_section_lattice_option_list @>@/
@=get_conic_section_lattice_option@>@/        
{

   @=$$@> = static_cast<void*>(0);

};

@q *** (3) get_conic_section_lattice_option.@>   
@*2 \§get conic section lattice option>.
\initials{LDF 2007.07.29.}

\LOG
\initials{LDF 2007.07.29.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=

@=%type <pointer_value> get_conic_section_lattice_option@>


@q **** (4) get_conic_section_lattice_option --> WITH_CULL.@>   
@*3 \§get conic section lattice option> $\longrightarrow$ \.{WITH\_CULL}.
\initials{LDF 2007.08.05.}

\LOG
\initials{LDF 2007.08.05.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=get_conic_section_lattice_option: WITH_CULL@>@/        
{

   Scanner_Node scanner_node = static_cast<Scanner_Node>(scanner_node); 
   
   if (scanner_node && scanner_node->conic_section_lattice_options)
       scanner_node->conic_section_lattice_options->do_cull 
          = true;

   @=$$@> = static_cast<void*>(0);

};

@q **** (4) get_conic_section_lattice_option --> WITH_NO_CULL.@>   
@*3 \§get conic section lattice option> $\longrightarrow$ \.{WITH\_NO\_CULL}.
\initials{LDF 2007.08.05.}

\LOG
\initials{LDF 2007.08.05.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=get_conic_section_lattice_option: WITH_NO_CULL@>@/        
{

   Scanner_Node scanner_node = static_cast<Scanner_Node>(scanner_node); 
   
   if (scanner_node && scanner_node->conic_section_lattice_options)
      scanner_node->conic_section_lattice_options->do_cull 
         = false;

   @=$$@> = static_cast<void*>(0);

};

@q **** (4) get_conic_section_lattice_option --> WITH_RECTIFY.@>   
@*3 \§get conic section lattice option> $\longrightarrow$ \.{WITH\_RECTIFY}.
\initials{LDF 2007.08.05.}

\LOG
\initials{LDF 2007.08.05.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=get_conic_section_lattice_option: WITH_RECTIFY@>@/        
{

   Scanner_Node scanner_node = static_cast<Scanner_Node>(scanner_node); 
   
   if (scanner_node && scanner_node->conic_section_lattice_options)
       scanner_node->conic_section_lattice_options->do_rectify 
          = true;

   @=$$@> = static_cast<void*>(0);

};

@q **** (4) get_conic_section_lattice_option --> WITH_NO_RECTIFY.@>   
@*3 \§get conic section lattice option> $\longrightarrow$ \.{WITH\_NO\_RECTIFY}.
\initials{LDF 2007.08.05.}

\LOG
\initials{LDF 2007.08.05.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=get_conic_section_lattice_option: WITH_NO_RECTIFY@>@/        
{

   Scanner_Node scanner_node = static_cast<Scanner_Node>(scanner_node); 
   
   if (scanner_node && scanner_node->conic_section_lattice_options)
      scanner_node->conic_section_lattice_options->do_rectify 
         = false;

   @=$$@> = static_cast<void*>(0);

};

@q **** (4) get_conic_section_lattice_option --> WITH_TOLERANCE.@>   
@*3 \§get conic section lattice option> $\longrightarrow$ \.{WITH\_TOLERANCE}.
\§numeric expression>.
\initials{LDF 2007.08.05.}

\LOG
\initials{LDF 2007.08.05.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=get_conic_section_lattice_option: WITH_TOLERANCE numeric_expression@>@/        
{

   Scanner_Node scanner_node = static_cast<Scanner_Node>(scanner_node); 
   
   if (scanner_node && scanner_node->conic_section_lattice_options)
       scanner_node->conic_section_lattice_options->tolerance 
          = @=$2@>;

   @=$$@> = static_cast<void*>(0);

};




@q **** (4) get_conic_section_lattice_option --> WITH_TEST.@>   
@*3 \§get conic section lattice option> $\longrightarrow$ \.{WITH\_TEST}.
\initials{LDF 2007.07.29.}

\LOG
\initials{LDF 2007.07.29.}
Added this rule.

\initials{LDF 2007.08.05.}
Removed \§boolean expresssion> from the right-hand side.
\ENDLOG

@<Define rules@>=
@=get_conic_section_lattice_option: WITH_TEST@>@/        
{

   Scanner_Node scanner_node = static_cast<Scanner_Node>(scanner_node); 
   
   if (scanner_node && scanner_node->conic_section_lattice_options)
       scanner_node->conic_section_lattice_options->do_test 
          = true;

   @=$$@> = static_cast<void*>(0);

};

@q **** (4) get_conic_section_lattice_option --> WITH_NO_TEST.@>   
@*3 \§get conic section lattice option> $\longrightarrow$ \.{WITH\_NO\_TEST}.
\initials{LDF 2007.08.05.}

\LOG
\initials{LDF 2007.08.05.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=get_conic_section_lattice_option: WITH_NO_TEST@>@/        
{

   Scanner_Node scanner_node = static_cast<Scanner_Node>(scanner_node); 
   
   if (scanner_node && scanner_node->conic_section_lattice_options)
      scanner_node->conic_section_lattice_options->do_test 
         = false;

   @=$$@> = static_cast<void*>(0);

};

@q **** (4) get_conic_section_lattice_option --> WITH_TRANSFORM @>   
@q **** (4) transform_expression                                @>
@*3 \§get conic section lattice option> $\longrightarrow$ 
\.{WITH\_TRANSFORM} \§transform expression>.
\initials{LDF 2007.08.06.}

\LOG
\initials{LDF 2007.08.06.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=get_conic_section_lattice_option: WITH_TRANSFORM@>@/
@=transform_expression@>@/
{

   Scanner_Node scanner_node = static_cast<Scanner_Node>(scanner_node); 
   
   if (scanner_node && scanner_node->conic_section_lattice_options)
      scanner_node->conic_section_lattice_options->transform
         = static_cast<Transform*>(@=$2@>);

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) conic_section_lattice_primary --> LAST @>
@q ** (2) conic_section_lattice_vector_expression.@>

@*1 \§conic section lattice primary> $\longrightarrow$ 
\.{LAST} \§conic section lattice vector expression>.
\initials{LDF 2007.07.29.}

\LOG
\initials{LDF 2007.07.29.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=conic_section_lattice_primary: LAST conic_section_lattice_vector_expression@>@/
{ 

   Conic_Section_Lattice* r;


         r = create_new<Conic_Section_Lattice>(0);

   Pointer_Vector<Conic_Section_Lattice>* pv 
      = static_cast<Pointer_Vector<Conic_Section_Lattice>*>(@=$2@>);

@q ******* (7) Error handling:  |pv == static_cast<Pointer_Vector<Conic_Section_Lattice>*>(0)|.@>

@ Error handling:  |pv == 0|.
\initials{LDF 2007.07.29.}

@<Define rules@>=

   if (pv == static_cast<Pointer_Vector<Conic_Section_Lattice>*>(0))
      {

          delete r;

          @=$$@> = static_cast<void*>(0);

      }  /* |if (pv == static_cast<Pointer_Vector<Conic_Section_Lattice>*>(0))|  */

@q ******* (7) Error handling:  |pv->ctr == 0|.@> 

@ Error handling:  |pv->ctr == 0|.
\initials{LDF 2007.07.29.}

@<Define rules@>=

   else if (pv->ctr == 0)
      {

          delete r;

          @=$$@> = static_cast<void*>(0);

      }  /* |else if (pv->ctr == 0)|  */

@q ******* (7) |pv != static_cast<Pointer_Vector<Conic_Section_Lattice>*>(0) && pv->ctr > 0|.@>

@ |pv != static_cast<Pointer_Vector<Conic_Section_Lattice>*>(0) && pv->ctr > 0|.  
Set |@=$$@>| to |*(pv->v[pv->ctr - 1])|.
\initials{LDF 2007.07.29.}

@<Define rules@>=

   else 
      {
         *r = *(pv->v[pv->ctr - 1]);
         @=$$@> = static_cast<void*>(r); 
      }
@q ******* (7) @> 

};

@q * (1) conic_section_lattice secondary.@>
@* \§conic section lattice secondary>.
\initials{LDF 2007.07.29.}

\LOG
\initials{LDF 2007.07.29.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> conic_section_lattice_secondary@>
  
@q ** (2) conic_section_lattice secondary --> conic_section_lattice_primary.@>
@*1 \§conic section lattice secondary> $\longrightarrow$ \§conic section lattice primary>.
\initials{LDF 2007.07.29.}

\LOG
\initials{LDF 2007.07.29.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=conic_section_lattice_secondary: conic_section_lattice_primary@>@/
{

  @=$$@> = @=$1@>;

};

@q ** (2) conic_section_lattice secondary -->           @>
@q        conic_section_lattice_secondary transformer.  @>
@*1 \§conic section lattice secondary> $\longrightarrow$ 
\§conic section lattice secondary> \§transformer>.

\LOG
\initials{LDF 2007.08.21.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=conic_section_lattice_secondary: conic_section_lattice_secondary transformer@>@/
{

  Conic_Section_Lattice* c = static_cast<Conic_Section_Lattice*>(@=$1@>);
  Transform* t = static_cast<Transform*>(@=$2@>);

  if (c && t)
  {
     *c *= *t;
     delete t;
     @=$$@> = static_cast<void*>(c); 
  }
  else if (c)
  {
      @=$$@> = static_cast<void*>(c); 
  }
  else if (t)
  {
      delete t;
      @=$$@> = static_cast<void*>(0);
  }
};

@q * (1) conic_section_lattice_tertiary.@>
@* \§conic section lattice tertiary>.
\initials{LDF 2007.07.29.}

\LOG
\initials{LDF 2007.07.29.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> conic_section_lattice_tertiary@>

@q ** (2) conic_section_lattice tertiary --> conic_section_lattice_secondary.  @>
@*1 \§conic section lattice tertiary> $\longrightarrow$ \§conic section lattice secondary>.
\initials{LDF 2007.07.29.}

\LOG
\initials{LDF 2007.07.29.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=conic_section_lattice_tertiary: conic_section_lattice_secondary@>@/
{
   @=$$@> = @=$1@>;

};



@q * (1) conic_section_lattice expression.  @>
@* \§conic section lattice expression>.
\initials{LDF 2007.07.29.}

\LOG
\initials{LDF 2007.07.29.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> conic_section_lattice_expression@>

@q ** (2) conic_section_lattice expression --> conic_section_lattice_tertiary.  @>
@*1 \§conic section lattice expression> $\longrightarrow$ \§conic section lattice tertiary>.
\initials{LDF 2007.07.29.}

\LOG
\initials{LDF 2007.07.29.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=conic_section_lattice_expression: conic_section_lattice_tertiary@>@/
{

  @=$$@> = @=$1@>;

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

