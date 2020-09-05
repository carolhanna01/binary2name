@q pcubexpr.w @> 
@q Created by Laurence Finston Tue Jul  6 14:19:33 CEST 2004 @>
       
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



@q * (0) cuboid expressions.  @>
@** {\bf cuboid} expressions.

\LOG
\initials{LDF 2004.07.06.}
Created this file.
\ENDLOG 

@q * (1) cuboid primary.@>
@* \§cuboid primary>.
  
\LOG
\initials{LDF 2004.07.06.}  
Added this section.
\ENDLOG 

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> cuboid_primary@>@/

@q ** (2) cuboid_primary --> cuboid_variable.@>
@*1 \§cuboid primary> $\longrightarrow$ \§cuboid variable>.  
\initials{LDF 2004.07.06.}

\LOG
\initials{LDF 2004.07.06.}
Added this rule.

\initials{LDF 2004.11.23.}
No longer issuing an error message if |entry->object == 0|.
This condition occurs legitimately when one tries to show
an ``unknown |cuboid|''.
\ENDLOG 

\syntax{cuboid_primary: cuboid_variable}

@<Define rules@>=
@=cuboid_primary: cuboid_variable@>@/
{

  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

  if (entry == static_cast<Id_Map_Entry_Node>(0) || entry->object == static_cast<void*>(0))
    {

      @=$$@> = static_cast<void*>(0);

    } /* |if (entry == 0 || entry->object == 0)|  */

  else /* |entry != 0 && entry->object != 0|  */

    @=$$@> = static_cast<void*>(create_new<Cuboid>(
                                  static_cast<Cuboid*>(
                                     entry->object))); 
};

@q ** (2) cuboid_primary --> ( cuboid_expression ).@>
@*1 \§cuboid primary> $\longrightarrow$ \.{\LP} 
\§cuboid expression> \.{\RP}.
\initials{LDF 2004.07.06.}

\LOG
\initials{LDF 2004.07.06.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=cuboid_primary: LEFT_PARENTHESIS cuboid_expression RIGHT_PARENTHESIS@>@/
{

  @=$$@> = @=$2@>;

};

@q ** (2) cuboid_primary --> IN_CUBOID ellipsoid_primary.@>
@*1 \§cuboid primary> $\longrightarrow$ \.{IN\_CUBOID} 
\§ellipsoid primary>.
\initials{LDF 2005.12.07.}

\LOG
\initials{LDF 2005.12.07.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=cuboid_primary: IN_CUBOID ellipsoid_primary@>@/
{

   @=$$@> = in_cuboid_func<Ellipsoid>(static_cast<Ellipsoid*>(@=$2@>), 
                                      parameter);

};


@q ** (2) cuboid_primary --> OUT_CUBOID ellipsoid_primary.@>
@*1 \§cuboid primary> $\longrightarrow$ \.{OUT\_CUBOID} 
\§ellipsoid primary>.
\initials{LDF 2005.12.07.}

\LOG
\initials{LDF 2005.12.07.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=cuboid_primary: OUT_CUBOID ellipsoid_primary@>@/
{

   @=$$@> = out_cuboid_func<Ellipsoid>(static_cast<Ellipsoid*>(@=$2@>), 
                                       parameter);

};


@q ** (2) cuboid_primary --> LAST cuboid_vector_expression.@>

@*1 \§cuboid primary> $\longrightarrow$ 
\.{LAST} \§cuboid vector expression>.
\initials{LDF 2005.01.14.}

\LOG
\initials{LDF 2005.01.14.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>=
@=cuboid_primary: LAST cuboid_vector_expression@>@/
{ 

   Cuboid* c = create_new<Cuboid>(0);

   Pointer_Vector<Cuboid>* pv 
      = static_cast<Pointer_Vector<Cuboid>*>(@=$2@>);

@q ******* (7) Error handling:  |pv == 0|.@> 

@ Error handling:  |pv == 0|.
\initials{LDF 2005.01.14.}

@<Define rules@>=

   if (pv == static_cast<Pointer_Vector<Cuboid>*>(0))
      {
          delete c;

          @=$$@> = static_cast<void*>(0);

      }  /* |if (pv == 0)|  */

@q ******* (7) Error handling:  |pv->ctr == 0|.@> 

@ Error handling:  |pv->ctr == 0|.
\initials{LDF 2005.01.14.}

@<Define rules@>=

   else if (pv->ctr == 0)
      {

          delete c;

          @=$$@> = static_cast<void*>(0);

      }  /* |else if (pv->ctr == 0)|  */

@q ******* (7) |pv != 0 && pv->ctr > 0|.@> 

@ |pv != 0 && pv->ctr > 0|.  Set |@=$$@>| to |*(pv->v[pv->ctr - 1])|.
\initials{LDF 2005.01.14.}

@<Define rules@>=

   else 
      {
         *c = *(pv->v[pv->ctr - 1]);
         @=$$@> = static_cast<void*>(c); 
      }
@q ******* (7) @> 

};



@q * (1) cuboid secondary.  @>
@*1 \§cuboid secondary>.
\initials{LDF 2004.07.06.}  

\LOG
\initials{LDF 2004.07.06.}  
Added this section.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> cuboid_secondary@>
  
@q ** (2) cuboid secondary --> cuboid_primary.  @>
@*1 \§cuboid secondary> $\longrightarrow$ \§cuboid primary>.
\initials{LDF 2004.07.06.}

\LOG
\initials{LDF 2004.07.06.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=cuboid_secondary: cuboid_primary@>@/
{
  @=$$@> = @=$1@>;

};

@q ** (2) cuboid secondary --> cuboid_secondary transformer.@>
@*1 \§cuboid secondary> $\longrightarrow$ 
\§cuboid secondary> \§transformer>.
\initials{LDF 2004.07.06.}


\LOG
\initials{LDF 2004.07.06.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=cuboid_secondary: cuboid_secondary transformer@>@/
{

  Cuboid* p = static_cast<Cuboid*>(@=$1@>);
  Transform* t = static_cast<Transform*>(@=$2@>);

  *p *= *t;

  @=$$@> = static_cast<void*>(p); 

};

@q ** (2) cuboid secondary --> cuboid_secondary REFLECTED_IN @>
@q ** (2) path_expression.@> 

@*2 \§cuboid secondary> $\longrightarrow$ 
\§cuboid secondary> 
\.{REFLECTED\_IN} \§path expression>.
\initials{LDF 2004.10.05.}

\LOG
\initials{LDF 2004.10.05.}
Added this rule.

\initials{LDF 2004.10.09.}
Changed \§optional in> to \.{IN}.

\initials{LDF 2004.12.03.}
Changed |REFLECTED IN| to |REFLECTED_IN|.

\initials{LDF 2005.10.24.}
Changed |path_like_expression| to |path_expression|.
Removed debugging code.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=
@=cuboid_secondary: cuboid_secondary REFLECTED_IN @>
@=path_expression@>@/ 
{

   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

   Cuboid* p = reflect_in_func<Cuboid>(scanner_node,
                                       static_cast<Cuboid*>(@=$1@>),
                                       static_cast<Path*>(@=$3@>));

   @=$$@> = static_cast<void*>(p);

};

@q * (1) cuboid tertiary.  @>
@*2 \§cuboid tertiary>.
\initials{LDF 2004.07.06.}  

\LOG
\initials{LDF 2004.07.06.}  
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> cuboid_tertiary@>

@q ** (2) cuboid tertiary --> cuboid_secondary.  @>
@*1 \§cuboid tertiary> $\longrightarrow$ \§cuboid secondary>.
\initials{LDF 2004.07.06.}

\LOG
\initials{LDF 2004.07.06.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=cuboid_tertiary: cuboid_secondary@>@/
{

  @=$$@> = @=$1@>;

};



@q * (1) cuboid expression.  @>
@* \§cuboid expression>.
\initials{LDF 2004.07.06.}  

\LOG
\initials{LDF 2004.07.06.}  
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> cuboid_expression@>

@q ** (2) cuboid expression --> cuboid_tertiary.  @>
@*1 \§cuboid expression> $\longrightarrow$ \§cuboid tertiary>.

\LOG
\initials{LDF 2004.07.06.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=cuboid_expression: cuboid_tertiary@>@/
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

