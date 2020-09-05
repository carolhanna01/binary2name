@q pullvxpr.w @> 
@q Created by Laurence Finston Mon Dec  5 21:30:13 CET 2005 @>

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


 
@q * (0) Unsigned long long vector expressions.  @>
@** Unsigned long long vector  expressions.
\initials{LDF 2005.12.05.}

\LOG
\initials{LDF 2005.12.05.}
Created this file.
\ENDLOG 

@q * (1) ulong_long_vector_primary.@>
@* \§ulong long vector primary>.
\initials{LDF 2005.12.05.}

\LOG
\initials{LDF 2005.12.05.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> ulong_long_vector_primary@>@/

@q ** (2) ulong_long_vector_primary --> ulong_long_vector_variable.  @>
@*1 \§ulong long vector primary> $\longrightarrow$ \§ulong long vector variable>.
\initials{LDF 2005.12.05.}

\LOG
\initials{LDF 2005.12.05.}
Added this rule.

\initials{LDF 2005.12.07.}
Added real code to this rule.
\ENDLOG

@<Define rules@>= 
@=ulong_long_vector_primary: ulong_long_vector_variable@>
{
   Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);
   
   if (entry == static_cast<Id_Map_Entry_Node>(0) || entry->object == static_cast<void*>(0))
     {

       @=$$@> = static_cast<void*>(0);

     }  /* |if (entry == 0 || entry->object == 0)| */


   else /* |entry != 0| */
      { 
          typedef Pointer_Vector<ulong_long> PV;

          PV* pv = new PV;

          *pv = *static_cast<PV*>(entry->object);

          @=$$@> = static_cast<void*>(pv);                    

      }  /* |else| (|entry != 0|) */

};

@q ** (2) ulong_long_vector_primary --> get_prime_vector_sub @>
@q ** (2) ulong_long_primary ulong_long_primary.             @>
@*1 \§ulong long vector primary> $\longrightarrow$ 
\§get prime vector sub> \§ulong long primary> \§ulong long primary>.
\initials{LDF 2005.12.06.}

\LOG
\initials{LDF 2005.12.06.}
Added this rule.

\initials{LDF 2005.12.07.}
Now calling |Prime_Numbers::get_prime_vector()|.
\ENDLOG

@<Define rules@>= 
@=ulong_long_vector_primary: get_prime_vector_sub ulong_long_primary ulong_long_primary@>
{
      static_cast<Scanner_Node>(parameter)->ulong_long_switch = false;
      @=$$@> = Prime_Numbers::get_prime_vector(@=$2@>, 
                                               @=$3@>, 
                                               static_cast<Scanner_Node>(parameter));
};

@q ** (2) get_prime_vector_sub.@> 
@*1 \§get prime vector sub>.
\initials{LDF 2005.12.06.}

\LOG
\initials{LDF 2005.12.06.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <int_value> get_prime_vector_sub@>@/

@q *** (3) get_prime_vector_sub --> GET_PRIME_VECTOR@> 
@*2 \§get prime vector sub> --> \.{GET\_PRIME\_VECTOR}.
\initials{LDF 2005.12.06.}

\LOG
\initials{LDF 2005.12.06.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=get_prime_vector_sub: GET_PRIME_VECTOR@>@/
{
   static_cast<Scanner_Node>(parameter)->ulong_long_switch = true;
   @=$$@> = @=$1@>;
}

@q * (1) ulong_long_vector secondary.  @>
@* Unsigned long long vector secondary.
\initials{LDF 2005.12.05.}

\LOG
\initials{LDF 2005.12.05.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> ulong_long_vector_secondary@>@/

@q ** (2) ulong_long_vector_secondary --> ulong_long_vector_primary.@>

@*1 \§ulong long vector secondary> $\longrightarrow$ \§ulong long vector primary>.  
\initials{LDF 2005.12.05.}

\LOG
\initials{LDF 2005.12.05.}
Added this rule.
\ENDLOG

@<Define rules@>= 
@=ulong_long_vector_secondary: ulong_long_vector_primary@>
{

  @=$$@> = @=$1@>;
  
};

@q * (1) ulong_long_vector tertiary.@>
@* \§ulong long vector tertiary>.
\initials{LDF 2005.12.05.}

\LOG
\initials{LDF 2005.12.05.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> ulong_long_vector_tertiary@>@/

@q ** (2) ulong_long_vector_tertiary --> ulong_long_vector_secondary.@>
@*1 \§ulong long vector tertiary> $\longrightarrow$ \§ulong long vector secondary>.
@<Define rules@>= 
@=ulong_long_vector_tertiary: ulong_long_vector_secondary@>
{

  @=$$@> = @=$1@>;
  

};

@q * (1) ulong_long_vector_expression.@>
@* \§ulong long vector expression>.
\initials{LDF 2005.12.05.}

\LOG
\initials{LDF 2005.12.05.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> ulong_long_vector_expression@>@/

@q ** (2) ulong_long_vector_expression --> ulong_long_vector_tertiary.@> 
@*1 \§ulong long vector expression> $\longrightarrow$ \§ulong long vector tertiary>.
\initials{LDF 2005.12.05.} 

\LOG
\initials{LDF 2005.12.05.}
Added this rule.
\ENDLOG

@
@<Define rules@>= 
@=ulong_long_vector_expression: ulong_long_vector_tertiary@>
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
