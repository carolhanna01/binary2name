@q pullexpr.w @> 
@q Created by Laurence Finston Mon Dec  5 20:18:41 CET 2005 @>

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


 
@q * (0) Unsigned long long expressions.  @>
@** Unsigned long long  expressions.
\initials{LDF 2005.12.05.}

\LOG
\initials{LDF 2005.12.05.}
Created this file.
\ENDLOG 

@q * (1) ulong_long_primary.@>
@* \§ulong long primary>.
\initials{LDF 2005.12.05.}

\LOG
\initials{LDF 2005.12.05.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <ulong_long_value> ulong_long_primary@>@/

@q ** (2) ulong_long_primary --> ulong_long_variable.  @>
@*1 \§ulong long primary> $\longrightarrow$ \§ulong long variable>.
\initials{LDF 2005.12.05.}

\LOG
\initials{LDF 2005.12.05.}
Added this rule.
\ENDLOG

@<Define rules@>= 
@=ulong_long_primary: ulong_long_variable@>
{

    Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$1@>);

    if (entry == static_cast<Id_Map_Entry_Node>(0) || entry->object == static_cast<void*>(0))
      {

        @=$$@> = 0ULL;

      }  /* |if (entry == 0 || entry->object == 0)| */


    else /* |entry != 0| */
 
        @=$$@> = *static_cast<unsigned long long*>(entry->object);
 
};

@q ** (2) ulong_long_primary --> ULONG_LONG_TYPE.  @>
@*1 \§ulong long primary> $\longrightarrow$ \.{ULONG\_LONG\_TYPE}.
\initials{LDF 2005.12.06.}

\LOG
\initials{LDF 2005.12.06.}
Added this rule.
\ENDLOG

@<Define rules@>= 
@=ulong_long_primary: ULONG_LONG_TYPE@>
{
        @=$$@> = @=$1@>;
};



@q ** (2) ulong_long_primary --> get_prime_sub ulong_long_primary.  @>
@*1 \§ulong long primary> $\longrightarrow$ \§get prime sub> 
\§ulong long primary>.
\initials{LDF 2005.12.05.}

\LOG
\initials{LDF 2005.12.05.}
Added this rule.

\initials{LDF 2005.12.06.}
Changed \.{GET\_PRIME} to \§get prime sub>.  Now setting 
|static_cast<Scanner_Node>(parameter)->ulong_long_switch| to |false|.
\ENDLOG

@<Define rules@>= 
@=ulong_long_primary: get_prime_sub ulong_long_primary@>
{
    static_cast<Scanner_Node>(parameter)->ulong_long_switch = false;
    @=$$@> = Prime_Numbers::get_prime(@=$2@>, static_cast<Scanner_Node>(parameter));
 
};

@q ** (2) get_prime_sub.@> 
@*1 \§get prime sub>.
\initials{LDF 2005.12.06.}

\LOG
\initials{LDF 2005.12.06.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <int_value> get_prime_sub@>@/

@q *** (3) get_prime_sub --> GET_PRIME@> 
@*2 \§get prime sub> --> \.{GET\_PRIME}.
\initials{LDF 2005.12.06.}

\LOG
\initials{LDF 2005.12.06.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=get_prime_sub: GET_PRIME@>@/
{
   static_cast<Scanner_Node>(parameter)->ulong_long_switch = true;
   @=$$@> = @=$1@>;
}

@q * (1) ulong_long secondary.  @>
@* Unsigned long long secondary.
\initials{LDF 2005.12.05.}

\LOG
\initials{LDF 2005.12.05.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <ulong_long_value> ulong_long_secondary@>@/

@q ** (2) ulong_long_secondary --> ulong_long_primary.@>

@*1 \§ulong long secondary> $\longrightarrow$ \§ulong long primary>.  
\initials{LDF 2005.12.05.}

\LOG
\initials{LDF 2005.12.05.}
Added this rule.
\ENDLOG

@<Define rules@>= 
@=ulong_long_secondary: ulong_long_primary@>
{

  @=$$@> = @=$1@>;
  
};

@q * (1) ulong_long tertiary.@>
@* \§ulong long tertiary>.
\initials{LDF 2005.12.05.}

\LOG
\initials{LDF 2005.12.05.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <ulong_long_value> ulong_long_tertiary@>@/

@q ** (2) ulong_long_tertiary --> ulong_long_secondary.@>
@*1 \§ulong long tertiary> $\longrightarrow$ \§ulong long secondary>.
@<Define rules@>= 
@=ulong_long_tertiary: ulong_long_secondary@>
{

  @=$$@> = @=$1@>;
  

};

@q * (1) ulong_long_expression.@>
@* \§ulong long expression>.
\initials{LDF 2005.12.05.}

\LOG
\initials{LDF 2005.12.05.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <ulong_long_value> ulong_long_expression@>@/

@q ** (2) ulong_long_expression --> ulong_long_tertiary.@> 
@*1 \§ulong long expression> $\longrightarrow$ \§ulong long tertiary>.
\initials{LDF 2005.12.05.} 

\LOG
\initials{LDF 2005.12.05.}
Added this rule.
\ENDLOG

@
@<Define rules@>= 
@=ulong_long_expression: ulong_long_tertiary@>
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
