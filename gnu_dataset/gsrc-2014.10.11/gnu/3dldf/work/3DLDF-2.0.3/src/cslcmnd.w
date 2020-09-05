@q cslcmnd.w @> 
@q Created by Laurence Finston Fri Aug  3 14:34:23 CEST 2007 @>
       
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


@q * (0) Commands for conic_section_lattice.  @>
@** Commands for \§conic section lattice>.
\initials{LDF 2007.08.03.}

\LOG
\initials{LDF 2007.08.03.}
Created this file.
\ENDLOG 

@q * (1) command --> SHIFT_LATTICE   @> 
@q * (1) conic_section_lattice_variable.@> 
@* \§command> $\longrightarrow$ \.{SHIFT\_LATTICE}
\§conic section lattice variable>.
\initials{LDF 2007.08.07.}

\LOG
\initials{LDF 2007.08.07.}
Added this rule.
\ENDLOG

@q ** (2) Definition.@> 

@<Define rules@>=
@=command:  SHIFT_LATTICE conic_section_lattice_variable@>@/
{

    Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$2@>);
   
    if (entry && entry->object)
    {
        static_cast<Conic_Section_Lattice*>(entry->object)->
           shift_lattice(1, static_cast<Scanner_Node>(parameter)); 
    } 

    @=$$@> = static_cast<void*>(0);

};

@q * (1) command --> SHIFT_LATTICE conic_section_lattice_variable @> 
@q * (1) COMMA numeric_expression                                 @> 
@* \§command> $\longrightarrow$ \.{SHIFT\_LATTICE} 
\§conic section lattice variable> \.{COMMA} \§numeric expression>.
\initials{LDF 2007.08.07.}

\LOG
\initials{LDF 2007.08.07.}
Added this rule.
\ENDLOG

@q ** (2) Definition.@> 

@<Define rules@>=
@=command:  SHIFT_LATTICE conic_section_lattice_variable@>@/
@=COMMA numeric_expression@>@/
{
    
    Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$2@>);
   
    if (entry && entry->object)
    {
        real r = @=$4@>;

        signed short sign;

        if (r < 0 )
        {
           sign = -1;
           r *= -1;
        }
        else 
           sign = 1;

        r = floor(r + .5);

        int i = static_cast<int>(r) % 5;

        i *= sign;

        static_cast<Conic_Section_Lattice*>(entry->object)->
           shift_lattice(i, static_cast<Scanner_Node>(parameter)); 
    }
 
    @=$$@> = static_cast<void*>(0);


};

@q * (1) command --> SET_TRANSFORM conic_section_lattice_variable @> 
@q * (1) COMMA transform_expression                               @> 
@* \§command> $\longrightarrow$ \.{SET\_TRANSFORM} 
\§conic section lattice variable> \.{COMMA} \§transform expression>.
\initials{LDF 2007.08.21.}

\LOG
\initials{LDF 2007.08.21.}
Added this rule.
\ENDLOG

@q ** (2) Definition.@> 

@<Define rules@>=
@=command:  SET_TRANSFORM conic_section_lattice_variable@>@/
@=COMMA transform_expression@>@/
{
    
    Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$2@>);

    Transform* t = static_cast<Transform*>(@=$4@>); 
   
    if (entry && entry->object)
    {
        Conic_Section_Lattice* c 
           = static_cast<Conic_Section_Lattice*>(entry->object);
        c->set_transform(*t);
    }

    delete t;    

    @=$$@> = static_cast<void*>(0);

};

@q * (1) command --> RESET_TRANSFORM conic_section_lattice_variable.@> 

@* \§command> $\longrightarrow$ 
\.{RESET\_TRANSFORM} \§conic section lattice variable>.
\initials{LDF 2007.08.21.}

\LOG
\initials{LDF 2007.08.21.}
Added this rule.
\ENDLOG

@q ** (2) Definition.@> 

@<Define rules@>=
@=command:  RESET_TRANSFORM conic_section_lattice_variable@>@/
{
    
    Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$2@>);

    if (entry && entry->object)
    {
        Conic_Section_Lattice* c 
           = static_cast<Conic_Section_Lattice*>(entry->object);
        c->reset_transform();
    }

    @=$$@> = static_cast<void*>(0);

};

@q * (1) command --> DELETE_TRANSFORM conic_section_lattice_variable.@> 

@* \§command> $\longrightarrow$ 
\.{DELETE\_TRANSFORM} \§conic section lattice variable>.
\initials{LDF 2007.08.21.}

\LOG
\initials{LDF 2007.08.21.}
Added this rule.
\ENDLOG

@q ** (2) Definition.@> 

@<Define rules@>=
@=command:  DELETE_TRANSFORM conic_section_lattice_variable@>@/
{
    
    Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$2@>);

    if (entry && entry->object)
    {
        Conic_Section_Lattice* c 
           = static_cast<Conic_Section_Lattice*>(entry->object);
        c->delete_transform();
    }

    @=$$@> = static_cast<void*>(0);

};


@q * (1)@>


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

