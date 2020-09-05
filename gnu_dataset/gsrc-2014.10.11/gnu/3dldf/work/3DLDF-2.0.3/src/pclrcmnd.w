@q pclrcmnd.w @> 
@q Created by Laurence Finston Sun Aug 29 18:22:07 CEST 2004  @>
     
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



@q * (0) Clear Commands.@>
@** Clear Commands.

@q ** (2) command --> CLEAR vector_type_variable.@>

@*1 \§command> $\longrightarrow$ \.{CLEAR} 
\§vector type variable>.

\LOG
\initials{LDF 2004.08.27.}
Added this rule.  It must be cleaned up and put into a function in 
|namespace Scan_Parse|.

\initials{LDF 2005.01.03.}
Removed code from this rule.  I'm about to start putting it in
|Scan_Parse::clear_vector_func()| in \filename{scanprsf.web}.
\ENDLOG 

@q *** (3) Definition.@> 

@<Define rules@>=
@=command: CLEAR vector_type_variable@>
{


   Scan_Parse::clear_vector_func(static_cast<Scanner_Node>(parameter), 
                                 static_cast<Id_Map_Entry_Node>(@=$2@>)); 

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) command --> CLEAR path_variable.@>

@*1 \§command> $\longrightarrow$ \.{CLEAR} \§path variable>.

\LOG
\initials{LDF 2004.11.01.}
Added this rule.

\initials{LDF 2005.10.24.}
Changed |path_like_variable| to |path_variable|.
Removed debugging code.
\ENDLOG 

@q *** (3) Definition.@> 

@<Define rules@>=
@=command: CLEAR_CONNECTORS path_variable@>
{

  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$2@>); 

@q **** (4) Error handling:  |entry == 0|.@>   
@ Error handling:  |entry == 0|.
\initials{LDF 2004.11.01.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0) || entry->object == static_cast<void*>(0))
   {
       @=$$@> = static_cast<void*>(0);

   } /* |if (entry == 0 || entry->object == 0)|  */


@q **** (4) |entry != 0 && entry->object != 0|.@>   

@ |entry != 0 && entry->object != 0|.
\initials{LDF 2004.11.01.}

@<Define rules@>=

   Path* p = static_cast<Path*>(entry->object);
  
   p->clear_connectors(); 

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) command --> CLEAR pen_variable.@>

@*1 \§command> $\longrightarrow$ \.{CLEAR} \§pen variable>.

\LOG
\initials{LDF 2004.11.12.}
Added this rule.
\ENDLOG 

@q *** (3) Definition.@> 

@<Define rules@>=
@=command: CLEAR pen_variable@>
{

  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$2@>); 

@q **** (4) Error handling:  |entry == 0|.@>   
@ Error handling:  |entry == 0|.
\initials{LDF 2004.11.12.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0) || entry->object == static_cast<void*>(0))
   {
       @=$$@> = static_cast<void*>(0);

   } /* |if (entry == 0 || entry->object == 0)|  */


@q **** (4) |entry != 0 && entry->object != 0|.@>   

@ |entry != 0 && entry->object != 0|.
\initials{LDF 2004.11.12.}

@<Define rules@>=
  
   delete static_cast<Pen*>(entry->object);

   entry->object = 0;

   @=$$@> = static_cast<void*>(0);

};

@q ** (2) command --> CLEAR picture_variable.@>

@*1 \§command> $\longrightarrow$ \.{CLEAR} \§picture variable>.

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG 

@q *** (3) Definition.@> 

@<Define rules@>=
@=command: CLEAR picture_variable@>
{
  Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$2@>); 

@q **** (4) Error handling:  |entry == 0|.@>   
@ Error handling:  |entry == 0|.
\initials{LDF 2004.12.14.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0)) 
   {
       @=$$@> = static_cast<void*>(0);

   } /* |if (entry == 0 || entry->object == 0)|  */


@q **** (4) |entry != 0|.@>   

@ |entry != 0|.
\initials{LDF 2004.12.14.}

If |entry->object == 0|, just don't anything.  
Clearing an empty `picture' is allowed.  There's no need to 
signal an error or issue a warning.
\initials{LDF 2004.12.14.}

@<Define rules@>=
  
   if (entry->object)
      static_cast<Picture*>(entry->object)->clear(); 

   @=$$@> = static_cast<void*>(0);

};


@q ** (2) command --> CLEAR transform_variable.@>
@*1 \§command> $\longrightarrow$ \.{CLEAR} \§transform variable>.

\LOG
\initials{LDF 2007.08.21.}
Added this rule.
\ENDLOG 

@q *** (3) Definition.@> 

@<Define rules@>=
@=command: CLEAR transform_variable@>
{
    Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$2@>); 
    delete static_cast<Transform*>(entry->object);
    entry->object = 0;
    @=$$@> = static_cast<void*>(0);
};


@q * (0) @>



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
@q fill-column:80 @>
@q run-cweave-on-file:"3DLDFprg.web"  @>
@q End: @>
