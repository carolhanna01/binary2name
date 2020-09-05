@q fcscmnd.w @> 
@q Created by Laurence Finston Sun Sep  2 18:50:12 CEST 2007 @>
       
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



@q * (0) Commands for Focuses.  @>
@** Commands for {\bf Focuses}>.
\initials{LDF 2007.09.02.}

\LOG
\initials{LDF 2007.09.02.}
Created this file.
\ENDLOG 

@q * (1) command --> RESET_ANGLE focus_variable numeric_expression.  @> 
@* \§command> $\longrightarrow$ \.{RESET\_ANGLE} 
\§focus variable> \§numeric expression>.
\initials{LDF 2007.09.02.}

\LOG
\initials{LDF 2007.09.02.}
Added this rule.
\ENDLOG

@q ** (2) Definition.@> 

@<Define rules@>=
@=command:  RESET_ANGLE focus_variable numeric_expression@>@/
{

    Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$2@>);
   
    if (entry && entry->object)
    {
        static_cast<Focus*>(entry->object)->reset_angle(@=$3@>);
    } 

    @=$$@> = static_cast<void*>(0);

};

@q * (1) Transformations.@>
@* Transformations.
\initials{LDF 2007.09.19.}

\LOG
\initials{LDF 2007.09.19.}
Added this section.
\ENDLOG

@q ** (2) focus_option_list.@>
@*1 \§focus option list>.
\initials{LDF 2007.09.19.}

\LOG
\initials{LDF 2007.09.19.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <int_value> focus_option_list@>

@q *** (3) focus_option_list: /* Empty  */@>
@*2 \§focus option list> $\longrightarrow$ \.{EMPTY}.
\initials{LDF 2007.09.19.}

\LOG
\initials{LDF 2007.09.19.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=focus_option_list: /*  Empty  */@>@/
{
   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

   if (scanner_node)
   {
      if (scanner_node->focus_options)
      {
          scanner_node->focus_options->clear();   
      }
      else
         scanner_node->focus_options = new Focus_Options;
   }

   @=$$@> = 0; 
};


@q *** (3) focus_option_list: focus_option_list @>
@q *** (3) focus_option.                        @>
@*2 \§focus option list> $\longrightarrow$ 
\§focus option>.
\initials{LDF 2007.09.19.}

\LOG
\initials{LDF 2007.09.19.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=focus_option_list: focus_option_list focus_option@>@/
{
   @=$$@> = 0; 
};


@q ** (2) focus_option.@>
@*1 \§focus option>.
\initials{LDF 2007.09.19.}

\LOG
\initials{LDF 2007.09.19.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <int_value> focus_option@>


@q *** (3) focus_option: WITH_FOCUS_AXES.@>
@*2 \§focus option>$\longrightarrow$ \.{WITH\_FOCUS\_AXES.}.
\initials{LDF 2007.09.19.}

\LOG
\initials{LDF 2007.09.19.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=focus_option: WITH_FOCUS_AXES@>@/
{
   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

   if (scanner_node && scanner_node->focus_options)
      scanner_node->focus_options->axes = Focus::FOCUS_AXES,

   @=$$@> = 0;
};

@q *** (3) focus_option: WITH_MAIN_AXES.@>
@*2 \§focus option> $\longrightarrow$ \.{WITH\_MAIN\_AXES.}.
\initials{LDF 2007.09.19.}

\LOG
\initials{LDF 2007.09.19.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=focus_option: WITH_MAIN_AXES@>@/
{

   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

   if (scanner_node && scanner_node->focus_options)
      scanner_node->focus_options->axes = Focus::MAIN_AXES,

   @=$$@> = 0;
};

@q *** (3) focus_option: WITH_TOLERANCE numeric_expression.@>
@*2 \§focus option> $\longrightarrow$ \.{WITH\_TOLERANCE.}.
\§numeric expression>.
\initials{LDF 2007.09.19.}

\LOG
\initials{LDF 2007.09.19.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=focus_option: WITH_TOLERANCE numeric_expression@>@/
{

   Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

   if (scanner_node && scanner_node->focus_options)
      scanner_node->focus_options->tolerance = @=$2@>;

   @=$$@> = 0;
};

@q ** (2) Rotation.@>
@*1 Rotation.
\initials{LDF 2007.09.19.}

\LOG
\initials{LDF 2007.09.19.}
Added this section.
\ENDLOG

@q *** (3) Rotation about the |Focus| axes or the main axes.@>
@*2 Rotation about the {\bf Focus} axes or the main axes.
\initials{LDF 2007.09.19.}

In the rules using |ROTATE|, the default behavior is for rotation to be performed 
about the main axes.  In the rules using |TILT|, the default behavior is for rotation 
to be performed about the ``focus axes''.  In each cases, the options \§with main axes> 
and \§with focus axes> can be specified.  Depending on the option and the command, 
this will either make no difference or cause rotation about the other set of axes.
\initials{LDF 2007.09.24.}

These options may appear multiple times in the command.  If they do, the last 
occurrence is the one that will take effect.
\initials{LDF 2007.09.24.}

\LOG
\initials{LDF 2007.09.19.}
Added this section.
\ENDLOG

@q **** (4) command --> ROTATE focus_variable numeric_expression  @> 
@q **** (4) focus_option_list.                          @>

@*3 \§command> $\longrightarrow$ \.{ROTATE} \§focus variable> 
\§numeric expression> \§focus option list>.
\initials{LDF 2007.09.02.}

\LOG
\initials{LDF 2007.09.02.}
Added this rule.

\initials{LDF 2007.09.19.}
Added \§focus option list> to the right-hand side.

\initials{LDF 2007.09.24.}
Changed the default value for |axes| from |Focus::FOCUS_AXES| to 
|Focus::MAIN_AXES|.

\initials{LDF 2007.09.24.}
Now calling |Scan_Parse::rotate_focus_func|.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=
@=command:  ROTATE focus_variable numeric_expression focus_option_list@>@/
{

    Pointer_Vector<real>* rpv = new Pointer_Vector<real>;

    *rpv += new real(@=$3@>);

    rotate_focus_func(@=$2@>, 
                      static_cast<void*>(rpv), 
                      Focus::MAIN_AXES, 
                      parameter);

    @=$$@> = static_cast<void*>(0);

};

@q **** (4) command: ROTATE focus_variable numeric_list focus_option_list.@> 

@*3 \§command> $\longrightarrow$ \.{ROTATE} \§focus variable> 
\§numeric list> \§focus option list>.
\initials{LDF 2007.09.19.}

\LOG
\initials{LDF 2007.09.19.}
Added this rule.

\initials{LDF 2007.09.24.}
Changed the default value for |axes| from |Focus::FOCUS_AXES| to 
|Focus::MAIN_AXES|.
\ENDLOG

@<Define rules@>=
@=command: ROTATE focus_variable numeric_list focus_option_list@>@/
{

    rotate_focus_func(@=$2@>, 
                      @=$3@>, 
                      Focus::MAIN_AXES, 
                      parameter);

    @=$$@> = static_cast<void*>(0);

};

@q **** (4) command --> TILT focus_variable numeric_expression  @> 
@q **** (4) focus_option_list.                                  @>

@*3 \§command> $\longrightarrow$ \.{TILT} \§focus variable> 
\§numeric expression> \§focus option list>.
\initials{LDF 2007.09.24.}

\LOG
\initials{LDF 2007.09.24.}
Added this rule.
\ENDLOG

@q ***** (5) Definition.@> 

@<Define rules@>=
@=command:  TILT focus_variable numeric_expression focus_option_list@>@/
{

    Pointer_Vector<real>* rpv = new Pointer_Vector<real>;

    *rpv += new real(@=$3@>);

    rotate_focus_func(@=$2@>, 
                      static_cast<void*>(rpv), 
                      Focus::FOCUS_AXES, 
                      parameter);

    @=$$@> = static_cast<void*>(0);

};

@q **** (4) command: TILT focus_variable numeric_list focus_option_list.@> 

@*3 \§command> $\longrightarrow$ \.{TILT} \§focus variable> 
\§numeric list> \§focus option list>.
\initials{LDF 2007.09.24.}

\LOG
\initials{LDF 2007.09.24.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=command: TILT focus_variable numeric_list focus_option_list@>@/
{

    rotate_focus_func(@=$2@>, 
                      @=$3@>, 
                      Focus::FOCUS_AXES, 
                      parameter);

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

