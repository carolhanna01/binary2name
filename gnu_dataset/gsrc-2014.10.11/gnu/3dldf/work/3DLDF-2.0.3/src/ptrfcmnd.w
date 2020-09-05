@q ptrfcmnd.w @> 
@q Created by Laurence Finston Fri Sep 24 15:35:24 CEST 2004@>
     
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



@q * (0) Transformation Commands.@>
@** Transformation Commands.
\initials{LDF 2004.09.24.}

\LOG
\initials{LDF 2004.09.24.}
Created this file. 
\ENDLOG 

@q **** (4) command: ROTATE transform_variable numeric_list.@> 

@*3 \§command> $\longrightarrow$ \.{ROTATE} \§transform variable> 
\§numeric list>.
\initials{LDF 2007.09.24.}

\LOG
\initials{LDF 2007.09.24.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=command: ROTATE transform_variable numeric_list@>@/
{

    transformation_command_func(static_cast<Scanner_Node>(parameter),
                                ROTATE,
                                static_cast<Id_Map_Entry_Node>(@=$2@>),
                                static_cast<Pointer_Vector<real>*>(@=$3@>));

    @=$$@> = static_cast<void*>(0);


};

@q **** (4) command: ROTATE picture_variable numeric_list.@> 

@*3 \§command> $\longrightarrow$ \.{ROTATE} \§picture variable> 
\§numeric list>.
\initials{LDF 2007.10.16.}

\LOG
\initials{LDF 2007.10.16.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=command: ROTATE picture_variable numeric_list@>@/
{

    transformation_command_func(static_cast<Scanner_Node>(parameter),
                                ROTATE,
                                static_cast<Id_Map_Entry_Node>(@=$2@>),
                                static_cast<Pointer_Vector<real>*>(@=$3@>));

    @=$$@> = static_cast<void*>(0);


};




@q **** (4) command: ROTATE point_variable numeric_list.@> 

@*3 \§command> $\longrightarrow$ \.{ROTATE} \§point variable> 
\§numeric list>.
\initials{LDF 2007.09.26.}

\LOG
\initials{LDF 2007.09.26.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=command: ROTATE point_variable numeric_list@>@/
{

    transformation_command_func(static_cast<Scanner_Node>(parameter),
                                ROTATE,
                                static_cast<Id_Map_Entry_Node>(@=$2@>),
                                static_cast<Pointer_Vector<real>*>(@=$3@>));

    @=$$@> = static_cast<void*>(0);

};

@q **** (4) command: ROTATE path_variable numeric_list.@> 

@*3 \§command> $\longrightarrow$ \.{ROTATE} \§path variable> 
\§numeric list>.
\initials{LDF 2007.09.26.}

\LOG
\initials{LDF 2007.09.26.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=command: ROTATE path_variable numeric_list@>@/
{

    transformation_command_func(static_cast<Scanner_Node>(parameter),
                                ROTATE,
                                static_cast<Id_Map_Entry_Node>(@=$2@>),
                                static_cast<Pointer_Vector<real>*>(@=$3@>));

    @=$$@> = static_cast<void*>(0);

};



@q **** (4) command: ROTATE ellipse_variable numeric_list.@> 

@*3 \§command> $\longrightarrow$ \.{ROTATE} \§ellipse variable> 
\§numeric list>.
\initials{LDF 2007.09.26.}

\LOG
\initials{LDF 2007.09.26.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=command: ROTATE ellipse_variable numeric_list@>@/
{

    transformation_command_func(static_cast<Scanner_Node>(parameter),
                                ROTATE,
                                static_cast<Id_Map_Entry_Node>(@=$2@>),
                                static_cast<Pointer_Vector<real>*>(@=$3@>));

    @=$$@> = static_cast<void*>(0);

};

@q **** (4) command: ROTATE circle_variable numeric_list.@> 

@*3 \§command> $\longrightarrow$ \.{ROTATE} \§circle variable> 
\§numeric list>.
\initials{LDF 2007.09.26.}

\LOG
\initials{LDF 2007.09.26.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=command: ROTATE circle_variable numeric_list@>@/
{

    transformation_command_func(static_cast<Scanner_Node>(parameter),
                                ROTATE,
                                static_cast<Id_Map_Entry_Node>(@=$2@>),
                                static_cast<Pointer_Vector<real>*>(@=$3@>));

    @=$$@> = static_cast<void*>(0);

};

@q **** (4) command: ROTATE parabola_variable numeric_list.@> 

@*3 \§command> $\longrightarrow$ \.{ROTATE} \§parabola variable> 
\§numeric list>.
\initials{LDF 2007.09.26.}

\LOG
\initials{LDF 2007.09.26.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=command: ROTATE parabola_variable numeric_list@>@/
{

    transformation_command_func(static_cast<Scanner_Node>(parameter),
                                ROTATE,
                                static_cast<Id_Map_Entry_Node>(@=$2@>),
                                static_cast<Pointer_Vector<real>*>(@=$3@>));

    @=$$@> = static_cast<void*>(0);

};

@q **** (4) command: ROTATE hyperbola_variable numeric_list.@> 

@*3 \§command> $\longrightarrow$ \.{ROTATE} \§hyperbola variable> 
\§numeric list>.
\initials{LDF 2007.09.26.}

\LOG
\initials{LDF 2007.09.26.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=command: ROTATE hyperbola_variable numeric_list@>@/
{

    transformation_command_func(static_cast<Scanner_Node>(parameter),
                                ROTATE,
                                static_cast<Id_Map_Entry_Node>(@=$2@>),
                                static_cast<Pointer_Vector<real>*>(@=$3@>));

    @=$$@> = static_cast<void*>(0);

};

@q **** (4) command: ROTATE triangle_variable numeric_list.@> 

@*3 \§command> $\longrightarrow$ \.{ROTATE} \§triangle variable> 
\§numeric list>.
\initials{LDF 2007.09.26.}

\LOG
\initials{LDF 2007.09.26.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=command: ROTATE triangle_variable numeric_list@>@/
{

    transformation_command_func(static_cast<Scanner_Node>(parameter),
                                ROTATE,
                                static_cast<Id_Map_Entry_Node>(@=$2@>),
                                static_cast<Pointer_Vector<real>*>(@=$3@>));

    @=$$@> = static_cast<void*>(0);

};



@q **** (4) command: ROTATE rectangle_variable numeric_list.@> 

@*3 \§command> $\longrightarrow$ \.{ROTATE} \§rectangle variable> 
\§numeric list>.
\initials{LDF 2007.09.26.}

\LOG
\initials{LDF 2007.09.26.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=command: ROTATE rectangle_variable numeric_list@>@/
{

    transformation_command_func(static_cast<Scanner_Node>(parameter),
                                ROTATE,
                                static_cast<Id_Map_Entry_Node>(@=$2@>),
                                static_cast<Pointer_Vector<real>*>(@=$3@>));

    @=$$@> = static_cast<void*>(0);

};

@q **** (4) command: ROTATE polygon_variable numeric_list.@> 

@*3 \§command> $\longrightarrow$ \.{ROTATE} \§polygon variable> 
\§numeric list>.
\initials{LDF 2007.09.26.}

\LOG
\initials{LDF 2007.09.26.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=command: ROTATE polygon_variable numeric_list@>@/
{

    transformation_command_func(static_cast<Scanner_Node>(parameter),
                                ROTATE,
                                static_cast<Id_Map_Entry_Node>(@=$2@>),
                                static_cast<Pointer_Vector<real>*>(@=$3@>));

    @=$$@> = static_cast<void*>(0);

};

@q **** (4) command: ROTATE reg_polygon_variable numeric_list.@> 

@*3 \§command> $\longrightarrow$ \.{ROTATE} \§reg\_polygon variable> 
\§numeric list>.
\initials{LDF 2007.09.26.}

\LOG
\initials{LDF 2007.09.26.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=command: ROTATE reg_polygon_variable numeric_list@>@/
{

    transformation_command_func(static_cast<Scanner_Node>(parameter),
                                ROTATE,
                                static_cast<Id_Map_Entry_Node>(@=$2@>),
                                static_cast<Pointer_Vector<real>*>(@=$3@>));

    @=$$@> = static_cast<void*>(0);

};




@q **** (4) command: ROTATE cuboid_variable numeric_list.@> 

@*3 \§command> $\longrightarrow$ \.{ROTATE} \§cuboid variable> 
\§numeric list>.
\initials{LDF 2007.09.26.}

\LOG
\initials{LDF 2007.09.26.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=command: ROTATE cuboid_variable numeric_list@>@/
{

    transformation_command_func(static_cast<Scanner_Node>(parameter),
                                ROTATE,
                                static_cast<Id_Map_Entry_Node>(@=$2@>),
                                static_cast<Pointer_Vector<real>*>(@=$3@>));

    @=$$@> = static_cast<void*>(0);

};

@q **** (4) command: ROTATE polyhedron_variable numeric_list.@> 

@*3 \§command> $\longrightarrow$ \.{ROTATE} \§polyhedron variable> 
\§numeric list>.
\initials{LDF 2007.09.26.}

\LOG
\initials{LDF 2007.09.26.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=command: ROTATE polyhedron_variable numeric_list@>@/
{

    transformation_command_func(static_cast<Scanner_Node>(parameter),
                                ROTATE,
                                static_cast<Id_Map_Entry_Node>(@=$2@>),
                                static_cast<Pointer_Vector<real>*>(@=$3@>));

    @=$$@> = static_cast<void*>(0);

};



@q **** (4) command: ROTATE sphere_variable numeric_list.@> 

@*3 \§command> $\longrightarrow$ \.{ROTATE} \§sphere variable> 
\§numeric list>.
\initials{LDF 2009.09.09.}

\LOG
\initials{LDF 2009.09.09.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=command: ROTATE sphere_variable numeric_list@>@/
{

    transformation_command_func(static_cast<Scanner_Node>(parameter),
                                ROTATE,
                                static_cast<Id_Map_Entry_Node>(@=$2@>),
                                static_cast<Pointer_Vector<real>*>(@=$3@>));

    @=$$@> = static_cast<void*>(0);

};



@q **** (4) command: ROTATE helix_variable numeric_list.@> 

@*3 \§command> $\longrightarrow$ \.{ROTATE} \§helix variable> 
\§numeric list>.
\initials{LDF 2007.09.26.}

\LOG
\initials{LDF 2007.09.26.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=command: ROTATE helix_variable numeric_list@>@/
{

    transformation_command_func(static_cast<Scanner_Node>(parameter),
                                ROTATE,
                                static_cast<Id_Map_Entry_Node>(@=$2@>),
                                static_cast<Pointer_Vector<real>*>(@=$3@>));

    @=$$@> = static_cast<void*>(0);

};

@q **** (4) command: ROTATE cone_variable numeric_list.@> 

@*3 \§command> $\longrightarrow$ \.{ROTATE} \§cone variable> 
\§numeric list>.
\initials{LDF 2007.09.26.}

\LOG
\initials{LDF 2007.09.26.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=command: ROTATE cone_variable numeric_list@>@/
{

    transformation_command_func(static_cast<Scanner_Node>(parameter),
                                ROTATE,
                                static_cast<Id_Map_Entry_Node>(@=$2@>),
                                static_cast<Pointer_Vector<real>*>(@=$3@>));

    @=$$@> = static_cast<void*>(0);

};

@q **** (4) command: ROTATE cylinder_variable numeric_list.@> 

@*3 \§command> $\longrightarrow$ \.{ROTATE} \§cylinder variable> 
\§numeric list>.
\initials{LDF 2007.09.26.}

\LOG
\initials{LDF 2007.09.26.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=command: ROTATE cylinder_variable numeric_list@>@/
{

    transformation_command_func(static_cast<Scanner_Node>(parameter),
                                ROTATE,
                                static_cast<Id_Map_Entry_Node>(@=$2@>),
                                static_cast<Pointer_Vector<real>*>(@=$3@>));

    @=$$@> = static_cast<void*>(0);

};

@q ** (2) command --> ROTATE @>
@q ** (2) transform_variable BY numeric_primary. @>
@*1 \§command> $\longrightarrow$ 
\.{ROTATE} \§transform variable> \.{BY} \§numeric primary>.
\initials{LDF 2007.09.26.}

\LOG
\initials{LDF 2007.09.26.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 
@
@<Define rules@>=
  
@=command: ROTATE transform_variable BY numeric_primary@>@/
{

   Pointer_Vector<real>* pv = new Pointer_Vector<real>;

   real* r = new real(@=$4@>);

   *pv += r;

   transformation_command_func(static_cast<Scanner_Node>(parameter),
                               ROTATE,
                               static_cast<Id_Map_Entry_Node>(@=$2@>),
                               pv);

  @=$$@> = static_cast<void*>(0);

};

@q ** (2) command --> ROTATE @>
@q ** (2) point_variable BY numeric_primary. @>
@*1 \§command> $\longrightarrow$ 
\.{ROTATE} \§point variable> \.{BY} \§numeric primary>.
\initials{LDF 2007.09.26.}

\LOG
\initials{LDF 2007.09.26.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 
@
@<Define rules@>=
  
@=command: ROTATE point_variable BY numeric_primary@>@/
{

   Pointer_Vector<real>* pv = new Pointer_Vector<real>;

   real* r = new real(@=$4@>);

   *pv += r;

   transformation_command_func(static_cast<Scanner_Node>(parameter),
                               ROTATE,
                               static_cast<Id_Map_Entry_Node>(@=$2@>),
                               pv);

  @=$$@> = static_cast<void*>(0);

};

@q ** (2) command --> ROTATE @>
@q ** (2) path_variable BY numeric_primary. @>
@*1 \§command> $\longrightarrow$ 
\.{ROTATE} \§path variable> \.{BY} \§numeric primary>.
\initials{LDF 2007.09.26.}

\LOG
\initials{LDF 2007.09.26.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 
@
@<Define rules@>=
  
@=command: ROTATE path_variable BY numeric_primary@>@/
{

   Pointer_Vector<real>* pv = new Pointer_Vector<real>;

   real* r = new real(@=$4@>);

   *pv += r;

   transformation_command_func(static_cast<Scanner_Node>(parameter),
                               ROTATE,
                               static_cast<Id_Map_Entry_Node>(@=$2@>),
                               pv);

  @=$$@> = static_cast<void*>(0);

};

@q ** (2) command --> ROTATE @>
@q ** (2) ellipse_variable BY numeric_primary. @>
@*1 \§command> $\longrightarrow$ 
\.{ROTATE} \§ellipse variable> \.{BY} \§numeric primary>.
\initials{LDF 2007.09.26.}

\LOG
\initials{LDF 2007.09.26.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 
@
@<Define rules@>=
  
@=command: ROTATE ellipse_variable BY numeric_primary@>@/
{

   Pointer_Vector<real>* pv = new Pointer_Vector<real>;

   real* r = new real(@=$4@>);

   *pv += r;

   transformation_command_func(static_cast<Scanner_Node>(parameter),
                               ROTATE,
                               static_cast<Id_Map_Entry_Node>(@=$2@>),
                               pv);

  @=$$@> = static_cast<void*>(0);

};

@q ** (2) command --> ROTATE @>
@q ** (2) circle_variable BY numeric_primary. @>
@*1 \§command> $\longrightarrow$ 
\.{ROTATE} \§circle variable> \.{BY} \§numeric primary>.
\initials{LDF 2007.09.26.}

\LOG
\initials{LDF 2007.09.26.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 
@
@<Define rules@>=
  
@=command: ROTATE circle_variable BY numeric_primary@>@/
{

   Pointer_Vector<real>* pv = new Pointer_Vector<real>;

   real* r = new real(@=$4@>);

   *pv += r;

   transformation_command_func(static_cast<Scanner_Node>(parameter),
                               ROTATE,
                               static_cast<Id_Map_Entry_Node>(@=$2@>),
                               pv);

  @=$$@> = static_cast<void*>(0);

};

@q ** (2) command --> ROTATE @>
@q ** (2) parabola_variable BY numeric_primary. @>
@*1 \§command> $\longrightarrow$ 
\.{ROTATE} \§parabola variable> \.{BY} \§numeric primary>.
\initials{LDF 2007.09.26.}

\LOG
\initials{LDF 2007.09.26.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 
@
@<Define rules@>=
  
@=command: ROTATE parabola_variable BY numeric_primary@>@/
{

   Pointer_Vector<real>* pv = new Pointer_Vector<real>;

   real* r = new real(@=$4@>);

   *pv += r;

   transformation_command_func(static_cast<Scanner_Node>(parameter),
                               ROTATE,
                               static_cast<Id_Map_Entry_Node>(@=$2@>),
                               pv);

  @=$$@> = static_cast<void*>(0);

};

@q ** (2) command --> ROTATE @>
@q ** (2) hyperbola_variable BY numeric_primary. @>
@*1 \§command> $\longrightarrow$ 
\.{ROTATE} \§hyperbola variable> \.{BY} \§numeric primary>.
\initials{LDF 2007.09.26.}

\LOG
\initials{LDF 2007.09.26.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 
@
@<Define rules@>=
  
@=command: ROTATE hyperbola_variable BY numeric_primary@>@/
{

   Pointer_Vector<real>* pv = new Pointer_Vector<real>;

   real* r = new real(@=$4@>);

   *pv += r;

   transformation_command_func(static_cast<Scanner_Node>(parameter),
                               ROTATE,
                               static_cast<Id_Map_Entry_Node>(@=$2@>),
                               pv);

  @=$$@> = static_cast<void*>(0);

};

@q ** (2) command --> ROTATE @>
@q ** (2) triangle_variable BY numeric_primary. @>
@*1 \§command> $\longrightarrow$ 
\.{ROTATE} \§triangle variable> \.{BY} \§numeric primary>.
\initials{LDF 2007.09.26.}

\LOG
\initials{LDF 2007.09.26.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 
@
@<Define rules@>=
  
@=command: ROTATE triangle_variable BY numeric_primary@>@/
{

   Pointer_Vector<real>* pv = new Pointer_Vector<real>;

   real* r = new real(@=$4@>);

   *pv += r;

   transformation_command_func(static_cast<Scanner_Node>(parameter),
                               ROTATE,
                               static_cast<Id_Map_Entry_Node>(@=$2@>),
                               pv);

  @=$$@> = static_cast<void*>(0);

};

@q ** (2) command --> ROTATE @>
@q ** (2) polygon_variable BY numeric_primary. @>
@*1 \§command> $\longrightarrow$ 
\.{ROTATE} \§polygon variable> \.{BY} \§numeric primary>.
\initials{LDF 2007.09.26.}

\LOG
\initials{LDF 2007.09.26.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 
@
@<Define rules@>=
  
@=command: ROTATE polygon_variable BY numeric_primary@>@/
{

   Pointer_Vector<real>* pv = new Pointer_Vector<real>;

   real* r = new real(@=$4@>);

   *pv += r;

   transformation_command_func(static_cast<Scanner_Node>(parameter),
                               ROTATE,
                               static_cast<Id_Map_Entry_Node>(@=$2@>),
                               pv);

  @=$$@> = static_cast<void*>(0);

};

@q ** (2) command --> ROTATE @>
@q ** (2) reg_polygon_variable BY numeric_primary. @>
@*1 \§command> $\longrightarrow$ 
\.{ROTATE} \§reg\_polygon variable> \.{BY} \§numeric primary>.
\initials{LDF 2007.09.26.}

\LOG
\initials{LDF 2007.09.26.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 
@
@<Define rules@>=
  
@=command: ROTATE reg_polygon_variable BY numeric_primary@>@/
{

   Pointer_Vector<real>* pv = new Pointer_Vector<real>;

   real* r = new real(@=$4@>);

   *pv += r;

   transformation_command_func(static_cast<Scanner_Node>(parameter),
                               ROTATE,
                               static_cast<Id_Map_Entry_Node>(@=$2@>),
                               pv);

  @=$$@> = static_cast<void*>(0);

};

@q ** (2) command --> ROTATE @>
@q ** (2) cuboid_variable BY numeric_primary. @>
@*1 \§command> $\longrightarrow$ 
\.{ROTATE} \§cuboid variable> \.{BY} \§numeric primary>.
\initials{LDF 2007.09.26.}

\LOG
\initials{LDF 2007.09.26.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 
@
@<Define rules@>=
  
@=command: ROTATE cuboid_variable BY numeric_primary@>@/
{

   Pointer_Vector<real>* pv = new Pointer_Vector<real>;

   real* r = new real(@=$4@>);

   *pv += r;

   transformation_command_func(static_cast<Scanner_Node>(parameter),
                               ROTATE,
                               static_cast<Id_Map_Entry_Node>(@=$2@>),
                               pv);

  @=$$@> = static_cast<void*>(0);

};

@q ** (2) command --> ROTATE @>
@q ** (2) polyhedron_variable BY numeric_primary. @>
@*1 \§command> $\longrightarrow$ 
\.{ROTATE} \§polyhedron variable> \.{BY} \§numeric primary>.
\initials{LDF 2007.09.26.}

\LOG
\initials{LDF 2007.09.26.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 
@
@<Define rules@>=
  
@=command: ROTATE polyhedron_variable BY numeric_primary@>@/
{

   Pointer_Vector<real>* pv = new Pointer_Vector<real>;

   real* r = new real(@=$4@>);

   *pv += r;

   transformation_command_func(static_cast<Scanner_Node>(parameter),
                               ROTATE,
                               static_cast<Id_Map_Entry_Node>(@=$2@>),
                               pv);

  @=$$@> = static_cast<void*>(0);

};

@q ** (2) command --> ROTATE @>
@q ** (2) helix_variable BY numeric_primary. @>
@*1 \§command> $\longrightarrow$ 
\.{ROTATE} \§helix variable> \.{BY} \§numeric primary>.
\initials{LDF 2007.09.26.}

\LOG
\initials{LDF 2007.09.26.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 
@
@<Define rules@>=
  
@=command: ROTATE helix_variable BY numeric_primary@>@/
{

   Pointer_Vector<real>* pv = new Pointer_Vector<real>;

   real* r = new real(@=$4@>);

   *pv += r;

   transformation_command_func(static_cast<Scanner_Node>(parameter),
                               ROTATE,
                               static_cast<Id_Map_Entry_Node>(@=$2@>),
                               pv);

  @=$$@> = static_cast<void*>(0);

};

@q ** (2) command --> ROTATE @>
@q ** (2) cone_variable BY numeric_primary. @>
@*1 \§command> $\longrightarrow$ 
\.{ROTATE} \§cone variable> \.{BY} \§numeric primary>.
\initials{LDF 2007.09.26.}

\LOG
\initials{LDF 2007.09.26.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 
@
@<Define rules@>=
  
@=command: ROTATE cone_variable BY numeric_primary@>@/
{

   Pointer_Vector<real>* pv = new Pointer_Vector<real>;

   real* r = new real(@=$4@>);

   *pv += r;

   transformation_command_func(static_cast<Scanner_Node>(parameter),
                               ROTATE,
                               static_cast<Id_Map_Entry_Node>(@=$2@>),
                               pv);

  @=$$@> = static_cast<void*>(0);

};

@q ** (2) command --> ROTATE @>
@q ** (2) cylinder_variable BY numeric_primary. @>
@*1 \§command> $\longrightarrow$ 
\.{ROTATE} \§cylinder variable> \.{BY} \§numeric primary>.
\initials{LDF 2007.09.26.}

\LOG
\initials{LDF 2007.09.26.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 
@
@<Define rules@>=
  
@=command: ROTATE cylinder_variable BY numeric_primary@>@/
{

   Pointer_Vector<real>* pv = new Pointer_Vector<real>;

   real* r = new real(@=$4@>);

   *pv += r;

   transformation_command_func(static_cast<Scanner_Node>(parameter),
                               ROTATE,
                               static_cast<Id_Map_Entry_Node>(@=$2@>),
                               pv);

  @=$$@> = static_cast<void*>(0);

};




@q ** (2) command --> basic_transformation_command @>
@q ** (2) any_variable BY numeric_primary. @>
@*1 \§command> $\longrightarrow$ 
\§basic transformation command> \§any variable> 
\.{BY} \§numeric primary>.
\initials{LDF 2004.11.09.}

\LOG
\initials{LDF 2004.11.09.}
Added this rule.  |BY| is needed, because otherwise the 
parser doesn't resolve the |numeric_primary| properly.

\initials{LDF 2007.09.26.}
Simplified this rule.  
Removed debugging output and error-handling code.
\ENDLOG

@q *** (3) Definition.@> 
@
@<Define rules@>=
  
@=command: basic_transformation_command any_variable @>
@=BY numeric_primary@>@/

{

   Pointer_Vector<real>* pv = new Pointer_Vector<real>;

   real* r = new real(@=$4@>);

   *pv += r;

   if (@=$1@> == SCALE)
   {
       r = new real(@=$4@>);
       *pv += r;
     
       r = new real(@=$4@>);
       *pv += r;

   } /* |if (basic_transformation_command == SCALE)|  */
               

  transformation_command_func(static_cast<Scanner_Node>(parameter),
                              @=$1@>,
                              static_cast<Id_Map_Entry_Node>(@=$2@>),
                              pv);

  @=$$@> = static_cast<void*>(0);

};

@q ** (2) command --> basic_transformation_command any_variable BY point_expression.@>
@*1 \§command> $\longrightarrow$ \§basic transformation command> \§any variable> 
\.{BY} \§point expression>.
\initials{LDF 2007.10.19.}

\LOG
\initials{LDF 2007.10.19.}
Added this rule.  
\ENDLOG

@q *** (3) Definition.@> 
@
@<Define rules@>=
  
@=command: basic_transformation_command any_variable @>
@=BY point_expression@>@/

{

   Point* p = static_cast<Point*>(@=$4@>); 

   if (p == static_cast<Point*>(0) || *p == INVALID_POINT)
   {
        @=$$@> = static_cast<void*>(0);
   }
   else
   {
       Pointer_Vector<real>* pv = new Pointer_Vector<real>;

       *pv += new real(p->get_x());
       *pv += new real(p->get_y());
       *pv += new real(p->get_z());

        transformation_command_func(static_cast<Scanner_Node>(parameter),
                                 @=$1@>,
                                 static_cast<Id_Map_Entry_Node>(@=$2@>),
                                 pv);

   }  /* |else|  */  
  
   delete p;
   @=$$@> = static_cast<void*>(0);

};

@q ** (2) command --> basic_transformation_command @>
@q ** (2) any_variable numeric_list.               @>
@*1 \§command> $\longrightarrow$ 
\§basic transformation command> \§any variable> 
\§numeric list>.
\initials{LDF 2004.09.24.}

\LOG
\initials{LDF 2004.09.24.}
Added this rule.

\initials{LDF 2004.09.24.}
Now calling |Scan_Parse::transformation_command_func()|, which contains code
I've removed from this rule. 

\initials{LDF 2005.10.25.}
Removed all unnecessary code.
\ENDLOG

@q *** (3) Definition.@> 
@
@<Define rules@>=
  
@=command: basic_transformation_command any_variable@>
@= numeric_list@>@/

{

    transformation_command_func(static_cast<Scanner_Node>(parameter),
                                @=$1@>,
                                static_cast<Id_Map_Entry_Node>(@=$2@>),
                                static_cast<Pointer_Vector<real>*>(@=$3@>));

    @=$$@> = static_cast<void*>(0);

};

@q * (1) |basic_transformation_command|. @>
@*  \§basic transformation command>.
\initials{LDF 2004.09.25.}

\LOG
\initials{LDF 2004.09.25.}
Added this type declaration.
\ENDLOG


@<Type declarations for non-terminal symbols@>=
@=%type <int_value> basic_transformation_command@>

@q ** (2) basic_transformation_command --> SCALE@>
@*3 \§basic transformation command> --> \.{SCALE}.
\initials{LDF 2004.09.25.}

\LOG
\initials{LDF 2004.09.25.}
Added this rule.

\initials{LDF 2005.10.25.}
Removed all unnecessary code.
\ENDLOG

@<Define rules@>=

@=basic_transformation_command: SCALE@>
{

  @=$$@> = SCALE;

};

@q ** (2) basic_transformation_command --> SHEAR@>
@*3 \§basic transformation command> --> \.{SHEAR}.
\initials{LDF 2004.09.25.}

\LOG
\initials{LDF 2004.09.25.}
Added this rule.

\initials{LDF 2005.10.25.}
Removed all unnecessary code.
\ENDLOG

@<Define rules@>=

@=basic_transformation_command: SHEAR@>
{

  @=$$@> = SHEAR;

};

@q ** (2) basic_transformation_command --> SHIFT@>
@*3 \§basic transformation command> --> \.{SHIFT}.
\initials{LDF 2004.09.25.}

\LOG
\initials{LDF 2004.09.25.}
Added this rule.

\initials{LDF 2005.10.25.}
Removed all unnecessary code.
\ENDLOG

@<Define rules@>=

@=basic_transformation_command: SHIFT@>
{

  @=$$@> = SHIFT;

};

@q ** (2) transformation_command --> ROTATE_AROUND any_variable @>
@q ** (2) LEFT_PARENTHESIS point_expression COMMA               @>
@q ** (2) point_expression RIGHT_PARENTHESIS                    @>
@q ** (2) numeric_expression.                                   @>

@*1 \§command> $\longrightarrow$ 
\.{ROTATE\_AROUND} \§any variable> 
\.{LEFT\_PARENTHESIS } \§point expression>
\.{COMMA} \§point expression> \.{RIGHT\_PARENTHESIS}
\§numeric expression>.


\LOG
\initials{LDF 2004.09.29.}
Added this rule.

\initials{LDF 2004.10.13.}
@:BUG FIX@> BUG FIX: 
Changed this rule from
``\§command> $\longrightarrow$ 
\.{ROTATE} \§any variable> \.{AROUND} $\ldots$''
to 
``\§command> $\longrightarrow$ 
\.{ROTATE\_AROUND} \§any variable> $\ldots$''.  This was necessary
because using the token |ROTATE| for both rotation about the main axes
and rotation around an arbitrary axis didn't work.  The rule 
\§basic transformation command> $\longrightarrow$ \.{ROTATE} was 
never reduced.
\ENDLOG

@q *** (3) Definition.@> 
@
@<Define rules@>=
@=command: ROTATE_AROUND any_variable @>
@=LEFT_PARENTHESIS point_expression COMMA point_expression @>
@=RIGHT_PARENTHESIS numeric_expression@>@/                
{

  @<Common declarations for rules@>@; 

#if DEBUG_COMPILE
  DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
      cerr_strm << thread_name 
                << "*** Parser: `transformation_command --> "
                << "ROTATE_AROUND any_variable "
                << endl 
                << "LEFT_PARENTHESIS point_expression "
                << endl 
                << "COMMA point_expression RIGHT_PARENTHESIS "
                << "numeric_expression'.";            

      log_message(cerr_strm); 
      cerr_message(cerr_strm); 
      cerr_strm.str(""); 
    }
#endif /* |DEBUG_COMPILE|  */@;

  entry = static_cast<Id_Map_Entry_Node>(@=$2@>); 

@q **** (4) Error handling:  |entry == 0 || entry->object == 0|.@>   

@ Error handling:  |entry == 0 || entry->object == 0|.
\initials{LDF 2004.09.29.}

@<Define rules@>=

  if (entry == static_cast<Id_Map_Entry_Node>(0) || entry->object == static_cast<void*>(0))
    {


        cerr_strm << thread_name << "ERROR! In `yyparse()', rule "
                  << endl 
                  << "`transformation_command --> "
                  << "ROTATE_AROUND any_variable "
                  << endl 
                  << "LEFT_PARENTHESIS point_expression "
                  << endl 
                  << "COMMA point_expression RIGHT_PARENTHESIS "
                  << "numeric_expression':"
                  << endl 
                  << "`any_variable' is invalid."
                  << "Setting `transformation_command' to 0 and exiting rule.";

        log_message(cerr_strm);
        cerr_message(cerr_strm, error_stop_value);
        cerr_strm.str("");
        
        @=$$@> = static_cast<void*>(0);
     

    }  /* |if (entry == 0 || entry->object == 0)|  */


@q **** (4) |entry != 0 &&  entry->object != 0|.@>   

@ |entry != 0 &&  entry->object != 0|.
\initials{LDF 2004.09.29.}

@<Define rules@>=


  else  /* |!(entry == 0 || entry->object == 0)|  */
    {


      Transform* t = rotate_around_func(scanner_node, 
                                        static_cast<Point*>(@=$4@>),
                                        static_cast<Point*>(@=$6@>),
                                        @=$8@>);


@q ***** (5) Error handling:  |Scan_Parse::rotate_around_func()| failed.@>

@ Error handling:  |Scan_Parse::rotate_around_func()| failed.
\initials{LDF 2004.09.29.}

@<Define rules@>=

  if (t == static_cast<Transform*>(0))
    {

      cerr_strm << thread_name << "ERROR! In `yyparse()', rule "
                << endl 
                << "`transformation_command --> "
                << "ROTATE_AROUND any_variable "
                << endl 
                << "LEFT_PARENTHESIS point_expression "
                << endl 
                << "COMMA point_expression RIGHT_PARENTHESIS "
                << "numeric_expression':"
                << endl 
                << "`Scan_Parse::rotate_around_func()' failed."
                << "Not rotating `" << entry->name << "'. "
                << "Setting `transformation_command' to 0 and exiting rule.";

      log_message(cerr_strm);
      cerr_message(cerr_strm, error_stop_value);
      cerr_strm.str("");
      
      @=$$@> = static_cast<void*>(0);

    } /* |if (t == 0)|  */


@q ***** (5) |Scan_Parse::rotate_around_func()| succeeded.@>

@ |Scan_Parse::rotate_around_func()| succeeded.
\initials{LDF 2004.09.29.}

@<Define rules@>=

  else  /* |t != 0|  */
    {

#if DEBUG_COMPILE
      if (DEBUG)
        {
          cerr_strm << thread_name << "In `yyparse()', "
                    << "rule `transformation_command --> "
                    << "ROTATE_AROUND any_variable "
                    << endl 
                    << "LEFT_PARENTHESIS point_expression "
                    << endl 
                    << "COMMA point_expression RIGHT_PARENTHESIS "
                    << "numeric_expression':"
                    << endl 
                    << "`Scan_Parse::rotate_around_func()' succeeded.";

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str("");
          
        }
#endif /* |DEBUG_COMPILE|  */@; 

      *entry *= *t;

      delete t;

      @=$$@> = static_cast<void*>(0);

    }  /* |else| (|t != 0|)  */


@q ***** (5).@> 

    }  /* |else| (|!(entry == 0 || entry->object == 0)|)  */

};

@q ** (2) command --> INVERT transform_variable.@>
@*3 \§command> $\longrightarrow$ \.{INVERT} \§transform variable>.
\initials{LDF 2007.10.14.}

\LOG
\initials{LDF 2007.10.14.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=command: INVERT transform_variable@>
{

     Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$2@>);

     if (entry && entry->object)
     {
        Transform* t = static_cast<Transform*>(entry->object);
        t->inverse(true);
     } 

     @=$$@> = static_cast<void*>(0);

};

@q ** (2) command --> TRANSPOSE transform_variable.@>
@*3 \§command> $\longrightarrow$ \.{TRANSPOSE} \§transform variable>.
\initials{LDF 2007.10.14.}

\LOG
\initials{LDF 2007.10.14.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=command: TRANSPOSE transform_variable@>
{

     Id_Map_Entry_Node entry = static_cast<Id_Map_Entry_Node>(@=$2@>);

     if (entry && entry->object)
     {
        Transform* t = static_cast<Transform*>(entry->object);
        t->transpose(true);
     } 

     @=$$@> = static_cast<void*>(0);

};

@q * (1) |transformation_assignment_command|. @>
@*  \§transformation assignment command>.
\initials{LDF 2004.10.01.}

\LOG
\initials{LDF 2004.10.01.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <pointer_value> transformation_assignment_command@>


@q ** (2) transformation_assignment_command -->          @>
@q ** (2) any_variable TIMES_ASSIGN transform_expression.@>

@*1 \§transformation assignment command> $\longrightarrow$ 
\§any variable> \.{TIMES\_ASSIGN} \§transform expression>.
\initials{LDF 2004.10.01.}

\LOG
\initials{LDF 2004.10.01.}
Added this rule.

\initials{LDF 2004.10.01.}
Removed code from this rule to 
|Scan_Parse::transformation_assignment_command_func()|, which is defined in
\filename{scanprsf.web}. 

\initials{LDF 2005.10.25.}
Removed all unnecessary code.
\ENDLOG


@q *** (3) Definition.@> 

@<Define rules@>=

@=transformation_assignment_command: any_variable TIMES_ASSIGN @>@/
@=transform_expression@>@/
{


     Transform* t
        = transformation_assignment_command_func(static_cast<Scanner_Node>(parameter),
                                                 static_cast<Id_Map_Entry_Node>($1),
                                                 static_cast<Transform*>(@=$3@>));
 
   @=$$@> = t;
  
};

@q ** (2) transformation_assignment_command --> any_variable @>
@q ** (2) TIMES_ASSIGN transformation_assignment_command.    @>

@*1 \§transformation assignment command> $\longrightarrow$ 
\§any variable> \.{TIMES\_ASSIGN} 
\§transformation assignment command>.
\initials{LDF 2004.10.01.}

\LOG
\initials{LDF 2004.10.01.}
Added this rule.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=transformation_assignment_command: any_variable TIMES_ASSIGN @>@/
@=transformation_assignment_command@>@/
{

    Transform* t
       = transformation_assignment_command_func(static_cast<Scanner_Node>(parameter),
                                                static_cast<Id_Map_Entry_Node>($1),
                                                static_cast<Transform*>(@=$3@>));

    @=$$@> = t;

};

@q ** (2) transformation_assignment_command -->            @>
@q ** (2) point_variable TIMES_ASSIGN transform_expression.@>

@*1 \§transformation assignment command> $\longrightarrow$ 
\§any variable> \.{TIMES\_ASSIGN} \§transform expression>.
\initials{LDF 2005.02.21.}

\LOG
\initials{LDF 2005.02.21.}
Added this rule.

\initials{LDF 2005.10.25.}
Removed all unnecessary code.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=transformation_assignment_command: point_variable TIMES_ASSIGN @>@/
@=transform_expression@>@/
{

    Transform* t
      = transformation_assignment_command_func(static_cast<Scanner_Node>(parameter),
                                               static_cast<Id_Map_Entry_Node>($1),
                                               static_cast<Transform*>(@=$3@>));

  
 
    @=$$@> = t;
  

};

@q ** (2) transformation_assignment_command --> point_variable @>
@q ** (2) TIMES_ASSIGN transformation_assignment_command.    @>

@*1 \§transformation assignment command> $\longrightarrow$ 
\§any variable> \.{TIMES\_ASSIGN} 
\§transformation assignment command>.
\initials{LDF 2005.02.21.}

\LOG
\initials{LDF 2005.02.21.}
Added this rule.

\initials{LDF 2005.10.25.}
Removed all unnecessary code.
\ENDLOG

@q *** (3) Definition.@> 

@<Define rules@>=

@=transformation_assignment_command: point_variable TIMES_ASSIGN @>@/
@=transformation_assignment_command@>@/
{



     Transform* t
        = transformation_assignment_command_func(static_cast<Scanner_Node>(parameter),
                                                 static_cast<Id_Map_Entry_Node>($1),
                                                 static_cast<Transform*>(@=$3@>));

     @=$$@> = t;
  
};





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
