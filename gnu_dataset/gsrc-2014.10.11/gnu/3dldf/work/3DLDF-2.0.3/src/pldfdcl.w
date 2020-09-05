@q pldfdcl.w @> 
@q Created by Laurence Finston Thu Apr 29 13:46:49 MEST 2004 @>

@q * Copyright and License.@>

@q This file is part of 3DLDF, a package for three-dimensional drawing. @>
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

@q ("@@" stands for a single at-sign in the following paragraph.) @>

@q Please send bug reports to Laurence.Finston@@gmx.de @>
@q The mailing list help-3dldf@@gnu.org is available for people to @>
@q ask other users for help.  @>
@q The mailing list info-3dldf@@gnu.org is for sending @>
@q announcements to users. To subscribe to these mailing lists, send an @>
@q email with "subscribe <email-address>" as the subject.  @>

@q The author can be contacted at: @>

@q Laurence D. Finston                 @> 
@q c/o Free Software Foundation, Inc.  @>
@q 51 Franklin St, Fifth Floor         @> 
@q Boston, MA  02110-1301              @>
@q USA                                 @>

@q Laurence.Finston@@gmx.de (@@ stands for a single "at" sign.)@>



@q * (0) Declarations.  @>
@** Declarations.

@q **  declaration.  @>
@ {\bf declaration}.  

@q *** (3) declaration --> boolean declaration.  @>
@*2 \§declaration> $\longrightarrow$ \§boolean declaration>.
\initials{LDF 2004.05.17.}  

\LOG
\initials{LDF 2004.05.17.}  
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: boolean_declaration@>
{

#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @;
  if (DEBUG)
    {
      stringstream cerr_strm;
      cerr_strm << "*** Parser: 'declaration --> boolean_declaration'."; 

      log_message(cerr_strm);
      cerr_message(cerr_strm);
      cerr_strm.str("");
      
    }
#endif /* |DEBUG_COMPILE|  */

};

@q *** (3) declaration --> complex declaration.  @>
@*2 \§declaration> $\longrightarrow$ \§complex declaration>.
\initials{LDF 2007.12.02.}

\LOG
\initials{LDF 2007.12.02.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: complex_declaration@>
;

@q *** (3) declaration --> matrix declaration.  @>
@*2 \§declaration> $\longrightarrow$ \§matrix declaration>.
\initials{LDF 2007.11.28.}

\LOG
\initials{LDF 2007.11.28.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: matrix_declaration@>
;

@q *** (3) declaration --> string declaration.  @>
@*2 \§declaration> $\longrightarrow$ \§string declaration>.
\initials{LDF 2004.05.19.}  

\LOG
\initials{LDF 2004.05.19.}  
Added this rule.

\initials{LDF 2004.12.12.}
Made debugging output thread-safe.
\ENDLOG 


@<Define rules@>=
@=declaration: string_declaration@>
{

#if DEBUG_COMPILE
   bool DEBUG = false; /* |true| */ @;
   if (DEBUG)
      {
          stringstream cerr_strm;
          cerr_strm  
                    << "*** Parser:  'declaration --> string_declaration'."; 

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str();
    }
#endif /* |DEBUG_COMPILE|  */

};

@q *** (3) declaration --> pen declaration.  @>
@*2 \§declaration> $\longrightarrow$ \§pen declaration>.
\§declaration>.

\LOG
\initials{LDF 2004.05.21.}  
Added this rule.

\initials{LDF 2004.12.12.}
Made debugging output thread-safe.
\ENDLOG 

@<Define rules@>=
@=declaration: pen_declaration@>
{

#if DEBUG_COMPILE
   bool DEBUG = false; /* |true| */ @;
   if (DEBUG)
      {
         stringstream cerr_strm;
         cerr_strm  << "*** Parser:  'declaration --> pen_declaration'."; 

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str();
    }
#endif /* |DEBUG_COMPILE|  */

};

@q *** (3) declaration --> dash_pattern_declaration.  @>
@*2 \§declaration> $\longrightarrow$ \§dash pattern declaration>.
\§declaration>.

\LOG
\initials{LDF 2004.05.21.}  
Added this rule.

\initials{LDF 2004.12.12.}
Made debugging output thread-safe.
\ENDLOG 

@<Define rules@>=
@=declaration: dash_pattern_declaration@>
{

#if DEBUG_COMPILE
   bool DEBUG = false; /* |true| */ @;
   if (DEBUG)
      {
          stringstream cerr_strm;
          cerr_strm  << "*** Parser:  'declaration --> "
                    << "dash_pattern_declaration'."; 

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str();
      }
#endif /* |DEBUG_COMPILE|  */

};

@q *** (3) declaration --> focus_declaration.  @>
@*2 \§declaration> $\longrightarrow$ \§focus declaration>.
\§declaration>.
\initials{LDF 2004.05.21.}  

\LOG
\initials{LDF 2004.05.21.}  
Added this rule.

\initials{LDF 2004.12.12.}
Made debugging output thread-safe.
\ENDLOG 

@<Define rules@>=
@=declaration: focus_declaration@>
{
#if DEBUG_COMPILE
   bool DEBUG = false; /* |true| */ @;
   if (DEBUG)
      {
          stringstream cerr_strm;
          cerr_strm  << "*** Parser:  'declaration --> focus_declaration'."; 

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str();

      }
#endif /* |DEBUG_COMPILE|  */

};

@q *** (3) declaration --> macro_declaration.  @>
@*2 \§declaration> $\longrightarrow$ \§macro declaration>.
\§declaration>.
\initials{LDF 2004.12.28.}

\LOG
\initials{LDF 2004.12.28.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: macro_declaration@>
{
#if DEBUG_COMPILE
   bool DEBUG = false; /* |true| */ @;
   if (DEBUG)
      {
          stringstream cerr_strm;
          cerr_strm  << "*** Parser:  'declaration --> macro_declaration'."; 

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str();

      }
#endif /* |DEBUG_COMPILE|  */

};

@q *** (3) declaration --> color declaration.  @>
@*2 \§declaration> $\longrightarrow$ \§color declaration>.
\initials{LDF 2004.05.25.}  

\LOG
\initials{LDF 2004.05.25.}  
Added this rule.

\initials{LDF 2004.12.11.}
Made debugging output thread-safe.
\ENDLOG 

@<Define rules@>=
@=declaration: color_declaration@>
{

#if DEBUG_COMPILE
   bool DEBUG = false; /* |true| */ @;
   if (DEBUG)
      {
         stringstream cerr_strm;
         cerr_strm  
                   << "*** Parser:  'declaration --> color_declaration'."; 

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str();
      }
#endif /* |DEBUG_COMPILE|  */

};

@q *** (3) declaration --> numeric declaration.  @>
@*2 \§declaration> $\longrightarrow$ \§numeric declaration>.

\LOG
\initials{LDF 2004.12.11.}
Made debugging output thread-safe.
\ENDLOG 

@<Define rules@>=
@=declaration: numeric_declaration@>
;

@q *** (3) declaration --> ulong_long declaration.  @>
@*2 \§declaration> $\longrightarrow$ \§ulong long declaration>.
\initials{LDF 2005.12.05.}

\LOG
\initials{LDF 2005.12.05.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=declaration: ulong_long_declaration@>
;

@q **** (4) declaration --> transform_declaration.  @>
@*3 \§declaration> $\longrightarrow$ \§transform declaration>.  

\LOG
\initials{LDF 2004.12.11.}
Made debugging output thread-safe.
\ENDLOG 

@<Define rules@>=

@=declaration: transform_declaration@>
;


@q *** (3) declaration --> picture declaration.  @>
@*2 \§declaration> $\longrightarrow$ \§picture declaration>.
\initials{LDF 2004.05.21.}  

\LOG
\initials{LDF 2004.05.21.}  
Added this rule.

\initials{LDF 2004.12.11.}
Made debugging output thread-safe.
\ENDLOG 

@<Define rules@>=
@=declaration: picture_declaration@>
{

#if DEBUG_COMPILE
   bool DEBUG = false; /* |true| */ @;
   if (DEBUG)
      {
          stringstream cerr_strm;
          cerr_strm  
                    << "*** Parser: 'declaration --> picture_declaration'."; 

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str();
      }
#endif /* |DEBUG_COMPILE|  */

};

@q *** (3) declaration --> point_declaration.  @>
@*2 \§declaration> $\longrightarrow$ \§point declaration>.

\LOG
\initials{LDF 2004.12.11.}
Made debugging output thread-safe.
\ENDLOG 

@<Define rules@>=
@=declaration: point_declaration@>
{

#if DEBUG_COMPILE
   bool DEBUG = false; /* |true| */ @; 
   if (DEBUG)
      {
          stringstream cerr_strm;
          cerr_strm  
                    << "*** Parser: 'declaration --> point_declaration'.";

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str();
      }
#endif /* |DEBUG_COMPILE|  */

};

@q *** (3) declaration --> bool_point declaration.  @>
@*2 \§declaration> $\longrightarrow$ \§bool-point declaration>.

\LOG
\initials{LDF 2004.09.01.}  
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: bool_point_declaration@>
{

#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @;
  if (DEBUG)
    {
      stringstream cerr_strm;
      cerr_strm  
                << "*** Parser: 'declaration --> bool_point_declaration'."; 

      log_message(cerr_strm);
      cerr_message(cerr_strm);
      cerr_strm.str("");
      
    }
#endif /* |DEBUG_COMPILE|  */

};

@q *** (3) declaration --> path declaration.  @>
@*2 \§declaration> $\longrightarrow$ |path_declaration|.  

\LOG
\initials{LDF 2004.12.11.}
Made debugging output thread-safe.
\ENDLOG 


@<Define rules@>=
@=declaration: path_declaration@>
{

#if DEBUG_COMPILE
   bool DEBUG = false; /* |true| */ @; 
   if (DEBUG)
      {
          stringstream cerr_strm;
          cerr_strm  
                    << "*** Parser: 'declaration --> path_declaration'.";
          
          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str();
      }
#endif /* |DEBUG_COMPILE|  */

};

@q *** (3) declaration --> circle declaration.  @>
@*2 \§declaration> $\longrightarrow$ |circle_declaration|.  

\LOG
\initials{LDF 2004.06.17.}
Added this rule.

\initials{LDF 2004.12.11.}
Made debugging output thread-safe.
\ENDLOG

@<Define rules@>=
@=declaration: circle_declaration@>
{

#if DEBUG_COMPILE
   bool DEBUG = false; /* |true| */ @; 
   if (DEBUG)
      {
         stringstream cerr_strm;
         cerr_strm  
                   << "*** Parser:  'declaration --> circle_declaration'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str();

      }
#endif /* |DEBUG_COMPILE|  */

};

@q *** (3) declaration --> parabola declaration.  @>
@*2 \§declaration> $\longrightarrow$ \§parabola declaration>.  
\initials{LDF 2005.11.07.}

\LOG
\initials{LDF 2005.11.07.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=declaration: parabola_declaration@>
;


@q *** (3) declaration --> hyperbola declaration.  @>
@*2 \§declaration> $\longrightarrow$ \§hyperbola declaration>.  
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=declaration: hyperbola_declaration@>
;

@q *** (3) declaration --> arc declaration.  @>
@*2 \§declaration> $\longrightarrow$ \§arc declaration>.  
\initials{LDF 2007.10.13.}

\LOG
\initials{LDF 2007.10.13.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=declaration: arc_declaration@>
;



@q *** (3) declaration --> conic_section_lattice declaration.  @>
@*2 \§declaration> $\longrightarrow$ \§conic section lattice declaration>.  
\initials{LDF 2007.07.29.}

\LOG
\initials{LDF 2007.07.29.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=declaration: conic_section_lattice_declaration@>
;


@q *** (3) declaration --> helix declaration.  @>
@*2 \§declaration> $\longrightarrow$ |helix_declaration|.  

\LOG
\initials{LDF 2005.05.18.}
Added this rule.

\initials{LDF 2005.11.07.}
Removed debugging code.
\ENDLOG

@<Define rules@>=
@=declaration: helix_declaration@>
;

@q *** (3) declaration --> triangle_declaration.  @>
@*2 \§declaration> $\longrightarrow$ |triangle_declaration|.  

\LOG
\initials{LDF 2004.12.11.}
Made debugging output thread-safe.
\ENDLOG 

@<Define rules@>=
@=declaration: triangle_declaration@>
{

#if DEBUG_COMPILE
   bool DEBUG = false; /* |true| */ @; 
   if (DEBUG)
      {
         stringstream cerr_strm;
         cerr_strm  
                   << "*** Parser:  'declaration --> triangle_declaration'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str();

     }
#endif /* |DEBUG_COMPILE|  */

};

@q *** (3) declaration --> nurb_declaration.  @>
@*2 \§declaration> $\longrightarrow$ |nurb_declaration|.  
\initials{LDF 2005.01.26.}

\LOG
\initials{LDF 2005.01.26.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=declaration: nurb_declaration@>
{

#if DEBUG_COMPILE
   bool DEBUG = false; /* |true| */ @; 
   if (DEBUG)
      {
         stringstream cerr_strm;
         cerr_strm  
                   << "*** Parser:  'declaration --> nurb_declaration'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str();

     }
#endif /* |DEBUG_COMPILE|  */

};


@q **** (4) declaration --> polygon_declaration.@>
@*3 \§declaration> $\longrightarrow$ \§polygon declaration>.  
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.02.11.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=declaration: polygon_declaration@>
{

#if DEBUG_COMPILE
   bool DEBUG = false; /* |true| */ @; 
   if (DEBUG)
      {
          stringstream cerr_strm;
          cerr_strm  
                    << "*** Parser:  'declaration --> polygon_declaration'.";

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str();
      }
#endif /* |DEBUG_COMPILE|  */

};

@q **** (4) declaration --> reg_polygon_declaration.  @>
@*3 \§declaration> $\longrightarrow$ \§regular polygon declaration>.  

\LOG
\initials{LDF 2004.12.11.}
Made debugging output thread-safe.
\ENDLOG 


@<Define rules@>=
@=declaration: reg_polygon_declaration@>
{
#if DEBUG_COMPILE
   bool DEBUG = false; /* |true| */ @; 
   if (DEBUG)
      {
          stringstream cerr_strm;
          cerr_strm  
                    << "*** Parser:  'declaration --> reg_polygon_declaration'.";

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str();
      }
#endif /* |DEBUG_COMPILE|  */

};


@q **** (4) declaration --> rectangle_declaration.  @>
@*3 \§declaration> $\longrightarrow$ \§rectangle declaration>.

\LOG
\initials{LDF 2004.12.11.}
Made debugging output thread-safe.
\ENDLOG 


@<Define rules@>=
@=declaration: rectangle_declaration@>
{
#if DEBUG_COMPILE
   bool DEBUG = false; /* |true| */ @; 
   if (DEBUG)
      {
          stringstream cerr_strm;
          cerr_strm  
                    << "*** Parser:  'declaration --> rectangle_declaration'.";

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str();
      }
#endif /* |DEBUG_COMPILE|  */

};

@q **** (4) declaration --> square_declaration.  @>
@*3 \§declaration> $\longrightarrow$ \§square declaration>.

@<Define rules@>=
@=declaration: square_declaration@>
{

#if DEBUG_COMPILE
   bool DEBUG = false; /* |true| */ @; 
   if (DEBUG)
      {
         stringstream cerr_strm;
         cerr_strm  
                   << "*** Parser:  'declaration --> square_declaration'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str();
      }
#endif /* |DEBUG_COMPILE|  */

};

@q **** (4) declaration --> ellipse_declaration.  @>
@*3 \§declaration> $\longrightarrow$ \§ellipse declaration>.

\LOG
\initials{LDF 2004.12.11.}
Made debugging output thread-safe.
\ENDLOG 

@<Define rules@>=
@=declaration: ellipse_declaration@>
{

#if DEBUG_COMPILE
   bool DEBUG = false; /* |true| */ @; 
   if (DEBUG)
      {
          stringstream cerr_strm;
          cerr_strm  
                    << "*** Parser:  'declaration --> ellipse_declaration'.";

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str();
      }
#endif /* |DEBUG_COMPILE|  */

};

@q *** (3) declaration --> cone declaration.  @>
@*2 \§declaration> $\longrightarrow$ |cone_declaration|.  
\initials{LDF 2005.05.25.}

\LOG
\initials{LDF 2005.05.25.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=declaration: cone_declaration@>
{

#if DEBUG_COMPILE
   bool DEBUG = false; /* |true| */ @; 
   if (DEBUG)
      {
         stringstream cerr_strm;
         cerr_strm  
                   << "*** Parser:  'declaration --> cone_declaration'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str();

      }
#endif /* |DEBUG_COMPILE|  */

};

@q *** (3) declaration --> cylinder declaration.  @>
@*2 \§declaration> $\longrightarrow$ |cylinder_declaration|.  
\initials{LDF 2005.05.25.}

\LOG
\initials{LDF 2005.05.25.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=declaration: cylinder_declaration@>
{

    
#if DEBUG_COMPILE
   bool DEBUG = false; /* |true| */ @; 
   if (DEBUG)
      {
         stringstream cerr_strm;
         cerr_strm  
                   << "*** Parser:  'declaration --> cylinder_declaration'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str();

      }
#endif /* |DEBUG_COMPILE|  */

};
  
@q **** (4) declaration --> cuboid_declaration.  @>
@*3 \§declaration> $\longrightarrow$ \§cuboid declaration>.

\LOG
\initials{LDF 2004.12.11.}
Made debugging output thread-safe.
\ENDLOG 

@<Define rules@>=
@=declaration: cuboid_declaration@>
{


#if DEBUG_COMPILE
    bool DEBUG = false; /* |true| */ @; 
    if (DEBUG)
       {
           stringstream cerr_strm;
           cerr_strm  
                     << "*** Parser:  'declaration --> cuboid_declaration'.";

           log_message(cerr_strm);
           cerr_message(cerr_strm);
           cerr_strm.str();
       }
#endif /* |DEBUG_COMPILE|  */

};

@q **** (4) declaration --> polyhedron_declaration.  @>
@*3 \§declaration> $\longrightarrow$ \§polyhedron declaration>.

@<Define rules@>=
@=declaration: polyhedron_declaration@>
{

#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
      stringstream cerr_strm;
      cerr_strm  
                << "*** Parser: 'declaration --> polyhedron_declaration'.";

      log_message(cerr_strm);
      cerr_message(cerr_strm);
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */

};

@q *** (3) declaration --> ellipsoid declaration.  @>
@*2 \§declaration> $\longrightarrow$ |ellipsoid_declaration|.  
\initials{LDF 2005.05.26.}

\LOG
\initials{LDF 2005.05.26.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=declaration: ellipsoid_declaration@>
{


#if DEBUG_COMPILE
   bool DEBUG = false; /* |true| */ @; 
   if (DEBUG)
      {
         stringstream cerr_strm;
         cerr_strm  
                   << "*** Parser:  'declaration --> ellipsoid_declaration'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str();

      }
#endif /* |DEBUG_COMPILE|  */

};

@q *** (3) declaration --> sphere declaration.  @>
@*2 \§declaration> $\longrightarrow$ \§sphere declaration>.  
\initials{LDF 2005.06.06.}

\LOG
\initials{LDF 2005.06.06.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=declaration: sphere_declaration@>
{

#if DEBUG_COMPILE
   bool DEBUG = false; /* |true| */ @; 
   if (DEBUG)
      {
         stringstream cerr_strm;
         cerr_strm  
                   << "*** Parser:  'declaration --> sphere_declaration'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str();

      }
#endif /* |DEBUG_COMPILE|  */

};


@q *** (3) declaration --> sphere_development declaration.  @>
@*2 \§declaration> $\longrightarrow$ \%sphere development declaration>.  
\initials{LDF 2009.11.04.}

\LOG
\initials{LDF 2009.11.04.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=declaration: sphere_development_declaration@>
{

#if DEBUG_COMPILE
   bool DEBUG = false; /* |true| */ @; 
   if (DEBUG)
      {
         stringstream cerr_strm;
         cerr_strm << "*** Parser:  'declaration --> sphere_development_declaration'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str();

      }
#endif /* |DEBUG_COMPILE|  */

};

@q *** (3) declaration --> paraboloid declaration.  @>
@*2 \§declaration> $\longrightarrow$ |paraboloid_declaration|.  
\initials{LDF 2006.01.23.}

\LOG
\initials{LDF 2006.01.23.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=declaration: paraboloid_declaration@>
{

#if DEBUG_COMPILE
   bool DEBUG = false; /* |true| */ @; 
   if (DEBUG)
      {
         stringstream cerr_strm;
         cerr_strm  
                   << "*** Parser:  'declaration --> paraboloid_declaration'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str();

      }
#endif /* |DEBUG_COMPILE|  */

};


@q *** (3) declaration --> glyph declaration.  @>
@*2 \§declaration> $\longrightarrow$ |glyph_declaration|.  
\initials{LDF 2005.06.08.}

\LOG
\initials{LDF 2005.06.08.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=declaration: glyph_declaration@>
{
   

#if DEBUG_COMPILE
   bool DEBUG = false; /* |true| */ @; 
   if (DEBUG)
      {
         stringstream cerr_strm;
         cerr_strm  
                   << "*** Parser:  'declaration --> glyph_declaration'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str();

      }
#endif /* |DEBUG_COMPILE|  */

};

@q *** (3) declaration --> plane declaration.  @>
@*2 \§declaration> $\longrightarrow$ |plane_declaration|.  
\initials{LDF 2005.10.30.}

\LOG
\initials{LDF 2005.10.30.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=declaration: plane_declaration@>
{

#if DEBUG_COMPILE
   bool DEBUG = false; /* |true| */ @; 
   if (DEBUG)
      {
         stringstream cerr_strm;
         cerr_strm  
                   << "*** Parser:  'declaration --> plane_declaration'.";

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str();

      }
#endif /* |DEBUG_COMPILE|  */

};



@q **** (4) declaration --> ellipse_slice_declaration.  @>
@*3 \§declaration> $\longrightarrow$ \§ellipse slice declaration>.
\initials{LDF 2005.03.29.}

\LOG
\initials{LDF 2005.03.29.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=declaration: ellipse_slice_declaration@>
{


#if DEBUG_COMPILE
   bool DEBUG = false; /* |true| */ @; 
   if (DEBUG)
      {
          stringstream cerr_strm;
          cerr_strm  
                    << "*** Parser:  'declaration --> ellipse_slice_declaration'.";

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str();
      }
#endif /* |DEBUG_COMPILE|  */

};

@q **** (4) declaration --> circle_slice_declaration.  @>
@*3 \§declaration> $\longrightarrow$ \§circle slice declaration>.
\initials{LDF 2005.03.29.}

\LOG
\initials{LDF 2005.03.29.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=declaration: circle_slice_declaration@>
{
   
#if DEBUG_COMPILE
   bool DEBUG = false; /* |true| */ @; 
   if (DEBUG)
      {
          stringstream cerr_strm;
          cerr_strm  
                    << "*** Parser:  'declaration --> circle_slice_declaration'.";

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str();
      }
#endif /* |DEBUG_COMPILE|  */

};

@q **** (4) declaration --> polyhedron_slice_declaration.  @>
@*3 \§declaration> $\longrightarrow$ \§polyhedron slice declaration>.
\initials{LDF 2005.03.29.}

\LOG
\initials{LDF 2005.03.29.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=declaration: polyhedron_slice_declaration@>
{


#if DEBUG_COMPILE
   bool DEBUG = false; /* |true| */ @; 
   if (DEBUG)
      {
          stringstream cerr_strm;
          cerr_strm  
                    << "*** Parser:  'declaration --> polyhedron_slice_declaration'.";

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str();
      }
#endif /* |DEBUG_COMPILE|  */

};

@q **** (4) declaration --> ellipse_slice_vector_declaration.  @>
@*3 \§declaration> $\longrightarrow$ \§ellipse slice vector declaration>.
\initials{LDF 2005.03.29.}

\LOG
\initials{LDF 2005.03.29.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=declaration: ellipse_slice_vector_declaration@>
{
   
#if DEBUG_COMPILE
   bool DEBUG = false; /* |true| */ @; 
   if (DEBUG)
      {
          stringstream cerr_strm;
          cerr_strm  
                    << "*** Parser:  'declaration --> ellipse_slice_vector_declaration'.";

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str();
      }
#endif /* |DEBUG_COMPILE|  */

};

@q **** (4) declaration --> circle_slice_vector_declaration.  @>
@*3 \§declaration> $\longrightarrow$ \§circle slice vector declaration>.
\initials{LDF 2005.03.29.}

\LOG
\initials{LDF 2005.03.29.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=declaration: circle_slice_vector_declaration@>
{
   
#if DEBUG_COMPILE
   bool DEBUG = false; /* |true| */ @; 
   if (DEBUG)
      {
          stringstream cerr_strm;
          cerr_strm  
                    << "*** Parser:  'declaration --> circle_slice_vector_declaration'.";

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str();
      }
#endif /* |DEBUG_COMPILE|  */

};

@q **** (4) declaration --> polyhedron_slice_vector_declaration.  @>
@*3 \§declaration> $\longrightarrow$ \§polyhedron slice vector declaration>.
\initials{LDF 2005.03.29.}

\LOG
\initials{LDF 2005.03.29.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=declaration: polyhedron_slice_vector_declaration@>
{

#if DEBUG_COMPILE
   bool DEBUG = false; /* |true| */ @; 
   if (DEBUG)
      {
          stringstream cerr_strm;
          cerr_strm  
                    << "*** Parser:  'declaration --> polyhedron_slice_vector_declaration'.";

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str();
      }
#endif /* |DEBUG_COMPILE|  */

};


@q **** (4) declaration --> origami_figure declaration.@>
@*3 \§declaration> $\longrightarrow$ \§origami figure declaration>.
\initials{LDF 2005.02.04.}

\LOG
\initials{LDF 2005.02.04.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=declaration: origami_figure_declaration@>@/
{
   
#if DEBUG_COMPILE
   bool DEBUG = false; /* |true| */ @; 
   if (DEBUG)
      {
          stringstream cerr_strm;
          cerr_strm  
                    << "*** Parser:  declaration --> "
                    << "origami_figure_declaration'.";

          log_message(cerr_strm);
          cerr_message(cerr_strm);
          cerr_strm.str();
      }
#endif /* |DEBUG_COMPILE|  */

};


@q *** (3) Vector-type declarations.@>
@*2 Vector-type declarations.

\LOG
\initials{LDF 2004.12.11.}
Added this section.
\ENDLOG

@q **** (4) Non-|Shape| types.@> 
@*3 Non-{\bf Shape} types.
\initials{LDF 2004.12.12.}

@q ***** (5) declaration --> numeric_vector declaration.  @>
@*4 \§declaration> $\longrightarrow$ \§numeric vector declaration>.
\initials{LDF 2005.01.07.}

\LOG
\initials{LDF 2005.01.07.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: numeric_vector_declaration@>
;

@q ***** (5) declaration --> ulong_long_vector declaration.  @>
@*4 \§declaration> $\longrightarrow$ \§ulong long vector declaration>.
\initials{LDF 2005.12.05.}

\LOG
\initials{LDF 2005.12.05.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: ulong_long_vector_declaration@>
;

@q ***** (5) declaration --> boolean_vector declaration.  @>
@*4 \§declaration> $\longrightarrow$ \§boolean vector declaration>.

\LOG
\initials{LDF 2005.01.07.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: boolean_vector_declaration@>
;

@q ***** (5) declaration --> complex_vector declaration.  @>
@*4 \§declaration> $\longrightarrow$ \§complex vector declaration>.
\initials{LDF 2007.12.02.}

\LOG
\initials{LDF 2007.12.02.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: complex_vector_declaration@>
;

@q ***** (5) declaration --> matrix_vector declaration.  @>
@*4 \§declaration> $\longrightarrow$ \§matrix vector declaration>.

\LOG
\initials{LDF 2007.11.28.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: matrix_vector_declaration@>
;

@q ***** (5) declaration --> string_vector declaration.  @>
@*4 \§declaration> $\longrightarrow$ \§string vector declaration>.
\initials{LDF 2005.01.09.}

\LOG
\initials{LDF 2005.01.09.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: string_vector_declaration@>
;

@q ***** (5) declaration --> pen_vector declaration.  @>
@*4 \§declaration> $\longrightarrow$ \§pen vector declaration>.
\initials{LDF 2005.01.11.}

\LOG
\initials{LDF 2005.01.11.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: pen_vector_declaration@>
;

@q ***** (5) declaration --> dash_pattern_vector declaration.  @>
@*4 \§declaration> $\longrightarrow$ 
\§dash pattern vector declaration>.
\initials{LDF 2005.01.13.}

\LOG
\initials{LDF 2005.01.13.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: dash_pattern_vector_declaration@>
;

@q ***** (5) declaration --> color_vector declaration.  @>
@*4 \§declaration> $\longrightarrow$ \§color vector declaration>.

\LOG
\initials{LDF 2004.08.23.}  
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: color_vector_declaration@>
;

@q ***** (5) declaration --> transform_vector declaration.  @>
@*4 \§declaration> $\longrightarrow$ 
\§transform vector declaration>.
\initials{LDF 2005.01.13.}

\LOG
\initials{LDF 2005.01.13.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: transform_vector_declaration@>
;

@q ***** (5) declaration --> picture_vector declaration.  @>
@*4 \§declaration> $\longrightarrow$ 
\§picture vector declaration>.
\initials{LDF 2005.01.15.}

\LOG
\initials{LDF 2005.01.15.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: picture_vector_declaration@>
;

@q ***** (5) declaration --> focus_vector declaration.  @>
@*4 \§declaration> $\longrightarrow$ 
\§focus vector declaration>.
\initials{LDF 2005.01.17.}

\LOG
\initials{LDF 2005.01.17.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: focus_vector_declaration@>
;


@q ***** (5) declaration --> macro_vector declaration.  @>
@*4 \§declaration> $\longrightarrow$ \§macro vector declaration>.

\LOG
\initials{LDF 2004.12.28.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: macro_vector_declaration@>
;

@q **** (4) |Shape| types.@> 
@*3 {\bf Shape} types.
\initials{LDF 2004.12.12.}


@q ***** (5) declaration --> point_vector declaration.  @>
@*4 \§declaration> $\longrightarrow$ \§point vector declaration>.

\LOG
\initials{LDF 2004.08.31.}  
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: point_vector_declaration@>
{

#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @;
  if (DEBUG)
    {
      stringstream cerr_strm;
      cerr_strm << "*** Parser: declaration --> point_vector_declaration.";

      log_message(cerr_strm); 
      cerr_message(cerr_strm); 
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */

};

@q ***** (5) declaration --> bool_point_vector declaration.  @>
@*4 \§declaration> $\longrightarrow$ \§bool-point vector declaration>.

\LOG
\initials{LDF 2004.09.01.}  
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: bool_point_vector_declaration@>
{

#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @;
  if (DEBUG)
    {
      stringstream cerr_strm;
      cerr_strm << "*** Parser: declaration --> bool_point_vector_declaration.";

      log_message(cerr_strm); 
      cerr_message(cerr_strm); 
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */

};

@q ***** (5) declaration --> path_vector declaration.  @>
@*4 \§declaration> $\longrightarrow$ \§path vector declaration>.

\LOG
\initials{LDF 2004.12.10.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: path_vector_declaration@>
{

#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @;
  if (DEBUG)
    {
      stringstream cerr_strm;
      cerr_strm << "*** Parser: declaration --> path_vector_declaration.";

      log_message(cerr_strm); 
      cerr_message(cerr_strm); 
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */

};


@q ***** (5) declaration --> ellipse_vector declaration.  @>
@*4 \§declaration> $\longrightarrow$ \§ellipse vector declaration>.

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: ellipse_vector_declaration@>
{

#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @;
  if (DEBUG)
    {
      stringstream cerr_strm;
      cerr_strm << "*** Parser: declaration --> ellipse_vector_declaration.";

      log_message(cerr_strm); 
      cerr_message(cerr_strm); 
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */

};


@q ***** (5) declaration --> circle_vector declaration.  @>
@*4 \§declaration> $\longrightarrow$ \§circle vector declaration>.

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: circle_vector_declaration@>
{

#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @;
  if (DEBUG)
    {
      stringstream cerr_strm;
      cerr_strm << "*** Parser: declaration --> circle_vector_declaration.";

      log_message(cerr_strm); 
      cerr_message(cerr_strm); 
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */

};

@q ***** (5) declaration --> parabola_vector declaration.  @>
@*4 \§declaration> $\longrightarrow$ \§parabola vector declaration>.
\initials{LDF 2005.11.07.}

\LOG
\initials{LDF 2005.11.07.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: parabola_vector_declaration@>
{
};

@q ***** (5) declaration --> hyperbola_vector declaration.  @>
@*4 \§declaration> $\longrightarrow$ \§hyperbola vector declaration>.
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: hyperbola_vector_declaration@>
{
};

@q ***** (5) declaration --> arc_vector declaration.  @>
@*4 \§declaration> $\longrightarrow$ \§arc vector declaration>.
\initials{LDF 2007.10.13.}

\LOG
\initials{LDF 2007.10.13.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: arc_vector_declaration@>
{
};



@q ***** (5) declaration --> conic_section_lattice_vector declaration.  @>
@*4 \§declaration> $\longrightarrow$ \§conic section lattice vector declaration>.
\initials{LDF 2007.07.29.}

\LOG
\initials{LDF 2007.07.29.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: conic_section_lattice_vector_declaration@>
{
};


@q ***** (5) declaration --> helix_vector declaration.  @>
@*4 \§declaration> $\longrightarrow$ \§helix vector declaration>.

\LOG
\initials{LDF 2005.05.18.}
Added this rule.

\initials{LDF 2005.11.07.}
Removed debugging code.
\ENDLOG 

@<Define rules@>=
@=declaration: helix_vector_declaration@>
{
};

@q ***** (5) declaration --> triangle_vector declaration.  @>
@*4 \§declaration> $\longrightarrow$ \§triangle vector declaration>.
\initials{LDF 2005.01.25.}

\LOG
\initials{LDF 2005.01.25.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: triangle_vector_declaration@>
{

#if DEBUG_COMPILE
   bool DEBUG = false; /* |true| */ @;
   if (DEBUG)
      {
          stringstream cerr_strm;
          cerr_strm << "*** Parser:  declaration --> triangle_vector_declaration.";

          log_message(cerr_strm); 
          cerr_message(cerr_strm); 
          cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */

};

@q ***** (5) declaration --> nurb_vector declaration.  @>
@*4 \§declaration> $\longrightarrow$ \§nurb vector declaration>.
\initials{LDF 2005.01.26.}

\LOG
\initials{LDF 2005.01.26.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: nurb_vector_declaration@>
{

#if DEBUG_COMPILE
   bool DEBUG = false; /* |true| */ @;
   if (DEBUG)
      {
          stringstream cerr_strm;
          cerr_strm << "*** Parser:  declaration --> nurb_vector_declaration.";

          log_message(cerr_strm); 
          cerr_message(cerr_strm); 
          cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */

};



@q ***** (5) declaration --> rectangle_vector declaration.  @>
@*4 \§declaration> $\longrightarrow$ \§rectangle vector declaration>.

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: rectangle_vector_declaration@>
{

#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @;
  if (DEBUG)
    {
      stringstream cerr_strm;
      cerr_strm << "*** Parser: declaration --> rectangle_vector_declaration.";

      log_message(cerr_strm); 
      cerr_message(cerr_strm); 
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */

};

@q ***** (5) declaration --> polygon_vector declaration.  @>
@*4 \§declaration> $\longrightarrow$ \§polygon vector declaration>.
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.02.11.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: polygon_vector_declaration@>
{

#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @;
  if (DEBUG)
    {
      stringstream cerr_strm;
      cerr_strm  
                << "*** Parser:  'declaration --> polygon_vector_declaration'.";

      log_message(cerr_strm); 
      cerr_message(cerr_strm); 
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */

};


@q ***** (5) declaration --> reg_polygon_vector declaration.  @>
@*4 \§declaration> $\longrightarrow$ \§regular polygon vector declaration>.

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: reg_polygon_vector_declaration@>
{

#if DEBUG_COMPILE
   bool DEBUG = false; /* |true| */ @;
   if (DEBUG)
      {
          stringstream cerr_strm;
          cerr_strm  
                    << "*** Parser:  'declaration --> "
                    << "reg_polygon_vector_declaration'.";

          log_message(cerr_strm); 
          cerr_message(cerr_strm); 
          cerr_strm.str("");
      }
#endif /* |DEBUG_COMPILE|  */

};

@q ***** (5) declaration --> cone_vector declaration.  @>
@*4 \§declaration> $\longrightarrow$ \§cone vector declaration>.
\initials{LDF 2005.05.25.}

\LOG
\initials{LDF 2005.05.25.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: cone_vector_declaration@>
{

#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @;
  if (DEBUG)
    {
      stringstream cerr_strm;
      cerr_strm << "*** Parser:  'declaration --> cone_vector_declaration'.";

      log_message(cerr_strm); 
      cerr_message(cerr_strm); 
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */

};

@q ***** (5) declaration --> cylinder_vector declaration.  @>
@*4 \§declaration> $\longrightarrow$ \§cylinder vector declaration>.
\initials{LDF 2005.05.25.}

\LOG
\initials{LDF 2005.05.25.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: cylinder_vector_declaration@>
{

#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @;
  if (DEBUG)
    {
      stringstream cerr_strm;
      cerr_strm << "*** Parser:  'declaration --> cylinder_vector_declaration'.";

      log_message(cerr_strm); 
      cerr_message(cerr_strm); 
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */

};



@q ***** (5) declaration --> cuboid_vector declaration.  @>
@*4 \§declaration> $\longrightarrow$ \§cuboid vector declaration>.

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: cuboid_vector_declaration@>
{

#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @;
  stringstream cerr_strm;
  if (DEBUG)
    {
      cerr_strm << "*** Parser: declaration --> cuboid_vector_declaration.";

      log_message(cerr_strm); 
      cerr_message(cerr_strm); 
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */

};

@q ***** (5) declaration --> polyhedron_vector declaration.  @>
@*4 \§declaration> $\longrightarrow$ \§polyhedron vector declaration>.
\initials{LDF 2005.01.14.}

\LOG
\initials{LDF 2005.01.14.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: polyhedron_vector_declaration@>
{

#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @;
  if (DEBUG)
    {
      stringstream cerr_strm;
      cerr_strm << "*** Parser: declaration --> polyhedron_vector_declaration.";

      log_message(cerr_strm); 
      cerr_message(cerr_strm); 
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */

};

@q ***** (5) declaration --> ellipsoid_vector declaration.  @>
@*4 \§declaration> $\longrightarrow$ \§ellipsoid vector declaration>.
\initials{LDF 2005.05.26.}

\LOG
\initials{LDF 2005.05.26.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: ellipsoid_vector_declaration@>
{

#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @;
  if (DEBUG)
    {
      stringstream cerr_strm;
      cerr_strm << "*** Parser:  'declaration --> ellipsoid_vector_declaration'.";

      log_message(cerr_strm); 
      cerr_message(cerr_strm); 
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */

};

@q ***** (5) declaration --> sphere_vector declaration.  @>
@*4 \§declaration> $\longrightarrow$ \§sphere vector declaration>.
\initials{LDF 2005.06.06.}

\LOG
\initials{LDF 2005.06.06.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: sphere_vector_declaration@>
;


@q ***** (5) declaration --> sphere_development_vector declaration.  @>
@*4 \§declaration> $\longrightarrow$ \§sphere development vector declaration>.
\initials{LDF 2009.11.04.}

\LOG
\initials{LDF 2009.11.04.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: sphere_development_vector_declaration@>
;

@q ***** (5) declaration --> paraboloid_vector declaration.  @>
@*4 \§declaration> $\longrightarrow$ \§paraboloid vector declaration>.
\initials{LDF 2006.01.23.}

\LOG
\initials{LDF 2006.01.23.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: paraboloid_vector_declaration@>
;

@q ***** (5) declaration --> glyph_vector declaration.  @>
@*4 \§declaration> $\longrightarrow$ \§glyph vector declaration>.
\initials{LDF 2005.06.08.}

\LOG
\initials{LDF 2005.06.08.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: glyph_vector_declaration@>
{

#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @;
  if (DEBUG)
    {
      stringstream cerr_strm;
      cerr_strm << "*** Parser:  'declaration --> glyph_vector_declaration'.";

      log_message(cerr_strm); 
      cerr_message(cerr_strm); 
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */

};


@q ***** (5) declaration --> plane_vector declaration.  @>
@*4 \§declaration> $\longrightarrow$ \§plane vector declaration>.
\initials{LDF 2005.10.30.}

\LOG
\initials{LDF 2005.10.30.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: plane_vector_declaration@>
{

#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @;
  if (DEBUG)
    {
      stringstream cerr_strm;
      cerr_strm << "*** Parser:  'declaration --> plane_vector_declaration'.";

      log_message(cerr_strm); 
      cerr_message(cerr_strm); 
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */

};



@q ***** (5) declaration --> origami_figure_vector declaration.  @>
@*4 \§declaration> $\longrightarrow$ 
\§origami figure vector declaration>.
\initials{LDF 2005.02.04.}

\LOG
\initials{LDF 2005.02.04.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=declaration: origami_figure_vector_declaration@>
{

#if DEBUG_COMPILE
   bool DEBUG = false; /* |true| */ @;
   if (DEBUG)
     {
         stringstream cerr_strm;
         cerr_strm  
                   << "*** Parser: declaration --> "
                   << "origami_figure_vector_declaration.";

         log_message(cerr_strm); 
         cerr_message(cerr_strm); 
         cerr_strm.str("");
     }
#endif /* |DEBUG_COMPILE|  */

};



@q *** (3) Type declarations.  @>
@*2 Type declarations.


@q **** (4) Non-vector types.@> 
@*3 Non-vector types. 
\initials{LDF 2004.12.12.}

\LOG
\initials{LDF 2004.12.12.}
Added this section.
\ENDLOG

@q ***** (5) Non-|Shape| types.@> 

@*4 Non-{\bf Shape} types.
\initials{LDF 2004.12.12.}

\LOG
\initials{LDF 2004.12.12.}
Added this section.
\ENDLOG

@q ***** (5) boolean_declaration.  @>
@*4 \§boolean declaration>.  
\initials{LDF 2004.05.17.}  

\LOG
\initials{LDF 2004.05.17.}  
Added this rule.
\ENDLOG 

@<Define rules@>=
@=boolean_declaration: BOOLEAN_DECLARATOR declaration_list@>
{
    stringstream cerr_strm;
    Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter); 
    
   string thread_name;

#ifdef HAVE_PTHREAD_H  
   if (   scanner_node != static_cast<Scanner_Node>(0) 
       && scanner_node->thread_info != static_cast<Thread_Info_Type*>(0)
       && (   scanner_node->run_state.multithread_input 
           || scanner_node->run_state.multithread_output))      

      thread_name = scanner_node->thread_info->name;

#else /* |HAVE_PTHREAD_H| is undefined.  */@; 

   if (0)  /* Dummy conditional.  */
      ;

#endif /* |HAVE_PTHREAD_H| is undefined.  */@; 

   else  /* Threads not available or not using them.  */

      thread_name = "";


   bool error_stop_value = (      scanner_node->run_state.error_stop_mode
                         == Run_State::STOPPING
                      && scanner_node->in->type != Io_Struct::STDIN_TYPE)
                      
                      ? true : false;

#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
      cerr_strm  << "*** Parser:  'boolean_declaration -->"
                << endl << "BOOLEAN_DECLARATOR declaration_list'.";

      log_message(cerr_strm); 
      cerr_message(cerr_strm); 
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */

  int return_value
    = Scan_Parse::non_shape_decl_func(scanner_node, BOOLEAN);

#if DEBUG_COMPILE
  if (DEBUG)
    {
      cerr_strm  << "*** Parser: boolean_declaration --> : "
                << endl << "   BOOLEAN_DECLARATOR declaration_list:"
                << endl << "   'return_value' == " << return_value << ".";
      cerr_message(cerr_strm); 
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */@; 

  if (return_value != 0)
    {
      cerr_strm  << "ERROR! In parser:" << endl 
                << "Rule \"boolean_declaration --> : "
                << "BOOLEAN_DECLARATOR 'declaration_list\":"
                << endl << "'Scan_Parse::non_shape_decl_func()' "
                << "returned " << return_value << "."
                << endl << "Will try to continue.";
      cerr_message(cerr_strm, error_stop_value); 
      cerr_strm.str("");
    }

};

@q ***** (5) complex_declaration.  @>
@*4 \§complex declaration>.  
\initials{LDF 2007.12.02.}

\LOG
\initials{LDF 2007.12.02.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=complex_declaration: COMPLEX_DECLARATOR declaration_list@>
{
    stringstream cerr_strm;
    Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter); 
    
   string thread_name;

#ifdef HAVE_PTHREAD_H  
   if (   scanner_node != static_cast<Scanner_Node>(0) 
       && scanner_node->thread_info != static_cast<Thread_Info_Type*>(0)
       && (   scanner_node->run_state.multithread_input 
           || scanner_node->run_state.multithread_output))      

      thread_name = scanner_node->thread_info->name;

#else /* |HAVE_PTHREAD_H| is undefined.  */@; 

   if (0)  /* Dummy conditional.  */
      ;

#endif /* |HAVE_PTHREAD_H| is undefined.  */@; 

   else  /* Threads not available or not using them.  */

      thread_name = "";


   bool error_stop_value = (      scanner_node->run_state.error_stop_mode
                         == Run_State::STOPPING
                      && scanner_node->in->type != Io_Struct::STDIN_TYPE)
                      
                      ? true : false;

#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
      cerr_strm  << "*** Parser:  'complex_declaration -->"
                << endl << "COMPLEX_DECLARATOR declaration_list'.";

      log_message(cerr_strm); 
      cerr_message(cerr_strm); 
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */

  int return_value
    = Scan_Parse::non_shape_decl_func(scanner_node, COMPLEX);

#if DEBUG_COMPILE
  if (DEBUG)
    {
      cerr_strm  << "*** Parser: complex_declaration --> : "
                << endl << "   COMPLEX_DECLARATOR declaration_list:"
                << endl << "   'return_value' == " << return_value << ".";
      cerr_message(cerr_strm); 
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */@; 

  if (return_value != 0)
    {
      cerr_strm  << "ERROR! In parser:" << endl 
                << "Rule \"complex_declaration --> : "
                << "COMPLEX_DECLARATOR 'declaration_list\":"
                << endl << "'Scan_Parse::non_shape_decl_func()' "
                << "returned " << return_value << "."
                << endl << "Will try to continue.";
      cerr_message(cerr_strm, error_stop_value); 
      cerr_strm.str("");
    }

};

@q ***** (5) matrix_declaration.  @>
@*4 \§matrix declaration>.  
\initials{LDF 2007.11.28.}

\LOG
\initials{LDF 2007.11.28.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=matrix_declaration: MATRIX_DECLARATOR declaration_list@>
{
    stringstream cerr_strm;
    Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter); 
    
   string thread_name;

#ifdef HAVE_PTHREAD_H  
   if (   scanner_node != static_cast<Scanner_Node>(0) 
       && scanner_node->thread_info != static_cast<Thread_Info_Type*>(0)
       && (   scanner_node->run_state.multithread_input 
           || scanner_node->run_state.multithread_output))      

      thread_name = scanner_node->thread_info->name;

#else /* |HAVE_PTHREAD_H| is undefined.  */@; 

   if (0)  /* Dummy conditional.  */
      ;

#endif /* |HAVE_PTHREAD_H| is undefined.  */@; 

   else  /* Threads not available or not using them.  */

      thread_name = "";


   bool error_stop_value = (      scanner_node->run_state.error_stop_mode
                         == Run_State::STOPPING
                      && scanner_node->in->type != Io_Struct::STDIN_TYPE)
                      
                      ? true : false;

#if DEBUG_COMPILE
  bool DEBUG = false; /* |true| */ @; 
  if (DEBUG)
    {
      cerr_strm  << "*** Parser:  'matrix_declaration -->"
                << endl << "MATRIX_DECLARATOR declaration_list'.";

      log_message(cerr_strm); 
      cerr_message(cerr_strm); 
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */

  int return_value
    = Scan_Parse::non_shape_decl_func(scanner_node, MATRIX);

#if DEBUG_COMPILE
  if (DEBUG)
    {
      cerr_strm  << "*** Parser: matrix_declaration --> : "
                << endl << "   MATRIX_DECLARATOR declaration_list:"
                << endl << "   'return_value' == " << return_value << ".";
      cerr_message(cerr_strm); 
      cerr_strm.str("");
    }
#endif /* |DEBUG_COMPILE|  */@; 

  if (return_value != 0)
    {
      cerr_strm  << "ERROR! In parser:" << endl 
                << "Rule \"matrix_declaration --> : "
                << "MATRIX_DECLARATOR 'declaration_list\":"
                << endl << "'Scan_Parse::non_shape_decl_func()' "
                << "returned " << return_value << "."
                << endl << "Will try to continue.";
      cerr_message(cerr_strm, error_stop_value); 
      cerr_strm.str("");
    }

};


@q ***** (5) string_declaration.  @>
@*4 \§string declaration>.
\initials{LDF 2004.05.17.}  

\LOG
\initials{LDF 2004.05.17.}  
Added this rule.
\ENDLOG 


@<Define rules@>=
@=string_declaration: STRING_DECLARATOR declaration_list@>
{
    Scan_Parse::non_shape_decl_func(static_cast<Scanner_Node>(parameter), STRING);
};

@q ***** (5) pen_declaration.  @>
@*4 \§pen declaration>.  
\initials{LDF 2004.05.21.}  

\LOG
\initials{LDF 2004.05.21.}  
Added this rule.
\ENDLOG 


@<Define rules@>=
@=pen_declaration: PEN_DECLARATOR declaration_list@>
{
   Scan_Parse::non_shape_decl_func(static_cast<Scanner_Node>(parameter), PEN);
};

@q **** (4) dash_pattern_declaration.  @>
@ {\bf dash\_pattern\_declaration}.  
@q { @>
@:dash_pattern_declaration}{{\bf dash\_pattern\_declaration}@>
@q } @>

\LOG
\initials{LDF 2004.06.07.}  Added this rule.
\ENDLOG 


@<Define rules@>=
@=dash_pattern_declaration: DASH_PATTERN_DECLARATOR declaration_list@>
{
   Scan_Parse::non_shape_decl_func(static_cast<Scanner_Node>(parameter), DASH_PATTERN);
};

@q **** (4) focus_declaration.  @>
@*3 \§focus declaration>.  
\initials{LDF 2004.06.07.}  

\LOG
\initials{LDF 2004.06.07.}  
Added this rule.
\ENDLOG 


@<Define rules@>=
@=focus_declaration: FOCUS_DECLARATOR declaration_list@>
{
    Scan_Parse::non_shape_decl_func(static_cast<Scanner_Node>(parameter), FOCUS);
};

@q **** (4) macro_declaration.  @>
@*3 \§macro declaration>.  
\initials{LDF 2004.12.28.}

\LOG
\initials{LDF 2004.12.28.}
Added this rule.
\ENDLOG 


@<Define rules@>=
@=macro_declaration: MACRO_DECLARATOR declaration_list@>
{
    Scan_Parse::non_shape_decl_func(static_cast<Scanner_Node>(parameter), MACRO);
};


@q **** (4) color_declaration --> COLOR_DECLARATOR declaration_list.  @>
@ \§color declaration> $\longrightarrow$ \.{COLOR\_DECLARATOR} 
\§declaration list>.

\LOG
\initials{LDF 2004.05.25.}  Added this rule.
\ENDLOG 


@<Define rules@>=
@=color_declaration: COLOR_DECLARATOR declaration_list@>
{
    Scan_Parse::non_shape_decl_func(static_cast<Scanner_Node>(parameter), COLOR);
};

@q **** (4) numeric_declaration.  @>
@*3 \§numeric declaration>.

@:BUG FIX@> BUG FIX:  Now calling 
|scanner_node->token_string_bool_vector.clear()| 
at the end of this rule.  Not doing this caused entries in 
|scanner_node->id_map_node->id_map| to be overwritten.
\initials{LDF 2004.04.30.}

@<Define rules@>=
@=numeric_declaration: NUMERIC_DECLARATOR declaration_list@>
{
    Scan_Parse::non_shape_decl_func(static_cast<Scanner_Node>(parameter), NUMERIC);

};

@q **** (4) ulong_long_declaration.  @>
@*3 \§ulong long declaration>.
\initials{LDF 2005.12.05.}

\LOG
\initials{LDF 2005.12.05.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=ulong_long_declaration: ULONG_LONG_DECLARATOR declaration_list@>
{
    Scan_Parse::non_shape_decl_func(static_cast<Scanner_Node>(parameter), ULONG_LONG);

};

@q **** (4) transform_declaration.  @>
@ {\bf transform\_declaration}.  
@q { @>
@:transform_declaration}{{\bf transform\_declaration}@>
@q } @>

\LOG
\initials{LDF 2004.05.09.}  This rule now works.  Formerly, it only contained
dummy code.
\ENDLOG 

@<Define rules@>=
@=transform_declaration: TRANSFORM_DECLARATOR declaration_list@>
{
   Scan_Parse::non_shape_decl_func(static_cast<Scanner_Node>(parameter), TRANSFORM);
};

@q **** (4) picture_declaration.  @>
@ {\bf picture\_declaration}.  
@q { @>
@:picture_declaration}{{\bf picture\_declaration}@>
@q } @>

\LOG
\initials{LDF 2004.05.21.}  Added this rule.
\ENDLOG 


@<Define rules@>=
@=picture_declaration: PICTURE_DECLARATOR declaration_list@>
{
    Scan_Parse::non_shape_decl_func(static_cast<Scanner_Node>(parameter), PICTURE);
};

@q **** (4) Shape declarations.  @>
@ \§Shape> declarations.
\initials{LDF Undated.}

@q ***** (5) point_declaration.  @>
@*4 \§point declaration>.
\initials{LDF Undated.}

\LOG
\initials{LDF 2004.07.22.}
Now calling |shape_decl_func<Point>()| instead of including
|@<Common code for |Shape| declarations@>|.  About to change the other rules to
use |shape_decl_func()|, too.
\ENDLOG 

@<Define rules@>=
@=point_declaration: POINT_DECLARATOR declaration_list@>
{
   shape_decl_func<Point>(static_cast<Scanner_Node>(parameter), POINT);
};

@q ***** (5) bool_point_declaration --> BOOL_POINT_DECLARATOR declaration_list.@>

@*4 \§bool-point declaration> $\longrightarrow$ 
\.{BOOL\_POINT\_DECLARATOR} \§declaration list>.
\initials{LDF 2004.09.01.}  

\LOG
\initials{LDF 2004.09.01.}  
Added this rule.

\initials{LDF 2004.09.01.}
Added debugging code.
\ENDLOG 

@<Define rules@>=
@=bool_point_declaration: BOOL_POINT_DECLARATOR declaration_list@>
{
   Scan_Parse::non_shape_decl_func(static_cast<Scanner_Node>(parameter), BOOL_POINT);
};


@q ***** (5) path_declaration.  @>
@ {\bf path\_declaration}.
@q { @>
@:path_declaration}{{\bf path\_declaration}@>
@q } @>
@<Define rules@>=
@=path_declaration: PATH_DECLARATOR declaration_list@>
{
   shape_decl_func<Path>(static_cast<Scanner_Node>(parameter), PATH);
};

@q ***** (5) triangle_declaration.  @>
@*4 \§triangle declaration>.
\initials{LDF 2005.01.20.}

\LOG
\initials{LDF 2005.01.20.}
Started working on this rule.
Previously, it had a dummy definition that 
signalled an error.
\ENDLOG 

@<Define rules@>=
@=triangle_declaration: TRIANGLE_DECLARATOR declaration_list@>
{
   shape_decl_func<Triangle>(static_cast<Scanner_Node>(parameter), TRIANGLE);

};

@q ***** (5) nurb_declaration.  @>
@*4 \§nurb declaration>.
\initials{LDF 2005.01.26.}

\LOG
\initials{LDF 2005.01.26.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=nurb_declaration: NURB_DECLARATOR declaration_list@>
{
   shape_decl_func<Nurb>(static_cast<Scanner_Node>(parameter), NURB);
};

@q ***** (5) polygon_declaration.  @>

@*4 \§polygon declaration>.
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.02.11.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=polygon_declaration: POLYGON_DECLARATOR declaration_list@>
{
   shape_decl_func<Polygon>(static_cast<Scanner_Node>(parameter), POLYGON);
};


@q ***** (5) reg_polygon_declaration.  @>

@*4 \§regular polygon declaration>.
\initials{LDF Undated.}

@<Define rules@>=
@=reg_polygon_declaration: REG_POLYGON_DECLARATOR declaration_list@>
{
  shape_decl_func<Reg_Polygon>(static_cast<Scanner_Node>(parameter), REG_POLYGON);
};

@q ***** (5) rectangle_declaration.  @>
@*4 \§rectangle declaration>.
\initials{LDF Undated.}

@<Define rules@>=
@=rectangle_declaration: RECTANGLE_DECLARATOR declaration_list@>
{
  shape_decl_func<Rectangle>(static_cast<Scanner_Node>(parameter), RECTANGLE);
};

@q ***** (5) square_declaration.  @>
@*4 \§square declaration>.

\TODO
@q { @>
@:TO DO}{{\bf TO DO}@>
@q } @> 
!! TO DO:  Program this case.  \initials{LDF 2004.04.23.} 
\ENDTODO  


@<Define rules@>=
@=square_declaration: SQUARE_DECLARATOR declaration_list@>
{
#if 0 
  shape_decl_func<Square>(static_cast<Scanner_Node>(parameter), SQUARE);
#endif 
};

@q ***** (5) ellipse_declaration.  @>
@ {\bf ellipse\_declaration}.

@<Define rules@>=

@=ellipse_declaration: ELLIPSE_DECLARATOR declaration_list@>
{
   shape_decl_func<Ellipse>(static_cast<Scanner_Node>(parameter), ELLIPSE);
};

@q ***** (5) circle_declaration.  @>
@ {\bf circle\_declaration}.
\initials{LDF Undated.}

@<Define rules@>=
@=circle_declaration: CIRCLE_DECLARATOR declaration_list@>
{
    shape_decl_func<Circle>(static_cast<Scanner_Node>(parameter), CIRCLE);
};

@q ***** (5) parabola_declaration.  @>
@*4 \§parabola declaration>.
\initials{LDF 2005.11.07.}

\LOG
\initials{LDF 2005.11.07.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=parabola_declaration: PARABOLA_DECLARATOR declaration_list@>
{

     Scan_Parse::shape_decl_func<Parabola>(static_cast<Scanner_Node>(parameter), PARABOLA);

};

@q ***** (5) hyperbola_declaration.  @>
@*4 \§hyperbola declaration>.
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=hyperbola_declaration: HYPERBOLA_DECLARATOR declaration_list@>
{

     Scan_Parse::shape_decl_func<Hyperbola>(static_cast<Scanner_Node>(parameter), HYPERBOLA);

};

@q ***** (5) arc_declaration.  @>
@*4 \§arc declaration>.
\initials{LDF 2007.10.13.}

\LOG
\initials{LDF 2007.10.13.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=arc_declaration: ARC_DECLARATOR declaration_list@>
{

     Scan_Parse::shape_decl_func<Arc>(static_cast<Scanner_Node>(parameter), ARC);

};

@q ***** (5) conic_section_lattice_declaration.  @>
@*4 \§conic section lattice declaration>.
\initials{LDF 2007.07.29.}

\LOG
\initials{LDF 2007.07.29.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=conic_section_lattice_declaration: CONIC_SECTION_LATTICE_DECLARATOR declaration_list@>
{

     Scan_Parse::non_shape_decl_func(static_cast<Scanner_Node>(parameter), 
                                     CONIC_SECTION_LATTICE);

};

@q ***** (5) helix_declaration.  @>
@*4 {\bf helix\_declaration}.

\LOG
\initials{LDF 2005.05.18.}
Added this rule.

\initials{LDF 2005.11.07.}
Removed debugging code.
\ENDLOG

@<Define rules@>=
@=helix_declaration: HELIX_DECLARATOR declaration_list@>
{

     Scan_Parse::shape_decl_func<Helix>(static_cast<Scanner_Node>(parameter), HELIX);
 
};


@q ***** (5) cone_declaration.  @>
@*4 {\bf cone\_declaration}.
\initials{LDF 2005.05.25.}

\LOG
\initials{LDF 2005.05.25.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=cone_declaration: CONE_DECLARATOR declaration_list@>
{
  shape_decl_func<Cone>(static_cast<Scanner_Node>(parameter), CONE);
};

@q ***** (5) cylinder_declaration.  @>
@*4 {\bf cylinder\_declaration}.
\initials{LDF 2005.05.25.}

\LOG
\initials{LDF 2005.05.25.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=cylinder_declaration: CYLINDER_DECLARATOR declaration_list@>
{

#if DEBUG_COMPILE
    stringstream cerr_strm;
    bool DEBUG = false;  /* |true|  */
#endif 

    int status;
    Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

#if DEBUG_COMPILE
   if (DEBUG)
     {
         cerr_strm  << "*** Parser: cylinder_declaration --> "
                   << "CYLINDER_DECLARATOR declaration_list.";

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str("");

     }
#endif /* |DEBUG_COMPILE|  */@; 

  status = Scan_Parse::shape_decl_func<Cylinder>(scanner_node, CYLINDER);

#if DEBUG_COMPILE
   if (DEBUG)
     {
         cerr_strm  << "In 'yyparse()', "
                   << "rule 'cylinder_declaration: --> "
                   << "CYLINDER_DECLARATOR declaration_list':"
                   << endl << "   'shape_decl_func()' returned "
                   << status << ".";

         log_message(cerr_strm);
         cerr_message(cerr_strm);
         cerr_strm.str("");

     }
#endif /* |DEBUG_COMPILE|  */@; 

};

@q ***** (5) cuboid_declaration.  @>
@ {\bf cuboid\_declaration}.
@q { @>
@:cuboid_declaration}{{\bf cuboid\_declaration}@>
@q } @>
@<Define rules@>=
@=cuboid_declaration: CUBOID_DECLARATOR declaration_list@>
{
    shape_decl_func<Cuboid>(static_cast<Scanner_Node>(parameter), CUBOID);
};

@q ***** (5) polyhedron_declaration.  @>
@ {\bf polyhedron\_declaration}.
@q { @>
@:polyhedron_declaration}{{\bf polyhedron\_declaration}@>
@q } @>
@<Define rules@>=
@=polyhedron_declaration: POLYHEDRON_DECLARATOR declaration_list@>
{
  shape_decl_func<Polyhedron>(static_cast<Scanner_Node>(parameter), POLYHEDRON);
};

@q ***** (5) ellipsoid_declaration.  @>
@*4 {\bf ellipsoid\_declaration}.
\initials{LDF 2005.05.26.}

\LOG
\initials{LDF 2005.05.26.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=ellipsoid_declaration: ELLIPSOID_DECLARATOR declaration_list@>
{
  shape_decl_func<Ellipsoid>(static_cast<Scanner_Node>(parameter), ELLIPSOID);
};

@q ***** (5) sphere_declaration.  @>
@*4 {\bf sphere\_declaration}.
\initials{LDF 2005.06.06.}

\LOG
\initials{LDF 2005.06.06.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=sphere_declaration: SPHERE_DECLARATOR declaration_list@>
{

    shape_decl_func<Sphere>(static_cast<Scanner_Node>(parameter), SPHERE);

};

@q ***** (5) sphere_development_declaration.  @>
@*4 {\bf sphere\_development\_declaration}.
\initials{LDF 2009.11.04.}

\LOG
\initials{LDF 2009.11.04.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=sphere_development_declaration: SPHERE_DEVELOPMENT_DECLARATOR declaration_list@>
{

    shape_decl_func<Sphere_Development>(static_cast<Scanner_Node>(parameter), 
                                        SPHERE_DEVELOPMENT);

};

@q ***** (5) paraboloid_declaration.  @>
@*4 {\bf paraboloid\_declaration}.
\initials{LDF 2006.01.23.}

\LOG
\initials{LDF 2006.01.23.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=paraboloid_declaration: PARABOLOID_DECLARATOR declaration_list@>
{

    shape_decl_func<Paraboloid>(static_cast<Scanner_Node>(parameter), PARABOLOID);

};


@q ***** (5) glyph_declaration.  @>
@*4 {\bf glyph\_declaration}.
\initials{LDF 2005.06.08.}

\LOG
\initials{LDF 2005.06.08.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=glyph_declaration: GLYPH_DECLARATOR declaration_list@>
{
  shape_decl_func<Glyph>(static_cast<Scanner_Node>(parameter), GLYPH);
};


@q ***** (5) plane_declaration.  @>
@*4 {\bf plane\_declaration}.
\initials{LDF 2005.10.30.}

\LOG
\initials{LDF 2005.10.30.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=plane_declaration: PLANE_DECLARATOR declaration_list@>
{
  shape_decl_func<Plane>(static_cast<Scanner_Node>(parameter), PLANE);
};

@q ***** (5) ellipse_slice_declaration.  @>
@*4 {\bf ellipse\_slice\_declaration}.
\initials{LDF 2005.03.29.}

\LOG
\initials{LDF 2005.03.29.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=ellipse_slice_declaration: ELLIPSE_SLICE_DECLARATOR declaration_list@>
{
   shape_decl_func<Ellipse_Slice>(static_cast<Scanner_Node>(parameter), ELLIPSE_SLICE);
  
};

@q ***** (5) circle_slice_declaration.  @>
@*4 {\bf circle\_slice\_declaration}.
\initials{LDF 2005.03.29.}

\LOG
\initials{LDF 2005.03.29.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=circle_slice_declaration: CIRCLE_SLICE_DECLARATOR declaration_list@>
{
   shape_decl_func<Circle_Slice>(static_cast<Scanner_Node>(parameter), CIRCLE_SLICE);
};

@q ***** (5) polyhedron_slice_declaration.  @>
@*4 {\bf polyhedron\_slice\_declaration}.
\initials{LDF 2005.03.29.}

\LOG
\initials{LDF 2005.03.29.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=polyhedron_slice_declaration: POLYHEDRON_SLICE_DECLARATOR declaration_list@>
{
   shape_decl_func<Polyhedron_Slice>(static_cast<Scanner_Node>(parameter), POLYHEDRON_SLICE);
};

@q ***** (5) origami_figure_declaration -->             @>
@q ***** (5) ORIGAMI_FIGURE_DECLARATOR declaration_list.@>

@*4 \§origami figure declaration> $\longrightarrow$ 
\.{ORIGAMI\_FIGURE\_DECLARATOR} \§declaration list>.
\initials{LDF 2005.02.04.}

\LOG
\initials{LDF 2005.02.04.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=origami_figure_declaration: ORIGAMI_FIGURE_DECLARATOR declaration_list@>@/
{
   non_shape_decl_func(static_cast<Scanner_Node>(parameter), ORIGAMI_FIGURE);
};

@q **** (4) Vector-types.@> 
@*3 Vector-types.
\initials{LDF 2004.12.12.}

\LOG
\initials{LDF 2004.12.12.}
Added this section.
\ENDLOG

@q ***** (5) Non-|Shape| types.@> 
@*4  Non-{\bf Shape} types.
\initials{LDF 2004.12.12.}

\LOG
\initials{LDF 2004.12.12.}
Added this section.
\ENDLOG

@q ****** (6) boolean_vector_declaration -->               @>
@q ****** (6) BOOLEAN_VECTOR_DECLARATOR declaration_list.  @>

@*5 \§boolean vector declaration> $\longrightarrow$ 
\.{BOOLEAN\_VECTOR\_DECLARATOR} \§declaration list>.

\LOG
\initials{LDF 2005.01.07.}
Added this rule.
\ENDLOG 


@<Define rules@>=
@=boolean_vector_declaration: BOOLEAN_VECTOR_DECLARATOR declaration_list@>
{
  vector_type_decl<bool>(static_cast<Scanner_Node>(parameter),
                         BOOLEAN_VECTOR,
                         BOOLEAN);
};

@q ****** (6) complex_vector_declaration -->               @>
@q ****** (6) COMPLEX_VECTOR_DECLARATOR declaration_list.  @>

@*5 \§complex vector declaration> $\longrightarrow$ 
\.{COMPLEX\_VECTOR\_DECLARATOR} \§declaration list>.
\initials{LDF 2007.12.02.}

\LOG
\initials{LDF 2007.12.02.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=complex_vector_declaration: COMPLEX_VECTOR_DECLARATOR declaration_list@>
{

  vector_type_decl<bool>(static_cast<Scanner_Node>(parameter),
                         COMPLEX_VECTOR,
                         COMPLEX);

};

@q ****** (6) matrix_vector_declaration -->               @>
@q ****** (6) MATRIX_VECTOR_DECLARATOR declaration_list.  @>

@*5 \§matrix vector declaration> $\longrightarrow$ 
\.{MATRIX\_VECTOR\_DECLARATOR} \§declaration list>.
\initials{LDF 2007.11.28.}

\LOG
\initials{LDF 2007.11.28.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=matrix_vector_declaration: MATRIX_VECTOR_DECLARATOR declaration_list@>
{

  vector_type_decl<bool>(static_cast<Scanner_Node>(parameter),
                         MATRIX_VECTOR,
                         MATRIX);

};

@q ****** (6) numeric_vector_declaration -->               @>
@q ****** (6) NUMERIC_VECTOR_DECLARATOR declaration_list.  @>

@*5 \§numeric vector declaration> $\longrightarrow$ 
\.{NUMERIC\_VECTOR\_DECLARATOR} \§declaration list>.

\LOG
\initials{LDF 2005.01.07.}
Added this rule.
\ENDLOG 


@<Define rules@>=
@=numeric_vector_declaration: NUMERIC_VECTOR_DECLARATOR declaration_list@>
{

 Scan_Parse::vector_type_decl<real>(static_cast<Scanner_Node>(parameter),
                                    NUMERIC_VECTOR,
                                    NUMERIC);
};


@q ****** (6) ulong_long_vector_declaration -->               @>
@q ****** (6) ULONG_LONG_VECTOR_DECLARATOR declaration_list.  @>

@*5 \§ulong long vector declaration> $\longrightarrow$ 
\.{ULONG\_LONG\_VECTOR\_DECLARATOR} \§declaration list>.
\initials{LDF 2005.12.05.}

\LOG
\initials{LDF 2005.12.05.}
Added this rule.

\initials{LDF 2005.12.07.}
Debugged this rule.
\ENDLOG 

@<Define rules@>=
@=ulong_long_vector_declaration: ULONG_LONG_VECTOR_DECLARATOR declaration_list@>
{

 Scan_Parse::vector_type_decl<ulong_long>(static_cast<Scanner_Node>(parameter),
                                    ULONG_LONG_VECTOR,
                                    ULONG_LONG);
};

@q ****** (6) string_vector_declaration -->               @>
@q ****** (6) STRING_VECTOR_DECLARATOR declaration_list.  @>

@*5 \§string vector declaration> $\longrightarrow$ 
\.{STRING\_VECTOR\_DECLARATOR} \§declaration list>.
\initials{LDF 2005.01.09.}

\LOG
\initials{LDF 2005.01.09.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=string_vector_declaration: STRING_VECTOR_DECLARATOR declaration_list@>
{

  vector_type_decl<string>(static_cast<Scanner_Node>(parameter),
                           STRING_VECTOR,
                           STRING);
};

@q ****** (6) pen_vector_declaration -->               @>
@q ****** (6) PEN_VECTOR_DECLARATOR declaration_list.  @>

@*5 \§pen vector declaration> $\longrightarrow$ 
\.{PEN\_VECTOR\_DECLARATOR} \§declaration list>.
\initials{LDF 2005.01.11.}

\LOG
\initials{LDF 2005.01.11.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=pen_vector_declaration: PEN_VECTOR_DECLARATOR declaration_list@>
{
  vector_type_decl<Pen>(static_cast<Scanner_Node>(parameter),
                        PEN_VECTOR,
                        PEN);
};

@q ****** (6) dash_pattern_vector_declaration -->               @>
@q ****** (6) DASH_PATTERN_VECTOR_DECLARATOR declaration_list.  @>

@*5 \§dash pattern vector declaration> $\longrightarrow$ 
\.{DASH\_PATTERN\_VECTOR\_DECLARATOR} \§declaration list>.
\initials{LDF 2005.01.13.}

\LOG
\initials{LDF 2005.01.13.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=dash_pattern_vector_declaration: @>
@=DASH_PATTERN_VECTOR_DECLARATOR declaration_list@>
{
   vector_type_decl<Dash_Pattern>(static_cast<Scanner_Node>(parameter),
                                          DASH_PATTERN_VECTOR,
                                          DASH_PATTERN);
};




@q ****** (6) color_vector_declaration -->               @>
@q ****** (6) COLOR_VECTOR_DECLARATOR declaration_list.  @>

@*5 \§color vector declaration> $\longrightarrow$ 
\.{COLOR\_VECTOR\_DECLARATOR} \§declaration list>.

\LOG
\initials{LDF 2004.08.23.}  
Added this rule.

\initials{LDF 2004.08.24.}
Changed the call to |Scan_Parse::vector_type_decl()|.
It's no longer a template function and I've changed its arguments. 

\initials{LDF 2004.08.254.}
Changed the call to |Scan_Parse::vector_type_decl()|
to reflect the fact that I've made it a template function again.
\ENDLOG 


@<Define rules@>=
@=color_vector_declaration: COLOR_VECTOR_DECLARATOR declaration_list@>
{
  vector_type_decl<Color>(static_cast<Scanner_Node>(parameter),
                                   COLOR_VECTOR,
                                   COLOR);
};

@q ****** (6) transform_vector_declaration -->               @>
@q ****** (6) TRANSFORM_VECTOR_DECLARATOR declaration_list.  @>

@*5 \§transform vector declaration> $\longrightarrow$ 
\.{TRANSFORM\_VECTOR\_DECLARATOR} \§declaration list>.
\initials{LDF 2005.01.13.}

\LOG
\initials{LDF 2005.01.13.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=transform_vector_declaration: @>
@=TRANSFORM_VECTOR_DECLARATOR declaration_list@>
{

   vector_type_decl<Transform>(static_cast<Scanner_Node>(parameter),
                                          TRANSFORM_VECTOR,
                                          TRANSFORM);
};

@q ****** (6) picture_vector_declaration -->               @>
@q ****** (6) PICTURE_VECTOR_DECLARATOR declaration_list.  @>

@*5 \§picture vector declaration> $\longrightarrow$ 
\.{PICTURE\_VECTOR\_DECLARATOR} \§declaration list>.
\initials{LDF 2005.01.15.}

\LOG
\initials{LDF 2005.01.15.}
Added this rule.

\initials{LDF 2005.01.17.}
@:BUG FIX@> BUG FIX:  Changed |Definition_Info_Type| to |Picture| in 
the call to |Scan_Parse::vector_type_decl()|.  I don't know why this
apparently caused no problems.
\ENDLOG 


@<Define rules@>=
@=picture_vector_declaration: PICTURE_VECTOR_DECLARATOR declaration_list@>
{
   vector_type_decl<Picture>(static_cast<Scanner_Node>(parameter),
                                      PICTURE_VECTOR,
                                      PICTURE);

};

@q ****** (6) focus_vector_declaration -->               @>
@q ****** (6) FOCUS_VECTOR_DECLARATOR declaration_list.  @>

@*5 \§focus vector declaration> $\longrightarrow$ 
\.{FOCUS\_VECTOR\_DECLARATOR} \§declaration list>.
\initials{LDF 2005.01.17.}

\LOG
\initials{LDF 2005.01.17.}
Added this rule.
\ENDLOG 


@<Define rules@>=
@=focus_vector_declaration: FOCUS_VECTOR_DECLARATOR declaration_list@>
{

   vector_type_decl<Focus>(static_cast<Scanner_Node>(parameter),
                                    FOCUS_VECTOR,
                                    FOCUS);
};

@q ****** (6) macro_vector_declaration -->               @>
@q ****** (6) MACRO_VECTOR_DECLARATOR declaration_list.  @>

@*5 \§macro vector declaration> $\longrightarrow$ 
\.{MACRO\_VECTOR\_DECLARATOR} \§declaration list>.

\LOG
\initials{LDF 2004.12.28.}
Added this rule.
\ENDLOG 


@<Define rules@>=
@=macro_vector_declaration: MACRO_VECTOR_DECLARATOR declaration_list@>
{

  vector_type_decl<Definition_Info_Type>(static_cast<Scanner_Node>(parameter),
                                                  MACRO_VECTOR,
                                                  MACRO);
};



@q ***** (5) |Shape| types.@> 
@*4 {\bf Shape} types.
\initials{LDF 2004.12.12.}

\LOG
\initials{LDF 2004.12.12.}
Added this section.
\ENDLOG

@q ****** (6) point_vector_declaration -->               @>
@q ****** (6) POINT_VECTOR_DECLARATOR declaration_list.  @>
@*5 \§point vector declaration> $\longrightarrow$ 
\.{POINT\_VECTOR\_DECLARATOR} \§declaration list>.

\LOG
\initials{LDF 2004.08.31.}  
Added this rule.
\ENDLOG 

@<Define rules@>=
@=point_vector_declaration: POINT_VECTOR_DECLARATOR declaration_list@>
{
  vector_type_decl<Point>(static_cast<Scanner_Node>(parameter),
                                   POINT_VECTOR,
                                   POINT);

};

@q ****** (6) bool_point_vector_declaration -->               @>
@q ****** (6) BOOL_POINT_VECTOR_DECLARATOR declaration_list.  @>
@ \§bool-point vector declaration> $\longrightarrow$ 
\.{BOOL\_POINT\_VECTOR\_DECLARATOR} \§declaration list>.

\LOG
\initials{LDF 2004.09.01.}  
Added this rule.
\ENDLOG 

@<Define rules@>=
@=bool_point_vector_declaration: BOOL_POINT_VECTOR_DECLARATOR declaration_list@>
{

     Scan_Parse::vector_type_decl<Bool_Point>(static_cast<Scanner_Node>(parameter),
                                              BOOL_POINT_VECTOR,
                                              BOOL_POINT);
};

@q ****** (6) path_vector_declaration -->               @>
@q ****** (6) PATH_VECTOR_DECLARATOR declaration_list.  @>
@*3 \§path vector declaration> $\longrightarrow$ 
\.{PATH\_VECTOR\_DECLARATOR} \§declaration list>.

\LOG
\initials{LDF 2004.12.10.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=path_vector_declaration: PATH_VECTOR_DECLARATOR declaration_list@>
{

     vector_type_decl<Path>(static_cast<Scanner_Node>(parameter),
                            PATH_VECTOR,
                            PATH); 
};

@q ****** (6) ellipse_vector_declaration -->               @>
@q ****** (6) ELLIPSE_VECTOR_DECLARATOR declaration_list.  @>

@*3 \§ellipse vector declaration> $\longrightarrow$ 
\.{ELLIPSE\_VECTOR\_DECLARATOR} \§declaration list>.

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=ellipse_vector_declaration: ELLIPSE_VECTOR_DECLARATOR declaration_list@>
{

  Scan_Parse::vector_type_decl<Ellipse>(static_cast<Scanner_Node>(parameter),
                                        ELLIPSE_VECTOR,
                                        ELLIPSE); 
};

@q ****** (6) circle_vector_declaration -->               @>
@q ****** (6) CIRCLE_VECTOR_DECLARATOR declaration_list.  @>
@*3 \§circle vector declaration> $\longrightarrow$ 
\.{CIRCLE\_VECTOR\_DECLARATOR} \§declaration list>.

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=circle_vector_declaration: CIRCLE_VECTOR_DECLARATOR declaration_list@>
{

  vector_type_decl<Circle>(static_cast<Scanner_Node>(parameter),
                           CIRCLE_VECTOR,
                           CIRCLE);
};

@q ****** (6) parabola_vector_declaration -->               @>
@q ****** (6) PARABOLA_VECTOR_DECLARATOR declaration_list.  @>

@*3 \§parabola vector declaration> $\longrightarrow$ 
\.{PARABOLA\_VECTOR\_DECLARATOR} \§declaration list>.
\initials{LDF 2005.11.07.}

\LOG
\initials{LDF 2005.11.07.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=parabola_vector_declaration: PARABOLA_VECTOR_DECLARATOR declaration_list@>
{

    Scan_Parse::vector_type_decl<Parabola>(static_cast<Scanner_Node>(parameter),
                                           PARABOLA_VECTOR,
                                           PARABOLA);

};

@q ****** (6) hyperbola_vector_declaration -->               @>
@q ****** (6) HYPERBOLA_VECTOR_DECLARATOR declaration_list.  @>

@*3 \§hyperbola vector declaration> $\longrightarrow$ 
\.{HYPERBOLA\_VECTOR\_DECLARATOR} \§declaration list>.
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=hyperbola_vector_declaration: HYPERBOLA_VECTOR_DECLARATOR declaration_list@>
{

    Scan_Parse::vector_type_decl<Hyperbola>(static_cast<Scanner_Node>(parameter),
                                           HYPERBOLA_VECTOR,
                                           HYPERBOLA);

};

@q ****** (6) arc_vector_declaration -->               @>
@q ****** (6) ARC_VECTOR_DECLARATOR declaration_list.  @>

@*3 \§arc vector declaration> $\longrightarrow$ 
\.{ARC\_VECTOR\_DECLARATOR} \§declaration list>.
\initials{LDF 2007.10.13.}

\LOG
\initials{LDF 2007.10.13.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=arc_vector_declaration: ARC_VECTOR_DECLARATOR declaration_list@>
{

    Scan_Parse::vector_type_decl<Arc>(static_cast<Scanner_Node>(parameter),
                                           ARC_VECTOR,
                                           ARC);

};



@q ****** (6) conic_section_lattice_vector_declaration -->               @>
@q ****** (6) CONIC_SECTION_LATTICE_VECTOR_DECLARATOR declaration_list.  @>

@*3 \§conic section lattice vector declaration> $\longrightarrow$ 
\.{CONIC\_SECTION\_LATTICE\_VECTOR\_DECLARATOR} \§declaration list>.
\initials{LDF 2007.07.29.}

\LOG
\initials{LDF 2007.07.29.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=conic_section_lattice_vector_declaration: CONIC_SECTION_LATTICE_VECTOR_DECLARATOR @>@/
@= declaration_list@>@/
{

    Scan_Parse::vector_type_decl<Conic_Section_Lattice>(
                                           static_cast<Scanner_Node>(parameter),
                                           CONIC_SECTION_LATTICE_VECTOR,
                                           CONIC_SECTION_LATTICE);

};




@q ****** (6) helix_vector_declaration -->               @>
@q ****** (6) HELIX_VECTOR_DECLARATOR declaration_list.  @>

@*3 \§helix vector declaration> $\longrightarrow$ 
\.{HELIX\_VECTOR\_DECLARATOR} \§declaration list>.
\initials{LDF 2005.05.18.}

\LOG
\initials{LDF 2005.05.18.}
Added this rule.

\initials{LDF 2005.11.07.}
Removed debugging code.
\ENDLOG 

@<Define rules@>=
@=helix_vector_declaration: HELIX_VECTOR_DECLARATOR declaration_list@>
{

    Scan_Parse::vector_type_decl<Helix>(static_cast<Scanner_Node>(parameter),
                                        HELIX_VECTOR,
                                        HELIX);

};


@q ****** (6) rectangle_vector_declaration -->               @>
@q ****** (6) RECTANGLE_VECTOR_DECLARATOR declaration_list.  @>
@*3 \§rectangle vector declaration> $\longrightarrow$ 
\.{RECTANGLE\_VECTOR\_DECLARATOR} \§declaration list>.

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=rectangle_vector_declaration: RECTANGLE_VECTOR_DECLARATOR declaration_list@>
{

  vector_type_decl<Rectangle>(static_cast<Scanner_Node>(parameter),
                                     RECTANGLE_VECTOR,
                                     RECTANGLE);

};

@q ****** (6) triangle_vector_declaration -->               @>
@q ****** (6) TRIANGLE_VECTOR_DECLARATOR declaration_list.  @>

@*5 \§triangle vector declaration> $\longrightarrow$ 
\.{TRIANGLE\_VECTOR\_DECLARATOR} \§declaration list>.
\initials{LDF 2005.01.20.}

\LOG
\initials{LDF 2005.01.20.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=triangle_vector_declaration: TRIANGLE_VECTOR_DECLARATOR declaration_list@>
{

  vector_type_decl<Triangle>(static_cast<Scanner_Node>(parameter),
                                      TRIANGLE_VECTOR,
                                      TRIANGLE);

};


@q ****** (6) nurb_vector_declaration -->               @>
@q ****** (6) NURB_VECTOR_DECLARATOR declaration_list.  @>

@*5 \§nurb vector declaration> $\longrightarrow$ 
\.{NURB\_VECTOR\_DECLARATOR} \§declaration list>.
\initials{LDF 2005.01.26.}

\LOG
\initials{LDF 2005.01.26.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=nurb_vector_declaration: NURB_VECTOR_DECLARATOR declaration_list@>
{
  vector_type_decl<Nurb>(static_cast<Scanner_Node>(parameter),
                                  NURB_VECTOR,
                                  NURB);

};


@q ****** (6) polygon_vector_declaration -->               @>
@q ****** (6) POLYGON_VECTOR_DECLARATOR declaration_list.  @>
@*3 \§polygon vector declaration> $\longrightarrow$ 
\.{POLYGON\_VECTOR\_DECLARATOR} \§declaration list>.
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.02.11.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=polygon_vector_declaration: POLYGON_VECTOR_DECLARATOR declaration_list@>
{

   vector_type_decl<Polygon>(static_cast<Scanner_Node>(parameter),
                                      POLYGON_VECTOR,
                                      POLYGON);

};



@q ****** (6) reg_polygon_vector_declaration -->               @>
@q ****** (6) REG_POLYGON_VECTOR_DECLARATOR declaration_list.  @>
@*3 \§regular polygon vector declaration> $\longrightarrow$ 
\.{REG\_POLYGON\_VECTOR\_DECLARATOR} \§declaration list>.

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=reg_polygon_vector_declaration: REG_POLYGON_VECTOR_DECLARATOR declaration_list@>
{
  vector_type_decl<Reg_Polygon>(static_cast<Scanner_Node>(parameter),
                                     REG_POLYGON_VECTOR,
                                     REG_POLYGON);

};

@q ****** (6) cone_vector_declaration -->               @>
@q ****** (6) CONE_VECTOR_DECLARATOR declaration_list.  @>

@*3 \§cone vector declaration> $\longrightarrow$ 
\.{CONE\_VECTOR\_DECLARATOR} \§declaration list>.
\initials{LDF 2005.05.25.}

\LOG
\initials{LDF 2005.05.25.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=cone_vector_declaration: CONE_VECTOR_DECLARATOR declaration_list@>
{
  vector_type_decl<Cone>(static_cast<Scanner_Node>(parameter),
                                     CONE_VECTOR,
                                     CONE);

};

@q ****** (6) cylinder_vector_declaration -->               @>
@q ****** (6) CYLINDER_VECTOR_DECLARATOR declaration_list.  @>

@*3 \§cylinder vector declaration> $\longrightarrow$ 
\.{CYLINDER\_VECTOR\_DECLARATOR} \§declaration list>.
\initials{LDF 2005.05.25.}

\LOG
\initials{LDF 2005.05.25.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=cylinder_vector_declaration: CYLINDER_VECTOR_DECLARATOR declaration_list@>
{

  vector_type_decl<Cylinder>(static_cast<Scanner_Node>(parameter),
                                     CYLINDER_VECTOR,
                                     CYLINDER);

};

@q ****** (6) cuboid_vector_declaration -->               @>
@q ****** (6) CUBOID_VECTOR_DECLARATOR declaration_list.  @>
@*3 \§cuboid vector declaration> $\longrightarrow$ 
\.{CUBOID\_VECTOR\_DECLARATOR} \§declaration list>.

\LOG
\initials{LDF 2004.12.14.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=cuboid_vector_declaration: CUBOID_VECTOR_DECLARATOR declaration_list@>
{
  vector_type_decl<Cuboid>(static_cast<Scanner_Node>(parameter),
                                     CUBOID_VECTOR,
                                     CUBOID);

};

@q ****** (6) polyhedron_vector_declaration -->               @>
@q ****** (6) POLYHEDRON_VECTOR_DECLARATOR declaration_list.  @>

@*3 \§polyhedron vector declaration> $\longrightarrow$ 
\.{POLYHEDRON\_VECTOR\_DECLARATOR} \§declaration list>.
\initials{LDF 2005.01.14.}

\LOG
\initials{LDF 2005.01.14.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=polyhedron_vector_declaration: POLYHEDRON_VECTOR_DECLARATOR @>
@=declaration_list@>@/
{
  vector_type_decl<Polyhedron>(static_cast<Scanner_Node>(parameter),
                                     POLYHEDRON_VECTOR,
                                     POLYHEDRON);

};

@q ****** (6) ellipsoid_vector_declaration -->               @>
@q ****** (6) ELLIPSOID_VECTOR_DECLARATOR declaration_list.  @>

@*3 \§ellipsoid vector declaration> $\longrightarrow$ 
\.{ELLIPSOID\_VECTOR\_DECLARATOR} \§declaration list>.
\initials{LDF 2005.05.26.}

\LOG
\initials{LDF 2005.05.26.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=ellipsoid_vector_declaration: ELLIPSOID_VECTOR_DECLARATOR declaration_list@>
{
  vector_type_decl<Ellipsoid>(static_cast<Scanner_Node>(parameter),
                                       ELLIPSOID_VECTOR,
                                       ELLIPSOID);

 
};

@q ****** (6) sphere_vector_declaration -->               @>
@q ****** (6) SPHERE_VECTOR_DECLARATOR declaration_list.  @>

@*3 \§sphere vector declaration> $\longrightarrow$ 
\.{SPHERE\_VECTOR\_DECLARATOR} \§declaration list>.
\initials{LDF 2005.06.06.}

\LOG
\initials{LDF 2005.06.06.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=sphere_vector_declaration: SPHERE_VECTOR_DECLARATOR declaration_list@>
{
    vector_type_decl<Sphere>(static_cast<Scanner_Node>(parameter),
                             SPHERE_VECTOR,
                             SPHERE);
};

@q ****** (6) sphere_development_vector_declaration -->               @>
@q ****** (6) SPHERE_DEVELOPMENT_VECTOR_DECLARATOR declaration_list.  @>

@*3 \§sphere development vector declaration> $\longrightarrow$ 
\.{SPHERE_DEVELOPMENT\_VECTOR\_DECLARATOR} \§declaration list>.
\initials{LDF 2009.11.04.}

\LOG
\initials{LDF 2009.11.04.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=sphere_development_vector_declaration: SPHERE_DEVELOPMENT_VECTOR_DECLARATOR declaration_list@>
{
    vector_type_decl<Sphere_Development>(static_cast<Scanner_Node>(parameter),
                                         SPHERE_DEVELOPMENT_VECTOR,
                                         SPHERE_DEVELOPMENT);
};


@q ****** (6) paraboloid_vector_declaration -->               @>
@q ****** (6) PARABOLOID_VECTOR_DECLARATOR declaration_list.  @>

@*3 \§paraboloid vector declaration> $\longrightarrow$ 
\.{PARABOLOID\_VECTOR\_DECLARATOR} \§declaration list>.
\initials{LDF 2005.06.06.}

\LOG
\initials{LDF 2005.06.06.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=paraboloid_vector_declaration: PARABOLOID_VECTOR_DECLARATOR declaration_list@>
{
    vector_type_decl<Paraboloid>(static_cast<Scanner_Node>(parameter),
                                 PARABOLOID_VECTOR,
                                 PARABOLOID);
};


@q ****** (6) glyph_vector_declaration -->               @>
@q ****** (6) GLYPH_VECTOR_DECLARATOR declaration_list.  @>

@*3 \§glyph vector declaration> $\longrightarrow$ 
\.{GLYPH\_VECTOR\_DECLARATOR} \§declaration list>.
\initials{LDF 2005.06.08.}

\LOG
\initials{LDF 2005.06.08.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=glyph_vector_declaration: GLYPH_VECTOR_DECLARATOR declaration_list@>
{

  vector_type_decl<Glyph>(static_cast<Scanner_Node>(parameter),
                                       GLYPH_VECTOR,
                                       GLYPH);

};

@q ****** (6) plane_vector_declaration -->               @>
@q ****** (6) PLANE_VECTOR_DECLARATOR declaration_list.  @>

@*3 \§plane vector declaration> $\longrightarrow$ 
\.{PLANE\_VECTOR\_DECLARATOR} \§declaration list>.
\initials{LDF 2005.10.30.}

\LOG
\initials{LDF 2005.10.30.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=plane_vector_declaration: PLANE_VECTOR_DECLARATOR declaration_list@>
{
  vector_type_decl<Plane>(static_cast<Scanner_Node>(parameter),
                                       PLANE_VECTOR,
                                       PLANE);
};




@q ****** (6) ellipse_slice_vector_declaration -->               @>
@q ****** (6) ELLIPSE_SLICE_VECTOR_DECLARATOR declaration_list.  @>

@*5 \§ellipse slice vector declaration> $\longrightarrow$ 
\.{ELLIPSE\_SLICE\_VECTOR\_DECLARATOR} \§declaration list>.
\initials{LDF 2005.03.29.}

\LOG
\initials{LDF 2005.03.29.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=ellipse_slice_vector_declaration: ELLIPSE_SLICE_VECTOR_DECLARATOR declaration_list@>
{
  vector_type_decl<Ellipse_Slice>(static_cast<Scanner_Node>(parameter),
                                           ELLIPSE_SLICE_VECTOR,
                                           ELLIPSE_SLICE);

};

@q ****** (6) circle_slice_vector_declaration -->               @>
@q ****** (6) CIRCLE_SLICE_VECTOR_DECLARATOR declaration_list.  @>

@*5 \§circle slice vector declaration> $\longrightarrow$ 
\.{CIRCLE\_SLICE\_VECTOR\_DECLARATOR} \§declaration list>.
\initials{LDF 2005.03.29.}

\LOG
\initials{LDF 2005.03.29.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=circle_slice_vector_declaration: CIRCLE_SLICE_VECTOR_DECLARATOR declaration_list@>
{
  vector_type_decl<Circle_Slice>(static_cast<Scanner_Node>(parameter),
                                           CIRCLE_SLICE_VECTOR,
                                           CIRCLE_SLICE);

};

@q ****** (6) polyhedron_slice_vector_declaration -->               @>
@q ****** (6) POLYHEDRON_SLICE_VECTOR_DECLARATOR declaration_list.  @>

@*5 \§polyhedron slice vector declaration> $\longrightarrow$ 
\.{POLYHEDRON\_SLICE\_VECTOR\_DECLARATOR} \§declaration list>.
\initials{LDF 2005.03.29.}

\LOG
\initials{LDF 2005.03.29.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=polyhedron_slice_vector_declaration: POLYHEDRON_SLICE_VECTOR_DECLARATOR@>@/ 
@=declaration_list@>
{
  vector_type_decl<Polyhedron_Slice>(static_cast<Scanner_Node>(parameter),
                                           POLYHEDRON_SLICE_VECTOR,
                                           POLYHEDRON_SLICE);

};

@q ****** (6) origami_figure_vector_declaration -->               @>
@q ****** (6) ORIGAMI_FIGURE_VECTOR_DECLARATOR declaration_list.  @>

@*3 \§origami figure vector declaration> $\longrightarrow$ 
\.{ORIGAMI\_FIGURE\_VECTOR\_DECLARATOR} \§declaration list>.
\initials{LDF 2005.02.04.}

\LOG
\initials{LDF 2005.02.04.}
Added this rule.
\ENDLOG 

@<Define rules@>=
@=origami_figure_vector_declaration: ORIGAMI_FIGURE_VECTOR_DECLARATOR @>
@=declaration_list@>@/
{
  vector_type_decl<Origami_Figure>(static_cast<Scanner_Node>(parameter),
                                            ORIGAMI_FIGURE_VECTOR,
                                            ORIGAMI_FIGURE);

};

@q *** (3) declaration_list --> declared_variable.@>
@*2 \§declaration list> $\longrightarrow$ \§declared variable>.

\LOG
\initials{LDF 2004.12.12.}
Made debugging output thread-safe.
\ENDLOG 

@<Define rules@>=

@=declaration_list: declared_variable@>
;

@q *** (3) declaration_list --> COMMA declared_variable.@>
@*2 \§declaration list> $\longrightarrow$ {\tt COMMA} \§declared variable>.

\LOG
\initials{LDF 2004.12.12.}
Made debugging output thread-safe.
\ENDLOG 

@<Define rules@>=

@=declaration_list: declaration_list COMMA declared_variable@>
;

@q *** (3) declared_variable.  @>
@*2 \§declared variable>.

@<Type declarations for non-terminal symbols@>=
@=%type <string_value> declared_variable@>@/

@q **** (4) declared_variable --> symbolic_token declared_suffix.@> 
@*3 \§declared variable> $\longrightarrow$ \§symbolic token> \§declared suffix>.

\LOG
\initials{LDF 2004.12.12.}
Made debugging output thread-safe.

\initials{LDF 2004.12.28.}
Now using the character octal 243 (British Pound) as the placeholder
for subscripts rather than \.{-}.
\ENDLOG 

@<Define rules@>=
@=declared_variable: symbolic_token declared_suffix@>
{
  strcpy(@=$$@>, @=$1@>);
  strcat(@=$$@>, @=$2@>);

  String_Bool sb;

  sb.first = @=$$@>;

  sb.second = (sb.first.find('£') == string::npos) ? false : true;

  static_cast<Scanner_Node>(parameter)->token_string_bool_vector.push_back(sb);

};

@q * (1) any_declarator.@> 
@* \§any declarator>.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this type declaration.
\ENDLOG

@<Type declarations for non-terminal symbols@>=
@=%type <int_value> any_declarator@>@/

@q ** (2) any_declarator --> BOOLEAN_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{BOOLEAN\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=any_declarator: BOOLEAN_DECLARATOR@>
{

   @=$$@> = BOOLEAN_DECLARATOR;

};

@q ** (2) any_declarator --> BOOLEAN_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{BOOLEAN\_VECTOR\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=any_declarator: BOOLEAN_VECTOR_DECLARATOR@>
{
   @=$$@> = BOOLEAN_VECTOR_DECLARATOR;
};

@q ** (2) any_declarator --> COMPLEX_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{COMPLEX\_DECLARATOR}.
\initials{LDF 2007.12.02.}

\LOG
\initials{LDF 2007.12.02.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=any_declarator: COMPLEX_DECLARATOR@>
{

   @=$$@> = COMPLEX_DECLARATOR;

};

@q ** (2) any_declarator --> COMPLEX_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{COMPLEX\_VECTOR\_DECLARATOR}.
\initials{LDF 2007.12.02.}

\LOG
\initials{LDF 2007.12.02.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=any_declarator: COMPLEX_VECTOR_DECLARATOR@>
{
   @=$$@> = COMPLEX_VECTOR_DECLARATOR;
};

@q ** (2) any_declarator --> MATRIX_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{MATRIX\_DECLARATOR}.
\initials{LDF 2007.11.28.}

\LOG
\initials{LDF 2007.11.28.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=any_declarator: MATRIX_DECLARATOR@>
{

   @=$$@> = MATRIX_DECLARATOR;

};

@q ** (2) any_declarator --> MATRIX_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{MATRIX\_VECTOR\_DECLARATOR}.
\initials{LDF 2007.11.28.}

\LOG
\initials{LDF 2007.11.28.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=any_declarator: MATRIX_VECTOR_DECLARATOR@>
{
   @=$$@> = MATRIX_VECTOR_DECLARATOR;
};

@q ** (2) any_declarator --> STRING_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{STRING\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=any_declarator: STRING_DECLARATOR@>
{
   @=$$@> = STRING_DECLARATOR;

};


@q ** (2) any_declarator --> STRING_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{STRING\_VECTOR\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=any_declarator: STRING_VECTOR_DECLARATOR@>
{

   @=$$@> = STRING_VECTOR_DECLARATOR;

};

@q ** (2) any_declarator --> PEN_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{PEN\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: PEN_DECLARATOR@>
{

   @=$$@> = PEN_DECLARATOR;

};

@q ** (2) any_declarator --> PEN_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{PEN\_VECTOR\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: PEN_VECTOR_DECLARATOR@>
{

   @=$$@> = PEN_VECTOR_DECLARATOR;

};

@q ** (2) any_declarator --> DASH_PATTERN_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{DASH\_PATTERN\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: DASH_PATTERN_DECLARATOR@>
{

   @=$$@> = DASH_PATTERN_DECLARATOR;

};

@q ** (2) any_declarator --> DASH_PATTERN_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{DASH\_PATTERN\_VECTOR\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: DASH_PATTERN_VECTOR_DECLARATOR@>
{

   @=$$@> = DASH_PATTERN_VECTOR_DECLARATOR;

};

@q ** (2) any_declarator --> FOCUS_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{FOCUS\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: FOCUS_DECLARATOR@>
{
   @=$$@> = FOCUS_DECLARATOR;
};

@q ** (2) any_declarator --> FOCUS_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{FOCUS\_VECTOR\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: FOCUS_VECTOR_DECLARATOR@>
{
   @=$$@> = FOCUS_VECTOR_DECLARATOR;
};

@q ** (2) any_declarator --> MACRO_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{MACRO\_DECLARATOR}.
\initials{LDF 2004.12.28.}

\LOG
\initials{LDF 2004.12.28.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: MACRO_DECLARATOR@>
{
   @=$$@> = MACRO_DECLARATOR;
};

@q ** (2) any_declarator --> MACRO_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{MACRO\_VECTOR\_DECLARATOR}.
\initials{LDF 2004.12.28.}

\LOG
\initials{LDF 2004.12.28.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: MACRO_VECTOR_DECLARATOR@>
{
   @=$$@> = MACRO_VECTOR_DECLARATOR;
};


@q ** (2) any_declarator --> COLOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{COLOR\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: COLOR_DECLARATOR@>
{
   @=$$@> = COLOR_DECLARATOR;
};

@q ** (2) any_declarator --> COLOR_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{COLOR\_VECTOR\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=any_declarator: COLOR_VECTOR_DECLARATOR@>
{
   @=$$@> = COLOR_VECTOR_DECLARATOR;
};

@q ** (2) any_declarator --> NUMERIC_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{NUMERIC\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: NUMERIC_DECLARATOR@>
{
   @=$$@> = NUMERIC_DECLARATOR;
};

@q ** (2) any_declarator --> NUMERIC_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{NUMERIC\_VECTOR\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: NUMERIC_VECTOR_DECLARATOR@>
{
   @=$$@> = NUMERIC_VECTOR_DECLARATOR;
};

@q ** (2) any_declarator --> ULONG_LONG_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{ULONG\_LONG\_DECLARATOR}.
\initials{LDF 2005.12.05.}

\LOG
\initials{LDF 2005.12.05.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=any_declarator: ULONG_LONG_DECLARATOR@>
{
   @=$$@> = ULONG_LONG_DECLARATOR;
};

@q ** (2) any_declarator --> ULONG_LONG_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{ULONG\_LONG\_VECTOR\_DECLARATOR}.
\initials{LDF 2005.12.05.}

\LOG
\initials{LDF 2005.12.05.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=any_declarator: ULONG_LONG_VECTOR_DECLARATOR@>
{
   @=$$@> = ULONG_LONG_VECTOR_DECLARATOR;
};

@q ** (2) any_declarator --> TRANSFORM_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{TRANSFORM\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: TRANSFORM_DECLARATOR@>
{
   @=$$@> = TRANSFORM_DECLARATOR;
};

@q ** (2) any_declarator --> TRANSFORM_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{TRANSFORM\_VECTOR\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: TRANSFORM_VECTOR_DECLARATOR@>
{
   @=$$@> = TRANSFORM_VECTOR_DECLARATOR;
};

@q ** (2) any_declarator --> PICTURE_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{PICTURE\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: PICTURE_DECLARATOR@>
{
   @=$$@> = PICTURE_DECLARATOR;
};

@q ** (2) any_declarator --> PICTURE_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{PICTURE\_VECTOR\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: PICTURE_VECTOR_DECLARATOR@>
{
   @=$$@> = PICTURE_VECTOR_DECLARATOR;
};

@q ** (2) any_declarator --> POINT_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{POINT\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: POINT_DECLARATOR@>
{
   @=$$@> = POINT_DECLARATOR;

};

@q ** (2) any_declarator --> POINT_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{POINT\_VECTOR\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: POINT_VECTOR_DECLARATOR@>
{
   @=$$@> = POINT_VECTOR_DECLARATOR;

};

@q ** (2) any_declarator --> BOOL_POINT_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{BOOL\_POINT\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: BOOL_POINT_DECLARATOR@>
{
   @=$$@> = BOOL_POINT_DECLARATOR;

};

@q ** (2) any_declarator --> BOOL_POINT_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{BOOL\_POINT\_VECTOR\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: BOOL_POINT_VECTOR_DECLARATOR@>
{
   @=$$@> = BOOL_POINT_VECTOR_DECLARATOR;

};

@q ** (2) any_declarator --> PATH_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{PATH\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: PATH_DECLARATOR@>
{
   @=$$@> = PATH_DECLARATOR;

};

@q ** (2) any_declarator --> PATH_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{PATH\_VECTOR\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: PATH_VECTOR_DECLARATOR@>
{
   @=$$@> = PATH_VECTOR_DECLARATOR;

};

@q ** (2) any_declarator --> TRIANGLE_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{TRIANGLE\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: TRIANGLE_DECLARATOR@>
{
   @=$$@> = TRIANGLE_DECLARATOR;

};

@q ** (2) any_declarator --> NURB_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{NURB\_DECLARATOR}.
\initials{LDF 2005.01.26.}

\LOG
\initials{LDF 2005.01.26.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: NURB_DECLARATOR@>
{

   @=$$@> = NURB_DECLARATOR;

};


@q ** (2) any_declarator --> TRIANGLE_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{TRIANGLE\_VECTOR\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: TRIANGLE_VECTOR_DECLARATOR@>
{
   @=$$@> = TRIANGLE_VECTOR_DECLARATOR;

};

@q ** (2) any_declarator --> NURB_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{NURB\_VECTOR\_DECLARATOR}.
\initials{LDF 2005.01.26.}

\LOG
\initials{LDF 2005.01.26.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: NURB_VECTOR_DECLARATOR@>
{
   @=$$@> = NURB_VECTOR_DECLARATOR;

};


@q ** (2) any_declarator --> POLYGON_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{POLYGON\_DECLARATOR}.
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.02.11.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: POLYGON_DECLARATOR@>
{
   @=$$@> = POLYGON_DECLARATOR;

};

@q ** (2) any_declarator --> POLYGON_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{POLYGON\_VECTOR\_DECLARATOR}.
\initials{LDF 2005.02.11.}

\LOG
\initials{LDF 2005.02.11.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=any_declarator: POLYGON_VECTOR_DECLARATOR@>
{

   @=$$@> = POLYGON_VECTOR_DECLARATOR;

};


@q ** (2) any_declarator --> REG_POLYGON_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{REG\_POLYGON\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: REG_POLYGON_DECLARATOR@>
{

   @=$$@> = REG_POLYGON_DECLARATOR;

};

@q ** (2) any_declarator --> REG_POLYGON_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{REG\_POLYGON\_VECTOR\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: REG_POLYGON_VECTOR_DECLARATOR@>
{

   @=$$@> = REG_POLYGON_VECTOR_DECLARATOR;

};

@q ** (2) any_declarator --> RECTANGLE_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{RECTANGLE\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: RECTANGLE_DECLARATOR@>
{

   @=$$@> = RECTANGLE_DECLARATOR;

};

@q ** (2) any_declarator --> RECTANGLE_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{RECTANGLE\_VECTOR\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: RECTANGLE_VECTOR_DECLARATOR@>
{

   @=$$@> = RECTANGLE_VECTOR_DECLARATOR;

};

@q ** (2) any_declarator --> SQUARE_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{SQUARE\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: SQUARE_DECLARATOR@>
{
   @=$$@> = SQUARE_DECLARATOR;

};

@q ** (2) any_declarator --> SQUARE_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{SQUARE\_VECTOR\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: SQUARE_VECTOR_DECLARATOR@>
{

   @=$$@> = SQUARE_VECTOR_DECLARATOR;

};

@q ** (2) any_declarator --> ELLIPSE_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{ELLIPSE\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: ELLIPSE_DECLARATOR@>
{

   @=$$@> = ELLIPSE_DECLARATOR;

};

@q ** (2) any_declarator --> ELLIPSE_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{ELLIPSE\_VECTOR\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: ELLIPSE_VECTOR_DECLARATOR@>
{

   @=$$@> = ELLIPSE_VECTOR_DECLARATOR;

};

@q ** (2) any_declarator --> CIRCLE_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{CIRCLE\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: CIRCLE_DECLARATOR@>
{

   @=$$@> = CIRCLE_DECLARATOR;

};

@q ** (2) any_declarator --> CIRCLE_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{CIRCLE\_VECTOR\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: CIRCLE_VECTOR_DECLARATOR@>
{

   @=$$@> = CIRCLE_VECTOR_DECLARATOR;

};

@q ** (2) any_declarator --> PARABOLA_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{PARABOLA\_DECLARATOR}.
\initials{LDF 2005.11.07.}

\LOG
\initials{LDF 2005.11.07.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=any_declarator: PARABOLA_DECLARATOR@>
{

   @=$$@> = PARABOLA_DECLARATOR;

};


@q ** (2) any_declarator --> PARABOLA_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{PARABOLA\_VECTOR\_DECLARATOR}.
\initials{LDF 2005.11.07.}

\LOG
\initials{LDF 2005.11.07.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: PARABOLA_VECTOR_DECLARATOR@>
{

   @=$$@> = PARABOLA_VECTOR_DECLARATOR;

};

@q ** (2) any_declarator --> HYPERBOLA_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{HYPERBOLA\_DECLARATOR}.
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=any_declarator: HYPERBOLA_DECLARATOR@>
{

   @=$$@> = HYPERBOLA_DECLARATOR;

};

@q ** (2) any_declarator --> HYPERBOLA_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{HYPERBOLA\_VECTOR\_DECLARATOR}.
\initials{LDF 2005.11.14.}

\LOG
\initials{LDF 2005.11.14.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: HYPERBOLA_VECTOR_DECLARATOR@>
{

   @=$$@> = HYPERBOLA_VECTOR_DECLARATOR;

};


@q ** (2) any_declarator --> ARC_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{ARC\_DECLARATOR}.
\initials{LDF 2007.10.13.}

\LOG
\initials{LDF 2007.10.13.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=any_declarator: ARC_DECLARATOR@>
{

   @=$$@> = ARC_DECLARATOR;

};

@q ** (2) any_declarator --> ARC_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{ARC\_VECTOR\_DECLARATOR}.
\initials{LDF 2007.10.13.}

\LOG
\initials{LDF 2007.10.13.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=any_declarator: ARC_VECTOR_DECLARATOR@>
{

   @=$$@> = ARC_VECTOR_DECLARATOR;

};




@q ** (2) any_declarator --> CONIC_SECTION_LATTICE_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{CONIC\_SECTION\_LATTICE\_DECLARATOR}.
\initials{LDF 2007.07.29.}

\LOG
\initials{LDF 2007.07.29.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=any_declarator: CONIC_SECTION_LATTICE_DECLARATOR@>
{

   @=$$@> = CONIC_SECTION_LATTICE_DECLARATOR;

};

@q ** (2) any_declarator --> CONIC_SECTION_LATTICE_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{CONIC\_SECTION\_LATTICE\_VECTOR\_DECLARATOR}.
\initials{LDF 2007.07.29.}

\LOG
\initials{LDF 2007.07.29.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=any_declarator: CONIC_SECTION_LATTICE_VECTOR_DECLARATOR@>
{

   @=$$@> = CONIC_SECTION_LATTICE_VECTOR_DECLARATOR;

};

@q ** (2) any_declarator --> HELIX_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{HELIX\_DECLARATOR}.
\initials{LDF 2005.05.18.}

\LOG
\initials{LDF 2005.05.18.}
Added this rule.

\initials{LDF 2005.11.07.}
Removed debugging code.
\ENDLOG

@<Define rules@>=
@=any_declarator: HELIX_DECLARATOR@>
{

   @=$$@> = HELIX_DECLARATOR;

};

@q ** (2) any_declarator --> HELIX_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{HELIX\_VECTOR\_DECLARATOR}.
\initials{LDF 2005.05.18.}

\LOG
\initials{LDF 2005.05.18.}
Added this rule.

\initials{LDF 2005.11.07.}
Removed debugging code.
\ENDLOG


@<Define rules@>=
@=any_declarator: HELIX_VECTOR_DECLARATOR@>
{

   @=$$@> = HELIX_VECTOR_DECLARATOR;

};

@q ** (2) any_declarator --> CONE_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{CONE\_DECLARATOR}.
\initials{LDF 2005.05.25.}

\LOG
\initials{LDF 2005.05.25.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=any_declarator: CONE_DECLARATOR@>
{


   @=$$@> = CONE_DECLARATOR;

};

@q ** (2) any_declarator --> CONE_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{CONE\_VECTOR\_DECLARATOR}.
\initials{LDF 2005.05.25.}

\LOG
\initials{LDF 2005.05.25.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: CONE_VECTOR_DECLARATOR@>
{

   @=$$@> = CONE_VECTOR_DECLARATOR;

};

@q ** (2) any_declarator --> CYLINDER_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{CYLINDER\_DECLARATOR}.
\initials{LDF 2005.05.25.}

\LOG
\initials{LDF 2005.05.25.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=any_declarator: CYLINDER_DECLARATOR@>
{

   @=$$@> = CYLINDER_DECLARATOR;

};

@q ** (2) any_declarator --> CYLINDER_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{CYLINDER\_VECTOR\_DECLARATOR}.
\initials{LDF 2005.05.25.}

\LOG
\initials{LDF 2005.05.25.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: CYLINDER_VECTOR_DECLARATOR@>
{

   @=$$@> = CYLINDER_VECTOR_DECLARATOR;

};

@q ** (2) any_declarator --> CUBOID_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{CUBOID\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: CUBOID_DECLARATOR@>
{

   @=$$@> = CUBOID_DECLARATOR;

};

@q ** (2) any_declarator --> CUBOID_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{CUBOID\_VECTOR\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: CUBOID_VECTOR_DECLARATOR@>
{

   @=$$@> = CUBOID_VECTOR_DECLARATOR;

};

@q ** (2) any_declarator --> POLYHEDRON_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{POLYHEDRON\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: POLYHEDRON_DECLARATOR@>
{

   @=$$@> = POLYHEDRON_DECLARATOR;

};

@q ** (2) any_declarator --> POLYHEDRON_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{POLYHEDRON\_VECTOR\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: POLYHEDRON_VECTOR_DECLARATOR@>
{

   @=$$@> = POLYHEDRON_VECTOR_DECLARATOR;

};

@q ** (2) any_declarator --> ELLIPSOID_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{ELLIPSOID\_DECLARATOR}.
\initials{LDF 2005.05.26.}

\LOG
\initials{LDF 2005.05.26.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=any_declarator: ELLIPSOID_DECLARATOR@>
{

   @=$$@> = ELLIPSOID_DECLARATOR;

};

@q ** (2) any_declarator --> SPHERE_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{SPHERE\_DECLARATOR}.
\initials{LDF 2005.06.06.}

\LOG
\initials{LDF 2005.06.06.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=any_declarator: SPHERE_DECLARATOR@>
{
   @=$$@> = SPHERE_DECLARATOR;
};

@q ** (2) any_declarator --> SPHERE_DEVELOPMENT_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{SPHERE\_DEVELOPMENT\_DECLARATOR}.
\initials{LDF 2009.11.04.}

\LOG
\initials{LDF 2009.11.04.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=any_declarator: SPHERE_DEVELOPMENT_DECLARATOR@>
{
   @=$$@> = SPHERE_DEVELOPMENT_DECLARATOR;
};


@q ** (2) any_declarator --> PARABOLOID_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{PARABOLOID\_DECLARATOR}.
\initials{LDF 2006.01.23.}

\LOG
\initials{LDF 2006.01.23.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=any_declarator: PARABOLOID_DECLARATOR@>
{
   @=$$@> = PARABOLOID_DECLARATOR;
};


@q ** (2) any_declarator --> GLYPH_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{GLYPH\_DECLARATOR}.
\initials{LDF 2005.06.08.}

\LOG
\initials{LDF 2005.06.08.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=any_declarator: GLYPH_DECLARATOR@>
{

   @=$$@> = GLYPH_DECLARATOR;

};

@q ** (2) any_declarator --> PLANE_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{PLANE\_DECLARATOR}.
\initials{LDF 2005.10.30.}

\LOG
\initials{LDF 2005.10.30.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=any_declarator: PLANE_DECLARATOR@>
{

   @=$$@> = PLANE_DECLARATOR;

};



@q ** (2) any_declarator --> ELLIPSOID_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{ELLIPSOID\_VECTOR\_DECLARATOR}.
\initials{LDF 2005.05.26.}

\LOG
\initials{LDF 2005.05.26.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: ELLIPSOID_VECTOR_DECLARATOR@>
{

   @=$$@> = ELLIPSOID_VECTOR_DECLARATOR;

};

@q ** (2) any_declarator --> SPHERE_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{SPHERE\_VECTOR\_DECLARATOR}.
\initials{LDF 2005.06.06.}

\LOG
\initials{LDF 2005.06.06.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=any_declarator: SPHERE_VECTOR_DECLARATOR@>
{

   @=$$@> = SPHERE_VECTOR_DECLARATOR;

};

@q ** (2) any_declarator --> SPHERE_DEVELOPMENT_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{SPHERE\_DEVELOPMENT\_VECTOR\_DECLARATOR}.
\initials{LDF 2009.11.04.}

\LOG
\initials{LDF 2009.11.04.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=any_declarator: SPHERE_DEVELOPMENT_VECTOR_DECLARATOR@>
{

   @=$$@> = SPHERE_DEVELOPMENT_VECTOR_DECLARATOR;

};


@q ** (2) any_declarator --> PARABOLOID_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{PARABOLOID\_VECTOR\_DECLARATOR}.
\initials{LDF 2006.01.23.}

\LOG
\initials{LDF 2006.01.23.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=any_declarator: PARABOLOID_VECTOR_DECLARATOR@>
{

   @=$$@> = PARABOLOID_VECTOR_DECLARATOR;

};

@q ** (2) any_declarator --> GLYPH_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{GLYPH\_VECTOR\_DECLARATOR}.
\initials{LDF 2005.06.08.}

\LOG
\initials{LDF 2005.06.08.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=any_declarator: GLYPH_VECTOR_DECLARATOR@>
{

   @=$$@> = GLYPH_VECTOR_DECLARATOR;

};

@q ** (2) any_declarator --> PLANE_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{PLANE\_VECTOR\_DECLARATOR}.
\initials{LDF 2005.10.30.}

\LOG
\initials{LDF 2005.10.30.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=any_declarator: PLANE_VECTOR_DECLARATOR@>
{

   @=$$@> = PLANE_VECTOR_DECLARATOR;

};




@q ** (2) any_declarator --> ELLIPSE_SLICE_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{ELLIPSE\_SLICE\_DECLARATOR}.
\initials{LDF 2005.03.29.}

\LOG
\initials{LDF 2005.03.29.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=any_declarator: ELLIPSE_SLICE_DECLARATOR@>
{

   @=$$@> = ELLIPSE_SLICE_DECLARATOR;

};

@q ** (2) any_declarator --> ELLIPSE_SLICE_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{ELLIPSE\_SLICE\_VECTOR\_DECLARATOR}.
\initials{LDF 2005.03.29.}

\LOG
\initials{LDF 2005.03.29.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=any_declarator: ELLIPSE_SLICE_VECTOR_DECLARATOR@>
{

   @=$$@> = ELLIPSE_SLICE_VECTOR_DECLARATOR;

};

@q ** (2) any_declarator --> CIRCLE_SLICE_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{CIRCLE\_SLICE\_DECLARATOR}.
\initials{LDF 2005.03.29.}

\LOG
\initials{LDF 2005.03.29.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=any_declarator: CIRCLE_SLICE_DECLARATOR@>
{
   @=$$@> = CIRCLE_SLICE_DECLARATOR;

};

@q ** (2) any_declarator --> CIRCLE_SLICE_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{CIRCLE\_SLICE\_VECTOR\_DECLARATOR}.
\initials{LDF 2005.03.29.}

\LOG
\initials{LDF 2005.03.29.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=any_declarator: CIRCLE_SLICE_VECTOR_DECLARATOR@>
{
   @=$$@> = CIRCLE_SLICE_VECTOR_DECLARATOR;

};

@q ** (2) any_declarator --> POLYHEDRON_SLICE_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{POLYHEDRON\_SLICE\_DECLARATOR}.
\initials{LDF 2005.03.29.}

\LOG
\initials{LDF 2005.03.29.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=any_declarator: POLYHEDRON_SLICE_DECLARATOR@>
{

   @=$$@> = POLYHEDRON_SLICE_DECLARATOR;

};

@q ** (2) any_declarator --> POLYHEDRON_SLICE_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{POLYHEDRON\_SLICE\_VECTOR\_DECLARATOR}.
\initials{LDF 2005.03.29.}

\LOG
\initials{LDF 2005.03.29.}
Added this rule.
\ENDLOG

@<Define rules@>=

@=any_declarator: POLYHEDRON_SLICE_VECTOR_DECLARATOR@>
{

   @=$$@> = POLYHEDRON_SLICE_VECTOR_DECLARATOR;

};

@q ** (2) any_declarator --> ORIGAMI_FIGURE_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{ORIGAMI\_FIGURE\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG


@<Define rules@>=
@=any_declarator: ORIGAMI_FIGURE_DECLARATOR@>
{
   @=$$@> = ORIGAMI_FIGURE_DECLARATOR;

};

@q ** (2) any_declarator --> ORIGAMI_FIGURE_VECTOR_DECLARATOR.@> 
@*1 \§any declarator> $\longrightarrow$ \.{ORIGAMI\_FIGURE\_VECTOR\_DECLARATOR}.
\initials{LDF 2004.12.21.}

\LOG
\initials{LDF 2004.12.21.}
Added this rule.
\ENDLOG

@<Define rules@>=
@=any_declarator: ORIGAMI_FIGURE_VECTOR_DECLARATOR@>
{
   @=$$@> = ORIGAMI_FIGURE_VECTOR_DECLARATOR;
};


@q * Emacs-Lisp code for use in indirect buffers when using the          @>
@q   GNU Emacs editor.  The local variable list is not evaluated when an @>
@q   indirect buffer is visited, so it's necessary to evaluate the       @>
@q   following s-expression in order to use the facilities normally      @>
@q   accessed via the local variables list.                              @>
@q   \initials{LDF 2004.02.12}.                                          @>
@q   (progn (cweb-mode) (outline-minor-mode t) (setq fill-column 70))    @>


@q Local Variables: @>
@q mode:CWEB  @>
@q eval:(outline-minor-mode t)  @>
@q abbrev-file-name:"~/.abbrev_defs" @>
@q eval:(read-abbrev-file)  @>
@q fill-column:70 @>
@q run-cweave-on-file:"3DLDFprg.web"  @>
@q End: @>
  
