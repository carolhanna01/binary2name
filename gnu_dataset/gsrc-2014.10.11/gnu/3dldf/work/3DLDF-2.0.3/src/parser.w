@q parser.w @> 
@q Created by Laurence Finston Thu Jan 29 19:09:21 MET 2004  @>
     
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




@q * (0) Parsing.  @>
@** Parsing\quad ({\tt parser\PERIOD w}).\hfil

@q * (1) Declarations.  @>
  
@i pbsndecl.w

@q * (1) Rules.  @> 
@* Rules.

@q ** (2) Start symbol.  Statements.  `pprgstmt.w'  @>  

@i pprgstmt.w

@q * (1) Options.  `poptions.w'  @>
  
@i poptions.w

@q ** (2) Assignments.  `passign.w'  @>
@i  passign.w

@q *** (3) Complex Assignments.  `cmpxassn.w'  @>

@i cmpxassn.w

@q *** (3) Matrix Assignments.  `mtrxassn.w'  @>
@i  mtrxassn.w

@q ** (2) Operation Assignments.  `popassgn.w'  @>
@i  popassgn.w

@q ** (2) Intersection Assignment Commands.  `pintacmd.w'  @>
@q ** (2) LDF 2004.11.05.  Commented-out.@> 

@q pintacmd.w@>

@q ** (2)  Variables.  @>
@i pvariabl.w

@q ** (2) Commands.  `pcommand.w' @>
@i pcommand.w

@q ** (2) Drawing Commands.  `pdrwcmnd.w' @>
@i pdrwcmnd.w

@q ** (2) Clearing Commands.  `pclrcmnd.w' @>
@i pclrcmnd.w

@q ** (2) Group Commands.  `pgrpcmnd.w' @>
@i pgrpcmnd.w

@q ** (2) Input Command.  `pinptcmd.w' @>

@i pinptcmd.w

@q ** (2) Transformation Commands.  `ptrfcmnd.w'@>

@i ptrfcmnd.w

@q ** (2) Vector Manipulation Commands.  `pvcmncmd.w'@>

@i pvcmncmd.w


@q ** (2) Reverse Command.  `reverse.w'@>

@i reverse.w

@q START HERE!! Conditionally compiling "Common code" section, where possible.@>
@q LDF 2005.10.20.                                                            @>
@q In the middle of the following file.  LDF 2005.10.26.@>

@q ** (2) Set Commands.  `psetcmnd.w' @>

@i psetcmnd.w  

@q *** (3) Set Matrix Commands.  `setmatrx.w' @>

@i setmatrx.w  

@q *** (3) Set Arc Commands.  `setarc.w' @>

@i setarc.w  

@q ** (2) Label Commands.  `plblcmnd.w' @>

@i plblcmnd.w

@q ** (2) Figure and Output Commands.  `pfgopcmd.w' @>

@i pfgopcmd.w

@q ** (2) Focus Commands.  `fcscmnd.w' @>

@i fcscmnd.w

@q ** (2) Conic Section Lattice Commands.  `cslcmnd.w' @>

@i cslcmnd.w

@q ** (2) Show Commands.  `pshowcmd.w' @>
@i pshowcmd.w

@q ** (2) Conditionals.  `pcondit.w' @>
@i pcondit.w

@q ** (2) Loops.  `ploops.w' @>
@i ploops.w

@q ** (2) Equations.  @>

@i peqexpr.w

@q ** (2) Types of expressions.@>
@*1 Types of expressions.

@q *** (3) boolean expressions.  `pblexpr.w'.  @>

@i pblexpr.w

@q *** (3) numeric expressions.  `pnumexpr.w'.  @>

@i pnumexpr.w

@q *** (3) unsigned long long expressions.  `pullexpr.w'.  @>

@i pullexpr.w

@q *** (3) matrix expressions.  `mtrxexpr.w'.  @>

@i mtrxexpr.w

@q *** (3) complex expressions.  `cmpxexpr.w'.  @>

@i cmpxexpr.w

@q *** (3) string expressions.  `pstrexpr.w'.  @>

@i pstrexpr.w

@q *** (3) pen expressions.  `ppenexpr.w'.  @>

@i ppenexpr.w

@q *** (3) dash_pattern expressions.  `pdshexpr.w'.  @>

@i pdshexpr.w

@q *** (3) color expressions.  `pcolexpr.w'.  @>

@i pcolexpr.w

@q *** (3) transform expressions.  `ptrnexpr.w'.  @>

@i ptrnexpr.w


@q *** (3) picture expressions.  `ppicexpr.w'.  @>

@i ppicexpr.w

@q *** (3) point expressions.  `ppntexpr.w'.  @>

@i ppntexpr.w

@q *** (3) bool_point expressions.  `pbpexpr.w'.  @>

@i pbpexpr.w

@q *** (3) focus expressions.  `pfocexpr.w'.  @>

@i pfocexpr.w

@q *** (3) nurb expressions.  `pnrbexpr.w'.  @>

@i pnrbexpr.w

@q *** (3) path expressions.  `ppthexpr.w'.  @>

@i ppthexpr.w

@q *** (3) ellipse expressions.  `pellexpr.w'.  @>

@i pellexpr.w

@q *** (3) circle expressions.  `pcirexpr.w'.  @>

@i pcirexpr.w

@q *** (3) ellipse_slice expressions.  `pellpslc.w'.  @>

@i pellpslc.w

@q *** (3) circle_slice expressions.  `pcrclslc.w'.  @>

@i pcrclslc.w

@q *** (3) parabola expressions.  `pprbexpr.w'.  @>

@i pprbexpr.w

@q *** (3) hyperbola expressions.  `phypexpr.w'.  @>

@i phypexpr.w

@q *** (3) conic_section_lattice expressions.  `cslexpr.w'.  @>

@i cslexpr.w

@q *** (3) arc expressions.  `arcexpr.w'.  @>

@i arcexpr.w

@q *** (3) helix expressions.  'phlxexpr.w'.  @>

@i phlxexpr.w

@q *** (3) polygon expressions.  `ppygexpr.w'.@>

@i ppygexpr.w

@q *** (3) rectangle expressions.  `precexpr.w'.  @>

@i precexpr.w

@q *** (3) reg_polygon expressions.  `prplexpr.w'.  @>

@i prplexpr.w

@q *** (3) triangle expressions.  `ptglexpr.w'.  @>

@i ptglexpr.w

@q *** (3) cone expressions.  'pconexpr.w'.  @>

@i pconexpr.w

@q *** (3) cylinder expressions.  'pcylexpr.w'.  @>

@i pcylexpr.w

@q *** (3) cuboid expressions.  `pcubexpr.w'.  @>

@i pcubexpr.w

@q *** (3) polyhedron expressions.  `pphdexpr.w'.  @>

@i pphdexpr.w

@q *** (3) polyhedron_slice expressions.  `pplhdslc.w'.  @>

@i pplhdslc.w

@q *** (3) ellipsoid expressions.  'peldexpr.w'.  @>

@i peldexpr.w

@q *** (3) sphere expressions.  'psphexpr.w'.  @>

@i psphexpr.w

@q *** (3) sphere_development expressions.  'spdvexpr.w'.  @>

@i spdvexpr.w

@q *** (3) paraboloid expressions.  'pprdexpr.w'.  @>

@i pprdexpr.w

@q *** (3) glyph expressions.  'pglyexpr.w'.  @>

@i pglyexpr.w

@q *** (3) plane expressions.  'pplnexpr.w'.  @>

@i pplnexpr.w

@q *** (3) origami_figure expressions.  `porgexpr.w'.  @>

@i porgexpr.w

@q ** (2) Vector-type expressions.@>

@q *** (3) Non-|Shape| types.@>

@q **** (4) boolean_vector expressions.  `pblvexpr.w'.  @>

@i pblvexpr.w

@q **** (4) numeric_vector expressions.  `pnmvexpr.w'.  @>

@i pnmvexpr.w

@q *** (3) unsigned long long vector expressions.  `pullvxpr.w'.  @>

@i pullvxpr.w

@q *** (3) matrix vector expressions.  `mtrxvxpr.w'.  @>

@i mtrxvxpr.w

@q *** (3) complex vector expressions.  `cmpxvxpr.w'.  @>

@i cmpxvxpr.w

@q **** (4) string_vector expressions.  `pstvexpr.w'.  @>

@i pstvexpr.w

@q **** (4) color_vector expressions.  `pclvexpr.w'.  @>

@i pclvexpr.w

@q **** (4) pen_vector expressions.  `ppnvexpr.w'.  @>

@i ppnvexpr.w 

@q **** (4) dash_pattern_vector expressions.  `pdpvexpr.w'.  @>

@i pdpvexpr.w 

@q **** (4) transform_vector expressions.  `ptfvexpr.w'.  @>

@i ptfvexpr.w 

@q **** (4) focus_vector expressions.  `pfcvexpr.w'.  @>

@i pfcvexpr.w 

@q *** (3) |Shape| types.@>

@q **** (4) point_vector expressions.  `pptvexpr.w'.  @>

@i pptvexpr.w

@q **** (4) bool_point_vector expressions.  `pbpvexpr.w'.  @>

@i pbpvexpr.w

@q *** (3) nurb vector expressions.  `pnbvexpr.w'.  @>

@i pnbvexpr.w

@q **** (4) path_vector expressions.  `pphvexpr.w'.  @>

@i pphvexpr.w

@q **** (4) ellipse_vector expressions.  `pelvexpr.w'.  @>

@i pelvexpr.w

@q **** (4) circle_vector expressions.  `pcrvexpr.w'.  @>

@i pcrvexpr.w

@q *** (3) parabola_vector expressions.  `ppbvexpr.w'.  @>

@i ppbvexpr.w

@q *** (3) hyperbola_vector expressions.  `phpvexpr.w'.  @>

@i phpvexpr.w

@q *** (3) arc vector expressions.  `arcvexpr.w'.  @>

@i arcvexpr.w

@q *** (3) conic_section_lattice_vector expressions.  `cslvexpr.w'.  @>

@i cslvexpr.w

@q *** (3) helix_vector expressions.  `phxvexpr.w'.  @>

@i phxvexpr.w

@q **** (4) polygon_vector expressions.  `ppgvexpr.w'.  @>

@i ppgvexpr.w

@q **** (4) rectangle_vector expressions.  `prcvexpr.w'.  @>

@i prcvexpr.w

@q **** (4) reg_polygon_vector expressions.  `prpvexpr.w'.  @>

@i prpvexpr.w

@q *** (3) triangle_vector expressions.  `ptgvexpr.w'.  @>

@i ptgvexpr.w

@q *** (3) cone vector expressions.  'pcnvexpr.w'.  @>

@i pcnvexpr.w

@q *** (3) cylinder vector expressions.  'pcyvexpr.w'.  @>

@i pcyvexpr.w

@q **** (4) cuboid_vector expressions.  `pcbvexpr.w'.  @>

@i pcbvexpr.w

@q **** (4) polyhedron_vector expressions.  `pplvexpr.w'.  @>

@i pplvexpr.w

@q *** (3) ellipsoid vector expressions.  'pedvexpr.w'.  @>

@i pedvexpr.w

@q *** (3) sphere vector expressions.  'pspvexpr.w'.  @>

@i pspvexpr.w

@q *** (3) sphere_development vector expressions.  'spdvvexp.w'.  @>

@i spdvvexp.w

@q *** (3) paraboloid vector expressions.  'ppdvexpr.w'.  @>

@i ppdvexpr.w

@q *** (3) glyph vector expressions.  'pglvexpr.w'.  @>

@i pglvexpr.w 

@q *** (3) plane vector expressions.  'pplnvxpr.w'.  @>

@i pplnvxpr.w


@q *** (3) origami_figure_vector expressions.  `porvexpr.w'.  @>

@i porvexpr.w

@q ** (2) Declarations.  `pldfdcl.w'.  @>

@i pldfdcl.w

@q ** (2) Tokens and Declarators.  @>

@i ptokdecl.w

@q ** (2) Macros.  `pmacros.w'.  @>

@i pmacros.w 

@q * (1) Additional C++ code. @>

@i pfuncfin.w


  
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
