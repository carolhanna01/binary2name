@q cmpxassn.w @> 
@q Created by Laurence Finston (LDF) Sun Dec  2 19:16:43 CET 2007 @>

@q * (0) Copyright and License.@>

@q This file is part of GNU 3DLDF, a package for three-dimensional drawing. @>
@q Copyright (C) 2007, 2008, 2009, 2010, 2011, 2012, 2013 The Free Software Foundation  @>

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


@q **** (4) complex assignments.@>
@*3 complex assignments. 
\initials{LDF 2007.12.02.}

@q ***** (5) assignment --> complex_variable := complex_expression.@>   
@*4 \§assignment> $\longrightarrow$ \§complex variable> 
\.{:=} \§complex expression>. 
\initials{LDF 2007.12.02.}

\LOG
\initials{LDF 2007.12.02.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>= 
 
@=assignment: complex_variable ASSIGN complex_expression@>
{

  Complex* a = static_cast<Complex*>(@=$3@>);

  Int_Void_Ptr ivp = 
     Scan_Parse::assign_simple<Complex>(static_cast<Scanner_Node>(parameter),
                                       "Complex",
                                       @=$1@>,
                                       a);

  @=$$@> = ivp.v;

};

@q ***** (5) assignment --> complex_variable ASSIGN numeric_list @>   
@q ***** (5) RIGHT_PARENTHESIS                                   @>   

@*4 \§assignment> $\longrightarrow$ \§complex variable> 
\.{ASSIGN} \§numeric list>.
\initials{LDF 2007.12.04.}

\LOG
\initials{LDF 2007.12.04.}
Added this rule.
\ENDLOG

@q ****** (6) Definition.@> 

@<Define rules@>= 
 
@=assignment: complex_variable ASSIGN numeric_list@>
{
  Pointer_Vector<real>* pv = static_cast<Pointer_Vector<real>*>(@=$3@>); 

  if (pv && pv->v.size() > 0)
  {
      Complex* c = create_new<Complex>(0);

      real r = 0;
      real i = 0;

      if (pv->v.size() > 0)
         r = *pv->v[0];

      if (pv->v.size() > 1)
         i = *pv->v[1];

      delete pv;

      Scanner_Node scanner_node = static_cast<Scanner_Node>(parameter);

      c->set(r, i, scanner_node);

      Int_Void_Ptr ivp = 
         Scan_Parse::assign_simple<Complex>(scanner_node,
                                           "Complex",
                                           @=$1@>,
                                           c);

      @=$$@> = ivp.v;
  }
  else
  {
 
     delete pv;

     @=$$@> = static_cast<void*>(0);
  }

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

