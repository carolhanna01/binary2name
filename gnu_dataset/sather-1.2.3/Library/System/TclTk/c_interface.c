/*------------------------->  ANSI C - sourcefile  <-------------------------*/
/* Copyright (C) 199x by International Computer Science Institute            */
/* This file is part of the GNU Sather library. It is free software; you may */
/* redistribute  and/or modify it under the terms of the GNU Library General */
/* Public  License (LGPL)  as published  by the  Free  Software  Foundation; */
/* either version 3 of the license, or (at your option) any later version.   */
/* This  library  is distributed  in the  hope that it will  be  useful, but */
/* WITHOUT ANY WARRANTY without even the implied warranty of MERCHANTABILITY */
/* or FITNESS FOR A PARTICULAR PURPOSE. See Doc/LGPL for more details.       */
/* The license text is also available from:  Free Software Foundation, Inc., */
/* 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA                     */
/*------------>  Please email comments to <bug-sather@gnu.org>  <------------*/

#include <tcl.h>
#include <tk.h>
#include "tkRaster.h"

int to_int(Tcl_Interp *tcl) {
  return ((int) tcl);
}

int ConstTclOk() {
  return TCL_OK;
}

int ConstTclError() {
  return TCL_ERROR;
}

void *ConstTclVolatile() {
  return TCL_VOLATILE;
}

void delete_proc(ClientData client_data) {}

int tkkit_cb_cmd(ClientData user_data, Tcl_Interp *tcl, int argc, char **argv) {
  C_TCL_TK_tkkit_cb(argv);
}

int sather_cmd(ClientData user_data, Tcl_Interp *tcl, int argc, char **argv) {
  return C_TCL_TK_sather_cb((int) tcl, argv);
}

void init_tcl_tk(Tcl_Interp *tcl) {
  /*  Initialize the two new tcl commands tkkit_cb and sather */
  Tcl_CreateCommand(tcl, "tkkit_cb", tkkit_cb_cmd, NULL, delete_proc);
  Tcl_CreateCommand(tcl, "sather", sather_cmd, NULL, delete_proc);
}

Tk_Window get_main_window(Tcl_Interp *tcl, char *basename, char *classname) {
  Tk_Window result;
  Tk_Init(tcl);
  result = Tk_MainWindow(tcl);
  Tk_SetClass(result, classname);
  return result;
}

void init_raster(Tcl_Interp *tcl) {
  /* Initialize the raster commands */
  Tk_AddRaster(tcl);
  RasterInit(tcl);
}

char *str_ind(char **argv, int i) {
  return argv[i];
}

char *Tcl_GetResult(Tcl_Interp *interpreter) {
  return interpreter->result;
}

int Tk_AddRaster(Tcl_Interp *interp)
{
    Tk_Window main;
 
    main = Tk_MainWindow(interp);
    if ( main == NULL ) {
      return TCL_ERROR;
    }
    if (RasterInit(interp) == TCL_ERROR) {
       return TCL_ERROR;
    }
 
    Tcl_CreateCommand(interp, "raster", RasterCmd, (ClientData) main,
            (void (*)()) NULL);
    Tcl_SetVar(interp,"tcl_rcFileName","~/.wishrc",0);
    return TCL_OK;
}

int Tcl_EvalFile_wrapper(Tcl_Interp *interp, char *fileName)
{
#ifdef __CYGWIN32__
    if(filename[1] == ':')
#endif
	return Tcl_EvalFile(interp,fileName);
#ifdef __CYGWIN32__
    else
    {
	int i;
	char *buf = malloc(length(fileName) + 4);
	int res;
	buf[0] = 'c'; buf[1] = ':'; buf[2] = '\\';
	for(int i=0; fileName[i]; i++)
	    buf[i+3] = fileName[i]=='/'?'\\':fileName[i];
	buf[i+3] = 0;
	res = Tcl_EvalFile(interp,buf);
	free(buf);
	return res;
    }
#endif
}
