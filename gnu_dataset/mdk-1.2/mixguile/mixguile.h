/* -*-c-*- ---------------- mixguile.h :
 * Interface to the mixguile interpreter.
 * ------------------------------------------------------------------
 *  $Id: mixguile.h,v 1.5 2001/09/28 23:10:45 jao Exp $
 * ------------------------------------------------------------------
 * Copyright (C) 2001 Free Software Foundation, Inc.
 *  
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *  
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *  
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *  
 */


#ifndef MIXGUILE_H
#define MIXGUILE_H

#include <mixlib/mix.h>
#include <mixlib/mix_vm_command.h>
#include <guile/gh.h> 

/* the main function type */
typedef void (*main_func_t) (int argc, char *argv[]);


/* enter and do the initialisation manually inside the guile world */
#define mixguile_enter(argc,argv,main_fun) gh_enter (argc, argv, main_fun)

/* load mixguile startup file */
extern void
mixguile_load_bootstrap (gboolean localinit);

/*
  initialise the guile command dispatcher and enter the provided
  main function.
*/
extern void
mixguile_init (int argc, char *argv[], gboolean initfile, main_func_t main_fun,
	       mix_vm_cmd_dispatcher_t *dis);

/* set the command dispatcher */
extern void
mixguile_set_cmd_dispatcher (mix_vm_cmd_dispatcher_t *dis);

/* enter the guile repl */
extern void
mixguile_enter_repl (int argc, char *argv[]);

/* access the comand dispatcher */
extern mix_vm_cmd_dispatcher_t *
mixguile_get_cmd_dispatcher (void);

/* execute a string or file using the guile interpreter */
extern void
mixguile_interpret_file (const gchar *path);

extern void
mixguile_interpret_command (const gchar *command);


#endif /* MIXGUILE_H */

