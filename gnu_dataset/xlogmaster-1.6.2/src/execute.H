#ifndef EXECUTE_H
#define EXECUTE_H

/* 
   execute.H
   include file for xlogmaster execution routines
*/


/*{{{  Variables  */

gint cleanup_tag;
gint pid_reference;
GList* pid_list;

/*}}}*/

/*{{{  Subroutines  */
void execute_program(gint, gint, gchar*);
gchar** split_execute_line(gchar*);
gint my_setenv(const gchar*, const gchar*);
static gchar* substitute(gchar*, gchar*, gchar*);

/* overall subroutines for execution & child handling */
void reference_child(gint);
gint check_terminate(gint);
static gint cleanup_interrupt(gpointer);


/*}}}*/


#endif /* EXECUTE_H */
