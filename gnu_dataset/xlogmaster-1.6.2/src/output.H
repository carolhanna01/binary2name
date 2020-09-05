#ifndef OUTPUT_H
#define OUTPUT_H

/* 
   output.H
   include file for xlogmaster output filter/plugin routines
*/


/*{{{  Variables  */




/*}}}*/

/*{{{  Subroutines  */

glong pipe_magic(gint, gchar*, glong, glong);
static gint pipe_magic_interrupt(gpointer);
static void assure_plugin_running(gint);
void deactivate_plugin(gint);

/*}}}*/


#endif /* OUTPUT_H */

