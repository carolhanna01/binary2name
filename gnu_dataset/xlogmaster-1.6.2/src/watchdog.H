#ifndef WATCHDOG_H
#define WATCHDOG_H

/* 
   watchdog.H
   include file for xlogmaster background status checks
*/


/*{{{  Variables  */
// GTK based:




// others:
/* 
   Class 0 filters are filters that apply at display-time 
*/
gint class0_filters[] = {RAISE,LOWER,HIDE,-1};
/* 
   Class 1 filters are filters that apply "during background"
   and require to keep the logfile open even if it isn't being
   watched at the moment 
*/
gint class1_filters[] = {ALERT,NOTICE,UNICONIFY,EXECUTE,-1};


/*
  The buffer for the fstat calls..
*/
struct stat stat_buffer;

/*}}}*/

/* Subroutines: */

/*{{{  shared routines  */
gint reopen_logfile(gint);
static gint position_within_boundaries(gint, gint);
glong read_from_filedescriptor(gint, off_t = -1);

/*}}}*/

/*{{{  execution subroutines  */
gint run_program(gint);


/*}}}*/

/*{{{  text subroutines  */
static void output_tail_mode(gchar*, glong);
static void output_run_mode(gchar*, glong, gint);
static void output_cat_mode(gchar*, glong);

static void append_text(gchar*, glong);
static void replace_text(gchar*, glong);

/*}}}*/

/*{{{  Watchdog  */
void start_watchdog();
void stop_watchdog();

static gint watchdog_tail_interrupt(gpointer);
static gint watchdog_run_interrupt(gpointer);
static gint watchdog_run_restart_interrupt(gpointer);
static gint watchdog_cat_interrupt(gpointer);
/*}}}*/

/*{{{  Class 1 routines  */
static void process_class1_filters(gint, glong);

/*}}}*/

/*{{{  Watchdog error subroutines  */
static void watchdog_file_error(gint);
static void watchdog_run_error(gint);
/*}}}*/

/*{{{  logswitching routines  */
void activate();
void deactivate();
/*}}}*/

/*{{{  Class 0 Logfile handling  */
static gint tail_interrupt(gpointer);
static gint cat_interrupt(gpointer);
static gint  run_interrupt(gpointer);
static gint run_restart_interrupt(gpointer);
/*}}}*/

#endif /* WATCHDOG_H */
