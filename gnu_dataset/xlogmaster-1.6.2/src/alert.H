#ifndef ALERT_H
#define ALERT_H

/* 
   alert.H
   include file for xlogmaster notifying
*/


/*{{{  Variables  */

/* GtkBased: */
static GtkWidget* notice_dialog;
static GtkWidget* text_field;
static GtkWidget* notice_list;
static GtkWidget* dismiss_button;
static GtkWidget* notice_scrolled_window;




#define LINE_TAG "xlogmaster::alert::notice::line"

/* Color fading: */
/* base for fading */
gdouble fade_base = 10000;
/* amount of steps to fade back with to old color */
gint steps = XLM_FADE_STEPS;
/* the stepsize for the fading */
gdouble fade_step =  (glong) (-1.0 * ( (gdouble) fade_base / ( 10 * XLM_FADE_SECONDS )));

/*}}}*/

/*{{{  Subroutines  */
void trigger_alert(int);
void remove_alert(int);
static int alert_interrupt(gpointer);

void create_notice_window();
void notice_taken(GtkWidget *, gpointer *);
void notice_item_select(GtkList*, GtkWidget*);
void destroy_notice_entry(GtkWidget *, gpointer *);
void notice_alert(gchar*, gchar*);
/*}}}*/


#endif /* ALERT_H */
