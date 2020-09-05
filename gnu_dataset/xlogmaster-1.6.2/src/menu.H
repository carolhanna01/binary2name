#ifndef MENU_H
#define MENU_H

/* 
   menu.h
   include file for xlogmaster menus
*/

/*{{{  Variables  */
// PopUp menu:
/* these are the widget that are being used in the
   pop-up menu */
static GtkWidget* menu;
static GtkWidget* load_config;
static GtkWidget* save_config;
static GtkWidget* customize_entry;
static GtkWidget* quit_menu;
static GtkWidget* about;
static GtkWidget* separator1;
static GtkWidget* separator2;
static GtkWidget* separator3;

// Fileselector
static GtkWidget *filew;


/* Flag to tell us whether the customization is already running */
gint customizing;

/*}}}*/

/*{{{  Subroutines  */
void fire_up_menu(GtkWidget*);
static gint popup_handler(GtkWidget*, GdkEventButton*, GtkWidget*);
void destroy (GtkWidget *, gpointer *);
void hide_show_buttons_proc();
void load_conf_proc(GtkWidget*, gpointer*);
static void do_load();
void file_ok_sel (GtkWidget *, GtkFileSelection *);
void file_cancel_sel (GtkWidget *, GtkFileSelection *);
void save_conf_proc(GtkWidget*, gpointer*);
void file_save_ok (GtkWidget*, GtkFileSelection*);
void customize_entry_proc(GtkWidget*, gpointer*);
void about_proc(GtkWidget*, gpointer*);

/*}}}*/

#endif /* MENU_H */
