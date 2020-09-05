#ifndef CUSTOMIZE_H
#define CUSTOMIZE_H

/* 
   customize.H 
   include file for xlogmaster customize menu
*/


/*{{{  Variables  */
/* this variable blocks the interrupt handlers for
   reconfiguration */
static gboolean block_handlers;

/* for main structure */
static Log* last_entry;
static gint last_row;

/* for filter structure */
static Filter* last_filter;
static gint last_filter_row;
static gboolean block_filter_handlers;


/*}}}*/

/*{{{  GTK+ based variables  */
static GtkWidget* form;
static GtkTooltips* tooltips;

static GtkWidget* apply_button;
static GtkWidget* cancel_button;
GtkWidget* database_button;
static GtkWidget* add_button;
static GtkWidget* move_up_button;
static GtkWidget* move_down_button;
static GtkWidget* delete_button;

static GtkWidget *entry_list, *filter_list;

static GtkWidget *button_text_entry, *help_text_entry;
static GtkWidget *filename_entry, *select_filename_button;
static GtkWidget *plugin_entry, *select_plugin_button;
static GtkWidget *interval_selection, *delay_selection;
static GtkWidget *tail_button, *cat_button, *run_button;
static GtkWidget *file_selection, *plugin_selection;

static GtkWidget *add_filter_button, *delete_filter_button;
static GtkWidget *filter_string, *execute_string;
static GtkWidget *invert_button, *case_button;
static GtkWidget *raise_button, *lower_button, *hide_button;
static GtkWidget *alert_button, *notice_button, *uniconify_button;
static GtkWidget *execute_button;

/*}}}*/

/*{{{  Tags  */





/*}}}*/

/*{{{  Subroutines  */

/* startup_customize */
void startup_customize(Log**);
static void attach_label(GtkWidget*, gchar*, gint, gint, gint, gint, gfloat);
static GtkWidget* entry_box(GtkWidget*, gchar*, gint, gint, gint, gint, gchar*);

/* terminate customize */
static void form_popdown (GtkWidget *, gpointer *);
static void delete_clist_log_element(gpointer data);

/* customization main routines - fileselection */
static void select_file(GtkWidget*, gpointer*);
static void select_plugin(GtkWidget*, gpointer*);
static GtkWidget* do_selection(GtkWidget*);
static void filesel_ok_selected (GtkWidget *, GtkFileSelection *);
static void filesel_cancel_selected (GtkWidget *, GtkFileSelection *);

/* customization main routines */
static void accept_values(GtkWidget*, gpointer*);
static void select_entry_row_callback( GtkWidget* , gint , gint , GdkEventButton* , gpointer );
static void unselect_entry_row_callback( GtkWidget* , gint , gint , GdkEventButton* , gpointer );
static void set_fields();
static void get_fields();
static void clear_all_fields();

/* Main Buttons */
static void set_general_sensitivity(gboolean);
static void insert_new_entry(GtkWidget*, gpointer);
static void move_entry_up(GtkWidget*, gpointer);
static void move_entry_down(GtkWidget*, gpointer);
static void delete_entry(GtkWidget*, gpointer);
static void customization_done(GtkWidget*, gpointer);
static void customization_abort(GtkWidget*, gpointer);

/* Database Interaction */
static void open_database(GtkWidget*, gpointer);
void remove_all_entries();
void add_entry_from_database(Log*);

/* Filter Initialisation & Evaluation */
static void set_filter_sensitive(gboolean);
static gint compare_filter_clist_elements (GtkCList*, gconstpointer, gconstpointer);
static void delete_clist_filter_element(gpointer);
static void setup_filter();
static void insert_entry(Filter*, gint);
static void determine_columns(Filter*, gchar**);
static void retrieve_filter();

/* Filter Menu Maintenance Routines */
static void select_filter_entry_row_callback(GtkWidget*,gint,gint,GdkEventButton*,gpointer);
static void unselect_filter_entry_row_callback(GtkWidget*,gint,gint,GdkEventButton*,gpointer);
static void accept_filter_values(GtkWidget*, gpointer);
static void set_filter_fields();
static void get_filter_fields(gint);
static void clear_all_filter_fields();

/* Filter Buttons */
static void add_filter(GtkWidget*, gpointer);
static void delete_filter(GtkWidget*, gpointer);

/*}}}*/


#endif /* CUSTOMIZE_H */

