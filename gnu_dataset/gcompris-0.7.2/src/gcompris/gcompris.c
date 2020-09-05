/* gcompris - gcompris.c
 *
 * Time-stamp: <2001/08/28 02:00:28 bruno>
 *
 * Copyright (C) 2000,2001 Bruno Coudoin
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
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "gcompris.h"

GtkWidget *window;
GtkWidget *drawing_area;
GnomeCanvas *canvas;
GnomeCanvas *canvas_bar;

static gint pause_board_cb (GtkWidget *widget, gpointer data);
static void quit_cb (GtkWidget *widget, gpointer data);
static void about_cb (GtkWidget *widget, gpointer data);
static void help_cb (GtkWidget *widget, gpointer data);
static gint end_board_box (void);
static gint board_widget_key_press_callback (GtkWidget   *widget,
					    GdkEventKey *event,
					    gpointer     client_data);


static GnomeUIInfo game_menu[] =
{
  GNOMEUIINFO_MENU_EXIT_ITEM (quit_cb, NULL),
  GNOMEUIINFO_END
};

static GnomeUIInfo help_menu[] = {
	GNOMEUIINFO_ITEM ("Help", NULL, help_cb, NULL),
	GNOMEUIINFO_MENU_ABOUT_ITEM (about_cb, NULL),
	GNOMEUIINFO_END
};

static GnomeUIInfo main_menu[] = {
	GNOMEUIINFO_MENU_GAME_TREE (game_menu),
	GNOMEUIINFO_MENU_HELP_TREE (help_menu),
	GNOMEUIINFO_END
};

GcomprisProperties	*properties = NULL;
GcomprisBoard		*gcomprisBoardMenu = NULL;

/****************************************************************************/
/* Some constants.  */

/* This message will be printed in the statusline, if no message was
   specified in a board file.  */

static char *default_message= "This software is a GNU Package";


static gint
board_widget_key_press_callback (GtkWidget   *widget,
				GdkEventKey *event,
				gpointer     client_data)
{

  switch (event->keyval)
    {
    case GDK_Escape:
      quit_cb(NULL, NULL);
      return TRUE;
    case GDK_KP_Enter:
    case GDK_Return:
      /* We make here a shortcup to OK 
	 Warning, a board that would need the enter key will never receive it */
      if (get_current_board_plugin()!=NULL && get_current_board_plugin()->ok)
	{
	  get_current_board_plugin()->ok ();
	}
      return TRUE;
    default:
      /* If the board needs to receive key pressed */
      if (get_current_board_plugin()!=NULL && get_current_board_plugin()->key_press)
	{
	  return(get_current_board_plugin()->key_press (event->keyval));
	}
    }

  return FALSE;
};

/**
 * Return the main canvas we run in
 */
GnomeCanvasGroup *gcompris_get_canvas()
{
  return canvas;
}

GnomeCanvasItem *gcompris_set_background(GnomeCanvasGroup *parent, gchar *file)
{
  GdkPixbuf *background_pixmap = NULL;
  static GnomeCanvasItem *backgroundimg = NULL;
  double xratio, yratio;
  guint yminus = BARHEIGHT+30;
  guint xminus = 30;
  double boardwidth, boardheight;

  background_pixmap = gcompris_load_pixmap (file);
  
  yratio=(gdk_screen_height()-yminus)/(float)gdk_pixbuf_get_height(background_pixmap);
  xratio=(gdk_screen_width()-xminus)/(float)gdk_pixbuf_get_width(background_pixmap);
  //  printf("The gdk_screen_width()=%f gdk_screen_height()=%f\n", 
  //	 (double)gdk_screen_width(), (double)gdk_screen_height());
  //  printf("The xratio=%f yratio=%f\n", xratio, yratio);

  yratio=xratio=MIN(xratio, yratio);

  /* The canvas does not look pretty when resized above 1 ratio. Avoid that */
  xratio=MIN(1.0, xratio);

  //  printf("Calculated x ratio xratio=%f\n", 
  //	 xratio);
  boardheight = gdk_pixbuf_get_height(background_pixmap);
  boardwidth = gdk_pixbuf_get_width(background_pixmap);

  gnome_canvas_set_pixels_per_unit (canvas, xratio);

  if(backgroundimg)
    gtk_object_destroy (GTK_OBJECT(backgroundimg));

  backgroundimg=gnome_canvas_item_new (parent,
				       gnome_canvas_pixbuf_get_type (),
				       "pixbuf", background_pixmap, 
				       "x", 0.0,
				       "y", 0.0,
				       "width", (double) boardwidth,
				       "height", (double) boardheight,
				       NULL);
  gdk_pixbuf_unref(background_pixmap);

  gnome_canvas_set_scroll_region (canvas,
				  0, 0,
				  boardwidth,
				  boardheight);

  gtk_widget_set_usize (GTK_WIDGET(canvas), boardwidth*xratio, boardheight*xratio);

  /* Create the spot for the bar */
  gnome_canvas_set_pixels_per_unit (canvas_bar, xratio);
  gnome_canvas_set_scroll_region (canvas_bar,
				  0, 0,
				  boardwidth,
				  BARHEIGHT);
  gtk_widget_set_usize (GTK_WIDGET(canvas_bar),  boardwidth*xratio,  BARHEIGHT*xratio);

  return (backgroundimg);
}

static void setup_window ()
{
  GtkWidget *vbox;
  GtkWidget *statusbar= NULL;

  /*  GdkCursor *hand_cursor; */

  window = gnome_app_new (PACKAGE, _("GCompris I Have Understood"));
  gtk_window_set_policy (GTK_WINDOW (window), FALSE, FALSE, TRUE);

  gtk_widget_realize (window);
  gtk_signal_connect (GTK_OBJECT (window), "delete_event",
		      GTK_SIGNAL_FUNC (quit_cb), NULL);
  gtk_signal_connect (GTK_OBJECT (window), "key_press_event",
		      GTK_SIGNAL_FUNC (board_widget_key_press_callback), 0);


  /*  hand_cursor = gdk_cursor_new(GDK_HAND2);
      gdk_window_set_cursor	 (window->window, hand_cursor);
      gdk_cursor_destroy(hand_cursor);
  */

  /* -------------- */

  /* For non anti alias canvas */
  gtk_widget_push_visual (gdk_rgb_get_visual ());
  gtk_widget_push_colormap (gdk_rgb_get_cmap ());

  /* For anti alias canvas */
  /*
  gtk_widget_push_visual(gdk_rgb_get_visual());
  gtk_widget_push_colormap(gdk_rgb_get_cmap());
  */

  /* For non anti alias canvas */
  canvas     = GNOME_CANVAS(gnome_canvas_new ());
  canvas_bar = GNOME_CANVAS(gnome_canvas_new ());

  /* For anti alias canvas */
  /*
  canvas     = GNOME_CANVAS(gnome_canvas_new_aa ());
  canvas_bar = GNOME_CANVAS(gnome_canvas_new_aa ());
  */

  /* Create a vertical box in which I put first the play board area, then the button bar */
  vbox = gtk_vbox_new (FALSE, 0);

  gnome_app_set_contents (GNOME_APP (window), GTK_WIDGET(vbox));

  gtk_box_pack_start (GTK_BOX (vbox), GTK_WIDGET(canvas), TRUE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX (vbox), GTK_WIDGET(canvas_bar), TRUE, TRUE, 0);

  gtk_widget_pop_colormap ();
  gtk_widget_pop_visual ();


  gtk_widget_show (GTK_WIDGET(vbox));
  gtk_widget_show (GTK_WIDGET(canvas));
  gtk_widget_show (GTK_WIDGET(canvas_bar));
  
  init_plugins();

  /* Load and Run the menu */
  gcomprisBoardMenu = gcompris_read_xml_file(PACKAGE_DATA_DIR "/menu.xml");
  if(!board_check_file(gcomprisBoardMenu))
    g_error("Cant't find the menu board or plugin execution error");

  /* Create the status bar */
  statusbar = gnome_appbar_new (TRUE, TRUE, GNOME_PREFERENCES_USER);

  /* Run the bar */
  gcompris_bar_start(canvas_bar, statusbar);

  board_play (gcomprisBoardMenu);

  /* Menu creation */  
  gnome_app_create_menus (GNOME_APP (window), main_menu);
  
  gnome_app_set_statusbar (GNOME_APP (window), statusbar);
  gnome_appbar_set_default (GNOME_APPBAR (statusbar), _(default_message));

  gnome_app_install_menu_hints (GNOME_APP (window), main_menu);
  
}

/* It implements gcompris's own way to load help in order to be
 * Package relocatable.
 * Unfortunatly, this does not supports I18N
 */
static void help_cb (GtkWidget *widget, gpointer data)
{
  gnome_help_goto(NULL, PACKAGE_HELP_DIR "/C/gcompris.html");
}

static void about_cb (GtkWidget *widget, gpointer data)
{
  static GtkWidget *about;


  const gchar *authors[] = {
    "Bruno Coudoin <bruno.coudoin@free.fr>",
    NULL
  };

  if (about != NULL) {
    gdk_window_raise (about->window);
    gdk_window_show (about->window);
    return;
  }

  about = gnome_about_new ("GCompris", VERSION,
                        ("Copyright 2000,2001 Bruno Coudoin (Released under the GPL)"),
                        authors,
                        _("A simple educational board based game for children starting at 3. This software is a GNU Package"),
                        PACKAGE_DATA_DIR "/gcompris/gcompris-about.jpg");

  gtk_object_set_data (GTK_OBJECT (about), "about", about);
  gtk_window_set_modal (GTK_WINDOW (about), TRUE);

  gtk_signal_connect (GTK_OBJECT (about), "destroy", GTK_SIGNAL_FUNC
		      (gtk_widget_destroyed), &about);
  gnome_dialog_set_parent (GNOME_DIALOG (about), GTK_WINDOW (window));
  
  gtk_widget_show (about);
}

/*
 * Confirmation dialog
 */
static gint end_board_box ()
{
  static GtkWidget *box;
  gint status;

  if (box)
    return 0;

  board_pause();

  box = gnome_message_box_new (
			       _("Do you really want to quit GCompris?"),
			       GNOME_MESSAGE_BOX_QUESTION,
			       GNOME_STOCK_BUTTON_YES, GNOME_STOCK_BUTTON_NO,
			       NULL);
  gnome_dialog_set_parent (GNOME_DIALOG (box), GTK_WINDOW
			   (window));
  gnome_dialog_set_default (GNOME_DIALOG (box), 0);
  status = gnome_dialog_run (GNOME_DIALOG (box));
  box = NULL;
  
  board_pause();

  return (status);
}

static void cleanup()
{
  gcomprisBoardMenu->plugin->end_board ();
}

static gint pause_board_cb (GtkWidget *widget, gpointer data)
{
  board_pause();
  return(TRUE);
}

void gcompris_end_board()
{
  if (get_current_gcompris_board()->previous_board == NULL)
    {
      /* We are in the upper menu: leave GCompris? */
      quit_cb(NULL, NULL);

      /* Oups, the user changed his mind : restart the menu */
      board_play (get_current_gcompris_board());
      return;
    }

  /* Run the previous board */
  board_play (get_current_gcompris_board()->previous_board);
}

static void quit_cb (GtkWidget *widget, gpointer data)
{
  if (end_board_box ())
    return;

  /*  cleanup();
      cleanup_plugins(); */
  gcompris_properties_save(properties);

  gtk_main_quit ();

}

static void load_properties ()
{
  properties = gcompris_properties_new ();
}

int
main (int argc, char *argv[])
{

  srand (time (NULL));

  bindtextdomain (PACKAGE, PACKAGE_LOCALE_DIR);
  textdomain (PACKAGE);

  gnome_init ("GCompris", VERSION, argc, argv);

  /* Gdk-Pixbuf */
  gdk_rgb_init();

  load_properties ();

  setup_window ();

  gtk_widget_show (window);

  gtk_main ();
  return 0;
}


/* Local Variables: */
/* mode:c */
/* eval:(load-library "time-stamp") */
/* eval:(make-local-variable 'write-file-hooks) */
/* eval:(add-hook 'write-file-hooks 'time-stamp) */
/* eval:(setq time-stamp-format '(time-stamp-yyyy/mm/dd time-stamp-hh:mm:ss user-login-name)) */
/* End: */
