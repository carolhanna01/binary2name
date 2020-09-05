/* gcompris - gameutil.c
 *
 * Time-stamp: <2001/08/12 23:31:19 bruno>
 *
 * Copyright (C) 2000 Bruno Coudoin
 * 
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

/* libxml includes */
#include <gnome-xml/tree.h>
#include <gnome-xml/parser.h>

#include "gcompris.h"

#define IMAGEEXTENSION ".png"

/* default gnome pixmap directory in which this game tales the icon */
static char *lettersdir = "letters/";

extern GnomeCanvas *canvas;

GdkPixbuf *gcompris_load_operation_pixmap(char operation)
{
  gchar *filename;
  GdkPixbuf *pixmap;

  filename = g_strdup_printf("%s/%s/%s%c%s", PACKAGE_DATA_DIR, "gcompris", lettersdir, operation, IMAGEEXTENSION);

  if (!g_file_exists (filename)) {
    g_error (_("Couldn't find file %s !"), filename);
  }

  pixmap = gdk_pixbuf_new_from_file (filename);
  
  g_free (filename);
  
  return(pixmap);
}

GdkPixbuf *gcompris_load_number_pixmap(char number)
{
  gchar *filename;
  GdkPixbuf *pixmap;

  filename = g_strdup_printf("%s/%s/%s%c%s", PACKAGE_DATA_DIR, "gcompris", lettersdir, number, IMAGEEXTENSION);

  if (!g_file_exists (filename)) {
    g_error (_("Couldn't find file %s !"), filename);
  }

  pixmap = gdk_pixbuf_new_from_file (filename);
  
  g_free (filename);
  
  return(pixmap);
}


GdkPixbuf *gcompris_load_pixmap(char *pixmapfile)
{
  gchar *filename;
  GdkPixbuf *smallnumbers_pixmap;

  filename = g_strdup_printf("%s/%s", PACKAGE_DATA_DIR, pixmapfile);

  if (!g_file_exists (filename)) {
    g_error (_("Couldn't find file %s !"), filename);
  }

  smallnumbers_pixmap = gdk_pixbuf_new_from_file (filename);

  g_free (filename);
  
  return(smallnumbers_pixmap);
}

/**
 * Set the focus of the given image (highlight or not)
 *
 */
void gcompris_set_image_focus(GdkPixbuf *pixbuf1, gboolean focus)
{
}

/**
 * Callback over a canvas item, this function will highlight the focussed item
 *
 */
gint gcompris_item_event_focus(GnomeCanvasItem *item, GdkEvent *event, GdkPixbuf *pixbuf)
{
  /* I Have not been able to reinplement this feature with gdk-pixbuf. Help wanted.
     It worked fine with the imlib */
  /*
  gint width;
  gint height;
  width = gdk_pixbuf_get_width(pixbuf);
  height = gdk_pixbuf_get_height(pixbuf);

  switch (event->type) 
    {
    case GDK_ENTER_NOTIFY:
      printf("item_event_focus GDK_ENTER_NOTIFY:\n");
      gnome_canvas_item_set (item,
			     "pixbuf", pixbuf,
			     "width",  (double) width,
			     "height", (double)  height,
			     "width_set", TRUE, 
			     "height_set", TRUE, 
			     NULL);
      break;
    case GDK_LEAVE_NOTIFY: 
      printf("item_event_focus GDK_LEAVE_NOTIFY:\n");
      gnome_canvas_item_set (item,
			     "pixbuf", pixbuf,
			     "width",  (double) width,
			     "height", (double)  height,
			     "width_set", TRUE, 
			     "height_set", TRUE,
			     NULL);
      break;
    default:
      break;
    }
  */
  return FALSE;
}

/* Play a sound installed in the Gnome sound list */
void gcompris_play_sound (const char *soundlistfile, const char *which)
{
  gchar *filename;

  filename = g_strdup_printf("%s/%s.wav", PACKAGE_SOUNDS_DIR, which);

  if (!g_file_exists (filename)) {
    g_error (_("Couldn't find file %s !"), filename);
  }
  if (gcompris_get_properties()->fx) {
    gnome_sound_play (filename);
  }

  g_free (filename);
}


/*
 * Thanks for George Lebl <jirka@5z.com> for his Genealogy example
 * for all the XML stuff there
 */

static void
gcompris_add_xml_to_data(xmlNodePtr xmlnode, GNode * child, GcomprisBoard *gcomprisBoard)
{

  if(/* if the node has no name */
     !xmlnode->name ||
     /* or if the name is not "Board" */
     (g_strcasecmp(xmlnode->name,"Board")!=0)
     )
    return;

  /* get the type of the board */
  gcomprisBoard->type = xmlGetProp(xmlnode,"type");

  /* get the name of the board */
  gcomprisBoard->name = xmlGetProp(xmlnode,"name");

  /* get the description of the board */
  gcomprisBoard->description = xmlGetProp(xmlnode,"description");

  gcomprisBoard->icon_name = xmlGetProp(xmlnode,"icon");

  gcomprisBoard->author = xmlGetProp(xmlnode,"author");

  gcomprisBoard->boarddir = xmlGetProp(xmlnode,"boarddir");
}

/* parse the doc, add it to our internal structures and to the clist */
static void
parse_doc(xmlDocPtr doc, GcomprisBoard *gcomprisBoard)
{
  xmlNodePtr node;

  /* find <Board> nodes and add them to the list, this just
     loops through all the children of the root of the document */
  for(node = doc->root->childs; node != NULL; node = node->next) {
    /* add the board to the list, there are no children so
       we pass NULL as the node of the child */
     gcompris_add_xml_to_data(node, NULL, gcomprisBoard);
  }
}



/* read an xml file into our memory structures and update our view,
   dump any old data we have in memory if we can load a new set 
   Return a newly allocated GcomprisBoard or NULL if the parsing failed
*/
GcomprisBoard *gcompris_read_xml_file(char *fname)
{
  char *filename;
  /* pointer to the new doc */
  xmlDocPtr doc;
  GcomprisBoard *gcomprisBoard = NULL;

  gcomprisBoard = g_malloc (sizeof (GcomprisBoard));

  g_return_val_if_fail(fname!=NULL,FALSE);

  filename = g_strdup(fname);

  /* if the file doesn't exist */
  if(!g_file_exists(filename)) 
    {
      g_free(filename);

      /* if the file doesn't exist, try with our default prefix */
      filename = g_strdup_printf("%s/%s",  
				 PACKAGE_DATA_DIR, fname);

      if(!g_file_exists(filename)) 
	{
	  g_warning(_("Couldn't find file %s !"), filename);
	  g_free(filename);
	  return NULL;
	}

    }

  /* parse the new file and put the result into newdoc */
  doc = xmlParseFile(filename);

  /* in case something went wrong */
  if(!doc) {
    g_warning("Oups, the parsing of %s failed", filename);
    return NULL;
  }
  
  if(/* if there is no root element */
     !doc->root ||
     /* if it doesn't have a name */
     !doc->root->name ||
     /* if it isn't a GCompris node */
     g_strcasecmp(doc->root->name,"GCompris")!=0) {
    xmlFreeDoc(doc);
    g_warning("Oups, the file %s is not for gcompris", filename);
    return NULL;
  }
  
  /* parse our document and replace old data */
  parse_doc(doc, gcomprisBoard);

  xmlFreeDoc(doc);

  /* Store the file that belong to this board for trace and further need */
  gcomprisBoard->filename=filename;
  gcomprisBoard->board_ready=FALSE;
  gcomprisBoard->canvas=canvas;
  gcomprisBoard->previous_board=NULL;

  /* Fixed since I use the canvas own pixel_per_unit scheme */
  gcomprisBoard->width  = BOARDWIDTH;
  gcomprisBoard->height = BOARDHEIGHT;

  return gcomprisBoard;
}


/* Local Variables: */
/* mode:c */
/* eval:(load-library "time-stamp") */
/* eval:(make-local-variable 'write-file-hooks) */
/* eval:(add-hook 'write-file-hooks 'time-stamp) */
/* eval:(setq time-stamp-format '(time-stamp-yyyy/mm/dd time-stamp-hh:mm:ss user-login-name)) */
/* End: */
