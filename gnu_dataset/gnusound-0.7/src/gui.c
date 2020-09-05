/*
 * GNUsound - a sound editor for GNOME.
 * Copyright (C) 2002-2004  Pascal Haakmat <a.haakmat@chello.nl>
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
 *
 */

/**
 * @file
 * Grab bag of GUI initialization and related utility functions.
 */

#define GTK_DISABLE_DEPRECATED 1
#define GNOME_DISABLE_DEPRECATED 1

/* Has to come _before_ config.h ... */

#include <glade/glade.h>

#include <config.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <assert.h>
#include <unistd.h>
#include <math.h>
#include <signal.h>
#include <gdk/gdk.h>
#include <gnome.h>
#include <libgnomeui/gnome-window-icon.h>
#include "lib/misc.h"
#include "pref.h"
#include "mem.h"
#include "gui.h"
#include "file.h"
#include "module.h"
#include "shell.h"
#include "player.h"
#include "arbiter.h"
#include "cmd.h"
#include "dialog_pref.h"

/* Maintains a string->color mapping. */

static GHashTable *colors;

/* Maintains a string->pixmap mapping. */

static GHashTable *pixmaps;
static char **pixmap_names;
static GdkCursor *cursor_cache[GUI_CURSOR_CACHE] = { NULL };
static GdkCursorType cursor_type_cache[GUI_CURSOR_CACHE] = { 0 };

static struct dialog *preferences_dialog;

struct dialog *
gui_get_dialog(const char *name) {
    if(!strcmp(name, "preferences"))
        return preferences_dialog;
    return NULL;
}

void
gui_color_to_hex_string(char *hex,
                        GdkColor *color) {
    snprintf(hex, 8, "#%.2x%.2x%.2x", 
             color->red >> 8, color->green >> 8, color->blue >> 8);
    
}

GladeXML *
gui_get_xml(const char *glade_xml_file,
            const char *root) {
    GladeXML *xml;
    char *filename = findfile(GLADE_SEARCH_PATH, glade_xml_file, R_OK);
    
    if(!filename)
        return NULL;
    
    xml = glade_xml_new(filename, root, NULL);

    if(!xml) {
        free(filename);
        return NULL;
    }
    
    free(filename);
    
    return xml;
}

/**
 * Retrieves a color from the color cache.
 * @param name The color to retrieve.
 * @return The color.
 */

GdkColor *
gui_get_color(const char *name) {
    GdkColor *color;
    char pref_name[strlen(name) + strlen("colors.") + 1];
    snprintf(pref_name, sizeof pref_name, "colors.%s", name);
    color = g_hash_table_lookup(colors, pref_name);
    if(!color) {
        FAIL("%s\n", pref_name);
        abort();
    }
    return color;
}

/**
 * Allocates colors given by the colors.* pref hierachy in the system 
 * colormap.
 */

void
gui_alloc_colors() {
    GdkColor color, *color_copy, *old_color;
    GdkColormap *colormap;
    char **color_names;
    char *name, *old_name;
    int i;
    gpointer old_key, old_value;

    colormap = gdk_colormap_get_system();

    color_names = pref_match("colors.*");
    if(!color_names) {
        FAIL("can't get color names from prefs\n");
        abort();
    }
    for(i = 0; color_names[i]; i++) {
        gdk_color_parse(pref_get_as_string(color_names[i]), &color);
        gdk_colormap_alloc_color(colormap, &color, FALSE, TRUE);
        color_copy = gdk_color_copy(&color);
        name = strdup(color_names[i]);
        if(!name) {
            FAIL("can't copy color name %s\n", color_names[i]);
            abort();
        }

        if(g_hash_table_lookup_extended(colors,
                                        name,
                                        &old_key,
                                        &old_value)) {
            g_hash_table_remove(colors, name);
            old_name = old_key;
            old_color = old_value;
            free(old_name);
            gdk_color_free(old_color);
        }

        g_hash_table_insert(colors, name, color_copy);
    }
    free(color_names);
}

/**
 * Finds the window that the given widget belongs to.
 * @param w The widget.
 * @return The window.
 */

GdkWindow *
gui_get_window(GtkWidget *w) {
    GdkWindow *window;
    if(GTK_WIDGET_TOPLEVEL(w)) {
        window = w->window;
    } else {
        if (GTK_WIDGET_NO_WINDOW (w))
            window = w->window;
        else
            window = gdk_window_get_parent(w->window);
    }
    return window;
}

void
gui_get_widget_position_absolute(GtkWidget *w,
                                 int *x,
                                 int *y) {
    GdkWindow *window = gui_get_window(w);
    //gdk_window_get_position(window, x, y);
    gdk_window_get_origin(window, x, y);
}

struct gui_image {
    GdkPixmap *pixmap;
    GdkBitmap *mask;
};

/**
 * Retrieves a pixmap from the pixmap cache.
 * @param name The pixmap to retrieve.
 * @param pixmap Pointer to pointer which receives pixmap. Can be NULL.
 * @param mask Pointer to pointer which receives mask. Can be NULL.
 */

void
gui_get_pixmap(const char *name,
               GdkPixmap **pixmap,
               GdkBitmap **mask) {
    struct gui_image *image;
    char pref_name[strlen(name) + strlen("pixmaps.") + 1];

    snprintf(pref_name, sizeof pref_name, "pixmaps.%s", name);
    image = g_hash_table_lookup(pixmaps, pref_name);

    if(!pixmap) {
        FAIL("%s\n", pref_name);
        abort();
    }

    if(pixmap) 
        *pixmap = image->pixmap;
    
    if(mask)
        *mask = image->mask;
}

void
gui_unload_pixmap(void *key,
                  void *value,
                  void *user_data) {
    struct gui_image *image = value;

#ifdef HAVE_GNOME2
    g_object_unref(image->pixmap);
    g_object_unref(image->mask);
#else
    gdk_pixmap_unref(image->pixmap);
    gdk_bitmap_unref(image->mask);
#endif

    mem_free(image);
}

void
gui_unload_pixmaps() {
    g_hash_table_foreach(pixmaps, gui_unload_pixmap, NULL);
    g_hash_table_destroy(pixmaps);

    free(pixmap_names);
}

/**
 * Loads all pixmaps as specified by the "pixmaps.*" pref hierarchy.
 * @param colormap The colormap to use.
 * @param alpha The alpha color.
 */

void
gui_load_pixmaps(GdkColormap *colormap,
                 GdkColor *alpha) {
    int i, j;
    char spath[4096], path[4096], **paths;
    const char *filename;
    struct gui_image *image;

    pixmap_names = pref_match("pixmaps.*");
    if(!pixmap_names) {
        FAIL("cannot get pixmap names\n");
        abort();
    }

    pixmaps = g_hash_table_new(g_str_hash, g_str_equal);

    for(i = 0; pixmap_names[i]; i++) {
        filename = pref_get_as_string(pixmap_names[i]);
        if(!filename) {
            FAIL("cannot find filename preference for pixmap %s\n", 
                 pixmap_names[i]);
            abort();
        }
        //        DEBUG("looking for %s (%s)\n", pixmap_names[i], filename);
        snprintf(spath, sizeof spath, "%s:%s/%s:%s:%s",
                 "gui", 
                 g_get_home_dir(), "." PACKAGE "/pixmaps",
                 GUIDIR,
                 MODULE_SEARCH_PATH);
        paths = g_strsplit(spath, ":", 255);
        for(j = 0; paths[j]; j++) {
            snprintf(path, sizeof path, "%s/%s", paths[j], filename);
            //            DEBUG("         in %s\n", path);
            if(!access(path, R_OK))
                break;
        }
        g_strfreev(paths);

        if(access(path, R_OK)) {
            FAIL("cannot locate pixmap %s\n", filename);
            abort();
        }

        image = mem_alloc(sizeof(*image));
        if(!image) {
            FAIL("cannot store image %s\n", path);
            abort();
        }
        
        image->mask = NULL;
        image->pixmap = gdk_pixmap_colormap_create_from_xpm(NULL, 
                                                            colormap, 
                                                            &image->mask, 
                                                            alpha,
                                                            path);
        if(!image->pixmap || !image->mask) {
            FAIL("cannot load pixmap %s\n", path);
            abort();
        }

        g_hash_table_insert(pixmaps, pixmap_names[i], image);
    }
}

void
gui_window_set_cursor(GdkWindow *w,
                      GdkCursorType type) {
    int i;
    GdkCursor *cursor;
    for(i = 0; i < GUI_CURSOR_CACHE; i++) {
        if(cursor_cache[i] && cursor_type_cache[i] == type) {
            gdk_window_set_cursor(w, cursor_cache[i]);
            return;
        }
    }
    cursor = gdk_cursor_new(type);
    gdk_window_set_cursor(w, cursor);
    for(i = 0; i < GUI_CURSOR_CACHE; i++) {
        if(!cursor_cache[i]) {
            cursor_type_cache[i] = type;
            cursor_cache[i] = cursor;
            return;
        }
    }
}

void
gui_exit() {
    dialog_destroy(preferences_dialog);
    gui_unload_pixmaps();

}

/**
 * GUI initialization. 
 * @return 0 on success, non-zero otherwise.
 */

int 
gui_init() {
    GdkColor black;
    GdkColormap *colormap;
    GtkStyle *style;
    char *filename;

    glade_gnome_init();

    filename = findfile(LOGO_SEARCH_PATH, LOGO_FILE, R_OK);
    if(filename) {
        gnome_window_icon_set_default_from_file(filename);
        free(filename);
    }

    filename = findfile(GTKRC_SEARCH_PATH, GTKRC_FILE, R_OK);
    if(filename) {
        gtk_rc_parse(filename);
        free(filename);
    }

    /* Find and allocate colors. */

    colormap = gdk_colormap_get_system();
    colors = g_hash_table_new(g_str_hash, g_str_equal);
    gui_alloc_colors();

    /* Load images. */

    gdk_color_black(colormap, &black);

    style = gtk_style_new();
    gui_load_pixmaps(colormap, &style->bg[GTK_STATE_NORMAL]);
#ifdef HAVE_GNOME2
    g_object_unref(style);
#else
    gtk_style_unref(style);
#endif

    preferences_dialog = dialog_pref_new();

    return 0;
}
