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
 * A copy of the GNU General Public License can be found in the file
 * LICENSE in the top directory of the source distribution. If not,
 * write to the Free Software * Foundation, Inc., 675 Mass Ave,
 * Cambridge, MA 02139, USA.
 *
 */

/**
 * @file 
 * A tool is a shell component which responds to mouse clicks and
 * keypresses and (may) augment or replace the wavecanvas with its own
 * drawing routines.
 *
 * Tools register themselves as modules at module load time.
 * At shell creation time, the shell invokes tool_bind_all(), which
 * in turn invokes shell_bind_tool() to bind a tool to the shell.
 */

#include <config.h>
#include <gdk/gdk.h>
#include "lib/misc.h"
#include "mem.h"
#include "tool.h"
#include "pref.h"
#include "gui.h"

static GHashTable *tools;

struct tool_box {
    shell *shl;
    GList *l;
};

void 
tool_destroy(struct tool *tool) {
    mem_free(tool);
}

void
tool_exit() {
    g_hash_table_destroy(tools);
}

static int
tool_compare_tools_ordinal(gconstpointer a, 
                           gconstpointer b) {
    return ((struct tool *)a)->ordinal - ((struct tool *)b)->ordinal;
}

static void
tool_make_one(void *key,
              void *value,
              void *user_data) {
    struct tool *(*ctor)(shell *shl) = value;
    struct tool_box *tb = user_data;

    tb->l = g_list_insert_sorted(tb->l, 
                                 ctor(tb->shl),
                                 tool_compare_tools_ordinal);
    
}

static GList *
tool_make_all(shell *shl) {
    struct tool_box tb;
    tb.shl = shl;
    tb.l = NULL;
    g_hash_table_foreach(tools, tool_make_one, &tb);
    return tb.l;
}

void 
tool_bind_all(shell *shl) {
    GList *l, *sorted = tool_make_all(shl);
    struct tool *tool;
    
    for(l = sorted; l; l = l->next) {
        tool = (struct tool *)l->data;
        tool->shl = shl;
        shell_bind_tool(shl, tool);
    }
}

GtkWidget *
tool_get_panel(struct tool *tool) {
    GladeXML *xml;
    GtkWidget *window, *panel;
    char gladexml_file[256];
    char *filename;

    if(!tool->panel) {
        snprintf(gladexml_file, sizeof gladexml_file,
                 "tool_%s%s.glade", 
                 tool->name, GUI_GTK_VERSION_TAG);
        filename = findfile(MODULE_SEARCH_PATH, gladexml_file, R_OK);
        if(!filename)
            return NULL;

        xml = glade_xml_new(filename, NULL, NULL);
        if(!xml) {
            free(filename);
            return NULL;
        }
        free(filename);
        
        tool->panel = pane_new(xml);
        if(!tool->panel)
            return NULL;

        window = pane_get_widget(tool->panel, "tool_container");
        panel = GTK_BIN(window)->child;
        gtk_widget_ref(panel);
        gtk_container_remove(GTK_CONTAINER(window), panel);

        pane_register_widget(tool->panel, "tool_panel", panel);
    }

    return pane_get_widget(tool->panel, "tool_panel");
}

void
tool_get_icon(struct tool *tool,
              GdkPixmap **pixmap,
              GdkBitmap **mask) {
    char pixmap_name[256];
    snprintf(pixmap_name, sizeof pixmap_name, 
             "tools.icons.%s", tool->name);
    gui_get_pixmap(pixmap_name, pixmap, mask);
}

static int
tool_register_icon_pref(char *name,
                        char *value) {
    int r;
    struct pref icon_pref = PREF_STRING(name, value, "Undocumented");
    r = pref_register(1, &icon_pref);
    if(!r)
        pref_load(name);
    return r;
}

/**
 * Registers a tool. Registered tools are automatically bound to
 * newly created shells.
 * @param name The tool name.
 * @param ctor The tool constructor to invoke when a tool needs to be
 * bound to a new shell.
 * @param rsrc_path A filesystem path specifying where to locate
 * tool resources such as the tool icon.
 * @return 0 on success or non-zero error code otherwise.
 */

int
tool_register(char *name,
              struct tool *(*ctor)(shell *shl)) {
    char icon_pref_name[256], icon_pref_value[4096];

    /* 
     * The GUI initialization will automatically scan all prefs
     * beginning with "pixmaps." and load the corresponding pixmaps.
     */

    snprintf(icon_pref_name, sizeof icon_pref_name, 
             "pixmaps.tools.icons.%s", name);
    snprintf(icon_pref_value, sizeof icon_pref_value,
             "tool_icon_%s.xpm", name);
    tool_register_icon_pref(icon_pref_name, icon_pref_value);
    g_hash_table_insert(tools, name, ctor);
    
    return 0;
}

int
tool_init() {
    tools = g_hash_table_new(g_str_hash, g_str_equal);
    return 0;
}
