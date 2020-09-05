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

#include <config.h>
#include "pref.h"
#include "mem.h"
#include "gui.h"
#include "sample.h"
#include "module.h"
#include "arbiter.h"
#include "clipboard.h"
#include "emergency.h"

int
main(int argc, 
     char *argv[]) {
    int r;

    /* Get the default sighandlers before GNOME replaces them. */

    emergency_get_sighandlers();

    g_thread_init(NULL);

#ifdef HAVE_GNOME2
    gdk_threads_init();
    gnome_program_init(PACKAGE, VERSION, LIBGNOMEUI_MODULE, argc, argv, 
                       GNOME_PROGRAM_STANDARD_PROPERTIES, NULL);

#else
    gnome_init(PACKAGE, VERSION, argc, argv);
#endif

    if(mem_init()) {
        FAIL("fatal error initializing private malloc library.\n");
        return 1;
    }

    if(cmd_init()) {
        FAIL("fatal error initializing cmd.\n");
        return 1;
    }

    if(pref_init()) {
        FAIL("fatal error initializing preferences.\n");
        return 1;
    }

    if(arbiter_init()) {
        FAIL("fatal error initializing core.\n");
        return 1;
    }

    if(sample_init()) {
        FAIL("fatal error initializing sample utilities.\n");
        return 1;
    }
    
    if(tool_init()) {
        FAIL("fatal error initializing tool framework.\n");
        return 1;
    }

    if(module_init()) {
        FAIL("fatal error initializing modules.\n");
        return 1;
    }

    if(player_init()) {
        FAIL("fatal error initializing playback/record engine.\n");
        return 1;
    }

    if(gui_init()) {
        FAIL("fatal error initializing GUI.\n");
        return 1;
    }

    if(clipboard_init()) {
        FAIL("fatal error initializing clipboard.\n");
        return 1;
    }

    if(emergency_init()) {
        FAIL("fatal error initializing emergency routines.\n");
        return 1;
    }

    r = arbiter_run(argc, argv);

    emergency_exit();
    clipboard_exit();
    gui_exit();
    player_exit();
    tool_exit();
    module_exit();
    arbiter_exit();
    pref_exit();
    mem_exit();
    
    DEBUG("main done\n");

#ifndef HAVE_GNOME2
    gtk_exit(r);
#endif
    
    return r;

}
