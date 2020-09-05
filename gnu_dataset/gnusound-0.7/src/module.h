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

#ifndef MODULE_H
#define MODULE_H

#include <config.h>

#define MODULE_API_VERSION   0x0300 /* major, minor */
#define MODULE_API_VERSION_4 0x0400 /* major, minor */
#define MODULE_MAGIC         0x4242

/** Module flag: module does not appear in the GUI. */

#define MODULE_FLAG_FACELESS 0x01

struct _shell;

/**
 * Describes a module. Each module should provide an instance of this
 * structure with the symbol name 'manifest'.
 */

struct gnusound_module {

    /** Must be MODULE_MAGIC. */
    int magic;
    /** The API version that this module provides. */
    int api_version;
    /** The name of the module. */
    const char *name;
    /** The version of the module. */
    const char *version;
    /** The author(s) of the module. */
    const char *author;
    /** Copyrights for the module. */
    const char *copyright;
    /** Module license. */
    const char *license;
    /** URL for more information about the module. */
    const char *url;
    /** Module flags. */
    int flags;

    /** 
     * The global initialization function for the module. Called once at
     * module load time. Should return 0 if initialization was 
     * performed successfully. Can be NULL.
     */
    int (*init)(int id);
    /**
     * The per-shell initialization function for the module. Called
     * whenever a module is attached to a shell, generally whenever a
     * new shell is opened. Should return 0 if initialization was
     * successfull. Can be NULL.
     */
    int (*attach)(int id, struct _shell *shl);
    /**
     * The open function for the module. Called when the user opens a
     * module in the context of a shell. Should return an error, or a
     * value on success. If this function allocates resources, don't
     * forget to release them on close(). If the open was successful,
     * the function should set the is_open flag in the module's
     * module_state (see shell_get_module_state()). Similarly, if the
     * module gets closed for any reason, it should clear the is_open
     * flag. Can be NULL.
     */
    struct cmd_value *(*open)(int id, struct _shell *shl);
    /** 
     * The execute function for the module. Called when the module
     * should perform some work. Should return an error, or a value on
     * success. Can be NULL.
     */
    struct cmd_value *(*execute)(int id, struct _shell *shl, void *data);
    /**
     * The close function for the module. Called when the module
     * should close itself (usually because the shell is
     * closing). This should release any resources acquired in
     * open(). This function is only called when the is_open flag in
     * the module module_state (see shell_get_module_state()) is
     * set. Can be NULL.
     */
    void (*close)(int id, struct _shell *shl);
    /**
     * The shell detach function. Called whenever the module should
     * detach itself from a shell (usually because the shell is
     * closing). Can be NULL.
     */
    void (*detach)(int id, struct _shell *shl);
    /**
     * The cleanup function for the module. Called once at 
     * module unload time. Can be NULL.
     */
    void (*exit)(int id);
};

struct gnusound_module_container {
    char *filename;
    char *path;
    void *handle;
    struct gnusound_module *mod;
};

/**
 * Module state. Every shell maintains a list of these structures to
 * track the state for every open module in the shell. The module
 * must update the is_open field in its open() function.
 */

struct module_state {
    
    /**
     * This flag specifies whether the module is open. It is used by
     * the shell to determine whether to call the module's close()
     * function. The module should set this flag to true when
     * module_open() succeeds and should unset it again when
     * module_close() is invoked.
     */

    int is_open;

    /**
     * Pointer for use by the module. If this memory is allocated in
     * the module's open() function, it should be released in the
     * module's open() function. 
     */
    
    void *data;
};

void
module_exit();

int
module_init();

int
module_get_count();

struct gnusound_module *
module_get(int id);

const char *
module_get_filename(int id);

const char *
module_get_path(int id);

#endif /* ! MODULE_H */
