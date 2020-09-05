

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


#ifndef FILE_H
#define FILE_H

#include <config.h>
#include "sample.h"
#include "cmd.h"
#include "shell.h"
#include "sample.h"

struct file {
    /* Refcount. */
    int use;
    /** The file name. */
    char *name;
    /** Driver for this file format. */
    struct file_driver *driver;
    /** Private driver data. */
    void *driver_data;
};

struct file_params {
    /** The sample type. */
    enum sample_type sample_type;
    /** The sample rate. */
    float sample_rate;
    /** The number of channels. */
    int channels;
    /** The number of frames, or -1 if unknown. */
    long frame_count;
};

struct file_format {
    char *name;
    char *extension;
};

enum file_property {
    FILE_FORMAT,
    FILE_DETAILED_FORMAT
};

struct file_driver {
    /** Driver name. Should not contain colon (:), comma (,) or space. */
    const char *name;

    /** 
     * Allocate private driver data structures and attach them to the
     * file object. This fills in the file->driver and
     * file->driver_data fields in the file struct.
     *
     * The format parameter is a string specifying the preferred file
     * format. It is one of the strings returned by
     * get_write_formats(). It can be NULL, in which case the driver
     * should pick a default file format.
     */
    struct cmd_value *(*attach)(struct file *file,
                                const char *format);
    /** Open the file for reading or writing. */
    struct cmd_value *(*open)(struct file *file, 
                              const char *mode,
                              struct file_params *params);
    /** Read interleaved frames. */
    long (*read)(struct file *file, void *buf, long count);
    /** Write interleaved frames. */
    long (*write)(struct file *file, void *buf, long count);
    /** Close file. */
    struct cmd_value *(*close)(struct file *file);
    /** Release private data structures. */
    void (*detach)(struct file *file);

    /** Print a property into the given buffer. */
    int (*snprint)(struct file *file, 
                   enum file_property what, 
                   char *buf, 
                   int buflen);
    
    const struct file_format *(*get_read_formats)();
    const struct file_format *(*get_write_formats)();

    GtkWidget *(*open_global_config)();
    void (*commit_global_config)();
    void (*close_global_config)();

    GtkWidget *(*open_file_config)(struct file *file, const char *format);
    void (*commit_file_config)(struct file *file);
    void (*close_file_config)(struct file *file);
};

struct file_driver *
file_find_driver(const char *name);

void
file_foreach_driver(void (*func)(struct file_driver *fd,
                                 void *user_data),
                    void *user_data);

int
file_register_driver(struct file_driver *fd);

struct file *
file_new(const char *name);

int
file_set_name(struct file *file,
              const char *name);

void
file_addref(struct file *file);

void
file_destroy(struct file *file);

/**
 * These functions are really part of shell but they are placed
 * here to keep some semblance of oversight.
 */

struct cmd_value *
file_load(struct _shell *shl,
          struct file *file);

struct cmd_value *
file_save(struct _shell *shl, 
          struct file *file,
          gboolean keep_backup);


struct cmd_value *
file_mixdown(struct _shell *shl,
             mixer *output_mixer,
             struct file *file,
             gboolean keep_backup);

void
file_save_emergency(struct _shell *shl);

#endif /* ! FILE_H */
