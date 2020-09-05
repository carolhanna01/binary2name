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

#ifndef CMD_H
#define CMD_H

#include <config.h>
#include <limits.h>
#include <gdk/gdk.h>
#include <gtk/gtk.h>
#include "error.h"
#include "mem.h"
#include "sample.h"
#include "marker.h"
#include "mixer.h"
#include "clip.h"
#include "file.h"
#include "shell.h"

/* Require a least 32 bits for int type for the enum. */

#if INT_MAX < 2147483647
#error "Need at least 32 bits per int."
#endif

#define CMD_MAX_ARGS 100

enum cmd_type {
    CMD_VOID_T         = 0,
    CMD_ANY_T          = 1 << 0,
    CMD_int_T          = 1 << 1,
    CMD_long_T         = 1 << 2,
    CMD_double_T       = 1 << 3,
    CMD_charp_T        = 1 << 4,
    CMD_cmdp_T         = 1 << 5,
    CMD_clipp_T        = 1 << 6,
    CMD_shellp_T       = 1 << 7,
    CMD_markerp_T      = 1 << 8,
    CMD_marker_listp_T = 1 << 9,
    CMD_marker_list_arrayp_T = 1 << 10,
    CMD_sndp_T         = 1 << 11,
    CMD_mixerp_T       = 1 << 12,
    CMD_filep_T        = 1 << 13,
    CMD_GtkObjectp_T   = 1 << 20,
    CMD_GdkEventp_T    = 1 << 21,
    CMD_voidp_T        = 1 << 22,
    CMD_GListp_T       = 1 << 23,
    CMD_ERROR          = 1 << 30,
    CMD_INVALID        = 1 << 31
};

struct _shell;
struct clip;

struct cmd_value {
    struct error error;
    enum cmd_type type;
    void (*dtor)(struct cmd_value *v);
    union {
        int32_t v_int;
        int64_t v_long;
        double v_double;
        char *v_charp;
        struct _shell *v_shellp;
        struct _snd *v_sndp;
        struct clip *v_clipp;
        struct marker *v_markerp;
        struct marker_list *v_marker_listp;
        struct marker_list_array *v_marker_list_arrayp;
        struct cmd *v_cmdp;
        struct _mixer *v_mixerp;
        struct file *v_filep;
        GtkObject *v_GtkObjectp;
        GdkEvent *v_GdkEventp;
        GList *v_GListp;
        void *v_voidp;
    } v;
};

struct cmd_argv {
    int argc;
    struct cmd_value *argv[1];
};

struct cmd_paramdecl {
    int count;
    enum cmd_type type[1];
};

struct cmd_signature {
    char *name;
    char *description;
    struct cmd_value *(*func)(const char *name,
                              struct cmd_argv *argv);
    enum cmd_type returntype;
    struct cmd_paramdecl *pdecl;
};

struct cmd {
    char *name;
    struct cmd_argv *argv;
};

typedef char * charp;
typedef struct _shell * shellp;
typedef struct _snd * sndp;
typedef struct clip * clipp;
typedef struct marker * markerp;
typedef struct marker_list * marker_listp;
typedef struct marker_list_array * marker_list_arrayp;
typedef struct cmd * cmdp;
typedef struct _mixer * mixerp;
typedef struct file * filep;
typedef GtkObject * GtkObjectp;
typedef GdkEvent * GdkEventp;
typedef void * voidp;
typedef GList * GListp;

/**
 * Convenience macro to create a struct cmd_argv.
 * @return The argv or NULL on error.
 */

#define CMD_NEW_ARGV(args...) cmd_new_argv_terminated(1, args, (void *)-1)
#define CMD_NEW_ARGV0() cmd_new_argv_terminated(1, (void *)-1)

/**
 * Convenience macro to create a struct cmd.
 * @param name The command name.
 * @return The new command or NULL on error.
 */

/* Not using __VA_ARGS__ since gcc 2.95 can't handle it.  

   (void *)-1 is perhaps not the most elegant terminator, but we can't
   use NULL, since it's possible (through memory allocation failure or
   some other error) for one of the arguments to be NULL. */

#define CMD_NEW(name, args...) cmd_new(name, cmd_new_argv_terminated(1, args, (void *)-1))
#define CMD_NEW0(name) cmd_new(name, cmd_new_argv_terminated(1, (void *)-1))


/* Template prototypes and function definitions for value
   constructors/getters. */

const char *
cmd_type_as_string(enum cmd_type type);

#define CMD_TYPE(T) \
  static inline struct cmd_value *cmd_new_ ## T ## _val(T); \
  static inline struct cmd_value *cmd_new_ ## T ## _val(T v) { \
    struct cmd_value *cv = mem_alloc(sizeof(struct cmd_value)); \
    if(!cv) \
      return NULL; \
    cv->type = CMD_ ## T ## _T; \
    cv->dtor = NULL; \
    cv->v.v_ ## T = v; \
    return cv; \
  } \
  static inline T cmd_ ## T (struct cmd_value *v); \
  static inline T cmd_ ## T (struct cmd_value *v) { \
    if(v->type != CMD_ ## T ## _T) { \
      FAIL("tried to get " # T " on %s value!\n", cmd_type_as_string(v->type)); \
      abort(); \
    } \
    return v->v.v_ ## T; \
  } \
  static inline struct cmd_value *cmd_new_ ## T ## _val_with_dtor(T v, \
                                                       void (*dtor)(struct cmd_value *v)); \
  static inline struct cmd_value *cmd_new_ ## T ## _val_with_dtor(T v, \
                                                       void (*dtor)(struct cmd_value *v)) { \
    struct cmd_value *cv = cmd_new_ ## T ## _val(v); \
    if(!cv) \
      return NULL; \
    cv->dtor = dtor; \
    return cv; \
  }

/* Expand the template for each type. */

CMD_TYPE(int);
CMD_TYPE(long);
CMD_TYPE(double);
CMD_TYPE(charp);
CMD_TYPE(shellp);
CMD_TYPE(sndp);
CMD_TYPE(clipp);
CMD_TYPE(markerp);
CMD_TYPE(marker_listp);
CMD_TYPE(marker_list_arrayp);
CMD_TYPE(cmdp);
CMD_TYPE(mixerp);
CMD_TYPE(filep);
CMD_TYPE(GtkObjectp);
CMD_TYPE(GdkEventp);
CMD_TYPE(voidp);
CMD_TYPE(GListp);

/* Prototypes. */

void
cmd_free_signature(struct cmd_signature *sig);

struct cmd_signature *
cmd_copy_signature(struct cmd_signature *sig);

struct cmd_argv *
cmd_new_argv_terminated(int dummy, ...);

const char *
cmd_get_error_message(struct cmd_value *v);

int
cmd_is_error(struct cmd_value *v);

struct cmd_value *
cmd_on_error(struct cmd_value *v,
             const char *condition);

struct cmd_value *
cmd_new_void_val();

struct cmd_value *
cmd_new_error_val(const char *format, ...);

struct cmd_paramdecl *
cmd_new_paramdecl(int count, ...);

struct cmd_argv *
cmd_append_argv(struct cmd_argv *argv,
                struct cmd_value *v);

void
cmd_destroy_argv(struct cmd_argv *argv);

void
cmd_destroy_value(struct cmd_value *v);

struct cmd_argv *
cmd_new_argv(int argc, ...);

struct cmd *
cmd_new(const char *name,
        struct cmd_argv *args);

void
cmd_destroy_keep_argv(struct cmd *cmd);

void
cmd_destroy(struct cmd *cmd);

struct cmd_value *
cmd_do(struct cmd *cmd);

int
cmd_register(const char *name,
             const char *description,
             struct cmd_value *(*func)(const char *name,
                                       struct cmd_argv *argv),
             enum cmd_type returntype,
             struct cmd_paramdecl *pdecl);

void
cmd_unregister(const char *cmd);

int
cmd_verify_paramdecl(const char *name,
                     struct cmd_paramdecl *pdecl);

int
cmd_init();


void
cmd_print(struct cmd *cmd);

void
cmd_print_value(struct cmd_value *v);

int
cmd_do_or_fail(struct cmd *cmd,
               const char *on_error,
               struct cmd_value **result);

struct cmd_value *
cmd_error_cascade(struct cmd_value *v,
                  struct error *error);

/* Prototypes for all the subsystem initializers. */

int
cmd_shell_init();

int
cmd_file_init();

int
cmd_edit_init();

int
cmd_select_init();

int
cmd_markers_init();

int
cmd_view_init();

int
cmd_play_init();

int
cmd_events_init();

int
cmd_mixer_init();

/* Destructors. */

void
cmd_voidp_dtor(struct cmd_value *v);

void
cmd_charp_dtor(struct cmd_value *v);

void
cmd_clipp_dtor(struct cmd_value *v);

void
cmd_cmd_dtor(struct cmd_value *v);

void
cmd_sndp_dtor(struct cmd_value *v);

void
cmd_marker_list_arrayp_dtor(struct cmd_value *v);

void
cmd_filep_dtor(struct cmd_value *v);

void
cmd_GdkEventp_dtor(struct cmd_value *v);

void
cmd_GListp_dtor(struct cmd_value *v);

#endif /* ! CMD_H */
