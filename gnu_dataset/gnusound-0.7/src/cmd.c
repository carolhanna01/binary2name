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

#include <config.h>
#include <glib.h>
#include <stdarg.h>
#include <errno.h>
#include <gdk/gdk.h>
#include "cmd.h"
#include "mem.h"

static GHashTable *cmd_registry = NULL;

int
cmd_do_or_fail(struct cmd *cmd,
               const char *on_error,
               struct cmd_value **result) {
    struct cmd_value *r, *err;
    r = cmd_do(cmd);
    cmd_destroy(cmd);
    if(cmd_is_error(r)) {
        err = cmd_new_error_val(on_error, cmd_get_error_message(r));
        cmd_destroy_value(r);
        *result = err;
        return 1;
    }
    *result = r;
    return 0;
}


const char *
cmd_type_as_string(enum cmd_type type) {
    switch(type) {
    case CMD_cmdp_T:
        return "cmdp";
    case CMD_int_T:
        return "int";
    case CMD_long_T:
        return "long";
    case CMD_double_T:
        return "double";
    case CMD_shellp_T:
        return "shell";
    case CMD_clipp_T:
        return "clip";
    case CMD_charp_T:
        return "charp";
    case CMD_marker_listp_T:
        return "marker_listp";
    case CMD_marker_list_arrayp_T:
        return "marker_list_arrayp";
    case CMD_mixerp_T:
        return "mixer";
    case CMD_filep_T:
        return "file";
    case CMD_GtkObjectp_T:
        return "GtkObject";
    case CMD_GdkEventp_T:
        return "GdkEvent";
    default:
        return "<no string available>";
    }
}

void
cmd_print_argv(struct cmd_argv *argv) {
    int i;
    for(i = 0; i < argv->argc; i++) {
        cmd_print_value(argv->argv[i]);
        if(i + 1 < argv->argc)
            printf(", ");
    }
}

void
cmd_print(struct cmd *cmd) {
    printf("%s(", cmd->name);
    cmd_print_argv(cmd->argv);
    printf(")");
}

void
cmd_print_value(struct cmd_value *v) {
    switch(v->type) {
    case CMD_shellp_T:
        printf("shell @ %p", cmd_shellp(v));
        break;
    case CMD_sndp_T:
        printf("~%s", cmd_sndp(v) ? cmd_sndp(v)->namep : "NULL");
        break;
    case CMD_cmdp_T:
        cmd_print(cmd_cmdp(v));
        break;
    case CMD_int_T:
        printf("%d", cmd_int(v));
        break;
    case CMD_double_T:
        printf("%ff", cmd_double(v));
        break;
    case CMD_long_T:
        printf("%ldL", cmd_long(v));
        break;
    case CMD_charp_T:
        printf("\"%s\"", cmd_charp(v));
        break;
    default:
        printf("type: %s (%d @ address: %p", 
               cmd_type_as_string(v->type), v->type, (void *)v);
    }
}


struct cmd_value *
cmd_on_error(struct cmd_value *v,
             const char *condition) {
    struct cmd_value *r;
    if(v == NULL) {
        FAIL("execution failure\n");
        if(condition)
            return cmd_new_error_val("%s (Internal error)", (condition));
        else
            return cmd_new_error_val("Internal error");
    }
    if(cmd_is_error(v)) {
        if(condition) {
            FAIL("runtime failure: %s (%s)\n", condition,
                 cmd_get_error_message(v));
            r = cmd_new_error_val("%s (%s)", 
                                  condition, cmd_get_error_message(v));
            cmd_destroy_value(v);
            return r;
        } else {
            FAIL("runtime failure: %s\n", cmd_get_error_message(v));
            r = cmd_new_error_val("%s", cmd_get_error_message(v));
            cmd_destroy_value(v);
            return r;
        }
    }
    return NULL;
}

void
cmd_voidp_dtor(struct cmd_value *v) {
    if(v->type != CMD_voidp_T) {
        FAIL("attempt to destroy %d as voidp\n", v->type);
        return;
    }

    if(v->v.v_voidp)
        free(v->v.v_voidp);
}

void
cmd_cmd_dtor(struct cmd_value *v) {
    if(v->type != CMD_cmdp_T) {
        FAIL("attempt to destroy %d as cmdp\n", v->type);
        return;
    }
    
    cmd_destroy(v->v.v_cmdp);
}

void
cmd_error_dtor(struct cmd_value *v) {
    if(v->type != CMD_ERROR) {
        FAIL("attempt to destroy %d as error\n", v->type);
        return;
    }
    if(v->v.v_charp)
        free(v->v.v_charp);
}

void
cmd_sndp_dtor(struct cmd_value *v) {
    if(v->type != CMD_sndp_T) {
        FAIL("attempt to destroy %d as sndp\n", v->type);
        return;
    }
    DEBUG("destroying %p (%s)\n", v->v.v_sndp, v->v.v_sndp->namep);
    snd_destroy(v->v.v_sndp);
}

void
cmd_clipp_dtor(struct cmd_value *v) {
    if(v->type != CMD_clipp_T) {
        FAIL("attempt to destroy %d as clipp\n", v->type);
        return;
    }
    clip_destroy(v->v.v_clipp);
}

void
cmd_charp_dtor(struct cmd_value *v) {
    if(v->type != CMD_charp_T) {
        FAIL("attempt to destroy %d as charp\n", v->type);
        return;
    }
    free(v->v.v_charp);
}

void
cmd_marker_list_arrayp_dtor(struct cmd_value *v) {
    if(v->type != CMD_marker_list_arrayp_T) {
        FAIL("attempt to destroy %d as marker_list_arrayp\n", v->type);
        return;
    }
    marker_list_array_destroy(v->v.v_marker_list_arrayp);
}

void
cmd_filep_dtor(struct cmd_value *v) {
    if(v->type != CMD_filep_T) {
        FAIL("attempt to destroy %d as filep\n", v->type);
        return;
    }
    DEBUG("destroying filep\n");
    file_destroy(v->v.v_filep);
}

void
cmd_GdkEventp_dtor(struct cmd_value *v) {
    if(v->type != CMD_GdkEventp_T) {
        FAIL("attempt to destroy %d as GdkEventp\n", v->type);
        return;
    }
    gdk_event_free(v->v.v_GdkEventp);
}

void
cmd_GListp_dtor(struct cmd_value *v) {
    if(v->type != CMD_GListp_T) {
        FAIL("attempt to destroy %d as GListp\n", v->type);
        return;
    }
    g_list_free(v->v.v_GListp);
}

void
cmd_destroy_value(struct cmd_value *v) {
    if(!v)
        return;
    if(v->dtor)
        v->dtor(v);
    mem_free(v);
}

struct cmd_value *
cmd_new_void_val() {
    struct cmd_value *v = mem_alloc(sizeof(struct cmd_value));
    if(!v)
        return NULL;
    v->dtor = NULL;
    v->type = CMD_VOID_T;
    return v;
}

int
cmd_is_error(struct cmd_value *v) {
    if(!v)
        return 1;
    if(v->type == CMD_ERROR)
        return 1;
    return 0;
}

const char *
cmd_get_error_message(struct cmd_value *v) {
    if(!v)
        return "Execution error.";
    if(v->type != CMD_ERROR)
        return "No error.";
    if(v->v.v_charp) {
        return v->v.v_charp;
    }
    return "Unspecified error.";
}

struct cmd_value *
cmd_new_error_val(const char *fmt, ...) {
    va_list ap;
    struct cmd_value *v = mem_alloc(sizeof(*v));
    if(!v)
        return NULL;

    v->dtor = cmd_error_dtor;
    v->type = CMD_ERROR;
    v->v.v_charp = NULL;
    if(fmt) {
        va_start(ap, fmt);
        vasprintf(&v->v.v_charp, fmt, ap);
        va_end(ap);
    }
    return v;
}

struct cmd_value *
cmd_error_cascade(struct cmd_value *v,
                  struct error *error) {
    char *s;
    size_t sz;

    if(v->type != CMD_ERROR) {
        FAIL("trying to cascade but not an error\n");
        return v;
    }
    
    sz = strlen(error_get_message(error)) + 1;
    if(v->v.v_charp) 
        sz += strlen(v->v.v_charp) + 1;
    s = mem_alloc(sz + 4);
    if(!s)
        return v;

    snprintf(s, sz + 4, "%s (%s)", v->v.v_charp, error_get_message(error));
    DEBUG("error message: %s\n", error_get_message(error));
    if(v->v.v_charp)
        free(v->v.v_charp);
    DEBUG("s: %s\n", s);
    v->v.v_charp = s;
    //error_free(error);
    return v;
}

void
cmd_destroy_argv(struct cmd_argv *argv) {
    int i;

    if(!argv)
        return;

    for(i = 0; i < argv->argc; i++)
        if(argv->argv[i])
            cmd_destroy_value(argv->argv[i]);

    mem_free(argv);
}

struct cmd_paramdecl *
cmd_copy_paramdecl(struct cmd_paramdecl *pdecl) {
    int i;
    struct cmd_paramdecl *copy = 
        mem_alloc(sizeof(struct cmd_paramdecl) + 
                  pdecl->count * sizeof(enum cmd_type));

    g_return_val_if_fail(copy != NULL, NULL);

    for(i = 0; i < pdecl->count; i++)
        copy->type[i] = pdecl->type[i];

    return copy;
}

void
cmd_free_signature(struct cmd_signature *sig) {
    mem_free(sig->description);
    mem_free(sig->name);
    mem_free(sig);
}

struct cmd_signature *
cmd_copy_signature(struct cmd_signature *sig) {
    struct cmd_signature *copy = mem_alloc(sizeof(*copy));

    g_return_val_if_fail(copy != NULL, NULL);

    copy->name = strdup(sig->name);
    copy->description = strdup(sig->description);
    copy->func = sig->func;
    copy->returntype = sig->returntype;
    copy->pdecl = cmd_copy_paramdecl(sig->pdecl);

    return copy;
}

struct cmd_argv *
cmd_append_argv(struct cmd_argv *argv,
                struct cmd_value *v) {
    struct cmd_argv *av = 
        mem_realloc(argv, 
                    sizeof(struct cmd_argv) + 
                    (argv->argc * sizeof(struct cmd_value *)));

    g_return_val_if_fail(av != NULL, NULL);

    argv = av;
    argv->argv[argv->argc] = v;
    argv->argc++;;

    return argv;
}

struct cmd_argv *
cmd_new_argv(int argc, ...) {
    int i;
    struct cmd_argv *av = mem_alloc(sizeof(struct cmd_argv) + 
                                    argc * sizeof(struct cmd_value));
    va_list ap;

    if(!av)
        return NULL;

    va_start(ap, argc);
    
    av->argc = argc;
    for(i = 0; i < argc; i++) {
        av->argv[i] = va_arg(ap, struct cmd_value *);
        if(!av->argv[i]) {
            cmd_destroy_argv(av);
            return NULL;
        }
    }
    
    va_end(ap);
    return av;
}

struct cmd_argv *
cmd_new_argv_terminated(int dummy, ...) {
    int i;
    struct cmd_argv *av = mem_alloc(sizeof(struct cmd_argv) + 
                                    CMD_MAX_ARGS * sizeof(struct cmd_value));
    struct cmd_value *arg;
    va_list ap;

    if(!av)
        return NULL;

    va_start(ap, dummy);
    
    av->argc = 0;
    arg = va_arg(ap, struct cmd_value *);
    for(i = 0; arg != (void *)-1; i++) {
        av->argv[i] = arg;
        if(!av->argv[i]) {
            cmd_destroy_argv(av);
            return NULL;
        }
        av->argc = i + 1;
        arg = va_arg(ap, struct cmd_value *);
    }
    
    va_end(ap);
    return av;
}

struct cmd *
cmd_new(const char *name,
        struct cmd_argv *argv) {
    struct cmd *cmd = mem_alloc(sizeof(*cmd) + strlen(name) + 1);

    if(!cmd) {
        if(argv)
            cmd_destroy_argv(argv);
        return NULL;
    }

    cmd->name = (char *)&cmd[1];
    strcpy(cmd->name, name);
    cmd->argv = argv;

    return cmd;
}

void
cmd_destroy_keep_argv(struct cmd *cmd) {
    if(!cmd)
        return;

    mem_free(cmd);
}

void
cmd_destroy(struct cmd *cmd) {
    if(!cmd)
        return;

    cmd_destroy_argv(cmd->argv);
    mem_free(cmd);
}

int
cmd_verify_paramdecl(const char *name,
                     struct cmd_paramdecl *pdecl) {
    int i;
    struct cmd_signature *sig = g_hash_table_lookup(cmd_registry, name);

    if(!sig) {
        FAIL("unknown command %s\n", name);
        return 1;
    }

    if(sig->pdecl->count != pdecl->count) {
        FAIL("incorrect number of arguments for %s (got: %d, expected: %d)\n",
             name, sig->pdecl->count, pdecl->count);
        return 1;
    }
    
    for(i = 0; i < sig->pdecl->count; i++) {
        if(sig->pdecl->type[i] != pdecl->type[i]) {
            FAIL("parameter mismatch: parameter %d has incorrect type for %s (got: %s, expected: %s)\n", i+1, name, cmd_type_as_string(sig->pdecl->type[i]), cmd_type_as_string(pdecl->type[i]));
            return 1;
        }
    }
    return 0;
}

int
cmd_match_types(enum cmd_type left,
                enum cmd_type right) {
    if(left == CMD_ANY_T)
        return 1;
    return left == right;
}

int
cmd_verify_args(struct cmd *cmd,
                struct cmd_paramdecl *pdecl,
                struct cmd_value **err) {
    int i;
    if(cmd->argv->argc != pdecl->count) {
        *err = cmd_new_error_val("Incorrect number of arguments for %s "
                                 "(got: %d, expected: %d)",
                                 cmd->name, cmd->argv->argc, pdecl->count);
        return 1;
    }
    
    for(i = 0; i < cmd->argv->argc; i++) {
        if(!cmd_match_types(pdecl->type[i], cmd->argv->argv[i]->type)) {
            *err = cmd_new_error_val("Parameter mismatch: parameter %d "
                                     "has incorrect type for %s (got: %s, "
                                     "expected: %s)",
                                     i+1, cmd->name, 
                                     cmd_type_as_string(cmd->argv->argv[i]->type), cmd_type_as_string(pdecl->type[i]));
            return 1;
        }
    }
    return 0;
}

struct cmd_value *
cmd_do(struct cmd *cmd) {
    struct cmd_value *v = NULL;
    struct cmd_signature *sig = g_hash_table_lookup(cmd_registry, cmd->name);

    //    DEBUG("got: ");
    //    cmd_print(cmd);
    //    printf("\n");

    if(!cmd) {
        FAIL("NULL cmd\n");
        goto cleanup;
    }

    if(!sig) {
        v = cmd_new_error_val("The action '%s' does not exist", cmd->name);
        goto cleanup;
    }
    
    if(cmd_verify_args(cmd, sig->pdecl, &v)) 
        goto cleanup;
    
    v = sig->func(cmd->name, cmd->argv);

 cleanup:

    return v;
}

/* FIXME: actually check things. */

int
cmd_check_type(enum cmd_type type) {
    int i = 0, flag_count = 0;

    return 0;

    /* From 16 onwards special composite types. */
    
    for(i = 0; i < 16; i++)
        if((1 << i) & type)
            flag_count++;
    return flag_count > 1;
}

struct cmd_paramdecl *
cmd_new_paramdecl(int count, ...) {
    int i;
    struct cmd_paramdecl *pd = mem_alloc(sizeof(struct cmd_paramdecl) + 
                                         count * sizeof(enum cmd_type));
    va_list ap;

    if(!pd)
        return NULL;
    
    va_start(ap, count);
    
    pd->count = count;
    for(i = 0; i < count; i++)
        pd->type[i] = va_arg(ap, enum cmd_type);
    
    va_end(ap);
    return pd;
}

void
cmd_destroy_paramdecl(struct cmd_paramdecl *pd) {
    mem_free(pd);
}

int
cmd_register(const char *name,
             const char *description,
             struct cmd_value *(*func)(const char *name,
                                       struct cmd_argv *argv),
             enum cmd_type returntype,
             struct cmd_paramdecl *pdecl) {
    int i;
    struct cmd_signature *cmd;

    if(!cmd_registry)
        cmd_registry = g_hash_table_new(g_str_hash, g_str_equal);

    if(!cmd_registry)
        return ENOMEM;

    if(g_hash_table_lookup(cmd_registry, name))
        return EEXIST;

    if(cmd_check_type(returntype))
        return EINVAL;
    
    for(i = 0; i < pdecl->count; i++)
        if(cmd_check_type(pdecl->type[i]))
            return EINVAL;
    
    cmd = mem_alloc(sizeof(*cmd) + strlen(name) + strlen(description) + 2);
    
    if(!cmd)
        return ENOMEM;

    cmd->name = (char *)&cmd[1];
    strcpy(cmd->name, name);
    cmd->description = ((char *)&cmd[1]) + strlen(name) + 1;
    strcpy(cmd->description, description);
    cmd->func = func;
    cmd->returntype = returntype;
    cmd->pdecl = pdecl;

    g_hash_table_insert(cmd_registry, cmd->name, cmd);

    return 0;
}


void
cmd_unregister(const char *name) {
    struct cmd_signature *cmd = g_hash_table_lookup(cmd_registry, name);

    if(!cmd)
        return;

    g_hash_table_remove(cmd_registry, name);

    mem_free(cmd);

}

int
cmd_init() {
    cmd_shell_init();
    cmd_file_init();
    cmd_edit_init();
    cmd_markers_init();
    cmd_select_init();
    cmd_view_init();
    cmd_play_init();
    cmd_events_init();
    cmd_mixer_init();
    return 0;
}

