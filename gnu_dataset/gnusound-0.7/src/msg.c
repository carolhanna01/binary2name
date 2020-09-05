/*
 * GNUsound - a sound editor for GNOME.
 * Copyright (C) 2005  Pascal Haakmat <a.haakmat@chello.nl>
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
#include "cmd.h"
#include "msg.h"

/**
 * @file 
 *
 * This provides a mechanism which can be used to publish and
 * subscribe to events. It's similar to the g_signal stuff but uses
 * the command infrastructure to dispatch events.
 */

static int
msg_get_subscription_id(struct msg *msg) {

    /* FIXME: overflows without warning */

    return ++msg->next_id;
}

/**
 * Sends a message with arguments.
 * @param msg The message container.
 * @param message The message to send.
 * @param args Arguments for this message. The arguments are destroyed
 * on return.
 */

void
msg_send(struct msg *msg,
         const char *message,
         struct cmd_argv *args) {
    GList *l;
    struct subscription *sub;
    struct cmd *cmd;
    struct cmd_argv *hargs;
    struct cmd_value *r;

    g_return_if_fail(msg != NULL);
    g_return_if_fail(message != NULL);
    g_return_if_fail(args != NULL);

    hargs = cmd_append_argv(args, cmd_new_voidp_val(NULL));
    if(!hargs) {

        FAIL("couldn't append user_data argument\n");
        cmd_destroy_argv(args);
        return;

    }

    DEBUG("sending message %s\n", message);

    for(l = msg->subscriptions; l; l = l->next) {
        
        sub = l->data;
        
        if(strcmp(message, sub->message))
            continue;

        DEBUG("recipient: %s\n", sub->handler);
        cmd_destroy_value(hargs->argv[hargs->argc - 1]);
        hargs->argv[hargs->argc - 1] = cmd_new_voidp_val(sub->user_data);

        cmd = cmd_new(sub->handler, hargs);
        r = cmd_do(cmd);
        if(cmd_is_error(r)) {
            FAIL("recipient signalled error: %s\n",
                 cmd_get_error_message(r));
        }
        cmd_destroy_value(r);
        cmd_destroy_keep_argv(cmd);

    }

    cmd_destroy_argv(hargs);

}

/**
 * Removes a handler from a message.
 * @param msg The message container.
 * @param message The message.
 * @param id The subscription ID.
 */

void
msg_unsubscribe(struct msg *msg,
                int subscription_id) {
    GList *l;
    struct subscription *sub;

    for(l = msg->subscriptions; l; l = l->next) {

        sub = l->data;

        if(sub->id == subscription_id) {
            
            DEBUG("unsubscribing %s from %s\n", sub->handler, sub->message);

            msg->subscriptions = g_list_remove_link(msg->subscriptions, l);

            g_free(sub->message);
            g_free(sub->handler);
            mem_free(sub);

            g_list_free(l);

            return;
            
        }

    }

}

/**
 * Binds a handler to a message. The message must exist.
 * @param msg The message container.
 * @param message The message to bind to.
 * @param handler The handler to bind the message to.
 * @param user_data User data to pass to the handler.
 * @return Positive subscription ID or negative error code.
 */

int
msg_subscribe(struct msg *msg,
              const char *message,
              const char *handler,
              void *user_data) {
    GList *l;
    int message_exists = FALSE;
    struct subscription *sub;
    struct cmd_signature *msig;

    /* Find message. */

    for(l = msg->messages; l; l = l->next) {

        msig = l->data;

        if(!strcmp(msig->name, message)) {

            message_exists = TRUE;
            break;

        }

    }

    g_assert(message_exists == TRUE);

    g_return_val_if_fail(message_exists == TRUE, -1);

    sub = mem_alloc(sizeof(*sub));

    g_return_val_if_fail(sub != NULL, -1);

    sub->id = msg_get_subscription_id(msg);
    sub->message = g_strdup(message);
    sub->handler = g_strdup(handler);
    sub->user_data = user_data;
    
    msg->subscriptions = g_list_append(msg->subscriptions, sub);

    return sub->id;
}

/**
 * Publishes the specified messages.  After publication, messages
 * can be subscribed to using msg_subscribe(). A message is
 * defined in terms of struct cmd_signature, but with some additional
 * rules:
 *
 * - The message->func field is ignored -- handler functions are
 *   dynamically bound to messages through msg_subscribe() and
 *   msg_send(). 
 * - The message->return_type must be CMD_VOID_T since a msg
 *   allows for one-way communication only.
 * - The message->pdecl must specify the last parameter 
 *   to be of type CMD_voidp_T. This parameter is used to pass 
 *   handler-dependant user data.
 *
 * @param msg The msg.
 * @param message The signature describing the message. The message
 * cannot already exist.
 * @return 0 on success, non-zero error code otherwise.
 */

int
msg_publish(struct msg *msg,
            struct cmd_signature *message) {
    GList *l;
    struct cmd_signature *sig = cmd_copy_signature(message), *p;

    g_return_val_if_fail(msg != NULL, 1);
    g_return_val_if_fail(sig != NULL, 1);
    g_return_val_if_fail(message->pdecl->count > 1, 1);
    g_return_val_if_fail(message->pdecl->type[message->pdecl->count - 1] ==
                         CMD_voidp_T, 1);
    g_return_val_if_fail(message->returntype == CMD_VOID_T, 1);
    
    for(l = msg->messages; l; l = l->next) {
        
        p = l->data;

        if(!strcmp(p->name, sig->name)) {
            FAIL("message %s already exists\n", sig->name);
            return 1;
        }

    }

    msg->messages = g_list_append(msg->messages, sig);

    return 0;

}

void
msg_destroy(struct msg *msg) {
    GList *l;
    struct cmd_signature *sig;
    struct subscription *sub;

    for(l = msg->messages; l; l = l->next) {

        sig = l->data;

        cmd_free_signature(sig);

    }

    g_list_free(msg->messages);

    for(l = msg->subscriptions; l; l = l->next) {

        sub = l->data;

        mem_free(sub->message);
        mem_free(sub->handler);
        mem_free(sub);
        
    }

    g_list_free(msg->subscriptions);

    mem_free(msg);
}

/**
 * Creates a new msg dispatcher.
 * @return A pointer to the msg dispatcher or NULL on error.
 */

struct msg *
msg_new() {
    int i;
    struct msg *msg = mem_alloc(sizeof(*msg));

    g_return_val_if_fail(msg != NULL, NULL);

    msg->messages = NULL;
    msg->subscriptions = NULL;
    msg->next_id = 0;

    return msg;
}
