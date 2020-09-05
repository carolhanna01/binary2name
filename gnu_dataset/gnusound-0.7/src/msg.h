/*
 * GNUsound - a sound editor for GNOME.
 * Copyright (C) 2004  Pascal Haakmat <a.haakmat@chello.nl>
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

#ifndef MSG_H
#define MSG_H

#include <glib.h>
#include "cmd.h"

struct subscription {

    /* Subscription ID. */

    int id;

    /* Message being subscribed to. */

    char *message;

    /* Handler for the message. */

    char *handler;

    /* User data to pass to the handler. */

    void *user_data;
};

struct msg {

    /* Subscription ID counter. */

    int next_id;

    /* List of struct cmd_signature */

    GList *messages;

    /* List of struct subscriber */

    GList *subscriptions;

};

struct cmd_argv;
struct cmd_signature;

void
msg_send(struct msg *msg,
         const char *message,
         struct cmd_argv *args);

int
msg_subscribe(struct msg *msg,
              const char *message,
              const char *handler,
              void *user_data);

void
msg_unsubscribe(struct msg *msg,
                int subscription_id);


int
msg_publish(struct msg *msg,
            struct cmd_signature *message);

void
msg_destroy(struct msg *msglisher);

struct msg *
msg_new();


#endif /* MSG_H */
