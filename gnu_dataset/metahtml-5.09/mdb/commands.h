/* commands.h: -*- C -*-  Commands implemented for MDB. */

/* Author: Brian J. Fox (bfox@ai.mit.edu) Sat Sep 30 13:24:27 1995.

   This file is part of <Meta-HTML>(tm), a system for the rapid deployment
   of Internet and Intranet applications via the use of the Meta-HTML
   language.

   Copyright (c) 1995, 1996, Brian J. Fox (bfox@ai.mit.edu).
   Copyright (c) 1996, Universal Access Inc. (http://www.ua.com).

   Meta-HTML is free software; you can redistribute it and/or modify
   it under the terms of the UAI Free Software License as published
   by Universal Access Inc.; either version 1, or (at your option) any
   later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   UAI Free Software License for more details.

   You should have received a copy of the UAI Free Software License
   along with this program; if you have not, you may obtain one by
   writing to:

   Universal Access Inc.
   129 El Paseo Court
   Santa Barbara, CA
   93101  */

#if !defined (_MDB_COMMANDS_H_)
#define _MDB_COMMANDS_H_

#if defined (__cplusplus)
extern "C"
{
#endif
#define MDBArgs char *name, char *line
typedef char *MFunction (MDBArgs);

typedef struct {
  char *name;		/* Invocation name. */
  char *alias;		/* For commands which would otherwise be ambiguous. */
  MFunction *handler;	/* Function to call. */
  char *invocation;	/* Description of argumnts. */
  char *description;	/* Documentation string. */
} MDBCommand;

extern char *mdb_command (char *line);
extern char *mdb_redo (void);
extern MDBCommand mdb_command_table[];

/* When non-zero, it is time to quit. */
extern int MDB_QuitFlag;

/* When non-zero, it is time to continue. */
extern int MDB_ContFlag;

#if defined (__cplusplus)
}
#endif

#endif /* _MDB_COMMANDS_H_ */
