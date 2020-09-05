/* context.h - interface to context.c
   Copyright (C) 2001 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.  */

/* Written by Marc Tardif <intmktg@cam.org>.  */

#ifndef CONTEXT_H
#define CONTEXT_H

#define MAX_DEPTH 3

/* Supported file formats.                                               */
enum extension
{
  SOURCE,
  TEXT,
  HTML
};

/* Attributes retained for each line.                                    */
struct line
{
  int bpos;                      /* Backward position in str buffer.     */
  int fpos;                      /* Forward position in str buffer.      */
  int byte;                      /* Byte offset in input file.           */
  int line;                      /* Line offset in input file.           */
  char str[4];                   /* Context line of matched pattern.     */
};

/* Structure containing everything.                                      */
struct context
{
  unsigned char *pstack[MAX_DEPTH]; /* Stack of pattern states.          */
  unsigned char **pattern_table; /* Pattern state table.                 */
  unsigned char *fstack[MAX_DEPTH]; /* Stack of forward context states.  */
  unsigned char **forward_table; /* Forward context state table.         */
  int sstack[MAX_DEPTH];         /* Stack of states.                     */
  int *state;                    /* Current state.                       */
  struct line ***lines;          /* Context lines for each pattern.      */
  struct line *tmp;              /* Temporary line buffer.               */
  int num_pat;                   /* Number of patterns.                  */
  int num_bytes;                 /* Total byte count.                    */
  int num_words;                 /* Total word count.                    */
  int num_lines;                 /* Total line count.                    */
  int num_paragraphs;            /* Total paragraph count.               */
};

extern struct context *context;
extern int  context_init  (char *);
extern void context_prep  (enum extension);
extern int  context_find  (char **, char **, char *);
extern int  context_print (void);
extern void context_free  (void);

#endif
