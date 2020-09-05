/* context.c - Context functions.
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

/* The context algorithm in this program can be summarized with the
   following illustration:

   str             fpos                   max bpos
    v               v                       v v
    +------------------------------------------------------------+
                               opt.context

   The str address points to the beginning of the memory location where
   the context for a pattern is manipulated.  The size of this segment
   is kept in the opt.context global variable which is usually set to
   DEFAULT_CONTEXT.  The actual memory allocated is increased by one
   byte to account for padding as described below.

   When a keyword is found in the input, the context preceding the current
   position is copied backwards in the str buffer.  The starting position
   is set to str + opt.context + 1, where characters are copied from the
   input buffer until a paragraph delimiter is reached or the beginning
   of the str buffer.  The resulting offset from str is stored in the
   bpos variable.

   The maximum forward context is then set according to the number of
   characters read backwards.  The object is to retain as much context as
   possible while keeping the keyword as centered as possible.  Therefore,
   max is set to opt.context / 2 or bpos depending on which is greater.
   Characters are then copied from the input buffer starting at str until
   a paragraph delimiter is reached or max.  If the end of the current
   input buffer is reached prematurely, new data is read into the
   input buffer and the remaining context is processed.  If the last
   copied character is a space, fpos is set to the previous offset,
   otherwise it is set to the current offset.  This explains the need for
   the padding byte in opt.context, otherwise a byte from the backward
   context could be overwritten which would yield a context of one byte
   less than opt.context.

   After fpos has been determined, the value of bpos must be re-
   evaluated.  If fpos is greater than bpos, the latter is set to
   fpos + 1, otherwise it remains unchanged.  The actual context
   then consists of two fragments:
   - The first half is contained between str + bpos and str +
     opt.context.
   - The second half is located at the beginning of the str
     buffer and extends for fpos bytes.

   If a pattern consists of multiple keywords, when using the NEAR
   operator, the last two keyword matches are merged to extract as much
   relevant context as possible.  The previous match is stored in the
   line buffer for the current count.  The current match is stored in the
   tmp buffer.  To merge these buffers, the distance in number of bytes
   between each match is evaluated against the size of opt.context.
   If the distance is greater, two halves are concatenated with the
   sep_context string.  If smaller, the common segments of each buffer
   are centered to provide as much context on the left and right of
   each keyword.
*/

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "context.h"
#include "expr.h"
#include "mem.h"
#include "options.h"

#include "event.h"
#include "html.h"
#include "text.h"

#define POP()   (*(--context->state))
#define PUSH(v) (*(context->state++) = v)

enum state
{
  SRCH = -2, /* search buffer                   */
  SAVE       /* save buffer                     */
             /* this value should be zero       */
};

struct context *context;

/* separator strings */
static char sep_context[] = "... ";
static char sep_field[]   = ":";

/* pointer to text or html functions */
static unsigned char * (*go_forward) (char *, char *);
static int (*go_find) (char **, char **, char *);

int
context_init (char *str)
{
  mem_init ();
  context = mem_alloc (sizeof (struct context));
  context->num_pat = expr_init (str);
  if (!context->num_pat)
    return 0;

  context->tmp = mem_alloc (sizeof (struct line) - sizeof (char[4])
      + opt.context + 1);

  if (!opt.out_quiet)
    {
      register int i, j, occurrences;
      struct line **line;

      context->lines = mem_alloc (context->num_pat
          * sizeof (struct line **));
      occurrences = opt.occurrences ? opt.occurrences : 1;

      for (i = 0; i < context->num_pat; i++)
        {
          line = mem_alloc (occurrences * sizeof (struct line *));
          for (j = 0; j < occurrences; j++)
            line[j] = mem_alloc (sizeof (struct line) - sizeof (char[4])
                + opt.context + 1);

          context->lines[i] = line;
        }
    }

  return context->num_pat;
}

void
context_prep (enum extension type)
{
  context->num_bytes = 0;
  context->num_words = 0;
  context->num_lines = 1;
  context->num_paragraphs = 1;

  context->pattern_table = context->pstack;
  context->forward_table = context->fstack;
  *context->forward_table = NULL;
  context->state = context->sstack;
  PUSH (SRCH);

  if (type == HTML)
    {
      *context->pattern_table = html_first;
      go_forward = html_forward;
      go_find = html_find;
    }
  else
    {
      if (type == SOURCE)
        event['\n'] = NP;
      else
        event['\n'] = NL;

      *context->pattern_table = text_first;
      go_forward = text_forward;
      go_find = text_find;
    }

  expr_prep ();
}

static void
line_merge (struct line *dest, struct line *src, int distance)
{
  register int dest_len, src_len;

  dest_len = dest->bpos + distance + src->fpos;
  if (dest_len > opt.context)
    {
      if (distance > opt.context - 5)
        {
          register int half;

          half = (opt.context - sizeof sep_context + 1) / 2;
          src_len = dest->bpos - MIN (half / 2, dest->bpos);
          if (src_len)
            memmove (dest->str, dest->str + src_len, half);
          dest_len = half;

          memcpy (dest->str + dest_len,
              sep_context, sizeof sep_context - 1);
          dest_len += sizeof sep_context - 1;

          src_len = half - MIN (half / 2, src->fpos);
          memcpy (dest->str + dest_len,
              src->str + opt.context + 1 - src_len, src_len);
          dest_len += src_len;

          if (src->fpos)
            {
              memcpy (dest->str + dest_len, src->str,
                  half - src_len);
              dest_len += half - src_len;
            }
        }
      else
        {
          dest_len = (opt.context - distance) / 2;
          if (dest_len > src->fpos)
            dest_len += dest_len - src->fpos;
          if (dest_len < dest->bpos)
            memmove (dest->str, dest->str + dest->bpos - dest_len,
                dest_len + dest->fpos);
          else
            dest_len = dest->bpos;

          dest_len += dest->fpos;
          if (dest->fpos < distance)
            {
              src_len = distance - dest->fpos;
              memcpy (dest->str + dest_len,
                  src->str + opt.context + 1 - src_len, src_len);
              dest_len += src_len;

              src_len = MIN (opt.context - dest_len, src->fpos);
              if (src_len > 0)
                {
                  memcpy (dest->str + dest_len, src->str, src_len);
                  dest_len += src_len;
                }
            }
          else
            {
              src_len = MIN (opt.context - dest_len,
                  src->fpos - dest->fpos + distance);
              if (src_len > 0)
                {
                  memcpy (dest->str + dest_len,
                      src->str + dest->fpos - distance, src_len);
                  dest_len += src_len;
                }
            }
        }
    }
  if (dest->str[dest_len - 1] == ' ')
    dest_len--;
  dest->str[dest_len] = '\0';
}

static void
line_copy (struct line *dest, struct line *src)
{
  register int src_bpos;

  src_bpos = opt.context + 1 - src->bpos;
  memcpy (dest->str, src->str + src->bpos, src_bpos);
  memcpy (dest->str + src_bpos, src->str, src->fpos);
  dest->bpos = src_bpos;
  dest->fpos = src->fpos;
  dest->str[src_bpos + src->fpos] = '\0';
}

static void
line_print (struct line *line, int pat)
{
  if (opt.with_filename)
    printf ("%s%s", opt.filename, sep_field);
  if (opt.with_pattern)
    {
      expr_print (pat);
      printf ("%s", sep_field);
    }
  if (opt.out_line)
    printf ("%d%s", line->line, sep_field);
  if (opt.out_byte)
    printf ("%d%s", line->byte, sep_field);
  printf ("%s\n", line->str);
}

static char *
line_get (int kw, char *pos, char *buf, char *lim)
{
  struct line *tmp;
  int count, near, pat;

  tmp = context->tmp;
  if (*context->forward_table)
    {
      go_forward (buf, lim);
      near = POP ();
      pat = POP ();
      count = expr_count (pat);
      if (near >= 0)
        count--;

      goto mid;
    }

  tmp->bpos = tmp->fpos = 0;
  near = context->num_words;
  while ((pat = expr_next (kw, context->num_bytes, &near)) >= 0)
    {
      count = expr_count (pat);
      if (near >= 0)
        count--;

      if (!opt.out_quiet
          && (!opt.occurrences || count < (int)opt.occurrences))
        {
          struct line *line;

          if (!tmp->bpos)
            {
              char *beg;

              beg = text_backward (pos);
              if (go_forward (buf, lim))
                {
                  PUSH (pat);
                  PUSH (near);
                  return beg;
                }
 mid:
              if (tmp->bpos < tmp->fpos)
                tmp->bpos = tmp->fpos + 1;
              if (tmp->str[tmp->bpos] == ' ')
                tmp->bpos++;
            }

          if (!opt.occurrences)
            count = 0;

          line = context->lines[pat][count];

          if (near > 0)
            line_merge (line, tmp, context->num_bytes - line->byte);
          else
            line_copy (line, tmp);

          line->byte = context->num_bytes;
          line->line = context->num_lines;

          if (!opt.occurrences && near >= 0)
            line_print (line, pat);
        }
      near = context->num_words;
    }

  *context->forward_table = NULL;
  return NULL;
}

int
context_find (char **pos, char **buf, char *lim)
{
  register int state;
  char *beg;

  state = POP ();
  for (;;)
    {
      switch (state)
        {
        case SRCH:
          state = go_find (pos, buf, lim);
          break;
        case SAVE:
          if (!opt.out_quiet)
            beg = text_backward (*pos);
          else
            beg = *pos;
          state = SRCH;
          goto fin;
        default:
          beg = line_get (state, *pos, *buf, lim);
          if (beg)
            goto fin;
          state = SRCH;
          break;
        }
    }

 fin:
  PUSH (state);
  return *pos - beg;
}

int
context_print (void)
{
  register int i, j;
  int count;

  if (!expr_eval ())
    {
      if (opt.list_files == 1)
        printf ("%s\n", opt.filename);
      return 0;
    }

  if (opt.out_quiet)
    {
      if (opt.count_matches)
        for (i = 0; i < context->num_pat; i++)
          {
            if (opt.with_filename)
              printf ("%s%s", opt.filename, sep_field);
            if (opt.with_pattern)
              {
                expr_print (i);
                printf ("%s", sep_field);
              }
            printf ("%d\n", expr_count (i));
          }
      else if (opt.list_files == 2)
        printf ("%s\n", opt.filename);
    }
  else if (opt.occurrences)
    for (i = 0; i < context->num_pat; i++)
      {
        count = expr_count (i);
        for (j = 0; j < opt.occurrences && count--; j++)
          line_print (context->lines[i][j], i);
      }

  return 1;
}

void
context_free (void)
{
  mem_free ();
}

