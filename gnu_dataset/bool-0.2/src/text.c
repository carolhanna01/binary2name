/* text.c - TEXT state machine.
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

/* The state machine in this program can be summarized with the following
   components:

   - A list of events that are handled by the machine.  These are actually
     the characters read from the input which can take values from 0 to
     UCHAR_MAX.  The NUL character is special because it is also used as
     a sentinel to mark the buffer end.

   - A list of actions that describe how the machine reacts to an event.
     The return value is used in a multi-way decision tree to determine
     the statements to run on a particular action.

   - A set of states for each possible condition of the machine.  Each
     state corresponds to a table containing as many elements as events.
     For each event in a table, a corresponding action is defined.

   The list of events can be separated in subsets which constantly
   share the same actions.  For example, numeric characters always
   return the same action for a particular state.  Therefore, a GENERIC
   table is defined for the set of possible events and returns a subset.
   This smaller set can then be used to define smaller state tables.
*/

#include <stdlib.h>

#include "context.h"
#include "match.h"
#include "options.h"

#include "event.h"
#include "text.h"

enum
{
  NOP,  /* no operation                         */
  PAT,  /* pattern                              */
  L2F,  /* line to form feed                    */
  L2L,  /* line to line                         */
  PL2F, /* pattern-line to form feed            */
  PL2L, /* pattern-line to line                 */
  PL2P, /* pattern-line to pattern              */
  PL2D, /* pattern-line to dash                 */
  PS2P, /* pattern-space to pattern             */
  PS2D, /* pattern-space to dash                */
  PS2F, /* pattern-space to form feed           */
  PS2L, /* pattern-space to line                */
  PW2D, /* pattern to dash                      */
  PW2F, /* pattern to form feed                 */
  PW2L, /* pattern to line                      */
  PW2S, /* pattern to space                     */
  S2F,  /* space to form feed                   */
  S2L,  /* space to line                        */
  S2N,  /* space to NUL byte                    */
  S2P,  /* space to pattern                     */
  S2W,  /* space to word                        */
  W2L,  /* word to line                         */
  W2N,  /* word to NUL byte                     */
  W2P,  /* word to pattern                      */
  W2S,  /* word to space                        */
  WRD   /* Word                                 */
};

/*    NU,   SP,  pSP,   NL,  pNL,   NP,  pNP,   DQ,  pDQ,
            AM,  pAM,   SQ,  pSQ,   DA,  pDA,   DI,  pDI,
            LT,  pLT,   GT,  pGT,   LE,  pLE,   UN,  pUN */
static unsigned char pspace[] =
  {  S2N,  NOP,  NOP, PS2L, PS2L, PS2F, PS2F, PS2P, PS2P,
          PS2P, PS2P, PS2P, PS2P, PS2D, PS2D, PS2P, PS2P,
          PS2P, PS2P, PS2P, PS2P, PS2P, PS2P, PS2P, PS2P };
static unsigned char space[] =
  {  S2N,  NOP,  NOP,  S2L,  S2L,  S2F,  S2F,  S2W,  S2P,
           S2W,  S2P,  S2W,  S2P,  S2W,  S2P,  S2W,  S2P,
           S2W,  S2P,  S2W,  S2P,  S2W,  S2P,  S2W,  S2P };
static unsigned char pword[] =
  {  W2N, PW2S, PW2S, PW2L, PW2L, PW2F, PW2F,  PAT,  PAT,
           PAT,  PAT,  PAT,  PAT, PW2D, PW2D,  PAT,  PAT,
           PAT,  PAT,  PAT,  PAT,  PAT,  PAT,  PAT,  PAT };
static unsigned char word[] =
  {  W2N,  W2S,  W2S,  W2L,  W2L,  W2S,  W2S,  WRD,  W2P,
           WRD,  W2P,  WRD,  W2P,  WRD,  W2P,  WRD,  W2P,
           WRD,  W2P,  WRD,  W2P,  WRD,  W2P,  WRD,  W2P };
static unsigned char pline[] =
  {  S2N,  NOP,  NOP, PL2L, PL2L, PL2F, PL2F, PL2P, PL2P,
          PL2P, PL2P, PL2P, PL2P, PL2D, PL2D, PL2P, PL2P,
          PL2P, PL2P, PL2P, PL2P, PL2P, PL2P, PL2P, PL2P };
static unsigned char line[] =
  {  S2N,  NOP,  NOP,  L2L,  L2L,  L2F,  L2F,  S2W,  S2P,
           S2W,  S2P,  S2W,  S2P,  S2W,  S2P,  S2W,  S2P,
           S2W,  S2P,  S2W,  S2P,  S2W,  S2P,  S2W,  S2P };

unsigned char *text_first = space;

int
text_find (char **pos, char **buf, char *lim)
{
  register unsigned char c;
  register char *end;
  unsigned char *table;
  int ret, num_paragraphs, num_lines, num_words;

  table = *context->pattern_table;
  num_paragraphs = context->num_paragraphs;
  num_lines = context->num_lines;
  num_words = context->num_words;
  end = *pos;

  for (;;)
    {
      c = *end++;
      switch (table[event[c]])
        {
        case S2N:
        case W2N:
          if (end > lim)
            {
              end--;
              ret = 0;
              goto fin;
            }
        case WRD:
        case NOP:
          break;

        case PL2L:
          num_paragraphs++;
        case S2L:
        case W2L:
          num_lines++;
          table = line;
          break;

        case L2L:
          num_lines++;
        case L2F:
        case S2F:
          num_paragraphs++;
          break;

        case PL2F:
        case PS2F:
        case PW2F:
          num_paragraphs++;
        case W2S:
          table = space;
          break;

        case S2W:
          num_words++;
          table = word;
          break;

        case S2P:
          num_words++;
        case W2P:
          table = pword;
          find_first ();
          break;

        case PS2D: case PL2D:
        case PS2P: case PL2P:
          num_words++;
          table = pword;
        case PAT:
          find_next ();
          break;

        case PW2S:
          table = pspace;
          find_space ();
          break;
        case PW2L:
          num_lines++;
          table = pline;
          find_space ();
          break;

        case PW2D:
          table = pspace;
          find_maybe ();
          break;

        case PS2L:
          num_lines++;
          table = pline;
          break;

        default:
          abort ();
        }
    }

 fin:
  context->num_bytes += end - *pos;
  context->num_lines = num_lines;
  context->num_words = num_words;
  context->num_paragraphs = num_paragraphs;
  *context->pattern_table = table;
  *buf = *pos = end;

  return ret - 1;
}

unsigned char *
text_forward (char *pos, char *lim)
{
  register unsigned char c;
  register char *end, *fpos;
  unsigned char *table;
  char *max;

  table = *context->forward_table ? *context->forward_table : pword;
  max = context->tmp->str + MAX (opt.context / 2, context->tmp->bpos - 1);
  fpos = context->tmp->str + context->tmp->fpos;
  end = pos;

  while (fpos < max)
    {
      c = *end++;
      switch (table[event[c]])
        {
        case S2N:
          if (end <= lim)
            fpos--, table = NULL;
          goto fin;
        case W2N:
          if (end <= lim)
            table = NULL;
          goto fin;

        case PAT:
        case PW2D:
        case WRD:
          *fpos++ = c;
        case NOP:
          break;

        case PS2D: case PL2D:
        case PS2P: case PL2P:
          *fpos++ = c;
          table = pword;
          break;
        case PW2S:
          *fpos++ = ' ';
          table = pspace;
          break;
        case PW2L:
          *fpos++ = ' ';
        case PS2L:
          table = pline;
          break;

        case PL2L:
        case PL2F:
        case PS2F:
          fpos--;
        case PW2F:
          table = NULL;
          goto fin;

        default:
          abort ();
        }
    }

  if (table != pword)
    fpos--;
  table = NULL;

 fin:
  *context->forward_table = table;
  context->tmp->fpos = fpos - context->tmp->str;
  return table;
}

unsigned char *
text_backward (char *pos)
{
  register unsigned char c;
  register char *beg, *bpos;
  unsigned char *table;

  bpos = context->tmp->str + opt.context;
  table = pspace;
  beg = pos;

  while (bpos > context->tmp->str)
    {
      c = *(--beg);
      switch (table[event[c]])
        {
        case PAT:
        case PW2D:
        case WRD:
          *bpos-- = c;
        case NOP:
          break;

        case PL2D: case PS2D:
        case PL2P: case PS2P:
          *bpos-- = c;
          table = pword;
          break;
        case PW2S:
          *bpos-- = ' ';
          pos = beg + 1;
          table = pspace;
          break;
        case PW2L:
          *bpos-- = ' ';
          pos = beg + 1;
        case PS2L:
          table = pline;
          break;

        case W2N:
          pos = beg + 1;
          goto fin;

        case PL2L:
        case PL2F:
        case PS2F:
        case S2N:
          bpos++;
        case PW2F:
          goto fin;

        default:
          abort ();
        }
    }

 fin:
  context->tmp->bpos = bpos - context->tmp->str + 1;
  return pos;
}

