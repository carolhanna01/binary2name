/* html.c - HTML state machine.
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

/* The state machine in this program can be summarized with the
   following components:

   - A list of events that are handled by the machine.  These are
     actually the characters read from the input which can take
     values from 0 to UCHAR_MAX.  The NUL character is special
     because it is also used as a sentinel to mark the buffer end.

   - A list of actions that describe how the machine reacts to an
     event.  The return value is used in a multi-way decision tree
     to determine the statements to run on a particular action.

   - A set of states for each possible condition of the machine.
     Each state corresponds to a table containing as many elements
     as events.  For each event in a table, a corresponding action
     is defined.

   The list of events can be separated in subsets which constantly
   share the same actions.  For example, numeric characters always
   return the same action for a particular state.  Therefore, a
   GENERIC table is defined for the set of possible events and
   returns a subset.  This smaller set can then be used to define
   smaller state tables.
*/

#include <stdlib.h>

#include "context.h"
#include "match.h"
#include "options.h"

#include "event.h"
#include "html.h"
#include "sgml.h"

enum
{
  C2E,  /* Comment to element                   */
  E2C,  /* Element to comment                   */
  E2D,  /* Element to double-quote              */
  E2S,  /* Element to single-quote              */
  LINE, /* New line character                   */
  NOP,  /* No operation                         */
  PAT,  /* Pattern                              */
  POP,  /* Pop from stack                       */
  PL2C, /* Pattern-line to character reference  */
  PL2E, /* Pattern-line to element              */
  PL2P, /* Pattern-line to pattern              */
  PS2C, /* Pattern-space to character reference */
  PS2E, /* Pattern-space to element             */
  PS2P, /* Pattern-space to pattern             */
  PW2C, /* Pattern-word to character reference  */
  PW2E, /* Pattern-word to element              */
  PW2L, /* Pattern-word to line                 */
  PW2S, /* Pattern-word to space                */
  S2C,  /* Space to character reference         */
  S2E,  /* Space to element                     */
  S2N,  /* space to NUL byte                    */
  S2P,  /* Space to pattern                     */
  S2PC, /* Space to pattern character reference */
  S2PE, /* Space to pattern element             */
  S2W,  /* Space to word                        */
  W2C,  /* Word to character reference          */
  W2E,  /* Word to element                      */
  W2L,  /* Word to line                         */
  W2N,  /* word to NUL byte                     */
  W2P,  /* Word to pattern                      */
  W2PC, /* Word to pattern character reference  */
  W2PE, /* Word to pattern element              */
  W2S,  /* Word to space                        */
  WRD   /* Word                                 */
};

/*    NU,   SP,  pSP,   NL,  pNL,   NP,  pNP,   DQ,  pDQ,
            AM,  pAM,   SQ,  pSQ,   DA,  pDA,   DI,  pDI,
            LT,  pLT,   GT,  pGT,   LE,  pLE,   UN,  pUN */
static unsigned char pspace[] =
  {  S2N,  NOP,  NOP, LINE, LINE,  NOP,  NOP, PS2P, PS2P,
          PS2C, PS2C, PS2P, PS2P, PS2P, PS2P, PS2P, PS2P,
          PS2E, PS2E, PS2P, PS2P, PS2P, PS2P, PS2P, PS2P };
static unsigned char space[] =
  {  S2N,  NOP,  NOP, LINE, LINE,  NOP,  NOP,  S2W,  S2P,
           S2C, S2PC,  S2W,  S2P,  S2W,  S2P,  S2W,  S2P,
           S2E, S2PE,  S2W,  S2P,  S2W,  S2P,  S2W,  S2P };
static unsigned char pword[] =
  {  W2N, PW2S, PW2S, PW2L, PW2L, PW2S, PW2S,  PAT,  PAT,
          PW2C, PW2C,  PAT,  PAT,  PAT,  PAT,  PAT,  PAT,
          PW2E, PW2E,  PAT,  PAT,  PAT,  PAT,  PAT,  PAT };
static unsigned char word[] =
  {  W2N,  W2S,  W2S,  W2L,  W2L,  W2S,  W2S,  WRD,  W2P,
           W2C, W2PC,  WRD,  W2P,  WRD,  W2P,  WRD,  W2P,
           W2E, W2PE,  WRD,  W2P,  WRD,  W2P,  WRD,  W2P };
static unsigned char pline[] =
  {  S2N,  NOP,  NOP, LINE, LINE,  NOP,  NOP, PL2P, PL2P,
          PL2C, PL2C, PL2P, PL2P, PL2P, PL2P, PL2P, PL2P,
          PL2E, PL2E, PL2P, PL2P, PL2P, PL2P, PL2P, PL2P };
static unsigned char element[] =
  {  S2N,  NOP,  NOP, LINE, LINE,  NOP,  NOP,  E2D,  E2D,
           NOP,  NOP,  E2S,  E2S,  E2C,  E2C,  NOP,  NOP,
           NOP,  NOP,  POP,  POP,  NOP,  NOP,  NOP,  NOP };
static unsigned char comment[] =
  {  S2N,  NOP,  NOP, LINE, LINE,  NOP,  NOP,  NOP,  NOP,
           NOP,  NOP,  NOP,  NOP,  C2E,  C2E,  NOP,  NOP,
           NOP,  NOP,  NOP,  NOP,  NOP,  NOP,  NOP,  NOP };
static unsigned char squote[] =
  {  S2N,  NOP,  NOP, LINE, LINE,  NOP,  NOP,  NOP,  NOP,
           NOP,  NOP,  POP,  POP,  NOP,  NOP,  NOP,  NOP,
           NOP,  NOP,  NOP,  NOP,  NOP,  NOP,  NOP,  NOP };
static unsigned char dquote[] =
  {  S2N,  NOP,  NOP, LINE, LINE,  NOP,  NOP,  POP,  POP,
           NOP,  NOP,  NOP,  NOP,  NOP,  NOP,  NOP,  NOP,
           NOP,  NOP,  NOP,  NOP,  NOP,  NOP,  NOP,  NOP };

unsigned char *html_first = space;

#define POP(t)       (t = *(--context->pattern_table))
#define PUSH(t,v)    (*(context->pattern_table++) = t, t = v)

#define find_last()                                                       \
  do                                                                      \
    {                                                                     \
      ret = 0;                                                            \
      goto fin;                                                           \
    }                                                                     \
  while (0)

#define find_character(inc)                                               \
  do                                                                      \
    {                                                                     \
      c = get_character (&end, lim);                                      \
      switch (table[event[c]])                                            \
        {                                                                 \
        case S2N:                                                         \
        case W2N:                                                         \
          find_last ();                                                   \
        case NOP:                                                         \
          break;                                                          \
                                                                          \
        case LINE:                                                        \
          num_lines++;                                                    \
          break;                                                          \
                                                                          \
        case PL2C: case PS2C:                                             \
        case PL2E: case PS2E:                                             \
        case PL2P: case PS2P:                                             \
          table = pword;                                                  \
          inc;                                                            \
        case PAT:                                                         \
        case PW2C:                                                        \
        case PW2E:                                                        \
          *fpos++ = c;                                                    \
          find_next ();                                                   \
          break;                                                          \
                                                                          \
        case PW2L:                                                        \
          num_lines++;                                                    \
        case PW2S:                                                        \
          table = pspace;                                                 \
          *fpos++ = ' ';                                                  \
          find_space ();                                                  \
          break;                                                          \
                                                                          \
        case S2P:                                                         \
        case S2PC:                                                        \
        case S2PE:                                                        \
          inc;                                                            \
        case W2P:                                                         \
        case W2PC:                                                        \
        case W2PE:                                                        \
          table = pword;                                                  \
          *fpos++ = c;                                                    \
          find_first ();                                                  \
          break;                                                          \
                                                                          \
        case S2C:                                                         \
        case S2E:                                                         \
        case S2W:                                                         \
          table = word;                                                   \
          *fpos++ = c;                                                    \
          inc;                                                            \
          break;                                                          \
                                                                          \
        case W2L:                                                         \
          num_lines++;                                                    \
        case W2S:                                                         \
          table = space;                                                  \
          c = ' ';                                                        \
        case W2C:                                                         \
        case W2E:                                                         \
        case WRD:                                                         \
          *fpos++ = c;                                                    \
          break;                                                          \
                                                                          \
        default:                                                          \
          abort ();                                                       \
        }                                                                 \
    }                                                                     \
  while (0)

#define find_element(inc)                                                 \
  switch (get_element (&end, lim))                                        \
    {                                                                     \
    case E_NU:                                                            \
      find_last ();                                                       \
      break;                                                              \
    case E_IG:                                                            \
      PUSH (table, element);                                              \
      break;                                                              \
    case E_SP:                                                            \
      switch (table[event[c]])                                            \
        {                                                                 \
        case W2E:                                                         \
        case W2PE:                                                        \
          table = space;                                                  \
          *fpos++ = ' ';                                                  \
        case S2E:                                                         \
        case S2PE:                                                        \
        case PL2E:                                                        \
        case PS2E:                                                        \
          break;                                                          \
        case PW2E:                                                        \
          table = pspace;                                                 \
          *fpos++ = ' ';                                                  \
          find_space ();                                                  \
          break;                                                          \
        default:                                                          \
          abort ();                                                       \
        }                                                                 \
      PUSH (table, element);                                              \
      break;                                                              \
    case E_NL:                                                            \
      switch (table[event[c]])                                            \
        {                                                                 \
        case W2E:                                                         \
        case W2PE:                                                        \
          table = space;                                                  \
          *fpos++ = ' ';                                                  \
        case S2E:                                                         \
        case S2PE:                                                        \
          break;                                                          \
        case PL2E:                                                        \
          num_paragraphs++;                                               \
          table = space;                                                  \
          *(fpos - 1) = '\f';                                             \
          break;                                                          \
        case PS2E:                                                        \
          table = pline;                                                  \
          break;                                                          \
        case PW2E:                                                        \
          table = pline;                                                  \
          *fpos++ = ' ';                                                  \
          find_space ();                                                  \
          break;                                                          \
        default:                                                          \
          abort ();                                                       \
        }                                                                 \
      PUSH (table, element);                                              \
      break;                                                              \
    case E_NP:                                                            \
      switch (table[event[c]])                                            \
        {                                                                 \
        case W2E:                                                         \
        case W2PE:                                                        \
        case PW2E:                                                        \
          *fpos++ = '\f';                                                 \
          break;                                                          \
        case S2E:                                                         \
        case S2PE:                                                        \
        case PL2E:                                                        \
        case PS2E:                                                        \
          *(fpos - 1) = '\f';                                             \
          break;                                                          \
        default:                                                          \
          abort ();                                                       \
        }                                                                 \
      num_paragraphs++;                                                   \
      table = space;                                                      \
      PUSH (table, element);                                              \
      break;                                                              \
    case E_NA:                                                            \
      *fpos++ = c;                                                        \
      switch (table[event[c]])                                            \
        {                                                                 \
        case S2E:                                                         \
          inc;                                                            \
        case W2E:                                                         \
          break;                                                          \
        case S2PE:                                                        \
          inc;                                                            \
        case W2PE:                                                        \
          table = pword;                                                  \
          find_first ();                                                  \
          break;                                                          \
        case PL2E:                                                        \
        case PS2E:                                                        \
          table = pword;                                                  \
          inc;                                                            \
        case PW2E:                                                        \
          find_next ();                                                   \
          break;                                                          \
        default:                                                          \
          abort ();                                                       \
        }                                                                 \
      break;                                                              \
    default:                                                              \
      abort ();                                                           \
    }

int
html_find (char **pos, char **buf, char *lim)
{
  register unsigned char c;
  register char *fpos;
  unsigned char *table;
  int ret, num_paragraphs, num_lines, num_words;
  char *end;

  table = *context->pattern_table;
  num_paragraphs = context->num_paragraphs;
  num_lines = context->num_lines;
  num_words = context->num_words;
  end = *buf;
  fpos = *pos;

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
              find_last ();
            }
        case NOP:
          break;

        case LINE:
          num_lines++;
          break;

        case C2E:
          if (*end == '-')
            end++, POP (table);
          break;
        case E2C:
          if (*end == '-')
            end++, PUSH (table, comment);
          break;

        case W2C:  case S2C:
        case W2PC: case S2PC:
        case PW2C: case PS2C: case PL2C:
          find_character (num_words++);
          break;

        case S2E:  case W2E:
        case S2PE: case W2PE:
        case PS2E: case PW2E:
          find_element (num_words++);
          break;

        case E2D:
          PUSH (table, dquote);
          break;
        case E2S:
          PUSH (table, squote);
          break;

        case POP:
          POP (table);
          break;

        case S2P:
          num_words++;
        case W2P:
          table = pword;
          *fpos++ = c;
          find_first ();
          break;

        case PL2P:
        case PS2P:
          num_words++;
          table = pword;
        case PAT:
          *fpos++ = c;
          find_next ();
          break;

        case S2W:
          num_words++;
          table = word;
        case WRD:
          *fpos++ = c;
          break;

        case PW2L:
          num_lines++;
        case PW2S:
          table = pspace;
          *fpos++ = ' ';
          find_space ();
          break;

        case W2L:
          num_lines++;
        case W2S:
          table = space;
          *fpos++ = ' ';
          break;

        default:
          abort ();
        }
    }

 fin:
  context->num_bytes += end - *pos;
  context->num_words = num_words;
  context->num_lines = num_lines;
  context->num_paragraphs = num_paragraphs;
  *context->pattern_table = table;
  *buf = end;
  *pos = fpos;

  return ret - 1;
}

#undef POP
#undef PUSH
#undef find_first
#undef find_next
#undef find_space
#undef find_last

#define POP(t)       (t = *(--context->forward_table))
#define PUSH(t,v)    (*(context->forward_table++) = t, t = v)
#define find_first() nop
#define find_next()  nop
#define find_space() nop
#define find_last()  goto fin

unsigned char *
html_forward (char *pos, char *lim)
{
  register unsigned char c;
  register char *fpos;
  unsigned char *table;
  char *end, *max;
  int num_paragraphs, num_lines;

  table = *context->forward_table ? *context->forward_table : pword;
  max = context->tmp->str + MAX (opt.context/2, context->tmp->bpos - 1);
  fpos = context->tmp->str + context->tmp->fpos;
  num_paragraphs = num_lines = 0;
  end = pos;

  while (fpos < max)
    {
      c = *end++;
      switch (table[event[c]])
        {
        case S2N:
          if (end <= lim)
            fpos--, table = NULL;
          find_last ();
          break;
        case W2N:
          if (end <= lim)
            table = NULL;
          find_last ();
        case LINE:
        case NOP:
          break;

        case C2E:
          if (*end == '-')
            end++, POP (table);
          break;
        case E2C:
          if (*end == '-')
            end++, PUSH (table, comment);
          break;

        case PL2C:
        case PS2C:
        case PW2C:
          find_character (nop);
          break;

        case PL2E:
        case PS2E:
        case PW2E:
          find_element (nop);
          if (*(fpos - 1) == '\f')
            {
              table = NULL;
              fpos--;
              goto fin;
            }
          break;

        case E2D:
          PUSH (table, dquote);
          break;
        case E2S:
          PUSH (table, squote);
          break;

        case POP:
          POP (table);
          break;

        case PW2L:
        case PW2S:
          table = pspace;
          c = ' ';
        case PAT:
          *fpos++ = c;
          break;

        case PL2P:
        case PS2P:
          table = pword;
          *fpos++ = c;
          break;

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

