/* event.h - interface to event.c
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

#ifndef EVENT_H
#define EVENT_H

#define MAX(a,b) ((a)>(b) ? (a) : (b))
#define MIN(a,b) ((a)<(b) ? (a) : (b))

#define nop ((void)0)

#define find_first()                                                      \
  do                                                                      \
    {                                                                     \
      ret = match_first (c);                                              \
      if (ret > 0)                                                        \
        goto fin;                                                         \
    }                                                                     \
  while (0)

#define find_maybe()                                                      \
  do                                                                      \
    {                                                                     \
      ret = match_maybe (c);                                              \
      if (ret > 0)                                                        \
        goto fin;                                                         \
    }                                                                     \
  while (0)

#define find_next()                                                       \
  do                                                                      \
    {                                                                     \
      ret = match_next (c);                                               \
      if (ret < 0)                                                        \
        table = word;                                                     \
      else if (ret > 0)                                                   \
        goto fin;                                                         \
    }                                                                     \
  while (0)

#define find_space()                                                      \
  do                                                                      \
    {                                                                     \
      ret = match_next (' ');                                             \
      if (ret < 0)                                                        \
        table = space;                                                    \
      else if (ret > 0)                                                   \
        goto fin;                                                         \
    }                                                                     \
  while (0)

enum
{
  NU,  /* null          */
  SP,  /* space         */
  pSP, /* space         */
  NL,  /* new line      */
  pNL, /* new line      */
  NP,  /* new page      */
  pNP, /* new page      */
  DQ,  /* double quote  */
  pDQ, /* double quote  */
  AM,  /* ampersand     */
  pAM, /* ampersand     */
  SQ,  /* single quote  */
  pSQ, /* single quote  */
  DA,  /* dash          */
  pDA, /* dash          */
  DI,  /* digit         */
  pDI, /* digit         */
  LT,  /* less than     */
  pLT, /* less than     */
  GT,  /* greater than  */
  pGT, /* greater than  */
  LE,  /* letter        */
  pLE, /* letter        */
  UN,  /* unspecified   */
  pUN  /* unspecified   */
};

extern unsigned char event[];

#endif
