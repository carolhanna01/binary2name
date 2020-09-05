/*------------------------->  ANSI C - sourcefile  <-------------------------*/
/* Copyright (C) 199x by International Computer Science Institute            */
/* This file is part of the GNU Sather library. It is free software; you may */
/* redistribute  and/or modify it under the terms of the GNU Library General */
/* Public  License (LGPL)  as published  by the  Free  Software  Foundation; */
/* either version 3 of the license, or (at your option) any later version.   */
/* This  library  is distributed  in the  hope that it will  be  useful, but */
/* WITHOUT ANY WARRANTY without even the implied warranty of MERCHANTABILITY */
/* or FITNESS FOR A PARTICULAR PURPOSE. See Doc/LGPL for more details.       */
/* The license text is also available from:  Free Software Foundation, Inc., */
/* 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA                     */
/*------------>  Please email comments to <bug-sather@gnu.org>  <------------*/

/*								       
 * br_barrier.c - Brahma Barrier
 * Adopted from Lok Tin Liu's paragon NXAM barrier.
 * A "standard" up and down the tree implementation
 */

#include "brahma.h"

static int up_buf[2];
static int down_buf;

int BR_barrier_init()
{
  up_buf[0] = up_buf[1] = down_buf = 0;
}

void incr_handler(BR_cluster_t cl,BR_word_t val, BR_word_t a1, BR_word_t a2, BR_word_t a3)
{
	(*((int*)val))++;
}

void BR_barrier_up()
{
    int parent = (BR_HERE() - 1) / 2;
    int odd_child = 2 * BR_HERE() + 1;
    int even_child = 2 * BR_HERE() + 2;
    int parity = BR_HERE() & 1;

    if (BR_HERE() == 0) {
      if (BR_CLUSTERS() != 1) 
	if (BR_CLUSTERS() == 2) {
	  while (up_buf[1] == 0)
	    BR_POLL();
	} else {
	  while (up_buf[0] == 0 || up_buf[1] == 0)
	    BR_POLL();
	}
    } else {
      if (odd_child >= BR_CLUSTERS()) {
	BR_REQUEST_4(parent, incr_handler, (int)&up_buf[parity], 0, 0, 0);
      } else if (even_child >= BR_CLUSTERS()) {
	while (up_buf[1] == 0)
	  BR_POLL();
	BR_REQUEST_4(parent, incr_handler, (int)&up_buf[parity], 0, 0, 0);
      } else {
	while (up_buf[0] == 0 || up_buf[1] == 0)
	  BR_POLL();
	BR_REQUEST_4(parent, incr_handler, (int)&up_buf[parity], 0, 0, 0);
      }
    }
    up_buf[0] = up_buf[1] = 0;
}

void BR_barrier_down()
{
  int left = 2 * BR_HERE() + 1;
  int right = 2 * BR_HERE() + 2;

  if (BR_HERE() != 0) {
    while (down_buf == 0)
      BR_POLL();
  }
  if (left < BR_CLUSTERS())
    BR_REQUEST_4(left, incr_handler, (int)&down_buf, 0, 0, 0);
  if (right < BR_CLUSTERS())
    BR_REQUEST_4(right, incr_handler, (int)&down_buf, 0, 0, 0);
  down_buf = 0;
} 

void BR_BARRIER()
{
  BR_barrier_up();
  BR_barrier_down();
}

