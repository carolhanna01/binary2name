/*									tab:2
 *
 * lam_barrier.c - Active Messages barrier
 *
 * "Copyright (c) 1994 by Lok Tin Liu and The Regents of the University 
 * of California.  All rights reserved."
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose, without fee, and without written agreement is
 * hereby granted, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS."
 *
 * Author: 			Lok Tin Liu
 * Version:			39
 * Creation Date:	Fri Jul  8 15:22:47 1994
 * Filename:		lam_barrier.c
 * History:
 *	RPM	39	Tue Feb 28 22:36:44 1995
 *		took out gross Split-C commentary left in by Lok
 *	LTL	5	Sun Jul 10 15:07:17 1994
 *		Modified from paragon/nxam_barrier.c for TCPAM
 */


#include "brahma.h"
#include "am.h"


static int AM_myproc;
static int AM_procs;

static int up_buf[2];
static int down_buf;

int BR_BARRIER_init()
{
  AM_myproc = BR_HERE();
  AM_procs = BR_CLUSTERS();
  up_buf[0] = up_buf[1] = down_buf = 0;
}

void BR_incr_handler(vnn_t node, void *val)
{
  (*((int*)val))++;
}

void BR_BARRIER_up()
{
  int parent = (AM_myproc - 1) / 2;
  int odd_child = 2 * AM_myproc + 1;
  int even_child = 2 * AM_myproc + 2;
  int parity = AM_myproc & 1;
  
  if (AM_myproc == 0) {
    if (AM_procs != 1) 
      if (AM_procs == 2) {
	while (up_buf[1] == 0)
	  BR_POLL();

      } else {
	while (up_buf[0] == 0 || up_buf[1] == 0)
	  BR_POLL();
      }
  } else {
    if (odd_child >= AM_procs) {
      BR_REQUEST_4(parent, (BR_handler_4_t) BR_incr_handler, (int)&up_buf[parity], 0, 0, 0);
    } else if (even_child >= AM_procs) {
      while (up_buf[1] == 0)
	BR_POLL();
      BR_REQUEST_4(parent, (BR_handler_4_t)BR_incr_handler, (int)&up_buf[parity], 0, 0, 0);
    } else {
      while (up_buf[0] == 0 || up_buf[1] == 0)
	BR_POLL();
      BR_REQUEST_4(parent, (BR_handler_4_t)BR_incr_handler, (int)&up_buf[parity], 0, 0, 0);
    }
  }
  up_buf[0] = up_buf[1] = 0;
}

void BR_BARRIER_down()
{
  int left = 2 * AM_myproc + 1;
  int right = 2 * AM_myproc + 2;
  if (AM_myproc != 0) {
    while (down_buf == 0)
      BR_POLL();
  }
  if (left < AM_procs)
    BR_REQUEST_4(left, (BR_handler_4_t)BR_incr_handler, (int)&down_buf, 0, 0, 0);
  if (right < AM_procs)
    BR_REQUEST_4(right, (BR_handler_4_t)BR_incr_handler, (int)&down_buf, 0, 0, 0);
  down_buf = 0;
} 

void BR_BARRIER()
{
  /*  TNF_PROBE_0(BR_BARRIER_start, "BRAHMA BR_BARRIER BR_BARRIER_start", "")*/
  BR_BARRIER_up();
  BR_BARRIER_down();
  /*  TNF_PROBE_0(BR_BARRIER_end, "BRAHMA BR_BARRIER BR_BARRIER_end", "")*/
}

