/*                                                                      tab:8
 *
 * "Copyright (c) 1994 The Regents of the University of California.
 * All rights reserved.
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
 * Author:              Richard P. Martin
 * Version: 		1
 * Creation Date:       Thu Oct 28 10:47:28 PDT 1993
 * Filename:            ping.c ping test for HPAM library
 * History:
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <fcntl.h>
#include <time.h>
#include <sys/systeminfo.h> 
#include "am.h"

extern int errno;

extern int *UBLOCK[];
extern int unit;

/* program used on boot to force a copy-block load and then 
 * hold the lanai in reset */

main(int argc,char *argv[],char *envp[])
{
  int sts,copy_block_len;
  int units;

  unit = 0;
  units = open_lanai_copy_block(&copy_block_len,&sts);	
  if (copy_block_len<=0) {
    perror("ops: open_copy_block()");
    printf("error! Length = %d  <= 0\n",copy_block_len);
    exit(-1);
  }

  memset(UBLOCK[0],0,copy_block_len);
  lanai_interrupt_unit(0,0);
  lanai_reset_unit(0,1);  /* leave in reset */
  lanai_clear_memory(0);

}
